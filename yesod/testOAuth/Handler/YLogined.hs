{-# LANGUAGE OverloadedStrings #-}

module Handler.YLogined where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple

import Tools.Temporary

import qualified Data.ByteString.Char8 as BSC
import Data.Aeson
import qualified Data.HashMap.Lazy as HML

import qualified Data.ByteString.Base64.URL as B64

import qualified Data.Text as Text

import Crypto.MAC.HMAC
import qualified Crypto.Hash.SHA256 as SHA256

import Data.Time.Clock.POSIX

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getYLoginedR :: Handler Html
getYLoginedR = do
	Just code <- lookupGetParam "code"
	print code
	lookupGetParam "state" >>= print
	initReq <- parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
--	initReq <- parseRequest "http://localhost:4000"
	let	yClientIdSecret = B64.encode $
			BSC.pack yClientId <> ":" <> BSC.pack yClientSecret
		req = setRequestQueryString [
				("grant_type", Just "authorization_code"),
				("code", Just $ encodeUtf8 code),
				("redirect_uri", Just "http://localhost:3000/ylogined")
				] $
			initReq { method = "POST" }
		reqRB = setRequestBody (RequestBodyBS $
			"grant_type=authorization_code&code=" <>
			encodeUtf8 code <>
			"&redirect_uri=http://localhost:3000/ylogined") req
		req' = setRequestHeader "Authorization"
			["Basic " <> yClientIdSecret] reqRB
		req'' = setRequestHeader "Content-Type"
			["application/x-www-form-urlencoded"] req'
		req''' = setRequestHeader "Accept" ["*/*"] req''
	rBody <- getResponseBody <$> httpLBS req'''
	print rBody
	let Just (String at) =
		(HML.lookup "access_token") =<< (decode rBody :: Maybe Object)
	putStrLn ""
	putStr "Access Token: "
	print at
	let Just (String it) =
		(HML.lookup "id_token") =<< (decode rBody :: Maybe Object)
	putStrLn ""
	putStr "ID Token: "
	print it
	initReq2 <- parseRequest $ "https://userinfo.yahooapis.jp/yconnect/v1/attribute?schema=openid"
--	initReq2 <- parseRequest $ "https://graph.facebook.com/v2.8/me"
	let	req2 = setRequestHeader
			"Authorization"
			["Bearer " <> encodeUtf8 at]
--			["token " <> toStrict at]
			initReq2
	rBody2 <- getResponseBody <$> httpLBS req2
	print rBody2
	let Just json2 = decode rBody2 :: Maybe Object
	mapM_ print $ HML.toList json2
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	submission = Nothing :: Maybe FileForm
		handlerName = "getYLoginedR" :: Text
	let [hd, pl, sg] = Text.splitOn "." it
	let [hdd, pld] = map (either (error . show) id . B64.decode . encodeUtf8)
		[hd, pl]
	either print (lift . BSC.putStrLn) . B64.decode $ encodeUtf8 hd
	either print (lift . BSC.putStrLn) . B64.decode $ encodeUtf8 pl
	putStrLn sg
	lift . BSC.putStrLn . B64.encode
		. hmac SHA256.hash 64 (BSC.pack yClientSecret)
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	lift getPOSIXTime >>= print
	defaultLayout $ do
		let (commentFormId, commentTextareaId, commentListId) = commentIds
		aDomId <- newIdent
		setTitle "Welcome To Yesod!"
		$(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
