module Handler.FLogined where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple

import Tools.Temporary

import qualified Data.ByteString.Char8 as BSC
import Data.Aeson
import qualified Data.HashMap.Lazy as HML

import qualified Data.Text as Text

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
getFLoginedR :: Handler Html
getFLoginedR = do
	Just code <- lookupGetParam "code"
	print code
	lookupGetParam "state" >>= print
	initReq <- parseRequest "https://graph.facebook.com/v2.8/oauth/access_token"
	let	req = setRequestQueryString [
				("client_id", Just $ BSC.pack fClientId),
				("redirect_uri", Just "http://localhost:3000/flogined"),
				("client_secret", Just $ BSC.pack fClientSecret),
				("code", Just $ encodeUtf8 code)
				] $
			initReq { method = "POST" }
		req' = setRequestHeader "ACCEPT" ["application/json"] req
	rBody <- getResponseBody <$> httpLBS req'
	print rBody
	let Just (String at) =
		(HML.lookup "access_token") =<< (decode rBody :: Maybe Object)
	print at
	initReq2 <- parseRequest $ "https://graph.facebook.com/v2.8/me?access_token="
		<> Text.unpack at
--	initReq2 <- parseRequest $ "https://graph.facebook.com/v2.8/me"
	let	req2 = setRequestHeader
			"Authorization"
			["token " <> encodeUtf8 at]
--			["token " <> toStrict at]
			initReq2
		req2' = setRequestHeader
			"User-Agent"
			["Yesod"]
			req2
	rBody2 <- getResponseBody <$> httpLBS req2'
	let Just json2 = decode rBody2 :: Maybe Object
	mapM_ print $ HML.toList json2
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	submission = Nothing :: Maybe FileForm
		handlerName = "getFLoginedR" :: Text
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
