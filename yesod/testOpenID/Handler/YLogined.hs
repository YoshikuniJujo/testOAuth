module Handler.YLogined where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64.URL as B64

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS

import Crypto.MAC.HMAC
import qualified Crypto.Hash.SHA256 as SHA256

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
	Just state <- lookupGetParam "state"
	print code
	print state
	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	yClientId <-
		lift $ BS.concat . BSC.lines <$> BS.readFile "y_clientId.txt"
	yClientSecret <-
		lift $ BS.concat . BSC.lines <$> BS.readFile "y_clientSecret.txt"
	let	yClientIdSecret = B64.encode $ yClientId <> ":" <> yClientSecret
		req = setRequestHeader "Content-Type"
			["application/x-www-form-urlencoded"]
			initReq { method = "POST" }
		req' = setRequestHeader "Authorization"
			["Basic " <> yClientIdSecret] req
		req'' = setRequestBody (RequestBodyBS $
			"grant_type=authorization_code&code=" <>
			encodeUtf8 code <>
			"&redirect_uri=http://localhost:3000/ylogined") req'
	rBody <- getResponseBody <$> httpLBS req''
	print rBody
	let	Just resp  =  Aeson.decode rBody :: Maybe Aeson.Object
	print $ keys resp
	let	Just (String at) = HML.lookup "access_token" resp
		Just (String it) = HML.lookup "id_token" resp
	print at
	print it
	let	[hd, pl, sg] = Text.splitOn "." it
		[Just hdd, Just pld] = map
			((Aeson.decode :: LBS.ByteString -> Maybe Aeson.Object)
				. LBS.fromStrict
				. either (error . show) id
				. B64.decode . encodeUtf8)
			[hd, pl]
	print hdd
	print pld
	putStrLn sg
	lift . BSC.putStrLn . B64.encode
		. hmac SHA256.hash 64 yClientSecret
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	submission = Nothing :: Maybe FileForm
		handlerName = "getYLoginedR" :: Text
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
