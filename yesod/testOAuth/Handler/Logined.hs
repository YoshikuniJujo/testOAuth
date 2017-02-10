module Handler.Logined where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple

import Tools.Temporary

import qualified Data.ByteString.Char8 as BSC

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
getLoginedR :: Handler Html
getLoginedR = do
	Just code <- lookupGetParam "code"
	print code
	lookupGetParam "state" >>= print
	initReq <- parseRequest "https://github.com/login/oauth/access_token"
	let	req = setRequestQueryString [
				("client_id", Just $ BSC.pack clientId),
				("client_secret", Just $ BSC.pack clientSecret),
				("code", Just $ encodeUtf8 code)
				] $
			initReq { method = "POST" }
	httpLBS req >>= print . getResponseBody
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	submission = Nothing :: Maybe FileForm
		handlerName = "getLoginedR" :: Text
	defaultLayout $ do
		let (commentFormId, commentTextareaId, commentListId) = commentIds
		aDomId <- newIdent
		setTitle "Welcome To Yesod!"
		$(widgetFile "homepage")

postLoginedR :: Handler Html
postLoginedR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postLoginedR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

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
