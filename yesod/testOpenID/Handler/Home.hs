module Handler.Home where

import Import

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Tools.YahooJapan

getHomeR :: Handler Html
getHomeR = lift (Text.concat . Text.lines <$> Text.readFile "y_clientId.txt")
	>>= redirectToAuthYahoo
