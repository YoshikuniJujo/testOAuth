module Tools.YahooJapan (
	redirectToAuthYahoo
	) where

import Import

redirectToAuthYahoo :: Text -> Handler Html
redirectToAuthYahoo yClientId = redirect $
	"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
		"response_type=code+id_token&" <>
		"scope=openid+profile&" <>
		"client_id=" <> yClientId <> "&state=hogeru&" <>
		"nonce=abcdefghijklmnop&" <>
		"redirect_uri=http://localhost:3000/ylogined"
