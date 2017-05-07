module Twitter.AuthToken where

import BasicPrelude hiding (ByteString,lookup)

import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy hiding (zip)
import Data.Map.Strict (Map,lookup)
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header

import Safe
type AuthToken = ByteString

authClient :: IO Header
authClient = do
  manager  <- newManager tlsManagerSettings
  init_req <- parseRequest token_url
  let request = init_req { method = "POST" 
                         , requestHeaders = tokenHeader
                         , requestBody = RequestBodyLBS req_obj
                }
  body <- liftM (fromJustNote decode_fail_msg . decode)
            (responseBody <$> httpLbs request manager) :: IO (Map Text Text)
  return (authToken $ fromJustNote fail_msg $ lookup lookup_str body) 
  where
    req_obj = "grant_type=client_credentials"
    lookup_str = "access_token"
    decode_fail_msg = "JSON Error: No Body"
    fail_msg = "The impossible happened: no access token"
  
tokenHeader :: [Header]
tokenHeader = [content_header, auth_header] 
  where
    content_header, auth_header :: Header
    content_header = (,) hContentType contentBS
    auth_header    = (,) hAuthorization basicBS
    contentBS = "application/x-www-form-urlencoded;charset=UTF-8"
    basicBS   = "Basic aFh2MlVwUDJEZzlYRXBzMmJRekZZblloaToyenlsc2s3QWs2T1NCUVJoT1FmcFNsMGcxOG53QXpma0FlYktrRU9qWGtDZ0pVOTRJZA=="

authToken :: Text -> Header
authToken tok = (,) hAuthorization encoded
  where
    encoded = "Bearer " <> (encodeUtf8 tok)

token_url :: String
token_url = "https://api.twitter.com/oauth2/token"
{-
 curl -i -H 'Content-Type:application/x-www-form-urlencoded;charset=UTF-8' -H 'Authorization:Basic aFh2MlVwUDJEZzlYRXBzMmJRekZZblloaToyenlsc2s3QWs2T1NCUVJoT1FmcFNsMGcxOG53QXpma0FlYktrRU9qWGtDZ0pVOTRJZA==' -X POST -d 'grant_type=client_credentials' https://api.twitter.com/oauth2/token
-}
{-
 {"token_type":"bearer"
 ,"access_token":a_token}
-}

{-
 GET https://api.twitter.com/1.1/users/show.json?screen_name=twitterdev
-}

a_token = "AAAAAAAAAAAAAAAAAAAAAGjj0QAAAAAAep8kuaJV8ZVLlnorqbbeeDUowBE%3DikoFzjFZK5tO07ccNAN0mzYfUkoMH4xr84OPhlVI0NifzdwnuS"
