module Twitter.Client where

import BasicPrelude hiding (ByteString,lookup)

import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy hiding (unpack,zip)
import Data.Map.Strict (Map,lookup)
import Data.Text.Encoding
import Data.Text
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header

import Safe

import Twitter.Types

client :: TwitterName -> AuthHeader -> IO [TweetInfo]
client twitter_name auth_header = do
  manager  <- newManager tlsManagerSettings
  init_req <- parseRequest timeline_str
  let req = init_req { method = "GET" , requestHeaders = [auth_header]}
  liftM (fromJustNote decode_fail_msg . decode)
    (responseBody <$> httpLbs req manager) :: IO [TweetInfo]
  where
    timeline_str = timeline_uri <> "?" <> screen_name <> "&" <> tweet_count
    screen_name = "screen_name=" <> (unpack twitter_name)
    tweet_count = "count=100"
    decode_fail_msg = "Error: failed to decode timeline"

timeline_uri :: String
timeline_uri = "https://api.twitter.com/1.1/statuses/user_timeline.json"

