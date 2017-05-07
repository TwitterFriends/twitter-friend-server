module Twitter.Types where

import           BasicPrelude 

import           Data.Aeson

data Tag = Tag { tag :: Text } deriving Show
data UserMention = UserMention {user_mention :: Text} deriving Show

type Tweet = Text
type TwitterName = Text

data TweetInfo = TweetInfo
  { hashtags :: [Tag]
  , user_mentions :: [UserMention]
  , tweets :: Tweet
  } deriving Show

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \o -> do
    tag <- o .: "text"
    return Tag {..}

instance FromJSON UserMention where
  parseJSON = withObject "UserMention" $ \o -> do
    user_mention <- o .: "name"
    return UserMention {..}
 
instance FromJSON TweetInfo where
  parseJSON = withObject "UserInfo" $ \o -> do
    tweets        <- o .: "text"
    entities      <- o .: "entities" 
    hashtags      <- entities .: "hashtags"
    user_mentions <- entities .: "user_mentions"
    return TweetInfo {..}


