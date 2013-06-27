{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Types where
import Data.Time
import Network.Browser

-- Types
data RedditName a = RedditName {getName :: RedditID}
nullRedditName :: Show (RedditName a) => RedditName a -> RedditName ()
nullRedditName = RedditName . show
type RedditID = String

instance Show (RedditName Link) where
   show (RedditName i) = "t3_"++i
   
instance Show (RedditName Comment) where
   show (RedditName i) = "t1_"++i

instance Show (RedditName ()) where
	show = getName

data Votes = Votes {
    ups :: Int,
    downs :: Int,
    likes :: Maybe Bool} deriving Show

data CreatedInfo = CreatedInfo {
    created :: UTCTime} deriving Show

data ContentInfo 
  = ContentInfo {
	author :: String,
	author_flair :: Maybe (Either String String),
	edited :: Bool,
	subreddit ::  String,
	subreddit_id :: RedditID,
	distinguished :: Maybe Distintion} deriving Show

-- data Distintion = Moderator | Admin | Special
type Distintion = String -- to be improved

data Comment = Comment {
	approved_by :: Maybe String,
	comment_fullname :: RedditName Comment,
    comment_info :: ContentInfo,
    comment_votes :: Votes,
    --comment_created :: CreatedInfo,
    banned_by :: String,
    body :: (String,String),
    link_id :: RedditName Link,
    link_title :: String,
    num_reports :: Maybe Int,
    parent_id :: RedditID} deriving Show

data Link = Link {
	link_name :: RedditName Link,
	link_info :: ContentInfo,
	link_votes :: Votes,
	--link_created :: CreatedInfo,
	clicked :: Bool,
	domain :: String,
	hidden :: Bool,
	is_self :: Bool,
	link_flair :: Maybe (Either String String),
	media :: String, -- Object
	media_embed :: String, --Object
	num_comments :: Int,
	over_18 :: Bool,
	permalink :: String,
	saved :: Bool,
	score :: Int,
	selftext :: Maybe (String, String),
	thumbnail :: String,
	url :: String} deriving Show

type Modhash = String