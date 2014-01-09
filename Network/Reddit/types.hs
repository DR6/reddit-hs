{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Network.Reddit.Types where
import Data.Time
import Network.Browser
import Data.String

-- Types
data RedditName a = RedditName {getName :: RedditID}
forgetType :: Show (RedditName a) => RedditName a -> RedditName ()
forgetType = RedditName . show
type RedditID = String

instance Show (RedditName Link) where
   show (RedditName i) = "t3_"++i
instance IsString (RedditName Link) where
	fromString = RedditName . drop 3
instance Show (RedditName Comment) where
   show (RedditName i) = "t1_"++i

instance Show (RedditName Account) where
	show (RedditName i) = "t2_"++i

instance Show (RedditName Subreddit) where
	show (RedditName i) = "t5_"++i

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
	comment_rname :: RedditName Comment,
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
	link_rname :: RedditName Link,
	link_info :: ContentInfo,
	link_votes :: Votes,
	--link_created :: CreatedInfo,
	clicked :: Bool,
	domain :: String,
	hidden :: Bool,
	is_self :: Bool,
	link_flair :: Maybe (Either String String),
	-- media :: String, -- Object
	-- media_embed :: String, --Object
	num_comments :: Int,
	link_over_18 :: Bool,
	permalink :: String,
	saved :: Bool,
	score :: Int,
	selftext :: Maybe (String, String),
	thumbnail :: String,
	url :: String} deriving Show

data Account = Account {
	user_rname :: RedditName Account,
	user_comment_karma :: Int,
	--created
	--created_utc
	has_mail :: Maybe Bool,
	has_mod_mail :: Maybe Bool,
	has_verified_email :: Bool,
	is_friend :: Bool,
	is_gold :: Bool,
	is_mod :: Bool,
	user_link_karma :: Int,
	user_modhash :: Maybe String,
	user_name :: String,
	user_over_18 :: Bool}
		deriving Show

data Subreddit = Subreddit {
	subreddit_rname :: RedditName Subreddit,
	accounts_active :: Int,
	description :: String,
	description_html :: String,
	display_name :: String,
	header_img :: Maybe String,
	header_size :: Maybe (Int, Int),
	subreddit_over_18 :: Bool,
	public_description :: String,
	subscribers :: Int,
	title :: String,
	subreddit_url :: String}
		deriving Show

type RedditRange a = (RedditName a, RedditName a)