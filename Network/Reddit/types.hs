{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances #-}

module Network.Reddit.Types where
import Data.Time
import Data.String
import Control.Lens

-- Types
class Named a where
    name :: Lens' a (RedditName a)

type RedditID = String
data RedditName a = RedditName {getRedditName :: RedditID} deriving (Eq)
data AnyRedditName = AnyRedditName {getAnyRedditName :: RedditID} deriving (Show,Eq)
forgetType :: Show (RedditName a) => RedditName a -> AnyRedditName
forgetType = AnyRedditName . show
instance IsString (RedditName a) where
	fromString = RedditName . drop 3


class Voted a where
    votes :: Lens' a Votes
data Votes = Votes {
    _ups :: Int,
    _downs :: Int,
    _likes :: Maybe Bool} deriving Eq
makeLenses ''Votes

class Created a where
    created :: Lens' a CreatedInfo
data CreatedInfo = CreatedInfo {
    creationTime :: UTCTime} deriving Eq
makeLenses ''CreatedInfo

class Content a where
    content :: Lens' a ContentInfo
data ContentInfo 
  = ContentInfo {
	author :: String,
	author_flair :: Maybe (Either String String),
	edited :: Bool,
	subreddit ::  String,
	subreddit_id :: RedditID,
	distinguished :: Maybe Distintion} deriving Eq

-- data Distintion = Moderator | Admin | Special
type Distintion = String -- to be improved

data Link = Link {
	_link_rname :: RedditName Link,
	_link_info :: ContentInfo,
	_link_votes :: Votes,
	--link_created :: CreatedInfo,
	_clicked :: Bool,
	_domain :: String,
	_hidden :: Bool,
	_is_self :: Bool,
	_link_flair :: Maybe (Either String String),
	-- media :: String, -- Object
	-- media_embed :: String, --Object
	_num_comments :: Int,
	_link_over_18 :: Bool,
	_permalink :: String,
	_saved :: Bool,
	_score :: Int,
	_selftext :: Maybe (String, String),
	_thumbnail :: String,
	_url :: String} deriving Eq
makeLenses ''Link
instance Named Link where name = link_rname
instance Voted Link where votes = link_votes
instance Content Link where content = link_info

data Comment = Comment {
	_approved_by :: Maybe String,
	_comment_rname :: RedditName Comment,
    _comment_info :: ContentInfo,
    _comment_votes :: Votes,
    --comment_created :: CreatedInfo,
    _banned_by :: Maybe String,
    _body :: (String,String),
    _link_id :: RedditName Link,
    _num_reports :: Maybe Int,
    _parent_id :: RedditID} deriving Eq
makeLenses  ''Comment
instance Named Comment where name = comment_rname
instance Voted Comment where votes = comment_votes
instance Content Comment where content = comment_info

data Account = Account {
	_user_rname :: RedditName Account,
	_user_comment_karma :: Int,
	--created
	--created_utc
	_has_mail :: Maybe Bool,
	_has_mod_mail :: Maybe Bool,
	_has_verified_email :: Bool,
	_is_friend :: Bool,
	_is_gold :: Bool,
	_is_mod :: Bool,
	_user_link_karma :: Int,
	_user_modhash :: Maybe String,
	_user_name :: String,
	_user_over_18 :: Bool} deriving Eq
makeLenses ''Account
instance Named Account where name = user_rname

data Subreddit = Subreddit {
	_subreddit_rname :: RedditName Subreddit,
	_accounts_active :: Int,
	_description :: String,
	_description_html :: String,
	_display_name :: String,
	_header_img :: Maybe String,
	_header_size :: Maybe (Int, Int),
	_subreddit_over_18 :: Bool,
	_public_description :: String,
	_subscribers :: Int,
	_title :: String,
	_subreddit_url :: String} deriving Eq
makeLenses ''Subreddit

data More = More {
	link :: RedditName Link,
	children :: [RedditName Comment]} deriving Show

type RedditRange a = (RedditName a, RedditName a)

instance Show (RedditName Comment) where
   show (RedditName i) = "t1_"++i
instance Show (RedditName Account) where
	show (RedditName i) = "t2_"++i
instance Show (RedditName Subreddit) where
	show (RedditName i) = "t5_"++i
instance Show (RedditName Link) where
   show (RedditName i) = "t3_"++i