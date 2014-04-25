module Network.Reddit.Requests where

import Network.Reddit.Types
import Data.Tree

newtype LinkOnly = LinkOnly {getLinkOnly :: RedditName Link}

newtype LinkWithComments = LinkWithComments {getLinkWithComments :: RedditName Link}
type CommentForest = Forest (Either More Comment)

data Login = Login String String -- Username and password

data MeRequest = MeRequest

data MakeComment = MakeComment {text :: String, target_id :: RedditName ()}

newtype SubredditInfo = SubredditInfo {getSubredditInfo :: String}

data TimeSpan = Hour | Day | Week | Month | Year | All
data Sorting = Hot | New | Top TimeSpan | Controversial TimeSpan
instance Show Sorting where
	show Hot = "/hot"
	show New = "/new"
	show (Top _) = "/top"
	show (Controversial _) = "/controversial"
data GetLinkListing = GetLinkListing {
	fromsubreddit :: Maybe String,
	range :: Maybe (RedditRange Link),
	sorting :: Sorting,
	-- count
	limit :: Maybe Int}
	-- show
	-- target

defGetListing sub sort = GetLinkListing sub Nothing sort Nothing