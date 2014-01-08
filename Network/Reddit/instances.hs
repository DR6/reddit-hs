{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, TupleSections, TemplateHaskell #-}

module Network.Reddit.Instances where

import Network.Reddit.Types
import Network.Reddit.Monad

import Data.Aeson
import Control.Lens
import Control.Lens.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP as HTTP
import Network.URI hiding (path, query)
import Network.Browser

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Maybe(fromJust, catMaybes)
import Control.Lens.TH

-- Unspecific utility functions
convert :: (Enum a, Enum b) => a -> b
convert = toEnum . fromEnum
constructEither :: Maybe a -> Maybe b -> Maybe (Either a b)
constructEither (Just left) _ = Just . Left $ left
constructEither _ (Just right) = Just . Right $ right
constructEither _ _ = Nothing
maybeToResult s = maybe (Error s) (Success)


-- URI handling
$(makeLensesFor [("uriScheme","_uriScheme"),
	("uriAuthority","_uriAuthority"),
	("uriPath","_uriPath"),
	("uriQuery","_uriQuery"),
	("uriFragment","_uriFragment")
	] ''URI)

redditURI :: Bool -> URI -> URI -- the bool decides wether it's np or not
redditURI np = (_uriScheme .~ "http:") . (_uriAuthority .~ auth)
    where server = if np then "np.reddit.com" else "www.reddit.com"
          auth = Just $ URIAuth "" server ""

path :: String -> URI -> URI
path str = (_uriPath .~ ("/"++str) )

asjson :: URI -> URI
asjson =  _uriPath %~ (++".json")

query :: [(String, String)] -> URI -> URI
query attrs = (_uriQuery .~ HTTP.urlEncodeVars attrs)

-- Specific utility functions
value_from_request :: HTTP.Request_String -> StdBrowserAction (Result Value)
value_from_request = 
	fmap (maybeToResult "Decoding error" 
		. decode 
		. L.pack
		. map convert
		. HTTP.rspBody
		. snd)
	. request

get :: URI -> HTTP.Request_String
get = HTTP.getRequest . show
post :: URI -> HTTP.Request_String
post = HTTP.postRequest . show


-- RedditInteraction instances together with their associated types
newtype LinkOnly = LinkOnly {getLinkOnly :: RedditName Link}
instance RedditFetch LinkOnly Link where
     fetch link = fmap (>>= interpret) . value_from_request . get . redditURI False . asjson . path getlink $ nullURI
          where
		getlink = "by_id/"++ linkid
		linkid = show . getLinkOnly $ link
		interpret json = do
			linkJson <- maybeToResult "Error extracting link" $
				json ^? key "data" . key "children" . nth 0 . key "data"
			parse parseJSON linkJson

newtype LinkWithComments = LinkWithComments {getLinkWithComments :: RedditName Link}
instance RedditFetch LinkWithComments (Link, [Comment], Maybe (Reddit () [Comment])) where
    fetch link = fmap (>>= interpret) . value_from_request . get . redditURI False . asjson . path getlink $ nullURI
         where
		getlink = "comments/" ++ linkid
		linkid = show . getLinkWithComments $ link
		interpret json = do
			linkJson <- maybeToResult "Error extracting link" $
				json ^? nth 0 . key "data" . key "children" . nth 0 . key "data"
			commentsJson <- maybeToResult "Error extracting comments" $ 
				json ^? nth 1 . key "data" . key "children"
			unloadedCommentsJson <- Success Nothing -- TI
			link <- parse parseJSON linkJson
			comments <- parse parseJSON commentsJson
			unloadedComments <- Success Nothing -- TI
			return (link, comments, unloadedComments)


data Login = Login String String -- Username and password
instance RedditFetch Login String where
	fetch (Login un pw) = fmap (>>= interpret) . value_from_request . post . redditURI False . path ("api/login/"++un) . query vars $ nullURI
	     where
			vars = [("user",un),("password",pw),("api_type","json")]
			interpret json = do
				loginJson <- maybeToResult "Error extracting modhash" $
					json ^? key "json" . key "data" . key "modhash"
				parse parseJSON loginJson


data MeRequest = MeRequest
instance RedditAct MeRequest Account where
	act _ _ = fmap (>>= interpret) . value_from_request . HTTP.getRequest $ "http://www.reddit.com/api/me.json"
		where
			interpret json = do
				meJson <- maybeToResult "Error extracting user information" $
					json ^? key "data"
				parse parseJSON meJson


data MakeComment = MakeComment {text :: String, target_id :: RedditName ()}
instance RedditAct MakeComment Value where
	act m comment = fmap (>>= interpret) . value_from_request . post . redditURI False . path "api/comment" . query vars $ nullURI
	    where
		vars = [("api_type","json"), ("text",text comment), ("thing_id",show . target_id $ comment), ("uh",m)]
		interpret = Success


newtype SubredditInfo = SubredditInfo {getSubredditInfo :: String}
instance RedditFetch SubredditInfo Subreddit where
	fetch sub = fmap (>>= interpret) . value_from_request . get . redditURI False . asjson . path aboutpath $ nullURI
	    where
		aboutpath = getSubredditInfo sub ++ "about.json"
		interpret json = do
			subJson <- maybeToResult "Error extracting subreddit" $
				json ^? key "data"
			parse parseJSON subJson

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
instance RedditFetch GetLinkListing [Link] where
	fetch getlisting = fmap (>>= interpret) . value_from_request . get . path (usubreddit ++ current_sorting) . query vars $ nullURI
	    where
		usubreddit = maybe "" id . fromsubreddit $ getlisting
		current_sorting = show . sorting $ getlisting
		vars = catMaybes $ [
			fmap (("limit",) . show) (limit getlisting)
			]
		interpret json = do
			listingJson <- maybeToResult "Listing not found" $
				json ^? key "data" . key "children"
			let commentJsons = listingJson ^.. values . key "data"
			forM commentJsons $ \comment -> parse parseJSON comment

-- Parsing instances
instance FromJSON Link where
    parseJSON (Object o) = Link
	    <$> fmap (RedditName . drop 3) (o .: "name")
	    <*> parseJSON (Object o) -- ContentInfo
	    <*> parseJSON (Object o) -- Votes
	    <*> return False -- TI, clicked
	    <*> o .: "domain"
	    <*> return False -- TI, hidden
	    <*> (o .: "is_self")
	    <*> (constructEither
	              <$> o .:? "link_flair_css_class"
	              <*> o .:? "link_flair_text")
	    -- <*> (pure "to implement") -- media, TI
	    -- <*> (pure "to implement") -- media_embed, TI
	    <*> o .: "num_comments"
	    <*> o .: "over_18"
	    <*> o .: "permalink"
	    <*> o .: "saved"
	    <*> o .: "score"
	    <*> (fmap (\x -> (,) <$> fst x <*> snd x) $ (,) 
	    	 <$> o .:? "selftext"
	    	 <*> o .:? "selftext_html")
	    <*> o .: "thumbnail"
	    <*> o .: "url"
    parseJSON _ = mzero

instance FromJSON Comment where
    parseJSON (Object o) = (Comment
    	<$> o .:? "approved_by"
    	<*> (fmap (RedditName . drop 3) (o .: "name"))
        <*> parseJSON (Object o)
        <*> parseJSON (Object o)
        <*> o .: "banned_by"
        <*> ((,)
        	<$> o .: "body_markdown"
        	<*> o .: "body.html")
        <*> fmap (RedditName . drop 3) (o .: "link_id")
        <*> (o .: "link_title")
        <*> (o .:? "num_reports")
        <*> (o .: "parent_id"))

instance FromJSON ContentInfo where
    parseJSON (Object o) = (ContentInfo
	    <$> o .: "author"
	    <*> (constructEither
	         <$> o .:? "author_flair_css_class"
	         <*> o .:? "author_flair_text")
	    <*> return False -- TI, edited
	    <*> o .: "subreddit"
	    <*> o .: "subreddit_id"
	    <*> o .:? "distinguished")
    parseJSON _ = mzero
    
instance FromJSON Votes where
    parseJSON (Object o) = (Votes
	    <$> o .: "ups"
	    <*> o .: "downs"
	    <*> o .: "likes")
    parseJSON _ = mzero

instance FromJSON Account where
	parseJSON (Object o) = (Account
		<$> fmap RedditName (o .: "id")
		<*> o .: "comment_karma"
		-- created
		-- created_utc
		<*> o .:? "has_mail"
		<*> o .:? "has_mod_mail"
		<*> o .: "has_verified_email"
		<*> o .: "is_friend"
		<*> o .: "is_gold"
		<*> o .: "is_mod"
		<*> o .: "link_karma"
		<*> o .:? "modhash"
		<*> o .: "name"
		<*> o .: "over_18")
	parseJSON _ = mzero
	
instance FromJSON Subreddit where
	parseJSON (Object o) = (Subreddit
		<$> fmap RedditName (o .: "id")
		<*> o .: "accounts_active"
		<*> o .: "description"
		<*> o .: "description_html"
		<*> o .: "display_name"
		<*> o .:? "header_img"
		<*> o .:? "header_size"
		<*> o .: "over18"
		<*> o .: "description"
		<*> o .: "subscribers"
		<*> o .: "title"
		<*> o .: "url")