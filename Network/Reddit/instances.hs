{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances, TemplateHaskell, TupleSections #-}

module Network.Reddit.Instances where

import Network.Reddit.Types
import Network.Reddit.Monad
import Network.Reddit.Requests

import Data.Aeson
import Control.Lens
import Control.Lens.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP as HTTP
import Network.URI hiding (path, query)
import Network.Browser
import Data.Tree

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Maybe(fromJust, catMaybes)
import Control.Lens.TH
import Debug.Trace

-- Unspecific utility functions
traceShow :: Show a => a -> a
traceShow a = trace (show a) a
traceString s = trace s s
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
instance RedditRequest LinkOnly where
	type RedditResponse LinkOnly = Link
	redditRequest link = fmap (>>= interpret) . value_from_request . get . redditURI False . asjson . path getlink $ nullURI
		where
			getlink = "by_id/"++ linkid
			linkid = show . getLinkOnly $ link
			interpret json = do
				linkJson <- maybeToResult "Error extracting link" $
					json ^? key "data" . key "children" . nth 0 . key "data"
				parse parseJSON linkJson

instance RedditRequest LinkWithComments where
	type RedditResponse LinkWithComments = (Link, CommentForest)
	redditRequest link = fmap (>>= interpret) . value_from_request . get . redditURI False . asjson . path getlink $ nullURI
		where
			getlink = "comments/" ++ linkid
			linkid = drop 3 . show . getLinkWithComments $ link
			makeTree json = do
				kind <- maybeToResult "Value with no kind" $
					json ^? key "kind"
				data' <- maybeToResult "Value with no data" $
					json ^? key "data"
				case kind of
					"t1" -> do
						comment <- parse parseJSON data'
						children <- return $ data' ^.. key "replies" . key "data" . key "children" . values
						return (Right comment, children)
					"more" -> parse parseJSON data' >>= \more -> return (Left more, [])
			interpret json = do
				linkJson <- maybeToResult "Error extracting link" $
					json ^? nth 0 . key "data" . key "children" . nth 0 . key "data"
				firstLayerJson <- maybeToResult "Error extracting comments" $ 
					json ^? nth 1 . key "data" . key "children"
				firstLayer <- return $ firstLayerJson ^.. values
				link <- parse parseJSON linkJson
				comments <- unfoldForestM makeTree firstLayer
				return (link, comments)

instance RedditRequest Login where
	type RedditResponse Login = String
	redditRequest (Login un pw) = fmap (>>= interpret) . value_from_request . post . redditURI False . path ("api/login/"++un++"?") . query vars $ nullURI
	     where
			vars = [("user",un),("passwd",pw),("api_type","json")]
			interpret json = do
				modhash <- maybeToResult "Error extracting modhash" $
					json ^? key "json" . key "data" . key "modhash" . _String
				return $ show modhash

instance AuthRedditRequest MeRequest where
	type ActResponse MeRequest = Account
	redditRequest' _ _ = fmap (>>= interpret) . value_from_request . HTTP.getRequest $ "http://www.reddit.com/api/me.json"
		where
			interpret json = do
				meJson <- maybeToResult "Error extracting user information" $
					json ^? key "data"
				parse parseJSON meJson

instance AuthRedditRequest MakeComment where
	type ActResponse MakeComment = Value
	redditRequest' m comment = fmap (>>= interpret) . value_from_request . post . redditURI False . path "api/comment" . query vars $ nullURI
	    where
		vars = [("api_type","json"), ("text",text comment), ("thing_id",show . target_id $ comment), ("uh",m)]
		interpret = Success

instance RedditRequest SubredditInfo where
	type RedditResponse SubredditInfo = Subreddit
	redditRequest sub = fmap (>>= interpret) . value_from_request . get . redditURI False . asjson . path aboutpath $ nullURI
	    where
		aboutpath = getSubredditInfo sub ++ "about.json"
		interpret json = do
			subJson <- maybeToResult "Error extracting subreddit" $
				json ^? key "data"
			parse parseJSON subJson

instance RedditRequest GetLinkListing where
	type RedditResponse GetLinkListing = [Link]
	redditRequest getlisting = fmap (>>= interpret) . value_from_request . get . redditURI False . path (usubreddit ++ current_sorting) . query vars $ nullURI
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
        <*> o .:? "banned_by"
        <*> ((,)
        	<$> o .: "body"
        	<*> o .: "body_html")
        <*> fmap (RedditName . drop 3) (o .: "link_id")
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
	    <*> o .:? "likes")
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
	parseJSON _ = mzero

instance FromJSON More where
	parseJSON (Object o) = (More
		<$> fmap (RedditName . drop 3) (o .: "parent_id")
		<*> (fmap . fmap) (RedditName . drop 3) (o .: "children"))