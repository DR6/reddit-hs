{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings, GADTs, FlexibleInstances #-}

module Instances where

import Types

import Data.Aeson
import Control.Lens((^.), _1, _2, _3)
import Data.Aeson.Lens(nth,key)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP as HTTP
import qualified Network.URI as URI
import Network.Browser

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Maybe(fromJust)

-- Debugging functions and imports. Everything should work correctly without these
import Debug.Trace
import Data.Word
import System.IO.Unsafe

-- Type synonyms
type StdBrowserAction a = BrowserAction (HTTP.HandleStream String) a


-- Testing functions
traceValueWith f a = traceShow (f a) a
jsonStr = putStr . map (convert :: Word8 -> Char). L.unpack . encode . (\(Success a) -> a) :: Result Value -> IO ()

-- a <- getObject . LinkWithComments . RedditName $ "1dx0q5"
-- b <- getObject . LinkOnly . RedditName $ "1dx0q5"

-- Unspecific tility functions
convert :: (Enum a, Enum b) => a -> b
convert = toEnum . fromEnum
constructEither :: Maybe a -> Maybe b -> Maybe (Either a b)
constructEither (Just left) _ = Just . Left $ left
constructEither _ (Just right) = Just . Right $ right
constructEither _ _ = Nothing
maybeToResult s = maybe (Error s) (Success)

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
request_from_name :: (String -> String) -> RedditName a -> HTTP.Request_String
request_from_name f =
	HTTP.getRequest
	. f
	. getName

from_name_with_url :: (String -> String) -> RedditName a -> StdBrowserAction (Result Value)
from_name_with_url f = value_from_request . request_from_name f

makeLoginRequest (Login username password) = HTTP.postRequest $"http://www.reddit.com/api/login/"++username++"?user="++username++"&passwd="++password++"&api_type=json"
makeCommentRequest m c = HTTP.postRequest $ "http://www.reddit.com/api/comment?"++HTTP.urlEncodeVars [
	("api_type","json"),
	("text", text c),
	("thing_id", show . target_id $ c),
	("uh",m)]

-- RedditInteraction class and a few
class RedditInteraction i o | i -> o where
     fetchR :: Modhash -> i -> StdBrowserAction (Result Value)
     interpretR :: i -> Value -> Result o -- The g is only needed so that the typechecker can select the correct instance
     actionR :: Modhash -> i -> StdBrowserAction (Result o)
     actionR m i = fmap (interpretR i =<<) . fetchR m $ i

data More c where
    More :: (RedditInteraction g c) => g -> More c

-- RedditInteraction instances together with their associated types
newtype LinkOnly = LinkOnly {getLinkOnly :: RedditName Link}
instance RedditInteraction LinkOnly Link where
     fetchR _ = from_name_with_url (\i -> "http://www.reddit.com/by_id/t3_"++i++".json")
                 . getLinkOnly
     interpretR _ =
      join
      . maybeToResult "Pre-parsing error"
      . fmap (parse parseJSON)
      . (^. key "data" . key "children" . nth 0 . key "data")
      . Just
      
newtype LinkWithComments = LinkWithComments {getLinkWithComments :: RedditName Link}
instance RedditInteraction LinkWithComments (Link, [Comment], Maybe (More [Comment])) where
    fetchR _ = from_name_with_url (\i -> "http://www.reddit.com/comments/"++i++".json")
                  . getLinkWithComments
    interpretR _ =
    	join
    	. fmap (\t -> (,,) <$> (t ^. _1) <*> (t ^. _2) <*> (t ^. _3))
    	. (\t -> (,,) <$> (fst t) <*> (fst . snd $ t) <*> (snd . snd $ t)) -- this trickery is needed to emulate `sequence`
    	. (maybeToResult "Pre-parsing error" *** maybeToResult "Pre-parsing error" *** maybeToResult "Pre-parsing error")
    	. (fmap (parse parseJSON) *** fmap (parse parseJSON) *** fmap (Success))
    	. ((^. nth 0 . key "data" . key "children" . nth 0 . key "data")
    		&&& (^. nth 1 . key "data" . key "children")
    		&&& (const (Just Nothing))) -- TI
    	. Just
 
data Login = Login String String -- Username and password
instance RedditInteraction Login Modhash where
	fetchR _ =
		value_from_request
		. makeLoginRequest
	interpretR _ =
		join
		. maybeToResult "Pre-parsing error"
		. fmap (parse parseJSON)
		. (^. key "json" . key "data" . key "modhash")
		. Just

data MeRequest = MeRequest
instance RedditInteraction MeRequest Value where
	fetchR _ = 
		value_from_request
		. HTTP.getRequest
		. const "http://www.reddit.com/api/me.json"
	interpretR _ =
		join
		. maybeToResult "Pre-parsing error"
		. fmap (parse parseJSON)
		. (^. key "data")
		. Just


data MakeComment = MakeComment {text :: String, target_id :: RedditName ()}
instance RedditInteraction MakeComment Value where
	fetchR m = value_from_request . makeCommentRequest m . traceShow m
	interpretR _ = Success


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
	    <*> (pure "to implement") -- media, TI
	    <*> (pure "to implement") -- media_embed, TI
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

instance FromJSON User where
	parseJSON (Object o) = (User
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