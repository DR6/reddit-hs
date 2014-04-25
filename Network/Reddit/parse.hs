{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Applicative
import Network.Reddit.Types
import Control.Monad

constructEither :: Maybe a -> Maybe b -> Maybe (Either a b)
constructEither (Just left) _ = Just . Left $ left
constructEither _ (Just right) = Just . Right $ right
constructEither _ _ = Nothing
maybeToResult s = maybe (Error s) (Success)

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