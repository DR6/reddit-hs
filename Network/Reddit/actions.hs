{-# LANGUAGE OverloadedStrings #-}

import Network.Reddit.Parse
import Network.Reddit.Types

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Network.Wreq hiding (Link)
import qualified Network.Wreq.Session as Session

import Data.Text
import Data.Tree

type URI = String

buildRedditURI :: Bool -> Bool -> String -> URI
buildRedditURI np json path 
    =  "http://"
    ++ if np then "np" else "www"
    ++ ".reddit.com/"
    ++ path
    ++ if json then ".json" else []


parseGet :: URI -> [(Text, Text)] -> (Value -> Result a) -> Maybe Session.Session -> IO (Result a)
parseGet uri query interpret session = do
    response <- (maybe getWith (flip Session.getWith) session) (defaults & params .~ query) uri
    let value = maybeToResult "The response doesn't have a proper body or isn't JSON" $ response ^?responseBody._JSON
    return $ interpret =<< value

linkOnly :: RedditName Link -> IO (Result Link)
linkOnly l = parseGet
    (buildRedditURI False True $ "by_id/" ++ show l)
    []
    (\val -> do
        link <- maybeToResult "Error extracting link" $
            val ^? key "data" . key "children" . nth 0 . key "data" . _JSON
        parse parseJSON link)
    Nothing

type CommentForest = Forest (Either More Comment)
linkWithComments :: RedditName Link -> IO (Result (Link, CommentForest))
linkWithComments l = parseGet
    (buildRedditURI False True $ "comments/" ++ getRedditName l)
    []
    interpreter
    Nothing
        where
            interpreter val = do
                linkJson <- maybeToResult "Error extracting link" $
                	val ^? nth 0 . key "data" . key "children" . nth 0 . key "data"
                firstLayerJson <- maybeToResult "Error extracting comments" $ 
                	val ^? nth 1 . key "data" . key "children"
                firstLayer <- return $ firstLayerJson ^.. values
                link <- parse parseJSON linkJson
                comments <- unfoldForestM makeTree firstLayer
                return (link, comments)
            makeTree val = do
    			kind <- maybeToResult "Value with no kind" $
    				val ^? key "kind"
    			data' <- maybeToResult "Value with no data" $
    				val ^? key "data"
    			case kind of
    				"t1" -> do
    					comment <- parse parseJSON data'
    					children <- return $ data' ^.. key "replies" . key "data" . key "children" . values
    					return (Right comment, children)
    				"more" -> parse parseJSON data' >>= \more -> return (Left more, [])

    