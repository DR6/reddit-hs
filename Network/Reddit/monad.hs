{-# LANGUAGE GADTs, RankNTypes, EmptyDataDecls, FlexibleInstances, TypeFamilies #-}
module Network.Reddit.Monad where

import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Aeson.Types
import Control.Concurrent (threadDelay)
import qualified Network.HTTP as HTTP
import Network.Browser


data RequiresLogin
data RedditF requires next where
	Fetch :: (RedditFetch i)  => i -> (FetchResponse i -> next) -> RedditF a next
	Act :: (RedditAct i) => i -> (ActResponse i -> next) -> RedditF RequiresLogin next
	WithLogin  :: String -> RedditF RequiresLogin next -> RedditF () next
	LiftIO :: IO a -> RedditF r a

class RedditFetch i where
	type FetchResponse i
	fetch :: i -> StdBrowserAction (Result (FetchResponse i))

class RedditAct i where
	type ActResponse i
	act :: String -> i -> StdBrowserAction (Result (ActResponse i))

instance Functor (RedditF a) where
	fmap f (Fetch i handler) = Fetch i (f . handler)
	fmap f (Act i handler) = Act i (f . handler)
	fmap f (WithLogin m poster) = WithLogin m (fmap f poster)
	fmap f (LiftIO action) = LiftIO . fmap f $ action

type Reddit r a = Free (RedditF r) a
type StdBrowserAction a = BrowserAction (HTTP.HandleStream String) a

instance MonadIO (Free (RedditF a)) where
	liftIO = liftF . LiftIO 

data FromRedditOptions = FromRedditOptions {
	modifier :: forall a. StdBrowserAction a -> StdBrowserAction a -- Changes to each interaction
	}
defOptions = FromRedditOptions {modifier = \b -> liftIO (threadDelay 1000000) >> b}

defInitializer :: StdBrowserAction ()
defInitializer = do
	setAllowRedirects True
	setUserAgent "haskell reddit API wrapper, still in early development"


newtype ResultT' m a = ResultT' {getResultT' :: (m (Result a))} -- this is not a conventional monad transformer, as it is "inverted"
instance Monad m => Monad (ResultT' m) where
	return = ResultT' . return . return
	(ResultT' m) >>= f = ResultT' $ m >>= \intermediate -> case intermediate of
		Success a -> getResultT' . f $ a
		Error str -> return $ Error str

customToBrowserAction :: FromRedditOptions -> Reddit () a -> StdBrowserAction (Result a)
customToBrowserAction ops = getResultT' . iterM run
	where
		run (Fetch i handler) = ResultT' (fetch i) >>= handler
		run (LiftIO action) = ResultT' (fmap return (liftIO action)) >>= id
		run (WithLogin modhash logged) = case logged of
			(Fetch i handler) -> ResultT' (fetch i) >>= handler
			(Act i handler) -> ResultT' (act modhash i) >>= handler

toBrowserAction = customToBrowserAction defOptions
customPerformReddit initializer ops = browse . (initializer>>) . customToBrowserAction ops
performReddit = customPerformReddit defInitializer defOptions