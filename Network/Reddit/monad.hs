{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances #-}
module Network.Reddit.Monad where

import Network.Reddit.Types
import Network.Reddit.Instances

import Control.Monad.Free
import Data.Aeson.Types
import Network.Browser
import Control.Monad.State
import Control.Monad.Morph
import Control.Concurrent (threadDelay)
import Control.Applicative
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)


data RedditF next where
   Interaction :: (RedditInteraction i o) => i -> (Result o -> next) -> RedditF next
   SetModhash :: Modhash -> next -> RedditF next

instance Functor RedditF where
	fmap f (Interaction i c) = Interaction i (f . c)
	fmap f (SetModhash m next) = SetModhash m (f next)

type Reddit a = Free RedditF a

data FromRedditOptions = FromRedditOptions {
	modifier :: forall a. StdBrowserAction a -> StdBrowserAction a, -- Changes to each interaction
	modhash :: Modhash}
defOptions = FromRedditOptions {
	modifier = \b -> liftIO (threadDelay 1000000) >> b,
	modhash = ""}

defInitializer :: StdBrowserAction ()
defInitializer = do
	setAllowRedirects True
	setUserAgent "haskell reddit API wrapper, still in early development"

customToBrowserAction :: FromRedditOptions -> Reddit a -> StdBrowserAction a
customToBrowserAction ops = flip evalStateT (modhash ops) . retract . hoistFree (hoist (modifier ops) . runner)
	where
		runner (Interaction input f) = get >>= \modhash -> (lift . fmap f . actionR modhash $ input)
		runner (SetModhash m e) = put m >> return e

toBrowserAction = customToBrowserAction defOptions
customPerformReddit initializer ops = browse . (initializer>>) . customToBrowserAction ops
performReddit = customPerformReddit defInitializer defOptions