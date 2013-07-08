{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Network.Reddit.Monad where

import Control.Monad.Free
import Data.Aeson.Types
import Network.Browser
import Control.Monad.State
import Control.Monad.Morph
import Control.Concurrent (threadDelay)
import Control.Applicative
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Network.HTTP as HTTP

data RedditF next where
   Interaction :: (RedditInteraction i o) => i -> (Result o -> next) -> RedditF next
   SetModhash :: String -> next -> RedditF next

class RedditInteraction i o | i -> o where
     fetchR :: String -> i -> StdBrowserAction (Result Value)
     interpretR :: i -> Value -> Result o -- The g is only needed so that the typechecker can select the correct instance
     actionR :: String -> i -> StdBrowserAction (Result o)
     actionR m i = fmap (interpretR i =<<) . fetchR m $ i

instance Functor RedditF where
	fmap f (Interaction i c) = Interaction i (f . c)
	fmap f (SetModhash m next) = SetModhash m (f next)

type Reddit a = Free RedditF a
type StdBrowserAction a = BrowserAction (HTTP.HandleStream String) a

data FromRedditOptions = FromRedditOptions {
	modifier :: forall a. StdBrowserAction a -> StdBrowserAction a, -- Changes to each interaction
	modhash :: String}
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