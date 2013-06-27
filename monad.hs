{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, DeriveFunctor, RankNTypes #-}
module Monad where

import Types
import Instances

import Control.Monad.Free
import Data.Aeson.Types
import Network.Browser
import Control.Monad.State
import Control.Monad.Morph
import Control.Concurrent (threadDelay)
import Control.Applicative

data RedditF next where
   Interaction :: (RedditInteraction i o) => i -> (Result o -> next) -> RedditF next
   SetModhash :: Modhash -> next -> RedditF next

instance Functor RedditF where
	fmap f (Interaction i c) = Interaction i (f . c)
	fmap f (SetModhash m next) = SetModhash m (f next)

type Reddit a = Free RedditF a
interaction i = liftF $ Interaction i id
setModhash m = liftF $ SetModhash m ()
login u p = do
	m <- interaction $ Login u p
	case m of
		(Success modhash) -> setModhash modhash >> return Nothing
		(Error s) -> return . Just $ s

data FromRedditOptions = FromRedditOptions {
	modifier :: forall a. StdBrowserAction a -> StdBrowserAction a, -- Changes to each interaction
	modhash :: Modhash}
defOptions = FromRedditOptions {
	modifier = \b -> liftIO (threadDelay 1000000) >> b,
	modhash = ""}

--rawToBrowserAction :: FromRedditOptions -> Reddit a -> StdBrowserAction a
customToBrowserAction ops = flip evalStateT (modhash ops) . retract . hoistFree (hoist (modifier ops) . runner)
	where
		runner (Interaction input f) = get >>= \modhash -> (lift . fmap f . actionR modhash $ input)
		runner (SetModhash m e) = put m >> return e

{--}
toBrowserAction = customToBrowserAction defOptions
customPerformReddit initializer ops = browse . (initializer>>) . rawToBrowserAction ops

-- Test zone
{--}
testAction :: Reddit Bool
testAction = do
	loginerr <- login "DR6" "drgdrg493"
	case loginerr of
		Nothing -> do
			comment <- interaction $ MakeComment "Hello from Haskell!\n*Markdown should work*" (RedditName "t3_1h1qsg")
			return $ traceValueWith id comment
			return True
		Just e -> 
			return False

initializer :: StdBrowserAction ()
initializer = do
	setAllowRedirects True
	setUserAgent "haskell reddit API writer, still in early development"

main = print =<< customPerformReddit initializer defOptions testAction