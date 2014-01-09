{-# LANGUAGE GADTs #-}

module Network.Reddit(
	module Network.Reddit.Instances,
	module Network.Reddit.Types,
	module Network.Reddit.Monad,
	fetchR, actR, withModhash, withLogin) where

import Network.Reddit.Monad
import Network.Reddit.Types
import Network.Reddit.Instances


import Data.Aeson(Result(..))
import Control.Monad.Free
import Network.Browser(browse)
import Control.Monad(join)

fetchR i = liftF $ Fetch i id
actR i = liftF $ Act i id

withModhash :: String -> Reddit RequiresLogin a -> Reddit () a
withModhash m r = iterM useModhash r
	where useModhash :: RedditF RequiresLogin (Reddit () a) -> Reddit () a
	      useModhash (Fetch i handler) = fetchR i >>= handler
	      useModhash a@(Act _ _) = join . liftF $ WithLogin m a

withLogin :: String -> String -> Reddit RequiresLogin a -> Reddit () a
withLogin u p r = do
	modhash <- fetchR $ Login u p
	withModhash modhash r