{-# LANGUAGE GADTs #-}

module Network.Reddit where

import Network.Reddit.Monad
import Network.Reddit.Types
import Network.Reddit.Instances


import Data.Aeson(Result(..))
import Control.Monad.Free
import Network.Browser(browse)
import Control.Monad(join)

fetchR i = liftF $ Fetch i id
actR i = liftF $ Act i id

--withModhash :: String -> Reddit RequiresLogin a -> Reddit () a
withModhash m r = iterM useModhash r
	where useModhash :: RedditF RequiresLogin (Reddit () a) -> Reddit () a
	      useModhash (Fetch i handler) = fetchR i >>= handler
	      useModhash a@(Act _ _) = join . liftF $ WithLogin m a

-- Testing
-- test = print =<< (performReddit . getter $ defGetListing (Just "/r/no_sob_story") New)
{-
test = do
	result <- performReddit $ do
		m <- fetchR $ Login "DR6" "drgdrg493"
		case m of
			Success m -> WithLogin m $ actR MeRequest
			Error s -> return undefined
	print result
-}
