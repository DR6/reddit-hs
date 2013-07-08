module Network.Reddit where

import Network.Reddit.Monad
import Network.Reddit.Types
import Network.Reddit.Instances


import Data.Aeson(Result(..))
import Control.Monad.Free
import Network.Browser(browse)

interaction i = liftF $ Interaction i id
setModhash m = liftF $ SetModhash m ()
login u p = do
	m <- interaction $ Login u p
	case m of
		(Success modhash) -> setModhash modhash >> return Nothing
		(Error s) -> return . Just $ s

-- Testing
test = print =<< (performReddit . interaction $ defGetListing (Just "/r/no_sob_story") New)
