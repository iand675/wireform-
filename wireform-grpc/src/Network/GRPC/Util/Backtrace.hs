
module Network.GRPC.Util.Backtrace (
    Backtraces -- opaque
  , collectBacktraces
  ) where

import Control.Exception.Backtrace qualified as Base
import GHC.Stack

newtype Backtraces = Backtraces {
      getBacktraces :: Base.Backtraces
    }

instance Show Backtraces where
  show = Base.displayBacktraces . getBacktraces

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = Backtraces <$> Base.collectBacktraces
