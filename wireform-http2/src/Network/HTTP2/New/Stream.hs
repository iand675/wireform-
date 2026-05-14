-- | Stream utilities.
module Network.HTTP2.New.Stream where

import Data.IORef (readIORef)
import Network.HTTP2.New.Types

-- | Check whether a stream has finished sending.
isStreamTxDone :: Stream -> IO Bool
isStreamTxDone = readIORef . streamTxDone

-- | Check whether the peer has finished sending on this stream.
isStreamRxDone :: Stream -> IO Bool
isStreamRxDone = readIORef . streamRxDone
