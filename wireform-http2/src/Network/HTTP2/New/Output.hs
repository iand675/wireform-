-- | Output queue: a bounded channel holding one item at a time per
-- connection.  The sender drains it; the receiver/handler writes to it.
module Network.HTTP2.New.Output
    ( enqueueOutput
    , dequeueOutput
    , enqueueControl
    , dequeueControl
    ) where

import Control.Concurrent.STM
import Network.HTTP2.New.Types

-- | The connection carries two queues:
-- * @ctxOutputQ@  — data outputs (OUnary, OStreaming)
-- * @ctxControlQ@ — control frames (OControl) with higher priority
--
-- The 'Context' record holds these as 'TQueue Output' so we can use
-- STM priority-dequeue in the sender.  For now we use a simple TQueue.

enqueueOutput :: TQueue Output -> Output -> IO ()
enqueueOutput q = atomically . writeTQueue q

dequeueOutput :: TQueue Output -> TQueue Output -> IO Output
dequeueOutput controlQ outputQ = atomically $ do
    mc <- tryReadTQueue controlQ
    case mc of
        Just x  -> return x
        Nothing -> readTQueue outputQ

enqueueControl :: TQueue Output -> Output -> IO ()
enqueueControl q = atomically . writeTQueue q

dequeueControl :: TQueue Output -> IO Output
dequeueControl = atomically . readTQueue
