module Network.HTTP.Semantics.ReadN (
    -- * Reading n bytes
    ReadN,
    defaultReadN,
)
where

import qualified Data.ByteString as B
import Data.IORef
import Network.Socket
import qualified Network.Socket.ByteString as N

-- | Reading n bytes.
type ReadN = Int -> IO B.ByteString

-- | Naive implementation for readN.
--
-- /NOTE/: This function is intended to be used by a single thread only.
-- (It is probably quite rare anyway to want concurrent reads from the /same/
-- network socket.)
defaultReadN :: Socket -> IORef (Maybe B.ByteString) -> ReadN
defaultReadN _ _ 0 = return B.empty
defaultReadN s ref n = do
    mbs <- readIORef ref
    writeIORef ref Nothing
    case mbs of
        Nothing -> do
            bs <- N.recv s n
            if B.null bs
                then return B.empty
                else
                    if B.length bs == n
                        then return bs
                        else loop bs
        Just bs
            | B.length bs == n -> return bs
            | B.length bs > n -> do
                let (bs0, bs1) = B.splitAt n bs
                writeIORef ref (Just bs1)
                return bs0
            | otherwise -> loop bs
  where
    loop bs = do
        let n' = n - B.length bs
        bs1 <- N.recv s n'
        if B.null bs1
            then return B.empty
            else do
                let bs2 = bs `B.append` bs1
                if B.length bs2 == n
                    then return bs2
                    else loop bs2
