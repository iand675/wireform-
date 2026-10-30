{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Internal core of 'Kafka.Network.Connection'.

Holds the 'Connection' record and the 'connectionGet' \/
'connectionPut' \/ 'connectionClose' primitives, split out so
'Kafka.Network.Auth.SASL' can take a 'Connection' as input
without inducing a module cycle with the manager / config /
offload bits in 'Kafka.Network.Connection'.
-}
module Kafka.Network.Connection.Internal (
  Connection (..),
  connectionGet,
  connectionPut,
  connectionPutBuilder,
  connectionClose,
) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException, throwIO, try)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.IORef
import Data.Word (Word64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import Network.Socket qualified as NS
import Wireform.Builder qualified as WB
import Wireform.Network (DuplexTransport (..), closeDuplexTransport)
import Wireform.Network.TLS.OpenSSL (SslConn, SslCtx, freeConn, freeCtx)
import Wireform.Transport.Receive (
  ReceiveWait (..),
  receiveAdvanceTail,
  receiveLoadHead,
  receiveRingBase,
  receiveRingMask,
  receiveWaitData,
 )
import Wireform.Transport.Send (sendBuilderDirect, sendByteString)


-- | A live Kafka broker connection.
data Connection = Connection
  { connDuplex :: !DuplexTransport
  , connSocket :: !NS.Socket
  , connSslConn :: !(Maybe SslConn)
  , connCtx :: !(Maybe SslCtx)
  , connCursor :: !(IORef Word64)
  , connClosed :: !(IORef Bool)
  , connRequestLock :: !(MVar ())
  {- ^ Held across one request's send and its response's receive
  ('Kafka.Client.Internal.Request.sendRequestReceiveResponse'), so
  callers sharing the connection never read each other's responses.
  -}
  }


connectionGet :: Connection -> Int -> IO ByteString
connectionGet conn n
  | n <= 0 = pure BS.empty
  | otherwise = do
      let rx = duplexReceive (connDuplex conn)
      pos <- readIORef (connCursor conn)
      h0 <- receiveLoadHead rx
      h <-
        if h0 > pos
          then pure h0
          else do
            r <- receiveWaitData rx pos
            case r of
              ReceiveMoreData h' -> pure h'
              ReceiveEndOfInput -> pure pos
              ReceiveFailed e -> throwIO e
      if h <= pos
        then pure BS.empty
        else do
          let !want = min n (fromIntegral (h - pos))
              !off = fromIntegral pos .&. receiveRingMask rx
              !ptr = receiveRingBase rx `plusPtr` off
          bs <- BSI.create want $ \dst -> copyBytes dst ptr want
          let !newPos = pos + fromIntegral want
          writeIORef (connCursor conn) newPos
          receiveAdvanceTail rx newPos
          pure bs


connectionPut :: Connection -> ByteString -> IO ()
connectionPut conn = sendByteString (duplexSend (connDuplex conn))


connectionPutBuilder :: Connection -> WB.Builder -> IO ()
connectionPutBuilder conn b =
  sendBuilderDirect (duplexSend (connDuplex conn)) b


connectionClose :: Connection -> IO ()
connectionClose conn = do
  was <- atomicModifyIORef' (connClosed conn) (\c -> (True, c))
  if was
    then pure ()
    else do
      _ <- try @SomeException (closeDuplexTransport (connDuplex conn))
      case connSslConn conn of
        Just s -> do
          _ <- try @SomeException (freeConn s)
          pure ()
        Nothing -> pure ()
      case connCtx conn of
        Just c -> do
          _ <- try @SomeException (freeCtx c)
          pure ()
        Nothing -> pure ()
      _ <- try @SomeException (NS.close (connSocket conn))
      pure ()
