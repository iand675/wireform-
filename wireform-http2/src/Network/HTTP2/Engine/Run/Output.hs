module Network.HTTP2.Engine.Run.Output
  ( ciHeadersToRaw
  , finishTrailers
  , sendFinalBody
  , sendFixedBody
  , runStreamingBody
  , sendRstStream
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Header qualified as HTTP
import Network.HTTP2.Connection
  ( Connection, sendEncodedHeaders, sendFrame, sendPeerData, validateTrailerBlock )
import Network.HTTP2.Engine.Types
import Network.HTTP2.Frame
import Network.HTTP2.Types (ErrorCode, FrameType (FrameRSTStream), Header, StreamId)

ciHeadersToRaw :: [HTTP.Header] -> [Header]
ciHeadersToRaw = map (\(name, value) -> (CI.foldedCase name, value))

advanceTrailers :: TrailersMaker -> ByteString -> IO TrailersMaker
advanceTrailers maker bytes = do
  next <- maker (Just bytes)
  case next of
    NextTrailersMaker continue -> pure continue
    Trailers _ -> fail "trailer maker returned fields before end of body"

finishTrailers :: TrailersMaker -> IO [Header]
finishTrailers maker = do
  result <- maker Nothing
  case result of
    NextTrailersMaker _ -> fail "trailer maker requested another chunk after end of body"
    Trailers fields -> do
      let headers = ciHeadersToRaw fields
      case validateTrailerBlock flagEndStream headers of
        Nothing -> pure headers
        Just reason -> throwIO (userError (BS8.unpack reason))

sendFinalBody :: Connection -> StreamId -> ByteString -> [Header] -> IO ()
sendFinalBody conn sid bytes trailers
  | null trailers = sendPeerData conn sid bytes True
  | otherwise = do
      unless (BS.null bytes) (sendPeerData conn sid bytes False)
      sendEncodedHeaders conn sid True trailers

sendFixedBody :: Connection -> StreamId -> BSB.Builder -> Maybe TrailersMaker -> IO ()
sendFixedBody conn sid builder maker = do
  let bytes = LBS.toStrict (BSB.toLazyByteString builder)
  trailers <- case maker of
    Nothing -> pure []
    Just compute -> finishTrailers =<< advanceTrailers compute bytes
  sendFinalBody conn sid bytes trailers

data OutputState = OutputOpen | OutputSealed | OutputClosed

-- | Keep the final chunk visible to the peer while the producer returns.
-- Trailer finalization can depend on work the producer does after its
-- final push. A successful final push makes cancellation a no-op.
runStreamingBody
  :: Connection
  -> StreamId
  -> Maybe TrailersMaker
  -> (Maybe SomeException -> ErrorCode)
  -> (SomeException -> IO ())
  -> (OutBodyIface -> IO ())
  -> IO ()
runStreamingBody conn sid initialMaker cancelCode onFailure produce = do
  state <- newMVar OutputOpen
  makerVar <- traverse newMVar initialMaker
  let writeData nextState endStream bytes = modifyMVarMasked state $ \case
        OutputOpen -> do
          when (endStream || not (BS.null bytes)) (sendPeerData conn sid bytes endStream)
          pure (nextState, True)
        current -> pure (current, False)
      push final builder = do
        let bytes = LBS.toStrict (BSB.toLazyByteString builder)
        case makerVar of
          Nothing -> void (writeData (if final then OutputClosed else OutputOpen) final bytes)
          Just makerState -> modifyMVar_ makerState $ \maker -> do
            sent <- writeData (if final then OutputSealed else OutputOpen) False bytes
            if sent then advanceTrailers maker bytes else pure maker
      cancel reason = modifyMVarMasked_ state $ \case
        OutputOpen -> sendRstStream conn sid (cancelCode reason) >> pure OutputClosed
        current -> pure current
      finish = case makerVar of
        Nothing -> void (writeData OutputClosed True BS.empty)
        Just makerState -> withMVar makerState $ \maker -> do
          pending <- modifyMVarMasked state $ \case
            OutputClosed -> pure (OutputClosed, False)
            _ -> pure (OutputSealed, True)
          when pending $ do
            trailers <- finishTrailers maker
            modifyMVarMasked_ state $ \_ ->
              sendFinalBody conn sid BS.empty trailers >> pure OutputClosed
      iface = OutBodyIface
        { outBodyUnmask = id
        , outBodyPush = push False
        , outBodyPushFinal = push True
        , outBodyCancel = cancel
        , outBodyFlush = pure ()
        }
  (produce iface >> finish) `catch` \err -> do
    pending <- modifyMVarMasked state $ \case
      OutputClosed -> pure (OutputClosed, False)
      _ -> pure (OutputClosed, True)
    when pending (onFailure err)

sendRstStream :: Connection -> StreamId -> ErrorCode -> IO ()
sendRstStream conn sid code =
  sendFrame conn (Frame (FrameHeader 4 FrameRSTStream 0 sid) (RSTStreamFrame code))
    `catch` (\(_ :: SomeException) -> pure ())
