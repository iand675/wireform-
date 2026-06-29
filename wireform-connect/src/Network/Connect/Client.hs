{-# LANGUAGE OverloadedStrings #-}

-- | Connect client: issue Connect RPCs over a wireform-http 'Connection'.
module Network.Connect.Client
  ( -- * Configuration
    ConnectClientConfig (..),
    defaultConnectClientConfig,

    -- * Client
    ConnectClient (..),
    withConnectClient,

    -- * Calls
    nonStreaming,
    nonStreamingGet,
    serverStreaming,
    clientStreaming,
    biDiStreaming,

    -- * Re-exports
    Connection.Connection,
    Connection.ConnectionConfig (..),
    Connection.defaultConnectionConfig,
    Connection.TlsConnectionConfig (..),
    Connection.defaultTlsConnectionConfig,
  )
where

import Control.Concurrent.STM
  ( TQueue
  , atomically
  , newTQueueIO
  , readTQueue
  , writeTQueue
  )
import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Network.Connect.Codec (decodeOutputBody, encodeInputBody)
import Network.Connect.Error
  ( ConnectError (..)
  , ConnectException (..)
  , decodeConnectError
  , httpStatusToConnectCode
  )
import Network.Connect.Compression qualified as CC
import Network.Connect.Envelope
  ( EndStreamResponse (esError)
  , EnvelopeFlags (..)
  , FrameReader
  , buildFrameLazy
  , decodeEndStream
  , newFrameReader
  , readFrame
  )
import Network.Connect.Metadata (leadingToHeaders)
import Network.Connect.Protocol
  ( Codec (..)
  , connectGetVersion
  , connectProtocolVersion
  , hConnectAcceptEncoding
  , hConnectContentEncoding
  , hConnectTimeoutMs
  , qpBase64
  , qpCompression
  , qpConnect
  , qpEncoding
  , qpMessage
  , streamContentType
  , unaryContentType
  )
import Network.GRPC.Spec
  ( CustomMetadata
  , GrpcError (..)
  , HasStreamingType (..)
  , Input
  , IsRPC (..)
  , Output
  , StreamingType (..)
  , SupportsClientRpc
  )
import Network.HTTP.Connection qualified as Connection
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.PercentEncoding (renderQueryString)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header
  ( Headers
  , hAcceptEncoding
  , hContentEncoding
  , hContentType
  , insertHeader
  , lookupHeader
  )
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.Status (Status, pattern Status)
import Network.HTTP.Types.Version (pattern HTTP1_1)

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

data ConnectClientConfig = ConnectClientConfig
  { cccCodec :: !Codec
  , cccRequestCompression :: !CC.ContentCoding
  , cccAcceptCompression :: ![CC.ContentCoding]
  , cccTimeoutMs :: !(Maybe Int)
  , cccMetadata :: ![CustomMetadata]
  , cccSendProtocolVersion :: !Bool
  }

defaultConnectClientConfig :: ConnectClientConfig
defaultConnectClientConfig =
  ConnectClientConfig
    { cccCodec = CodecProto
    , cccRequestCompression = CC.Identity
    , cccAcceptCompression = [CC.Identity, CC.Gzip]
    , cccTimeoutMs = Nothing
    , cccMetadata = []
    , cccSendProtocolVersion = True
    }

------------------------------------------------------------------------
-- Client
------------------------------------------------------------------------

data ConnectClient = ConnectClient
  { clConn :: !Connection.Connection
  , clConfig :: !ConnectClientConfig
  , clAuthority :: !ByteString
  , clScheme :: !Scheme
  }

withConnectClient
  :: ConnectClientConfig
  -> Connection.ConnectionConfig
  -> (ConnectClient -> IO a)
  -> IO a
withConnectClient cfg connCfg action =
  Connection.withConnection connCfg $ \conn -> do
    let host = bsPack (Connection.connectionHost connCfg)
        scheme = case Connection.connectionTls connCfg of
          Just _ -> SchemeHttps
          Nothing -> SchemeHttp
    action (ConnectClient conn cfg host scheme)

------------------------------------------------------------------------
-- Unary
------------------------------------------------------------------------

nonStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (Output rpc)
nonStreaming cl p input = do
  let cfg = clConfig cl
      codec = cccCodec cfg
      raw = encodeInputBody codec p input
      (bodyBytes, _coding) = compressUnary (cccRequestCompression cfg) raw
      path = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = path
          , requestHeaders = unaryHeaders cl cfg codec
          , requestBody = BodyBytes bodyBytes
          }
  resp <- Connection.sendOn (clConn cl) req
  readUnaryResponse codec p resp

nonStreamingGet
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (Output rpc)
nonStreamingGet cl p input = do
  let cfg = clConfig cl
      codec = cccCodec cfg
      raw = encodeInputBody codec p input
      isBinary = codec == CodecProto || cccRequestCompression cfg /= CC.Identity
      (payload, _coding) = compressUnary (cccRequestCompression cfg) raw
      msgParam
        | isBinary = B64U.encodeUnpadded payload
        | otherwise = payload
      query =
        renderQueryString
          [ (qpConnect, connectGetVersion)
          , (qpEncoding, codecToken codec)
          , (qpBase64, if isBinary then "1" else "0")
          , (qpCompression, CC.codingName (cccRequestCompression cfg))
          , (qpMessage, msgParam)
          ]
      path = "/" <> rpcServiceName p <> "/" <> rpcMethodName p <> "?" <> query
      req =
        (baseRequest cl)
          { requestMethod = Method.mGet
          , requestTarget = path
          , requestHeaders = leadingToHeaders (cccMetadata cfg)
          , requestBody = BodyEmpty
          }
  resp <- Connection.sendOn (clConn cl) req
  readUnaryResponse codec p resp

readUnaryResponse
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> Response
  -> IO (Output rpc)
readUnaryResponse codec p resp = do
  let status = responseStatus resp
      headers = responseHeaders resp
  bodyBytes <- drainBody (responseBody resp)
  if is2xx status
    then do
      let mRespCoding = lookupHeader hContentEncoding headers >>= CC.codingFromName
      plain <- decompressResp bodyBytes mRespCoding
      case decodeOutputBody codec p plain of
        Left e ->
          throwConnectIO $
            ConnectError
              { ceCode = GrpcInternal
              , ceMessage = Just (T.pack ("connect: could not decode response: " <> e))
              , ceDetails = []
              }
        Right out -> pure out
    else do
      let err =
            case Aeson.decode (BL.fromStrict bodyBytes) of
              Just v -> case decodeConnectError v of
                Right ce -> ce
                Left _ -> inferError status
              Nothing -> inferError status
      throwIO (ConnectException err)

------------------------------------------------------------------------
-- Server streaming
------------------------------------------------------------------------

serverStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ServerStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> (IO (Maybe (Output rpc)) -> IO r)
  -> IO r
serverStreaming cl p input recvAction = do
  let cfg = clConfig cl
      codec = cccCodec cfg
      body = singleInputFrame codec p (cccRequestCompression cfg) input
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg (cccRequestCompression cfg)
          , requestBody = BodyBytes body
          }
  Connection.withResponseOn (clConn cl) req $ \resp -> do
    fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
    recvAction (readDataFrame codec p cfg fr)

------------------------------------------------------------------------
-- Client streaming
------------------------------------------------------------------------

clientStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ClientStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> ((Input rpc -> IO ()) -> IO ())
  -> IO (Output rpc)
clientStreaming cl p sendAction = do
  outQ <- newTQueueIO :: IO (TQueue (Maybe ByteString))
  let cfg = clConfig cl
      codec = cccCodec cfg
      reqCoding = cccRequestCompression cfg
      send msg = atomically (writeTQueue outQ (Just (inputFrame codec p reqCoding msg)))
  -- Pre-fill the request frames so the body producer never blocks (works on
  -- HTTP\/1.1, which sends the full request body before reading the response).
  sendAction send
  atomically (writeTQueue outQ Nothing)
  let producer =
        atomically $
          readTQueue outQ >>= \case
            Just fr -> pure (Just fr)
            Nothing -> pure Nothing
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg reqCoding
          , requestBody = BodyStream producer
          }
  Connection.withResponseOn (clConn cl) req $ \resp -> do
    fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
    readClientStreamOutput codec p cfg fr

------------------------------------------------------------------------
-- Bidirectional streaming
------------------------------------------------------------------------

biDiStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'BiDiStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> ((Input rpc -> IO ()) -> IO (Maybe (Output rpc)) -> IO r)
  -> IO r
biDiStreaming cl p action = do
  outQ <- newTQueueIO :: IO (TQueue (Maybe ByteString))
  let cfg = clConfig cl
      codec = cccCodec cfg
      reqCoding = cccRequestCompression cfg
      send msg = atomically (writeTQueue outQ (Just (inputFrame codec p reqCoding msg)))
      producer =
        atomically $
          readTQueue outQ >>= \case
            Just fr -> pure (Just fr)
            Nothing -> pure Nothing
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg reqCoding
          , requestBody = BodyStream producer
          }
  Connection.withResponseOn (clConn cl) req $ \resp -> do
    fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
    r <- action send (readDataFrame codec p cfg fr)
    atomically (writeTQueue outQ Nothing)
    pure r

------------------------------------------------------------------------
-- Request building
------------------------------------------------------------------------

baseRequest :: ConnectClient -> Request
baseRequest cl =
  Request
    { requestMethod = Method.mPost
    , requestTarget = "/"
    , requestAuthority = Just (clAuthority cl)
    , requestScheme = clScheme cl
    , requestHeaders = []
    , requestBody = BodyEmpty
    , requestVersion = HTTP1_1
    , requestTrailers = pure []
    }

unaryHeaders :: ConnectClient -> ConnectClientConfig -> Codec -> Headers
unaryHeaders _cl cfg codec =
  let h0 = leadingToHeaders (cccMetadata cfg) <> [(hContentType, unaryContentType codec)]
      h1 = case cccRequestCompression cfg of
        CC.Identity -> h0
        c -> insertHeader hContentEncoding (CC.codingName c) h0
      h2 = case cccAcceptCompression cfg of
        [] -> h1
        cs -> insertHeader hAcceptEncoding (renderAccept cs) h1
      h3 = case cccTimeoutMs cfg of
        Nothing -> h2
        Just ms -> insertHeader hConnectTimeoutMs (bsPack (show ms)) h2
      h4 =
        if cccSendProtocolVersion cfg
          then insertHeader "connect-protocol-version" connectProtocolVersion h3
          else h3
   in h4

streamingHeaders :: ConnectClient -> ConnectClientConfig -> CC.ContentCoding -> Headers
streamingHeaders _cl cfg reqCoding =
  let codec = cccCodec cfg
      baseHeaders =
        leadingToHeaders (cccMetadata cfg)
          <> [(hContentType, streamContentType codec)]
      h0 = case reqCoding of
        CC.Identity -> baseHeaders
        c -> insertHeader hConnectContentEncoding (CC.codingName c) baseHeaders
      h1 = case cccAcceptCompression cfg of
        [] -> h0
        cs -> insertHeader hConnectAcceptEncoding (renderAccept cs) h0
      h2 = case cccTimeoutMs cfg of
        Nothing -> h1
        Just ms -> insertHeader hConnectTimeoutMs (bsPack (show ms)) h1
   in h2

-- | A single enveloped input frame (for server streaming's one message).
singleInputFrame
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc))
  => Codec
  -> Proxy rpc
  -> CC.ContentCoding
  -> Input rpc
  -> ByteString
singleInputFrame codec p reqCoding input =
  let raw = encodeInputBody codec p input
      (payload, compressed) = compressUnary' reqCoding raw
      flags = EnvelopeFlags{efCompressed = compressed, efEndStream = False}
   in BL.toStrict (buildFrameLazy flags payload)

-- | A single enveloped input frame as an action (for streaming send).
inputFrame
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc))
  => Codec
  -> Proxy rpc
  -> CC.ContentCoding
  -> Input rpc
  -> ByteString
inputFrame = singleInputFrame

------------------------------------------------------------------------
-- Frame reading
------------------------------------------------------------------------

-- | Read the next output frame; @Nothing@ at the end-stream frame (throwing
-- if the end-stream carries an error).
readDataFrame
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> ConnectClientConfig
  -> FrameReader
  -> IO (Maybe (Output rpc))
readDataFrame codec p cfg fr = do
  mframe <- readFrame fr
  case mframe of
    Nothing -> pure Nothing
    Just (flags, payload)
      | efEndStream flags -> do
          checkEndStream payload
          pure Nothing
      | otherwise -> do
          plain <- decompressFrame flags payload cfg
          case decodeOutputBody codec p plain of
            Left e ->
              throwConnectIO $
                ConnectError
                  { ceCode = GrpcInternal
                  , ceMessage = Just (T.pack ("connect: could not decode response frame: " <> e))
                  , ceDetails = []
                  }
            Right out -> pure (Just out)

-- | Read the single output message of a client-streaming response, then the
-- end-stream frame.
readClientStreamOutput
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> ConnectClientConfig
  -> FrameReader
  -> IO (Output rpc)
readClientStreamOutput codec p cfg fr = do
  mframe <- readFrame fr
  case mframe of
    Nothing ->
      throwConnectIO $
        ConnectError
          { ceCode = GrpcInternal
          , ceMessage = Just "connect: client stream ended without output"
          , ceDetails = []
          }
    Just (flags, payload)
      | efEndStream flags -> do
          let esErr = case decodeEndStream payload of
                Right esr -> esError esr
                Left _ -> Nothing
          throwConnectIO $
            fromMaybe
              (ConnectError{ceCode = GrpcInternal, ceMessage = Just "connect: client stream ended without output", ceDetails = []})
              esErr
      | otherwise -> do
          plain <- decompressFrame flags payload cfg
          out <-
            case decodeOutputBody codec p plain of
              Left e ->
                throwConnectIO $
                  ConnectError
                    { ceCode = GrpcInternal
                    , ceMessage = Just (T.pack ("connect: could not decode response: " <> e))
                    , ceDetails = []
                    }
              Right x -> pure x
          -- Read and validate the end-stream frame.
          mEnd <- readFrame fr
          case mEnd of
            Just (ef, ep) | efEndStream ef -> checkEndStream ep
            _ -> pure ()
          pure out

-- | Throw if an end-stream frame carries an error.
checkEndStream :: ByteString -> IO ()
checkEndStream payload =
  case decodeEndStream payload of
    Right esr | Just ce <- esError esr -> throwIO (ConnectException ce)
    _ -> pure ()

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

compressUnary :: CC.ContentCoding -> ByteString -> (ByteString, CC.ContentCoding)
compressUnary CC.Identity bs = (bs, CC.Identity)
compressUnary c bs = (CC.compress c bs, c)

compressUnary' :: CC.ContentCoding -> ByteString -> (ByteString, Bool)
compressUnary' CC.Identity bs = (bs, False)
compressUnary' c bs = (CC.compress c bs, True)

decompressResp :: ByteString -> Maybe CC.ContentCoding -> IO ByteString
decompressResp bodyBytes mCoding = case mCoding of
  Nothing -> pure bodyBytes
  Just CC.Identity -> pure bodyBytes
  Just c
    | BS.null bodyBytes -> pure bodyBytes
    | otherwise -> do
        r <- CC.decompress c bodyBytes
        pure $ case r of
          Left _ -> bodyBytes
          Right bs -> bs

-- | Decompress a streaming response frame. The coding is taken from the
-- client's first accepted non-identity coding (sent in
-- @connect-accept-encoding@); the server's @connect-content-encoding@ header
-- isn't exposed here, so we rely on the frame's compressed bit.
decompressFrame :: EnvelopeFlags -> ByteString -> ConnectClientConfig -> IO ByteString
decompressFrame flags payload cfg =
  if efCompressed flags
    then case filter (/= CC.Identity) (cccAcceptCompression cfg) of
      (c : _) -> do
        r <- CC.decompress c payload
        pure $ case r of
          Left _ -> payload
          Right bs -> bs
      [] -> pure payload
    else pure payload

drainBody :: Body -> IO ByteString
drainBody BodyEmpty = pure BS.empty
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = go []
  where
    go acc =
      p >>= \case
        Nothing -> pure (BS.concat (reverse acc))
        Just chunk -> go (chunk : acc)

responseBodyProducer :: Body -> IO (IO (Maybe ByteString))
responseBodyProducer BodyEmpty = pure (pure Nothing)
responseBodyProducer (BodyBytes bs) = do
  r <- newIORef False
  pure
      ( do
          done <- readIORef r
          if done
            then pure Nothing
            else do
              writeIORef r True
              pure (Just bs)
      )
responseBodyProducer (BodyStream p) = pure p

renderAccept :: [CC.ContentCoding] -> ByteString
renderAccept = BS.intercalate ", " . map CC.codingName

is2xx :: Status -> Bool
is2xx (Status w) = w >= 200 && w < 300

inferError :: Status -> ConnectError
inferError status = ConnectError{ceCode = httpStatusToConnectCode status, ceMessage = Just "connect: server error", ceDetails = []}

throwConnectIO :: ConnectError -> IO a
throwConnectIO ce = throwIO (ConnectException ce)

codecToken :: Codec -> ByteString
codecToken CodecProto = "proto"
codecToken CodecJSON = "json"

bsPack :: String -> ByteString
bsPack = BS.pack . map (fromIntegral . fromEnum)
