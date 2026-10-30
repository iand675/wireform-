{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Client.Internal.Request
Description : Internal request/response handling utilities
Copyright   : (c) 2025
License     : BSD-3-Clause

Internal utilities for sending Kafka protocol requests and receiving responses.
This module provides low-level primitives for the higher-level client APIs.
-}
module Kafka.Client.Internal.Request (
  -- * Request/Response Operations
  sendRequestReceiveResponse,
  sendRequestReceiveResponseLocked,
  sendRawRequest,
  receiveRawResponse,

  -- * Frame Construction
  frameRequest,
  frameRequestBuilder,
  parseResponseFrame,

  -- * Header version selection (exposed for tests + the

  -- Pipeline module)
  requestHeaderVersionFor,
  responseHeaderVersionFor,
) where

import Control.Monad (when)
import Data.Bits (shiftL, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int16, Int32)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.IO (unsafePerformIO)
import Control.Concurrent.MVar (withMVar)
import Kafka.Network.Connection.Internal (Connection, connectionGet, connectionPut)
import Kafka.Network.Connection.Internal qualified as I
import Wireform.Builder qualified as WB
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RequestHeader qualified as RH
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire qualified as W
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


{- | Frame a request with its length prefix and header.

Kafka wire protocol format:
[4 bytes: message length] [header] [request body]

The message length is the size of header + request body.
-}
frameRequest
  :: Int16
  -- ^ API key
  -> Int16
  -- ^ API version
  -> Int32
  -- ^ Correlation ID
  -> P.KafkaString
  -- ^ Client ID
  -> ByteString
  -- ^ Serialized request body
  -> ByteString
frameRequest apiKey apiVersion correlationId clientId requestBody =
  let
    !header =
      RH.RequestHeader
        { RH.requestHeaderRequestApiKey = apiKey
        , RH.requestHeaderRequestApiVersion = apiVersion
        , RH.requestHeaderCorrelationId = correlationId
        , RH.requestHeaderClientId = clientId
        }
    -- The Kafka request header has its own version: v0 (for the
    -- ApiVersions request, which deliberately stays at v0 so the
    -- broker can negotiate from any client); v1 (no tagged fields,
    -- matches every non-flexible API); v2 (flexible — adds an
    -- empty TaggedFields trailer). The choice is determined by
    -- whether the API key + version pair has flexible-version
    -- support; sending a v1 header for a flexible body makes the
    -- broker read garbage off the wire and close the connection
    -- with @InvalidRequestException@ + BufferUnderflow.
    !headerVersion = requestHeaderVersionFor apiKey apiVersion
    !headerBytes = WC.runEncodeVer @RH.RequestHeader headerVersion header
    !headerLen = BS.length headerBytes
    !bodyLen = BS.length requestBody
    !messageSize = headerLen + bodyLen
    !totalLen = 4 + messageSize
  in
    -- Single-allocation framing: malloc one buffer of the exact
    -- final size, write the 4-byte size prefix + header + body
    -- directly into it. The previous shape did
    -- 'sizeBytes <> headerBytes <> requestBody', which is two
    -- ByteString '<>' calls = two full memcpies of the (possibly
    -- ~1 MiB) request body. For a high-throughput producer that
    -- doubled the per-request memory bandwidth.
    unsafePerformIO $ do
      fp <- mallocForeignPtrBytes totalLen
      withForeignPtr fp $ \basePtr -> do
        _ <- W.pokeInt32BE basePtr (fromIntegral messageSize :: Int32)
        let !hdrPtr = basePtr `plusPtr` 4
        _ <- W.pokeByteString hdrPtr headerBytes
        let !bodyPtr = hdrPtr `plusPtr` headerLen
        _ <- W.pokeByteString bodyPtr requestBody
        pure ()
      pure (BSI.fromForeignPtr fp 0 totalLen)


{- | Like 'frameRequest' but produces a 'WB.Builder' instead of
materialising a 'ByteString'. When sent via
'Kafka.Network.Transport.transportWriteBuilder', the 4-byte size
prefix, header, and body are streamed directly to the socket
without an intermediate copy.

The @requestBody@ is still a 'ByteString' (it comes from
'WC.runEncodeVer' which is direct-poke). This function avoids the
final copy that 'frameRequest' performs when assembling the frame.
-}
frameRequestBuilder
  :: Int16
  -- ^ API key
  -> Int16
  -- ^ API version
  -> Int32
  -- ^ Correlation ID
  -> P.KafkaString
  -- ^ Client ID
  -> ByteString
  -- ^ Serialized request body
  -> WB.Builder
frameRequestBuilder apiKey apiVersion correlationId clientId requestBody =
  let
    !header =
      RH.RequestHeader
        { RH.requestHeaderRequestApiKey = apiKey
        , RH.requestHeaderRequestApiVersion = apiVersion
        , RH.requestHeaderCorrelationId = correlationId
        , RH.requestHeaderClientId = clientId
        }
    !headerVersion = requestHeaderVersionFor apiKey apiVersion
    !headerBytes = WC.runEncodeVer @RH.RequestHeader headerVersion header
    !messageSize = BS.length headerBytes + BS.length requestBody
  in
    WB.int32BE (fromIntegral messageSize)
      <> WB.byteStringCopy headerBytes
      <> WB.byteString requestBody


{- | Pick the right Kafka request-header version for a given API
key + body version. Encodes the per-API flexible-version
thresholds upstream sets in the JSON message defs.

Anything in the table at body version >= the flexible-from
threshold uses header v2 (which adds an empty 'TaggedFields'
trailer); everything else uses header v1.

== ApiVersions special case

'ApiVersions' itself (api key 18) is /also/ in
'flexibleVersionTable' (it goes flexible at body v3); we
intentionally treat it like every other API. The Kafka
broker's parsing has a small concession in the other
direction — when the broker can't decode an ApiVersions
request body it falls back to assuming v0 of the body
/with header v1/ — but the request header version we /send/
still tracks the flexible-versions table.

(Earlier we returned v0 here, on the theory that older
brokers might want a v0 header; but our codegen doesn't
emit a v0 header encoder — the spec says header v0 is for
pre-Kafka-0.10 ControlledShutdown only — so v0 was never
a viable choice for any client request.)
-}
requestHeaderVersionFor :: Int16 -> Int16 -> Int16
requestHeaderVersionFor apiKey apiVersion =
  case lookup apiKey flexibleVersionTable of
    Nothing -> 1 -- non-flexible API
    Just t -> if apiVersion >= t then 2 else 1


{- | Pick the right Kafka /response/-header version for a given
API key + body version. Symmetric to 'requestHeaderVersionFor'
but needed because the response framing is asymmetric: flexible
responses carry the response header /v1/, which adds a
'TaggedFields' trailer between the correlation id and the body
proper. Skipping that trailer is what 'parseResponseFrame' uses
this for.

== ApiVersions special case

The 'ApiVersionsResponse' is the one exception that has bitten
everyone who ports a Kafka client: the broker /always/ sends
it with response header v0, even when the body is the flexible
v3+ shape. The JVM client documents this as a workaround for
the chicken-and-egg problem — the broker can't know which
header version the client expects until /after/ it has parsed
the @ApiVersionsRequest@ and consulted the negotiated set, by
which point the response is already going out. Mirror that
here so we don't try to consume a non-existent
'TaggedFields' trailer on the @ApiVersionsResponse@ for
v3+ bodies.

(Bug we found before this fix: with negotiation pushing
@DescribeConfigs@ up to v4 — flexible — every call returned
an empty @results@ array because the parser was reading the
v1-header tagged-fields byte (0x00) as the high byte of
@throttle_time_ms@, then four more bytes from what was
actually the throttle field, and finally interpreting the
next byte (the actual results-array compact-length prefix) as
the start of the first result. The single 0x00 of the empty
tagged-fields byte was enough to derail every subsequent
field decode.)
-}
responseHeaderVersionFor :: Int16 -> Int16 -> Int16
responseHeaderVersionFor apiKey apiVersion = case apiKey of
  18 -> 0 -- ApiVersions: always response-header v0
  _ -> case lookup apiKey flexibleVersionTable of
    Nothing -> 0 -- non-flexible API
    Just t -> if apiVersion >= t then 1 else 0


{- | Per-API flexible-from version (i.e. the lowest body version
that uses the v2 request header). Sourced from the upstream
@data/messages/*Request.json@ "flexibleVersions" field.

This is a denormalised mirror of what the generated
@messageFlexibleVersion@ on each request type carries; we keep
a simple Int -> Int table here so the framing layer doesn't
have to reach back into 'KafkaMessage' for every send.
-}
flexibleVersionTable :: [(Int16, Int16)]
flexibleVersionTable =
  -- Mechanically derived from
  -- @data/kafka-protocol-schemas/*Request.json@ via the
  -- @"flexibleVersions": "N+"@ field. Re-derive when bumping the
  -- vendored schemas; until then, do not hand-edit.
  [ (0, 9) -- Produce
  , (1, 12) -- Fetch
  , (2, 6) -- ListOffsets
  , (3, 9) -- Metadata
  , (8, 8) -- OffsetCommit
  , (9, 6) -- OffsetFetch
  , (10, 3) -- FindCoordinator (was 4 — wrong; broker rejected v3 with header v1)
  , (11, 6) -- JoinGroup
  , (12, 4) -- Heartbeat
  , (13, 4) -- LeaveGroup
  , (14, 4) -- SyncGroup
  , (15, 5) -- DescribeGroups
  , (16, 3) -- ListGroups
  -- ApiVersions: flexible at body v3+ on the wire, but the
  -- response is /always/ emitted with header v0; the
  -- responseHeaderVersionFor function below has the special case.
  , (18, 3) -- ApiVersions
  , (19, 5) -- CreateTopics
  , (20, 4) -- DeleteTopics
  , (21, 2) -- DeleteRecords
  , (22, 2) -- InitProducerId
  , (23, 4) -- OffsetForLeaderEpoch
  , (24, 3) -- AddPartitionsToTxn
  , (25, 3) -- AddOffsetsToTxn
  , (26, 3) -- EndTxn
  , (27, 1) -- WriteTxnMarkers
  , (28, 3) -- TxnOffsetCommit
  , (29, 2) -- DescribeAcls
  , (30, 2) -- CreateAcls
  , (31, 2) -- DeleteAcls
  , (32, 4) -- DescribeConfigs
  , (33, 2) -- AlterConfigs
  , (34, 2) -- AlterReplicaLogDirs
  , (35, 2) -- DescribeLogDirs
  , (36, 2) -- SaslAuthenticate (added — was missing; v2+ body paired with header v1 was malformed)
  , (37, 2) -- CreatePartitions
  , (38, 2) -- CreateDelegationToken
  , (39, 2) -- RenewDelegationToken
  , (40, 2) -- ExpireDelegationToken
  , (41, 2) -- DescribeDelegationToken
  , (42, 2) -- DeleteGroups
  , (43, 2) -- ElectLeaders
  , (44, 1) -- IncrementalAlterConfigs
  , (45, 0) -- AlterPartitionReassignments
  , (46, 0) -- ListPartitionReassignments
  , (47, 0) -- OffsetDelete (never went flexible per upstream; kept here for explicitness)
  , (48, 1) -- DescribeClientQuotas
  , (49, 1) -- AlterClientQuotas
  , (50, 0) -- DescribeUserScramCredentials
  , (51, 0) -- AlterUserScramCredentials
  , (52, 0) -- Vote
  , (53, 1) -- BeginQuorumEpoch
  , (54, 1) -- EndQuorumEpoch
  , (55, 0) -- DescribeQuorum
  , (56, 0) -- AlterPartition
  , (57, 0) -- UpdateFeatures
  , (58, 0) -- Envelope
  , (59, 0) -- FetchSnapshot
  , (60, 0) -- DescribeCluster
  , (61, 0) -- DescribeProducers
  , (62, 0) -- BrokerRegistration
  , (63, 0) -- BrokerHeartbeat
  , (64, 0) -- UnregisterBroker
  , (65, 0) -- DescribeTransactions
  , (66, 0) -- ListTransactions
  , (67, 0) -- AllocateProducerIds
  , (68, 0) -- ConsumerGroupHeartbeat
  , (69, 0) -- ConsumerGroupDescribe
  , (70, 0) -- ControllerRegistration
  , (71, 0) -- GetTelemetrySubscriptions
  , (72, 0) -- PushTelemetry
  , (73, 0) -- AssignReplicasToDirs
  , (74, 0) -- ListClientMetricsResources
  , (75, 0) -- DescribeTopicPartitions
  ]


{- | Parse a response frame, extracting the correlation ID and
response body.

Kafka response format:

@
[4 bytes: message length] [response header] [response body]
@

The response header is /version-aware/: non-flexible APIs use
header v0 (just the 4-byte correlation id); flexible APIs use
header v1 (correlation id + a 'TaggedFields' trailer).
'ApiVersionsResponse' is a special case — the broker always
sends it with header v0 regardless of body version, so we
mirror that here too. See 'responseHeaderVersionFor'.

Skipping the right number of header bytes is mandatory for
flexible-bodied responses: leaving the v1-header tagged-fields
trailer attached to @responseBody@ shifts every subsequent
field by one byte and corrupts the entire decode.
-}
parseResponseFrame
  :: Int16
  -- ^ API key
  -> Int16
  -- ^ API version
  -> ByteString
  -- ^ raw bytes (including the 4-byte size prefix)
  -> Either String (Int32, ByteString)
parseResponseFrame apiKey apiVersion bs = do
  when (BS.length bs < 4) $
    Left "Response too short: missing size prefix"

  -- 4-byte big-endian Int32 size prefix. Wire's 'readInt32BE'
  -- is a direct ForeignPtr peek; the previous shape went through
  -- 'runGetS deserialize' (Builder + parser-monad) for a single
  -- 4-byte read.
  messageSize <- W.readInt32BE (BS.take 4 bs)

  let remainingBytes = BS.drop 4 bs
  when (BS.length remainingBytes < fromIntegral (messageSize :: Int32)) $
    Left $
      "Response too short: expected "
        ++ show messageSize
        ++ " bytes, got "
        ++ show (BS.length remainingBytes)

  let messageBytes = BS.take (fromIntegral messageSize) remainingBytes

  -- Always at least the correlation id.
  when (BS.length messageBytes < 4) $
    Left "Response message too short: missing correlation ID"

  correlationId <- W.readInt32BE (BS.take 4 messageBytes)

  -- Response header v1 (flexible APIs) carries an extra
  -- 'TaggedFields' field after the correlation id. The
  -- canonical empty-tagged-fields encoding is a single 0x00
  -- byte (UVarInt 0); we skip exactly the bytes of whatever
  -- TaggedFields blob the broker sent, even if non-empty.
  let !headerVersion = responseHeaderVersionFor apiKey apiVersion
  if headerVersion == 0
    then return (correlationId, BS.drop 4 messageBytes)
    else do
      let !afterCid = BS.drop 4 messageBytes
      taggedLen <- consumeTaggedFieldsLen afterCid
      let !body = BS.drop taggedLen afterCid
      return (correlationId, body)
  where
    -- Decode the leading 'TaggedFields' value off @bs0@ and
    -- return how many bytes it occupied. We don't actually
    -- look inside any tagged field; the broker's response
    -- headers historically contain only the empty-tagged-fields
    -- placeholder (a single 0x00) but the wire format does
    -- allow non-empty trailers, so be defensive.
    consumeTaggedFieldsLen
      :: ByteString -> Either String Int
    consumeTaggedFieldsLen bs0 = do
      when (BS.null bs0) $
        Left "Response header v1: missing tagged-fields trailer"
      -- TaggedFields = UVarInt count + count * (UVarInt tag, UVarInt len, len bytes)
      (count, after1) <- decodeUVarInt bs0
      walkFields (fromIntegral count) after1 (BS.length bs0 - BS.length after1)

    walkFields
      :: Int
      -- \^ remaining count
      -> ByteString
      -- \^ slice past the count
      -> Int
      -- \^ bytes consumed so far
      -> Either String Int
    walkFields 0 _ acc = Right acc
    walkFields !n rest acc = do
      (_tag, r1) <- decodeUVarInt rest
      (sz, r2) <- decodeUVarInt r1
      let !szI = fromIntegral sz
      when (BS.length r2 < szI) $
        Left "Response header tagged field: payload shorter than declared length"
      let !consumedThisField = (BS.length rest - BS.length r2) + szI
      walkFields (n - 1) (BS.drop szI r2) (acc + consumedThisField)

    -- Inline UVarInt decoder so we don't pull in the Wire
    -- module here (the Wire module is for the hot record-batch
    -- path; this is one-shot per response).
    decodeUVarInt :: ByteString -> Either String (Int, ByteString)
    decodeUVarInt = go 0 0
      where
        go !shift !acc bs0
          | shift > 28 = Left "Response header tagged field: UVarInt > 5 bytes"
          | BS.null bs0 = Left "Response header tagged field: truncated UVarInt"
          | otherwise =
              let !b = BS.head bs0
                  !tail0 = BS.tail bs0
                  !v = acc + (fromIntegral (b .&. 0x7F) `shiftL` shift)
              in if b .&. 0x80 == 0
                   then Right (v, tail0)
                   else go (shift + 7) v tail0


-- | Send a raw framed request to the connection.
sendRawRequest :: Connection -> ByteString -> IO ()
sendRawRequest conn framedRequest = do
  connectionPut conn framedRequest


{- | Read exactly @n@ bytes from the connection into a single
pre-allocated buffer.

The previous shape did @acc <> chunk@ on every iteration of
the read loop: for an N-chunk read of an M-byte response that
was O(N*M) bytes copied (each '<>' on a strict ByteString is
a full memcpy). Allocate one buffer up front, then memcpy
each chunk straight into it at the correct offset — total
copy cost is just the one pass over the response.
-}
readExactly :: Connection -> Int -> IO ByteString
readExactly _ n | n <= 0 = pure BS.empty
readExactly conn !n = do
  fp <- mallocForeignPtrBytes n
  withForeignPtr fp $ \basePtr -> do
    let go !off !emptyReads
          | off >= n = pure ()
          | emptyReads >= 3 =
              fail
                ( "Connection appears closed: received "
                    ++ show emptyReads
                    ++ " consecutive empty reads"
                )
          | otherwise = do
              let !want = n - off
              chunk <- connectionGet conn want
              let !got = BS.length chunk
              if got == 0
                then go off (emptyReads + 1)
                else do
                  -- Copy the chunk directly into our buffer at
                  -- the right offset; chunk's source storage
                  -- can be GC'd as soon as we return.
                  _ <- W.pokeByteString (basePtr `plusPtr` off) chunk
                  go (off + got) 0
    go 0 0
  pure (BSI.fromForeignPtr fp 0 n)


{- | Receive a raw framed response from the connection.
Reads the 4-byte size prefix first, then reads exactly that
many bytes into a /single/ buffer big enough to hold both the
size prefix and the body; saves the previous shape's final
'sizeBytes <> messageBytes' concat.
-}
receiveRawResponse :: Connection -> IO ByteString
receiveRawResponse conn = do
  -- Read the 4-byte size prefix
  sizeBytes <- readExactly conn 4
  messageSize <- case W.readInt32BE sizeBytes of
    Left err -> fail $ "Failed to parse response size: " ++ err
    Right size -> return (size :: Int32)
  let !msgLen = fromIntegral messageSize :: Int
  -- Allocate a single buffer holding [size prefix | message body],
  -- then read the body straight into it at offset 4.
  fp <- mallocForeignPtrBytes (4 + msgLen)
  withForeignPtr fp $ \basePtr -> do
    _ <- W.pokeByteString basePtr sizeBytes
    let !bodyPtr = basePtr `plusPtr` 4
        readBody !off !emptyReads
          | off >= msgLen = pure ()
          | emptyReads >= 3 =
              fail ("Connection appears closed during response read")
          | otherwise = do
              chunk <- connectionGet conn (msgLen - off)
              let !got = BS.length chunk
              if got == 0
                then readBody off (emptyReads + 1)
                else do
                  _ <- W.pokeByteString (bodyPtr `plusPtr` off) chunk
                  readBody (off + got) 0
    readBody 0 0
  pure (BSI.fromForeignPtr fp 0 (4 + msgLen))


{- | Send a request and receive the corresponding response.

/Not thread-safe/. 'Network.Connection.Connection' has no
internal synchronization; if multiple threads can target the
same broker (the consumer's heartbeat thread + its poll /
commit path is the canonical case) call
'sendRequestReceiveResponseLocked' instead. Single-threaded
callers (admin client, producer pipeline, SASL handshake at
connection setup, ...) can keep using this entry point.
-}
sendRequestReceiveResponse
  :: Connection
  -> Int16
  -- ^ API key
  -> Int16
  -- ^ API version
  -> Int32
  -- ^ Correlation ID
  -> P.KafkaString
  -- ^ Client ID
  -> ByteString
  -- ^ Serialized request body
  -> IO (Either String (Int32, ByteString))
sendRequestReceiveResponse conn apiKey apiVersion correlationId clientId requestBody = do
  let framedRequest = frameRequest apiKey apiVersion correlationId clientId requestBody
  -- One request in flight per shared connection: whoever sends
  -- reads the next response, so the pair must not interleave with
  -- another caller's.
  response <- withMVar (I.connRequestLock conn) $ \_ -> do
    sendRawRequest conn framedRequest
    receiveRawResponse conn
  return $ parseResponseFrame apiKey apiVersion response


{- | Locked sibling of 'sendRequestReceiveResponse'.  The first
argument is a "run under the per-broker connection lock"
combinator (typically @Kafka.Network.Connection.withBrokerLock
cm addr@); it serializes the send + receive pair against any
other locked caller targeting the same broker.  Callers that
may race against each other on the /same broker/ (the
consumer's heartbeat thread vs. the foreground subscribe /
commit / poll path is the canonical case on a single-broker
cluster, where the coordinator and the data leader live on
the same socket) must use this variant.

The lock combinator is passed in (rather than imported) so
this module does not depend on the connection-manager module,
which would create an import cycle through SASL.
-}
sendRequestReceiveResponseLocked
  :: ( IO (Either String (Int32, ByteString))
       -> IO (Either String (Int32, ByteString))
     )
  -- ^ @withBrokerLock cm addr@
  -> Connection
  -> Int16
  -- ^ API key
  -> Int16
  -- ^ API version
  -> Int32
  -- ^ Correlation ID
  -> P.KafkaString
  -- ^ Client ID
  -> ByteString
  -- ^ Serialized request body
  -> IO (Either String (Int32, ByteString))
sendRequestReceiveResponseLocked withLock conn apiKey apiVersion correlationId clientId requestBody =
  withLock (sendRequestReceiveResponse conn apiKey apiVersion correlationId clientId requestBody)
