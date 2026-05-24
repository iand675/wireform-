{- | HTTP\/1.x message encoder.

All encoders return a 'Wireform.Builder.Builder' so the caller (client
or server) can chain them with chunked-body fragments and flush them in
one pinned write.

Framing rules (RFC 9112 § 6):

  * The body framing is decided up front and matched to the headers we
    emit.
  * If 'responseBody' is 'BodyEmpty' we emit @Content-Length: 0@ unless
    the status code forbids a body (1xx, 204, 304), in which case we
    omit it.
  * If the body is a 'BodyBytes', we emit @Content-Length: n@.
  * If the body is a 'BodyStream' and we're on HTTP\/1.1, we emit
    @Transfer-Encoding: chunked@. On HTTP\/1.0 (where chunked is not
    available) we omit framing headers; the caller must close the
    connection after streaming.

We avoid double-emitting framing headers: if the application supplied
@Content-Length@ or @Transfer-Encoding@ manually we drop our generated
one and trust theirs (so e.g. a proxy can pass-through an upstream's
framing decision verbatim).
-}
module Network.HTTP1.Encode
  ( -- * Builders
    requestBuilder
  , responseBuilder
  , headersBuilder

    -- * Convenience top-level encoders
  , encodeRequestHead
  , encodeResponseHead

    -- * Pre-encoding
  , precomputeResponse
  , precomputeResponseEither

    -- * Direct decimal encoder (also useful from handlers)
  , wordDecBS

    -- * Cached Date header (refreshed every second by a tick thread)
  , cachedHttpDate

    -- * Misc
  , spaceB
  , crlfB
  ) where

import Control.Concurrent (forkIO, myThreadId, threadCapability, threadDelay)
import Control.Monad (forM_, forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Conc (getNumCapabilities)
import System.IO.Unsafe (unsafePerformIO)

import qualified Wireform.Builder as B

import Network.HTTP1.Headers
import Network.HTTP1.Internal.Ascii (asciiIeq)
import Network.HTTP1.Method
import Network.HTTP1.Status
import Network.HTTP1.Types

------------------------------------------------------------------------
-- Public encoders
------------------------------------------------------------------------

-- | Build the wire form of an outgoing request *head* (request line +
-- headers + terminating CRLFCRLF). The body, if any, is emitted by the
-- caller — either as a single 'B.byteString' append for 'BodyBytes' or
-- via 'Network.HTTP1.Chunked.encodeChunk' for streaming.
requestBuilder :: Request -> B.Builder
requestBuilder Request{requestMethod=m, requestTarget=t, requestVersion=v, requestHeaders=h, requestBody=b} =
       B.byteString (methodToBytes m)
    <> spaceB
    <> B.byteString t
    <> spaceB
    <> B.byteString (versionToBytes v)
    <> crlfB
    <> headersBuilder (augmentRequestHeaders m v h b)
    <> crlfB

-- | Build the wire form of an outgoing response head (status line +
-- headers + terminating CRLFCRLF).
--
-- For HTTP\/1.1 status codes that match one of the IANA-registered
-- common codes (200, 201, 204, 301, 302, 304, 400, 404, 500, …) we
-- emit a precomputed status line — version + code + reason phrase +
-- CRLF in one 'B.byteString' append — instead of the three-piece
-- assembly the general path uses.
responseBuilder :: Response -> B.Builder
responseBuilder Response{responseStatus=s, responseVersion=v, responseHeaders=h, responseBody=b} =
       statusLineFor v s
    <> headersBuilder (augmentResponseHeaders s v h b)
    <> crlfB

-- | Pre-baked @\"HTTP\/X.Y <code> <reason>\\r\\n\"@ for the codes the
-- average HTTP\/1.1 server emits 99 % of the time. Anything outside
-- the lookup falls back to a per-call assembly.
{-# INLINE statusLineFor #-}
statusLineFor :: Version -> Status -> B.Builder
statusLineFor HTTP_1_1 (Status 200) = B.byteString "HTTP/1.1 200 OK\r\n"
statusLineFor HTTP_1_1 (Status 201) = B.byteString "HTTP/1.1 201 Created\r\n"
statusLineFor HTTP_1_1 (Status 204) = B.byteString "HTTP/1.1 204 No Content\r\n"
statusLineFor HTTP_1_1 (Status 206) = B.byteString "HTTP/1.1 206 Partial Content\r\n"
statusLineFor HTTP_1_1 (Status 301) = B.byteString "HTTP/1.1 301 Moved Permanently\r\n"
statusLineFor HTTP_1_1 (Status 302) = B.byteString "HTTP/1.1 302 Found\r\n"
statusLineFor HTTP_1_1 (Status 303) = B.byteString "HTTP/1.1 303 See Other\r\n"
statusLineFor HTTP_1_1 (Status 304) = B.byteString "HTTP/1.1 304 Not Modified\r\n"
statusLineFor HTTP_1_1 (Status 307) = B.byteString "HTTP/1.1 307 Temporary Redirect\r\n"
statusLineFor HTTP_1_1 (Status 308) = B.byteString "HTTP/1.1 308 Permanent Redirect\r\n"
statusLineFor HTTP_1_1 (Status 400) = B.byteString "HTTP/1.1 400 Bad Request\r\n"
statusLineFor HTTP_1_1 (Status 401) = B.byteString "HTTP/1.1 401 Unauthorized\r\n"
statusLineFor HTTP_1_1 (Status 403) = B.byteString "HTTP/1.1 403 Forbidden\r\n"
statusLineFor HTTP_1_1 (Status 404) = B.byteString "HTTP/1.1 404 Not Found\r\n"
statusLineFor HTTP_1_1 (Status 405) = B.byteString "HTTP/1.1 405 Method Not Allowed\r\n"
statusLineFor HTTP_1_1 (Status 409) = B.byteString "HTTP/1.1 409 Conflict\r\n"
statusLineFor HTTP_1_1 (Status 410) = B.byteString "HTTP/1.1 410 Gone\r\n"
statusLineFor HTTP_1_1 (Status 413) = B.byteString "HTTP/1.1 413 Content Too Large\r\n"
statusLineFor HTTP_1_1 (Status 429) = B.byteString "HTTP/1.1 429 Too Many Requests\r\n"
statusLineFor HTTP_1_1 (Status 500) = B.byteString "HTTP/1.1 500 Internal Server Error\r\n"
statusLineFor HTTP_1_1 (Status 502) = B.byteString "HTTP/1.1 502 Bad Gateway\r\n"
statusLineFor HTTP_1_1 (Status 503) = B.byteString "HTTP/1.1 503 Service Unavailable\r\n"
statusLineFor v s =
       B.byteString (versionToBytes v)
    <> spaceB
    <> statusCodeBytes s
    <> spaceB
    <> B.byteString (statusReason s)
    <> crlfB

-- | Just the request head as a strict 'ByteString'.
encodeRequestHead :: Request -> BS.ByteString
encodeRequestHead = B.toStrictByteString . requestBuilder

encodeResponseHead :: Response -> BS.ByteString
encodeResponseHead = B.toStrictByteString . responseBuilder

------------------------------------------------------------------------
-- Pre-encoding
------------------------------------------------------------------------

-- | Encode a 'Response' down to wire bytes once and wrap them in a
-- 'BodyPreEncoded' marker, so the server's send path emits the bytes
-- with a single @send()@ on every request and never touches the
-- encoder again.
--
-- This is the canonical idiom for static \/ semi-static responses
-- (health checks, OPTIONS preflights, hello-world fixtures): build
-- the 'Response' you would have returned, hand it to
-- 'precomputeResponse' /once/ at module init, and return the result
-- from your 'Network.HTTP1.Server.Handler'. The
-- 'Network.HTTP1.Server.runServer' machinery still does the
-- 'Connection: close' \/ HEAD \/ keep-alive bookkeeping by inspecting
-- the surrounding record's 'responseHeaders' and 'responseVersion'.
--
-- @
-- staticOk :: Response
-- staticOk = precomputeResponse $ Response
--   { responseStatus  = OK
--   , responseVersion = HTTP_1_1
--   , responseHeaders =
--       [ (\"Content-Type\", \"text\/plain\")
--       , (\"Server\", \"my-app\")
--       ]
--   , responseBody = BodyBytes \"Hello, world!\\n\"
--   }
--
-- handler :: Handler
-- handler _ = pure staticOk
-- @
--
-- 'BodyStream' bodies are not precomputable (their size is unknown
-- ahead of time) — passing one throws. Use 'precomputeResponseEither'
-- if you want the error as a 'Left'.
precomputeResponse :: Response -> Response
precomputeResponse r = case precomputeResponseEither r of
  Right r' -> r'
  Left msg -> error ("precomputeResponse: " <> msg)

-- | Pure-error variant of 'precomputeResponse'. Returns
-- @Left \"streaming body cannot be precomputed\"@ for a 'BodyStream'
-- input.
precomputeResponseEither :: Response -> Either String Response
precomputeResponseEither r = case responseBody r of
  BodyStream _ ->
    Left "streaming body cannot be precomputed"
  BodyPreEncoded _ ->
    Right r  -- idempotent
  body ->
    let !headBs = encodeResponseHead r
        !bodyBs = case body of
          BodyEmpty -> BS.empty
          BodyBytes bs -> bs
          _ -> BS.empty  -- exhaustive: stream + preencoded handled above
        !combined = headBs <> bodyBs
        !pe = PreEncoded combined (BS.length headBs)
    in Right r { responseBody = BodyPreEncoded pe }

------------------------------------------------------------------------
-- Header rendering
------------------------------------------------------------------------

-- | Render a header list as @name: value\\r\\n@ lines. We canonicalise
-- the separator to @": "@ (colon-space) but preserve case in field
-- names so user-controlled casing survives the round trip.
headersBuilder :: Headers -> B.Builder
headersBuilder = foldMap emit
  where
    emit (k, v) = B.byteString k <> colonSp <> B.byteString v <> crlfB
    colonSp = B.byteString ": "

------------------------------------------------------------------------
-- Framing-header injection
------------------------------------------------------------------------

-- | If the user did not supply explicit framing headers, append the
-- ones implied by the body shape. Also injects @Host@ for HTTP\/1.1
-- requests if missing (RFC 9112 § 3.2 makes it mandatory; we cannot
-- synthesise it without an authority, so we leave it absent and let
-- the application's request-builder fill it in).
augmentRequestHeaders :: Method -> Version -> Headers -> Body -> Headers
augmentRequestHeaders meth ver hdrs body =
  let
    !(hasCL, hasTE) = scanFramingPresence hdrs
  in
    if hasCL || hasTE
      then hdrs
      else case body of
        BodyEmpty
          | bodyAllowedInRequest meth && shouldAddZeroCL meth ->
              hdrs <> [("Content-Length", "0")]
          | otherwise -> hdrs
        BodyBytes bs ->
          hdrs <> [("Content-Length", decimalBS (toInteger (BS.length bs)))]
        BodyStream _
          | ver == HTTP_1_1 -> hdrs <> [("Transfer-Encoding", "chunked")]
          | otherwise -> hdrs <> [("Connection", "close")]
        BodyPreEncoded _ -> hdrs
        BodyFile fb ->
          hdrs <> [("Content-Length", decimalBS (toInteger (fbLength fb)))]
        -- ^ Both 'BodyPreEncoded' and 'BodyFile' on a *request* are
        -- unusual but legal — @Body@ is shared between request and
        -- response. The client'd then need to open / send the file
        -- itself (we don't sendfile on the client path).
  where
    shouldAddZeroCL POST = True
    shouldAddZeroCL PUT  = True
    shouldAddZeroCL PATCH = True
    shouldAddZeroCL _ = False

-- | Same idea for responses, with the status code factored in.
-- Scans the header list once to check for content-length,
-- transfer-encoding, and date simultaneously (avoids the 3× O(n)
-- scans the naive version would do).
augmentResponseHeaders :: Status -> Version -> Headers -> Body -> Headers
augmentResponseHeaders st ver hdrs body = withDate (framingAugmented)
  where
    !(hasCL, hasTE, hasDate) = scanResponsePresence hdrs
    framingAugmented
      | bodyForbidden = hdrs
      | hasCL || hasTE = hdrs
      | otherwise = case body of
          BodyEmpty -> hdrs <> [("Content-Length", "0")]
          BodyBytes bs ->
            hdrs <> [("Content-Length", decimalBS (toInteger (BS.length bs)))]
          BodyStream _
            | ver == HTTP_1_1 -> hdrs <> [("Transfer-Encoding", "chunked")]
            | otherwise -> hdrs <> [("Connection", "close")]
          BodyPreEncoded _ -> hdrs
          BodyFile fb ->
            hdrs <> [("Content-Length", decimalBS (toInteger (fbLength fb)))]
    sc = statusCode st
    bodyForbidden =
         (sc >= 100 && sc < 200)
      || sc == 204
      || sc == 304
    withDate hs
      | hasDate = hs
      | otherwise = ("Date", cachedHttpDate ()) : hs

-- | Single-pass scan for content-length and transfer-encoding.
scanFramingPresence :: Headers -> (Bool, Bool)
scanFramingPresence = go False False
  where
    go !cl !te [] = (cl, te)
    go !cl !te _ | cl && te = (cl, te)
    go !cl !te ((n, _) : rest)
      | not cl && asciiIeq n "content-length"    = go True te   rest
      | not te && asciiIeq n "transfer-encoding" = go cl   True rest
      | otherwise                                = go cl   te   rest

-- | Single-pass scan for content-length, transfer-encoding, and date.
scanResponsePresence :: Headers -> (Bool, Bool, Bool)
scanResponsePresence = go False False False
  where
    go !cl !te !dt [] = (cl, te, dt)
    go !cl !te !dt _ | cl && te && dt = (cl, te, dt)
    go !cl !te !dt ((n, _) : rest)
      | not cl && asciiIeq n "content-length"    = go True te   dt   rest
      | not te && asciiIeq n "transfer-encoding" = go cl   True dt   rest
      | not dt && asciiIeq n "date"              = go cl   te   True rest
      | otherwise                                = go cl   te   dt   rest

------------------------------------------------------------------------
-- Cached Date header
------------------------------------------------------------------------

-- | RFC 9110 § 6.6.1: origin servers SHOULD send a Date header in
-- every response unless the server can't produce a reliable clock.
-- We have a clock, so we always emit one.
--
-- == Cache strategy: per-capability tick
--
-- This is the same shape h2o uses ('h2o_now' + the loop's
-- per-thread timestamp): one cached IMF-fixdate per capability, with
-- a background tick thread refreshing them all once per second. The
-- hot path is one 'IOVector' index + one 'readIORef' — no syscall,
-- no formatting.
--
-- A single shared 'IORef' would be simpler but would bounce its
-- cache line on every tick (writer on one capability, readers on
-- the others). Per-capability slots cost us N writes per second
-- (where N = 'getNumCapabilities') but each write is contained to
-- one core's L1 — readers on the same capability hit the fresh
-- value with no cross-core fetch.
--
-- The thread starts lazily the first time 'cachedHttpDate' is
-- called. If a server never emits a Date header (unusual), the
-- thread never starts and nothing is wasted.
--
-- 'threadDelay' is not a real-time timer; the cached Date can be
-- stale by a few hundred ms under GC pressure. RFC 9110 § 6.6.1
-- only requires \"approximately the current time\"; nginx and h2o
-- have the same property.
{-# NOINLINE dateSlots #-}
dateSlots :: MV.IOVector BS.ByteString
dateSlots = unsafePerformIO $ do
  n <- getNumCapabilities
  now <- getCurrentTime
  let !initial = formatHttpDate now
  v <- MV.replicate (max 1 n) initial
  _tid <- forkIO (dateTickLoop v)
  pure v

dateTickLoop :: MV.IOVector BS.ByteString -> IO ()
dateTickLoop v = forever $ do
  threadDelay 1_000_000
  now <- getCurrentTime
  let !bs = formatHttpDate now
  let n = MV.length v
  forM_ [0 .. n - 1] $ \i -> MV.write v i bs

-- | Read the cached Date for the calling capability. One unboxed-int
-- modulo, one vector index, one pointer load. The () argument forces
-- a fresh call so GHC doesn't memoise this as a CAF.
cachedHttpDate :: () -> BS.ByteString
cachedHttpDate _ = unsafePerformIO $ do
  tid <- myThreadId
  (cap, _) <- threadCapability tid
  let !i = cap `mod` MV.length dateSlots
  MV.unsafeRead dateSlots i
{-# NOINLINE cachedHttpDate #-}

-- | IMF-fixdate per RFC 9110 § 5.6.7, e.g.
-- @Sun, 06 Nov 1994 08:49:37 GMT@.
formatHttpDate :: UTCTime -> BS.ByteString
formatHttpDate t = BSC.pack (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" t)

------------------------------------------------------------------------
-- Small builders
------------------------------------------------------------------------

{-# INLINE spaceB #-}
spaceB :: B.Builder
spaceB = B.word8 0x20

{-# INLINE crlfB #-}
crlfB :: B.Builder
crlfB = B.byteString "\r\n"

-- | Render a status code as 3 ASCII digits. Status codes are bounded
-- by RFC 9110 to 100..599 so this is always exactly 3 bytes; if the
-- caller stuffed a degenerate value in we fall back to general decimal.
statusCodeBytes :: Status -> B.Builder
statusCodeBytes (Status w)
  | w >= 100 && w < 1000 =
         B.word8 (0x30 + fromIntegral (w `div` 100))
      <> B.word8 (0x30 + fromIntegral ((w `div` 10) `mod` 10))
      <> B.word8 (0x30 + fromIntegral (w `mod` 10))
  | otherwise = B.byteString (decimalBS (toInteger w))

-- | Decimal-to-ASCII without going through 'show' or 'String'.
--
-- We write the digits backwards into a 20-byte scratch buffer (max
-- digits in a 'Word' on 64-bit), then slice. Used for
-- @Content-Length@; not on the recv hot path so a small per-call
-- allocation is fine, but the @show@-based round-trip we used to do
-- showed up in @+RTS -s@ heap profiles.
decimalBS :: Integer -> BS.ByteString
decimalBS n
  | n < 0 = BS.cons 0x2d (wordDecBS (fromInteger (negate n)))
  | otherwise = wordDecBS (fromInteger n)
{-# INLINE decimalBS #-}

-- | Render a 'Word' in base-10 ASCII. Single-allocation pinned write.
wordDecBS :: Word -> BS.ByteString
wordDecBS 0 = BS.singleton 0x30
wordDecBS w0 = unsafePerformIO $ do
  let maxDigits = 20
  fp <- BSI.mallocByteString maxDigits
  startOff <- withForeignPtr fp $ \p -> writeBackwards p (maxDigits - 1) w0
  pure $! BSI.fromForeignPtr fp startOff (maxDigits - startOff)
  where
    writeBackwards :: Ptr Word8 -> Int -> Word -> IO Int
    writeBackwards _ off 0 = pure (off + 1)
    writeBackwards p off w = do
      let !d = fromIntegral (w `rem` 10) :: Word8
      pokeByteOff p off (0x30 + d)
      writeBackwards p (off - 1) (w `quot` 10)
{-# INLINE wordDecBS #-}
