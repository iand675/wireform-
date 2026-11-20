{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Content-encoding handling, with an open typeclass-based
registry that mirrors the content-type system in
"Network.HTTP.Client.Media".

A content-encoding tag is a phantom type that has a
'HasContentEncoding' instance projecting it onto a header token
(@gzip@, @br@, @deflate@, ...). Adding a new encoding is just
declaring a tag, a 'HasContentEncoding' instance, and a
'Compress' / 'Decompress' instance:

@
data MyEncoding

instance HasContentEncoding MyEncoding where
  contentEncoding = Hermes.Custom "my-encoding"

instance Decompress MyEncoding where
  decompressBytes = myDecode
  decompressStream = myDecodeStream  -- optional override

instance Compress MyEncoding where
  compressBytes = myEncode
@

The middleware in this module doesn't dispatch on a closed sum.
Instead it takes a /list of erased handlers/ ('EncodingHandler')
built via 'asDecompressor'. Adding a new encoding to the wire is
@asDecompressor \@MyEncoding : defaultDecompressors@; replacing the
shipped set entirely is a fresh list.

Three handlers ship in 'defaultDecompressors':

* __Brotli__ (@br@), via the @brotli@ Haskell binding to
  libbrotlidec.
* __Gzip__ (@gzip@), via @zlib@'s @Codec.Compression.GZip@.
* __Deflate__ (@deflate@), via @zlib@'s @Codec.Compression.Zlib@.
  Servers in the wild send either RFC 1950 zlib-framed deflate or
  RFC 1951 raw deflate under @Content-Encoding: deflate@; we try
  zlib first and transparently fall back to raw when that throws.

'withDecompression' is enabled by default in
'Network.HTTP.Client.Config.withClient', sitting close to the base
transport so retries and tracing see the original status codes but
operate on already-decompressed bodies.

== Streaming decompression (§3.5 audit fix)

Each 'EncodingHandler' carries both a strict 'ehRun' (used for
request-body compression and stacked-encoding unit tests) and a
streaming 'ehRunStream'. The response middleware 'withDecompressionPolicy'
uses 'ehRunStream' so large compressed responses are not
materialised in full before the first byte reaches the caller.

The shipped Brotli, Gzip, and Deflate handlers implement
'decompressStream' (and therefore 'ehRunStream') via lazy
'Data.ByteString.Lazy' decompression driven through
'System.IO.Unsafe.unsafeInterleaveIO': the popper is consumed
on demand as the returned popper is pulled, so at any point only
the chunks that the downstream consumer has requested are
decompressed.  Zstd has no lazy binding and falls back to the
default (buffer then decompress).
-}
module Network.HTTP.Client.Compression (
  -- * Tags and classes
  HasContentEncoding (..),
  Decompress (..),
  Compress (..),

  -- ** Shipped tags
  Identity,
  Brotli,
  Gzip,
  Deflate,
  Zstd,

  -- * Type-erased handlers
  EncodingHandler (..),
  asDecompressor,
  asCompressor,
  defaultDecompressors,

  -- * Middleware
  withDecompression,
  withDecompressionPolicy,

  -- * Header helpers
  parseContentEncoding,
  parseContentEncodings,
  renderAcceptEncoding,

  -- * Request-body compression
  withCompression,
  withCompressionUsing,
) where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Codec.Compression.Zlib.Raw qualified as ZlibRaw
import Codec.Compression.Zstd qualified as Zstd
import Control.Exception (SomeException, evaluate, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Internal qualified as BSLI
import Data.CaseInsensitive qualified as CI
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text.Short qualified as ShortText
import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Protocol (mapProtocolTrailers, protocolRequestTrailers, withProtocolRequestTrailers)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Transport
import Network.HTTP.ContentCoding qualified as Hermes
import Network.HTTP.Headers qualified as Header
import Network.HTTP.Headers.ContentEncoding qualified as ContentEncoding
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Network.HTTP.Headers.Trailer qualified as Trailer
import Network.HTTP.Internal.Validation (isBodyDigestField)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.Status qualified as Status
import System.IO.Unsafe (unsafeInterleaveIO)
import Wireform.Builder qualified as WB


-- ---------------------------------------------------------------------------
-- Open typeclass machinery
-- ---------------------------------------------------------------------------

{- | Associate a content-encoding tag with a hermes 'ContentCoding'
value (the IANA-registered token, parsed and dispatched on as a
closed sum with a 'Custom' escape hatch for non-registered names).
-}
class HasContentEncoding tag where
  contentEncoding :: Hermes.ContentCoding


{- | The on-the-wire token for an encoding tag — convenience
projection through 'contentEncoding' rendered via the wireform
builder.
-}
contentEncodingToken :: forall tag. HasContentEncoding tag => ByteString
contentEncodingToken = renderCoding (contentEncoding @tag)


{- | Decompression for an encoding tag. Implementations are expected
to be deterministic; exceptions propagate to the caller as
transport errors.

The default 'decompressStream' buffers the full popper before
calling 'decompressBytes'. Instances that have a lazy or
incremental decoder (Gzip, Brotli, Deflate) override
'decompressStream' to avoid the full-body allocation.
-}
class HasContentEncoding tag => Decompress tag where
  decompressBytes :: ByteString -> IO ByteString


  {- | Streaming variant of 'decompressBytes'.  Returns a new popper
  whose output is the decompressed form of the input popper's
  output.  The default implementation materialises the entire
  input before decompressing; override for codecs with lazy or
  incremental APIs.
  -}
  decompressStream :: Popper -> IO Popper
  decompressStream p = do
    compressed <- popperBytes p
    plain <- decompressBytes @tag compressed
    popperFromStrict plain


{- | Compression for an encoding tag. Used for callers that want to
send compressed request bodies; not wired into the default
middleware stack.
-}
class HasContentEncoding tag => Compress tag where
  compressBytes :: ByteString -> IO ByteString


-- ---------------------------------------------------------------------------
-- Shipped tags
-- ---------------------------------------------------------------------------

-- | The no-op encoding. @Content-Encoding: identity@.
data Identity


instance HasContentEncoding Identity where
  contentEncoding = Hermes.Identity


instance Decompress Identity where
  decompressBytes = pure
  decompressStream p = pure p -- pass-through; no allocation


instance Compress Identity where
  compressBytes = pure


-- | Brotli (@br@).
data Brotli


instance HasContentEncoding Brotli where
  contentEncoding = Hermes.Brotli


instance Decompress Brotli where
  decompressBytes bs =
    evaluate (BSL.toStrict (Brotli.decompress (BSL.fromStrict bs)))


  -- Brotli.decompress is lazy (BSL → BSL); drive it via the popper
  -- using unsafeInterleaveIO so only the chunks the caller pulls are
  -- decompressed at any one time.
  decompressStream = streamDecompressLazy Brotli.decompress


instance Compress Brotli where
  compressBytes bs =
    evaluate (BSL.toStrict (Brotli.compress (BSL.fromStrict bs)))


-- | Gzip (@gzip@), via @zlib@.
data Gzip


instance HasContentEncoding Gzip where
  contentEncoding = Hermes.GZip


instance Decompress Gzip where
  decompressBytes bs =
    evaluate (BSL.toStrict (GZip.decompress (BSL.fromStrict bs)))


  decompressStream = streamDecompressLazy GZip.decompress


instance Compress Gzip where
  compressBytes bs =
    evaluate (BSL.toStrict (GZip.compress (BSL.fromStrict bs)))


{- | Deflate (@deflate@) — historically ambiguous between RFC 1950
zlib-wrapped and RFC 1951 raw. 'decompressBytes' tries the zlib
framing first and falls back to raw deflate when that fails.
'compressBytes' always emits the zlib framing.
-}
data Deflate


instance HasContentEncoding Deflate where
  contentEncoding = Hermes.Deflate


instance Decompress Deflate where
  decompressBytes bs = do
    result <- try (evaluate (BSL.toStrict (Zlib.decompress (BSL.fromStrict bs))))
    case (result :: Either SomeException ByteString) of
      Right out -> pure out
      Left _ -> evaluate (BSL.toStrict (ZlibRaw.decompress (BSL.fromStrict bs)))


  -- For deflate the zlib vs raw ambiguity only manifests at the
  -- very first bytes, so we attempt zlib streaming first; if that
  -- fails the fallback re-decompresses from the already-buffered
  -- input (error path only).
  decompressStream =
    streamDecompressLazyFallback
      Zlib.decompress
      ZlibRaw.decompress


instance Compress Deflate where
  compressBytes bs =
    evaluate (BSL.toStrict (Zlib.compress (BSL.fromStrict bs)))


{- | Zstandard (@zstd@), via the @zstd@ Haskell binding to libzstd.
The @zstd@ package decompresses from strict 'ByteString' only;
'decompressStream' uses the default buffering fallback.
-}
data Zstd


instance HasContentEncoding Zstd where
  contentEncoding = Hermes.ZStd


instance Decompress Zstd where
  decompressBytes bs = case Zstd.decompress bs of
    Zstd.Decompress out -> evaluate out
    Zstd.Skip -> pure BS.empty
    Zstd.Error msg -> throwIO (userError ("zstd decompress: " <> msg))


instance Compress Zstd where
  compressBytes bs = evaluate (Zstd.compress 3 bs)


-- ---------------------------------------------------------------------------
-- Type-erased handlers
-- ---------------------------------------------------------------------------

{- | A type-erased decompressor: the encoding identity plus actions
that decompress bytes in strict or streaming form.
'EncodingHandler's are what the middleware actually dispatches
on. Build them from a tag via 'asDecompressor' \/ 'asCompressor'.
-}
data EncodingHandler = EncodingHandler
  { ehCoding :: !Hermes.ContentCoding
  , ehRun :: !(ByteString -> IO ByteString)
  {- ^ Strict decompression. Used for stacked-encoding unit-tests
  and request-body compression.
  -}
  , ehRunStream :: !(Popper -> IO Popper)
  {- ^ Streaming decompression used by 'withDecompressionPolicy'.
  Avoids materialising the entire compressed response body
  when a lazy decoder is available (§3.5 audit fix).
  -}
  }


-- | The wire token for a handler's encoding, rendered via hermes.
ehToken :: EncodingHandler -> ByteString
ehToken h = renderCoding (ehCoding h)


{- | Render a 'ContentCoding' to its wire bytes through the
wireform builder. Kept as a tiny monomorphic helper so callers
don't need a 'WB.Builder' import on top of the hermes one.
-}
renderCoding :: Hermes.ContentCoding -> ByteString
renderCoding c = WB.toStrictByteString (Hermes.renderContentCoding c)


{- | Project a 'Decompress' tag into an 'EncodingHandler' suitable
for the decompression middleware.
-}
asDecompressor :: forall tag. Decompress tag => EncodingHandler
asDecompressor =
  EncodingHandler
    { ehCoding = contentEncoding @tag
    , ehRun = decompressBytes @tag
    , ehRunStream = decompressStream @tag
    }


{- | Project a 'Compress' tag into an 'EncodingHandler' that
compresses bytes. Symmetric with 'asDecompressor'; consumers (a
request-body compression middleware, say) decide which side they
want.  'ehRunStream' is not meaningful for compression (request
bodies are always materialised), so it buffers via 'popperBytes'.
-}
asCompressor :: forall tag. Compress tag => EncodingHandler
asCompressor =
  EncodingHandler
    { ehCoding = contentEncoding @tag
    , ehRun = compressBytes @tag
    , ehRunStream = \p -> do
        inp <- popperBytes p
        out <- compressBytes @tag inp
        popperFromStrict out
    }


{- | The default decompressor set, in preference order. Brotli first
(smallest payloads on the modern web), then gzip (most compatible),
then deflate (legacy).
-}
defaultDecompressors :: [EncodingHandler]
defaultDecompressors =
  [ asDecompressor @Brotli
  , asDecompressor @Gzip
  , asDecompressor @Deflate
  ]


-- ---------------------------------------------------------------------------
-- Streaming decompression helpers (§3.5 audit fix)
-- ---------------------------------------------------------------------------

{- | Build a lazy 'BSL.ByteString' from a popper via
'unsafeInterleaveIO'.  Each chunk is pulled from the popper only
when the lazy spine reaches it, so the decoder and the popper
interleave naturally without buffering the whole body.

'unsafeInterleaveIO' is safe here because:
* The popper is a one-time, single-threaded pull source.
* The lazy ByteString is consumed sequentially by the decompressor.
* No mutable state is shared between the lazy thunks.
-}
popperToLazy :: Popper -> IO BSL.ByteString
popperToLazy p = unsafeInterleaveIO go
  where
    go = do
      chunk <- p
      if BS.null chunk
        then pure BSLI.Empty
        else BSLI.Chunk chunk <$> unsafeInterleaveIO go


{- | Convert a lazy 'BSL.ByteString' back into a 'Popper' that yields
one strict chunk at a time, in order.
-}
lazyToPopper :: BSL.ByteString -> IO Popper
lazyToPopper lbs = do
  ref <- newIORef (BSL.toChunks lbs)
  pure $ atomicModifyIORef' ref $ \case
    [] -> ([], BS.empty)
    (c : cs) -> (cs, c)


{- | Build a streaming 'Popper' decompressor from a lazy
'BSL.ByteString -> BSL.ByteString' function.  The decompressor
sees the input as a lazy stream; the output is a popper that
yields chunks as they become available.
-}
streamDecompressLazy
  :: (BSL.ByteString -> BSL.ByteString)
  -> Popper
  -> IO Popper
streamDecompressLazy decompress p = do
  lazyIn <- popperToLazy p
  lazyToPopper (decompress lazyIn)


{- | Variant of 'streamDecompressLazy' with a fallback for the case
where the primary decoder fails (e.g. deflate's zlib vs raw
ambiguity).  On failure the fallback is applied to the
already-buffered strict bytes.  This means the input IS
materialised on the error path — but that path is rare and
signals a format mismatch rather than a large-body scenario.
-}
streamDecompressLazyFallback
  :: (BSL.ByteString -> BSL.ByteString)
  -- ^ primary (zlib)
  -> (BSL.ByteString -> BSL.ByteString)
  -- ^ fallback (raw deflate)
  -> Popper
  -> IO Popper
streamDecompressLazyFallback primary fallback p = do
  lazyIn <- popperToLazy p
  result <- try (lazyToPopper (primary lazyIn) >>= \q -> evaluate q)
  case (result :: Either SomeException Popper) of
    Right okPopper -> pure okPopper
    Left _ -> do
      strictIn <- evaluate (BSL.toStrict lazyIn)
      lazyToPopper (fallback (BSL.fromStrict strictIn))


{- | Thread a list of streaming decompressors left-to-right, each
consuming the previous popper's output.  Used by
'withDecompressionPolicy' to undo stacked encodings.
-}
chainStreamHandlers :: [EncodingHandler] -> Popper -> IO Popper
chainStreamHandlers [] p = pure p
chainStreamHandlers (h : hs) p = do
  p' <- ehRunStream h p
  chainStreamHandlers hs p'


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

{- | Decompression middleware using 'defaultDecompressors'. Adds
@Accept-Encoding@ when absent, using @identity@ for ranged requests.
Partial responses retain the bytes and fields as sent by the server,
since @Content-Range@ addresses the encoded representation. Responses
without content retain their representation metadata. For complete
coded bodies, the middleware removes @Content-Encoding@,
@Content-Length@, and the digests of the encoded bytes. It removes those
digests from the trailer section and its announcement as well. Origin
validators remain available for revalidation.
-}
withDecompression :: Middleware IO
withDecompression = withDecompressionPolicy defaultDecompressors


{- | Same as 'withDecompression' but with a caller-supplied handler
set. Pass @[]@ to disable advertising or dispatching entirely;
pass @asDecompressor \@MyEncoding : defaultDecompressors@ to add a
new encoding without losing the shipped set.

Decompression is streaming (§3.5 audit fix): 'ehRunStream' is used
for each encoding stage so large compressed responses are decoded
incrementally rather than materialised in full before the first
byte reaches the caller.
-}
withDecompressionPolicy :: [EncodingHandler] -> Middleware IO
withDecompressionPolicy [] inner = inner
withDecompressionPolicy handlers inner = Transport $ \req -> do
  let advertise preference [] = WReq.addHeader H.hAcceptEncoding preference req
      advertise preference ((name, _) : rest)
        | name == H.hAcceptEncoding = req
        | name == H.hRange = advertise "identity" rest
        | otherwise = advertise preference rest
  raw <- sendRaw inner (advertise supported (WReq.headers req))
  case H.lookupHeaders H.hContentEncoding (Resp.headers raw) of
    first : rest
      | canDecodeResponse (WReq.method req) (Resp.statusCode raw)
      , Just hs@(_ : _) <- lookupHandlers (parseContentEncodingLines (first :| rest)) handlers -> do
          -- Undo the codings in reverse order of application (RFC 9110 §8.4).
          let source = Resp.bodyPopper raw
          decoded <- chainStreamHandlers (reverse hs) source
          let consume = do
                chunk <- decoded
                -- A decoder can reach its end marker before the transport
                -- has consumed the trailing field section.
                if BS.null chunk then drainPopper source >> pure chunk else pure chunk
          pure (stripEncoding raw) {Resp.bodyPopper = consume}
    _ -> pure raw
  where
    supported = renderAcceptEncoding handlers


canDecodeResponse :: Method.Method -> Status.Status -> Bool
canDecodeResponse method status
  | method == Method.mHead = False
  | otherwise = case Status.statusCode status of
      204 -> False
      205 -> False
      206 -> False
      304 -> False
      code -> code >= 200 && (code >= 300 || method /= Method.mConnect)


-- ---------------------------------------------------------------------------
-- Request-body compression
-- ---------------------------------------------------------------------------

{- | Compress outgoing request bodies with @tag@. Buffers the request
body, compresses, sets @Content-Encoding@, and refreshes
@Content-Length@ to match the compressed payload. The middleware
removes the source digests from the headers, trailers, and announcement.
The identity handler and requests with an explicit @Content-Encoding@
pass through directly.

> withClient cfg { ccExtra = [withCompression @Brotli] } $ \t -> ...
-}
withCompression
  :: forall tag
   . Compress tag
  => Middleware IO
withCompression = withCompressionUsing (asCompressor @tag)


{- | Like 'withCompression' but takes the type-erased 'EncodingHandler'
explicitly. Useful for plugging in a runtime-chosen codec.
-}
withCompressionUsing :: EncodingHandler -> Middleware IO
withCompressionUsing handler inner
  | ehCoding handler == Hermes.Identity = inner
  | otherwise = Transport $ \req -> do
      let hdrs0 = WReq.headers req
      if H.hasHeader H.hContentEncoding hdrs0
        then sendRaw inner req
        else do
          raw <- bodyStreamBytes (WReq.body req)
          encoded <- ehRun handler raw
          bs' <- streamFromStrict encoded
          let token = ehToken handler
              hdrs1 = H.insertHeader H.hContentEncoding token (stripDigestFields (const False) hdrs0)
              hdrs2 =
                H.insertHeader
                  H.hContentLength
                  (BS8.pack (show (BS.length encoded)))
                  hdrs1
          sendRaw
            inner
            req
              { WReq.body = bs'
              , WReq.headers = hdrs2
              , WReq.protocolHints = stripRequestDigestTrailers (WReq.protocolHints req)
              }
  where
    stripRequestDigestTrailers hints = case protocolRequestTrailers hints of
      Nothing -> hints
      Just produce -> withProtocolRequestTrailers (Just (stripDigestTrailers <$> produce)) hints


-- ---------------------------------------------------------------------------
-- Header helpers
-- ---------------------------------------------------------------------------

{- | Parse a single @Content-Encoding@ token to a hermes
'ContentCoding'. Returns 'Nothing' if the input is not a single
recognised token. For multi-step encodings, use
'parseContentEncodings'.
-}
parseContentEncoding :: ByteString -> Maybe Hermes.ContentCoding
parseContentEncoding raw =
  let trimmed = BS.dropWhile isWS (BS.dropWhileEnd isWS raw)
  in case runParser Hermes.contentCodingParser trimmed of
       OK coding leftover | BS.null leftover -> Just coding
       _ -> Nothing
  where
    isWS w = w == 0x20 || w == 0x09


{- | Parse a comma-separated @Content-Encoding@ list (RFC 9110 §8.4)
into the underlying tokens, in the order they appeared. Returns
@[]@ if any token is malformed (the middleware then leaves the
body alone rather than half-decoding).
-}
parseContentEncodings :: ByteString -> [Hermes.ContentCoding]
parseContentEncodings raw = parseContentEncodingLines (raw :| [])


parseContentEncodingLines :: NonEmpty ByteString -> [Hermes.ContentCoding]
parseContentEncodingLines fields =
  either (const []) ContentEncoding.contentCodings $
    Header.parseFromHeaders Header.defaultHeaderSettings fields


{- | Resolve a list of codings against a handler set. Returns 'Nothing'
(i.e. "can't decode this") if any coding has no handler and isn't
the no-op 'Identity'.
-}
lookupHandlers
  :: [Hermes.ContentCoding] -> [EncodingHandler] -> Maybe [EncodingHandler]
lookupHandlers [] _ = Just []
lookupHandlers (c : cs) hs
  | c == Hermes.Identity = lookupHandlers cs hs
  | otherwise = case findHandler c hs of
      Nothing -> Nothing
      Just h -> (h :) <$> lookupHandlers cs hs


{- | Render a preference-ordered handler list as an
@Accept-Encoding@ value. Each handler's hermes 'ContentCoding'
is rendered through the wireform builder and the tokens joined
with @\", \"@.
-}
renderAcceptEncoding :: [EncodingHandler] -> ByteString
renderAcceptEncoding = BS.intercalate ", " . map ehToken


findHandler :: Hermes.ContentCoding -> [EncodingHandler] -> Maybe EncodingHandler
findHandler coding = List.find (Hermes.sameContentCoding coding . ehCoding)


stripEncoding :: Resp.RawResponse -> Resp.RawResponse
stripEncoding raw =
  raw
    { Resp.headers = stripDigestFields removeCoding (Resp.headers raw)
    , Resp.protocolInfo = mapProtocolTrailers stripDigestTrailers (Resp.protocolInfo raw)
    }
  where
    removeCoding name = name == H.hContentEncoding || name == H.hContentLength


stripDigestTrailers :: H.Headers -> H.Headers
stripDigestTrailers = filter (not . isBodyDigestField . fst)


stripDigestFields :: (H.HeaderName -> Bool) -> H.Headers -> H.Headers
stripDigestFields removeField = foldr stripField []
  where
    stripField field@(name, value) rest
      | removeField name || isBodyDigestField name = rest
      | name == H.hTrailer =
          case Header.parseFromHeaders Header.defaultHeaderSettings (value :| []) of
            Right Trailer.TrailerEmpty -> field : rest
            Right (Trailer.Trailer names)
              | not (any digestName names) -> field : rest
              | Just remaining <- NE.nonEmpty (NE.filter (not . digestName) names) ->
                  (name, WB.toStrictByteString (Trailer.renderTrailer (Trailer.Trailer remaining))) : rest
            _ -> rest
      | otherwise = field : rest
    digestName = isBodyDigestField . CI.mk . ShortText.toByteString
{-# INLINE stripDigestFields #-}
