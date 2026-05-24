{- | HTTP\/1.x message parser (RFC 9112 § 2-5).

The strategy is:

1. The connection's recv buffer hands us a contiguous slice of the
   header block (everything up to the first @\\r\\n\\r\\n@).
2. We walk that slice byte by byte with the SIMD CR \/ tchar \/
   field-value scanners. Header values and tokens are zero-copy
   sub-slices of the recv buffer.
3. The result is a strict 'Request' or 'Response' plus the parsed
   'Framing' that tells the connection layer how to read the body.

The parser is /strict/ about RFC 9112 § 3.5 \"robustness\":

* multiple @Content-Length@ values that disagree -> 'ParseLengthConflict'
* a non-numeric @Content-Length@ -> 'ParseInvalidLength'
* both @Content-Length@ and @Transfer-Encoding@ on the same message ->
  'ParseLengthAndTransferEncoding' (RFC 9112 § 6.3 requires
  'Transfer-Encoding' to take precedence; we accept and re-frame on
  TE, but flag this so the application can reject it if it's behind a
  cache that's known to be ambiguous).
* a @Transfer-Encoding@ chain that doesn't end with @chunked@ on a
  request -> 'ParseChunkedNotFinal'.
* CR \/ LF \/ NUL inside a header value -> 'ParseInvalidHeaderValue'
  (smuggling guard).

The header-name parser runs through the SIMD tchar scanner so values
like @\"Host:\"@ (no name) or @\"X-\\tFoo: bar\"@ (HTAB in name) are
rejected with a single SSE2 vector test.
-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP1.Parser
  ( -- * Errors
    ParseError (..)

    -- * Body framing
  , Framing (..)

    -- * Parsers
  , parseRequest
  , parseRequestLower
  , parseResponse
  , parseResponseLower
  , parseHeaderBlock
  , parseHeaderBlockLower
  , parseRequestLine
  , parseStatusLine

    -- * Chunked TE
  , ChunkHeader (..)
  , parseChunkHeader
  , parseChunkSize

    -- * Helpers (exported for testing)
  , findNonToken
  , findNonFieldValue

    -- * Cross-parser validation hooks
    -- $crossvalid
  , validateHost
  , validateTarget
  , requestFraming
  , responseFraming
  ) where

-- $crossvalid
-- These four helpers are exported so the streaming wireform parser
-- in "Network.HTTP1.StreamingParser" can re-use the exact same
-- structural / smuggling validation logic as the classic
-- whole-block parser.  Sharing them keeps the two parsers'
-- semantics aligned without duplicating the (subtle, frequently
-- audited) request-smuggling guards.

import Control.DeepSeq (NFData)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64, Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)

import Network.HTTP1.Internal.Ascii (asciiIeq)
import Network.HTTP1.Types
  -- Types re-exports Method, Version, Status, Headers; we import
  -- everything because the framing case-alts mention Status / Version /
  -- Method constructors directly.

foreign import ccall unsafe "hs_http1_find_non_token"
  c_find_non_token :: Ptr () -> CInt -> CInt -> CInt

foreign import ccall unsafe "hs_http1_find_non_fieldvalue"
  c_find_non_fv :: Ptr () -> CInt -> CInt -> CInt

foreign import ccall unsafe "hs_http1_to_lower_ascii"
  c_to_lower_ascii :: Ptr () -> Ptr () -> CInt -> IO ()

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Why a parse failed.
--
-- These are designed to map onto specific HTTP response codes:
--
--   * 'ParseMessageTooLong'        -> 431 / 414
--   * 'ParseBadRequestLine'        -> 400
--   * 'ParseBadStatusLine'         -> 502 (we were the client)
--   * 'ParseBadHeaderName' /
--     'ParseInvalidHeaderValue'    -> 400
--   * 'ParseLengthConflict' /
--     'ParseLengthAndTransferEncoding' /
--     'ParseChunkedNotFinal' /
--     'ParseInvalidLength'         -> 400 (request smuggling guards)
--   * 'ParseUnsupportedVersion'    -> 505
--   * 'ParseBadChunkHeader' /
--     'ParseChunkTooLarge'         -> 400
data ParseError
  = ParseMessageTooLong
  | ParseBadRequestLine
  | ParseBadStatusLine
  | ParseBadHeaderName
  | ParseInvalidHeaderValue
  | ParseLengthConflict
  | ParseLengthAndTransferEncoding
  | ParseChunkedNotFinal
  | ParseInvalidLength
  | ParseUnsupportedVersion
  | ParseBadChunkHeader
  | ParseChunkTooLarge
  | ParseUnexpectedEof
  | ParseMissingHost
    -- ^ HTTP\/1.1 request without a @Host@ header (RFC 9112 § 3.2).
  | ParseMultipleHosts
    -- ^ HTTP\/1.1 request with more than one @Host@ header (RFC 9112
    -- § 3.2).
  | ParseInvalidHost
    -- ^ The @Host@ header value is structurally invalid (contains
    -- userinfo, a path, NUL, whitespace, or is empty). RFC 9112 § 3.2:
    -- "Host = uri-host [ ":" port ]" — no @user\@@ prefix, no slashes.
  | ParseInvalidTarget
    -- ^ The request target contains forbidden bytes (NUL, CR, LF,
    -- non-ASCII octets in origin\/absolute form, …). Common
    -- smuggling vector.
  deriving stock (Eq, Show, Generic)

instance NFData ParseError

------------------------------------------------------------------------
-- Helper: SIMD scanners exposed as pure ByteString fns
------------------------------------------------------------------------

-- | Offset of the first non-tchar in @bs[offset..]@. Returns
-- @BS.length bs@ on miss.
{-# INLINE findNonToken #-}
findNonToken :: ByteString -> Int -> Int
findNonToken bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    pure $! fromIntegral (c_find_non_token (castPtr p) (fromIntegral off) (fromIntegral len))

-- | Offset of the first non-field-vchar (i.e. forbidden control byte)
-- in @bs[offset..]@. Returns @BS.length bs@ on miss.
{-# INLINE findNonFieldValue #-}
findNonFieldValue :: ByteString -> Int -> Int
findNonFieldValue bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    pure $! fromIntegral (c_find_non_fv (castPtr p) (fromIntegral off) (fromIntegral len))

------------------------------------------------------------------------
-- Framing
------------------------------------------------------------------------

-- | Body framing decision derived from the parsed headers.
--
-- ['NoBody']         The message has no body (e.g. 1xx \/ 204 \/ 304
--                    response, GET request without explicit body
--                    headers).
-- ['ContentLength']  Body length is a fixed number of octets.
-- ['Chunked']        Body uses @Transfer-Encoding: chunked@. Must be
--                    delimited by the terminating zero-size chunk.
-- ['CloseDelimited'] HTTP\/1.0 response without explicit length; body
--                    runs until the peer closes the connection.
data Framing
  = NoBody
  | ContentLength !Word64
  | Chunked
  | CloseDelimited
  deriving stock (Eq, Show, Generic)

instance NFData Framing

------------------------------------------------------------------------
-- Header block parser (shared by request + response)
------------------------------------------------------------------------

-- | Parse a header block (the raw bytes between the request \/ status
-- line and the @\\r\\n\\r\\n@ terminator) into a 'Headers' list.
--
-- The input is the slice that comes out of
-- 'recvBufferReadUntilDoubleCRLF', minus the request \/ status line.
--
-- Implements:
--
--   * RFC 9110 § 5.1 — field-name is a token; field-value is
--     field-vchar \[(SP\/HTAB)+ field-vchar\] (we accept trailing OWS
--     trimming).
--   * RFC 9112 § 5.2 — no CR \/ LF \/ NUL inside the field-value.
--   * RFC 9112 § 5.2 — reject obs-fold (a line beginning with SP\/HTAB
--     that continues the previous header). We map it to
--     'ParseInvalidHeaderValue' because RFC 9112 says recipients MAY
--     either reject or "replace the obs-fold with one or more SP
--     octets"; rejection is safer in the absence of explicit opt-in.
parseHeaderBlock :: ByteString -> Either ParseError Headers
parseHeaderBlock raw = go 0 []
  where
    !len = BS.length raw
    go !off acc
      | off >= len = Right (reverse acc)
      | BSU.unsafeIndex raw off == 0x20 || BSU.unsafeIndex raw off == 0x09 =
          Left ParseInvalidHeaderValue
      | otherwise = parseOneHeader raw off >>= \(hdr, off') -> go off' (hdr : acc)

-- | Like 'parseHeaderBlock' but lowercases header names in-place
-- using the SSE2 lowerer. The input MUST be an owned copy (e.g.
-- from 'BS.copy') — not a slice of shared or read-only memory.
-- The resulting name ByteStrings share the same ForeignPtr as the
-- input, with zero extra allocation for the fold.
parseHeaderBlockLower :: ByteString -> Either ParseError Headers
parseHeaderBlockLower raw = go 0 []
  where
    !len = BS.length raw
    go !off acc
      | off >= len = Right (reverse acc)
      | BSU.unsafeIndex raw off == 0x20 || BSU.unsafeIndex raw off == 0x09 =
          Left ParseInvalidHeaderValue
      | otherwise =
          let !nameEnd = findNonToken raw off
          in unsafePerformIO (lowercaseInPlace raw off nameEnd) `seq`
             parseOneHeader raw off >>= \(hdr, off') -> go off' (hdr : acc)

parseOneHeader :: ByteString -> Int -> Either ParseError (Header, Int)
parseOneHeader raw off0 =
  let !len = BS.length raw
      !nameEnd = findNonToken raw off0
  in
    if nameEnd == off0
      then Left ParseBadHeaderName
      else if nameEnd >= len || BSU.unsafeIndex raw nameEnd /= 0x3a  -- ':'
        then Left ParseBadHeaderName
        else
          let !name = sliceBS raw off0 nameEnd
              -- skip optional leading OWS
              valStart = skipOws raw (nameEnd + 1) len
              -- scan to end-of-line: must hit CRLF before any non-field-vchar
              vEnd0 = findNonFieldValue raw valStart
          in
            if vEnd0 >= len
              then
                -- End of the header-block slice, no trailing CRLF on
                -- the last line because the caller stripped it.
                let !valTrimEnd = trimOwsEnd raw valStart vEnd0
                    !val = sliceBS raw valStart valTrimEnd
                in Right ((name, val), len)
              else
                let !b = BSU.unsafeIndex raw vEnd0
                in if b /= 0x0d
                     -- Any other non-field-vchar byte (bare LF, NUL,
                     -- control char, DEL) is forbidden in the value.
                     then Left ParseInvalidHeaderValue
                     else
                       -- We landed on a CR. RFC 9112 § 2.2 forbids
                       -- bare CR; the CR MUST be immediately followed
                       -- by LF or it's a protocol error (smuggling
                       -- vector — a bare CR mid-value can desync
                       -- proxies).
                       if vEnd0 + 1 >= len
                          || BSU.unsafeIndex raw (vEnd0 + 1) /= 0x0a
                         then Left ParseInvalidHeaderValue
                         else
                           let !valTrimEnd = trimOwsEnd raw valStart vEnd0
                               !val = sliceBS raw valStart valTrimEnd
                           in Right ((name, val), vEnd0 + 2)

{-# INLINE sliceBS #-}
sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

-- | Lowercase bytes in [off, end) of a ByteString in-place using the
-- SSE2 lowerer. ONLY safe when the ByteString is an owned copy (not
-- a slice of shared memory). Used on the BS.copy'd header block.
{-# INLINE lowercaseInPlace #-}
lowercaseInPlace :: ByteString -> Int -> Int -> IO ()
lowercaseInPlace (BSI.BS fp _) off end =
  let !n = end - off
  in if n <= 0 then pure ()
     else withForeignPtr fp $ \base ->
       let !p = base `plusPtr` off
       in c_to_lower_ascii (castPtr p) (castPtr p) (fromIntegral n)

{-# INLINE skipOws #-}
skipOws :: ByteString -> Int -> Int -> Int
skipOws bs !i !lim
  | i >= lim = lim
  | let !b = BSU.unsafeIndex bs i in b == 0x20 || b == 0x09 = skipOws bs (i + 1) lim
  | otherwise = i

{-# INLINE trimOwsEnd #-}
trimOwsEnd :: ByteString -> Int -> Int -> Int
trimOwsEnd bs !start !i
  | i <= start = start
  | let !b = BSU.unsafeIndex bs (i - 1) in b == 0x20 || b == 0x09 =
      trimOwsEnd bs start (i - 1)
  | otherwise = i

------------------------------------------------------------------------
-- Request line
------------------------------------------------------------------------

-- | Parse a request line of the form @METHOD SP target SP HTTP\/1.x CRLF@.
-- Returns (method, target, version) on success.
parseRequestLine :: ByteString -> Either ParseError (Method, RawTarget, Version)
parseRequestLine line = do
  -- METHOD (1*tchar)
  let !len = BS.length line
      !methEnd = findNonToken line 0
  if methEnd == 0 || methEnd >= len || BSU.unsafeIndex line methEnd /= 0x20
    then Left ParseBadRequestLine
    else do
      let !meth = methodFromBytes (sliceBS line 0 methEnd)
          tgtStart = methEnd + 1
      -- Target runs until next SP. We do *not* run a strict URI parser
      -- here; the application layer is responsible for that.
      let tgtEnd = findByte line tgtStart 0x20
      if tgtEnd >= len || tgtEnd == tgtStart
        then Left ParseBadRequestLine
        else do
          let !tgt = BS.copy (sliceBS line tgtStart tgtEnd)
              verStart = tgtEnd + 1
              ver = sliceBS line verStart len
          case versionFromBytes ver of
            Just v -> Right (meth, tgt, v)
            Nothing
              -- "HTTP/<MAJOR>.<MINOR>" with single ASCII digits in
              -- each position is a well-formed but unsupported
              -- version (505). Anything else is structurally
              -- malformed (400) — "HTTP/1", "HTTP/01.01", "HTTP/ 1.1"
              -- all fall here. The split matters: 505 promises "I
              -- understood you and could send a major/minor pair
              -- back" whereas 400 means "I couldn't parse the line at
              -- all".
              | isWellFormedHttpVersion ver -> Left ParseUnsupportedVersion
              | otherwise -> Left ParseBadRequestLine

-- | True for @HTTP\/D.D@ exactly (one digit each side). Lets us emit
-- 505 (Unsupported Version) only for well-formed version strings;
-- everything else is 400.
isWellFormedHttpVersion :: ByteString -> Bool
isWellFormedHttpVersion bs =
     BS.length bs == 8
  && BSU.unsafeIndex bs 0 == 0x48  -- 'H'
  && BSU.unsafeIndex bs 1 == 0x54
  && BSU.unsafeIndex bs 2 == 0x54
  && BSU.unsafeIndex bs 3 == 0x50
  && BSU.unsafeIndex bs 4 == 0x2f  -- '/'
  && asciiDigit (BSU.unsafeIndex bs 5)
  && BSU.unsafeIndex bs 6 == 0x2e  -- '.'
  && asciiDigit (BSU.unsafeIndex bs 7)
  where
    asciiDigit b = b >= 0x30 && b <= 0x39

------------------------------------------------------------------------
-- Status line
------------------------------------------------------------------------

-- | Parse a status line of the form @HTTP\/1.x SP code SP reason CRLF@.
-- Returns (version, status). Reason phrase is preserved by the calling
-- code if it needs to; the typed @Response@ regenerates it from
-- 'statusReason' on outgoing messages.
parseStatusLine :: ByteString -> Either ParseError (Version, Status, ByteString)
parseStatusLine line = do
  let !len = BS.length line
      verEnd = findByte line 0 0x20
  if verEnd >= len
    then Left ParseBadStatusLine
    else do
      let ver = sliceBS line 0 verEnd
      case versionFromBytes ver of
        Nothing
          | isWellFormedHttpVersion ver -> Left ParseUnsupportedVersion
          | otherwise -> Left ParseBadStatusLine
        Just v -> do
          let codeStart = verEnd + 1
              codeEnd = findByte line codeStart 0x20
          if codeEnd >= len || codeEnd - codeStart /= 3
            then Left ParseBadStatusLine
            else case parseStatusCode line codeStart codeEnd of
              Nothing -> Left ParseBadStatusLine
              Just code ->
                let reason = sliceBS line (codeEnd + 1) len
                in Right (v, Status (fromIntegral code), reason)

{-# INLINE parseStatusCode #-}
parseStatusCode :: ByteString -> Int -> Int -> Maybe Int
parseStatusCode bs s e = go s 0
  where
    go !i !acc
      | i >= e = Just acc
      | otherwise =
          let !b = BSU.unsafeIndex bs i
          in if b < 0x30 || b > 0x39
               then Nothing
               else go (i + 1) (acc * 10 + fromIntegral (b - 0x30))

-- | Scalar byte search (used only off the recv-buffer path; the SIMD
-- variant in @Wireform.FFI.findByteBS@ would be marginally faster but
-- we'd pay the FFI call overhead on tiny request lines where the win
-- is < 16 bytes scanned). For lines this short, an inlined loop wins.
{-# INLINE findByte #-}
findByte :: ByteString -> Int -> Word8 -> Int
findByte bs !s !needle = go s
  where
    !len = BS.length bs
    go !i
      | i >= len = len
      | BSU.unsafeIndex bs i == needle = i
      | otherwise = go (i + 1)

------------------------------------------------------------------------
-- High-level parsers
------------------------------------------------------------------------

-- | Parse a single HTTP\/1.x request out of a header-block slice (i.e.
-- everything up to but not including the @\\r\\n\\r\\n@ terminator).
-- The returned 'Body' is always 'BodyEmpty' — the connection layer
-- replaces it with a streaming producer once the framing is known.
--
-- Also enforces the RFC 9112 § 3.2 Host requirements on HTTP\/1.1:
-- requests MUST carry exactly one @Host@ header. Requests with zero or
-- more than one fail with 'ParseMissingHost' \/ 'ParseMultipleHosts'.
parseRequest :: ByteString -> Either ParseError (Request, Framing)
parseRequest block = do
  let (line, rest) = splitFirstLine block
  (meth, tgt, ver) <- parseRequestLine line
  validateTarget meth tgt
  hdrs <- parseHeaderBlock rest
  validateHost ver hdrs
  framing <- requestFraming meth ver hdrs
  Right (Request meth tgt ver hdrs BodyEmpty (pure []), framing)

-- | Like 'parseRequest' but lowercases header names in-place.
-- The input MUST be an owned mutable copy (e.g. from 'BS.copy').
parseRequestLower :: ByteString -> Either ParseError (Request, Framing)
parseRequestLower block = do
  let (line, rest) = splitFirstLine block
  (meth, tgt, ver) <- parseRequestLine line
  validateTarget meth tgt
  hdrs <- parseHeaderBlockLower rest
  validateHost ver hdrs
  framing <- requestFraming meth ver hdrs
  Right (Request meth tgt ver hdrs BodyEmpty (pure []), framing)

-- | RFC 9112 § 3.2: HTTP\/1.1 requests MUST contain exactly one Host
-- header. We also validate the value: no userinfo (@user\@host@), no
-- path (@host\/foo@), no NUL, no whitespace, non-empty.
validateHost :: Version -> Headers -> Either ParseError ()
validateHost HTTP_1_0 _ = Right ()
validateHost HTTP_1_1 hdrs = case hLookupAll "host" hdrs of
  []  -> Left ParseMissingHost
  [_] | Just v <- findHost hdrs ->
        if validHostValue v then Right () else Left ParseInvalidHost
      | otherwise -> Right ()
  vs
    -- Some smuggling attacks send a single "Host: a, b" header as one
    -- value containing a comma — treat that as multiple too.
    | any commaInValue vs -> Left ParseMultipleHosts
    | otherwise -> Left ParseMultipleHosts
  where
    commaInValue v = BS.elem 0x2c v

-- | RFC 9112 § 3.2: @Host = uri-host [ \":\" port ]@. We accept any
-- non-empty sequence of bytes that does not include @\/@ (path),
-- @\@@ (userinfo), NUL, SP, HTAB, or any control byte.
validHostValue :: ByteString -> Bool
validHostValue v
  | BS.null v = False
  | otherwise = BS.all goodByte v
  where
    goodByte b =
         b /= 0x00
      && b /= 0x2f          -- '/'
      && b /= 0x40          -- '@'
      && b /= 0x20          -- SP
      && b /= 0x09          -- HTAB
      && b > 0x1f
      && b /= 0x7f

-- | Reject request targets that carry obviously-bad bytes (NUL, CR,
-- LF, SP) or non-ASCII octets. Per RFC 9112 § 3.2.1 the target is
-- built from URI bytes only.
--
-- Per-method extra constraints (RFC 9112 § 3.2.4 / § 3.2.3):
--
--   * Asterisk-form (@\"*\"@) is restricted to OPTIONS.
--   * CONNECT MUST use authority-form (@host:port@); no scheme, no
--     path, no asterisk. We don't fully URI-parse, just enforce that
--     a colon-separated host:port shape with no '\/' and no '?' is
--     present.
validateTarget :: Method -> RawTarget -> Either ParseError ()
validateTarget meth tgt
  | BS.null tgt = Left ParseInvalidTarget
  | tgt == "*" =
      if meth == OPTIONS then Right () else Left ParseInvalidTarget
  | BS.any badByte tgt = Left ParseInvalidTarget
  | meth == CONNECT = validateAuthorityForm tgt
  | otherwise = Right ()
  where
    badByte b =
         b == 0x00
      || b == 0x20          -- SP (request-line splitter; smuggle vector)
      || b == 0x09          -- HTAB
      || b == 0x0a          -- LF
      || b == 0x0d          -- CR
      || b > 0x7f           -- non-ASCII octet

-- | CONNECT authority-form: @host \":\" port@. No scheme, no path, no
-- query. We check for the presence of @:port@ and the absence of
-- characters that would indicate a different request-target form.
validateAuthorityForm :: ByteString -> Either ParseError ()
validateAuthorityForm tgt
  | BS.elem 0x2f tgt = Left ParseInvalidTarget    -- '/'
  | BS.elem 0x3f tgt = Left ParseInvalidTarget    -- '?'
  | BS.elem 0x23 tgt = Left ParseInvalidTarget    -- '#'
  | BS.elem 0x40 tgt = Left ParseInvalidTarget    -- '@' (userinfo)
  | not (BS.elem 0x3a tgt) = Left ParseInvalidTarget  -- need :port
  | otherwise = Right ()

-- | Parse a single HTTP\/1.x response out of a header-block slice.
parseResponse :: Method -> ByteString -> Either ParseError (Response, Framing)
parseResponse reqMethod block = do
  let (line, rest) = splitFirstLine block
  (ver, st, _reason) <- parseStatusLine line
  hdrs <- parseHeaderBlock rest
  framing <- responseFraming reqMethod ver st hdrs
  Right (Response st ver hdrs BodyEmpty (pure []), framing)

-- | Like 'parseResponse' but lowercases header names in-place.
-- The input MUST be an owned mutable copy.
parseResponseLower :: Method -> ByteString -> Either ParseError (Response, Framing)
parseResponseLower reqMethod block = do
  let (line, rest) = splitFirstLine block
  (ver, st, _reason) <- parseStatusLine line
  hdrs <- parseHeaderBlockLower rest
  framing <- responseFraming reqMethod ver st hdrs
  Right (Response st ver hdrs BodyEmpty (pure []), framing)

splitFirstLine :: ByteString -> (ByteString, ByteString)
splitFirstLine bs =
  let crIdx = findByte bs 0 0x0d
  in if crIdx + 1 >= BS.length bs
       then (bs, BS.empty)
       else
         if BSU.unsafeIndex bs (crIdx + 1) == 0x0a
           then (BSU.unsafeTake crIdx bs, BSU.unsafeDrop (crIdx + 2) bs)
           else (bs, BS.empty)

------------------------------------------------------------------------
-- Framing rules
------------------------------------------------------------------------

-- | Derive a request body framing from headers, per RFC 9112 § 6.3.
--
-- The smuggling guards here are what defines this as a "conformant"
-- parser rather than a permissive one — these are the exact checks
-- that h2o, nghttp2, and Hyper run.
requestFraming :: Method -> Version -> Headers -> Either ParseError Framing
requestFraming meth _ver hdrs
  | not (bodyAllowedInRequest meth) = Right NoBody
  | otherwise = do
      let mTE = findTransferEncoding hdrs
          cls = hLookupAll "content-length" hdrs
      case (mTE, cls) of
        (Just te, []) -> teFraming te
        (Just _,  _:_) -> Left ParseLengthAndTransferEncoding
        (Nothing, []) -> Right NoBody
        (Nothing, [v]) -> singleContentLength v
        (Nothing, v : vs)
          | all (== v) vs -> singleContentLength v
          | otherwise -> Left ParseLengthConflict

-- | Derive a response body framing per RFC 9112 § 6.3.
--
-- The "what method was the request" parameter matters: HEAD and CONNECT
-- responses never carry a body regardless of headers.
responseFraming :: Method -> Version -> Status -> Headers -> Either ParseError Framing
responseFraming HEAD _ _ _ = Right NoBody
responseFraming _ _ (Status sc) _
  | sc == 204 || sc == 304 || (sc >= 100 && sc < 200) = Right NoBody
responseFraming CONNECT _ (Status sc) _
  | sc >= 200 && sc < 300 = Right NoBody  -- tunnel takes over
responseFraming _ ver _ hdrs = do
  let mTE = findTransferEncoding hdrs
      cls = hLookupAll "content-length" hdrs
  case (mTE, cls) of
    (Just te, _) -> teFraming te
    (Nothing, []) ->
      -- RFC 9112 § 6.3: an HTTP/1.0 response without explicit framing
      -- is close-delimited; an HTTP/1.1 response without one is
      -- zero-length (the spec specifically forbids close-delimited
      -- here so receivers know when they've seen the full message).
      Right (if ver == HTTP_1_1 then NoBody else CloseDelimited)
    (Nothing, [v]) -> singleContentLength v
    (Nothing, v : vs)
      | all (== v) vs -> singleContentLength v
      | otherwise -> Left ParseLengthConflict

{-# INLINE singleContentLength #-}
singleContentLength :: ByteString -> Either ParseError Framing
singleContentLength v = case parseDecimalW64 (trimAll v) of
  Just n -> Right (ContentLength n)
  Nothing -> Left ParseInvalidLength

-- | Parse a @Transfer-Encoding@ value into a framing decision. We
-- accept the codings the RFC requires us to recognise (@chunked@,
-- @identity@) and reject anything that puts a non-@chunked@ coding
-- last (per RFC 9112 § 6.1, chunked MUST be the final coding so the
-- recipient can frame).
teFraming :: ByteString -> Either ParseError Framing
teFraming raw =
  let
    parts = map BSC.strip (BSC.split ',' raw)
    nonempty = filter (not . BS.null) parts
  in
    case nonempty of
      [] -> Right NoBody
      xs ->
        let lastCoding = last xs
        in if asciiIeq lastCoding "chunked"
             then Right Chunked
             else Left ParseChunkedNotFinal

{-# INLINE trimAll #-}
trimAll :: ByteString -> ByteString
trimAll = BSC.dropWhile isSp . BSC.dropWhileEnd isSp
  where
    isSp c = c == ' ' || c == '\t'

-- | Parse an unsigned decimal into a 'Word64', refusing overflow and
-- anything that isn't 1+ ASCII digits.
parseDecimalW64 :: ByteString -> Maybe Word64
parseDecimalW64 bs
  | BS.null bs = Nothing
  | otherwise = go 0 0
  where
    !len = BS.length bs
    go !i !acc
      | i >= len = Just acc
      | otherwise =
          let !b = BSU.unsafeIndex bs i
          in if b < 0x30 || b > 0x39
               then Nothing
               else
                 let !d = fromIntegral (b - 0x30) :: Word64
                     !acc' = acc * 10 + d
                 -- overflow guard: max Word64 = 1844...; checking
                 -- acc * 10 + 9 < acc (i.e. acc > maxBound `div` 10)
                 -- is cheap and conservative.
                 in if acc > 1844674407370955160 || (acc == 1844674407370955160 && d > 5)
                      then Nothing
                      else go (i + 1) acc'

------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------

foreign import ccall unsafe "hs_http1_parse_hex"
  c_parse_hex :: Ptr () -> CInt -> CInt -> Ptr Word64 -> Ptr CInt -> CInt

-- | A parsed chunk header. @chunkSize == 0@ marks the last chunk.
data ChunkHeader = ChunkHeader
  { chunkSize :: !Word64
  , chunkHeaderBytes :: !Int
    -- ^ How many bytes of the input the chunk-size + extensions +
    -- terminating CRLF occupy. The body follows immediately after.
  }
  deriving stock (Eq, Show, Generic)

instance NFData ChunkHeader

-- | Parse a chunk header from @bs[offset..]@. Returns the parsed
-- 'ChunkHeader' or 'Nothing' if more input is needed (the caller pulls
-- more bytes from the recv buffer and retries).
parseChunkHeader :: ByteString -> Int -> Either ParseError (Maybe ChunkHeader)
parseChunkHeader bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    alloca $ \outVal -> alloca $ \outConsumed -> do
      let r = c_parse_hex (castPtr p) (fromIntegral off) (fromIntegral len)
                          outVal outConsumed
      case r of
        -1 -> pure (Left ParseChunkTooLarge)
        0  -> pure (Right Nothing)  -- nothing parsed; need more input
        _  -> do
          val <- peek outVal
          consumed <- (fromIntegral :: CInt -> Int) <$> peek outConsumed
          let afterHex = off + consumed
          -- Skip chunk extensions: @;param[=val]*@ until CRLF. We do
          -- not validate extension token syntax (RFC 9112 § 7.1.1
          -- requires us to accept any) but we reject bare CR or LF
          -- inside the extension line.
          let crIdx = findByte bs afterHex 0x0d
          if crIdx >= len
            then pure (Right Nothing)  -- need more input
            else if crIdx + 1 >= len
              then pure (Right Nothing)
              else if BSU.unsafeIndex bs (crIdx + 1) /= 0x0a
                then pure (Left ParseBadChunkHeader)
                else pure (Right (Just (ChunkHeader val (crIdx + 2 - off))))

-- | Parse a chunk-size line (the size hex + optional @;ext=val@
-- extensions) /without/ the trailing CRLF — i.e. exactly what
-- 'Network.HTTP1.Internal.RecvBuffer.recvBufferReadUntilCRLF' returns.
--
-- Returns the parsed size on success, 'Left ParseBadChunkHeader' on
-- garbage, 'Left ParseChunkTooLarge' on >16 hex digits.
--
-- Strict validation per RFC 9112 § 7.1.1:
--
--   chunk        = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
--   chunk-size   = 1*HEXDIG
--   chunk-ext    = *( BWS \";\" BWS chunk-ext-name [ BWS \"=\" BWS chunk-ext-val ] )
--
-- We reject leading whitespace, sign characters, @0x@ prefixes,
-- underscores, trailing whitespace, bare @;@ with no name, and bytes
-- outside the tchar / quoted-string vocabulary inside the extension.
-- Many request-smuggling attacks slip through laxer parsers that
-- accept e.g. \"+10\" or \"3 \" or \"3;\\x07ext\".
parseChunkSize :: ByteString -> Either ParseError Word64
parseChunkSize line = unsafePerformIO $
  BSU.unsafeUseAsCStringLen line $ \(p, len) ->
    alloca $ \outVal -> alloca $ \outConsumed -> do
      let r = c_parse_hex (castPtr p) 0 (fromIntegral len) outVal outConsumed
      case r of
        -1 -> pure (Left ParseChunkTooLarge)
        0  -> pure (Left ParseBadChunkHeader)
        _  -> do
          consumed <- (fromIntegral :: CInt -> Int) <$> peek outConsumed
          if consumed >= len
            then do
              val <- peek outVal
              pure (Right val)
            else
              -- The byte immediately after the hex digits must be
              -- @;@ (start of extensions) — anything else (whitespace,
              -- letters, punctuation) is malformed.
              let !b = BSU.unsafeIndex line consumed
              in if b /= 0x3b
                   then pure (Left ParseBadChunkHeader)
                   else case validateChunkExtensions line (consumed + 1) of
                     Left e -> pure (Left e)
                     Right () -> do
                       val <- peek outVal
                       pure (Right val)

-- | Validate the @chunk-ext@ section that follows a @;@ after the
-- chunk size. We accept a permissive subset of the grammar
--
--   1*( ext-name [ \"=\" ext-value ] [ \";\" ... ] )
--
-- where @ext-name@ is one or more tchars and @ext-value@ is a token
-- or quoted-string. We do /not/ try to parse quoted-string fully;
-- instead we forbid any byte not in field-vchar (CR / LF / NUL /
-- other control bytes) so smuggling attacks can't sneak framing
-- characters in.
validateChunkExtensions :: ByteString -> Int -> Either ParseError ()
validateChunkExtensions bs off0
  | off0 >= len = Left ParseBadChunkHeader  -- "3;" with nothing
  | otherwise = scanName off0
  where
    !len = BS.length bs
    -- Each extension starts with at least one tchar (the ext-name).
    scanName !i
      | i >= len = Left ParseBadChunkHeader
      | otherwise =
          let !nameEnd = findNonToken bs i
          in if nameEnd == i
               then Left ParseBadChunkHeader  -- ";" not followed by tchar
               else scanAfterName nameEnd
    scanAfterName !i
      | i >= len = Right ()
      | otherwise = case BSU.unsafeIndex bs i of
          0x3b -> scanName (i + 1)                     -- another ext
          0x3d -> scanValue (i + 1)                    -- = <value>
          _    -> Left ParseBadChunkHeader
    scanValue !i
      | i >= len = Right ()
      | otherwise = case BSU.unsafeIndex bs i of
          0x22 -> scanQuoted (i + 1)                   -- quoted-string
          _    ->
            -- Bare token value: 1*tchar, then ';' or end-of-line.
            let !valEnd = findNonToken bs i
            in if valEnd == i
                 then Left ParseBadChunkHeader
                 else scanAfterValue valEnd
    scanAfterValue !i
      | i >= len = Right ()
      | BSU.unsafeIndex bs i == 0x3b = scanName (i + 1)
      | otherwise = Left ParseBadChunkHeader
    scanQuoted !i
      | i >= len = Left ParseBadChunkHeader
      | otherwise = case BSU.unsafeIndex bs i of
          0x22 -> scanAfterValue (i + 1)
          0x5c                                          -- '\\' (quoted-pair)
            | i + 1 < len -> scanQuoted (i + 2)
            | otherwise   -> Left ParseBadChunkHeader
          c
            | c == 0x0d || c == 0x0a || c == 0x00 -> Left ParseBadChunkHeader
            | otherwise -> scanQuoted (i + 1)

-- (unused import dance: keep @.&.@ available so future
-- extension parsing has the bit-twiddling primitive in scope.)
_keepBits :: Word64 -> Word64
_keepBits x = x .&. 0xffffffffffffffff
