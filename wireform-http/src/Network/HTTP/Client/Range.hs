{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Range request and partial-content helpers (RFC 9110 §14).

The wire grammar of @Range@ \/ @Content-Range@ \/ @Accept-Ranges@
lives in hermes ("Network.HTTP.Headers.Range",
"Network.HTTP.Headers.ContentRange",
"Network.HTTP.Headers.AcceptRanges"); this module is the
wireform-flavoured wrapper layer:

* The 'ByteRange' ADT mirrors hermes's @bytes-unit@ shape with the
  more ergonomic @from\/to\/suffix@ split that callers usually
  want.
* 'parseRange' \/ 'parseContentRange' delegate to the hermes
  parsers but project into wireform's vocabulary.
* 'parseAcceptRanges' surfaces the server's @Accept-Ranges@
  advertisement.
* 'parseMultipartByteranges' parses the @multipart\/byteranges@
  body shape (RFC 9110 §14.6) that servers use to return multiple
  ranges in a single 206 response.
* 'withRange' attaches a @Range@ header to a request.
-}
module Network.HTTP.Client.Range (
  -- * Range request header
  ByteRange (..),
  ResolvedByteRange (..),
  Range,
  byteRange,
  byteRangeFrom,
  byteRangeSuffix,
  rangeHeader,
  parseRange,
  resolveByteRange,

  -- * Content-Range response header
  ContentRange (..),
  parseContentRange,
  parseContentRangeFull,
  renderContentRange,

  -- * Accept-Ranges
  AcceptRanges (..),
  parseAcceptRanges,

  -- * Multipart byteranges (RFC 9110 §14.6)
  MultipartByterange (..),
  parseMultipartByteranges,

  -- * Request combinators
  withRange,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text.Short qualified as ST
import Data.Word (Word64, Word8)
import Foreign.Ptr (castPtr)
import Network.HTTP.Client.Request (Request, removeHeader, setHeader)
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.AcceptRanges qualified as HAR
import Network.HTTP.Headers.ContentRange qualified as HCR
import Network.HTTP.Headers.ContentType qualified as HCT
import Network.HTTP.Headers.Mason qualified as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Network.HTTP.Headers.Range (ResolvedByteRange (..))
import Network.HTTP.Headers.Range qualified as HR
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S
import Network.HTTP1.Parser qualified as H1
import System.IO.Unsafe (unsafePerformIO)
import Wireform.FFI (findByte)


-- ---------------------------------------------------------------------------
-- Range request header
-- ---------------------------------------------------------------------------

-- | A single byte-range. Mirrors the wire grammar.
data ByteRange
  = -- | @first-byte\/last-byte@, inclusive on both ends.
    ByteRange !Word64 !Word64
  | {- | @first-byte-/@: from the given offset to the end of the
    resource.
    -}
    ByteRangeFrom !Word64
  | -- | @-N@: the last N bytes of the resource.
    ByteRangeSuffix !Word64
  deriving stock (Eq, Show)


{- | A non-empty list of byte-ranges. The on-the-wire form is
@bytes=...,...@.
-}
type Range = [ByteRange]


byteRange :: Word64 -> Word64 -> ByteRange
byteRange = ByteRange


byteRangeFrom :: Word64 -> ByteRange
byteRangeFrom = ByteRangeFrom


byteRangeSuffix :: Word64 -> ByteRange
byteRangeSuffix = ByteRangeSuffix


{- | Render the byte-range list with the Hermes builder. An empty
list renders as @bytes=@. Use 'withRange' for checked request authoring.
-}
rangeHeader :: Range -> ByteString
rangeHeader rs = case NE.nonEmpty (map toHermesByteRange rs) of
  Nothing -> "bytes="
  Just ne -> M.toStrictByteString (HR.renderRange (HR.ByteRanges ne))


-- | Resolve the range against the selected representation's byte length.
resolveByteRange :: Word64 -> ByteRange -> Maybe ResolvedByteRange
resolveByteRange total = HR.resolveByteRange total . toHermesByteRange
{-# INLINE resolveByteRange #-}


toHermesByteRange :: ByteRange -> HR.ByteRange
toHermesByteRange = \case
  ByteRange a b -> HR.ByteRangeInt a (Just b)
  ByteRangeFrom a -> HR.ByteRangeInt a Nothing
  ByteRangeSuffix n -> HR.ByteRangeSuffix n
{-# INLINE toHermesByteRange #-}


{- | Parse a @Range@ header value. Returns 'Nothing' if the unit
isn't @bytes@ or any range component is malformed.
-}
parseRange :: ByteString -> Maybe Range
parseRange raw = case runParser HR.rangeParser raw of
  OK (HR.ByteRanges ne) leftover
    | BS.null (trim leftover) -> Just (map fromHermes (NE.toList ne))
  _ -> Nothing
  where
    fromHermes = \case
      HR.ByteRangeInt a (Just b) -> ByteRange a b
      HR.ByteRangeInt a Nothing -> ByteRangeFrom a
      HR.ByteRangeSuffix n -> ByteRangeSuffix n
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09


-- ---------------------------------------------------------------------------
-- Content-Range
-- ---------------------------------------------------------------------------

{- | RFC 9110 §14.4: server's response to a satisfied range
request. Mirrors the original wireform shape; for the
unsatisfied-range form (the @*\/N@ payload used on 416) use
'parseContentRangeFull'.
-}
data ContentRange = ContentRange
  { crStart :: !Word64
  , crEnd :: !Word64
  , crTotal :: !(Maybe Word64)
  {- ^ Total resource size. Servers may emit @*@ when the total
  length is unknown.
  -}
  }
  deriving stock (Eq, Show)


renderContentRange :: ContentRange -> ByteString
renderContentRange cr =
  M.toStrictByteString $
    HCR.renderContentRange (toHermes cr)
  where
    toHermes (ContentRange a b mTot) =
      HCR.ContentRange
        { HCR.contentRangeUnit = ST.fromString "bytes"
        , HCR.contentRangeResp = HCR.RangeRespSatisfied a b mTot
        }


{- | Parse a satisfied byte @Content-Range@. Returns 'Nothing' for
malformed input, other range units, and the unsatisfied form (@bytes *\/N@).

For full discrimination (including 416's unsatisfied form),
use 'parseContentRangeFull'.
-}
parseContentRange :: ByteString -> Maybe ContentRange
parseContentRange raw = case parseContentRangeFull raw of
  Just (HCR.ContentRange unit (HCR.RangeRespSatisfied a b mTot))
    | unit == "bytes" || CI.mk (ST.toByteString unit) == "bytes" -> Just (ContentRange a b mTot)
  _ -> Nothing


{- | Full @Content-Range@ parse including the unsatisfied form.
Retains the received unit for callers that recognize extension units.
-}
parseContentRangeFull :: ByteString -> Maybe HCR.ContentRange
parseContentRangeFull wire =
  either (const Nothing) Just (Typed.parseFromHeaders Typed.defaultHeaderSettings (wire :| []))


-- ---------------------------------------------------------------------------
-- Accept-Ranges
-- ---------------------------------------------------------------------------

-- | The server-advertised @Accept-Ranges@ value (RFC 9110 §14.3).
data AcceptRanges
  = {- | The server explicitly disabled range requests (the
    literal @"none"@).
    -}
    AcceptRangesNone
  | {- | Non-empty list of accepted range units in their received spelling.
    Range unit names compare case-insensitively.
    -}
    AcceptRangesUnits ![ByteString]
  deriving stock (Eq, Show)


{- | Parse an @Accept-Ranges@ value. Returns 'Nothing' on
malformed input.
-}
parseAcceptRanges :: ByteString -> Maybe AcceptRanges
parseAcceptRanges raw = case runParser HAR.acceptRangesParser raw of
  OK HAR.AcceptRangesNone leftover
    | BS.null (trim leftover) -> Just AcceptRangesNone
  OK (HAR.AcceptRangesUnits ne) leftover
    | BS.null (trim leftover) ->
        Just (AcceptRangesUnits (map ST.toByteString (NE.toList ne)))
  _ -> Nothing
  where
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09


-- ---------------------------------------------------------------------------
-- Multipart byteranges (RFC 9110 §14.6)
-- ---------------------------------------------------------------------------

{- | One part of a @multipart\/byteranges@ response payload (RFC
9110 §14.6). Carries the part's @Content-Range@ (which tells
the caller which span of the resource the bytes correspond to),
the optional @Content-Type@, and the raw body bytes.
-}
data MultipartByterange = MultipartByterange
  { mbRange :: !ContentRange
  , mbContentType :: !(Maybe ByteString)
  , mbBody :: !ByteString
  }
  deriving stock (Eq, Show)


{- | Parse a multipart/byteranges body with the decoded boundary parameter.
A correctly framed response can contain unusable parts. These are skipped.
Returned parts have one satisfied byte Content-Range and exactly the
number of octets it describes. Identity transfer encodings are accepted.
The opening delimiter selects CRLF or legacy LF framing for the body.
-}
parseMultipartByteranges :: ByteString -> ByteString -> Maybe [MultipartByterange]
parseMultipartByteranges boundary body = do
  guard (validBoundary boundary)
  first <- (BS.stripPrefix marker body >>= parseFirstDelimiter 0)
    <|> nextDelimiter "\n" marker parseFirstDelimiter body
  case first of
    OpenDelimiter _ eol rest -> collect eol rest []
    CloseDelimiter _ -> Nothing
  where
    marker = "--" <> boundary

    collect eol = go
      where
        go input !parts = do
          delimiter <- nextDelimiter eol marker (parseDelimiter eol) input
          case delimiter of
            OpenDelimiter size _ rest -> go rest (keepPart size input parts)
            CloseDelimiter size -> pure (reverse (keepPart size input parts))

    keepPart size input parts =
      case parseOnePart (BS.take size input) of
        Just part -> part : parts
        Nothing -> parts


-- RFC 2046 §5.1.1, bchars and bcharsnospace.
validBoundary :: ByteString -> Bool
validBoundary boundary =
  not (BS.null boundary)
    && BS.length boundary <= 70
    && BS.last boundary /= 0x20
    && BS.all allowed boundary
  where
    allowed w =
      (w >= 0x30 && w <= 0x39)
        || (w >= 0x41 && w <= 0x5A)
        || (w >= 0x61 && w <= 0x7A)
        || BS.elem w "'()+_,-./:=? "


data MultipartDelimiter
  = OpenDelimiter {-# UNPACK #-} !Int !ByteString !ByteString
  | CloseDelimiter {-# UNPACK #-} !Int


-- Every delimiter starts with a line break immediately followed by "--".
-- Look for both bytes in bulk so runs of line breaks or dashes can be skipped.
nextDelimiter :: ByteString -> ByteString -> (Int -> ByteString -> Maybe MultipartDelimiter) -> ByteString -> Maybe MultipartDelimiter
nextDelimiter eol marker parseCandidate input = unsafePerformIO $
  BSU.unsafeUseAsCStringLen input $ \(ptr, _) ->
    pure $! search (castPtr ptr) beforeLf
  where
    beforeLf = BS.length eol - 1
    inputLength = BS.length input
    lastMarkerStart = inputLength - BS.length marker

    search ptr !offset
      | offset >= lastMarkerStart = Nothing
      | otherwise =
          let !lf = findByte ptr offset inputLength 0x0A
          in if lf >= lastMarkerStart
               then Nothing
               else checkMarker ptr (findByte ptr (lf + 1) inputLength 0x2D)

    checkMarker ptr !dash
      | dash > lastMarkerStart = Nothing
      | BSU.unsafeIndex input (dash - 1) /= 0x0A = search ptr dash
      | BSU.unsafeIndex input (dash + 1) /= 0x2D = search ptr dash
      | beforeLf /= 0 && BSU.unsafeIndex input partEnd /= 0x0D = search ptr dash
      | otherwise =
          case BS.stripPrefix marker (BSU.unsafeDrop dash input) >>= parseCandidate partEnd of
            Just delimiter -> Just delimiter
            Nothing -> search ptr dash
      where
        partEnd = dash - 1 - beforeLf


parseFirstDelimiter :: Int -> ByteString -> Maybe MultipartDelimiter
parseFirstDelimiter size suffix =
  parseDelimiter "\r\n" size suffix <|> parseDelimiter "\n" size suffix


parseDelimiter :: ByteString -> Int -> ByteString -> Maybe MultipartDelimiter
parseDelimiter eol size suffix = case BS.stripPrefix "--" suffix of
  Just close ->
    let rest = BS.dropWhile isOws close
    in if BS.null rest then Just (CloseDelimiter size) else CloseDelimiter size <$ BS.stripPrefix eol rest
  Nothing -> OpenDelimiter size eol <$> BS.stripPrefix eol (BS.dropWhile isOws suffix)


parseOnePart :: ByteString -> Maybe MultipartByterange
parseOnePart raw = do
  PartHead headers payload <- parsePartHead raw
  rangeWire <- either (const Nothing) id (H.lookupSingletonHeader H.hContentRange headers)
  range <- parseContentRange rangeWire
  let size = BS.length payload
  guard (size > 0 && crEnd range - crStart range == fromIntegral (size - 1))
  mType <- either (const Nothing) Just (H.lookupSingletonHeader H.hContentType headers)
  traverse_ validateType mType
  guard (identityTransferEncoding headers)
  pure MultipartByterange{mbRange = range, mbContentType = mType, mbBody = payload}
  where
    validateType wire =
      either (const Nothing) (const (Just ()))
        (Typed.parseFromHeaders @HCT.ContentType Typed.defaultHeaderSettings (wire :| []))


identityTransferEncoding :: H.Headers -> Bool
identityTransferEncoding headers = case H.lookupSingletonHeader "Content-Transfer-Encoding" headers of
  Right Nothing -> True
  Right (Just wire) ->
    let encoding = CI.mk wire
    in encoding == "binary" || encoding == "8bit" || encoding == "7bit"
  Left _ -> False


data PartHead = PartHead !H.Headers !ByteString


data MimeLine = MimeLine !ByteString !ByteString


parsePartHead :: ByteString -> Maybe PartHead
parsePartHead = go []
  where
    go fields input = do
      MimeLine line rest <- nextMimeLine input
      if BS.null line
        then pure (PartHead (reverse fields) rest)
        else do
          MimeLine unfolded after <- unfoldMimeLine line rest
          (name, value) <- either (const Nothing) Just (H1.parseHeaderLine unfolded)
          go ((CI.mk name, value) : fields) after


nextMimeLine :: ByteString -> Maybe MimeLine
nextMimeLine bytes = case BS.break (== 0x0A) bytes of
  (line, rest)
    | BS.null rest -> Nothing
    | otherwise -> Just (MimeLine (maybe line id (BS.stripSuffix "\r" line)) (BS.drop 1 rest))


unfoldMimeLine :: ByteString -> ByteString -> Maybe MimeLine
unfoldMimeLine line bytes
  | startsWithOws bytes = gather [line] bytes
  | otherwise = Just (MimeLine line bytes)
  where
    gather fields rest
      | startsWithOws rest = do
          MimeLine continuation after <- nextMimeLine rest
          gather (continuation : fields) after
      | otherwise = Just (MimeLine (BS.concat (reverse fields)) rest)

    startsWithOws input = not (BS.null input) && isOws (BS.head input)


isOws :: Word8 -> Bool
isOws w = w == 0x20 || w == 0x09


-- ---------------------------------------------------------------------------
-- Request combinator
-- ---------------------------------------------------------------------------

{- | Replace the request's @Range@ field. An empty list or a range with
reversed endpoints removes both @Range@ and its @If-Range@ validator.
-}
withRange :: Range -> Request a -> Request a
withRange ranges
  | null ranges || any reversed ranges = removeHeader H.hIfRange . removeHeader H.hRange
  | otherwise = setHeader H.hRange (rangeHeader ranges)
  where
    reversed (ByteRange start end) = start > end
    reversed _ = False


{- | Predicate for @206 Partial Content@ responses, kept here so
the same module that exposes 'ContentRange' also has the
companion status helper.
-}
_status206 :: S.Status -> Bool
_status206 = (== S.status206)
