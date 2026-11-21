{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Range@ is a request header by which a client asks for only one or more
sub-ranges of the target resource instead of the whole representation,
typically to resume an interrupted download or to fetch large media
incrementally. The range unit is almost always @bytes@; a server that
honours the request answers @206 Partial Content@ with a matching
@Content-Range@, while one that ignores it returns the full @200@ response.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-14.2>

== Grammar

@
Range            = ranges-specifier
ranges-specifier = range-unit \"=\" range-set
range-set        = 1#range-spec
range-spec       = int-range / suffix-range / other-range
int-range        = first-pos \"-\" [ last-pos ]
suffix-range     = \"-\" suffix-length
other-range      = 1*( %x21-2B / %x2D-7E )  ; visible chars except \",\"
range-unit       = bytes-unit / other-range-unit
bytes-unit       = \"bytes\"
other-range-unit = token
@

The @bytes-unit@ form is the only one in widespread use; this
module supports it natively (typed positions) and surfaces other
units as raw 'RawRange' for callers that need them.

See also: "Network.HTTP.Headers.AcceptRanges", "Network.HTTP.Headers.ContentRange", "Network.HTTP.Headers.IfRange".
-}
module Network.HTTP.Headers.Range (
  Range (..),
  ByteRange (ByteRangeInt, ByteRangeSuffix),
  ResolvedByteRange (..),
  resolveByteRange,
  RawRange (..),
  rangeParser,
  renderRange,
  renderRangeChecked,
  byteRangesParser,
  renderByteRanges,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word64)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRange)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | A single byte-range spec.
data ByteRange
  = ByteRangeClosed {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | ByteRangeOpen {-# UNPACK #-} !Word64
  | -- | @-suffix-length@. The last N bytes.
    ByteRangeSuffix {-# UNPACK #-} !Word64
  deriving stock (Eq)


{- | @first-pos "-" [last-pos]@. @ByteRangeInt 0 (Just 499)@ means bytes
0 to 499 inclusive. @ByteRangeInt 500 Nothing@ means from byte 500 to end.
-}
pattern ByteRangeInt :: Word64 -> Maybe Word64 -> ByteRange
pattern ByteRangeInt first end <- (byteRangeEndpoints -> RangeEndpoints first end)
  where
    ByteRangeInt first (Just end) = ByteRangeClosed first end
    ByteRangeInt first Nothing = ByteRangeOpen first


{-# COMPLETE ByteRangeInt, ByteRangeSuffix #-}


data ByteRangeEndpoints = RangeEndpoints !Word64 !(Maybe Word64) | NoRangeEndpoints


byteRangeEndpoints :: ByteRange -> ByteRangeEndpoints
byteRangeEndpoints (ByteRangeClosed first end) = RangeEndpoints first (Just end)
byteRangeEndpoints (ByteRangeOpen first) = RangeEndpoints first Nothing
byteRangeEndpoints (ByteRangeSuffix _) = NoRangeEndpoints
{-# INLINE byteRangeEndpoints #-}


instance Show ByteRange where
  showsPrec p (ByteRangeInt first end) =
    showParen (p > 10) (showString "ByteRangeInt " . showsPrec 11 first . showChar ' ' . showsPrec 11 end)
  showsPrec p (ByteRangeSuffix suffix) =
    showParen (p > 10) (showString "ByteRangeSuffix " . showsPrec 11 suffix)


-- | An offset and byte count within a representation of known length.
data ResolvedByteRange = ResolvedByteRange
  { resolvedOffset :: {-# UNPACK #-} !Word64
  , resolvedLength :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)


{- | Resolve a byte range against the representation's length.
An inclusive end beyond the length is clipped. An oversized suffix
covers the whole representation. Invalid or unsatisfiable ranges have
no result. A nonzero suffix of an empty representation resolves to
zero bytes at offset zero, as permitted by RFC 9110 §14.1.2.
-}
resolveByteRange :: Word64 -> ByteRange -> Maybe ResolvedByteRange
resolveByteRange total (ByteRangeClosed first lastByte)
  | first < endExclusive = Just (ResolvedByteRange first (endExclusive - first))
  | otherwise = Nothing
  where
    -- Clipping before adding one also handles an inclusive end of maxBound.
    endExclusive = if lastByte < total then lastByte + 1 else total
resolveByteRange total (ByteRangeOpen first)
  | first < total = Just (ResolvedByteRange first (total - first))
  | otherwise = Nothing
resolveByteRange _ (ByteRangeSuffix 0) = Nothing
resolveByteRange total (ByteRangeSuffix suffix) =
  let count = min suffix total
  in Just (ResolvedByteRange (total - count) count)
{-# INLINE resolveByteRange #-}


{- | An entry in a non-bytes range-set. The body is the raw bytes
of a single @other-range@ spec (visible chars excluding @\",\"@).
-}
newtype RawRange = RawRange {rawRangeBytes :: ByteString}
  deriving stock (Eq, Show)


{- | The full @Range@ header value: a unit token plus a non-empty
list of specs.
-}
data Range
  = -- | @bytes=...@ form.
    ByteRanges !(NonEmpty ByteRange)
  | -- | @\<unit\>=...@ for any non-bytes unit.
    OtherRanges !ST.ShortText !(NonEmpty RawRange)
  deriving stock (Eq, Show)


instance KnownHeader Range where
  type ParseFailure Range = String
  type Cardinality Range = 'ZeroOrOne
  type Direction Range = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Range" rangeParser


  renderToHeaders _ = M.toStrictByteString . renderRange


  renderToHeadersChecked _ = either (const Nothing) (Just . M.toStrictByteString) . renderRangeChecked


  headerName _ = hRange


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parser for the full @Range@ value.
rangeParser :: ParserT st String Range
rangeParser = ows *> branch bytesUnit (ByteRanges <$> byteRangesParser) extension
  where
    bytesUnit = $(switchAsciiCI [|case _ of "bytes=" -> pure ()|])
    extension = OtherRanges <$> (rfc9110Token <* $(char '=')) <*> otherRangesParser


{- | The @bytes-unit@ range-set. Exposed so callers can reuse
it after stripping the leading @bytes=@ themselves.
-}
byteRangesParser :: ParserT st String (NonEmpty ByteRange)
byteRangesParser = rangesetSepBy byteRange
  where
    byteRange = branch $(char '-') (ByteRangeSuffix <$> position) intRange
    position = boundedDecimalWord64 (err "Byte range offset or length exceeds Word64")

    intRange = do
      a <- position
      $(char '-')
      withOption position (closed a) (pure (ByteRangeOpen a))

    closed first lastByte
      | lastByte < first = err "Byte range ends before it starts"
      | otherwise = pure (ByteRangeClosed first lastByte)


otherRangesParser :: ParserT st String (NonEmpty RawRange)
otherRangesParser = rangesetSepBy raw
  where
    raw = RawRange <$> byteStringOf (skipSome (skipSatisfyAscii otherRangeChar))


otherRangeChar :: Char -> Bool
otherRangeChar c = (c >= '\x21' && c <= '\x2B') || (c >= '\x2D' && c <= '\x7E')
{-# INLINE otherRangeChar #-}


-- | A non-empty 1#element list with OWS-around-comma separators.
rangesetSepBy :: ParserT st String a -> ParserT st String (NonEmpty a)
rangesetSepBy p = do
  ranges <- httpList p
  case ranges of
    [] -> err "Range requires at least one member"
    first : rest -> pure (first :| rest)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

-- | Render the structural value. For checked emission use 'renderRangeChecked'.
renderRange :: Range -> M.Builder
renderRange = \case
  ByteRanges rs ->
    "bytes=" <> renderByteRanges rs
  OtherRanges unit rs ->
    R.shortText unit
      <> M.char7 '='
      <> R.renderSeparated (M.char7 ',') renderRaw (NE.toList rs)
  where
    renderRaw (RawRange bs) = M.byteString bs


-- | Validate a range-set for emission as a typed request field.
renderRangeChecked :: Range -> Either String M.Builder
renderRangeChecked value@(ByteRanges ranges)
  | all validByteRange ranges = Right (renderRange value)
  | otherwise = Left "Byte range ends before it starts"
  where
    validByteRange (ByteRangeClosed first lastByte) = first <= lastByte
    validByteRange _ = True
renderRangeChecked value@(OtherRanges unit ranges)
  | not (isHttpTokenText unit) || namesBytesUnit = Left "Invalid extension range unit"
  | not (all validRawRange ranges) = Left "Invalid extension range member"
  | otherwise = Right (renderRange value)
  where
    unitBytes = ST.toShortByteString unit
    namesBytesUnit = SBS.length unitBytes == 5 && SBS.map (.|. 0x20) unitBytes == "bytes"
    validRawRange (RawRange bytes) = not (BS.null bytes) && BS.all (otherRangeChar . BSI.w2c) bytes


renderByteRanges :: NonEmpty ByteRange -> M.Builder
renderByteRanges = R.renderSeparated (M.char7 ',') renderByteRange . NE.toList


renderByteRange :: ByteRange -> M.Builder
renderByteRange = \case
  ByteRangeOpen a ->
    M.word64Dec a <> M.char7 '-'
  ByteRangeClosed a b ->
    M.word64Dec a <> M.char7 '-' <> M.word64Dec b
  ByteRangeSuffix n ->
    M.char7 '-' <> M.word64Dec n
