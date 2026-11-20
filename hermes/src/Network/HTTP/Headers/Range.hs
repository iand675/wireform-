{-# LANGUAGE TemplateHaskell #-}

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
  ByteRange (..),
  RawRange (..),
  rangeParser,
  renderRange,
  byteRangesParser,
  renderByteRanges,
) where

import Data.ByteString (ByteString)
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
  = {- | @first-pos \"-\" [last-pos]@. @ByteRangeInt 0 (Just 499)@
    means \"bytes 0\u2013499 inclusive\"; @ByteRangeInt 500
    Nothing@ means \"from byte 500 to end\".
    -}
    ByteRangeInt !Word64 !(Maybe Word64)
  | -- | @\"-\" suffix-length@. \"The last N bytes\".
    ByteRangeSuffix !Word64
  deriving stock (Eq, Show)


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
      b <- optional position
      case b of
        Just end | end < a -> err "Byte range ends before it starts"
        _ -> pure (ByteRangeInt a b)


otherRangesParser :: ParserT st String (NonEmpty RawRange)
otherRangesParser = rangesetSepBy raw
  where
    raw =
      RawRange
        <$> byteStringOf
          ( skipSome
              ( skipSatisfyAscii
                  ( \c ->
                      (c >= '\x21' && c <= '\x2B')
                        || (c >= '\x2D' && c <= '\x7E')
                  )
              )
          )


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

renderRange :: Range -> M.Builder
renderRange = \case
  ByteRanges rs ->
    "bytes=" <> renderByteRanges rs
  OtherRanges unit rs ->
    R.shortText unit
      <> M.char7 '='
      <> M.intersperse (M.char7 ',') (map renderRaw (NE.toList rs))
  where
    renderRaw (RawRange bs) = M.byteString bs


renderByteRanges :: NonEmpty ByteRange -> M.Builder
renderByteRanges rs =
  M.intersperse (M.char7 ',') (map renderByteRange (NE.toList rs))


renderByteRange :: ByteRange -> M.Builder
renderByteRange = \case
  ByteRangeInt a Nothing ->
    M.word64Dec a <> M.char7 '-'
  ByteRangeInt a (Just b) ->
    M.word64Dec a <> M.char7 '-' <> M.word64Dec b
  ByteRangeSuffix n ->
    M.char7 '-' <> M.word64Dec n
