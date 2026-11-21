{- | The @If-Range@ precondition request header lets a client pair a @Range@
request with a single validator so the range is honored only while the
representation is unchanged. If the supplied entity-tag or HTTP-date still
matches, the server returns @206 (Partial Content)@ with just the requested
bytes; otherwise it ignores the @Range@ and returns the full @200 (OK)@
representation. This keeps an interrupted download consistent in one round trip.

@
If-Range = entity-tag / HTTP-date
@

Entity-tags are reused from "Network.HTTP.Headers.ETag" and HTTP-dates from
"Network.HTTP.Headers.Date".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5>

See also: "Network.HTTP.Headers.Range", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfUnmodifiedSince".
-}
module Network.HTTP.Headers.IfRange (
  IfRange (..),
  ifRangeMatches,
  ifRangeParser,
  renderIfRange,
  renderIfRangeChecked,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (pokeByteOff)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate, renderDateBytesChecked, renderDateChecked)
import Network.HTTP.Headers.ETag (EntityTag (..), entityTagParser, isValidOpaqueTag, renderEntityTag, strongMatch)
import Network.HTTP.Headers.HeaderFieldName (hIfRange)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | The parsed value of an @If-Range@ header: either an entity-tag validator
or an HTTP-date validator.
-}
data IfRange
  = IfRangeETag EntityTag
  | IfRangeDate UTCTime
  deriving stock (Eq, Show)


{- | Evaluate an If-Range validator for a selected representation.
Supply its entity tag and its Last-Modified date if that date is a
strong validator under RFC 9110 §8.8.2.2. Entity tags must match
strongly. Dates must match exactly. A failed condition asks the
recipient to ignore Range, as specified in §13.1.5.
-}
ifRangeMatches :: Maybe EntityTag -> Maybe UTCTime -> IfRange -> Bool
ifRangeMatches tag strongDate = \case
  IfRangeETag expected -> matchTag expected
  IfRangeDate expected -> matchDate expected
  where
    matchTag = maybe (const False) strongMatch tag
    matchDate = maybe (const False) (==) strongDate
{-# INLINE ifRangeMatches #-}


instance KnownHeader IfRange where
  type ParseFailure IfRange = String
  type Cardinality IfRange = 'ZeroOrOne
  type Direction IfRange = 'Request


  parseFromHeaders settings = parseSingleFieldLine "If-Range" (ifRangeParserWith (dateParserWith settings))


  renderToHeaders _ = M.toStrictByteString . renderIfRange


  renderToHeadersChecked _ (IfRangeETag (StrongETagBytes bytes))
    | isValidOpaqueTag bytes = Just (encodeStrongTag bytes)
  renderToHeadersChecked _ (IfRangeETag _) = Nothing
  renderToHeadersChecked _ (IfRangeDate t) = renderDateBytesChecked t


  headerName _ = hIfRange


{- | Entity-tags begin with @"@ or the @W/@ marker, neither of which can start
an HTTP-date, so the entity-tag branch is tried first and falls through to
the date parser without ambiguity.
-}
ifRangeParser :: ParserT st String IfRange
ifRangeParser = ifRangeParserWith dateParser


ifRangeParserWith :: ParserT st String UTCTime -> ParserT st String IfRange
ifRangeParserWith parseDate =
  (IfRangeETag <$> entityTagParser)
    <|> (IfRangeDate <$> parseDate)


-- | Render the structural value. For checked emission use 'renderIfRangeChecked'.
renderIfRange :: IfRange -> M.Builder
renderIfRange = \case
  IfRangeETag tag -> renderEntityTag tag
  IfRangeDate t -> renderDate t


{- | Serialize a valid strong entity tag or a representable HTTP-date. The
caller must establish date strength and supply the accompanying Range field.
-}
renderIfRangeChecked :: IfRange -> Either String M.Builder
renderIfRangeChecked (IfRangeETag tag) = case tag of
  StrongETagBytes bytes
    | isValidOpaqueTag bytes -> Right (renderEntityTag tag)
    | otherwise -> Left "Invalid opaque If-Range entity tag"
  WeakETagBytes _ -> Left "If-Range requires a strong entity tag"
renderIfRangeChecked (IfRangeDate t) = maybe (Left "Invalid If-Range HTTP-date") Right (renderDateChecked t)


encodeStrongTag :: SBS.ShortByteString -> ByteString
encodeStrongTag bytes = BSI.unsafeCreate (BSI.checkedAdd "If-Range" size 2) $ \out -> do
  pokeByteOff out 0 (34 :: Word8)
  SBSI.copyToPtr bytes 0 (out `plusPtr` 1) size
  pokeByteOff out (size + 1) (34 :: Word8)
  where
    size = SBS.length bytes
