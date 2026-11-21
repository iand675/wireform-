{-# LANGUAGE BangPatterns #-}

{- |
@Accept-Charset@ — a request header by which a client indicated which
character encodings (charsets) it was willing to accept in the
response, each with an optional @;q=@ quality weight.

RFC 9110 deprecates this field because UTF-8 is nearly ubiquitous and
detailed charset lists enable fingerprinting. The field still appears
on the wire for legacy services.

Wire syntax mirrors "Network.HTTP.Headers.AcceptLanguage": a comma-separated list of charset
tokens, each optionally followed by @;q=value@. The token @*@ is allowed
and stands for \"any other charset not explicitly listed\".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.2>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptCharset (
  AcceptCharset (..),
  WeightedCharset (..),
  acceptedCharsetQuality,
  preferredCharset,
  acceptCharsetParser,
  renderAcceptCharset,
) where

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Maybe (listToMaybe)
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (
  encodeWeightedTokensChecked,
  negotiationList,
  normalizedQuality,
  parseNegotiationHeaders,
  preferredByQuality,
  renderWeight,
  sameNegotiationToken,
  weightParser,
 )
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptCharset)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A charset token (e.g. @utf-8@) plus its quality weight.  The
@*@ wildcard is represented by the literal short text @"*"@.
-}
data WeightedCharset = WeightedCharset
  { charsetTag :: !ST.ShortText
  , charsetWeight :: !Double
  }
  deriving stock (Eq, Show)


newtype AcceptCharset = AcceptCharset
  { acceptCharset :: [WeightedCharset]
  }
  deriving stock (Eq, Show)


{- | Rate a concrete charset name. Named entries compare case-insensitively
and take precedence over the wildcard, including when their quality is zero.
The first occurrence of a name or wildcard supplies its quality.

An absent field gives every charset quality one. An empty field or an
unmatched name without a wildcard gives it zero. Authored weights are
clamped to @[0,1]@, with NaN treated as zero.
-}
acceptedCharsetQuality :: Maybe AcceptCharset -> ST.ShortText -> Double
acceptedCharsetQuality Nothing _ = 1
acceptedCharsetQuality (Just (AcceptCharset ranges)) candidate = charsetQuality ranges candidate
{-# INLINE acceptedCharsetQuality #-}


-- | Select the available charset with the highest positive quality.
-- Order the available list by server preference to break ties.
preferredCharset :: Maybe AcceptCharset -> [ST.ShortText] -> Maybe ST.ShortText
preferredCharset Nothing = listToMaybe
preferredCharset (Just (AcceptCharset ranges)) = chooseCharset ranges
{-# INLINE preferredCharset #-}


chooseCharset :: [WeightedCharset] -> [ST.ShortText] -> Maybe ST.ShortText
chooseCharset ranges = preferredByQuality (charsetQuality ranges)


charsetQuality :: [WeightedCharset] -> ST.ShortText -> Double
charsetQuality [] _ = 0
charsetQuality ranges candidate = findWeight (-1) ranges
  where
    !candidateLength = SBS.length (ST.toShortByteString candidate)

    -- A negative fallback means that no wildcard has been seen.
    findWeight !fallback [] = max 0 fallback
    findWeight !fallback (WeightedCharset tag weight : rest)
      | tagLength == 1 && SBSI.unsafeIndex bytes 0 == 42 =
          findWeight (if fallback < 0 then normalizedQuality weight else fallback) rest
      | tagLength == candidateLength && sameNegotiationToken tag candidate = normalizedQuality weight
      | otherwise = findWeight fallback rest
      where
        bytes = ST.toShortByteString tag
        tagLength = SBS.length bytes


instance KnownHeader AcceptCharset where
  type ParseFailure AcceptCharset = String
  type Cardinality AcceptCharset = 'ZeroOrOne
  type Direction AcceptCharset = 'Request


  parseFromHeaders _ = fmap AcceptCharset . parseNegotiationHeaders "Accept-Charset" listParser


  renderToHeaders _ = M.toStrictByteString . renderAcceptCharset


  renderToHeadersChecked _ =
    encodeWeightedTokensChecked (isHttpTokenText . charsetTag) charsetTag charsetWeight . acceptCharset


  headerName _ = hAcceptCharset


acceptCharsetParser :: ParserT st String AcceptCharset
acceptCharsetParser = AcceptCharset <$> listParser


listParser :: ParserT st String [WeightedCharset]
listParser = negotiationList weightedCharsetParser
  where
    weightedCharsetParser = do
      tag <- rfc9110Token
      w <- weightParser
      pure $ WeightedCharset tag w


renderWeightedCharset :: WeightedCharset -> M.Builder
renderWeightedCharset (WeightedCharset tag w) =
  shortText tag
    <> renderWeight w


renderAcceptCharset :: AcceptCharset -> M.Builder
renderAcceptCharset (AcceptCharset xs) =
  M.intersperse ", " (map renderWeightedCharset xs)
