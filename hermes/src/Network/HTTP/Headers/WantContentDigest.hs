{- |
RFC 9530 §4 @Want-Content-Digest@ — communicates the sender's desire
to receive (and relative preference among) integrity digests of the
content using particular algorithms; either party may send it, on a
request or response, to ask the peer for a @Content-Digest@. It is an
RFC 9651\/8941
Structured Field Dictionary mapping a digest-algorithm key to an
Integer preference; a value of @0@ means \"not acceptable\", higher
values express stronger preference. Each member MAY carry parameters.

== Grammar

@
Want-Content-Digest = sf-dictionary
member              = algorithm \"=\" sf-integer *( \";\" parameter )
@

Spec: <https://www.rfc-editor.org/rfc/rfc9530.html#name-the-want-content-digest-and>

See also: "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.WantReprDigest", "Network.HTTP.Headers.ReprDigest", "Network.HTTP.Headers.WantDigest".
-}
module Network.HTTP.Headers.WantContentDigest (
  WantContentDigest (..),
  WantContentDigestEntry (..),
  wantContentDigestEntryList,
  wantContentDigestParser,
  renderWantContentDigest,
  renderWantContentDigestChecked,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hWantContentDigest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A single dictionary member: the digest-algorithm key, the Integer
preference for it, and any sf parameters.
-}
data WantContentDigestEntry = WantContentDigestEntry
  { wantContentDigestAlgorithm :: !ShortText
  , wantContentDigestPreference :: !Int
  , wantContentDigestParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | A dictionary with zero or more members. Use 'wantContentDigestEntryList' to
read both forms.
-}
data WantContentDigest
  = WantContentDigest {wantContentDigestEntries :: !(NonEmpty WantContentDigestEntry)}
  | WantContentDigestEmpty
  deriving stock (Eq, Show)


instance Semigroup WantContentDigest where
  WantContentDigestEmpty <> other = other
  other <> WantContentDigestEmpty = other
  WantContentDigest first <> WantContentDigest second =
    fromEntries (orderedFieldEntries wantContentDigestAlgorithm (NE.toList (first <> second)))


wantContentDigestEntryList :: WantContentDigest -> [WantContentDigestEntry]
wantContentDigestEntryList WantContentDigestEmpty = []
wantContentDigestEntryList (WantContentDigest entries) = NE.toList entries


instance KnownHeader WantContentDigest where
  type ParseFailure WantContentDigest = String
  type Cardinality WantContentDigest = 'ZeroOrMore
  type Direction WantContentDigest = 'RequestAndResponse


  parseFromHeaders _ fields = parseStructuredFieldDictionary "Want-Content-Digest" fields >>= fromDictionary
  renderToHeaders _ WantContentDigestEmpty = []
  renderToHeaders _ value = either (const []) (pure . M.toStrictByteString) (renderWantContentDigestChecked value)


  omitHeaderValue WantContentDigestEmpty = True
  omitHeaderValue _ = False
  {-# INLINE omitHeaderValue #-}


  headerName _ = hWantContentDigest


wantContentDigestParser :: ParserT st String WantContentDigest
wantContentDigestParser = structuredFieldDictionary >>= either err pure . fromDictionary


fromDictionary :: [StructuredFieldDictionaryEntry] -> Either String WantContentDigest
fromDictionary = fmap fromEntries . projectStructuredFieldItems "Want-Content-Digest" accept WantContentDigestEntry
  where
    accept (Integer value) | value >= 0 && value <= 10 = Just value
    accept _ = Nothing


fromEntries :: [WantContentDigestEntry] -> WantContentDigest
fromEntries = maybe WantContentDigestEmpty WantContentDigest . NE.nonEmpty


-- | Render the field value. An invalid dictionary produces no bytes.
renderWantContentDigest :: WantContentDigest -> M.Builder
renderWantContentDigest = either (const mempty) id . renderWantContentDigestChecked


-- | Validate the final dictionary before writing its canonical field value.
renderWantContentDigestChecked :: WantContentDigest -> Either String M.Builder
renderWantContentDigestChecked =
  R.renderStructuredFieldItemDictionaryChecked
    wantContentDigestAlgorithm
    wantContentDigestParameters
    renderPreference
    . wantContentDigestEntryList
  where
    renderPreference entry
      | preference >= 0 && preference <= 10 = Right (R.rfc8941Integer preference)
      | otherwise = Left "Want-Content-Digest preference must be between 0 and 10"
      where
        preference = wantContentDigestPreference entry
