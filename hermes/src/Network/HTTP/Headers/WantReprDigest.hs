{- |
RFC 9530 §4 @Want-Repr-Digest@ — communicates the sender's desire to
receive (and relative preference among) integrity digests of the
selected /representation/ using particular algorithms; either party
may send it, on a request or response, to ask the peer for a
@Repr-Digest@. It is an
RFC 9651\/8941 Structured Field Dictionary mapping a digest-algorithm
key to an Integer preference; a value of @0@ means \"not acceptable\",
higher values express stronger preference. Each member MAY carry
parameters.

== Grammar

@
Want-Repr-Digest = sf-dictionary
member           = algorithm \"=\" sf-integer *( \";\" parameter )
@

Spec: <https://www.rfc-editor.org/rfc/rfc9530.html#name-the-want-content-digest-and>

See also: "Network.HTTP.Headers.ReprDigest", "Network.HTTP.Headers.WantContentDigest", "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.WantDigest".
-}
module Network.HTTP.Headers.WantReprDigest (
  WantReprDigest (..),
  WantReprDigestEntry (..),
  wantReprDigestEntryList,
  wantReprDigestParser,
  renderWantReprDigest,
  renderWantReprDigestChecked,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hWantReprDigest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A single dictionary member: the digest-algorithm key, the Integer
preference for it, and any sf parameters.
-}
data WantReprDigestEntry = WantReprDigestEntry
  { wantReprDigestAlgorithm :: !ShortText
  , wantReprDigestPreference :: !Int
  , wantReprDigestParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | A dictionary with zero or more members. Use 'wantReprDigestEntryList' to
read both forms.
-}
data WantReprDigest
  = WantReprDigest {wantReprDigestEntries :: !(NonEmpty WantReprDigestEntry)}
  | WantReprDigestEmpty
  deriving stock (Eq, Show)


instance Semigroup WantReprDigest where
  WantReprDigestEmpty <> other = other
  other <> WantReprDigestEmpty = other
  WantReprDigest first <> WantReprDigest second =
    fromEntries (orderedFieldEntries wantReprDigestAlgorithm (NE.toList (first <> second)))


wantReprDigestEntryList :: WantReprDigest -> [WantReprDigestEntry]
wantReprDigestEntryList WantReprDigestEmpty = []
wantReprDigestEntryList (WantReprDigest entries) = NE.toList entries


instance KnownHeader WantReprDigest where
  type ParseFailure WantReprDigest = String
  type Cardinality WantReprDigest = 'ZeroOrMore
  type Direction WantReprDigest = 'RequestAndResponse


  parseFromHeaders _ fields = parseStructuredFieldDictionary "Want-Repr-Digest" fields >>= fromDictionary
  renderToHeaders _ WantReprDigestEmpty = []
  renderToHeaders _ value = either (const []) (pure . M.toStrictByteString) (renderWantReprDigestChecked value)


  omitHeaderValue WantReprDigestEmpty = True
  omitHeaderValue _ = False
  {-# INLINE omitHeaderValue #-}


  headerName _ = hWantReprDigest


wantReprDigestParser :: ParserT st String WantReprDigest
wantReprDigestParser = structuredFieldDictionary >>= either err pure . fromDictionary


fromDictionary :: [StructuredFieldDictionaryEntry] -> Either String WantReprDigest
fromDictionary = fmap fromEntries . projectStructuredFieldItems "Want-Repr-Digest" accept WantReprDigestEntry
  where
    accept (Integer value) | value >= 0 && value <= 10 = Just value
    accept _ = Nothing


fromEntries :: [WantReprDigestEntry] -> WantReprDigest
fromEntries = maybe WantReprDigestEmpty WantReprDigest . NE.nonEmpty


-- | Render the field value. An invalid dictionary produces no bytes.
renderWantReprDigest :: WantReprDigest -> M.Builder
renderWantReprDigest = either (const mempty) id . renderWantReprDigestChecked


-- | Validate the final dictionary before writing its canonical field value.
renderWantReprDigestChecked :: WantReprDigest -> Either String M.Builder
renderWantReprDigestChecked =
  R.renderStructuredFieldItemDictionaryChecked
    wantReprDigestAlgorithm
    wantReprDigestParameters
    renderPreference
    . wantReprDigestEntryList
  where
    renderPreference entry
      | preference >= 0 && preference <= 10 = Right (R.rfc8941Integer preference)
      | otherwise = Left "Want-Repr-Digest preference must be between 0 and 10"
      where
        preference = wantReprDigestPreference entry
