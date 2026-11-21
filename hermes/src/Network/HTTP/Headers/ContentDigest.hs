{- |
RFC 9530 §2 @Content-Digest@ — integrity digest(s) computed over the
actual message content (the bytes on the wire, after any content
coding). Sent on requests and responses, it lets the recipient verify
the content arrived intact. It is an RFC 9651\/8941 Structured Field
Dictionary mapping a digest-algorithm key to its digest value, encoded
as an sf-binary Byte Sequence; each member MAY carry parameters.

== Grammar

@
Content-Digest = sf-dictionary
member         = algorithm \"=\" sf-binary *( \";\" parameter )
sf-binary      = \":\" *base64 \":\"
@

Spec: <https://www.rfc-editor.org/rfc/rfc9530.html#name-the-content-digest-field>

See also: "Network.HTTP.Headers.ReprDigest", "Network.HTTP.Headers.WantContentDigest", "Network.HTTP.Headers.Digest", "Network.HTTP.Headers.ContentMD5".
-}
module Network.HTTP.Headers.ContentDigest (
  ContentDigest (..),
  ContentDigestEntry (..),
  contentDigestEntryList,
  contentDigestParser,
  renderContentDigest,
  renderContentDigestChecked,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentDigest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A single dictionary member: the digest-algorithm key, the digest
bytes (decoded from the sf-binary value), and any sf parameters.
-}
data ContentDigestEntry = ContentDigestEntry
  { contentDigestAlgorithm :: !ShortText
  , contentDigestValue :: !ByteString
  , contentDigestParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | A dictionary with zero or more members. Use 'contentDigestEntryList' to
read both forms.
-}
data ContentDigest
  = ContentDigest {contentDigestEntries :: !(NonEmpty ContentDigestEntry)}
  | ContentDigestEmpty
  deriving stock (Eq, Show)


instance Semigroup ContentDigest where
  ContentDigestEmpty <> other = other
  other <> ContentDigestEmpty = other
  ContentDigest first <> ContentDigest second =
    fromEntries (orderedFieldEntries contentDigestAlgorithm (NE.toList (first <> second)))


contentDigestEntryList :: ContentDigest -> [ContentDigestEntry]
contentDigestEntryList ContentDigestEmpty = []
contentDigestEntryList (ContentDigest entries) = NE.toList entries


instance KnownHeader ContentDigest where
  type ParseFailure ContentDigest = String
  type Cardinality ContentDigest = 'ZeroOrMore
  type Direction ContentDigest = 'RequestAndResponse


  parseFromHeaders _ fields = parseStructuredFieldDictionary "Content-Digest" fields >>= fromDictionary
  renderToHeaders _ ContentDigestEmpty = []
  renderToHeaders _ value = either (const []) (pure . M.toStrictByteString) (renderContentDigestChecked value)


  omitHeaderValue ContentDigestEmpty = True
  omitHeaderValue _ = False
  {-# INLINE omitHeaderValue #-}


  headerName _ = hContentDigest


contentDigestParser :: ParserT st String ContentDigest
contentDigestParser = structuredFieldDictionary >>= either err pure . fromDictionary


fromDictionary :: [StructuredFieldDictionaryEntry] -> Either String ContentDigest
fromDictionary = fmap fromEntries . projectStructuredFieldItems "Content-Digest" accept ContentDigestEntry
  where
    accept (Binary value) = Just value
    accept _ = Nothing


fromEntries :: [ContentDigestEntry] -> ContentDigest
fromEntries = maybe ContentDigestEmpty ContentDigest . NE.nonEmpty


-- | Render the field value. An invalid dictionary produces no bytes.
renderContentDigest :: ContentDigest -> M.Builder
renderContentDigest = either (const mempty) id . renderContentDigestChecked


-- | Validate the final dictionary before writing its canonical field value.
renderContentDigestChecked :: ContentDigest -> Either String M.Builder
renderContentDigestChecked =
  R.renderStructuredFieldItemDictionaryChecked
    contentDigestAlgorithm
    contentDigestParameters
    (Right . R.rfc8941Binary . contentDigestValue)
    . contentDigestEntryList
