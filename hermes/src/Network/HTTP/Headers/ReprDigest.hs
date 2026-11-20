{- |
RFC 9530 §3 @Repr-Digest@ — integrity digest(s) computed over the
selected /representation/ (the content together with the
representation metadata used to interpret it), independent of any
content coding applied for transfer. Sent on requests and responses,
it lets the recipient verify the representation's integrity. It is an
RFC 9651\/8941 Structured Field Dictionary mapping a digest-algorithm
key to its digest value, encoded as an sf-binary Byte Sequence; each
member MAY carry parameters.

== Grammar

@
Repr-Digest = sf-dictionary
member      = algorithm \"=\" sf-binary *( \";\" parameter )
sf-binary   = \":\" *base64 \":\"
@

Spec: <https://www.rfc-editor.org/rfc/rfc9530.html#name-the-repr-digest-field>

See also: "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.WantReprDigest", "Network.HTTP.Headers.Digest", "Network.HTTP.Headers.ContentEncoding".
-}
module Network.HTTP.Headers.ReprDigest (
  ReprDigest (..),
  ReprDigestEntry (..),
  reprDigestEntryList,
  reprDigestParser,
  renderReprDigest,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReprDigest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A single dictionary member: the digest-algorithm key, the digest
bytes (decoded from the sf-binary value), and any sf parameters.
-}
data ReprDigestEntry = ReprDigestEntry
  { reprDigestAlgorithm :: !ShortText
  , reprDigestValue :: !ByteString
  , reprDigestParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | A dictionary with zero or more members. Use 'reprDigestEntryList' to
read both forms.
-}
data ReprDigest
  = ReprDigest {reprDigestEntries :: !(NonEmpty ReprDigestEntry)}
  | ReprDigestEmpty
  deriving stock (Eq, Show)


instance Semigroup ReprDigest where
  ReprDigestEmpty <> other = other
  other <> ReprDigestEmpty = other
  ReprDigest first <> ReprDigest second =
    fromEntries (orderedFieldEntries reprDigestAlgorithm (NE.toList (first <> second)))


reprDigestEntryList :: ReprDigest -> [ReprDigestEntry]
reprDigestEntryList ReprDigestEmpty = []
reprDigestEntryList (ReprDigest entries) = NE.toList entries


instance KnownHeader ReprDigest where
  type ParseFailure ReprDigest = String
  type Cardinality ReprDigest = 'ZeroOrMore
  type Direction ReprDigest = 'RequestAndResponse


  parseFromHeaders _ fields = parseStructuredFieldDictionary "Repr-Digest" fields >>= fromDictionary
  renderToHeaders _ ReprDigestEmpty = []
  renderToHeaders _ value = [M.toStrictByteString (renderReprDigest value)]


  omitHeaderValue ReprDigestEmpty = True
  omitHeaderValue _ = False
  {-# INLINE omitHeaderValue #-}


  headerName _ = hReprDigest


reprDigestParser :: ParserT st String ReprDigest
reprDigestParser = structuredFieldDictionary >>= either err pure . fromDictionary


fromDictionary :: [StructuredFieldDictionaryEntry] -> Either String ReprDigest
fromDictionary = fmap fromEntries . projectStructuredFieldItems "Repr-Digest" accept ReprDigestEntry
  where
    accept (Binary value) = Just value
    accept _ = Nothing


fromEntries :: [ReprDigestEntry] -> ReprDigest
fromEntries = maybe ReprDigestEmpty ReprDigest . NE.nonEmpty


renderReprDigest :: ReprDigest -> M.Builder
renderReprDigest = M.intersperse ", " . map renderEntry . orderedFieldEntries reprDigestAlgorithm . reprDigestEntryList
  where
    renderEntry entry =
      R.shortText (reprDigestAlgorithm entry)
        <> M.char7 '='
        <> R.rfc8941Binary (reprDigestValue entry)
        <> foldMap renderParam (orderedFieldEntries fst (reprDigestParameters entry))
    renderParam (key, parameter) = R.renderStructuredFieldParameter key (maybe (Boolean True) id parameter)
