{-# LANGUAGE TemplateHaskell #-}

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
  reprDigestParser,
  renderReprDigest,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
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


-- | A non-empty dictionary of representation digests.
newtype ReprDigest = ReprDigest {reprDigestEntries :: NonEmpty ReprDigestEntry}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader ReprDigest where
  type ParseFailure ReprDigest = String
  type Cardinality ReprDigest = 'ZeroOrMore
  type Direction ReprDigest = 'RequestAndResponse


  parseFromHeaders _ headers = sconcat <$> traverse runReprDigest headers
  renderToHeaders _ = pure . M.toStrictByteString . renderReprDigest
  headerName _ = hReprDigest


runReprDigest :: ByteString -> Either String ReprDigest
runReprDigest bs = case runParser reprDigestParser bs of
  OK v rest
    | B.null (dropOws rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Repr-Digest: " <> show rest)
  Fail -> Left "Failed to parse Repr-Digest header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

reprDigestParser :: ParserT st String ReprDigest
reprDigestParser = do
  ows
  ReprDigest <$> (member `sepBy1` (ows *> $(char ',') *> ows))
  where
    member = do
      k <- sfDictKey
      $(char '=')
      v <- sfBinary
      ps <- rfc8941Parameters
      pure ReprDigestEntry {reprDigestAlgorithm = k, reprDigestValue = v, reprDigestParameters = ps}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderReprDigest :: ReprDigest -> M.Builder
renderReprDigest = M.intersperse ", " . fmap renderEntry . reprDigestEntries
  where
    renderEntry (ReprDigestEntry k v ps) =
      R.shortText k <> M.char7 '=' <> renderSfBinary v <> foldMap renderParam ps
    renderParam (k, mv) = R.rfc8941Parameter R.IncludeIfEmpty R.rfc8941ItemValue k mv


-- ---------------------------------------------------------------------------
-- Structured-field helpers
-- ---------------------------------------------------------------------------

{- | RFC 8941 §3.1.2 dictionary @key@:
@( lcalpha \/ \"*\" ) *( lcalpha \/ DIGIT \/ \"_\" \/ \"-\" \/ \".\" \/ \"*\" )@.
-}
sfDictKey :: ParserT st e ShortText
sfDictKey =
  shortASCIIFromParser_ $
    skipSatisfyAscii (`CharSet.member` firstChar) *> skipMany (skipSatisfyAscii (`CharSet.member` restChar))
  where
    firstChar = CharSet.range 'a' 'z' <> CharSet.singleton '*'
    restChar = CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "_-.*"


{- | RFC 8941 §3.3.5 sf-binary @\":\" *base64 \":\"@. The shared
'rfc8941Binary' parser consumes the opening colon and the base64 body
but leaves the trailing colon in place, so we consume it here.
-}
sfBinary :: ParserT st String ByteString
sfBinary = rfc8941Binary <* $(char ':')


-- | Render bytes as a complete sf-binary, both colons included.
renderSfBinary :: ByteString -> M.Builder
renderSfBinary bs = R.rfc8941Binary bs <> M.char7 ':'
