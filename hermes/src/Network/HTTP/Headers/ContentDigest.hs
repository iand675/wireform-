{-# LANGUAGE TemplateHaskell #-}

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
  contentDigestParser,
  renderContentDigest,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
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


-- | A non-empty dictionary of content digests.
newtype ContentDigest = ContentDigest {contentDigestEntries :: NonEmpty ContentDigestEntry}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader ContentDigest where
  type ParseFailure ContentDigest = String
  type Cardinality ContentDigest = 'ZeroOrMore
  type Direction ContentDigest = 'RequestAndResponse


  parseFromHeaders _ headers = sconcat <$> traverse runContentDigest headers
  renderToHeaders _ = pure . M.toStrictByteString . renderContentDigest
  headerName _ = hContentDigest


runContentDigest :: ByteString -> Either String ContentDigest
runContentDigest bs = case runParser contentDigestParser bs of
  OK v rest
    | B.null (dropOws rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Content-Digest: " <> show rest)
  Fail -> Left "Failed to parse Content-Digest header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

contentDigestParser :: ParserT st String ContentDigest
contentDigestParser = do
  ows
  ContentDigest <$> (member `sepBy1` (ows *> $(char ',') *> ows))
  where
    member = do
      k <- sfDictKey
      $(char '=')
      v <- sfBinary
      ps <- rfc8941Parameters
      pure ContentDigestEntry {contentDigestAlgorithm = k, contentDigestValue = v, contentDigestParameters = ps}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderContentDigest :: ContentDigest -> M.Builder
renderContentDigest = M.intersperse ", " . fmap renderEntry . contentDigestEntries
  where
    renderEntry (ContentDigestEntry k v ps) =
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
