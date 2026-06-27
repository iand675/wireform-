{-# LANGUAGE TemplateHaskell #-}

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
  wantContentDigestParser,
  renderWantContentDigest,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
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


-- | A non-empty dictionary of wanted content-digest algorithms.
newtype WantContentDigest = WantContentDigest {wantContentDigestEntries :: NonEmpty WantContentDigestEntry}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader WantContentDigest where
  type ParseFailure WantContentDigest = String
  type Cardinality WantContentDigest = 'ZeroOrMore
  type Direction WantContentDigest = 'RequestAndResponse


  parseFromHeaders _ headers = sconcat <$> traverse runWantContentDigest headers
  renderToHeaders _ = pure . M.toStrictByteString . renderWantContentDigest
  headerName _ = hWantContentDigest


runWantContentDigest :: ByteString -> Either String WantContentDigest
runWantContentDigest bs = case runParser wantContentDigestParser bs of
  OK v rest
    | B.null (dropOws rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Want-Content-Digest: " <> show rest)
  Fail -> Left "Failed to parse Want-Content-Digest header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

wantContentDigestParser :: ParserT st String WantContentDigest
wantContentDigestParser = do
  ows
  WantContentDigest <$> (member `sepBy1` (ows *> $(char ',') *> ows))
  where
    member = do
      k <- sfDictKey
      $(char '=')
      pref <- rfc8941Integer
      ps <- rfc8941Parameters
      pure WantContentDigestEntry {wantContentDigestAlgorithm = k, wantContentDigestPreference = pref, wantContentDigestParameters = ps}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderWantContentDigest :: WantContentDigest -> M.Builder
renderWantContentDigest = M.intersperse ", " . fmap renderEntry . wantContentDigestEntries
  where
    renderEntry (WantContentDigestEntry k p ps) =
      R.shortText k <> M.char7 '=' <> R.rfc8941Integer p <> foldMap renderParam ps
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
