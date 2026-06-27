{-# LANGUAGE TemplateHaskell #-}

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
  wantReprDigestParser,
  renderWantReprDigest,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
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


-- | A non-empty dictionary of wanted representation-digest algorithms.
newtype WantReprDigest = WantReprDigest {wantReprDigestEntries :: NonEmpty WantReprDigestEntry}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader WantReprDigest where
  type ParseFailure WantReprDigest = String
  type Cardinality WantReprDigest = 'ZeroOrMore
  type Direction WantReprDigest = 'RequestAndResponse


  parseFromHeaders _ headers = sconcat <$> traverse runWantReprDigest headers
  renderToHeaders _ = pure . M.toStrictByteString . renderWantReprDigest
  headerName _ = hWantReprDigest


runWantReprDigest :: ByteString -> Either String WantReprDigest
runWantReprDigest bs = case runParser wantReprDigestParser bs of
  OK v rest
    | B.null (dropOws rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Want-Repr-Digest: " <> show rest)
  Fail -> Left "Failed to parse Want-Repr-Digest header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

wantReprDigestParser :: ParserT st String WantReprDigest
wantReprDigestParser = do
  ows
  WantReprDigest <$> (member `sepBy1` (ows *> $(char ',') *> ows))
  where
    member = do
      k <- sfDictKey
      $(char '=')
      pref <- rfc8941Integer
      ps <- rfc8941Parameters
      pure WantReprDigestEntry {wantReprDigestAlgorithm = k, wantReprDigestPreference = pref, wantReprDigestParameters = ps}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderWantReprDigest :: WantReprDigest -> M.Builder
renderWantReprDigest = M.intersperse ", " . fmap renderEntry . wantReprDigestEntries
  where
    renderEntry (WantReprDigestEntry k p ps) =
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
