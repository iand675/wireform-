{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 3230 @Want-Digest@ — a comma-separated list of digest algorithms
the sender wishes to receive, each optionally weighted with a @q@
preference (@qvalue@, @0@–@1@ with up to three fractional digits). It
may be sent on requests and responses.

This field is /obsolete/: RFC 9530 deprecates it in favour of
'Network.HTTP.Headers.WantContentDigest.WantContentDigest' and
'Network.HTTP.Headers.WantReprDigest.WantReprDigest'. The qvalue is
preserved verbatim as a 'ShortText' to keep round-tripping exact.

== Grammar

@
Want-Digest = 1#( digest-algorithm [ \";\" \"q\" \"=\" qvalue ] )
qvalue      = ( \"0\" [ \".\" 0*3DIGIT ] ) \/ ( \"1\" [ \".\" 0*3(\"0\") ] )
@

Spec: <https://www.rfc-editor.org/rfc/rfc3230#section-4.3.1>

See also: "Network.HTTP.Headers.Digest", "Network.HTTP.Headers.WantContentDigest", "Network.HTTP.Headers.WantReprDigest", "Network.HTTP.Headers.ContentDigest".
-}
module Network.HTTP.Headers.WantDigest (
  WantDigest (..),
  WantDigestValue (..),
  wantDigestParser,
  renderWantDigest,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hWantDigest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A wanted digest algorithm together with an optional @q@-value
preference (preserved verbatim).
-}
data WantDigestValue = WantDigestValue
  { wantDigestAlgorithm :: !ShortText
  , wantDigestQ :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)


-- | A non-empty list of wanted digest algorithms.
newtype WantDigest = WantDigest {wantDigestValues :: NonEmpty WantDigestValue}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader WantDigest where
  type ParseFailure WantDigest = String
  type Cardinality WantDigest = 'ZeroOrMore
  type Direction WantDigest = 'RequestAndResponse


  parseFromHeaders _ headers = sconcat <$> traverse runWantDigest headers
  renderToHeaders _ = pure . M.toStrictByteString . renderWantDigest
  headerName _ = hWantDigest


runWantDigest :: ByteString -> Either String WantDigest
runWantDigest bs = case runParser wantDigestParser bs of
  OK v rest
    | B.null (dropOws rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Want-Digest: " <> show rest)
  Fail -> Left "Failed to parse Want-Digest header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

wantDigestParser :: ParserT st String WantDigest
wantDigestParser = do
  ows
  WantDigest <$> (entry `sepBy1` (ows *> $(char ',') *> ows))
  where
    entry = do
      alg <- rfc9110Token
      q <- optional qparam
      pure WantDigestValue {wantDigestAlgorithm = alg, wantDigestQ = q}
    qparam = do
      $(char ';')
      ows
      $(char 'q')
      $(char '=')
      shortASCIIFromParser_ (skipSome (skipSatisfyAscii (`CharSet.member` qvalueCharSet)))
    qvalueCharSet = CharSet.range '0' '9' <> "."


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderWantDigest :: WantDigest -> M.Builder
renderWantDigest = M.intersperse ", " . fmap renderEntry . wantDigestValues
  where
    renderEntry (WantDigestValue alg mq) =
      R.shortText alg <> maybe mempty (\q -> ";q=" <> R.shortText q) mq
