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
Want-Digest = #( digest-algorithm [ \";\" \"q\" \"=\" qvalue ] )
qvalue      = ( \"0\" [ \".\" 0*3DIGIT ] ) \/ ( \"1\" [ \".\" 0*3(\"0\") ] )
@

Spec: <https://www.rfc-editor.org/rfc/rfc3230#section-4.3.1>

See also: "Network.HTTP.Headers.Digest", "Network.HTTP.Headers.WantContentDigest", "Network.HTTP.Headers.WantReprDigest", "Network.HTTP.Headers.ContentDigest".
-}
module Network.HTTP.Headers.WantDigest (
  WantDigest (..),
  WantDigestValue (..),
  wantDigestValueList,
  wantDigestQuality,
  wantDigestParser,
  renderWantDigest,
) where

import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (qualityValueParser)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeCommaSeparatedPairs, renderHttpWordValue, renderSeparated, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hWantDigest)
import qualified Network.HTTP.Headers.Mason as M
import Wireform.Parser.Driver (parseByteString)


{- | A wanted digest algorithm together with an optional @q@-value
preference (preserved verbatim).
-}
data WantDigestValue = WantDigestValue
  { wantDigestAlgorithm :: !ShortText
  , wantDigestQ :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)


-- | An explicitly empty field or a non-empty list of wanted algorithms.
-- Use 'wantDigestValueList' when the field may be empty.
data WantDigest
  = WantDigest {wantDigestValues :: NE.NonEmpty WantDigestValue}
  | WantDigestEmpty
  deriving stock (Eq, Show)


instance Semigroup WantDigest where
  WantDigestEmpty <> wanted = wanted
  wanted <> WantDigestEmpty = wanted
  WantDigest left <> WantDigest right = WantDigest (left <> right)


instance KnownHeader WantDigest where
  type ParseFailure WantDigest = String
  type Cardinality WantDigest = 'ZeroOrMore
  type Direction WantDigest = 'RequestAndResponse


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Want-Digest" (httpList wantDigestEntry)
  renderToHeaders _ = pure . encodeCommaSeparatedPairs ";q=" wantDigestAlgorithm wantDigestQ . wantDigestValueList
  headerName _ = hWantDigest


fromEntries :: [WantDigestValue] -> WantDigest
fromEntries = maybe WantDigestEmpty WantDigest . NE.nonEmpty


-- | Return the requested algorithms, including an empty list for a blank field.
wantDigestValueList :: WantDigest -> [WantDigestValue]
wantDigestValueList WantDigestEmpty = []
wantDigestValueList (WantDigest entries) = NE.toList entries


-- | Interpret the entry's quality, defaulting to one when omitted.
-- Returns 'Nothing' for a malformed value supplied via the raw constructor.
wantDigestQuality :: WantDigestValue -> Maybe Double
wantDigestQuality (WantDigestValue _ Nothing) = Just 1
wantDigestQuality (WantDigestValue _ (Just wire)) =
  either (const Nothing) Just (parseByteString (qualityValueParser <* eof) (ST.toByteString wire))


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

wantDigestParser :: ParserT st String WantDigest
wantDigestParser = fromEntries <$> httpList wantDigestEntry


wantDigestEntry :: ParserT st String WantDigestValue
wantDigestEntry =
  WantDigestValue <$> rfc9110Token
    <*> withOption (ows *> $(char ';') *> ows) (const (Just <$> preference)) (pure Nothing)
  where
    preference = do
      $(char 'q') <|> $(char 'Q')
      ows *> $(char '=') *> ows
      shortASCIIFromParser_ qualityValueParser


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderWantDigest :: WantDigest -> M.Builder
renderWantDigest = renderSeparated ", " renderEntry . wantDigestValueList
  where
    renderEntry (WantDigestValue alg mq) =
      shortText alg <> maybe mempty (\q -> ";q=" <> renderHttpWordValue q) mq
