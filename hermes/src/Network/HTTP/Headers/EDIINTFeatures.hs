{-# LANGUAGE TemplateHaskell #-}

{- |
The @EDIINT-Features@ header field (RFC 6017), used by EDIINT/AS2 agents to
advertise the set of optional EDIINT features they support. Its value is a
non-empty, comma-separated list of feature names:

@
EDIINT-Features = feature-name *([WSP] "," [WSP] feature-name)
feature-name    = 1*(ALPHA / DIGIT / "-")
@

Feature names are case insensitive. The codec retains their spelling and
combines repeated HTTP field lines in wire order.

Spec: <https://www.rfc-editor.org/rfc/rfc6017>

See also: "Network.HTTP.Headers.MIMEVersion", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentDisposition".
-}
module Network.HTTP.Headers.EDIINTFeatures (
  EDIINTFeatures (..),
  ediintFeaturesParser,
  renderEDIINTFeatures,
) where

import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hEDIINTFeatures)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedText, renderSeparated, shortText)


-- | @EDIINT-Features@ value: the non-empty list of advertised feature tokens.
newtype EDIINTFeatures = EDIINTFeatures {ediintFeatures :: NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader EDIINTFeatures where
  type ParseFailure EDIINTFeatures = String
  type Cardinality EDIINTFeatures = 'ZeroOrOne
  type Direction EDIINTFeatures = 'RequestAndResponse


  parseFromHeaders _ = fmap EDIINTFeatures . parseListFieldLines "EDIINT-Features" featureNamesParser


  renderToHeaders _ = encodeCommaSeparatedText . ediintFeatures


  headerName _ = hEDIINTFeatures


ediintFeaturesParser :: ParserT st String EDIINTFeatures
ediintFeaturesParser = EDIINTFeatures <$> featureNamesParser


featureNamesParser :: ParserT st String (NonEmpty ST.ShortText)
featureNamesParser = do
  firstFeature <- ows *> featureName
  rest <- many (wsp *> $(char ',') *> wsp *> featureName)
  ows
  pure (firstFeature :| rest)
  where
    featureName = shortASCIIFromParser_ (asciiAlnumSetBytes featureNameChars)
    wsp = skipSatisfyAscii isOWS <|> pure ()


featureNameChars :: AsciiSet
featureNameChars = asciiSet (alnum <> CharSet.singleton '-')
{-# NOINLINE featureNameChars #-}


renderEDIINTFeatures :: EDIINTFeatures -> M.Builder
renderEDIINTFeatures (EDIINTFeatures (first :| rest)) =
  renderSeparated ", " shortText (first : rest)
