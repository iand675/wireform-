{-# LANGUAGE TemplateHaskell #-}

{- |
The @EDIINT-Features@ header field (RFC 6017), used by EDIINT/AS2 agents to
advertise the set of optional EDIINT features they support. Its value is a
non-empty, comma-separated list of feature tokens:

@
EDIINT-Features = #feature
feature         = "multiple-attachments" / "CEM" / feature-extension
feature-extension = token
@

We capture the list structure directly as a non-empty list of tokens, since the
grammar is a simple, current RFC 9110 token list.

Spec: <https://www.rfc-editor.org/rfc/rfc6017>

See also: "Network.HTTP.Headers.MIMEVersion", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentDisposition".
-}
module Network.HTTP.Headers.EDIINTFeatures (
  EDIINTFeatures (..),
  ediintFeaturesParser,
  renderEDIINTFeatures,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hEDIINTFeatures)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @EDIINT-Features@ value: the non-empty list of advertised feature tokens.
newtype EDIINTFeatures = EDIINTFeatures {ediintFeatures :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader EDIINTFeatures where
  type ParseFailure EDIINTFeatures = String
  type Cardinality EDIINTFeatures = 'ZeroOrOne
  type Direction EDIINTFeatures = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser ediintFeaturesParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing EDIINT-Features header: " <> show rest
    Fail -> Left "Failed to parse EDIINT-Features header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderEDIINTFeatures


  headerName _ = hEDIINTFeatures


ediintFeaturesParser :: ParserT st String EDIINTFeatures
ediintFeaturesParser = do
  firstFeature <- rfc9110Token
  rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
  pure $ EDIINTFeatures (firstFeature NE.:| rest)


renderEDIINTFeatures :: EDIINTFeatures -> M.Builder
renderEDIINTFeatures (EDIINTFeatures fs) = M.intersperse ", " $ map shortText $ NE.toList fs
