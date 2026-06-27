{- |
RFC 2295 §8.2 @Accept-Features@ — request header by which a user agent
reports the presence or absence of feature tags in its feature set, for
use by a remote variant selection algorithm.

== Grammar

@
Accept-Features = \"Accept-Features\" \":\" #( feature-expr *( \";\" feature-extension ) )
feature-expr    = [ \"!\" ] ftag
                | ftag ( \"=\" | \"!=\" ) tag-value
                | ftag \"=\" \"{\" tag-value \"}\"
                | \"*\"
feature-extension = token [ \"=\" ( token | quoted-string ) ]
@

The feature-predicate grammar is intricate (negation, the @=@, @!=@ and
@={…}@ value operators, the @*@ wildcard, plus @;@-separated
feature-extensions), and feature tags carry application-defined values.
Rather than fabricate a structured model of this experimental grammar we
preserve the field value verbatim as a faithful, round-tripping
'ST.ShortText'.

Spec: <https://www.rfc-editor.org/rfc/rfc2295#section-8.2>

See also: "Network.HTTP.Headers.Alternates", "Network.HTTP.Headers.Negotiate", "Network.HTTP.Headers.TCN", "Network.HTTP.Headers.VariantVary", "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptFeatures (
  AcceptFeatures (..),
  acceptFeaturesParser,
  renderAcceptFeatures,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptFeatures)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of an @Accept-Features@ header.
newtype AcceptFeatures = AcceptFeatures {rawAcceptFeatures :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader AcceptFeatures where
  type ParseFailure AcceptFeatures = String
  type Cardinality AcceptFeatures = 'ZeroOrOne
  type Direction AcceptFeatures = 'Request


  parseFromHeaders _ headers = case runParser acceptFeaturesParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Accept-Features: " <> show rest)
    Fail -> Left "Failed to parse Accept-Features header"
    Err err -> Left err


  renderToHeaders _ = M.toStrictByteString . renderAcceptFeatures


  headerName _ = hAcceptFeatures


acceptFeaturesParser :: ParserT st String AcceptFeatures
acceptFeaturesParser = AcceptFeatures <$> takeRestShortText


renderAcceptFeatures :: AcceptFeatures -> M.Builder
renderAcceptFeatures = shortText . rawAcceptFeatures
