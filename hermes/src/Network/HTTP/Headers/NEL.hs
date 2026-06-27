{- |
@NEL@ (Network Error Logging) response header — declares a network error
reporting policy for the origin.

The field value is a JSON object, e.g.

@
NEL: {"report_to":"default","max_age":86400,"include_subdomains":true}
@

Because the payload is JSON rather than an HTTP structured field, we preserve it
faithfully as an opaque 'ST.ShortText' instead of fabricating a JSON grammar
here; callers can hand the raw value to a JSON parser of their choosing. This is
the honest implementation recommended for opaque/host-specific values.

Spec: <https://www.w3.org/TR/network-error-logging/>

See also: "Network.HTTP.Headers.ReportingEndpoints",
"Network.HTTP.Headers.ContentSecurityPolicy",
"Network.HTTP.Headers.ContentSecurityPolicyReportOnly",
"Network.HTTP.Headers.ExpectCT".
-}
module Network.HTTP.Headers.NEL (
  NEL (..),
  nelParser,
  renderNEL,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hNEL)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A @NEL@ header value: the raw JSON policy object, preserved verbatim.
newtype NEL = NEL {nelPolicy :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader NEL where
  type ParseFailure NEL = String
  type Cardinality NEL = 'ZeroOrOne
  type Direction NEL = 'Response


  parseFromHeaders _ headers = case runParser nelParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing NEL header: " <> show rest
    Fail -> Left "Failed to parse NEL header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderNEL


  headerName _ = hNEL


nelParser :: ParserT st String NEL
nelParser = NEL <$> takeRestShortText


renderNEL :: NEL -> M.Builder
renderNEL (NEL v) = shortText v
