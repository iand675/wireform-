{- |
@Reporting-Endpoints@ response header — names one or more reporting endpoints to
which the user agent may deliver document reports, such as CSP violations
and deprecation notices.

The value is an RFC 8941 structured-field __dictionary__ whose keys are endpoint
names and whose values are 'String's holding the endpoint URL:

@
Reporting-Endpoints: default=\"https://reports.example/default\", backup=\"https://reports.example/backup\"
@

The codec combines field lines in order and retains members whose String values
are URI references. It ignores member parameters. The client resolves relative
references against the response URL and applies its origin trust policy.

Spec: <https://www.w3.org/TR/reporting-1/#header> (Reporting API, §3.2).

See also: "Network.HTTP.Headers.NEL",
"Network.HTTP.Headers.ContentSecurityPolicy",
"Network.HTTP.Headers.ContentSecurityPolicyReportOnly",
"Network.HTTP.Headers.ExpectCT".
-}
module Network.HTTP.Headers.ReportingEndpoints (
  ReportingEndpoints (..),
  ReportingEndpoint (..),
  reportingEndpointsParser,
  renderReportingEndpoints,
  renderReportingEndpointsChecked,
) where

import Data.Either (isRight)
import Data.Maybe (mapMaybe)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (uriReferenceBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReportingEndpoints)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.Parser.Driver (parseByteString)


-- | A single dictionary member: an endpoint name mapped to its URL.
data ReportingEndpoint = ReportingEndpoint
  { reportingEndpointName :: !ShortText
  , reportingEndpointUrl :: !RFC8941String
  }
  deriving stock (Eq, Show)


-- | The @Reporting-Endpoints@ dictionary, preserving member order.
newtype ReportingEndpoints = ReportingEndpoints {reportingEndpoints :: [ReportingEndpoint]}
  deriving stock (Eq, Show)


instance KnownHeader ReportingEndpoints where
  type ParseFailure ReportingEndpoints = String
  type Cardinality ReportingEndpoints = 'ZeroOrOne
  type Direction ReportingEndpoints = 'Response


  parseFromHeaders _ = parseStructuredFieldLines "Reporting-Endpoints" reportingEndpointsParser


  renderToHeaders settings = maybe "" id . renderToHeadersChecked settings


  renderToHeadersChecked _ = either (const Nothing) (Just . M.toStrictByteString) . renderReportingEndpointsChecked


  omitHeaderValue = null . reportingEndpoints
  {-# INLINE omitHeaderValue #-}


  headerName _ = hReportingEndpoints


reportingEndpointsParser :: ParserT st String ReportingEndpoints
reportingEndpointsParser = ReportingEndpoints . mapMaybe endpoint <$> structuredFieldDictionary
  where
    endpoint (StructuredFieldDictionaryEntry name (StructuredItem (StructuredFieldItem (String url) _)))
      | validEndpointURI url = Just (ReportingEndpoint name url)
    endpoint _ = Nothing


validEndpointURI :: RFC8941String -> Bool
validEndpointURI (RFC8941String url) = isRight (parseByteString (uriReferenceBytes <* eof) (ST.toByteString url))


-- | Render the field value. An invalid endpoint produces no bytes.
renderReportingEndpoints :: ReportingEndpoints -> M.Builder
renderReportingEndpoints = either (const mempty) id . renderReportingEndpointsChecked


-- | Require valid dictionary keys and RFC 3986 references for emission.
renderReportingEndpointsChecked :: ReportingEndpoints -> Either String M.Builder
renderReportingEndpointsChecked (ReportingEndpoints members)
  | all validEndpoint normalized = Right (R.renderSeparated ", " renderMember normalized)
  | otherwise = Left "Invalid Reporting-Endpoints name or URI reference"
  where
    normalized = orderedFieldEntries reportingEndpointName members
    validEndpoint (ReportingEndpoint name url) = isStructuredFieldKey name && validEndpointURI url
    -- A valid URI reference contains neither a literal quote nor a backslash.
    renderMember (ReportingEndpoint name (RFC8941String url)) =
      R.shortText name <> "=\"" <> R.shortText url <> "\""
