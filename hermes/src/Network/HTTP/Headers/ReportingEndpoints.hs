{-# LANGUAGE TemplateHaskell #-}

{- |
@Reporting-Endpoints@ response header — names one or more reporting endpoints to
which the user agent may deliver reports (CSP violations, NEL reports,
deprecation/intervention reports, etc.).

The value is an RFC 8941 structured-field __dictionary__ whose keys are endpoint
names and whose values are 'String's holding the endpoint URL:

@
Reporting-Endpoints: default=\"https://reports.example/default\", backup=\"https://reports.example/backup\"
@

Member order is preserved. Per the spec each value is a structured-field string;
no parameters are defined.

Spec: <https://www.w3.org/TR/reporting-1/#header> (Reporting API, §3.1) and
RFC 8941 §3.2 (Dictionaries).

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
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReportingEndpoints)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


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


  parseFromHeaders _ headers = case runParser reportingEndpointsParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Reporting-Endpoints header: " <> show rest
    Fail -> Left "Failed to parse Reporting-Endpoints header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderReportingEndpoints


  headerName _ = hReportingEndpoints


-- | RFC 8941 §3.1.2 dictionary @key@: @( lcalpha / \"*\" ) *( lcalpha / DIGIT / \"_\" / \"-\" / \".\" / \"*\" )@.
sfKey :: ParserT st e ShortText
sfKey =
  shortASCIIFromParser_
    ( skipSatisfyAscii (`CharSet.member` firstKeyChar)
        *> skipMany (skipSatisfyAscii (`CharSet.member` keyChar))
    )
  where
    firstKeyChar = CharSet.range 'a' 'z' <> CharSet.singleton '*'
    keyChar = CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "_-.*"


reportingEndpointsParser :: ParserT st String ReportingEndpoints
reportingEndpointsParser = do
  ows
  members <- member `sepBy` (ows *> $(char ',') *> ows)
  ows
  pure (ReportingEndpoints members)
  where
    member = do
      name <- sfKey
      $(char '=')
      ReportingEndpoint name <$> rfc8941String


renderReportingEndpoints :: ReportingEndpoints -> M.Builder
renderReportingEndpoints (ReportingEndpoints members) =
  M.intersperse ", " (map renderMember members)
  where
    renderMember (ReportingEndpoint name url) =
      R.shortText name <> M.char7 '=' <> R.rfc8941String url
