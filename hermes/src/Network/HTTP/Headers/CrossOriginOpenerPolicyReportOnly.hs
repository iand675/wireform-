{- |
@Cross-Origin-Opener-Policy-Report-Only@ response header — the
report-only twin of @Cross-Origin-Opener-Policy@. It reports what the
opener policy /would/ do without changing browsing-context-group
behaviour. The grammar is identical to the enforcing header, so this
module reuses "Network.HTTP.Headers.CrossOriginOpenerPolicy".

Spec: WHATWG HTML, <https://html.spec.whatwg.org/multipage/browsers.html#cross-origin-opener-policies>.

See also: "Network.HTTP.Headers.CrossOriginOpenerPolicy",
"Network.HTTP.Headers.CrossOriginEmbedderPolicyReportOnly",
"Network.HTTP.Headers.CrossOriginResourcePolicy",
"Network.HTTP.Headers.ReportingEndpoints".
-}
module Network.HTTP.Headers.CrossOriginOpenerPolicyReportOnly (
  CrossOriginOpenerPolicyReportOnly (..),
  crossOriginOpenerPolicyReportOnlyParser,
  renderCrossOriginOpenerPolicyReportOnly,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.CrossOriginOpenerPolicy (
  CrossOriginOpenerPolicy,
  crossOriginOpenerPolicyParser,
  renderCrossOriginOpenerPolicy,
 )
import Network.HTTP.Headers.HeaderFieldName (hCrossOriginOpenerPolicyReportOnly)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @Cross-Origin-Opener-Policy-Report-Only@ value.
newtype CrossOriginOpenerPolicyReportOnly = CrossOriginOpenerPolicyReportOnly
  { getCrossOriginOpenerPolicyReportOnly :: CrossOriginOpenerPolicy
  }
  deriving stock (Eq, Show)


instance KnownHeader CrossOriginOpenerPolicyReportOnly where
  type ParseFailure CrossOriginOpenerPolicyReportOnly = String
  type Cardinality CrossOriginOpenerPolicyReportOnly = 'ZeroOrOne
  type Direction CrossOriginOpenerPolicyReportOnly = 'Response


  parseFromHeaders _ headers = case runParser crossOriginOpenerPolicyReportOnlyParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cross-Origin-Opener-Policy-Report-Only header: " <> show rest
    Fail -> Left "Failed to parse Cross-Origin-Opener-Policy-Report-Only header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCrossOriginOpenerPolicyReportOnly


  headerName _ = hCrossOriginOpenerPolicyReportOnly


crossOriginOpenerPolicyReportOnlyParser :: ParserT st String CrossOriginOpenerPolicyReportOnly
crossOriginOpenerPolicyReportOnlyParser =
  CrossOriginOpenerPolicyReportOnly <$> crossOriginOpenerPolicyParser


renderCrossOriginOpenerPolicyReportOnly :: CrossOriginOpenerPolicyReportOnly -> M.Builder
renderCrossOriginOpenerPolicyReportOnly =
  renderCrossOriginOpenerPolicy . getCrossOriginOpenerPolicyReportOnly
