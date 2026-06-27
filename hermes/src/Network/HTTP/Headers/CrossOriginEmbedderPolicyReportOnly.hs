{- |
@Cross-Origin-Embedder-Policy-Report-Only@ response header — the
report-only twin of @Cross-Origin-Embedder-Policy@. It monitors and
reports what the embedder policy /would/ block without actually
enforcing it. The grammar is identical to the enforcing header, so this
module reuses "Network.HTTP.Headers.CrossOriginEmbedderPolicy".

Spec: WHATWG HTML, <https://html.spec.whatwg.org/multipage/browsers.html#coep>.

See also: "Network.HTTP.Headers.CrossOriginEmbedderPolicy",
"Network.HTTP.Headers.CrossOriginOpenerPolicyReportOnly",
"Network.HTTP.Headers.CrossOriginResourcePolicy",
"Network.HTTP.Headers.ReportingEndpoints".
-}
module Network.HTTP.Headers.CrossOriginEmbedderPolicyReportOnly (
  CrossOriginEmbedderPolicyReportOnly (..),
  crossOriginEmbedderPolicyReportOnlyParser,
  renderCrossOriginEmbedderPolicyReportOnly,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.CrossOriginEmbedderPolicy (
  CrossOriginEmbedderPolicy,
  crossOriginEmbedderPolicyParser,
  renderCrossOriginEmbedderPolicy,
 )
import Network.HTTP.Headers.HeaderFieldName (hCrossOriginEmbedderPolicyReportOnly)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @Cross-Origin-Embedder-Policy-Report-Only@ value.
newtype CrossOriginEmbedderPolicyReportOnly = CrossOriginEmbedderPolicyReportOnly
  { getCrossOriginEmbedderPolicyReportOnly :: CrossOriginEmbedderPolicy
  }
  deriving stock (Eq, Show)


instance KnownHeader CrossOriginEmbedderPolicyReportOnly where
  type ParseFailure CrossOriginEmbedderPolicyReportOnly = String
  type Cardinality CrossOriginEmbedderPolicyReportOnly = 'ZeroOrOne
  type Direction CrossOriginEmbedderPolicyReportOnly = 'Response


  parseFromHeaders _ headers = case runParser crossOriginEmbedderPolicyReportOnlyParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cross-Origin-Embedder-Policy-Report-Only header: " <> show rest
    Fail -> Left "Failed to parse Cross-Origin-Embedder-Policy-Report-Only header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCrossOriginEmbedderPolicyReportOnly


  headerName _ = hCrossOriginEmbedderPolicyReportOnly


crossOriginEmbedderPolicyReportOnlyParser :: ParserT st String CrossOriginEmbedderPolicyReportOnly
crossOriginEmbedderPolicyReportOnlyParser =
  CrossOriginEmbedderPolicyReportOnly <$> crossOriginEmbedderPolicyParser


renderCrossOriginEmbedderPolicyReportOnly :: CrossOriginEmbedderPolicyReportOnly -> M.Builder
renderCrossOriginEmbedderPolicyReportOnly =
  renderCrossOriginEmbedderPolicy . getCrossOriginEmbedderPolicyReportOnly
