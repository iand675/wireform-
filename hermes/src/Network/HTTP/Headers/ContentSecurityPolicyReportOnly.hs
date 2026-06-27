{- |
W3C Content Security Policy Level 3 @Content-Security-Policy-Report-Only@ —
identical in syntax to 'Network.HTTP.Headers.ContentSecurityPolicy.ContentSecurityPolicy',
but the user agent only /monitors/ the policy and reports violations
rather than enforcing it.

== Grammar

Identical to @Content-Security-Policy@: a @\";\"@-separated
@directive-set@ where each @directive@ is a @directive-name@ followed
by zero or more whitespace-separated values. The directive grammar
(parser and renderer) is shared with
'Network.HTTP.Headers.ContentSecurityPolicy'; directive order and raw
values are preserved.

Spec: <https://www.w3.org/TR/CSP3/#cspro-header>

See also: "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.ReportingEndpoints", "Network.HTTP.Headers.NEL", "Network.HTTP.Headers.PermissionsPolicy".
-}
module Network.HTTP.Headers.ContentSecurityPolicyReportOnly (
  ContentSecurityPolicyReportOnly (..),
  contentSecurityPolicyReportOnlyParser,
  renderContentSecurityPolicyReportOnly,
) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentSecurityPolicy (
  Directive,
  directiveSetParser,
  renderDirectiveSet,
 )
import Network.HTTP.Headers.HeaderFieldName (hContentSecurityPolicyReportOnly)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A report-only serialized policy: an ordered list of directives.
newtype ContentSecurityPolicyReportOnly = ContentSecurityPolicyReportOnly
  { csproDirectives :: [Directive]
  }
  deriving stock (Eq, Show)


instance KnownHeader ContentSecurityPolicyReportOnly where
  type ParseFailure ContentSecurityPolicyReportOnly = String
  type Cardinality ContentSecurityPolicyReportOnly = 'ZeroOrOne
  type Direction ContentSecurityPolicyReportOnly = 'Response


  parseFromHeaders _ headers = case runParser contentSecurityPolicyReportOnlyParser (NE.head headers) of
    OK cspro leftover
      | B.null (dropOws leftover) -> Right cspro
      | otherwise ->
          Left ("Unconsumed input after parsing Content-Security-Policy-Report-Only: " <> show leftover)
    Fail -> Left "Failed to parse Content-Security-Policy-Report-Only header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderContentSecurityPolicyReportOnly


  headerName _ = hContentSecurityPolicyReportOnly


-- | Parse an entire @Content-Security-Policy-Report-Only@ value.
contentSecurityPolicyReportOnlyParser :: ParserT st String ContentSecurityPolicyReportOnly
contentSecurityPolicyReportOnlyParser = ContentSecurityPolicyReportOnly <$> directiveSetParser


-- | Render an entire @Content-Security-Policy-Report-Only@ value.
renderContentSecurityPolicyReportOnly :: ContentSecurityPolicyReportOnly -> M.Builder
renderContentSecurityPolicyReportOnly = renderDirectiveSet . csproDirectives
