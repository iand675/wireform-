{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Content-Security-Policy-Report-Only@ carries an ordered list of independent
policies for reporting. It uses the same field grammar as
"Network.HTTP.Headers.ContentSecurityPolicy".

<https://www.w3.org/TR/CSP3/#cspro-header Content Security Policy Level 3>
-}
module Network.HTTP.Headers.ContentSecurityPolicyReportOnly (
  ContentSecurityPolicyReportOnly (ContentSecurityPolicyReportOnly, csproDirectives, ContentSecurityPolicyReportOnlyList, csproPolicies),
  contentSecurityPolicyReportOnlyParser,
  renderContentSecurityPolicyReportOnly,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentSecurityPolicy (
  CSPPolicy (..),
  Directive,
  encodePolicyListChecked,
  parsePolicyFieldLines,
  policyListParser,
  renderPolicyList,
 )
import Network.HTTP.Headers.HeaderFieldName (hContentSecurityPolicyReportOnly)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ContentSecurityPolicyReportOnly = ContentSecurityPolicyReportOnlyList
  { csproPolicies :: NonEmpty CSPPolicy
  }
  deriving stock (Eq, Show)


{- | Construct one report-only policy or read its first policy's directives.
Use 'csproPolicies' to inspect all policies.
-}
pattern ContentSecurityPolicyReportOnly :: [Directive] -> ContentSecurityPolicyReportOnly
pattern ContentSecurityPolicyReportOnly {csproDirectives} <- (policyDirectives . NE.head . csproPolicies -> csproDirectives)
  where
    ContentSecurityPolicyReportOnly directives = ContentSecurityPolicyReportOnlyList (CSPPolicy directives :| [])


{-# COMPLETE ContentSecurityPolicyReportOnly #-}


instance KnownHeader ContentSecurityPolicyReportOnly where
  type ParseFailure ContentSecurityPolicyReportOnly = String
  type Cardinality ContentSecurityPolicyReportOnly = 'ZeroOrOne
  type Direction ContentSecurityPolicyReportOnly = 'Response


  parseFromHeaders _ =
    fmap ContentSecurityPolicyReportOnlyList . parsePolicyFieldLines "Content-Security-Policy-Report-Only"


  renderToHeaders _ = M.toStrictByteString . renderContentSecurityPolicyReportOnly


  renderToHeadersChecked _ = encodePolicyListChecked . csproPolicies


  headerName _ = hContentSecurityPolicyReportOnly


contentSecurityPolicyReportOnlyParser :: ParserT st String ContentSecurityPolicyReportOnly
contentSecurityPolicyReportOnlyParser = ContentSecurityPolicyReportOnlyList <$> policyListParser


renderContentSecurityPolicyReportOnly :: ContentSecurityPolicyReportOnly -> M.Builder
renderContentSecurityPolicyReportOnly = renderPolicyList . csproPolicies
