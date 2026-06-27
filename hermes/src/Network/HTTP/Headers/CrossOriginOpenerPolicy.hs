{-# LANGUAGE TemplateHaskell #-}

{- |
@Cross-Origin-Opener-Policy@ (COOP) response header — controls whether a
top-level document shares a browsing-context group (and thus a @window@
reference) with cross-origin documents it opens or is opened by. Together
with COEP it gates cross-origin isolation.

The value is an RFC 8941 structured-field item: a bare token naming the
policy, optionally followed by a @report-to@ parameter naming a reporting
endpoint.

@
Cross-Origin-Opener-Policy = sf-token *( \";\" parameter )
sf-token                   = \"unsafe-none\" / \"same-origin\"
                           / \"same-origin-allow-popups\" / \"noopener-allow-popups\"
parameter                  = \"report-to\" \"=\" sf-string
@

Spec: WHATWG HTML, <https://html.spec.whatwg.org/multipage/browsers.html#cross-origin-opener-policies>.

See also: "Network.HTTP.Headers.CrossOriginOpenerPolicyReportOnly",
"Network.HTTP.Headers.CrossOriginEmbedderPolicy",
"Network.HTTP.Headers.CrossOriginResourcePolicy",
"Network.HTTP.Headers.OriginAgentCluster",
"Network.HTTP.Headers.ReportingEndpoints".
-}
module Network.HTTP.Headers.CrossOriginOpenerPolicy (
  CrossOriginOpenerPolicy (..),
  CoopDirective (..),
  crossOriginOpenerPolicyParser,
  renderCrossOriginOpenerPolicy,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCrossOriginOpenerPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The opener-policy directive.
data CoopDirective
  = -- | @unsafe-none@ — the default; document may share its browsing-context group.
    CoopUnsafeNone
  | -- | @same-origin@ — isolate into a group shared only with same-origin documents.
    CoopSameOrigin
  | -- | @same-origin-allow-popups@ — like @same-origin@ but keeps opened popups in the group.
    CoopSameOriginAllowPopups
  | -- | @noopener-allow-popups@ — sever the opener relationship while still allowing popups.
    CoopNoopenerAllowPopups
  deriving stock (Eq, Show)


-- | A parsed @Cross-Origin-Opener-Policy@ value.
data CrossOriginOpenerPolicy = CrossOriginOpenerPolicy
  { coopDirective :: !CoopDirective
  , coopReportTo :: !(Maybe RFC8941String)
  -- ^ Optional @report-to@ reporting endpoint name.
  }
  deriving stock (Eq, Show)


instance KnownHeader CrossOriginOpenerPolicy where
  type ParseFailure CrossOriginOpenerPolicy = String
  type Cardinality CrossOriginOpenerPolicy = 'ZeroOrOne
  type Direction CrossOriginOpenerPolicy = 'Response


  parseFromHeaders _ headers = case runParser crossOriginOpenerPolicyParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cross-Origin-Opener-Policy header: " <> show rest
    Fail -> Left "Failed to parse Cross-Origin-Opener-Policy header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCrossOriginOpenerPolicy


  headerName _ = hCrossOriginOpenerPolicy


coopDirectiveParser :: ParserT st String CoopDirective
coopDirectiveParser =
  $( switch
      [|
        case _ of
          "unsafe-none" -> pure CoopUnsafeNone
          "same-origin-allow-popups" -> pure CoopSameOriginAllowPopups
          "same-origin" -> pure CoopSameOrigin
          "noopener-allow-popups" -> pure CoopNoopenerAllowPopups
        |]
   )


-- | Parse the optional @;report-to="endpoint"@ structured-field parameter.
reportToParam :: ParserT st String RFC8941String
reportToParam = do
  $(char ';')
  ows
  $(string "report-to")
  $(char '=')
  rfc8941String


crossOriginOpenerPolicyParser :: ParserT st String CrossOriginOpenerPolicy
crossOriginOpenerPolicyParser = do
  d <- coopDirectiveParser
  rt <- optional reportToParam
  pure (CrossOriginOpenerPolicy d rt)


renderCoopDirective :: CoopDirective -> M.Builder
renderCoopDirective = \case
  CoopUnsafeNone -> "unsafe-none"
  CoopSameOrigin -> "same-origin"
  CoopSameOriginAllowPopups -> "same-origin-allow-popups"
  CoopNoopenerAllowPopups -> "noopener-allow-popups"


renderCrossOriginOpenerPolicy :: CrossOriginOpenerPolicy -> M.Builder
renderCrossOriginOpenerPolicy (CrossOriginOpenerPolicy d rt) =
  renderCoopDirective d
    <> R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941String "report-to" rt
