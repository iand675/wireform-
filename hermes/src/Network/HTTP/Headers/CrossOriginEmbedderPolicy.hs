{-# LANGUAGE TemplateHaskell #-}

{- |
@Cross-Origin-Embedder-Policy@ (COEP) response header — configures
embedding of cross-origin resources into the document, the server side
of cross-origin isolation.

The value is an RFC 8941 structured-field item: a bare token naming the
policy, optionally followed by a @report-to@ parameter naming a reporting
endpoint.

@
Cross-Origin-Embedder-Policy = sf-token *( \";\" parameter )
sf-token                     = \"unsafe-none\" / \"require-corp\" / \"credentialless\"
parameter                    = \"report-to\" \"=\" sf-string
@

Spec: WHATWG HTML, <https://html.spec.whatwg.org/multipage/browsers.html#coep>
(see also <https://fetch.spec.whatwg.org/#cross-origin-embedder-policy-header>).

See also: "Network.HTTP.Headers.CrossOriginEmbedderPolicyReportOnly",
"Network.HTTP.Headers.CrossOriginOpenerPolicy",
"Network.HTTP.Headers.CrossOriginResourcePolicy",
"Network.HTTP.Headers.Origin", "Network.HTTP.Headers.ReportingEndpoints".
-}
module Network.HTTP.Headers.CrossOriginEmbedderPolicy (
  CrossOriginEmbedderPolicy (..),
  CoepDirective (..),
  crossOriginEmbedderPolicyParser,
  renderCrossOriginEmbedderPolicy,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCrossOriginEmbedderPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The embedder-policy directive.
data CoepDirective
  = -- | @unsafe-none@ — the default; no cross-origin isolation.
    CoepUnsafeNone
  | -- | @require-corp@ — cross-origin resources must opt in via CORP or CORS.
    CoepRequireCorp
  | -- | @credentialless@ — cross-origin no-cors requests are sent without credentials.
    CoepCredentialless
  deriving stock (Eq, Show)


-- | A parsed @Cross-Origin-Embedder-Policy@ value.
data CrossOriginEmbedderPolicy = CrossOriginEmbedderPolicy
  { coepDirective :: !CoepDirective
  , coepReportTo :: !(Maybe RFC8941String)
  -- ^ Optional @report-to@ reporting endpoint name.
  }
  deriving stock (Eq, Show)


instance KnownHeader CrossOriginEmbedderPolicy where
  type ParseFailure CrossOriginEmbedderPolicy = String
  type Cardinality CrossOriginEmbedderPolicy = 'ZeroOrOne
  type Direction CrossOriginEmbedderPolicy = 'Response


  parseFromHeaders _ headers = case runParser crossOriginEmbedderPolicyParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cross-Origin-Embedder-Policy header: " <> show rest
    Fail -> Left "Failed to parse Cross-Origin-Embedder-Policy header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCrossOriginEmbedderPolicy


  headerName _ = hCrossOriginEmbedderPolicy


coepDirectiveParser :: ParserT st String CoepDirective
coepDirectiveParser =
  $( switch
      [|
        case _ of
          "unsafe-none" -> pure CoepUnsafeNone
          "require-corp" -> pure CoepRequireCorp
          "credentialless" -> pure CoepCredentialless
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


crossOriginEmbedderPolicyParser :: ParserT st String CrossOriginEmbedderPolicy
crossOriginEmbedderPolicyParser = do
  d <- coepDirectiveParser
  rt <- optional reportToParam
  pure (CrossOriginEmbedderPolicy d rt)


renderCoepDirective :: CoepDirective -> M.Builder
renderCoepDirective = \case
  CoepUnsafeNone -> "unsafe-none"
  CoepRequireCorp -> "require-corp"
  CoepCredentialless -> "credentialless"


renderCrossOriginEmbedderPolicy :: CrossOriginEmbedderPolicy -> M.Builder
renderCrossOriginEmbedderPolicy (CrossOriginEmbedderPolicy d rt) =
  renderCoepDirective d
    <> R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941String "report-to" rt
