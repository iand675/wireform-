{-# LANGUAGE TemplateHaskell #-}

{- |
@Cross-Origin-Embedder-Policy@ (COEP) response header — configures
embedding of cross-origin resources into the document, the server side
of cross-origin isolation.

The value is a Structured Fields Item whose bare value is a policy token.
The string parameter @report-to@ names a reporting endpoint. Other
parameters are validated and ignored.

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

import Data.ByteString (ByteString)
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


  parseFromHeaders _ = parseSingleFieldLine "Cross-Origin-Embedder-Policy" crossOriginEmbedderPolicyParser


  renderToHeaders _ (CrossOriginEmbedderPolicy directive Nothing) = coepDirectiveBytes directive
  renderToHeaders _ value = M.toStrictByteString (renderCrossOriginEmbedderPolicy value)


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


crossOriginEmbedderPolicyParser :: ParserT st String CrossOriginEmbedderPolicy
crossOriginEmbedderPolicyParser =
  CrossOriginEmbedderPolicy <$> coepDirectiveParser <*> structuredFieldStringParameter "report-to"


coepDirectiveBytes :: CoepDirective -> ByteString
coepDirectiveBytes = \case
  CoepUnsafeNone -> "unsafe-none"
  CoepRequireCorp -> "require-corp"
  CoepCredentialless -> "credentialless"


renderCrossOriginEmbedderPolicy :: CrossOriginEmbedderPolicy -> M.Builder
renderCrossOriginEmbedderPolicy (CrossOriginEmbedderPolicy d rt) =
  M.byteString (coepDirectiveBytes d)
    <> foldMap (\endpoint -> ";report-to=" <> R.rfc8941String endpoint) rt
