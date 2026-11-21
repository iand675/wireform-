{-# LANGUAGE TemplateHaskell #-}

{- |
@Cross-Origin-Opener-Policy@ (COOP) response header — controls whether a
top-level document shares a browsing-context group (and thus a @window@
reference) with cross-origin documents it opens or is opened by. Together
with COEP it gates cross-origin isolation.

The value is a Structured Fields Item whose bare value is a policy token.
The string parameter @report-to@ names a reporting endpoint. Other
parameters are validated and ignored.

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

import Data.ByteString (ByteString)
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


  parseFromHeaders _ = parseSingleFieldLine "Cross-Origin-Opener-Policy" crossOriginOpenerPolicyParser


  renderToHeaders _ (CrossOriginOpenerPolicy directive Nothing) = coopDirectiveBytes directive
  renderToHeaders _ value = M.toStrictByteString (renderCrossOriginOpenerPolicy value)


  renderToHeadersChecked _ (CrossOriginOpenerPolicy directive Nothing) = Just (coopDirectiveBytes directive)
  renderToHeadersChecked _ (CrossOriginOpenerPolicy directive (Just endpoint)) =
    R.encodeReportingPolicyChecked (coopDirectiveBytes directive) endpoint


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


crossOriginOpenerPolicyParser :: ParserT st String CrossOriginOpenerPolicy
crossOriginOpenerPolicyParser =
  CrossOriginOpenerPolicy <$> coopDirectiveParser <*> structuredFieldStringParameter "report-to"


coopDirectiveBytes :: CoopDirective -> ByteString
coopDirectiveBytes = \case
  CoopUnsafeNone -> "unsafe-none"
  CoopSameOrigin -> "same-origin"
  CoopSameOriginAllowPopups -> "same-origin-allow-popups"
  CoopNoopenerAllowPopups -> "noopener-allow-popups"


renderCrossOriginOpenerPolicy :: CrossOriginOpenerPolicy -> M.Builder
renderCrossOriginOpenerPolicy (CrossOriginOpenerPolicy d rt) =
  M.byteString (coopDirectiveBytes d)
    <> foldMap (\endpoint -> ";report-to=" <> R.rfc8941String endpoint) rt
