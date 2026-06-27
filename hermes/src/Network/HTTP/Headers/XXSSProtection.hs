{-# LANGUAGE TemplateHaskell #-}

{- |
@X-XSS-Protection@ — a /de-facto/ (non-IANA-registered) response header,
historically honoured by Internet Explorer, Chrome, and Safari to control the
browser's built-in reflected-XSS auditor. The feature is now obsolete (modern
browsers have removed it in favour of @Content-Security-Policy@) but the header
remains widely emitted.

== Grammar (de-facto)

@
X-XSS-Protection = "0"
                 / "1" *( OWS ";" OWS directive )
directive        = "mode" "=" "block"
                 / "report" "=" reporting-uri
@

@0@ disables the auditor; @1@ enables it. @mode=block@ asks the browser to
block rendering rather than sanitise, and @report=\<uri\>@ (a Chromium
extension) names a violation-report endpoint.

This is a /de-facto/ header with no governing RFC; see MDN
<https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection>.

See also: "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.XContentTypeOptions", "Network.HTTP.Headers.XFrameOptions".
-}
module Network.HTTP.Headers.XXSSProtection (
  XXSSProtection (..),
  xXSSProtectionParser,
  renderXXSSProtection,
) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXXSSProtection)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A parsed @X-XSS-Protection@ value.
data XXSSProtection
  = -- | @0@ — the XSS auditor is disabled.
    XXSSDisabled
  | -- | @1@ — the XSS auditor is enabled, with optional directives.
    XXSSEnabled
      { xxssModeBlock :: !Bool
      -- ^ Whether @; mode=block@ was present (block instead of sanitise).
      , xxssReport :: !(Maybe ST.ShortText)
      -- ^ The @; report=\<uri\>@ reporting endpoint, if present.
      }
  deriving stock (Eq, Show)


instance KnownHeader XXSSProtection where
  type ParseFailure XXSSProtection = String
  type Cardinality XXSSProtection = 'ZeroOrOne
  type Direction XXSSProtection = 'Response


  parseFromHeaders _ headers = case runParser xXSSProtectionParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing X-XSS-Protection header: " <> show leftover)
    Fail -> Left "Failed to parse X-XSS-Protection header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderXXSSProtection


  headerName _ = hXXSSProtection


-- | One parsed directive (internal).
data XXSSParam
  = ParamModeBlock
  | ParamReport !ST.ShortText


xXSSProtectionParser :: ParserT st String XXSSProtection
xXSSProtectionParser =
  (XXSSDisabled <$ $(char '0'))
    <|> ($(char '1') *> (assembleParams <$> many xxssParam))
  where
    xxssParam = ows *> $(char ';') *> ows *> paramBody
    paramBody =
      $( switch
          [|
            case _ of
              "mode" -> ows *> $(char '=') *> ows *> (ParamModeBlock <$ $(string "block"))
              "report" -> ows *> $(char '=') *> ows *> (ParamReport <$> reportValue)
            |]
       )
    reportValue =
      shortASCIIFromParser_ (some (satisfyAscii (\c -> c /= ';' && c /= ' ' && c /= '\t')))


-- | Fold parsed directives into the structured enabled value.
assembleParams :: [XXSSParam] -> XXSSProtection
assembleParams = go False Nothing
  where
    go mode report [] = XXSSEnabled mode report
    go mode report (p : ps) = case p of
      ParamModeBlock -> go True report ps
      ParamReport u -> go mode (Just u) ps


renderXXSSProtection :: XXSSProtection -> M.Builder
renderXXSSProtection = \case
  XXSSDisabled -> "0"
  XXSSEnabled mode report ->
    "1"
      <> (if mode then "; mode=block" else mempty)
      <> maybe mempty (\u -> "; report=" <> shortText u) report
