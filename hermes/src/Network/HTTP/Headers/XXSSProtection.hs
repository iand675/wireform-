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
                 / "1" *( OWS ";" OWS directive ) [ OWS ";" ]
directive        = "mode" "=" "block"
                 / "report" "=" URI-reference
@

@0@ disables the auditor; @1@ enables it. @mode=block@ asks the browser to
block rendering rather than sanitise. @report@ carries a URI-reference
for a violation-report endpoint. Directive keywords use ASCII case
folding and each directive can appear once.

<https://github.com/chromium/chromium/blob/3b8be2124e3f1d3ce09d534315fb9a8f50ead086/third_party/WebKit/Source/platform/network/HTTPParsers.cpp Chromium's historical parser>

See also: "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.XContentTypeOptions", "Network.HTTP.Headers.XFrameOptions".
-}
module Network.HTTP.Headers.XXSSProtection (
  XXSSProtection (..),
  xXSSProtectionParser,
  renderXXSSProtection,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (outboundURIReferenceBytes, uriReference, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXXSSProtection)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (copyText, shortText)
import Wireform.Encode.Direct (dBytes, directEncode)
import Wireform.Parser.Driver (parseByteString)


-- | A parsed @X-XSS-Protection@ value.
data XXSSProtection
  = -- | @0@ — the XSS auditor is disabled.
    XXSSDisabled
  | -- | @1@ — the XSS auditor is enabled, with optional directives.
    XXSSEnabled
      { xxssModeBlock :: !Bool
      -- ^ Whether @; mode=block@ was present (block instead of sanitise).
      , xxssReport :: !(Maybe ST.ShortText)
      -- ^ The original reporting URI-reference spelling.
      }
  deriving stock (Eq, Show)


instance KnownHeader XXSSProtection where
  type ParseFailure XXSSProtection = String
  type Cardinality XXSSProtection = 'ZeroOrOne
  type Direction XXSSProtection = 'Response


  parseFromHeaders _ ("0" :| []) = Right XXSSDisabled
  parseFromHeaders _ ("1" :| []) = Right (XXSSEnabled False Nothing)
  parseFromHeaders _ ("1; mode=block" :| []) = Right (XXSSEnabled True Nothing)
  parseFromHeaders _ fields = parseSingleFieldLine "X-XSS-Protection" xXSSProtectionParser fields


  renderToHeaders _ XXSSDisabled = "0"
  renderToHeaders _ (XXSSEnabled False Nothing) = "1"
  renderToHeaders _ (XXSSEnabled True Nothing) = "1; mode=block"
  renderToHeaders _ value = M.toStrictByteString (renderXXSSProtection value)


  renderToHeadersChecked _ (XXSSEnabled blocked (Just endpoint))
    | not (SBS.null bytes)
    , not (SBS.elem 59 bytes)
    , validURIText outboundURIReferenceBytes endpoint =
        Just $ directEncode capacity $ \ptr start -> dBytes ptr start prefix >>= \end -> copyText ptr end endpoint
    | otherwise = Nothing
    where
      bytes = ST.toShortByteString endpoint
      prefix = if blocked then "1; mode=block; report=" else "1; report="
      capacity = BSI.checkedAdd "X-XSS-Protection" (BS.length prefix) (SBS.length bytes)
  renderToHeadersChecked settings value = Just (renderToHeaders settings value)


  headerName _ = hXXSSProtection


xXSSProtectionParser :: ParserT st String XXSSProtection
xXSSProtectionParser = ows *> branch $(char '0') (pure XXSSDisabled) ($(char '1') *> enabled False Nothing)
  where
    enabled blocked endpoint =
      let result = pure (XXSSEnabled blocked endpoint)
      in branch (ows *> $(char ';') *> ows) (branch eof result (directive blocked endpoint)) result

    directive blocked endpoint =
      $( switchAsciiCI
           [|
             case _ of
               "mode" ->
                 if blocked
                   then err "Duplicate X-XSS-Protection mode"
                   else equals *> blockKeyword *> enabled True endpoint
               "report" -> case endpoint of
                 Just _ -> err "Duplicate X-XSS-Protection report"
                 Nothing -> do
                   value <- equals *> reportReference
                   enabled blocked (Just value)
             |]
       )
    equals = ows *> $(char '=') *> ows


blockKeyword :: ParserT st e ()
blockKeyword = $(switchAsciiCI [|case _ of "block" -> pure ()|])


reportReference :: ParserT st String ST.ShortText
reportReference = withByteString (skipSome (skipSatisfyAscii reportByte)) $ \() bytes ->
  case parseByteString (uriReference <* eof) bytes of
    Right result -> pure result
    Left _ -> err "Invalid X-XSS-Protection reporting URI-reference"
  where
    reportByte c = c >= '!' && c <= '~' && c /= ';'


renderXXSSProtection :: XXSSProtection -> M.Builder
renderXXSSProtection = \case
  XXSSDisabled -> "0"
  XXSSEnabled mode report ->
    "1"
      <> (if mode then "; mode=block" else mempty)
      <> maybe mempty (\u -> "; report=" <> shortText u) report
