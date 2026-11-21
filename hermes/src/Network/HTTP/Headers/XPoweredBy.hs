{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@X-Powered-By@ — de-facto response header advertising the technology stack
(framework, language, or server software) powering the origin server, e.g.
@PHP/8.2.0@ or @Express@.

Deployments define the payload. The codec checks HTTP field octets and
preserves the original bytes.

Spec: no formal specification (de-facto); see the OWASP Secure Headers project,
<https://owasp.org/www-project-secure-headers/>.

See also: "Network.HTTP.Headers.Server", "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.XPoweredBy (
  XPoweredBy (XPoweredBy, poweredBy),
  xPoweredByBytes,
  xPoweredByFromBytes,
  xPoweredByParser,
  renderXPoweredBy,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXPoweredBy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


-- | An opaque technology or product description.
newtype XPoweredBy = XPoweredByBytes ByteString
  deriving stock (Eq)


-- | Legacy text view. Use 'xPoweredByBytes' for the original bytes.
pattern XPoweredBy :: ST.ShortText -> XPoweredBy
pattern XPoweredBy {poweredBy} <- (opaqueFieldText . xPoweredByBytes -> poweredBy)
  where
    XPoweredBy text = XPoweredByBytes (ST.toByteString text)


{-# COMPLETE XPoweredBy #-}


instance Show XPoweredBy where
  showsPrec precedence (XPoweredBy text) =
    showParen (precedence > 10) $ showString "XPoweredBy {poweredBy = " . shows text . showChar '}'


-- | The original field bytes, including any received optional whitespace.
xPoweredByBytes :: XPoweredBy -> ByteString
xPoweredByBytes (XPoweredByBytes wire) = wire


-- | Check field-value octets and keep the payload unchanged.
xPoweredByFromBytes :: ByteString -> Either String XPoweredBy
xPoweredByFromBytes = checkOpaqueField "X-Powered-By" XPoweredByBytes


instance KnownHeader XPoweredBy where
  type ParseFailure XPoweredBy = String
  type Cardinality XPoweredBy = 'ZeroOrOne
  type Direction XPoweredBy = 'Response


  parseFromHeaders _ = parseOpaqueFieldLine "X-Powered-By" XPoweredByBytes


  renderToHeaders _ = xPoweredByBytes


  renderToHeadersChecked _ = renderOpaqueFieldChecked . xPoweredByBytes


  headerName _ = hXPoweredBy


xPoweredByParser :: ParserT st String XPoweredBy
xPoweredByParser = opaqueFieldParser "X-Powered-By" XPoweredByBytes


renderXPoweredBy :: XPoweredBy -> M.Builder
renderXPoweredBy = M.byteString . xPoweredByBytes
