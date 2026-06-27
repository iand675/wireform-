{-# LANGUAGE TemplateHaskell #-}

{- |
@Proxy-Authentication-Info@ — the proxy-side analogue of
@Authentication-Info@: a response header in which an intermediary proxy
carries the final, scheme-specific data it communicates at the end of a
successful authentication exchange with the client.

== Grammar (RFC 7615 §4)

@
Proxy-Authentication-Info = #auth-param
auth-param                = token BWS "=" BWS ( token / quoted-string )
@

Identical shape to @Authentication-Info@: a comma-separated list of
@auth-param@s, surfaced generically as 'CredentialParam' (token or
quoted-string) in document order so the field round-trips.  Parameter
names are case-insensitive per RFC 9110 §11.2; the on-the-wire
spelling is preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc7615#section-4>

See also: "Network.HTTP.Headers.AuthenticationInfo",
"Network.HTTP.Headers.ProxyAuthenticate",
"Network.HTTP.Headers.ProxyAuthorization".
-}
module Network.HTTP.Headers.ProxyAuthenticationInfo (
  ProxyAuthenticationInfo (..),
  proxyAuthenticationInfoParser,
  renderProxyAuthenticationInfo,
  CredentialParam (..),
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (CredentialParam (..))
import Network.HTTP.Headers.HeaderFieldName (hProxyAuthenticationInfo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | The @Proxy-Authentication-Info@ header value: an ordered
@auth-param@ list.
-}
newtype ProxyAuthenticationInfo = ProxyAuthenticationInfo
  { proxyAuthenticationInfoParams :: [(ShortText, CredentialParam)]
  }
  deriving stock (Eq, Show)


instance KnownHeader ProxyAuthenticationInfo where
  type ParseFailure ProxyAuthenticationInfo = String
  type Cardinality ProxyAuthenticationInfo = 'ZeroOrMore
  type Direction ProxyAuthenticationInfo = 'Response


  parseFromHeaders _ headers = do
    pss <- traverse parseOne (NE.toList headers)
    pure (ProxyAuthenticationInfo (concat pss))
    where
      parseOne hdr = case runParser proxyAuthenticationInfoParser hdr of
        OK (ProxyAuthenticationInfo ps) leftover
          | B.null (dropOws leftover) -> Right ps
          | otherwise ->
              Left ("Unconsumed input after parsing Proxy-Authentication-Info: " <> show leftover)
        Fail -> Left "Failed to parse Proxy-Authentication-Info header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ pai = [M.toStrictByteString (renderProxyAuthenticationInfo pai)]


  headerName _ = hProxyAuthenticationInfo


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

{- | Parse a @Proxy-Authentication-Info@ value: a comma-separated
@auth-param@ list.
-}
proxyAuthenticationInfoParser :: ParserT st String ProxyAuthenticationInfo
proxyAuthenticationInfoParser = do
  ows
  ps <- authParamParser `sepBy` (ows *> $(char ',') *> ows)
  ows
  pure (ProxyAuthenticationInfo ps)


-- | @token BWS "=" BWS ( token / quoted-string )@.
authParamParser :: ParserT st String (ShortText, CredentialParam)
authParamParser = do
  key <- rfc9110Token
  bws
  $(char '=')
  bws
  val <-
    (CredentialParamString <$> rfc8941String)
      <|> (CredentialParamToken <$> rfc9110Token)
  pure (key, val)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderProxyAuthenticationInfo :: ProxyAuthenticationInfo -> M.Builder
renderProxyAuthenticationInfo (ProxyAuthenticationInfo ps) =
  M.intersperse ", " (map renderParam ps)


renderParam :: (ShortText, CredentialParam) -> M.Builder
renderParam (k, v) =
  R.shortText k <> M.char7 '=' <> case v of
    CredentialParamToken t -> R.shortText t
    CredentialParamString s -> R.rfc8941String s
