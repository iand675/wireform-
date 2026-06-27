{-# LANGUAGE TemplateHaskell #-}

{- |
@Authentication-Info@ — a response header carrying the final,
scheme-specific data a server communicates at the end of a successful
authentication exchange (for example Digest's @nextnonce@, @rspauth@,
@qop@, @cnonce@, and @nc@), letting the server authenticate itself to
the client and supply state for the next request.

== Grammar (RFC 7615 §3)

@
Authentication-Info = #auth-param
auth-param          = token BWS "=" BWS ( token / quoted-string )
@

A comma-separated list of @auth-param@s.  The parameter vocabulary is
scheme-defined, so parameters are surfaced generically as the shared
'CredentialParam' (token or quoted-string) and kept in document order
so the field round-trips.  Parameter names are case-insensitive per
RFC 9110 §11.2; the on-the-wire spelling is preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc7615#section-3>

See also: "Network.HTTP.Headers.ProxyAuthenticationInfo",
"Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.Authorization",
"Network.HTTP.Headers.AuthenticationControl".
-}
module Network.HTTP.Headers.AuthenticationInfo (
  AuthenticationInfo (..),
  authenticationInfoParser,
  renderAuthenticationInfo,
  CredentialParam (..),
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (CredentialParam (..))
import Network.HTTP.Headers.HeaderFieldName (hAuthenticationInfo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | The @Authentication-Info@ header value: an ordered @auth-param@ list.
newtype AuthenticationInfo = AuthenticationInfo
  { authenticationInfoParams :: [(ShortText, CredentialParam)]
  }
  deriving stock (Eq, Show)


instance KnownHeader AuthenticationInfo where
  type ParseFailure AuthenticationInfo = String
  type Cardinality AuthenticationInfo = 'ZeroOrMore
  type Direction AuthenticationInfo = 'Response


  parseFromHeaders _ headers = do
    pss <- traverse parseOne (NE.toList headers)
    pure (AuthenticationInfo (concat pss))
    where
      parseOne hdr = case runParser authenticationInfoParser hdr of
        OK (AuthenticationInfo ps) leftover
          | B.null (dropOws leftover) -> Right ps
          | otherwise ->
              Left ("Unconsumed input after parsing Authentication-Info: " <> show leftover)
        Fail -> Left "Failed to parse Authentication-Info header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ ai = [M.toStrictByteString (renderAuthenticationInfo ai)]


  headerName _ = hAuthenticationInfo


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse an @Authentication-Info@ value: a comma-separated @auth-param@ list.
authenticationInfoParser :: ParserT st String AuthenticationInfo
authenticationInfoParser = do
  ows
  ps <- authParamParser `sepBy` (ows *> $(char ',') *> ows)
  ows
  pure (AuthenticationInfo ps)


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

renderAuthenticationInfo :: AuthenticationInfo -> M.Builder
renderAuthenticationInfo (AuthenticationInfo ps) =
  M.intersperse ", " (map renderParam ps)


renderParam :: (ShortText, CredentialParam) -> M.Builder
renderParam (k, v) =
  R.shortText k <> M.char7 '=' <> case v of
    CredentialParamToken t -> R.shortText t
    CredentialParamString s -> R.rfc8941String s
