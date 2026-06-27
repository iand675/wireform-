{-# LANGUAGE TemplateHaskell #-}

{- |
@Authentication-Control@ — an experimental response header (RFC 8053)
that lets a server give an interactive client finer control over an HTTP
authentication exchange: how the credential prompt is styled, where to
redirect when unauthenticated or after logout, idle\/logout timeouts, and
similar hints. It is sent on authentication-initializing responses
alongside (or in place of) a @WWW-Authenticate@ challenge.

== Grammar (RFC 8053 §4)

@
Authentication-Control = 1#auth-control-entry
auth-control-entry     = auth-scheme 1*SP 1#auth-control-param
auth-control-param     = extensive-token BWS "=" BWS token
                       / extensive-token "*" BWS "=" BWS ext-value
@

Each entry is an @auth-scheme@ followed by a comma-separated list of
@name=value@ parameters, so the comma plays a dual role exactly as in
@WWW-Authenticate@: it both separates parameters within an entry and
separates entries.  We disambiguate by look-ahead — a comma followed
by @token BWS "="@ continues the parameter list, whereas a comma
followed by @token 1*SP token "="@ (a scheme name then a parameter)
begins a new entry.

Recipients must accept both quoted and unquoted parameter values, and
the @name*=@ extended-value form of RFC 5987 is lexically a
@token "=" token@ (its @charset@lang@pct-encoded@ body and trailing
@*@ on the name are all token characters).  Both forms therefore parse
uniformly as the shared 'CredentialParam' (token or quoted-string),
preserved in document order; the @*@ suffix on a name marks the
extended form.  Parameter names are case-insensitive; the on-the-wire
spelling is preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc8053#section-4>

See also: "Network.HTTP.Headers.OptionalWWWAuthenticate",
"Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.Authorization",
"Network.HTTP.Headers.AuthenticationInfo".
-}
module Network.HTTP.Headers.AuthenticationControl (
  AuthenticationControl (..),
  AuthControlEntry (..),
  authenticationControlParser,
  renderAuthenticationControl,
  AuthScheme (..),
  CredentialParam (..),
) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (AuthScheme (..), CredentialParam (..))
import Network.HTTP.Headers.HeaderFieldName (hAuthenticationControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A single @auth-control-entry@: an authentication scheme and its
(non-empty) ordered parameter list.
-}
data AuthControlEntry = AuthControlEntry
  { authControlScheme :: !AuthScheme
  , authControlParams :: ![(ShortText, CredentialParam)]
  }
  deriving stock (Eq, Show)


-- | The @Authentication-Control@ header value: a list of entries.
newtype AuthenticationControl = AuthenticationControl
  { authControlEntries :: [AuthControlEntry]
  }
  deriving stock (Eq, Show)


instance KnownHeader AuthenticationControl where
  type ParseFailure AuthenticationControl = String
  type Cardinality AuthenticationControl = 'ZeroOrMore
  type Direction AuthenticationControl = 'Response


  parseFromHeaders _ headers = do
    entryss <- traverse parseOne (NE.toList headers)
    pure (AuthenticationControl (concat entryss))
    where
      parseOne hdr = case runParser authenticationControlParser hdr of
        OK (AuthenticationControl es) leftover
          | B.null (dropOws leftover) -> Right es
          | otherwise ->
              Left ("Unconsumed input after parsing Authentication-Control: " <> show leftover)
        Fail -> Left "Failed to parse Authentication-Control header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ ac = [M.toStrictByteString (renderAuthenticationControl ac)]


  headerName _ = hAuthenticationControl


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse an @Authentication-Control@ value: a comma-separated list of entries.
authenticationControlParser :: ParserT st String AuthenticationControl
authenticationControlParser = do
  ows
  first <- entryParser
  rest <- many continueEntry
  ows
  pure (AuthenticationControl (first : rest))
  where
    continueEntry = do
      lookahead $ do
        ows
        $(char ',')
        ows
        _ <- rfc9110Token
        rws
        _ <- rfc9110Token
        bws
        $(char '=')
      ows
      $(char ',')
      ows
      entryParser


-- | Parse one entry: @auth-scheme 1*SP 1#auth-control-param@.
entryParser :: ParserT st String AuthControlEntry
entryParser = do
  scheme <- AuthScheme <$> rfc9110Token
  rws
  AuthControlEntry scheme <$> paramList


{- | Parse the entry's parameter list, stopping at any comma that
begins a new entry (@token 1*SP token "="@) rather than another
parameter (@token BWS "="@).
-}
paramList :: ParserT st String [(ShortText, CredentialParam)]
paramList = do
  first <- authParamParser
  rest <- many continueParam
  pure (first : rest)
  where
    continueParam = do
      lookahead $ do
        ows
        $(char ',')
        ows
        _ <- rfc9110Token
        bws
        $(char '=')
      ows
      $(char ',')
      ows
      authParamParser


-- | @extensive-token BWS "=" BWS ( token / quoted-string )@.
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

renderAuthenticationControl :: AuthenticationControl -> M.Builder
renderAuthenticationControl (AuthenticationControl es) =
  M.intersperse ", " (map renderEntry es)


renderEntry :: AuthControlEntry -> M.Builder
renderEntry (AuthControlEntry (AuthScheme scheme) ps) =
  R.shortText scheme
    <> M.char7 ' '
    <> M.intersperse ", " (map renderParam ps)


renderParam :: (ShortText, CredentialParam) -> M.Builder
renderParam (k, v) =
  R.shortText k <> M.char7 '=' <> case v of
    CredentialParamToken t -> R.shortText t
    CredentialParamString s -> R.rfc8941String s
