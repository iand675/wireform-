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
separates entries. A parameter name followed by @BWS "="@ continues
the current entry.

Recipients must accept both quoted and unquoted parameter values, and
the @name*=@ form uses the RFC 8187 extended-value grammar. The
extended value is kept as a 'CredentialParamToken' with its original
wire spelling. Parameter names are case-insensitive and their original
spelling and order are preserved.

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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Short (ShortText)
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.ExtendedValue (extendedValueBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (
  AuthScheme (..),
  CredentialParam (..),
  authParamValueParser,
  authParamsUniqueOn,
  renderAuthParam,
 )
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


  parseFromHeaders _ (first :| rest) =
    parseFieldLine "Authentication-Control" authenticationControlParser $
      case rest of
        [] -> first
        _ -> B.intercalate "," (first : rest)


  renderToHeaders _ ac = [M.toStrictByteString (renderAuthenticationControl ac)]


  headerName _ = hAuthenticationControl


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse an @Authentication-Control@ value: a comma-separated list of entries.
authenticationControlParser :: ParserT st String AuthenticationControl
authenticationControlParser = do
  ows
  skipMany comma
  first <- entryParser
  rest <- many (commas *> entryParser)
  skipMany comma
  ows
  pure (AuthenticationControl (first : rest))


comma :: ParserT st e ()
comma = ows *> $(char ',') *> ows
{-# INLINE comma #-}


commas :: ParserT st e ()
commas = comma *> skipMany comma
{-# INLINE commas #-}


-- | Parse one entry: @auth-scheme 1*SP 1#auth-control-param@.
entryParser :: ParserT st String AuthControlEntry
entryParser = do
  scheme <- AuthScheme <$> rfc9110Token
  skipSome $(char ' ')
  skipMany comma
  first <- authParamParser
  rest <- many (commas *> authParamParser)
  let params = first : rest
  if authParamsUniqueOn baseName params
    then pure (AuthControlEntry scheme params)
    else err "Duplicate Authentication-Control parameter"
  where
    baseName bytes
      | B.last bytes == 42 = B.take (B.length bytes - 1) bytes
      | otherwise = bytes


authParamParser :: ParserT st String (ShortText, CredentialParam)
authParamParser =
  withByteString parameterName $ \extended key -> do
    bws
    $(char '=')
    bws
    value <-
      if extended
        then CredentialParamToken <$> shortASCIIFromParser_ extendedValueBytes
        else authParamValueParser
    pure (STU.fromByteStringUnsafe key, value)
  where
    parameterName = do
      bareToken <|> ($(char '-') *> bareToken *> skipSome ($(char '.') *> bareToken))
      (True <$ $(char '*')) <|> pure False
    bareToken =
      skipSatisfyAscii alphanumeric
        *> skipMany (skipSatisfyAscii (\c -> alphanumeric c || c == '-' || c == '_'))
    alphanumeric c = isLatinLetter c || isDigit c


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
    <> M.intersperse ", " (map renderAuthParam ps)
