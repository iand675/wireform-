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
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.CaseInsensitive as CI
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Grammar.ExtendedValue (ExtendedValue (..), extendedValueBytes, parseExtendedValue)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (
  AuthScheme (..),
  CredentialContents (..),
  CredentialParam (..),
  Credentials (Credentials),
  authParamValueParser,
  authParamsUnique,
  authParamsUniqueOn,
  credentialParamBytes,
  encodeCredentialEntriesChecked,
  renderAuthParam,
 )
import Network.HTTP.Headers.HeaderFieldName (hAuthenticationControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.FFI (validateUtf8SWAR)
import Wireform.Parser.Driver (parseByteString)


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


  renderToHeadersChecked _ (AuthenticationControl entries)
    | null entries || not (all validControlEntry entries && uniqueControlEntries entries) = Nothing
    | otherwise = pure <$> encodeCredentialEntriesChecked asCredentials entries
    where
      asCredentials (AuthControlEntry scheme params) =
        Credentials scheme (maybe CredentialBare CredentialParams (NE.nonEmpty params))


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
  if controlParamsUnique params
    then pure (AuthControlEntry scheme params)
    else err "Duplicate Authentication-Control parameter"


parameterBaseName :: B.ByteString -> B.ByteString
parameterBaseName bytes = case B.unsnoc bytes of
  Just (base, 42) -> base
  _ -> bytes


controlParamsUnique :: [(ShortText, CredentialParam)] -> Bool
controlParamsUnique params
  | any (hasExtendedName . fst) params = authParamsUniqueOn parameterBaseName params
  | otherwise = authParamsUnique params


hasExtendedName :: ShortText -> Bool
hasExtendedName name =
  let bytes = ST.toShortByteString name
      len = SBS.length bytes
  in len > 0 && SBS.unsafeIndex bytes (len - 1) == 42


authParamParser :: ParserT st String (ShortText, CredentialParam)
authParamParser =
  withByteString controlParameterName $ \extended key -> do
    bws
    $(char '=')
    bws
    value <-
      if extended
        then CredentialParamToken <$> shortASCIIFromParser_ extendedValueBytes
        else authParamValueParser
    pure (STU.fromByteStringUnsafe key, value)


controlParameterName :: ParserT st e Bool
controlParameterName = do
  bareToken <|> ($(char '-') *> bareToken *> skipSome ($(char '.') *> bareToken))
  (True <$ $(char '*')) <|> pure False
  where
    bareToken = skipSatisfyAscii controlNameHead *> skipMany (skipSatisfyAscii controlNameTail)
{-# INLINE controlParameterName #-}


controlNameHead :: Char -> Bool
controlNameHead c = isLatinLetter c || isDigit c
{-# INLINE controlNameHead #-}


controlNameTail :: Char -> Bool
controlNameTail c = controlNameHead c || c == '-' || c == '_'
{-# INLINE controlNameTail #-}


validControlEntry :: AuthControlEntry -> Bool
validControlEntry (AuthControlEntry _ params) =
  not (null params) && all validControlParam params && uniqueBaseNames
  where
    -- The shared credential writer checks plain names.
    uniqueBaseNames =
      not (any (hasExtendedName . fst) params) || authParamsUniqueOn parameterBaseName params
    validControlParam (name, value)
      | let bytes = ST.toShortByteString name
      , not (SBS.null bytes)
      , controlNameHead (BSI.w2c (SBS.unsafeIndex bytes 0))
      , scanCompactBytes (controlNameTail . BSI.w2c) bytes 1 = True
      | otherwise = case parseByteString (controlParameterName <* eof) (ST.toByteString name) of
          Right False -> True
          Right True -> validExtendedParameter value
          Left _ -> False


validExtendedParameter :: CredentialParam -> Bool
validExtendedParameter (CredentialParamToken text) =
  case parseExtendedValue (ST.toByteString text) of
    Right (ExtendedValue charset Nothing octets) ->
      sameNegotiationToken "UTF-8" charset && B.any (>= 128) octets && validateUtf8SWAR octets
    _ -> False
validExtendedParameter _ = False


-- | Parameters are unique across entries for the same scheme and realm.
uniqueControlEntries :: [AuthControlEntry] -> Bool
uniqueControlEntries [] = True
uniqueControlEntries [_] = True
uniqueControlEntries [first, second] =
  not (sameScheme first second)
    || controlRealm first /= controlRealm second
    || Set.disjoint (parameterKeys first) (parameterKeys second)
  where
    sameScheme (AuthControlEntry (AuthScheme a) _) (AuthControlEntry (AuthScheme b) _) =
      sameNegotiationToken a b
uniqueControlEntries entries = go Map.empty entries
  where
    go _ [] = True
    go seen (entry@(AuthControlEntry (AuthScheme scheme) _) : rest) =
      let scope = ControlScope (CI.mk (ST.toByteString scheme)) (controlRealm entry)
          keys = parameterKeys entry
      in case Map.lookup scope seen of
           Nothing -> go (Map.insert scope keys seen) rest
           Just previous ->
             Set.disjoint previous keys && go (Map.insert scope (Set.union previous keys) seen) rest


data ControlScope = ControlScope !(CI.CI B.ByteString) !(Maybe B.ByteString)
  deriving stock (Eq, Ord)


parameterKeys :: AuthControlEntry -> Set.Set (CI.CI B.ByteString)
parameterKeys = Set.fromList . map (CI.mk . parameterBaseName . ST.toByteString . fst) . authControlParams


controlRealm :: AuthControlEntry -> Maybe B.ByteString
controlRealm entry = do
  (_, value) <- find (sameNegotiationToken "realm" . fst) (authControlParams entry)
  pure $ case value of
    CredentialParamString text -> M.toStrictByteString (R.renderLatin1ShortText (unsafeToRFC8941String text))
    _ -> credentialParamBytes value


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
