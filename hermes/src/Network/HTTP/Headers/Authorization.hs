{-# LANGUAGE TemplateHaskell #-}

{- |
@Authorization@ — the request header a client uses to present
credentials to an origin server, typically answering a
@WWW-Authenticate@ challenge (though it may be sent pre-emptively). It
carries a single @credentials@ value: an auth-scheme (e.g. @Basic@,
@Bearer@, @Digest@) optionally followed by a @token68@ blob or a
comma-separated @auth-param@ list.

== Grammar (RFC 9110 §11.6.2)

@
Authorization = credentials
credentials   = auth-scheme [ 1*SP ( token68 / #auth-param ) ]
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.2>

See also: "Network.HTTP.Headers.WWWAuthenticate",
"Network.HTTP.Headers.ProxyAuthorization",
"Network.HTTP.Headers.AuthenticationInfo", "Network.HTTP.Headers.DPoP".
-}
module Network.HTTP.Headers.Authorization (
  Authorization (..),
  Credentials (..),
  CredentialContents (..),
  AuthScheme (..),
  CredentialParam (..),
  credentialParamBytes,
  token68Parser,
  authParamParser,
  authParamValueParser,
  authParamsUnique,
  authParamsUniqueOn,
  renderAuthParam,
  credentialsParser,
  renderCredentials,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.String
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


newtype AuthScheme = AuthScheme ShortText
  deriving stock (Eq, Show)


data CredentialParam
  = CredentialParamToken {-# UNPACK #-} !ShortText
  | CredentialParamString {-# UNPACK #-} !RFC8941String
  | CredentialParamOpaque !ByteString
  deriving stock (Show)


instance Eq CredentialParam where
  CredentialParamToken a == CredentialParamToken b = a == b
  CredentialParamString a == CredentialParamString b = a == b
  a == b = credentialParamBytes a == credentialParamBytes b


-- | The original parameter value bytes, without quoted-string framing.
credentialParamBytes :: CredentialParam -> ByteString
credentialParamBytes = \case
  CredentialParamToken token -> ST.toByteString token
  CredentialParamString str -> ST.toByteString (unsafeToRFC8941String str)
  CredentialParamOpaque bytes -> bytes


instance IsString CredentialParam where
  fromString str = case runParser rfc9110Token $ fromString str of
    OK token "" -> CredentialParamToken token
    _ -> case mkRFC8941String $ fromString str of
      Just s -> CredentialParamString s
      Nothing -> error "Failed to parse string as RFC8941String"


data CredentialContents
  = CredentialBare
  | CredentialToken ByteString
  | CredentialParams (NE.NonEmpty (ShortText, CredentialParam))
  deriving stock (Eq, Show)


data Credentials = Credentials
  { scheme :: !AuthScheme
  , contents :: !CredentialContents
  }
  deriving stock (Eq, Show)


credentialsParser :: ParserT st e Credentials
credentialsParser = do
  s <- AuthScheme <$> rfc9110Token
  c <- (skipSome $(char ' ') *> payload) <|> pure CredentialBare
  pure $ Credentials s c
  where
    payload = (tokenCredential <* lookahead (ows *> eof)) <|> paramCredential
    tokenCredential = CredentialToken <$> token68Parser
    paramCredential = do
      ps <- httpList authParamParser
      case NE.nonEmpty ps of
        Nothing -> pure CredentialBare
        Just params | authParamsUnique ps -> pure (CredentialParams params)
        _ -> failed


-- | A token68 body followed by optional padding.
token68Parser :: ParserT st e ByteString
token68Parser =
  byteStringOf $
    asciiAlnumSetBytes token68Chars *> skipMany (skipSatisfyAscii (== '='))
{-# INLINE token68Parser #-}


token68Chars :: AsciiSet
token68Chars = asciiSet (CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "-._~+/")
{-# NOINLINE token68Chars #-}


authParamParser :: ParserT st e (ShortText, CredentialParam)
authParamParser = do
  key <- rfc9110Token
  bws
  $(char '=')
  bws
  value <- authParamValueParser
  pure (key, value)
{-# INLINE authParamParser #-}


authParamValueParser :: ParserT st e CredentialParam
authParamValueParser =
  (CredentialParamToken <$> rfc9110Token) <|> (fromQuoted <$> httpQuotedBytes)
  where
    fromQuoted bytes
      | BS.all (< 128) bytes = CredentialParamString (RFC8941String (STU.fromByteStringUnsafe bytes))
      | otherwise = CredentialParamOpaque bytes
{-# INLINE authParamValueParser #-}


authParamsUnique :: [(ShortText, CredentialParam)] -> Bool
authParamsUnique = authParamsUniqueOn id


-- | Check parameter names case-insensitively after any field-specific normalization.
authParamsUniqueOn :: (ByteString -> ByteString) -> [(ShortText, CredentialParam)] -> Bool
authParamsUniqueOn _ [] = True
authParamsUniqueOn _ [_] = True
authParamsUniqueOn normalize [first, second] = key first /= key second
  where
    key = CI.mk . normalize . ST.toByteString . fst
authParamsUniqueOn normalize params = go Set.empty params
  where
    go _ [] = True
    go seen ((name, _) : rest) =
      let key = CI.mk (normalize (ST.toByteString name))
      in Set.notMember key seen && go (Set.insert key seen) rest
{-# INLINE authParamsUniqueOn #-}


renderCredentials :: Credentials -> M.Builder
renderCredentials = \case
  Credentials (AuthScheme s) CredentialBare -> R.shortText s
  Credentials (AuthScheme s) (CredentialToken tok) -> R.shortText s <> M.char7 ' ' <> M.byteString tok
  Credentials (AuthScheme s) (CredentialParams params) -> R.shortText s <> M.char7 ' ' <> R.sepByCommas1 (fmap renderAuthParam params)


renderAuthParam :: (ShortText, CredentialParam) -> M.Builder
renderAuthParam (key, val) =
  R.shortText key <> M.char7 '=' <> case val of
    CredentialParamToken token -> R.shortText token
    CredentialParamString str -> R.renderHttpQuotedString (ST.toText (unsafeToRFC8941String str))
    CredentialParamOpaque bytes -> R.renderHttpQuotedString (TextEncoding.decodeLatin1 bytes)


newtype Authorization = Authorization {authorizationCredentials :: Credentials}
  deriving stock (Eq, Show)


instance KnownHeader Authorization where
  type ParseFailure Authorization = String
  type Cardinality Authorization = 'ZeroOrOne
  type Direction Authorization = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Authorization" (Authorization <$> credentialsParser)


  renderToHeaders _ = M.toStrictByteString . renderCredentials . authorizationCredentials


  headerName _ = hAuthorization
