{-# LANGUAGE TemplateHaskell #-}

{- |
@Authorization@ — the request header a client uses to present
credentials to an origin server, typically answering a
@WWW-Authenticate@ challenge (though it may be sent pre-emptively). It
carries a single @credentials@ value: an auth-scheme (e.g. @Basic@,
@Bearer@, @Digest@) followed by either a @token68@ blob or a
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
  credentialsParser,
  renderCredentials,
) where

import Data.ByteString (ByteString)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.Text.Short (ShortText)
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
  deriving stock (Show)


instance Eq CredentialParam where
  CredentialParamToken a == CredentialParamToken b = a == b
  CredentialParamString a == CredentialParamString b = a == b
  CredentialParamString a == CredentialParamToken b = unsafeToRFC8941String a == b
  _ == _ = False


instance IsString CredentialParam where
  fromString str = case runParser rfc9110Token $ fromString str of
    OK token "" -> CredentialParamToken token
    _ -> case mkRFC8941String $ fromString str of
      Just s -> CredentialParamString s
      Nothing -> error "Failed to parse string as RFC8941String"


data CredentialContents
  = CredentialToken ByteString
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
  skipSome $(char ' ')
  c <- paramCredential <|> tokenCredential
  pure $ Credentials s c
  where
    tokenCredential = CredentialToken <$> rfc7230Token68Parser
    paramCredential = CredentialParams <$> credentialsP
    credentialsP = do
      rfc8941List1 authParamP
    authParamP = do
      key <- rfc9110Token
      bws
      $(char '=')
      bws
      val <- (CredentialParamString <$> rfc8941String) <|> (CredentialParamToken <$> rfc9110Token)
      pure (key, val)
    rfc7230Token68Parser = byteStringOf (skipSome $ skipSatisfyAscii (`CharSet.member` token68Chars))
      where
        token68Chars = CharSet.fromList $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-._~+/="


renderCredentials :: Credentials -> M.Builder
renderCredentials = \case
  Credentials (AuthScheme s) (CredentialToken tok) -> R.shortText s <> M.char7 ' ' <> M.byteString tok
  Credentials (AuthScheme s) (CredentialParams params) -> R.shortText s <> M.char7 ' ' <> M.intersperse (M.char7 ',') (NE.toList $ fmap renderParam params)
  where
    renderParam (key, val) =
      R.shortText key
        <> M.char7 '='
        <> ( case val of
              CredentialParamToken tok -> R.shortText tok
              CredentialParamString str -> R.rfc8941String str
           )


newtype Authorization = Authorization {authorizationCredentials :: Credentials}
  deriving stock (Eq, Show)


instance KnownHeader Authorization where
  type ParseFailure Authorization = String
  type Cardinality Authorization = 'One
  type Direction Authorization = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser credentialsParser header of
      OK creds "" -> Right $ Authorization creds
      OK _ rest -> Left $ "Unconsumed input after parsing Authorization header: " <> show rest
      Fail -> Left "Failed to parse Authorization header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCredentials . authorizationCredentials


  headerName _ = hAuthorization
