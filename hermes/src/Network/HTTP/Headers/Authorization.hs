{-# LANGUAGE BangPatterns #-}
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
  encodeCredentialsChecked,
  encodeCredentialEntriesChecked,
  encodeAuthParamsChecked,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.CaseInsensitive as CI
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.String
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.Encode.Direct (dBytes, dWord8)


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
authParamsUnique [(first, _), (second, _)] = not (sameNegotiationToken first second)
authParamsUnique params = authParamsUniqueOn id params


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


encodeCredentialsChecked :: Credentials -> Maybe ByteString
encodeCredentialsChecked value
  | bound < 0 = Nothing
  | otherwise = Just (BSI.unsafeCreateUptoN bound (\ptr -> writeCredentials ptr 0 value))
  where
    bound = credentialsBound value


encodeCredentialEntriesChecked :: (a -> Credentials) -> [a] -> Maybe ByteString
encodeCredentialEntriesChecked credentials =
  R.encodeCommaSeparatedChecked (credentialsBound . credentials) (\ptr offset -> writeCredentials ptr offset . credentials)
{-# INLINE encodeCredentialEntriesChecked #-}


encodeAuthParamsChecked :: [(ShortText, CredentialParam)] -> Maybe ByteString
encodeAuthParamsChecked = R.encodeCommaSeparatedChecked authParamBound writeAuthParam


credentialsBound :: Credentials -> Int
credentialsBound (Credentials (AuthScheme name) payload)
  | not (isHttpTokenText name) = -1
  | otherwise = case payload of
      CredentialBare -> width
      CredentialToken token
        | validToken68 token -> addAuthSize (addAuthSize width 1) (BS.length token)
        | otherwise -> -1
      CredentialParams params
        | authParamsUnique entries -> sizeParams (addAuthSize width 1) entries
        | otherwise -> -1
        where
          entries = NE.toList params
  where
    width = shortWidth name
    sizeParams !total [] = total
    sizeParams !total (param : later)
      | size < 0 = -1
      | null later = addAuthSize total size
      | otherwise = sizeParams (addAuthSize (addAuthSize total size) 2) later
      where
        size = authParamBound param
{-# INLINE credentialsBound #-}


validToken68 :: ByteString -> Bool
validToken68 wire = case token68Chars of
  AsciiSet low high ->
    size /= 0 && allowedByte low high 0 && body low high 1
  where
    size = BS.length wire
    allowedByte low high i = asciiMember low high (BSI.w2c (BSU.unsafeIndex wire i))
    body low high !i
      | i == size = True
      | allowedByte low high i = body low high (i + 1)
      | otherwise = padding i
    padding !i =
      i == size || (BSU.unsafeIndex wire i == 61 && padding (i + 1))


authParamBound :: (ShortText, CredentialParam) -> Int
authParamBound (key, value)
  | not (isHttpTokenText key) = -1
  | otherwise = case value of
      CredentialParamToken token
        | isHttpTokenText token -> addAuthSize start (shortWidth token)
      CredentialParamString str
        | let text = unsafeToRFC8941String str
        , isHttpQuotedText text -> quotedBound (shortWidth text)
      CredentialParamOpaque bytes
        | BS.all (isOpaqueByte . fromIntegral) bytes -> quotedBound (BS.length bytes)
      _ -> -1
  where
    start = addAuthSize 1 (shortWidth key)
    quotedBound len = addAuthSize (addAuthSize start 2) (addAuthSize len len)


shortWidth :: ShortText -> Int
shortWidth = SBS.length . ST.toShortByteString
{-# INLINE shortWidth #-}


addAuthSize :: Int -> Int -> Int
addAuthSize = BSI.checkedAdd "HTTP authentication field"
{-# INLINE addAuthSize #-}


writeCredentials :: Ptr Word8 -> Int -> Credentials -> IO Int
writeCredentials ptr offset (Credentials (AuthScheme name) payload) = do
  end <- writeAuthText ptr offset name
  case payload of
    CredentialBare -> pure end
    CredentialToken token -> dWord8 ptr end 32 >>= \start -> dBytes ptr start token
    CredentialParams (first NE.:| rest) -> do
      start <- dWord8 ptr end 32
      writeAuthParam ptr start first >>= writeRest rest
  where
    writeRest [] !start = pure start
    writeRest (param : later) !start = do
      afterComma <- dWord8 ptr start 44
      next <- dWord8 ptr afterComma 32
      writeAuthParam ptr next param >>= writeRest later
{-# INLINE writeCredentials #-}


writeAuthParam :: Ptr Word8 -> Int -> (ShortText, CredentialParam) -> IO Int
writeAuthParam ptr offset (key, value) = do
  end <- writeAuthText ptr offset key
  start <- dWord8 ptr end 61
  case value of
    CredentialParamToken token -> writeAuthText ptr start token
    CredentialParamString str ->
      R.foldHttpQuotedShortBytes (dWord8 ptr) start (ST.toShortByteString (unsafeToRFC8941String str))
    CredentialParamOpaque bytes -> writeOpaqueQuoted ptr start bytes


writeAuthText :: Ptr Word8 -> Int -> ShortText -> IO Int
writeAuthText ptr offset text = do
  let bytes = ST.toShortByteString text
      len = SBS.length bytes
  SBS.copyToPtr bytes 0 (ptr `plusPtr` offset) len
  pure $! offset + len
{-# INLINE writeAuthText #-}


writeOpaqueQuoted :: Ptr Word8 -> Int -> ByteString -> IO Int
writeOpaqueQuoted ptr offset bytes =
  BSU.unsafeUseAsCStringLen bytes $ \(source, size) ->
    let write !readOffset !out
          | readOffset == size = dWord8 ptr out 34
          | otherwise = do
              byte <- peekByteOff source readOffset
              unescaped <- if byte == 34 || byte == 92 then dWord8 ptr out 92 else pure out
              next <- dWord8 ptr unescaped byte
              write (readOffset + 1) next
    in dWord8 ptr offset 34 >>= write 0


newtype Authorization = Authorization {authorizationCredentials :: Credentials}
  deriving stock (Eq, Show)


instance KnownHeader Authorization where
  type ParseFailure Authorization = String
  type Cardinality Authorization = 'ZeroOrOne
  type Direction Authorization = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Authorization" (Authorization <$> credentialsParser)


  renderToHeaders _ = M.toStrictByteString . renderCredentials . authorizationCredentials


  renderToHeadersChecked _ = encodeCredentialsChecked . authorizationCredentials


  headerName _ = hAuthorization
