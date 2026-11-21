{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | RFC 8473 §2 @Sec-Token-Binding@ carries an unpadded base64url
TokenBindingMessage. The codec checks the RFC 8471 binary framing and
the HTTP rules for provided and referred bindings.
-}
module Network.HTTP.Headers.SecTokenBinding (
  SecTokenBinding (SecTokenBinding, secTokenBindingMessage),
  TokenBinding (..),
  TokenBindingType (..),
  TokenBindingKeyParameters (..),
  TokenBindingExtension (..),
  secTokenBindingBytes,
  secTokenBindingEntries,
  secTokenBindingFromBytes,
  secTokenBindingFromEntries,
  secTokenBindingParser,
  renderSecTokenBinding,
) where

import Control.Monad (foldM, when)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64URL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word16, Word8)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecTokenBinding)
import qualified Network.HTTP.Headers.Mason as M
import qualified Wireform.Builder as WB


data SecTokenBinding
  = RawSecTokenBinding !ST.ShortText
  | ParsedSecTokenBinding !ByteString !ByteString !(NonEmpty TokenBinding)


{- | The legacy encoded-text view. Use the checked constructors for
typed authoring.
-}
pattern SecTokenBinding :: ST.ShortText -> SecTokenBinding
pattern SecTokenBinding {secTokenBindingMessage} <- (legacyText -> secTokenBindingMessage)
  where
    SecTokenBinding text = RawSecTokenBinding text


{-# COMPLETE SecTokenBinding #-}


instance Eq SecTokenBinding where
  left == right = wireBytes left == wireBytes right


instance Show SecTokenBinding where
  showsPrec precedence (SecTokenBinding wire) =
    showParen (precedence > 10) $ showString "SecTokenBinding {secTokenBindingMessage = " . shows wire . showChar '}'


legacyText :: SecTokenBinding -> ST.ShortText
legacyText (RawSecTokenBinding text) = text
legacyText (ParsedSecTokenBinding wire _ _) = STU.fromByteStringUnsafe wire


wireBytes :: SecTokenBinding -> ByteString
wireBytes (RawSecTokenBinding text) = ST.toByteString text
wireBytes (ParsedSecTokenBinding wire _ _) = wire


data TokenBindingType
  = ProvidedTokenBinding
  | ReferredTokenBinding
  | OtherTokenBindingType !Word8
  deriving stock (Eq, Show)


data TokenBindingKeyParameters
  = RSA2048PKCS1
  | RSA2048PSS
  | ECDSAP256
  | OtherTokenBindingKeyParameters !Word8
  deriving stock (Eq, Show)


data TokenBindingExtension = TokenBindingExtension
  { bindingExtensionType :: {-# UNPACK #-} !Word8
  , bindingExtensionBytes :: !ByteString
  }
  deriving stock (Eq, Show)


{- | An entry with its public key in RFC 8471 binary form and its opaque
signature bytes. Signature verification requires the TLS context.
-}
data TokenBinding = TokenBinding
  { bindingType :: !TokenBindingType
  , bindingKeyParameters :: !TokenBindingKeyParameters
  , bindingPublicKey :: !ByteString
  , bindingSignature :: !ByteString
  , bindingExtensions :: ![TokenBindingExtension]
  }
  deriving stock (Eq, Show)


secTokenBindingBytes :: SecTokenBinding -> Either String ByteString
secTokenBindingBytes (RawSecTokenBinding text) = do
  bytes <- decodeWire (ST.toByteString text)
  _ <- decodeEntries bytes
  pure bytes
secTokenBindingBytes (ParsedSecTokenBinding _ bytes _) = Right bytes


secTokenBindingEntries :: SecTokenBinding -> Either String (NonEmpty TokenBinding)
secTokenBindingEntries (RawSecTokenBinding text) = decodeWire (ST.toByteString text) >>= decodeEntries
secTokenBindingEntries (ParsedSecTokenBinding _ _ entries) = Right entries


secTokenBindingFromBytes :: ByteString -> Either String SecTokenBinding
secTokenBindingFromBytes bytes =
  ParsedSecTokenBinding (Base64URL.encodeUnpadded bytes) bytes <$> decodeEntries bytes


secTokenBindingFromEntries :: NonEmpty TokenBinding -> Either String SecTokenBinding
secTokenBindingFromEntries entries = do
  seen <- foldM (\flags binding -> bindingFlags flags (bindingType binding)) 0 entries
  when (not (testBit seen 0)) (Left missingProvided)
  packet@(EncodedVector size _) <- foldM addBinding emptyVector entries
  when (size < minimumBindingVectorLength) (Left shortMessage)
  let bytes = WB.toStrictByteString (renderVector packet)
  pure (ParsedSecTokenBinding (Base64URL.encodeUnpadded bytes) bytes entries)


instance KnownHeader SecTokenBinding where
  type ParseFailure SecTokenBinding = String
  type Cardinality SecTokenBinding = 'ZeroOrOne
  type Direction SecTokenBinding = 'Request


  parseFromHeaders _ (wire :| []) = decodeField wire
  parseFromHeaders _ _ = Left "Multiple Sec-Token-Binding field lines"


  renderToHeaders _ = wireBytes


  renderToHeadersChecked _ (ParsedSecTokenBinding wire _ _) = Just wire
  renderToHeadersChecked _ (RawSecTokenBinding text) =
    let wire = ST.toByteString text
    in either (const Nothing) (const (Just wire)) (decodeField wire)


  headerName _ = hSecTokenBinding


secTokenBindingParser :: ParserT st String SecTokenBinding
secTokenBindingParser = takeRest >>= either err pure . decodeField


decodeField :: ByteString -> Either String SecTokenBinding
decodeField input = do
  bytes <- decodeWire wire
  ParsedSecTokenBinding wire bytes <$> decodeEntries bytes
  where
    wire = BS.dropWhileEnd isFieldOWS (BS.dropWhile isFieldOWS input)
    isFieldOWS octet = octet == 32 || octet == 9


-- A TLS uint16 vector prefix plus 132 to 65535 bytes of entries.
decodeWire :: ByteString -> Either String ByteString
decodeWire wire
  | size < encodedLength (minimumBindingVectorLength + 2) || size > encodedLength (vectorLimit + 2) = Left "Invalid Sec-Token-Binding message length"
  | otherwise = Base64URL.decodeUnpadded wire
  where
    size = BS.length wire
    encodedLength bytes = (4 * bytes + 2) `quot` 3


decodeEntries :: ByteString -> Either String (NonEmpty TokenBinding)
decodeEntries bytes = case runParser (messageParser <* eof) bytes of
  OK entries _ -> Right entries
  Fail -> Left "Invalid TokenBindingMessage framing"
  Err message -> Left message


messageParser :: ParserT st String (NonEmpty TokenBinding)
messageParser = do
  size <- anyWord16be
  if fromIntegral size < minimumBindingVectorLength
    then err shortMessage
    else isolate (fromIntegral size) $ do
      first <- bindingParser
      flags <- either err pure (bindingFlags 0 (bindingType first))
      rest <- bindingsUntilEnd flags
      pure (first :| rest)
  where
    bindingsUntilEnd flags =
      branch
        eof
        (if testBit flags 0 then pure [] else err missingProvided)
        ( do
            binding <- bindingParser
            updated <- either err pure (bindingFlags flags (bindingType binding))
            (binding :) <$> bindingsUntilEnd updated
        )


bindingFlags :: Word8 -> TokenBindingType -> Either String Word8
bindingFlags flags = \case
  ProvidedTokenBinding
    | testBit flags 0 -> Left "Duplicate provided Token Binding"
    | otherwise -> Right (flags + 1)
  ReferredTokenBinding
    | testBit flags 1 -> Left "Duplicate referred Token Binding"
    | otherwise -> Right (flags + 2)
  OtherTokenBindingType code
    | code < 2 -> Left "Known Token Binding type needs its named constructor"
    | otherwise -> Right flags
{-# INLINE bindingFlags #-}


missingProvided :: String
missingProvided = "Missing provided Token Binding"


minimumBindingVectorLength :: Int
minimumBindingVectorLength = 132


shortMessage :: String
shortMessage = "TokenBindingMessage requires at least 132 entry bytes"


bindingParser :: ParserT st String TokenBinding
bindingParser = do
  typ <- withAnyWord8 $ \case
    0 -> pure ProvidedTokenBinding
    1 -> pure ReferredTokenBinding
    code -> pure (OtherTokenBindingType code)
  params <- withAnyWord8 $ \case
    0 -> pure RSA2048PKCS1
    1 -> pure RSA2048PSS
    2 -> pure ECDSAP256
    code -> pure (OtherTokenBindingKeyParameters code)
  key <- vectorBytes
  signature <- vectorBytes
  either err pure (checkKeyAndSignature params key signature)
  extensionSize <- anyWord16be
  extensions <- isolate (fromIntegral extensionSize) (many extensionParser)
  pure (TokenBinding typ params key signature extensions)
  where
    extensionParser = TokenBindingExtension <$> anyWord8 <*> vectorBytes
{-# NOINLINE bindingParser #-}


vectorBytes :: ParserT st e ByteString
vectorBytes = anyWord16be >>= takeBs . fromIntegral
{-# INLINE vectorBytes #-}


checkKeyAndSignature :: TokenBindingKeyParameters -> ByteString -> ByteString -> Either String ()
checkKeyAndSignature params key signature
  | BS.length signature < 64 = Left "Token Binding signature requires at least 64 bytes"
  | otherwise = case params of
      RSA2048PKCS1 -> requireRSA
      RSA2048PSS -> requireRSA
      ECDSAP256
        | BS.length key == 65 && BS.head key == 64 && BS.length signature == 64 -> Right ()
        | otherwise -> Left "Invalid P-256 Token Binding key or signature length"
      OtherTokenBindingKeyParameters _ -> Right ()
  where
    requireRSA
      | BS.length signature == 256
      , BS.length key >= 260
      , BS.index key 0 == 1
      , BS.index key 1 == 0
      , testBit (BS.index key 2) 7
      , let exponentSize = fromIntegral (BS.index key 258)
      , exponentSize == BS.length key - 259
      , BS.index key 259 /= 0 =
          Right ()
      | otherwise = Left "Invalid RSA-2048 Token Binding key or signature length"
{-# INLINE checkKeyAndSignature #-}


renderSecTokenBinding :: SecTokenBinding -> M.Builder
renderSecTokenBinding (RawSecTokenBinding text) = shortText text
renderSecTokenBinding (ParsedSecTokenBinding wire _ _) = M.byteString wire


data EncodedVector = EncodedVector {-# UNPACK #-} !Int !WB.Builder


emptyVector :: EncodedVector
emptyVector = EncodedVector 0 mempty


vectorLimit :: Int
vectorLimit = fromIntegral (maxBound :: Word16)


appendVector :: EncodedVector -> Int -> WB.Builder -> Either String EncodedVector
appendVector (EncodedVector size builder) next fragment
  | next <= vectorLimit - size = Right (EncodedVector (size + next) (builder <> fragment))
  | otherwise = Left "Token Binding vector exceeds 65535 bytes"


renderVector :: EncodedVector -> WB.Builder
renderVector (EncodedVector size builder) = WB.word16BE (fromIntegral size) <> builder


addBinding :: EncodedVector -> TokenBinding -> Either String EncodedVector
addBinding packet (TokenBinding typ params key signature extensions) = do
  case params of
    OtherTokenBindingKeyParameters code | code < 3 -> Left "Known Token Binding key parameters need their named constructor"
    _ -> Right ()
  checkKeyAndSignature params key signature
  keyVector <- appendVector emptyVector (BS.length key) (WB.byteString key)
  signatureVector <- appendVector emptyVector (BS.length signature) (WB.byteString signature)
  extensionVector@(EncodedVector extensionSize _) <- foldM addExtension emptyVector extensions
  let size = 8 + BS.length key + BS.length signature + extensionSize
      builder =
        WB.word8 (typeCode typ)
          <> WB.word8 (parameterCode params)
          <> renderVector keyVector
          <> renderVector signatureVector
          <> renderVector extensionVector
  appendVector packet size builder
  where
    addExtension acc (TokenBindingExtension code bytes)
      | BS.length bytes <= vectorLimit - 3 =
          appendVector acc (3 + BS.length bytes) (WB.word8 code <> WB.word16BE (fromIntegral (BS.length bytes)) <> WB.byteString bytes)
      | otherwise = Left "Token Binding extension exceeds vector length"
    typeCode ProvidedTokenBinding = 0
    typeCode ReferredTokenBinding = 1
    typeCode (OtherTokenBindingType code) = code
    parameterCode RSA2048PKCS1 = 0
    parameterCode RSA2048PSS = 1
    parameterCode ECDSAP256 = 2
    parameterCode (OtherTokenBindingKeyParameters code) = code
