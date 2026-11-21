{-# LANGUAGE BangPatterns #-}

{- |
The @ALPN@ HTTP request header field, defined by
[RFC 7639](https://www.rfc-editor.org/rfc/rfc7639).

A client includes @ALPN@ in an HTTP @CONNECT@ request to advertise the
application protocol(s) it intends to use inside the tunnel, identified by their
[RFC 7301](https://www.rfc-editor.org/rfc/rfc7301) ALPN protocol IDs:

> ALPN        = 1#protocol-id
> protocol-id = token ; percent-encoded ALPN protocol identifier

Each @protocol-id@ is a 'Network.TLS.Extensions.ALPNProtocol' identification
sequence (an opaque octet string) rendered as a @token@. Octets that are not
valid @token@ characters (RFC 9110 §5.6.2), and the percent octet @0x25@
itself, are percent-encoded with __uppercase__ hex digits; every other octet is
emitted verbatim. This yields exactly one canonical spelling per protocol, so
@h2@ stays @h2@ while @http\/1.1@ becomes @http%2F1.1@:

> ALPN: h2, http%2F1.1

Note on layering: this module does /not/ reuse
'Network.TLS.Extensions.alpnProtocolParser' \/
'Network.TLS.Extensions.renderALPNProtocol' directly, because those describe the
raw TLS on-wire identification sequence, whereas the HTTP header form is
percent-encoded. We build on the contract that matters — the
'Network.TLS.Extensions.ALPNProtocol' newtype over the raw octet sequence
('Network.TLS.Extensions.mkALPNProtocol' \/
'Network.TLS.Extensions.alpnIdentificationSequence') — and layer the RFC 7639
percent-codec on top.

Spec: <https://www.rfc-editor.org/rfc/rfc7639>

See also: "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.SecWebSocketProtocol", "Network.HTTP.Headers.SecWebSocketVersion".
-}
module Network.HTTP.Headers.ALPN (
  ALPN (..),
  alpnParser,
  renderALPN,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Word (Word8)
import Network.HTTP.Grammar.Rendering (encodeCommaSeparatedChecked)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hALPN)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.URL.Decode (urlDecode)
import Network.TLS.Extensions (ALPNProtocol (alpnIdentificationSequence), mkALPNProtocol)
import Wireform.Encode.Direct (dWord8)


-- | A nonempty list of ALPN protocol IDs. Each ID uses 1–255 decoded octets.
newtype ALPN = ALPN {alpnProtocols :: NonEmpty ALPNProtocol}
  deriving stock (Eq, Show)


instance KnownHeader ALPN where
  type ParseFailure ALPN = String
  type Cardinality ALPN = 'ZeroOrOne
  type Direction ALPN = 'Request


  parseFromHeaders _ (wire :| [])
    | not (BS.null wire)
    , BS.length wire <= 255
    , BS.all isRawTokenByte wire =
        Right $! ALPN (mkALPNProtocol wire :| [])
  parseFromHeaders _ headers =
    ALPN <$> (parseListFieldLines "ALPN" alpnList headers >>= requireNonEmptyList "ALPN")


  renderToHeaders _ = M.toStrictByteString . renderALPN


  renderToHeadersChecked _ (ALPN ids@(first :| []))
    | validProtocolSize (BS.length wire), BS.all isRawTokenByte wire = Just wire
    | otherwise = encodeProtocolsChecked ids
    where
      wire = alpnIdentificationSequence first
  renderToHeadersChecked _ (ALPN ids) = encodeProtocolsChecked ids


  headerName _ = hALPN


{- | Parse @1#protocol-id@: a comma-separated, non-empty list of percent-encoded
ALPN protocol IDs, each decoded back to its raw octet sequence.
-}
alpnParser :: ParserT st String ALPN
alpnParser = alpnList >>= either err (pure . ALPN) . requireNonEmptyList "ALPN"


alpnList :: ParserT st String [ALPNProtocol]
alpnList = httpList (withByteString tokenBytes (\() -> either err pure . decodeProtocol))


decodeProtocol :: ByteString -> Either String ALPNProtocol
decodeProtocol wire
  | size > 3 * 255 = Left "ALPN protocol identifier exceeds 255 octets"
  | otherwise = case BS.elemIndex 37 wire of
      Nothing
        | size <= 255 -> Right (mkALPNProtocol wire)
        | otherwise -> Left "ALPN protocol identifier exceeds 255 octets"
      Just start -> do
        checkEscapes start
        case urlDecode wire of
          Right decoded
            | BS.length decoded <= 255 -> Right (mkALPNProtocol decoded)
            | otherwise -> Left "ALPN protocol identifier exceeds 255 octets"
          Left _ -> Left "Invalid ALPN percent escape"
  where
    size = BS.length wire
    checkEscapes at
      | size - at < 3 = Left "Truncated ALPN percent escape"
      | hi > 15 || lo > 15 || isRawTokenByte (hi * 16 + lo) = Left "Noncanonical ALPN percent escape"
      | otherwise =
          let next = at + 3
          in case BS.elemIndex 37 (BSU.unsafeDrop next wire) of
               Nothing -> Right ()
               Just rest -> checkEscapes (next + rest)
      where
        hi = upperHexValue (BSU.unsafeIndex wire (at + 1))
        lo = upperHexValue (BSU.unsafeIndex wire (at + 2))


upperHexValue :: Word8 -> Word8
upperHexValue byte
  | byte >= 48 && byte <= 57 = byte - 48
  | byte >= 65 && byte <= 70 = byte - 55
  | otherwise = 255
{-# INLINE upperHexValue #-}


{- | Render the @ALPN@ header value: each protocol ID percent-encoded per
RFC 7639, joined with @", "@.
-}
renderALPN :: ALPN -> M.Builder
renderALPN (ALPN (first :| rest)) =
  renderProtocolId first <> foldMap ((", " <>) . renderProtocolId) rest


-- | Percent-encode one 'ALPNProtocol' octet sequence into a @protocol-id@ token.
renderProtocolId :: ALPNProtocol -> M.Builder
renderProtocolId = renderBytes . alpnIdentificationSequence
  where
    renderBytes bytes =
      let (prefix, suffix) = BS.break (not . isRawTokenByte) bytes
      in M.byteString prefix <> case BS.uncons suffix of
           Nothing -> mempty
           Just (byte, rest) -> encodeOctet byte <> renderBytes rest


encodeOctet :: Word8 -> M.Builder
encodeOctet w = M.char7 '%' <> hexDigit (w `shiftR` 4) <> hexDigit (w .&. 0x0F)


validProtocolSize :: Int -> Bool
validProtocolSize size = size >= 1 && size <= 255
{-# INLINE validProtocolSize #-}


encodeProtocolsChecked :: NonEmpty ALPNProtocol -> Maybe ByteString
encodeProtocolsChecked = encodeCommaSeparatedChecked bound writeId . NE.toList
  where
    bound protocol
      | validProtocolSize size = 3 * size
      | otherwise = -1
      where
        size = BS.length (alpnIdentificationSequence protocol)
    writeId ptr start protocol = writeBytes 0 start
      where
        bytes = alpnIdentificationSequence protocol
        size = BS.length bytes
        writeBytes !i !offset
          | i == size = pure offset
          | isRawTokenByte byte = dWord8 ptr offset byte >>= writeBytes (i + 1)
          | otherwise = do
              high <- dWord8 ptr offset 37
              low <- dWord8 ptr high (hexDigitByte (byte `shiftR` 4))
              end <- dWord8 ptr low (hexDigitByte (byte .&. 0x0F))
              writeBytes (i + 1) end
          where
            byte = BSU.unsafeIndex bytes i


{- | Whether an octet may appear verbatim in a @protocol-id@: an ASCII @tchar@
other than the percent sign (@0x25@).
-}
isRawTokenByte :: Word8 -> Bool
isRawTokenByte w = w /= 0x25 && isTokenChar (toEnum (fromIntegral w))
{-# INLINE isRawTokenByte #-}


-- | Render a nibble (0–15) as a single uppercase hex digit.
hexDigit :: Word8 -> M.Builder
hexDigit = M.word8 . hexDigitByte


hexDigitByte :: Word8 -> Word8
hexDigitByte n
  | n < 10 = 0x30 + n
  | otherwise = 0x41 + n - 10
{-# INLINE hexDigitByte #-}
