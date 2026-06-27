{-# LANGUAGE TemplateHaskell #-}

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

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.Char (chr, digitToInt, isHexDigit, ord)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Word (Word8)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hALPN)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.TLS.Extensions (ALPNProtocol (alpnIdentificationSequence), mkALPNProtocol)


-- | A non-empty list of ALPN protocol IDs, as carried by the @ALPN@ header.
newtype ALPN = ALPN {alpnProtocols :: NE.NonEmpty ALPNProtocol}
  deriving stock (Eq, Show)


instance KnownHeader ALPN where
  type ParseFailure ALPN = String
  type Cardinality ALPN = 'ZeroOrOne
  type Direction ALPN = 'Request


  parseFromHeaders _ headers = case runParser alpnParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing ALPN header: " <> show rest
    Fail -> Left "Failed to parse ALPN header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderALPN


  headerName _ = hALPN


{- | Parse @1#protocol-id@: a comma-separated, non-empty list of percent-encoded
ALPN protocol IDs, each decoded back to its raw octet sequence.
-}
alpnParser :: ParserT st String ALPN
alpnParser = ALPN <$> protocolIdParser `sepBy1` (ows *> $(char ',') *> ows)


{- | Parse one @protocol-id@ @token@ and percent-decode it into the underlying
'ALPNProtocol' octet sequence.
-}
protocolIdParser :: ParserT st String ALPNProtocol
protocolIdParser = mkALPNProtocol . BS.pack <$> some (pctOctet <|> rawOctet)
  where
    -- A literal token octet, never the percent sign (which always introduces an
    -- escape on the wire).
    rawOctet = fromIntegral . ord <$> satisfyAscii isRawTokenChar
    -- A @%XX@ escape decoded to a single octet.
    pctOctet = $(char '%') *> (combineNibbles <$> hexNibble <*> hexNibble)
    combineNibbles hi lo = hi * 16 + lo


hexNibble :: ParserT st String Word8
hexNibble = fromIntegral . digitToInt <$> satisfyAscii isHexDigit


{- | A @token@ character that does not require percent-encoding: any RFC 9110
@tchar@ other than the percent sign.
-}
isRawTokenChar :: Char -> Bool
isRawTokenChar c = c /= '%' && CharSet.member c tokenCharSet


{- | Render the @ALPN@ header value: each protocol ID percent-encoded per
RFC 7639, joined with @", "@.
-}
renderALPN :: ALPN -> M.Builder
renderALPN = M.intersperse ", " . map renderProtocolId . NE.toList . alpnProtocols


-- | Percent-encode one 'ALPNProtocol' octet sequence into a @protocol-id@ token.
renderProtocolId :: ALPNProtocol -> M.Builder
renderProtocolId = foldMap encodeOctet . BS.unpack . alpnIdentificationSequence


encodeOctet :: Word8 -> M.Builder
encodeOctet w
  | isRawTokenByte w = M.word8 w
  | otherwise = M.char7 '%' <> hexDigit (w `shiftR` 4) <> hexDigit (w .&. 0x0F)


{- | Whether an octet may appear verbatim in a @protocol-id@: an ASCII @tchar@
other than the percent sign (@0x25@).
-}
isRawTokenByte :: Word8 -> Bool
isRawTokenByte w = w /= 0x25 && w < 0x80 && CharSet.member (chr (fromIntegral w)) tokenCharSet


-- | Render a nibble (0–15) as a single uppercase hex digit.
hexDigit :: Word8 -> M.Builder
hexDigit n
  | n < 10 = M.word8 (0x30 + n)
  | otherwise = M.word8 (0x41 + n - 10)
