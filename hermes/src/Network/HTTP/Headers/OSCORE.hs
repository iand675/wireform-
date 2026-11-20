{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

{- | RFC 8613 §11.1 @OSCORE@ carries a CoAP OSCORE option in unpadded
base64url. The wire value @AA@ represents the empty CoAP option.
-}
module Network.HTTP.Headers.OSCORE (
  OSCORE (OSCORE, oscoreValue),
  oscoreOptionBytes,
  oscoreFromOptionBytes,
  oscoreParser,
  renderOSCORE,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import GHC.Exts (Int (I#), indexWord8OffAddr#, isTrue#, keepAlive#, mutableByteArrayContents#, newPinnedByteArray#, runRW#, word2Int#, word8ToWord#, writeWord8Array#, (+#), (>#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), ForeignPtrContents (PlainPtr))
import GHC.Word (Word8 (W8#))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOSCORE)
import qualified Network.HTTP.Headers.Mason as M


data OSCORE
  = RawOSCORE !ST.ShortText
  | ParsedOSCORE !ByteString !ByteString


{- | The legacy encoded-text view. Use 'oscoreFromOptionBytes' to construct
the HTTP field from the mapped CoAP option.
-}
pattern OSCORE :: ST.ShortText -> OSCORE
pattern OSCORE {oscoreValue} <- (legacyText -> oscoreValue)
  where
    OSCORE text = RawOSCORE text


{-# COMPLETE OSCORE #-}


instance Eq OSCORE where
  left == right = wireBytes left == wireBytes right


instance Show OSCORE where
  showsPrec precedence (OSCORE wire) =
    showParen (precedence > 10) $ showString "OSCORE {oscoreValue = " . shows wire . showChar '}'


legacyText :: OSCORE -> ST.ShortText
legacyText (RawOSCORE text) = text
legacyText (ParsedOSCORE wire _) = STU.fromByteStringUnsafe wire


wireBytes :: OSCORE -> ByteString
wireBytes (RawOSCORE text) = ST.toByteString text
wireBytes (ParsedOSCORE wire _) = wire
{-# INLINE wireBytes #-}


-- | Decode the HTTP-to-CoAP mapping in RFC 8613 §11.3.
oscoreOptionBytes :: OSCORE -> Either String ByteString
oscoreOptionBytes (RawOSCORE text) = decodeOption (ST.toByteString text)
oscoreOptionBytes (ParsedOSCORE _ option) = Right option
{-# INLINE oscoreOptionBytes #-}


{- | Encode a CoAP option. A single zero octet shares the empty option's
wire representation and decodes as the empty option.
-}
oscoreFromOptionBytes :: ByteString -> OSCORE
oscoreFromOptionBytes option
  | BS.null option || BS.length option == 1 && BSU.unsafeHead option == 0 = emptyOption
  | otherwise = ParsedOSCORE (encodeOption option) option
{-# INLINE oscoreFromOptionBytes #-}


-- The common one- to three-byte options need just one base64 quantum.
encodeOption :: ByteString -> ByteString
encodeOption option
  | BS.length option <= 3 = encodeShort option
  | otherwise = Base64URL.encodeUnpadded option
{-# INLINE encodeOption #-}


-- The caller guarantees one to three source octets. Keep the source alive
-- through the indexed reads, and expose only the unpadded output length.
encodeShort :: ByteString -> ByteString
encodeShort (BSI.BS (ForeignPtr source backing) (I# size)) = runRW# $ \initial ->
  case keepAlive# backing initial writeQuantum of
    (# _, encoded #) -> encoded
  where
    writeQuantum state =
      case newPinnedByteArray# 4# state of
        (# next, output #) ->
          let first = W8# (indexWord8OffAddr# source 0#)
              second = if isTrue# (size ># 1#) then W8# (indexWord8OffAddr# source 1#) else 0
              third = if isTrue# (size ># 2#) then W8# (indexWord8OffAddr# source 2#) else 0
              a = writeDigit output 0# (first `shiftR` 2) next
              b = writeDigit output 1# (((first .&. 3) `shiftL` 4) .|. (second `shiftR` 4)) a
              c = writeDigit output 2# (((second .&. 15) `shiftL` 2) .|. (third `shiftR` 6)) b
              d = writeDigit output 3# (third .&. 63) c
          in (# d, BSI.BS (ForeignPtr (mutableByteArrayContents# output) (PlainPtr output)) (I# (size +# 1#)) #)
    writeDigit output position (W8# sextet) =
      writeWord8Array# output position (indexWord8OffAddr# "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"# (word2Int# (word8ToWord# sextet)))
{-# INLINE encodeShort #-}


emptyOption :: OSCORE
emptyOption = ParsedOSCORE "AA" BS.empty


instance KnownHeader OSCORE where
  type ParseFailure OSCORE = String
  type Cardinality OSCORE = 'ZeroOrOne
  type Direction OSCORE = 'RequestAndResponse


  parseFromHeaders _ (wire :| []) = decodeField wire
  parseFromHeaders _ _ = Left "Multiple OSCORE field lines"


  renderToHeaders _ = wireBytes


  headerName _ = hOSCORE


oscoreParser :: ParserT st String OSCORE
oscoreParser = takeRest >>= either err pure . decodeField


decodeField :: ByteString -> Either String OSCORE
decodeField input
  | input == "AA" || wire == "AA" = Right emptyOption
  | otherwise = ParsedOSCORE wire <$> decodeNonEmptyOption wire
  where
    wire = BS.dropWhileEnd isFieldOWS (BS.dropWhile isFieldOWS input)
    isFieldOWS octet = octet == 32 || octet == 9


decodeOption :: ByteString -> Either String ByteString
decodeOption wire
  | wire == "AA" = Right BS.empty
  | otherwise = decodeNonEmptyOption wire


decodeNonEmptyOption :: ByteString -> Either String ByteString
decodeNonEmptyOption wire
  | BS.length wire < 2 = Left "OSCORE requires at least two base64url characters"
  | otherwise = Base64URL.decodeUnpadded wire


renderOSCORE :: OSCORE -> M.Builder
renderOSCORE (RawOSCORE text) = shortText text
renderOSCORE (ParsedOSCORE wire _) = M.byteString wire
