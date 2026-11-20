{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
@traceparent@ request\/response header — W3C Trace Context.

The @traceparent@ field carries the incoming distributed-trace
context across service boundaries. For the only currently defined
version (@00@) its value is four hyphen-separated hex fields:

@
traceparent = version \"-\" trace-id \"-\" parent-id \"-\" trace-flags
version     = 2HEXDIG     ; \"00\"
trace-id    = 32HEXDIG    ; 16-byte trace identifier, lowercase hex
parent-id   = 16HEXDIG    ; 8-byte calling-span identifier
trace-flags = 2HEXDIG     ; 8-bit field, e.g. \"01\" == sampled
@

Identifiers are surfaced as their lowercase wire text. Version and
flags are decoded to octets. Future versions can carry an opaque suffix
after the known fields, which the codec retains for forwarding.

Spec: <https://www.w3.org/TR/trace-context/#traceparent-header>.

See also: "Network.HTTP.Headers.Tracestate", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.XCorrelationID", "Network.HTTP.Headers.XRequestID".
-}
module Network.HTTP.Headers.Traceparent (
  Traceparent (..),
  traceparentParser,
  renderTraceparent,
) where

import Data.Bits (complement, shiftL, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (Int (I#), eqAddr#, indexWord8OffAddrAsWord64#, isTrue#, minusAddr#, plusAddr#, (*#), (<=#))
import GHC.Word (Word64 (W64#))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTraceparent)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Builder.Internal.Prim (word8HexFixed)
import Wireform.Encode.Direct (dBytes)
import Wireform.Parser.Internal (Parser (..), pattern Fail#, pattern OK#)


-- | The known fields and, for newer versions, an optional opaque extension.
data Traceparent
  = Traceparent
      { traceparentVersion :: {-# UNPACK #-} !Word8
      , traceparentTraceId :: !ST.ShortText
      , traceparentParentId :: !ST.ShortText
      , traceparentFlags :: {-# UNPACK #-} !Word8
      }
  | TraceparentWithExtension
      { traceparentVersion :: {-# UNPACK #-} !Word8
      , traceparentTraceId :: !ST.ShortText
      , traceparentParentId :: !ST.ShortText
      , traceparentFlags :: {-# UNPACK #-} !Word8
      , traceparentExtension :: !ByteString
      -- ^ The opaque bytes following the extension separator.
      }
  deriving stock (Eq, Show)


instance KnownHeader Traceparent where
  type ParseFailure Traceparent = String
  type Cardinality Traceparent = 'ZeroOrOne
  type Direction Traceparent = 'RequestAndResponse


  parseFromHeaders _ = parseSingleFieldLine "traceparent" traceparentParser


  renderToHeaders _ = renderTraceparentField


  headerName _ = hTraceparent


traceparentParser :: ParserT st String Traceparent
traceparentParser = do
  version <- hexWord8
  if version == 255 then err "Reserved traceparent version ff" else pure ()
  $(char '-')
  traceId <- nonzeroHexWords 4
  $(char '-')
  parentId <- nonzeroHexWords 2
  $(char '-')
  flags <- hexWord8
  let base = Traceparent version traceId parentId flags
  if version == 0
    then pure base
    else branch $(char '-') (TraceparentWithExtension version traceId parentId flags <$> extensionBytes) (pure base)
  where
    extensionBytes = do
      rest <- takeRest
      let suffix = BS.dropWhileEnd (\b -> b == 32 || b == 9) rest
      if BS.all (\b -> b == 9 || (b >= 32 && b /= 127)) suffix
        then pure suffix
        else err "Invalid byte in traceparent extension"


renderTraceparent :: Traceparent -> M.Builder
renderTraceparent value =
  word8HexFixed (traceparentVersion value)
    <> M.char7 '-'
    <> shortText (traceparentTraceId value)
    <> M.char7 '-'
    <> shortText (traceparentParentId value)
    <> M.char7 '-'
    <> word8HexFixed (traceparentFlags value)
    <> case value of
      Traceparent {} -> mempty
      TraceparentWithExtension {traceparentExtension = suffix} -> M.char7 '-' <> M.byteString suffix


-- The header map needs one strict field line. Size the supplied identifiers
-- so legacy record constructors retain their rendering too.
renderTraceparentField :: Traceparent -> ByteString
renderTraceparentField value = BSI.unsafeCreate size $ \out -> do
  PI.runF P.word8HexFixed (traceparentVersion value) out
  dash out 2
  SBSI.copyToPtr traceId 0 (out `plusPtr` 3) traceSize
  dash out (3 + traceSize)
  SBSI.copyToPtr parentId 0 (out `plusPtr` parentStart) parentSize
  dash out (baseSize - 3)
  PI.runF P.word8HexFixed (traceparentFlags value) (out `plusPtr` (baseSize - 2))
  case value of
    Traceparent {} -> pure ()
    TraceparentWithExtension {traceparentExtension = suffix} -> do
      dash out baseSize
      _ <- dBytes out (baseSize + 1) suffix
      pure ()
  where
    traceId = ST.toShortByteString (traceparentTraceId value)
    parentId = ST.toShortByteString (traceparentParentId value)
    traceSize = SBS.length traceId
    parentSize = SBS.length parentId
    parentStart = 4 + traceSize
    baseSize = addSize (addSize traceSize parentSize) 7
    size = case value of
      Traceparent {} -> baseSize
      TraceparentWithExtension {traceparentExtension = suffix} -> addSize (addSize baseSize 1) (BS.length suffix)
    addSize = BSI.checkedAdd "traceparent"
    dash out offset = pokeByteOff out offset (45 :: Word8)


-- | Scan a fixed number of eight-byte groups in one bounded identifier.
nonzeroHexWords :: Int -> ParserT st e ST.ShortText
nonzeroHexWords (I# wordsInId) = shortASCIIFromParser_ $ Parser $ \_ end start st ->
  case width <=# minusAddr# end start of
    1# ->
      let stop = plusAddr# start width
          scan cur !nonzero
            | isTrue# (eqAddr# cur stop) =
                if nonzero == 0 then (# st, Fail# #) else (# st, OK# () cur #)
            | otherwise =
                let bytes = W64# (indexWord8OffAddrAsWord64# cur 0#)
                in if lowercaseHex bytes
                     then scan (plusAddr# cur 8#) (nonzero .|. (bytes `xor` 0x3030_3030_3030_3030))
                     else (# st, Fail# #)
      in scan start 0
    _ -> (# st, Fail# #)
  where
    width = wordsInId *# 8#
{-# INLINE nonzeroHexWords #-}


-- With each lane biased by 128, subtraction tests inclusive byte bounds
-- without borrowing into a neighbouring lane. The masks are endian-neutral.
lowercaseHex :: Word64 -> Bool
lowercaseHex bytes = (digits .|. letters) .&. complement bytes .&. high == high
  where
    high = 0x8080_8080_8080_8080
    low = bytes .&. complement high
    biased = bytes .|. high
    digits = (biased - 0x3030_3030_3030_3030) .&. (0xb9b9_b9b9_b9b9_b9b9 - low)
    letters = (biased - 0x6161_6161_6161_6161) .&. (0xe6e6_e6e6_e6e6_e6e6 - low)
{-# INLINE lowercaseHex #-}


-- | Parse exactly two hex digits as a single octet.
hexWord8 :: ParserT st e Word8
hexWord8 = withAnyWord8 $ \a -> withAnyWord8 $ \b ->
  let hi = hexNibble a
      lo = hexNibble b
  in if hi <= 15 && lo <= 15 then pure (hi `shiftL` 4 .|. lo) else failed
{-# INLINE hexWord8 #-}


hexNibble :: Word8 -> Word8
hexNibble byte
  | digit <= 9 = digit
  | letter <= 5 = letter + 10
  | otherwise = 255
  where
    digit = byte - 48
    letter = byte - 97
{-# INLINE hexNibble #-}
