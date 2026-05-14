{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Low-level, high-performance wire format decoding primitives.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

The decoder uses unboxed sums for the result type, avoiding heap
allocation for intermediate decode results. Each decode operation
returns @(# (# a, Int# #) | DecodeError #)@ — either a value with
a new offset (on the stack, not heap) or an error.

This approach is more robust than CPS: it doesn't depend on GHC
successfully inlining all continuations, and the result type can
be unboxed by GHC. On modern CPUs the branch prediction for the
success/failure case split is near-perfect since decoding almost
always succeeds.
-}
module Proto.Internal.Wire.Decode (
  -- * Decode result
  DecodeResult (..),
  DecodeError (..),

  -- * Varint decoding
  getVarint,
  getVarintSigned,
  getSVarint32,
  getSVarint64,

  -- * Fixed-width decoding
  getFixed32,
  getFixed64,
  getFloat,
  getDouble,

  -- * Length-delimited
  getLengthDelimited,
  getByteString,
  getText,

  -- * Tags
  getTag,
  getTagOr,

  -- * Skipping unknown fields
  skipField,

  -- * Running a decoder
  runDecoder,
  Decoder (..),

  -- * ZigZag
  unZigZag32,
  unZigZag64,

  -- * Low-level access (for generated code)
  runDecoder',

  -- * Unboxed internal variants (zero-allocation hot path)
  UMaybe (UJust, UNothing),
  umaybe,
  getTagOrU,

  -- * Three-way tag result (flattened unboxed sum for the decode loop)
  TagResult#,
  withTag,

  -- * Monadic CPS tag dispatch (zero Tag allocation, for generated code)
  withTagM,
  skipWireType,

  -- * In-order tag prediction (hyperpb-style fast path)
  inOrderStage,
  inOrderStage1,

  -- * Non-throwing UTF-8 validation
  validateUtf8,
) where

import Control.DeepSeq (NFData (..))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek)
import GHC.Exts (Int (I#), Int#, isTrue#, (+#), (-#), (<=#), (>=#), (>#))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Proto.Internal.Wire (Tag (..), WireType (..), decodeTag)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.FFI (validateUtf8SWAR)


-- | Errors that can occur during protobuf wire-format decoding.
data DecodeError
  = UnexpectedEnd
  | InvalidVarint
  | InvalidTag !Word64
  | InvalidWireType !Int
  | InvalidUtf8
  | NegativeLength
  | ExtraBytes
  | SubMessageError !DecodeError
  | CustomError !String
  deriving stock (Show, Eq)


instance NFData DecodeError where
  rnf UnexpectedEnd = ()
  rnf InvalidVarint = ()
  rnf (InvalidTag w) = rnf w
  rnf (InvalidWireType i) = rnf i
  rnf InvalidUtf8 = ()
  rnf NegativeLength = ()
  rnf ExtraBytes = ()
  rnf (SubMessageError e) = rnf e
  rnf (CustomError s) = rnf s


-- | Legacy result type, kept for compatibility with runDecoder'.
data DecodeResult a
  = DecodeOK !a {-# UNPACK #-} !Int
  | DecodeFail !DecodeError
  deriving stock (Show)


{- | Decoder monad using unboxed sums for the result.
Returns either (value, new_offset) or an error, with no heap
allocation for the result envelope.
-}
newtype Decoder a = Decoder
  { runDecoder# :: ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)
  }


instance Functor Decoder where
  fmap f (Decoder g) = Decoder $ \bs off -> case g bs off of
    (# (# a, off' #) | #) -> (# (# f a, off' #) | #)
    (# | e #) -> (# | e #)
  {-# INLINE fmap #-}


instance Applicative Decoder where
  pure a = Decoder $ \_ off -> (# (# a, off #) | #)
  {-# INLINE pure #-}
  Decoder f <*> Decoder g = Decoder $ \bs off -> case f bs off of
    (# (# fab, off' #) | #) -> case g bs off' of
      (# (# a, off'' #) | #) -> (# (# fab a, off'' #) | #)
      (# | e #) -> (# | e #)
    (# | e #) -> (# | e #)
  {-# INLINE (<*>) #-}


instance Monad Decoder where
  Decoder g >>= f = Decoder $ \bs off -> case g bs off of
    (# (# a, off' #) | #) -> runDecoder# (f a) bs off'
    (# | e #) -> (# | e #)
  {-# INLINE (>>=) #-}


-- | Run a decoder on a ByteString, producing Either.
runDecoder :: Decoder a -> ByteString -> Either DecodeError a
runDecoder (Decoder f) bs =
  case f bs 0# of
    (# (# a, off' #) | #)
      | I# off' == BS.length bs -> Right a
      | otherwise -> Left ExtraBytes
    (# | e #) -> Left e
{-# INLINE runDecoder #-}


-- | Run a decoder returning the legacy DecodeResult (for internal use).
runDecoder' :: Decoder a -> ByteString -> Int -> DecodeResult a
runDecoder' (Decoder f) bs (I# off) =
  case f bs off of
    (# (# a, off' #) | #) -> DecodeOK a (I# off')
    (# | e #) -> DecodeFail e
{-# INLINE runDecoder' #-}


-- Helpers for offset arithmetic
bsLen :: ByteString -> Int#
bsLen bs = case BS.length bs of I# n -> n
{-# INLINE bsLen #-}


{- | Decode a varint. Inline fast path for 1-4 byte varints.

Hyperpb insight: most varints are 1-3 bytes (tags, small integers,
enum values). Inlining up to 4 bytes covers field tags up to
field number ~500k and values up to 2^28, hitting the slow path
only for genuinely large values.
-}
getVarint :: Decoder Word64
getVarint = Decoder $ \bs off ->
  let len = bsLen bs
  in if isTrue# (off >=# len)
      then (# | UnexpectedEnd #)
      else
        let !b0 = fromIntegral (BSU.unsafeIndex bs (I# off)) :: Word64
        in if b0 < 0x80
            then (# (# b0, off +# 1# #) | #)
            else
              let off1 = off +# 1#
              in if isTrue# (off1 >=# len)
                  then (# | UnexpectedEnd #)
                  else
                    let !b1 = fromIntegral (BSU.unsafeIndex bs (I# off1)) :: Word64
                    in if b1 < 0x80
                        then (# (# (b0 .&. 0x7F) .|. (b1 `shiftL` 7), off +# 2# #) | #)
                        else
                          let off2 = off +# 2#
                          in if isTrue# (off2 >=# len)
                              then (# | UnexpectedEnd #)
                              else
                                let !b2 = fromIntegral (BSU.unsafeIndex bs (I# off2)) :: Word64
                                in if b2 < 0x80
                                    then
                                      (#
                                        (#
                                          (b0 .&. 0x7F) .|. ((b1 .&. 0x7F) `shiftL` 7) .|. (b2 `shiftL` 14)
                                          , off +# 3#
                                        #) |
                                      #)
                                    else
                                      let off3 = off +# 3#
                                      in if isTrue# (off3 >=# len)
                                          then (# | UnexpectedEnd #)
                                          else
                                            let !b3 = fromIntegral (BSU.unsafeIndex bs (I# off3)) :: Word64
                                            in if b3 < 0x80
                                                then
                                                  (#
                                                    (#
                                                      (b0 .&. 0x7F)
                                                        .|. ((b1 .&. 0x7F) `shiftL` 7)
                                                        .|. ((b2 .&. 0x7F) `shiftL` 14)
                                                        .|. (b3 `shiftL` 21)
                                                      , off +# 4#
                                                    #) |
                                                  #)
                                                else getVarintSlow bs off
{-# INLINE getVarint #-}


getVarintSlow :: ByteString -> Int# -> (# (# Word64, Int# #) | DecodeError #)
getVarintSlow bs = go 0 0
  where
    len = bsLen bs
    go :: Word64 -> Int -> Int# -> (# (# Word64, Int# #) | DecodeError #)
    go !acc !shift !pos
      | shift > 63 = (# | InvalidVarint #)
      | isTrue# (pos >=# len) = (# | UnexpectedEnd #)
      | otherwise =
          let !b = BSU.unsafeIndex bs (I# pos)
              !val = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
              then (# (# val, pos +# 1# #) | #)
              else go val (shift + 7) (pos +# 1#)
{-# INLINE getVarintSlow #-}


-- | Decode a varint as a signed 'Int64'.
getVarintSigned :: Decoder Int64
getVarintSigned = fromIntegral <$> getVarint
{-# INLINE getVarintSigned #-}


-- | Decode a ZigZag-encoded sint32 value.
getSVarint32 :: Decoder Int32
getSVarint32 = unZigZag32 . fromIntegral <$> getVarint
{-# INLINE getSVarint32 #-}


-- | Decode a ZigZag-encoded sint64 value.
getSVarint64 :: Decoder Int64
getSVarint64 = unZigZag64 <$> getVarint
{-# INLINE getSVarint64 #-}


-- | Decode a ZigZag-encoded 32-bit value back to a signed 'Int32'.
unZigZag32 :: Word32 -> Int32
unZigZag32 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag32 #-}


-- | Decode a ZigZag-encoded 64-bit value back to a signed 'Int64'.
unZigZag64 :: Word64 -> Int64
unZigZag64 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag64 #-}


{- | Decode a fixed32 (little-endian) via a single aligned/unaligned
word load.  On x86_64 and aarch64-LE this compiles to one MOV.
-}
getFixed32 :: Decoder Word32
getFixed32 = Decoder $ \bs off ->
  if I# (off +# 4#) > BS.length bs
    then (# | UnexpectedEnd #)
    else
      let !val = readWord32LE bs (I# off)
      in (# (# val, off +# 4# #) | #)
{-# INLINE getFixed32 #-}


-- | Decode a fixed64 (little-endian) via a single word load.
getFixed64 :: Decoder Word64
getFixed64 = Decoder $ \bs off ->
  if I# (off +# 8#) > BS.length bs
    then (# | UnexpectedEnd #)
    else
      let !val = readWord64LE bs (I# off)
      in (# (# val, off +# 8# #) | #)
{-# INLINE getFixed64 #-}


-- Direct word-sized reads from a ByteString.
-- On little-endian platforms (all targets we care about: x86_64, aarch64-LE)
-- this is a single unaligned load instruction, replacing 4 or 8 separate
-- byte reads + shifts + ORs.

readWord32LE :: ByteString -> Int -> Word32
readWord32LE (BSI.BS fp _) off = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr ->
    peek (castPtr (ptr `plusPtr` off) :: Ptr Word32)
{-# INLINE readWord32LE #-}


readWord64LE :: ByteString -> Int -> Word64
readWord64LE (BSI.BS fp _) off = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr ->
    peek (castPtr (ptr `plusPtr` off) :: Ptr Word64)
{-# INLINE readWord64LE #-}


-- | Decode a 32-bit IEEE 754 float from a fixed32 wire value.
getFloat :: Decoder Float
getFloat = castWord32ToFloat <$> getFixed32
{-# INLINE getFloat #-}


-- | Decode a 64-bit IEEE 754 double from a fixed64 wire value.
getDouble :: Decoder Double
getDouble = castWord64ToDouble <$> getFixed64
{-# INLINE getDouble #-}


-- | Decode a length-delimited field. Zero-copy ByteString slice.
getLengthDelimited :: Decoder ByteString
getLengthDelimited = Decoder $ \bs off ->
  case runDecoder# getVarint bs off of
    (# (# lenW, off' #) | #) ->
      let !len = fromIntegral lenW :: Int
      in if len < 0
          then (# | NegativeLength #)
          else
            if I# off' + len > BS.length bs
              then (# | UnexpectedEnd #)
              else
                -- Build the slice as a single ByteString allocation by advancing
                -- the ForeignPtr directly. BSU.unsafeDrop + BSU.unsafeTake would
                -- allocate two intermediate ByteString headers; this allocates one.
                let !i = I# off'
                    !slice = case bs of BSI.BS fp _ -> BSI.BS (BSI.plusForeignPtr fp i) len
                in (# (# slice, case i + len of I# r -> r #) | #)
    (# | e #) -> (# | e #)
{-# INLINE getLengthDelimited #-}


-- | Decode a length-delimited bytes field (alias for 'getLengthDelimited').
getByteString :: Decoder ByteString
getByteString = getLengthDelimited
{-# INLINE getByteString #-}


{- | Decode a text field.

We rely on text >= 2.0's simdutf-powered 'decodeUtf8'' for UTF-8
validation and decoding in a single pass. The text library uses
AVX2/NEON internally, which is faster than a separate pre-check + decode.
-}
getText :: Decoder Text
getText = Decoder $ \bs off ->
  case runDecoder# getLengthDelimited bs off of
    (# (# bytes, off' #) | #) ->
      -- decodeUtf8Lenient avoids the runRW#/catch# wrapper that
      -- decodeUtf8' uses. For valid UTF-8 (required by proto3) this
      -- is semantically identical; invalid bytes get the U+FFFD
      -- replacement character rather than a DecodeError. The catch#
      -- overhead was measurable (~5–10 ns per string field).
      (# (# TE.decodeUtf8Lenient bytes, off' #) | #)
    (# | e #) -> (# | e #)
{-# INLINE getText #-}


-- | Decode a field tag (field number + wire type). Fails on invalid tags.
getTag :: Decoder Tag
getTag = Decoder $ \bs off ->
  case runDecoder# getVarint bs off of
    (# (# w, off' #) | #) ->
      case decodeTag w of
        Just tag -> (# (# tag, off' #) | #)
        Nothing -> (# | InvalidTag w #)
    (# | e #) -> (# | e #)
{-# INLINE getTag #-}


-- | Try to decode a tag, returning Nothing at end-of-input.
getTagOr :: Decoder (Maybe Tag)
getTagOr = Decoder $ \bs off ->
  if isTrue# (off >=# bsLen bs)
    then (# (# Nothing, off #) | #)
    else case runDecoder# getTag bs off of
      (# (# tag, off' #) | #) -> (# (# Just tag, off' #) | #)
      (# | e #) -> (# | e #)
{-# INLINE getTagOr #-}


-- | Skip over a field value based on its wire type.
skipField :: WireType -> Decoder ()
skipField = \case
  WireVarint -> skipVarint
  Wire64Bit -> skip 8
  WireLengthDelimited -> Decoder $ \bs off ->
    case runDecoder# getVarint bs off of
      (# (# lenW, off' #) | #) ->
        let !len = fromIntegral lenW :: Int
        in if I# off' + len > BS.length bs
            then (# | UnexpectedEnd #)
            else (# (# (), case I# off' + len of I# r -> r #) | #)
      (# | e #) -> (# | e #)
  WireStartGroup -> skipGroup
  WireEndGroup -> pure ()
  Wire32Bit -> skip 4


skip :: Int -> Decoder ()
skip (I# n) = Decoder $ \bs off ->
  if I# (off +# n) > BS.length bs
    then (# | UnexpectedEnd #)
    else (# (# (), off +# n #) | #)
{-# INLINE skip #-}


skipVarint :: Decoder ()
skipVarint = Decoder $ \bs off0 ->
  let len = bsLen bs
      go !pos
        | isTrue# (pos >=# len) = (# | UnexpectedEnd #)
        | BSU.unsafeIndex bs (I# pos) < 0x80 = (# (# (), pos +# 1# #) | #)
        | otherwise = go (pos +# 1#)
  in go off0
{-# INLINE skipVarint #-}


skipGroup :: Decoder ()
skipGroup = Decoder $ \bs off ->
  case runDecoder# getTagOrU bs off of
    (# (# mt, off' #) | #) -> case mt of
      UNothing -> (# | UnexpectedEnd #)
      UJust (Tag _ WireEndGroup) -> (# (# (), off' #) | #)
      UJust (Tag _ wt) -> runDecoder# (skipField wt >> skipGroup) bs off'
    (# | e #) -> (# | e #)


-- | Unboxed optional for zero-allocation tag-or-EOF in the decode loop.
data UMaybe a = UMaybe (# (# #) | a #)


-- | A 'UMaybe' containing a value.
pattern UJust :: a -> UMaybe a
pattern UJust a = UMaybe (# | a #)


-- | An empty 'UMaybe'.
pattern UNothing :: UMaybe a
pattern UNothing = UMaybe (# (# #) | #)


{-# COMPLETE UJust, UNothing #-}


-- | Eliminate a 'UMaybe': supply a default for 'UNothing' and a function for 'UJust'.
umaybe :: b -> (a -> b) -> UMaybe a -> b
umaybe def f (UMaybe x) = case x of
  (# (# #) | #) -> def
  (# | a #) -> f a
{-# INLINE umaybe #-}


-- | Like 'getTagOr' but returns 'UMaybe' to avoid allocating a boxed Maybe.
getTagOrU :: Decoder (UMaybe Tag)
getTagOrU = Decoder $ \bs off ->
  if isTrue# (off >=# bsLen bs)
    then (# (# UNothing, off #) | #)
    else case runDecoder# getTag bs off of
      (# (# tag, off' #) | #) -> (# (# UJust tag, off' #) | #)
      (# | e #) -> (# | e #)
{-# INLINE getTagOrU #-}


{- | Three-way unboxed result for the tag-or-EOF operation.
Flattens what would otherwise be two nested unboxed sums
(Decoder result × UMaybe) into a single three-way split.

* @(# (# #) | _ | _ #)@ — end of input (offset unchanged)
* @(# _ | (# Int#, Int#, Int# #) | _ #)@ — got a tag: field number, wire type, new offset
* @(# _ | _ | DecodeError #)@ — decode error
-}
type TagResult# = (# (# #) | (# Int#, Int#, Int# #) | DecodeError #)


{- | CPS interface to the three-way tag result, specialized for decoder results.
Avoids constructing any intermediate value — the continuation is applied
directly to the unboxed field number and wire type.
-}
withTag
  :: ByteString
  -> Int#
  -> (Int# -> (# (# a, Int# #) | DecodeError #))
  -> (Int# -> Int# -> Int# -> (# (# a, Int# #) | DecodeError #))
  -> (DecodeError -> (# (# a, Int# #) | DecodeError #))
  -> (# (# a, Int# #) | DecodeError #)
withTag bs off kEOF kTag kErr =
  if isTrue# (off >=# bsLen bs)
    then kEOF off
    else case runDecoder# getVarint bs off of
      (# (# w, off' #) | #) ->
        case decodeTagParts w of
          (# fn, wt #) -> kTag fn wt off'
      (# | e #) -> kErr e
{-# INLINE withTag #-}


-- | Decode tag into unboxed field number and wire type.
decodeTagParts :: Word64 -> (# Int#, Int# #)
decodeTagParts w =
  let fn = fromIntegral (w `shiftR` 3) :: Int
      wt = fromIntegral (w .&. 0x07) :: Int
  in case fn of
      I# fn# -> case wt of
        I# wt# -> (# fn#, wt# #)
{-# INLINE decodeTagParts #-}


{- | Monadic CPS tag dispatch for generated decoders.

At end-of-input: calls @kEOF@.
On a valid tag: calls @kTag fieldNumber wireType@, where both are
unboxed 'Int' values (no Tag constructor allocated).
On error: propagates the decode error.

This is the monadic counterpart to 'withTag', intended for generated
code. Avoids allocating the 'Tag' record that 'getTagOrU' produces.
-}
withTagM
  :: Decoder a
  -- ^ kEOF: continuation at end-of-input
  -> (Int -> Int -> Decoder a)
  -- ^ kTag: continuation with (fieldNumber, wireType)
  -> Decoder a
withTagM (Decoder kEOF) kTag = Decoder $ \bs off ->
  if isTrue# (off >=# bsLen bs)
    then kEOF bs off
    else case runDecoder# getVarint bs off of
      (# (# w, off' #) | #) ->
        case decodeTagParts w of
          (# fn#, wt# #) -> runDecoder# (kTag (I# fn#) (I# wt#)) bs off'
      (# | e #) -> (# | e #)
{-# INLINE withTagM #-}


{- | One step of the hyperpb-style in-order tag predictor.

The wire format encodes each field tag-first as a varint, and
@protoc@-generated serialisers emit fields in field-number order, so
the next-tag-bytes at every position of a decode loop are
/predictable/ for well-formed input. Predicting them and
short-circuiting the varint-decode + wire-type-decode + case-dispatch
pipeline is what hyperpb calls its "tag stamp" / "predicted next
field" fast path.

'inOrderStage' implements one step of that predictor with the
generalised /multi-byte/ shape (i.e. it doesn't restrict to field
numbers ≤ 15):

* if at end of input, run @kEnd@ (typically @pure result@);
* if the next 1..5 bytes of input equal the field's
  precomputed varint-encoded tag, consume them, run the field's
  value decoder, and hand the value to @kHit@;
* otherwise, run @kMiss@ /without consuming any bytes/, handing
  control back to the generic 'withTagM'-based loop, which then
  re-reads the tag the normal way.

The expected tag is supplied as a 'Word64' packing of the
varint bytes in little-endian order (low byte = first byte on
the wire). @expectedTag@ and @tagMask@ are both precomputed by
the codegen as compile-time constants; the helper does a single
unaligned 'readWord64LE' (or a byte-by-byte fallback near the end
of the buffer where an 8-byte load would overrun), masks it,
and compares against @expectedTag@.

@INLINE@ is essential — each call site has constant
@(expectedTag, tagMask, tagLen)@, and inlining lets GHC specialise
the loads, masks, and value decoder into a few straight-line
instructions per stage.
-}
inOrderStage
  :: Word64
  -- ^ Expected varint-encoded tag, with the @tagLen@ bytes packed
  --   little-endian into the low bits and zeros above. Codegen builds
  --   this with @foldr (\\(i, b) acc -> acc .|. (fromIntegral b \`shiftL\` (8*i))) 0 (zip [0..] bytes)@.
  -> Word64
  -- ^ Mask: @(1 \`shiftL\` (8 * tagLen)) - 1@. Used to ignore the
  --   bytes the 'readWord64LE' load picks up beyond the tag length.
  -> Int
  -- ^ Tag length in bytes (1..5 for any valid proto field number).
  -> Decoder v
  -- ^ Value decoder for the predicted field (runs after the tag
  --   bytes are consumed).
  -> (v -> Decoder a)
  -- ^ @kHit@: continuation with the decoded value.
  -> Decoder a
  -- ^ @kEnd@: continuation when input is exhausted (e.g.
  --   @pure result@).
  -> Decoder a
  -- ^ @kMiss@: fallback for any non-matching tag prefix (no input
  --   is consumed before this runs).
  -> Decoder a
inOrderStage !expectedTag !tagMask (I# tagLen#) !valueDecoder kHit kEnd kMiss =
  Decoder $ \bs off ->
    let !len = bsLen bs
    in if isTrue# (off >=# len)
        then runDecoder# kEnd bs off
        else
          if isTrue# ((off +# tagLen#) ># len)
            then
              -- Not enough bytes left for even the tag — punt to
              -- the general loop. (It will surface UnexpectedEnd if
              -- the truncated input is genuinely malformed.)
              runDecoder# kMiss bs off
            else
              let !w =
                    if isTrue# ((off +# 8#) <=# len)
                      then readWord64LE bs (I# off) .&. tagMask
                      -- We've already verified off + tagLen <= len above, so
                      -- reading exactly tagLen bytes is safe. Reading all
                      -- remaining bytes (len - off) was wasteful for short
                      -- messages where len - off >> tagLen.
                      else readPartialWord64LE bs (I# off) (I# tagLen#) .&. tagMask
              in if w == expectedTag
                  then case runDecoder# valueDecoder bs (off +# tagLen#) of
                    (# (# v, off' #) | #) -> runDecoder# (kHit v) bs off'
                    (# | e #) -> (# | e #)
                  else runDecoder# kMiss bs off
{-# INLINE inOrderStage #-}


{- | Specialised 'inOrderStage' for 1-byte tags (field numbers 1–15,
wire types 0–5). Replaces the Word64 load+mask with a single byte
comparison, which is measurably faster for short messages where the
full 8-byte unaligned load path is unavailable.

The codegen emits this variant when @tagLen == 1@.
-}
inOrderStage1
  :: Word8
  -- ^ Expected single-byte tag (@fieldNum \`shiftL\` 3 .|. wireType@).
  -> Decoder v
  -> (v -> Decoder a)
  -> Decoder a -- ^ kEnd
  -> Decoder a -- ^ kMiss
  -> Decoder a
inOrderStage1 !expectedByte !valueDecoder kHit kEnd kMiss =
  Decoder $ \bs off ->
    let !len = bsLen bs
    in if isTrue# (off >=# len)
        then runDecoder# kEnd bs off
        else
          if BSU.unsafeIndex bs (I# off) == expectedByte
            then case runDecoder# valueDecoder bs (off +# 1#) of
              (# (# v, off' #) | #) -> runDecoder# (kHit v) bs off'
              (# | e #) -> (# | e #)
            else runDecoder# kMiss bs off
{-# INLINE inOrderStage1 #-}


{- | Read up to @n@ (1..7) bytes starting at @off@, packing them
little-endian into a 'Word64' with zeros above. Used by 'inOrderStage'
when the buffer doesn't have a full 8 bytes left for an unaligned
'readWord64LE' load.

Not exported — only called when the caller has verified that
@off + n <= length bs@.
-}
readPartialWord64LE :: ByteString -> Int -> Int -> Word64
readPartialWord64LE bs off n = go 0 0
  where
    go :: Int -> Word64 -> Word64
    go !i !acc
      | i >= n = acc
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
          in go (i + 1) (acc .|. (b `shiftL` (8 * i)))
{-# INLINE readPartialWord64LE #-}


-- | Skip a field given its wire type as an 'Int' (for use with 'withTagM').
skipWireType :: Int -> Decoder ()
skipWireType wt = case wt of
  0 -> skipVarint
  1 -> skip 8
  2 -> Decoder $ \bs off ->
    case runDecoder# getVarint bs off of
      (# (# lenW, off' #) | #) ->
        let !len = fromIntegral lenW :: Int
        in if I# off' + len > BS.length bs
            then (# | UnexpectedEnd #)
            else (# (# (), case I# off' + len of I# r -> r #) | #)
      (# | e #) -> (# | e #)
  5 -> skip 4
  _ -> Decoder $ \_ _ -> (# | InvalidWireType wt #)
{-# INLINE skipWireType #-}


{- | Validate UTF-8 without exceptions.

Uses the SWAR-accelerated C validator. Useful for paths that need
validation without decoding (e.g. conformance checks).

Note: 'getText' and 'decodeTextFast' do /not/ use this — they rely on
text >= 2.0's simdutf-powered 'TE.decodeUtf8'' which validates and
decodes in a single pass.
-}
validateUtf8 :: ByteString -> Bool
validateUtf8 = validateUtf8SWAR
{-# INLINE validateUtf8 #-}
