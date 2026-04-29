{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
-- | wireform types matching bench.proto for benchmark comparison.
module WireformTypes where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Exts (Int#, Int(I#))
import Control.DeepSeq (NFData)

import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Proto.Encode
import Proto.Decode.Fast
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVU
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Proto.Decode
import Proto.Encode.Archetype
import Wireform.Encode.Direct
import qualified Proto.SizedBuilder as SB
import Proto.Wire.Encode (varintSize)
import Proto.Wire.Decode (runDecoder', Decoder(..))
import Proto.VectorBuilder

-- Small

data HSmall = HSmall
  { hsId     :: {-# UNPACK #-} !Int64
  , hsName   :: !Text
  , hsActive :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedSmall :: HSmall -> SB.SizedBuilder
buildSizedSmall (HSmall i n a) =
  (if i == 0 then mempty else sbArchVarint 0x08 (fromIntegral i)) <>
  (if n == "" then mempty else sbArchString 0x12 n) <>
  (if not a then mempty else sbArchBool 0x18 True)
{-# INLINE buildSizedSmall #-}

instance MessageEncode HSmall where
  buildMessage m = SB.toBuilder (buildSizedSmall m)
  {-# INLINE buildMessage #-}

instance MessageSize HSmall where
  messageSize m = SB.size (buildSizedSmall m)
  {-# INLINE messageSize #-}

instance MessageDecode HSmall where
  messageDecoder = Decoder (\bs off -> loop 0 "" False bs off)
    where
      loop :: Int64 -> Text -> Bool -> ByteString -> Int# -> (# (# HSmall, Int# #) | DecodeError #)
      loop !i !n !a !bs !off =
        withTag bs off
          (\off' -> (# (# HSmall i n a, off' #) | #))
          (\fn _wt off' -> case I# fn of
            1 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop (fromIntegral v) n a bs off''
              (# | e #) -> (# | e #)
            2 -> case runDecoder# getText bs off' of
              (# (# v, off'' #) | #) -> loop i v a bs off''
              (# | e #) -> (# | e #)
            3 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop i n (v /= 0) bs off''
              (# | e #) -> (# | e #)
            _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
              (# (# _, off'' #) | #) -> loop i n a bs off''
              (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}

-- Medium

data HMedium = HMedium
  { hmTitle       :: !Text
  , hmCount       :: {-# UNPACK #-} !Int32
  , hmScore       :: {-# UNPACK #-} !Double
  , hmPayload     :: !ByteString
  , hmEnabled     :: !Bool
  , hmTimestamp   :: {-# UNPACK #-} !Int64
  , hmDescription :: !Text
  , hmRatio       :: {-# UNPACK #-} !Float
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedMedium :: HMedium -> SB.SizedBuilder
buildSizedMedium m =
  (if hmTitle m == "" then mempty else sbArchString 0x0a (hmTitle m)) <>
  (if hmCount m == 0 then mempty else sbArchVarint 0x10 (fromIntegral (hmCount m))) <>
  (if hmScore m == 0 then mempty else sbArchDouble 0x19 (hmScore m)) <>
  (if BS.null (hmPayload m) then mempty else sbArchBytes 0x22 (hmPayload m)) <>
  (if not (hmEnabled m) then mempty else sbArchBool 0x28 True) <>
  (if hmTimestamp m == 0 then mempty else sbArchVarint 0x30 (fromIntegral (hmTimestamp m))) <>
  (if hmDescription m == "" then mempty else sbArchString 0x3a (hmDescription m)) <>
  (if hmRatio m == 0 then mempty else sbArchFloat 0x45 (hmRatio m))
{-# INLINE buildSizedMedium #-}

instance MessageEncode HMedium where
  buildMessage m = SB.toBuilder (buildSizedMedium m)
  {-# INLINE buildMessage #-}

instance MessageSize HMedium where
  messageSize m = SB.size (buildSizedMedium m)
  {-# INLINE messageSize #-}

instance MessageDecode HMedium where
  messageDecoder = Decoder (\bs off -> loop "" 0 0 "" False 0 "" 0 bs off)
    where
      loop :: Text -> Int32 -> Double -> ByteString -> Bool -> Int64 -> Text -> Float -> ByteString -> Int# -> (# (# HMedium, Int# #) | DecodeError #)
      loop !t !c !sc !p !e !ts !d !r !bs !off =
        withTag bs off
          (\off' -> (# (# HMedium t c sc p e ts d r, off' #) | #))
          (\fn _wt off' -> case I# fn of
            1 -> case runDecoder# decodeFieldString bs off' of
              (# (# v, off'' #) | #) -> loop v c sc p e ts d r bs off''
              (# | err #) -> (# | err #)
            2 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop t (fromIntegral v) sc p e ts d r bs off''
              (# | err #) -> (# | err #)
            3 -> case runDecoder# getDouble bs off' of
              (# (# v, off'' #) | #) -> loop t c v p e ts d r bs off''
              (# | err #) -> (# | err #)
            4 -> case runDecoder# decodeFieldBytes bs off' of
              (# (# v, off'' #) | #) -> loop t c sc v e ts d r bs off''
              (# | err #) -> (# | err #)
            5 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop t c sc p (v /= 0) ts d r bs off''
              (# | err #) -> (# | err #)
            6 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop t c sc p e (fromIntegral v) d r bs off''
              (# | err #) -> (# | err #)
            7 -> case runDecoder# decodeFieldString bs off' of
              (# (# v, off'' #) | #) -> loop t c sc p e ts v r bs off''
              (# | err #) -> (# | err #)
            8 -> case runDecoder# getFloat bs off' of
              (# (# v, off'' #) | #) -> loop t c sc p e ts d v bs off''
              (# | err #) -> (# | err #)
            _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
              (# (# _, off'' #) | #) -> loop t c sc p e ts d r bs off''
              (# | err #) -> (# | err #)
          )
          (\err -> (# | err #))
  {-# INLINE messageDecoder #-}

-- WithNested

data HWithNested = HWithNested
  { hwnId    :: {-# UNPACK #-} !Int64
  , hwnInner :: !(Maybe HSmall)
  , hwnLabel :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedNested :: HWithNested -> SB.SizedBuilder
buildSizedNested m =
  (if hwnId m == 0 then mempty else sbArchVarint 0x08 (fromIntegral (hwnId m))) <>
  maybe mempty (\inner -> sbArchSubmessage 0x12 (buildSizedSmall inner)) (hwnInner m) <>
  (if hwnLabel m == "" then mempty else sbArchString 0x1a (hwnLabel m))
{-# INLINE buildSizedNested #-}

instance MessageEncode HWithNested where
  buildMessage m = SB.toBuilder (buildSizedNested m)
  {-# INLINE buildMessage #-}

instance MessageSize HWithNested where
  messageSize m = SB.size (buildSizedNested m)
  {-# INLINE messageSize #-}

instance MessageDecode HWithNested where
  messageDecoder = Decoder (\bs off -> loop 0 Nothing "" bs off)
    where
      loop :: Int64 -> Maybe HSmall -> Text -> ByteString -> Int# -> (# (# HWithNested, Int# #) | DecodeError #)
      loop !i !inner !lbl !bs !off =
        withTag bs off
          (\off' -> (# (# HWithNested i inner lbl, off' #) | #))
          (\fn _wt off' -> case I# fn of
            1 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop (fromIntegral v) inner lbl bs off''
              (# | e #) -> (# | e #)
            2 -> case runDecoder# decodeFieldMessage bs off' of
              (# (# v, off'' #) | #) -> loop i (Just v) lbl bs off''
              (# | e #) -> (# | e #)
            3 -> case runDecoder# decodeFieldString bs off' of
              (# (# v, off'' #) | #) -> loop i inner v bs off''
              (# | e #) -> (# | e #)
            _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
              (# (# _, off'' #) | #) -> loop i inner lbl bs off''
              (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}

-- WithRepeated

data HWithRepeated = HWithRepeated
  { hwrValues :: !(VU.Vector Int32)
  , hwrTags   :: !(V.Vector Text)
  , hwrItems  :: !(V.Vector HSmall)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedRepeated :: HWithRepeated -> SB.SizedBuilder
buildSizedRepeated m =
  sbArchPackedVarints 0x0a (hwrValues m) <>
  V.foldl' (\acc s -> acc <> sbArchString 0x12 s) mempty (hwrTags m) <>
  V.foldl' (\acc item -> acc <> sbArchSubmessage 0x1a (buildSizedSmall item)) mempty (hwrItems m)
{-# INLINE buildSizedRepeated #-}

instance MessageEncode HWithRepeated where
  buildMessage m = SB.toBuilder (buildSizedRepeated m)
  {-# INLINE buildMessage #-}

instance MessageSize HWithRepeated where
  messageSize m = SB.size (buildSizedRepeated m)
  {-# INLINE messageSize #-}

instance MessageDecode HWithRepeated where
  messageDecoder = Decoder (\bs off -> loop emptyGrowList emptyGrowList emptyGrowList bs off)
    where
      loop :: GrowList Int32 -> GrowList Text -> GrowList HSmall -> ByteString -> Int# -> (# (# HWithRepeated, Int# #) | DecodeError #)
      loop !vals !tags !items !bs !off =
        withTag bs off
          (\off' -> (# (# HWithRepeated (growListToVectorU vals) (growListToVector tags) (growListToVector items), off' #) | #))
          (\fn wt off' -> case I# fn of
            1 -> case I# wt of
              2 -> case runDecoder# getLengthDelimited bs off' of
                (# (# chunk, off'' #) | #) ->
                  let !vals' = decodePackedInto vals chunk
                  in loop vals' tags items bs off''
                (# | e #) -> (# | e #)
              _ -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop (snocGrowList vals (fromIntegral v)) tags items bs off''
                (# | e #) -> (# | e #)
            2 -> case runDecoder# decodeFieldString bs off' of
              (# (# v, off'' #) | #) -> loop vals (snocGrowList tags v) items bs off''
              (# | e #) -> (# | e #)
            3 -> case runDecoder# decodeFieldMessage bs off' of
              (# (# v, off'' #) | #) -> loop vals tags (snocGrowList items v) bs off''
              (# | e #) -> (# | e #)
            _ -> case runDecoder# (skipField (toEnum (I# wt))) bs off' of
              (# (# _, off'' #) | #) -> loop vals tags items bs off''
              (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}

-- | Tag-byte dispatch: single byte read + case match.
-- Fields: id(1,varint)=0x08, name(2,bytes)=0x12, active(3,varint)=0x18
fastDecodeSmall :: ByteString -> Either DecodeError HSmall
fastDecodeSmall origBs = runFastDecode origBs $ \fd off0 ->
  let go !i !n !a !off
        | fdDone fd off = Right (HSmall i n a, off)
        | otherwise =
            let !tag = readByte fd off
                !off1 = off + 1
            in case tag of
              0x08 -> let (!v, !off2) = fdVarint fd off1
                      in go (fromIntegral v) n a off2
              0x12 -> let (!v, !off2) = fdText fd off1 origBs
                      in go i v a off2
              0x18 -> let (!v, !off2) = fdVarint fd off1
                      in go i n (v /= 0) off2
              _    -> let (!fn, !wt, !off1') = fdTag fd off
                      in go i n a (fdSkipField fd off1' wt)
  in go 0 "" False off0
{-# NOINLINE fastDecodeSmall #-}

-- | Fast decoder for HMedium. Tag-byte dispatch: reads the single-byte tag
-- directly and dispatches without going through fdTag/fdVarint for the tag.
-- Fields 1-8 all have single-byte tags (0x0a,0x10,0x19,0x22,0x28,0x30,0x3a,0x45).
fastDecodeMedium :: ByteString -> Either DecodeError HMedium
fastDecodeMedium origBs = runFastDecode origBs $ \fd off0 ->
  let go !t !c !sc !p !e !ts !d !r !off
        | fdDone fd off = Right (HMedium t c sc p e ts d r, off)
        | otherwise =
            let !tag = readByte fd off
                !off1 = off + 1
            in case tag of
              0x0a -> let (!v, !off2) = fdText fd off1 origBs in go v c sc p e ts d r off2
              0x10 -> let (!v, !off2) = fdVarint fd off1 in go t (fromIntegral v) sc p e ts d r off2
              0x19 -> let (!v, !off2) = fdDouble fd off1 in go t c v p e ts d r off2
              0x22 -> let (!v, !off2) = fdBytes fd off1 origBs in go t c sc v e ts d r off2
              0x28 -> let (!v, !off2) = fdVarint fd off1 in go t c sc p (v /= 0) ts d r off2
              0x30 -> let (!v, !off2) = fdVarint fd off1 in go t c sc p e (fromIntegral v) d r off2
              0x3a -> let (!v, !off2) = fdText fd off1 origBs in go t c sc p e ts v r off2
              0x45 -> let (!v, !off2) = fdFloat fd off1 in go t c sc p e ts d v off2
              _    -> let (!fn, !wt, !off1') = fdTag fd off
                      in go t c sc p e ts d r (fdSkipField fd off1' wt)
  in go "" 0 0.0 BS.empty False 0 "" 0.0 off0
{-# NOINLINE fastDecodeMedium #-}

-- | Tag-byte dispatch for nested. id(1)=0x08, inner(2)=0x12, label(3)=0x1a
fastDecodeNested :: ByteString -> Either DecodeError HWithNested
fastDecodeNested origBs = runFastDecode origBs $ \fd off0 ->
  let go !i !inner !lbl !off
        | fdDone fd off = Right (HWithNested i inner lbl, off)
        | otherwise =
            let !tag = readByte fd off
                !off1 = off + 1
            in case tag of
              0x08 -> let (!v, !off2) = fdVarint fd off1 in go (fromIntegral v) inner lbl off2
              0x12 -> let (!subBs, !off2) = fdBytes fd off1 origBs
                      in case fastDecodeSmallInner subBs of
                           Right m -> go i (Just m) lbl off2
                           Left e -> Left (SubMessageError e)
              0x1a -> let (!v, !off2) = fdText fd off1 origBs in go i inner v off2
              _    -> let (!fn, !wt, !off1') = fdTag fd off
                      in go i inner lbl (fdSkipField fd off1' wt)
  in go 0 Nothing "" off0
{-# NOINLINE fastDecodeNested #-}

fastDecodeSmallInner :: ByteString -> Either DecodeError HSmall
fastDecodeSmallInner origBs = runFastDecode origBs $ \fd off0 ->
  let go !i !n !a !off
        | fdDone fd off = Right (HSmall i n a, off)
        | otherwise =
            let (!fn, !wt, !off1) = fdTag fd off in
            case fn of
              1 -> let (!v, !off2) = fdVarint fd off1 in go (fromIntegral v) n a off2
              2 -> let (!v, !off2) = fdText fd off1 origBs in go i v a off2
              3 -> let (!v, !off2) = fdVarint fd off1 in go i n (v /= 0) off2
              _ -> go i n a (fdSkipField fd off1 wt)
  in go 0 "" False off0

-- | Fast repeated decoder. Two passes:
-- Pass 1: scan to count elements per field (cheap — just tag decode + skip)
-- Pass 2: decode with pre-allocated exact-size vectors (zero grow)
fastDecodeRepeated :: ByteString -> Either DecodeError HWithRepeated
fastDecodeRepeated origBs = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr -> do
    let !fd = FastDec (castPtr ptr) len

    -- Pass 1: count elements
    let count !nv !nt !ni !off
          | off >= len = (nv, nt, ni)
          | otherwise =
              let (!fn, !wt, !off1) = fdTag fd off in
              case fn of
                1 | wt == 2 ->
                    let (!blen, !off2) = fdVarint fd off1
                        !bl = fromIntegral blen
                        -- Count varints in packed buffer
                        !nVarints = countVarints fd off2 (off2 + bl)
                    in count (nv + nVarints) nt ni (off2 + bl)
                  | otherwise ->
                    count (nv + 1) nt ni (snd (fdVarint fd off1))
                2 -> count nv (nt + 1) ni (fdSkipField fd off1 wt)
                3 -> count nv nt (ni + 1) (fdSkipField fd off1 wt)
                _ -> count nv nt ni (fdSkipField fd off1 wt)

    let (!numVals, !numTags, !numItems) = count 0 0 0 0

    -- Pass 2: decode with exact-size pre-allocated vectors
    mvVals  <- MVU.unsafeNew numVals
    mvTags  <- MV.unsafeNew numTags
    mvItems <- MV.unsafeNew numItems

    let go !vi !ti !ii !off
          | off >= len = pure (Right off)
          | otherwise = do
              let (!fn, !wt, !off1) = fdTag fd off
              case fn of
                1 | wt == 2 -> do
                    let (!blen, !off2) = fdVarint fd off1
                        !bl = fromIntegral blen
                        !endOff = off2 + bl
                    vi' <- goP vi off2 endOff
                    go vi' ti ii endOff
                  | otherwise -> do
                    let (!v, !off2) = fdVarint fd off1
                    MVU.unsafeWrite mvVals vi (fromIntegral v)
                    go (vi + 1) ti ii off2
                2 -> do
                  let (!v, !off2) = fdText fd off1 origBs
                  MV.unsafeWrite mvTags ti v
                  go vi (ti + 1) ii off2
                3 -> do
                  let (!subBs, !off2) = fdBytes fd off1 origBs
                  case fastDecodeSmallInner subBs of
                    Right m -> do
                      MV.unsafeWrite mvItems ii m
                      go vi ti (ii + 1) off2
                    Left e -> pure (Left (SubMessageError e))
                _ -> go vi ti ii (fdSkipField fd off1 wt)

        goP !vi !p !endOff
          | p >= endOff = pure vi
          | otherwise = do
              let (!v, !p') = fdVarint fd p
              MVU.unsafeWrite mvVals vi (fromIntegral v)
              goP (vi + 1) p' endOff

    result <- go 0 0 0 0
    case result of
      Left e -> pure (Left e)
      Right off
        | off == len -> do
            vals  <- VU.unsafeFreeze mvVals
            tags  <- V.unsafeFreeze mvTags
            items <- V.unsafeFreeze mvItems
            pure (Right (HWithRepeated vals tags items))
        | otherwise -> pure (Left ExtraBytes)
  where
    !(BSI.BS fp len) = origBs

    countVarints :: FastDec -> Int -> Int -> Int
    countVarints fd = go 0
      where
        go !n !p !endP
          | p >= endP = n
          | otherwise =
              let (!_, !p') = fdVarint fd p
              in go (n + 1) p' endP
{-# NOINLINE fastDecodeRepeated #-}

decodePackedInto :: GrowList Int32 -> ByteString -> GrowList Int32
decodePackedInto !gl bs = go gl 0
  where
    bsLen = BS.length bs
    go !acc !off
      | off >= bsLen = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (snocGrowList acc (fromIntegral v)) off'
          DecodeFail _    -> acc

-- | Direct-write encode for HSmall. Zero Builder overhead.
directEncodeSmall :: HSmall -> ByteString
directEncodeSmall msg =
  let !sz = sizeSmall msg
  in directEncode sz (writeSmall msg)
{-# NOINLINE directEncodeSmall #-}

sizeSmall :: HSmall -> Int
sizeSmall (HSmall i n a) =
  (if i == 0 then 0 else archVarintSize (fromIntegral i)) +
  (if n == "" then 0 else archStringSize n) +
  (if not a then 0 else archBoolSize)
{-# INLINE sizeSmall #-}

writeSmall :: HSmall -> Ptr Word8 -> Int -> IO Int
writeSmall (HSmall i n a) !p !off = do
  off1 <- if i == 0 then pure off
          else dVarintField p off 0x08 (fromIntegral i)
  off2 <- if n == "" then pure off1
          else dStringField p off1 0x12 n
  if not a then pure off2
  else dBoolField p off2 0x18 True
{-# INLINE writeSmall #-}

-- | Direct-write encode for HMedium.
directEncodeMedium :: HMedium -> ByteString
directEncodeMedium msg =
  let !sz = sizeMedium msg
  in directEncode sz (writeMedium msg)
{-# NOINLINE directEncodeMedium #-}

sizeMedium :: HMedium -> Int
sizeMedium m =
  (if hmTitle m == "" then 0 else archStringSize (hmTitle m)) +
  (if hmCount m == 0 then 0 else archVarintSize (fromIntegral (hmCount m))) +
  (if hmScore m == 0 then 0 else archFixed64Size) +
  (if BS.null (hmPayload m) then 0 else archBytesSize (hmPayload m)) +
  (if not (hmEnabled m) then 0 else archBoolSize) +
  (if hmTimestamp m == 0 then 0 else archVarintSize (fromIntegral (hmTimestamp m))) +
  (if hmDescription m == "" then 0 else archStringSize (hmDescription m)) +
  (if hmRatio m == 0 then 0 else archFixed32Size)
{-# INLINE sizeMedium #-}

writeMedium :: HMedium -> Ptr Word8 -> Int -> IO Int
writeMedium m !p !off = do
  off1 <- if hmTitle m == "" then pure off
          else dStringField p off 0x0a (hmTitle m)
  off2 <- if hmCount m == 0 then pure off1
          else dVarintField p off1 0x10 (fromIntegral (hmCount m))
  off3 <- if hmScore m == 0 then pure off2
          else dDoubleField p off2 0x19 (hmScore m)
  off4 <- if BS.null (hmPayload m) then pure off3
          else dBytesField p off3 0x22 (hmPayload m)
  off5 <- if not (hmEnabled m) then pure off4
          else dBoolField p off4 0x28 True
  off6 <- if hmTimestamp m == 0 then pure off5
          else dVarintField p off5 0x30 (fromIntegral (hmTimestamp m))
  off7 <- if hmDescription m == "" then pure off6
          else dStringField p off6 0x3a (hmDescription m)
  if hmRatio m == 0 then pure off7
  else dFloatField p off7 0x45 (hmRatio m)
{-# INLINE writeMedium #-}

-- | Direct-write encode for HWithNested.
directEncodeNested :: HWithNested -> ByteString
directEncodeNested msg =
  let !sz = sizeNested msg
  in directEncode sz (writeNested msg)
{-# NOINLINE directEncodeNested #-}

sizeNested :: HWithNested -> Int
sizeNested m =
  (if hwnId m == 0 then 0 else archVarintSize (fromIntegral (hwnId m))) +
  maybe 0 (\inner -> archSubmessageSize (sizeSmall inner)) (hwnInner m) +
  (if hwnLabel m == "" then 0 else archStringSize (hwnLabel m))
{-# INLINE sizeNested #-}

writeNested :: HWithNested -> Ptr Word8 -> Int -> IO Int
writeNested m !p !off = do
  off1 <- if hwnId m == 0 then pure off
          else dVarintField p off 0x08 (fromIntegral (hwnId m))
  off2 <- case hwnInner m of
    Nothing -> pure off1
    Just inner -> do
      let !innerSz = sizeSmall inner
      off1a <- dWord8 p off1 0x12
      off1b <- dVarint p off1a (fromIntegral innerSz)
      writeSmall inner p off1b
  if hwnLabel m == "" then pure off2
  else dStringField p off2 0x1a (hwnLabel m)
{-# INLINE writeNested #-}

-- | Direct-write encode for HWithRepeated.
directEncodeRepeated :: HWithRepeated -> ByteString
directEncodeRepeated msg =
  let !sz = sizeRepeated msg
  in directEncode sz (writeRepeated msg)
{-# NOINLINE directEncodeRepeated #-}

sizeRepeated :: HWithRepeated -> Int
sizeRepeated m =
  (let vs = hwrValues m in if VU.null vs then 0
     else let !packedSz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v :: Word64)) 0 vs
          in 1 + varintSize (fromIntegral packedSz) + packedSz) +
  V.foldl' (\acc s -> acc + archStringSize s) 0 (hwrTags m) +
  V.foldl' (\acc item -> acc + archSubmessageSize (sizeSmall item)) 0 (hwrItems m)
{-# INLINE sizeRepeated #-}

writeRepeated :: HWithRepeated -> Ptr Word8 -> Int -> IO Int
writeRepeated m !p !off = do
  off1 <- if VU.null (hwrValues m) then pure off
          else do
            let !packedSz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v :: Word64)) 0 (hwrValues m)
            off1a <- dWord8 p off 0x0a
            off1b <- dVarint p off1a (fromIntegral packedSz)
            VU.foldM' (\o v -> dVarint p o (fromIntegral v)) off1b (hwrValues m)
  off2 <- V.foldM' (\o s -> dStringField p o 0x12 s) off1 (hwrTags m)
  V.foldM' (\o item -> do
    let !innerSz = sizeSmall item
    off2a <- dWord8 p o 0x1a
    off2b <- dVarint p off2a (fromIntegral innerSz)
    writeSmall item p off2b) off2 (hwrItems m)
{-# INLINE writeRepeated #-}

