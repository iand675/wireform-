{-# LANGUAGE BangPatterns #-}

-- | Encode an unpacked, single-segment Cap'n Proto message.
module CapnProto.Encode (encode) where

import CapnProto.Value qualified as C
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as Text
import Data.Vector qualified as V
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Wireform.Encode.Direct (directEncode)


-- | Encode a root struct. A standalone scalar or pointer value is put in
-- the corresponding section of a one-field root struct.
encode :: C.Value -> ByteString
encode value =
  let !root = case value of
        C.Struct {} -> value
        C.Void -> C.Void
        _ | isPointer value -> C.Struct V.empty (V.singleton value)
          | otherwise -> C.Struct (V.singleton value) V.empty
      !size = 16 + objectSize root
  in directEncode size $ \ptr _ -> do
       pokeByteOff ptr 0 (0 :: Word32)
       pokeByteOff ptr 4 (fromIntegral (size `quot` 8 - 1) :: Word32)
       putPointer ptr 8 16 root
{-# NOINLINE encode #-}


isPointer :: C.Value -> Bool
isPointer = \case
  C.Struct {} -> True
  C.List {} -> True
  C.Text {} -> True
  C.Data {} -> True
  _ -> False


objectSize :: C.Value -> Int
objectSize = \case
  C.Void -> 0
  C.Struct dat pointers ->
    8 * (V.length dat + V.length pointers) + V.foldl' (\n v -> n + objectSize v) 0 pointers
  C.Text (Text.Text _ _ bytes) -> alignWord (bytes + 1)
  C.Data bytes -> alignWord (BS.length bytes)
  C.List values -> case listLayout values of
    DataList code -> alignWord (listDataBytes code (V.length values))
    PointerList -> 8 * V.length values + V.foldl' (\n v -> n + objectSize v) 0 values
    StructList dw pw ->
      8 + 8 * ((dw + pw) * V.length values)
        + V.foldl' (\n v -> n + structChildrenSize v) 0 values
  _ -> error "CapnProto.Encode: scalar in pointer section"
  where
    structChildrenSize (C.Struct _ pointers) = V.foldl' (\n v -> n + objectSize v) 0 pointers
    structChildrenSize _ = error "CapnProto.Encode: mixed struct list"


alignWord :: Int -> Int
alignWord bytes = (bytes + 7) .&. (-8)
{-# INLINE alignWord #-}


data ListLayout = DataList !Int | PointerList | StructList !Int !Int


listLayout :: V.Vector C.Value -> ListLayout
listLayout values = case values V.!? 0 of
  Nothing -> DataList 0
  Just (C.Struct dat pointers) ->
    V.foldl' extend (StructList (V.length dat) (V.length pointers)) values
  Just value
    | Just width <- dataListCode value
    , V.all ((== Just width) . dataListCode) values -> DataList width
    | V.all (\v -> isPointer v || v == C.Void) values -> PointerList
    | otherwise -> error "CapnProto.Encode: list elements have incompatible layouts"
  where
    extend (StructList dw pw) (C.Struct dat pointers) =
      StructList (max dw (V.length dat)) (max pw (V.length pointers))
    extend _ _ = error "CapnProto.Encode: mixed struct list"


dataListCode :: C.Value -> Maybe Int
dataListCode = \case
  C.Void -> Just 0
  C.Bool {} -> Just 1
  C.Int8 {} -> Just 2
  C.UInt8 {} -> Just 2
  C.Int16 {} -> Just 3
  C.UInt16 {} -> Just 3
  C.Enum {} -> Just 3
  C.Int32 {} -> Just 4
  C.UInt32 {} -> Just 4
  C.Float32 {} -> Just 4
  C.Int64 {} -> Just 5
  C.UInt64 {} -> Just 5
  C.Float64 {} -> Just 5
  _ -> Nothing


listDataBytes :: Int -> Int -> Int
listDataBytes code count = case code of
  0 -> 0
  1 -> (count + 7) `quot` 8
  _ -> count `shiftL` (code - 2)
{-# INLINE listDataBytes #-}


putPointer :: Ptr Word8 -> Int -> Int -> C.Value -> IO Int
putPointer ptr slot target = \case
  C.Void -> pokeByteOff ptr slot (0 :: Word64) >> pure target
  C.Struct dat pointers -> do
    let !dw = V.length dat
        !pw = V.length pointers
        !offset = if dw + pw == 0 then -1 else pointerOffset slot target
    pokeByteOff ptr slot (structWord offset dw pw)
    putStruct ptr target (target + 8 * (dw + pw)) dw pw dat pointers
  C.Text text -> putBytes ptr slot target (TE.encodeUtf8 text) 1
  C.Data bytes -> putBytes ptr slot target bytes 0
  C.List values -> putList ptr slot target values
  _ -> error "CapnProto.Encode: scalar in pointer section"


putStruct :: Ptr Word8 -> Int -> Int -> Int -> Int -> V.Vector C.Value -> V.Vector C.Value -> IO Int
putStruct ptr start next dw pw dat pointers = do
  V.imapM_ (\i v -> pokeByteOff ptr (start + 8 * i) (dataWord v)) dat
  fillBytes (ptr `plusPtr` (start + 8 * V.length dat)) 0 (8 * (dw - V.length dat))
  let !pointerStart = start + 8 * dw
  fillBytes (ptr `plusPtr` (pointerStart + 8 * V.length pointers)) 0 (8 * (pw - V.length pointers))
  V.ifoldM' (\end i v -> putPointer ptr (pointerStart + 8 * i) end v) next pointers


putBytes :: Ptr Word8 -> Int -> Int -> ByteString -> Int -> IO Int
putBytes ptr slot target bytes suffix = do
  let !len = BS.length bytes
      !size = alignWord (len + suffix)
  pokeByteOff ptr slot (listWord (pointerOffset slot target) 2 (len + suffix))
  BSU.unsafeUseAsCString bytes $ \source -> copyBytes (ptr `plusPtr` target) (castPtr source) len
  fillBytes (ptr `plusPtr` (target + len)) 0 (size - len)
  pure (target + size)


putList :: Ptr Word8 -> Int -> Int -> V.Vector C.Value -> IO Int
putList ptr slot target values =
  let !count = V.length values
      !offset = pointerOffset slot target
  in case listLayout values of
       DataList code -> do
         let !size = alignWord (listDataBytes code count)
         pokeByteOff ptr slot (listWord offset code count)
         fillBytes (ptr `plusPtr` target) 0 size
         case code of
           0 -> pure ()
           1 -> mapM_ (putBoolByte ptr target values) [0 .. (count + 7) `quot` 8 - 1]
           2 -> V.imapM_ (\i v -> pokeByteOff ptr (target + i) (fromIntegral (dataWord v) :: Word8)) values
           3 -> V.imapM_ (\i v -> pokeByteOff ptr (target + i * 2) (fromIntegral (dataWord v) :: Word16)) values
           4 -> V.imapM_ (\i v -> pokeByteOff ptr (target + i * 4) (fromIntegral (dataWord v) :: Word32)) values
           _ -> V.imapM_ (\i v -> pokeByteOff ptr (target + i * 8) (dataWord v)) values
         pure (target + size)
       PointerList -> do
         pokeByteOff ptr slot (listWord offset 6 count)
         V.ifoldM' (\end i v -> putPointer ptr (target + 8 * i) end v) (target + 8 * count) values
       StructList dw pw -> do
         let !wordsPerElement = dw + pw
             !body = target + 8
         pokeByteOff ptr slot (listWord offset 7 (count * wordsPerElement))
         pokeByteOff ptr target (structWord count dw pw)
         V.ifoldM'
           (\end i -> \case
              C.Struct dat pointers -> putStruct ptr (body + 8 * i * wordsPerElement) end dw pw dat pointers
              _ -> error "CapnProto.Encode: mixed struct list")
           (body + 8 * count * wordsPerElement)
           values


putBoolByte :: Ptr Word8 -> Int -> V.Vector C.Value -> Int -> IO ()
putBoolByte ptr start values i =
  let from = 8 * i
      set acc bit value = acc .|. (fromIntegral (dataWord value) `shiftL` bit)
      !byte = V.ifoldl' set (0 :: Word8) (V.slice from (min 8 (V.length values - from)) values)
  in pokeByteOff ptr (start + i) byte


pointerOffset :: Int -> Int -> Int
pointerOffset slot target = (target - slot) `quot` 8 - 1
{-# INLINE pointerOffset #-}


structWord :: Int -> Int -> Int -> Word64
structWord offset dw pw
  | dw > 65535 || pw > 65535 = error "CapnProto.Encode: struct section exceeds 16-bit size"
  | otherwise =
      ((fromIntegral offset .&. 0x3fffffff) `shiftL` 2)
        .|. (fromIntegral dw `shiftL` 32) .|. (fromIntegral pw `shiftL` 48)
{-# INLINE structWord #-}


listWord :: Int -> Int -> Int -> Word64
listWord offset code count
  | count > 0x1fffffff = error "CapnProto.Encode: list exceeds 29-bit size"
  | otherwise =
      1 .|. (fromIntegral offset `shiftL` 2)
        .|. (fromIntegral code `shiftL` 32) .|. (fromIntegral count `shiftL` 35)
{-# INLINE listWord #-}


dataWord :: C.Value -> Word64
dataWord = \case
  C.Void -> 0
  C.Bool b -> if b then 1 else 0
  C.Int8 n -> fromIntegral n .&. 0xff
  C.Int16 n -> fromIntegral n .&. 0xffff
  C.Int32 n -> fromIntegral n .&. 0xffffffff
  C.Int64 n -> fromIntegral n
  C.UInt8 n -> fromIntegral n
  C.UInt16 n -> fromIntegral n
  C.UInt32 n -> fromIntegral n
  C.UInt64 n -> n
  C.Float32 n -> fromIntegral (castFloatToWord32 n)
  C.Float64 n -> castDoubleToWord64 n
  C.Enum n -> fromIntegral n
  _ -> error "CapnProto.Encode: pointer in data section"
