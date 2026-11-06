{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

{- | Statistics over checked PLAIN spans. Bounds retain offsets rather than
allocating decoded values or ByteString slices inside the scan.
-}
module Parquet.Nested.Statistics (
  statisticsOrderFor,
  columnStatistics,
) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import GHC.Exts (Int (I#), Int#, isTrue#, (+#), (==#))
import Parquet.Nested.Plain
import Parquet.Types


-- Prefer the explicit orderings recommended by the current format. Legacy
-- entry points without auxiliary options keep their original metadata.
statisticsOrderFor :: SchemaElement -> ColumnOrder
statisticsOrderFor field
  | not (hasDefinedStatisticsOrder field) = TypeDefinedOrder
  | floatingField field = IEEE754TotalOrder
  | seType field == Just PTInt96 = Int96TimestampOrder
  | otherwise = TypeDefinedOrder


data SortKind
  = NoOrder
  | BooleanOrder
  | UnsignedOrder
  | SignedOrder
  | FloatingOrder
  | BinaryOrder
  | DecimalOrder
  | Timestamp96Order
  | LegacyTimestampOrder
  deriving stock (Eq)


floatingField :: SchemaElement -> Bool
floatingField field = seType field `elem` [Just PTFloat, Just PTDouble] || seLogicalType field == Just LTFloat16


sortKind :: ColumnOrder -> SchemaElement -> SortKind
sortKind order field
  | not (hasDefinedStatisticsOrder field) = NoOrder
  | order == UnsupportedColumnOrder = NoOrder
  | floatingField field =
      if order `elem` [TypeDefinedOrder, IEEE754TotalOrder] then FloatingOrder else NoOrder
  | seType field == Just PTInt96 =
      if order == Int96TimestampOrder then LegacyTimestampOrder else NoOrder
  | order /= TypeDefinedOrder = NoOrder
  | otherwise = case seType field of
      Just PTBoolean -> BooleanOrder
      Just PTInt32 -> integerOrder
      Just PTInt64 -> integerOrder
      Just PTFixedLenByteArray
        | Just LTTimestamp {} <- seLogicalType field -> Timestamp96Order
      Just PTFixedLenByteArray -> byteOrder
      Just PTByteArray -> byteOrder
      _ -> NoOrder
  where
    integerOrder = case seLogicalType field of
      Just (LTInteger _ False) -> UnsignedOrder
      _ | seConvertedType field `elem` map Just [CTUInt8, CTUInt16, CTUInt32, CTUInt64] -> UnsignedOrder
      _ -> SignedOrder
    byteOrder = case seLogicalType field of
      Just LTDecimal {} -> DecimalOrder
      _ | seConvertedType field == Just CTDecimal -> DecimalOrder
      _ -> BinaryOrder


columnStatistics :: ColumnOrder -> SchemaElement -> Int -> Int -> ByteString -> Either String Statistics
columnStatistics order field nulls count bytes = do
  layout <- plainLayout field
  if nulls < 0 || not (validPlainValues layout count bytes)
    then Left "Parquet.Nested: statistics values disagree with the physical PLAIN layout"
    else
      if kind == DecimalOrder && emptyDecimal layout 0 0
        then Left "Parquet.Nested: empty DECIMAL value in statistics input"
        else
          if kind == NoOrder
            then Right empty
            else Right $
              case kind of
                UnsignedOrder -> integer
                SignedOrder -> integer
                FloatingOrder -> numeric 0 0 0 (-1) 0 (-1) 0
                _ -> scan layout 0# 0# (-1#) 0# (-1#) 0# 0#
  where
    emptyDecimal VariableValues !index !offset
      | index == count = False
      | otherwise = let size = lengthAt bytes offset in size == 0 || emptyDecimal VariableValues (index + 1) (offset + 4 + size)
    emptyDecimal _ _ _ = False
    !kind = sortKind order field
    !width = case seType field of
      Just PTInt32 -> 4
      Just PTInt64 -> 8
      Just PTFloat -> 4
      Just PTDouble -> 8
      _ -> maybe 0 fromIntegral (seTypeLength field)
    !sign = if width `elem` [2, 4, 8] then (1 :: Word64) `shiftL` (width * 8 - 1) else 0
    !mask = if width == 8 then maxBound else sign + (sign - 1)
    !infinity = case width of 2 -> 0x7c00; 4 -> 0x7f800000; _ -> 0x7ff0000000000000
    empty = Statistics Nothing Nothing (Just (fromIntegral nulls)) Nothing Nothing Nothing Nothing Nothing Nothing
    !(I# count#) = count
    !(I# width#) = width
    !(I# byteSize#) = BS.length bytes
    integer
      | count == 0 = finish (-1) (-1) 0
      | otherwise = let key = raw 0 `xor` integerSign in scanInteger width# key 0# key 0#
    !integerSign = if kind == SignedOrder then sign else 0
    -- Integer ordering needs no NaN state or per-value ordering dispatch.
    -- The checked fixed-width layout bounds every exact-width read.
    scanInteger :: Int# -> Word64 -> Int# -> Word64 -> Int# -> Statistics
    scanInteger cursor# !lowKey low# !highKey high#
      | isTrue# (cursor# ==# byteSize#) = finish (I# low#) (I# high#) 0
      | otherwise =
          let !key = raw (I# cursor#) `xor` integerSign
              !replaceLow = key < lowKey
              !replaceHigh = key > highKey
          in scanInteger
               (cursor# +# width#)
               (if replaceLow then key else lowKey)
               (if replaceLow then cursor# else low#)
               (if replaceHigh then key else highKey)
               (if replaceHigh then cursor# else high#)
    -- Cache comparison keys rather than rereading both extrema for every
    -- value. Width and ordering are constant for the whole physical column.
    numeric :: Int -> Int -> Word64 -> Int -> Word64 -> Int -> Int -> Statistics
    numeric !index !cursor !lowKey !low !highKey !high !nans
      | index == count = finish low high nans
      | otherwise =
          let !bits = raw cursor
              !nan = kind == FloatingOrder && bits .&. (sign - 1) > infinity
              !key = case kind of
                SignedOrder -> bits `xor` sign
                FloatingOrder -> floatKey bits
                _ -> bits
              !firstNumeric = not nan && index == nans
              !include = not nan || index == nans
              !replaceLow = firstNumeric || (include && (low < 0 || key < lowKey))
              !replaceHigh = firstNumeric || (include && (high < 0 || key > highKey))
          in numeric
               (index + 1)
               (cursor + width)
               (if replaceLow then key else lowKey)
               (if replaceLow then cursor else low)
               (if replaceHigh then key else highKey)
               (if replaceHigh then cursor else high)
               (nans + if nan then 1 else 0)
    finish low high nans =
      let nanCount = if kind == FloatingOrder then Just (fromIntegral nans :: Int64) else Nothing
          noBounds = low < 0 || (kind == FloatingOrder && order == TypeDefinedOrder && nans == count)
      in if noBounds
           then empty {statNanCount = nanCount}
           else
             empty
               { statMinValue = Just (bound True low width)
               , statMaxValue = Just (bound False high width)
               , statMinValueExact = Just True
               , statMaxValueExact = Just True
               , statNanCount = nanCount
               }
    scan :: PlainLayout -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Statistics
    scan layout index# cursor# low# lowLength# high# highLength# nans#
      | isTrue# (index# ==# count#) =
          let nanCount = if kind == FloatingOrder then Just (fromIntegral nans :: Int64) else Nothing
              noBounds = low < 0 || (kind == FloatingOrder && order == TypeDefinedOrder && nans == count)
          in if noBounds
               then empty {statNanCount = nanCount}
               else
                 empty
                   { statMinValue = Just (bound True low lowLength)
                   , statMaxValue = Just (bound False high highLength)
                   , statMinValueExact = Just True
                   , statMaxValueExact = Just True
                   , statNanCount = nanCount
                   }
      | otherwise =
          let !offset = case layout of VariableValues -> cursor + 4; BooleanValues -> index; _ -> cursor
              !size = case layout of VariableValues -> lengthAt bytes cursor; BooleanValues -> 1; FixedValues n -> n
              !nan = kind == FloatingOrder && raw offset .&. (sign - 1) > infinity
              !firstNumeric = not nan && index == nans
              !include = not nan || index == nans
              !replaceLow = firstNumeric || (include && (low < 0 || compareAt offset size low lowLength == LT))
              !replaceHigh = firstNumeric || (include && (high < 0 || compareAt offset size high highLength == GT))
          in case offset of
               I# offset# -> case size of
                 I# size# ->
                   scan
                     layout
                     (index# +# 1#)
                     (offset# +# size#)
                     (if replaceLow then offset# else low#)
                     (if replaceLow then size# else lowLength#)
                     (if replaceHigh then offset# else high#)
                     (if replaceHigh then size# else highLength#)
                     (nans# +# if nan then 1# else 0#)
      where
        index = I# index#
        cursor = I# cursor#
        low = I# low#
        lowLength = I# lowLength#
        high = I# high#
        highLength = I# highLength#
        nans = I# nans#
    {-# INLINE raw #-}
    raw offset = case width of
      2 -> fromIntegral (word16At bytes offset)
      4 -> fromIntegral (word32At bytes offset)
      _ -> word64At bytes offset
    {-# INLINE compareAt #-}
    compareAt left leftSize right rightSize = case kind of
      BooleanOrder -> compare (booleanAt bytes left) (booleanAt bytes right)
      UnsignedOrder -> compare (raw left) (raw right)
      SignedOrder -> compare (raw left `xor` sign) (raw right `xor` sign)
      FloatingOrder -> compare (floatKey (raw left)) (floatKey (raw right))
      DecimalOrder -> compareDecimal bytes left leftSize right rightSize
      Timestamp96Order -> compareLittleSigned96 bytes left right
      LegacyTimestampOrder ->
        case compare (word32At bytes (left + 8) `xor` 0x80000000) (word32At bytes (right + 8) `xor` 0x80000000) of
          EQ -> compare (word64At bytes left `xor` 0x8000000000000000) (word64At bytes right `xor` 0x8000000000000000)
          result -> result
      _ -> compareSpan bytes left leftSize right rightSize
    floatKey bits = if bits .&. sign /= 0 then complement bits .&. mask else bits `xor` sign
    bound lower offset size
      | kind == BooleanOrder = BS.singleton (booleanAt bytes offset)
      | kind == FloatingOrder && order == TypeDefinedOrder && raw offset .&. (sign - 1) == 0 =
          let zero = if lower then sign else 0
          in BL.toStrict $ Builder.toLazyByteString $ case width of
               2 -> Builder.word16LE (fromIntegral zero)
               4 -> Builder.word32LE (fromIntegral zero)
               _ -> Builder.word64LE zero
      | otherwise = BS.take size (BS.drop offset bytes)


booleanAt :: ByteString -> Int -> Word8
booleanAt bytes index = (BSU.unsafeIndex bytes (index `quot` 8) `shiftR` (index `rem` 8)) .&. 1
{-# INLINE booleanAt #-}


compareSpan :: ByteString -> Int -> Int -> Int -> Int -> Ordering
compareSpan bytes left leftSize right rightSize = go 0
  where
    !common = min leftSize rightSize
    go !index
      | index == common = compare leftSize rightSize
      | otherwise = case compare (BSU.unsafeIndex bytes (left + index)) (BSU.unsafeIndex bytes (right + index)) of
          EQ -> go (index + 1)
          result -> result
{-# INLINE compareSpan #-}


compareLittleSigned96 :: ByteString -> Int -> Int -> Ordering
compareLittleSigned96 bytes left right =
  case compare (BSU.unsafeIndex bytes (left + 11) `xor` 128) (BSU.unsafeIndex bytes (right + 11) `xor` 128) of
    EQ -> go 10
    result -> result
  where
    go !index
      | index < 0 = EQ
      | otherwise = case compare (BSU.unsafeIndex bytes (left + index)) (BSU.unsafeIndex bytes (right + index)) of
          EQ -> go (index - 1)
          result -> result
{-# INLINE compareLittleSigned96 #-}


compareDecimal :: ByteString -> Int -> Int -> Int -> Int -> Ordering
compareDecimal bytes left leftSize right rightSize
  | leftSize == 0 || rightSize == 0 = compare leftSize rightSize
  | leftNegative /= rightNegative = if leftNegative then LT else GT
  | leftSize == rightSize = compareSpan bytes left leftSize right rightSize
  | otherwise =
      let !leftStart = significant left (left + leftSize) leftNegative
          !rightStart = significant right (right + rightSize) rightNegative
          !leftLength = left + leftSize - leftStart
          !rightLength = right + rightSize - rightStart
      in case compare leftLength rightLength of
           EQ -> compareSpan bytes leftStart leftLength rightStart rightLength
           result -> if leftNegative then invert result else result
  where
    leftNegative = BSU.unsafeIndex bytes left .&. 128 /= 0
    rightNegative = BSU.unsafeIndex bytes right .&. 128 /= 0
    significant !start !end negative
      | start + 1 == end = start
      | BSU.unsafeIndex bytes start == (if negative then 255 else 0)
      , (BSU.unsafeIndex bytes (start + 1) .&. 128 /= 0) == negative =
          significant (start + 1) end negative
      | otherwise = start
    invert LT = GT
    invert GT = LT
    invert EQ = EQ
{-# INLINE compareDecimal #-}
