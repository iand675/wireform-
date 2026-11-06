{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | The Parquet Variant v1 binary value format. This module owns the
metadata/value codec; Parquet column shredding is a separate layer.
-}
module Parquet.Variant (
  Variant (..),
  EncodedVariant (..),
  VariantMetadata,
  metadataBytes,
  metadataKeys,
  metadataContains,
  VariantLimits (..),
  defaultVariantLimits,
  encodeVariant,
  encodeVariantWithLimits,
  decodeVariant,
  decodeVariantWithLimits,
  compileVariantMetadata,
  compileVariantMetadataWithLimits,
  buildVariantMetadata,
  buildVariantMetadataWithLimits,
  buildVariantMetadataFor,
  buildVariantMetadataForWithLimits,
  validateVariantWithMetadata,
  validateVariantWithMetadataWithLimits,
  encodeVariantValue,
  encodeVariantValueWithLimits,
  decodeVariantValue,
  decodeVariantValueWithLimits,
  decodeVariantFragment#,
) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word32, Word64, Word8)
import GHC.Exts (Int (I#), Int#, isTrue#, runRW#, (+#), (==#))
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import GHC.ST (ST (ST))


data Variant
  = VNull
  | VBool !Bool
  | VInt8 !Int8
  | VInt16 !Int16
  | VInt32 !Int32
  | VInt64 !Int64
  | VDouble !Double
  | VDecimal4 !Word8 !Int32
  | VDecimal8 !Word8 !Int64
  | VDecimal16 !Word8 !Integer
  | VDate !Int32
  | VTimestamp !Int64
  | VTimestampNtz !Int64
  | VFloat !Float
  | VBinary !ByteString
  | VString !Text
  | VTime !Int64
  | VTimestampNanos !Int64
  | VTimestampNtzNanos !Int64
  | VUuid !ByteString
  | VObject !(Map Text Variant)
  | VArray !(Vector Variant)
  deriving (Show, Eq)


data EncodedVariant = EncodedVariant
  { variantMetadata :: !ByteString
  , variantValue :: !ByteString
  }
  deriving (Show, Eq)


{- | Validated shared dictionary. All fallback values inside a shredded
Variant must use the same dictionary, including names only present in
typed fields. Keep the original bytes when reading an existing dictionary.
-}
data VariantMetadata = VariantMetadata
  { metadataBytes :: !ByteString
  , metadataKeys :: !(Vector Text)
  , metadataIndices :: !(Map Text Int)
  }
  deriving (Show, Eq)


{- | Bounds on decoded/encoded trees, independently of the byte-buffer size.
Counts include the root. Callers can raise these for large valid values.
-}
data VariantLimits = VariantLimits
  { variantMaxDepth :: {-# UNPACK #-} !Int
  , variantMaxValues :: {-# UNPACK #-} !Int
  , variantMaxDictionaryEntries :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)


defaultVariantLimits :: VariantLimits
defaultVariantLimits = VariantLimits 500 1048576 1048576


bad :: String -> Either String a
bad = Left . ("Parquet.Variant: " ++)


check :: Bool -> String -> Either String ()
check condition message = unless condition (bad message)


checkLimits :: VariantLimits -> Either String ()
checkLimits limits =
  check
    (variantMaxDepth limits >= 0 && variantMaxValues limits > 0 && variantMaxDictionaryEntries limits >= 0)
    "invalid resource limits"


type Decoded# = (# String | (# Variant, Int#, Int# #) #)


decodeVariant :: ByteString -> ByteString -> Either String Variant
decodeVariant = decodeVariantWithLimits defaultVariantLimits


decodeVariantWithLimits :: VariantLimits -> ByteString -> ByteString -> Either String Variant
decodeVariantWithLimits limits metadata bytes = do
  dictionary <- compileVariantMetadataWithLimits limits metadata
  decodeVariantValueWithLimits limits dictionary bytes


compileVariantMetadata :: ByteString -> Either String VariantMetadata
compileVariantMetadata = compileVariantMetadataWithLimits defaultVariantLimits


compileVariantMetadataWithLimits :: VariantLimits -> ByteString -> Either String VariantMetadata
compileVariantMetadataWithLimits limits bytes = do
  checkLimits limits
  names <- decodeDictionary limits bytes
  pure (VariantMetadata bytes names (M.fromList (V.toList (V.imap (\i name -> (name, i)) names))))


{- | Construct one sorted, unique dictionary for a complete shredded value.
The supplied names must include every object key, whether typed or binary.
-}
buildVariantMetadata :: Vector Text -> Either String VariantMetadata
buildVariantMetadata = buildVariantMetadataWithLimits defaultVariantLimits


buildVariantMetadataWithLimits :: VariantLimits -> Vector Text -> Either String VariantMetadata
buildVariantMetadataWithLimits limits keys = do
  checkLimits limits
  let names = V.fromList (S.toAscList (V.foldl' (flip S.insert) S.empty keys))
  check (V.length names <= variantMaxDictionaryEntries limits) "dictionary entry limit exceeded"
  bytes <- encodeDictionary names
  pure (VariantMetadata bytes names (M.fromDistinctAscList (zip (V.toList names) [0 ..])))


decodeVariantValue :: VariantMetadata -> ByteString -> Either String Variant
decodeVariantValue = decodeVariantValueWithLimits defaultVariantLimits


decodeVariantValueWithLimits :: VariantLimits -> VariantMetadata -> ByteString -> Either String Variant
decodeVariantValueWithLimits limits metadata bytes = case variantMaxValues limits of
  I# remaining -> case decodeVariantFragment# limits metadata 0# remaining bytes of
    (# message | #) -> Left message
    (# | (# value, _ #) #) -> Right value


-- | Dictionary membership without rebuilding a set for every typed object.
metadataContains :: Text -> VariantMetadata -> Bool
metadataContains name = M.member name . metadataIndices
{-# INLINE metadataContains #-}


{- | Decode one fallback fragment at its actual depth and with the remaining
value budget of a complete shredded tree. Returns the unspent budget so
sibling fragments cannot each reset the resource limits. The shared metadata
is compiled once for the row. A fragment must consume its whole byte region.
-}
decodeVariantFragment# :: VariantLimits -> VariantMetadata -> Int# -> Int# -> ByteString -> (# String | (# Variant, Int# #) #)
decodeVariantFragment# limits metadata depth remaining bytes
  | variantMaxDepth limits < 0 || variantMaxValues limits <= 0 || variantMaxDictionaryEntries limits < 0 = (# "Parquet.Variant: invalid resource limits" | #)
  | I# depth < 0 || I# remaining < 0 || I# remaining > variantMaxValues limits = (# "Parquet.Variant: invalid fragment budget" | #)
  | V.length (metadataKeys metadata) > variantMaxDictionaryEntries limits = (# "Parquet.Variant: dictionary entry limit exceeded" | #)
  | otherwise = case (variantMaxDepth limits, BS.length bytes) of
      (I# maximumDepth, I# end) -> case decodeAt# maximumDepth (metadataKeys metadata) bytes depth remaining 0# end of
        (# message | #) -> (# message | #)
        (# | (# value, consumed, left #) #)
          | isTrue# (consumed ==# end) -> (# | (# value, left #) #)
          | otherwise -> (# "Parquet.Variant: trailing value bytes" | #)


encodeVariantValue :: VariantMetadata -> Variant -> Either String ByteString
encodeVariantValue = encodeVariantValueWithLimits defaultVariantLimits


encodeVariantValueWithLimits :: VariantLimits -> VariantMetadata -> Variant -> Either String ByteString
encodeVariantValueWithLimits limits metadata value = do
  checkLimits limits
  check (V.length (metadataKeys metadata) <= variantMaxDictionaryEntries limits) "dictionary entry limit exceeded"
  _ <- validateTree limits 0 (variantMaxValues limits) value
  encodeValue (metadataIndices metadata) value


decodeDictionary :: VariantLimits -> ByteString -> Either String (Vector Text)
decodeDictionary limits bs = do
  check (not (BS.null bs)) "empty metadata"
  let header = BS.head bs
      width = fromIntegral (header `shiftR` 6) + 1
  check (header .&. 15 == 1) "unsupported metadata version"
  count <- readCount width bs 1 (BS.length bs)
  check (count <= variantMaxDictionaryEntries limits) "dictionary entry limit exceeded"
  let offsets = 1 + width
      available = BS.length bs - offsets
  check (count < available `quot` width) "truncated metadata offset table"
  let start = offsets + (count + 1) * width
      total = BS.length bs - start
      offset :: Int -> Int
      offset i = fromIntegral (readLE width bs (offsets + i * width))
  check (offset 0 == 0 && offset count == total) "metadata first/last offsets do not cover the dictionary"
  entries <- V.generateM count $ \i -> do
    let lo = offset i
        hi = offset (i + 1)
    check (lo >= 0 && lo <= hi && hi <= total) "invalid metadata offsets"
    utf8 (BS.take (hi - lo) (BS.drop (start + lo) bs))
  when (header .&. 16 /= 0) $
    check (V.and (V.zipWith (<) entries (V.drop 1 entries))) "sorted dictionary is not sorted and unique"
  pure entries


readLE :: Int -> ByteString -> Int -> Word64
readLE width bs start = go 0 0
  where
    go !i !acc
      | i == width = acc
      | otherwise = go (i + 1) (acc .|. (fromIntegral (BS.index bs (start + i)) `shiftL` (i * 8)))
{-# INLINE readLE #-}


readCount :: Int -> ByteString -> Int -> Int -> Either String Int
readCount width bs off end = do
  check (off >= 0 && off <= end && width <= end - off) "truncated count"
  let value = readLE width bs off
  check (value <= fromIntegral (maxBound :: Int)) "count exceeds machine indexing range"
  pure (fromIntegral value)


utf8 :: ByteString -> Either String Text
utf8 = either (const (bad "invalid UTF-8")) Right . TE.decodeUtf8'


-- Only the requested Variant value is materialized. Cursor/budget/error
-- control flow stays unboxed across recursive calls.
good# :: Variant -> Int -> Int -> Decoded#
good# !value (I# end) (I# remaining) = (# | (# value, end, remaining #) #)
{-# INLINE good# #-}


failure# :: String -> Decoded#
failure# message = (# "Parquet.Variant: " ++ message | #)
{-# INLINE failure# #-}


readCount# :: Int -> ByteString -> Int -> Int -> (# String | Int# #)
readCount# width bs off end
  | off < 0 || off > end || width > end - off = (# "Parquet.Variant: truncated count" | #)
  | otherwise =
      let value = readLE width bs off
      in if value > fromIntegral (maxBound :: Int)
           then (# "Parquet.Variant: count exceeds machine indexing range" | #)
           else case fromIntegral value of I# count -> (# | count #)
{-# INLINE readCount# #-}


decodeAt# :: Int# -> Vector Text -> ByteString -> Int# -> Int# -> Int# -> Int# -> Decoded#
decodeAt# maximumDepth dictionary bs depth# remaining# off# end#
  | depth > I# maximumDepth = failure# "nesting depth limit exceeded"
  | remaining <= 0 = failure# "value count limit exceeded"
  | off < 0 || off >= end || end > BS.length bs = failure# "missing value"
  | otherwise =
      let tag = fromIntegral (BS.index bs off) :: Int
          header = tag `shiftR` 2
          start = off + 1
          result value consumed = good# value consumed (remaining - 1)
          fixed width make
            | width > end - start = failure# "truncated primitive"
            | otherwise = result (make (readLE width bs start)) (start + width)
          string size begin
            | size > end - begin = failure# "truncated string"
            | otherwise = case TE.decodeUtf8' (BS.take size (BS.drop begin bs)) of
                Left _ -> failure# "invalid UTF-8"
                Right text -> result (VString text) (begin + size)
          blob make = case readCount# 4 bs start end of
            (# message | #) -> (# message | #)
            (# | size# #) ->
              let size = I# size#
                  begin = start + 4
              in if size > end - begin
                   then failure# "truncated binary/string"
                   else result (make (BS.take size (BS.drop begin bs))) (begin + size)
      in case tag .&. 3 of
           1 -> string header start
           2 -> decodeObject# maximumDepth dictionary bs (depth# +# 1#) (remaining# +# (-1#)) header start end
           3 -> decodeArray# maximumDepth dictionary bs (depth# +# 1#) (remaining# +# (-1#)) header start end
           _ -> case header of
             0 -> result VNull start
             1 -> result (VBool True) start
             2 -> result (VBool False) start
             3 -> fixed 1 (VInt8 . fromIntegral)
             4 -> fixed 2 (VInt16 . fromIntegral)
             5 -> fixed 4 (VInt32 . fromIntegral)
             6 -> fixed 8 (VInt64 . fromIntegral)
             7 -> fixed 8 (VDouble . castWord64ToDouble)
             8
               | end - start < 5 -> failure# "truncated decimal"
               | otherwise ->
                   let scale = BS.index bs start
                       value = fromIntegral (readLE 4 bs (start + 1)) :: Int32
                   in if scale > 38
                        then failure# "decimal scale exceeds 38"
                        else
                          if value < (-999999999) || value > 999999999
                            then failure# "decimal precision exceeds its physical width"
                            else result (VDecimal4 scale value) (start + 5)
             9
               | end - start < 9 -> failure# "truncated decimal"
               | otherwise ->
                   let scale = BS.index bs start
                       value = fromIntegral (readLE 8 bs (start + 1)) :: Int64
                   in if scale > 38
                        then failure# "decimal scale exceeds 38"
                        else
                          if value < (-999999999999999999) || value > 999999999999999999
                            then failure# "decimal precision exceeds its physical width"
                            else result (VDecimal8 scale value) (start + 9)
             10
               | end - start < 17 -> failure# "truncated decimal"
               | otherwise ->
                   let scale = BS.index bs start
                       value = signedLE 16 bs (start + 1)
                   in case checkDecimal 16 scale value of
                        Left message -> (# message | #)
                        Right () -> result (VDecimal16 scale value) (start + 17)
             11 -> fixed 4 (VDate . fromIntegral)
             12 -> fixed 8 (VTimestamp . fromIntegral)
             13 -> fixed 8 (VTimestampNtz . fromIntegral)
             14 -> fixed 4 (VFloat . castWord32ToFloat . fromIntegral)
             15 -> blob VBinary
             16 -> case readCount# 4 bs start end of
               (# message | #) -> (# message | #)
               (# | size# #) -> string (I# size#) (start + 4)
             17
               | end - start < 8 -> failure# "truncated primitive"
               | otherwise ->
                   let value = fromIntegral (readLE 8 bs start) :: Int64
                   in if value < 0 || value >= 86400000000
                        then failure# "time is outside a day"
                        else result (VTime value) (start + 8)
             18 -> fixed 8 (VTimestampNanos . fromIntegral)
             19 -> fixed 8 (VTimestampNtzNanos . fromIntegral)
             20
               | end - start < 16 -> failure# "truncated primitive"
               | otherwise -> result (VUuid (BS.take 16 (BS.drop start bs))) (start + 16)
             _ -> failure# "unknown primitive type"
  where
    depth = I# depth#
    remaining = I# remaining#
    off = I# off#
    end = I# end#


signedLE :: Int -> ByteString -> Int -> Integer
signedLE width bs off =
  let raw = go 0 0
  in if BS.index bs (off + width - 1) .&. 128 == 0 then raw else raw - (1 `shiftL` (8 * width))
  where
    go !i !acc
      | i == width = acc
      | otherwise = go (i + 1) (acc .|. (fromIntegral (BS.index bs (off + i)) `shiftL` (8 * i)))


checkDecimal :: Int -> Word8 -> Integer -> Either String ()
checkDecimal width scale value = do
  check (scale <= 38) "decimal scale exceeds 38"
  let precision :: Int
      precision = case width of
        4 -> 9
        8 -> 18
        _ -> 38
  check (abs value < 10 ^ precision) "decimal precision exceeds its physical width"


checkTime :: Int64 -> Either String ()
checkTime micros = check (micros >= 0 && micros < 86400000000) "time is outside a day"


-- Object offsets follow key order, not physical value order. The offset
-- index is required only to establish a precise enclosing bound per child.
-- It also catches duplicate/overlapping child offsets without quadratic scans.
decodeObject# :: Int# -> Vector Text -> ByteString -> Int# -> Int# -> Int -> Int -> Int -> Decoded#
decodeObject# maximumDepth dictionary bs depth# remaining# header off end =
  case readCount# cw bs off end of
    (# message | #) -> (# message | #)
    (# | count# #) ->
      let count = I# count#
          idsStart = off + cw
      in if count > I# remaining#
           then failure# "value count limit exceeded"
           else
             if ow > end - idsStart || count > (end - idsStart - ow) `quot` (iw + ow)
               then failure# "truncated object table"
               else
                 let offsetsStart = idsStart + count * iw
                     start = offsetsStart + (count + 1) * ow
                     offset i = fromIntegral (readLE ow bs (offsetsStart + i * ow)) :: Int
                     total = offset count
                     -- Validate names in key order, build the physical-order
                     -- index in the same pass. No intermediate names vector.
                     slotsLoop !i !previousId !slots
                       | i == count = (# | slots #)
                       | otherwise =
                           let keyId = readLE iw bs (idsStart + i * iw)
                               lo = offset i
                           in if keyId >= fromIntegral (V.length dictionary)
                                then (# "Parquet.Variant: object field ID outside dictionary" | #)
                                else
                                  if lo < 0 || lo >= total
                                    then (# "Parquet.Variant: invalid object offsets" | #)
                                    else
                                      let name = V.unsafeIndex dictionary (fromIntegral keyId)
                                      in if i > 0 && V.unsafeIndex dictionary previousId >= name
                                           then (# "Parquet.Variant: object keys are not sorted and unique" | #)
                                           else slotsLoop (i + 1) (fromIntegral keyId) (IM.insert lo name slots)
                 in if total < 0 || total > end - start
                      then failure# "object exceeds enclosing value"
                      else case slotsLoop 0 0 IM.empty of
                        (# message | #) -> (# message | #)
                        (# | slots #)
                          | IM.size slots /= count -> failure# "object values share an offset"
                          | if count == 0 then total /= 0 else fst (IM.findMin slots) /= 0 -> failure# "object values do not start at zero"
                          | otherwise -> objectLoop start total (IM.toAscList slots) M.empty remaining#
  where
    ow = (header .&. 3) + 1
    iw = ((header `shiftR` 2) .&. 3) + 1
    cw = if header .&. 16 == 0 then 1 else 4
    objectLoop start total slots !values budget# = case slots of
      [] -> case start + total of I# consumed -> (# | (# VObject values, consumed, budget# #) #)
      (lo, name) : rest ->
        let hi = case rest of [] -> total; (next, _) : _ -> next
        in if lo >= hi || hi > total
             then failure# "invalid object offsets"
             else case (start + lo, start + hi) of
               (I# begin, I# finish) -> case decodeAt# maximumDepth dictionary bs depth# budget# begin finish of
                 (# message | #) -> (# message | #)
                 (# | (# value, consumed, remaining #) #)
                   | not (isTrue# (consumed ==# finish)) -> failure# "object child does not fill its byte range"
                   | otherwise -> objectLoop start total rest (M.insert name value values) remaining


decodeArray# :: Int# -> Vector Text -> ByteString -> Int# -> Int# -> Int -> Int -> Int -> Decoded#
decodeArray# maximumDepth dictionary bs depth# remaining# header off end =
  case readCount# cw bs off end of
    (# message | #) -> (# message | #)
    (# | count# #) ->
      let count = I# count#
          offsetsStart = off + cw
      in if count > I# remaining#
           then failure# "value count limit exceeded"
           else
             if count >= (end - offsetsStart) `quot` ow
               then failure# "truncated array table"
               else
                 let start = offsetsStart + (count + 1) * ow
                     offset i = fromIntegral (readLE ow bs (offsetsStart + i * ow)) :: Int
                     total = offset count
                 in if offset 0 /= 0 || total < 0 || total > end - start
                      then failure# "invalid array first/last offsets"
                      else runRW# $ \state -> case MV.unsafeNew count of
                        ST allocate -> case allocate state of
                          (# state1, output #) ->
                            let loop i# budget# state2
                                  | isTrue# (i# ==# count#) = case V.unsafeFreeze output of
                                      ST freeze -> case freeze state2 of
                                        (# _, values #) -> case start + total of
                                          I# consumed -> (# | (# VArray values, consumed, budget# #) #)
                                  | otherwise =
                                      let lo = offset (I# i#)
                                          hi = offset (I# (i# +# 1#))
                                      in if lo < 0 || lo >= hi || hi > total
                                           then failure# "invalid array offsets"
                                           else case (start + lo, start + hi) of
                                             (I# begin, I# finish) -> case decodeAt# maximumDepth dictionary bs depth# budget# begin finish of
                                               (# message | #) -> (# message | #)
                                               (# | (# value, consumed, remaining #) #)
                                                 | not (isTrue# (consumed ==# finish)) -> failure# "array child does not fill its byte range"
                                                 | otherwise -> case value of
                                                     !strictValue -> case MV.unsafeWrite output (I# i#) strictValue of
                                                       ST write -> case write state2 of
                                                         (# state3, () #) -> loop (i# +# 1#) remaining state3
                            in loop 0# remaining# state1
  where
    ow = (header .&. 3) + 1
    cw = if header .&. 4 == 0 then 1 else 4


encodeVariant :: Variant -> Either String EncodedVariant
encodeVariant = encodeVariantWithLimits defaultVariantLimits


encodeVariantWithLimits :: VariantLimits -> Variant -> Either String EncodedVariant
encodeVariantWithLimits limits value =
  withVariantMetadataFor limits value $ \metadata _ indices -> do
    bytes <- encodeValue indices value
    pure (EncodedVariant metadata bytes)


-- | Collect all actual object names, including names selected for typed
-- columns, after validating the complete tree's depth/value/type limits.
buildVariantMetadataFor :: Variant -> Either String VariantMetadata
buildVariantMetadataFor = buildVariantMetadataForWithLimits defaultVariantLimits


buildVariantMetadataForWithLimits :: VariantLimits -> Variant -> Either String VariantMetadata
buildVariantMetadataForWithLimits limits value =
  withVariantMetadataFor limits value $ \bytes dictionary indices ->
    pure (VariantMetadata bytes dictionary indices)
{-# INLINE buildVariantMetadataForWithLimits #-}


-- Keep the components separate until the caller chooses its output. This
-- avoids constructing a temporary compiled-metadata record in the binary
-- encoder, including with GHC 9.14's simplifier.
withVariantMetadataFor :: VariantLimits -> Variant -> (ByteString -> Vector Text -> M.Map Text Int -> Either String a) -> Either String a
withVariantMetadataFor limits value use = do
  checkLimits limits
  _ <- validateTree limits 0 (variantMaxValues limits) value
  let dictionary = V.fromList (S.toAscList (collectNames value S.empty))
  check (V.length dictionary <= variantMaxDictionaryEntries limits) "dictionary entry limit exceeded"
  bytes <- encodeDictionary dictionary
  use bytes dictionary (M.fromDistinctAscList (zip (V.toList dictionary) [0 ..]))
{-# INLINE withVariantMetadataFor #-}


-- | Validate a whole value against an original compiled dictionary without
-- producing a second encoded copy. Every actual key must be present even
-- when the value will be fully shredded into typed columns.
validateVariantWithMetadata :: VariantMetadata -> Variant -> Either String ()
validateVariantWithMetadata = validateVariantWithMetadataWithLimits defaultVariantLimits


validateVariantWithMetadataWithLimits :: VariantLimits -> VariantMetadata -> Variant -> Either String ()
validateVariantWithMetadataWithLimits limits metadata value = do
  checkLimits limits
  check (V.length (metadataKeys metadata) <= variantMaxDictionaryEntries limits) "dictionary entry limit exceeded"
  _ <- validateTree limits 0 (variantMaxValues limits) value
  check (namesPresent value) "object key absent from shared dictionary"
  where
    namesPresent current = case current of
      VObject fields -> M.foldrWithKey (\name child rest -> metadataContains name metadata && namesPresent child && rest) True fields
      VArray values -> V.all namesPresent values
      _ -> True



validateTree :: VariantLimits -> Int -> Int -> Variant -> Either String Int
validateTree limits depth remaining value = do
  check (depth <= variantMaxDepth limits) "nesting depth limit exceeded"
  check (remaining > 0) "value count limit exceeded"
  let left = remaining - 1
      child = validateTree limits (depth + 1)
  case value of
    VArray xs -> V.foldM' child left xs
    VObject fields -> M.foldl' (\acc v -> acc >>= \budget -> child budget v) (pure left) fields
    VDecimal4 scale raw -> checkDecimal 4 scale (toInteger raw) >> pure left
    VDecimal8 scale raw -> checkDecimal 8 scale (toInteger raw) >> pure left
    VDecimal16 scale raw -> checkDecimal 16 scale raw >> pure left
    VTime micros -> checkTime micros >> pure left
    VUuid bytes -> check (BS.length bytes == 16) "UUID must contain exactly 16 bytes" >> pure left
    _ -> pure left


collectNames :: Variant -> S.Set Text -> S.Set Text
collectNames value acc = case value of
  VArray xs -> V.foldl' (flip collectNames) acc xs
  VObject fields -> M.foldlWithKey' (\names name v -> collectNames v (S.insert name names)) acc fields
  _ -> acc


encodeDictionary :: Vector Text -> Either String ByteString
encodeDictionary dictionary = do
  let strings = V.map TE.encodeUtf8 dictionary
      offsets = V.scanl' (\n s -> n + toInteger (BS.length s)) 0 strings
      total = V.last offsets
      count = V.length strings
  bound32 (max total (toInteger count))
  let width = pickWidth (fromInteger (max total (toInteger count)))
  pure (build (B.word8 (17 .|. fromIntegral ((width - 1) `shiftL` 6)) <> unsigned width count <> foldMap (unsigned width . fromInteger) offsets <> foldMap B.byteString strings))


encodeValue :: Map Text Int -> Variant -> Either String ByteString
encodeValue dictionary = fmap build . go
  where
    prim tag bytes = pure (B.word8 (tag `shiftL` 2) <> bytes)
    go = \case
      VNull -> prim 0 mempty
      VBool True -> prim 1 mempty
      VBool False -> prim 2 mempty
      VInt8 n -> prim 3 (B.int8 n)
      VInt16 n -> prim 4 (B.int16LE n)
      VInt32 n -> prim 5 (B.int32LE n)
      VInt64 n -> prim 6 (B.int64LE n)
      VDouble n -> prim 7 (B.word64LE (castDoubleToWord64 n))
      VDecimal4 scale n -> prim 8 (B.word8 scale <> B.int32LE n)
      VDecimal8 scale n -> prim 9 (B.word8 scale <> B.int64LE n)
      VDecimal16 scale n -> prim 10 (B.word8 scale <> int128 n)
      VDate n -> prim 11 (B.int32LE n)
      VTimestamp n -> prim 12 (B.int64LE n)
      VTimestampNtz n -> prim 13 (B.int64LE n)
      VFloat n -> prim 14 (B.word32LE (castFloatToWord32 n))
      VBinary bytes -> long 15 bytes
      VString text ->
        let bytes = TE.encodeUtf8 text
            size = BS.length bytes
        in if size < 64 then pure (B.word8 (fromIntegral (size `shiftL` 2) .|. 1) <> B.byteString bytes) else long 16 bytes
      VTime n -> prim 17 (B.int64LE n)
      VTimestampNanos n -> prim 18 (B.int64LE n)
      VTimestampNtzNanos n -> prim 19 (B.int64LE n)
      VUuid bytes -> prim 20 (B.byteString bytes)
      VArray xs -> do
        children <- V.mapM (encodeValue dictionary) xs
        compound 3 V.empty children
      VObject fields -> do
        let names = V.fromList (M.keys fields)
        ids <- V.mapM (\name -> maybe (bad "object key absent from shared dictionary") pure (M.lookup name dictionary)) names
        children <- V.mapM (encodeValue dictionary) (V.fromList (M.elems fields))
        compound 2 ids children
    long tag bytes = do
      bound32 (toInteger (BS.length bytes))
      prim tag (B.word32LE (fromIntegral (BS.length bytes)) <> B.byteString bytes)
    compound basic ids children = do
      let count = V.length children
          offsets = V.scanl' (\n b -> n + toInteger (BS.length b)) 0 children
          total = V.last offsets
          maxId = V.foldl' max 0 ids
      bound32 (max (toInteger count) total)
      let ow = pickWidth (fromInteger total)
          iw = pickWidth maxId
          large = count > 255
          header = if basic == 2 then (if large then 16 else 0) .|. ((iw - 1) `shiftL` 2) .|. (ow - 1) else (if large then 4 else 0) .|. (ow - 1)
      pure (B.word8 (fromIntegral ((header `shiftL` 2) .|. basic)) <> unsigned (if large then 4 else 1) count <> foldMap (unsigned iw) ids <> foldMap (unsigned ow . fromInteger) offsets <> foldMap B.byteString children)


bound32 :: Integer -> Either String ()
bound32 n = check (n <= toInteger (maxBound :: Word32)) "encoded count/offset exceeds unsigned 32-bit width"


pickWidth :: Int -> Int
pickWidth n
  | n < 256 = 1
  | n < 65536 = 2
  | n < 16777216 = 3
  | otherwise = 4


unsigned :: Int -> Int -> B.Builder
unsigned width value = go 0
  where
    go !i
      | i == width = mempty
      | otherwise = B.word8 (fromIntegral (value `shiftR` (i * 8))) <> go (i + 1)


int128 :: Integer -> B.Builder
int128 value = go 0
  where
    go !i
      | i == 16 = mempty
      | otherwise = B.word8 (fromIntegral (value `shiftR` (i * 8))) <> go (i + 1)


build :: B.Builder -> ByteString
build = BL.toStrict . B.toLazyByteString
