{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Semantic reconstruction of canonical @arrow.parquet.variant@ arrays.

Compile field names, storage profiles and primitive conversions once. Each
present row uses one original metadata dictionary and one depth/value budget
across its typed nodes and binary fallback fragments. Null storage slots,
present Variant nulls and missing object fields remain distinct.

See https://arrow.apache.org/docs/format/CanonicalExtensions.html#parquet-variant
and https://parquet.apache.org/docs/file-format/types/variantshredding/.
-}
module Parquet.Variant.Arrow (
  VariantArrowReader,
  compileVariantArrowReader,
  compileVariantArrowReaderWithLimits,
  variantArrowLength,
  variantArrowAt,
  variantArrowAt#,
) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Arrow.Write.Columns (validateColumns)
import Control.Monad (unless)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Storable qualified as S
import Foreign.Storable (Storable)
import GHC.Exts (Int (I#), Int#, isTrue#, runRW#, (+#), (==#))
import GHC.ST (ST (..))
import Parquet.Variant qualified as B
import Parquet.Variant.Array (BytesReader (..), alignVariantColumns, compileBytes)
import Parquet.Variant.Schema qualified as Schema


data VariantArrowReader = VariantArrowReader
  { variantArrowLength :: {-# UNPACK #-} !Int
  , readerValidity :: !(Maybe C.Validity)
  , readerMetadata :: !BytesReader
  , readerNode :: !NodePlan
  , readerLimits :: !B.VariantLimits
  }


newtype PrimitiveReader = PrimitiveReader {primitiveAt# :: Int# -> (# String | B.Variant #)}


newtype RangeReader = RangeReader {rangeAt# :: Int# -> (# Int#, Int# #)}


data NodePlan = NodePlan !(Maybe C.Validity) !BytesReader !(Maybe TypedPlan)


data TypedPlan
  = PrimitivePlan !(Maybe C.Validity) !PrimitiveReader
  | ObjectPlan !(Maybe C.Validity) !(V.Vector ObjectField)
  | ArrayPlan !(Maybe C.Validity) !RangeReader !NodePlan


data ObjectField = ObjectField
  { objectName :: !Text
  , objectPlan :: !NodePlan
  }


type NodeResult# = (# String | (# B.Variant, Int# #) | Int# #)


type PresentResult# = (# String | (# B.Variant, Int# #) #)


compileVariantArrowReader :: A.Field -> C.ColumnArray -> Either String VariantArrowReader
compileVariantArrowReader = compileVariantArrowReaderWithLimits B.defaultVariantLimits Schema.defaultVariantSchemaLimits


{- | Independent limits bound schema compilation and each reconstructed tree.
Arrow buffer shapes are checked once. Required-child validity is checked only
where its containing Variant/object/array is present; null ancestors mask
arbitrary children, including malformed Variant payload bytes.
-}
compileVariantArrowReaderWithLimits :: B.VariantLimits -> Schema.VariantSchemaLimits -> A.Field -> C.ColumnArray -> Either String VariantArrowReader
compileVariantArrowReaderWithLimits limits schemaLimits field input = do
  unless (B.variantMaxDepth limits >= 0 && B.variantMaxValues limits > 0 && B.variantMaxDictionaryEntries limits >= 0) (Left "Parquet.Variant.Arrow: invalid resource limits")
  Schema.validateVariantFieldWithLimits schemaLimits field
  column <- alignVariantColumns field input
  validateColumns (V.singleton field {A.fieldChildren = V.map relax (A.fieldChildren field)}) (V.singleton column)
  metadata <- namedColumn "metadata" column >>= compileBytes
  plan <- compileNode field column
  pure (VariantArrowReader (C.columnLength column) (C.validity column) metadata plan limits)
  where
    relax current = current {A.fieldNullable = True, A.fieldChildren = V.map relax (A.fieldChildren current)}


namedColumn :: Text -> C.ColumnArray -> Either String C.ColumnArray
namedColumn name (C.ColStruct _ _ children) = maybe (Left "Parquet.Variant.Arrow: missing named child") (Right . snd) (V.find ((== name) . fst) children)
namedColumn _ _ = Left "Parquet.Variant.Arrow: expected struct storage"


compileNode :: A.Field -> C.ColumnArray -> Either String NodePlan
compileNode field column = do
  let children = A.fieldChildren field
  fallback <- case V.find ((== "value") . A.fieldName) children of
    Nothing -> pure (BytesReader (\_ -> (# | () #)))
    Just _ -> namedColumn "value" column >>= compileBytes
  typed <- case V.find ((== "typed_value") . A.fieldName) children of
    Nothing -> pure Nothing
    Just child -> Just <$> (namedColumn "typed_value" column >>= compileTyped child)
  -- Preserve required-child checks at visible rows without scanning payloads
  -- beneath null parents. Usually no required child has a validity bitmap;
  -- only the exceptional case needs one combined mask at compile time.
  required <-
    V.mapM
      ( \child ->
          if not (A.fieldNullable child) && A.fieldName child /= "metadata"
            then C.validity <$> namedColumn (A.fieldName child) column
            else pure Nothing
      )
      children
  let requiredMasks = V.mapMaybe id required
      visible =
        if V.null requiredMasks
          then C.validity column
          else
            C.validityGenerate
              (C.columnLength column)
              ( \index ->
                  C.isValidAt (C.validity column) index && V.all (\mask -> C.bitAt (C.validityBits mask) index) requiredMasks
              )
  pure (NodePlan visible fallback typed)


compileTyped :: A.Field -> C.ColumnArray -> Either String TypedPlan
compileTyped field column = case (A.fieldType field, column) of
  (A.AStruct, C.ColStruct _ validity columns) -> do
    -- Name alignment has already bound every object once, before shape
    -- validation. Positional compilation now needs no second name map.
    children <- V.zipWithM (\child (_, value) -> ObjectField (A.fieldName child) <$> compileNode child value) (A.fieldChildren field) columns
    pure (ObjectPlan validity children)
  (_, C.ColList validity offsets child) -> list validity (offsetsRange offsets) child
  (_, C.ColLargeList validity offsets child) -> list validity (offsetsRange offsets) child
  (_, C.ColListView validity offsets sizes child) -> list validity (viewRange offsets sizes) child
  _ -> PrimitivePlan (C.validity column) <$> compilePrimitive field column
  where
    list validity ranges child = do
      plan <- compileNode (V.head (A.fieldChildren field)) child
      pure (ArrayPlan validity ranges plan)


offsetsRange :: (Storable a, Integral a) => S.Vector a -> RangeReader
offsetsRange offsets = RangeReader $ \index ->
  case fromIntegral (S.unsafeIndex offsets (I# index)) of
    I# start -> case fromIntegral (S.unsafeIndex offsets (I# (index +# 1#))) - I# start of
      I# count -> (# start, count #)
{-# INLINE offsetsRange #-}


viewRange :: (Storable a, Integral a) => S.Vector a -> S.Vector a -> RangeReader
viewRange offsets sizes = RangeReader $ \index ->
  case fromIntegral (S.unsafeIndex offsets (I# index)) of
    I# start -> case fromIntegral (S.unsafeIndex sizes (I# index)) of
      I# count -> (# start, count #)
{-# INLINE viewRange #-}


compilePrimitive :: A.Field -> C.ColumnArray -> Either String PrimitiveReader
compilePrimitive field column = case column of
  C.ColInt8 _ xs -> pure (primitive B.VInt8 xs)
  C.ColInt16 _ xs -> pure (primitive B.VInt16 xs)
  C.ColInt32 _ xs -> pure (primitive B.VInt32 xs)
  C.ColInt64 _ xs -> pure (primitive B.VInt64 xs)
  C.ColFloat _ xs -> pure (primitive B.VFloat xs)
  C.ColDouble _ xs -> pure (primitive B.VDouble xs)
  C.ColDate32 _ xs -> pure (primitive B.VDate xs)
  C.ColTime64 _ xs -> pure $ PrimitiveReader $ \index ->
    let value = S.unsafeIndex xs (I# index)
    in if value >= 0 && value < 86400000000 then (# | B.VTime value #) else (# "Parquet.Variant.Arrow: time outside one day" | #)
  C.ColTimestamp _ xs -> pure (primitive timestamp xs)
  C.ColDecimal32 precision scale _ xs ->
    let !bound = fromInteger (10 ^ precision) :: Int32
    in pure $ PrimitiveReader $ \index ->
         let !value = S.unsafeIndex xs (I# index)
         in if value > negate bound && value < bound then (# | B.VDecimal4 (fromIntegral scale) value #) else badDecimal#
  C.ColDecimal64 precision scale _ xs ->
    let !bound = fromInteger (10 ^ precision) :: Int64
    in pure $ PrimitiveReader $ \index ->
         let !value = S.unsafeIndex xs (I# index)
         in if value > negate bound && value < bound then (# | B.VDecimal8 (fromIntegral scale) value #) else badDecimal#
  C.ColDecimal128 precision scale _ xs ->
    let !bound = 10 ^ precision
    in pure $ PrimitiveReader $ \index ->
         let !value = C.decimal128ToInteger (S.unsafeIndex xs (I# index))
         in if value > negate bound && value < bound then (# | B.VDecimal16 (fromIntegral scale) value #) else badDecimal#
  _
    | A.fieldType field == A.ABool
    , Just values <- C.asBool column ->
        pure $ PrimitiveReader $ \index -> case C.boolArrayAt values (I# index) of
          Just value -> (# | B.VBool value #)
          Nothing -> missingPrimitive#
    | A.fieldType field `elem` [A.AUtf8, A.ALargeUtf8, A.AUtf8View] ->
        pure $ PrimitiveReader $ \index -> case C.anyTextAt column (I# index) of
          Just value -> (# | B.VString value #)
          Nothing -> missingPrimitive#
    | otherwise ->
        pure $ PrimitiveReader $ \index -> case C.anyBytesAt column (I# index) of
          Just value -> (# | if A.fieldType field == A.AFixedSizeBinary 16 then B.VUuid value else B.VBinary value #)
          Nothing -> missingPrimitive#
  where
    timestamp = case A.fieldType field of
      A.ATimestamp A.Microsecond (Just "UTC") -> B.VTimestamp
      A.ATimestamp A.Nanosecond (Just "UTC") -> B.VTimestampNanos
      A.ATimestamp A.Microsecond Nothing -> B.VTimestampNtz
      _ -> B.VTimestampNtzNanos
    primitive :: Storable a => (a -> B.Variant) -> S.Vector a -> PrimitiveReader
    primitive make values = PrimitiveReader $ \index -> let !value = make (S.unsafeIndex values (I# index)) in (# | value #)
    {-# INLINE primitive #-}
    badDecimal# :: (# String | B.Variant #)
    badDecimal# = (# "Parquet.Variant.Arrow: decimal exceeds declared precision" | #)
    missingPrimitive# :: (# String | B.Variant #)
    missingPrimitive# = (# "Parquet.Variant.Arrow: missing typed primitive" | #)


variantArrowAt :: VariantArrowReader -> Int -> Either String (Maybe B.Variant)
variantArrowAt reader (I# index) = case variantArrowAt# reader index of
  (# message | | #) -> Left message
  (# | value | #) -> Right (Just value)
  (# | | () #) -> Right Nothing


variantArrowAt# :: VariantArrowReader -> Int# -> (# String | B.Variant | () #)
variantArrowAt# reader index
  | I# index < 0 || I# index >= variantArrowLength reader = (# "Parquet.Variant.Arrow: row index out of range" | | #)
  | not (C.isValidAt (readerValidity reader) (I# index)) = (# | | () #)
  | otherwise = case bytesAt# (readerMetadata reader) index of
      (# | () #) -> (# "Parquet.Variant.Arrow: present root has null metadata" | | #)
      (# bytes | #) -> case B.compileVariantMetadataWithLimits (readerLimits reader) bytes of
        Left message -> (# message | | #)
        Right dictionary -> case B.variantMaxValues (readerLimits reader) of
          I# budget -> case required# (readerLimits reader) dictionary (readerNode reader) 0# budget index of
            (# message | #) -> (# message | | #)
            (# | (# value, _ #) #) -> (# | value | #)
{-# INLINE variantArrowAt# #-}


charge# :: B.VariantLimits -> Int# -> Int# -> (# String | Int# #)
charge# limits depth budget
  | I# depth > B.variantMaxDepth limits = (# "Parquet.Variant.Arrow: nesting depth limit exceeded" | #)
  | I# budget <= 0 = (# "Parquet.Variant.Arrow: value count limit exceeded" | #)
  | otherwise = (# | budget +# (-1#) #)
{-# INLINE charge# #-}


required# :: B.VariantLimits -> B.VariantMetadata -> NodePlan -> Int# -> Int# -> Int# -> PresentResult#
required# limits dictionary plan depth budget index = case readNode# limits dictionary plan depth budget index of
  (# message | | #) -> (# message | #)
  (# | (# value, left #) | #) -> (# | (# value, left #) #)
  (# | | left #) -> case charge# limits depth left of
    (# message | #) -> (# message | #)
    (# | remaining #) -> (# | (# B.VNull, remaining #) #)
{-# INLINE required# #-}


readNode# :: B.VariantLimits -> B.VariantMetadata -> NodePlan -> Int# -> Int# -> Int# -> NodeResult#
readNode# limits dictionary (NodePlan validity fallback typed) depth budget index
  | not (C.isValidAt validity (I# index)) = (# "Parquet.Variant.Arrow: null shredded wrapper or required child" | | #)
  | otherwise = case typed of
      Just plan | typedPresent plan -> case plan of
        PrimitivePlan _ primitive -> case bytesAt# fallback index of
          (# _ | #) -> conflict#
          (# | () #) -> case charge# limits depth budget of
            (# message | #) -> (# message | | #)
            (# | left #) -> case primitiveAt# primitive index of
              (# message | #) -> (# message | | #)
              (# | !value #) -> (# | (# value, left #) | #)
        ArrayPlan _ ranges child -> case bytesAt# fallback index of
          (# _ | #) -> conflict#
          (# | () #) -> case charge# limits depth budget of
            (# message | #) -> (# message | | #)
            (# | left #) -> case rangeAt# ranges index of
              (# start, count #) -> case readArray# limits dictionary child (depth +# 1#) left start count of
                (# message | #) -> (# message | | #)
                (# | (# value, remaining #) #) -> (# | (# value, remaining #) | #)
        ObjectPlan _ fields -> case bytesAt# fallback index of
          (# | () #) -> case charge# limits depth budget of
            (# message | #) -> (# message | | #)
            (# | left #) -> object fields M.empty left
          (# bytes | #) -> case B.decodeVariantFragment# limits dictionary depth budget bytes of
            (# message | #) -> (# message | | #)
            (# | (# B.VObject rest, left #) #)
              | V.any (\field -> M.member (objectName field) rest) fields -> (# "Parquet.Variant.Arrow: object field appears in typed and fallback storage" | | #)
              | otherwise -> object fields rest left
            _ -> (# "Parquet.Variant.Arrow: partial fallback must be an object" | | #)
      _ -> case bytesAt# fallback index of
        (# | () #) -> (# | | budget #)
        (# bytes | #) -> case B.decodeVariantFragment# limits dictionary depth budget bytes of
          (# message | #) -> (# message | | #)
          (# | (# value, left #) #) -> case (typed, value) of
            (Just ObjectPlan {}, B.VObject {}) -> (# "Parquet.Variant.Arrow: object is missing its typed storage" | | #)
            (Just ArrayPlan {}, B.VArray {}) -> (# "Parquet.Variant.Arrow: array is missing its typed storage" | | #)
            _ -> (# | (# value, left #) | #)
  where
    typedPresent plan = C.isValidAt (case plan of PrimitivePlan v _ -> v; ObjectPlan v _ -> v; ArrayPlan v _ _ -> v) (I# index)
    object :: V.Vector ObjectField -> M.Map Text B.Variant -> Int# -> NodeResult#
    object fields rest left = case readObject# limits dictionary fields (depth +# 1#) left index 0# rest of
      (# message | #) -> (# message | | #)
      (# | (# value, remaining #) #) -> (# | (# value, remaining #) | #)
    conflict# :: NodeResult#
    conflict# = (# "Parquet.Variant.Arrow: value and typed_value conflict" | | #)


-- A dictionary omission can be detected from storage presence alone. Do
-- not construct a large typed subtree only to reject its field name later.
nodePresent# :: NodePlan -> Int# -> (# String | Bool #)
nodePresent# (NodePlan validity fallback typed) index
  | not (C.isValidAt validity (I# index)) = (# "Parquet.Variant.Arrow: null shredded wrapper or required child" | #)
  | otherwise = case typed of
      Just plan | C.isValidAt (case plan of PrimitivePlan v _ -> v; ObjectPlan v _ -> v; ArrayPlan v _ _ -> v) (I# index) -> (# | True #)
      _ -> case bytesAt# fallback index of
        (# _ | #) -> (# | True #)
        (# | () #) -> (# | False #)
{-# INLINE nodePresent# #-}


readObject# :: B.VariantLimits -> B.VariantMetadata -> V.Vector ObjectField -> Int# -> Int# -> Int# -> Int# -> M.Map Text B.Variant -> PresentResult#
readObject# limits dictionary fields depth budget index fieldIndex values
  | isTrue# (fieldIndex ==# count) = (# | (# B.VObject values, budget #) #)
  | otherwise = case V.unsafeIndex fields (I# fieldIndex) of
      field
        | not (B.metadataContains (objectName field) dictionary) -> case nodePresent# (objectPlan field) index of
            (# message | #) -> (# message | #)
            (# | True #) -> (# "Parquet.Variant.Arrow: typed object field missing from shared dictionary" | #)
            (# | False #) -> readObject# limits dictionary fields depth budget index (fieldIndex +# 1#) values
        | otherwise -> case readNode# limits dictionary (objectPlan field) depth budget index of
            (# message | | #) -> (# message | #)
            (# | | left #) -> readObject# limits dictionary fields depth left index (fieldIndex +# 1#) values
            (# | (# value, left #) | #) -> readObject# limits dictionary fields depth left index (fieldIndex +# 1#) (M.insert (objectName field) value values)
  where
    !(I# count) = V.length fields


readArray# :: B.VariantLimits -> B.VariantMetadata -> NodePlan -> Int# -> Int# -> Int# -> Int# -> PresentResult#
readArray# limits dictionary plan depth budget start count
  | I# count > I# budget = (# "Parquet.Variant.Arrow: value count limit exceeded" | #)
  | I# count > 0 && I# depth > B.variantMaxDepth limits = (# "Parquet.Variant.Arrow: nesting depth limit exceeded" | #)
  | otherwise = runRW# $ \state -> case MV.unsafeNew (I# count) of
      ST allocate -> case allocate state of
        (# state1, output #) ->
          let loop offset left state2
                | isTrue# (offset ==# count) = case V.unsafeFreeze output of
                    ST freeze -> case freeze state2 of (# _, array #) -> (# | (# B.VArray array, left #) #)
                | otherwise = case required# limits dictionary plan depth left (start +# offset) of
                    (# message | #) -> (# message | #)
                    (# | (# !value, remaining #) #) -> case MV.unsafeWrite output (I# offset) value of
                      ST write -> case write state2 of (# state3, () #) -> loop (offset +# 1#) remaining state3
          in loop 0# budget state1
