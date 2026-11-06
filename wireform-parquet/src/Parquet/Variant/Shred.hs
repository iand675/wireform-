{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Shred semantic Variant values to a declared canonical typed shape.
Exact Variant constructors and decimal scales select typed storage; a
mismatch stays in binary fallback with its original tag and coefficient.
-}
module Parquet.Variant.Shred (
  VariantShredder,
  compileVariantShredder,
  compileVariantShredderWithLimits,
  variantShredSchema,
  shredVariant,
  shredVariantWithMetadata,
) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int32, Int64)
import Data.Map.Internal qualified as MI
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (byteSwap64)
import Foreign.Storable (pokeByteOff)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#))
import Parquet.Nested.Generate qualified as Generate
import Parquet.Nested.Types
import Parquet.Variant qualified as B
import Parquet.Variant.Schema qualified as Schema


data VariantShredder = VariantShredder !VariantStorageSchema !NodePlan


data NodePlan = NodePlan
  { nodeFields :: {-# UNPACK #-} !Int
  , nodeMetadata :: {-# UNPACK #-} !Int
  , nodeValue :: {-# UNPACK #-} !Int
  , nodeTyped :: {-# UNPACK #-} !Int
  , nodePlan :: !(Maybe TypedPlan)
  }


data TypedPlan
  = PrimitivePlan !(B.Variant -> (# LeafValue | () #))
  | ObjectPlan !(V.Vector ChildPlan) !(Set.Set Text)
  | ArrayPlan !NodePlan


data ChildPlan = ChildPlan !Text !NodePlan


compileVariantShredder :: A.Field -> Either String VariantShredder
compileVariantShredder = compileVariantShredderWithLimits B.defaultVariantLimits Schema.defaultVariantSchemaLimits


compileVariantShredderWithLimits :: B.VariantLimits -> Schema.VariantSchemaLimits -> A.Field -> Either String VariantShredder
compileVariantShredderWithLimits limits schemaLimits field = do
  Schema.validateVariantFieldWithLimits schemaLimits field
  let complete = completeNode field
  storage <- Schema.compileVariantStorageSchemaWithLimits limits schemaLimits complete
  plan <- compileNode complete
  pure (VariantShredder storage plan)
  where
    completeNode current =
      let fields = A.fieldChildren current
          normalized =
            V.map
              ( \child -> case A.fieldName child of
                  "metadata" -> child {A.fieldType = A.ABinary, A.fieldDictionary = Nothing, A.fieldChildren = V.empty}
                  "value" -> child {A.fieldType = A.ABinary, A.fieldNullable = True}
                  "typed_value" -> completeTyped child
                  _ -> child
              )
              fields
          full = if V.any ((== "value") . A.fieldName) fields then normalized else V.snoc normalized (A.defaultLeafField "value" True A.ABinary)
      in current {A.fieldChildren = full}
    completeTyped current =
      current
        { A.fieldNullable = True
        , A.fieldChildren = case A.fieldType current of
            A.AStruct -> completeNode <$> A.fieldChildren current
            typ | typ `elem` [A.AList, A.ALargeList, A.AListView] -> completeNode <$> A.fieldChildren current
            _ -> A.fieldChildren current
        }


variantShredSchema :: VariantShredder -> NestedSchema
variantShredSchema (VariantShredder storage _) =
  (if A.fieldNullable (storageField storage) then NSOptional else NSRequired) (NSVariantTyped storage)


shredVariant :: VariantShredder -> Maybe B.Variant -> Either String NestedRow
shredVariant _ Nothing = Right NRNull
shredVariant shredder@(VariantShredder storage _) (Just value) = do
  metadata <- B.buildVariantMetadataForWithLimits (storageLimits storage) value
  shredVariantWithMetadata shredder metadata (Just value)


{- | Use one original dictionary for every fallback fragment and all fully
shredded names. The complete semantic tree is checked against one budget
before choosing any typed or binary branch. No scale conversion is done.
-}
shredVariantWithMetadata :: VariantShredder -> B.VariantMetadata -> Maybe B.Variant -> Either String NestedRow
shredVariantWithMetadata _ _ Nothing = Right NRNull
shredVariantWithMetadata (VariantShredder storage plan) metadata (Just value) = do
  B.validateVariantWithMetadataWithLimits (storageLimits storage) metadata value
  case nodeRow# (storageLimits storage) metadata plan (# value | #) of
    (# message | #) -> Left message
    (# | row #) -> Right row


compileNode :: A.Field -> Either String NodePlan
compileNode field = do
  let fields = A.fieldChildren field
      index name = maybe (-1) id (V.findIndex ((== name) . A.fieldName) fields)
      typed = index "typed_value"
  plan <- case fields V.!? typed of
    Nothing -> pure Nothing
    Just child -> Just <$> compileTyped child
  pure (NodePlan (V.length fields) (index "metadata") (index "value") typed plan)


compileTyped :: A.Field -> Either String TypedPlan
compileTyped field = case A.fieldType field of
  A.AStruct -> do
    children <- V.mapM (\child -> ChildPlan (A.fieldName child) <$> compileNode child) (A.fieldChildren field)
    pure (ObjectPlan children (Set.fromList (V.toList (A.fieldName <$> A.fieldChildren field))))
  typ | typ `elem` [A.AList, A.ALargeList, A.AListView] -> ArrayPlan <$> compileNode (V.head (A.fieldChildren field))
  _ -> PrimitivePlan <$> primitivePlan field


type RowResult# = (# String | NestedRow #)


nodeRow# :: B.VariantLimits -> B.VariantMetadata -> NodePlan -> (# B.Variant | () #) -> RowResult#
nodeRow# limits metadata plan value = case value of
  (# | () #) -> finish NRNull NRNull
  (# present | #) -> case (nodePlan plan, present) of
    (Just (PrimitivePlan select), _) -> case select present of
      (# leaf | #) -> finish NRNull (NRLeaf leaf)
      (# | () #) -> fallback present
    (Just (ArrayPlan element), B.VArray values) -> case V.length values of
      I# count -> case Generate.generate# count (\index -> nodeRow# limits metadata element (# V.unsafeIndex values (I# index) | #)) of
        (# message | #) -> (# message | #)
        (# | rows #) -> finish NRNull (NRList rows)
    (Just (ObjectPlan children names), B.VObject fields) -> case V.length children of
      I# count -> case Generate.generate#
        count
        ( \index -> case V.unsafeIndex children (I# index) of
            ChildPlan name child -> nodeRow# limits metadata child (lookupValue# name fields)
        ) of
        (# message | #) -> (# message | #)
        (# | rows #) ->
          let remainder = M.withoutKeys fields names
          in if M.null remainder
               then finish NRNull (NRStruct rows)
               else case B.encodeVariantValueWithLimits limits metadata (B.VObject remainder) of
                 Left message -> (# message | #)
                 Right bytes -> finish (NRLeaf (LvBinary bytes)) (NRStruct rows)
    _ -> fallback present
  where
    finish binary typed = case nodeFields plan of
      I# count -> case Generate.generate#
        count
        ( \index ->
            (#
              | if I# index == nodeMetadata plan
                then NRLeaf (LvBinary (B.metadataBytes metadata))
                else
                  if I# index == nodeValue plan
                    then binary
                    else
                      if I# index == nodeTyped plan
                        then typed
                        else NRNull
            #)
        ) of
        (# message | #) -> (# message | #)
        (# | rows #) -> (# | NRStruct rows #)
    {-# INLINE finish #-}
    fallback present = case B.encodeVariantValueWithLimits limits metadata present of
      Left message -> (# message | #)
      Right bytes -> finish (NRLeaf (LvBinary bytes)) NRNull
    {-# INLINE fallback #-}


-- Data.Map.lookup's boxed Maybe would allocate per shredded field. Keep the
-- same ordered-tree search while returning an unboxed finite result.
lookupValue# :: Text -> M.Map Text B.Variant -> (# B.Variant | () #)
lookupValue# key current = case current of
  MI.Tip -> (# | () #)
  MI.Bin _ name value left right -> case compare key name of
    LT -> lookupValue# key left
    GT -> lookupValue# key right
    EQ -> (# value | #)


primitivePlan :: A.Field -> Either String (B.Variant -> (# LeafValue | () #))
primitivePlan field = do
  _ <- Schema.variantTypedLeaf field
  pure $ case A.fieldType field of
    A.ABool -> \value -> case value of B.VBool x -> (# LvBool x | #); _ -> no# ()
    A.AInt 8 True -> \value -> case value of B.VInt8 x -> (# LvInt32 (fromIntegral x) | #); _ -> no# ()
    A.AInt 16 True -> \value -> case value of B.VInt16 x -> (# LvInt32 (fromIntegral x) | #); _ -> no# ()
    A.AInt 32 True -> \value -> case value of B.VInt32 x -> (# LvInt32 x | #); _ -> no# ()
    A.AInt 64 True -> \value -> case value of B.VInt64 x -> (# LvInt64 x | #); _ -> no# ()
    A.AFloatingPoint A.Single -> \value -> case value of B.VFloat x -> (# LvFloat x | #); _ -> no# ()
    A.AFloatingPoint A.DoublePrecision -> \value -> case value of B.VDouble x -> (# LvDouble x | #); _ -> no# ()
    A.ADecimal32 precision scale ->
      let !bound = fromInteger (10 ^ precision - 1) :: Int32
      in \value -> case value of B.VDecimal4 s x | fromIntegral s == scale && x >= negate bound && x <= bound -> (# LvInt32 x | #); _ -> no# ()
    A.ADecimal64 precision scale ->
      let !bound = fromInteger (10 ^ precision - 1) :: Int64
      in \value -> case value of B.VDecimal8 s x | fromIntegral s == scale && x >= negate bound && x <= bound -> (# LvInt64 x | #); _ -> no# ()
    A.ADecimal precision scale ->
      let !bound = 10 ^ precision - 1
      in \value -> case value of B.VDecimal16 s x | fromIntegral s == scale && x >= negate bound && x <= bound -> (# LvFixedBinary (decimalBytes (C.decimal128FromInteger x)) | #); _ -> no# ()
    A.ADate A.DateDay -> \value -> case value of B.VDate x -> (# LvInt32 x | #); _ -> no# ()
    A.ATime A.Microsecond 64 -> \value -> case value of B.VTime x -> (# LvInt64 x | #); _ -> no# ()
    A.ATimestamp A.Microsecond Nothing -> \value -> case value of B.VTimestampNtz x -> (# LvInt64 x | #); _ -> no# ()
    A.ATimestamp A.Microsecond (Just "UTC") -> \value -> case value of B.VTimestamp x -> (# LvInt64 x | #); _ -> no# ()
    A.ATimestamp A.Nanosecond Nothing -> \value -> case value of B.VTimestampNtzNanos x -> (# LvInt64 x | #); _ -> no# ()
    A.ATimestamp A.Nanosecond (Just "UTC") -> \value -> case value of B.VTimestampNanos x -> (# LvInt64 x | #); _ -> no# ()
    A.AFixedSizeBinary 16 -> \value -> case value of B.VUuid x -> (# LvFixedBinary x | #); _ -> no# ()
    typ | typ `elem` [A.ABinary, A.ALargeBinary, A.ABinaryView] -> \value -> case value of B.VBinary x -> (# LvBinary x | #); _ -> no# ()
    typ | typ `elem` [A.AUtf8, A.ALargeUtf8, A.AUtf8View] -> \value -> case value of B.VString x -> (# LvString x | #); _ -> no# ()
    _ -> \_ -> no# ()


no# :: () -> (# LeafValue | () #)
no# () = (# | () #)
{-# INLINE no# #-}


decimalBytes :: C.Decimal128 -> BS.ByteString
decimalBytes (C.Decimal128 lo hi) = BSI.unsafeCreate 16 $ \pointer -> do
  let big word = case targetByteOrder of LittleEndian -> byteSwap64 word; BigEndian -> word
  pokeByteOff pointer 0 (big hi)
  pokeByteOff pointer 8 (big lo)
