{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Internal semantic validation after every native leaf's structural and
byte-count preflight. Typed primitives were already range-checked there;
this pass checks shared dictionaries, conflicts and one global tree budget.
-}
module Parquet.Variant.Nested (normalizeStorageRow, validateStorageRow) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Vector qualified as V
import GHC.Exts (Int (I#), Int#, isTrue#, (+#), (==#))
import Parquet.Nested.Types
import Parquet.Variant qualified as B


-- Native array elements must actually contain a Variant value. The Arrow
-- convention permits both slots to be missing and reads that as Variant null;
-- supply binary 00 in the native fallback without losing any array elements.
-- The same normalization applies to a missing required root value. Metadata
-- and all supplied fragments are retained exactly, never re-encoded.
normalizeStorageRow :: VariantStorageSchema -> NestedRow -> NestedRow
normalizeStorageRow schema = node True (storageNode schema)
  where
    node required plan row = case row of
      NRStruct fields
        | V.length fields == storageFieldCount plan ->
            let fallback = slot (storageValueIndex plan) fields
                typed = slot (storageTypedIndex plan) fields
                value = if required && missing fallback && missing typed then NRLeaf (LvBinary (BS.singleton 0)) else fallback
                typedValue = maybe typed (`typedRow` typed) (storageTyped plan)
                output = V.imap (\index original -> if index == storageValueIndex plan then value else if index == storageTypedIndex plan then typedValue else original) fields
            in if not (needs required plan row) then row else NRStruct (if storageValueIndex plan < 0 then V.snoc output value else output)
      _ -> row
    needs required plan row = case row of
      NRStruct fields
        | V.length fields == storageFieldCount plan ->
            storageValueIndex plan < 0
              || (required && missing (slot (storageValueIndex plan) fields) && missing (slot (storageTypedIndex plan) fields))
              || (storageNormalizationBelow plan && maybe False (\kind -> typedNeeds kind (slot (storageTypedIndex plan) fields)) (storageTyped plan))
      _ -> False
    typedNeeds kind row = case (kind, row) of
      (StorageObject children _, NRStruct fields) | V.length fields == V.length children -> V.any (\child -> needs False (storageChildNode child) (V.unsafeIndex fields (storageChildIndex child))) children
      (StorageArray child, NRList values) -> V.any (needs True child) values
      _ -> False
    typedRow kind row = case (kind, row) of
      (StorageObject children _, NRStruct fields)
        | V.length fields == V.length children ->
            NRStruct (V.zipWith (\child value -> node False (storageChildNode child) value) children fields)
      (StorageArray child, NRList values) -> NRList (V.map (node True child) values)
      _ -> row


slot :: Int -> V.Vector NestedRow -> NestedRow
slot index fields = maybe NRNull id (fields V.!? index)
{-# INLINE slot #-}


missing :: NestedRow -> Bool
missing NRNull = True
missing _ = False
{-# INLINE missing #-}


type Checked# = (# String | Int# #)


type NodeChecked# = (# String | Int# | () #)


validateStorageRow :: VariantStorageSchema -> NestedRow -> Either String ()
validateStorageRow _ NRNull = Right ()
validateStorageRow schema row = case row of
  NRStruct fields | V.length fields == storageFieldCount (storageNode schema) -> case slot (storageMetadataIndex schema) fields of
    NRLeaf (LvBinary metadata) -> case B.compileVariantMetadataWithLimits limits metadata of
      Left message -> Left message
      Right dictionary -> case B.variantMaxValues limits of
        I# budget -> case required# limits dictionary (storageNode schema) 0# budget row of
          (# message | #) -> Left message
          (# | _ #) -> Right ()
    _ -> Left "Parquet.Variant.Nested: present root requires binary metadata"
  _ -> Left "Parquet.Variant.Nested: root storage shape mismatch"
  where
    limits = storageLimits schema


charge# :: B.VariantLimits -> Int# -> Int# -> Checked#
charge# limits depth budget
  | I# depth > B.variantMaxDepth limits = (# "Parquet.Variant.Nested: nesting depth limit exceeded" | #)
  | I# budget <= 0 = (# "Parquet.Variant.Nested: value count limit exceeded" | #)
  | otherwise = (# | budget +# (-1#) #)
{-# INLINE charge# #-}


required# :: B.VariantLimits -> B.VariantMetadata -> VariantStorageNode -> Int# -> Int# -> NestedRow -> Checked#
required# limits dictionary plan depth budget row = case checkNode# limits dictionary plan depth budget row of
  (# message | | #) -> (# message | #)
  (# | left | #) -> (# | left #)
  (# | | () #) -> charge# limits depth budget
{-# INLINE required# #-}


presence :: VariantStorageNode -> NestedRow -> Either String Bool
presence plan (NRStruct fields)
  | V.length fields /= storageFieldCount plan = Left "Parquet.Variant.Nested: wrapper field count mismatch"
  | storageValueRequired plan && missing value = Left "Parquet.Variant.Nested: null at required value field"
  | storageTypedRequired plan && missing typed = Left "Parquet.Variant.Nested: null at required typed field"
  | otherwise = Right (not (missing value && missing typed))
  where
    value = slot (storageValueIndex plan) fields
    typed = slot (storageTypedIndex plan) fields
presence _ _ = Left "Parquet.Variant.Nested: shredded wrapper must be a required struct"
{-# INLINE presence #-}


checkNode# :: B.VariantLimits -> B.VariantMetadata -> VariantStorageNode -> Int# -> Int# -> NestedRow -> NodeChecked#
checkNode# limits dictionary plan depth budget row = case presence plan row of
  Left message -> (# message | | #)
  Right False -> (# | | () #)
  Right True
    | NRStruct fields <- row ->
        let fallback = slot (storageValueIndex plan) fields
            typed = slot (storageTypedIndex plan) fields
        in case (storageTyped plan, typed) of
             (Just (StorageObject children names), NRStruct values) ->
               case fallback of
                 NRNull -> case charge# limits depth budget of
                   (# message | #) -> (# message | | #)
                   (# | left #) -> object children values left
                 NRLeaf (LvBinary bytes) -> case B.decodeVariantFragment# limits dictionary depth budget bytes of
                   (# message | #) -> (# message | | #)
                   (# | (# B.VObject rest, left #) #)
                     | M.foldrWithKey (\name _ found -> Set.member name names || found) False rest -> (# "Parquet.Variant.Nested: object field appears in typed and fallback storage" | | #)
                     | otherwise -> object children values left
                   _ -> (# "Parquet.Variant.Nested: partial fallback must be an object" | | #)
                 _ -> badShapeResult# ()
             (Just (StorageArray child), NRList values) -> case fallback of
               NRNull -> case charge# limits depth budget of
                 (# message | #) -> (# message | | #)
                 (# | left #) -> case array# limits dictionary child (depth +# 1#) left values 0# of
                   (# message | #) -> (# message | | #)
                   (# | remaining #) -> (# | remaining | #)
               _ -> conflictResult# ()
             (Just StoragePrimitive, NRLeaf _) -> case fallback of
               NRNull -> case charge# limits depth budget of
                 (# message | #) -> (# message | | #)
                 (# | left #) -> (# | left | #)
               _ -> conflictResult# ()
             (_, NRNull) -> case fallback of
               NRLeaf (LvBinary bytes) -> case B.decodeVariantFragment# limits dictionary depth budget bytes of
                 (# message | #) -> (# message | | #)
                 (# | (# value, remaining #) #) -> case (storageTyped plan, value) of
                   (Just StorageObject {}, B.VObject {}) -> (# "Parquet.Variant.Nested: object is missing its typed storage" | | #)
                   (Just StorageArray {}, B.VArray {}) -> (# "Parquet.Variant.Nested: array is missing its typed storage" | | #)
                   _ -> (# | remaining | #)
               _ -> badShapeResult# ()
             _ -> badShapeResult# ()
    where
      object children values left
        | V.length values /= V.length children = badShapeResult# ()
        | otherwise = case object# limits dictionary children (depth +# 1#) left values 0# of
            (# message | #) -> (# message | | #)
            (# | remaining #) -> (# | remaining | #)
  _ -> badShapeResult# ()


badShapeResult# :: () -> NodeChecked#
badShapeResult# () = (# "Parquet.Variant.Nested: typed storage shape mismatch" | | #)


conflictResult# :: () -> NodeChecked#
conflictResult# () = (# "Parquet.Variant.Nested: value and typed_value conflict" | | #)


object# :: B.VariantLimits -> B.VariantMetadata -> V.Vector VariantStorageChild -> Int# -> Int# -> V.Vector NestedRow -> Int# -> Checked#
object# limits dictionary children depth budget values index
  | isTrue# (index ==# count) = (# | budget #)
  | otherwise =
      let child = V.unsafeIndex children (I# index)
          row = V.unsafeIndex values (storageChildIndex child)
      in case presence (storageChildNode child) row of
           Left message -> (# message | #)
           Right False -> object# limits dictionary children depth budget values (index +# 1#)
           Right True | not (B.metadataContains (storageChildName child) dictionary) -> (# "Parquet.Variant.Nested: typed field name is missing from the shared dictionary" | #)
           _ -> case checkNode# limits dictionary (storageChildNode child) depth budget row of
             (# message | | #) -> (# message | #)
             (# | left | #) -> object# limits dictionary children depth left values (index +# 1#)
             (# | | () #) -> object# limits dictionary children depth budget values (index +# 1#)
  where
    I# count = V.length children


array# :: B.VariantLimits -> B.VariantMetadata -> VariantStorageNode -> Int# -> Int# -> V.Vector NestedRow -> Int# -> Checked#
array# limits dictionary child depth budget values index
  | isTrue# (index ==# count) = (# | budget #)
  | otherwise = case required# limits dictionary child depth budget (V.unsafeIndex values (I# index)) of
      (# message | #) -> (# message | #)
      (# | left #) -> array# limits dictionary child depth left values (index +# 1#)
  where
    I# count = V.length values
