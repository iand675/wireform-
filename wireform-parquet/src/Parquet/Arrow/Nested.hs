{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

{- | Internal schema planning and columnar Dremel assembly. Physical leaf
ordinals remain distinct even when several structs use the same names.
-}
module Parquet.Arrow.Nested (
  Plan (..),
  PlanKind (..),
  LeafColumn (..),
  plans,
  assemble,
  assembleWith,
  assembleWithRows,
  unbackedValues,
) where

import Arrow.Column qualified as C
import Arrow.Column.Buffer qualified as Buffer
import Arrow.Types qualified as A
import Control.Monad.ST (runST)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict qualified as Names
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as P
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as M
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import Parquet.Arrow.File qualified as FileArrow
import Parquet.File qualified as File
import Parquet.Types qualified as T
import Parquet.Variant.Schema qualified as VariantSchema


data Physical = Physical
  { physicalElement :: !T.SchemaElement
  , physicalChildren :: !(V.Vector Physical)
  , physicalLeaf :: {-# UNPACK #-} !Int
  , physicalDef :: {-# UNPACK #-} !Int
  , physicalRep :: {-# UNPACK #-} !Int
  }


data Parsed = Parsed
  { parsedNodes :: ![Physical]
  , parsedNext :: {-# UNPACK #-} !Int
  , parsedLeaf :: {-# UNPACK #-} !Int
  }


data Plan = Plan
  { planField :: !A.Field
  , planAnchor :: {-# UNPACK #-} !Int
  , planSlotDef :: {-# UNPACK #-} !Int
  , planSlotRep :: {-# UNPACK #-} !Int
  , planPresence :: {-# UNPACK #-} !Int
  , planKind :: !PlanKind
  }


data PlanKind = Primitive | Struct !(V.Vector Plan) | List !Plan | Map !Plan !Plan | EmptyFile


data LeafColumn = LeafColumn
  { leafReps :: !(P.Vector Int32)
  , leafDefs :: !(P.Vector Int32)
  , leafValues :: !C.ColumnArray
  }


plans :: (T.SchemaElement -> Either String A.Field) -> V.Vector T.SchemaElement -> Either String (V.Vector Plan)
plans leafField schema
  | V.null schema = Left "Parquet.Arrow: schema is missing its root"
  | T.seType root /= Nothing = Left "Parquet.Arrow: schema root must be a group"
  | T.seNumChildren root == Nothing = Left "Parquet.Arrow: schema root is missing num_children"
  | otherwise = do
      children <- parseChildren 0 0 0 1 0 (maybe 0 fromIntegral (T.seNumChildren root))
      if parsedNext children /= V.length schema
        then Left "Parquet.Arrow: schema has trailing nodes"
        else
          if not (unique (V.fromList (parsedNodes children)))
            then Left "Parquet.Arrow: duplicate sibling field names"
            else V.mapM (node 0 0) (V.fromList (parsedNodes children))
  where
    root = V.head schema
    parseChildren :: Int -> Int -> Int -> Int -> Int -> Int -> Either String Parsed
    parseChildren depth def rep index ordinal count
      | count < 0 || count > V.length schema - index = Left "Parquet.Arrow: invalid schema child count"
      | count == 0 = Right (Parsed [] index ordinal)
      | depth > 128 = Left "Parquet.Arrow: schema nesting exceeds 128 levels"
      | otherwise = do
          element <- maybe (Left "Parquet.Arrow: truncated schema") Right (schema V.!? index)
          case T.seRepetition element of
            Nothing -> Left "Parquet.Arrow: non-root schema node is missing repetition_type"
            Just _ -> Right ()
          case (T.seType element, T.seNumChildren element) of
            (Just _, Just _) -> Left "Parquet.Arrow: primitive schema node has num_children"
            (Nothing, Nothing) -> Left "Parquet.Arrow: group schema node is missing num_children"
            _ -> Right ()
          let ownDef = def + if T.seRepetition element `elem` [Just T.Optional, Just T.Repeated] then 1 else 0
              ownRep = rep + if T.seRepetition element == Just T.Repeated then 1 else 0
              isLeaf = T.seType element /= Nothing
              nChildren = maybe 0 fromIntegral (T.seNumChildren element)
          nested <- parseChildren (depth + 1) ownDef ownRep (index + 1) ordinal nChildren
          let nextLeaf = if isLeaf then ordinal + 1 else parsedLeaf nested
              physical = Physical element (V.fromList (parsedNodes nested)) ordinal ownDef ownRep
          siblings <- parseChildren depth def rep (parsedNext nested) nextLeaf (count - 1)
          Right siblings {parsedNodes = physical : parsedNodes siblings}
    node slotDef slotRep physical
      | T.seRepetition (physicalElement physical) == Just T.Repeated = do
          child <- content (physicalDef physical) (physicalRep physical) physical
          let field =
                (planField child)
                  { A.fieldNullable = False
                  , A.fieldType = A.AList
                  , A.fieldChildren = V.singleton ((planField child) {A.fieldNullable = False})
                  , A.fieldMetadata = FileArrow.fieldIdMetadata (T.seFieldId (physicalElement physical))
                  }
          Right
            ( Plan
                field
                (planAnchor child)
                slotDef
                slotRep
                (physicalDef physical - 1)
                (List child)
            )
      | otherwise = content slotDef slotRep physical
    content slotDef slotRep physical
      | not (unique children) = Left "Parquet.Arrow: duplicate sibling field names"
      | T.seLogicalType element == Just (T.LTVariant 1) && T.seType element /= Nothing = Left "Parquet.Arrow: VARIANT(1) requires group storage"
      | T.seType element /= Nothing = do
          fld <- leafField element
          Right (base fld Primitive)
      | T.seLogicalType element == Just (T.LTVariant 1) = do
          require (T.seConvertedType element == Nothing) "Variant root has a conflicting converted annotation"
          result <- variantNode True slotDef slotRep physical
          let annotated = (planField result) {A.fieldMetadata = VariantSchema.variantExtensionMetadata V.++ FileArrow.fieldIdMetadata (T.seFieldId element)}
          VariantSchema.validateVariantFieldWithLimits (VariantSchema.VariantSchemaLimits 128 (V.length schema)) annotated
          Right result {planField = annotated}
      | isList element = case V.toList children of
          [repeated] | T.seRepetition (physicalElement repeated) == Just T.Repeated -> do
            child <- listElement element repeated
            Right (base (field A.AList (V.singleton (planField child))) (List child))
          _ -> Left "Parquet.Arrow: LIST must contain one repeated field"
      | isMap element = case V.toList children of
          [entries] | T.seRepetition (physicalElement entries) == Just T.Repeated ->
            case V.toList (physicalChildren entries) of
              [key] -> do
                child <- node (physicalDef entries) (physicalRep entries) key
                Right (base (field A.AList (V.singleton (planField child))) (List child))
              [key, value] -> do
                keyPlan <- node (physicalDef entries) (physicalRep entries) key
                let kp = case planKind keyPlan of
                      EmptyFile
                        | T.seRepetition (physicalElement key) == Just T.Required ->
                            keyPlan {planField = (planField keyPlan) {A.fieldNullable = False}}
                      _ -> keyPlan
                vp <- node (physicalDef entries) (physicalRep entries) value
                if A.fieldNullable (planField kp)
                  then Left "Parquet.Arrow: MAP keys cannot be optional"
                  else do
                    let entryField =
                          -- The repeated Parquet group is a storage
                          -- wrapper; Arrow's logical map child is entries.
                          A.Field
                            "entries"
                            False
                            A.AStruct
                            (V.fromList [planField kp, planField vp])
                            Nothing
                            V.empty
                    Right (base (field (A.AMap False) (V.singleton entryField)) (Map kp vp))
              _ -> Left "Parquet.Arrow: MAP must contain key and optional value fields"
          _ -> Left "Parquet.Arrow: MAP must contain one repeated group"
      | V.null children && T.seLogicalType element == Just T.LTFile = do
          _ <- File.parseFileSchema element V.empty
          let empty =
                (field A.AStruct V.empty)
                  { A.fieldNullable = True
                  , A.fieldMetadata = FileArrow.fileMetadata V.++ FileArrow.fieldIdMetadata (T.seFieldId element)
                  }
          Right ((base empty EmptyFile) {planAnchor = -1})
      | V.null children = Left "Parquet.Arrow: an empty group has no physical leaf"
      | otherwise = do
          case T.seLogicalType element of
            Just T.LTFile -> () <$ File.parseFileSchema element (physicalElement <$> children)
            _ -> Right ()
          fields <- V.mapM (node slotDef slotRep) children
          let withId physicalChild childPlan =
                let childField = planField childPlan
                in childPlan {planField = childField {A.fieldMetadata = A.fieldMetadata childField V.++ FileArrow.fieldIdMetadata (T.seFieldId (physicalElement physicalChild))}}
              preserved = if T.seLogicalType element == Just T.LTFile then V.zipWith withId children fields else fields
              structField = field A.AStruct (planField <$> preserved)
              annotated =
                if T.seLogicalType element == Just T.LTFile
                  then structField {A.fieldMetadata = FileArrow.fileMetadata V.++ FileArrow.fieldIdMetadata (T.seFieldId element)}
                  else structField
          Right (base annotated (Struct preserved))
      where
        element = physicalElement physical
        children = physicalChildren physical
        field ty kids =
          A.Field
            (T.seName element)
            (T.seRepetition element == Just T.Optional)
            ty
            kids
            Nothing
            V.empty
        base fld kind = Plan fld (if hasLeaf physical then physicalLeaf physical else -1) slotDef slotRep (physicalDef physical) kind
    -- Known Variant layouts use their own exact schema rules. In particular,
    -- the generic LIST legacy-name heuristics must not change element shape.
    variantNode root slotDef slotRep physical = do
      let element = physicalElement physical
          children = physicalChildren physical
          names = T.seName . physicalElement <$> children
          allowed = if root then ["metadata", "value", "typed_value"] else ["value", "typed_value"]
          typed = V.elem "typed_value" names
      require (T.seType element == Nothing && unique children) "Variant node must be a group with unique fields"
      require (root || (plain element && T.seRepetition element == Just T.Required)) "Variant wrapper must be a required unannotated group"
      require (V.length names <= length allowed && V.all (`elem` allowed) names) "unexpected Variant node field"
      require ((not root || V.elem "metadata" names) && (typed || V.elem "value" names)) "Variant node is missing metadata or value storage"
      fields <-
        V.mapM
          ( \child -> case T.seName (physicalElement child) of
              "metadata" -> variantBinary T.Required slotDef slotRep child
              "value" -> variantBinary (if root && not typed then T.Required else T.Optional) slotDef slotRep child
              "typed_value" -> variantTyped slotDef slotRep child
              _ -> Left "Parquet.Arrow: unexpected Variant field"
          )
          children
      Right (variantBase slotDef slotRep physical A.AStruct fields (Struct fields))
    variantBinary repetition slotDef slotRep physical = do
      let element = physicalElement physical
      require (T.seType element == Just T.PTByteArray && plain element && T.seRepetition element == Just repetition) "Variant binary storage has an invalid type, annotation or repetition"
      result <- node slotDef slotRep physical
      let field = planField result
      Right result {planField = field {A.fieldMetadata = A.fieldMetadata field V.++ FileArrow.fieldIdMetadata (T.seFieldId element)}}
    variantTyped slotDef slotRep physical = do
      let element = physicalElement physical
          children = physicalChildren physical
      require (T.seRepetition element == Just T.Optional) "Variant typed_value must be optional"
      if T.seType element /= Nothing
        then do
          typ <- variantPrimitive element
          result <- node slotDef slotRep physical
          let field = planField result
              metadata = if T.seLogicalType element == Just T.LTUUID then V.fromList [("ARROW:extension:name", "arrow.uuid"), ("ARROW:extension:metadata", "")] else V.empty
          Right result {planField = field {A.fieldType = typ, A.fieldMetadata = metadata V.++ FileArrow.fieldIdMetadata (T.seFieldId element)}}
        else
          if isList element
            then case V.toList children of
              [repeated]
                | let wrapper = physicalElement repeated
                , T.seType wrapper == Nothing && plain wrapper && T.seRepetition wrapper == Just T.Repeated
                , [item] <- V.toList (physicalChildren repeated) -> do
                    require (logical element == Just T.LTList) "Variant list has a conflicting annotation"
                    child <- variantNode False (physicalDef repeated) (physicalRep repeated) item
                    Right (variantBase slotDef slotRep physical A.AList (V.singleton child) (List child))
              _ -> Left "Parquet.Arrow: Variant arrays require an exact three-level LIST with a required element group"
            else do
              require (plain element && unique children) "Variant object must be an unannotated group with unique fields"
              fields <- V.mapM (variantNode False slotDef slotRep) children
              Right (variantBase slotDef slotRep physical A.AStruct fields (Struct fields))
    variantBase slotDef slotRep physical typ fields kind =
      let element = physicalElement physical
          field = (A.defaultField (T.seName element) (T.seRepetition element == Just T.Optional) typ (planField <$> fields)) {A.fieldMetadata = FileArrow.fieldIdMetadata (T.seFieldId element)}
      in Plan field (if hasLeaf physical then physicalLeaf physical else -1) slotDef slotRep (physicalDef physical) kind
    plain element = T.seLogicalType element == Nothing && T.seConvertedType element == Nothing
    require good message = if good then Right () else Left ("Parquet.Arrow: " ++ message)
    logical element = case T.seLogicalType element of
      Just annotation -> Just annotation
      Nothing -> case T.seConvertedType element of
        Nothing -> Nothing
        Just T.CTUtf8 -> Just T.LTString
        Just T.CTList -> Just T.LTList
        Just T.CTDate -> Just T.LTDate
        Just T.CTInt8 -> Just (T.LTInteger 8 True)
        Just T.CTInt16 -> Just (T.LTInteger 16 True)
        Just T.CTInt32 -> Just (T.LTInteger 32 True)
        Just T.CTInt64 -> Just (T.LTInteger 64 True)
        Just T.CTTimestampMicros -> Just (T.LTTimestamp True T.LtMicros)
        _ -> Just T.LTUnsupported
    variantPrimitive element = case (T.seType element, logical element) of
      (Just T.PTBoolean, Nothing) -> Right A.ABool
      (Just T.PTInt32, Just (T.LTInteger width True)) | width `elem` [8, 16, 32] -> Right (A.AInt (fromIntegral width) True)
      (Just T.PTInt32, Nothing) -> Right (A.AInt 32 True)
      (Just T.PTInt64, Nothing) -> Right (A.AInt 64 True)
      (Just T.PTInt64, Just (T.LTInteger 64 True)) -> Right (A.AInt 64 True)
      (Just T.PTFloat, Nothing) -> Right (A.AFloatingPoint A.Single)
      (Just T.PTDouble, Nothing) -> Right (A.AFloatingPoint A.DoublePrecision)
      (Just physical, Just (T.LTDecimal precision scale))
        | precision >= 1 && precision <= 38 && scale >= 0 && scale <= precision && T.hasCompatiblePhysicalType element ->
            let p = fromIntegral precision; s = fromIntegral scale
            in case physical of
                 T.PTInt32 | precision <= 9 -> Right (A.ADecimal32 p s)
                 T.PTInt64 | precision >= 10 && precision <= 18 -> Right (A.ADecimal64 p s)
                 _ | physical `elem` [T.PTByteArray, T.PTFixedLenByteArray] && precision >= 19 -> Right (A.ADecimal p s)
                 _ -> invalid
      (Just T.PTInt32, Just T.LTDate) -> Right (A.ADate A.DateDay)
      (Just T.PTInt64, Just (T.LTTime False T.LtMicros)) -> Right (A.ATime A.Microsecond 64)
      (Just T.PTInt64, Just (T.LTTimestamp utc unit))
        | unit `elem` [T.LtMicros, T.LtNanos] ->
            Right (A.ATimestamp (if unit == T.LtMicros then A.Microsecond else A.Nanosecond) (if utc then Just "UTC" else Nothing))
      (Just T.PTByteArray, Nothing) -> Right A.ABinary
      (Just T.PTByteArray, Just T.LTString) -> Right A.AUtf8
      (Just T.PTFixedLenByteArray, Just T.LTUUID) | T.seTypeLength element == Just 16 -> Right (A.AFixedSizeBinary 16)
      _ -> invalid
      where
        invalid = Left "Parquet.Arrow: primitive has no exact native Variant mapping"
    hasLeaf physical = T.seType (physicalElement physical) /= Nothing || V.any hasLeaf (physicalChildren physical)
    listElement parent repeated =
      let kids = physicalChildren repeated
          name = T.seName (physicalElement repeated)
          legacyElement =
            T.seType (physicalElement repeated) /= Nothing
              || V.length kids /= 1
              || name == "array"
              || name == T.seName parent <> "_tuple"
              || isList (physicalElement repeated)
              || isMap (physicalElement repeated)
      in if legacyElement
           then content (physicalDef repeated) (physicalRep repeated) repeated
           else node (physicalDef repeated) (physicalRep repeated) (V.head kids)
    isList e = T.seLogicalType e == Just T.LTList || T.seConvertedType e == Just T.CTList
    isMap e = T.seLogicalType e == Just T.LTMap || T.seConvertedType e == Just T.CTMap
    unique nodes =
      Names.size
        ( Names.fromList
            ( V.toList
                (V.map (\n -> (T.seName (physicalElement n), ())) nodes)
            )
        )
        == V.length nodes


{- | Assemble one logical root column from lazily shared physical leaves.
Every child is built column-wise; no row-major object tree is allocated.
-}
assemble :: V.Vector (Either String LeafColumn) -> Plan -> Either String C.ColumnArray
assemble = assembleWith (\_ -> Right)


{- | Transform a primitive after reconstruction. Container plans can be
narrowed/reordered before assembly, keeping unused physical leaves lazy.
-}
assembleWith
  :: (Plan -> C.ColumnArray -> Either String C.ColumnArray)
  -> V.Vector (Either String LeafColumn)
  -> Plan
  -> Either String C.ColumnArray
assembleWith = assembleIn Nothing


{- | Supply the row count for zero-leaf FILEs. Nested containers pass their
own slot count down; no neighboring column is mistaken for a FILE leaf.
-}
assembleWithRows :: Int -> (Plan -> C.ColumnArray -> Either String C.ColumnArray) -> V.Vector (Either String LeafColumn) -> Plan -> Either String C.ColumnArray
assembleWithRows rows = assembleIn (Just rows)


assembleIn :: Maybe Int -> (Plan -> C.ColumnArray -> Either String C.ColumnArray) -> V.Vector (Either String LeafColumn) -> Plan -> Either String C.ColumnArray
assembleIn count transform leaves plan = case planKind plan of
  EmptyFile -> case count of
    Just rows -> C.mkStruct rows (C.validityGenerate rows (const False)) V.empty
    Nothing -> Left "Parquet.Arrow: zero-leaf FILE needs its containing row count"
  Struct children | planAnchor plan < 0 -> case count of
    Just 0 -> C.emptyColumnFor (planField plan)
    Just rows | not (A.fieldNullable (planField plan)) -> do
      columns <- V.mapM (assembleIn (Just rows) transform leaves) children
      C.mkStruct rows Nothing (V.zip (A.fieldName . planField <$> children) columns)
    _ -> missing
  _ | planAnchor plan < 0 -> case count of
    Just 0 -> C.emptyColumnFor (planField plan)
    _ -> missing
  _ -> assembleAnchored transform leaves plan
  where
    missing = Left "Parquet.Arrow: container has no physical leaf to recover its shape"


assembleAnchored :: (Plan -> C.ColumnArray -> Either String C.ColumnArray) -> V.Vector (Either String LeafColumn) -> Plan -> Either String C.ColumnArray
assembleAnchored transformPrimitive leaves plan = do
  anchor <- maybe (Left "Parquet.Arrow: missing physical leaf") id (leaves V.!? planAnchor plan)
  let slots = slotIndices plan anchor
      n = P.length slots
      present i = fromIntegral (leafDefs anchor P.! (slots P.! i)) >= planPresence plan
      validity = if A.fieldNullable (planField plan) then C.validityGenerate n present else Nothing
      flat =
        planSlotDef plan == 0
          && planSlotRep plan == 0
          && P.length (leafReps anchor) == P.length (leafDefs anchor)
          && P.all (== 0) (leafReps anchor)
  result <- case planKind plan of
    Primitive
      | C.ColPrim primitive Nothing values <- leafValues anchor
      , flat ->
          flatPrimitive plan (leafDefs anchor) primitive values
    Primitive
      | C.ColBool Nothing values <- leafValues anchor
      , flat ->
          flatBool plan (leafDefs anchor) values
    Primitive
      | C.ColUtf8 Nothing offsets bytes <- leafValues anchor
      , flat ->
          flatVariable plan (leafDefs anchor) C.mkUtf8 offsets bytes
    Primitive
      | C.ColBinary Nothing offsets bytes <- leafValues anchor
      , flat ->
          flatVariable plan (leafDefs anchor) C.mkBinary offsets bytes
    Primitive
      | C.ColLargeUtf8 Nothing offsets bytes <- leafValues anchor
      , flat ->
          flatVariable plan (leafDefs anchor) C.mkLargeUtf8 offsets bytes
    Primitive
      | C.ColLargeBinary Nothing offsets bytes <- leafValues anchor
      , flat ->
          flatVariable plan (leafDefs anchor) C.mkLargeBinary offsets bytes
    Primitive -> do
      let dense = leafValues anchor
          ranks = P.scanl' (\rank index -> if fromIntegral (leafDefs anchor P.! index) == planPresence plan then rank + 1 else rank) 0 slots
          good i = fromIntegral (leafDefs anchor P.! (slots P.! i)) == planPresence plan
      if P.last ranks /= C.columnLength dense
        then Left "Parquet.Arrow: defined values and levels disagree"
        else case dense of
          C.ColUtf8 Nothing offsets bytes -> variableFromRanks C.mkUtf8 validity ranks offsets bytes
          C.ColBinary Nothing offsets bytes -> variableFromRanks C.mkBinary validity ranks offsets bytes
          C.ColLargeUtf8 Nothing offsets bytes -> variableFromRanks C.mkLargeUtf8 validity ranks offsets bytes
          C.ColLargeBinary Nothing offsets bytes -> variableFromRanks C.mkLargeBinary validity ranks offsets bytes
          _ -> do
            scattered <-
              if C.columnLength dense == 0
                then Right (C.fillerColumn n dense)
                else
                  if C.columnLength dense == n
                    then Right dense
                    else C.takeColumnArray (S.generate n (\i -> if good i then ranks P.! i else 0)) dense
            C.maskValidity validity scattered
    Struct children -> do
      V.mapM_ (\child -> if planAnchor child < 0 then Right () else sameShape leaves plan (planAnchor child)) children
      columns <- V.mapM (assembleIn (Just n) transformPrimitive leaves) children
      if V.any ((/= n) . C.columnLength) columns
        then Left "Parquet.Arrow: struct children have inconsistent row counts"
        else C.mkStruct n validity (V.zip (A.fieldName . planField <$> children) columns)
    List child ->
      if A.fieldType (planField plan) == A.ALargeList
        then do
          offsets <- listOffsets child anchor slots :: Either String (S.Vector Int64)
          values <- assembleIn (Just (fromIntegral (S.last offsets))) transformPrimitive leaves child
          if fromIntegral (S.last offsets) /= C.columnLength values
            then Left "Parquet.Arrow: list offsets and child count disagree"
            else C.mkLargeList validity offsets values
        else do
          offsets <- listOffsets child anchor slots :: Either String (S.Vector Int32)
          values <- assembleIn (Just (fromIntegral (S.last offsets))) transformPrimitive leaves child
          if fromIntegral (S.last offsets) /= C.columnLength values
            then Left "Parquet.Arrow: list offsets and child count disagree"
            else C.mkList validity offsets values
    Map key value -> do
      if planAnchor value < 0
        then Right ()
        else do
          sameShape leaves plan (planAnchor value)
          if planAnchor key < 0 then Right () else sameShape leaves (key {planPresence = planSlotDef key}) (planAnchor value)
      offsets <- listOffsets key anchor slots
      let entries = Just (fromIntegral (S.last offsets))
      keys <- assembleIn entries transformPrimitive leaves key
      values <- assembleIn entries transformPrimitive leaves value
      if C.columnLength keys /= C.columnLength values || fromIntegral (S.last offsets) /= C.columnLength keys
        then Left "Parquet.Arrow: map entries have inconsistent row counts"
        else C.mkMap validity offsets keys values
    EmptyFile -> Left "Parquet.Arrow: zero-leaf FILE cannot have a physical anchor"
  case planKind plan of
    Primitive -> transformPrimitive plan result
    _ -> Right result


{- | Count synthesized FILE values before allocating their null bitmaps. A
count-only level scan avoids constructing a second vector of slot indices.
-}
unbackedValues :: Int -> V.Vector (Either String LeafColumn) -> Plan -> Either String Int64
unbackedValues rows leaves plan
  | not (needsValues plan) = Right 0
  | otherwise = count (Just rows) plan
  where
    needsValues current = case planKind current of
      EmptyFile -> True
      Struct children -> V.any needsValues children
      List child -> needsValues child
      Map key value -> needsValues key || needsValues value
      Primitive -> False
    leaf current = maybe (Left "Parquet.Arrow: missing physical anchor") id (leaves V.!? planAnchor current)
    slots current column =
      P.ifoldl'
        ( \total index definition ->
            if fromIntegral (leafReps column P.! index) <= planSlotRep current && fromIntegral definition >= planSlotDef current
              then total + 1
              else total
        )
        0
        (leafDefs column)
    slotCount context current
      | planAnchor current >= 0 = slots current <$> leaf current
      | otherwise = maybe (Left "Parquet.Arrow: missing containing row count") Right context
    add left right = if left > maxBound - right then maxBound else left + right
    childrenTotal context = V.foldM (\total child -> add total <$> count context child) 0
    count context current
      | not (needsValues current) = Right 0
      | otherwise = case planKind current of
          EmptyFile -> fromIntegral <$> slotCount context current
          Struct children -> do
            size <- slotCount context current
            childrenTotal (Just size) children
          List child
            | planAnchor current < 0 -> case context of
                Just 0 -> Right 0
                _ -> missing
            | otherwise -> do
                column <- leaf current
                count (Just (slots child column)) child
          Map key value
            | planAnchor current < 0 -> case context of
                Just 0 -> Right 0
                _ -> missing
            | otherwise -> do
                column <- leaf current
                let entries = Just (slots key column)
                keyCount <- count entries key
                valueCount <- count entries value
                Right (add keyCount valueCount)
          Primitive -> Right 0
    missing = Left "Parquet.Arrow: container has no physical leaf to recover its shape"


-- Defined byte/text values are already in output order. Inserting empty null
-- slots requires new offsets, not a gather that copies the first string into
-- every null row (which can overflow Arrow's offsets for a small valid file).
flatVariable
  :: Storable o
  => Plan
  -> P.Vector Int32
  -> (Maybe C.Validity -> S.Vector o -> ByteString -> Either String C.ColumnArray)
  -> S.Vector o
  -> ByteString
  -> Either String C.ColumnArray
flatVariable plan defs mk offsets bytes
  | S.length offsets /= defined + 1 = Left "Parquet.Arrow: defined values and offsets disagree"
  | defined == n = mk validity offsets bytes
  | otherwise = mk validity expanded bytes
  where
    n = P.length defs
    presence = fromIntegral (planPresence plan)
    defined = P.foldl' (\count def -> count + if def == presence then 1 else 0) 0 defs
    validity = if A.fieldNullable (planField plan) then C.validityGenerate n (\i -> P.unsafeIndex defs i >= presence) else Nothing
    expanded = runST $ do
      output <- M.unsafeNew (n + 1)
      let loop !i !rank = do
            M.unsafeWrite output i (S.unsafeIndex offsets rank)
            if i == n
              then S.unsafeFreeze output
              else loop (i + 1) (rank + if P.unsafeIndex defs i == presence then 1 else 0)
      loop 0 0
{-# INLINE flatVariable #-}


variableFromRanks
  :: Storable o
  => (Maybe C.Validity -> S.Vector o -> ByteString -> Either String C.ColumnArray)
  -> Maybe C.Validity
  -> P.Vector Int
  -> S.Vector o
  -> ByteString
  -> Either String C.ColumnArray
variableFromRanks mk validity ranks offsets bytes =
  mk validity (S.generate (P.length ranks) (\i -> S.unsafeIndex offsets (P.unsafeIndex ranks i))) bytes
{-# INLINE variableFromRanks #-}


-- Flat BOOLEAN needs only value/validity bitmaps. Walk definitions with
-- a scalar source rank instead of allocating row-sized slots/ranks/indices.
flatBool :: Plan -> P.Vector Int32 -> C.Bitmap -> Either String C.ColumnArray
flatBool plan defs source
  | defined /= C.bitmapLength source = Left "Parquet.Arrow: defined values and levels disagree"
  | defined == n = C.mkBool Nothing source
  | otherwise =
      let buffers = scatterBoolBits presence defs source
          validity = if A.fieldNullable (planField plan) then Just (Buffer.Validity (scatteredBoolValidity buffers) (n - defined)) else Nothing
      in C.mkBool validity (scatteredBoolValues buffers)
  where
    n = P.length defs
    presence = fromIntegral (planPresence plan)
    defined = P.foldl' (\count def -> count + if def == presence then 1 else 0) 0 defs


data ScatteredBool = ScatteredBool
  { scatteredBoolValues :: !C.Bitmap
  , scatteredBoolValidity :: !C.Bitmap
  }


scatterBoolBits :: Int32 -> P.Vector Int32 -> C.Bitmap -> ScatteredBool
scatterBoolBits presence defs source = runST $ do
  let n = P.length defs
      (!whole, !tailBits) = n `quotRem` 8
      size = whole + if tailBits == 0 then 0 else 1
  values <- M.unsafeNew size
  validity <- M.unsafeNew size
  let finish = do
        vb <- S.unsafeFreeze values
        nb <- S.unsafeFreeze validity
        pure (ScatteredBool (Buffer.Bitmap (Buffer.storableToBytes vb) 0 n) (Buffer.Bitmap (Buffer.storableToBytes nb) 0 n))
      emit !row !rank !vb !nb !bit continue
        | P.unsafeIndex defs row == presence =
            let !nextValues = if Buffer.unsafeBitAt source rank then vb .|. bit else vb
            in continue (rank + 1) nextValues (nb .|. bit)
        | otherwise = continue rank vb nb
      {-# INLINE emit #-}
      tailRows !row !rank !lane !vb !nb
        | row == n = M.unsafeWrite values (row `quot` 8) vb *> M.unsafeWrite validity (row `quot` 8) nb *> finish
        | otherwise = emit row rank vb nb (1 `shiftL` lane) $ \nextRank nextValues nextValidity -> tailRows (row + 1) nextRank (lane + 1) nextValues nextValidity
      loop !row !rank
        | row == n = finish
        | n - row < 8 = tailRows row rank 0 0 0
        | otherwise =
            emit row rank (0 :: Word8) (0 :: Word8) 1 $ \r1 v1 n1 ->
              emit (row + 1) r1 v1 n1 2 $ \r2 v2 n2 ->
                emit (row + 2) r2 v2 n2 4 $ \r3 v3 n3 ->
                  emit (row + 3) r3 v3 n3 8 $ \r4 v4 n4 ->
                    emit (row + 4) r4 v4 n4 16 $ \r5 v5 n5 ->
                      emit (row + 5) r5 v5 n5 32 $ \r6 v6 n6 ->
                        emit (row + 6) r6 v6 n6 64 $ \r7 v7 n7 ->
                          emit (row + 7) r7 v7 n7 128 $ \r8 v8 n8 -> do
                            M.unsafeWrite values (row `quot` 8) v8
                            M.unsafeWrite validity (row `quot` 8) n8
                            loop (row + 8) r8
  loop 0 0
{-# INLINE scatterBoolBits #-}


-- A non-repeated primitive has one definition event per output row. Scatter
-- directly, rather than building slots, prefix ranks and gather indices.
-- Concrete tags at each call site let specialization remove Storable dispatch
-- from the row loop, including for the composite decimal/interval values.
flatPrimitive :: Plan -> P.Vector Int32 -> C.PrimType a -> S.Vector a -> Either String C.ColumnArray
flatPrimitive plan defs primitive values = case primitive of
  C.PInt8 -> flatPrimitiveValues plan defs C.PInt8 values
  C.PInt16 -> flatPrimitiveValues plan defs C.PInt16 values
  C.PInt32 -> flatPrimitiveValues plan defs C.PInt32 values
  C.PInt64 -> flatPrimitiveValues plan defs C.PInt64 values
  C.PUInt8 -> flatPrimitiveValues plan defs C.PUInt8 values
  C.PUInt16 -> flatPrimitiveValues plan defs C.PUInt16 values
  C.PUInt32 -> flatPrimitiveValues plan defs C.PUInt32 values
  C.PUInt64 -> flatPrimitiveValues plan defs C.PUInt64 values
  C.PFloat16 -> flatPrimitiveValues plan defs C.PFloat16 values
  C.PFloat -> flatPrimitiveValues plan defs C.PFloat values
  C.PDouble -> flatPrimitiveValues plan defs C.PDouble values
  C.PDate32 -> flatPrimitiveValues plan defs C.PDate32 values
  C.PDate64 -> flatPrimitiveValues plan defs C.PDate64 values
  C.PTime32 -> flatPrimitiveValues plan defs C.PTime32 values
  C.PTime64 -> flatPrimitiveValues plan defs C.PTime64 values
  C.PTimestamp -> flatPrimitiveValues plan defs C.PTimestamp values
  C.PDuration -> flatPrimitiveValues plan defs C.PDuration values
  C.PIntervalYearMonth -> flatPrimitiveValues plan defs C.PIntervalYearMonth values
  C.PIntervalDayTime -> flatPrimitiveValues plan defs C.PIntervalDayTime values
  C.PIntervalMonthDayNano -> flatPrimitiveValues plan defs C.PIntervalMonthDayNano values
  C.PDecimal32 precision scale -> flatPrimitiveValues plan defs (C.PDecimal32 precision scale) values
  C.PDecimal64 precision scale -> flatPrimitiveValues plan defs (C.PDecimal64 precision scale) values
  C.PDecimal128 precision scale -> flatPrimitiveValues plan defs (C.PDecimal128 precision scale) values
  C.PDecimal256 precision scale -> flatPrimitiveValues plan defs (C.PDecimal256 precision scale) values


flatPrimitiveValues :: Storable a => Plan -> P.Vector Int32 -> C.PrimType a -> S.Vector a -> Either String C.ColumnArray
flatPrimitiveValues plan defs primitive values
  | defined /= S.length values = Left "Parquet.Arrow: defined values and levels disagree"
  | defined == n = C.mkPrim primitive Nothing values
  | defined == 0 = C.maskValidity validity (C.fillerColumn n (C.primColumn primitive values))
  | A.fieldNullable (planField plan) =
      let buffers = scatterWithValidity presence defs values defined
      in C.mkPrim primitive (Just (scatteredValidity buffers)) (scatteredValues buffers)
  | otherwise = C.mkPrim primitive validity scattered
  where
    n = P.length defs
    presence = fromIntegral (planPresence plan)
    defined = P.foldl' (\count def -> count + if def == presence then 1 else 0) 0 defs
    validity = if A.fieldNullable (planField plan) then C.validityGenerate n (\i -> P.unsafeIndex defs i >= presence) else Nothing
    scattered = runST $ do
      output <- M.unsafeNew n
      let loop !i !rank
            | i == n = S.unsafeFreeze output
            | P.unsafeIndex defs i == presence = do
                M.unsafeWrite output i (S.unsafeIndex values rank)
                loop (i + 1) (rank + 1)
            | otherwise = do
                M.unsafeWrite output i (S.unsafeIndex values 0)
                loop (i + 1) rank
      loop 0 0
{-# INLINEABLE flatPrimitiveValues #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Int8 -> S.Vector Int8 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Int16 -> S.Vector Int16 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Int32 -> S.Vector Int32 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Int64 -> S.Vector Int64 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Word8 -> S.Vector Word8 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Word16 -> S.Vector Word16 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Word32 -> S.Vector Word32 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Word64 -> S.Vector Word64 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType C.Float16 -> S.Vector C.Float16 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Float -> S.Vector Float -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType Double -> S.Vector Double -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType C.IntervalDayTime -> S.Vector C.IntervalDayTime -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType C.IntervalMonthDayNano -> S.Vector C.IntervalMonthDayNano -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType C.Decimal128 -> S.Vector C.Decimal128 -> Either String C.ColumnArray #-}
{-# SPECIALIZE flatPrimitiveValues :: Plan -> P.Vector Int32 -> C.PrimType C.Decimal256 -> S.Vector C.Decimal256 -> Either String C.ColumnArray #-}


data ScatteredPrimitive a = ScatteredPrimitive
  { scatteredValues :: !(S.Vector a)
  , scatteredValidity :: !C.Validity
  }


-- The caller preflights the present count before allocating or reading a
-- value. Build its bitmap while scattering, rather than scanning definitions
-- again. Null slots keep the same first-value filler as the generic path.
scatterWithValidity :: Storable a => Int32 -> P.Vector Int32 -> S.Vector a -> Int -> ScatteredPrimitive a
scatterWithValidity presence defs values defined = runST $ do
  let n = P.length defs
      bytes = n `quot` 8 + if n `rem` 8 == 0 then 0 else 1
      filler = S.unsafeIndex values 0
  output <- M.unsafeNew n
  bitmap <- M.unsafeNew bytes
  let finish = do
        frozen <- S.unsafeFreeze output
        bits <- S.unsafeFreeze bitmap
        pure (ScatteredPrimitive frozen (Buffer.Validity (Buffer.Bitmap (Buffer.storableToBytes bits) 0 n) (n - defined)))
      emit !i !rank !mask !bit continue
        | P.unsafeIndex defs i == presence = do
            M.unsafeWrite output i (S.unsafeIndex values rank)
            continue (rank + 1) (mask .|. bit)
        | otherwise = do
            M.unsafeWrite output i filler
            continue rank mask
      {-# INLINE emit #-}
      tailRows !i !rank !lane !mask
        | i == n = M.unsafeWrite bitmap (i `quot` 8) mask *> finish
        | otherwise = emit i rank mask (1 `shiftL` lane) $ \nextRank nextMask ->
            tailRows (i + 1) nextRank (lane + 1) nextMask
      bytesLoop !i !rank
        | i == n = finish
        | n - i < 8 = tailRows i rank 0 0
        | otherwise =
            -- Constant bit positions eliminate per-row shifts and the
            -- byte-boundary branch. Known continuations inline as joins.
            emit i rank (0 :: Word8) 1 $ \r1 m1 ->
              emit (i + 1) r1 m1 2 $ \r2 m2 ->
                emit (i + 2) r2 m2 4 $ \r3 m3 ->
                  emit (i + 3) r3 m3 8 $ \r4 m4 ->
                    emit (i + 4) r4 m4 16 $ \r5 m5 ->
                      emit (i + 5) r5 m5 32 $ \r6 m6 ->
                        emit (i + 6) r6 m6 64 $ \r7 m7 ->
                          emit (i + 7) r7 m7 128 $ \r8 m8 -> do
                            M.unsafeWrite bitmap (i `quot` 8) m8
                            bytesLoop (i + 8) r8
  bytesLoop 0 0
{-# INLINE scatterWithValidity #-}


slotIndices :: Plan -> LeafColumn -> P.Vector Int
slotIndices plan leaf =
  P.filter
    ( \i ->
        fromIntegral (leafReps leaf P.! i) <= planSlotRep plan
          && fromIntegral (leafDefs leaf P.! i) >= planSlotDef plan
    )
    (P.enumFromN 0 (P.length (leafDefs leaf)))


listOffsets :: forall o. (Storable o, Integral o, Bounded o) => Plan -> LeafColumn -> P.Vector Int -> Either String (S.Vector o)
listOffsets child leaf slots = runST $ do
  offsets <- M.new (P.length slots + 1)
  let loop !i !row !count !previousDef
        | i == P.length (leafDefs leaf) = do
            M.unsafeWrite offsets (P.length slots) (fromIntegral count)
            if toInteger count > toInteger (maxBound :: o)
              then pure (Left "Parquet.Arrow: list exceeds requested Arrow offset width")
              else Right <$> S.unsafeFreeze offsets
        | otherwise = do
            let !rep = fromIntegral (leafReps leaf P.! i)
                !def = fromIntegral (leafDefs leaf P.! i)
                !continued = rep >= planSlotRep child
                !entryDef = planSlotDef child
            if (i == 0 && rep /= 0) || (continued && (previousDef < entryDef || def < entryDef))
              then pure (Left "Parquet.Arrow: repeated value continues an absent container")
              else do
                next <-
                  if row < P.length slots && slots P.! row == i
                    then M.unsafeWrite offsets row (fromIntegral count) >> pure (row + 1)
                    else pure row
                let entry = rep <= planSlotRep child && def >= entryDef
                loop (i + 1) next (if entry then count + 1 else count) def
  loop 0 0 (0 :: Int) (0 :: Int)
{-# INLINEABLE listOffsets #-}
{-# SPECIALIZE listOffsets :: Plan -> LeafColumn -> P.Vector Int -> Either String (S.Vector Int32) #-}
{-# SPECIALIZE listOffsets :: Plan -> LeafColumn -> P.Vector Int -> Either String (S.Vector Int64) #-}


-- Siblings share their ancestors' presence and repetition boundaries.
-- Equal total lengths alone would miss children assigned to different rows.
sameShape :: V.Vector (Either String LeafColumn) -> Plan -> Int -> Either String ()
sameShape leaves parent otherIndex = do
  first <- maybe (Left "Parquet.Arrow: missing anchor leaf") id (leaves V.!? planAnchor parent)
  other <- maybe (Left "Parquet.Arrow: missing sibling leaf") id (leaves V.!? otherIndex)
  let a = slotIndices parent first
      b = slotIndices parent other
      matches i =
        leafReps first P.! (a P.! i) == leafReps other P.! (b P.! i)
          && min (fromIntegral (leafDefs first P.! (a P.! i))) (planPresence parent)
            == min (fromIntegral (leafDefs other P.! (b P.! i))) (planPresence parent)
  if P.length a == P.length b && P.all matches (P.enumFromN 0 (P.length a))
    then Right ()
    else Left "Parquet.Arrow: siblings disagree on ancestor levels"
