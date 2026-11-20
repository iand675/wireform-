{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for Apache Parquet.

Parquet is a file-format with column-chunk layout, page splitting,
compression, and footer metadata. Fully automating a row codec
would require inventing layout decisions the user typically wants
to control. This deriver therefore ships /schema-only/ TH plus a
per-row leaf projection:

* 'parquetSchemaFor' splices a flat
  @V.Vector "Parquet.Types".SchemaElement@ — the same shape
  "Parquet.Footer" expects in @fmSchema@. Index 0 is a synthetic
  root struct (@seName = "schema"@); indices @1..N@ are the leaves
  in record-declaration order. Field names honour 'rename' \/
  'renameStyle' modifiers.
* 'ToParquetRow' \/ 'FromParquetRow' convert a record to a
  per-row @V.Vector (Maybe PN.LeafValue)@ with one slot per
  non-skipped field. 'Maybe' fields lower to a 'Nothing' slot.
* 'deriveParquet' emits both class instances at once.

== Limitations

* Records only. Newtypes \/ sums \/ enums fail at splice time.
* Only the recognised flat scalar leaves (@Int32 \/ Int64 \/ Int
  \/ Word32 \/ Word64 \/ Float \/ Double \/ Bool \/ Text \/
  String \/ ByteString@) are supported, plus newtypes thereof
  when annotated with 'coerced'.
-}
module Parquet.Derive (
  parquetSchemaFor,
  ToParquetRow (..),
  FromParquetRow (..),
  deriveParquet,
  deriveToParquetRow,
  deriveFromParquetRow,
) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Language.Haskell.TH
import Parquet.Nested qualified as PN
import Parquet.Types qualified as PT
import Wireform.Derive.Backend (backendParquet)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Row classes
-- ---------------------------------------------------------------------------

{- | Convert a record to a row of optional Parquet leaf values, in
the same left-to-right order 'parquetSchemaFor' produces leaves.
Required fields always carry @Just@; @Maybe@ fields use @Nothing@
to mark a missing value.
-}
class ToParquetRow a where
  toParquetRow :: a -> V.Vector (Maybe PN.LeafValue)


{- | Decode a record from a row of optional leaf values. Returns
'Left' if the leaf count mismatches the schema, a leaf's runtime
type is wrong, or a 'Nothing' appears in a required column.
-}
class FromParquetRow a where
  fromParquetRow :: V.Vector (Maybe PN.LeafValue) -> Either String a


-- ---------------------------------------------------------------------------
-- Public splice entry points
-- ---------------------------------------------------------------------------

{- | Splice a @V.Vector "Parquet.Types".SchemaElement@ for the given
record type. The first element is a synthetic root struct named
@"schema"@ with @seNumChildren = Just N@; subsequent elements are
the field leaves, with names, physical types, converted types and
repetition derived from the record fields and any 'rename' \/
'coerced' modifiers attached to them.
-}
parquetSchemaFor :: Name -> Q Exp
parquetSchemaFor nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord c -> recordSchemaE c
    _ ->
      fail $
        "Parquet.Derive.parquetSchemaFor: "
          ++ nameBase nm
          ++ " must be a single-constructor record"


-- | Derive both 'ToParquetRow' and 'FromParquetRow' for a record.
deriveParquet :: Name -> Q [Dec]
deriveParquet nm = (++) <$> deriveToParquetRow nm <*> deriveFromParquetRow nm


deriveToParquetRow :: Name -> Q [Dec]
deriveToParquetRow nm = do
  ti <- reifyTypeInfo nm
  c <- recordCon nm ti
  body <- toRowBody c
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''ToParquetRow) typ)
        [FunD 'toParquetRow [Clause [] (NormalB body) []]]
    ]


deriveFromParquetRow :: Name -> Q [Dec]
deriveFromParquetRow nm = do
  ti <- reifyTypeInfo nm
  c <- recordCon nm ti
  body <- fromRowBody c
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''FromParquetRow) typ)
        [FunD 'fromParquetRow [Clause [] (NormalB body) []]]
    ]


-- ---------------------------------------------------------------------------
-- Schema construction
-- ---------------------------------------------------------------------------

recordSchemaE :: ConInfo -> Q Exp
recordSchemaE c = do
  let n = length (conInfoFields c)
      nLit = litE (integerL (fromIntegral n))
  rootE <-
    [|
      PT.SchemaElement
        { PT.seTypeLength = Nothing
        , PT.seName = T.pack "schema"
        , PT.seRepetition = Nothing
        , PT.seType = Nothing
        , PT.seNumChildren = Just $nLit
        , PT.seConvertedType = Nothing
        , PT.seLogicalType = Nothing
        , PT.seFieldId = Nothing
        }
      |]
  leafExps <- mapM fieldSchemaE (conInfoFields c)
  [|V.fromList ($(pure rootE) : $(pure (ListE leafExps)))|]


fieldSchemaE :: FieldInfo -> Q Exp
fieldSchemaE (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendParquet selName
  let selBase = T.pack (nameBase selName)
  keyExp <- renderWireKey mi selBase
  let (isOptional, innerTy) = unwrapMaybe ty
      effectiveTy = case miCoerce mi of
        Just tgtName -> ConT tgtName
        Nothing -> innerTy
  (ptExp, ctExp) <- physicalAndConvertedE selName effectiveTy
  let repExp =
        if isOptional
          then [|Just PT.Optional|]
          else [|Just PT.Required|]
  [|
    PT.SchemaElement
      { PT.seTypeLength = Nothing
      , PT.seName = $(pure keyExp)
      , PT.seRepetition = $repExp
      , PT.seType = Just $(pure ptExp)
      , PT.seNumChildren = Nothing
      , PT.seConvertedType = $(pure ctExp)
      , PT.seLogicalType = Nothing
      , PT.seFieldId = Nothing
      }
    |]


-- | Map a Haskell scalar type to (parquet physical type, converted type).
physicalAndConvertedE :: Name -> Type -> Q (Exp, Exp)
physicalAndConvertedE selName ty = case typeBaseName ty of
  Just "Int32" -> pair [|PT.PTInt32|] [|Nothing|]
  Just "Int64" -> pair [|PT.PTInt64|] [|Nothing|]
  Just "Int" -> pair [|PT.PTInt64|] [|Nothing|]
  Just "Word32" -> pair [|PT.PTInt32|] [|Just PT.CTUInt32|]
  Just "Word64" -> pair [|PT.PTInt64|] [|Just PT.CTUInt64|]
  Just "Float" -> pair [|PT.PTFloat|] [|Nothing|]
  Just "Double" -> pair [|PT.PTDouble|] [|Nothing|]
  Just "Bool" -> pair [|PT.PTBoolean|] [|Nothing|]
  Just "Text" -> pair [|PT.PTByteArray|] [|Just PT.CTUtf8|]
  Just "String" -> pair [|PT.PTByteArray|] [|Just PT.CTUtf8|]
  Just "ByteString" -> pair [|PT.PTByteArray|] [|Nothing|]
  _ ->
    fail $
      "Parquet.Derive: unsupported field type for "
        ++ nameBase selName
        ++ ": "
        ++ pprint ty
  where
    pair p c = (,) <$> p <*> c


-- ---------------------------------------------------------------------------
-- toParquetRow body
-- ---------------------------------------------------------------------------

toRowBody :: ConInfo -> Q Exp
toRowBody c = do
  x <- newName "x"
  leafExps <- mapM (fieldToLeafE x) (conInfoFields c)
  lamE [varP x] [|V.fromList $(pure (ListE leafExps))|]


fieldToLeafE :: Name -> FieldInfo -> Q Exp
fieldToLeafE x (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendParquet selName
  let getter = AppE (VarE selName) (VarE x)
      (isOptional, innerTy) = unwrapMaybe ty
      effectiveTy = case miCoerce mi of
        Just tgtName -> ConT tgtName
        Nothing -> innerTy
      coercedToInner expr =
        case miCoerce mi of
          Just _ -> [|coerce $expr|]
          Nothing -> expr
  if isOptional
    then do
      v <- newName "y"
      inner <- coercedToInner (varE v)
      leaf <- toLeafE selName effectiveTy inner
      [|
        case $(pure getter) of
          Nothing -> Nothing
          Just $(varP v) -> Just $(pure leaf)
        |]
    else do
      inner <- coercedToInner (pure getter)
      leaf <- toLeafE selName effectiveTy inner
      [|Just $(pure leaf)|]


{- | Wrap a Haskell value of the given inner type into the matching
'PN.LeafValue' constructor. Caller must pass the unwrapped value
(i.e. after any 'coerce').
-}
toLeafE :: Name -> Type -> Exp -> Q Exp
toLeafE selName ty getter = case typeBaseName ty of
  Just "Int32" -> [|PN.LvInt32 $(pure getter)|]
  Just "Int64" -> [|PN.LvInt64 $(pure getter)|]
  Just "Int" -> [|PN.LvInt64 (fromIntegral ($(pure getter) :: Int))|]
  Just "Word32" -> [|PN.LvInt32 (fromIntegral ($(pure getter) :: Word32))|]
  Just "Word64" -> [|PN.LvInt64 (fromIntegral ($(pure getter) :: Word64))|]
  Just "Float" -> [|PN.LvFloat $(pure getter)|]
  Just "Double" -> [|PN.LvDouble $(pure getter)|]
  Just "Bool" -> [|PN.LvBool $(pure getter)|]
  Just "Text" -> [|PN.LvString $(pure getter)|]
  Just "String" -> [|PN.LvString (T.pack $(pure getter))|]
  Just "ByteString" -> [|PN.LvBinary $(pure getter)|]
  _ ->
    fail $
      "Parquet.Derive: unsupported row field type for "
        ++ nameBase selName
        ++ ": "
        ++ pprint ty


-- ---------------------------------------------------------------------------
-- fromParquetRow body
-- ---------------------------------------------------------------------------

fromRowBody :: ConInfo -> Q Exp
fromRowBody c = do
  v <- newName "v"
  let conName = conInfoName c
      fields = conInfoFields c
      n = length fields
      nLit = litE (integerL (fromIntegral n))
      indexedFs = zip [0 ..] fields
  parserExps <- mapM (fieldFromLeafE v) indexedFs
  chainE <- case parserExps of
    [] -> [|Right $(conE conName)|]
    (p0 : ps) -> do
      hd <- [|$(conE conName) <$> $(pure p0)|]
      foldM (\acc e -> [|$(pure acc) <*> $(pure e)|]) hd ps
  body <-
    [|
      if V.length $(varE v) /= $nLit
        then
          Left
            ( "Parquet.Derive: expected "
                ++ show ($nLit :: Int)
                ++ " leaves, got "
                ++ show (V.length $(varE v))
            )
        else $(pure chainE)
      |]
  lamE [varP v] (pure body)


fieldFromLeafE :: Name -> (Int, FieldInfo) -> Q Exp
fieldFromLeafE v (idx, FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendParquet selName
  let nameStr = nameBase selName
      idxLit = litE (integerL (fromIntegral idx))
      leafE = [|V.unsafeIndex $(varE v) $idxLit|]
      (isOptional, innerTy) = unwrapMaybe ty
      effectiveTy = case miCoerce mi of
        Just tgtName -> ConT tgtName
        Nothing -> innerTy
      mapBackE expr =
        case miCoerce mi of
          Just _ -> [|fmap coerce $expr|]
          Nothing -> expr
  if isOptional
    then do
      x <- newName "x"
      decoded <- fromLeafE nameStr effectiveTy (varE x)
      mapped <- mapBackE [|$(pure decoded)|]
      [|
        case $leafE of
          Nothing -> Right Nothing
          Just $(varP x) ->
            case $(pure mapped) of
              Right val -> Right (Just val)
              Left err -> Left err
        |]
    else do
      x <- newName "x"
      decoded <- fromLeafE nameStr effectiveTy (varE x)
      mapped <- mapBackE [|$(pure decoded)|]
      [|
        case $leafE of
          Nothing ->
            Left
              ( "Parquet.Derive: "
                  ++ nameStr
                  ++ ": null in required column"
              )
          Just $(varP x) -> $(pure mapped)
        |]


{- | Decode a single 'PN.LeafValue' (already unwrapped from 'Just')
into the given inner Haskell type. Returns @Either String t@.
-}
fromLeafE :: String -> Type -> Q Exp -> Q Exp
fromLeafE nameStr ty leafE = case typeBaseName ty of
  Just "Int32" ->
    [|
      case $leafE of
        PN.LvInt32 z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvInt32, got "
                ++ show other
            )
      |]
  Just "Int64" ->
    [|
      case $leafE of
        PN.LvInt64 z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvInt64, got "
                ++ show other
            )
      |]
  Just "Int" ->
    [|
      case $leafE of
        PN.LvInt64 z -> Right (fromIntegral z :: Int)
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvInt64, got "
                ++ show other
            )
      |]
  Just "Word32" ->
    [|
      case $leafE of
        PN.LvInt32 z -> Right (fromIntegral z :: Word32)
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvInt32, got "
                ++ show other
            )
      |]
  Just "Word64" ->
    [|
      case $leafE of
        PN.LvInt64 z -> Right (fromIntegral z :: Word64)
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvInt64, got "
                ++ show other
            )
      |]
  Just "Float" ->
    [|
      case $leafE of
        PN.LvFloat z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvFloat, got "
                ++ show other
            )
      |]
  Just "Double" ->
    [|
      case $leafE of
        PN.LvDouble z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvDouble, got "
                ++ show other
            )
      |]
  Just "Bool" ->
    [|
      case $leafE of
        PN.LvBool z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvBool, got "
                ++ show other
            )
      |]
  Just "Text" ->
    [|
      case $leafE of
        PN.LvString z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvString, got "
                ++ show other
            )
      |]
  Just "String" ->
    [|
      case $leafE of
        PN.LvString z -> Right (T.unpack z)
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvString, got "
                ++ show other
            )
      |]
  Just "ByteString" ->
    [|
      case $leafE of
        PN.LvBinary z -> Right z
        other ->
          Left
            ( "Parquet.Derive: "
                ++ nameStr
                ++ ": expected LvBinary, got "
                ++ show other
            )
      |]
  _ ->
    fail $
      "Parquet.Derive: unsupported row field type for "
        ++ nameStr
        ++ ": "
        ++ pprint ty


_unused :: (Int32, Int64, Word32, Word64, ByteString)
_unused = (0, 0, 0, 0, mempty)


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

unwrapMaybe :: Type -> (Bool, Type)
unwrapMaybe (AppT (ConT n) t) | n == ''Maybe = (True, t)
unwrapMaybe t = (False, t)


typeBaseName :: Type -> Maybe String
typeBaseName = \case
  ConT n -> Just (nameBase n)
  AppT t _ -> typeBaseName t
  _ -> Nothing


recordCon :: Name -> TypeInfo -> Q ConInfo
recordCon nm ti = case typeInfoShape ti of
  TypeShapeRecord c -> pure c
  _ ->
    fail $
      "Parquet.Derive: "
        ++ nameBase nm
        ++ " must be a single-constructor record"


requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "Parquet.Derive: cannot derive Parquet for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT


foldM :: (a -> b -> Q a) -> a -> [b] -> Q a
foldM _ acc [] = pure acc
foldM f acc (x : xs) = do
  acc' <- f acc x
  foldM f acc' xs
