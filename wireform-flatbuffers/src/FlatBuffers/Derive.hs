{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for FlatBuffers.

FlatBuffers' type system has no generic typeclass in this package,
so this module also exports the @ToFlatBuffers@ / @FromFlatBuffers@
classes the deriver targets.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — FlatBuffers 'VTable' with one positional slot
  per record field. Each slot is @Just inner@ for a set field,
  @Nothing@ for an absent 'Maybe' field, and @Nothing@ for a
  @skip@ped field (with 'defaults' supplying the value on decode).
* 'TypeShapeEnum'    — encoded as a 'VInt32' carrying the
  constructor's zero-based ordinal, overridable with @tag N@.
* 'TypeShapeSum'     — rejected at splice time. FlatBuffers has a
  'union' construct but it requires out-of-band type-tag data that
  this value-level AST does not model.

Modifiers honoured: 'tag' (enum ordinal override), 'skip',
'defaults', 'optional', 'coerced'. 'rename' is ignored because
FlatBuffers' vtables are positional, not keyed.
-}
module FlatBuffers.Derive (
  -- * Classes
  ToFlatBuffers (..),
  FromFlatBuffers (..),

  -- * Derivers
  deriveFlatBuffers,
  deriveToFlatBuffers,
  deriveFromFlatBuffers,

  -- * Zero-copy decode (see "FlatBuffers.View")
  deriveView,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import FlatBuffers.Value qualified as FB
import FlatBuffers.View qualified as FV
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Classes
-- ---------------------------------------------------------------------------

-- | Haskell values that can be projected into a FlatBuffers 'FB.Value'.
class ToFlatBuffers a where
  toFlatBuffers :: a -> FB.Value


-- | FlatBuffers 'FB.Value' views that can be parsed back into Haskell.
class FromFlatBuffers a where
  fromFlatBuffers :: FB.Value -> Either String a


-- ---------------------------------------------------------------------------
-- Scalar instances
-- ---------------------------------------------------------------------------

instance ToFlatBuffers FB.Value where
  toFlatBuffers = id


instance FromFlatBuffers FB.Value where
  fromFlatBuffers = Right


instance ToFlatBuffers Bool where toFlatBuffers = FB.VBool


instance FromFlatBuffers Bool where
  fromFlatBuffers = \case
    FB.VBool b -> Right b
    v -> Left ("FlatBuffers.Derive: expected VBool, got " <> showCtor v)


instance ToFlatBuffers Int8 where toFlatBuffers = FB.VInt8


instance FromFlatBuffers Int8 where
  fromFlatBuffers = \case
    FB.VInt8 n -> Right n
    FB.VInt16 n -> Right (fromIntegral n)
    FB.VInt32 n -> Right (fromIntegral n)
    FB.VInt64 n -> Right (fromIntegral n)
    v -> Left ("FlatBuffers.Derive: expected VInt8, got " <> showCtor v)


instance ToFlatBuffers Int16 where toFlatBuffers = FB.VInt16


instance FromFlatBuffers Int16 where
  fromFlatBuffers = \case
    FB.VInt8 n -> Right (fromIntegral n)
    FB.VInt16 n -> Right n
    FB.VInt32 n -> Right (fromIntegral n)
    FB.VInt64 n -> Right (fromIntegral n)
    v -> Left ("FlatBuffers.Derive: expected VInt16, got " <> showCtor v)


instance ToFlatBuffers Int32 where toFlatBuffers = FB.VInt32


instance FromFlatBuffers Int32 where
  fromFlatBuffers = \case
    FB.VInt8 n -> Right (fromIntegral n)
    FB.VInt16 n -> Right (fromIntegral n)
    FB.VInt32 n -> Right n
    FB.VInt64 n -> Right (fromIntegral n)
    v -> Left ("FlatBuffers.Derive: expected VInt32, got " <> showCtor v)


instance ToFlatBuffers Int64 where toFlatBuffers = FB.VInt64


instance FromFlatBuffers Int64 where
  fromFlatBuffers = \case
    FB.VInt8 n -> Right (fromIntegral n)
    FB.VInt16 n -> Right (fromIntegral n)
    FB.VInt32 n -> Right (fromIntegral n)
    FB.VInt64 n -> Right n
    v -> Left ("FlatBuffers.Derive: expected VInt64, got " <> showCtor v)


instance ToFlatBuffers Int where
  toFlatBuffers = FB.VInt64 . fromIntegral


instance FromFlatBuffers Int where
  fromFlatBuffers v = fromIntegral <$> (fromFlatBuffers v :: Either String Int64)


instance ToFlatBuffers Word8 where toFlatBuffers = FB.VWord8


instance FromFlatBuffers Word8 where
  fromFlatBuffers = \case
    FB.VWord8 n -> Right n
    FB.VWord16 n -> Right (fromIntegral n)
    FB.VWord32 n -> Right (fromIntegral n)
    FB.VWord64 n -> Right (fromIntegral n)
    v -> Left ("FlatBuffers.Derive: expected VWord8, got " <> showCtor v)


instance ToFlatBuffers Word16 where toFlatBuffers = FB.VWord16


instance FromFlatBuffers Word16 where
  fromFlatBuffers = \case
    FB.VWord8 n -> Right (fromIntegral n)
    FB.VWord16 n -> Right n
    FB.VWord32 n -> Right (fromIntegral n)
    FB.VWord64 n -> Right (fromIntegral n)
    v -> Left ("FlatBuffers.Derive: expected VWord16, got " <> showCtor v)


instance ToFlatBuffers Word32 where toFlatBuffers = FB.VWord32


instance FromFlatBuffers Word32 where
  fromFlatBuffers = \case
    FB.VWord8 n -> Right (fromIntegral n)
    FB.VWord16 n -> Right (fromIntegral n)
    FB.VWord32 n -> Right n
    FB.VWord64 n -> Right (fromIntegral n)
    v -> Left ("FlatBuffers.Derive: expected VWord32, got " <> showCtor v)


instance ToFlatBuffers Word64 where toFlatBuffers = FB.VWord64


instance FromFlatBuffers Word64 where
  fromFlatBuffers = \case
    FB.VWord8 n -> Right (fromIntegral n)
    FB.VWord16 n -> Right (fromIntegral n)
    FB.VWord32 n -> Right (fromIntegral n)
    FB.VWord64 n -> Right n
    v -> Left ("FlatBuffers.Derive: expected VWord64, got " <> showCtor v)


instance ToFlatBuffers Float where toFlatBuffers = FB.VFloat


instance FromFlatBuffers Float where
  fromFlatBuffers = \case
    FB.VFloat f -> Right f
    FB.VDouble d -> Right (realToFrac d)
    v -> Left ("FlatBuffers.Derive: expected VFloat, got " <> showCtor v)


instance ToFlatBuffers Double where toFlatBuffers = FB.VDouble


instance FromFlatBuffers Double where
  fromFlatBuffers = \case
    FB.VDouble d -> Right d
    FB.VFloat f -> Right (realToFrac f)
    v -> Left ("FlatBuffers.Derive: expected VDouble, got " <> showCtor v)


instance ToFlatBuffers Text where toFlatBuffers = FB.VString


instance FromFlatBuffers Text where
  fromFlatBuffers = \case
    FB.VString t -> Right t
    v -> Left ("FlatBuffers.Derive: expected VString, got " <> showCtor v)


{- | FlatBuffers has no native @bytes@ type; @ByteString@ maps to a
vector of @ubyte@ (@[VWord8]@), which matches the canonical
FlatBuffers idiom for raw byte payloads.
-}
instance ToFlatBuffers ByteString where
  toFlatBuffers bs =
    FB.VVector (V.generate (BS.length bs) (\i -> FB.VWord8 (BS.index bs i)))


instance FromFlatBuffers ByteString where
  fromFlatBuffers = \case
    FB.VVector vs -> BS.pack <$> traverse asByte (V.toList vs)
      where
        asByte (FB.VWord8 n) = Right n
        asByte x =
          Left
            ( "FlatBuffers.Derive: byte-vector element not VWord8: "
                <> showCtor x
            )
    FB.VString _ -> Left "FlatBuffers.Derive: expected VVector for ByteString, got VString"
    v ->
      Left
        ( "FlatBuffers.Derive: expected VVector for ByteString, got "
            <> showCtor v
        )


instance ToFlatBuffers a => ToFlatBuffers [a] where
  toFlatBuffers xs = FB.VVector (V.fromList (map toFlatBuffers xs))


instance FromFlatBuffers a => FromFlatBuffers [a] where
  fromFlatBuffers = \case
    FB.VVector vs -> traverse fromFlatBuffers (V.toList vs)
    v -> Left ("FlatBuffers.Derive: expected VVector, got " <> showCtor v)


instance ToFlatBuffers a => ToFlatBuffers (Vector a) where
  toFlatBuffers xs = FB.VVector (V.map toFlatBuffers xs)


instance FromFlatBuffers a => FromFlatBuffers (Vector a) where
  fromFlatBuffers = \case
    FB.VVector vs -> V.mapM fromFlatBuffers vs
    v -> Left ("FlatBuffers.Derive: expected VVector, got " <> showCtor v)


{- | Standalone 'Maybe' instance. FlatBuffers' natural notion of
optionality is the vtable slot, which only exists inside a record.
For values that appear /outside/ a record (e.g. list elements) we
encode 'Maybe a' as a one-slot 'VTable': 'Nothing' is an empty slot,
'Just x' is a single 'Just' slot.
-}
instance ToFlatBuffers a => ToFlatBuffers (Maybe a) where
  toFlatBuffers = \case
    Nothing -> FB.VTable (V.singleton Nothing)
    Just x -> FB.VTable (V.singleton (Just (toFlatBuffers x)))


instance FromFlatBuffers a => FromFlatBuffers (Maybe a) where
  fromFlatBuffers = \case
    FB.VTable vs
      | V.length vs == 1 ->
          case V.head vs of
            Nothing -> Right Nothing
            Just v -> Just <$> fromFlatBuffers v
      | otherwise -> Left "FlatBuffers.Derive: Maybe expected 1-slot VTable"
    v -> Left ("FlatBuffers.Derive: expected VTable for Maybe, got " <> showCtor v)


showCtor :: FB.Value -> String
showCtor = \case
  FB.VBool _ -> "VBool"
  FB.VInt8 _ -> "VInt8"
  FB.VInt16 _ -> "VInt16"
  FB.VInt32 _ -> "VInt32"
  FB.VInt64 _ -> "VInt64"
  FB.VWord8 _ -> "VWord8"
  FB.VWord16 _ -> "VWord16"
  FB.VWord32 _ -> "VWord32"
  FB.VWord64 _ -> "VWord64"
  FB.VFloat _ -> "VFloat"
  FB.VDouble _ -> "VDouble"
  FB.VString _ -> "VString"
  FB.VVector _ -> "VVector"
  FB.VTable _ -> "VTable"
  FB.VStruct _ -> "VStruct"
  FB.VUnion _ _ -> "VUnion"


-- ---------------------------------------------------------------------------
-- Public deriver entry points
-- ---------------------------------------------------------------------------

deriveFlatBuffers :: Name -> Q [Dec]
deriveFlatBuffers nm =
  (++) <$> deriveToFlatBuffers nm <*> deriveFromFlatBuffers nm


deriveToFlatBuffers :: Name -> Q [Dec]
deriveToFlatBuffers nm = do
  ti <- reifyTypeInfo nm
  body <- toFBBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''ToFlatBuffers) typ)
          [FunD 'toFlatBuffers [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromFlatBuffers :: Name -> Q [Dec]
deriveFromFlatBuffers nm = do
  ti <- reifyTypeInfo nm
  body <- fromFBBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FromFlatBuffers) typ)
          [FunD 'fromFlatBuffers [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToFlatBuffers: dispatch on shape
-- ---------------------------------------------------------------------------

toFBBody :: TypeInfo -> Q Exp
toFBBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toFBNewtype c
  TypeShapeRecord c -> toFBRecord c
  TypeShapeEnum cs -> toFBEnum cs
  TypeShapeSum _ -> fail flatBuffersSumErr


flatBuffersSumErr :: String
flatBuffersSumErr =
  "FlatBuffers.Derive: refusing to derive instances for a multi-constructor \
  \sum type. FlatBuffers models tagged alternatives as 'union' values, \
  \which require schema-side type-tag metadata not captured by \
  \FlatBuffers.Value. Use a different backend (e.g. wireform-bson) or \
  \hand-write the instance."


toFBNewtype :: ConInfo -> Q Exp
toFBNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|toFlatBuffers ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|toFlatBuffers $(varE x)|]
  _ -> fail "FlatBuffers.Derive: newtype must have exactly one field"


toFBRecord :: ConInfo -> Q Exp
toFBRecord c = do
  x <- newName "x"
  slots <- recordToFBSlots (varE x) c
  lamE [varP x] [|FB.VTable (V.fromList $(pure slots))|]


-- | Emit a @[Maybe FB.Value]@ literal: one slot per field, positional.
recordToFBSlots :: Q Exp -> ConInfo -> Q Exp
recordToFBSlots varExp c = do
  slotExps <- mapM (toFBSlot varExp) (conInfoFields c)
  pure (ListE slotExps)


{- | Slot encoding for a single record field, applying modifier logic:

* @skip@      — always 'Nothing'.
* Maybe field — 'Nothing' / @Just inner@ (no double-wrap).
* otherwise   — @Just (toFlatBuffers value)@.
-}
toFBSlot :: Q Exp -> FieldInfo -> Q Exp
toFBSlot varExp (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendFlatBuffers selName
  if miSkip mi
    then [|Nothing :: Maybe FB.Value|]
    else do
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> getter
            Just _ -> [|coerce $getter|]
      case unwrapMaybe ty of
        Just _ ->
          [|
            case $encoded of
              Nothing -> Nothing :: Maybe FB.Value
              Just inn -> Just (toFlatBuffers inn)
            |]
        Nothing -> [|Just (toFlatBuffers $encoded)|]


toFBEnum :: [ConInfo] -> Q Exp
toFBEnum cs = do
  v <- newName "v"
  matches <- mapM enumMatchTo (zip [0 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    enumMatchTo :: (Int32, ConInfo) -> Q Match
    enumMatchTo (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendFlatBuffers (conInfoName c)
      let n = case miTag mi of
            Just t -> fromIntegral t :: Int32
            Nothing -> defaultIdx
      bodyE <- [|FB.VInt32 $(litE (integerL (fromIntegral n)))|]
      pure (Match (ConP (conInfoName c) [] []) (NormalB bodyE) [])


-- ---------------------------------------------------------------------------
-- FromFlatBuffers: dispatch on shape
-- ---------------------------------------------------------------------------

fromFBBody :: TypeInfo -> Q Exp
fromFBBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromFBNewtype c
  TypeShapeRecord c -> fromFBRecord c
  TypeShapeEnum cs -> fromFBEnum cs
  TypeShapeSum _ -> fail flatBuffersSumErr


fromFBNewtype :: ConInfo -> Q Exp
fromFBNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . fromFlatBuffers|]
  _ -> fail "FlatBuffers.Derive: newtype must have exactly one field"


fromFBRecord :: ConInfo -> Q Exp
fromFBRecord c = do
  v <- newName "v"
  slots <- newName "slots"
  bodyE <- recordParser slots c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'FB.VTable [varP slots])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "FlatBuffers.Derive: expected VTable for record type"|]
            )
            []
        ]
    )


-- | Build @Ctor <$> f0 <*> f1 <*> ...@ using positional slot indices.
recordParser :: Name -> ConInfo -> Q Exp
recordParser slots c = case zip [0 ..] (conInfoFields c) of
  [] -> [|Right $(conE (conInfoName c))|]
  (p0 : ps) -> do
    e0 <- fieldParser slots p0
    hd <- [|$(conE (conInfoName c)) <$> $(pure e0)|]
    foldlM
      ( \acc p -> do
          ef <- fieldParser slots p
          [|$(pure acc) <*> $(pure ef)|]
      )
      hd
      ps


-- | Parser for a single record field at a given positional index.
fieldParser :: Name -> (Int, FieldInfo) -> Q Exp
fieldParser slots (idx, FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendFlatBuffers selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "FlatBuffers.Derive: missing 'defaults' for skipped field "
                ++ $(litE (stringL (nameBase selName)))
            )
          |]
    else do
      let idxLit = litE (integerL (fromIntegral idx))
          nameLit = litE (stringL (nameBase selName))
      base <- case unwrapMaybe ty of
        Just _ ->
          [|
            case lookupSlot $idxLit $(varE slots) of
              Nothing -> Right Nothing
              Just Nothing -> Right Nothing
              Just (Just v) -> Just <$> fromFlatBuffers v
            |]
        Nothing ->
          [|
            case lookupSlot $idxLit $(varE slots) of
              Nothing ->
                Left
                  ( "FlatBuffers.Derive: missing slot "
                      ++ show ($idxLit :: Int)
                      ++ " for field "
                      ++ $nameLit
                  )
              Just Nothing ->
                Left
                  ( "FlatBuffers.Derive: empty slot "
                      ++ show ($idxLit :: Int)
                      ++ " for non-optional field "
                      ++ $nameLit
                  )
              Just (Just v) -> fromFlatBuffers v
            |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


{- | Lookup a positional slot in a vtable-slot vector. Returns
@Nothing@ if out of range, @Just Nothing@ for a present-but-absent
slot, @Just (Just v)@ for a set slot.
-}
lookupSlot :: Int -> Vector (Maybe FB.Value) -> Maybe (Maybe FB.Value)
lookupSlot i vs
  | i < 0 || i >= V.length vs = Nothing
  | otherwise = Just (vs V.! i)


fromFBEnum :: [ConInfo] -> Q Exp
fromFBEnum cs = do
  v <- newName "v"
  i <- newName "i"
  branches <- mapM (enumMatchFrom i) (zip [0 ..] cs)
  let multi =
        MultiIfE
          ( map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
              ++ [
                   ( NormalG (ConE 'True)
                   , AppE
                       (ConE 'Left)
                       ( AppE
                           ( AppE
                               (VarE 'mappend)
                               (LitE (StringL "FlatBuffers.Derive: unknown enum value "))
                           )
                           (AppE (VarE 'show) (VarE i))
                       )
                   )
                 ]
          )
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'FB.VInt32 [varP i])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "FlatBuffers.Derive: enum expected VInt32"|])
            []
        ]
    )
  where
    enumMatchFrom :: Name -> (Int32, ConInfo) -> Q (Guard, Exp)
    enumMatchFrom iVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendFlatBuffers (conInfoName c)
      let n = case miTag mi of
            Just t -> fromIntegral t :: Int32
            Nothing -> defaultIdx
          guardE =
            InfixE
              (Just (VarE iVar))
              (VarE '(==))
              (Just (LitE (IntegerL (fromIntegral n))))
      pure (NormalG guardE, ConE (conInfoName c))


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Detect @Maybe a@ in a field type.
unwrapMaybe :: Type -> Maybe Type
unwrapMaybe (AppT (ConT n) t) | n == ''Maybe = Just t
unwrapMaybe _ = Nothing


requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "FlatBuffers.Derive: cannot derive FlatBuffers for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT


-- ---------------------------------------------------------------------------
-- View deriver: zero-copy decode straight off a 'FV.Table' cursor
-- ---------------------------------------------------------------------------
--
-- Compared with 'deriveFromFlatBuffers', the generated code:
--

-- * never materialises 'FB.Value' anywhere — every field bottoms


--   out in 'FV.viewSlot' (= one aligned scalar peek for inline
--   fields, or one uoffset chase + slice for strings / nested
--   tables);

-- * handles 'Maybe' fields with 'FV.viewSlotMaybe' (= 'Right


--   Nothing' for absent slots, no allocation of a placeholder);

-- * honours 'skip' + 'defaults' identically to the value-shaped


--   deriver ('Right defaultValue' on absent / skipped slots);

-- * rejects sum types — same constraint as the 'FB.Value' shape


--   for the same reason (tagged unions need schema-side metadata).

{- | Derive a 'FV.View' instance for a record / newtype / enum.

@
{\-# LANGUAGE TemplateHaskell #-\}

data Position = Position
  { posName :: !Text
  , posX    :: !Int32
  , posY    :: !Int32
  } deriving Show

deriveView ''Position
@

The generated instance reads each field from the buffer on
demand; no 'FB.Value' AST is built.
-}
deriveView :: Name -> Q [Dec]
deriveView nm = do
  ti <- reifyTypeInfo nm
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  case typeInfoShape ti of
    TypeShapeNewtype c -> deriveViewNewtype typ c
    TypeShapeEnum cs -> deriveViewEnum typ cs
    TypeShapeRecord c -> deriveViewRecord typ c
    TypeShapeSum _ -> fail flatBuffersSumErr


{- | Records become a 'View' (for the root / nested-table case)
plus 'SlotView' (so the type can appear as a record field
inside another type) plus 'VectorElem' (so the type can
appear as a vector element). 'SlotView' / 'VectorElem' both
chase a uoffset and hand the resulting position back to the
record's own 'view'.
-}
deriveViewRecord :: Type -> ConInfo -> Q [Dec]
deriveViewRecord typ c = do
  body <- viewRecord c
  let vDec =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FV.View) typ)
          [FunD 'FV.view [Clause [] (NormalB body) []]]
  slotE <-
    [|
      \bs off ->
        FV.followUOffset bs off
          >>= \p -> FV.view (FV.tableFromBuffer bs p)
      |]
  vecS <- [|\_ -> 4 :: Int|]
  vecRE <-
    [|
      \bs off ->
        FV.followUOffset bs off
          >>= \p -> FV.view (FV.tableFromBuffer bs p)
      |]
  let slotDec =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FV.SlotView) typ)
          [FunD 'FV.readSlot [Clause [] (NormalB slotE) []]]
      vecDec =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FV.VectorElem) typ)
          [ FunD
              'FV.vectorStride
              [Clause [] (NormalB vecS) []]
          , FunD
              'FV.readVectorElem
              [Clause [] (NormalB vecRE) []]
          ]
  pure [vDec, slotDec, vecDec]


{- | Newtype: pass-through. Emits 'SlotView' and 'VectorElem'
instances that delegate to the inner field's instances; does
/not/ emit a 'View' instance because newtypes wrap a single
inner field and don't have their own table layout. (If you
need 'View' for a newtype around a record, use 'coerce'.)
-}
deriveViewNewtype :: Type -> ConInfo -> Q [Dec]
deriveViewNewtype typ c = case conInfoFields c of
  [FieldInfo _ _] -> do
    slotE <- [|\bs off -> fmap $(conE (conInfoName c)) (FV.readSlot bs off)|]
    -- Use the same stride / decode shape as the inner field. We
    -- can't read the stride from a runtime call (we'd need a
    -- proxy of the inner type at the value level); use 4 as the
    -- default which is right for uoffset-shaped inner types.
    -- For inline scalars, the user can hand-write the instance.
    vecS <- [|\_ -> 4 :: Int|]
    vecRE <- [|\bs off -> fmap $(conE (conInfoName c)) (FV.readVectorElem bs off)|]
    let slotDec =
          InstanceD
            Nothing
            []
            (AppT (ConT ''FV.SlotView) typ)
            [FunD 'FV.readSlot [Clause [] (NormalB slotE) []]]
        vecDec =
          InstanceD
            Nothing
            []
            (AppT (ConT ''FV.VectorElem) typ)
            [ FunD
                'FV.vectorStride
                [Clause [] (NormalB vecS) []]
            , FunD
                'FV.readVectorElem
                [Clause [] (NormalB vecRE) []]
            ]
    pure [slotDec, vecDec]
  _ -> fail "FlatBuffers.Derive.deriveView: newtype must have exactly one field"


{- | Enum: 'SlotView' reads an Int32 inline and dispatches.
'VectorElem' reuses the same shape with stride 4.
-}
deriveViewEnum :: Type -> [ConInfo] -> Q [Dec]
deriveViewEnum typ cs = do
  iSlot <- newName "i"
  iVec <- newName "iv"
  branchesSlot <- mapM (enumGuardedV iSlot) (zip [0 ..] cs)
  branchesVec <- mapM (enumGuardedV iVec) (zip [0 ..] cs)
  let multiSlot =
        MultiIfE
          ( map (\(g, e) -> (g, AppE (ConE 'Right) e)) branchesSlot
              ++ [
                   ( NormalG (ConE 'True)
                   , AppE
                       (ConE 'Left)
                       ( AppE
                           ( AppE
                               (VarE 'mappend)
                               (LitE (StringL "FlatBuffers.Derive.deriveView: unknown enum value "))
                           )
                           (AppE (VarE 'show) (VarE iSlot))
                       )
                   )
                 ]
          )
      multiVec =
        MultiIfE
          ( map (\(g, e) -> (g, AppE (ConE 'Right) e)) branchesVec
              ++ [
                   ( NormalG (ConE 'True)
                   , AppE
                       (ConE 'Left)
                       ( AppE
                           ( AppE
                               (VarE 'mappend)
                               (LitE (StringL "FlatBuffers.Derive.deriveView: unknown enum value "))
                           )
                           (AppE (VarE 'show) (VarE iVec))
                       )
                   )
                 ]
          )
  bs <- newName "bs"
  off <- newName "off"
  bsv <- newName "bsv"
  offv <- newName "offv"
  slotBody <-
    [|
      do
        $(varP iSlot) <- (FV.readSlot $(varE bs) $(varE off) :: Either String Int32)
        $(pure multiSlot)
      |]
  vecBody <-
    [|
      do
        $(varP iVec) <- (FV.readVectorElem $(varE bsv) $(varE offv) :: Either String Int32)
        $(pure multiVec)
      |]
  let slotE = LamE [VarP bs, VarP off] slotBody
      vecRE = LamE [VarP bsv, VarP offv] vecBody
      vecS = LamE [WildP] (SigE (LitE (IntegerL 4)) (ConT ''Int))
      slotDec =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FV.SlotView) typ)
          [FunD 'FV.readSlot [Clause [] (NormalB slotE) []]]
      vecDec =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FV.VectorElem) typ)
          [ FunD
              'FV.vectorStride
              [Clause [] (NormalB vecS) []]
          , FunD
              'FV.readVectorElem
              [Clause [] (NormalB vecRE) []]
          ]
  pure [slotDec, vecDec]
  where
    enumGuardedV :: Name -> (Int32, ConInfo) -> Q (Guard, Exp)
    enumGuardedV iVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendFlatBuffers (conInfoName c)
      let n = case miTag mi of
            Just t -> fromIntegral t :: Int32
            Nothing -> defaultIdx
          guardE =
            InfixE
              (Just (VarE iVar))
              (VarE '(==))
              (Just (LitE (IntegerL (fromIntegral n))))
      pure (NormalG guardE, ConE (conInfoName c))


{- | Record: build @Ctor \<$\> slot 0 \<*\> slot 1 \<*\> ...@ from
the table cursor, applying the same modifier rules as the
value-shaped deriver.
-}
viewRecord :: ConInfo -> Q Exp
viewRecord c = do
  t <- newName "t"
  let pairs = zip [0 :: Int ..] (conInfoFields c)
  case pairs of
    [] -> lamE [varP t] [|Right $(conE (conInfoName c))|]
    (p0 : ps) -> do
      e0 <- viewFieldParser t p0
      hd <- [|$(conE (conInfoName c)) <$> $(pure e0)|]
      bodyE <-
        foldlM
          ( \acc p -> do
              ef <- viewFieldParser t p
              [|$(pure acc) <*> $(pure ef)|]
          )
          hd
          ps
      lamE [varP t] (pure bodyE)


{- | Per-field decode for the View deriver. Mirrors 'fieldParser'
structurally so the two shapes can't diverge silently.
-}
viewFieldParser :: Name -> (Int, FieldInfo) -> Q Exp
viewFieldParser tNm (idx, FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendFlatBuffers selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "FlatBuffers.Derive.deriveView: missing 'defaults' for skipped field "
                ++ $(litE (stringL (nameBase selName)))
            )
          |]
    else do
      let idxLit = litE (integerL (fromIntegral idx))
      base <- case unwrapMaybe ty of
        Just _ -> [|FV.viewSlotMaybe $(varE tNm) $idxLit|]
        Nothing -> [|FV.viewSlot $(varE tNm) $idxLit|]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]
