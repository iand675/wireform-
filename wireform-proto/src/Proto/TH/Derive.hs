{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- TH name-quotes ('' / ''' / ' / 'foo) need TemplateHaskell, but hlint
-- can't see the syntax and reports the pragma as unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

{- | Annotation-driven Template Haskell deriver for protobuf wire
instances on hand-written Haskell records.

This module is the TH counterpart to 'Proto.CodeGen' (which emits
pure Haskell text from a parsed @.proto@ AST). The text codegen
stays the home for cross-platform builds where TH is awkward; this
deriver is for users who prefer to write the Haskell record
themselves, with explicit @ANN@ annotations supplying the proto
field numbers and wire-encoding choices.

The IDL-driven entry point 'Proto.TH.loadProto' may also reuse the
low-level body builders in "Proto.Internal.Derive" to share the
encoder \/ decoder \/ size logic without going through @ANN@ +
'Language.Haskell.TH.reifyAnnotations'.

== Scope

* Records (@TypeShapeRecord@) only. The IDL bridge
  ('deriveProtoFromTranslated') is the route for newtypes / enums /
  sums declared in the same splice as the deriver call.
* Singular fields of one of the recognized scalar types
  (@Int32 \/ Int64 \/ Word32 \/ Word64 \/ Bool \/ Float \/ Double \/
  Text \/ ByteString@) or a submessage with existing
  'MessageEncode' \/ 'MessageDecode' instances.
* @Maybe a@ for explicit field presence (proto2 optional or proto3
  @optional@).
* Repeated containers — outer 'Data.Vector.Vector' / list \/
  'Data.Sequence.Seq' constructors are auto-detected and routed
  through 'I.FKRepeated' with the matching 'I.RepeatedRep'. For
  packable scalars the encoder defaults to packed; for non-packable
  element types (string / bytes / submessage / enum) it falls back
  to one record per element. Decoders accept both shapes per the
  proto3 spec, regardless of which the writer chose.
* @Map.Map K V@ — auto-detected as a proto3 @map<K, V>@. The
  key encoding is inferred from the key type (or supplied via the
  'mapKey' modifier when the type is ambiguous, e.g. @Word32@ as
  @uint32@ vs @fixed32@).
* Sum types whose every constructor has exactly one argument and a
  per-constructor @tag N@ annotation are recognised as oneofs and
  routed through 'I.FKOneof'.
* Datatypes whose every constructor is nullary
  ('Wireform.Derive.TypeInfo.TypeShapeEnum') are recognised as
  enums and emit varint encoding via 'fromEnum' \/ 'toEnum'.
* @wireOverride WireZigZag@ to force ZigZag for sint32 \/ sint64.
* @wireOverride WireFixed@ to force fixed-width for fixed32 \/
  fixed64 \/ sfixed32 \/ sfixed64.

== Required modifiers

Every record field MUST carry an explicit @tag N@ modifier — there
is no positional default. This matches Thrift's discipline and is
the only correct policy for a wire format where field numbers are
part of the contract.

== Generated instances

* 'MessageEncode' — @buildSized@ with proto3 default-value skip;
  size is computed inline via the sized-builder machinery.
* 'MessageDecode' — accumulator loop with field-number dispatch.
* 'IsMessage'     — provides 'messageTypeName'. Defaults to the
  Haskell type's base name; override via the @customModifier
  "wireform-proto.message-type"@ payload.

== Out of scope (for now)

* 'ProtoMessage' schema metadata.
* Proto3 JSON ('Aeson.ToJSON' \/ 'Aeson.FromJSON').
* 'Hashable' (use @deriving anyclass Hashable@ on the type instead).
* Unknown-field preservation on the annotation-driven path
  ('deriveProto'); the IDL bridge ('loadProto' /
  'deriveProtoFromTranslated') routes unknown tags through a
  message-level slot via 'I.MessageMeta.mmUnknownFieldsSel'.
-}
module Proto.TH.Derive (
  -- * Annotation-driven entry points
  deriveProto,
  deriveProtoEncode,
  deriveProtoDecode,

  -- * Pre-translated entry point (for IDL bridges)
  TranslatedField (..),
  TranslatedMessage (..),
  TranslatedOneofVariant (..),
  translatedField,
  deriveProtoFromTranslated,
  deriveProtoFromTranslatedWith,

  -- * Re-exports from the internal field model
  I.ProtoField (..),
  I.ProtoFieldKind (..),
  I.ProtoFieldType (..),
  I.RepeatedMode (..),
  I.scalarPackable,
  I.OneofVariant (..),
  I.oneofVariant,
  I.Scalar (..),
  I.MessageMeta (..),
  I.defaultMessageMeta,
  StringRep (..),
  BytesRep (..),
) where

import Data.Functor qualified
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH
import Proto.Internal.Derive qualified as I
import Proto.Repr (BytesAdapter, BytesRep (..), RepeatedAdapter, StringAdapter, StringRep (..))
import Proto.Repr qualified as PR
import Wireform.Derive.Backend (backendProto)
import Wireform.Derive.Modifier (
  MapKeyScalar (..),
  Modifier,
  WireOverride (..),
 )
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points (annotation-driven)
-- ---------------------------------------------------------------------------

{- | Derive 'MessageEncode', 'MessageDecode', and
'IsMessage' for a record type.
-}
deriveProto :: Name -> Q [Dec]
deriveProto nm = do
  enc <- deriveProtoEncode nm
  dec <- deriveProtoDecode nm
  pure (enc ++ dec)


-- | Derive only the 'MessageEncode' instance for a record type.
deriveProtoEncode :: Name -> Q [Dec]
deriveProtoEncode nm = do
  ti <- recordOnly nm =<< reifyTypeInfo nm
  fis <- protoFields ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  inst <- I.mkEncodeInstance typ fis
  pure [inst]


-- | Derive only the 'MessageDecode' instance for a record type.
deriveProtoDecode :: Name -> Q [Dec]
deriveProtoDecode nm = do
  ti <- recordOnly nm =<< reifyTypeInfo nm
  fis <- protoFields ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  conName <- conNameOf nm
  inst <- I.mkDecodeInstance typ conName fis
  pure [inst]


-- ---------------------------------------------------------------------------
-- Pre-translated entry point (for IDL bridges)
-- ---------------------------------------------------------------------------

{- | A field already lifted from a non-Haskell source (e.g. a parsed
@.proto@ message). Sidesteps GHC's reify graph so callers may
emit the corresponding @data@ declaration in the same TH splice.

The @tf*Shape@ fields disambiguate the field kind for shapes
that are otherwise indistinguishable by Haskell type alone
(a @Vector Foo@ might be a @repeated@ field; a @Map Text Bar@
might be a proto3 @map@; a @Maybe Sum@ might be a @oneof@).
Construct via 'translatedField' for the common defaults.
-}
data TranslatedField = TranslatedField
  { tfSelector :: !Name
  -- ^ Record selector to be used for this field. Must match the
  -- accompanying record constructor that the caller emits.
  , tfInnerType :: !Type
  -- ^ For singular fields: the value type (with 'Maybe' stripped).
  -- For repeated fields: the element type.
  -- For map fields: the value type.
  -- For oneof fields: ignored (each variant carries its own).
  , tfOptional :: !Bool
  -- ^ True iff the Haskell field is wrapped in 'Maybe' (and is
  -- not a oneof or repeated/map; those have their own kinds).
  , tfRepeated :: !(Maybe RepeatedAdapter)
  -- ^ When @Just@, this field is a @repeated@ backed by the
  -- given container adapter.
  , tfPacked :: !(Maybe Bool)
  -- ^ Override for the packed-encoding choice on this repeated
  -- field. @Nothing@ (the default) lets the bridge decide:
  -- packable scalars (everything except @string@ \/ @bytes@ \/
  -- submessage \/ enum) get 'I.ModePacked'; non-packable
  -- elements stay unpacked. @Just True@ forces packed (only
  -- legal for packable scalars). @Just False@ forces the
  -- one-record-per-element \"expanded\" shape — useful for
  -- proto2 fields without @[packed = true]@ or for
  -- byte-compat with very old wire data.
  , tfMapKey :: !(Maybe MapKeyScalar)
  -- ^ When @Just@, this field is a proto3 @map<K, V>@. The key
  -- wire encoding is supplied here; the value's wire encoding
  -- is inferred from 'tfInnerType' \/ 'tfModifiers' as usual.
  , tfIsEnum :: !Bool
  -- ^ True iff the inner type is encoded as a varint via
  -- 'fromEnum' \/ 'toEnum'. Bridges should set this for all
  -- proto enum types.
  , tfOneofVariants :: ![TranslatedOneofVariant]
  -- ^ Non-empty iff this field is a proto @oneof@. Each variant
  -- pairs a sum-type constructor with its tag and payload type.
  , tfStringAdapter :: !StringAdapter
  -- ^ Adapter for proto @string@ fields. Defaults to
  -- 'strictTextAdapter'. Only consulted when the field is a
  -- string-typed scalar.
  , tfBytesAdapter :: !BytesAdapter
  -- ^ Adapter for proto @bytes@ fields. Defaults to
  -- 'strictBytesAdapter'. Only consulted when the field is a
  -- bytes-typed scalar.
  , tfModifiers :: ![Modifier]
  -- ^ Additional modifiers (tag for non-oneof fields, wire
  -- override, custom payloads, etc.). The proto backend is
  -- consulted via 'foldModifiers'.
  }


-- | One arm of a oneof (used by IDL bridges).
data TranslatedOneofVariant = TranslatedOneofVariant
  { tovConstructor :: !Name
  -- ^ Sum-type constructor name.
  , tovInnerType :: !Type
  -- ^ The constructor's single argument type.
  , tovModifiers :: ![Modifier]
  -- ^ Per-arm modifiers (must include a 'tag N').
  }
  deriving stock (Show)


{- | Convenience: build a 'TranslatedField' for a singular,
non-repeated, non-map, non-oneof, non-enum field. Sets the
shape fields to their empty defaults; only @tfSelector@,
@tfInnerType@, @tfOptional@, and @tfModifiers@ vary at the
common call site.
-}
translatedField :: Name -> Type -> Bool -> [Modifier] -> TranslatedField
translatedField sel ty opt mods =
  TranslatedField
    { tfSelector = sel
    , tfInnerType = ty
    , tfOptional = opt
    , tfRepeated = Nothing
    , tfPacked = Nothing
    , tfMapKey = Nothing
    , tfIsEnum = False
    , tfOneofVariants = []
    , tfStringAdapter = PR.strictTextAdapter
    , tfBytesAdapter = PR.strictBytesAdapter
    , tfModifiers = mods
    }


-- | A whole message lifted from an external source.
data TranslatedMessage = TranslatedMessage
  { tmType :: !Type
  -- ^ Fully applied type, e.g. @ConT ''Person@.
  , tmConstructor :: !Name
  -- ^ Record constructor; for single-constructor records this is
  -- usually the type name.
  , tmProtoName :: !Text
  -- ^ Logical proto name returned by 'PM.messageTypeName'.
  , tmFields :: ![TranslatedField]
  , tmUnknownFieldsSel :: !(Maybe Name)
  -- ^ Optional selector for an @[Decode.UnknownField]@ field on
  -- the record. When set, the synthesised codecs preserve
  -- unknown tags through that slot. 'Nothing' means unknown
  -- fields are silently dropped (the original
  -- 'Proto.TH.Derive.deriveProto' behaviour).
  }


{- | Derive 'MessageEncode', 'MessageDecode', and 'IsMessage' for a
'TranslatedMessage' without consulting the reify graph. Intended for
IDL-driven splices that synthesise a fresh @data@ declaration alongside
the instance group.
-}
deriveProtoFromTranslated :: TranslatedMessage -> Q [Dec]
deriveProtoFromTranslated tm = do
  let meta = I.MessageMeta {I.mmUnknownFieldsSel = tmUnknownFieldsSel tm}
  deriveProtoFromTranslatedWith meta tm


{- | Like 'deriveProtoFromTranslated' but lets the caller supply an
explicit 'I.MessageMeta'. Useful when the unknown-fields slot
(or any future per-message knob) must be threaded in by name
rather than derived from the 'TranslatedMessage' fields.
-}
deriveProtoFromTranslatedWith :: I.MessageMeta -> TranslatedMessage -> Q [Dec]
deriveProtoFromTranslatedWith meta tm = do
  fis <- traverse translatedFieldToProtoField (tmFields tm)
  I.synthesiseProtoInstancesWith
    meta
    (tmType tm)
    (tmConstructor tm)
    (tmProtoName tm)
    fis


translatedFieldToProtoField :: TranslatedField -> Q I.ProtoField
translatedFieldToProtoField tf = do
  mi <- case foldModifiers backendProto (tfModifiers tf) of
    Right info -> pure info
    Left err ->
      fail $
        "Proto.TH.Derive: invalid modifiers on "
          ++ nameBase (tfSelector tf)
          ++ ": "
          ++ show err
  -- Oneofs use the variant tags exclusively; the field-level tag is
  -- ignored.
  let isOneof = not (null (tfOneofVariants tf))
  tagN <- case miTag mi of
    Just n -> pure n
    Nothing
      | isOneof -> pure 0
      | otherwise ->
          fail $
            "Proto.TH.Derive: field "
              ++ nameBase (tfSelector tf)
              ++ " is missing a `tag N` modifier"
  pft <-
    if tfIsEnum tf
      then pure I.PFEnum
      else pickFieldType (tfSelector tf) (tfInnerType tf) (miWireOverride mi)
  let kind = case (tfRepeated tf, tfMapKey tf, tfOneofVariants tf, tfOptional tf) of
        (_, _, vs@(_ : _), _) ->
          I.FKOneof (map (translatedVariantOf pft) vs)
        (Just rep, _, _, _) ->
          I.FKRepeated rep (chooseMode (tfPacked tf) pft)
        (_, Just mks, _, _) -> I.FKMap mks
        (_, _, _, True) -> I.FKMaybe
        _ -> I.FKBare
  pure
    (I.protoField (tfSelector tf) tagN kind pft (tfInnerType tf))
      { I.pfStringAdapter = tfStringAdapter tf
      , I.pfBytesAdapter = tfBytesAdapter tf
      }
  where
    -- Pick packed vs. unpacked. Defaults to packed for packable
    -- scalars (proto3 spec default); falls through to unpacked for
    -- string/bytes/submessage/enum and any field the caller
    -- explicitly opted out of with @tfPacked = Just False@.
    chooseMode :: Maybe Bool -> I.ProtoFieldType -> I.RepeatedMode
    chooseMode override pft' = case override of
      Just True -> I.ModePacked
      Just False -> I.ModeUnpacked
      Nothing -> case pft' of
        I.PFScalar sc | I.scalarPackable sc -> I.ModePacked
        I.PFEnum -> I.ModePacked
        _ -> I.ModeUnpacked
    translatedVariantOf _outer tov =
      case foldModifiers backendProto (tovModifiers tov) of
        Left err ->
          error $
            "Proto.TH.Derive: invalid modifiers on oneof variant "
              ++ nameBase (tovConstructor tov)
              ++ ": "
              ++ show err
        Right vmi -> case miTag vmi of
          Nothing ->
            error $
              "Proto.TH.Derive: oneof variant "
                ++ nameBase (tovConstructor tov)
                ++ " is missing a `tag N` modifier"
          Just vt ->
            let vpft = case (typeBaseName (tovInnerType tov), miWireOverride vmi) of
                  (Just "Int32", Just WireZigZag) -> I.PFScalar I.SSInt32
                  (Just "Int64", Just WireZigZag) -> I.PFScalar I.SSInt64
                  (Just "Word32", Just WireFixed) -> I.PFScalar I.SFixed32
                  (Just "Word64", Just WireFixed) -> I.PFScalar I.SFixed64
                  (Just "Int32", Just WireFixed) -> I.PFScalar I.SSFixed32
                  (Just "Int64", Just WireFixed) -> I.PFScalar I.SSFixed64
                  (Just "Int32", _) -> I.PFScalar I.SInt32
                  (Just "Int64", _) -> I.PFScalar I.SInt64
                  (Just "Word32", _) -> I.PFScalar I.SUInt32
                  (Just "Word64", _) -> I.PFScalar I.SUInt64
                  (Just "Bool", _) -> I.PFScalar I.SBool
                  (Just "Float", _) -> I.PFScalar I.SFloat
                  (Just "Double", _) -> I.PFScalar I.SDouble
                  (Just "Text", _) -> I.PFScalar I.SString
                  (Just "ByteString", _) -> I.PFScalar I.SBytes
                  _ -> I.PFSubmessage
            in I.oneofVariant
                (tovConstructor tov)
                vt
                (tovInnerType tov)
                vpft


-- ---------------------------------------------------------------------------
-- Annotation-driven field analysis (for deriveProto)
-- ---------------------------------------------------------------------------

protoFields :: TypeInfo -> Q [I.ProtoField]
protoFields ti = case typeInfoShape ti of
  TypeShapeRecord c -> traverse (analyseField (typeInfoName ti)) (conInfoFields c)
  _ -> fail "Proto.TH.Derive: only records are supported"


analyseField :: Name -> FieldInfo -> Q I.ProtoField
analyseField tyName (FieldInfo mSel fieldTy) = do
  selName <- case mSel of
    Just n -> pure n
    Nothing ->
      fail $
        "Proto.TH.Derive: "
          ++ nameBase tyName
          ++ " has a positional (non-record) field; only records are supported"
  mi <- reifyModifierInfoFor backendProto selName

  -- Sniff the outer container shape /before/ asking for a tag,
  -- because oneofs derive their per-variant tags from the
  -- constructor-level @ANN@s rather than a single field-level
  -- @tag N@. A oneof-shaped field with no field-level tag is
  -- legitimate; everything else still needs one.
  shape <- detectShape selName fieldTy (miMapKey mi)
  case shape of
    ShapeOneof carrierTy variants -> do
      pure
        ( I.protoField
            selName
            0
            (I.FKOneof variants)
            I.PFSubmessage
            carrierTy
        )
    ShapeRepeated rep elemTy -> withTag selName mi $ \tagN -> do
      pftBase <- pickFieldType selName elemTy (miWireOverride mi)
      pft <- maybeUpgradeToEnum pftBase elemTy
      let mode = case pft of
            I.PFScalar sc | I.scalarPackable sc -> I.ModePacked
            I.PFEnum -> I.ModePacked
            _ -> I.ModeUnpacked
      pure
        ( I.protoField
            selName
            tagN
            (I.FKRepeated rep mode)
            pft
            elemTy
        )
    ShapeMap mks valTy -> withTag selName mi $ \tagN -> do
      pft <- pickFieldType selName valTy (miWireOverride mi)
      pure (I.protoField selName tagN (I.FKMap mks) pft valTy)
    ShapeSingular kind innerTy -> withTag selName mi $ \tagN -> do
      pftBase <- pickFieldType selName innerTy (miWireOverride mi)
      -- Reify the inner type so a Haskell @Enum@ datatype gets
      -- the @PFEnum@ wire treatment automatically — without this
      -- step every named type became a length-delimited
      -- submessage and proto enums silently broke on the wire.
      pft <- maybeUpgradeToEnum pftBase innerTy
      pure (I.protoField selName tagN kind pft innerTy)
  where
    withTag selN mi k = case miTag mi of
      Just n -> k n
      Nothing ->
        fail $
          "Proto.TH.Derive: field "
            ++ nameBase selN
            ++ " is missing a `tag N` modifier"


{- | Detected outer shape of a record field's Haskell type. Drives
the @ProtoFieldKind@ choice in 'analyseField' without making the
caller decide between repeated / map / oneof / singular by
inspecting the type tree.
-}
data DetectedShape
  = -- | Outer container; carries the element type.
    ShapeRepeated RepeatedAdapter !Type
  | -- | Outer @Map.Map K V@ where @K@ is a permitted proto map
    -- key scalar; carries the value type.
    ShapeMap !MapKeyScalar !Type
  | -- | A Haskell sum type (or @Maybe@-wrapped sum) every
    -- constructor of which has exactly one argument and a
    -- @tag N@ annotation. The variant list is built once at
    -- detect time so 'analyseField' doesn't need to re-reify.
    ShapeOneof !Type ![I.OneofVariant]
  | -- | Anything else: an @FKBare@ singular field, or @FKMaybe@
    -- if the outer constructor was @Maybe@.
    ShapeSingular !I.ProtoFieldKind !Type


{- | Detect the outer shape of a field's type. The lookup priority
is documented inline; the order matters because @Maybe (Map a
b)@ has both a 'Maybe' wrapper and an inner 'Map', and only the
inner shape should drive the proto kind.
-}
detectShape :: Name -> Type -> Maybe MapKeyScalar -> Q DetectedShape
-- Repeated containers take precedence over the Maybe stripping
-- because @Maybe (Vector a)@ is essentially never what users
-- want — proto3 doesn't have nullable repeated fields. We
-- inspect the original (un-stripped) type for repeated detection
-- so @Vector (Maybe a)@ is recognised as repeated-with-Maybe-
-- elements (which the deriver currently doesn't support but
-- can warn about cleanly).
detectShape selName fieldTy mMapKey = case detectRepeated fieldTy of
  Just (rep, elemTy) -> pure (ShapeRepeated rep elemTy)
  Nothing -> case detectMap fieldTy mMapKey of
    Just (mks, valTy) -> pure (ShapeMap mks valTy)
    Nothing ->
      let (kind, innerTy) = unwrapMaybe fieldTy
      in detectOneof selName innerTy >>= \case
          Just variants -> pure (ShapeOneof fieldTy variants)
          Nothing -> pure (ShapeSingular kind innerTy)


-- | Strip a single outer 'Maybe' constructor.
stripMaybe :: Type -> Type
stripMaybe (AppT (ConT n) t) | n == ''Maybe = t
stripMaybe t = t


{- | Detect a repeated container at the outermost type position.
We keep the recognised list explicit (Vector / [] / Seq) so we
don't accidentally classify e.g. @Set@ as repeated; the bridge
has no snoc/empty for arbitrary containers.
-}
detectRepeated :: Type -> Maybe (RepeatedAdapter, Type)
detectRepeated = \case
  AppT ListT t -> Just (PR.listAdapter, t)
  AppT (ConT n) t
    | nameBase n == "Vector" -> Just (PR.vectorAdapter, t)
    | nameBase n == "Seq" -> Just (PR.seqAdapter, t)
  _ -> Nothing


{- | Detect a proto3 @map<K, V>@ at the outermost type position.
The user's @mapKey@ modifier is required to disambiguate when
the key type has multiple legal proto encodings (e.g. @Word32@
could be @uint32@ or @fixed32@); when it's absent we infer a
default for unambiguous types and fail otherwise.
-}
detectMap :: Type -> Maybe MapKeyScalar -> Maybe (MapKeyScalar, Type)
detectMap ty mAnn = case ty of
  AppT (AppT (ConT n) keyTy) valTy
    | nameBase n == "Map" -> Just (resolveKey keyTy mAnn, valTy)
  _ -> Nothing
  where
    -- Annotation always wins if supplied. Otherwise, infer the
    -- canonical proto3 map-key encoding for the obvious base
    -- types; ambiguous integer types default to the non-fixed
    -- non-zigzag variant (matching what a hand-written .proto
    -- would write).
    resolveKey kTy = \case
      Just k -> k
      Nothing -> case typeBaseName kTy of
        Just "Int32" -> MapKeyInt32
        Just "Int64" -> MapKeyInt64
        Just "Word32" -> MapKeyUInt32
        Just "Word64" -> MapKeyUInt64
        Just "Bool" -> MapKeyBool
        Just "Text" -> MapKeyString
        _ -> MapKeyString -- best-effort fallback


{- | Detect a oneof: a Haskell sum whose every constructor has
exactly one argument and a @tag N@ annotation.

* @Maybe SumType@ with such a sum reifies as a oneof on a
  present-or-absent oneof field (the proto-canonical shape).
* Bare @SumType@ would imply \"this oneof is always set\",
  which proto3 has no way to express; we still permit it (the
  record always needs a value) but the encoder writes a single
  variant per encode call regardless.
-}
detectOneof :: Name -> Type -> Q (Maybe [I.OneofVariant])
detectOneof selName ty = case ty of
  ConT tyN -> do
    ti <- reifyTypeInfo tyN
    case typeInfoShape ti of
      TypeShapeSum cs -> traverse (variantOf selName) cs Data.Functor.<&> sequence
      _ -> pure Nothing
  _ -> pure Nothing


{- | Build a 'I.OneofVariant' from a single constructor — fails
the splice with a clear message if the constructor isn't the
one-arg shape we need.
-}
variantOf :: Name -> ConInfo -> Q (Maybe I.OneofVariant)
variantOf parentSel ci = case conInfoFields ci of
  [FieldInfo _ argTy] -> do
    cmi <- reifyModifierInfoFor backendProto (conInfoName ci)
    case miTag cmi of
      Nothing ->
        -- A sum-shaped field with at least one constructor missing
        -- a tag is /not/ a oneof — the user probably means a plain
        -- submessage that happens to be a sum. Give up on oneof
        -- detection and let 'pickFieldType' handle it as
        -- 'PFSubmessage' (which will fail later if no MessageEncode
        -- instance exists, but with a clearer error than we'd give
        -- here).
        pure Nothing
      Just tagN -> do
        pft <- pickFieldType parentSel argTy (miWireOverride cmi)
        pure (Just (I.oneofVariant (conInfoName ci) tagN argTy pft))
  _ -> pure Nothing -- multi-arg constructors aren't oneof variants


{- | Promote a 'PFSubmessage' result to 'PFEnum' when the inner
type is a Haskell @Enum@-shaped datatype. We can't ask GHC
for an Enum dictionary at splice time, so we look at the
declaration shape: a @TypeShapeEnum@ from
'Wireform.Derive.TypeInfo' is a sum where every constructor is
nullary, which is exactly the shape Stock-derive can give
@Enum@ to.
-}
maybeUpgradeToEnum :: I.ProtoFieldType -> Type -> Q I.ProtoFieldType
maybeUpgradeToEnum pft ty = case (pft, ty) of
  (I.PFSubmessage, ConT tyN) -> do
    ti <- reifyTypeInfo tyN
    case typeInfoShape ti of
      TypeShapeEnum _ -> pure I.PFEnum
      _ -> pure pft
  _ -> pure pft


unwrapMaybe :: Type -> (I.ProtoFieldKind, Type)
unwrapMaybe (AppT (ConT n) t) | n == ''Maybe = (I.FKMaybe, t)
unwrapMaybe t = (I.FKBare, t)


{- | Choose the wire encoding for a field. The supplied
@WireOverride@ (if any) takes precedence over the type-driven
default.
-}
pickFieldType :: Name -> Type -> Maybe WireOverride -> Q I.ProtoFieldType
pickFieldType selName ty mOverride = case (typeBaseName ty, mOverride) of
  (Just "Int32", Just WireZigZag) -> pure (I.PFScalar I.SSInt32)
  (Just "Int64", Just WireZigZag) -> pure (I.PFScalar I.SSInt64)
  (Just "Word32", Just WireFixed) -> pure (I.PFScalar I.SFixed32)
  (Just "Word64", Just WireFixed) -> pure (I.PFScalar I.SFixed64)
  (Just "Int32", Just WireFixed) -> pure (I.PFScalar I.SSFixed32)
  (Just "Int64", Just WireFixed) -> pure (I.PFScalar I.SSFixed64)
  (Just "Int32", _) -> pure (I.PFScalar I.SInt32)
  (Just "Int64", _) -> pure (I.PFScalar I.SInt64)
  (Just "Word32", _) -> pure (I.PFScalar I.SUInt32)
  (Just "Word64", _) -> pure (I.PFScalar I.SUInt64)
  (Just "Bool", _) -> pure (I.PFScalar I.SBool)
  (Just "Float", _) -> pure (I.PFScalar I.SFloat)
  (Just "Double", _) -> pure (I.PFScalar I.SDouble)
  (Just "Text", _) -> pure (I.PFScalar I.SString)
  (Just "ByteString", _) -> pure (I.PFScalar I.SBytes)
  _ -> pure I.PFSubmessage
  where
    _ = selName


typeBaseName :: Type -> Maybe String
typeBaseName = \case
  ConT n -> Just (nameBase n)
  AppT t _ -> typeBaseName t
  _ -> Nothing


recordOnly :: Name -> TypeInfo -> Q TypeInfo
recordOnly nm ti = case typeInfoShape ti of
  TypeShapeRecord _ -> pure ti
  _ ->
    fail $
      "Proto.TH.Derive: "
        ++ nameBase nm
        ++ " must be a single-constructor record (got "
        ++ describeShape (typeInfoShape ti)
        ++ ")"


describeShape :: TypeShape -> String
describeShape = \case
  TypeShapeNewtype _ -> "newtype"
  TypeShapeRecord _ -> "record"
  TypeShapeEnum _ -> "enum"
  TypeShapeSum _ -> "sum"


conNameOf :: Name -> Q Name
conNameOf tyName = do
  ti <- reifyTypeInfo tyName
  case typeInfoShape ti of
    TypeShapeRecord c -> pure (conInfoName c)
    _ -> fail "Proto.TH.Derive: not a record"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
