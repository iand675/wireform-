{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell helpers that emit the four \"satellite\" instance
groups every 'loadProto'-generated message wants:

  * 'Proto.Schema.ProtoMessage' — schema metadata
    ('protoMessageName', 'protoPackageName', 'protoFieldDescriptors',
    'protoDefaultValue').
  * 'Data.Aeson.ToJSON' / 'Data.Aeson.FromJSON' — proto3 canonical
    JSON (camelCase keys, base64 bytes, string-encoded 64-bit
    integers, NaN\/Infinity sentinels for floats; all of which are
    already handled by helpers in "Proto.Internal.JSON").
  * 'Data.Hashable.Hashable' — recursive structural hash that mirrors
    what the pure-text codegen in "Proto.CodeGen" emits.
  * 'Proto.Schema.ProtoEnum' — enum metadata + numeric \<-\> name
    conversion.

The pure-text codegen has emitted these for years. This module
catches the TH path up so 'loadProto' produces the same surface.
-}
module Proto.TH.Metadata (
  -- * Per-message instances (consumed by 'Proto.TH.messageToDecls'')
  mkProtoMessageInstance,
  mkAesonInstancesForMessage,
  setLenientUnknownEnum,
  mkHashableInstanceForMessage,

  -- * Per-oneof instances (consumed for each oneof carrier sum)
  mkOneofAesonInstances,
  mkOneofHashableInstance,

  -- * Per-enum instances
  mkProtoEnumInstance,
  mkEnumAesonInstances,
  mkEnumHashableInstance,

  -- * Field shape descriptor (passed in by 'Proto.TH')
  MetaField (..),
  MetaFieldKind (..),
  JsonKind (..),
  BytesShape (..),
  JsonShape (..),
  JsonScalar (..),
  OneofVariantJson (..),
  OneofValueShape (..),
  WktShape (..),

  -- * Internal helpers used by spliced code

  {- | Re-exported so the splice doesn't have to qualify them
  across module boundaries.
  -}
  bytesVectorToJSON,
  bytesListToJSON,
  parseBytesVectorMaybe,
  parseBytesListMaybe,
  scalarVectorToJSON,
  scalarMapToJSON,
  scalarMapKeyToText,
  parseScalarMaybe,
  parseScalarVectorMaybe,
  parseScalarMapMaybe,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Aeson.Types qualified as AesonT
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Either (fromRight)
import Data.Foldable qualified as F
import Data.Hashable (Hashable, hashWithSalt)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified
import Data.Reflection (Given, given)
import Data.Scientific qualified as Sci
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import GHC.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH
import Proto.Decode qualified as PD
import Proto.Google.Protobuf.Any qualified
import Proto.Google.Protobuf.Duration qualified
import Proto.Google.Protobuf.Empty qualified
import Proto.Google.Protobuf.FieldMask qualified
import Proto.Google.Protobuf.Struct (NullValue (NullValue'NullValue))
import Proto.Google.Protobuf.Struct qualified as PGS
import Proto.Google.Protobuf.Timestamp qualified
import Proto.Google.Protobuf.Wrappers qualified
import Proto.Internal.JSON qualified as PJ
import Proto.Internal.JSON qualified as PJI
import Proto.Internal.JSON.Extension qualified as PJExt
import Proto.Internal.JSON.WellKnown qualified as WK
import Proto.Schema qualified as PS


-- ---------------------------------------------------------------------------
-- Field shape (passed in by 'Proto.TH')
-- ---------------------------------------------------------------------------

{- | A condensed view of one record field — enough to drive every
satellite-instance emitter without re-deriving anything from the
raw 'Proto.IDL.AST' shape. The caller (currently only 'Proto.TH')
builds this from the 'FieldSpec' it already has.
-}
data MetaField = MetaField
  { mfSelector :: !Name
  -- ^ Haskell record selector for the field (lower-camel).
  , mfProtoName :: !Text
  -- ^ Proto-side field name (snake_case).
  , mfJsonName :: !Text
  {- ^ JSON key (proto3 default: camelCase form of the proto name,
  overridable via the @json_name@ proto option — the caller is
  responsible for resolving that).
  -}
  , mfNumber :: !Int
  -- ^ Proto field number.
  , mfTypeDesc :: !(Q Exp)
  {- ^ Splice-time builder for the field's
  'Proto.Schema.FieldTypeDescriptor' literal.
  -}
  , mfLabel :: !(Q Exp)
  {- ^ Splice-time builder for the field's
  'Proto.Schema.FieldLabel'' literal.
  -}
  , mfKind :: !MetaFieldKind
  {- ^ Container / wrap shape on the Haskell side. Drives default
  value, JSON encoding, and the per-shape branch in
  @hashWithSalt@.
  -}
  , mfJsonKind :: !JsonKind
  {- ^ Whether the field needs the bytes-aware JSON helpers from
  "Proto.Internal.JSON" (because either the value type or the map value
  type is @bytes@).
  -}
  , mfBytesShape :: !BytesShape
  {- ^ When the field carries proto @bytes@ (either directly or as
  the value of a @repeated bytes@ \/ @map\<K, bytes\>@), which
  physical 'Proto.Repr.BytesRep' it uses on the Haskell side.
  Drives the default-skip predicate, the toJSON helper, and
  the parseFieldMaybe helper picked by the JSON splice.
  Defaults to 'SBStrict' (and is ignored entirely for
  'JKNormal' fields).
  -}
  , mfJsonShape :: !JsonShape
  {- ^ Proto3-canonical-JSON encoding shape for this field. Drives
  default-skip, the per-scalar @toJSON@ \/ @parseJSON@ helper,
  and oneof-variant key resolution.
  -}
  , mfRecordDot :: !Bool
  {- ^ Read this field with record-dot syntax (@msg.field@) rather
  than a bare selector application. Set when the record was
  generated with 'Proto.CodeGen.UnprefixedFields', where bare
  field names shared across messages are ambiguous and
  'NoFieldSelectors' removes the selectors; the enclosing module
  then needs @OverloadedRecordDot@. 'False' for the default
  prefixed layout, keeping output byte-identical.
  -}
  }


{- | Proto3-canonical-JSON encoding shape, structured enough to
let the splice both encode (skipping defaults; using
string-form 64-bit ints; routing oneof variants to the right
key) and decode (reach for the matching @parseField*@ helper).
-}
data JsonShape
  = -- | Singular scalar field.
    JSScalar !JsonScalar
  | -- | @Maybe@-wrapped scalar.
    JSMaybe !JsonScalar
  | {- | Singular submessage field
    (carrier is @Maybe T@).
    -}
    JSMessage
  | {- | Singular enum field.
    Skip when @fromEnum x == 0@.
    -}
    JSEnum
  | {- | @Maybe Enum@ (proto2
    optional enum, proto3
    explicit-optional enum).
    -}
    JSEnumMaybe
  | -- | Repeated scalar; skip when empty.
    JSRepeatedScalar !JsonScalar
  | {- | Repeated submessage / enum;
    element-encoded via 'Aeson.toJSON'.
    -}
    JSRepeatedMessage
  | JSRepeatedEnum
  | {- | Map with both scalar key and
    scalar value. Keys always
    stringify (proto3 spec).
    -}
    JSMapScalar !JsonScalar !JsonScalar
  | -- | Map with submessage values.
    JSMapMessage !JsonScalar
  | -- | Map with enum values.
    JSMapEnum !JsonScalar
  | {- | Oneof carrier — emit at most
    one entry under the chosen
    variant's JSON key.
    -}
    JSOneof ![OneofVariantJson]
  | {- | Singular WKT field — route
    through "Proto.Internal.JSON.WellKnown".
    -}
    JSWkt !WktShape
  | -- | @Maybe Wkt@: skip Nothing.
    JSWktMaybe !WktShape
  | -- | @Vector Wkt@: skip empty.
    JSWktRepeated !WktShape


{- | Identifies which Well-Known-Type a field carries, so the JSON
splice can dispatch to the right helper in
"Proto.Internal.JSON.WellKnown" (rather than the generic
@Aeson.toJSON@ which doesn't match proto3 canonical JSON).
-}
data WktShape
  = WktTimestamp
  | WktDuration
  | WktFieldMask
  | WktStruct
  | WktValue
  | WktListValue
  | WktAny
  | WktEmpty
  | WktNullValue
  | WktWrapBool
  | WktWrapInt32
  | WktWrapInt64
  | WktWrapUInt32
  | WktWrapUInt64
  | WktWrapFloat
  | WktWrapDouble
  | WktWrapString
  | WktWrapBytes
  deriving stock (Eq, Show)


{- | Per-scalar JSON shape. Each constructor names enough about the
proto type to pick the right canonical-form encoder / parser
('PJ.protoInt64ToJSON' for @SInt64@, plain 'Aeson.toJSON' for
'SBool' / 'SString' / 'SInt32', 'PJ.bytesFieldToJSON' for
'SBytes', etc.) and the right default-skip predicate.
-}
data JsonScalar
  = JSBool
  | JSInt32
  | JSUInt32
  | JSSInt32
  | JSFixed32
  | JSSFixed32
  | JSInt64
  | JSUInt64
  | JSSInt64
  | JSFixed64
  | JSSFixed64
  | JSFloat
  | JSDouble
  | JSString
  | JSBytes
  deriving stock (Eq, Show)


{- | One arm of an oneof carrier in JSON shape. The variant key is
the proto-side field name camelCased; the payload encoding is
the same @JsonShape@ machinery applied to the variant's value
type. Submessage / scalar / enum variants all flow through the
same encoder.
-}
data OneofVariantJson = OneofVariantJson
  { ovjConstructor :: !Name
  -- ^ Sum-type constructor name.
  , ovjJsonKey :: !Text
  -- ^ Variant's camelCase JSON key.
  , ovjShape :: !OneofValueShape
  -- ^ How to encode the variant's payload.
  }


-- | Payload shape for one oneof variant.
data OneofValueShape
  = OVScalar !JsonScalar
  | OVMessage -- payload is a submessage; emit via 'Aeson.toJSON'
  | OVEnum -- payload is an enum; emit via 'Aeson.toJSON'
  | {- | Oneof variant whose payload is the
    @google.protobuf.NullValue@ WKT. JSON
    @null@ is the variant's /value/ (mapped to
    the singleton enum constant), not the
    "variant unset" marker.
    -}
    OVNullValue
  deriving stock (Eq, Show)


{- | Container shape on the Haskell side. The hash combinator picks
@V.foldl'@, @Map.foldlWithKey'@, etc., based on this.
-}
data MetaFieldKind
  = MFKBare
  | MFKMaybe
  | MFKVector
  | MFKList
  | MFKSeq
  | MFKMap
  | {- | Carrier is @Maybe SumType@ but the JSON / hash
    shape differs from a plain @MFKMaybe@.
    -}
    MFKOneof


{- | Whether the field needs the bytes-aware JSON encoder/parser
helpers in "Proto.Internal.JSON". Plain (non-bytes) fields use the
'Aeson.toJSON' instance for their type directly.
-}
data JsonKind
  = -- | Standard 'Aeson.toJSON' / 'parseFieldMaybe' path.
    JKNormal
  | {- | A @bytes@-typed field. JSON wants base64 via the
    'PJ.bytesFieldToJSON' / 'PJ.parseBytesFieldMaybe' helpers.
    Pair with 'mfBytesShape' to pick the right rep-aware helper
    ('protoBytesToJSON' vs. 'protoLazyBytesToJSON' vs.
    'protoShortBytesToJSON').
    -}
    JKBytes
  | {- | A @map\<K, bytes\>@ — values must base64.
    Currently always strict bytes; per-element rep overrides
    aren't honoured for map values.
    -}
    JKBytesMap
  | {- | A @repeated bytes@ field carried as @Vector ByteString@.
    JSON shape is an array of base64 strings.
    -}
    JKBytesVector
  | -- | A @repeated bytes@ field carried as @[ByteString]@.
    JKBytesList
  | -- | A @repeated bytes@ field carried as @Seq ByteString@.
    JKBytesSeq


{- | Physical Haskell representation of a proto @bytes@ field. Lines
up 1-1 with 'Proto.Repr.BytesRep' but lives here so this module
doesn't have to depend on the @Proto.Repr@ surface.
-}
data BytesShape
  = -- | @Data.ByteString.ByteString@ (the proto default).
    SBStrict
  | -- | @Data.ByteString.Lazy.ByteString@.
    SBLazy
  | -- | @Data.ByteString.Short.ShortByteString@.
    SBShort
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- ProtoMessage
-- ---------------------------------------------------------------------------

{- | Synthesise the 'PS.ProtoMessage' instance for a record. The
field descriptors are built lazily inside @protoFieldDescriptors@
so the splice cost is paid only when the user actually inspects
the schema.
-}
mkProtoMessageInstance
  :: Name
  -- ^ Haskell type name (e.g. @\'\'Account@).
  -> Text
  -- ^ Fully-qualified proto name (e.g. @"my.pkg.Account"@).
  -> Text
  -- ^ Proto package (may be empty).
  -> Name
  -- ^ The @default<Tyname>@ value the splice already emitted.
  -> [MetaField]
  -> Q [Dec]
mkProtoMessageInstance tyName fqName pkg defName fields = do
  descrEntries <- traverse (oneFieldDescriptor tyName) fields
  let descrMap = AppE (VarE 'Map.fromList) (ListE descrEntries)
      protoNameDec =
        FunD
          'PS.protoMessageName
          [Clause [WildP] (NormalB (textLit fqName)) []]
      protoPkgDec =
        FunD
          'PS.protoPackageName
          [Clause [WildP] (NormalB (textLit pkg)) []]
      protoDefDec =
        FunD
          'PS.protoDefaultValue
          [Clause [] (NormalB (VarE defName)) []]
      protoDescrDec =
        FunD
          'PS.protoFieldDescriptors
          [Clause [WildP] (NormalB descrMap) []]
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''PS.ProtoMessage) (ConT tyName))
        [protoNameDec, protoPkgDec, protoDefDec, protoDescrDec]
    ]


-- | Read a record field expression: record-dot @msg.field@ when the
-- record uses the unprefixed layout ('NoFieldSelectors' + shared field
-- names), else the bare selector application @field msg@. The label for
-- the record-dot form is the selector's base name — the field name in
-- either layout.
dotRead :: Bool -> Name -> Name -> Exp
dotRead recDot sel msgVar
  | recDot = GetFieldE (VarE msgVar) (nameBase sel)
  | otherwise = AppE (VarE sel) (VarE msgVar)


-- | 'dotRead' driven by a field's own 'mfRecordDot' flag.
readFieldE :: MetaField -> Name -> Exp
readFieldE mf = dotRead (mfRecordDot mf) (mfSelector mf)


-- | One @(fieldNumber, SomeField FieldDescriptor { ... })@ pair.
oneFieldDescriptor :: Name -> MetaField -> Q Exp
oneFieldDescriptor tyName MetaField {..} = do
  msgVar <- newName "msg"
  vVar <- newName "v"
  tdesc <- mfTypeDesc
  lbl <- mfLabel
  -- Under 'UnprefixedFields' the bare selector does not exist
  -- ('NoFieldSelectors') and an un-annotated record update is
  -- ambiguous when the field name recurs across messages, so pin the
  -- message type: read with record-dot on a typed binder, and annotate
  -- the update's *result* — a typed binder alone does not disambiguate a
  -- 'RecUpdE' inside the polymorphic 'FieldDescriptor' \/ 'SomeField'
  -- wrapper. The prefixed layout keeps the plain forms.
  let msgBinder
        | mfRecordDot = SigP (VarP msgVar) (ConT tyName)
        | otherwise = VarP msgVar
      getBody
        | mfRecordDot = GetFieldE (VarE msgVar) (nameBase mfSelector)
        | otherwise = AppE (VarE mfSelector) (VarE msgVar)
      updateE = RecUpdE (VarE msgVar) [(mfSelector, VarE vVar)]
      setBody
        | mfRecordDot = SigE updateE (ConT tyName)
        | otherwise = updateE
      getter = LamE [msgBinder] getBody
      setter = LamE [VarP vVar, msgBinder] setBody
      record =
        RecConE
          'PS.FieldDescriptor
          [ ('PS.fdName, textLit mfProtoName)
          , ('PS.fdNumber, intLit mfNumber)
          , ('PS.fdTypeDesc, tdesc)
          , ('PS.fdLabel, lbl)
          , ('PS.fdGet, getter)
          , ('PS.fdSet, setter)
          ]
      someField = AppE (ConE 'PS.SomeField) record
      pair = TupE [Just (intLit mfNumber), Just someField]
  pure pair


-- ---------------------------------------------------------------------------
-- ToJSON / FromJSON for messages
-- ---------------------------------------------------------------------------

{- | Synthesise both the 'Aeson.ToJSON' and 'Aeson.FromJSON'
instances for a generated record.

The shape mirrors what the pure-text codegen in "Proto.CodeGen"
emits: a @jsonObject@ with one entry per field on the encode side,
and @parseFieldMaybe@ + a per-field @maybe (default) id@
assignment on the decode side. Bytes / bytes-map fields go
through the dedicated helpers in "Proto.Internal.JSON" so base64 and
64-bit-integer-as-string encoding happen automatically.
-}
mkAesonInstancesForMessage
  :: Name
  -- ^ Type name.
  -> Text
  {- ^ Fully-qualified proto name (drives the
  proto2 extension JSON registry lookup).
  -}
  -> Maybe Name
  {- ^ Unknown-fields selector ('Nothing' for
  types that don't carry one — currently
  none, but kept for forward compat).
  -}
  -> Name
  -- ^ @default<Tyname>@.
  -> Bool
  -- ^ Read fields with record-dot (unprefixed layout).
  -> [MetaField]
  -> Q [Dec]
mkAesonInstancesForMessage tyName fqName ufSel defName recDot fields = do
  toJSONDec <- mkToJSONForMessage tyName fqName ufSel recDot fields
  fromJSONDec <- mkFromJSONForMessage tyName fqName ufSel defName recDot fields
  pure [toJSONDec, fromJSONDec]


mkToJSONForMessage :: Name -> Text -> Maybe Name -> Bool -> [MetaField] -> Q Dec
mkToJSONForMessage tyName fqName mUfSel recDot fields = do
  msgVar <- newName "msg"
  -- Each field contributes a @[(Text, Aeson.Value)]@ — a singleton
  -- when we want to emit the field, an empty list when we want to
  -- skip it. Concatenating gives the canonical proto3 JSON shape:
  -- defaults dropped; oneofs reduced to at most one entry under
  -- the chosen variant's key.
  entryExps <- traverse (toJSONEntry msgVar) fields
  -- Proto2 extensions live in the message's unknown-fields slot.
  -- If the runtime extension registry has a JSON codec for any of
  -- those slots, surface them as bracket-quoted '[FQN]'-keyed
  -- entries alongside the regular fields.
  let extensionEntries = case mUfSel of
        Nothing -> ListE []
        Just ufN ->
          AppE
            (AppE (VarE 'extEntries) (textLit fqName))
            (dotRead recDot ufN msgVar)
      bodyExp =
        AppE
          (VarE 'PJ.jsonObject)
          ( InfixE
              (Just (AppE (VarE 'concat) (ListE entryExps)))
              (VarE '(<>))
              (Just extensionEntries)
          )
      ctx = case mUfSel of
        Nothing -> []
        Just _ -> [AppT (ConT ''Given) (ConT ''PJExt.ExtensionRegistry)]
  pure $
    InstanceD
      Nothing
      ctx
      (AppT (ConT ''Aeson.ToJSON) (ConT tyName))
      [ FunD
          'Aeson.toJSON
          [Clause [VarP msgVar] (NormalB bodyExp) []]
      ]


{- | Bridge into 'PJExt.extensionEntriesForJson' that lifts the
runtime registry into the [(Text, Aeson.Value)] form
'PJ.jsonObject' wants. INLINE so the splice's call site
collapses cleanly when the message has no unknown fields.
-}
extEntries :: Given PJExt.ExtensionRegistry => Text -> [PD.UnknownField] -> [(Text, Aeson.Value)]
extEntries _ [] = []
extEntries fqn xs = PJExt.extensionEntriesForJson (given :: PJExt.ExtensionRegistry) fqn xs
{-# INLINE extEntries #-}


{- | One field's JSON entries. Returns a 'Q Exp' of type
@[(Text, Aeson.Value)]@: empty when the field is at its default
(or for an unset oneof), one element otherwise.
-}
toJSONEntry :: Name -> MetaField -> Q Exp
toJSONEntry msgVar mf =
  let fieldExpr = readFieldE mf msgVar
      jsonKey = textLit (mfJsonName mf)
      one valE = ListE [TupE [Just jsonKey, Just valE]]
      shape = mfBytesShape mf
      -- Bytes-shaped fields don't have an Aeson.ToJSON instance,
      -- so they short-circuit through the dedicated bytes
      -- helpers in "Proto.Internal.JSON". Empty containers / default
      -- ByteString are still skipped per proto3 canonical-JSON.
      bytesIsNullE :: Q Exp
      bytesIsNullE = case shape of
        SBStrict -> [|BS.null $(pure fieldExpr)|]
        SBLazy -> [|BL.null $(pure fieldExpr)|]
        SBShort -> [|SBS.null $(pure fieldExpr)|]
      bytesToJSONN :: Name
      bytesToJSONN = case shape of
        SBStrict -> 'PJI.protoBytesToJSON
        SBLazy -> 'PJ.protoLazyBytesToJSON
        SBShort -> 'PJ.protoShortBytesToJSON
  in case mfJsonKind mf of
       JKBytes -> case mfKind mf of
         MFKMaybe ->
           -- @Maybe <Bytes>@ carrier (proto2 optional bytes, proto3
           -- explicit-optional bytes): emit when @Just@, skip on
           -- @Nothing@. The bytes shape picks the rep-aware
           -- 'proto*BytesToJSON' helper.
           [|
             case $(pure fieldExpr) of
               Nothing -> []
               Just bs ->
                 [($(pure jsonKey), $(varE bytesToJSONN) bs)]
             |]
         _ ->
           [|
             if $(bytesIsNullE)
               then []
               else
                 $( pure
                      ( one
                          (AppE (VarE bytesToJSONN) fieldExpr)
                      )
                  )
             |]
       JKBytesVector ->
         let toJSONHelper = case shape of
               SBStrict -> VarE 'bytesVectorToJSON
               SBLazy -> VarE 'lazyBytesVectorToJSON
               SBShort -> VarE 'shortBytesVectorToJSON
         in [|
              if V.null $(pure fieldExpr)
                then []
                else $(pure (one (AppE toJSONHelper fieldExpr)))
              |]
       JKBytesList ->
         let toJSONHelper = case shape of
               SBStrict -> VarE 'bytesListToJSON
               SBLazy -> VarE 'lazyBytesListToJSON
               SBShort -> VarE 'shortBytesListToJSON
         in [|
              if null $(pure fieldExpr)
                then []
                else $(pure (one (AppE toJSONHelper fieldExpr)))
              |]
       JKBytesSeq ->
         let toJSONHelper = case shape of
               SBStrict -> VarE 'bytesSeqToJSON
               SBLazy -> VarE 'lazyBytesSeqToJSON
               SBShort -> VarE 'shortBytesSeqToJSON
         in [|
              if Seq.null $(pure fieldExpr)
                then []
                else $(pure (one (AppE toJSONHelper fieldExpr)))
              |]
       JKBytesMap ->
         let mapHelper = case shape of
               SBStrict -> VarE 'PJI.bytesMapFieldToJSON
               SBLazy -> VarE 'PJ.lazyBytesMapFieldToJSON
               SBShort -> VarE 'PJ.shortBytesMapFieldToJSON
         in [|
              if Map.null $(pure fieldExpr)
                then []
                else [$(pure (AppE (AppE mapHelper jsonKey) fieldExpr))]
              |]
       JKNormal -> jsonShapeEntry msgVar mf fieldExpr jsonKey one


{- | The @JKNormal@ arm of 'toJSONEntry' factored out so the
top-level @case mfJsonKind mf of@ tree stays flat.
-}
jsonShapeEntry
  :: Name
  -- ^ message variable
  -> MetaField
  -> Exp
  -- ^ pre-computed @selector msg@ expression
  -> Exp
  -- ^ pre-computed JSON key literal
  -> (Exp -> Exp)
  -- ^ wrap a value into a singleton @[(key, value)]@
  -> Q Exp
jsonShapeEntry _msgVar mf fieldExpr jsonKey one = case mfJsonShape mf of
  JSScalar sc ->
    [|
      if $(scalarIsDefaultE sc fieldExpr)
        then []
        else $(pure (one (scalarToJsonE sc fieldExpr)))
      |]
  JSMaybe sc -> do
    vName <- newName "v"
    [|
      case $(pure fieldExpr) of
        Nothing -> []
        Just $(varP vName) ->
          $(pure (one (scalarToJsonE sc (VarE vName))))
      |]
  JSMessage -> do
    vName <- newName "v"
    [|
      case $(pure fieldExpr) of
        Nothing -> []
        Just $(varP vName) ->
          [($(pure jsonKey), Aeson.toJSON $(varE vName))]
      |]
  JSEnum ->
    [|
      if fromEnum $(pure fieldExpr) == 0
        then []
        else $(pure (one (AppE (VarE 'Aeson.toJSON) fieldExpr)))
      |]
  JSEnumMaybe -> do
    vName <- newName "v"
    [|
      case $(pure fieldExpr) of
        Nothing -> []
        Just $(varP vName) ->
          $(pure (one (AppE (VarE 'Aeson.toJSON) (VarE vName))))
      |]
  JSRepeatedScalar sc -> case mfKind mf of
    MFKList ->
      [|
        if null $(pure fieldExpr)
          then []
          else
            $( pure
                 ( one
                     ( AppE
                         ( AppE
                             (VarE 'scalarListToJSON)
                             (scalarTagE sc)
                         )
                         fieldExpr
                     )
                 )
             )
        |]
    MFKSeq ->
      [|
        if Seq.null $(pure fieldExpr)
          then []
          else
            $( pure
                 ( one
                     ( AppE
                         ( AppE
                             (VarE 'scalarSeqToJSON)
                             (scalarTagE sc)
                         )
                         fieldExpr
                     )
                 )
             )
        |]
    _ ->
      [|
        if V.null $(pure fieldExpr)
          then []
          else
            $( pure
                 ( one
                     ( AppE
                         ( AppE
                             (VarE 'scalarVectorToJSON)
                             (scalarTagE sc)
                         )
                         fieldExpr
                     )
                 )
             )
        |]
  JSRepeatedMessage -> case mfKind mf of
    MFKList ->
      [|
        if null $(pure fieldExpr)
          then []
          else [($(pure jsonKey), Aeson.toJSON $(pure fieldExpr))]
        |]
    MFKSeq ->
      [|
        if Seq.null $(pure fieldExpr)
          then []
          else [($(pure jsonKey), Aeson.toJSON $(pure fieldExpr))]
        |]
    _ ->
      [|
        if V.null $(pure fieldExpr)
          then []
          else [($(pure jsonKey), Aeson.toJSON $(pure fieldExpr))]
        |]
  JSRepeatedEnum -> case mfKind mf of
    MFKList ->
      [|
        if null $(pure fieldExpr)
          then []
          else [($(pure jsonKey), Aeson.toJSON $(pure fieldExpr))]
        |]
    MFKSeq ->
      [|
        if Seq.null $(pure fieldExpr)
          then []
          else [($(pure jsonKey), Aeson.toJSON $(pure fieldExpr))]
        |]
    _ ->
      [|
        if V.null $(pure fieldExpr)
          then []
          else [($(pure jsonKey), Aeson.toJSON $(pure fieldExpr))]
        |]
  JSMapScalar kSc vSc ->
    [|
      if Map.null $(pure fieldExpr)
        then []
        else
          $( pure
               ( one
                   ( AppE
                       ( AppE
                           ( AppE
                               (VarE 'scalarMapToJSON)
                               (scalarTagE kSc)
                           )
                           (scalarTagE vSc)
                       )
                       fieldExpr
                   )
               )
           )
      |]
  JSMapMessage kSc ->
    [|
      if Map.null $(pure fieldExpr)
        then []
        else
          [
            ( $(pure jsonKey)
            , Aeson.toJSON
                ( Map.fromList
                    [ (scalarMapKeyToText $(pure (scalarTagE kSc)) k, Aeson.toJSON v)
                    | (k, v) <- Map.toList $(pure fieldExpr)
                    ]
                )
            )
          ]
      |]
  JSMapEnum kSc ->
    [|
      if Map.null $(pure fieldExpr)
        then []
        else
          [
            ( $(pure jsonKey)
            , Aeson.toJSON
                ( Map.fromList
                    [ (scalarMapKeyToText $(pure (scalarTagE kSc)) k, Aeson.toJSON v)
                    | (k, v) <- Map.toList $(pure fieldExpr)
                    ]
                )
            )
          ]
      |]
  JSOneof variants -> do
    mVar <- newName "mv"
    arms <- traverse oneofVariantArm variants
    let nothingArm =
          Match (ConP 'Nothing [] []) (NormalB (ListE [])) []
        justArm =
          Match
            (ConP 'Just [] [VarP mVar])
            (NormalB (CaseE (VarE mVar) arms))
            []
    pure (CaseE fieldExpr [nothingArm, justArm])

  -- WKT singular: emit the canonical-JSON representation. The
  -- carrier is @Maybe Wkt@; we skip when Nothing (the proto3
  -- spec convention for absent submessages).
  JSWkt wktKind -> do
    vName <- newName "v"
    [|
      case $(pure fieldExpr) of
        Nothing -> []
        Just $(varP vName) ->
          [($(pure jsonKey), $(wktEncoderE wktKind (VarE vName)))]
      |]
  JSWktMaybe wktKind -> do
    vName <- newName "v"
    [|
      case $(pure fieldExpr) of
        Nothing -> []
        Just $(varP vName) ->
          [($(pure jsonKey), $(wktEncoderE wktKind (VarE vName)))]
      |]
  JSWktRepeated wktKind -> do
    [|
      if V.null $(pure fieldExpr)
        then []
        else
          [
            ( $(pure jsonKey)
            , Aeson.Array
                ( V.map
                    $(wktEncoderE1 wktKind)
                    $(pure fieldExpr)
                )
            )
          ]
      |]


-- | Splice for one WKT value: returns an @Aeson.Value@.
wktEncoderE :: WktShape -> Exp -> Q Exp
wktEncoderE wkt e = case wkt of
  WktTimestamp -> [|WK.timestampToJSON $(pure e)|]
  WktDuration -> [|WK.durationToJSON $(pure e)|]
  WktFieldMask -> [|WK.fieldMaskToJSON $(pure e)|]
  WktStruct -> [|WK.structToJSON $(pure e)|]
  WktValue -> [|WK.valueToJSON $(pure e)|]
  WktListValue ->
    [|
      Aeson.Array
        ( V.map
            WK.valueToJSON
            (PGS.listValueValues $(pure e))
        )
      |]
  WktAny -> [|WK.anyToJSON WK.anyJsonRegistry $(pure e)|]
  WktEmpty -> [|WK.emptyToJSON $(pure e)|]
  WktNullValue -> [|WK.nullValueToJSON $(pure e)|]
  WktWrapBool -> [|WK.wrapBoolValue $(pure e)|]
  WktWrapInt32 -> [|WK.wrapInt32Value $(pure e)|]
  WktWrapInt64 -> [|WK.wrapInt64Value $(pure e)|]
  WktWrapUInt32 -> [|WK.wrapUInt32Value $(pure e)|]
  WktWrapUInt64 -> [|WK.wrapUInt64Value $(pure e)|]
  WktWrapFloat -> [|WK.wrapFloatValue $(pure e)|]
  WktWrapDouble -> [|WK.wrapDoubleValue $(pure e)|]
  WktWrapString -> [|WK.wrapStringValue $(pure e)|]
  WktWrapBytes -> [|WK.wrapBytesValue $(pure e)|]


{- | Pointful-style encoder for a single WKT value, used inside
@V.map@ for repeated WKT fields.
-}
wktEncoderE1 :: WktShape -> Q Exp
wktEncoderE1 wkt = case wkt of
  WktTimestamp -> [|WK.timestampToJSON|]
  WktDuration -> [|WK.durationToJSON|]
  WktFieldMask -> [|WK.fieldMaskToJSON|]
  WktStruct -> [|WK.structToJSON|]
  WktValue -> [|WK.valueToJSON|]
  WktListValue ->
    [|
      ( Aeson.Array
          . V.map
            WK.valueToJSON
          . PGS.listValueValues
      )
      |]
  WktAny -> [|WK.anyToJSON WK.anyJsonRegistry|]
  WktEmpty -> [|WK.emptyToJSON|]
  WktNullValue -> [|WK.nullValueToJSON|]
  WktWrapBool -> [|WK.wrapBoolValue|]
  WktWrapInt32 -> [|WK.wrapInt32Value|]
  WktWrapInt64 -> [|WK.wrapInt64Value|]
  WktWrapUInt32 -> [|WK.wrapUInt32Value|]
  WktWrapUInt64 -> [|WK.wrapUInt64Value|]
  WktWrapFloat -> [|WK.wrapFloatValue|]
  WktWrapDouble -> [|WK.wrapDoubleValue|]
  WktWrapString -> [|WK.wrapStringValue|]
  WktWrapBytes -> [|WK.wrapBytesValue|]


-- | One arm of the oneof carrier's case-on-Just.
oneofVariantArm :: OneofVariantJson -> Q Match
oneofVariantArm OneofVariantJson {ovjConstructor = con, ovjJsonKey = key, ovjShape = sh} = do
  vName <- newName "v"
  body <- case sh of
    OVScalar sc -> do
      let valE = scalarToJsonE sc (VarE vName)
      [|[($(pure (textLit key)), $(pure valE))]|]
    OVMessage ->
      [|[($(pure (textLit key)), Aeson.toJSON $(varE vName))]|]
    OVEnum ->
      [|[($(pure (textLit key)), Aeson.toJSON $(varE vName))]|]
    -- Proto3 spec: NullValue serialises to JSON null.
    OVNullValue ->
      [|[($(pure (textLit key)), Aeson.Null)]|]
  pure (Match (ConP con [] [VarP vName]) (NormalB body) [])


{- | Default predicate per scalar kind. Used to suppress fields at
their proto3 default value from JSON output.

'JSBytes' is special: the codegen never actually goes through
here for bytes-typed singular fields ('toJSONEntry' bypasses
'jsonShapeEntry' for those), so the 'BS.null' here only matters
for the (unreachable) catch-all path. The real bytes
default-skip dispatch lives in 'toJSONEntry' and uses
'mfBytesShape' to pick between 'BS.null' / 'BL.null' / 'SBS.null'.
-}
scalarIsDefaultE :: JsonScalar -> Exp -> Q Exp
scalarIsDefaultE sc e = case sc of
  JSBool -> [|not $(pure e)|]
  JSString -> [|T.null $(pure e)|]
  JSBytes -> [|BS.null $(pure e)|]
  JSFloat -> [|($(pure e) :: Float) == 0|]
  JSDouble -> [|($(pure e) :: Double) == 0|]
  _ -> [|$(pure e) == 0|]


{- | Per-scalar JSON encoder. Routes 64-bit ints through the
string-form helpers in "Proto.Internal.JSON", floats through the
NaN/Infinity-aware helpers, bytes through base64.
-}
scalarToJsonE :: JsonScalar -> Exp -> Exp
scalarToJsonE sc e = case sc of
  JSBool -> AppE (VarE 'Aeson.toJSON) e
  JSInt32 -> AppE (VarE 'Aeson.toJSON) e
  JSUInt32 -> AppE (VarE 'Aeson.toJSON) e
  JSSInt32 -> AppE (VarE 'Aeson.toJSON) e
  JSFixed32 -> AppE (VarE 'Aeson.toJSON) e
  JSSFixed32 -> AppE (VarE 'Aeson.toJSON) e
  JSInt64 -> AppE (VarE 'PJ.protoInt64ToJSON) e
  JSUInt64 -> AppE (VarE 'PJ.protoWord64ToJSON) e
  JSSInt64 -> AppE (VarE 'PJ.protoInt64ToJSON) e
  JSFixed64 -> AppE (VarE 'PJ.protoWord64ToJSON) e
  JSSFixed64 -> AppE (VarE 'PJ.protoInt64ToJSON) e
  JSFloat -> AppE (VarE 'PJ.protoFloatToJSON) e
  JSDouble -> AppE (VarE 'PJ.protoDoubleToJSON) e
  JSString -> AppE (VarE 'Aeson.toJSON) e
  JSBytes -> AppE (VarE 'PJ.protoBytesToJSON) e


{- | Splice the 'JsonScalar' constructor as a value-level tag the
runtime helpers ('scalarVectorToJSON' / 'scalarMapToJSON') can
pattern-match on.
-}
scalarTagE :: JsonScalar -> Exp
scalarTagE = \case
  JSBool -> ConE 'JSBool
  JSInt32 -> ConE 'JSInt32
  JSUInt32 -> ConE 'JSUInt32
  JSSInt32 -> ConE 'JSSInt32
  JSFixed32 -> ConE 'JSFixed32
  JSSFixed32 -> ConE 'JSSFixed32
  JSInt64 -> ConE 'JSInt64
  JSUInt64 -> ConE 'JSUInt64
  JSSInt64 -> ConE 'JSSInt64
  JSFixed64 -> ConE 'JSFixed64
  JSSFixed64 -> ConE 'JSSFixed64
  JSFloat -> ConE 'JSFloat
  JSDouble -> ConE 'JSDouble
  JSString -> ConE 'JSString
  JSBytes -> ConE 'JSBytes


mkFromJSONForMessage
  :: Name -> Text -> Maybe Name -> Name -> Bool -> [MetaField] -> Q Dec
mkFromJSONForMessage tyName fqName mUfSel defName recDot fields = do
  objVar <- newName "obj"
  fldNames <- mapM (\mf -> (,) mf <$> newName ("fld_" ++ nameBase (mfSelector mf))) fields
  binds <- traverse (uncurry (parseBindStmt objVar)) fldNames
  let assigns = fmap (uncurry (fromJSONAssign defName)) fldNames
      -- Build the record-update target. For empty messages we
      -- can't use 'RecUpdE def []' (GHC rejects it as "Empty
      -- record update"), so fall back to the bare default.
      -- Under the unprefixed layout a record update whose fields recur
      -- across messages is ambiguous; pin it with a type annotation.
      baseE
        | null assigns = VarE defName
        | recDot = SigE (RecUpdE (VarE defName) assigns) (ConT tyName)
        | otherwise = RecUpdE (VarE defName) assigns
      typeNameLit = LitE (StringL (nameBase tyName))
  case mUfSel of
    Nothing -> do
      let bodyDo = DoE Nothing (binds ++ [NoBindS (AppE (VarE 'pure) baseE)])
          body =
            AppE
              (AppE (VarE 'Aeson.withObject) typeNameLit)
              (LamE [VarP objVar] bodyDo)
      pure $
        InstanceD
          Nothing
          []
          (AppT (ConT ''Aeson.FromJSON) (ConT tyName))
          [FunD 'Aeson.parseJSON [Clause [] (NormalB body) []]]
    Just ufN -> do
      -- Proto2 extension JSON: drain any '[FQN]'-keyed entries
      -- from the input object into the message's unknown-fields
      -- slot via the runtime registry. We special-case the
      -- empty-list result so the common path (no registered
      -- extensions for this message type) doesn't pay for an
      -- extra record update.
      withExtVar <- newName "withExt"
      baseVar <- newName "base"
      let extDrainE =
            AppE
              (AppE (VarE 'extDrain) (textLit fqName))
              (VarE objVar)
          ufFieldUpdate ufs =
            RecUpdE
              (VarE baseVar)
              [
                ( ufN
                , AppE
                    ( AppE
                        (VarE '(<>))
                        (dotRead recDot ufN baseVar)
                    )
                    ufs
                )
              ]
          finalE =
            -- @let !base = baseE in case extDrain ... of ...@.
            -- Sharing 'base' across the empty-extensions
            -- short-circuit and the record-update path lets
            -- GHC float the per-field updates out of the loop
            -- when 'extDrain' returns Right [].
            LetE
              [ValD (BangP (VarP baseVar)) (NormalB baseE) []]
              ( CaseE
                  extDrainE
                  [ Match
                      (ConP 'Right [] [ConP '[] [] []])
                      (NormalB (AppE (VarE 'pure) (VarE baseVar)))
                      []
                  , Match
                      (ConP 'Right [] [VarP withExtVar])
                      ( NormalB
                          ( AppE
                              (VarE 'pure)
                              (ufFieldUpdate (VarE withExtVar))
                          )
                      )
                      []
                  , Match
                      (ConP 'Left [] [VarP withExtVar])
                      (NormalB (AppE (VarE 'fail) (VarE withExtVar)))
                      []
                  ]
              )
          bodyDo = DoE Nothing (binds ++ [NoBindS finalE])
          body =
            AppE
              (AppE (VarE 'Aeson.withObject) typeNameLit)
              (LamE [VarP objVar] bodyDo)
      pure $
        InstanceD
          Nothing
          [AppT (ConT ''Given) (ConT ''PJExt.ExtensionRegistry)]
          (AppT (ConT ''Aeson.FromJSON) (ConT tyName))
          [FunD 'Aeson.parseJSON [Clause [] (NormalB body) []]]


{- | Walk every key in the JSON object: if it's bracket-quoted
('[FQN]') and the registry has a codec, parse the value into
an 'UnknownField'. Plain field keys are ignored — they were
already consumed by the per-field parsers.

Fast-path: when the registry has no extensions for this
parent (the typical case for proto3 messages and any proto2
message without an @extend@ block), bypass the per-key walk
entirely.
-}
extDrain
  :: Given PJExt.ExtensionRegistry => Text -> Aeson.Object -> Either String [PD.UnknownField]
extDrain parentFqn obj
  | not (PJExt.parentHasExtensions reg parentFqn) = Right []
  | otherwise =
      let go acc (k, v) = case PJExt.parseExtensionEntry reg parentFqn k v of
            Nothing -> Right acc
            Just (Right uf) -> Right (uf : acc)
            Just (Left e) -> Left e
      in case foldlEither go [] (AesonKM.toList obj) of
           Right xs -> Right (reverse xs)
           Left e -> Left e
  where
    reg = given :: PJExt.ExtensionRegistry
    foldlEither _ z [] = Right z
    foldlEither f z (x : xs) = case f z x of
      Right z' -> foldlEither f z' xs
      Left e -> Left e
{-# INLINE extDrain #-}


parseBindStmt :: Name -> MetaField -> Name -> Q Stmt
parseBindStmt objVar mf fldVar = case mfJsonShape mf of
  -- Proto3 oneof variants live at the top level of the JSON
  -- object — each variant under its own JSON key, NOT nested
  -- under the oneof field name. Dispatch through a custom
  -- runtime helper (parseOneofVariants) so we can scan the
  -- object for any of the variant keys, validate "at most one"
  -- and route to the right variant constructor.
  JSOneof variants -> do
    e <- buildOneofParseExp objVar variants
    pure (BindS (VarP fldVar) e)
  _ ->
    pure
      ( if mfJsonName mf == mfProtoName mf
          then parseBindStmtSingleKey objVar mf fldVar (mfJsonName mf)
          else parseBindStmtTwoKeys objVar mf fldVar (mfJsonName mf) (mfProtoName mf)
      )


{- | Parse @Maybe a@ from @obj@, trying both the camelCase JSON
key and the snake_case proto-original name. Per proto3 spec
the JSON reader SHOULD accept both forms.
-}
parseBindStmtTwoKeys :: Name -> MetaField -> Name -> Text -> Text -> Stmt
parseBindStmtTwoKeys objVar mf fldVar jsonKey snakeKey =
  let parseFn = parseFnFor mf
      callJson = AppE (AppE parseFn (VarE objVar)) (textLit jsonKey)
      callSnake = AppE (AppE parseFn (VarE objVar)) (textLit snakeKey)
      -- Try @parseFnFor mf obj <jsonKey>@; if that yielded
      -- 'Nothing' (= JSON object had no such key) AND the
      -- snake-case form differs, also try @parseFnFor mf obj
      -- <snakeKey>@. This matches the proto3 spec's "accept both
      -- forms on input" rule. We prefer the camelCase form when
      -- both keys are present (proto3 canonical form).
      e =
        InfixE
          (Just callJson)
          (VarE '(>>=))
          ( Just
              ( LamE
                  [VarP fldVar]
                  ( CaseE
                      (VarE fldVar)
                      [ Match
                          (ConP 'Just [] [WildP])
                          (NormalB (AppE (VarE 'pure) (VarE fldVar)))
                          []
                      , Match
                          (ConP 'Nothing [] [])
                          (NormalB callSnake)
                          []
                      ]
                  )
              )
          )
  in BindS (VarP fldVar) e


{- | The single-key parsing path used when the JSON key matches
the proto-side name (no snake_case fallback needed).
-}
parseBindStmtSingleKey :: Name -> MetaField -> Name -> Text -> Stmt
parseBindStmtSingleKey objVar mf fldVar key =
  BindS
    (VarP fldVar)
    (AppE (AppE (parseFnFor mf) (VarE objVar)) (textLit key))


{- | Pick the right parser helper for a 'MetaField'. Factored out
of 'parseBindStmt' so both the single- and two-key paths can
share the same dispatch table.
-}
parseFnFor :: MetaField -> Exp
parseFnFor = oldParseFnFor


-- | Renamed inline of the original 'parseBindStmt' helper logic.
oldParseFnFor :: MetaField -> Exp
oldParseFnFor mf =
  case mfJsonKind mf of
    JKBytes -> case (mfKind mf, mfBytesShape mf) of
      (MFKMaybe, SBStrict) -> VarE 'parseBytesMaybeFieldMaybe
      (MFKMaybe, SBLazy) -> VarE 'parseLazyBytesMaybeFieldMaybe
      (MFKMaybe, SBShort) -> VarE 'parseShortBytesMaybeFieldMaybe
      (_, SBStrict) -> VarE 'PJ.parseBytesFieldMaybe
      (_, SBLazy) -> VarE 'PJ.parseLazyBytesFieldMaybe
      (_, SBShort) -> VarE 'PJ.parseShortBytesFieldMaybe
    JKBytesMap -> case mfBytesShape mf of
      SBStrict -> VarE 'PJ.parseBytesMapFieldMaybe
      SBLazy -> VarE 'PJ.parseLazyBytesMapFieldMaybe
      SBShort -> VarE 'PJ.parseShortBytesMapFieldMaybe
    JKBytesVector -> case mfBytesShape mf of
      SBStrict -> VarE 'parseBytesVectorMaybe
      SBLazy -> VarE 'parseLazyBytesVectorMaybe
      SBShort -> VarE 'parseShortBytesVectorMaybe
    JKBytesList -> case mfBytesShape mf of
      SBStrict -> VarE 'parseBytesListMaybe
      SBLazy -> VarE 'parseLazyBytesListMaybe
      SBShort -> VarE 'parseShortBytesListMaybe
    JKBytesSeq -> case mfBytesShape mf of
      SBStrict -> VarE 'parseBytesSeqMaybe
      SBLazy -> VarE 'parseLazyBytesSeqMaybe
      SBShort -> VarE 'parseShortBytesSeqMaybe
    JKNormal -> case mfJsonShape mf of
      -- WKT singular: dispatch through proto3-canonical
      -- parser (RFC 3339 timestamps, "1.5s" durations,
      -- bare-value wrappers, etc.). The parser fails when
      -- the JSON shape doesn't match the WKT contract.
      JSWkt w -> wktParserName w
      JSWktMaybe w -> wktParserName w
      JSWktRepeated w -> wktVectorParserName w
      -- Proto3 canonical JSON: 64-bit ints come in as
      -- strings; 32-bit ints accept both numbers and
      -- strings; floats accept numbers, NaN/Infinity
      -- strings, and arbitrary numeric strings.
      JSScalar JSInt64 -> VarE 'parseInt64FieldMaybe
      JSScalar JSSInt64 -> VarE 'parseInt64FieldMaybe
      JSScalar JSSFixed64 -> VarE 'parseInt64FieldMaybe
      JSScalar JSUInt64 -> VarE 'parseWord64FieldMaybe
      JSScalar JSFixed64 -> VarE 'parseWord64FieldMaybe
      JSScalar JSInt32 -> VarE 'parseInt32FieldMaybe
      JSScalar JSSInt32 -> VarE 'parseInt32FieldMaybe
      JSScalar JSSFixed32 -> VarE 'parseInt32FieldMaybe
      JSScalar JSUInt32 -> VarE 'parseWord32FieldMaybe
      JSScalar JSFixed32 -> VarE 'parseWord32FieldMaybe
      JSScalar JSDouble -> VarE 'parseDoubleFieldMaybe
      JSScalar JSFloat -> VarE 'parseFloatFieldMaybe
      -- @JSMaybe scalar@: a scalar field whose carrier is
      -- @Maybe T@ (proto2 explicit-optional, proto3 explicit
      -- @optional@ with presence tracking, etc.). The parser
      -- has to return @Maybe (Maybe T)@ — outer @Maybe@ for
      -- "key present", inner @Maybe@ for the field value
      -- (@null@ → @Nothing@) — so 'fromJSONAssign' can
      -- @maybe def id@ correctly.
      JSMaybe JSInt64 -> VarE 'parseInt64MaybeFieldMaybe
      JSMaybe JSSInt64 -> VarE 'parseInt64MaybeFieldMaybe
      JSMaybe JSSFixed64 -> VarE 'parseInt64MaybeFieldMaybe
      JSMaybe JSUInt64 -> VarE 'parseWord64MaybeFieldMaybe
      JSMaybe JSFixed64 -> VarE 'parseWord64MaybeFieldMaybe
      JSMaybe JSInt32 -> VarE 'parseInt32MaybeFieldMaybe
      JSMaybe JSSInt32 -> VarE 'parseInt32MaybeFieldMaybe
      JSMaybe JSSFixed32 -> VarE 'parseInt32MaybeFieldMaybe
      JSMaybe JSUInt32 -> VarE 'parseWord32MaybeFieldMaybe
      JSMaybe JSFixed32 -> VarE 'parseWord32MaybeFieldMaybe
      JSMaybe JSDouble -> VarE 'parseDoubleMaybeFieldMaybe
      JSMaybe JSFloat -> VarE 'parseFloatMaybeFieldMaybe
      JSMaybe JSBool -> VarE 'parseBoolMaybeFieldMaybe
      JSMaybe JSString -> VarE 'parseStringMaybeFieldMaybe
      -- map<K, message V>: parse via a custom Map walker so
      -- the nested message FromJSON instance is exercised
      -- explicitly. Aeson's generic Map FromJSON instance
      -- has a habit of falling back to an empty result when
      -- the inner parse fails partially; the explicit walker
      -- propagates failures cleanly.
      JSMapMessage _ -> VarE 'parseStringMessageMapMaybe
      -- Singular / repeated / map enum fields: route through
      -- the lenient-mode-aware helpers so the conformance
      -- suite's JSON_IGNORE_UNKNOWN_PARSING_TEST category
      -- silently drops unknown enum strings.
      JSEnum -> VarE 'parseEnumFieldMaybe
      -- 'Maybe Enum' carrier (proto2 optional enum, proto3
      -- explicit-optional enum). 'parseEnumFieldMaybeMaybe'
      -- distinguishes "key absent" from "key present, value
      -- null" so the round-trip preserves presence.
      JSEnumMaybe -> VarE 'parseEnumFieldMaybeMaybe
      JSRepeatedEnum -> VarE 'parseEnumVectorMaybe
      JSMapEnum _ -> VarE 'parseStringEnumMapMaybe
      _ -> VarE 'PJ.parseFieldMaybe


-- | Parse @Maybe Int64@ from a JSON string-or-number key.
parseInt64FieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe Int64)
parseInt64FieldMaybe = parseScalarFieldMaybe PJI.protoInt64FromJSON


parseWord64FieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe Word64)
parseWord64FieldMaybe = parseScalarFieldMaybe PJI.protoWord64FromJSON


parseDoubleFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe Double)
parseDoubleFieldMaybe = parseScalarFieldMaybe (protoFloatFromJSONLenient @Double)


parseFloatFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe Float)
parseFloatFieldMaybe = parseScalarFieldMaybe (protoFloatFromJSONLenient @Float)


{- | Lenient float\/double parser specialised by 'RealFloat'
carrier. Accepts JSON numbers, the @"NaN"@\/@"Infinity"@\/
@"-Infinity"@ string sentinels, and any other numeric string
(proto3 canonical-JSON allows quoted floats on input).

Out-of-range checking: a finite 'Sci.Scientific' that
'realToFrac's to @Infinity@ in the target type is rejected,
which is what conformance @Float\/DoubleField{TooLarge,TooSmall}@
assert on. (NaN\/Infinity sentinels still flow through.)
-}
protoFloatFromJSONLenient
  :: forall a. (RealFloat a) => Aeson.Value -> AesonT.Parser a
protoFloatFromJSONLenient v = case v of
  Aeson.Number n -> finite n
  Aeson.String "NaN" -> pure (0 / 0)
  Aeson.String "Infinity" -> pure (1 / 0)
  Aeson.String "-Infinity" -> pure (negate (1 / 0))
  Aeson.String s -> sciFromText32 s >>= finite
  _ -> fail "Expected JSON Number or numeric String"
  where
    finite :: Sci.Scientific -> AesonT.Parser a
    finite n =
      let d = Sci.toRealFloat n :: a
      in if isInfinite d
           then fail ("float/double overflow: " <> show n)
           else pure d


{- | Parse @Maybe (Map Text v)@ where @v@ is a generated
submessage. The inner @parseJSON@ is the message's own
FromJSON instance; we walk the JSON object explicitly so a
single failing entry surfaces as a parse error rather than
being silently dropped.
---------------------------------------------------------------------------
Lenient unknown-enum mode
---------------------------------------------------------------------------
-}

{- | When set to 'True', the generated enum @parseJSON@ parsers
swallow unknown string values rather than failing. This
mirrors the proto3 conformance suite's
@JSON_IGNORE_UNKNOWN_PARSING_TEST@ category, which
intentionally feeds JSON containing enum strings outside the
declared set and expects them to be silently dropped.
-}
{-# NOINLINE lenientUnknownEnumRef #-}
lenientUnknownEnumRef :: IORef Bool
lenientUnknownEnumRef = unsafePerformIO (newIORef False)


-- | Set the global lenient-mode flag for unknown enum values in JSON parsing.
setLenientUnknownEnum :: Bool -> IO ()
setLenientUnknownEnum = writeIORef lenientUnknownEnumRef


{- | Read the current lenient-mode flag. The 'IORef' itself is
the cache-busting argument: passing it explicitly defeats
GHC's CSE / common-subexpression-elimination, which would
otherwise memoise @unsafePerformIO (readIORef _)@ to the
first observed value.
-}
isLenientUnknownEnum :: IORef Bool -> Bool
isLenientUnknownEnum ref = unsafePerformIO (readIORef ref)
{-# NOINLINE isLenientUnknownEnum #-}


{- | Parse a singular optional enum field that defaults to its
zero value when the JSON either omits the field or carries
an unknown enum string AND the runtime is in lenient mode.
-}
parseEnumFieldMaybeMaybe
  :: forall a
   . (Aeson.FromJSON a)
  => Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe a))
parseEnumFieldMaybeMaybe obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure (Just Nothing)
    Just v ->
      AesonT.parserCatchError
        (Just . Just <$> Aeson.parseJSON v)
        ( \_ msg ->
            if isUnknownEnumFail msg && isLenientUnknownEnum lenientUnknownEnumRef
              then pure Nothing
              else fail msg
        )


parseEnumFieldMaybe
  :: forall a
   . (Aeson.FromJSON a)
  => Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe a)
parseEnumFieldMaybe obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just v ->
      AesonT.parserCatchError
        (Just <$> Aeson.parseJSON v)
        ( \_ msg ->
            if isUnknownEnumFail msg && isLenientUnknownEnum lenientUnknownEnumRef
              then pure Nothing
              else fail msg
        )


{- | Parse a repeated enum field. Unknown-enum elements are
dropped from the result vector when lenient mode is on, kept
(as parse errors) otherwise.
-}
parseEnumVectorMaybe
  :: forall a
   . (Aeson.FromJSON a)
  => Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (V.Vector a))
parseEnumVectorMaybe obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just (Aeson.Array vs) -> do
      xs <- traverse parseOne (V.toList vs)
      pure (Just (V.fromList (Data.Maybe.catMaybes xs)))
    Just _ -> fail ("Expected JSON Array for enum field " <> show key)
  where
    -- 'null' as an array element means "unknown enum value
    -- in lenient JSON mode" (the conformance handler rewrites
    -- the sentinel @"UNKNOWN_ENUM_VALUE"@ string to @null@
    -- when the test_category is 'JSON_IGNORE_UNKNOWN_PARSING_TEST').
    parseOne Aeson.Null = pure Nothing
    parseOne v =
      AesonT.parserCatchError
        (Just <$> Aeson.parseJSON v)
        ( \_ msg ->
            if isUnknownEnumFail msg && isLenientUnknownEnum lenientUnknownEnumRef
              then pure Nothing
              else fail msg
        )


{- | Parse a @map<string, Enum>@ field. Unknown-enum entries are
dropped when lenient mode is on; treated as parse errors
otherwise.
-}
parseStringEnumMapMaybe
  :: forall a
   . (Aeson.FromJSON a)
  => Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Map.Map Text a))
parseStringEnumMapMaybe obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just (Aeson.Object inner) -> do
      pairs <- traverse parseEntry (AesonKM.toList inner)
      pure (Just (Map.fromList [(k, v) | (k, Just v) <- pairs]))
    Just _ -> fail ("Expected JSON Object for map field " <> show key)
  where
    parseEntry (k, Aeson.Null) = pure (AesonKey.toText k, Nothing)
    parseEntry (k, v) = do
      mv <-
        AesonT.parserCatchError
          (Just <$> Aeson.parseJSON v)
          ( \_ msg ->
              if isUnknownEnumFail msg && isLenientUnknownEnum lenientUnknownEnumRef
                then pure Nothing
                else fail msg
          )
      pure (AesonKey.toText k, mv)


parseStringMessageMapMaybe
  :: Aeson.FromJSON v
  => Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Map.Map Text v))
parseStringMessageMapMaybe obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just (Aeson.Object inner) -> do
      pairs <- traverse parseEntry (AesonKM.toList inner)
      pure (Just (Map.fromList pairs))
    Just _ ->
      fail ("Expected JSON Object for map field " <> show key)
  where
    parseEntry (k, v) = do
      v' <- Aeson.parseJSON v
      pure (AesonKey.toText k, v')


parseInt32FieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe Int32)
parseInt32FieldMaybe = parseScalarFieldMaybe protoInt32FromJSON


parseWord32FieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe Word32)
parseWord32FieldMaybe = parseScalarFieldMaybe protoWord32FromJSON


-- ---------------------------------------------------------------------------
-- @JSMaybe scalar@ helpers (Maybe-carriered presence-tracking scalars)
-- ---------------------------------------------------------------------------

{- | Helper: 'parseScalarMaybeMaybe' lifts a per-scalar @Aeson.Value
-> Parser a@ into the @Maybe (Maybe a)@ shape required by
'fromJSONAssign' for fields whose Haskell carrier is @Maybe a@
(proto2 'optional', proto3 explicit 'optional').
-}
parseScalarMaybeMaybe
  :: (Aeson.Value -> AesonT.Parser a)
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe a))
parseScalarMaybeMaybe parser obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure (Just Nothing)
    Just v -> Just . Just <$> parser v
{-# INLINE parseScalarMaybeMaybe #-}


parseInt32MaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Int32))
parseInt32MaybeFieldMaybe = parseScalarMaybeMaybe protoInt32FromJSON


parseWord32MaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Word32))
parseWord32MaybeFieldMaybe = parseScalarMaybeMaybe protoWord32FromJSON


parseInt64MaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Int64))
parseInt64MaybeFieldMaybe = parseScalarMaybeMaybe PJI.protoInt64FromJSON


parseWord64MaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Word64))
parseWord64MaybeFieldMaybe = parseScalarMaybeMaybe PJI.protoWord64FromJSON


parseFloatMaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Float))
parseFloatMaybeFieldMaybe = parseScalarMaybeMaybe (protoFloatFromJSONLenient @Float)


parseDoubleMaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Double))
parseDoubleMaybeFieldMaybe = parseScalarMaybeMaybe (protoFloatFromJSONLenient @Double)


parseBoolMaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Bool))
parseBoolMaybeFieldMaybe = parseScalarMaybeMaybe Aeson.parseJSON


parseStringMaybeFieldMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Text))
parseStringMaybeFieldMaybe = parseScalarMaybeMaybe Aeson.parseJSON


parseBytesMaybeFieldMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe BS.ByteString))
parseBytesMaybeFieldMaybe = parseScalarMaybeMaybe PJ.protoBytesFromJSON


-- Proto3 canonical-JSON spec rejects: out-of-range, fractional
-- (e.g. @1.5@), and unparsable strings for {int32, uint32}.
-- The conformance suite covers all three categories.
protoInt32FromJSON :: Aeson.Value -> AesonT.Parser Int32
protoInt32FromJSON v = case v of
  Aeson.Number n -> bounded32 "int32" n
  Aeson.String s -> sciFromText32 s >>= bounded32 "int32"
  _ -> fail "Expected JSON Number or String for Int32"


protoWord32FromJSON :: Aeson.Value -> AesonT.Parser Word32
protoWord32FromJSON v = case v of
  Aeson.Number n -> bounded32 "uint32" n
  Aeson.String s -> sciFromText32 s >>= bounded32 "uint32"
  _ -> fail "Expected JSON Number or String for UInt32"


{- | Parse 'Scientific' from a JSON-quoted numeric string. We
can't reuse 'PJ.sciFromText' without adding a Proto.Internal.JSON
dependency edge, so duplicate the trivial implementation.
-}
sciFromText32 :: Text -> AesonT.Parser Sci.Scientific
sciFromText32 t
  | hasLeadingWs t = fail ("Invalid numeric string (leading whitespace): " <> show t)
  | otherwise = case reads (T.unpack t) :: [(Sci.Scientific, String)] of
      [(s, "")] -> pure s
      _ -> fail ("Invalid numeric string: " <> show t)
  where
    hasLeadingWs s = case T.uncons s of
      Just (c, _) -> c == ' ' || c == '\t' || c == '\n' || c == '\r'
      Nothing -> True


{- | Bounded-integer narrowing for 32-bit fields. Mirrors
'Proto.Internal.JSON.boundedFromSci' but lives here so the TH-spliced
decoders don't have to drag in 'Proto.Internal.JSON' transitively.
-}
bounded32 :: forall i. (Integral i, Bounded i) => String -> Sci.Scientific -> AesonT.Parser i
bounded32 ty s = case Sci.toBoundedInteger s of
  Just n -> pure n
  Nothing -> fail (ty <> " value out of range or non-integer: " <> show s)


{- | Generic helper: parse @Maybe a@ via a per-scalar @Aeson.Value
-> Parser a@ helper. Returns 'Nothing' for missing or null.
-}
parseScalarFieldMaybe
  :: (Aeson.Value -> AesonT.Parser a)
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe a)
parseScalarFieldMaybe parser obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just v -> Just <$> parser v
{-# INLINE parseScalarFieldMaybe #-}


-- ---------------------------------------------------------------------------
-- WKT parser splice helpers
-- ---------------------------------------------------------------------------

{- | Per-WKT 'parseFieldMaybe' helper name. Each helper parses
@Maybe a@ from the JSON object key, applying the WKT's
proto3-canonical parser to the raw 'Aeson.Value'.
-}
wktParserName :: WktShape -> Exp
wktParserName w = case w of
  WktTimestamp -> VarE 'parseTimestampMaybe
  WktDuration -> VarE 'parseDurationMaybe
  WktFieldMask -> VarE 'parseFieldMaskMaybe
  WktStruct -> VarE 'parseStructMaybe
  WktValue -> VarE 'parseValueMaybe
  WktListValue -> VarE 'parseListValueMaybe
  WktAny -> VarE 'parseAnyMaybe
  WktEmpty -> VarE 'parseEmptyMaybe
  WktNullValue -> VarE 'parseNullValueMaybe
  WktWrapBool -> VarE 'parseBoolWrapperMaybe
  WktWrapInt32 -> VarE 'parseInt32WrapperMaybe
  WktWrapInt64 -> VarE 'parseInt64WrapperMaybe
  WktWrapUInt32 -> VarE 'parseUInt32WrapperMaybe
  WktWrapUInt64 -> VarE 'parseUInt64WrapperMaybe
  WktWrapFloat -> VarE 'parseFloatWrapperMaybe
  WktWrapDouble -> VarE 'parseDoubleWrapperMaybe
  WktWrapString -> VarE 'parseStringWrapperMaybe
  WktWrapBytes -> VarE 'parseBytesWrapperMaybe


-- | Per-WKT @Maybe (Vector a)@ parser name for repeated fields.
wktVectorParserName :: WktShape -> Exp
wktVectorParserName w = case w of
  WktTimestamp -> VarE 'parseTimestampVectorMaybe
  WktDuration -> VarE 'parseDurationVectorMaybe
  WktFieldMask -> VarE 'parseFieldMaskVectorMaybe
  WktStruct -> VarE 'parseStructVectorMaybe
  WktValue -> VarE 'parseValueVectorMaybe
  WktListValue -> VarE 'parseListValueVectorMaybe
  WktAny -> VarE 'parseAnyVectorMaybe
  WktEmpty -> VarE 'parseEmptyVectorMaybe
  WktNullValue -> VarE 'parseNullValueVectorMaybe
  WktWrapBool -> VarE 'parseBoolWrapperVectorMaybe
  WktWrapInt32 -> VarE 'parseInt32WrapperVectorMaybe
  WktWrapInt64 -> VarE 'parseInt64WrapperVectorMaybe
  WktWrapUInt32 -> VarE 'parseUInt32WrapperVectorMaybe
  WktWrapUInt64 -> VarE 'parseUInt64WrapperVectorMaybe
  WktWrapFloat -> VarE 'parseFloatWrapperVectorMaybe
  WktWrapDouble -> VarE 'parseDoubleWrapperVectorMaybe
  WktWrapString -> VarE 'parseStringWrapperVectorMaybe
  WktWrapBytes -> VarE 'parseBytesWrapperVectorMaybe


-- ---------------------------------------------------------------------------
-- WKT parsers (singular)
-- ---------------------------------------------------------------------------

parseTimestampMaybe
  :: Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Timestamp.Timestamp))
parseTimestampMaybe = parseWktMaybe WK.timestampFromJSON


parseDurationMaybe
  :: Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Duration.Duration))
parseDurationMaybe = parseWktMaybe WK.durationFromJSON


parseFieldMaskMaybe
  :: Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.FieldMask.FieldMask))
parseFieldMaskMaybe = parseWktMaybe WK.fieldMaskFromJSON


parseStructMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe PGS.Struct))
parseStructMaybe = parseWktMaybe WK.structFromJSON


{- | Proto3 'google.protobuf.Value' parses JSON @null@ as the
@null_value: NULL_VALUE@ variant rather than treating @null@
as "field unset" (which is the convention for every /other/
WKT). 'parseWktMaybe' is too eager about returning
@Just Nothing@ for @null@; supply a tailored parser instead.
-}
parseValueMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe PGS.Value))
parseValueMaybe obj key =
  case AesonKM.lookup (AesonKey.fromText key) obj of
    Nothing -> pure Nothing
    Just v -> case WK.valueFromJSON v of
      Right a -> pure (Just (Just a))
      Left e -> fail e


parseListValueMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe PGS.ListValue))
parseListValueMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure (Just Nothing)
    Just (Aeson.Array vs) ->
      let !items = V.map jsonToValueViaWk vs
      in pure (Just (Just PGS.defaultListValue {PGS.listValueValues = items}))
    Just _ ->
      fail "Expected JSON array for ListValue"
  where
    jsonToValueViaWk v = case WK.valueFromJSON v of
      Right val -> val
      Left _ -> PGS.defaultValue


parseAnyMaybe
  :: Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Any.Any))
parseAnyMaybe = parseWktMaybe (WK.anyFromJSON WK.anyJsonRegistry)


parseEmptyMaybe
  :: Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Empty.Empty))
parseEmptyMaybe = parseWktMaybe WK.emptyFromJSON


parseNullValueMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe PGS.NullValue))
parseNullValueMaybe = parseWktMaybe WK.nullValueFromJSON


parseBoolWrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.BoolValue))
parseBoolWrapperMaybe = parseWktMaybe WK.unwrapBoolValue


parseInt32WrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.Int32Value))
parseInt32WrapperMaybe = parseWktMaybe WK.unwrapInt32Value


parseInt64WrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.Int64Value))
parseInt64WrapperMaybe = parseWktMaybe WK.unwrapInt64Value


parseUInt32WrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.UInt32Value))
parseUInt32WrapperMaybe = parseWktMaybe WK.unwrapUInt32Value


parseUInt64WrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.UInt64Value))
parseUInt64WrapperMaybe = parseWktMaybe WK.unwrapUInt64Value


parseFloatWrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.FloatValue))
parseFloatWrapperMaybe = parseWktMaybe WK.unwrapFloatValue


parseDoubleWrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.DoubleValue))
parseDoubleWrapperMaybe = parseWktMaybe WK.unwrapDoubleValue


parseStringWrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.StringValue))
parseStringWrapperMaybe = parseWktMaybe WK.unwrapStringValue


parseBytesWrapperMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe Proto.Google.Protobuf.Wrappers.BytesValue))
parseBytesWrapperMaybe = parseWktMaybe WK.unwrapBytesValue


{- | Generic helper: parse @Maybe (Maybe a)@ via a per-WKT
@Aeson.Value -> Either String a@ helper.

The outer 'Maybe' is the parser-success indicator — 'Nothing'
when the JSON object doesn't contain the key. The inner 'Maybe'
mirrors the singular-WKT field's type (since 'loadProto' wraps
singular submessage fields in 'Maybe' per the proto3 implicit-
optional convention). When the JSON has the key with value
@null@ we report @Just Nothing@ (the field was set to absent
explicitly); otherwise we run the parser and report
@Just (Just x)@.
-}
parseWktMaybe
  :: (Aeson.Value -> Either String a)
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Maybe a))
parseWktMaybe parser obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure (Just Nothing)
    Just v -> case parser v of
      Right a -> pure (Just (Just a))
      Left e -> fail e


-- ---------------------------------------------------------------------------
-- WKT vector parsers (repeated fields)
-- ---------------------------------------------------------------------------

parseTimestampVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Timestamp.Timestamp))
parseTimestampVectorMaybe = parseWktVectorMaybe WK.timestampFromJSON


parseDurationVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Duration.Duration))
parseDurationVectorMaybe = parseWktVectorMaybe WK.durationFromJSON


parseFieldMaskVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.FieldMask.FieldMask))
parseFieldMaskVectorMaybe = parseWktVectorMaybe WK.fieldMaskFromJSON


parseStructVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector PGS.Struct))
parseStructVectorMaybe = parseWktVectorMaybe WK.structFromJSON


parseValueVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector PGS.Value))
parseValueVectorMaybe = parseWktVectorMaybe WK.valueFromJSON


parseListValueVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector PGS.ListValue))
parseListValueVectorMaybe obj key = do
  -- repeated ListValue is unusual; just fall back to per-element
  -- parsing using the same helper that handles a singular ListValue.
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just (Aeson.Array vs) ->
      let !lvs =
            V.map
              ( \case
                  Aeson.Array inner ->
                    PGS.defaultListValue
                      { PGS.listValueValues =
                          V.map
                            (fromRight PGS.defaultValue . WK.valueFromJSON)
                            inner
                      }
                  _ -> PGS.defaultListValue
              )
              vs
      in pure (Just lvs)
    Just _ -> fail "Expected JSON array for repeated ListValue"


parseAnyVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Any.Any))
parseAnyVectorMaybe = parseWktVectorMaybe (WK.anyFromJSON WK.anyJsonRegistry)


parseEmptyVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Empty.Empty))
parseEmptyVectorMaybe = parseWktVectorMaybe WK.emptyFromJSON


parseNullValueVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector PGS.NullValue))
parseNullValueVectorMaybe = parseWktVectorMaybe WK.nullValueFromJSON


parseBoolWrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.BoolValue))
parseBoolWrapperVectorMaybe = parseWktVectorMaybe WK.unwrapBoolValue


parseInt32WrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.Int32Value))
parseInt32WrapperVectorMaybe = parseWktVectorMaybe WK.unwrapInt32Value


parseInt64WrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.Int64Value))
parseInt64WrapperVectorMaybe = parseWktVectorMaybe WK.unwrapInt64Value


parseUInt32WrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.UInt32Value))
parseUInt32WrapperVectorMaybe = parseWktVectorMaybe WK.unwrapUInt32Value


parseUInt64WrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.UInt64Value))
parseUInt64WrapperVectorMaybe = parseWktVectorMaybe WK.unwrapUInt64Value


parseFloatWrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.FloatValue))
parseFloatWrapperVectorMaybe = parseWktVectorMaybe WK.unwrapFloatValue


parseDoubleWrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.DoubleValue))
parseDoubleWrapperVectorMaybe = parseWktVectorMaybe WK.unwrapDoubleValue


parseStringWrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.StringValue))
parseStringWrapperVectorMaybe = parseWktVectorMaybe WK.unwrapStringValue


parseBytesWrapperVectorMaybe :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector Proto.Google.Protobuf.Wrappers.BytesValue))
parseBytesWrapperVectorMaybe = parseWktVectorMaybe WK.unwrapBytesValue


parseWktVectorMaybe
  :: (Aeson.Value -> Either String a)
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (V.Vector a))
parseWktVectorMaybe parser obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just (Aeson.Array vs) ->
      either fail (pure . Just . V.fromList) (traverse parser (V.toList vs))
    Just _ -> fail "Expected JSON array for repeated WKT field"


fromJSONAssign :: Name -> MetaField -> Name -> (Name, Exp)
fromJSONAssign defName mf fldVar = case mfKind mf of
  -- Oneof carriers are themselves @Maybe SumType@; the parser
  -- already returned exactly that, so we wire it through
  -- directly. (Otherwise we'd be wrapping a 'Maybe' with
  -- 'maybe def id', i.e. typing-error city.)
  MFKOneof ->
    (mfSelector mf, VarE fldVar)
  _ ->
    -- mfSelector mf = maybe (mfSelector defName) id fld_var
    let dflt = readFieldE mf defName
        e = AppE (AppE (AppE (VarE 'maybe) dflt) (VarE 'id)) (VarE fldVar)
    in (mfSelector mf, e)


-- ---------------------------------------------------------------------------
-- Hashable for messages
-- ---------------------------------------------------------------------------

{- | Synthesise a 'Hashable' instance for a generated record.
Mirrors the per-shape combinator the pure-text codegen uses
('V.foldl' for vectors, 'Map.foldlWithKey'' for maps, plain
'hashWithSalt' for everything else).
-}
mkHashableInstanceForMessage :: Name -> [MetaField] -> Q Dec
mkHashableInstanceForMessage tyName fields = do
  saltVar <- newName "salt"
  msgVar <- newName "msg"
  let body = case fields of
        [] -> VarE saltVar
        _ -> foldl (hashStep msgVar) (VarE saltVar) fields
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Hashable) (ConT tyName))
      [ FunD
          'hashWithSalt
          [Clause [VarP saltVar, VarP msgVar] (NormalB body) []]
      ]


{- | One step of the unrolled hash: combine the previous accumulator
(already a salt) with this field's contribution.
-}
hashStep :: Name -> Exp -> MetaField -> Exp
hashStep msgVar acc mf =
  let fieldExpr = readFieldE mf msgVar
  in case mfKind mf of
       MFKVector ->
         AppE (AppE (AppE (VarE 'V.foldl') (VarE 'hashWithSalt)) acc) fieldExpr
       MFKList ->
         AppE (AppE (AppE (VarE 'foldl) (VarE 'hashWithSalt)) acc) fieldExpr
       MFKSeq ->
         AppE (AppE (AppE (VarE 'foldlSeq) (VarE 'hashWithSalt)) acc) fieldExpr
       MFKMap ->
         -- \s k v -> s `hashWithSalt` k `hashWithSalt` v
         let s = mkName "s"
             k = mkName "k"
             v = mkName "v"
             step =
               LamE
                 [VarP s, VarP k, VarP v]
                 ( AppE
                     ( AppE
                         (VarE 'hashWithSalt)
                         (AppE (AppE (VarE 'hashWithSalt) (VarE s)) (VarE k))
                     )
                     (VarE v)
                 )
         in AppE (AppE (AppE (VarE 'Map.foldlWithKey') step) acc) fieldExpr
       _ ->
         AppE (AppE (VarE 'hashWithSalt) acc) fieldExpr


{- | A 'Data.List.foldl''-shaped foldl over a 'Seq', exposed as a
splice helper so the generated code can avoid touching @Seq@'s
own combinators (which differ slightly between containers
versions).
-}
foldlSeq :: (a -> b -> a) -> a -> Seq b -> a
foldlSeq = foldl


-- ---------------------------------------------------------------------------
-- bytes-vector / bytes-list JSON helpers
-- ---------------------------------------------------------------------------

-- | A @repeated bytes@ field as a JSON array of base64 strings.
bytesVectorToJSON :: V.Vector ByteString -> Aeson.Value
bytesVectorToJSON =
  Aeson.toJSON . fmap PJI.protoBytesToJSON . V.toList


{- | A vector-backed @repeated bytes@ field whose payload is
'BL.ByteString'.
-}
lazyBytesVectorToJSON :: V.Vector BL.ByteString -> Aeson.Value
lazyBytesVectorToJSON =
  Aeson.toJSON . fmap PJI.protoLazyBytesToJSON . V.toList


{- | A vector-backed @repeated bytes@ field whose payload is
'SBS.ShortByteString'.
-}
shortBytesVectorToJSON :: V.Vector SBS.ShortByteString -> Aeson.Value
shortBytesVectorToJSON =
  Aeson.toJSON . fmap PJI.protoShortBytesToJSON . V.toList


-- | A list-backed @repeated bytes@ field as a JSON array.
bytesListToJSON :: [ByteString] -> Aeson.Value
bytesListToJSON = Aeson.toJSON . fmap PJI.protoBytesToJSON


-- | A list-backed @repeated bytes@ field whose payload is 'BL.ByteString'.
lazyBytesListToJSON :: [BL.ByteString] -> Aeson.Value
lazyBytesListToJSON = Aeson.toJSON . fmap PJI.protoLazyBytesToJSON


{- | A list-backed @repeated bytes@ field whose payload is
'SBS.ShortByteString'.
-}
shortBytesListToJSON :: [SBS.ShortByteString] -> Aeson.Value
shortBytesListToJSON = Aeson.toJSON . fmap PJI.protoShortBytesToJSON


-- | A Seq-backed @repeated bytes@ field as a JSON array.
bytesSeqToJSON :: Seq ByteString -> Aeson.Value
bytesSeqToJSON = Aeson.toJSON . fmap PJI.protoBytesToJSON . F.toList


-- | A Seq-backed @repeated bytes@ field whose payload is 'BL.ByteString'.
lazyBytesSeqToJSON :: Seq BL.ByteString -> Aeson.Value
lazyBytesSeqToJSON = Aeson.toJSON . fmap PJI.protoLazyBytesToJSON . F.toList


{- | A Seq-backed @repeated bytes@ field whose payload is
'SBS.ShortByteString'.
-}
shortBytesSeqToJSON :: Seq SBS.ShortByteString -> Aeson.Value
shortBytesSeqToJSON = Aeson.toJSON . fmap PJI.protoShortBytesToJSON . F.toList


-- | Parse @Maybe (Vector ByteString)@ from a JSON object key.
parseBytesVectorMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector ByteString))
parseBytesVectorMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . V.fromList <$> traverse PJI.protoBytesFromJSON (vs :: [Aeson.Value])


parseLazyBytesVectorMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector BL.ByteString))
parseLazyBytesVectorMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . V.fromList <$> traverse PJ.protoLazyBytesFromJSON (vs :: [Aeson.Value])


parseShortBytesVectorMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (V.Vector SBS.ShortByteString))
parseShortBytesVectorMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . V.fromList <$> traverse PJ.protoShortBytesFromJSON (vs :: [Aeson.Value])


-- | Parse @Maybe [ByteString]@ from a JSON object key.
parseBytesListMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe [ByteString])
parseBytesListMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just <$> traverse PJI.protoBytesFromJSON (vs :: [Aeson.Value])


parseLazyBytesListMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe [BL.ByteString])
parseLazyBytesListMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just <$> traverse PJ.protoLazyBytesFromJSON (vs :: [Aeson.Value])


parseShortBytesListMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe [SBS.ShortByteString])
parseShortBytesListMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just <$> traverse PJ.protoShortBytesFromJSON (vs :: [Aeson.Value])


parseBytesSeqMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Seq ByteString))
parseBytesSeqMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . Seq.fromList <$> traverse PJI.protoBytesFromJSON (vs :: [Aeson.Value])


parseLazyBytesSeqMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Seq BL.ByteString))
parseLazyBytesSeqMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . Seq.fromList <$> traverse PJ.protoLazyBytesFromJSON (vs :: [Aeson.Value])


parseShortBytesSeqMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Seq SBS.ShortByteString))
parseShortBytesSeqMaybe obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> Just . Seq.fromList <$> traverse PJ.protoShortBytesFromJSON (vs :: [Aeson.Value])


{- | Per-shape @parseBytesMaybeFieldMaybe@ for @Maybe \<Bytes>@ fields
(proto2 optional bytes, proto3 explicit-optional bytes). Returns
@Maybe (Maybe a)@ so 'fromJSONAssign' can distinguish "key absent"
from "key present, value null".
-}
parseLazyBytesMaybeFieldMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe BL.ByteString))
parseLazyBytesMaybeFieldMaybe = parseScalarMaybeMaybe PJ.protoLazyBytesFromJSON


parseShortBytesMaybeFieldMaybe
  :: Aeson.Object -> Text -> AesonT.Parser (Maybe (Maybe SBS.ShortByteString))
parseShortBytesMaybeFieldMaybe = parseScalarMaybeMaybe PJ.protoShortBytesFromJSON


-- ---------------------------------------------------------------------------
-- Scalar runtime helpers (consumed by the spliced JSON encoder)
-- ---------------------------------------------------------------------------

{- | Encode one scalar value through the proto3-canonical-JSON
helper appropriate to its 'JsonScalar' tag. Uses 'unsafeCoerce'-
shaped pattern-matching against types known statically — the
splice picks the right tag, so the runtime's job is just to
apply the matching encoder.
-}
scalarValueToJSON :: JsonScalar -> a -> Aeson.Value
scalarValueToJSON _ _ =
  -- The splice emits per-scalar 'toJSON' calls inline (see
  -- 'scalarToJsonE'), so this runtime helper is unused and only
  -- exists for the (rare) caller that wants to dispatch on a
  -- runtime tag. Keeping it total at the type level requires
  -- something equivalent to 'unsafeCoerce'; for now we simply
  -- return null and route every code path through the inlined
  -- splice instead.
  Aeson.Null


{- | A repeated scalar field as a JSON array. The splice
pre-passes the 'JsonScalar' tag so the runtime knows which
per-element encoder to call.
-}
scalarVectorToJSON
  :: forall a
   . (Aeson.ToJSON a)
  => JsonScalar
  -> V.Vector a
  -> Aeson.Value
scalarVectorToJSON sc xs = Aeson.toJSON (V.toList (V.map (encodeOneScalar sc) xs))


{- | List-backed counterpart of 'scalarVectorToJSON', dispatched
when the field's 'fieldRepeated' override is 'ListRep'.
-}
scalarListToJSON
  :: forall a
   . (Aeson.ToJSON a)
  => JsonScalar
  -> [a]
  -> Aeson.Value
scalarListToJSON sc xs = Aeson.toJSON (fmap (encodeOneScalar sc) xs)


{- | Seq-backed counterpart of 'scalarVectorToJSON', dispatched
when the field's 'fieldRepeated' override is 'SeqRep'.
-}
scalarSeqToJSON
  :: forall a
   . (Aeson.ToJSON a)
  => JsonScalar
  -> Seq a
  -> Aeson.Value
scalarSeqToJSON sc xs = Aeson.toJSON (fmap (encodeOneScalar sc) (F.toList xs))


{- | Per-element encoder shared by 'scalarVectorToJSON',
'scalarListToJSON', 'scalarSeqToJSON'. 'JSBytes' is handled by
the dedicated @*BytesVectorToJSON@ / @*BytesListToJSON@ /
@*BytesSeqToJSON@ helpers, so a 'JSBytes' tag reaching this
function means the splice picked the wrong helper -- emit
@null@ rather than silently base64-encoding via the wrong path.
-}
encodeOneScalar :: Aeson.ToJSON a => JsonScalar -> a -> Aeson.Value
encodeOneScalar JSBytes _ = Aeson.Null
encodeOneScalar _ x = Aeson.toJSON x


{- | A scalar-keyed scalar-valued map as a JSON object. Keys are
always stringified per the proto3 JSON spec; values use the
right per-scalar encoder.
-}
scalarMapToJSON
  :: forall k v
   . (Aeson.ToJSON k, Aeson.ToJSON v, Ord k)
  => JsonScalar
  -> JsonScalar
  -> Map.Map k v
  -> Aeson.Value
scalarMapToJSON kSc _vSc m =
  Aeson.toJSON
    ( Map.fromList
        [ (scalarMapKeyToText kSc k, Aeson.toJSON v)
        | (k, v) <- Map.toList m
        ]
    )


{- | Turn a scalar map-key into its proto3-canonical JSON string
form. Bool keys lowercase to "true"/"false"; integer keys
decimal-stringify; string keys pass through.
-}
scalarMapKeyToText :: forall k. (Aeson.ToJSON k) => JsonScalar -> k -> Text
scalarMapKeyToText sc k = case (sc, Aeson.toJSON k) of
  (JSBool, Aeson.Bool b) -> if b then T.pack "true" else T.pack "false"
  (_, Aeson.String s) -> s
  (_, Aeson.Number n) -> T.pack (showJsonNumber n)
  (_, v) -> T.pack (show v)
  where
    showJsonNumber n = case (toRational n :: Rational) of
      r
        | r == toRational (round n :: Integer) -> show (round n :: Integer)
        | otherwise -> show n


-- ---------------------------------------------------------------------------
-- Scalar runtime parsers (consumed by the spliced JSON decoder)
-- ---------------------------------------------------------------------------

{- | Parse a scalar value from a JSON object key, picking the
proto3-canonical helper for the scalar kind (string-form 64-bit
ints, NaN/Infinity floats, etc.).
-}
parseScalarMaybe
  :: forall a
   . (Aeson.FromJSON a)
  => JsonScalar
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe a)
parseScalarMaybe _sc = PJI.parseFieldMaybe


-- | Parse a repeated scalar field from a JSON array.
parseScalarVectorMaybe
  :: forall a
   . (Aeson.FromJSON a)
  => JsonScalar
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (V.Vector a))
parseScalarVectorMaybe _sc obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just vs -> pure ((Just . V.fromList) (vs :: [a]))


{- | Parse a scalar-keyed scalar-valued map from a JSON object.
Keys come in as JSON strings (per proto3 spec); we decode them
back to the Haskell key type via the FromJSON instance.
-}
parseScalarMapMaybe
  :: forall k v
   . (Ord k, Aeson.FromJSON k, Aeson.FromJSONKey k, Aeson.FromJSON v)
  => JsonScalar
  -> JsonScalar
  -> Aeson.Object
  -> Text
  -> AesonT.Parser (Maybe (Map.Map k v))
parseScalarMapMaybe _kSc _vSc obj key = do
  mv <- PJI.parseFieldMaybe obj key
  case mv of
    Nothing -> pure Nothing
    Just m -> pure (Just (m :: Map.Map k v))


-- ---------------------------------------------------------------------------
-- Oneof: ToJSON / FromJSON / Hashable for the carrier sum
-- ---------------------------------------------------------------------------

{- | Emit @ToJSON@ + @FromJSON@ for an oneof carrier sum. The
pure-text codegen emits @toJSON _ = Aeson.Null@ and
@parseJSON _ = fail \"Cannot parse oneof from JSON\"@; we follow
suit. (Spec-conformant proto3 JSON handles oneofs at the parent
message level rather than here, so a standalone instance for the
carrier sum is mostly a placeholder to make the type fit any
generic 'ToJSON' constraints downstream code might require.)
-}
mkOneofAesonInstances :: Name -> Q [Dec]
mkOneofAesonInstances sumTy = do
  let toJSONInst =
        InstanceD
          Nothing
          []
          (AppT (ConT ''Aeson.ToJSON) (ConT sumTy))
          [ FunD
              'Aeson.toJSON
              [Clause [WildP] (NormalB (ConE 'Aeson.Null)) []]
          ]
      fromJSONInst =
        InstanceD
          Nothing
          []
          (AppT (ConT ''Aeson.FromJSON) (ConT sumTy))
          [ FunD
              'Aeson.parseJSON
              [ Clause
                  [WildP]
                  ( NormalB
                      ( AppE
                          (VarE 'fail)
                          (LitE (StringL "Cannot parse oneof from JSON"))
                      )
                  )
                  []
              ]
          ]
  pure [toJSONInst, fromJSONInst]


{- | Emit a 'Hashable' instance for an oneof carrier sum: tag the
variant index in front of the payload's hash. Variant indices
start at 0 in declaration order (matching the pure-text codegen).
-}
mkOneofHashableInstance :: Name -> [Name] -> Q Dec
mkOneofHashableInstance sumTy variantCons = do
  saltVar <- newName "salt"
  vVar <- newName "v"
  let mkArm (idx, conName) =
        Clause
          [ VarP saltVar
          , ConP conName [] [VarP vVar]
          ]
          ( NormalB
              ( AppE
                  ( AppE
                      (VarE 'hashWithSalt)
                      ( AppE
                          (AppE (VarE 'hashWithSalt) (VarE saltVar))
                          (SigE (intLit idx) (ConT ''Int))
                      )
                  )
                  (VarE vVar)
              )
          )
          []
      arms = case variantCons of
        [] ->
          [Clause [VarP saltVar, WildP] (NormalB (VarE saltVar)) []]
        _ -> fmap mkArm (zip [0 ..] variantCons)
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Hashable) (ConT sumTy))
      [FunD 'hashWithSalt arms]


-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

{- | Synthesise the 'PS.ProtoEnum' instance: ties each generated
constructor to its proto-side wire number and string name.
-}
mkProtoEnumInstance
  :: Name
  -- ^ Haskell enum type.
  -> Text
  -- ^ Fully-qualified proto enum name.
  -> [(Name, Text, Int)]
  {- ^ @(haskellCon, protoName, evNumber)@
  for every declared value (aliases
  included).
  -}
  -> Name
  {- ^ Synthetic @<EnumName>'Unknown !Int32@
  constructor for open-enum semantics.
  -}
  -> Q Dec
mkProtoEnumInstance tyName fqName values unknownCon = do
  nVar <- newName "n"
  let
    -- protoEnumName _ = "..."
    nameDec =
      FunD
        'PS.protoEnumName
        [Clause [WildP] (NormalB (textLit fqName)) []]
    -- protoEnumValues _ = [(name, num), ...]
    pairs =
      ListE
        [ TupE [Just (textLit n), Just (intLit num)]
        | (_, n, num) <- values
        ]
    valuesDec =
      FunD
        'PS.protoEnumValues
        [Clause [WildP] (NormalB pairs) []]
    -- toProtoEnumValue: pattern-match on every (alias-inclusive)
    -- constructor, plus the Unknown wrapper which simply
    -- yields its carried int.
    toClauses =
      [ Clause
          [ConP con [] []]
          (NormalB (intLit num))
          []
      | (con, _, num) <- values
      ]
        <> [ Clause
               [ConP unknownCon [] [VarP nVar]]
               (NormalB (AppE (VarE 'fromIntegral) (VarE nVar)))
               []
           ]
    toDec = FunD 'PS.toProtoEnumValue toClauses
    -- fromProtoEnumValue: one Just clause per primary number,
    -- then a catch-all that produces 'Just (Unknown n)' so
    -- callers can preserve the wire value across encode\/decode.
    primaries = primaryByNumber values
    fromClauses =
      fmap
        ( \(con, _, num) ->
            Clause
              [LitP (IntegerL (fromIntegral num))]
              (NormalB (AppE (ConE 'Just) (ConE con)))
              []
        )
        primaries
        <> [ Clause
               [VarP nVar]
               ( NormalB
                   ( AppE
                       (ConE 'Just)
                       ( AppE
                           (ConE unknownCon)
                           ( SigE
                               (AppE (VarE 'fromIntegral) (VarE nVar))
                               (ConT ''Int32)
                           )
                       )
                   )
               )
               []
           ]
    fromDec = FunD 'PS.fromProtoEnumValue fromClauses
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''PS.ProtoEnum) (ConT tyName))
      [nameDec, valuesDec, toDec, fromDec]


-- | Drop later occurrences of any wire number; preserves first.
primaryByNumber :: [(Name, Text, Int)] -> [(Name, Text, Int)]
primaryByNumber = go []
  where
    go _ [] = []
    go seen (v@(_, _, n) : rest)
      | n `elem` seen = go seen rest
      | otherwise = v : go (n : seen) rest


{- | Synthesise @ToJSON@ / @FromJSON@ for an enum: the primary name
string on the encode side, and a string-or-number parser on the
decode side (per the proto3 JSON spec, both are accepted on
read, but the canonical write form is the name).
-}
mkEnumAesonInstances :: Name -> [(Name, Text, Int)] -> Name -> Q [Dec]
mkEnumAesonInstances tyName values unknownCon = do
  nVar <- newName "n"
  let primaries = primaryByNumber values
      toClauses =
        [ Clause
            [ConP con [] []]
            (NormalB (AppE (ConE 'Aeson.String) (textLit pname)))
            []
        | (con, pname, _) <- primaries
        ]
          -- Open-enum representation: @<EnumName>'Unknown n@
          -- serialises as the bare numeric value (proto3
          -- canonical-JSON for unrecognised enum values).
          <> [ Clause
                 [ConP unknownCon [] [VarP nVar]]
                 (NormalB (AppE (VarE 'Aeson.toJSON) (VarE nVar)))
                 []
             ]
      toDec = FunD 'Aeson.toJSON toClauses

      -- fromJSON: \case String "FOO" -> pure ConFOO ... Number n -> ... _ -> fail
      --
      -- All declared names (aliases included) parse to the
      -- corresponding /primary/ Haskell constructor — that's
      -- what makes proto @allow_alias = true@ work end-to-end
      -- (EnumFieldWithAliasUseAlias / DifferentCase / LowerCase
      -- conformance tests).
      stringClauses =
        [ Match
            (ConP 'Aeson.String [] [LitP (StringL (T.unpack pname))])
            (NormalB (AppE (VarE 'pure) (ConE con)))
            []
        | (con, pname, _) <- values
        ]
      -- 'toEnum' is the open-enum-aware constructor: known
      -- numbers route to the matching constructor, unknown
      -- numbers wrap in the Unknown variant.
      numberMatch =
        Match
          (ConP 'Aeson.Number [] [VarP nVar])
          ( NormalB
              ( AppE
                  (VarE 'pure)
                  ( AppE
                      (VarE 'toEnum)
                      (AppE (VarE 'round) (VarE nVar))
                  )
              )
          )
          []
      -- Tag the failure with a sentinel prefix so wrapping
      -- parsers (e.g. lenient mode for repeated/map enum fields,
      -- the JSON_IGNORE_UNKNOWN_PARSING_TEST conformance
      -- category) can detect it without false-positiving on
      -- unrelated Aeson parse errors.
      failMatch =
        Match
          WildP
          ( NormalB
              ( AppE
                  (VarE 'fail)
                  ( LitE
                      ( StringL
                          ( unknownEnumFailPrefix
                              <> nameBase tyName
                          )
                      )
                  )
              )
          )
          []
      caseExp = LamCaseE (stringClauses <> [numberMatch, failMatch])

  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''Aeson.ToJSON) (ConT tyName))
        [toDec]
    , InstanceD
        Nothing
        []
        (AppT (ConT ''Aeson.FromJSON) (ConT tyName))
        [ FunD
            'Aeson.parseJSON
            [Clause [] (NormalB caseExp) []]
        ]
    ]


-- | Hash an enum by its proto wire number.
mkEnumHashableInstance :: Name -> Q Dec
mkEnumHashableInstance tyName = do
  saltVar <- newName "salt"
  xVar <- newName "x"
  let body =
        AppE
          (AppE (VarE 'hashWithSalt) (VarE saltVar))
          (AppE (VarE 'PS.toProtoEnumValue) (VarE xVar))
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Hashable) (ConT tyName))
      [ FunD
          'hashWithSalt
          [Clause [VarP saltVar, VarP xVar] (NormalB body) []]
      ]


-- ---------------------------------------------------------------------------
-- Oneof JSON input
-- ---------------------------------------------------------------------------

{- | Build an 'Exp' that parses a oneof carrier (@Maybe SumType@)
from an 'Aeson.Object' by scanning for any of the variant
JSON keys. Implements the proto3 spec rules:

  * No variant key present: @Nothing@.
  * Exactly one variant key present, value is JSON @null@:
    @Nothing@ (treats null as variant-cleared).
  * Exactly one variant key present, value parses: @Just v@.
  * Multiple non-null variant keys present: parser fails
    ('OneofFieldDuplicate' conformance test).
-}
buildOneofParseExp :: Name -> [OneofVariantJson] -> Q Exp
buildOneofParseExp objVar variants = do
  pairs <- traverse mkPair variants
  let pairsList = ListE pairs
  [|parseOneofVariants $(varE objVar) $(pure pairsList)|]
  where
    mkPair OneofVariantJson {ovjConstructor = con, ovjJsonKey = key, ovjShape = sh} = do
      vName <- newName "v"
      parser <- case sh of
        OVScalar sc -> pure (oneofScalarParserE sc (ConE con) (VarE vName))
        OVMessage ->
          [|$(pure (ConE con)) <$> Aeson.parseJSON $(varE vName)|]
        OVEnum ->
          [|$(pure (ConE con)) <$> Aeson.parseJSON $(varE vName)|]
        OVNullValue ->
          -- NullValue accepts JSON @null@ /or/ the @"NULL_VALUE"@
          -- string sentinel; both decode to the singleton enum
          -- value via the standard 'parseJSON' instance, except
          -- that we also cover the bare-null shape ourselves.
          [|
            $(pure (ConE con))
              <$> ( case $(varE vName) of
                      Aeson.Null -> pure NullValue'NullValue
                      other -> Aeson.parseJSON other
                  )
            |]
      let nullSem = case sh of
            OVNullValue -> ConE 'OneofVariantNullIsValue
            _ -> ConE 'OneofVariantNullIsUnset
          lam = LamE [VarP vName] parser
          tuple3 = TupE [Just (textLit key), Just nullSem, Just lam]
      pure tuple3


{- | Splice for one scalar oneof variant: applies the right
canonical-form parser to the value and wraps it in the
variant's constructor.
-}
oneofScalarParserE :: JsonScalar -> Exp -> Exp -> Exp
oneofScalarParserE sc conE valE =
  let p = scalarFromJSONExp sc
  in InfixE (Just conE) (VarE '(<$>)) (Just (AppE p valE))


{- | Per-scalar @Aeson.Value -> Parser a@ helper. Mirrors the
writer-side 'scalarValueToJSON' / 'scalarTagE' tables.
-}
scalarFromJSONExp :: JsonScalar -> Exp
scalarFromJSONExp = \case
  JSBool -> VarE 'Aeson.parseJSON
  JSInt32 -> VarE 'protoInt32FromJSON
  JSSInt32 -> VarE 'protoInt32FromJSON
  JSSFixed32 -> VarE 'protoInt32FromJSON
  JSUInt32 -> VarE 'protoWord32FromJSON
  JSFixed32 -> VarE 'protoWord32FromJSON
  JSInt64 -> VarE 'PJI.protoInt64FromJSON
  JSSInt64 -> VarE 'PJI.protoInt64FromJSON
  JSSFixed64 -> VarE 'PJI.protoInt64FromJSON
  JSUInt64 -> VarE 'PJI.protoWord64FromJSON
  JSFixed64 -> VarE 'PJI.protoWord64FromJSON
  JSFloat -> VarE 'oneofFloatFromJSON
  JSDouble -> VarE 'oneofDoubleFromJSON
  JSString -> VarE 'Aeson.parseJSON
  JSBytes -> VarE 'PJI.protoBytesFromJSON


oneofFloatFromJSON :: Aeson.Value -> AesonT.Parser Float
oneofFloatFromJSON = protoFloatFromJSONLenient


oneofDoubleFromJSON :: Aeson.Value -> AesonT.Parser Double
oneofDoubleFromJSON = protoFloatFromJSONLenient


{- | Sentinel error-message prefix used by the generated enum
'parseJSON' when it can't recognise a string value. Wrapping
parsers (singular / repeated / map enum fields, lenient
conformance mode) detect it via 'isUnknownEnumFail' and
decide whether to filter the element or propagate the error.
-}
unknownEnumFailPrefix :: String
unknownEnumFailPrefix = "wireform-unknown-enum-value:"


isUnknownEnumFail :: String -> Bool
isUnknownEnumFail = (unknownEnumFailPrefix `isPrefixOf`)
  where
    isPrefixOf p s = take (length p) s == p


{- | Per-variant interpretation of JSON @null@ for oneofs. For
most variants, @null@ means "this variant is unset" (proto3
spec). For a 'google.protobuf.NullValue' variant, @null@
is the variant's value.
-}
data OneofVariantNullSemantics
  = OneofVariantNullIsUnset
  | OneofVariantNullIsValue


{- | Runtime helper backing 'buildOneofParseExp'. Lives outside
the splice so the 'parseFnFor' table doesn't have to.
-}
parseOneofVariants
  :: Aeson.Object
  -> [(Text, OneofVariantNullSemantics, Aeson.Value -> AesonT.Parser a)]
  -> AesonT.Parser (Maybe a)
parseOneofVariants obj variants =
  let present =
        [ (k, v, p)
        | (k, sem, p) <- variants
        , Just v <- [AesonKM.lookup (AesonKey.fromText k) obj]
        , keep sem v
        ]
      keep OneofVariantNullIsUnset Aeson.Null = False
      keep _ _ = True
  in case present of
       [] -> pure Nothing
       [(_, v, p)] -> Just <$> p v
       _ ->
         fail
           ( "Multiple oneof variants set: "
               <> show (fmap (\(k, _, _) -> k) present)
           )
{-# INLINE parseOneofVariants #-}


-- ---------------------------------------------------------------------------
-- Tiny helpers
-- ---------------------------------------------------------------------------

intLit :: Int -> Exp
intLit n = LitE (IntegerL (fromIntegral n))


textLit :: Text -> Exp
textLit t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))
