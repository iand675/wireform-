{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Internal building blocks used by 'Proto.TH.Derive' and intended for
reuse from other Template Haskell entry points (notably the IDL
bridge in 'Proto.TH').

The split exists so that callers who synthesise a fresh @data@
declaration in the same splice (and therefore cannot @qReify@ the
type) can still drive the same encoder \/ decoder \/ size body
generation logic by handing in a pre-computed @['ProtoField']@
list.

/Stability/: this module exposes the deriver's internals. Breaking
changes here only force a corresponding update in 'Proto.TH.Derive'
and 'Proto.TH'; users who only call 'Proto.TH.Derive.deriveProto'
will not be affected.
-}
module Proto.Internal.Derive (
  -- * Field model
  ProtoField (..),
  protoField,
  ProtoFieldKind (..),
  ProtoFieldType (..),
  Scalar (..),
  RepeatedMode (..),
  scalarPackable,
  OneofVariant (..),
  oneofVariant,
  scalarOfMapKey,

  -- * Message-level metadata
  MessageMeta (..),
  defaultMessageMeta,

  -- * Body builders (consume @['ProtoField']@)
  buildMessageBody,
  buildMessageBodyWith,
  messageSizeBody,
  messageSizeBodyWith,
  messageDecoderBody,
  messageDecoderBodyWith,

  -- * Instance synthesis (no reification required)
  mkEncodeInstance,
  mkEncodeInstanceWith,
  mkDecodeInstance,
  mkDecodeInstanceWith,
  mkSemigroupInstance,
  mkSemigroupInstanceWith,
  mkMonoidInstance,
  mkMonoidInstanceWith,
  mkIsMessageInstance,

  -- * Convenience: synthesise the full instance group
  synthesiseProtoInstances,
  synthesiseProtoInstancesWith,

  -- * Wire-tag helpers
  scalarWireType,
  wireVarint,
  wire64Bit,
  wireLengthDelimited,
  wire32Bit,
  tagByteFor,
) where

import Data.Bifunctor qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Language.Haskell.TH
import Proto.Internal.Decode qualified as PD
import Proto.Internal.Encode qualified as PE
import Proto.Internal.Encode.Archetype qualified as PA
import Proto.Internal.Wire (Tag (..))
import Proto.Internal.Wire qualified as PWire
import Proto.Internal.Wire.Decode qualified as PWD
import Proto.Internal.SizedBuilder qualified as PSB
import Proto.Internal.Wire.Encode qualified as PWE
import Proto.Registry qualified as PReg
import Proto.Repr (BytesAdapter (..), BytesRep (..), MapAdapter (..), RepeatedAdapter (..), StringAdapter (..), StringRep (..))
import Proto.Repr qualified as PR
import Proto.Schema qualified as PS
import Wireform.Builder qualified as BB
import Wireform.Derive.Modifier (MapKeyScalar (..))


-- ---------------------------------------------------------------------------
-- Field model (lifted out of Proto.TH.Derive)
-- ---------------------------------------------------------------------------

-- | How the field is wrapped on the Haskell side.
data ProtoFieldKind
  = -- | Singular scalar / submessage / enum field. Encode with the
    -- proto3 default-skip rule (scalars only; submessages and enums
    -- always encode at least the tag, but a missing tag decodes to
    -- the zero value).
    FKBare
  | -- | @Maybe a@-wrapped singular field. Encode only when 'Just'.
    FKMaybe
  | -- | A repeated field carried by a container chosen by the
    -- 'RepeatedAdapter'. The element type lives in @pfInnerTy@ and
    -- the wire encoding in @pfType@. 'RepeatedMode' selects packed
    -- vs. unpacked on the encoder side; the decoder always accepts
    -- both when the element is a packable scalar (per the proto3
    -- spec, a parser must accept both encodings regardless of how
    -- the writer chose).
    FKRepeated !RepeatedAdapter !RepeatedMode
  | -- | A proto3 @map<K, V>@ field carried by a strict
    -- 'Data.Map.Strict.Map'. The key's wire encoding is the
    -- supplied 'MapKeyScalar'; the value's wire encoding is in
    -- @pfType@ and the value's Haskell type is @pfInnerTy@.
    FKMap !MapKeyScalar
  | -- | A proto @oneof@ carried as a Haskell @Maybe SumType@. Each
    -- variant's tag, constructor, and payload type lives inline.
    -- @pfTag@ is unused for oneofs; @pfType@ / @pfInnerTy@ describe
    -- the carrier @Maybe SumType@ but are ignored by the body
    -- builders.
    FKOneof ![OneofVariant]


{- | Wire encoding shape for a repeated field on the encoder side.

* 'ModeUnpacked' — emit one tag+value record per element. Required
  for non-packable scalars ('SString', 'SBytes') and for
  submessages (which are not packable).
* 'ModePacked' — emit a single length-delimited record containing
  the concatenated payloads. Proto3's default for packable
  scalars; legal but opt-in in proto2.

The decoder accepts both shapes regardless of which the writer
picked, so this only affects the bytes the encoder emits.
-}
data RepeatedMode
  = ModeUnpacked
  | ModePacked
  deriving stock (Eq, Show)


{- | Whether a 'Scalar' may legally be packed on the wire. Strings
and bytes are length-delimited and so cannot be packed; every
other 'Scalar' the deriver tracks is packable.
-}
scalarPackable :: Scalar -> Bool
scalarPackable = \case
  SString -> False
  SBytes -> False
  _ -> True


-- | One arm of a proto @oneof@.
data OneofVariant = OneofVariant
  { ovConstructor :: !Name
  -- ^ Sum-type constructor name.
  , ovTag :: !Int
  -- ^ Proto field number for this arm.
  , ovInnerTy :: !Type
  -- ^ The constructor's single argument type.
  , ovType :: !ProtoFieldType
  -- ^ Wire encoding for the arm's payload.
  , ovStringAdapter :: StringAdapter
  -- ^ String adapter for this variant when
  -- @ovType = PFScalar SString@.
  , ovBytesAdapter :: BytesAdapter
  -- ^ Bytes adapter for this variant when
  -- @ovType = PFScalar SBytes@.
  }


{- | Smart constructor for 'OneofVariant' that defaults the string/
bytes adapters to strict 'Text' / 'ByteString'. Bridges
can override the adapter slots after construction (record update
syntax) for variants whose payloads need custom representations.
-}
oneofVariant :: Name -> Int -> Type -> ProtoFieldType -> OneofVariant
oneofVariant con tg innerTy ty =
  OneofVariant
    { ovConstructor = con
    , ovTag = tg
    , ovInnerTy = innerTy
    , ovType = ty
    , ovStringAdapter = PR.strictTextAdapter
    , ovBytesAdapter = PR.strictBytesAdapter
    }


-- | What lives inside the field on the wire.
data ProtoFieldType
  = -- | Recognised scalar.
    PFScalar !Scalar
  | -- | Submessage with existing 'PE.MessageEncode' \/
    -- 'PD.MessageDecode' \/ 'PE.MessageSize' instances.
    PFSubmessage
  | -- | A Haskell @Enum@ (any datatype with derived @Enum@).
    -- Encoded as a varint via 'fromEnum' / 'toEnum'.
    PFEnum
  deriving stock (Eq, Show)


-- | Proto wire scalars supported by the deriver.
data Scalar
  = SInt32
  | SInt64
  | SUInt32
  | SUInt64
  | SSInt32
  | SSInt64
  | SFixed32
  | SFixed64
  | SSFixed32
  | SSFixed64
  | SBool
  | SFloat
  | SDouble
  | SString
  | SBytes
  deriving stock (Eq, Show)


{- | Project a 'MapKeyScalar' onto the deriver's 'Scalar' type so
the existing scalar-encoding machinery can be reused for map
keys.
-}
scalarOfMapKey :: MapKeyScalar -> Scalar
scalarOfMapKey = \case
  MapKeyInt32 -> SInt32
  MapKeyInt64 -> SInt64
  MapKeyUInt32 -> SUInt32
  MapKeyUInt64 -> SUInt64
  MapKeySInt32 -> SSInt32
  MapKeySInt64 -> SSInt64
  MapKeyFixed32 -> SFixed32
  MapKeyFixed64 -> SFixed64
  MapKeySFixed32 -> SSFixed32
  MapKeySFixed64 -> SSFixed64
  MapKeyBool -> SBool
  MapKeyString -> SString


{- | A field after annotation resolution and type inspection. The
selector 'Name' is used for both the encoder getter and the
decoder's record assignment, so it must match the constructor that
will eventually be in scope.
-}
data ProtoField = ProtoField
  { pfSelector :: !Name
  , pfTag :: !Int
  , pfKind :: !ProtoFieldKind
  , pfType :: !ProtoFieldType
  -- ^ Wire encoding for the field's element/value (singular,
  -- repeated, map-value). Ignored for 'FKOneof' (each variant
  -- carries its own).
  , pfInnerTy :: !Type
  -- ^ Type of the unwrapped value (no 'Maybe', no container).
  -- For maps, this is the value type. For oneofs this is the
  -- carrier @Maybe SumType@ — body builders ignore it.
  , pfStringAdapter :: StringAdapter
  -- ^ Adapter for proto @string@ fields. Only consulted when
  -- @pfType = PFScalar SString@.
  , pfBytesAdapter :: BytesAdapter
  -- ^ Adapter for proto @bytes@ fields. Only consulted when
  -- @pfType = PFScalar SBytes@.
  }


{- | Smart constructor with adapters defaulted to strict
'Text' \/ 'ByteString'. Most call sites are happy with these.
-}
protoField :: Name -> Int -> ProtoFieldKind -> ProtoFieldType -> Type -> ProtoField
protoField sel tg knd ty inner =
  ProtoField
    { pfSelector = sel
    , pfTag = tg
    , pfKind = knd
    , pfType = ty
    , pfInnerTy = inner
    , pfStringAdapter = PR.strictTextAdapter
    , pfBytesAdapter = PR.strictBytesAdapter
    }


-- ---------------------------------------------------------------------------
-- Message-level metadata
-- ---------------------------------------------------------------------------

{- | Per-message knobs that can\'t be expressed at the field level.

The current resident is 'mmUnknownFieldsSel': when set to
@Just sel@, the synthesised codecs honour an unknown-fields slot
on the record (the encoder appends captured tags after the
declared fields, the decoder routes any unrecognised tag into a
parallel accumulator, and the sizer adds the slot\'s on-wire
footprint). When 'Nothing', the codecs match the original
'Proto.TH.Derive.deriveProto' output and silently drop unknown
tags.
-}
newtype MessageMeta = MessageMeta
  { mmUnknownFieldsSel :: Maybe Name
  -- ^ Selector for an @[Decode.UnknownField]@ field on the
  -- record. When set, the codecs round-trip unknown tags through
  -- this slot.
  }
  deriving stock (Show)


-- | Default 'MessageMeta' with no unknown-fields slot.
defaultMessageMeta :: MessageMeta
defaultMessageMeta = MessageMeta {mmUnknownFieldsSel = Nothing}


-- ---------------------------------------------------------------------------
-- Wire-tag helpers
-- ---------------------------------------------------------------------------

wireVarint, wire64Bit, wireLengthDelimited, wire32Bit :: Int
wireVarint = 0
wire64Bit = 1
wireLengthDelimited = 2
wire32Bit = 5


{- | @(fn << 3) | wt@. Encoders below assume the result fits in one
byte; callers should check field-number bounds.
-}
tagByteFor :: Int -> Int -> Int
tagByteFor fn wt = (fn * 8) + wt


scalarWireType :: Scalar -> Int
scalarWireType = \case
  SInt32 -> wireVarint
  SInt64 -> wireVarint
  SUInt32 -> wireVarint
  SUInt64 -> wireVarint
  SSInt32 -> wireVarint
  SSInt64 -> wireVarint
  SBool -> wireVarint
  SFixed32 -> wire32Bit
  SSFixed32 -> wire32Bit
  SFloat -> wire32Bit
  SFixed64 -> wire64Bit
  SSFixed64 -> wire64Bit
  SDouble -> wire64Bit
  SString -> wireLengthDelimited
  SBytes -> wireLengthDelimited


-- ---------------------------------------------------------------------------
-- Body builders
-- ---------------------------------------------------------------------------

{- | RHS of @buildMessage@ for the supplied fields. Equivalent to
@'buildMessageBodyWith' 'defaultMessageMeta'@.
-}
buildMessageBody :: [ProtoField] -> Q Exp
buildMessageBody = buildMessageBodyWith defaultMessageMeta


{- | RHS of @buildMessage@. When @mmUnknownFieldsSel = Just sel@,
appends @Decode.encodeUnknownFields (sel msg)@ after the
declared fields.
-}
buildMessageBodyWith :: MessageMeta -> [ProtoField] -> Q Exp
buildMessageBodyWith meta fs = do
  msg <- newName "msg"
  parts <- mapM (encodeOne msg) fs
  let ufPart = case mmUnknownFieldsSel meta of
        Nothing -> Nothing
        Just sel ->
          Just
            ( AppE
                (VarE 'PD.encodeUnknownFieldsSized)
                (AppE (VarE sel) (VarE msg))
            )
      allParts = parts <> maybeToList ufPart
  case allParts of
    [] -> lamE [varP msg] [|mempty|]
    _ ->
      let body = foldr1 (\a b -> InfixE (Just a) (VarE '(<>)) (Just b)) allParts
      in lamE [varP msg] (pure body)


{- | RHS of @messageSize@ for the supplied fields. Equivalent to
@'messageSizeBodyWith' 'defaultMessageMeta'@.
-}
messageSizeBody :: [ProtoField] -> Q Exp
messageSizeBody = messageSizeBodyWith defaultMessageMeta


{- | RHS of @messageSize@. When @mmUnknownFieldsSel = Just sel@,
adds @Decode.unknownFieldsSize (sel msg)@.
-}
messageSizeBodyWith :: MessageMeta -> [ProtoField] -> Q Exp
messageSizeBodyWith meta fs = do
  msg <- newName "msg"
  parts <- mapM (sizeOne msg) fs
  let ufPart = case mmUnknownFieldsSel meta of
        Nothing -> Nothing
        Just sel ->
          Just
            ( AppE
                (VarE 'PD.unknownFieldsSize)
                (AppE (VarE sel) (VarE msg))
            )
      allParts = parts <> maybeToList ufPart
  case allParts of
    [] -> lamE [varP msg] [|0 :: Int|]
    _ ->
      lamE
        [varP msg]
        (pure (foldr1 (\a b -> InfixE (Just a) (VarE '(+)) (Just b)) allParts))


{- | RHS of @messageDecoder@ for the supplied constructor and
fields. Equivalent to @'messageDecoderBodyWith'
'defaultMessageMeta'@.
-}
messageDecoderBody :: Name -> [ProtoField] -> Q Exp
messageDecoderBody = messageDecoderBodyWith defaultMessageMeta


{- | RHS of @messageDecoder@. The 'Name' is the record constructor
(typically the same as the type name for single-constructor
records). When @mmUnknownFieldsSel = Just sel@, captures
unrecognised tags into a parallel accumulator and writes the
reversed list to the slot in the final record.
-}
messageDecoderBodyWith :: MessageMeta -> Name -> [ProtoField] -> Q Exp
messageDecoderBodyWith meta conName fs = do
  accNames <- mapM (\(i, _) -> newName ("acc_" ++ show (i :: Int))) (zip [0 ..] fs)
  let pairs = zip fs accNames
  initEs <- mapM initFor pairs
  loopName <- newName "loop"
  ufAccM <- case mmUnknownFieldsSel meta of
    Nothing -> pure Nothing
    Just _ -> Just <$> newName "acc_unknown_"
  loopBody <- decodeLoopBody meta conName loopName pairs ufAccM
  let allParams = map VarP accNames <> maybe [] (\n -> [VarP n]) ufAccM
      loopFun =
        FunD
          loopName
          [Clause allParams (NormalB loopBody) []]
      ufInit = case mmUnknownFieldsSel meta of
        Nothing -> []
        Just _ -> [ListE []]
      bodyExp =
        LetE
          [loopFun]
          (foldl AppE (VarE loopName) (initEs <> ufInit))
  pure bodyExp


-- ---------------------------------------------------------------------------
-- Instance synthesis (no reification required)
-- ---------------------------------------------------------------------------

{- | Generate an 'PE.MessageEncode' instance for @ty@ with the
supplied field list. The @ty@ must be a fully applied type whose
record selectors match @pfSelector@ values.
-}
mkEncodeInstance :: Type -> [ProtoField] -> Q Dec
mkEncodeInstance = mkEncodeInstanceWith defaultMessageMeta


mkEncodeInstanceWith :: MessageMeta -> Type -> [ProtoField] -> Q Dec
mkEncodeInstanceWith meta ty fs = do
  body <- buildMessageBodyWith meta fs
  -- 'buildMessageBodyWith' now produces a 'SizedBuilder'-valued
  -- lambda (every field encoder underneath has been migrated to
  -- the sized archetypes), so this instance defines 'buildSized'
  -- directly. 'buildMessage' falls out of the class default in
  -- "Proto.Encode" via @SB.toBuilder . buildSized@ and
  -- 'messageSize' via @SB.size . buildSized@; neither needs a
  -- separate TH-emitted method.
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''PE.MessageEncode) ty)
      [FunD 'PE.buildSized [Clause [] (NormalB body) []]]


-- The old @mkSizeInstance@ / @mkSizeInstanceWith@ helpers (which
-- emitted standalone @MessageSize@ instances) are gone — the
-- 'MessageSize' class was removed, and the size pass is now fused
-- into 'PE.buildSized' via 'mkEncodeInstance'. If you want the
-- message's wire size you go through 'PE.messageSize' (which is
-- @'SB.size' . 'PE.buildSized'@).


{- | Generate a 'PD.MessageDecode' instance using @conName@ as the
record constructor.
-}
mkDecodeInstance :: Type -> Name -> [ProtoField] -> Q Dec
mkDecodeInstance = mkDecodeInstanceWith defaultMessageMeta


mkDecodeInstanceWith :: MessageMeta -> Type -> Name -> [ProtoField] -> Q Dec
mkDecodeInstanceWith meta ty conName fs = do
  body <- messageDecoderBodyWith meta conName fs
  -- agents.md ("Performance" → "Decoder monad style") requires
  -- @{-# INLINE messageDecoder #-}@ on every instance: the
  -- continuation-passing dispatch in 'getTagOrU' is only worth its
  -- weight when GHC can see the lambda for each field. Mirrors what
  -- 'Proto.CodeGen' emits in pure-text codegen.
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''PD.MessageDecode) ty)
      [ PragmaD (InlineP 'PD.messageDecoder Inline FunLike AllPhases)
      , FunD 'PD.messageDecoder [Clause [] (NormalB body) []]
      ]


{- | Generate an empty @instance IsMessage T@. 'IsMessage' is a marker
class; GHC checks that every superclass instance ('MessageEncode',
'MessageDecode', 'ProtoMessage', 'Aeson.ToJSON', 'Aeson.FromJSON',
'Typeable') is in scope. The instance is what
'Proto.Registry.discoverRegistry' looks up via 'reify' to build the
runtime type registry.
-}
mkIsMessageInstance :: Type -> Dec
mkIsMessageInstance ty =
  InstanceD Nothing [] (AppT (ConT ''PReg.IsMessage) ty) []


{- | Generate a 'Semigroup' instance encoding protobuf merge semantics.

For each field, @a '<>' b@ combines using the following rules:

  * Repeated: append via the field's underlying 'Semigroup' (vector @++@).
  * Map: take the union via the field's underlying 'Semigroup'
    (@Data.Map.Strict@ is left-biased, so existing keys win on conflict).
  * Optional (@Maybe@) submessage: incoming 'Just' wins; otherwise keep
    the existing value.
  * Oneof: incoming variant wins when set; otherwise keep the existing.
  * Scalar / bytes / text / bool / enum: incoming value wins only when
    non-default (matching MergeFrom in protoc-gen-go/cpp).

The monoid laws hold: @mempty '<>' a = a@ and @a '<>' mempty = a@.
-}
mkSemigroupInstance :: Type -> Name -> [ProtoField] -> Q Dec
mkSemigroupInstance = mkSemigroupInstanceWith defaultMessageMeta


mkSemigroupInstanceWith :: MessageMeta -> Type -> Name -> [ProtoField] -> Q Dec
mkSemigroupInstanceWith meta ty conName fs = do
  aName <- newName "a"
  bName <- newName "b"
  let mergeField pf =
        let sel = pfSelector pf
            getA = AppE (VarE sel) (VarE aName)
            getB = AppE (VarE sel) (VarE bName)
        in case pfKind pf of
            FKRepeated _ _ -> [|$(pure getA) <> $(pure getB)|]
            FKMap _ -> [|$(pure getA) <> $(pure getB)|]
            FKOneof _ -> [|case $(pure getB) of Nothing -> $(pure getA); b' -> b'|]
            FKMaybe -> [|case $(pure getB) of Nothing -> $(pure getA); b' -> b'|]
            _ -> case pfType pf of
              PFSubmessage -> [|case $(pure getB) of Nothing -> $(pure getA); b' -> b'|]
              PFEnum -> [|if $(pure getB) == toEnum 0 then $(pure getA) else $(pure getB)|]
              PFScalar SBool -> [|if not $(pure getB) then $(pure getA) else $(pure getB)|]
              PFScalar SString -> [|if $(pure getB) == mempty then $(pure getA) else $(pure getB)|]
              PFScalar SBytes -> [|if $(pure getB) == mempty then $(pure getA) else $(pure getB)|]
              PFScalar _ -> [|if $(pure getB) == 0 then $(pure getA) else $(pure getB)|]
  fieldExps <-
    mapM
      ( \pf -> do
          val <- mergeField pf
          pure (pfSelector pf, val)
      )
      fs
  let ufMerge = case mmUnknownFieldsSel meta of
        Just ufSel ->
          [
            ( ufSel
            , AppE
                ( AppE
                    (VarE '(<>))
                    (AppE (VarE ufSel) (VarE aName))
                )
                (AppE (VarE ufSel) (VarE bName))
            )
          ]
        Nothing -> []
  let body = RecConE conName (fmap (\(sel, val) -> (sel, val)) fieldExps ++ ufMerge)
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Semigroup) ty)
      [ FunD
          '(<>)
          [Clause [VarP aName, VarP bName] (NormalB body) []]
      ]


{- | Generate a 'Monoid' instance whose identity is the proto3 default
message (all scalars zero, all bool 'False', all strings \/ bytes empty,
all repeated \/ map fields empty, all optional and oneof fields
'Nothing'). The 'Semigroup' instance skips default-valued scalars
from the right operand, so @a '<>' mempty = a@ holds.
-}
mkMonoidInstance :: Type -> Name -> [ProtoField] -> Q Dec
mkMonoidInstance = mkMonoidInstanceWith defaultMessageMeta


mkMonoidInstanceWith :: MessageMeta -> Type -> Name -> [ProtoField] -> Q Dec
mkMonoidInstanceWith meta ty conName fs = do
  let defaultField pf = case pfKind pf of
        FKRepeated _ _ -> [|mempty|]
        FKMap _ -> [|mempty|]
        FKMaybe -> [|Nothing|]
        FKOneof _ -> [|Nothing|]
        FKBare -> case pfType pf of
          PFSubmessage -> [|mempty|]
          PFEnum -> [|toEnum 0|]
          PFScalar SBool -> [|False|]
          PFScalar SString -> [|mempty|]
          PFScalar SBytes -> [|mempty|]
          PFScalar _ -> [|0|]
  fieldExps <-
    mapM
      ( \pf -> do
          val <- defaultField pf
          pure (pfSelector pf, val)
      )
      fs
  let ufDefault = case mmUnknownFieldsSel meta of
        Just ufSel -> [(ufSel, VarE 'mempty)]
        Nothing -> []
  let body = RecConE conName (fieldExps ++ ufDefault)
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Monoid) ty)
      [FunD 'mempty [Clause [] (NormalB body) []]]


{- | One-shot: emit the five core codec instances ('PE.MessageEncode',
'PE.MessageSize', 'PD.MessageDecode', 'Semigroup', 'Monoid') for a
pre-translated message.

Note that the 'PReg.IsMessage' marker (and the 'PS.ProtoMessage' and
'Aeson.ToJSON' \/ 'Aeson.FromJSON' superclasses it requires) is /not/
emitted here. Callers that want registry-eligibility — typically the
'Proto.TH.loadProto' splice — emit those instances separately and then
append an empty @instance IsMessage T@ via 'mkIsMessageInstance' once
all the superclasses are in scope.
-}
synthesiseProtoInstances
  :: Type
  -- ^ Fully applied type, e.g. @ConT ''Person@.
  -> Name
  -- ^ Record constructor name (often equal to type name).
  -> Text
  -- ^ Logical proto message name (used by 'PM.messageTypeName').
  -> [ProtoField]
  -> Q [Dec]
synthesiseProtoInstances = synthesiseProtoInstancesWith defaultMessageMeta


synthesiseProtoInstancesWith
  :: MessageMeta
  -> Type
  -> Name
  -> Text
  -> [ProtoField]
  -> Q [Dec]
synthesiseProtoInstancesWith meta ty conName _protoName fs = do
  enc <- mkEncodeInstanceWith meta ty fs
  dec <- mkDecodeInstanceWith meta ty conName fs
  semi <- mkSemigroupInstanceWith meta ty conName fs
  mon <- mkMonoidInstanceWith meta ty conName fs
  pure [enc, dec, semi, mon]


-- ---------------------------------------------------------------------------
-- Encoder / size internals
-- ---------------------------------------------------------------------------

encodeOne :: Name -> ProtoField -> Q Exp
encodeOne msg pf = do
  let getter = AppE (VarE (pfSelector pf)) (VarE msg)
      tagInt = pfTagByte pf
  case pfKind pf of
    FKBare -> do
      v <- newName "v"
      letE
        [valD (varP v) (normalB (pure getter)) []]
        (condDefaultSkipE pf v (encodeSingleE pf tagInt v))
    FKMaybe -> do
      v <- newName "v"
      caseE
        (pure getter)
        [ match (conP 'Nothing []) (normalB [|mempty|]) []
        , match
            (conP 'Just [varP v])
            (normalB (encodeSingleE pf tagInt v))
            []
        ]
    FKRepeated rep mode ->
      case (mode, pfType pf) of
        -- Packed encoding only kicks in for packable scalars; the
        -- caller is responsible for not setting 'ModePacked' on
        -- 'PFSubmessage' / 'PFEnum' / 'PFScalar SString' /
        -- 'PFScalar SBytes' (the deriver's bridges enforce this,
        -- and 'scalarPackable' is the source of truth).
        (ModePacked, PFScalar sc) | scalarPackable sc -> do
          encodePackedScalarE pf sc getter
        (ModePacked, PFEnum) -> do
          encodePackedEnumE pf getter
        _ -> do
          v <- newName "v"
          acc <- newName "acc"
          perElement <- encodeSingleE pf tagInt v
          foldFnE <- repeatedFoldlE rep
          let step =
                LamE
                  [VarP acc, VarP v]
                  (InfixE (Just (VarE acc)) (VarE '(<>)) (Just perElement))
          pure (AppE (AppE (AppE foldFnE step) (VarE 'mempty)) getter)
    FKMap mks -> do
      kV <- newName "k"
      vV <- newName "v"
      acc <- newName "acc"
      keyEnc <- encodeMapKeyE mks kV
      valEnc <- encodeMapValueE pf vV
      -- 'mapField' takes the already-sized key and value
      -- builders and produces a sized map entry (one length-prefixed
      -- submessage per entry). Same shape as the pure-text codegen.
      let entry =
            AppE
              ( AppE
                  ( AppE
                      (VarE 'PE.mapField)
                      (LitE (IntegerL (fromIntegral (pfTag pf))))
                  )
                  keyEnc
              )
              valEnc
          step =
            LamE
              [VarP acc, VarP kV, VarP vV]
              (InfixE (Just (VarE acc)) (VarE '(<>)) (Just entry))
      pure (AppE (AppE (AppE (VarE 'Map.foldlWithKey') step) (VarE 'mempty)) getter)
    FKOneof variants -> do
      inner <- newName "ov"
      arms <- traverse (oneofEncodeArm inner) variants
      let allArms =
            match (conP 'Nothing []) (normalB [|mempty|]) []
              : zipWith
                ( \v body ->
                    match
                      (conP 'Just [conP (ovConstructor v) [varP inner]])
                      (normalB (pure body))
                      []
                )
                variants
                arms
      caseE (pure getter) allArms


sizeOne :: Name -> ProtoField -> Q Exp
sizeOne msg pf = do
  let getter = AppE (VarE (pfSelector pf)) (VarE msg)
  case pfKind pf of
    FKBare -> do
      v <- newName "v"
      letE
        [valD (varP v) (normalB (pure getter)) []]
        (condDefaultSkipS pf v (sizeSingleE pf v))
    FKMaybe -> do
      v <- newName "v"
      caseE
        (pure getter)
        [ match (conP 'Nothing []) (normalB [|0 :: Int|]) []
        , match
            (conP 'Just [varP v])
            (normalB (sizeSingleE pf v))
            []
        ]
    FKRepeated rep mode ->
      case (mode, pfType pf) of
        (ModePacked, PFScalar sc)
          | scalarPackable sc ->
              sizePackedScalarE pf sc getter
        (ModePacked, PFEnum) ->
          sizePackedEnumE pf getter
        _ -> do
          v <- newName "v"
          acc <- newName "acc"
          -- 'sizeSingleE' returns the FULL per-element wire size
          -- (tag + payload), so accumulate it directly. The
          -- previous version added 'tagSize' on top, which
          -- overcounted by one tag width per element and broke
          -- two-pass encoders for messages whose unpacked
          -- repeated fields had multi-byte tags (e.g. field 89
          -- in test_messages_proto3.TestAllTypesProto3 —
          -- exercised by the conformance suite's
          -- ValidDataOneof.MESSAGE.Merge tests).
          per <- sizeSingleE pf v
          foldFnE <- repeatedFoldlE rep
          let step =
                LamE
                  [VarP acc, VarP v]
                  (InfixE (Just (VarE acc)) (VarE '(+)) (Just per))
          pure (AppE (AppE (AppE foldFnE step) (LitE (IntegerL 0))) getter)
    FKMap mks -> do
      -- Exact map size: for each entry we sum
      --   tag(2) + len(varint) + entryPayload
      -- where entryPayload is keyPayload(tag1+key bytes)
      --                   + valuePayload(tag2+value bytes).
      -- This replaces the coarse 10-byte-per-entry upper bound the
      -- old emitter shipped, which broke two-pass encoders for
      -- maps whose entries exceeded 10 bytes (long string keys,
      -- submessage values, etc.).
      kV <- newName "k"
      vV <- newName "v"
      acc <- newName "acc"
      keySize <- mapKeyEntrySizeE mks kV
      valSize <- mapValueEntrySizeE pf vV
      tagSz <- [|PWE.tagSize $(litE (integerL (fromIntegral (pfTag pf))))|]
      let entrySize =
            InfixE (Just keySize) (VarE '(+)) (Just valSize)
          step =
            LamE
              [VarP acc, VarP kV, VarP vV]
              ( InfixE
                  (Just (VarE acc))
                  (VarE '(+))
                  ( Just
                      ( InfixE
                          (Just tagSz)
                          (VarE '(+))
                          ( Just
                              ( InfixE
                                  ( Just
                                      ( AppE
                                          (VarE 'PWE.varintSize)
                                          (AppE (VarE 'fromIntegral) entrySize)
                                      )
                                  )
                                  (VarE '(+))
                                  (Just entrySize)
                              )
                          )
                      )
                  )
              )
      pure
        ( AppE
            ( AppE
                (AppE (VarE 'Map.foldlWithKey') step)
                (LitE (IntegerL 0))
            )
            getter
        )
    FKOneof variants -> do
      inner <- newName "ov"
      arms <- traverse (oneofSizeArm inner) variants
      let allArms =
            match (conP 'Nothing []) (normalB [|0 :: Int|]) []
              : zipWith
                ( \v body ->
                    match
                      (conP 'Just [conP (ovConstructor v) [varP inner]])
                      (normalB (pure body))
                      []
                )
                variants
                arms
      caseE (pure getter) allArms


oneofEncodeArm :: Name -> OneofVariant -> Q Exp
oneofEncodeArm v ov =
  let pseudo =
        ( protoField
            (ovConstructor ov)
            (ovTag ov)
            FKBare
            (ovType ov)
            (ovInnerTy ov)
        )
          { pfStringAdapter = ovStringAdapter ov
          , pfBytesAdapter = ovBytesAdapter ov
          }
      tagInt = case ovType ov of
        PFScalar s -> tagByteFor (ovTag ov) (scalarWireType s)
        PFSubmessage -> tagByteFor (ovTag ov) wireLengthDelimited
        PFEnum -> tagByteFor (ovTag ov) wireVarint
  in encodeSingleE pseudo tagInt v


oneofSizeArm :: Name -> OneofVariant -> Q Exp
oneofSizeArm v ov =
  let pseudo =
        ( protoField
            (ovConstructor ov)
            (ovTag ov)
            FKBare
            (ovType ov)
            (ovInnerTy ov)
        )
          { pfStringAdapter = ovStringAdapter ov
          , pfBytesAdapter = ovBytesAdapter ov
          }
  in [|
      PWE.tagSize $(litE (integerL (fromIntegral (ovTag ov))))
        + $(sizeSingleE pseudo v)
      |]


pfTagByte :: ProtoField -> Int
pfTagByte pf = case pfKind pf of
  FKMap _ -> tagByteFor (pfTag pf) wireLengthDelimited
  FKOneof _ -> 0
  -- Repeated packed scalars use a length-delimited block; the
  -- tag-per-element shape only kicks in for unpacked encoding,
  -- which 'encodeOne' handles via the legacy path. The packed
  -- branch ignores @pfTagByte@ entirely (it builds the tag itself
  -- via 'putTag').
  FKRepeated _ ModePacked -> tagByteFor (pfTag pf) wireLengthDelimited
  _ -> case pfType pf of
    PFScalar s -> tagByteFor (pfTag pf) (scalarWireType s)
    PFSubmessage -> tagByteFor (pfTag pf) wireLengthDelimited
    PFEnum -> tagByteFor (pfTag pf) wireVarint


{- | Build the encoder for one map key. Keys are always singular
scalars at field number 1 inside the entry message.
-}
encodeMapKeyE :: MapKeyScalar -> Name -> Q Exp
encodeMapKeyE mks kVar =
  let pseudo =
        protoField
          kVar
          1
          FKBare
          (PFScalar (scalarOfMapKey mks))
          (ConT ''Int)
      tagInt = tagByteFor 1 (scalarWireType (scalarOfMapKey mks))
  in encodeSingleE pseudo tagInt kVar


{- | Bytes contributed by one map-key on the wire (1 entry-tag byte
+ the key payload). Used by the exact entry-size emitter for
'FKMap'.
-}
mapKeyEntrySizeE :: MapKeyScalar -> Name -> Q Exp
mapKeyEntrySizeE mks kVar =
  let pseudo =
        protoField
          kVar
          1
          FKBare
          (PFScalar (scalarOfMapKey mks))
          (ConT ''Int)
  in [|1 + $(sizeSingleE pseudo kVar)|]


{- | Bytes contributed by one map-value on the wire (1 entry-tag
byte + the value payload).
-}
mapValueEntrySizeE :: ProtoField -> Name -> Q Exp
mapValueEntrySizeE pf vVar =
  let valueField = pf {pfTag = 2} -- inside the entry
  in [|1 + $(sizeSingleE valueField vVar)|]


{- | Build the encoder for one map value at field number 2 inside
the entry message.
-}
encodeMapValueE :: ProtoField -> Name -> Q Exp
encodeMapValueE pf vVar =
  let valueField = pf {pfTag = 2} -- inside the entry
      tagInt = case pfType pf of
        PFScalar s -> tagByteFor 2 (scalarWireType s)
        PFSubmessage -> tagByteFor 2 wireLengthDelimited
        PFEnum -> tagByteFor 2 wireVarint
  in encodeSingleE valueField tagInt vVar


condDefaultSkipE :: ProtoField -> Name -> Q Exp -> Q Exp
condDefaultSkipE pf v body =
  case pfType pf of
    PFSubmessage -> body
    PFScalar sc ->
      [|if $(defaultPredFieldE pf sc v) then mempty else $body|]
    PFEnum ->
      [|if fromEnum $(varE v) == 0 then mempty else $body|]


condDefaultSkipS :: ProtoField -> Name -> Q Exp -> Q Exp
condDefaultSkipS pf v body =
  case pfType pf of
    PFSubmessage -> body
    PFScalar sc ->
      [|if $(defaultPredFieldE pf sc v) then (0 :: Int) else $body|]
    PFEnum ->
      [|if fromEnum $(varE v) == 0 then (0 :: Int) else $body|]


{- | Default predicate, dispatched by string / bytes adapter
when applicable. For non-string/non-bytes scalars the
adapter is ignored.
-}
defaultPredFieldE :: ProtoField -> Scalar -> Name -> Q Exp
defaultPredFieldE pf sc v = case sc of
  SString -> [|$(stringIsEmpty (pfStringAdapter pf)) $(varE v)|]
  SBytes -> [|$(bytesIsEmpty (pfBytesAdapter pf)) $(varE v)|]
  _ -> defaultPredE sc v


defaultPredE :: Scalar -> Name -> Q Exp
defaultPredE sc v = case sc of
  SBool -> [|not $(varE v)|]
  SString -> [|T.null $(varE v)|]
  SBytes -> [|BS.null $(varE v)|]
  SFloat -> [|($(varE v) :: Float) == 0|]
  SDouble -> [|($(varE v) :: Double) == 0|]
  _ -> [|$(varE v) == 0|]


encodeSingleE :: ProtoField -> Int -> Name -> Q Exp
encodeSingleE pf tagInt v
  -- Field numbers > 31 produce tag bytes > 255 (since
  -- @tag = (fn << 3) | wt@ — fn=32 with the smallest wire type
  -- already gives 256). The 'PA.archXxx' family bakes the tag
  -- into a single 'Word8'; we have to reach for the slower
  -- @PE.encodeField*@ helpers in those cases (those varint-encode
  -- the tag and so handle any field number).
  | tagInt > 0x7F = encodeSingleSlowE pf v
  | otherwise = encodeSingleArchE pf tagInt v


{- | Single-byte-tag fast path. Caller has verified
@tagInt <= 0x7F@ (i.e. field number <= 15) so the
'PA.archXxx' bake-in is safe. (We keep the threshold at the
one-byte varint boundary rather than 255 because that's the
spec-correct boundary; anything above it would round-trip but
the varint bookkeeping would silently differ from the
pure-text codegen.)
-}
encodeSingleArchE :: ProtoField -> Int -> Name -> Q Exp
encodeSingleArchE pf tagInt v =
  let tagWord = litE (integerL (fromIntegral tagInt))
      var = varE v
  in case pfType pf of
      PFScalar SInt32 -> [|PA.archVarint $tagWord (fromIntegral ($var :: Int32))|]
      PFScalar SInt64 -> [|PA.archVarint $tagWord (fromIntegral ($var :: Int64))|]
      PFScalar SUInt32 -> [|PA.archVarint $tagWord (fromIntegral ($var :: Word32))|]
      PFScalar SUInt64 -> [|PA.archVarint $tagWord ($var :: Word64)|]
      PFScalar SSInt32 -> [|PA.archSVarint32 $tagWord ($var :: Int32)|]
      PFScalar SSInt64 -> [|PA.archSVarint64 $tagWord ($var :: Int64)|]
      PFScalar SFixed32 -> [|PA.archFixed32 $tagWord ($var :: Word32)|]
      PFScalar SFixed64 -> [|PA.archFixed64 $tagWord ($var :: Word64)|]
      PFScalar SSFixed32 -> [|PA.archFixed32 $tagWord (fromIntegral ($var :: Int32))|]
      PFScalar SSFixed64 -> [|PA.archFixed64 $tagWord (fromIntegral ($var :: Int64))|]
      PFScalar SBool -> [|PA.archBool $tagWord ($var :: Bool)|]
      PFScalar SFloat -> [|PA.archFloat $tagWord ($var :: Float)|]
      PFScalar SDouble -> [|PA.archDouble $tagWord ($var :: Double)|]
      PFScalar SString -> stringEncodeE (pfStringAdapter pf) tagInt v
      PFScalar SBytes -> bytesEncodeE (pfBytesAdapter pf) tagInt v
      PFSubmessage ->
        -- New shape: 'archSubmessage' takes the child's
        -- 'SizedBuilder' directly. The size flows up from the
        -- child's own 'buildSized' traversal, so there's no
        -- separate @messageSize@ pre-pass.
        [|PA.archSubmessage $tagWord (PE.buildSized $var)|]
      PFEnum ->
        -- 1-byte-tag enum field: bake the tag into 'archVarint'
        -- (which returns 'SizedBuilder', carrying the enum's
        -- varint byte count for the parent's size accumulator).
        [|PA.archVarint $tagWord (fromIntegral (fromEnum $var))|]


{- | Slow path: field number > 15, so the wire tag is two or more
bytes. We dispatch through @Proto.Encode.sizedField*@, which
takes a field number (Int) and varint-encodes the tag while
producing a 'SizedBuilder' that carries the fragment's byte
count up to the enclosing message.
-}
encodeSingleSlowE :: ProtoField -> Name -> Q Exp
encodeSingleSlowE pf v =
  let var = varE v
      fieldN = litE (integerL (fromIntegral (pfTag pf)))
  in case pfType pf of
      PFScalar SInt32 -> [|PE.fieldVarint $fieldN (fromIntegral ($var :: Int32))|]
      PFScalar SInt64 -> [|PE.fieldVarint $fieldN (fromIntegral ($var :: Int64))|]
      PFScalar SUInt32 -> [|PE.fieldVarint $fieldN (fromIntegral ($var :: Word32))|]
      PFScalar SUInt64 -> [|PE.fieldVarint $fieldN ($var :: Word64)|]
      PFScalar SSInt32 -> [|PE.fieldSVarint32 $fieldN ($var :: Int32)|]
      PFScalar SSInt64 -> [|PE.fieldSVarint64 $fieldN ($var :: Int64)|]
      PFScalar SFixed32 -> [|PE.fieldFixed32 $fieldN ($var :: Word32)|]
      PFScalar SFixed64 -> [|PE.fieldFixed64 $fieldN ($var :: Word64)|]
      PFScalar SSFixed32 -> [|PE.fieldFixed32 $fieldN (fromIntegral ($var :: Int32))|]
      PFScalar SSFixed64 -> [|PE.fieldFixed64 $fieldN (fromIntegral ($var :: Int64))|]
      PFScalar SBool -> [|PE.fieldBool $fieldN ($var :: Bool)|]
      PFScalar SFloat -> [|PE.fieldFloat $fieldN ($var :: Float)|]
      PFScalar SDouble -> [|PE.fieldDouble $fieldN ($var :: Double)|]
      -- Slow-path (multi-byte tag) string / bytes: same trick as
      -- 'stringEncodeE' / 'bytesEncodeE' — compose the adapter's
      -- pre-existing @size@ and @encode@ pieces into a 'SizedBuilder'
      -- with the tag-width correction.
      PFScalar SString ->
        [|
          PSB.sized
            ($(stringSize (pfStringAdapter pf)) $var + PWE.tagSize $fieldN - 1)
            ($(stringEncode (pfStringAdapter pf)) $fieldN $var)
          |]
      PFScalar SBytes ->
        [|
          PSB.sized
            ($(bytesSize (pfBytesAdapter pf)) $var + PWE.tagSize $fieldN - 1)
            ($(bytesEncode (pfBytesAdapter pf)) $fieldN $var)
          |]
      PFSubmessage -> [|PE.fieldMessage $fieldN (PE.buildSized $var)|]
      PFEnum ->
        [|
          PE.fieldVarint
            $fieldN
            (fromIntegral (fromEnum $var))
          |]


{- | Adapter-based string encoder. Returns a 'SizedBuilder' built
from two adapter pieces:

* the adapter's @'stringEncode'@ Q-spliced function — @Int -> a ->
  Builder@ — emits the tag varint + length varint + UTF-8 bytes;
* the adapter's @'stringSize'@ Q-spliced function — @a -> Int@ —
  returns the wire byte count assuming a /one-byte/ tag.

To produce a correct 'SizedBuilder' for /any/ field number we
correct the size for the actual tag width: @stringSize val + tagSize
fieldN - 1@. No materialisation, no extra allocation — just two
existing adapter functions composed.
-}
stringEncodeE :: StringAdapter -> Int -> Name -> Q Exp
stringEncodeE adapter tagInt v =
  let fieldN = litE (integerL (fromIntegral (tagInt `quot` 8)))
      var = varE v
  in
    [|
      PSB.sized
        ($(stringSize adapter) $var + PWE.tagSize $fieldN - 1)
        ($(stringEncode adapter) $fieldN $var)
      |]


-- | Adapter-based bytes encoder. Same shape as 'stringEncodeE',
-- using 'bytesSize' \/ 'bytesEncode' instead.
bytesEncodeE :: BytesAdapter -> Int -> Name -> Q Exp
bytesEncodeE adapter tagInt v =
  let fieldN = litE (integerL (fromIntegral (tagInt `quot` 8)))
      var = varE v
  in
    [|
      PSB.sized
        ($(bytesSize adapter) $var + PWE.tagSize $fieldN - 1)
        ($(bytesEncode adapter) $fieldN $var)
      |]


{- | Encode a packed repeated scalar field. Walks the (possibly
boxed) container twice: once with 'sizeSingleE'-equivalent
per-element sizing to compute the payload length, once to emit
the per-element payload bytes. The on-wire shape is
@tag(LengthDelimited) || varint(payloadLen) || elem ... || elem@.

Only called for packable scalars; non-packable elements
(string / bytes / submessage / enum) are routed through
'encodeSingleE' per occurrence.
-}
encodePackedScalarE :: ProtoField -> Scalar -> Exp -> Q Exp
encodePackedScalarE pf sc getter = do
  foldFnE <- repeatedFoldlE rep
  let tagN = fromIntegral (pfTag pf) :: Integer
      fieldN = litE (integerL (fromIntegral (pfTag pf `quot` 8)))
  v <- newName "v"
  acc <- newName "acc"
  perSizeE <- packedElemSizeE sc v
  perBytesE <- packedElemBytesE sc v
  let sizeStep =
        LamE
          [VarP acc, VarP v]
          (InfixE (Just (VarE acc)) (VarE '(+)) (Just perSizeE))
      bytesStep =
        LamE
          [VarP acc, VarP v]
          (InfixE (Just (VarE acc)) (VarE '(<>)) (Just perBytesE))
      sizeE =
        AppE
          ( AppE
              (AppE foldFnE sizeStep)
              (LitE (IntegerL 0))
          )
          getter
      bytesE =
        AppE
          ( AppE
              (AppE foldFnE bytesStep)
              (VarE 'mempty)
          )
          getter
  [|
    if $(emptyContainerE rep getter)
      then mempty
      else
        let !payloadSize = ($(pure sizeE)) :: Int
        in PSB.sized
            ( PWE.tagSize $fieldN
                + PWE.varintSize (fromIntegral payloadSize)
                + payloadSize
            )
            ( PWE.putTag $(litE (IntegerL tagN)) PWire.WireLengthDelimited
                <> PWE.putVarint (fromIntegral payloadSize)
                <> $(pure bytesE)
            )
    |]
  where
    rep = case pfKind pf of
      FKRepeated r _ -> r
      _ -> PR.vectorAdapter -- never reached


{- | Size of a packed repeated scalar field on the wire (tag +
length-prefix + payload, or 0 when empty).
-}
sizePackedScalarE :: ProtoField -> Scalar -> Exp -> Q Exp
sizePackedScalarE pf sc getter = do
  foldFnE <- repeatedFoldlE rep
  let tagN = fromIntegral (pfTag pf) :: Integer
  v <- newName "v"
  acc <- newName "acc"
  perSizeE <- packedElemSizeE sc v
  let step =
        LamE
          [VarP acc, VarP v]
          (InfixE (Just (VarE acc)) (VarE '(+)) (Just perSizeE))
      foldedE =
        AppE
          ( AppE
              (AppE foldFnE step)
              (LitE (IntegerL 0))
          )
          getter
  [|
    if $(emptyContainerE rep getter)
      then 0 :: Int
      else
        let !payloadSize = ($(pure foldedE)) :: Int
        in PWE.tagSize $(litE (IntegerL tagN))
            + PWE.varintSize (fromIntegral payloadSize)
            + payloadSize
    |]
  where
    rep = case pfKind pf of
      FKRepeated r _ -> r
      _ -> PR.vectorAdapter


{- | Per-element on-wire byte size for a packed scalar (no tag,
no length prefix — payload bytes only).
-}
packedElemSizeE :: Scalar -> Name -> Q Exp
packedElemSizeE sc v =
  let var = varE v
  in case sc of
      SInt32 -> [|PWE.varintSize (fromIntegral ($var :: Int32))|]
      SInt64 -> [|PWE.varintSize (fromIntegral ($var :: Int64))|]
      SUInt32 -> [|PWE.varintSize (fromIntegral ($var :: Word32))|]
      SUInt64 -> [|PWE.varintSize ($var :: Word64)|]
      SSInt32 -> [|PWE.varintSize (fromIntegral (PWE.zigZag32 ($var :: Int32)))|]
      SSInt64 -> [|PWE.varintSize (PWE.zigZag64 ($var :: Int64))|]
      SFixed32 -> [|4 :: Int|]
      SFixed64 -> [|8 :: Int|]
      SSFixed32 -> [|4 :: Int|]
      SSFixed64 -> [|8 :: Int|]
      SBool -> [|1 :: Int|]
      SFloat -> [|4 :: Int|]
      SDouble -> [|8 :: Int|]
      SString -> error "Proto.Internal.Derive: SString is not packable"
      SBytes -> error "Proto.Internal.Derive: SBytes is not packable"


{- | Per-element on-wire bytes for a packed scalar (just the
payload — no tag, no length prefix).
-}
packedElemBytesE :: Scalar -> Name -> Q Exp
packedElemBytesE sc v =
  let var = varE v
  in case sc of
      SInt32 -> [|PWE.putVarint (fromIntegral ($var :: Int32))|]
      SInt64 -> [|PWE.putVarint (fromIntegral ($var :: Int64))|]
      SUInt32 -> [|PWE.putVarint (fromIntegral ($var :: Word32))|]
      SUInt64 -> [|PWE.putVarint ($var :: Word64)|]
      SSInt32 -> [|PWE.putSVarint32 ($var :: Int32)|]
      SSInt64 -> [|PWE.putSVarint64 ($var :: Int64)|]
      SFixed32 -> [|PWE.putFixed32 ($var :: Word32)|]
      SFixed64 -> [|PWE.putFixed64 ($var :: Word64)|]
      SSFixed32 -> [|PWE.putFixed32 (fromIntegral ($var :: Int32))|]
      SSFixed64 -> [|PWE.putFixed64 (fromIntegral ($var :: Int64))|]
      SBool -> [|PWE.putVarint (if ($var :: Bool) then 1 else 0)|]
      SFloat -> [|PWE.putFloat ($var :: Float)|]
      SDouble -> [|PWE.putDouble ($var :: Double)|]
      SString -> error "Proto.Internal.Derive: SString is not packable"
      SBytes -> error "Proto.Internal.Derive: SBytes is not packable"


-- | Emptiness predicate for a repeated container.
emptyContainerE :: RepeatedAdapter -> Exp -> Q Exp
emptyContainerE adapter getter =
  [|$(repeatedIsEmpty adapter) $(pure getter)|]


{- | Packed encoder for a repeated enum field. Mirrors
'encodePackedScalarE' but uses @fromEnum@ to project the
element to a varint-encoded int32.
-}
encodePackedEnumE :: ProtoField -> Exp -> Q Exp
encodePackedEnumE pf getter = do
  foldFnE <- repeatedFoldlE rep
  let tagN = fromIntegral (pfTag pf) :: Integer
      fieldN = litE (integerL (fromIntegral (pfTag pf `quot` 8)))
  v <- newName "v"
  acc <- newName "acc"
  let perSizeE =
        AppE
          (VarE 'PWE.varintSize)
          ( SigE
              ( AppE
                  (VarE 'fromIntegral)
                  (AppE (VarE 'fromEnum) (VarE v))
              )
              (ConT ''Word64)
          )
      perBytesE =
        AppE
          (VarE 'PWE.putVarint)
          ( SigE
              ( AppE
                  (VarE 'fromIntegral)
                  (AppE (VarE 'fromEnum) (VarE v))
              )
              (ConT ''Word64)
          )
      sizeStep =
        LamE
          [VarP acc, VarP v]
          (InfixE (Just (VarE acc)) (VarE '(+)) (Just perSizeE))
      bytesStep =
        LamE
          [VarP acc, VarP v]
          (InfixE (Just (VarE acc)) (VarE '(<>)) (Just perBytesE))
      sizeE =
        AppE
          ( AppE
              (AppE foldFnE sizeStep)
              (LitE (IntegerL 0))
          )
          getter
      bytesE =
        AppE
          ( AppE
              (AppE foldFnE bytesStep)
              (VarE 'mempty)
          )
          getter
  [|
    if $(emptyContainerE rep getter)
      then mempty
      else
        let !payloadSize = ($(pure sizeE)) :: Int
        in PSB.sized
            ( PWE.tagSize $fieldN
                + PWE.varintSize (fromIntegral payloadSize)
                + payloadSize
            )
            ( PWE.putTag $(litE (IntegerL tagN)) PWire.WireLengthDelimited
                <> PWE.putVarint (fromIntegral payloadSize)
                <> $(pure bytesE)
            )
    |]
  where
    rep = case pfKind pf of
      FKRepeated r _ -> r
      _ -> PR.vectorAdapter


-- | Sizer for a repeated enum field in packed mode.
sizePackedEnumE :: ProtoField -> Exp -> Q Exp
sizePackedEnumE pf getter = do
  foldFnE <- repeatedFoldlE rep
  let tagN = fromIntegral (pfTag pf) :: Integer
  v <- newName "v"
  acc <- newName "acc"
  let perSizeE =
        AppE
          (VarE 'PWE.varintSize)
          ( SigE
              ( AppE
                  (VarE 'fromIntegral)
                  (AppE (VarE 'fromEnum) (VarE v))
              )
              (ConT ''Word64)
          )
      step =
        LamE
          [VarP acc, VarP v]
          (InfixE (Just (VarE acc)) (VarE '(+)) (Just perSizeE))
      foldedE =
        AppE
          ( AppE
              (AppE foldFnE step)
              (LitE (IntegerL 0))
          )
          getter
  [|
    if $(emptyContainerE rep getter)
      then 0 :: Int
      else
        let !payloadSize = ($(pure foldedE)) :: Int
        in PWE.tagSize $(litE (IntegerL tagN))
            + PWE.varintSize (fromIntegral payloadSize)
            + payloadSize
    |]
  where
    rep = case pfKind pf of
      FKRepeated r _ -> r
      _ -> PR.vectorAdapter


{- | Size of one value's wire-form footprint (tag byte(s) +
payload). Two paths: fields with field number ≤ 15 use the
one-byte-tag @arch*Size@ family; fields with larger field
numbers fall back to @PWE.tagSize@ + the payload size, which
handles multi-byte varint tags correctly.
-}
sizeSingleE :: ProtoField -> Name -> Q Exp
sizeSingleE pf v
  | tag1Byte = archSizeE
  | otherwise = slowSizeE
  where
    tag1Byte = pfTag pf <= 15
    tagSzE = [|PWE.tagSize $(litE (integerL (fromIntegral (pfTag pf))))|]
    var = varE v

    archSizeE = case pfType pf of
      PFScalar SInt32 -> [|PA.archVarintSize (fromIntegral ($var :: Int32))|]
      PFScalar SInt64 -> [|PA.archVarintSize (fromIntegral ($var :: Int64))|]
      PFScalar SUInt32 -> [|PA.archVarintSize (fromIntegral ($var :: Word32))|]
      PFScalar SUInt64 -> [|PA.archVarintSize ($var :: Word64)|]
      PFScalar SSInt32 -> [|1 + PWE.varintSize (fromIntegral (PWE.zigZag32 ($var :: Int32)))|]
      PFScalar SSInt64 -> [|1 + PWE.varintSize (PWE.zigZag64 ($var :: Int64))|]
      PFScalar SFixed32 -> [|PA.archFixed32Size|]
      PFScalar SFixed64 -> [|PA.archFixed64Size|]
      PFScalar SSFixed32 -> [|PA.archFixed32Size|]
      PFScalar SSFixed64 -> [|PA.archFixed64Size|]
      PFScalar SBool -> [|PA.archBoolSize|]
      PFScalar SFloat -> [|PA.archFixed32Size|]
      PFScalar SDouble -> [|PA.archFixed64Size|]
      PFScalar SString -> stringSizeE (pfStringAdapter pf) v
      PFScalar SBytes -> bytesSizeE (pfBytesAdapter pf) v
      PFSubmessage -> [|PA.archSubmessageSize (PE.messageSize $var)|]
      PFEnum ->
        [|PA.archVarintSize (fromIntegral (fromEnum $var) :: Word64)|]

    -- payload-only sizes: tag byte(s) added separately.
    payloadOnlyE = case pfType pf of
      PFScalar SInt32 -> [|PWE.varintSize (fromIntegral ($var :: Int32))|]
      PFScalar SInt64 -> [|PWE.varintSize (fromIntegral ($var :: Int64))|]
      PFScalar SUInt32 -> [|PWE.varintSize (fromIntegral ($var :: Word32))|]
      PFScalar SUInt64 -> [|PWE.varintSize ($var :: Word64)|]
      PFScalar SSInt32 -> [|PWE.varintSize (fromIntegral (PWE.zigZag32 ($var :: Int32)))|]
      PFScalar SSInt64 -> [|PWE.varintSize (PWE.zigZag64 ($var :: Int64))|]
      PFScalar SFixed32 -> [|4 :: Int|]
      PFScalar SFixed64 -> [|8 :: Int|]
      PFScalar SSFixed32 -> [|4 :: Int|]
      PFScalar SSFixed64 -> [|8 :: Int|]
      PFScalar SBool -> [|1 :: Int|]
      PFScalar SFloat -> [|4 :: Int|]
      PFScalar SDouble -> [|8 :: Int|]
      PFScalar SString -> [|(let !sz = BS.length (TE.encodeUtf8 ($var :: Text)) in PWE.varintSize (fromIntegral sz) + sz)|]
      PFScalar SBytes -> [|(let !sz = BS.length ($var :: ByteString) in PWE.varintSize (fromIntegral sz) + sz)|]
      PFSubmessage -> [|(let !sz = PE.messageSize $var in PWE.varintSize (fromIntegral sz) + sz)|]
      PFEnum -> [|PWE.varintSize (fromIntegral (fromEnum $var) :: Word64)|]

    slowSizeE = [|$tagSzE + $payloadOnlyE|]


-- ---------------------------------------------------------------------------
-- Decoder internals
-- ---------------------------------------------------------------------------

initFor :: (ProtoField, Name) -> Q Exp
initFor (pf, _) = case (pfKind pf, pfType pf) of
  (FKMaybe, _) -> [|Nothing|]
  (FKRepeated rep _, _) -> repeatedEmptyE rep
  (FKMap _, _) -> [|Map.empty|]
  (FKOneof _, _) -> [|Nothing|]
  (FKBare, PFScalar SBool) -> [|False|]
  (FKBare, PFScalar SString) -> stringEmptyE (pfStringAdapter pf)
  (FKBare, PFScalar SBytes) -> bytesEmptyE (pfBytesAdapter pf)
  (FKBare, PFScalar SFloat) -> [|0 :: Float|]
  (FKBare, PFScalar SDouble) -> [|0 :: Double|]
  (FKBare, PFScalar SInt32) -> [|0 :: Int32|]
  (FKBare, PFScalar SInt64) -> [|0 :: Int64|]
  (FKBare, PFScalar SUInt32) -> [|0 :: Word32|]
  (FKBare, PFScalar SUInt64) -> [|0 :: Word64|]
  (FKBare, PFScalar SSInt32) -> [|0 :: Int32|]
  (FKBare, PFScalar SSInt64) -> [|0 :: Int64|]
  (FKBare, PFScalar SFixed32) -> [|0 :: Word32|]
  (FKBare, PFScalar SFixed64) -> [|0 :: Word64|]
  (FKBare, PFScalar SSFixed32) -> [|0 :: Int32|]
  (FKBare, PFScalar SSFixed64) -> [|0 :: Int64|]
  (FKBare, PFEnum) -> [|toEnum 0|]
  (FKBare, PFSubmessage) ->
    [|error "Proto.TH.Derive: bare submessage field has no zero value; wrap in Maybe"|]


-- | Adapter-based empty value for a string field.
stringEmptyE :: StringAdapter -> Q Exp
stringEmptyE = stringEmpty


-- | Adapter-based empty value for a bytes field.
bytesEmptyE :: BytesAdapter -> Q Exp
bytesEmptyE = bytesEmpty


-- | Adapter-based size for a string field.
stringSizeE :: StringAdapter -> Name -> Q Exp
stringSizeE adapter v =
  let var = varE v
  in [|$(stringSize adapter) $var|]


-- | Adapter-based size for a bytes field.
bytesSizeE :: BytesAdapter -> Name -> Q Exp
bytesSizeE adapter v =
  let var = varE v
  in [|$(bytesSize adapter) $var|]


{- | Foldl over a repeated field's container, used by the encoder
and sizer. Splices the adapter's foldl function.
-}
repeatedFoldlE :: RepeatedAdapter -> Q Exp
repeatedFoldlE = repeatedFoldl


{- | Empty value for a repeated container, used by the decoder
accumulator initialiser.
-}
repeatedEmptyE :: RepeatedAdapter -> Q Exp
repeatedEmptyE = repeatedEmpty


{- | @container `snoc` v@ in 'Q'-friendly form. Used by the decode
arm for repeated fields.
-}
repeatedSnocE :: RepeatedAdapter -> Exp -> Name -> Q Exp
repeatedSnocE adapter accE vName =
  [|$(repeatedSnoc adapter) $(pure accE) $(varE vName)|]


{- | A two-argument @snoc@ /function/ for a repeated container, in
the shape required by 'decodePackedInto'.
-}
repeatedSnocFnE :: RepeatedAdapter -> Q Exp
repeatedSnocFnE = repeatedSnoc


{- | True iff this repeated field's element type is a packable
scalar. Submessages, enums, strings, and bytes are non-packable
under the proto3 wire spec.
| True iff this repeated field's element type is packable on
the wire. Submessages aren't (they're length-delimited per
element); strings / bytes aren't either. Scalars and enums
both are: enums are wire-type @varint@ on the singular path,
and proto3 packs them by default.
-}
scalarPackableType :: ProtoField -> Bool
scalarPackableType pf = case pfType pf of
  PFScalar sc -> scalarPackable sc
  PFEnum -> True
  _ -> False


{- | Decode a packed length-delimited block of elements into the
supplied accumulator using 'snocFn' to append each value.

This lives outside the deriver-emitted code so the inner loop
compiles in one place rather than once per call site. The block
length is consumed from the parent decoder; we then drive the
inner element decoder directly against the slice via
'PD.runDecoder''. Termination is on offset reaching the slice
length.
-}
decodePackedInto
  :: PD.Decoder a
  -> (acc -> a -> acc)
  -> acc
  -> PD.Decoder acc
decodePackedInto elemDec snocFn acc0 = do
  bs <- PD.getLengthDelimited
  let total = BS.length bs
      go !acc !off
        | off >= total = pure acc
        | otherwise =
            case PWD.runDecoder' elemDec bs off of
              PWD.DecodeOK v off' -> go (snocFn acc v) off'
              PWD.DecodeFail e -> PD.decodeFail e
  go acc0 0
{-# INLINE decodePackedInto #-}


{- | Decode a singular submessage field that may appear multiple
times on the wire, merging into any previous occurrence.

Proto3 spec: when the same singular submessage field appears
multiple times, the parser must concatenate-and-decode rather
than overwrite. By the proto3 catenation property
(@concat(serialize(x), serialize(y)) == serialize(merge(x, y))@),
we can implement this by re-encoding the previous value and
prepending its bytes to the new occurrence's bytes.

Lives here (rather than in 'Proto.Decode') because the
@MessageEncode@ constraint would otherwise create a cyclic
dependency between 'Proto.Encode' and 'Proto.Decode'.
-}
decodeFieldMessageMerge
  :: forall a
   . (PE.MessageEncode a, PD.MessageDecode a)
  => Maybe a
  -> PD.Decoder (Maybe a)
decodeFieldMessageMerge prev = do
  newBytes <- PD.getLengthDelimited
  let combined = case prev of
        Nothing -> newBytes
        Just old ->
          -- Encode the previous value to bytes (no length prefix
          -- — that's what 'buildMessage' produces) and concat
          -- with the new bytes. Decoding the result gives the
          -- spec-mandated merge.
          let !oldBytes = BL.toStrict (BB.toLazyByteString (PE.buildMessage old))
          in oldBytes <> newBytes
  case PD.decodeMessage combined of
    Right merged -> pure (Just merged)
    Left e ->
      -- Concatenation of two valid messages is always valid per
      -- the proto3 catenation property. A decode failure here
      -- means the new bytes themselves were malformed (truncated
      -- submessage, etc.) — propagate the error rather than
      -- silently drop the new occurrence.
      PD.decodeFail (PD.SubMessageError e)
{-# INLINE decodeFieldMessageMerge #-}


decodeLoopBody
  :: MessageMeta
  -> Name
  -- ^ Record constructor.
  -> Name
  -- ^ Loop function name.
  -> [(ProtoField, Name)]
  -> Maybe Name
  -- ^ Unknown-fields accumulator name (when
  --   'mmUnknownFieldsSel' is set).
  -> Q Exp
decodeLoopBody meta conName loopName pairs ufAccM = do
  fnVar <- newName "fn"
  wtVar <- newName "wt"
  let declaredAssigns = map (Data.Bifunctor.bimap pfSelector VarE) pairs
      ufAssigns = case (mmUnknownFieldsSel meta, ufAccM) of
        (Just sel, Just ufAcc) ->
          [(sel, AppE (VarE 'reverse) (VarE ufAcc))]
        _ -> []
      recE = RecConE conName (declaredAssigns <> ufAssigns)

  fieldMatches <- concat <$> mapM (decodeArm meta loopName wtVar pairs ufAccM) pairs
  let allAccs = map snd pairs
  defaultBody <- case (mmUnknownFieldsSel meta, ufAccM) of
    (Just _, Just ufAcc) -> do
      ufVar <- newName "uf"
      [|
        do
          -- 'withTagM' passes 'wt' as a raw 'Int'; 'captureUnknownField'
          -- and 'skipField' both want a 'WireType'. 'toEnum' bridges
          -- the two without allocating a 'Tag' record on the hot path.
          $(varP ufVar) <- PD.captureUnknownField $(varE fnVar) (toEnum $(varE wtVar))
          $(recurseLoopWithUFE loopName allAccs ufAcc (VarE ufVar))
        |]
    _ ->
      [|
        do
          _ <- PD.skipField (toEnum $(varE wtVar))
          $(recurseLoopE loopName allAccs ufAccM)
        |]
  let defaultMatch = Match WildP (NormalB defaultBody) []
      dispatchE = CaseE (VarE fnVar) (fieldMatches ++ [defaultMatch])

  -- 'withTagM' is the CPS counterpart to 'getTagOrU' + Tag pattern
  -- match: it threads the field number and wire type into the
  -- continuation as raw 'Int' values, never allocating the 'Tag'
  -- record or the 'UMaybe Tag' wrapper. On hot decoders (the small
  -- WKTs and any user message under heavy traffic) this saves one
  -- unboxed-sum case-split and one constructor-tag dispatch per
  -- field, which adds up over millions of messages per second.
  let eofK = AppE (VarE 'pure) recE
      tagK = LamE [VarP fnVar, VarP wtVar] dispatchE
  pure $ AppE (AppE (VarE 'PD.withTagM) eofK) tagK


{- | Build the dispatch arms for one logical field. Most kinds emit
a single arm; oneofs emit one per variant; repeated/map share
the same arm for every occurrence of the same tag.
-}
decodeArm
  :: MessageMeta
  -> Name
  -- ^ recursive loop name
  -> Name
  -- ^ wire-type variable (for packed support)
  -> [(ProtoField, Name)]
  -- ^ all (field, accumulator) pairs
  -> Maybe Name
  -- ^ unknown-fields accumulator (when in scope)
  -> (ProtoField, Name)
  -- ^ the one we're emitting arms for
  -> Q [Match]
decodeArm meta loopName wtVar allPairs ufAccM (pf, accForThis) = case pfKind pf of
  FKBare -> singletonArm pf $ \vName -> do
    decoderE <- fieldDecoderE pf vName
    let recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf) (VarE vName)
    [|
      do
        $(varP vName) <- $(pure decoderE)
        $(pure recurse)
      |]
  FKMaybe -> singletonArm pf $ \vName -> case pfType pf of
    -- Submessage fields under @Maybe@ have a special wire-format
    -- contract: the proto3 spec says "if the same singular
    -- submessage field appears multiple times on the wire, the
    -- parser must merge them" rather than overwrite. We honour
    -- that by feeding the previous accumulator value into the
    -- decoder, which re-encodes it and concatenates with the new
    -- bytes (proto3 catenation property:
    -- @concat(serialize(x), serialize(y)) == serialize(merge(x, y))@).
    PFSubmessage -> do
      let recurse =
            updateAccsE
              loopName
              allPairs
              ufAccM
              (pfSelector pf)
              (VarE vName)
      [|
        do
          $(varP vName) <- decodeFieldMessageMerge $(varE accForThis)
          $(pure recurse)
        |]
    _ -> do
      decoderE <- fieldDecoderE pf vName
      let recurse =
            updateAccsE
              loopName
              allPairs
              ufAccM
              (pfSelector pf)
              (AppE (ConE 'Just) (VarE vName))
      [|
        do
          $(varP vName) <- $(pure decoderE)
          $(pure recurse)
        |]
  FKRepeated rep _mode -> do
    vName <- newName "v"
    payload <- scalarDecoderE pf
    snocE <- repeatedSnocE rep (VarE accForThis) vName
    let recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf) snocE
        recurseAfterPack acc' =
          updateAccsE loopName allPairs ufAccM (pfSelector pf) (VarE acc')
    body <-
      if scalarPackableType pf
        then do
          -- A repeated packable element type (any packable scalar
          -- OR an enum) accepts both wire encodings: wire-type 2
          -- (LengthDelimited) means a packed block, anything else
          -- (in practice the element's natural wire type =
          -- varint / fixed32 / fixed64) means a single unpacked
          -- element. Discriminate at runtime on @wt@ so the same
          -- arm covers both shapes.
          acc' <- newName "acc'"
          elemDec <- scalarDecoderE pf
          elemSnocE <- repeatedSnocFnE rep
          -- 'wt' is now a raw 'Int' (from 'withTagM'), so dispatch on
          -- the numeric wire-type constant directly. 2 is
          -- 'PWire.WireLengthDelimited' (packed wire type per the
          -- proto spec).
          [|
            case $(varE wtVar) of
              2 -> do
                $(varP acc') <-
                  decodePackedInto
                    $(pure elemDec)
                    $(pure elemSnocE)
                    $(varE accForThis)
                $(pure (recurseAfterPack acc'))
              _ -> do
                $(varP vName) <- $(pure payload)
                $(pure recurse)
            |]
        else
          -- Non-packable element type (string, bytes, submessage):
          -- fall through to the original one-element-per-occurrence
          -- shape.
          [|
            do
              $(varP vName) <- $(pure payload)
              $(pure recurse)
            |]
    pure
      [ Match
          (LitP (IntegerL (fromIntegral (pfTag pf))))
          (NormalB body)
          []
      ]
  FKMap mks -> do
    let lit = LitP (IntegerL (fromIntegral (pfTag pf)))
    bsVar <- newName "bs"
    kVar <- newName "k"
    vVar <- newName "v"
    keyDec <- mapKeyDecoderE mks
    valDec <- scalarDecoderE pf
    keyZero <- mapKeyZeroE mks
    valZero <- mapValueZeroE pf
    let insertE =
          AppE
            ( AppE
                (AppE (VarE 'Map.insert) (VarE kVar))
                (VarE vVar)
            )
            (VarE accForThis)
        recurse = updateAccsE loopName allPairs ufAccM (pfSelector pf) insertE
        skipRecurse =
          updateAccsE
            loopName
            allPairs
            ufAccM
            (pfSelector pf)
            (VarE accForThis)
    body <-
      [|
        do
          $(varP bsVar) <- PD.getLengthDelimited
          case PD.runDecoder
            ( PD.decodeMapEntry
                $(pure keyDec)
                $(pure valDec)
                $(pure keyZero)
                $(pure valZero)
            )
            $(varE bsVar) of
            Left _ -> $(pure skipRecurse)
            Right ($(varP kVar), $(varP vVar)) -> $(pure recurse)
        |]
    pure [Match lit (NormalB body) []]
  FKOneof variants ->
    traverse (oneofDecodeArm loopName allPairs ufAccM (pfSelector pf)) variants
  where
    singletonArm thisPf mkBody = do
      vName <- newName "v"
      body <- mkBody vName
      pure
        [ Match
            (LitP (IntegerL (fromIntegral (pfTag thisPf))))
            (NormalB body)
            []
        ]
    _unused_meta = meta -- reserved for future per-arm decisions


oneofDecodeArm
  :: Name
  -> [(ProtoField, Name)]
  -> Maybe Name
  -- ^ unknown-fields accumulator (when in scope)
  -> Name
  -- ^ parent field's selector
  -> OneofVariant
  -> Q Match
oneofDecodeArm loopName allPairs ufAccM sel ov = case ovType ov of
  -- Submessage variants in a oneof have the same merge semantics
  -- as singular submessage fields outside an oneof: when the same
  -- variant is encountered twice on the wire, the parser must
  -- merge both occurrences instead of overwriting. Look up the
  -- previous accumulator value for the oneof carrier; if it
  -- already holds the same variant, decode-and-merge with that
  -- inner submessage; otherwise just decode fresh.
  PFSubmessage -> do
    vName <- newName "v"
    oldVar <- newName "old"
    let con = ovConstructor ov
        prevAcc = lookupAccByName allPairs sel
        newVal = AppE (ConE 'Just) (AppE (ConE con) (VarE vName))
        recurse = updateAccsE loopName allPairs ufAccM sel newVal
        -- @case acc of Just (Con old) -> Just old; _ -> Nothing@
        prevExtractE =
          CaseE
            (VarE prevAcc)
            [ Match
                (ConP 'Just [] [ConP con [] [VarP oldVar]])
                (NormalB (AppE (ConE 'Just) (VarE oldVar)))
                []
            , Match WildP (NormalB (ConE 'Nothing)) []
            ]
    body <-
      [|
        do
          mNew <- decodeFieldMessageMerge $(pure prevExtractE)
          case mNew of
            Just $(varP vName) -> $(pure recurse)
            Nothing ->
              -- 'decodeFieldMessageMerge' only returns
              -- Nothing when its input bytes were empty
              -- AND the previous accumulator was Nothing,
              -- which can't happen here (the wire just
              -- delivered a length-delimited block for
              -- this variant, so newBytes is non-empty
              -- at minimum).
              PD.decodeFail
                ( PD.CustomError
                    "oneof submessage merge produced Nothing"
                )
        |]
    pure (Match (LitP (IntegerL (fromIntegral (ovTag ov)))) (NormalB body) [])
  _ -> do
    vName <- newName "v"
    payload <- variantDecoderE ov
    let conApp = AppE (ConE (ovConstructor ov)) (VarE vName)
        newVal = AppE (ConE 'Just) conApp
        recurse = updateAccsE loopName allPairs ufAccM sel newVal
    body <-
      [|
        do
          $(varP vName) <- $(pure payload)
          $(pure recurse)
        |]
    pure (Match (LitP (IntegerL (fromIntegral (ovTag ov)))) (NormalB body) [])


{- | Look up the accumulator name for a given record selector
inside the loop's per-field pair list. Used by the oneof-merge
splice; should always succeed (the oneof field is a member of
'allPairs').
-}
lookupAccByName :: [(ProtoField, Name)] -> Name -> Name
lookupAccByName pairs sel = case [acc | (pf, acc) <- pairs, pfSelector pf == sel] of
  (acc : _) -> acc
  [] ->
    error
      ( "Proto.Internal.Derive: no accumulator for "
          ++ show sel
      )


{- | Build the recursive @loop a1 a2 ... aN [acc_unknown_]@
application, with the named selector's accumulator replaced by
the given expression. The unknown-fields accumulator is passed
through unchanged.
-}
updateAccsE :: Name -> [(ProtoField, Name)] -> Maybe Name -> Name -> Exp -> Exp
updateAccsE loopName allPairs ufAccM sel newE =
  let args = map pickArg allPairs
      pickArg (pf, acc)
        | pfSelector pf == sel = newE
        | otherwise = VarE acc
      ufArgs = case ufAccM of
        Just ufAcc -> [VarE ufAcc]
        Nothing -> []
  in foldl AppE (VarE loopName) (args <> ufArgs)


fieldDecoderE :: ProtoField -> Name -> Q Exp
fieldDecoderE pf _v = scalarDecoderE pf


variantDecoderE :: OneofVariant -> Q Exp
variantDecoderE ov =
  scalarDecoderE
    ( (protoField (ovConstructor ov) (ovTag ov) FKBare (ovType ov) (ovInnerTy ov))
        { pfStringAdapter = ovStringAdapter ov
        , pfBytesAdapter = ovBytesAdapter ov
        }
    )


scalarDecoderE :: ProtoField -> Q Exp
scalarDecoderE pf = case pfType pf of
  PFScalar SInt32 -> [|(fromIntegral :: Word64 -> Int32) <$> PD.decodeFieldVarint|]
  PFScalar SInt64 -> [|(fromIntegral :: Word64 -> Int64) <$> PD.decodeFieldVarint|]
  PFScalar SUInt32 -> [|(fromIntegral :: Word64 -> Word32) <$> PD.decodeFieldVarint|]
  PFScalar SUInt64 -> [|PD.decodeFieldVarint|]
  PFScalar SSInt32 -> [|PD.decodeFieldSVarint32|]
  PFScalar SSInt64 -> [|PD.decodeFieldSVarint64|]
  PFScalar SFixed32 -> [|PD.decodeFieldFixed32|]
  PFScalar SFixed64 -> [|PD.decodeFieldFixed64|]
  PFScalar SSFixed32 -> [|(fromIntegral :: Word32 -> Int32) <$> PD.decodeFieldFixed32|]
  PFScalar SSFixed64 -> [|(fromIntegral :: Word64 -> Int64) <$> PD.decodeFieldFixed64|]
  PFScalar SBool -> [|PD.decodeFieldBool|]
  PFScalar SFloat -> [|PD.decodeFieldFloat|]
  PFScalar SDouble -> [|PD.decodeFieldDouble|]
  PFScalar SString -> stringDecoderE (pfStringAdapter pf)
  PFScalar SBytes -> bytesDecoderE (pfBytesAdapter pf)
  PFSubmessage -> [|PD.decodeFieldMessage|]
  PFEnum -> [|PD.decodeFieldEnum|]


{- | Adapter-based string decoder. The adapter's stringDecode field is
expected to produce a post-processing function @Text -> a@ (for
strict text it's 'id', for lazy it's 'TL.fromStrict', etc.).
The codegen wraps it: @adapter <$> PD.decodeFieldString@.
-}
stringDecoderE :: StringAdapter -> Q Exp
stringDecoderE adapter = [|$(stringDecode adapter) <$> PD.decodeFieldString|]


-- | Adapter-based bytes decoder.
bytesDecoderE :: BytesAdapter -> Q Exp
bytesDecoderE adapter = [|$(bytesDecode adapter) <$> PD.decodeFieldBytes|]


-- | Decoder for one map key, picking by 'MapKeyScalar'.
mapKeyDecoderE :: MapKeyScalar -> Q Exp
mapKeyDecoderE mks =
  scalarDecoderE
    ( protoField
        (mkName "_k")
        1
        FKBare
        (PFScalar (scalarOfMapKey mks))
        (ConT ''Int)
    )


mapKeyZeroE :: MapKeyScalar -> Q Exp
mapKeyZeroE = \case
  MapKeyInt32 -> [|0 :: Int32|]
  MapKeyInt64 -> [|0 :: Int64|]
  MapKeyUInt32 -> [|0 :: Word32|]
  MapKeyUInt64 -> [|0 :: Word64|]
  MapKeySInt32 -> [|0 :: Int32|]
  MapKeySInt64 -> [|0 :: Int64|]
  MapKeyFixed32 -> [|0 :: Word32|]
  MapKeyFixed64 -> [|0 :: Word64|]
  MapKeySFixed32 -> [|0 :: Int32|]
  MapKeySFixed64 -> [|0 :: Int64|]
  MapKeyBool -> [|False|]
  MapKeyString -> [|T.empty|]


mapValueZeroE :: ProtoField -> Q Exp
mapValueZeroE pf = case pfType pf of
  PFScalar SBool -> [|False|]
  -- For SString / SBytes the per-rep empty value lines up with what
  -- 'stringEmptyE' / 'bytesEmptyE' produce for a regular field; the
  -- 'ProtoField' carries 'pfStringAdapter' / 'pfBytesAdapter' for exactly
  -- this reason, so 'map<K, bytes>' with @fieldBytes = LazyBytesRep@
  -- gets @BL.empty@ here (not @BS.empty@).
  PFScalar SString -> stringEmptyE (pfStringAdapter pf)
  PFScalar SBytes -> bytesEmptyE (pfBytesAdapter pf)
  PFScalar SFloat -> [|0 :: Float|]
  PFScalar SDouble -> [|0 :: Double|]
  PFScalar SInt32 -> [|0 :: Int32|]
  PFScalar SInt64 -> [|0 :: Int64|]
  PFScalar SUInt32 -> [|0 :: Word32|]
  PFScalar SUInt64 -> [|0 :: Word64|]
  PFScalar SSInt32 -> [|0 :: Int32|]
  PFScalar SSInt64 -> [|0 :: Int64|]
  PFScalar SFixed32 -> [|0 :: Word32|]
  PFScalar SFixed64 -> [|0 :: Word64|]
  PFScalar SSFixed32 -> [|0 :: Int32|]
  PFScalar SSFixed64 -> [|0 :: Int64|]
  PFEnum -> [|toEnum 0|]
  PFSubmessage ->
    -- Map value of submessage type: proto3 spec says a missing
    -- value field defaults to the type's default empty message.
    -- Route through 'protoDefaultValue' from the message type's
    -- 'ProtoMessage' instance — GHC infers the type from the
    -- decoder's return type at the use site.
    [|PS.protoDefaultValue|]


recurseLoopE :: Name -> [Name] -> Maybe Name -> Q Exp
recurseLoopE loopName accs ufAccM =
  let ufArgs = case ufAccM of
        Just ufAcc -> [VarE ufAcc]
        Nothing -> []
  in pure (foldl AppE (VarE loopName) (map VarE accs <> ufArgs))


{- | Recurse, replacing the unknown-fields accumulator with @uf : acc@.
Used by the wildcard arm of the decode loop when unknown-field
preservation is enabled.
-}
recurseLoopWithUFE :: Name -> [Name] -> Name -> Exp -> Q Exp
recurseLoopWithUFE loopName accs ufAcc newUF =
  let consE = InfixE (Just newUF) (ConE '(:)) (Just (VarE ufAcc))
  in pure (foldl AppE (VarE loopName) (map VarE accs <> [consE]))
