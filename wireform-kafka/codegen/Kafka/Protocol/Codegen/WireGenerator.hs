{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Kafka.Protocol.Codegen.WireGenerator
Description : Direct-poke 'Wire' code generator for Kafka messages

Mirrors "Kafka.Protocol.Codegen.Generator" but emits code targeting
'Kafka.Protocol.Wire.Wire' instead of 'Data.Bytes.Serial':

  * Three top-level functions per supported message:
    @wireMaxSizeFooMessage :: Int -> FooMessage -> Int@,
    @wirePokeFooMessage    :: Int -> Ptr Word8 -> FooMessage -> IO (Ptr Word8)@,
    @wirePeekFooMessage    :: Int -> 'ForeignPtr' Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FooMessage, Ptr Word8)@.
  * The same trio for every nested + common struct, so the
    message-level codec can recursively call into per-struct pokes
    without going through a 'Serial' detour.
  * Same version-dispatch shape as the existing 'Serial'-shape generator
    (one branch per @(minV, maxV)@ field-set group).
  * Tagged-fields handling at flexible-version boundaries.
  * Per-field flexibleVersions opt-out: a field with
    @"flexibleVersions": "none"@ stays on the non-compact codec even
    when the surrounding message is on a flexible version.

== Coverage

The Wire generator currently handles every type the Kafka 3.x / 4.x
schemas use:

  * primitive scalars (@bool@, @int8 / int16 / int32 / int64@,
    @uint16 / uint32@, @float64@),
  * @string@ / @bytes@ (compact + non-compact + nullable; the
    @records@ alias the parser maps to bytes),
  * @uuid@,
  * arrays of primitives, strings/bytes, and nested structs (compact
    + non-compact + nullable variants),
  * nested struct fields (recursive call into the struct's wirePoke),
  * tagged-fields trailer on flexible message versions,
  * the per-field @flexibleVersions: none@ opt-out,
  * version dispatch over the @(minV, maxV)@ groups the legacy
    generator uses.

Tagged fields with payload bodies (KIP-866-style; e.g.
@CurrentLeader@ on @ProduceResponse v10+@) are emitted natively:
encode builds a @[(tag, ByteString)]@ entry list and writes it via
'WP.pokeTaggedFieldEntries'; decode builds a
@Map Word32 ByteString@ via 'WP.peekTaggedFieldsMap' and
dispatches each known tag through a per-field 'W.runWireGetWith'
decoder, with field defaults for missing tags. There is no Serial
fallback — every schema the parser accepts gets a native codec.
-}
module Kafka.Protocol.Codegen.WireGenerator
  ( -- * Per-message generation
    generateWireFunctions
  , generateNestedWireFunctions
  , generateWireCodecOverride
    -- * Imports the generated module needs
  , generateWireImports
  , isWireSupported
  ) where

import Data.Char (toLower, toUpper)
import Data.Int (Int16)
import Data.List (groupBy, sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Kafka.Protocol.Codegen.Types
import Prettyprinter

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

generateWireImports :: Doc ann
generateWireImports = vsep
  [ "import Foreign.ForeignPtr (ForeignPtr)"
  , "import Foreign.Ptr (Ptr)"
  , "import Data.Word (Word8)"
  , "import qualified Data.ByteString"
  , "import qualified Data.Int"
  , "import qualified Data.Map.Strict"
  , "import qualified Data.Word"
  , "import qualified Kafka.Protocol.Wire as W"
  , "import qualified Kafka.Protocol.Wire.Primitives as WP"
  ]

----------------------------------------------------------------------
-- Support detection
----------------------------------------------------------------------

-- | @True@ iff every field of the schema (recursively, into nested
-- + common structs) is in the WireGenerator's supported subset.
isWireSupported :: ProtocolSchema -> Bool
isWireSupported schema =
  fieldsSupportedTransitive (schemaFields schema)
    && all (\cs -> case fieldFields cs of
                     Just fs -> fieldsSupportedTransitive fs
                     Nothing -> True)
           (schemaCommonStructs schema)

-- | A field set is "wire-supported" iff every nested struct + array
-- element is wire-supported (recursively). Tagged fields with
-- payloads are now handled natively (KIP-866 NodeEndpoints,
-- ProduceResponse CurrentLeader, ...) so they no longer disqualify a
-- schema; the encode emits a per-tag entry list and the decode
-- builds a 'Map Word32 ByteString' that per-tag lookups dispatch
-- through.
fieldsSupportedTransitive :: [FieldSpec] -> Bool
fieldsSupportedTransitive = all isFieldSupportedTransitive

isFieldSupportedTransitive :: FieldSpec -> Bool
isFieldSupportedTransitive f = case fieldType f of
  PrimitiveType t -> isSupportedPrimitive t
  StructType _    -> case fieldFields f of
    Just fs -> fieldsSupportedTransitive fs
    Nothing -> True   -- references a top-level common struct (we
                      -- can't reach its fields from here; trust the
                      -- common-struct check above)
  ArrayType inner -> case fieldFields f of
    Just fs -> fieldsSupportedTransitive fs
    Nothing -> isElementSupported inner

isElementSupported :: TypeSpec -> Bool
isElementSupported = \case
  PrimitiveType t -> isSupportedPrimitive t
  StructType _    -> True
  ArrayType _     -> False  -- nested arrays-of-arrays don't appear in Kafka schemas

isSupportedPrimitive :: Text -> Bool
isSupportedPrimitive = \case
  "bool"    -> True
  "int8"    -> True
  "int16"   -> True
  "int32"   -> True
  "int64"   -> True
  "uint16"  -> True
  "uint32"  -> True
  "string"  -> True
  "bytes"   -> True
  "uuid"    -> True
  "float64" -> True
  _         -> False

----------------------------------------------------------------------
-- Per-message generation
----------------------------------------------------------------------

-- | Emit @wireMaxSizeFoo@ + @wirePokeFoo@ + @wirePeekFoo@ for the
-- supplied schema. Returns 'Nothing' only if the schema's parser
-- output contains a type the generator can't handle (currently:
-- nothing — the 'isWireSupported' check is structural and always
-- returns True against the trunk schema set). The 'Maybe' wrapper
-- is kept defensively in case a future schema introduces a new
-- type spec we haven't taught the generator about.
generateWireFunctions :: ProtocolSchema -> Maybe [Doc ann]
generateWireFunctions schema
  | not (isWireSupported schema) = Nothing
  | otherwise = Just
    [ generateWireMaxSize typeName fields flexibleVer
    , generateWirePoke    typeName fields flexibleVer versions
    , generateWirePeek    typeName fields flexibleVer versions
    ]
  where
    typeName    = toHaskellTypeName (schemaName schema)
    fields      = schemaFields schema
    flexibleVer = parseFlexibleVer (schemaFlexibleVersions schema)
    versions = case parseVersionSpec (schemaValidVersions schema) of
      Right spec -> expandVersionSpec spec
      Left  _    -> []

-- | Emit @wireMaxSize@ + @wirePoke@ + @wirePeek@ for a /nested/
-- struct (or a top-level common struct). The shape is the same as
-- the message-level functions, just with the message's flexible
-- threshold inherited from the parent schema.
--
-- Returns 'Nothing' if the struct contains any field type the
-- WireGenerator doesn't yet handle natively.
generateNestedWireFunctions
  :: Maybe Int16            -- ^ message-level flexible threshold
  -> Text                   -- ^ struct type name (e.g. "MetadataRequestTopic")
  -> [FieldSpec]            -- ^ struct fields
  -> Maybe [Doc ann]
generateNestedWireFunctions flexibleVer structName fields
  | not (fieldsSupportedTransitive fields) = Nothing
  | otherwise = Just
    [ generateNestedWireMaxSize structName fields flexibleVer
    , generateNestedWirePoke    structName fields flexibleVer
    , generateNestedWirePeek    structName fields flexibleVer
      -- A 'defaultStructName' value used by absent-version
      -- field defaults inside other structs that embed this one
      -- as a tagged or version-gated field. Without this, the
      -- decoder would fall back to @undefined :: StructName@,
      -- which crashes the strict record constructor for any
      -- response that doesn't carry the field on the wire.
    , generateNestedWireDefault structName fields
    ]

-- | Emit a native @WireCodec@ instance for /every/ schema. The
-- instance points at codegen-emitted 'wirePokeFoo' / 'wirePeekFoo'
-- / 'wireMaxSizeFoo' functions.
generateWireCodecOverride :: ProtocolSchema -> Doc ann
generateWireCodecOverride schema = vsep
  [ "-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /"
  , "-- 'WC.runDecodeVer' dispatch into the direct-poke functions"
  , "-- generated above."
  , "instance WC.WireCodec" <+> pretty typeName <+> "where"
  , indent 2 $ vsep
      [ "wireCodec = WC.WireCodecImpl"
      , indent 2 $ vsep
          [ "{ WC.wireMaxSizeFor = \\v msg -> wireMaxSize" <> pretty typeName
              <+> "(fromIntegral v) msg"
          , ", WC.wirePokeFor    = \\v p msg -> wirePoke" <> pretty typeName
              <+> "(fromIntegral v) p msg"
          , ", WC.wirePeekFor    = \\v fp basePtr p endPtr ->"
          , indent 4 ("wirePeek" <> pretty typeName
              <+> "(fromIntegral v) fp basePtr p endPtr")
          , "}"
          ]
      , "{-# INLINE wireCodec #-}"
      ]
  ]
  where
    typeName = toHaskellTypeName (schemaName schema)

----------------------------------------------------------------------
-- wireMaxSize
----------------------------------------------------------------------

-- | Worst-case size estimator. Sums per-field upper bounds; never
-- recurses into the value beyond fixed-width metadata, so it's O(1)
-- in field-count terms (and the actual length the poke advances to
-- is what gets shipped on the wire).
generateWireMaxSize
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateWireMaxSize typeName fields flexibleVer =
  let funName = "wireMaxSize" <> pretty typeName
  in vsep
    [ "-- | Worst-case wire size of a" <+> pretty typeName <> "."
    , funName <+> ":: Int -> " <> pretty typeName <+> "-> Int"
    , funName <+> "_version msg ="
    , indent 2 $ buildSizeExpr typeName fields flexibleVer
    , ""
    ]

generateNestedWireMaxSize
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateNestedWireMaxSize structName fields flexibleVer =
  let funName = "wireMaxSize" <> pretty structName
  in vsep
    [ "-- | Worst-case wire size of a" <+> pretty structName <> "."
    , funName <+> ":: Int -> " <> pretty structName <+> "-> Int"
    , funName <+> "_version msg ="
    , indent 2 $ buildSizeExpr structName fields flexibleVer
    , ""
    ]

buildSizeExpr :: Text -> [FieldSpec] -> Maybe Int16 -> Doc ann
buildSizeExpr typeName fields flexibleVer =
  let perField f
        | isPotentiallyTaggedField f, Just _ <- flexibleVer =
            "+" <+> parens ("if" <+> taggedFieldPresent "_version" typeName f
              <+> "then" <+> size <+> "+ 10 else 0")
        | otherwise = "+" <+> size
        where size = fieldMaxSize typeName flexibleVer f
      tagCount = length (filter isPotentiallyTaggedField fields)
      taggedTrailer = case flexibleVer of
        Just _
          | tagCount >= 128 -> "+ 5"
          | otherwise -> "+ 1"
        Nothing -> ""
  in vsep
       [ "0"
       , vsep (map perField fields)
       , taggedTrailer
       ]

fieldMaxSize :: Text -> Maybe Int16 -> FieldSpec -> Doc ann
fieldMaxSize typeName flexibleVer f =
  let acc = parens (pretty (toHaskellFieldName typeName (fieldName f))
                      <+> "msg")
  in case fieldType f of
    PrimitiveType "bool"    -> "1"
    PrimitiveType "int8"    -> "1"
    PrimitiveType "int16"   -> "2"
    PrimitiveType "int32"   -> "4"
    PrimitiveType "int64"   -> "8"
    PrimitiveType "uint16"  -> "2"
    PrimitiveType "uint32"  -> "4"
    PrimitiveType "uuid"    -> "16"
    PrimitiveType "float64" -> "8"
    PrimitiveType "string"
      -- Flexible-API string fields straddle the flexible-version
      -- threshold at runtime: compact at @version >= flexibleVer@,
      -- plain Int16-prefixed at lower versions. Use the dual-format
      -- estimator so the per-message buffer is large enough for
      -- whichever encoding 'wirePoke' picks. (Plain Null is 2
      -- bytes vs compact's 1, so the previous compact-only
      -- estimate was actually undersized for fields encoding
      -- 'P.KafkaString P.Null' on the non-flexible path.)
      | fieldUsesCompact f flexibleVer ->
          "WP.dualStringMaxSize" <+> acc
      | otherwise ->
          "WP.kafkaStringMaxSize" <+> acc
    PrimitiveType "bytes"
      | fieldUsesCompact f flexibleVer ->
          "WP.dualBytesMaxSize" <+> acc
      | otherwise ->
          "WP.kafkaBytesMaxSize" <+> acc
    PrimitiveType _ -> "0"  -- unreachable (gated by isWireSupported)
    -- Nested struct: recurse via the per-struct estimator.
    StructType structName | isFieldNullable f ->
      "(case" <+> acc <+> "of {"
        <+> "P.Null -> 1; P.NotNull s -> 1 + wireMaxSize"
        <> pretty structName <+> "_version s })"
    StructType structName ->
      "wireMaxSize" <> pretty structName <+> "_version" <+> acc
    -- Array: 5-byte header + per-element bound times length.
    ArrayType inner ->
      let header = "5"  -- worst case: UVarInt
          pokeBound = elementMaxSize structName flexibleVer inner
            where structName = case inner of
                    StructType s -> s
                    _            -> ""
      in parens
           ( header <+> "+"
               <+> "(case P.unKafkaArray" <+> acc <+> "of {"
                 <+> "P.NotNull v -> sum (fmap (\\x ->"
                   <+> pokeBound <+> ") v); P.Null -> 0 })"
           )

elementMaxSize :: Text -> Maybe Int16 -> TypeSpec -> Doc ann
elementMaxSize _structName _flexibleVer inner = case inner of
  PrimitiveType "bool"    -> "1"
  PrimitiveType "int8"    -> "1"
  PrimitiveType "int16"   -> "2"
  PrimitiveType "int32"   -> "4"
  PrimitiveType "int64"   -> "8"
  PrimitiveType "uint16"  -> "2"
  PrimitiveType "uint32"  -> "4"
  PrimitiveType "uuid"    -> "16"
  PrimitiveType "float64" -> "8"
  PrimitiveType "string"  -> "WP.compactStringMaxSize (P.toCompactString x)"
  PrimitiveType "bytes"   -> "WP.compactBytesMaxSize (P.toCompactBytes x)"
  PrimitiveType _         -> "0"
  StructType s            -> "wireMaxSize" <> pretty s <+> "_version x"
  ArrayType _             -> "0"  -- arrays-of-arrays are unsupported

----------------------------------------------------------------------
-- wirePoke
----------------------------------------------------------------------

generateWirePoke
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWirePoke typeName _fields _flexibleVer [] =
  emptyPokeStub typeName
generateWirePoke typeName fields flexibleVer versions =
  let funName = "wirePoke" <> pretty typeName
      groups  = groupVersionsByFieldSet versions fields flexibleVer
      branches = map (generatePokeBranch typeName fields flexibleVer) groups
  in vsep
    [ "-- | Direct-poke encoder for" <+> pretty typeName <> "."
    , funName
        <+> ":: Int -> Ptr Word8 -> " <> pretty typeName
        <+> "-> IO (Ptr Word8)"
    , funName <+> "version basePtr msg"
    , vsep branches
    , "  | otherwise = error $ \"wirePoke" <+> pretty typeName
        <+> ": unsupported version: \" ++ show version"
    , ""
    ]

generateNestedWirePoke
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateNestedWirePoke structName fields flexibleVer =
  let funName       = "wirePoke" <> pretty structName
      isFlexCond    = case flexibleVer of
        Just v  -> "version >=" <+> pretty v
        Nothing -> "False"
      regular       = filter (not . isPotentiallyTaggedField) fields
      taggedFields  = filter isPotentiallyTaggedField fields
      (lastVar, stmts) = pokeFieldStmts structName flexibleVer regular
      -- Tagged-fields trailer; same three-way split as
      -- 'generatePokeBranch'. The nested struct uses the parent
      -- message's version, so the tagged-field gates compose
      -- transparently.
      trailer = case flexibleVer of
        Nothing -> ["pure" <+> pretty lastVar]
        Just _ -> case taggedFields of
          [] ->
            [ "if" <+> isFlexCond <+> "then WP.pokeEmptyTaggedFields"
                <+> pretty lastVar <+> "else pure" <+> pretty lastVar
            ]
          _ ->
            [ "if" <+> isFlexCond <+> "then do"
            , indent 2 $ vsep
                [ "let !_taggedEntries = "
                    <> taggedEntriesExpr structName taggedFields
                , "WP.pokeTaggedFieldEntries"
                    <+> pretty lastVar <+> "_taggedEntries"
                ]
            , "else pure" <+> pretty lastVar
            ]
  in vsep
    [ "-- | Direct-poke encoder for" <+> pretty structName <> "."
    , funName
        <+> ":: Int -> Ptr Word8 -> " <> pretty structName
        <+> "-> IO (Ptr Word8)"
    , funName <+> "version basePtr msg = do"
    , indent 2 (vsep (("p0 <- pure basePtr" :: Doc ann) : stmts ++ trailer))
    , ""
    ]

generatePokeBranch
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> (Int16, Int16)
  -> Doc ann
generatePokeBranch typeName fields flexibleVer (minV, maxV) =
  let isFlexible = maybe False (<= minV) flexibleVer
      regularFields =
        filter (\f -> fieldInVersionRange minV maxV f
                       && not (isPotentiallyTaggedField f)) fields
      taggedFields =
        filter isPotentiallyTaggedField fields
      guard
        | minV == maxV =
            "  | version ==" <+> pretty minV <+> "= do"
        | otherwise    =
            "  | version >=" <+> pretty minV
              <+> "&& version <=" <+> pretty maxV <+> "= do"
      (lastVar, stmts) = pokeFieldStmts typeName flexibleVer regularFields
      -- Tagged-fields trailer. Three cases:
      --   * non-flexible message version: nothing (no trailer at all)
      --   * flexible + no tagged fields: emit the empty-tag marker
      --   * flexible + tagged fields: build the entry list, then
      --     emit it via 'pokeTaggedFieldEntries'.
      taggedStmt
        | not isFlexible       = ["pure" <+> pretty lastVar]
        | null taggedFields    =
            ["WP.pokeEmptyTaggedFields" <+> pretty lastVar]
        | otherwise =
            [ "let !_taggedEntries = "
                <> taggedEntriesExpr typeName taggedFields
            , "WP.pokeTaggedFieldEntries"
                <+> pretty lastVar <+> "_taggedEntries"
            ]
  in vsep
    [ guard
    , indent 4 (vsep (("p0 <- pure basePtr" :: Doc ann) : stmts ++ taggedStmt))
    ]

-- | Build the @[(Word32, ByteString)]@ expression for a message's
-- tagged-field trailer. Each entry is conditionally included based
-- on the field's @taggedVersions@ gate.
taggedEntriesExpr :: Text -> [FieldSpec] -> Doc ann
taggedEntriesExpr _typeName [] = "([] :: [(Data.Word.Word32, Data.ByteString.ByteString)])"
taggedEntriesExpr typeName fs =
  hsep $ punctuate " ++"
    [ taggedEntryGuard typeName f
    | f <- fs
    ]

taggedEntryGuard :: Text -> FieldSpec -> Doc ann
taggedEntryGuard typeName f =
  let acc = parens (pretty (toHaskellFieldName typeName (fieldName f))
                      <+> "msg")
      tagNum = case fieldTag f of
        Just t  -> pretty t
        Nothing -> "0"
      payload = taggedFieldPayloadExpr f acc
  in parens
       ("if" <+> taggedFieldPresent "version" typeName f
          <+> "then [(" <> tagNum <> "," <+> payload <> ")]"
          <+> "else []")


taggedFieldPresent :: Doc ann -> Text -> FieldSpec -> Doc ann
taggedFieldPresent version typeName f =
  let gate = taggedVersionGate version f
      acc = parens (pretty (toHaskellFieldName typeName (fieldName f)) <+> "msg")
  in case fieldType f of
       PrimitiveType "uuid" -> gate <+> "&&" <+> acc <+> "/=" <+> generateFieldDefaultDoc f
       _ -> gate


taggedVersionGate :: Doc ann -> FieldSpec -> Doc ann
taggedVersionGate version f = case fieldTaggedVersions f of
  Just spec -> case parseVersionSpec spec of
    Right (VersionFrom v) -> version <+> ">=" <+> pretty v
    Right (VersionRange a b) ->
      version <+> ">=" <+> pretty a <+> "&&" <+> version <+> "<=" <+> pretty b
    Right (ExactVersion v) -> version <+> "==" <+> pretty v
    _ -> "True"
  Nothing -> "True"

-- | Encode the field's value as a 'ByteString' for inclusion in the
-- tagged-field trailer. Tagged fields always use the flexible
-- encoding for their payload (compact strings / bytes / arrays);
-- the wire codec of the field's type takes care of that.
taggedFieldPayloadExpr :: FieldSpec -> Doc ann -> Doc ann
taggedFieldPayloadExpr f acc = case fieldType f of
  PrimitiveType "bool"    -> "W.runWirePut" <+> acc
  PrimitiveType "int8"    -> "W.runWirePut" <+> parens ("(" <> acc <> " :: Data.Int.Int8)")
  PrimitiveType "int16"   -> "W.runWirePut" <+> acc
  PrimitiveType "int32"   -> "W.runWirePut" <+> acc
  PrimitiveType "int64"   -> "W.runWirePut" <+> acc
  PrimitiveType "uint16"  -> "W.runWirePut" <+> acc
  PrimitiveType "uint32"  -> "W.runWirePut" <+> acc
  PrimitiveType "float64" -> "W.runWirePut" <+> acc
  PrimitiveType "uuid"    -> "W.runWirePut" <+> acc
  PrimitiveType "string"  -> "W.runWirePut" <+> parens ("P.toCompactString" <+> acc)
  PrimitiveType "bytes"   -> "W.runWirePut" <+> parens ("P.toCompactBytes" <+> acc)
  PrimitiveType _         -> "(error \"WireGenerator: tagged primitive\")"
  StructType structName ->
    "W.runWirePokeWith"
      <+> parens ("wireMaxSize" <> pretty structName <+> "version" <+> acc)
      <+> parens ("\\p -> wirePoke" <> pretty structName <+> "version p" <+> acc)
  ArrayType inner ->
    "W.runWirePokeWith"
      <+> parens ("5 + (case P.unKafkaArray" <+> acc
                    <+> "of { P.NotNull v -> sum (fmap (\\x -> "
                      <> elementSize inner
                      <> ") v); P.Null -> 0 })")
      <+> parens ("\\p -> WP.pokeCompactArray"
                    <+> elementPokeForTagged inner
                    <+> "p" <+> acc)
  where
    elementSize :: TypeSpec -> Doc ann
    elementSize = \case
      PrimitiveType "bool"    -> "1"
      PrimitiveType "int8"    -> "1"
      PrimitiveType "int16"   -> "2"
      PrimitiveType "int32"   -> "4"
      PrimitiveType "int64"   -> "8"
      PrimitiveType "uint16"  -> "2"
      PrimitiveType "uint32"  -> "4"
      PrimitiveType "uuid"    -> "16"
      PrimitiveType "float64" -> "8"
      PrimitiveType "string"  -> "WP.compactStringMaxSize (P.toCompactString x)"
      PrimitiveType "bytes"   -> "WP.compactBytesMaxSize  (P.toCompactBytes  x)"
      StructType s            -> "wireMaxSize" <> pretty s <+> "version x"
      _                       -> "0"

    elementPokeForTagged :: TypeSpec -> Doc ann
    elementPokeForTagged = \case
      PrimitiveType "bool"    -> "(\\p_ x -> W.pokeWord8 p_ (if x then 1 else 0))"
      PrimitiveType "int8"    -> "(\\p_ x -> W.pokeWord8 p_ (fromIntegral (x :: Data.Int.Int8)))"
      PrimitiveType "int16"   -> "W.pokeInt16BE"
      PrimitiveType "int32"   -> "W.pokeInt32BE"
      PrimitiveType "int64"   -> "W.pokeInt64BE"
      PrimitiveType "uint16"  -> "W.pokeWord16BE"
      PrimitiveType "uint32"  -> "W.pokeWord32BE"
      PrimitiveType "uuid"    -> "WP.pokeKafkaUuid"
      PrimitiveType "float64" -> "W.pokeFloat64BE"
      PrimitiveType "string"  -> "(\\p_ s -> WP.pokeCompactString p_ (P.toCompactString s))"
      PrimitiveType "bytes"   -> "(\\p_ b -> WP.pokeCompactBytes  p_ (P.toCompactBytes  b))"
      StructType s            -> "(\\p_ x -> wirePoke" <> pretty s <+> "version p_ x)"
      _                       -> "(\\p_ _ -> pure p_)"

-- | Emit per-field poke statements; threads cursor through @p0, p1, …@.
--
-- For fields whose @versions:@ spec is anything other than @"0+"@
-- (always present) we wrap the per-field encoder in a runtime
-- version guard:
--
-- @
-- p1 <- if version >= 13 then WP.pokeKafkaUuid p0 (...) else pure p0
-- @
--
-- Without the guard, nested-struct decoders for messages like
-- 'TopicProduceResponse' would unconditionally pull a
-- @topicProduceResponseTopicId@ KafkaUuid (KIP-516, v13+) off the
-- wire even at v3-v12 — eating into the next field's bytes and
-- corrupting the rest of the response. (Top-level messages
-- don't hit this because 'generatePokeBranch' filters by
-- 'fieldInVersionRange' before calling here; nested structs
-- emit a single un-branched function and so /must/ guard at
-- runtime.)
pokeFieldStmts
  :: Text
  -> Maybe Int16
  -> [FieldSpec]
  -> (Text, [Doc ann])
pokeFieldStmts typeName flexibleVer fs = go (0 :: Int) fs []
  where
    go i [] acc = (cur i, reverse acc)
    go i (f : rest) acc =
      let !next = i + 1
          body  = pokeFieldExpr typeName flexibleVer f (cur i)
          guarded = case fieldVersionGuard f of
            Nothing   -> body
            Just cond -> parens
              ("if" <+> cond <+> "then" <+> body
                 <+> "else pure" <+> pretty (cur i))
          stmt  = pretty (cur next) <+> "<-" <+> guarded
      in go next rest (stmt : acc)

    cur n = "p" <> T.pack (show n)

-- | Compile the field's @versions:@ JSON spec to an optional
-- runtime guard expression in terms of the surrounding
-- @version@ binding. 'Nothing' means \"always present\" — emit
-- no guard at all and let the encoder/decoder run unconditionally.
fieldVersionGuard :: FieldSpec -> Maybe (Doc ann)
fieldVersionGuard f =
  case parseVersionSpec (fieldVersions f) of
    Right (VersionFrom 0)         -> Nothing
    Right (VersionFrom v)         -> Just ("version >=" <+> pretty v)
    Right (ExactVersion v)        -> Just ("version ==" <+> pretty v)
    Right (VersionRange minV maxV)
      | minV == 0                 -> Just ("version <=" <+> pretty maxV)
      | otherwise                 -> Just ("version >=" <+> pretty minV
                                            <+> "&& version <=" <+> pretty maxV)
    Right NoVersions              -> Just "False"
    Left _                        -> Nothing

pokeFieldExpr
  :: Text
  -> Maybe Int16
  -> FieldSpec
  -> Text
  -> Doc ann
pokeFieldExpr typeName flexibleVer f cur =
  let acc = parens (pretty (toHaskellFieldName typeName (fieldName f))
                      <+> "msg")
      cp  = pretty cur
  in case fieldType f of
    PrimitiveType "bool" ->
      "W.pokeWord8" <+> cp <+> parens
        ("if" <+> acc <+> "then 1 else 0")
    PrimitiveType "int8" ->
      "W.pokeWord8" <+> cp <+> parens ("fromIntegral" <+> acc)
    PrimitiveType "int16"   -> "W.pokeInt16BE"   <+> cp <+> acc
    PrimitiveType "int32"   -> "W.pokeInt32BE"   <+> cp <+> acc
    PrimitiveType "int64"   -> "W.pokeInt64BE"   <+> cp <+> acc
    PrimitiveType "uint16"  -> "W.pokeWord16BE"  <+> cp <+> acc
    PrimitiveType "uint32"  -> "W.pokeWord32BE"  <+> cp <+> acc
    PrimitiveType "uuid"    -> "WP.pokeKafkaUuid" <+> cp <+> acc
    PrimitiveType "float64" -> "W.pokeFloat64BE" <+> cp <+> acc
    -- Flexible-aware string/bytes encoding.  When the message is
    -- flexible at all (and the field doesn't opt out with
    -- @flexibleVersions: "none"@), the codec needs a runtime
    -- check on the version: at @version >= flexibleVer@ the
    -- field is encoded compact (varint length+1), at lower
    -- versions it's the legacy 4-byte / 2-byte length prefix.
    -- The codegen used to emit a static compile-time choice per
    -- field, which sent the compact shape on /every/ version
    -- and made the broker close the connection with
    -- @InvalidRequestException@ + buffer-underflow on
    -- non-flexible versions.
    PrimitiveType "string" ->
      if fieldUsesCompact f flexibleVer
        then parens ("if version >=" <+> threshold flexibleVer
                       <+> "then WP.pokeCompactString" <+> cp
                         <+> parens ("P.toCompactString" <+> acc)
                       <+> "else WP.pokeKafkaString" <+> cp <+> acc)
        else "WP.pokeKafkaString"   <+> cp <+> acc
    PrimitiveType "bytes" ->
      if fieldUsesCompact f flexibleVer
        then parens ("if version >=" <+> threshold flexibleVer
                       <+> "then WP.pokeCompactBytes" <+> cp
                         <+> parens ("P.toCompactBytes" <+> acc)
                       <+> "else WP.pokeKafkaBytes" <+> cp <+> acc)
        else "WP.pokeKafkaBytes"   <+> cp <+> acc
    PrimitiveType _ -> "error \"WireGenerator: unsupported primitive\""

    StructType structName | isFieldNullable f ->
      parens
        ("case" <+> acc
           <+> "of { P.Null -> W.pokeWord8" <+> cp
             <+> "0; P.NotNull s -> W.pokeWord8" <+> cp
             <+> "1 >>= \\p' -> wirePoke" <> pretty structName
             <+> "version p' s }")
    StructType structName ->
      "wirePoke" <> pretty structName <+> "version" <+> cp <+> acc

    ArrayType inner ->
      let elementPokeFn = elementPoke flexibleVer inner
          nullable      = isFieldNullable f
      in case (nullable, isJust flexibleVer) of
        (True, True)   -> "WP.pokeVersionedNullableArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPokeFn <+> cp <+> acc
        (False, True)  -> "WP.pokeVersionedArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPokeFn <+> cp <+> acc
        (True, False)  -> "WP.pokeNullableKafkaArray"
                            <+> elementPokeFn <+> cp <+> acc
        (False, False) -> "WP.pokeKafkaArray"
                            <+> elementPokeFn <+> cp <+> acc

threshold :: Maybe Int16 -> Doc ann
threshold (Just v) = pretty v
threshold Nothing  = "999"

-- | Per-element poke for an array. 'Wire'-shaped: takes a cursor +
-- the element value, returns the advanced cursor.
elementPoke :: Maybe Int16 -> TypeSpec -> Doc ann
elementPoke flexibleVer = \case
  PrimitiveType "bool"    ->
    "(\\p x -> W.pokeWord8 p (if x then 1 else 0))"
  PrimitiveType "int8"    ->
    "(\\p x -> W.pokeWord8 p (fromIntegral (x :: Int8)))"
  PrimitiveType "int16"   -> "W.pokeInt16BE"
  PrimitiveType "int32"   -> "W.pokeInt32BE"
  PrimitiveType "int64"   -> "W.pokeInt64BE"
  PrimitiveType "uint16"  -> "W.pokeWord16BE"
  PrimitiveType "uint32"  -> "W.pokeWord32BE"
  PrimitiveType "uuid"    -> "WP.pokeKafkaUuid"
  PrimitiveType "float64" -> "W.pokeFloat64BE"
  PrimitiveType "string" ->
    case flexibleVer of
      Just _  -> "(\\p s -> if version >="
                   <+> threshold flexibleVer
                   <+> "then WP.pokeCompactString p (P.toCompactString s)"
                   <+> "else WP.pokeKafkaString p s)"
      Nothing -> "WP.pokeKafkaString"
  PrimitiveType "bytes" ->
    case flexibleVer of
      Just _  -> "(\\p b -> if version >="
                   <+> threshold flexibleVer
                   <+> "then WP.pokeCompactBytes p (P.toCompactBytes b)"
                   <+> "else WP.pokeKafkaBytes p b)"
      Nothing -> "WP.pokeKafkaBytes"
  PrimitiveType _ ->
    "(\\p _ -> pure p)"  -- unreachable
  StructType structName ->
    "(\\p x -> wirePoke" <> pretty structName <+> "version p x)"
  ArrayType _ ->
    "(\\p _ -> pure p)"  -- arrays-of-arrays unreachable

----------------------------------------------------------------------
-- wirePeek
----------------------------------------------------------------------

generateWirePeek
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> [Int16]
  -> Doc ann
generateWirePeek typeName _fields _flexibleVer [] =
  emptyPeekStub typeName
generateWirePeek typeName fields flexibleVer versions =
  let funName = "wirePeek" <> pretty typeName
      groups  = groupVersionsByFieldSet versions fields flexibleVer
      branches = map (generatePeekBranch typeName fields flexibleVer) groups
  in vsep
    [ "-- | Direct-poke decoder for" <+> pretty typeName <> "."
    , funName
        <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
        <+> "-> IO (" <> pretty typeName <> ", Ptr Word8)"
    , funName <+> "version _fp _basePtr p0 endPtr"
    , vsep branches
    , "  | otherwise = error $ \"wirePeek" <+> pretty typeName
        <+> ": unsupported version: \" ++ show version"
    , ""
    ]

generateNestedWirePeek
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> Doc ann
generateNestedWirePeek structName fields flexibleVer =
  let funName       = "wirePeek" <> pretty structName
      regular       = filter (not . isPotentiallyTaggedField) fields
      taggedFields  = filter isPotentiallyTaggedField fields
      (lastVar, decodeStmts, fieldVarMap) =
        peekFieldStmts structName flexibleVer regular
      -- Tagged-fields trailer. When the nested struct has known
      -- tagged fields and the parent is at a flexible version, read
      -- the tagged-fields envelope into a 'Map Word32 ByteString'
      -- and bind each known tag via a per-field decode + default.
      (trailerStmt, taggedVarMap, finalCur) = case flexibleVer of
        Nothing ->
          ([], [], lastVar)
        Just v -> case taggedFields of
          [] ->
            ( [ "pTagsEnd <- if version >=" <+> pretty v
                  <+> "then WP.peekAndSkipTaggedFields"
                    <+> pretty lastVar <+> "endPtr"
                  <+> "else pure" <+> pretty lastVar
              ]
            , []
            , "pTagsEnd"
            )
          _ ->
            ( [ "(_taggedMap, pTagsEnd) <- if version >=" <+> pretty v
                  <+> "then WP.peekTaggedFieldsMap"
                    <+> pretty lastVar <+> "endPtr"
                  <+> "else pure (Data.Map.Strict.empty, "
                    <> pretty lastVar <> ")"
              ]
              ++ map (taggedFieldBinding structName) taggedFields
            , [ (fieldName f, taggedVarFor f) | f <- taggedFields ]
            , "pTagsEnd"
            )
      recordBuild =
        buildRecordWithTagged structName fields fieldVarMap taggedVarMap
  in vsep
    [ "-- | Direct-poke decoder for" <+> pretty structName <> "."
    , funName
        <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
        <+> "-> IO (" <> pretty structName <> ", Ptr Word8)"
    , funName <+> "version _fp _basePtr p0 endPtr = do"
    , indent 2 $ vsep $ decodeStmts ++ trailerStmt ++
        [ "pure" <+> parens (recordBuild <> "," <+> pretty finalCur) ]
    , ""
    ]

generatePeekBranch
  :: Text
  -> [FieldSpec]
  -> Maybe Int16
  -> (Int16, Int16)
  -> Doc ann
generatePeekBranch typeName allFields flexibleVer (minV, maxV) =
  let isFlexible = maybe False (<= minV) flexibleVer
      regularFields =
        filter (\f -> fieldInVersionRange minV maxV f
                       && not (isPotentiallyTaggedField f)) allFields
      taggedFields =
        filter isPotentiallyTaggedField allFields
      guard
        | minV == maxV =
            "  | version ==" <+> pretty minV <+> "= do"
        | otherwise    =
            "  | version >=" <+> pretty minV
              <+> "&& version <=" <+> pretty maxV <+> "= do"
      (lastVar, decodeStmts, fieldVarMap) =
        peekFieldStmts typeName flexibleVer regularFields
      -- Tagged-fields trailer. If the message has known tagged
      -- fields with payloads, read them into a 'Map Word32
      -- ByteString' and bind each known field via a per-tag
      -- decode + default.
      (trailerStmt, taggedVarMap, finalCur)
        | not isFlexible =
            ([], [], lastVar)
        | null taggedFields =
            ( ["pTagsEnd <- WP.peekAndSkipTaggedFields"
                <+> pretty lastVar <+> "endPtr"]
            , []
            , "pTagsEnd"
            )
        | otherwise =
            let mapStmt =
                  "(_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap"
                    <+> pretty lastVar <+> "endPtr"
                taggedBindings = map (taggedFieldBinding typeName) taggedFields
            in ( mapStmt : taggedBindings
               , [ (fieldName f, taggedVarFor f) | f <- taggedFields ]
               , "pTagsEnd"
               )
      recordBuild =
        buildRecordWithTagged typeName allFields fieldVarMap taggedVarMap
  in vsep
    [ guard
    , indent 4 $ vsep $ decodeStmts ++ trailerStmt ++
        [ "pure" <+> parens (recordBuild <> "," <+> pretty finalCur) ]
    ]

-- | Variable name we bind a tagged field's decoded value to.
taggedVarFor :: FieldSpec -> Text
taggedVarFor f = "_tag_" <> sanitiseVar (fieldName f)

-- | Emit @let _tag_x = case Map.lookup tag _taggedMap of { Just bs ->
-- ... ; Nothing -> default }@ for a single tagged field.
taggedFieldBinding :: Text -> FieldSpec -> Doc ann
taggedFieldBinding typeName f =
  let var       = pretty (taggedVarFor f)
      tagNum    = case fieldTag f of
        Just t  -> pretty t
        Nothing -> "0"
      gate = taggedVersionGate "version" f
      decoder = taggedFieldDecodeExpr typeName f
      defaultVal = generateFieldDefaultDoc f
  in "let !" <> var <+> "= if" <+> gate
       <+> "then case Data.Map.Strict.lookup"
       <+> tagNum <+> "_taggedMap of {"
       <+> "Just _bs -> case" <+> decoder <+> "_bs of {"
       <+> "Right _v -> _v ; Left _ -> " <> defaultVal
       <> "}; Nothing -> " <> defaultVal
       <> "} else " <> defaultVal

-- | Wire-shape decoder for a single tagged field's payload.
taggedFieldDecodeExpr :: Text -> FieldSpec -> Doc ann
taggedFieldDecodeExpr _typeName f = case fieldType f of
  PrimitiveType "bool"    -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Bool)"
  PrimitiveType "int8"    -> "(\\b -> fmap (fromIntegral :: Data.Word.Word8 -> Data.Int.Int8) (W.runWireGet b))"
  PrimitiveType "int16"   -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Int.Int16)"
  PrimitiveType "int32"   -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Int.Int32)"
  PrimitiveType "int64"   -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Int.Int64)"
  PrimitiveType "uint16"  -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Word.Word16)"
  PrimitiveType "uint32"  -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Word.Word32)"
  PrimitiveType "float64" -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String Double)"
  PrimitiveType "uuid"    -> "(W.runWireGet :: Data.ByteString.ByteString -> Either String P.KafkaUuid)"
  PrimitiveType "string"  ->
    "(\\b -> fmap P.fromCompactString ((W.runWireGet :: Data.ByteString.ByteString -> Either String P.CompactString) b))"
  PrimitiveType "bytes"   ->
    "(\\b -> fmap P.fromCompactBytes  ((W.runWireGet :: Data.ByteString.ByteString -> Either String P.CompactBytes ) b))"
  PrimitiveType _         -> "(\\_ -> Left \"WireGenerator: tagged primitive\")"
  StructType s ->
    "(W.runWireGetWith (\\_fp _bp p e -> wirePeek" <> pretty s
      <+> "version _fp _bp p e))"
  ArrayType inner ->
    "(W.runWireGetWith (\\_fp _bp p e -> WP.peekCompactArray "
      <> elementPeekForTagged inner <+> "p e))"
  where
    elementPeekForTagged :: TypeSpec -> Doc ann
    elementPeekForTagged = \case
      PrimitiveType "bool"    -> "(\\p e -> (\\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p e)"
      PrimitiveType "int8"    -> "(\\p e -> (\\(w, p') -> (fromIntegral w :: Data.Int.Int8, p')) <$> W.peekWord8 p e)"
      PrimitiveType "int16"   -> "W.peekInt16BE"
      PrimitiveType "int32"   -> "W.peekInt32BE"
      PrimitiveType "int64"   -> "W.peekInt64BE"
      PrimitiveType "uint16"  -> "W.peekWord16BE"
      PrimitiveType "uint32"  -> "W.peekWord32BE"
      PrimitiveType "uuid"    -> "WP.peekKafkaUuid"
      PrimitiveType "float64" -> "W.peekFloat64BE"
      PrimitiveType "string"  -> "(\\p e -> (\\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e)"
      PrimitiveType "bytes"   -> "(\\p e -> (\\(cb, p') -> (P.fromCompactBytes  cb, p')) <$> WP.peekCompactBytes  p e)"
      StructType s            -> "(\\p e -> wirePeek" <> pretty s <+> "version _fp _bp p e)"
      _                       -> "(\\p _ -> error \"WireGenerator: unsupported tagged-array element\")"

-- | Same as 'buildRecord' but also splices in tagged-field var
-- bindings.
buildRecordWithTagged
  :: Text
  -> [FieldSpec]
  -> [(Text, Text)]    -- regular field var map
  -> [(Text, Text)]    -- tagged field var map
  -> Doc ann
buildRecordWithTagged typeName allFields regularMap taggedMap =
  let fieldDoc f =
        let recName = toHaskellFieldName typeName (fieldName f)
            value   = case lookup (fieldName f) regularMap of
              Just var -> pretty var
              Nothing  -> case lookup (fieldName f) taggedMap of
                Just var -> pretty var
                Nothing  -> generateFieldDefaultDoc f
        in pretty recName <+> "=" <+> value
      assignments = punctuate "," (map fieldDoc allFields)
  in pretty typeName <+> "{" <+> hsep assignments <+> "}"

peekFieldStmts
  :: Text
  -> Maybe Int16
  -> [FieldSpec]
  -> (Text, [Doc ann], [(Text, Text)])
peekFieldStmts _typeName flexibleVer fs = go (0 :: Int) fs [] []
  where
    go i [] acc varMap = (cur i, reverse acc, reverse varMap)
    go i (f : rest) acc varMap =
      let !next = i + 1
          var   = "f" <> T.pack (show i) <> "_" <> sanitiseVar (fieldName f)
          body  = peekFieldExpr flexibleVer f (cur i)
          -- Mirror image of 'pokeFieldStmts': nested-struct
          -- decoders emit a single un-branched function, so they
          -- /must/ guard each version-bracketed field at runtime
          -- and substitute the field's documented default when
          -- the wire doesn't carry it. Without this, decoding
          -- e.g. a v3 'TopicProduceResponse' would over-read
          -- by 16 bytes (the v13+ TopicId field).
          guarded = case fieldVersionGuard f of
            Nothing   -> body
            Just cond -> parens
              ("if" <+> cond <+> "then" <+> body
                 <+> "else pure" <+> parens
                       (generateFieldDefaultDoc f <> "," <+> pretty (cur i)))
          stmt  = parens (pretty var <> "," <+> pretty (cur next))
                    <+> "<-" <+> guarded
      in go next rest (stmt : acc) ((fieldName f, var) : varMap)

    cur n = "p" <> T.pack (show n)

sanitiseVar :: Text -> Text
sanitiseVar = T.toLower . T.filter (\c -> c == '_'
                                       || (c >= 'a' && c <= 'z')
                                       || (c >= 'A' && c <= 'Z')
                                       || (c >= '0' && c <= '9'))

peekFieldExpr
  :: Maybe Int16
  -> FieldSpec
  -> Text
  -> Doc ann
peekFieldExpr flexibleVer f cur =
  let cp  = pretty cur
      end = "endPtr"     :: Doc ann
      fp  = "_fp"        :: Doc ann
      bp  = "_basePtr"   :: Doc ann
  in case fieldType f of
    PrimitiveType "bool" ->
      "(\\(w, p') -> (w /= 0, p')) <$> W.peekWord8" <+> cp <+> end
    PrimitiveType "int8" ->
      "(\\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8"
        <+> cp <+> end
    PrimitiveType "int16"   -> "W.peekInt16BE"  <+> cp <+> end
    PrimitiveType "int32"   -> "W.peekInt32BE"  <+> cp <+> end
    PrimitiveType "int64"   -> "W.peekInt64BE"  <+> cp <+> end
    PrimitiveType "uint16"  -> "W.peekWord16BE" <+> cp <+> end
    PrimitiveType "uint32"  -> "W.peekWord32BE" <+> cp <+> end
    PrimitiveType "uuid"    -> "WP.peekKafkaUuid" <+> cp <+> end
    PrimitiveType "float64" -> "W.peekFloat64BE" <+> cp <+> end
    -- Mirror image of the encoder's flexible-aware switch — see
    -- the long comment in 'pokeFieldExpr' for why we emit a
    -- runtime check rather than a compile-time choice.
    PrimitiveType "string" ->
      if fieldUsesCompact f flexibleVer
        then parens ("if version >=" <+> threshold flexibleVer
                       <+> "then (\\(cs, p') -> (P.fromCompactString cs, p'))"
                         <+> "<$> WP.peekCompactString" <+> cp <+> end
                       <+> "else WP.peekKafkaString" <+> cp <+> end)
        else "WP.peekKafkaString" <+> cp <+> end
    PrimitiveType "bytes" ->
      if fieldUsesCompact f flexibleVer
        then parens ("if version >=" <+> threshold flexibleVer
                       <+> "then (\\(cb, p') -> (P.fromCompactBytes cb, p'))"
                         <+> "<$> WP.peekCompactBytes" <+> cp <+> end
                       <+> "else WP.peekKafkaBytes" <+> cp <+> end)
        else "WP.peekKafkaBytes" <+> cp <+> end
    PrimitiveType _ -> "error \"WireGenerator: unsupported primitive\""

    StructType structName | isFieldNullable f ->
      parens
        ("do { (flag, pAfterFlag) <- W.peekWord8" <+> cp <+> end
           <> "; case flag of {"
           <+> "0 -> pure (P.Null, pAfterFlag); _ -> do {"
           <+> "(s, p'') <- wirePeek" <> pretty structName
             <+> "version" <+> fp <+> bp
             <+> "pAfterFlag" <+> end
           <> "; pure (P.NotNull s, p'') } } }")
    StructType structName ->
      "wirePeek" <> pretty structName
        <+> "version" <+> fp <+> bp <+> cp <+> end

    ArrayType inner ->
      let elementPeekFn = elementPeek flexibleVer inner
          nullable      = isFieldNullable f
      in case (nullable, isJust flexibleVer) of
        (True, True)   -> "WP.peekVersionedNullableArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPeekFn <+> cp <+> end
        (False, True)  -> "WP.peekVersionedArray"
                            <+> "version" <+> threshold flexibleVer
                            <+> elementPeekFn <+> cp <+> end
        (True, False)  -> "WP.peekNullableKafkaArray"
                            <+> elementPeekFn <+> cp <+> end
        (False, False) -> "WP.peekKafkaArray"
                            <+> elementPeekFn <+> cp <+> end

elementPeek :: Maybe Int16 -> TypeSpec -> Doc ann
elementPeek flexibleVer = \case
  PrimitiveType "bool" ->
    "(\\p e -> (\\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p e)"
  PrimitiveType "int8" ->
    "(\\p e -> (\\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p e)"
  PrimitiveType "int16"   -> "W.peekInt16BE"
  PrimitiveType "int32"   -> "W.peekInt32BE"
  PrimitiveType "int64"   -> "W.peekInt64BE"
  PrimitiveType "uint16"  -> "W.peekWord16BE"
  PrimitiveType "uint32"  -> "W.peekWord32BE"
  PrimitiveType "uuid"    -> "WP.peekKafkaUuid"
  PrimitiveType "float64" -> "W.peekFloat64BE"
  PrimitiveType "string" -> case flexibleVer of
    Just _  -> "(\\p e -> if version >=" <+> threshold flexibleVer
                 <+> "then (\\(cs, p') -> (P.fromCompactString cs, p'))"
                 <+> "<$> WP.peekCompactString p e else WP.peekKafkaString p e)"
    Nothing -> "WP.peekKafkaString"
  PrimitiveType "bytes" -> case flexibleVer of
    Just _  -> "(\\p e -> if version >=" <+> threshold flexibleVer
                 <+> "then (\\(cb, p') -> (P.fromCompactBytes cb, p'))"
                 <+> "<$> WP.peekCompactBytes p e else WP.peekKafkaBytes p e)"
    Nothing -> "WP.peekKafkaBytes"
  PrimitiveType _ ->
    "(\\p _ -> error \"WireGenerator: unsupported element\")"
  StructType structName ->
    "(\\p e -> wirePeek" <> pretty structName
      <+> "version _fp _basePtr p e)"
  ArrayType _ ->
    "(\\p _ -> error \"WireGenerator: arrays-of-arrays unsupported\")"

emptyPokeStub :: Text -> Doc ann
emptyPokeStub typeName = vsep
  [ "wirePoke" <> pretty typeName
      <+> ":: Int -> Ptr Word8 -> " <> pretty typeName
      <+> "-> IO (Ptr Word8)"
  , "wirePoke" <> pretty typeName <+> "_version _basePtr _msg ="
  , indent 2 ("error \"wirePoke" <+> pretty typeName
              <> ": no valid versions\"")
  , ""
  ]

emptyPeekStub :: Text -> Doc ann
emptyPeekStub typeName = vsep
  [ "wirePeek" <> pretty typeName
      <+> ":: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8"
      <+> "-> IO (" <> pretty typeName <> ", Ptr Word8)"
  , "wirePeek" <> pretty typeName
      <+> "_version _fp _basePtr _p _endPtr ="
  , indent 2 ("error \"wirePeek" <+> pretty typeName
              <> ": no valid versions\"")
  , ""
  ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

parseFlexibleVer :: Text -> Maybe Int16
parseFlexibleVer t = case parseVersionSpec t of
  Right (VersionFrom v)    -> Just v
  Right (VersionRange v _) -> Just v
  Right (ExactVersion v)   -> Just v
  _                        -> Nothing

-- | Whether a string / bytes / array field uses the compact codec at
-- the supplied flexible-version threshold.
fieldUsesCompact :: FieldSpec -> Maybe Int16 -> Bool
fieldUsesCompact f flexibleVer
  | fieldFlexibleVersions f == Just "none" = False
  | otherwise = case flexibleVer of
      Nothing -> False
      Just _  -> True

-- | Whether a field is nullable (carries a Null wire encoding /
-- 'P.Nullable' wrapper).
isFieldNullable :: FieldSpec -> Bool
isFieldNullable f = isJust (fieldNullableVersions f)

-- | Convert a protocol name to a Haskell type name (PascalCase).
toHaskellTypeName :: Text -> Text
toHaskellTypeName = T.pack . ensureUpper . T.unpack
  where
    ensureUpper []     = []
    ensureUpper (x:xs) = toUpper x : xs

-- | Match the legacy generator's name munging exactly: the type
-- name's first char goes lowercase, the field name is appended
-- verbatim (the JSON spec already uses PascalCase for field names
-- where appropriate, and lowercase for camelCase fields like
-- @timeoutMs@). Diverging from the legacy here would silently
-- break the @messageX accessor msg@ patterns the codegen-emitted
-- pokes / peeks rely on.
toHaskellFieldName :: Text -> Text -> Text
toHaskellFieldName typeName fname =
  let typePrefix
        | T.null typeName = ""
        | otherwise       = T.pack (toCamelCase (T.unpack typeName))
  in typePrefix <> fname
  where
    toCamelCase []     = []
    toCamelCase (x:xs) = toLower x : xs

isPotentiallyTaggedField :: FieldSpec -> Bool
isPotentiallyTaggedField f = isJust (fieldTag f)

fieldInVersionRange :: Int16 -> Int16 -> FieldSpec -> Bool
fieldInVersionRange minV maxV field =
  case parseVersionSpec (fieldVersions field) of
    Right spec -> all (\v -> inVersionRange v spec) [minV .. maxV]
    Left _     -> False

groupVersionsByFieldSet
  :: [Int16]
  -> [FieldSpec]
  -> Maybe Int16
  -> [(Int16, Int16)]
groupVersionsByFieldSet [] _ _ = []
groupVersionsByFieldSet versions fields flexibleVer =
  let versionsWithFields =
        [(v, (fieldsInVersion v, isVersionFlexible v)) | v <- versions]
      sorted  = sortBy (comparing snd <> comparing fst) versionsWithFields
      grouped = groupBy (\(_, f1) (_, f2) -> f1 == f2) sorted
      ranges  = concatMap (groupConsecutive . map fst) grouped
  in sortBy compareRanges ranges
  where
    fieldsInVersion :: Int16 -> [Text]
    fieldsInVersion v =
      [fieldName f | f <- fields, fieldInVersionRange v v f]

    isVersionFlexible :: Int16 -> Bool
    isVersionFlexible v = maybe False (<= v) flexibleVer

    groupConsecutive :: [Int16] -> [(Int16, Int16)]
    groupConsecutive []     = []
    groupConsecutive (v:vs) = go v v vs
      where
        go start end []     = [(start, end)]
        go start end (x:xs)
          | x == end + 1 = go start x xs
          | otherwise    = (start, end) : go x x xs

    compareRanges :: (Int16, Int16) -> (Int16, Int16) -> Ordering
    compareRanges (min1, max1) (min2, max2)
      | min1 == max1 && min2 /= max2 = LT
      | min1 /= max1 && min2 == max2 = GT
      | size1 /= size2               = compare size1 size2
      | otherwise                    = compare min1 min2
      where
        size1 = max1 - min1
        size2 = max2 - min2

generateFieldDefaultDoc :: FieldSpec -> Doc ann
generateFieldDefaultDoc field = case fieldType field of
  -- Honour the schema-supplied @"default": "..."@ attribute when
  -- present. This matters for tagged fields (whose decoder
  -- substitutes the default when the broker omits the entry) and
  -- for absent-version fields (whose decoder always uses the
  -- default). The most consequential case is
  -- @CurrentLeader.LeaderId :: int32@ (KIP-951) which has
  -- @"default": "-1"@ — without honouring it, we'd silently
  -- patch the metadata cache's leader to broker 0 every time
  -- the broker omitted the tagged field, breaking subsequent
  -- produces.
  PrimitiveType "bool"    -> boolDefault   "False"
  PrimitiveType "int8"    -> intDefault    "0"
  PrimitiveType "int16"   -> intDefault    "0"
  PrimitiveType "int32"   -> intDefault    "0"
  PrimitiveType "int64"   -> intDefault    "0"
  PrimitiveType "uint16"  -> intDefault    "0"
  PrimitiveType "uint32"  -> intDefault    "0"
  PrimitiveType "float64" -> doubleDefault "0.0"
  PrimitiveType "string"  -> "P.KafkaString Null"
  PrimitiveType "bytes"   -> "P.KafkaBytes Null"
  PrimitiveType "uuid"    -> "P.nullUuid"
  StructType _ | isFieldNullable field -> "P.Null"
  StructType s | otherwise ->
    -- 'default<StructName>' is co-emitted by
    -- 'generateNestedWireDefault' for every struct the codegen
    -- visits. The strict record constructor can't accept
    -- 'undefined' here without crashing on response decoding,
    -- so we always reference the per-struct value instead.
    "default" <> pretty s
  ArrayType _ | isFieldNullable field -> "P.KafkaArray P.Null"
  ArrayType _ -> "P.mkKafkaArray V.empty"
  PrimitiveType _ -> "undefined"
  where
    -- Read the schema's "default" field and emit it as a
    -- Haskell literal, falling back to the type's natural
    -- zero-equivalent when the field is missing or the value
    -- isn't shaped like the type expects. The schema files use
    -- string-encoded numbers ("-1", "0", "1.0") so we parse +
    -- normalise here rather than emit the raw JSON string.

    -- Lowercase 'true' / 'false' (JSON convention in the
    -- schemas) -> capitalised Haskell.
    boolDefault :: Text -> Doc ann
    boolDefault zero =
      case fieldDefault field of
        Just (Aeson.Bool True)  -> "True"
        Just (Aeson.Bool False) -> "False"
        Just (Aeson.String "true")  -> "True"
        Just (Aeson.String "false") -> "False"
        _ -> pretty zero

    -- Integer literal: parse the schema string as an Int64; if
    -- it doesn't parse cleanly, emit the type's zero default.
    intDefault :: Text -> Doc ann
    intDefault zero =
      case fieldDefault field of
        Just (Aeson.Number n) -> pretty (show (truncate n :: Integer))
        Just (Aeson.String t) ->
          case readsPrec 0 (T.unpack t) :: [(Integer, String)] of
            [(i, "")] -> pretty (show i)
            _         -> pretty zero
        _ -> pretty zero

    -- Float literal: must include a decimal point so Haskell
    -- types it as Double, not Integer.
    doubleDefault :: Text -> Doc ann
    doubleDefault zero =
      case fieldDefault field of
        Just (Aeson.Number n)
          | let d = realToFrac n :: Double
          -> pretty (show d)
        Just (Aeson.String t) ->
          case readsPrec 0 (T.unpack t) :: [(Double, String)] of
            [(d, "")] -> pretty (show d)
            _         -> pretty zero
        _ -> pretty zero

-- | Emit a default value for a struct: a record literal whose
-- fields are themselves recursively defaulted.  Lives at module
-- scope as @defaultStructName :: StructName@ so version-gated
-- absent fields elsewhere can reference it without re-deriving
-- the constructor every time.
generateNestedWireDefault
  :: Text          -- ^ struct type name
  -> [FieldSpec]   -- ^ struct fields
  -> Doc ann
generateNestedWireDefault structName fields =
  let valName = "default" <> pretty structName
      fieldDoc f =
        pretty (toHaskellFieldName structName (fieldName f))
          <+> "=" <+> generateFieldDefaultDoc f
      assignments = punctuate "," (map fieldDoc fields)
  in vsep
    [ "-- | Per-struct default value referenced by 'generateFieldDefaultDoc'"
    , "-- when an absent-version field elsewhere needs a placeholder."
    , valName <+> ":: " <> pretty structName
    , valName <+> "=" <+> pretty structName
        <+> "{" <+> hsep assignments <+> "}"
    , ""
    ]

