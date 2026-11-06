{-# LANGUAGE BangPatterns #-}

module FlatBuffers.Internal.Schema where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatBuffers.Schema
import FlatBuffers.Value qualified as F


data CompiledSchema = CompiledSchema
  { csRoot :: !TableLayout
  , csTables :: !(Map Text TableLayout)
  , csStructs :: !(Map Text StructLayout)
  , csEnums :: !(Map Text FBEnumDef)
  , csUnions :: !(Map Text FBUnionDef)
  , csIdentifier :: !(Maybe ByteString)
  }


data TableLayout = TableLayout
  { tlFields :: !(Vector FieldLayout)
  , tlSlots :: {-# UNPACK #-} !Int
  }


data FieldLayout = FieldLayout
  { flType :: !FBType
  , flSlot :: {-# UNPACK #-} !Int
  , flDefault :: !(Maybe F.Value)
  , flRequired :: !Bool
  , flDeprecated :: !Bool
  }


data StructLayout = StructLayout
  { slFields :: !(Vector StructField)
  , slSize :: {-# UNPACK #-} !Int
  , slAlign :: {-# UNPACK #-} !Int
  }


data StructField = StructField
  { sfType :: !FBType
  , sfOffset :: {-# UNPACK #-} !Int
  }


data StructBuild = StructBuild
  { sbFields :: ![StructField]
  , sbEnd :: {-# UNPACK #-} !Int
  , sbAlign :: {-# UNPACK #-} !Int
  }


compileSchema :: FlatBuffersSchema -> Either String CompiledSchema
compileSchema schema = do
  let declarations = V.toList (fbsDecls schema)
      tables = Map.fromList (concatMap (\case FBTable t -> [(tdName t, t)]; _ -> []) declarations)
      structs = Map.fromList (concatMap (\case FBStruct s -> [(fsdName s, s)]; _ -> []) declarations)
      enums = Map.fromList (concatMap (\case FBEnum e -> [(fedName e, e)]; _ -> []) declarations)
      unions = Map.fromList (concatMap (\case FBUnion u -> [(fudName u, u)]; _ -> []) declarations)
      names = Map.keys tables <> Map.keys structs <> Map.keys enums <> Map.keys unions
  unless (length names == Set.size (Set.fromList names)) (Left "FlatBuffers: duplicate declaration name")
  unless (V.null (fbsIncludes schema)) (Left "FlatBuffers: combine included schemas before compilation")
  V.mapM_ (validateDecl tables structs enums unions) (fbsDecls schema)
  structLayouts <- traverse (layoutStruct structs enums Set.empty) structs
  let seed = CompiledSchema (TableLayout V.empty 0) Map.empty structLayouts enums unions Nothing
  tableLayouts <- traverse (layoutTable seed) tables
  root <- maybe (Left "FlatBuffers: root_type must name a declared table") Right (fbsRootType schema >>= (`Map.lookup` tableLayouts))
  ident <- traverse identifier (fbsFileIdentifier schema)
  Right seed {csRoot = root, csTables = tableLayouts, csIdentifier = ident}
  where
    identifier text =
      let bytes = TE.encodeUtf8 text
      in if T.length text == 4 && T.all (\c -> c >= ' ' && c <= '~') text
           then Right bytes
           else Left "FlatBuffers: file_identifier must contain four ASCII characters"


validateDecl :: Map Text TableDef -> Map Text FBStructDef -> Map Text FBEnumDef -> Map Text FBUnionDef -> FBDeclaration -> Either String ()
validateDecl tables structs enums unions = \case
  FBTable table -> do
    unique "table field" (V.map tfName (tdFields table))
    V.mapM_ (validateType False . tfType) (tdFields table)
  FBStruct structure -> do
    when (V.null (fsdFields structure)) (Left "FlatBuffers: empty struct")
    unique "struct field" (V.map fst (fsdFields structure))
    V.mapM_ (validateType True . snd) (fsdFields structure)
  FBEnum enumeration -> do
    unless (integralType (fedUnderlyingType enumeration)) (Left "FlatBuffers: enum storage must be integral")
    unique "enum member" (V.map fst (fedValues enumeration))
    _ <-
      V.foldM
        ( \next (_, explicit) -> do
            let value = maybe next toInteger explicit
            _ <- integerScalar (fedUnderlyingType enumeration) value
            Right (value + 1)
        )
        0
        (fedValues enumeration)
    Right ()
  FBUnion union -> do
    unique "union member" (fudMembers union)
    when (V.length (fudMembers union) > 255) (Left "FlatBuffers: too many union members")
    V.mapM_ (\name -> unless (Map.member name tables) (Left "FlatBuffers: dynamic unions currently require table members")) (fudMembers union)
  where
    unique label names = unless (V.length names == Set.size (Set.fromList (V.toList names))) (Left ("FlatBuffers: duplicate " <> label))
    validateType inline = \case
      FTNamed name
        | Map.member name structs || Map.member name enums -> Right ()
        | not inline && (Map.member name tables || Map.member name unions) -> Right ()
        | otherwise -> Left ("FlatBuffers: unknown or non-inline type " <> T.unpack name)
      FTString | inline -> Left "FlatBuffers: strings cannot occur inline in structs"
      FTVector ty
        | inline -> Left "FlatBuffers: vectors cannot occur inline in structs"
        | otherwise -> case ty of
            FTVector _ -> Left "FlatBuffers: nested vectors require wrapper tables"
            FTNamed name | Map.member name unions -> Left "FlatBuffers: vectors of unions are unsupported"
            _ -> validateType False ty
      _ -> Right ()


integralType :: FBType -> Bool
integralType ty = ty `elem` [FTByte, FTUByte, FTShort, FTUShort, FTInt, FTUInt, FTLong, FTULong]


alignUp :: Int -> Int -> Int
alignUp offset alignment = ((offset + alignment - 1) `quot` alignment) * alignment


layoutStruct :: Map Text FBStructDef -> Map Text FBEnumDef -> Set.Set Text -> FBStructDef -> Either String StructLayout
layoutStruct structs enums seen structure = do
  when (Set.member (fsdName structure) seen || Set.size seen >= 64) (Left "FlatBuffers: recursive or excessively nested struct")
  built <- V.foldM step (StructBuild [] 0 1) (fsdFields structure)
  let size = alignUp (sbEnd built) (sbAlign built)
  when (size > 32768) (Left "FlatBuffers: struct exceeds dynamic codec size limit")
  Right (StructLayout (V.fromList (reverse (sbFields built))) size (sbAlign built))
  where
    step built (_, ty) = do
      layout <- case ty of
        FTNamed name
          | Just child <- Map.lookup name structs ->
              layoutStruct structs enums (Set.insert (fsdName structure) seen) child
        FTNamed name | Just enumeration <- Map.lookup name enums -> Right (scalarLayout (fedUnderlyingType enumeration))
        _ -> Right (scalarLayout ty)
      let offset = alignUp (sbEnd built) (slAlign layout)
      Right (StructBuild (StructField ty offset : sbFields built) (offset + slSize layout) (max (sbAlign built) (slAlign layout)))


scalarLayout :: FBType -> StructLayout
scalarLayout ty = StructLayout V.empty width width
  where
    width = case ty of
      FTBool -> 1
      FTByte -> 1
      FTUByte -> 1
      FTShort -> 2
      FTUShort -> 2
      FTLong -> 8
      FTULong -> 8
      FTDouble -> 8
      _ -> 4


typeLayout :: CompiledSchema -> FBType -> StructLayout
typeLayout schema = \case
  FTNamed name
    | Just layout <- Map.lookup name (csStructs schema) -> layout
    | Just enumeration <- Map.lookup name (csEnums schema) -> scalarLayout (fedUnderlyingType enumeration)
  ty -> scalarLayout ty


referenceType :: CompiledSchema -> FBType -> Bool
referenceType schema = \case
  FTString -> True
  FTVector _ -> True
  FTNamed name -> not (Map.member name (csStructs schema) || Map.member name (csEnums schema))
  _ -> False


layoutTable :: CompiledSchema -> TableDef -> Either String TableLayout
layoutTable schema table = do
  let fields = V.toList (tdFields table)
      hasId field = lookup "id" (V.toList (tfMetadata field))
      explicit = any (maybe False (const True) . hasId) fields
      width field = case tfType field of
        FTNamed name | Map.member name (csUnions schema) -> 2
        _ -> 1
      slotCount = sum (map width fields)
  when (slotCount > 4096) (Left "FlatBuffers: too many table fields")
  layouts <- build explicit 0 fields
  let physical field = if isUnion schema (flType field) then [flSlot field - 1, flSlot field] else [flSlot field]
  unless (sort (concatMap physical layouts) == enumFromTo 0 (slotCount - 1)) (Left "FlatBuffers: field IDs must be unique and contiguous")
  let worst = foldl (\end field -> let ty = flType field; layout = typeLayout schema ty in alignUp end (slAlign layout) + slSize layout + 8) 4 layouts
  when (worst >= 65536) (Left "FlatBuffers: table exceeds 16-bit layout limit")
  Right (TableLayout (V.fromList layouts) slotCount)
  where
    build _ _ [] = Right []
    build explicit next (field : rest) = do
      let union = isUnion schema (tfType field)
          metadata = V.toList (tfMetadata field)
          required = any ((== "required") . fst) metadata
      slot <-
        if explicit
          then case lookup "id" metadata of
            Just (Just text) -> do
              n <- integerText text
              if n < 0 || n > 4095 then Left "FlatBuffers: field ID out of range" else Right (fromInteger n)
            _ -> Left "FlatBuffers: every field needs an explicit ID"
          else Right (next + if union then 1 else 0)
      when (any (\(key, _) -> key `elem` ["offset64", "vector64"]) metadata) (Left "FlatBuffers: 64-bit offsets/vectors are unsupported")
      when (any (\(key, val) -> key == "required" && val /= Nothing) metadata) (Left "FlatBuffers: required metadata takes no value")
      when (required && tfDeprecated field) (Left "FlatBuffers: required fields cannot be deprecated")
      let nonScalar =
            referenceType schema (tfType field) || case tfType field of
              FTNamed name -> Map.member name (csStructs schema)
              _ -> False
      when (required && not nonScalar) (Left "FlatBuffers: required applies only to non-scalar fields")
      def <- fieldDefault schema field
      remaining <- build explicit (next + if union then 2 else 1) rest
      Right (FieldLayout (tfType field) slot def required (tfDeprecated field) : remaining)


isUnion :: CompiledSchema -> FBType -> Bool
isUnion schema (FTNamed name) = Map.member name (csUnions schema)
isUnion _ _ = False


fieldDefault :: CompiledSchema -> TableField -> Either String (Maybe F.Value)
fieldDefault schema field
  | referenceType schema ty || Map.member structName (csStructs schema) = Right Nothing
  | tfDefault field == Just "null" = Right Nothing
  | otherwise =
      Just <$> case ty of
        FTNamed name | Just enumeration <- Map.lookup name (csEnums schema) -> do
          let defaultText = maybe "0" id (tfDefault field)
          n <- case enumMember enumeration defaultText of
            Just value -> Right value
            Nothing -> integerText defaultText
          integerScalar (fedUnderlyingType enumeration) n
        _ -> scalarDefault ty (maybe "0" id (tfDefault field))
  where
    ty = tfType field
    structName = case ty of FTNamed name -> name; _ -> ""


enumMember :: FBEnumDef -> Text -> Maybe Integer
enumMember enumeration name = go 0 (V.toList (fedValues enumeration))
  where
    go _ [] = Nothing
    go next ((member, explicit) : rest) =
      let number = maybe next toInteger explicit
      in if member == name then Just number else go (number + 1) rest


integerText :: Text -> Either String Integer
integerText text = case TR.signed number text of
  Right (value, rest) | T.null rest -> Right value
  _ -> Left "FlatBuffers: invalid integer default"
  where
    number t = if "0x" `T.isPrefixOf` t || "0X" `T.isPrefixOf` t then TR.hexadecimal (T.drop 2 t) else TR.decimal t


scalarDefault :: FBType -> Text -> Either String F.Value
scalarDefault FTBool "true" = Right (F.VBool True)
scalarDefault FTBool "false" = Right (F.VBool False)
scalarDefault ty text
  | ty == FTFloat || ty == FTDouble = case TR.signed TR.double text of
      Right (value, rest) | T.null rest -> scalarValue ty (F.VDouble value)
      _ -> Left "FlatBuffers: invalid floating default"
  | otherwise = do
      value <- integerText text
      integerScalar ty value


integerScalar :: FBType -> Integer -> Either String F.Value
integerScalar ty n = case ty of
  FTBool | n == 0 || n == 1 -> Right (F.VBool (n /= 0))
  FTByte -> bounded (-128) 127 (F.VInt8 . fromInteger)
  FTUByte -> bounded 0 255 (F.VWord8 . fromInteger)
  FTShort -> bounded (-32768) 32767 (F.VInt16 . fromInteger)
  FTUShort -> bounded 0 65535 (F.VWord16 . fromInteger)
  FTInt -> bounded (-2147483648) 2147483647 (F.VInt32 . fromInteger)
  FTUInt -> bounded 0 4294967295 (F.VWord32 . fromInteger)
  FTLong -> bounded (-9223372036854775808) 9223372036854775807 (F.VInt64 . fromInteger)
  FTULong -> bounded 0 18446744073709551615 (F.VWord64 . fromInteger)
  _ -> Left "FlatBuffers: expected scalar value"
  where
    bounded lo hi constructor = if n >= lo && n <= hi then Right (constructor n) else Left "FlatBuffers: scalar value out of range"


scalarValue :: FBType -> F.Value -> Either String F.Value
-- Already-typed fields need no Integer conversion or replacement Value node.
scalarValue FTByte value@(F.VInt8 _) = Right value
scalarValue FTUByte value@(F.VWord8 _) = Right value
scalarValue FTShort value@(F.VInt16 _) = Right value
scalarValue FTUShort value@(F.VWord16 _) = Right value
scalarValue FTInt value@(F.VInt32 _) = Right value
scalarValue FTUInt value@(F.VWord32 _) = Right value
scalarValue FTLong value@(F.VInt64 _) = Right value
scalarValue FTULong value@(F.VWord64 _) = Right value
scalarValue FTBool value@(F.VBool _) = Right value
scalarValue FTFloat value@(F.VFloat _) = Right value
scalarValue FTFloat (F.VDouble value)
  | isInfinite (realToFrac value :: Float) && not (isInfinite value) = Left "FlatBuffers: float value out of range"
  | otherwise = Right (F.VFloat (realToFrac value))
scalarValue FTDouble (F.VFloat value) = Right (F.VDouble (realToFrac value))
scalarValue FTDouble value@(F.VDouble _) = Right value
scalarValue ty value = case value of
  F.VInt8 n -> integerScalar ty (toInteger n)
  F.VInt16 n -> integerScalar ty (toInteger n)
  F.VInt32 n -> integerScalar ty (toInteger n)
  F.VInt64 n -> integerScalar ty (toInteger n)
  F.VWord8 n -> integerScalar ty (toInteger n)
  F.VWord16 n -> integerScalar ty (toInteger n)
  F.VWord32 n -> integerScalar ty (toInteger n)
  F.VWord64 n -> integerScalar ty (toInteger n)
  _ -> Left "FlatBuffers: value does not match schema type"
