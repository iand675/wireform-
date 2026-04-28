-- | Avro schema resolution (reader/writer compatibility).
--
-- When a reader uses a different schema than the writer, Avro defines
-- rules for resolving differences. This module implements those rules
-- per the Avro specification.
module Avro.Resolution
  ( ResolvedSchema(..)
  , FieldResolution(..)
  , resolveSchema
  , resolveValue
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import qualified Avro.Value as AV

-- | Describes how to convert a writer field to a reader field.
data FieldResolution
  = FieldFromWriter !Int !ResolvedSchema
  | FieldDefault !AV.Value
  deriving stock (Show, Eq)

-- | A plan for transforming writer-schema values into reader-schema values.
data ResolvedSchema
  = ResolvedSame
  | ResolvedPromoteIntToLong
  | ResolvedPromoteIntToFloat
  | ResolvedPromoteIntToDouble
  | ResolvedPromoteLongToFloat
  | ResolvedPromoteLongToDouble
  | ResolvedPromoteFloatToDouble
  -- | @string@ → @bytes@: re-encode the UTF-8 string as raw bytes.
  | ResolvedPromoteStringToBytes
  -- | @bytes@ → @string@: decode the raw bytes as UTF-8.
  | ResolvedPromoteBytesToString
  | ResolvedRecord !(V.Vector FieldResolution)
  | ResolvedEnum !Text !(V.Vector Int)
  | ResolvedArray !ResolvedSchema
  | ResolvedMap !ResolvedSchema
  | ResolvedUnion !(V.Vector ResolvedSchema)
  deriving stock (Show, Eq)

-- | Check compatibility between a writer and reader schema and produce a
-- resolution plan. Returns 'Left' with an error message if incompatible.
resolveSchema :: AvroType -> AvroType -> Either String ResolvedSchema
resolveSchema writerTy readerTy
  | writerTy == readerTy = Right ResolvedSame
  | otherwise = resolve writerTy readerTy

resolve :: AvroType -> AvroType -> Either String ResolvedSchema
resolve (AvroPrimitive w) (AvroPrimitive r) = resolvePrim w r
resolve (AvroUnion{avroUnionBranches = wBranches}) readerTy =
  resolveWriterUnion wBranches readerTy
resolve writerTy (AvroUnion{avroUnionBranches = rBranches}) =
  resolveReaderUnion writerTy rBranches
resolve w@AvroRecord{} r@AvroRecord{} = resolveRecords w r
resolve w@AvroEnum{} r@AvroEnum{} = resolveEnums w r
resolve (AvroArray wItems) (AvroArray rItems) =
  ResolvedArray <$> resolveSchema wItems rItems
resolve (AvroMap wVals) (AvroMap rVals) =
  ResolvedMap <$> resolveSchema wVals rVals
resolve w@AvroFixed{} r@AvroFixed{} = resolveFixed w r
resolve (AvroLogical{avroLogicalBase = wBase}) r = resolveSchema wBase r
resolve w (AvroLogical{avroLogicalBase = rBase}) = resolveSchema w rBase
resolve w r = Left $ "incompatible types: writer " ++ showType w ++ " vs reader " ++ showType r

-- ============================================================
-- Primitive resolution
-- ============================================================

resolvePrim :: AvroSchema -> AvroSchema -> Either String ResolvedSchema
resolvePrim AvroInt  AvroLong   = Right ResolvedPromoteIntToLong
resolvePrim AvroInt  AvroFloat  = Right ResolvedPromoteIntToFloat
resolvePrim AvroInt  AvroDouble = Right ResolvedPromoteIntToDouble
resolvePrim AvroLong AvroFloat  = Right ResolvedPromoteLongToFloat
resolvePrim AvroLong AvroDouble = Right ResolvedPromoteLongToDouble
resolvePrim AvroFloat AvroDouble = Right ResolvedPromoteFloatToDouble
-- Avro spec: "string is promotable to bytes" and vice-versa.
resolvePrim AvroString AvroBytes = Right ResolvedPromoteStringToBytes
resolvePrim AvroBytes  AvroString = Right ResolvedPromoteBytesToString
resolvePrim w r
  | w == r    = Right ResolvedSame
  | otherwise = Left $ "incompatible primitives: " ++ show w ++ " vs " ++ show r

-- ============================================================
-- Record resolution
-- ============================================================

resolveRecords :: AvroType -> AvroType -> Either String ResolvedSchema
resolveRecords w r = do
  let wFields = avroRecordFields w
      rFields = avroRecordFields r
  fieldResolutions <- V.mapM (resolveOneField wFields) rFields
  Right (ResolvedRecord fieldResolutions)

resolveOneField :: V.Vector AvroField -> AvroField -> Either String FieldResolution
resolveOneField wFields rField =
  case findFieldWithAliases wFields rField of
    Just wIdx -> do
      let wField = wFields V.! wIdx
      res <- resolveSchema (avroFieldType wField) (avroFieldType rField)
      Right (FieldFromWriter wIdx res)
    Nothing ->
      case avroFieldDefault rField of
        Just dflt -> Right (FieldDefault (defaultToValue (avroFieldType rField) dflt))
        Nothing   -> Left $ "reader field '" ++ T.unpack (avroFieldName rField)
                           ++ "' not in writer and has no default"

findFieldWithAliases :: V.Vector AvroField -> AvroField -> Maybe Int
findFieldWithAliases wFields rField =
  case V.findIndex (\f -> avroFieldName f == avroFieldName rField) wFields of
    Just idx -> Just idx
    Nothing ->
      case V.findIndex (\f -> avroFieldName f `V.elem` avroFieldAliases rField) wFields of
        Just idx -> Just idx
        Nothing ->
          let rName = avroFieldName rField
          in V.findIndex (\f -> rName `V.elem` avroFieldAliases f) wFields

defaultToValue :: AvroType -> AvroSchema -> AV.Value
defaultToValue _ AvroNull   = AV.Null
defaultToValue _ AvroBool   = AV.Bool False
defaultToValue _ AvroInt    = AV.Int 0
defaultToValue _ AvroLong   = AV.Long 0
defaultToValue _ AvroFloat  = AV.Float 0
defaultToValue _ AvroDouble = AV.Double 0
defaultToValue _ AvroBytes  = AV.Bytes ""
defaultToValue _ AvroString = AV.String ""
defaultToValue _ _          = AV.Null

-- ============================================================
-- Enum resolution
-- ============================================================

resolveEnums :: AvroType -> AvroType -> Either String ResolvedSchema
resolveEnums w r = do
  let wSyms = avroEnumSymbols w
      rSyms = avroEnumSymbols r
      rName = avroEnumName r
  mapping <- V.mapM (\sym ->
    case V.findIndex (== sym) rSyms of
      Just ri -> Right ri
      Nothing -> Left $ "writer enum symbol '" ++ T.unpack sym
                       ++ "' not in reader enum '" ++ T.unpack rName ++ "'"
    ) wSyms
  Right (ResolvedEnum rName mapping)

-- ============================================================
-- Fixed resolution
-- ============================================================

resolveFixed :: AvroType -> AvroType -> Either String ResolvedSchema
resolveFixed w r
  | avroFixedName w == avroFixedName r && avroFixedSize w == avroFixedSize r = Right ResolvedSame
  | avroFixedName w /= avroFixedName r =
      Left $ "fixed name mismatch: " ++ T.unpack (avroFixedName w)
           ++ " vs " ++ T.unpack (avroFixedName r)
  | otherwise =
      Left $ "fixed size mismatch for " ++ T.unpack (avroFixedName w)
           ++ ": " ++ show (avroFixedSize w) ++ " vs " ++ show (avroFixedSize r)

-- ============================================================
-- Union resolution
-- ============================================================

resolveWriterUnion :: V.Vector AvroType -> AvroType -> Either String ResolvedSchema
resolveWriterUnion wBranches readerTy = do
  resolutions <- V.mapM (\wb -> resolveSchema wb readerTy) wBranches
  Right (ResolvedUnion resolutions)

resolveReaderUnion :: AvroType -> V.Vector AvroType -> Either String ResolvedSchema
resolveReaderUnion writerTy rBranches =
  case findFirstMatch writerTy rBranches 0 of
    Just _  -> Right ResolvedSame
    Nothing -> Left $ "writer type " ++ showType writerTy
                    ++ " does not match any reader union branch"

findFirstMatch :: AvroType -> V.Vector AvroType -> Int -> Maybe Int
findFirstMatch wt branches idx
  | idx >= V.length branches = Nothing
  | otherwise = case resolveSchema wt (branches V.! idx) of
      Right _  -> Just idx
      Left _   -> findFirstMatch wt branches (idx + 1)

-- ============================================================
-- Value resolution
-- ============================================================

-- | Transform a writer-schema value into a reader-schema value using
-- a previously computed 'ResolvedSchema'.
resolveValue :: ResolvedSchema -> AV.Value -> Either String AV.Value
resolveValue ResolvedSame v = Right v
resolveValue ResolvedPromoteIntToLong (AV.Int n) = Right (AV.Long (fromIntegral n))
resolveValue ResolvedPromoteIntToFloat (AV.Int n) = Right (AV.Float (fromIntegral n))
resolveValue ResolvedPromoteIntToDouble (AV.Int n) = Right (AV.Double (fromIntegral n))
resolveValue ResolvedPromoteLongToFloat (AV.Long n) = Right (AV.Float (fromIntegral n))
resolveValue ResolvedPromoteLongToDouble (AV.Long n) = Right (AV.Double (fromIntegral n))
resolveValue ResolvedPromoteFloatToDouble (AV.Float f) = Right (AV.Double (realToFrac f))
-- Avro spec: 'string' and 'bytes' share their on-the-wire shape (a
-- length prefix + raw bytes), so promotion is just a tag swap.
-- string -> bytes: the bytes are exactly the UTF-8 encoding.
resolveValue ResolvedPromoteStringToBytes (AV.String t) =
  Right (AV.Bytes (Data.Text.Encoding.encodeUtf8 t))
-- bytes -> string: validate UTF-8 just like a fresh decode would.
resolveValue ResolvedPromoteBytesToString (AV.Bytes bs) =
  case Data.Text.Encoding.decodeUtf8' bs of
    Right t -> Right (AV.String t)
    Left _  -> Left "bytes -> string promotion: invalid UTF-8"
resolveValue (ResolvedRecord fieldRes) (AV.Record wFields) =
  AV.Record <$> V.mapM (resolveField wFields) fieldRes
resolveValue (ResolvedEnum name mapping) (AV.Enum wIdx) =
  if wIdx < V.length mapping
  then Right (AV.Enum (mapping V.! wIdx))
  else Left $ "enum '" ++ T.unpack name ++ "': writer index out of range"
resolveValue (ResolvedArray res) (AV.Array items) =
  AV.Array <$> V.mapM (resolveValue res) items
resolveValue (ResolvedMap res) (AV.Map entries) =
  AV.Map <$> V.mapM (\(k, v) -> (k,) <$> resolveValue res v) entries
resolveValue (ResolvedUnion resolutions) (AV.Union wIdx val) = do
  if wIdx < V.length resolutions
  then resolveValue (resolutions V.! wIdx) val
  else Left "union: writer branch index out of range"
resolveValue _ _ = Left "resolution/value mismatch"

resolveField :: V.Vector AV.Value -> FieldResolution -> Either String AV.Value
resolveField wFields (FieldFromWriter idx res) =
  if idx < V.length wFields
  then resolveValue res (wFields V.! idx)
  else Left "field index out of range in writer record"
resolveField _ (FieldDefault val) = Right val

-- ============================================================
-- Helpers
-- ============================================================

showType :: AvroType -> String
showType (AvroPrimitive s)                = show s
showType AvroRecord{avroRecordName = n}   = "record<" ++ T.unpack n ++ ">"
showType AvroEnum{avroEnumName = n}       = "enum<" ++ T.unpack n ++ ">"
showType AvroArray{}                      = "array"
showType AvroMap{}                        = "map"
showType AvroUnion{}                      = "union"
showType AvroFixed{avroFixedName = n}     = "fixed<" ++ T.unpack n ++ ">"
showType AvroLogical{}                    = "logical"
