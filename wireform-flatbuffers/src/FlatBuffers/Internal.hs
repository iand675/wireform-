{-# LANGUAGE BangPatterns #-}
-- | Internal helpers shared by 'FlatBuffers.Encode' and
-- 'FlatBuffers.Decode'.
--
-- Not part of the public API; the contents here are subject to change
-- without notice.
module FlatBuffers.Internal
  ( -- * Compiled schema environment
    Env(..)
  , mkEnv
  , schemaFileIdentifier
  , padIdent
    -- * Schema lookups
  , rootTableDef
  , lookupTable
  , lookupStruct
  , lookupEnum
  , lookupUnion
    -- * Field-type classification
  , isReferenceType
  , inlineSizeOfType
    -- * Defaults
  , defaultForField
  , defaultForFieldType
  , defaultStruct
  , lookupEnumMember
  , coerceToEnumStorage
    -- * Coercions
  , coerceBool
  , coerceI8, coerceI16, coerceI32, coerceI64
  , coerceU8, coerceU16, coerceU32, coerceU64
  , coerceFloat, coerceDouble
    -- * Misc
  , traverseN
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)

import qualified FlatBuffers.Value as F
import FlatBuffers.Schema

------------------------------------------------------------------------
-- Compiled schema environment
------------------------------------------------------------------------

-- | A schema with its table\/struct\/enum\/union declarations indexed
-- by name for fast field-by-field encode and decode passes.
data Env = Env
  { envTables  :: !(Map Text TableDef)
  , envStructs :: !(Map Text FBStructDef)
  , envEnums   :: !(Map Text FBEnumDef)
  , envUnions  :: !(Map Text FBUnionDef)
  }

mkEnv :: FlatBuffersSchema -> Env
mkEnv schema = V.foldl' step (Env Map.empty Map.empty Map.empty Map.empty) (fbsDecls schema)
  where
    step !e d = case d of
      FBTable  t -> e { envTables  = Map.insert (tdName t)  t (envTables  e) }
      FBStruct s -> e { envStructs = Map.insert (fsdName s) s (envStructs e) }
      FBEnum   n -> e { envEnums   = Map.insert (fedName n) n (envEnums   e) }
      FBUnion  u -> e { envUnions  = Map.insert (fudName u) u (envUnions  e) }

-- | Schema-declared file identifier truncated to four bytes (the
-- FlatBuffers wire layout slot is exactly four bytes wide).
schemaFileIdentifier :: FlatBuffersSchema -> Maybe ByteString
schemaFileIdentifier s = (TE.encodeUtf8 . T.take 4) <$> fbsFileIdentifier s

-- | Pad / truncate a candidate file identifier to four bytes.
padIdent :: ByteString -> ByteString
padIdent bs
  | BS.length bs >= 4 = BS.take 4 bs
  | otherwise = bs <> BS.replicate (4 - BS.length bs) 0

------------------------------------------------------------------------
-- Schema lookups (declared-order traversal; cheap because schemas are
-- small).
------------------------------------------------------------------------

rootTableDef :: FlatBuffersSchema -> Either String TableDef
rootTableDef schema = case fbsRootType schema of
  Nothing -> Left "FlatBuffers: schema has no root_type declaration"
  Just rootName -> case lookupTable schema rootName of
    Just td -> Right td
    Nothing -> Left $ "FlatBuffers: root_type "
                   ++ T.unpack rootName ++ " is not a declared table"

lookupTable :: FlatBuffersSchema -> Text -> Maybe TableDef
lookupTable schema name = lookupDecl (fbsDecls schema) pickTable
  where
    pickTable (FBTable t) | tdName t == name = Just t
    pickTable _ = Nothing

lookupStruct :: FlatBuffersSchema -> Text -> Maybe FBStructDef
lookupStruct schema name = lookupDecl (fbsDecls schema) pickStruct
  where
    pickStruct (FBStruct s) | fsdName s == name = Just s
    pickStruct _ = Nothing

lookupEnum :: FlatBuffersSchema -> Text -> Maybe FBEnumDef
lookupEnum schema name = lookupDecl (fbsDecls schema) pickEnum
  where
    pickEnum (FBEnum e) | fedName e == name = Just e
    pickEnum _ = Nothing

lookupUnion :: FlatBuffersSchema -> Text -> Maybe FBUnionDef
lookupUnion schema name = lookupDecl (fbsDecls schema) pickUnion
  where
    pickUnion (FBUnion u) | fudName u == name = Just u
    pickUnion _ = Nothing

lookupDecl :: Vector FBDeclaration -> (FBDeclaration -> Maybe a) -> Maybe a
lookupDecl decls f =
  V.foldr (\d acc -> case f d of Just x -> Just x; Nothing -> acc) Nothing decls

------------------------------------------------------------------------
-- Field-type classification
------------------------------------------------------------------------

-- | Reference-typed fields are absent on decode if their vtable cell is
-- 0; scalar / struct / enum fields fall back to a default value
-- instead.
isReferenceType :: Env -> FBType -> Bool
isReferenceType _   FTString     = True
isReferenceType _   (FTVector _) = True
isReferenceType env (FTNamed n)  =
  case Map.lookup n (envStructs env) of
    Just _  -> False  -- inline, has an all-zero default
    Nothing -> case Map.lookup n (envEnums env) of
      Just _  -> False  -- enums are scalars
      Nothing -> True   -- table or union — absent
isReferenceType _ _ = False

-- | Inline byte size of a type when stored inside a vector or struct.
inlineSizeOfType :: Env -> FBType -> Int
inlineSizeOfType env = \case
  FTBool    -> 1
  FTByte    -> 1
  FTUByte   -> 1
  FTShort   -> 2
  FTUShort  -> 2
  FTInt     -> 4
  FTUInt    -> 4
  FTLong    -> 8
  FTULong   -> 8
  FTFloat   -> 4
  FTDouble  -> 8
  FTString  -> 4   -- uoffset
  FTVector _ -> 4  -- uoffset
  FTNamed name -> case Map.lookup name (envStructs env) of
    Just sd -> sum (map (inlineSizeOfType env . snd) (V.toList (fsdFields sd)))
    Nothing -> case Map.lookup name (envEnums env) of
      Just ed -> inlineSizeOfType env (fedUnderlyingType ed)
      Nothing -> 4   -- table or union: uoffset

------------------------------------------------------------------------
-- Default values
------------------------------------------------------------------------

-- | Resolve a field's default value when the vtable says it's absent.
-- Handles enum-name-style defaults (e.g. @= Green@) by looking the name
-- up against the enum's member assignments.
defaultForField :: Env -> TableField -> F.Value
defaultForField env tf = case tfType tf of
  FTNamed n -> case Map.lookup n (envEnums env) of
    Just ed -> case tfDefault tf of
      Just txt -> case lookupEnumMember ed txt of
        Just int -> coerceToEnumStorage (fedUnderlyingType ed) int
        Nothing  -> defaultScalar tf
      Nothing -> defaultScalar tf
    Nothing -> case Map.lookup n (envStructs env) of
      Just sd -> defaultStruct env sd
      Nothing -> defaultScalar tf
  _ -> defaultScalar tf

-- | Per-spec, structs have no per-field default; their default is all
-- zero bytes.
defaultStruct :: Env -> FBStructDef -> F.Value
defaultStruct env sd =
  F.VStruct (V.fromList (map (defaultForFieldType env . snd) (V.toList (fsdFields sd))))

-- | Zero-valued default for a bare 'FBType' (used for struct fields,
-- which never carry per-field defaults).
defaultForFieldType :: Env -> FBType -> F.Value
defaultForFieldType env = \case
  FTBool   -> F.VBool False
  FTByte   -> F.VInt8 0
  FTUByte  -> F.VWord8 0
  FTShort  -> F.VInt16 0
  FTUShort -> F.VWord16 0
  FTInt    -> F.VInt32 0
  FTUInt   -> F.VWord32 0
  FTLong   -> F.VInt64 0
  FTULong  -> F.VWord64 0
  FTFloat  -> F.VFloat 0
  FTDouble -> F.VDouble 0
  FTNamed n -> case Map.lookup n (envStructs env) of
    Just sd -> defaultStruct env sd
    Nothing -> case Map.lookup n (envEnums env) of
      Just ed -> coerceToEnumStorage (fedUnderlyingType ed) 0
      Nothing -> F.VInt32 0
  _ -> F.VInt32 0

defaultScalar :: TableField -> F.Value
defaultScalar tf = case (tfType tf, tfDefault tf) of
  (FTBool,   Just t) -> F.VBool   (parseBoolDefault t)
  (FTByte,   Just t) -> F.VInt8   (parseIntDefault t)
  (FTUByte,  Just t) -> F.VWord8  (parseIntDefault t)
  (FTShort,  Just t) -> F.VInt16  (parseIntDefault t)
  (FTUShort, Just t) -> F.VWord16 (parseIntDefault t)
  (FTInt,    Just t) -> F.VInt32  (parseIntDefault t)
  (FTUInt,   Just t) -> F.VWord32 (parseIntDefault t)
  (FTLong,   Just t) -> F.VInt64  (parseIntDefault t)
  (FTULong,  Just t) -> F.VWord64 (parseIntDefault t)
  (FTFloat,  Just t) -> F.VFloat  (parseFloatDefault t)
  (FTDouble, Just t) -> F.VDouble (parseFloatDefault t)
  (FTBool,   Nothing) -> F.VBool   False
  (FTByte,   Nothing) -> F.VInt8   0
  (FTUByte,  Nothing) -> F.VWord8  0
  (FTShort,  Nothing) -> F.VInt16  0
  (FTUShort, Nothing) -> F.VWord16 0
  (FTInt,    Nothing) -> F.VInt32  0
  (FTUInt,   Nothing) -> F.VWord32 0
  (FTLong,   Nothing) -> F.VInt64  0
  (FTULong,  Nothing) -> F.VWord64 0
  (FTFloat,  Nothing) -> F.VFloat  0
  (FTDouble, Nothing) -> F.VDouble 0
  _ -> F.VBool False

parseBoolDefault :: Text -> Bool
parseBoolDefault t = case T.toLower t of
  "true" -> True
  "1"    -> True
  _      -> False

parseIntDefault :: Integral a => Text -> a
parseIntDefault t = case TR.signed TR.decimal t of
  Right (n, _) -> fromInteger n
  Left _       -> 0

parseFloatDefault :: RealFloat a => Text -> a
parseFloatDefault t = case TR.signed TR.rational t of
  Right (n, _) -> realToFrac (n :: Double)
  Left _       -> 0

-- | Resolve an enum member by name to its assigned integer value.
lookupEnumMember :: FBEnumDef -> Text -> Maybe Integer
lookupEnumMember ed name =
  let assigned = computeEnumAssignments (V.toList (fedValues ed))
  in lookup name assigned

-- | Walk the enum members assigning values: explicit values stand,
-- unspecified members take @previous + 1@ (FlatBuffers semantics).
computeEnumAssignments :: [(Text, Maybe Int64)] -> [(Text, Integer)]
computeEnumAssignments = go (-1)
  where
    go _ [] = []
    go !prev ((nm, mv) : rest) =
      let !cur = case mv of
            Just v  -> fromIntegral v
            Nothing -> prev + 1
      in (nm, cur) : go cur rest

-- | Wrap an integer in the 'F.Value' constructor matching the enum's
-- declared underlying scalar type.
coerceToEnumStorage :: FBType -> Integer -> F.Value
coerceToEnumStorage ty n = case ty of
  FTBool   -> F.VBool   (n /= 0)
  FTByte   -> F.VInt8   (fromInteger n)
  FTUByte  -> F.VWord8  (fromInteger n)
  FTShort  -> F.VInt16  (fromInteger n)
  FTUShort -> F.VWord16 (fromInteger n)
  FTInt    -> F.VInt32  (fromInteger n)
  FTUInt   -> F.VWord32 (fromInteger n)
  FTLong   -> F.VInt64  (fromInteger n)
  FTULong  -> F.VWord64 (fromInteger n)
  _        -> F.VInt32  (fromInteger n)

------------------------------------------------------------------------
-- Coercions: accept any 'F.Value' of compatible width.
------------------------------------------------------------------------

coerceBool :: F.Value -> Bool
coerceBool = \case
  F.VBool b   -> b
  F.VInt8 n   -> n /= 0
  F.VWord8 n  -> n /= 0
  F.VInt16 n  -> n /= 0
  F.VWord16 n -> n /= 0
  F.VInt32 n  -> n /= 0
  F.VWord32 n -> n /= 0
  F.VInt64 n  -> n /= 0
  F.VWord64 n -> n /= 0
  _ -> False

coerceI8 :: F.Value -> Int8
coerceI8 = fromInteger . valueAsInteger

coerceU8 :: F.Value -> Word8
coerceU8 = fromInteger . valueAsInteger

coerceI16 :: F.Value -> Int16
coerceI16 = fromInteger . valueAsInteger

coerceU16 :: F.Value -> Word16
coerceU16 = fromInteger . valueAsInteger

coerceI32 :: F.Value -> Int32
coerceI32 = fromInteger . valueAsInteger

coerceU32 :: F.Value -> Word32
coerceU32 = fromInteger . valueAsInteger

coerceI64 :: F.Value -> Int64
coerceI64 = fromInteger . valueAsInteger

coerceU64 :: F.Value -> Word64
coerceU64 = fromInteger . valueAsInteger

coerceFloat :: F.Value -> Float
coerceFloat = \case
  F.VFloat f  -> f
  F.VDouble d -> realToFrac d
  v           -> fromInteger (valueAsInteger v)

coerceDouble :: F.Value -> Double
coerceDouble = \case
  F.VFloat f  -> realToFrac f
  F.VDouble d -> d
  v           -> fromInteger (valueAsInteger v)

valueAsInteger :: F.Value -> Integer
valueAsInteger = \case
  F.VBool b   -> if b then 1 else 0
  F.VInt8 n   -> fromIntegral n
  F.VInt16 n  -> fromIntegral n
  F.VInt32 n  -> fromIntegral n
  F.VInt64 n  -> fromIntegral n
  F.VWord8 n  -> fromIntegral n
  F.VWord16 n -> fromIntegral n
  F.VWord32 n -> fromIntegral n
  F.VWord64 n -> fromIntegral n
  F.VFloat f  -> truncate f
  F.VDouble d -> truncate d
  _           -> 0

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

-- | @traverseN n f@ runs @f@ over @[0..n-1]@, collecting results.
traverseN :: Int -> (Int -> Either String a) -> Either String [a]
traverseN n f = go 0
  where
    go !i
      | i >= n = Right []
      | otherwise = do
          x <- f i
          xs <- go (i + 1)
          Right (x : xs)
