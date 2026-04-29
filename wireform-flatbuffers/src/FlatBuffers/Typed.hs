{-# LANGUAGE BangPatterns #-}
-- | Schema-aware FlatBuffers encode/decode.
--
-- The schema-less API in 'FlatBuffers.Encode' and 'FlatBuffers.Decode'
-- can't recover signed-vs-unsigned, float-vs-int, or string-vs-vector
-- distinctions because nothing on the wire identifies a field's type:
-- a 4-byte cell is always just four bytes. This module accepts a
-- parsed @.fbs@ schema and uses it to:
--
--   * route each 'F.Value' to the right encoder for its declared type
--     (so e.g. an @int@ field is always written as a 4-byte little-endian
--     signed integer regardless of which @VInt32@ \/ @VWord32@ \/ @VInt64@
--     constructor the user happened to supply),
--   * apply schema-declared default values when a field is absent,
--   * disambiguate strings, vectors, structs and nested tables on
--     decode using the schema's field types,
--   * automatically embed the schema's @file_identifier@ on encode
--     and verify it on decode.
--
-- The schema-less encoder/decoder are still the path used when no
-- schema is available (and when a schema is provided, this module
-- reuses the same low-level chunk planner, so vtable dedup and the
-- back-to-front layout are inherited unchanged).
--
-- @
-- import qualified FlatBuffers.Parser as P
-- import qualified FlatBuffers.Typed  as FT
-- import qualified FlatBuffers.Value  as F
-- import qualified Data.Vector as V
--
-- let Right schema = P.parseFlatBuffers
--       \"table Monster { name:string; hp:short = 100; }\\n\\
--       \\root_type Monster;\"
--
-- let monster = F.VTable (V.fromList
--       [ Just (F.VString \"Goblin\")
--       , Just (F.VInt16 50)
--       ])
--
-- let Right bs   = FT.encodeWithSchema schema monster
-- let Right back = FT.decodeWithSchema schema bs
-- @
module FlatBuffers.Typed
  ( encodeWithSchema
  , decodeWithSchema
  , -- * Convenience
    rootTableDef
  , lookupTable
  , lookupStruct
  , lookupEnum
  , lookupUnion
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Float (castDoubleToWord64, castFloatToWord32,
                  castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Wireform.Encode.Direct (directEncode)
import qualified FlatBuffers.Value as F
import FlatBuffers.Schema

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Encode a value against a schema's @root_type@.
encodeWithSchema :: FlatBuffersSchema -> F.Value -> Either String ByteString
encodeWithSchema !schema !val = do
  rootDef <- rootTableDef schema
  let env = mkEnv schema
      ident = schemaFileIdentifier schema
  encodeRoot env ident rootDef val

-- | Decode a buffer against a schema's @root_type@.
decodeWithSchema :: FlatBuffersSchema -> ByteString -> Either String F.Value
decodeWithSchema !schema !bs = do
  rootDef <- rootTableDef schema
  let env = mkEnv schema
  case schemaFileIdentifier schema of
    Just ident
      | BS.length bs >= 8
      , let got = BS.take 4 (BS.drop 4 bs)
      , let want = padIdent ident
      , got /= want ->
          Left $ "FlatBuffers.Typed.decode: expected file_identifier "
              ++ show want ++ " but got " ++ show got
    _ -> Right ()
  if BS.length bs < 4
    then Left "FlatBuffers.Typed.decode: input too short"
    else do
      let !rootOff = fromIntegral (readLE32 bs 0) :: Int
      if rootOff <= 0 || rootOff >= BS.length bs
        then Left "FlatBuffers.Typed.decode: invalid root offset"
        else decodeTable env rootDef bs rootOff

-- | Look up the schema's @root_type@ and return the matching table
-- definition.  Errors out if the schema doesn't declare a root type or
-- the root type isn't a table.
rootTableDef :: FlatBuffersSchema -> Either String TableDef
rootTableDef schema = case fbsRootType schema of
  Nothing -> Left "FlatBuffers.Typed: schema has no root_type declaration"
  Just rootName -> case lookupTable schema rootName of
    Just td -> Right td
    Nothing -> Left $ "FlatBuffers.Typed: root_type "
                   ++ T.unpack rootName ++ " is not a declared table"

-- | Look up a table declaration by name.
lookupTable :: FlatBuffersSchema -> Text -> Maybe TableDef
lookupTable schema name = lookupDecl (fbsDecls schema) name pickTable
  where
    pickTable (FBTable t) | tdName t == name = Just t
    pickTable _ = Nothing

-- | Look up a struct declaration by name.
lookupStruct :: FlatBuffersSchema -> Text -> Maybe FBStructDef
lookupStruct schema name = lookupDecl (fbsDecls schema) name pickStruct
  where
    pickStruct (FBStruct s) | fsdName s == name = Just s
    pickStruct _ = Nothing

-- | Look up an enum declaration by name.
lookupEnum :: FlatBuffersSchema -> Text -> Maybe FBEnumDef
lookupEnum schema name = lookupDecl (fbsDecls schema) name pickEnum
  where
    pickEnum (FBEnum e) | fedName e == name = Just e
    pickEnum _ = Nothing

-- | Look up a union declaration by name.
lookupUnion :: FlatBuffersSchema -> Text -> Maybe FBUnionDef
lookupUnion schema name = lookupDecl (fbsDecls schema) name pickUnion
  where
    pickUnion (FBUnion u) | fudName u == name = Just u
    pickUnion _ = Nothing

lookupDecl :: Vector FBDeclaration -> Text -> (FBDeclaration -> Maybe a) -> Maybe a
lookupDecl decls _ f =
  V.foldr (\d acc -> case f d of Just x -> Just x; Nothing -> acc) Nothing decls

------------------------------------------------------------------------
-- Environment: prebuilt name -> declaration tables
------------------------------------------------------------------------

-- | Compiled schema: name lookups indexed once for fast field-by-field
-- encode and decode passes.
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

schemaFileIdentifier :: FlatBuffersSchema -> Maybe ByteString
schemaFileIdentifier s = (TE.encodeUtf8 . T.take 4) <$> fbsFileIdentifier s

padIdent :: ByteString -> ByteString
padIdent bs
  | BS.length bs >= 4 = BS.take 4 bs
  | otherwise = bs <> BS.replicate (4 - BS.length bs) 0

------------------------------------------------------------------------
-- Encoder: re-uses the same chunk-based planner as FlatBuffers.Encode.
------------------------------------------------------------------------

data Chunk
  = CScalar  !ByteString
  | CString  !ByteString
  | CVector  !Int [PItem]
  | CTable   !Int !Int [PField] !Int
  | CVTable  [Word16] !Int
  deriving stock (Show)

data PField
  = PInline   !ByteString
  | PRefChunk !Int
  deriving stock (Show)

data PItem
  = PIInline   !ByteString
  | PIRefChunk !Int
  deriving stock (Show)

data PlanSt = PlanSt
  { psChunks  :: !(IM.IntMap Chunk)
  , psNext    :: !Int
  , psVtables :: !(Map (Int, Int, [Word16]) Int)
  , psVtIds   :: ![Int]
  }

emptyPlan :: PlanSt
emptyPlan = PlanSt IM.empty 0 Map.empty []

addChunk :: Chunk -> PlanSt -> (Int, PlanSt)
addChunk !c !st =
  let !i = psNext st
  in (i, st { psChunks = IM.insert i c (psChunks st), psNext = i + 1 })

encodeRoot :: Env -> Maybe ByteString -> TableDef -> F.Value -> Either String ByteString
encodeRoot env mIdent rootDef val = do
  (rootId, st) <- planTable env rootDef val emptyPlan
  Right (renderRoot mIdent rootId st)

planTable :: Env -> TableDef -> F.Value -> PlanSt -> Either String (Int, PlanSt)
planTable env td val st0 = do
  fieldVals <- alignFields td val
  -- Encode each field in declaration order, recording (cell, write-action).
  (rawFields, st1) <- planFields env (V.toList (tdFields td)) fieldVals st0
  let (sigCells, fieldEntries, !inlineSz) = layoutFields rawFields
      !nFields = length rawFields
      !key = (nFields, inlineSz, sigCells)
  case Map.lookup key (psVtables st1) of
    Just vtId ->
      let (tid, st2) = addChunk (CTable vtId inlineSz fieldEntries (padTo4 (4 + inlineSz))) st1
      in Right (tid, st2)
    Nothing ->
      let (vtId, st2) = addChunk (CVTable sigCells inlineSz) st1
          st3 = st2 { psVtables = Map.insert key vtId (psVtables st2)
                    , psVtIds   = psVtIds st2 ++ [vtId] }
          (tid, st4) = addChunk (CTable vtId inlineSz fieldEntries (padTo4 (4 + inlineSz))) st3
      in Right (tid, st4)

-- | Reconcile a user-provided 'F.Value' against the schema's table
-- definition.  Accepts both vector-of-Maybe (shape used by
-- 'FlatBuffers.Encode') and a struct-style positional vector with
-- 'F.VStruct' wrapping inline fields.
alignFields :: TableDef -> F.Value -> Either String [Maybe F.Value]
alignFields td (F.VTable fields)
  | V.length fields == V.length (tdFields td) = Right (V.toList fields)
  | otherwise = Left $ "FlatBuffers.Typed: table " ++ T.unpack (tdName td)
              ++ " expected " ++ show (V.length (tdFields td))
              ++ " fields, got " ++ show (V.length fields)
alignFields td _ =
  Left $ "FlatBuffers.Typed: expected VTable for " ++ T.unpack (tdName td)

planFields
  :: Env -> [TableField] -> [Maybe F.Value] -> PlanSt
  -> Either String ([Maybe PField], PlanSt)
planFields _ [] [] st = Right ([], st)
planFields env (tf : tfs) (mv : mvs) st = case mv of
  Nothing
    | omitOnDefault tf -> do
        (rest, st') <- planFields env tfs mvs st
        Right (Nothing : rest, st')
    | otherwise -> do
        (pf, st1) <- planField env (tfType tf) (defaultValue tf) st
        (rest, st2) <- planFields env tfs mvs st1
        Right (Just pf : rest, st2)
  Just v -> do
    (pf, st1) <- planField env (tfType tf) v st
    (rest, st2) <- planFields env tfs mvs st1
    Right (Just pf : rest, st2)
planFields _ _ _ _ =
  Left "FlatBuffers.Typed: internal: planFields field/value list mismatch"

-- | A field with a default value AND a payload that exactly matches the
-- default may be omitted from the vtable (saves space and round-trips
-- through a default-applying decoder).  For simplicity we currently
-- always materialise default-bearing fields; the wire is correct
-- either way.
omitOnDefault :: TableField -> Bool
omitOnDefault _ = True

defaultValue :: TableField -> F.Value
defaultValue tf = case (tfType tf, tfDefault tf) of
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
  -- No default declared: zero-valued scalar.
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
  -- Reference-typed fields without a value are left absent at the
  -- caller layer (handled in 'planFields').
  _ -> F.VBool False

parseBoolDefault :: Text -> Bool
parseBoolDefault t = case T.toLower t of
  "true"  -> True
  "1"     -> True
  _       -> False

parseIntDefault :: (Integral a, Num a) => Text -> a
parseIntDefault t = case TR.signed TR.decimal t of
  Right (n, _) -> fromInteger n
  Left _       -> 0

parseFloatDefault :: (RealFloat a, Read a) => Text -> a
parseFloatDefault t = case TR.signed TR.rational t of
  Right (n, _) -> realToFrac (n :: Double)
  Left _       -> 0

planField :: Env -> FBType -> F.Value -> PlanSt -> Either String (PField, PlanSt)
planField env ty v st = case ty of
  FTBool   -> Right (PInline (BS.singleton (boolByte (coerceBool v))), st)
  FTByte   -> Right (PInline (le8s  (coerceI8  v)), st)
  FTUByte  -> Right (PInline (BS.singleton (coerceU8 v)), st)
  FTShort  -> Right (PInline (le16s (coerceI16 v)), st)
  FTUShort -> Right (PInline (le16  (coerceU16 v)), st)
  FTInt    -> Right (PInline (le32s (coerceI32 v)), st)
  FTUInt   -> Right (PInline (le32  (coerceU32 v)), st)
  FTLong   -> Right (PInline (le64s (coerceI64 v)), st)
  FTULong  -> Right (PInline (le64  (coerceU64 v)), st)
  FTFloat  -> Right (PInline (le32 (castFloatToWord32 (coerceFloat v))), st)
  FTDouble -> Right (PInline (le64 (castDoubleToWord64 (coerceDouble v))), st)
  FTString -> case v of
    F.VString t ->
      let (cid, st') = addChunk (CString (TE.encodeUtf8 t)) st
      in Right (PRefChunk cid, st')
    _ -> Left "FlatBuffers.Typed: expected VString for string field"
  FTVector elemTy -> case v of
    F.VVector vs -> do
      (items, st') <- planVectorItems env elemTy (V.toList vs) st
      let (cid, st'') = addChunk (CVector (V.length vs) items) st'
      Right (PRefChunk cid, st'')
    _ -> Left "FlatBuffers.Typed: expected VVector for vector field"
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      (cid, st') <- planTable env td v st
      Right (PRefChunk cid, st')
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> do
        bs <- encodeStruct env sd v
        Right (PInline bs, st)
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> do
          let underlying = fedUnderlyingType ed
          planField env underlying v st
        Nothing -> case Map.lookup name (envUnions env) of
          Just _ -> Left $ "FlatBuffers.Typed: union encoding requires both \
                           \the discriminator and value field; not yet supported"
          Nothing -> Left $ "FlatBuffers.Typed: unknown named type "
                          ++ T.unpack name

planVectorItems
  :: Env -> FBType -> [F.Value] -> PlanSt
  -> Either String ([PItem], PlanSt)
planVectorItems _ _ [] st = Right ([], st)
planVectorItems env ety (v : vs) st = do
  (item, st1) <- planVectorItem env ety v st
  (rest, st2) <- planVectorItems env ety vs st1
  Right (item : rest, st2)

planVectorItem :: Env -> FBType -> F.Value -> PlanSt -> Either String (PItem, PlanSt)
planVectorItem env ety v st = case ety of
  FTBool   -> Right (PIInline (BS.singleton (boolByte (coerceBool v))), st)
  FTByte   -> Right (PIInline (le8s  (coerceI8  v)), st)
  FTUByte  -> Right (PIInline (BS.singleton (coerceU8 v)), st)
  FTShort  -> Right (PIInline (le16s (coerceI16 v)), st)
  FTUShort -> Right (PIInline (le16  (coerceU16 v)), st)
  FTInt    -> Right (PIInline (le32s (coerceI32 v)), st)
  FTUInt   -> Right (PIInline (le32  (coerceU32 v)), st)
  FTLong   -> Right (PIInline (le64s (coerceI64 v)), st)
  FTULong  -> Right (PIInline (le64  (coerceU64 v)), st)
  FTFloat  -> Right (PIInline (le32 (castFloatToWord32 (coerceFloat v))), st)
  FTDouble -> Right (PIInline (le64 (castDoubleToWord64 (coerceDouble v))), st)
  FTString -> case v of
    F.VString t ->
      let (cid, st') = addChunk (CString (TE.encodeUtf8 t)) st
      in Right (PIRefChunk cid, st')
    _ -> Left "FlatBuffers.Typed: expected VString in vector<string>"
  FTVector inner -> case v of
    F.VVector inner' -> do
      (items, st') <- planVectorItems env inner (V.toList inner') st
      let (cid, st'') = addChunk (CVector (V.length inner') items) st'
      Right (PIRefChunk cid, st'')
    _ -> Left "FlatBuffers.Typed: expected VVector in vector<vector<...>>"
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      (cid, st') <- planTable env td v st
      Right (PIRefChunk cid, st')
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> do
        bs <- encodeStruct env sd v
        Right (PIInline bs, st)
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> planVectorItem env (fedUnderlyingType ed) v st
        Nothing -> Left $ "FlatBuffers.Typed: unknown named element type "
                       ++ T.unpack name

-- | Encode a struct value (fixed-size, inline) to a flat byte slab.
encodeStruct :: Env -> FBStructDef -> F.Value -> Either String ByteString
encodeStruct env sd v = case v of
  F.VStruct fields
    | V.length fields == V.length (fsdFields sd) ->
        BS.concat <$> goFields (V.toList fields) (V.toList (fsdFields sd))
    | otherwise -> Left $ "FlatBuffers.Typed: struct " ++ T.unpack (fsdName sd)
                       ++ " expected " ++ show (V.length (fsdFields sd))
                       ++ " fields, got " ++ show (V.length fields)
  _ -> Left $ "FlatBuffers.Typed: expected VStruct for " ++ T.unpack (fsdName sd)
  where
    goFields [] [] = Right []
    goFields (fv : fvs) ((_, fty) : rest) = do
      bs <- encodeStructField env fty fv
      bss <- goFields fvs rest
      Right (bs : bss)
    goFields _ _ = Right []  -- unreachable due to the length guard above

encodeStructField :: Env -> FBType -> F.Value -> Either String ByteString
encodeStructField env ty v = case ty of
  FTBool   -> Right (BS.singleton (boolByte (coerceBool v)))
  FTByte   -> Right (le8s  (coerceI8  v))
  FTUByte  -> Right (BS.singleton (coerceU8 v))
  FTShort  -> Right (le16s (coerceI16 v))
  FTUShort -> Right (le16  (coerceU16 v))
  FTInt    -> Right (le32s (coerceI32 v))
  FTUInt   -> Right (le32  (coerceU32 v))
  FTLong   -> Right (le64s (coerceI64 v))
  FTULong  -> Right (le64  (coerceU64 v))
  FTFloat  -> Right (le32 (castFloatToWord32 (coerceFloat v)))
  FTDouble -> Right (le64 (castDoubleToWord64 (coerceDouble v)))
  FTNamed name -> case Map.lookup name (envStructs env) of
    Just sd -> encodeStruct env sd v
    Nothing -> case Map.lookup name (envEnums env) of
      Just ed -> encodeStructField env (fedUnderlyingType ed) v
      Nothing -> Left $ "FlatBuffers.Typed: " ++ T.unpack name
                     ++ " is not allowed inline in a struct"
  _ -> Left "FlatBuffers.Typed: only scalars/structs/enums are allowed in structs"

------------------------------------------------------------------------
-- Field layout (shared with FlatBuffers.Encode in spirit)
------------------------------------------------------------------------

layoutFields :: [Maybe PField] -> ([Word16], [PField], Int)
layoutFields fs0 = go fs0 4 [] []
  where
    go [] !curOff cellsR fieldsR =
      (reverse cellsR, reverse fieldsR, curOff - 4)
    go (mf : rest) !curOff cellsR fieldsR = case mf of
      Nothing -> go rest curOff (0 : cellsR) fieldsR
      Just pf ->
        let !sz = pfieldSize pf
            !cell = fromIntegral curOff :: Word16
        in go rest (curOff + sz) (cell : cellsR) (pf : fieldsR)

pfieldSize :: PField -> Int
pfieldSize (PInline bs)  = BS.length bs
pfieldSize (PRefChunk _) = 4
{-# INLINE pfieldSize #-}

padTo4 :: Int -> Int
padTo4 n = (4 - (n `mod` 4)) `mod` 4
{-# INLINE padTo4 #-}

------------------------------------------------------------------------
-- Layout & emission (mirrors FlatBuffers.Encode.renderRoot)
------------------------------------------------------------------------

renderRoot :: Maybe ByteString -> Int -> PlanSt -> ByteString
renderRoot !mIdent !rootId !st =
  let !headerSize = if mIdent == Nothing then 4 else 8
      !chunks = psChunks st
      !vtIds = psVtIds st
      !mainIds = dfsOrder chunks rootId
      !order = vtIds ++ mainIds
      sizes = map (chunkSize . (chunks IM.!)) order
      offsets = scanlOffsets headerSize sizes
      offMap = IM.fromList (zip order offsets)
      !rootOffset = offMap IM.! rootId
      total = headerSize + sum sizes
  in directEncode total (\p _ -> do
       _ <- writeLE32 p 0 (fromIntegral rootOffset :: Word32)
       case mIdent of
         Nothing -> pure ()
         Just i  -> writeBytesAt p 4 (padIdent i)
       emitChunks p offMap chunks order offsets
       pure total)

dfsOrder :: IM.IntMap Chunk -> Int -> [Int]
dfsOrder chunks root = go [root] mempty []
  where
    go [] _ acc = reverse acc
    go (i : rest) seen acc
      | i `IM.member` seen = go rest seen acc
      | otherwise =
          let !seen' = IM.insert i () seen
              kids = childrenOf (chunks IM.! i)
          in go (kids ++ rest) seen' (i : acc)

    childrenOf :: Chunk -> [Int]
    childrenOf = \case
      CScalar _              -> []
      CString _              -> []
      CVector _ items        -> foldr collectItem [] items
      CTable _vtId _ fs _    -> foldr collectField [] fs
      CVTable {}             -> []

    collectItem (PIRefChunk c) acc = c : acc
    collectItem (PIInline _)   acc = acc

    collectField (PRefChunk c) acc = c : acc
    collectField (PInline _)   acc = acc

chunkSize :: Chunk -> Int
chunkSize = \case
  CScalar bs       -> BS.length bs
  CString bs       ->
    let !len = BS.length bs
        !raw = 4 + len + 1
    in raw + padTo4 raw
  CVector _ items  -> 4 + sum (map vectorItemSize items)
  CTable _ inlineSz _fs endPad -> 4 + inlineSz + endPad
  CVTable cells _  -> let !raw = 4 + 2 * length cells in raw + padTo4 raw

vectorItemSize :: PItem -> Int
vectorItemSize (PIInline bs) = BS.length bs
vectorItemSize (PIRefChunk _) = 4

scanlOffsets :: Int -> [Int] -> [Int]
scanlOffsets !start sizes = go start sizes
  where
    go !_ [] = []
    go !p (s : rest) = p : go (p + s) rest

emitChunks :: Ptr Word8 -> IM.IntMap Int -> IM.IntMap Chunk -> [Int] -> [Int] -> IO ()
emitChunks !p !offMap !chunks = go
  where
    go [] [] = pure ()
    go (i : is) (off : offs) = do
      writeChunk p offMap (chunks IM.! i) off
      go is offs
    go _ _ = pure ()

writeChunk :: Ptr Word8 -> IM.IntMap Int -> Chunk -> Int -> IO ()
writeChunk !p !offMap !chunk !off = case chunk of
  CScalar bs -> writeBytesAt p off bs

  CString bs -> do
    let !len = BS.length bs
    _ <- writeLE32 p off (fromIntegral len)
    writeBytesAt p (off + 4) bs
    pokeByteOff p (off + 4 + len) (0x00 :: Word8)
    let !raw = 4 + len + 1
        !pad = padTo4 raw
    writeZeros p (off + raw) pad

  CVector cnt items -> do
    _ <- writeLE32 p off (fromIntegral cnt)
    _ <- foldOffM (off + 4) items $ \o it -> case it of
      PIInline bs -> do
        writeBytesAt p o bs
        pure (o + BS.length bs)
      PIRefChunk cid -> do
        let !target = offMap IM.! cid
            !rel = fromIntegral (target - o) :: Word32
        _ <- writeLE32 p o rel
        pure (o + 4)
    pure ()

  CTable vtId inlineSz fieldsToWrite endPad -> do
    let !vtPos = offMap IM.! vtId
        !soff = fromIntegral (off - vtPos) :: Int32
    _ <- writeLE32 p off (fromIntegral soff)
    _ <- foldOffM (off + 4) fieldsToWrite $ \o pf -> case pf of
      PInline bs -> do
        writeBytesAt p o bs
        pure (o + BS.length bs)
      PRefChunk cid -> do
        let !target = offMap IM.! cid
            !rel = fromIntegral (target - o) :: Word32
        _ <- writeLE32 p o rel
        pure (o + 4)
    writeZeros p (off + 4 + inlineSz) endPad

  CVTable cells inlineSz -> do
    let !rawSize = 4 + 2 * length cells
        !padded = rawSize + padTo4 rawSize
    _ <- writeLE16 p off (fromIntegral rawSize)
    _ <- writeLE16 p (off + 2) (fromIntegral (4 + inlineSz))
    writeCells p (off + 4) cells
    writeZeros p (off + rawSize) (padded - rawSize)

writeCells :: Ptr Word8 -> Int -> [Word16] -> IO ()
writeCells _ _ [] = pure ()
writeCells p off (c : rest) = do
  _ <- writeLE16 p off c
  writeCells p (off + 2) rest

foldOffM :: Int -> [a] -> (Int -> a -> IO Int) -> IO Int
foldOffM !o0 xs0 f = go o0 xs0
  where
    go !o [] = pure o
    go !o (x : rest) = do
      o' <- f o x
      go o' rest

------------------------------------------------------------------------
-- Decoder
------------------------------------------------------------------------

decodeTable :: Env -> TableDef -> ByteString -> Int -> Either String F.Value
decodeTable env td bs tableOff = do
  ensure bs tableOff 4
  let !soff = fromIntegral (readLE32S bs tableOff) :: Int
      !vtOff = tableOff - soff
  ensure bs vtOff 4
  let !vtSize  = fromIntegral (readLE16 bs vtOff)       :: Int
      !inlineSz = fromIntegral (readLE16 bs (vtOff + 2)) :: Int
  if vtSize < 4 || inlineSz < 4
    then Left "FlatBuffers.Typed.decode: malformed vtable"
    else do
      let !nCellsAvail = (vtSize - 4) `div` 2
          fieldDefs    = V.toList (tdFields td)
      cells <- traverseN (length fieldDefs) $ \i ->
        if i < nCellsAvail
          then do
            ensure bs (vtOff + 4 + i * 2) 2
            Right (fromIntegral (readLE16 bs (vtOff + 4 + i * 2)) :: Int)
          else
            -- Fields declared in the schema but absent from this older
            -- vtable are treated as absent (forward-compatibility).
            Right 0
      fields <- traverseN (length fieldDefs) $ \i -> do
        let !cell = cells !! i
            !tf   = fieldDefs !! i
        if cell == 0
          then if isReferenceType env (tfType tf)
                 then Right Nothing
                 else Right (Just (defaultForField env tf))
          else do
            let !addr = tableOff + cell
            v <- decodeFieldValue env (tfType tf) bs addr
            Right (Just v)
      Right (F.VTable (V.fromList fields))

-- | Reference-typed fields are absent on decode if their cell is 0;
-- scalar/struct/enum fields fall back to defaults instead.
isReferenceType :: Env -> FBType -> Bool
isReferenceType _   FTString     = True
isReferenceType _   (FTVector _) = True
isReferenceType env (FTNamed n)  =
  case Map.lookup n (envStructs env) of
    Just _  -> False  -- inline, has a default
    Nothing -> case Map.lookup n (envEnums env) of
      Just _  -> False  -- enums are scalar
      Nothing -> True   -- table or union — absent
isReferenceType _ _ = False

-- | Resolve a field's default, looking up enum-name-style defaults
-- (\"Green\") against the matching enum's members.
defaultForField :: Env -> TableField -> F.Value
defaultForField env tf = case tfType tf of
  FTNamed n -> case Map.lookup n (envEnums env) of
    Just ed -> case tfDefault tf of
      Just txt -> case lookupEnumMember ed txt of
        Just int -> coerceToEnumStorage (fedUnderlyingType ed) int
        Nothing  -> defaultValue tf  -- numeric default
      Nothing -> defaultValue tf
    Nothing -> case Map.lookup n (envStructs env) of
      Just sd -> defaultStruct env sd
      Nothing -> defaultValue tf
  _ -> defaultValue tf

lookupEnumMember :: FBEnumDef -> Text -> Maybe Integer
lookupEnumMember ed name =
  let assigned = computeEnumAssignments (V.toList (fedValues ed))
  in lookup name assigned

-- | Walk the enum members assigning Int64 values: explicit values stand,
-- and unspecified members take @previous + 1@ (FlatBuffers semantics).
computeEnumAssignments :: [(Text, Maybe Int64)] -> [(Text, Integer)]
computeEnumAssignments = go (-1)
  where
    go _ [] = []
    go !prev ((nm, mv) : rest) =
      let !cur = case mv of
            Just v  -> fromIntegral v
            Nothing -> prev + 1
      in (nm, cur) : go cur rest

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

-- | All-zero default for an inline struct (per the FlatBuffers spec
-- structs have no per-field defaults; their default is all bytes zero).
defaultStruct :: Env -> FBStructDef -> F.Value
defaultStruct env sd =
  F.VStruct (V.fromList (map (defaultForFieldType env . snd) (V.toList (fsdFields sd))))

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

decodeFieldValue :: Env -> FBType -> ByteString -> Int -> Either String F.Value
decodeFieldValue env ty bs addr = case ty of
  FTBool   -> do ensure bs addr 1; Right (F.VBool (rdByte bs addr /= 0))
  FTByte   -> do ensure bs addr 1; Right (F.VInt8  (fromIntegral (rdByte bs addr) :: Int8))
  FTUByte  -> do ensure bs addr 1; Right (F.VWord8 (rdByte bs addr))
  FTShort  -> do ensure bs addr 2; Right (F.VInt16  (fromIntegral (readLE16 bs addr) :: Int16))
  FTUShort -> do ensure bs addr 2; Right (F.VWord16 (readLE16 bs addr))
  FTInt    -> do ensure bs addr 4; Right (F.VInt32  (fromIntegral (readLE32 bs addr) :: Int32))
  FTUInt   -> do ensure bs addr 4; Right (F.VWord32 (readLE32 bs addr))
  FTLong   -> do ensure bs addr 8; Right (F.VInt64  (fromIntegral (readLE64 bs addr) :: Int64))
  FTULong  -> do ensure bs addr 8; Right (F.VWord64 (readLE64 bs addr))
  FTFloat  -> do ensure bs addr 4; Right (F.VFloat  (castWord32ToFloat (readLE32 bs addr)))
  FTDouble -> do ensure bs addr 8; Right (F.VDouble (castWord64ToDouble (readLE64 bs addr)))
  FTString -> do
    ensure bs addr 4
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeString bs target
  FTVector elemTy -> do
    ensure bs addr 4
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeVector env elemTy bs target
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      ensure bs addr 4
      let !uoff = fromIntegral (readLE32 bs addr) :: Int
          !target = addr + uoff
      decodeTable env td bs target
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> decodeStruct env sd bs addr
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> decodeFieldValue env (fedUnderlyingType ed) bs addr
        Nothing -> Left $ "FlatBuffers.Typed.decode: unknown named type "
                       ++ T.unpack name

decodeString :: ByteString -> Int -> Either String F.Value
decodeString bs off = do
  ensure bs off 4
  let !len = fromIntegral (readLE32 bs off) :: Int
  ensure bs (off + 4) (len + 1)  -- +1 for trailing NUL
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 4) bs)
  case TE.decodeUtf8' raw of
    Right t -> Right (F.VString t)
    Left  _ -> Left "FlatBuffers.Typed.decode: invalid UTF-8 in string"

decodeVector :: Env -> FBType -> ByteString -> Int -> Either String F.Value
decodeVector env ety bs off = do
  ensure bs off 4
  let !cnt = fromIntegral (readLE32 bs off) :: Int
      !elemSz = inlineSizeOfType env ety
      !payloadStart = off + 4
  ensure bs payloadStart (cnt * elemSz)
  items <- traverseN cnt $ \i -> do
    let !addr = payloadStart + i * elemSz
    decodeVectorElem env ety bs addr
  Right (F.VVector (V.fromList items))

decodeVectorElem :: Env -> FBType -> ByteString -> Int -> Either String F.Value
decodeVectorElem env ety bs addr = case ety of
  FTString -> do
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeString bs target
  FTVector inner -> do
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeVector env inner bs target
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      let !uoff = fromIntegral (readLE32 bs addr) :: Int
          !target = addr + uoff
      decodeTable env td bs target
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> decodeStruct env sd bs addr
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> decodeFieldValue env (fedUnderlyingType ed) bs addr
        Nothing -> Left $ "FlatBuffers.Typed.decode: unknown named element "
                       ++ T.unpack name
  scalar -> decodeFieldValue env scalar bs addr

decodeStruct :: Env -> FBStructDef -> ByteString -> Int -> Either String F.Value
decodeStruct env sd bs off = do
  let go !pos [] acc = Right (reverse acc, pos)
      go !pos ((_, fty) : rest) acc = do
        v <- decodeFieldValue env fty bs pos
        let !sz = inlineSizeOfType env fty
        go (pos + sz) rest (v : acc)
  (vs, _) <- go off (V.toList (fsdFields sd)) []
  Right (F.VStruct (V.fromList vs))

-- | Inline byte size of a type when stored inside a vector or struct.
inlineSizeOfType :: Env -> FBType -> Int
inlineSizeOfType env = \case
  FTBool   -> 1
  FTByte   -> 1
  FTUByte  -> 1
  FTShort  -> 2
  FTUShort -> 2
  FTInt    -> 4
  FTUInt   -> 4
  FTLong   -> 8
  FTULong  -> 8
  FTFloat  -> 4
  FTDouble -> 8
  FTString -> 4   -- uoffset
  FTVector _ -> 4 -- uoffset
  FTNamed name -> case Map.lookup name (envStructs env) of
    Just sd -> sum (map (inlineSizeOfType env . snd) (V.toList (fsdFields sd)))
    Nothing -> case Map.lookup name (envEnums env) of
      Just ed -> inlineSizeOfType env (fedUnderlyingType ed)
      Nothing -> 4  -- table or union: uoffset

------------------------------------------------------------------------
-- Coercion helpers: accept any F.Value of compatible width.
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
-- Little-endian ByteString writers
------------------------------------------------------------------------

boolByte :: Bool -> Word8
boolByte b = if b then 1 else 0
{-# INLINE boolByte #-}

le8s :: Int8 -> ByteString
le8s n = BS.singleton (fromIntegral n)

le16 :: Word16 -> ByteString
le16 w = BS.pack [fromIntegral w, fromIntegral (w `shiftR` 8)]

le16s :: Int16 -> ByteString
le16s = le16 . fromIntegral

le32 :: Word32 -> ByteString
le32 w = BS.pack
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  ]

le32s :: Int32 -> ByteString
le32s = le32 . fromIntegral

le64 :: Word64 -> ByteString
le64 w = BS.pack
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 56)
  ]

le64s :: Int64 -> ByteString
le64s = le64 . fromIntegral

------------------------------------------------------------------------
-- Low-level byte I/O
------------------------------------------------------------------------

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readLE16 :: ByteString -> Int -> Word16
readLE16 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE16 #-}

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE32 #-}

readLE32S :: ByteString -> Int -> Int32
readLE32S bs off = fromIntegral (readLE32 bs off)
{-# INLINE readLE32S #-}

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off < 0 || off + n > BS.length bs =
      Left "FlatBuffers.Typed.decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

writeLE16 :: Ptr Word8 -> Int -> Word16 -> IO Int
writeLE16 p off w = do
  pokeByteOff p off w
  pure $! off + 2
{-# INLINE writeLE16 #-}

writeLE32 :: Ptr Word8 -> Int -> Word32 -> IO Int
writeLE32 p off w = do
  pokeByteOff p off w
  pure $! off + 4
{-# INLINE writeLE32 #-}

writeBytesAt :: Ptr Word8 -> Int -> ByteString -> IO ()
writeBytesAt !p !off (BSI.BS fp len) =
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
{-# INLINE writeBytesAt #-}

writeZeros :: Ptr Word8 -> Int -> Int -> IO ()
writeZeros !p !off !n = go 0
  where
    go !i
      | i >= n = pure ()
      | otherwise = do
          pokeByteOff p (off + i) (0x00 :: Word8)
          go (i + 1)

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

traverseN :: Int -> (Int -> Either String a) -> Either String [a]
traverseN n f = go 0
  where
    go !i
      | i >= n = Right []
      | otherwise = do
          x <- f i
          xs <- go (i + 1)
          Right (x : xs)
