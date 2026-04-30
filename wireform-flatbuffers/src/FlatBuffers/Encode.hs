{-# LANGUAGE BangPatterns #-}
-- | Schema-driven FlatBuffers binary encoder.
--
-- FlatBuffers is a schema-driven format: the wire encoding does not
-- carry per-field type tags, so encoding (and decoding) requires a
-- parsed @.fbs@ schema. This module accepts a 'FlatBuffersSchema' and
-- uses it to:
--
--   * route each 'F.Value' to the right wire encoder for its declared
--     type (so e.g. an @int@ field is always written as a 4-byte
--     little-endian signed integer regardless of which @VInt32@,
--     @VWord32@, or @VInt64@ constructor the user happens to supply),
--   * apply schema-declared default values when a field is absent,
--   * automatically embed the schema's @file_identifier@ if one is
--     declared.
--
-- Internally the encoder uses a two-pass plan/render pipeline:
--
--   1. 'planTable' walks the value tree, producing 'Chunk's for each
--      table, vtable, string, vector, and (inline) struct. Vtables are
--      deduplicated by their @(field_count, inline_size,
--      field_offsets)@ signature, so identical layouts share a single
--      vtable on the wire.
--   2. 'renderRoot' lays out chunks back-to-front in the order
--
--        * 4-byte root @uoffset_t@,
--        * optional 4-byte file identifier,
--        * every distinct vtable (so @soffset_t@s remain positive),
--        * the root table followed by a DFS preorder of its
--          dependents (so all @uoffset_t@s point forward).
module FlatBuffers.Encode
  ( encode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits (shiftR)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import Wireform.Encode.Direct (directEncode)
import qualified FlatBuffers.Value as F
import FlatBuffers.Schema
import FlatBuffers.Internal

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Encode a value against a schema's declared @root_type@. The schema
-- must declare a @root_type@ that names a table.
encode :: FlatBuffersSchema -> F.Value -> Either String ByteString
encode !schema !val = do
  rootDef <- rootTableDef schema
  let !env = mkEnv schema
      !mIdent = schemaFileIdentifier schema
  (rootId, st) <- planTable env rootDef val emptyPlan
  Right (renderRoot mIdent rootId st)
{-# NOINLINE encode #-}

------------------------------------------------------------------------
-- Plan: chunks describe pieces of the final buffer
------------------------------------------------------------------------

data Chunk
  = CString  !ByteString
    -- ^ Length-prefixed UTF-8 string with a trailing NUL and 4-byte pad.
  | CVector  !Int [PItem]
    -- ^ Length-prefixed homogeneous vector.
  | CTable   !Int !Int [PField] !Int
    -- ^ A table: @vtableId, inlineSize, fields, endPad@.
  | CVTable  [Word16] !Int
    -- ^ A canonical vtable: cell offsets and the inline size of every
    -- table that uses it (i.e. @sizeof(soffset) + inline-fields@).
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

------------------------------------------------------------------------
-- Plan construction
------------------------------------------------------------------------

planTable :: Env -> TableDef -> F.Value -> PlanSt -> Either String (Int, PlanSt)
planTable env td val st0 = do
  fieldVals <- alignFields td val
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
-- definition: the value must be a 'F.VTable' with one entry per
-- declared field (use 'Nothing' to mark a field as absent).
alignFields :: TableDef -> F.Value -> Either String [Maybe F.Value]
alignFields td (F.VTable fields)
  | V.length fields == V.length (tdFields td) = Right (V.toList fields)
  | otherwise = Left $ "FlatBuffers.Encode: table " ++ T.unpack (tdName td)
              ++ " expected " ++ show (V.length (tdFields td))
              ++ " fields, got " ++ show (V.length fields)
alignFields td _ =
  Left $ "FlatBuffers.Encode: expected VTable for " ++ T.unpack (tdName td)

-- | Plan one or more vtable cells for each schema field.  Most field
-- types produce a single cell; union fields produce two (a @ubyte@
-- discriminator followed by a @uoffset_t@).
planFields
  :: Env -> [TableField] -> [Maybe F.Value] -> PlanSt
  -> Either String ([Maybe PField], PlanSt)
planFields _ [] [] st = Right ([], st)
planFields env (tf : tfs) (mv : mvs) st = do
  (cells, st1) <- planFieldCells env (tfType tf) mv st
  (rest, st2)  <- planFields env tfs mvs st1
  Right (cells ++ rest, st2)
planFields _ _ _ _ =
  Left "FlatBuffers.Encode: internal: planFields field/value list mismatch"

-- | Produce the vtable cells for a single schema-level field, given an
-- optional user-supplied value.  Returns either one cell (most types)
-- or two (unions: discriminator + offset).
planFieldCells
  :: Env -> FBType -> Maybe F.Value -> PlanSt
  -> Either String ([Maybe PField], PlanSt)
planFieldCells env ty mv st = case (ty, mv) of
  -- Union: two cells. NONE / Nothing → both absent.
  (FTNamed name, Nothing) | isUnionType env name ->
    Right ([Nothing, Nothing], st)
  (FTNamed name, Just (F.VUnion 0 _)) | isUnionType env name ->
    Right ([Nothing, Nothing], st)
  (FTNamed name, Just (F.VUnion disc inner)) | Just udef <- Map.lookup name (envUnions env) -> do
    -- Validate the discriminator against the union's member count.
    let !nMembers = V.length (fudMembers udef)
    if fromIntegral disc < 1 || fromIntegral disc > nMembers
      then Left $ "FlatBuffers.Encode: union " ++ T.unpack name
                ++ " discriminator " ++ show disc
                ++ " out of range [1.." ++ show nMembers ++ "]"
      else do
        let !memberName = fudMembers udef V.! (fromIntegral disc - 1)
        case Map.lookup memberName (envTables env) of
          Just memberTd -> do
            (cid, st') <- planTable env memberTd inner st
            let !discCell = PInline (BS.singleton disc)
                !offCell  = PRefChunk cid
            Right ([Just discCell, Just offCell], st')
          Nothing -> Left $ "FlatBuffers.Encode: union member "
                         ++ T.unpack memberName ++ " is not a declared table"
  (FTNamed name, Just _) | isUnionType env name ->
    Left $ "FlatBuffers.Encode: union field " ++ T.unpack name
        ++ " requires a VUnion value"
  -- Any other field: one cell (possibly absent).
  (_, Nothing) -> Right ([Nothing], st)
  (_, Just v)  -> do
    (pf, st') <- planField env ty v st
    Right ([Just pf], st')

isUnionType :: Env -> Text -> Bool
isUnionType env name = Map.member name (envUnions env)

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
    _ -> Left "FlatBuffers.Encode: expected VString for string field"
  FTVector elemTy -> case v of
    F.VVector vs -> do
      (items, st') <- planVectorItems env elemTy (V.toList vs) st
      let (cid, st'') = addChunk (CVector (V.length vs) items) st'
      Right (PRefChunk cid, st'')
    _ -> Left "FlatBuffers.Encode: expected VVector for vector field"
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      (cid, st') <- planTable env td v st
      Right (PRefChunk cid, st')
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> do
        bs <- encodeStruct env sd v
        Right (PInline bs, st)
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> planField env (fedUnderlyingType ed) v st
        Nothing -> case Map.lookup name (envUnions env) of
          Just _  -> Left $ "FlatBuffers.Encode: union " ++ T.unpack name
                         ++ " can only appear at the table-field level (not in vectors or as a referent)"
          Nothing -> Left $ "FlatBuffers.Encode: unknown named type "
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
    _ -> Left "FlatBuffers.Encode: expected VString in vector<string>"
  FTVector inner -> case v of
    F.VVector inner' -> do
      (items, st') <- planVectorItems env inner (V.toList inner') st
      let (cid, st'') = addChunk (CVector (V.length inner') items) st'
      Right (PIRefChunk cid, st'')
    _ -> Left "FlatBuffers.Encode: expected VVector in vector<vector<...>>"
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
        Nothing -> Left $ "FlatBuffers.Encode: unknown named element type "
                       ++ T.unpack name

-- | Encode a struct value (fixed-size, inline) to a flat byte slab.
encodeStruct :: Env -> FBStructDef -> F.Value -> Either String ByteString
encodeStruct env sd v = case v of
  F.VStruct fields
    | V.length fields == V.length (fsdFields sd) ->
        BS.concat <$> goFields (V.toList fields) (V.toList (fsdFields sd))
    | otherwise -> Left $ "FlatBuffers.Encode: struct " ++ T.unpack (fsdName sd)
                       ++ " expected " ++ show (V.length (fsdFields sd))
                       ++ " fields, got " ++ show (V.length fields)
  _ -> Left $ "FlatBuffers.Encode: expected VStruct for " ++ T.unpack (fsdName sd)
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
      Nothing -> Left $ "FlatBuffers.Encode: " ++ T.unpack name
                     ++ " is not allowed inline in a struct"
  _ -> Left "FlatBuffers.Encode: only scalars/structs/enums are allowed in structs"

------------------------------------------------------------------------
-- Field layout
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
-- Layout & emission
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
  CString bs       ->
    let !len = BS.length bs
        !raw = 4 + len + 1
    in raw + padTo4 raw
  CVector _ items  -> 4 + sum (map vectorItemSize items)
  CTable _ inlineSz _fs endPad -> 4 + inlineSz + endPad
  CVTable cells _  -> let !raw = 4 + 2 * length cells in raw + padTo4 raw

vectorItemSize :: PItem -> Int
vectorItemSize (PIInline bs)  = BS.length bs
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
-- Low-level writers
------------------------------------------------------------------------

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
