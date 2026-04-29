{-# LANGUAGE BangPatterns #-}
-- | FlatBuffers binary encoding.
--
-- FlatBuffers stores data with @uoffset_t@ values that are unsigned offsets
-- pointing forward in the buffer (i.e. the referent is at a higher address
-- than the reference site). Vtables are referenced via @soffset_t@, but in
-- practice they are kept at lower addresses than the tables that point at
-- them so decoders that treat @soffset_t@ as unsigned still work.
--
-- We build the buffer in two passes:
--
--   1. 'planValue' walks the value tree, producing 'Chunk's. While planning
--      we deduplicate vtables: tables with identical @(vtable_size,
--      inline_size, field_offsets)@ signatures share the same vtable chunk.
--   2. 'renderRoot' lays out chunks in this final order:
--
--        * the 4-byte root @uoffset_t@,
--        * the optional 4-byte file identifier,
--        * every distinct vtable, in plan order,
--        * the root table followed by all of its dependents in DFS preorder,
--          guaranteeing that every uoffset reference points forward.
--
-- Buffer layout:
--
-- > [root_offset (uoffset_t, 4 bytes)]
-- > [file_identifier (4 bytes, optional)]
-- > [vtable_0] [vtable_1] ...
-- > [root_table] [child_chunks ...]
module FlatBuffers.Encode
  ( encode
  , encodeWithFileIdentifier
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Wireform.Encode.Direct (directEncode)
import qualified FlatBuffers.Value as F

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Encode a FlatBuffers value into a binary buffer (no file identifier).
encode :: F.Value -> ByteString
encode = encodeImpl Nothing
{-# NOINLINE encode #-}

-- | Encode with a 4-byte file identifier (truncated or zero-padded if it
-- isn't exactly four bytes) placed immediately after the root offset.
encodeWithFileIdentifier :: ByteString -> F.Value -> ByteString
encodeWithFileIdentifier !ident !val = encodeImpl (Just (normaliseIdent ident)) val
{-# NOINLINE encodeWithFileIdentifier #-}

normaliseIdent :: ByteString -> ByteString
normaliseIdent bs
  | BS.length bs >= 4 = BS.take 4 bs
  | otherwise = bs <> BS.replicate (4 - BS.length bs) 0
{-# INLINE normaliseIdent #-}

------------------------------------------------------------------------
-- Plan: chunks describe pieces of the final buffer
------------------------------------------------------------------------

data Chunk
  = CScalar  !ByteString
    -- ^ A standalone scalar/struct payload (only used for top-level
    -- non-table roots).
  | CString  !ByteString
    -- ^ Length-prefixed UTF-8 string with a trailing NUL and 4-byte pad.
  | CVector  !Int [PItem]
    -- ^ Length-prefixed homogeneous vector.
  | CTable   !Int !Int [PField] !Int
    -- ^ A table: @vtableId, inlineSize, fields, endPad@.
  | CVTable  [Word16] !Int
    -- ^ A canonical vtable: cell offsets and the inline size of every
    -- table that uses it (i.e. sizeof(soffset) + inline-fields).
    -- The vtable header size on the wire is @4 + 2 * length cells@; any
    -- 4-byte alignment padding is part of the chunk but not part of the
    -- header.
  deriving stock (Show)

-- | Per-field write directive inside a table's inline area.
data PField
  = PInline   !ByteString   -- ^ Bytes written verbatim (scalar / inline struct).
  | PRefChunk !Int          -- ^ Forward uoffset to another chunk.
  deriving stock (Show)

-- | Per-element write directive inside a vector's payload.
data PItem
  = PIInline   !ByteString
  | PIRefChunk !Int
  deriving stock (Show)

data PlanSt = PlanSt
  { psChunks  :: !(IM.IntMap Chunk)        -- ^ Chunks indexed by id.
  , psNext    :: !Int                       -- ^ Next chunk id to allocate.
  , psVtables :: !(Map.Map VTKey Int)       -- ^ Canonical vtable -> chunk id.
  , psVtIds   :: ![Int]                     -- ^ Vtable chunk ids in insertion order.
  }

-- | Canonical vtable signature for deduplication.
type VTKey = (Int, Int, [Word16])

emptyPlan :: PlanSt
emptyPlan = PlanSt IM.empty 0 Map.empty []

addChunk :: Chunk -> PlanSt -> (Int, PlanSt)
addChunk !c !st =
  let !i = psNext st
      !st' = st { psChunks = IM.insert i c (psChunks st), psNext = i + 1 }
  in (i, st')

------------------------------------------------------------------------
-- Plan construction
------------------------------------------------------------------------

encodeImpl :: Maybe ByteString -> F.Value -> ByteString
encodeImpl !mIdent !val = case val of
  F.VTable _ ->
    let (rootId, st) = planValue val emptyPlan
    in renderRoot mIdent rootId st
  other ->
    renderScalar mIdent (scalarPayload other)

planValue :: F.Value -> PlanSt -> (Int, PlanSt)
planValue v !st = case v of
  F.VString t ->
    addChunk (CString (TE.encodeUtf8 t)) st
  F.VVector vs ->
    let (items, st') = foldrV (\x (acc, s) ->
          let (it, s'') = planVectorItem x s
          in (it : acc, s'')) ([], st) vs
    in addChunk (CVector (V.length vs) items) st'
  F.VTable fields ->
    planTable fields st
  _ ->
    addChunk (CScalar (scalarPayload v)) st

foldrV :: (a -> b -> b) -> b -> V.Vector a -> b
foldrV f z v = V.foldr f z v
{-# INLINE foldrV #-}

planVectorItem :: F.Value -> PlanSt -> (PItem, PlanSt)
planVectorItem v !st = case v of
  F.VString _ -> let (cid, st') = planValue v st in (PIRefChunk cid, st')
  F.VVector _ -> let (cid, st') = planValue v st in (PIRefChunk cid, st')
  F.VTable _  -> let (cid, st') = planValue v st in (PIRefChunk cid, st')
  _           -> (PIInline (scalarPayload v), st)

-- | Build a table, deduplicating its vtable.
planTable :: V.Vector (Maybe F.Value) -> PlanSt -> (Int, PlanSt)
planTable !fields !st0 =
  let (rawFields, st1) = V.foldr step ([], st0) fields
      step !mf (acc, s) = case mf of
        Nothing -> (Nothing : acc, s)
        Just v ->
          let (pf, s') = planTableField v s
          in (Just pf : acc, s')
      (sigCells, fieldEntries, !inlineSz) = layoutFields rawFields
      !key = (V.length fields, inlineSz, sigCells)
  in case Map.lookup key (psVtables st1) of
       Just vtId ->
         let (tid, st2) = addChunk
               (CTable vtId inlineSz fieldEntries (padTo4 (4 + inlineSz))) st1
         in (tid, st2)
       Nothing ->
         let (vtId, st2) = addChunk (CVTable sigCells inlineSz) st1
             st3 = st2 { psVtables = Map.insert key vtId (psVtables st2)
                       , psVtIds = psVtIds st2 ++ [vtId] }
             (tid, st4) = addChunk
               (CTable vtId inlineSz fieldEntries (padTo4 (4 + inlineSz))) st3
         in (tid, st4)

planTableField :: F.Value -> PlanSt -> (PField, PlanSt)
planTableField v !st = case v of
  F.VString _ -> let (cid, st') = planValue v st in (PRefChunk cid, st')
  F.VVector _ -> let (cid, st') = planValue v st in (PRefChunk cid, st')
  F.VTable _  -> let (cid, st') = planValue v st in (PRefChunk cid, st')
  _           -> (PInline (scalarPayload v), st)

-- | Compute the vtable cell array, the table's inline writes, and the
-- inline byte size from the field list. Field order is preserved.
layoutFields :: [Maybe PField] -> ([Word16], [PField], Int)
layoutFields fs0 = go fs0 4 [] []
  where
    go [] !curOff cellsR fieldsR =
      (reverse cellsR, reverse fieldsR, curOff - 4)
    go (mf : rest) !curOff cellsR fieldsR =
      case mf of
        Nothing ->
          go rest curOff (0 : cellsR) fieldsR
        Just pf ->
          let !sz = pfieldSize pf
              !cell = fromIntegral curOff :: Word16
          in go rest (curOff + sz) (cell : cellsR) (pf : fieldsR)

pfieldSize :: PField -> Int
pfieldSize (PInline bs) = BS.length bs
pfieldSize (PRefChunk _) = 4
{-# INLINE pfieldSize #-}

------------------------------------------------------------------------
-- Sizing helpers
------------------------------------------------------------------------

padTo4 :: Int -> Int
padTo4 n = (4 - (n `mod` 4)) `mod` 4
{-# INLINE padTo4 #-}

------------------------------------------------------------------------
-- Scalar payload
------------------------------------------------------------------------

-- | Serialise a scalar/struct value as a flat little-endian slab.
-- Strings and vectors here are inline-recursive, used only when the value
-- is a non-table top-level root.
scalarPayload :: F.Value -> ByteString
scalarPayload = \case
  F.VBool b      -> BS.singleton (if b then 1 else 0)
  F.VInt8 n      -> BS.singleton (fromIntegral n)
  F.VInt16 n     -> le16 (fromIntegral n)
  F.VInt32 n     -> le32 (fromIntegral n)
  F.VInt64 n     -> le64 (fromIntegral n)
  F.VWord8 n     -> BS.singleton n
  F.VWord16 n    -> le16 n
  F.VWord32 n    -> le32 n
  F.VWord64 n    -> le64 n
  F.VFloat f     -> le32 (castFloatToWord32 f)
  F.VDouble d    -> le64 (castDoubleToWord64 d)
  F.VString t    ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
        !raw = le32 (fromIntegral len) <> bs <> BS.singleton 0
    in raw <> BS.replicate (padTo4 (BS.length raw)) 0
  F.VVector vs   ->
    let !cnt = V.length vs
        body = V.foldl' (\acc x -> acc <> scalarPayload x) BS.empty vs
    in le32 (fromIntegral cnt) <> body
  F.VStruct vs   -> V.foldl' (\acc x -> acc <> scalarPayload x) BS.empty vs
  F.VTable _     -> BS.replicate 4 0  -- unused (tables route through chunks)

le16 :: Word16 -> ByteString
le16 w = BS.pack [fromIntegral w, fromIntegral (w `div` 0x100)]

le32 :: Word32 -> ByteString
le32 w = BS.pack
  [ fromIntegral (w `div` 0x1)
  , fromIntegral (w `div` 0x100)
  , fromIntegral (w `div` 0x10000)
  , fromIntegral (w `div` 0x1000000)
  ]

le64 :: Word64 -> ByteString
le64 w = BS.pack
  [ fromIntegral (w `div` 0x1)
  , fromIntegral (w `div` 0x100)
  , fromIntegral (w `div` 0x10000)
  , fromIntegral (w `div` 0x1000000)
  , fromIntegral (w `div` 0x100000000)
  , fromIntegral (w `div` 0x10000000000)
  , fromIntegral (w `div` 0x1000000000000)
  , fromIntegral (w `div` 0x100000000000000)
  ]

------------------------------------------------------------------------
-- Layout & emission
------------------------------------------------------------------------

renderScalar :: Maybe ByteString -> ByteString -> ByteString
renderScalar !mIdent !payload =
  let !headerSize = if mIdent == Nothing then 4 else 8
      !rootOff = fromIntegral headerSize :: Word32
      !sz = headerSize + BS.length payload
  in directEncode sz (\p _ -> do
       _ <- writeLE32 p 0 rootOff
       case mIdent of
         Nothing -> pure ()
         Just i  -> writeBytesAt p 4 i
       writeBytesAt p headerSize payload)

renderRoot :: Maybe ByteString -> Int -> PlanSt -> ByteString
renderRoot !mIdent !rootId !st =
  let !headerSize = if mIdent == Nothing then 4 else 8
      !chunks = psChunks st
      -- Emission order: all distinct vtables first (so soffset_t is positive),
      -- then DFS preorder from root for tables/strings/vectors.
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
         Just i  -> writeBytesAt p 4 i
       emitChunks p offMap chunks order offsets)

-- | DFS preorder traversal from the root over non-vtable dependents.
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
  CVector _ items  ->
    4 + sum (map vectorItemSize items)
  CTable _ inlineSz _fs endPad ->
    4 + inlineSz + endPad
  CVTable cells _inlineSz ->
    let !raw = 4 + 2 * length cells
    in raw + padTo4 raw

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
  CScalar bs ->
    writeBytesAt p off bs

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
