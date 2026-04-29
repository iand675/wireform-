{-# LANGUAGE BangPatterns #-}
-- | FlatBuffers binary decoding.
--
-- Decodes a FlatBuffers binary buffer into a 'FlatBuffers.Value.Value'.
-- FlatBuffers is a schema-driven format, so a fully type-accurate decode
-- is impossible without the schema (a 4-byte field could be 'VInt32',
-- 'VWord32', 'VFloat', or a @uoffset_t@). This decoder recovers the
-- structural shape — nested tables, strings, vectors, and per-field
-- scalar widths — and falls back to unsigned-integer constructors for
-- raw scalar bytes.
--
-- Algorithm:
--
--   1. Read the 4-byte @uoffset_t@ at offset 0 to find the root.
--   2. At a table position, read the @soffset_t@ to find the vtable.
--   3. Walk the vtable cells: each cell either says \"absent\" (0) or
--      gives the field's offset from the start of the table. Adjacent
--      non-zero cells (taken in cell order) imply the field width.
--   4. For width-4 fields, probe whether the value is a plausible
--      forward @uoffset_t@ pointing at a string, vector, or table; if
--      so, follow it. Otherwise treat it as 'VWord32'.
--
-- Buffers produced by 'FlatBuffers.Encode.encode' (with our DFS layout
-- and vtable dedup) round-trip through this decoder up to the
-- well-known scalar-width ambiguity.
module FlatBuffers.Decode
  ( decode
  , fileIdentifier
  , hasFileIdentifier
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32)
import Data.List (sort)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified FlatBuffers.Value as F

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

decode :: ByteString -> Either String F.Value
decode !bs
  | BS.length bs < 4 = Left "FlatBuffers.Decode: input too short"
  | otherwise =
      let !rootOff = fromIntegral (readLE32 bs 0) :: Int
      in if rootOff <= 0 || rootOff >= BS.length bs
           then Left "FlatBuffers.Decode: invalid root offset"
           else case decodeTable bs rootOff of
             Right v  -> Right v
             Left _  ->
               -- Buffers produced for non-table top-level values place
               -- their payload directly at @rootOff@. Fall back to
               -- reading a single byte there (the historical behaviour
               -- of this decoder).
               if rootOff < BS.length bs
                 then Right (F.VWord8 (rdByte bs rootOff))
                 else Left "FlatBuffers.Decode: no root payload"

-- | Read the 4-byte file identifier embedded immediately after the root
-- offset, if any. This always reads bytes 4..7 of the buffer; callers
-- decide whether the buffer was written with a file identifier.
fileIdentifier :: ByteString -> Maybe ByteString
fileIdentifier !bs
  | BS.length bs < 8 = Nothing
  | otherwise = Just (BS.take 4 (BS.drop 4 bs))

-- | @hasFileIdentifier expected bs@ is 'True' iff @bs@ has at least 8
-- bytes and bytes 4..7 match @expected@ (which must be at most 4 bytes).
hasFileIdentifier :: ByteString -> ByteString -> Bool
hasFileIdentifier !expected !bs
  | BS.length bs < 8 = False
  | otherwise =
      let !got = BS.take 4 (BS.drop 4 bs)
          !want = if BS.length expected >= 4
                    then BS.take 4 expected
                    else expected <> BS.replicate (4 - BS.length expected) 0
      in got == want

------------------------------------------------------------------------
-- Low-level reads
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

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off < 0 || off + n > BS.length bs =
      Left "FlatBuffers.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

------------------------------------------------------------------------
-- Table decoding
------------------------------------------------------------------------

-- | Decode a table at @tableOff@, following its vtable to determine
-- field widths and follow uoffset references to nested chunks.
decodeTable :: ByteString -> Int -> Either String F.Value
decodeTable bs !tableOff = do
  ensure bs tableOff 4
  let !soff = fromIntegral (readLE32S bs tableOff) :: Int
      !vtOff = tableOff - soff
  ensure bs vtOff 4
  let !vtSize  = fromIntegral (readLE16 bs vtOff)       :: Int
      !inlineSz = fromIntegral (readLE16 bs (vtOff + 2)) :: Int
  if vtSize < 4 || inlineSz < 4
    then Left "FlatBuffers.Decode: malformed vtable header"
    else do
      let !nFields = (vtSize - 4) `div` 2
      ensure bs (vtOff + 4) (nFields * 2)
      cells <- traverseN nFields $ \i ->
        Right (fromIntegral (readLE16 bs (vtOff + 4 + i * 2)) :: Int)
      -- Compute per-field byte width by walking cells in cell order and
      -- looking at the next non-zero cell (or the end of the inline area).
      let !endOfInline = inlineSz   -- includes the soffset's 4 bytes
          fieldWidths = computeWidths cells endOfInline
      fields <- traverseN nFields $ \i ->
        let !cell = cells !! i
            !fieldWidth = fieldWidths !! i
        in if cell == 0
             then Right Nothing
             else do
               let !addr = tableOff + cell
               v <- decodeInlineField bs addr fieldWidth
               Right (Just v)
      Right (F.VTable (V.fromList fields))

-- | For each field cell, the field's inline width is the difference
-- between this cell and the next greater cell (in cell-order). For the
-- last field the upper bound is the end of the table's inline area.
-- Cell value 0 means \"absent\" — we report width 0 (the result is unused).
computeWidths :: [Int] -> Int -> [Int]
computeWidths cells endOfInline =
  let nonZeroSorted = sort [c | c <- cells, c /= 0]
      nextHigher c =
        case dropWhile (<= c) nonZeroSorted of
          (h : _) -> h
          []      -> endOfInline
  in map (\c -> if c == 0 then 0 else nextHigher c - c) cells

decodeInlineField :: ByteString -> Int -> Int -> Either String F.Value
decodeInlineField bs !addr !width = case width of
  1 -> do ensure bs addr 1; Right (F.VWord8 (rdByte bs addr))
  2 -> do ensure bs addr 2; Right (F.VWord16 (readLE16 bs addr))
  4 -> decodeWidth4 bs addr
  8 -> do ensure bs addr 8; Right (F.VWord64 (readLE64 bs addr))
  _ ->
    -- Unusual width — treat as an inline struct of bytes.
    if width <= 0
      then Left "FlatBuffers.Decode: zero-width field"
      else do
        ensure bs addr width
        let !raw = BSU.unsafeTake width (BSU.unsafeDrop addr bs)
        Right (F.VStruct (V.fromList (map F.VWord8 (BS.unpack raw))))

-- | Width-4 fields can be plain scalars or @uoffset_t@ references.  We
-- probe the value: if it's a forward, in-bounds offset whose target
-- looks like a string, vector, or table, follow it. Otherwise treat it
-- as 'VWord32'.
decodeWidth4 :: ByteString -> Int -> Either String F.Value
decodeWidth4 bs !addr = do
  ensure bs addr 4
  let !raw = readLE32 bs addr
      !uoff = fromIntegral raw :: Int
      !target = addr + uoff
      !len = BS.length bs
      forwardInBounds = uoff > 0 && target < len && target >= addr + 4
  if not forwardInBounds
    then Right (F.VWord32 raw)
    else
      case probeReferent bs target of
        Just v  -> Right v
        Nothing -> Right (F.VWord32 raw)

------------------------------------------------------------------------
-- Probing & nested decoders
------------------------------------------------------------------------

-- | Try to interpret @target@ as a string, then a table, then a
-- byte-vector.  FlatBuffers @vector<T>@ and @string@ share the same
-- length-prefixed shape; without a schema the only reliable way to
-- distinguish them is the trailing NUL terminator that strings carry.
probeReferent :: ByteString -> Int -> Maybe F.Value
probeReferent bs !target =
  case probeString bs target of
    Just s  -> Just s
    Nothing -> case probeTable bs target of
      Just t  -> Just t
      Nothing -> probeByteVector bs target

-- | Last-resort vector probe: if the target looks like a length-prefix
-- followed by exactly that many bytes (or that many bytes plus
-- alignment padding), decode as a byte vector. We only use this after
-- string and table probes have already failed.
probeByteVector :: ByteString -> Int -> Maybe F.Value
probeByteVector bs !pos
  | pos + 4 > BS.length bs = Nothing
  | otherwise =
      let !cnt = fromIntegral (readLE32 bs pos) :: Int
          !len = BS.length bs
          !endByte = pos + 4 + cnt
      in if cnt < 0 || endByte > len
           then Nothing
           else
             let !raw = BSU.unsafeTake cnt (BSU.unsafeDrop (pos + 4) bs)
             in Just (F.VVector (V.fromList (map F.VWord8 (BS.unpack raw))))

probeString :: ByteString -> Int -> Maybe F.Value
probeString bs !pos
  | pos + 4 > BS.length bs = Nothing
  | otherwise =
      let !len = fromIntegral (readLE32 bs pos) :: Int
          !endByte = pos + 4 + len
      in if len < 0 || endByte >= BS.length bs
           then Nothing
           else
             -- Validate UTF-8 + trailing NUL.
             if rdByte bs endByte /= 0x00
               then Nothing
               else
                 let !raw = BSU.unsafeTake len (BSU.unsafeDrop (pos + 4) bs)
                 in case TE.decodeUtf8' raw of
                      Right t -> Just (F.VString t)
                      Left _  -> Nothing

probeTable :: ByteString -> Int -> Maybe F.Value
probeTable bs !pos =
  case decodeTable bs pos of
    Right t@(F.VTable _) -> Just t
    _                    -> Nothing

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Signed 32-bit read for soffset_t.
readLE32S :: ByteString -> Int -> Int32
readLE32S bs off = fromIntegral (readLE32 bs off)
{-# INLINE readLE32S #-}

-- | @traverseN n f@ runs @f@ over @[0..n-1]@, collecting results in order.
traverseN :: Int -> (Int -> Either String a) -> Either String [a]
traverseN n f = go 0
  where
    go !i
      | i >= n = Right []
      | otherwise = do
          x <- f i
          xs <- go (i + 1)
          Right (x : xs)
