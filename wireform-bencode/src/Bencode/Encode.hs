{-# LANGUAGE BangPatterns #-}
-- | Bencode binary encoding using directEncode.
--
-- Per BEP-3 (the BitTorrent metainfo spec) dictionary keys must be
-- emitted /in sorted order, compared as raw byte strings/. This
-- module enforces both. Encoded output is byte-canonical; two
-- callers that hand the same logical dictionary to 'encode' produce
-- the same bytes, which is the property BitTorrent infohashes and
-- many bencode-using protocols rely on.
module Bencode.Encode
  ( encode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)

import Wireform.Encode.Direct (directEncode)
import qualified Bencode.Value as B

-- | Canonicalise a dictionary entry vector: sort by key (byte order
-- as required by BEP-3), then drop later duplicates of the same
-- key. The decoder already returns sorted entries, so the common
-- decode → re-encode round trip is allocation-free at this layer.
{-# INLINE canonDictEntries #-}
canonDictEntries :: V.Vector (ByteString, B.Value)
                 -> V.Vector (ByteString, B.Value)
canonDictEntries kvs
  | V.length kvs <= 1 = kvs
  | otherwise =
      let sorted = sortBy (comparing fst) (V.toList kvs)
      in V.fromList (dedupSorted sorted)
  where
    dedupSorted [] = []
    dedupSorted [x] = [x]
    -- Sort is stable in 'Data.List.sortBy', so the first occurrence
    -- of each key wins on a duplicate input. BEP-3 forbids
    -- duplicates outright but a defensive writer should not crash.
    dedupSorted (x@(k1, _) : (k2, _) : rest) | k1 == k2 = dedupSorted (x : rest)
    dedupSorted (x : rest) = x : dedupSorted rest

encode :: B.Value -> ByteString
encode val =
  let !sz = valueSize val
  in directEncode sz (\p off -> writeValue p off val)

valueSize :: B.Value -> Int
valueSize = \case
  B.BString bs ->
    let !len = BS.length bs
        !digits = intDigits len
    in digits + 1 + len
  B.BInteger n ->
    let !s = show n
    in 1 + length s + 1
  B.BList vs -> 1 + V.foldl' (\acc v -> acc + valueSize v) 0 vs + 1
  B.BDict kvs ->
    let !canonical = canonDictEntries kvs
    in 1
       + V.foldl' (\acc (k, v) ->
           acc + valueSize (B.BString k) + valueSize v) 0 canonical
       + 1

intDigits :: Int -> Int
intDigits n
  | n < 10    = 1
  | n < 100   = 2
  | n < 1000  = 3
  | n < 10000 = 4
  | otherwise = length (show n)

writeValue :: Ptr Word8 -> Int -> B.Value -> IO Int
writeValue p off = \case
  B.BString bs -> do
    let !len = BS.length bs
        !lenBS = BS8.pack (show len)
    off1 <- writeRaw p off lenBS
    off2 <- pokeByte p off1 0x3A -- ':'
    writeRaw p off2 bs

  B.BInteger n -> do
    off1 <- pokeByte p off 0x69 -- 'i'
    let !numBS = BS8.pack (show n)
    off2 <- writeRaw p off1 numBS
    pokeByte p off2 0x65 -- 'e'

  B.BList vs -> do
    off1 <- pokeByte p off 0x6C -- 'l'
    off2 <- V.foldM' (\o v -> writeValue p o v) off1 vs
    pokeByte p off2 0x65 -- 'e'

  B.BDict kvs -> do
    off1 <- pokeByte p off 0x64 -- 'd'
    let !canonical = canonDictEntries kvs
    off2 <- V.foldM' (\o (k, v) -> do
      o1 <- writeValue p o (B.BString k)
      writeValue p o1 v) off1 canonical
    pokeByte p off2 0x65 -- 'e'

pokeByte :: Ptr Word8 -> Int -> Word8 -> IO Int
pokeByte !p !off !b = do
  pokeByteOff p off b
  pure $! off + 1
{-# INLINE pokeByte #-}

writeRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRaw !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRaw #-}
