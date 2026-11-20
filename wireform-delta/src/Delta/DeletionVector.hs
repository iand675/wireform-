{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

{- | Checked readers for Delta's Roaring deletion vectors. Positions
are zero-based physical row numbers within a single Parquet file.
-}
module Delta.DeletionVector (
  DeletionVector,
  RowSelection (..),
  decodeDeletionVector,
  readDeletionVector,
  DeletionVectorIdentity,
  deletionVectorIdentity,
  sameDeletionVectorDescriptor,
  descriptorCardinality,
  validDeletionVectorFields,
  deletionCardinality,
  lastDeletedRow,
  countLiveRows,
  selectLiveRows,
  selectDeletedRows,
  subtractRowSelection,
) where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Control.Monad.ST (ST)
import Data.Aeson (Value (Null, Number, Object, String), withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific, base10Exponent, coefficient, toBoundedInteger)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word (Word32, Word64, byteSwap32, byteSwap64)
import Delta.Path (localAbsolutePath, safeRelativePath)
import Delta.Z85 (decodeZ85)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Word64#, indexWord8OffAddrAsWord64#)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Ptr (Ptr (..))
import GHC.Word (Word64 (W64#))
import Numeric (showHex)
import Parquet.Page (verifyPageChecksum)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hSeek, withBinaryFile)
import System.IO.Unsafe (unsafeDupablePerformIO)


-- Keep compressed data and its container index. A large run or bitset never
-- expands into an array of deleted row numbers.
data DeletionVector = DeletionVector !ByteString !(V.Vector Container) !Word64 !(Maybe Word64)


data Container = Container
  { blockStart :: !Word64
  , payloadStart :: !Int
  , payloadCount :: !Int
  , encoding :: !Encoding
  , cardinality :: !Int
  }


data Encoding = Array | Bitmap | Runs


data RowSelection
  = AllRows
  | NoRows
  | SelectedRows !(VS.Vector Int)
  deriving stock (Eq, Show)


deletionCardinality :: DeletionVector -> Word64
deletionCardinality (DeletionVector _ _ count _) = count


lastDeletedRow :: DeletionVector -> Maybe Word64
lastDeletedRow (DeletionVector _ _ _ lastRow) = lastRow


{- | Decode raw bitmap bytes, including the Delta magic. Read both portable
vectors and the legacy Spark format used by the protocol's inline example.
-}
decodeDeletionVector :: ByteString -> Either String DeletionVector
decodeDeletionVector bytes = do
  available bytes 0 8
  case u32 bytes 0 of
    1681511376 -> startBuckets (Just u32) (fromIntegral (u32 bytes 4)) 8
    -- The published legacy example uses a big-endian wrapper around
    -- the same little-endian Roaring32 payload.
    0xd0d33964 -> startBuckets (Just u32be) (fromIntegral (u32be bytes 4)) 8
    1681511377 -> do
      available bytes 0 12
      startBuckets Nothing (u64 bytes 4) 12
    _ -> Left "Delta.DeletionVector: unsupported bitmap magic"
  where
    startBuckets :: Maybe (ByteString -> Int -> Word32) -> Word64 -> Int -> Either String DeletionVector
    startBuckets native n off = do
      check (n <= fromIntegral ((BS.length bytes - off) `quot` 12)) "truncated bucket table"
      buckets native (fromIntegral n :: Int) off Nothing [] 0 Nothing
    buckets _ 0 !off _ acc !count lastRow = do
      check (off == BS.length bytes) "trailing bitmap data"
      pure (DeletionVector bytes (V.fromList (reverse acc)) count lastRow)
    buckets native !remaining !off previous acc !count lastRow = do
      available bytes off 4
      let hi = maybe (u32 bytes off) (const (maybe 0 (+ 1) previous)) native
      check (hi <= 0x7fffffff && maybe True (< hi) previous) "invalid bucket key"
      let start = off + 4
          serializedSize = fmap (\readSize -> fromIntegral (readSize bytes off)) native
      mapM_ (available bytes start) serializedSize
      available bytes start 4
      let cookie = u32 bytes start
          hasRunHeader = cookie .&. 0xffff == 12347
      (n, keys) <-
        if hasRunHeader
          then
            let containers = fromIntegral (cookie `shiftR` 16) + 1
            in pure (containers, start + 4 + (containers + 7) `quot` 8)
          else do
            check (cookie == 12346) "invalid Roaring cookie"
            available bytes start 8
            let containers = u32 bytes (start + 4)
            check (containers <= 65536) "too many Roaring containers"
            pure (fromIntegral containers, start + 8)
      let hasOffsets = not hasRunHeader || n >= 4
          offsets = keys + 4 * n
          payload = offsets + if hasOffsets then 4 * n else 0
          hiBase = fromIntegral hi `shiftL` 32
          containers !i !pos !prevKey !items !total !lastPosition
            | i == n = do
                mapM_ (\size -> check (pos - start == size) "legacy bucket size mismatch") serializedSize
                buckets native (remaining - 1) pos (Just hi) items total lastPosition
            | otherwise = do
                let key = u16 bytes (keys + i * 4)
                    card = 1 + u16 bytes (keys + i * 4 + 2)
                    base = hiBase .|. (fromIntegral key `shiftL` 16)
                    run = hasRunHeader && testBit (BSU.unsafeIndex bytes (start + 4 + i `quot` 8)) (i .&. 7)
                check (key > prevKey) "unsorted or duplicate container key"
                when hasOffsets $
                  check (fromIntegral (u32 bytes (offsets + i * 4)) == pos - start) "invalid container offset"
                (kind, values, size, maxLow) <- validateContainer bytes pos card run
                let !entry = Container base (pos + if run then 2 else 0) values kind card
                containers (i + 1) (pos + size) key (entry : items) (total + fromIntegral card) (Just (base + fromIntegral maxLow))
      available bytes start (payload - start)
      containers 0 payload (-1) acc count lastRow


validateContainer :: ByteString -> Int -> Int -> Bool -> Either String (Encoding, Int, Int, Int)
validateContainer bytes off card run
  | run = do
      available bytes off 2
      let n = u16 bytes off
          size = 2 + 4 * n
          loop !i !previous !total
            | i == n = do
                check (total == card) "run cardinality mismatch"
                pure previous
            | otherwise = do
                let p = off + 2 + 4 * i
                    low = u16 bytes p
                    len = 1 + u16 bytes (p + 2)
                    high = low + len - 1
                check (low > previous && high <= 65535) "overlapping, unordered or out-of-range run"
                loop (i + 1) high (total + len)
      available bytes off size
      lastValue <- loop 0 (-1) 0
      pure (Runs, n, size, lastValue)
  | card <= 4096 = do
      let size = 2 * card
          loop !i !previous
            | i == card = pure previous
            | otherwise = do
                let low = u16 bytes (off + 2 * i)
                check (low > previous) "unsorted or duplicate array position"
                loop (i + 1) low
      available bytes off size
      lastValue <- loop 0 (-1)
      pure (Array, card, size, lastValue)
  | otherwise = do
      available bytes off 8192
      lastValue <- validateBitmapWords bytes off card
      pure (Bitmap, 1024, 8192, lastValue)


-- The full 8 KiB container was bounds-checked by the caller. Force the
-- bounded validation loop while the borrowed bytes are alive.
validateBitmapWords :: ByteString -> Int -> Int -> Either String Int
validateBitmapWords (BSI.BS fp _) off card = unsafeDupablePerformIO $
  unsafeWithForeignPtr fp $ \(Ptr address) -> pure $! loop address 0 0 (-1)
  where
    loop address !i !total !lastValue
      | i == 1024 = do
          check (total == card) "bitmap cardinality mismatch"
          pure lastValue
      | otherwise =
          let !word = case off + i * 8 of
                I# offset -> littleEndian (indexWord8OffAddrAsWord64# address offset)
          in if word == 0
               then loop address (i + 1) total lastValue
               else loop address (i + 1) (total + popCount word) (i * 64 + 63 - countLeadingZeros word)


-- | Count the live rows in a physical file interval without making indices.
countLiveRows :: DeletionVector -> Word64 -> Int -> Either String Int
countLiveRows (DeletionVector bytes index _ _) start n
  | n < 0 || fromIntegral n > maxBound - start = Left "Delta.DeletionVector: row interval out of range"
  | n == 0 = Right 0
  | otherwise = Right $! n - countRemovedIn bytes index start stop (firstContainer index start) (containerAtOrAfter index stop)
  where
    stop = start + fromIntegral n


{- | Select row-group-local indices for a physical file interval. Unaffected
and fully deleted groups don't allocate a gather vector.
-}
selectLiveRows :: DeletionVector -> Word64 -> Int -> Either String RowSelection
selectLiveRows = selectRows False
{-# INLINE selectLiveRows #-}


-- | Select deleted rather than live row-group-local positions.
selectDeletedRows :: DeletionVector -> Word64 -> Int -> Either String RowSelection
selectDeletedRows = selectRows True
{-# INLINE selectDeletedRows #-}


selectRows :: Bool -> DeletionVector -> Word64 -> Int -> Either String RowSelection
selectRows deleted (DeletionVector bytes index _ _) start n
  | n < 0 || fromIntegral n > maxBound - start = Left "Delta.DeletionVector: row interval out of range"
  | n == 0 || selected == n = Right AllRows
  | selected == 0 = Right NoRows
  | otherwise = Right (SelectedRows (VS.create fill))
  where
    stop = start + fromIntegral n
    first = firstContainer index start
    limit = containerAtOrAfter index stop
    removed = countRemovedIn bytes index start stop first limit
    selected = if deleted then removed else n - removed

    fill :: ST s (VM.MVector s Int)
    fill = do
      out <- VM.unsafeNew selected
      let writeRange !dst !from !to
            | from >= to = pure dst
            | otherwise = VM.unsafeWrite out dst from >> writeRange (dst + 1) (from + 1) to
          blocks !i !pos !dst
            | i >= limit = do
                unless deleted (writeRange dst pos n >> pure ())
                pure out
            | otherwise = do
                let c = V.unsafeIndex index i
                    base = blockStart c
                    low = blockOffset base start
                    high = blockOffset base stop
                    local = fromIntegral (base + fromIntegral low - start) - low
                    next !p !j = blocks (i + 1) p j
                    emit !p !j !a !b continue = do
                      let a' = max low a
                          b' = min high b
                      if a' >= b'
                        then continue p j
                        else do
                          j' <- if deleted
                            then writeRange j (local + a') (local + b')
                            else writeRange j p (local + a')
                          continue (local + b') j'
                    array !k !p !j
                      | k >= payloadCount c || v >= high = next p j
                      | otherwise = emit p j v (v + 1) (array (k + 1))
                      where
                        v = u16 bytes (payloadStart c + 2 * k)
                    runs !k !p !j
                      | k >= payloadCount c || a >= high = next p j
                      | otherwise = emit p j a (a + 1 + u16 bytes (off + 2)) (runs (k + 1))
                      where
                        off = payloadStart c + 4 * k
                        a = u16 bytes off
                    bitmap !k !p !j
                      | k * 64 >= high = next p j
                      | otherwise = bits k (u64 bytes (payloadStart c + 8 * k)) p j
                    bits !k !word !p !j
                      | word == 0 = bitmap (k + 1) p j
                      | otherwise =
                          let a = countTrailingZeros word
                              b = a + countTrailingZeros (complement (word `shiftR` a))
                              rest = if b >= 64 then 0 else word .&. (maxBound `shiftL` b)
                          in emit p j (k * 64 + a) (k * 64 + b) (bits k rest)
                case encoding c of
                  Array -> array (arrayRank bytes c low) pos dst
                  Runs -> runs (firstIntersectingRun bytes c low) pos dst
                  Bitmap -> bitmap (low `quot` 64) pos dst
      blocks first 0 0
{-# INLINE selectRows #-}


-- | Subtract two sorted selections within the same row group. Partial
-- selections are allocated only up to the size of the first selection.
subtractRowSelection :: Int -> RowSelection -> RowSelection -> RowSelection
subtractRowSelection _ NoRows _ = NoRows
subtractRowSelection _ _ AllRows = NoRows
subtractRowSelection _ left NoRows = left
subtractRowSelection n left (SelectedRows right) =
  let selected = VS.create $ do
        out <- VM.unsafeNew leftCount
        let go !i !j !k
              | i == leftCount = pure (VM.unsafeSlice 0 k out)
              | j < rightCount && VS.unsafeIndex right j < value = go i (j + 1) k
              | j < rightCount && VS.unsafeIndex right j == value = go (i + 1) (j + 1) k
              | otherwise = VM.unsafeWrite out k value >> go (i + 1) j (k + 1)
              where value = leftAt i
        go 0 0 0
  in if VS.null selected then NoRows else SelectedRows selected
  where
    rightCount = VS.length right
    (leftCount, leftAt) = case left of
      AllRows -> (n, id)
      SelectedRows positions -> (VS.length positions, VS.unsafeIndex positions)


firstContainer :: V.Vector Container -> Word64 -> Int
firstContainer index start = containerAtOrAfter index (start .&. complement 65535)
{-# INLINE firstContainer #-}


containerAtOrAfter :: V.Vector Container -> Word64 -> Int
containerAtOrAfter index position = lowerBound (V.length index) (\i -> blockStart (V.unsafeIndex index i) >= position)
{-# INLINE containerAtOrAfter #-}


countRemovedIn :: ByteString -> V.Vector Container -> Word64 -> Word64 -> Int -> Int -> Int
countRemovedIn bytes index start stop first limit = go first 0
  where
    go !i !total
      | i >= limit = total
      | otherwise =
          let c = V.unsafeIndex index i
              low = blockOffset (blockStart c) start
              high = blockOffset (blockStart c) stop
          in go (i + 1) (total + countInRange bytes c low high)


blockOffset :: Word64 -> Word64 -> Int
blockOffset base x
  | x <= base = 0
  | otherwise = fromIntegral (min 65536 (x - base))


countInRange :: ByteString -> Container -> Int -> Int -> Int
countInRange bytes c low high
  | low == 0 && high == 65536 = cardinality c
  | otherwise = case encoding c of
      Array -> arrayRank bytes c high - arrayRank bytes c low
      Runs -> countRuns (firstIntersectingRun bytes c low) 0
      Bitmap -> countWords (low `quot` 64) 0
  where
    off = payloadStart c
    countRuns !i !total
      | i == payloadCount c || start >= high = total
      | otherwise = countRuns (i + 1) (total + max 0 (end - max low start))
      where
        p = off + 4 * i
        start = u16 bytes p
        end = min high (start + 1 + u16 bytes (p + 2))
    -- Visit only the words covered by this row group. Reading two prefix
    -- ranks would reread most of a bitmap split across many row groups.
    countWords !i !total
      | start >= high = total
      | otherwise =
          let validFrom = max 0 (low - start)
              validUntil = min 64 (high - start)
              mask = (maxBound `shiftL` validFrom) .&. (maxBound `shiftR` (64 - validUntil))
              word = u64 bytes (off + 8 * i)
          in countWords (i + 1) (total + popCount (word .&. mask))
      where
        start = i * 64


arrayRank :: ByteString -> Container -> Int -> Int
arrayRank bytes c end =
  lowerBound (payloadCount c) (\i -> u16 bytes (payloadStart c + i * 2) >= end)
{-# INLINE arrayRank #-}


-- Sorted, disjoint runs also have sorted ends. Seek by end so a run
-- starting before the row group can still delete rows inside it.
firstIntersectingRun :: ByteString -> Container -> Int -> Int
firstIntersectingRun bytes c low
  | low == 0 = 0
  | otherwise = lowerBound (payloadCount c) reachesStart
  where
    reachesStart i =
      let off = payloadStart c + 4 * i
      in u16 bytes off + u16 bytes (off + 2) >= low
{-# INLINE firstIntersectingRun #-}


lowerBound :: Int -> (Int -> Bool) -> Int
lowerBound size accept = go 0 size
  where
    go !lo !hi
      | lo == hi = lo
      | accept mid = go lo mid
      | otherwise = go (mid + 1) hi
      where
        mid = lo + (hi - lo) `quot` 2
{-# INLINE lowerBound #-}


data Descriptor = Descriptor !Text !Text !(Maybe Int32) !Int32 !Int64
  deriving (Eq)


-- Size and cardinality describe the bitmap, while these three fields
-- identify it. An absent offset and an explicit zero are distinct IDs.
data DeletionVectorIdentity = DeletionVectorIdentity !Text !Text !(Maybe Int32)
  deriving (Eq, Ord)


deletionVectorIdentity :: Value -> Either String DeletionVectorIdentity
deletionVectorIdentity value = do
  Descriptor storage payload offset _ _ <- parseDescriptor value
  pure (DeletionVectorIdentity storage payload offset)


-- | Compare the validated descriptor fields used by Delta. Checkpoint
-- decoding can omit extra JSON properties and null offsets.
sameDeletionVectorDescriptor :: Maybe Value -> Maybe Value -> Bool
sameDeletionVectorDescriptor Nothing Nothing = True
sameDeletionVectorDescriptor (Just left) (Just right) =
  case (parseDescriptor left, parseDescriptor right) of
    (Right actual, Right expected) -> actual == expected
    _ -> False
sameDeletionVectorDescriptor _ _ = False


descriptorCardinality :: Value -> Either String Word64
descriptorCardinality value = do
  Descriptor _ _ _ _ count <- parseDescriptor value
  pure (fromIntegral count)


parseDescriptor :: Value -> Either String Descriptor
parseDescriptor (Object fields)
  | Just (String storage) <- KM.lookup "storageType" fields
  , Just (String payload) <- KM.lookup "pathOrInlineDv" fields
  , Just offset <- parseOffset (KM.lookup "offset" fields)
  , Just (Number rawSize) <- KM.lookup "sizeInBytes" fields
  , Just size <- descriptorInteger rawSize
  , Just (Number rawCount) <- KM.lookup "cardinality" fields
  , Just count <- descriptorInteger rawCount
  , validDeletionVectorFields storage (T.null payload) offset size count =
      Right (Descriptor storage payload offset size count)
  where
    parseOffset Nothing = Just Nothing
    parseOffset (Just Null) = Just Nothing
    parseOffset (Just (Number raw)) = Just <$> descriptorInteger raw
    parseOffset _ = Nothing
parseDescriptor value = parseInvalidDescriptor value


-- The JSON log and Parquet checkpoint normally supply integral coefficients.
-- Preserve integer-valued decimals and exponents accepted by the JSON reader.
descriptorInteger :: forall a. (Bounded a, Integral a) => Scientific -> Maybe a
descriptorInteger raw
  | base10Exponent raw /= 0 = toBoundedInteger raw
  | n >= 0 && n <= toInteger (maxBound :: a) = Just (fromInteger n)
  | otherwise = Nothing
  where
    n = coefficient raw
{-# INLINE descriptorInteger #-}


-- Use Aeson's field paths when a descriptor cannot be read directly.
parseInvalidDescriptor :: Value -> Either String Descriptor
parseInvalidDescriptor value = do
  descriptor@(Descriptor storage payload offset size count) <-
    parseEither (withObject "Delta deletion vector" $ \o -> Descriptor
      <$> o .: "storageType"
      <*> o .: "pathOrInlineDv"
      <*> o .:? "offset"
      <*> o .: "sizeInBytes"
      <*> o .: "cardinality") value
  check (validDeletionVectorFields storage (T.null payload) offset size count) "invalid deletion vector descriptor"
  pure descriptor
{-# NOINLINE parseInvalidDescriptor #-}


-- Spark has a zero-byte inline representation of an empty bitmap.
-- Keep its descriptor in replay, including its distinct logical identity.
validDeletionVectorFields :: (Eq t, IsString t) => t -> Bool -> Maybe Int32 -> Int32 -> Int64 -> Bool
validDeletionVectorFields storage emptyPayload offset size count
  | storage == "i" =
      offset == Nothing && (if emptyPayload then size == 0 && count == 0 else size >= 8 && count >= 0)
  | storage == "p" || storage == "u" =
      not emptyPayload && size >= 8 && count >= 0 && maybe True (>= 0) offset
  | otherwise = False
{-# INLINE validDeletionVectorFields #-}


{- | Load a descriptor from an add action. Local files are read by range,
with the declared size, file version, CRC-32 and cardinality checked.
-}
readDeletionVector :: FilePath -> Value -> IO (Either String DeletionVector)
readDeletionVector root value = case parseDescriptor value of
  Left err -> pure (Left err)
  Right (Descriptor _ _ _ 0 0) -> pure (Right (DeletionVector BS.empty V.empty 0 Nothing))
  Right (Descriptor storage payload offset size count) -> do
    let sizeBytes = fromIntegral size
    bytes <- case storage of
      "i" -> pure $ do
        let encoded = TE.encodeUtf8 payload
            padded = (sizeBytes + 3) `quot` 4 * 4
        check (BS.length encoded == padded `quot` 4 * 5) "inline size mismatch"
        decoded <- decodeZ85 encoded
        check (BS.all (== 0) (BS.drop sizeBytes decoded)) "invalid inline padding"
        pure (BS.take sizeBytes decoded)
      _ -> case vectorPath storage payload of
        Left err -> pure (Left err)
        -- With no offset, use the first record after the version byte.
        -- A supplied offset is an absolute byte position in the file.
        Right path -> readStored path (maybe 1 toInteger offset) sizeBytes
    pure $ do
      vector <- bytes >>= decodeDeletionVector
      check (deletionCardinality vector == fromIntegral count) "descriptor cardinality mismatch"
      pure vector
  where
    vectorPath "u" payload = do
      let (prefix, uuidText) = T.splitAt (T.length payload - 20) payload
      check (T.length uuidText == 20) "invalid UUID length"
      uuid <- uuidFileName <$> decodeZ85 (TE.encodeUtf8 uuidText)
      dir <- if T.null prefix then Right root else (root </>) <$> safeRelativePath prefix
      pure (dir </> uuid)
    vectorPath "p" payload = localAbsolutePath payload
    vectorPath _ _ = Left "Delta.DeletionVector: unknown storage type"


readStored :: FilePath -> Integer -> Int -> IO (Either String ByteString)
readStored _ 0 _ = pure (Left "Delta.DeletionVector: stored offset points at the file version")
readStored path off size = do
  result <- try @IOException $ withBinaryFile path ReadMode $ \handle -> do
    fileSize <- hFileSize handle
    if off + toInteger size + 8 > fileSize
      then pure (Left "Delta.DeletionVector: truncated stored bitmap")
      else do
        version <- BS.hGet handle 1
        if version /= BS.singleton 1
          then pure (Left "Delta.DeletionVector: unsupported file version")
          else do
            hSeek handle AbsoluteSeek off
            lengthBytes <- BS.hGet handle 4
            if BS.length lengthBytes /= 4 || u32be lengthBytes 0 /= fromIntegral size
              then pure (Left "Delta.DeletionVector: stored size mismatch")
              else do
                bitmap <- BS.hGet handle size
                crc <- BS.hGet handle 4
                pure $ do
                  check (BS.length bitmap == size && BS.length crc == 4) "truncated stored bitmap"
                  case verifyPageChecksum (u32be crc 0) bitmap of
                    Left _ -> Left "Delta.DeletionVector: CRC-32 checksum mismatch"
                    Right () -> pure bitmap
  pure (either (Left . show) id result)


uuidFileName :: ByteString -> FilePath
uuidFileName bytes = "deletion_vector_" ++ concatMap hexAt [0 .. 15] ++ ".bin"
  where
    hexAt i =
      let b = BSU.unsafeIndex bytes i
          hex = if b < 16 then '0' : showHex b "" else showHex b ""
      in if i == 4 || i == 6 || i == 8 || i == 10 then '-' : hex else hex


check :: Bool -> String -> Either String ()
check ok why = unless ok (Left ("Delta.DeletionVector: " ++ why))


available :: ByteString -> Int -> Int -> Either String ()
available bytes off size = check (off <= BS.length bytes && size <= BS.length bytes - off) "truncated bitmap"


u16 :: ByteString -> Int -> Int
u16 bytes off =
  fromIntegral (BSU.unsafeIndex bytes off) .|. (fromIntegral (BSU.unsafeIndex bytes (off + 1)) `shiftL` 8)
{-# INLINE u16 #-}


u32 :: ByteString -> Int -> Word32
u32 bytes off =
  fromIntegral (u16 bytes off) .|. (fromIntegral (u16 bytes (off + 2)) `shiftL` 16)
{-# INLINE u32 #-}


u64 :: ByteString -> Int -> Word64
u64 bytes off =
  fromIntegral (u32 bytes off) .|. (fromIntegral (u32 bytes (off + 4)) `shiftL` 32)
{-# INLINE u64 #-}


littleEndian :: Word64# -> Word64
littleEndian raw = case targetByteOrder of
  LittleEndian -> W64# raw
  BigEndian -> byteSwap64 (W64# raw)
{-# INLINE littleEndian #-}


u32be :: ByteString -> Int -> Word32
u32be bytes off = byteSwap32 (u32 bytes off)
