{-# LANGUAGE BangPatterns #-}


{- | The original list-of-chunks "FlatBuffers.Builder", kept verbatim
(minus comments) as a byte-for-byte oracle: the rewritten builder must
emit exactly the same buffers for the same calls.
-}
module Test.FlatBuffers.Builder.Reference (
  Builder,
  newBuilder,
  finish,
  currentUOff,

  prependBS,
  prependU8,
  prependU16,
  prependU32,
  prependU64,
  prependI16,
  prependI32,
  prependI64,
  prepForObject,
  noteMinAlign,

  Field' (..),
  scalar,
  struct,
  voff,
  writeTable,

  writeString,
  writeVectorOfOffsets,
  writeVectorOfStructs,
  writeVectorInt32,
  writeVectorInt64,

  alignUp,
) where

import Data.Bits (complement, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int16, Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (pokeByteOff)


data Builder = Builder
  { bufPayload :: !(IORef [BS.ByteString])

  , bufSize :: !(IORef Int)
  , bufVTables :: !(IORef (Map.Map VTableKey Int))

  , bufMinAlign :: !(IORef Int)
  }


newtype VTableKey = VTableKey (Int, Int, [Int])
  deriving stock (Eq, Ord)


newBuilder :: IO Builder
newBuilder = Builder <$> newIORef [] <*> newIORef 0 <*> newIORef Map.empty <*> newIORef 1


noteMinAlign :: Builder -> Int -> IO ()
noteMinAlign b a = modifyIORef' (bufMinAlign b) (max a)


prepForObject :: Builder -> Int -> Int -> IO ()
prepForObject b objSize objAlign = do
  noteMinAlign b objAlign
  !cur <- readIORef (bufSize b)
  let !after = cur + objSize
      !pad = (negate after) .&. (objAlign - 1)
  prependBS b (BS.replicate pad 0)


prependBS :: Builder -> BS.ByteString -> IO ()
prependBS b bs = do
  modifyIORef' (bufPayload b) (bs :)
  modifyIORef' (bufSize b) (+ BS.length bs)


prependU8 :: Builder -> Word8 -> IO ()
prependU8 b w = prependBS b (BS.singleton w)


prependU16 :: Builder -> Word16 -> IO ()
prependU16 b w = prependBS b $ BS.pack [fromIntegral w, fromIntegral (w `div` 0x100)]


prependU32 :: Builder -> Word32 -> IO ()
prependU32 b w =
  prependBS b $
    BS.pack
      [ fromIntegral w
      , fromIntegral (w `div` 0x100)
      , fromIntegral (w `div` 0x10000)
      , fromIntegral (w `div` 0x1000000)
      ]


prependU64 :: Builder -> Word64 -> IO ()
prependU64 b w = do
  prependU32 b (fromIntegral (w `div` 0x100000000))
  prependU32 b (fromIntegral w)


prependI16 :: Builder -> Int16 -> IO ()
prependI16 b i = prependU16 b (fromIntegral i)


prependI32 :: Builder -> Int32 -> IO ()
prependI32 b i = prependU32 b (fromIntegral i)


prependI64 :: Builder -> Int64 -> IO ()
prependI64 b i = prependU64 b (fromIntegral i)


finish :: Builder -> Int -> IO ByteString
finish b rootUOff = do
  !minA <- readIORef (bufMinAlign b)
  prepForObject b 4 (max minA 4)
  !curBefore <- readIORef (bufSize b)
  let !rootFromStart = (curBefore + 4) - rootUOff
  prependU32 b (fromIntegral rootFromStart)
  chunks <- readIORef (bufPayload b)
  pure $! BS.concat chunks


currentUOff :: Builder -> IO Int
currentUOff b = readIORef (bufSize b)


data Field' = Field'
  { fsAlign :: !Int
  , fsWrite :: !(Builder -> Int -> IO ())

  }


scalar :: Int -> (Builder -> IO ()) -> Field'
scalar !align writer = Field' align $ \b _ -> writer b


struct :: Int -> Int -> (Builder -> IO ()) -> Field'
struct !size !_align writer = Field' size $ \b _ -> writer b


voff :: Int -> Field'
voff !targetUOff = Field' 4 $ \b _ -> do
  cur <- currentUOff b
  prependU32 b (fromIntegral (cur + 4 - targetUOff))


writeTable :: Builder -> [Maybe Field'] -> IO Int
writeTable b slots = do
  let !present = collectPresent 0 slots
      !nSlots = length slots
      layout !_pos [] = ([], 0)
      layout !pos ((idx, fs) : rest) =
        let !padPos = alignUp pos (fsAlign fs)
            !pos' = padPos + fsAlign fs
            (rs, end) = layout pos' rest
        in ((idx, padPos) : rs, max pos' end)
      (inlineOffs, rawEnd) = layout 4 present
      !maxAlign = foldr (\(_, fs) m -> max m (fsAlign fs)) 4 present
      !tableSize = alignUp rawEnd maxAlign

  prepForObject b tableSize maxAlign

  prependBS b (BS.replicate (tableSize - rawEnd) 0)

  let emit !nextExpect [] = prependBS b (BS.replicate (nextExpect - 4) 0)
      emit !expectedEnd ((idx, fs) : rest) = do
        let !off = case lookup idx inlineOffs of
              Just o -> o
              Nothing -> error "FlatBuffers.Builder: internal error (missing inlineOff for present slot)"
            !fieldEnd = off + fsAlign fs
            !padAfter = expectedEnd - fieldEnd
        prependBS b (BS.replicate padAfter 0)
        fsWrite fs b 0
        emit off rest
  emit rawEnd (reverse present)

  curBeforeSoff <- currentUOff b
  let !tableUOff = curBeforeSoff + 4
      (vtKey, vtBytesCount, vtBytes) = makeVTableBytes inlineOffs tableSize nSlots
  dedup <- readIORef (bufVTables b)
  case Map.lookup vtKey dedup of
    Just existingUOff -> do
      prependI32 b (fromIntegral (existingUOff - tableUOff))
      pure tableUOff
    Nothing -> do
      prependI32 b (fromIntegral vtBytesCount)
      prependBS b vtBytes
      newU <- currentUOff b
      modifyIORef' (bufVTables b) (Map.insert vtKey newU)
      pure tableUOff


collectPresent :: Int -> [Maybe Field'] -> [(Int, Field')]
collectPresent !_ [] = []
collectPresent !i (Nothing : xs) = collectPresent (i + 1) xs
collectPresent !i (Just fs : xs) = (i, fs) : collectPresent (i + 1) xs


makeVTableBytes
  :: [(Int, Int)]
  -> Int
  -> Int
  -> (VTableKey, Int, BS.ByteString)
makeVTableBytes present tableSize nSlots =
  let slotMap = Map.fromList present
      slots = mkSlots 0 slotMap nSlots
      trimmed = reverse (dropWhile (== 0) (reverse slots))
      !nT = length trimmed
      !vtSize = 2 + 2 + 2 * nT
      !bytes = BSI.unsafeCreate vtSize $ \p -> do
        pokeByteOff p 0 (fromIntegral vtSize :: Word8)
        pokeByteOff p 1 (fromIntegral (vtSize `div` 0x100) :: Word8)
        pokeByteOff p 2 (fromIntegral tableSize :: Word8)
        pokeByteOff p 3 (fromIntegral (tableSize `div` 0x100) :: Word8)
        let writeSlot !i (s : ss) = do
              pokeByteOff p (4 + 2 * i) (fromIntegral s :: Word8)
              pokeByteOff p (4 + 2 * i + 1) (fromIntegral (s `div` 0x100) :: Word8)
              writeSlot (i + 1) ss
            writeSlot !_ [] = pure ()
        writeSlot 0 trimmed
      !key = VTableKey (vtSize, tableSize, trimmed)
  in (key, vtSize, bytes)
  where
    mkSlots !i !_ !n | i >= n = []
    mkSlots !i !sm !n = Map.findWithDefault 0 i sm : mkSlots (i + 1) sm n


alignUp :: Int -> Int -> Int
alignUp n a = (n + a - 1) .&. complement (a - 1)


writeString :: Builder -> T.Text -> IO Int
writeString b txt = do
  let !bytes = TE.encodeUtf8 txt
      !n = BS.length bytes
  prepForObject b (4 + n + 1) 4
  prependBS b (BS.snoc bytes 0)
  prependU32 b (fromIntegral n)
  currentUOff b


writeVectorOfOffsets :: Builder -> [Int] -> IO Int
writeVectorOfOffsets b targetUOffs = do
  let !n = length targetUOffs
  prepForObject b (4 + 4 * n) 4
  mapM_
    ( \t -> do
        cur <- currentUOff b
        prependU32 b (fromIntegral (cur + 4 - t))
    )
    (reverse targetUOffs)
  prependU32 b (fromIntegral n)
  currentUOff b


writeVectorOfStructs
  :: Builder
  -> Int
  -> Int
  -> [Builder -> IO ()]
  -> IO Int
writeVectorOfStructs b elemSize elemAlign writers = do
  let !n = length writers
      !totalBytes = 4 + elemSize * n
      !align = max 4 elemAlign
  prepForObject b totalBytes align
  mapM_ (\w -> w b) (reverse writers)
  prependU32 b (fromIntegral n)
  currentUOff b


writeVectorInt32 :: Builder -> [Int32] -> IO Int
writeVectorInt32 b xs = do
  let !n = length xs
  prepForObject b (4 + 4 * n) 4
  mapM_ (prependI32 b) (reverse xs)
  prependU32 b (fromIntegral n)
  currentUOff b


writeVectorInt64 :: Builder -> [Int64] -> IO Int
writeVectorInt64 b xs = do
  let !n = length xs
  prepForVector b (8 * n) 8
  mapM_ (prependI64 b) (reverse xs)
  prependU32 b (fromIntegral n)
  currentUOff b


prepForVector :: Builder -> Int -> Int -> IO ()
prepForVector b dataBytes align = do
  noteMinAlign b align
  !cur <- readIORef (bufSize b)
  let !pad = (negate cur) .&. (align - 1)
  prependBS b (BS.replicate pad 0)
  let _ = dataBytes
  pure ()
