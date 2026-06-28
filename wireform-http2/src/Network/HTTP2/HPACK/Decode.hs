module Network.HTTP2.HPACK.Decode
  ( decodeHeader
  , decodeHeaderBlock
  , decodeHeaderBlockWithMaxSize
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import System.IO.Unsafe (unsafePerformIO)

import Network.HTTP2.HPACK.Huffman
import Network.HTTP2.HPACK.Table
import Network.HTTP2.HPACK.Types
import Network.HTTP2.Types (Header)


decodeHeaderBlock :: DynamicTable -> ByteString -> IO (Either DecodeError [Header])
decodeHeaderBlock dt bs = decodeHeaderBlockWithMaxSize dt 4096 bs

decodeHeaderBlockWithMaxSize :: DynamicTable -> Int -> ByteString -> IO (Either DecodeError [Header])
decodeHeaderBlockWithMaxSize dt maxTableSize bs = go 0 [] True
  where
    len = BS.length bs

    go !off !acc !allowTableUpdate
      | off >= len = pure (Right (reverse acc))
      | otherwise = do
          let b = BS.index bs off
          if b .&. 0xE0 == 0x20
            then
              if allowTableUpdate
                then do
                  result <- decodeDynamicTableUpdate dt maxTableSize bs off
                  case result of
                    Left err -> pure (Left err)
                    Right (Nothing, off') -> go off' acc True
                    Right (Just _, _) -> pure (Left (InvalidTableSizeUpdate 0))
                else pure (Left (InvalidTableSizeUpdate 0))
            else do
              result <- decodeHeader dt bs off
              case result of
                Left err -> pure (Left err)
                Right (Nothing, off') -> go off' acc False
                Right (Just hdr, off') -> go off' (hdr : acc) False

decodeHeader :: DynamicTable -> ByteString -> Int
             -> IO (Either DecodeError (Maybe Header, Int))
decodeHeader dt bs off
  | off >= BS.length bs = pure (Left HeaderBlockTruncated)
  | otherwise = do
      let b = BS.index bs off
      if testBit b 7
        then decodeIndexed dt bs off
        else if testBit b 6
          then decodeLiteralIncremental dt bs off
          else if b .&. 0xF0 == 0
            then decodeLiteralNoIndex dt bs off
            else if b .&. 0xF0 == 0x10
              then decodeLiteralNeverIndex dt bs off
              else decodeDynamicTableUpdate dt 4096 bs off

decodeIndexed :: DynamicTable -> ByteString -> Int
              -> IO (Either DecodeError (Maybe Header, Int))
decodeIndexed dt bs off =
  case decodeInteger bs off 7 of
    Left err -> pure (Left err)
    Right (idx, off') -> do
      mEntry <- lookupEntry dt (fromIntegral idx)
      case mEntry of
        Nothing -> pure (Left (IndexOutOfRange (fromIntegral idx)))
        Just hdr -> pure (Right (Just hdr, off'))

decodeLiteralIncremental :: DynamicTable -> ByteString -> Int
                         -> IO (Either DecodeError (Maybe Header, Int))
decodeLiteralIncremental dt bs off =
  case decodeInteger bs off 6 of
    Left err -> pure (Left err)
    Right (idx, off') -> do
      nameResult <- if idx == 0
        then pure (case decodeString bs off' of
          Left err -> Left err
          Right (name, off'') -> Right (internName name, off''))
        else do
          mEntry <- lookupEntry dt (fromIntegral idx)
          case mEntry of
            Nothing -> pure (Left (IndexOutOfRange (fromIntegral idx)))
            Just (name, _) -> pure (Right (name, off'))
      case nameResult of
        Left err -> pure (Left err)
        Right (name, off'') ->
          case decodeString bs off'' of
            Left err -> pure (Left err)
            Right (value, off''') -> do
              let hdr = (name, value)
              insertEntry dt hdr
              pure (Right (Just hdr, off'''))

decodeLiteralNoIndex :: DynamicTable -> ByteString -> Int
                     -> IO (Either DecodeError (Maybe Header, Int))
decodeLiteralNoIndex dt bs off =
  decodeLiteralCommon dt bs off 4

decodeLiteralNeverIndex :: DynamicTable -> ByteString -> Int
                        -> IO (Either DecodeError (Maybe Header, Int))
decodeLiteralNeverIndex dt bs off =
  decodeLiteralCommon dt bs off 4

decodeLiteralCommon :: DynamicTable -> ByteString -> Int -> Int
                    -> IO (Either DecodeError (Maybe Header, Int))
decodeLiteralCommon dt bs off prefix =
  case decodeInteger bs off prefix of
    Left err -> pure (Left err)
    Right (idx, off') -> do
      nameResult <- if idx == 0
        then pure (case decodeString bs off' of
          Left err -> Left err
          Right (name, off'') -> Right (internName name, off''))
        else do
          mEntry <- lookupEntry dt (fromIntegral idx)
          case mEntry of
            Nothing -> pure (Left (IndexOutOfRange (fromIntegral idx)))
            Just (name, _) -> pure (Right (name, off'))
      case nameResult of
        Left err -> pure (Left err)
        Right (name, off'') ->
          case decodeString bs off'' of
            Left err -> pure (Left err)
            Right (value, off''') -> pure (Right (Just (name, value), off'''))

decodeDynamicTableUpdate :: DynamicTable -> Int -> ByteString -> Int
                         -> IO (Either DecodeError (Maybe Header, Int))
decodeDynamicTableUpdate dt maxSize bs off =
  case decodeInteger bs off 5 of
    Left err -> pure (Left err)
    Right (newSize, off') -> do
      if fromIntegral newSize > maxSize
        then pure (Left (InvalidTableSizeUpdate (fromIntegral newSize)))
        else do
          setMaxSize dt (fromIntegral newSize)
          pure (Right (Nothing, off'))

decodeInteger :: ByteString -> Int -> Int -> Either DecodeError (Word64, Int)
decodeInteger bs off n
  | off >= BS.length bs = Left HeaderBlockTruncated
  | otherwise =
      let mask = (1 `unsafeShiftL` n) - 1 :: Word8
          prefix = BS.index bs off .&. mask
      in if prefix < mask
           then Right (fromIntegral prefix, off + 1)
           else decodeContinuation bs (off + 1) (fromIntegral prefix) 0

decodeContinuation :: ByteString -> Int -> Word64 -> Int -> Either DecodeError (Word64, Int)
decodeContinuation bs off value shift
  | off >= BS.length bs = Left HeaderBlockTruncated
  | shift > 56 = Left IntegerOverflow
  | otherwise =
      let b = BS.index bs off
          contrib = fromIntegral (b .&. 0x7F) `unsafeShiftL` shift :: Word64
          value' = value + contrib
      in if testBit b 7
           then decodeContinuation bs (off + 1) value' (shift + 7)
           else Right (value', off + 1)

decodeString :: ByteString -> Int -> Either DecodeError (ByteString, Int)
decodeString bs off
  | off >= BS.length bs = Left HeaderBlockTruncated
  | otherwise =
      let huffmanFlag = testBit (BS.index bs off) 7
      in case decodeInteger bs off 7 of
           Left err -> Left err
           Right (strLen, off') ->
             let end = off' + fromIntegral strLen
             in if end > BS.length bs
                  then Left HeaderBlockTruncated
                  else let raw = BS.take (fromIntegral strLen) (BS.drop off' bs)
                       in if huffmanFlag
                            then case huffmanDecode raw of
                                   Left err -> Left err
                                   -- 'huffmanDecode' wraps its memcpy
                                   -- in a lazy 'unsafePerformIO'.  Our
                                   -- @raw@ is a slice of @bs@ which is
                                   -- a slice of the recv ring buffer's
                                   -- per-frame @copied@ block; once the
                                   -- enclosing case branch ends GC may
                                   -- reclaim that block, and a deferred
                                   -- memcpy then reads recycled bytes.
                                   -- Force the IO now by demanding the
                                   -- result's WHNF.
                                   Right decoded -> decoded `seq` Right (decoded, end)
                            else
                              -- Same hazard for literal (non-Huffman)
                              -- strings — they're slices of @bs@.
                              -- 'forceCopy' produces a strict copy
                              -- whose backing memory is independent
                              -- of the source.
                              Right (forceCopy raw, end)

-- | Strict, eager copy of a ByteString.  Allocates a fresh buffer and
-- runs memcpy synchronously inside 'unsafePerformIO', so the returned
-- bytestring's memory is independent of the source.  Used to detach
-- HPACK literal slices from the recv ring buffer they came from
-- (otherwise the next 'recvBufferRead' overwrites their bytes).
-- 'unsafePerformIO' (not 'unsafeDupablePerformIO'): we don't want the
-- memcpy duplicated under multi-threaded forcing.  Walks every byte
-- in the destination after memcpy to make sure the IO has actually
-- run (and therefore read from the source) before we return — a bare
-- 'pure ps' lets GHC defer evaluation of the result, which would
-- defer the memcpy itself.
forceCopy :: ByteString -> ByteString
forceCopy (BSI.PS srcFp srcOff srcLen) = unsafePerformIO $ do
  dstFp <- BSI.mallocByteString srcLen
  withForeignPtr srcFp $ \src ->
    withForeignPtr dstFp $ \dst ->
      BSI.memcpy dst (src `plusPtr` srcOff) srcLen
  let !ps = BSI.PS dstFp 0 srcLen
  BS.foldr' (\b acc -> b `seq` acc) () ps `seq` pure ps

