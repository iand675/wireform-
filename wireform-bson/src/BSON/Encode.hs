{-# LANGUAGE BangPatterns #-}
-- | BSON binary encoding.
--
-- Encodes a 'BSON.Value.Value' to its BSON wire format using direct
-- buffer writes via 'Proto.Encode.Direct.directEncode'. Integers are
-- little-endian, strings are UTF-8 with null terminators, and documents
-- include a leading 4-byte length prefix.
module BSON.Encode
  ( encode
  , encodeChecked
  , validateKeys
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64)

import Wireform.Encode.Direct (directEncode)
import qualified BSON.Value as B

encode :: B.Value -> ByteString
encode val = case val of
  B.Document fields -> encodeDocument fields
  _ -> encodeDocument (V.singleton (T.pack "0", val))

-- | Like 'encode', but validates that no document or sub-document key
-- contains a NUL byte. The BSON spec defines element keys as
-- @cstring@ — a NUL-terminated UTF-8 byte sequence — so a NUL inside
-- a key would silently truncate the document on parse. Returns
-- @Left@ on the first offending key (along with a path-style hint).
encodeChecked :: B.Value -> Either String ByteString
encodeChecked val = case validateKeys val of
  Left err -> Left err
  Right () -> Right (encode val)

-- | Walk a value tree and reject any document key that contains a
-- NUL byte. Used by 'encodeChecked'; exported so callers can run the
-- check separately.
validateKeys :: B.Value -> Either String ()
validateKeys = goValue ""
  where
    goValue path = \case
      B.Document fs -> goFields path fs
      B.Array vs    ->
        let go i v = goValue (path ++ "[" ++ show i ++ "]") v
        in V.ifoldr (\i v acc -> go i v >> acc) (Right ()) vs
      B.JavaScriptScope _ scope -> goValue (path ++ ".$scope") scope
      _ -> Right ()

    goFields path fs =
      V.foldr
        (\(k, v) acc -> do
           if T.any (== '\0') k
             then Left $
               "BSON.Encode: key " ++ show k
                 ++ " at " ++ (if null path then "<root>" else path)
                 ++ " contains a NUL byte (cstring would truncate)"
             else pure ()
           goValue (path ++ "." ++ T.unpack k) v
           acc)
        (Right ())
        fs

encodeDocument :: V.Vector (T.Text, B.Value) -> ByteString
encodeDocument !fields =
  let !sz = docSize fields
  in directEncode sz (\p off -> writeDocument p off fields)
{-# NOINLINE encodeDocument #-}

docSize :: V.Vector (T.Text, B.Value) -> Int
docSize fields = 4 + V.foldl' (\acc (k, v) -> acc + elementSize k v) 0 fields + 1

elementSize :: T.Text -> B.Value -> Int
elementSize key val = 1 + cstringSize key + valuePayloadSize val

cstringSize :: T.Text -> Int
cstringSize t = BS.length (TE.encodeUtf8 t) + 1

valuePayloadSize :: B.Value -> Int
valuePayloadSize = \case
  B.Double _       -> 8
  B.String t       -> 4 + BS.length (TE.encodeUtf8 t) + 1
  B.Document fs    -> docSize fs
  B.Array vs       -> arrayDocSize vs
  B.Binary _ bs    -> 4 + 1 + BS.length bs
  B.Bool _         -> 1
  B.DateTime _     -> 8
  B.Null           -> 0
  B.Int32 _        -> 4
  B.Int64 _        -> 8
  B.ObjectId _     -> 12
  B.Regex p o      -> cstringSize p + cstringSize o
  B.Decimal128 _   -> 16
  B.MinKey         -> 0
  B.MaxKey         -> 0
  B.JavaScript code -> 4 + BS.length (TE.encodeUtf8 code) + 1
  B.JavaScriptScope code scope ->
    let codeBS = TE.encodeUtf8 code
        scopeSz = docSize (case scope of B.Document fs -> fs; _ -> V.empty)
    in 4 + 4 + BS.length codeBS + 1 + scopeSz
  B.Timestamp _    -> 8
  B.Symbol t       -> 4 + BS.length (TE.encodeUtf8 t) + 1
  B.Undefined      -> 0
  B.DBPointer ns _ -> 4 + BS.length (TE.encodeUtf8 ns) + 1 + 12

arrayDocSize :: V.Vector B.Value -> Int
arrayDocSize vs = 4 + V.ifoldl' (\acc i v -> acc + elementSize (T.pack (show i)) v) 0 vs + 1

writeDocument :: Ptr Word8 -> Int -> V.Vector (T.Text, B.Value) -> IO Int
writeDocument p off fields = do
  let !sz = docSize fields
  off1 <- writeLE32 p off (fromIntegral sz)
  off2 <- V.foldM' (\o (k, v) -> writeElement p o k v) off1 fields
  pokeByteOff p off2 (0x00 :: Word8)
  pure $! off2 + 1

writeElement :: Ptr Word8 -> Int -> T.Text -> B.Value -> IO Int
writeElement p off key val = do
  off1 <- pokeByteOff p off (typeTag val) >> pure (off + 1)
  off2 <- writeCString p off1 key
  writeValuePayload p off2 val

typeTag :: B.Value -> Word8
typeTag = \case
  B.Double _    -> 0x01
  B.String _    -> 0x02
  B.Document _  -> 0x03
  B.Array _     -> 0x04
  B.Binary _ _  -> 0x05
  B.Bool _      -> 0x08
  B.DateTime _  -> 0x09
  B.Null        -> 0x0A
  B.Int32 _     -> 0x10
  B.Int64 _     -> 0x12
  B.ObjectId _  -> 0x07
  B.Regex _ _   -> 0x0B
  B.Undefined   -> 0x06
  B.JavaScript _ -> 0x0D
  B.Symbol _    -> 0x0E
  B.DBPointer _ _ -> 0x0C
  B.JavaScriptScope _ _ -> 0x0F
  B.Timestamp _ -> 0x11
  B.Decimal128 _ -> 0x13
  B.MinKey      -> 0xFF
  B.MaxKey      -> 0x7F

writeCString :: Ptr Word8 -> Int -> T.Text -> IO Int
writeCString p off t = do
  let !bs = TE.encodeUtf8 t
  off1 <- writeRawBytes p off bs
  pokeByteOff p off1 (0x00 :: Word8)
  pure $! off1 + 1

writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}

writeLE32 :: Ptr Word8 -> Int -> Word32 -> IO Int
writeLE32 p off w = do
  pokeByteOff p off w
  pure $! off + 4
{-# INLINE writeLE32 #-}

writeLE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeLE64 p off w = do
  pokeByteOff p off w
  pure $! off + 8
{-# INLINE writeLE64 #-}

writeValuePayload :: Ptr Word8 -> Int -> B.Value -> IO Int
writeValuePayload p off = \case
  B.Double d -> writeLE64 p off (castDoubleToWord64 d)
  B.String t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs + 1
    off1 <- writeLE32 p off (fromIntegral len)
    off2 <- writeRawBytes p off1 bs
    pokeByteOff p off2 (0x00 :: Word8)
    pure $! off2 + 1
  B.Document fields -> writeDocument p off fields
  B.Array vs -> do
    let arrayFields = V.imap (\i v -> (T.pack (show i), v)) vs
    writeDocument p off arrayFields
  B.Binary sub bs -> do
    let !len = BS.length bs
    off1 <- writeLE32 p off (fromIntegral len)
    pokeByteOff p off1 sub
    writeRawBytes p (off1 + 1) bs
  B.Bool b -> do
    pokeByteOff p off (if b then 0x01 else 0x00 :: Word8)
    pure $! off + 1
  B.DateTime ms -> writeLE64 p off (fromIntegral ms)
  B.Null -> pure off
  B.Int32 n -> writeLE32 p off (fromIntegral n)
  B.Int64 n -> writeLE64 p off (fromIntegral n)
  B.ObjectId bs -> writeRawBytes p off (BS.take 12 (bs <> BS.replicate 12 0))
  B.Regex pat opts -> do
    off1 <- writeCString p off pat
    writeCString p off1 opts
  B.Undefined -> pure off
  B.JavaScript code -> do
    let !bs = TE.encodeUtf8 code
        !len = BS.length bs + 1
    off1 <- writeLE32 p off (fromIntegral len)
    off2 <- writeRawBytes p off1 bs
    pokeByteOff p off2 (0x00 :: Word8)
    pure $! off2 + 1
  B.Symbol t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs + 1
    off1 <- writeLE32 p off (fromIntegral len)
    off2 <- writeRawBytes p off1 bs
    pokeByteOff p off2 (0x00 :: Word8)
    pure $! off2 + 1
  B.DBPointer ns oid -> do
    let !nsBs = TE.encodeUtf8 ns
        !len = BS.length nsBs + 1
    off1 <- writeLE32 p off (fromIntegral len)
    off2 <- writeRawBytes p off1 nsBs
    pokeByteOff p off2 (0x00 :: Word8)
    writeRawBytes p (off2 + 1) (BS.take 12 (oid <> BS.replicate 12 0))
  B.JavaScriptScope code scope -> do
    let !codeBS = TE.encodeUtf8 code
        !scopeFields = case scope of B.Document fs -> fs; _ -> V.empty
        !codeSzLen = BS.length codeBS + 1
        !scopeSz = docSize scopeFields
        !totalSz = 4 + codeSzLen + scopeSz
    off1 <- writeLE32 p off (fromIntegral totalSz)
    off2 <- writeLE32 p off1 (fromIntegral codeSzLen)
    off3 <- writeRawBytes p off2 codeBS
    pokeByteOff p off3 (0x00 :: Word8)
    writeDocument p (off3 + 1) scopeFields
  B.Timestamp w -> writeLE64 p off w
  B.Decimal128 bs -> writeRawBytes p off (BS.take 16 (bs <> BS.replicate 16 0))
  B.MinKey -> pure off
  B.MaxKey -> pure off
