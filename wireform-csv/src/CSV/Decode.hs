{-# LANGUAGE BangPatterns #-}
-- | CSV/TSV parser with SIMD-accelerated scanning.
--
-- Implements RFC 4180 with configurable delimiter, quote, and escape characters.
-- Uses @hs_xml_find_byte@ from @cbits\/fast_xml.c@ for 16-byte vectorized
-- scanning of delimiter, quote, and newline characters.
module CSV.Decode
  ( decode
  , decodeStream
  , decodeRecords
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import CSV.Value
import CSV.Class (FromCSV(..))

foreign import ccall unsafe "hs_xml_find_byte"
  c_find_byte :: Ptr () -> CInt -> CInt -> Word8 -> CInt

findByte :: ByteString -> Int -> Word8 -> Int
findByte bs off target = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    let !r = c_find_byte (castPtr ptr) (fromIntegral off) (fromIntegral len) target
    in pure $! if r < 0 then len else fromIntegral r
{-# INLINE findByte #-}

decode :: CSVConfig -> ByteString -> Either String CSVDocument
decode cfg bs0 = do
  let !bs = stripUtf8Bom bs0
      !rows = parseAllRows cfg bs
  if csvHasHeader cfg
    then case V.uncons rows of
      Nothing -> Right (CSVDocument Nothing V.empty)
      Just (hdr, rest) -> Right (CSVDocument (Just hdr) rest)
    else Right (CSVDocument Nothing rows)

-- | Strip a leading UTF-8 BOM (EF BB BF). Spreadsheet exports
-- (notably from Excel) routinely prefix CSV with this marker; if it
-- isn't stripped the first header field will appear with a phantom
-- three-byte prefix on its name.
stripUtf8Bom :: ByteString -> ByteString
stripUtf8Bom bs
  | BS.length bs >= 3
      && BSU.unsafeIndex bs 0 == 0xEF
      && BSU.unsafeIndex bs 1 == 0xBB
      && BSU.unsafeIndex bs 2 == 0xBF =
      BS.drop 3 bs
  | otherwise = bs
{-# INLINE stripUtf8Bom #-}

decodeStream :: CSVConfig -> ByteString -> (Vector Text -> IO ()) -> IO (Either String ())
decodeStream cfg bs callback = do
  let !len = BS.length bs
      !delimW = fromIntegral (fromEnum (csvDelimiter cfg)) :: Word8
      !quoteW = fromIntegral (fromEnum (csvQuote cfg)) :: Word8
      !isHeader = csvHasHeader cfg
  go isHeader 0 len delimW quoteW
  where
    go !skipFirst !off !len !delimW !quoteW
      | off >= len = pure (Right ())
      | otherwise = do
          let (row, nextOff) = parseRow cfg bs off len delimW quoteW
          if skipFirst
            then go False nextOff len delimW quoteW
            else do
              callback row
              go False nextOff len delimW quoteW

decodeRecords :: FromCSV a => CSVConfig -> ByteString -> Either String (Vector a)
decodeRecords cfg bs = do
  doc <- decode cfg bs
  V.mapM fromCSVRow (csvRows doc)

parseAllRows :: CSVConfig -> ByteString -> Vector (Vector Text)
parseAllRows cfg bs = V.create $ do
  let !len = BS.length bs
      !delimW = fromIntegral (fromEnum (csvDelimiter cfg)) :: Word8
      !quoteW = fromIntegral (fromEnum (csvQuote cfg)) :: Word8
      initCap = max 16 (len `div` 40)
  mv <- MV.new initCap
  let go !off !count !cap !vec
        | off >= len = pure (vec, count)
        | otherwise = do
            let (row, nextOff) = parseRow cfg bs off len delimW quoteW
            vec' <- if count >= cap
                    then MV.grow vec cap
                    else pure vec
            let cap' = if count >= cap then cap * 2 else cap
            MV.write vec' count row
            go nextOff (count + 1) cap' vec'
  (vec, count) <- go 0 0 initCap mv
  pure (MV.take count vec)

parseRow :: CSVConfig -> ByteString -> Int -> Int -> Word8 -> Word8
         -> (Vector Text, Int)
parseRow cfg bs !off !len !delimW !quoteW =
  let (!fields, !endOff) = parseFields cfg bs off len delimW quoteW
  in (V.fromList (reverse fields), endOff)
{-# INLINE parseRow #-}

parseFields :: CSVConfig -> ByteString -> Int -> Int -> Word8 -> Word8
            -> ([Text], Int)
parseFields _cfg bs !off !len !delimW !quoteW = go off []
  where
    go !pos !acc
      | pos >= len = (T.empty : acc, len)
      | BSU.unsafeIndex bs pos == quoteW =
          let (!field, !afterField) = parseQuotedField bs (pos + 1) len quoteW
          in consumeAfterField afterField (field : acc)
      | otherwise =
          let (!field, !afterField) = parseUnquotedField bs pos len delimW
          in consumeAfterField afterField (field : acc)

    consumeAfterField !pos !acc
      | pos >= len = (acc, skipNewline bs pos len)
      | BSU.unsafeIndex bs pos == delimW = go (pos + 1) acc
      | BSU.unsafeIndex bs pos == 0x0D =
          if pos + 1 < len && BSU.unsafeIndex bs (pos + 1) == 0x0A
            then (acc, pos + 2)
            else (acc, pos + 1)
      | BSU.unsafeIndex bs pos == 0x0A = (acc, pos + 1)
      | otherwise = (acc, pos + 1)

skipNewline :: ByteString -> Int -> Int -> Int
skipNewline bs !pos !len
  | pos >= len = len
  | BSU.unsafeIndex bs pos == 0x0D =
      if pos + 1 < len && BSU.unsafeIndex bs (pos + 1) == 0x0A
        then pos + 2
        else pos + 1
  | BSU.unsafeIndex bs pos == 0x0A = pos + 1
  | otherwise = pos

parseQuotedField :: ByteString -> Int -> Int -> Word8 -> (Text, Int)
parseQuotedField bs !start !len !quoteW = go start []
  where
    quoteText :: Text
    quoteText = T.singleton (toEnum (fromIntegral quoteW))

    go !pos !chunks
      | pos >= len = (assemble pos chunks, len)
      | otherwise =
          let !qPos = findByte bs pos quoteW
          in if qPos >= len
               then (assemble qPos chunks, len)
               else if qPos + 1 < len && BSU.unsafeIndex bs (qPos + 1) == quoteW
                    then go (qPos + 2) (sliceText bs pos qPos : chunks)
                    else
                      let !lastChunk = sliceText bs pos qPos
                      in (T.intercalate quoteText (reverse (lastChunk : chunks)), qPos + 1)

    assemble !endPos [] = sliceText bs start endPos
    assemble !endPos chunks =
      let !lastChunk = sliceText bs start endPos
      in T.intercalate quoteText (reverse (lastChunk : chunks))

parseUnquotedField :: ByteString -> Int -> Int -> Word8 -> (Text, Int)
parseUnquotedField bs !start !len !delimW = scanTo start
  where
    scanTo !pos
      | pos >= len = (sliceText bs start pos, len)
      | otherwise =
          let !b = BSU.unsafeIndex bs pos
          in if b == delimW || b == 0x0A || b == 0x0D
               then (sliceText bs start pos, pos)
               else scanTo (pos + 1)

sliceText :: ByteString -> Int -> Int -> Text
sliceText bs !start !end
  | start >= end = T.empty
  | otherwise = case TE.decodeUtf8' (BSU.unsafeTake (end - start) (BSU.unsafeDrop start bs)) of
      Right t -> t
      Left _  -> T.empty
{-# INLINE sliceText #-}
