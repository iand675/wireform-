{-# LANGUAGE BangPatterns #-}
-- | NDJSON (Newline-Delimited JSON) decoder.
--
-- Uses SIMD scanning via @hs_xml_find_byte@ to find newline boundaries
-- in 16-byte chunks, then delegates each line to aeson for JSON parsing.
module NDJSON.Decode
  ( decode
  , decodeStream
  , decodeRecords
  , decodeConcurrent
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.Aeson as Aeson

foreign import ccall unsafe "hs_xml_find_byte"
  c_find_byte :: Ptr () -> CInt -> CInt -> Word8 -> CInt

findNewline :: ByteString -> Int -> Int
findNewline bs off = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    let !r = c_find_byte (castPtr ptr) (fromIntegral off) (fromIntegral len) 0x0A
    in pure $! if r < 0 then len else fromIntegral r
{-# INLINE findNewline #-}

-- | Trim a trailing '\\r' (0x0D) before the LF terminator. Strict
-- ndjson-spec uses LF only, but real-world data routinely has CRLF
-- line endings (Windows-authored streams, HTTP responses with
-- preserved line endings). Without this trim Aeson sees a stray
-- carriage return after each JSON value and either tolerates or
-- rejects it depending on aeson version; trimming keeps behaviour
-- identical to mainstream JSON-lines consumers.
trimCR :: ByteString -> ByteString
trimCR !bs
  | BS.length bs > 0 && BSU.unsafeIndex bs (BS.length bs - 1) == 0x0D =
      BSU.unsafeTake (BS.length bs - 1) bs
  | otherwise = bs
{-# INLINE trimCR #-}

decode :: ByteString -> Either String (Vector Aeson.Value)
decode bs = do
  let !lines_ = splitLines bs
  V.mapM parseLine lines_

decodeStream :: ByteString -> (Aeson.Value -> IO ()) -> IO (Either String ())
decodeStream bs callback = do
  let !len = BS.length bs
  go 0 len
  where
    go !off !len
      | off >= len = pure (Right ())
      | otherwise =
          let !nlPos = findNewline bs off
              !lineLen = nlPos - off
          in if lineLen == 0
               then go (nlPos + 1) len
               else do
                 let !line = trimCR (BSU.unsafeTake lineLen (BSU.unsafeDrop off bs))
                 case Aeson.eitherDecodeStrict' line of
                   Left err  -> pure (Left err)
                   Right val -> do
                     callback val
                     go (nlPos + 1) len

decodeRecords :: Aeson.FromJSON a => ByteString -> Either String (Vector a)
decodeRecords bs = do
  let !lines_ = splitLines bs
  V.mapM (\line -> case Aeson.eitherDecodeStrict' line of
    Left err  -> Left err
    Right val -> Right val) lines_

decodeConcurrent :: ByteString -> Int -> (Aeson.Value -> IO ()) -> IO (Either String ())
decodeConcurrent bs bufSize callback = do
  chan <- newTBQueueIO (fromIntegral bufSize)
  resultVar <- newTVarIO (Right ())

  _ <- forkIO $ do
    let !len = BS.length bs
        produce !off
          | off >= len = atomically $ writeTBQueue chan Nothing
          | otherwise =
              let !nlPos = findNewline bs off
                  !lineLen = nlPos - off
              in if lineLen == 0
                   then produce (nlPos + 1)
                   else do
                     let !line = trimCR (BSU.unsafeTake lineLen (BSU.unsafeDrop off bs))
                     case Aeson.eitherDecodeStrict' line of
                       Left err -> do
                         atomically $ do
                           writeTVar resultVar (Left err)
                           writeTBQueue chan Nothing
                       Right val -> do
                         atomically $ writeTBQueue chan (Just val)
                         produce (nlPos + 1)
    produce 0

  let consume = do
        mval <- atomically $ readTBQueue chan
        case mval of
          Nothing  -> atomically $ readTVar resultVar
          Just val -> do
            callback val
            consume
  consume

splitLines :: ByteString -> Vector ByteString
splitLines bs = V.create $ do
  let !len = BS.length bs
      initCap = max 16 (len `div` 60)
  mv <- MV.new initCap
  let go !off !count !cap !vec
        | off >= len = pure (vec, count)
        | otherwise =
            let !nlPos = findNewline bs off
                !lineLen = nlPos - off
            in if lineLen == 0
                 then go (nlPos + 1) count cap vec
                 else do
                   let !line = trimCR (BSU.unsafeTake lineLen (BSU.unsafeDrop off bs))
                   vec' <- if count >= cap
                           then MV.grow vec cap
                           else pure vec
                   let cap' = if count >= cap then cap * 2 else cap
                   MV.write vec' count line
                   go (nlPos + 1) (count + 1) cap' vec'
  (vec, count) <- go 0 0 initCap mv
  pure (MV.take count vec)

parseLine :: ByteString -> Either String Aeson.Value
parseLine line = case Aeson.eitherDecodeStrict' line of
  Left err  -> Left err
  Right val -> Right val
