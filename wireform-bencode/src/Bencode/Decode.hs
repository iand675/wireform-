{-# LANGUAGE BangPatterns #-}
-- | Bencode binary decoding using peekByteOff-based offset parsing.
module Bencode.Decode
  ( decode
  , decodeStrict
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (ST, runST)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Bencode.Value as B

decode :: ByteString -> Either String B.Value
decode = decodeWith False

-- | Like 'decode', but rejects non-canonical input: dictionary keys
-- MUST appear in strictly ascending byte order and there MUST NOT
-- be duplicate keys (BEP-3 §dictionaries). The lenient 'decode'
-- silently sorts and deduplicates.
decodeStrict :: ByteString -> Either String B.Value
decodeStrict = decodeWith True

decodeWith :: Bool -> ByteString -> Either String B.Value
decodeWith strict !bs
  | BS.null bs = Left "Bencode.Decode: empty input"
  | otherwise = case parseValueWith strict bs 0 of
      Left err -> Left err
      Right (val, off) ->
        if off == BS.length bs
          then Right val
          else Left "Bencode.Decode: trailing data"

type Parser a = ByteString -> Int -> Either String (a, Int)

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

parseValue :: Parser B.Value
parseValue = parseValueWith False

parseValueWith :: Bool -> Parser B.Value
parseValueWith strict bs off
  | off >= BS.length bs = Left "Bencode.Decode: unexpected end of input"
  | otherwise =
    let !b = rdByte bs off
    in case b of
      0x69 -> parseInteger bs (off + 1)  -- 'i'
      0x6C -> parseListWith strict bs (off + 1)     -- 'l'
      0x64 -> parseDictWith strict bs (off + 1)     -- 'd'
      _ | b >= 0x30 && b <= 0x39 -> parseString bs off
        | otherwise -> Left $ "Bencode.Decode: unexpected byte " ++ show b

parseString :: Parser B.Value
parseString bs off = do
  (len, off1) <- parseLength bs off
  if len < 0
    then Left "Bencode.Decode: negative string length"
    else
      let !off2 = off1 + len
      in if off2 < off1 || off2 > BS.length bs
           then Left "Bencode.Decode: string length exceeds input"
           else Right (B.BString (BSU.unsafeTake len (BSU.unsafeDrop off1 bs)), off2)

parseLength :: ByteString -> Int -> Either String (Int, Int)
parseLength bs off = go off 0
  where
    !bsLen = BS.length bs
    go !i !acc
      | i >= bsLen = Left "Bencode.Decode: unterminated string length"
      | rdByte bs i == 0x3A = Right (acc, i + 1)  -- ':'
      | otherwise =
          let !b = rdByte bs i
          in if b >= 0x30 && b <= 0x39
               then go (i + 1) (acc * 10 + fromIntegral (b - 0x30))
               else Left "Bencode.Decode: non-digit in string length"

-- | Parse a Bencode integer (\"i...e\").
--
-- BEP-3 forbids leading zeros (except for \"i0e\" itself) and
-- forbids the negative-zero literal \"i-0e\". This parser rejects
-- both shapes.
parseInteger :: Parser B.Value
parseInteger bs off = go off []
  where
    !bsLen = BS.length bs
    go !i !acc
      | i >= bsLen = Left "Bencode.Decode: unterminated integer"
      | rdByte bs i == 0x65 =  -- 'e'
          let !s = reverse acc
          in case validateIntLiteral s of
               Left e   -> Left e
               Right () -> case reads s :: [(Integer, String)] of
                 [(n, "")] -> Right (B.BInteger n, i + 1)
                 _ -> Left "Bencode.Decode: invalid integer"
      | otherwise =
          let !c = toEnum (fromIntegral (rdByte bs i)) :: Char
          in go (i + 1) (c : acc)

-- | Reject the BEP-3-forbidden leading-zero / negative-zero shapes.
validateIntLiteral :: String -> Either String ()
validateIntLiteral s = case s of
  ""        -> Left "Bencode.Decode: empty integer literal"
  "-0"      -> Left "Bencode.Decode: negative zero is not allowed"
  '-' : '0' : _ : _ ->
      Left "Bencode.Decode: leading zeros after sign are not allowed"
  '0' : _ : _ ->
      Left "Bencode.Decode: leading zeros are not allowed"
  _         -> Right ()

parseList :: Parser B.Value
parseList = parseListWith False

parseListWith :: Bool -> Parser B.Value
parseListWith strict bs off0 = runST $ do
  mv <- MV.new 8
  go mv 0 8 off0
  where
    go :: MV.MVector s B.Value -> Int -> Int -> Int -> ST s (Either String (B.Value, Int))
    go !mv !i !cap !off
      | off >= BS.length bs = pure $! Left "Bencode.Decode: unterminated list"
      | rdByte bs off == 0x65 = do  -- 'e'
          vec <- V.unsafeFreeze (MV.take i mv)
          pure $! Right (B.BList vec, off + 1)
      | otherwise = case parseValueWith strict bs off of
          Left e -> pure $! Left e
          Right (v, off') -> do
            mv' <- if i >= cap then MV.grow mv cap else pure mv
            let !cap' = if i >= cap then cap * 2 else cap
            MV.unsafeWrite mv' i v
            go mv' (i + 1) cap' off'

parseDict :: Parser B.Value
parseDict = parseDictWith False

parseDictWith :: Bool -> Parser B.Value
parseDictWith strict bs off0 = runST $ do
  mv <- MV.new 8
  go mv 0 8 off0 Nothing
  where
    go :: MV.MVector s (ByteString, B.Value)
       -> Int -> Int -> Int -> Maybe ByteString
       -> ST s (Either String (B.Value, Int))
    go !mv !i !cap !off !prevKey
      | off >= BS.length bs = pure $! Left "Bencode.Decode: unterminated dict"
      | rdByte bs off == 0x65 = do  -- 'e'
          vec <- V.unsafeFreeze (MV.take i mv)
          let !sorted = if strict
                          then vec
                          else V.fromList (sortBy (comparing fst) (V.toList vec))
          pure $! Right (B.BDict sorted, off + 1)
      | otherwise = case parseString bs off of
          Left e -> pure $! Left e
          Right (B.BString key, off1) ->
            -- BEP-3 §dictionaries: keys must be sorted and unique.
            -- The strict mode rejects any violation; the lenient
            -- mode silently re-canonicalises in the trailing sort.
            if strict && case prevKey of { Just pk -> pk >= key; Nothing -> False }
              then pure $! Left
                "Bencode.Decode: dictionary keys not in strictly ascending order"
              else case parseValueWith strict bs off1 of
                Left e -> pure $! Left e
                Right (val, off2) -> do
                  mv' <- if i >= cap then MV.grow mv cap else pure mv
                  let !cap' = if i >= cap then cap * 2 else cap
                  MV.unsafeWrite mv' i (key, val)
                  go mv' (i + 1) cap' off2 (Just key)
          Right _ -> pure $! Left "Bencode.Decode: expected string key in dict"
