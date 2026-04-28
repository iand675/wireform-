{-# LANGUAGE BangPatterns #-}
-- | High-level Thrift decoding for Binary and Compact protocols.
--
-- Reads a wire-format 'ByteString' and produces a 'Thrift.Value.Value' tree.
-- Uses pre-allocated mutable vectors instead of list accumulation + reverse.
--
-- @
-- import Thrift.Decode (decodeBinary, decodeCompact)
--
-- case decodeBinary bytes of
--   Right val -> print val
--   Left err  -> putStrLn err
-- @
module Thrift.Decode
  ( decodeBinary
  , decodeCompact
  , decodeCompactFrom
  ) where

import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified Thrift.Value as TV
import Thrift.Wire
import Wireform.FFI (decodeTextFast)

--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

decodeBinary :: ByteString -> Either String TV.Value
decodeBinary bs = case decodeBinStruct bs 0 of
  Nothing       -> Left "decodeBinary: failed to decode struct"
  Just (v, _off) -> Right v

decodeBinStruct :: ByteString -> Int -> Maybe (TV.Value, Int)
decodeBinStruct bs off = go off []
  where
    go !o !acc = case tBinDecodeFieldBegin bs o of
      Nothing -> Nothing
      Just (TT_STOP, _, o') -> Just (TV.Struct (V.fromList (reverse acc)), o')
      Just (tt, fid, o') -> case decodeBinValue tt bs o' of
        Nothing -> Nothing
        Just (v, o'') -> go o'' ((fid, v) : acc)
{-# INLINE decodeBinStruct #-}

decodeBinValue :: ThriftType -> ByteString -> Int -> Maybe (TV.Value, Int)
decodeBinValue !tt !bs !off = case tt of
  TT_BOOL -> case tBinDecodeBool bs off of
    Just (b, o) -> Just (TV.Bool b, o)
    Nothing -> Nothing

  TT_BYTE -> case tBinDecodeI8 bs off of
    Just (v, o) -> Just (TV.Byte v, o)
    Nothing -> Nothing

  TT_I16 -> case tBinDecodeI16 bs off of
    Just (v, o) -> Just (TV.I16 v, o)
    Nothing -> Nothing

  TT_I32 -> case tBinDecodeI32 bs off of
    Just (v, o) -> Just (TV.I32 v, o)
    Nothing -> Nothing

  TT_I64 -> case tBinDecodeI64 bs off of
    Just (v, o) -> Just (TV.I64 v, o)
    Nothing -> Nothing

  TT_DOUBLE -> case tBinDecodeDouble bs off of
    Just (d, o) -> Just (TV.Double d, o)
    Nothing -> Nothing

  TT_STRING -> case tBinDecodeString bs off of
    Just (b, o) -> case decodeTextFast b of
      Right t -> Just (TV.String t, o)
      Left _  -> Just (TV.Binary b, o)
    Nothing -> Nothing

  TT_STRUCT -> decodeBinStruct bs off

  TT_MAP -> case tBinDecodeMapBegin bs off of
    Nothing -> Nothing
    Just (kt, vt, sz, o) ->
      let !count = fromIntegral sz
      in decodeBinMapEntries kt vt count bs o

  TT_LIST -> case tBinDecodeListBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) ->
      let !count = fromIntegral sz
      in decodeBinListEntries et count bs o

  TT_SET -> case tBinDecodeSetBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) ->
      let !count = fromIntegral sz
      in decodeBinSetEntries et count bs o

  TT_UUID
    | off + 16 > BS.length bs -> Nothing
    | otherwise -> Just (TV.UUID (BS.take 16 (BS.drop off bs)), off + 16)

  TT_STOP -> Nothing
{-# INLINE decodeBinValue #-}

decodeBinMapEntries :: ThriftType -> ThriftType -> Int -> ByteString -> Int
                    -> Maybe (TV.Value, Int)
decodeBinMapEntries kt vt !count bs !off
  | count <= 0 = Just (TV.Map kt vt V.empty, off)
  -- Each entry needs at least two bytes (one for the key, one for
  -- the value). Reject DoS-shaped counts up-front.
  | count > (BS.length bs - off) `div` 2 = Nothing
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s (TV.Value, TV.Value) -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.Map kt vt vec, o)
      | otherwise = case decodeBinValue kt bs o of
          Nothing -> pure Nothing
          Just (k, o1) -> case decodeBinValue vt bs o1 of
            Nothing -> pure Nothing
            Just (v, o2) -> do
              MV.unsafeWrite mv i (k, v)
              go mv (i + 1) o2
{-# INLINE decodeBinMapEntries #-}

decodeBinListEntries :: ThriftType -> Int -> ByteString -> Int
                     -> Maybe (TV.Value, Int)
decodeBinListEntries et !count bs !off
  | count <= 0 = Just (TV.List et V.empty, off)
  | count > BS.length bs - off = Nothing
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s TV.Value -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.List et vec, o)
      | otherwise = case decodeBinValue et bs o of
          Nothing -> pure Nothing
          Just (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeBinListEntries #-}

decodeBinSetEntries :: ThriftType -> Int -> ByteString -> Int
                    -> Maybe (TV.Value, Int)
decodeBinSetEntries et !count bs !off
  | count <= 0 = Just (TV.Set et V.empty, off)
  | count > BS.length bs - off = Nothing
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s TV.Value -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.Set et vec, o)
      | otherwise = case decodeBinValue et bs o of
          Nothing -> pure Nothing
          Just (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeBinSetEntries #-}

--------------------------------------------------------------------------------
-- Compact Protocol
--------------------------------------------------------------------------------

decodeCompact :: ByteString -> Either String TV.Value
decodeCompact bs = case decodeCompStruct bs 0 of
  Nothing        -> Left "decodeCompact: failed to decode struct"
  Just (v, _off) -> Right v

-- | Decode one Thrift Compact struct starting at an offset; return the value
-- and the next byte offset after the struct.
decodeCompactFrom :: ByteString -> Int -> Either String (TV.Value, Int)
decodeCompactFrom bs off = case decodeCompStruct bs off of
  Nothing -> Left "decodeCompactFrom: failed to decode struct"
  Just (v, o) -> Right (v, o)

decodeCompStruct :: ByteString -> Int -> Maybe (TV.Value, Int)
decodeCompStruct bs off = go off 0 []
  where
    go :: Int -> Int16 -> [(Int16, TV.Value)] -> Maybe (TV.Value, Int)
    go !o !lastFid !acc = case tCompDecodeFieldBegin bs o lastFid of
      Nothing -> Nothing
      Just (TT_STOP, _, o', _) -> Just (TV.Struct (V.fromList (reverse acc)), o')
      Just (tt, fid, o', boolVal) -> case tt of
        TT_BOOL -> go o' fid ((fid, TV.Bool boolVal) : acc)
        _ -> case decodeCompValue tt bs o' of
          Nothing -> Nothing
          Just (v, o'') -> go o'' fid ((fid, v) : acc)
{-# INLINE decodeCompStruct #-}

decodeCompValue :: ThriftType -> ByteString -> Int -> Maybe (TV.Value, Int)
decodeCompValue !tt !bs !off = case tt of
  TT_BOOL -> case tCompDecodeBool bs off of
    Just (b, o) -> Just (TV.Bool b, o)
    Nothing -> Nothing

  TT_BYTE -> case tCompDecodeI8 bs off of
    Just (v, o) -> Just (TV.Byte v, o)
    Nothing -> Nothing

  TT_I16 -> case tCompDecodeI16 bs off of
    Just (v, o) -> Just (TV.I16 v, o)
    Nothing -> Nothing

  TT_I32 -> case tCompDecodeI32 bs off of
    Just (v, o) -> Just (TV.I32 v, o)
    Nothing -> Nothing

  TT_I64 -> case tCompDecodeI64 bs off of
    Just (v, o) -> Just (TV.I64 v, o)
    Nothing -> Nothing

  TT_DOUBLE -> case tCompDecodeDouble bs off of
    Just (d, o) -> Just (TV.Double d, o)
    Nothing -> Nothing

  TT_STRING -> case tCompDecodeString bs off of
    Just (b, o) -> case decodeTextFast b of
      Right t -> Just (TV.String t, o)
      Left _  -> Just (TV.Binary b, o)
    Nothing -> Nothing

  TT_STRUCT -> decodeCompStruct bs off

  TT_MAP -> case tCompDecodeMapBegin bs off of
    Nothing -> Nothing
    Just (kt, vt, sz, o)
      | sz == 0   -> Just (TV.Map kt vt V.empty, o)
      | otherwise ->
          let !count = fromIntegral sz
          in decodeCompMapEntries kt vt count bs o

  TT_LIST -> case tCompDecodeListBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) ->
      let !count = fromIntegral sz
      in decodeCompListEntries et count bs o

  TT_SET -> case tCompDecodeSetBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) ->
      let !count = fromIntegral sz
      in decodeCompSetEntries et count bs o

  TT_UUID
    | off + 16 > BS.length bs -> Nothing
    | otherwise -> Just (TV.UUID (BS.take 16 (BS.drop off bs)), off + 16)

  TT_STOP -> Nothing
{-# INLINE decodeCompValue #-}

decodeCompMapEntries :: ThriftType -> ThriftType -> Int -> ByteString -> Int
                     -> Maybe (TV.Value, Int)
decodeCompMapEntries kt vt !count bs !off
  | count <= 0 = Just (TV.Map kt vt V.empty, off)
  | count > (BS.length bs - off) `div` 2 = Nothing
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s (TV.Value, TV.Value) -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.Map kt vt vec, o)
      | otherwise = case decodeCompValue kt bs o of
          Nothing -> pure Nothing
          Just (k, o1) -> case decodeCompValue vt bs o1 of
            Nothing -> pure Nothing
            Just (v, o2) -> do
              MV.unsafeWrite mv i (k, v)
              go mv (i + 1) o2
{-# INLINE decodeCompMapEntries #-}

decodeCompListEntries :: ThriftType -> Int -> ByteString -> Int
                      -> Maybe (TV.Value, Int)
decodeCompListEntries et !count bs !off
  | count <= 0 = Just (TV.List et V.empty, off)
  | count > BS.length bs - off = Nothing
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s TV.Value -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.List et vec, o)
      | otherwise = case decodeCompValue et bs o of
          Nothing -> pure Nothing
          Just (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeCompListEntries #-}

decodeCompSetEntries :: ThriftType -> Int -> ByteString -> Int
                     -> Maybe (TV.Value, Int)
decodeCompSetEntries et !count bs !off
  | count <= 0 = Just (TV.Set et V.empty, off)
  | count > BS.length bs - off = Nothing
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s TV.Value -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.Set et vec, o)
      | otherwise = case decodeCompValue et bs o of
          Nothing -> pure Nothing
          Just (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeCompSetEntries #-}
