{-# LANGUAGE BangPatterns #-}

{- | High-level Thrift decoding for Binary and Compact protocols.

Reads a wire-format 'ByteString' and produces a 'Thrift.Value.Value' tree.
Container elements use pre-allocated mutable vectors.

@
import Thrift.Decode (decodeBinary, decodeCompact)

case decodeBinary bytes of
  Right val -> print val
  Left err  -> putStrLn err
@
-}
module Thrift.Decode (
  decodeBinary,
  decodeCompact,
  decodeCompactFrom,
  decodeCompactBinaryStrings,
) where

import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Thrift.Value qualified as TV
import Thrift.Wire
import Wireform.FFI (decodeTextFast)


--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

decodeBinary :: ByteString -> Either String TV.Value
decodeBinary bs = case decodeBinStruct bs 0 of
  Nothing -> Left "decodeBinary: failed to decode struct"
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
      Left _ -> Just (TV.Binary b, o)
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


decodeBinMapEntries
  :: ThriftType
  -> ThriftType
  -> Int
  -> ByteString
  -> Int
  -> Maybe (TV.Value, Int)
decodeBinMapEntries kt vt !count bs !off
  | count < 0 || off < 0 || off > BS.length bs || count > (BS.length bs - off) `quot` 2 = Nothing
  | count == 0 = Just (TV.Map kt vt V.empty, off)
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


decodeBinListEntries
  :: ThriftType
  -> Int
  -> ByteString
  -> Int
  -> Maybe (TV.Value, Int)
decodeBinListEntries et !count bs !off
  | count < 0 || off < 0 || off > BS.length bs || count > BS.length bs - off = Nothing
  | count == 0 = Just (TV.List et V.empty, off)
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


decodeBinSetEntries
  :: ThriftType
  -> Int
  -> ByteString
  -> Int
  -> Maybe (TV.Value, Int)
decodeBinSetEntries et !count bs !off
  | count < 0 || off < 0 || off > BS.length bs || count > BS.length bs - off = Nothing
  | count == 0 = Just (TV.Set et V.empty, off)
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
decodeCompact = decodeCompactWith True


{- | Preserve every Compact @STRING@ payload as 'TV.Binary'. Thrift uses the
same wire type for UTF-8 strings and opaque bytes. Schema-aware readers can
decode text fields later without copying binary statistics or index bounds
through Text. Nested structs and containers retain the same policy.
Binary payloads are slices sharing the input buffer's storage.
-}
decodeCompactBinaryStrings :: ByteString -> Either String TV.Value
decodeCompactBinaryStrings = decodeCompactWith False


decodeCompactWith :: Bool -> ByteString -> Either String TV.Value
decodeCompactWith decodeStrings bs = case decodeCompStruct decodeStrings bs 0 of
  Nothing -> Left "decodeCompact: failed to decode struct"
  Just (v, _off) -> Right v


{- | Decode one Thrift Compact struct starting at an offset; return the value
and the next byte offset after the struct.
-}
decodeCompactFrom :: ByteString -> Int -> Either String (TV.Value, Int)
decodeCompactFrom bs off = case decodeCompStruct True bs off of
  Nothing -> Left "decodeCompactFrom: failed to decode struct"
  Just (v, o) -> Right (v, o)


decodeCompStruct :: Bool -> ByteString -> Int -> Maybe (TV.Value, Int)
decodeCompStruct decodeStrings bs off = go off 0 []
  where
    go :: Int -> Int16 -> [(Int16, TV.Value)] -> Maybe (TV.Value, Int)
    go !o !lastFid !acc = case tCompDecodeFieldBegin bs o lastFid of
      Nothing -> Nothing
      Just (TT_STOP, _, o', _) -> Just (TV.Struct (V.fromList (reverse acc)), o')
      Just (tt, fid, o', boolVal) -> case tt of
        TT_BOOL -> go o' fid ((fid, TV.Bool boolVal) : acc)
        _ -> case decodeCompValue decodeStrings tt bs o' of
          Nothing -> Nothing
          Just (v, o'') -> go o'' fid ((fid, v) : acc)
{-# INLINE decodeCompStruct #-}


decodeCompValue :: Bool -> ThriftType -> ByteString -> Int -> Maybe (TV.Value, Int)
decodeCompValue decodeStrings !tt !bs !off = case tt of
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
    Just (b, o)
      | not decodeStrings -> Just (TV.Binary b, o)
      | otherwise -> case decodeTextFast b of
          Right t -> Just (TV.String t, o)
          Left _ -> Just (TV.Binary b, o)
    Nothing -> Nothing
  TT_STRUCT -> decodeCompStruct decodeStrings bs off
  TT_MAP -> case tCompDecodeMapBegin bs off of
    Nothing -> Nothing
    Just (kt, vt, sz, o)
      | sz == 0 -> Just (TV.Map kt vt V.empty, o)
      | otherwise ->
          let !count = fromIntegral sz
          in decodeCompMapEntries decodeStrings kt vt count bs o
  TT_LIST -> case tCompDecodeListBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) ->
      let !count = fromIntegral sz
      in decodeCompListEntries decodeStrings et count bs o
  TT_SET -> case tCompDecodeSetBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) ->
      let !count = fromIntegral sz
      in decodeCompSetEntries decodeStrings et count bs o
  TT_UUID
    | off + 16 > BS.length bs -> Nothing
    | otherwise -> Just (TV.UUID (BS.take 16 (BS.drop off bs)), off + 16)
  TT_STOP -> Nothing
{-# INLINE decodeCompValue #-}


decodeCompMapEntries
  :: Bool
  -> ThriftType
  -> ThriftType
  -> Int
  -> ByteString
  -> Int
  -> Maybe (TV.Value, Int)
decodeCompMapEntries decodeStrings kt vt !count bs !off
  | count < 0 || off < 0 || off > BS.length bs || count > (BS.length bs - off) `quot` 2 = Nothing
  | count == 0 = Just (TV.Map kt vt V.empty, off)
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s (TV.Value, TV.Value) -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.Map kt vt vec, o)
      | otherwise = case decodeCompValue decodeStrings kt bs o of
          Nothing -> pure Nothing
          Just (k, o1) -> case decodeCompValue decodeStrings vt bs o1 of
            Nothing -> pure Nothing
            Just (v, o2) -> do
              MV.unsafeWrite mv i (k, v)
              go mv (i + 1) o2
{-# INLINE decodeCompMapEntries #-}


decodeCompListEntries
  :: Bool
  -> ThriftType
  -> Int
  -> ByteString
  -> Int
  -> Maybe (TV.Value, Int)
decodeCompListEntries decodeStrings et !count bs !off
  | count < 0 || off < 0 || off > BS.length bs || count > BS.length bs - off = Nothing
  | count == 0 = Just (TV.List et V.empty, off)
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s TV.Value -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.List et vec, o)
      | otherwise = case decodeCompValue decodeStrings et bs o of
          Nothing -> pure Nothing
          Just (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeCompListEntries #-}


decodeCompSetEntries
  :: Bool
  -> ThriftType
  -> Int
  -> ByteString
  -> Int
  -> Maybe (TV.Value, Int)
decodeCompSetEntries decodeStrings et !count bs !off
  | count < 0 || off < 0 || off > BS.length bs || count > BS.length bs - off = Nothing
  | count == 0 = Just (TV.Set et V.empty, off)
  | otherwise = runST $ do
      mv <- MV.new count
      go mv 0 off
  where
    go :: MV.MVector s TV.Value -> Int -> Int -> ST s (Maybe (TV.Value, Int))
    go !mv !i !o
      | i >= count = do
          vec <- V.unsafeFreeze mv
          pure $! Just (TV.Set et vec, o)
      | otherwise = case decodeCompValue decodeStrings et bs o of
          Nothing -> pure Nothing
          Just (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeCompSetEntries #-}
