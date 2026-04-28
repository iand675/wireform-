{-# LANGUAGE BangPatterns #-}
-- | High-level Avro binary decoding.
--
-- Schema-driven: the writer's schema is required to interpret the raw bytes.
-- Uses the wire primitives from "Avro.Wire".
-- Uses mutable vectors for array\/map block decoding.
--
-- @
-- import Avro.Decode (decodeAvro)
-- import Avro.Schema (AvroType(..), AvroPrimitive(..))
--
-- case decodeAvro (AvroPrimitive AvroString) bytes of
--   Right val -> print val
--   Left err  -> putStrLn err
-- @
module Avro.Decode
  ( decodeAvro
  , decodeAvroAt
  , decodeAvroResolved
  ) where

import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import Avro.Resolution (resolveSchema, resolveValue)
import qualified Avro.Value as AV
import Avro.Wire
  ( AvroDecodeResult(..)
  , avroDecodeNull, avroDecodeBool, avroDecodeInt, avroDecodeLong
  , avroDecodeFloat, avroDecodeDouble, avroDecodeBytes, avroDecodeString
  )

-- | Decode a value from a strict 'ByteString' given its schema.
-- Returns 'Left' on any wire-format error.
decodeAvro :: AvroType -> ByteString -> Either String AV.Value
decodeAvro !ty !bs = case decodeValue ty bs 0 of
  AvroDecodeOK val off
    | off == BS.length bs -> Right val
    | otherwise -> Left $ "Avro.Decode: " ++ show (BS.length bs - off) ++ " trailing bytes"
  AvroDecodeFail e -> Left e

-- | Decode a value starting at a given offset, returning the value and the new offset.
decodeAvroAt :: AvroType -> ByteString -> Int -> Either String (AV.Value, Int)
decodeAvroAt !ty !bs !off = case decodeValue ty bs off of
  AvroDecodeOK val off' -> Right (val, off')
  AvroDecodeFail e      -> Left e

-- | Decode with schema evolution. Uses the writer schema to parse bytes,
-- then applies resolution to produce a value matching the reader schema.
decodeAvroResolved :: AvroType      -- ^ writer schema
                   -> AvroType      -- ^ reader schema
                   -> ByteString    -- ^ encoded data
                   -> Either String AV.Value
decodeAvroResolved writerSchema readerSchema bytes = do
  resolved <- resolveSchema writerSchema readerSchema
  val <- decodeAvro writerSchema bytes
  resolveValue resolved val

decodeValue :: AvroType -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeValue (AvroPrimitive s) bs off = decodePrimitive s bs off
decodeValue (AvroRecord {avroRecordFields = fields}) bs off =
  decodeRecord fields bs off
decodeValue (AvroEnum {}) bs off =
  case avroDecodeInt bs off of
    AvroDecodeOK n off' -> AvroDecodeOK (AV.Enum (fromIntegral n)) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodeValue (AvroArray {avroArrayItems = itemTy}) bs off =
  decodeArray itemTy bs off
decodeValue (AvroMap {avroMapValues = valTy}) bs off =
  decodeMap valTy bs off
decodeValue (AvroUnion {avroUnionBranches = branches}) bs off =
  case avroDecodeLong bs off of
    AvroDecodeOK idxI64 off' ->
      let !idx = fromIntegral idxI64 :: Int
      in if idx < 0 || idx >= V.length branches
         then AvroDecodeFail $ "Avro.Decode: union index out of range: " ++ show idx
         else case decodeValue (V.unsafeIndex branches idx) bs off' of
           AvroDecodeOK val off'' -> AvroDecodeOK (AV.Union idx val) off''
           AvroDecodeFail e       -> AvroDecodeFail e
    AvroDecodeFail e -> AvroDecodeFail e
decodeValue (AvroFixed {avroFixedSize = sz}) bs off
  | off + sz > BS.length bs = AvroDecodeFail "Avro.Decode: fixed: unexpected end of input"
  | otherwise = AvroDecodeOK (AV.Fixed (BS.take sz (BS.drop off bs))) (off + sz)
decodeValue (AvroLogical {avroLogicalBase = base}) bs off =
  decodeValue base bs off

decodePrimitive :: AvroSchema -> ByteString -> Int -> AvroDecodeResult AV.Value
decodePrimitive AvroNull bs off =
  case avroDecodeNull bs off of
    AvroDecodeOK _ off' -> AvroDecodeOK AV.Null off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroBool bs off =
  case avroDecodeBool bs off of
    AvroDecodeOK b off' -> AvroDecodeOK (AV.Bool b) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroInt bs off =
  case avroDecodeInt bs off of
    AvroDecodeOK n off' -> AvroDecodeOK (AV.Int n) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroLong bs off =
  case avroDecodeLong bs off of
    AvroDecodeOK n off' -> AvroDecodeOK (AV.Long n) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroFloat bs off =
  case avroDecodeFloat bs off of
    AvroDecodeOK f off' -> AvroDecodeOK (AV.Float f) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroDouble bs off =
  case avroDecodeDouble bs off of
    AvroDecodeOK d off' -> AvroDecodeOK (AV.Double d) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroBytes bs off =
  case avroDecodeBytes bs off of
    AvroDecodeOK b off' -> AvroDecodeOK (AV.Bytes b) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroString bs off =
  case avroDecodeString bs off of
    AvroDecodeOK t off' -> AvroDecodeOK (AV.String t) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive s _ _ = AvroDecodeFail $ "Avro.Decode: unsupported primitive: " ++ show s
{-# INLINE decodePrimitive #-}

decodeRecord :: V.Vector AvroField -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeRecord fields bs off0 =
  let !n = V.length fields
  in runST $ do
    mv <- MV.new n
    go mv 0 off0
  where
    !nFields = V.length fields
    go :: MV.MVector s AV.Value -> Int -> Int -> ST s (AvroDecodeResult AV.Value)
    go mv !i !off
      | i >= nFields = do
          vec <- V.unsafeFreeze mv
          pure $! AvroDecodeOK (AV.Record vec) off
      | otherwise =
          case decodeValue (avroFieldType (V.unsafeIndex fields i)) bs off of
            AvroDecodeOK val off' -> do
              MV.unsafeWrite mv i val
              go mv (i + 1) off'
            AvroDecodeFail e -> pure $! AvroDecodeFail e
{-# INLINE decodeRecord #-}

decodeArray :: AvroType -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeArray itemTy bs off0 = decodeArrayBlocks bs off0 []
  where
    decodeArrayBlocks :: ByteString -> Int -> [V.Vector AV.Value] -> AvroDecodeResult AV.Value
    decodeArrayBlocks bsB off blockAcc =
      case avroDecodeLong bsB off of
        AvroDecodeOK cnt64 off' ->
          let !cnt = cnt64
          in if cnt == 0
             then AvroDecodeOK (AV.Array (V.concat (reverse blockAcc))) off'
             else if cnt < 0
             then case avroDecodeLong bsB off' of
               AvroDecodeOK _blockSize off'' ->
                 decodeArrayBlock bsB off'' (fromIntegral (negate cnt)) blockAcc
               AvroDecodeFail e -> AvroDecodeFail e
             else decodeArrayBlock bsB off' (fromIntegral cnt) blockAcc
        AvroDecodeFail e -> AvroDecodeFail e

    decodeArrayBlock :: ByteString -> Int -> Int -> [V.Vector AV.Value] -> AvroDecodeResult AV.Value
    decodeArrayBlock bsI off blockCount blockAcc
      -- Reject impossible block counts up-front; otherwise an Avro
      -- input with a 9-byte header that declares a 9-quintillion
      -- element block would MV.new before consuming any payload.
      | blockCount < 0 =
          AvroDecodeFail "Avro.Decode: array block count overflows Int"
      | blockCount > BS.length bsI - off =
          AvroDecodeFail "Avro.Decode: array block count exceeds remaining input"
      | otherwise = runST $ do
          mv <- MV.new blockCount
          goBlock mv 0 off
      where
        goBlock :: MV.MVector s AV.Value -> Int -> Int -> ST s (AvroDecodeResult AV.Value)
        goBlock mv !i !o
          | i >= blockCount = do
              vec <- V.unsafeFreeze mv
              pure $! decodeArrayBlocks bsI o (vec : blockAcc)
          | otherwise =
              case decodeValue itemTy bsI o of
                AvroDecodeOK val o' -> do
                  MV.unsafeWrite mv i val
                  goBlock mv (i + 1) o'
                AvroDecodeFail e -> pure $! AvroDecodeFail e

decodeMap :: AvroType -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeMap valTy bs off0 = decodeMapBlocks bs off0 []
  where
    decodeMapBlocks :: ByteString -> Int -> [V.Vector (Text, AV.Value)] -> AvroDecodeResult AV.Value
    decodeMapBlocks bsB off blockAcc =
      case avroDecodeLong bsB off of
        AvroDecodeOK cnt64 off' ->
          let !cnt = cnt64
          in if cnt == 0
             then AvroDecodeOK (AV.Map (V.concat (reverse blockAcc))) off'
             else if cnt < 0
             then case avroDecodeLong bsB off' of
               AvroDecodeOK _blockSize off'' ->
                 decodeMapBlock bsB off'' (fromIntegral (negate cnt)) blockAcc
               AvroDecodeFail e -> AvroDecodeFail e
             else decodeMapBlock bsB off' (fromIntegral cnt) blockAcc
        AvroDecodeFail e -> AvroDecodeFail e

    decodeMapBlock :: ByteString -> Int -> Int -> [V.Vector (Text, AV.Value)] -> AvroDecodeResult AV.Value
    decodeMapBlock bsE off blockCount blockAcc
      | blockCount < 0 =
          AvroDecodeFail "Avro.Decode: map block count overflows Int"
      -- Each entry needs at least 1 byte for key length + 1 byte for
      -- value tag.
      | blockCount > (BS.length bsE - off) `div` 2 =
          AvroDecodeFail "Avro.Decode: map block count exceeds remaining input"
      | otherwise = runST $ do
          mv <- MV.new blockCount
          goBlock mv 0 off
      where
        goBlock :: MV.MVector s (Text, AV.Value) -> Int -> Int -> ST s (AvroDecodeResult AV.Value)
        goBlock mv !i !o
          | i >= blockCount = do
              vec <- V.unsafeFreeze mv
              pure $! decodeMapBlocks bsE o (vec : blockAcc)
          | otherwise =
              case avroDecodeString bsE o of
                AvroDecodeOK key o' ->
                  case decodeValue valTy bsE o' of
                    AvroDecodeOK val o'' -> do
                      MV.unsafeWrite mv i (key, val)
                      goBlock mv (i + 1) o''
                    AvroDecodeFail e -> pure $! AvroDecodeFail e
                AvroDecodeFail e -> pure $! AvroDecodeFail e
