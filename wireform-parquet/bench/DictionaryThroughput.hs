{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (unless)
import Criterion.Main (bench, bgroup, defaultMain, env, whnf)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as M
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Parquet.Write qualified as W
import System.Mem (performMajorGC)


rowCount :: Int
rowCount = 8192


data Input = Input
  { inputName :: !String
  , inputColumn :: !W.ColumnData
  , inputUniques :: !W.ColumnData
  , inputIndices :: !(P.Vector Int)
  }


data Spacing = Dense | Sparse


data Kind
  = Int32Keys !Spacing
  | Int64Keys !Spacing
  | FloatKeys !Spacing
  | DoubleKeys !Spacing
  | BinaryKeys {-# UNPACK #-} !Int
  | ShortTextKeys
  | CommonEndKeys
  | MultipleMiddleKeys


data InputSpec = InputSpec
  { specName :: !String
  , specCardinality :: {-# UNPACK #-} !Int
  , specKind :: !Kind
  }


-- Recipes contain no input buffers. Criterion keeps only its current
-- fixture alive, matching the reference harness's per-case lifetime.
inputSpecs :: [InputSpec]
inputSpecs = concatMap shapes [8, 1024, rowCount]
  where
    shapes cardinality =
      let sample label kind = InputSpec (label ++ " " ++ show cardinality ++ " unique") cardinality kind
      in [ sample "INT32" (Int32Keys Dense)
         , sample "INT64" (Int64Keys Dense)
         , sample "FLOAT" (FloatKeys Dense)
         , sample "DOUBLE" (DoubleKeys Dense)
         , sample "BYTE_ARRAY" (BinaryKeys 8)
         , sample "INT32 sparse" (Int32Keys Sparse)
         , sample "INT64 sparse" (Int64Keys Sparse)
         , sample "FLOAT sparse" (FloatKeys Sparse)
         , sample "DOUBLE sparse" (DoubleKeys Sparse)
         , sample "BYTE_ARRAY 64 bytes" (BinaryKeys 64)
         , sample "BYTE_ARRAY 4096 bytes" (BinaryKeys 4096)
         , sample "BYTE_ARRAY common ends 64 bytes" CommonEndKeys
         , sample "BYTE_ARRAY short text" ShortTextKeys
         , sample "BYTE_ARRAY common ends multiple words 64 bytes" MultipleMiddleKeys
         ]


-- Explicit IO allocation prevents a recipe from retaining a pure vector
-- shared between its preflight and later Criterion environment.
newPrimitiveVector :: (Prim a) => (Int -> a) -> IO (P.Vector a)
newPrimitiveVector valueAt = do
  values <- M.unsafeNew rowCount
  let fill !row
        | row == rowCount = P.unsafeFreeze values
        | otherwise = M.unsafeWrite values row (valueAt row) >> fill (row + 1)
  fill 0


spread32 :: Int -> Word32
spread32 key = fromIntegral key * 1664525 + 1013904223


spread64 :: Int -> Word64
spread64 key = fromIntegral key * 6364136223846793005 + 1442695040888963407


putWordLE :: Ptr Word8 -> Int -> Word64 -> IO ()
putWordLE ptr offset value = fill 0
  where
    fill !byte
      | byte == 8 = pure ()
      | otherwise = do
          pokeByteOff ptr (offset + byte) (fromIntegral (value `shiftR` (byte * 8)) :: Word8)
          fill (byte + 1)


newByteValue :: Kind -> Word64 -> IO BS.ByteString
newByteValue kind key = case kind of
  BinaryKeys width -> filled width (\ptr -> putWordLE ptr 0 key)
  CommonEndKeys -> filled 64 (\ptr -> putWordLE ptr 24 key)
  MultipleMiddleKeys ->
    let (high, low) = key `quotRem` 4
    in filled 64 (\ptr -> putWordLE ptr 16 low >> putWordLE ptr 24 high)
  ShortTextKeys -> do
    let digits !count !value
          | value < 10 = count
          | otherwise = digits (count + 1) (value `quot` 10)
        count = digits 1 key
    BSI.create count $ \ptr ->
      let writeDigit !offset !value = do
            let (remaining, digit) = value `quotRem` 10
            pokeByteOff ptr offset (48 + fromIntegral digit :: Word8)
            if offset == 0 then pure () else writeDigit (offset - 1) remaining
      in writeDigit (count - 1) key
  _ -> error "numeric recipe passed to newByteValue"
  where
    filled size fields = BSI.create size $ \ptr -> fillBytes ptr 97 size >> fields ptr


newInput :: InputSpec -> IO Input
newInput spec = do
  let cardinality = specCardinality spec
      keyAt row = (row * 104729) `rem` cardinality
  indices <- newPrimitiveVector (`rem` cardinality)
  column <- case specKind spec of
    Int32Keys spacing -> W.ColInt32 <$> newPrimitiveVector (\row -> case spacing of Dense -> fromIntegral (keyAt row); Sparse -> fromIntegral (spread32 (keyAt row)))
    Int64Keys spacing -> W.ColInt64 <$> newPrimitiveVector (\row -> case spacing of Dense -> fromIntegral (keyAt row); Sparse -> fromIntegral (spread64 (keyAt row)))
    FloatKeys spacing -> W.ColFloat <$> newPrimitiveVector (\row -> castWord32ToFloat (0x3f000000 .|. case spacing of Dense -> fromIntegral (keyAt row); Sparse -> spread32 (keyAt row) .&. 0x007fffff))
    DoubleKeys spacing -> W.ColDouble <$> newPrimitiveVector (\row -> castWord64ToDouble (0x3fe0000000000000 .|. case spacing of Dense -> fromIntegral (keyAt row); Sparse -> spread64 (keyAt row) .&. 0x000fffffffffffff))
    kind -> W.ColByteArray <$> V.generateM rowCount (\row -> newByteValue kind (fromIntegral (keyAt row)))
  pure (Input (specName spec) column (takeColumn cardinality column) indices)


takeColumn :: Int -> W.ColumnData -> W.ColumnData
takeColumn count column = case column of
  W.ColInt32 values -> W.ColInt32 (P.take count values)
  W.ColInt64 values -> W.ColInt64 (P.take count values)
  W.ColFloat values -> W.ColFloat (P.take count values)
  W.ColDouble values -> W.ColDouble (P.take count values)
  W.ColBool values -> W.ColBool (V.take count values)
  W.ColByteArray values -> W.ColByteArray (V.take count values)


forceColumn :: W.ColumnData -> ()
forceColumn column = case column of
  W.ColByteArray values -> V.foldl' (\count value -> count + BS.length value) 0 values `seq` ()
  W.ColBool values -> V.foldl' (\count value -> if value then count + 1 else count) (0 :: Int) values `seq` ()
  _ -> W.columnDataLength column `seq` ()


validateInput :: Input -> IO ()
validateInput sample = do
  _ <- evaluate (forceColumn (inputColumn sample))
  let dictionary = W.buildDictionary (inputColumn sample)
  unless
    (W.dictUniques dictionary == inputUniques sample && W.dictIndices dictionary == P.map fromIntegral (inputIndices sample))
    (fail (inputName sample ++ ": dictionary values/first-appearance indices differ"))


-- Return only the source column; expected outputs and preflight results
-- must not become part of the live heap scanned during timed iterations.
newtype PreparedInput = PreparedInput W.ColumnData


instance NFData PreparedInput where
  rnf (PreparedInput column) = forceColumn column


prepareInput :: InputSpec -> IO PreparedInput
prepareInput spec = do
  performMajorGC
  prepared <- do
    sample <- newInput spec
    validateInput sample
    column <- evaluate (inputColumn sample)
    pure (PreparedInput column)
  _ <- evaluate (rnf prepared)
  performMajorGC
  pure prepared


main :: IO ()
main = do
  -- Validate every recipe even for --list, but discard each input before
  -- preparing the next one. Environments allocate fresh timed fixtures.
  mapM_ (\spec -> newInput spec >>= validateInput) inputSpecs
  performMajorGC
  defaultMain
    [ bgroup "dictionary build" $
        map
          (\spec -> env (prepareInput spec) (\(PreparedInput column) -> bench (specName spec) (whnf W.buildDictionary column)))
          inputSpecs
    ]
