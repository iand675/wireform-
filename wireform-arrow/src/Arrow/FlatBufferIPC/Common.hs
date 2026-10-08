{-# LANGUAGE StrictData #-}
-- | Data types shared by the Arrow IPC encoder
-- ("Arrow.FlatBufferIPC.Write") and decoder
-- ("Arrow.FlatBufferIPC.Read"): dictionary batches and the
-- tensor / sparse-tensor message payloads.
module Arrow.FlatBufferIPC.Common
  ( DictBatch (..)
  , Tensor (..)
  , TensorDim (..)
  , SparseTensor (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V

import Arrow.Types (ArrowType, RecordBatchDef)

-- ============================================================
-- Tensor / SparseTensor messages
-- ============================================================

-- | Dense tensor metadata. Mirrors @table Tensor@ from Arrow's
-- @format/Tensor.fbs@. @tensorBody@ is the raw buffer carrying
-- @product(shape) * bytesPerElement(tensorType)@ little-endian
-- elements in row-major order (unless 'tensorStrides' is set).
data Tensor = Tensor
  { tensorType    :: !ArrowType
  , tensorShape   :: !(V.Vector TensorDim)
  , tensorStrides :: !(V.Vector Int64)
  , tensorBody    :: !ByteString
  } deriving (Show, Eq)

-- | One entry of a tensor's shape vector. Name is optional
-- (empty string by default).
data TensorDim = TensorDim
  { tdSize :: !Int64
  , tdName :: !T.Text
  } deriving (Show, Eq)

-- ============================================================
-- SparseTensor
-- ============================================================
--
-- Arrow's SparseTensor union covers four index formats (COO,
-- CSR, CSC, CSF). We model the common case — SparseTensorIndexCOO
-- — explicitly; callers with other formats can drop down to the
-- raw flatbuffer builder. A SparseTensor message (header_type=5)
-- wraps:
--
-- @
-- table SparseTensor {
--   type_type: Type;           // slot 0
--   type:      Type;           // slot 1
--   shape:    [TensorDim];     // slot 2
--   non_zero_length: long;     // slot 3
--   sparseIndex_type: SparseTensorIndex;  // slot 4 (union tag)
--   sparseIndex: SparseTensorIndex;       // slot 5
--   data: Buffer;              // slot 6
-- }
-- @
--
-- For COO:
--
-- @
-- table SparseTensorIndexCOO {
--   indicesType: Int;       // slot 0
--   indicesStrides: [long]; // slot 1
--   indicesBuffer: Buffer;  // slot 2
--   isCanonical: bool;      // slot 3
-- }
-- @

-- | A sparse tensor in coordinate (COO) layout. @indicesType@
-- is the integer width of each coordinate; @indicesBuffer@
-- carries @non_zero_length * ndim@ integers; @tensorBody@
-- carries @non_zero_length@ values of @tensorType@.
data SparseTensor = SparseTensor
  { sparseTensorType    :: !ArrowType
  , sparseTensorShape   :: !(V.Vector TensorDim)
  , sparseNonZeroLength :: !Int64
  , sparseIndicesType   :: !ArrowType
    -- ^ typically @AInt 64 True@ or @AInt 32 True@
  , sparseIndicesBody   :: !ByteString
  , sparseIndicesCanonical :: !Bool
  , sparseTensorBody    :: !ByteString
  } deriving (Show, Eq)

-- | One decoded dictionary batch — the raw payload that defines a
-- dictionary's index → value mapping. The @data@ field is the
-- inner @RecordBatch@ (a single column whose values are the
-- dictionary values, in index order).
data DictBatch = DictBatch
  { dbId      :: !Int64
    -- ^ Dictionary id; matches @DictionaryEncoding.id@ in the
    -- schema.
  , dbIsDelta :: !Bool
    -- ^ When @True@, the values append to the existing dictionary
    -- with this id; otherwise they replace it.
  , dbData    :: !RecordBatchDef
  , dbBody    :: !ByteString
  } deriving stock (Show, Eq)
