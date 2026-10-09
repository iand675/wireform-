{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Streaming column builders in 'ST' or 'IO' ('PrimMonad').

Each builder appends rows into growable, pinned, 64-byte aligned
buffers (amortised doubling) and freezes into a 'ColumnArray' whose
buffers are those allocations: no per-row heap objects, no final
copy. The validity bitmap is only allocated once the first null is
appended, so a builder that never sees a null freezes with no
validity.

Values are valid by construction ('appendText' copies the bytes of a
'Text', which are UTF-8), so freezing runs no validation.

'freezeBuilder' hands the buffers to the column and resets the
builder to empty; appending afterwards starts a new column in fresh
memory and never touches the frozen one.

Appending more than 2^31 - 1 bytes to a 32-bit offset builder
('newUtf8Builder', 'newBinaryBuilder') is an error; use the large
variants for such data.
-}
module Arrow.Column.Builder (
  -- * Common interface
  ColumnBuilder (..),

  -- * Fixed width
  PrimBuilder,
  newPrimBuilder,
  appendPrim,
  appendPrimMaybe,

  -- * Booleans
  BoolBuilder,
  newBoolBuilder,
  appendBool,
  appendBoolMaybe,

  -- * Strings and bytes
  Utf8Builder,
  newUtf8Builder,
  newLargeUtf8Builder,
  appendText,
  appendTextMaybe,
  BinaryBuilder,
  newBinaryBuilder,
  newLargeBinaryBuilder,
  appendBytes,
  appendBytesMaybe,
) where

import Arrow.Column.Internal
import Control.Monad.Primitive (PrimMonad, PrimState, unsafeIOToPrim)
import Data.Bits (complement, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, setPrimArray, writePrimArray)
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))


-- | Operations every builder supports.
class ColumnBuilder b where
  -- | Append a null row.
  appendNull :: PrimMonad m => b (PrimState m) -> m ()

  -- | Rows appended so far.
  builderLength :: PrimMonad m => b (PrimState m) -> m Int

  -- | Freeze into a column and reset the builder to empty.
  freezeBuilder :: PrimMonad m => b (PrimState m) -> m ColumnArray


-- ============================================================
-- Growable byte buffers
-- ============================================================

-- | A growable pinned byte buffer: the memory and its capacity in bytes.
data Grow s = Grow !(MutVar s (ForeignPtr Word8)) !(MutablePrimArray s Int)


{-# INLINABLE newGrow #-}
newGrow :: PrimMonad m => Int -> m (Grow (PrimState m))
newGrow cap0 = do
  let !cap = max 0 cap0
  fp <- unsafeIOToPrim $ do
    fp <- mallocAligned cap
    unsafeWithForeignPtr fp $ \p -> fillBytes p 0 cap
    pure fp
  ref <- newMutVar fp
  meta <- newPrimArray 1
  writePrimArray meta 0 cap
  pure (Grow ref meta)


-- | Make room for @need@ bytes, keeping the first @used@ bytes and zeroing the rest.
ensureGrow :: PrimMonad m => Grow (PrimState m) -> Int -> Int -> m ()
ensureGrow (Grow ref meta) !used !need = do
  cap <- readPrimArray meta 0
  if need <= cap
    then pure ()
    else do
      let !cap' = max need (max 64 (cap * 2))
      old <- readMutVar ref
      new <- unsafeIOToPrim $ do
        fp <- mallocAligned cap'
        unsafeWithForeignPtr fp $ \dst -> unsafeWithForeignPtr old $ \src -> do
          copyBytes dst src used
          fillBytes (dst `plusPtr` used) 0 (cap' - used)
        pure fp
      writeMutVar ref new
      writePrimArray meta 0 cap'
{-# INLINE ensureGrow #-}


-- | Hand the memory out and replace it with an empty allocation.
{-# INLINABLE takeGrow #-}
takeGrow :: PrimMonad m => Grow (PrimState m) -> m (ForeignPtr Word8)
takeGrow (Grow ref meta) = do
  fp <- readMutVar ref
  fresh <- unsafeIOToPrim (mallocAligned 0)
  writeMutVar ref fresh
  writePrimArray meta 0 0
  pure fp


-- | Validity under construction: the bits, plus [null count, materialised flag].
data ValidBuf s = ValidBuf !(Grow s) !(MutablePrimArray s Int)


{-# INLINABLE newValidBuf #-}
newValidBuf :: PrimMonad m => m (ValidBuf (PrimState m))
newValidBuf = do
  g <- newGrow 0
  meta <- newPrimArray 2
  setPrimArray meta 0 2 0
  pure (ValidBuf g meta)


writeBit :: PrimMonad m => Grow (PrimState m) -> Int -> Bool -> m ()
writeBit g@(Grow ref _) !i !b = do
  let !byte = i `unsafeShiftR` 3
  ensureGrow g byte (byte + 1)
  fp <- readMutVar ref
  unsafeIOToPrim $ unsafeWithForeignPtr fp $ \p -> do
    x <- peekByteOff p byte :: IO Word8
    let !m = 1 `unsafeShiftL` (i .&. 7)
    pokeByteOff p byte (if b then x .|. m else x .&. complement m)
{-# INLINE writeBit #-}


-- | Row @i@ is valid.
validAt :: PrimMonad m => ValidBuf (PrimState m) -> Int -> m ()
validAt (ValidBuf g meta) !i = do
  materialised <- readPrimArray meta 1
  if materialised == 0 then pure () else writeBit g i True
{-# INLINE validAt #-}


-- | Row @i@ is null; rows before it that were appended valid get their bits now.
{-# INLINE nullAt #-}
nullAt :: PrimMonad m => ValidBuf (PrimState m) -> Int -> m ()
nullAt (ValidBuf g@(Grow ref _) meta) !i = do
  materialised <- readPrimArray meta 1
  if materialised /= 0
    then pure ()
    else do
      let !full = i `unsafeShiftR` 3
      ensureGrow g 0 (full + 1)
      fp <- readMutVar ref
      unsafeIOToPrim $ unsafeWithForeignPtr fp $ \p -> do
        fillBytes p 0xFF full
        pokeByteOff p full ((1 `unsafeShiftL` (i .&. 7)) - 1 :: Word8)
      writePrimArray meta 1 1
  writeBit g i False
  n <- readPrimArray meta 0
  writePrimArray meta 0 (n + 1)


{-# INLINABLE freezeValid #-}
freezeValid :: PrimMonad m => ValidBuf (PrimState m) -> Int -> m (Maybe Validity)
freezeValid (ValidBuf g meta) !rows = do
  nulls <- readPrimArray meta 0
  fp <- takeGrow g
  setPrimArray meta 0 2 0
  pure $
    if nulls == 0
      then Nothing
      else Just (Validity (Bitmap (BSI.BS fp ((rows + 7) `unsafeShiftR` 3)) 0 rows) nulls)


-- ============================================================
-- Fixed width
-- ============================================================

-- | Builder for a fixed-width column of tag @PrimType a@.
data PrimBuilder a s = PrimBuilder !(PrimType a) !(Grow s) !(ValidBuf s) !(MutablePrimArray s Int)


-- | A builder with room for the given number of rows before it first grows.
{-# INLINABLE newPrimBuilder #-}
newPrimBuilder :: PrimMonad m => PrimType a -> Int -> m (PrimBuilder a (PrimState m))
newPrimBuilder t hint = do
  g <- newGrow (max 0 hint * primWidth t)
  vb <- newValidBuf
  len <- newPrimArray 1
  writePrimArray len 0 0
  pure (PrimBuilder t g vb len)


appendPrim :: (PrimMonad m, Storable a) => PrimBuilder a (PrimState m) -> a -> m ()
appendPrim (PrimBuilder _ g@(Grow ref _) vb lenRef) x = do
  n <- readPrimArray lenRef 0
  let !w = sizeOf x
  ensureGrow g (n * w) ((n + 1) * w)
  fp <- readMutVar ref
  unsafeIOToPrim $ unsafeWithForeignPtr fp $ \p -> pokeElemOff (castPtr p) n x
  validAt vb n
  writePrimArray lenRef 0 (n + 1)
{-# INLINE appendPrim #-}


appendPrimMaybe :: (PrimMonad m, Storable a) => PrimBuilder a (PrimState m) -> Maybe a -> m ()
appendPrimMaybe b = maybe (appendNull b) (appendPrim b)
{-# INLINE appendPrimMaybe #-}


instance ColumnBuilder (PrimBuilder a) where
  appendNull (PrimBuilder t g vb lenRef) = do
    n <- readPrimArray lenRef 0
    let !w = primWidth t
    ensureGrow g (n * w) ((n + 1) * w) -- new bytes are zeroed
    nullAt vb n
    writePrimArray lenRef 0 (n + 1)
  builderLength (PrimBuilder _ _ _ lenRef) = readPrimArray lenRef 0
  {-# INLINE appendNull #-}
  {-# INLINE builderLength #-}
  {-# INLINABLE freezeBuilder #-}
  freezeBuilder (PrimBuilder t g vb lenRef) = withPrim t $ do
    n <- readPrimArray lenRef 0
    fp <- takeGrow g
    v <- freezeValid vb n
    writePrimArray lenRef 0 0
    pure (ColPrim t v (VS.unsafeFromForeignPtr0 (castForeignPtr fp) n))


-- ============================================================
-- Booleans
-- ============================================================

data BoolBuilder s = BoolBuilder !(Grow s) !(ValidBuf s) !(MutablePrimArray s Int)


{-# INLINABLE newBoolBuilder #-}
newBoolBuilder :: PrimMonad m => Int -> m (BoolBuilder (PrimState m))
newBoolBuilder hint = do
  g <- newGrow ((max 0 hint + 7) `unsafeShiftR` 3)
  vb <- newValidBuf
  len <- newPrimArray 1
  writePrimArray len 0 0
  pure (BoolBuilder g vb len)


appendBool :: PrimMonad m => BoolBuilder (PrimState m) -> Bool -> m ()
appendBool (BoolBuilder g vb lenRef) x = do
  n <- readPrimArray lenRef 0
  writeBit g n x
  validAt vb n
  writePrimArray lenRef 0 (n + 1)
{-# INLINE appendBool #-}


appendBoolMaybe :: PrimMonad m => BoolBuilder (PrimState m) -> Maybe Bool -> m ()
appendBoolMaybe b = maybe (appendNull b) (appendBool b)
{-# INLINE appendBoolMaybe #-}


instance ColumnBuilder BoolBuilder where
  appendNull (BoolBuilder g vb lenRef) = do
    n <- readPrimArray lenRef 0
    writeBit g n False
    nullAt vb n
    writePrimArray lenRef 0 (n + 1)
  builderLength (BoolBuilder _ _ lenRef) = readPrimArray lenRef 0
  {-# INLINE appendNull #-}
  {-# INLINE builderLength #-}
  {-# INLINABLE freezeBuilder #-}
  freezeBuilder (BoolBuilder g vb lenRef) = do
    n <- readPrimArray lenRef 0
    -- A builder with no rows may never have allocated a byte.
    let !nbytes = (n + 7) `unsafeShiftR` 3
    ensureGrow g nbytes nbytes
    fp <- takeGrow g
    v <- freezeValid vb n
    writePrimArray lenRef 0 0
    pure (ColBool v (Bitmap (BSI.BS fp nbytes) 0 n))


-- ============================================================
-- Var-length
-- ============================================================

{- | Offsets, data, validity, [rows, data length], and the constructor
for the frozen column.
-}
data VarBuilder o s
  = VarBuilder
      !(Grow s)
      !(Grow s)
      !(ValidBuf s)
      !(MutablePrimArray s Int)
      !(Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray)


{-# INLINABLE newVarBuilder #-}
newVarBuilder
  :: forall o m
   . (PrimMonad m, Offset o)
  => (Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray)
  -> Int
  -> m (VarBuilder o (PrimState m))
newVarBuilder mk hint = do
  let !w = sizeOf (0 :: o)
  offs <- newGrow ((max 0 hint + 1) * w)
  ensureGrow offs 0 w -- offset 0, zeroed
  dat <- newGrow (max 0 hint * 8)
  vb <- newValidBuf
  meta <- newPrimArray 2
  setPrimArray meta 0 2 0
  pure (VarBuilder offs dat vb meta mk)


-- | Append one row of @len@ bytes copied by the action.
appendVar :: forall o m. (PrimMonad m, Offset o) => VarBuilder o (PrimState m) -> Int -> (Ptr Word8 -> IO ()) -> m ()
appendVar (VarBuilder offs@(Grow offRef _) dat@(Grow datRef _) vb meta _) !len copy = do
  n <- readPrimArray meta 0
  used <- readPrimArray meta 1
  let !end = used + len
      !w = sizeOf (0 :: o)
  if toInteger end > toInteger (maxBound :: o)
    then errorWithoutStackTrace "Arrow.Column.Builder: var-length data exceeds the offset width (use a large builder)"
    else pure ()
  ensureGrow dat used end
  fp <- readMutVar datRef
  unsafeIOToPrim $ unsafeWithForeignPtr fp $ \p -> copy (p `plusPtr` used)
  ensureGrow offs ((n + 1) * w) ((n + 2) * w)
  ofp <- readMutVar offRef
  unsafeIOToPrim $ unsafeWithForeignPtr ofp $ \p -> pokeElemOff (castPtr p) (n + 1) (fromIntegral end :: o)
  validAt vb n
  writePrimArray meta 0 (n + 1)
  writePrimArray meta 1 end
{-# INLINE appendVar #-}


{-# INLINE appendVarNull #-}
appendVarNull :: forall o m. (PrimMonad m, Offset o) => VarBuilder o (PrimState m) -> m ()
appendVarNull (VarBuilder offs@(Grow offRef _) _ vb meta _) = do
  n <- readPrimArray meta 0
  used <- readPrimArray meta 1
  let !w = sizeOf (0 :: o)
  ensureGrow offs ((n + 1) * w) ((n + 2) * w)
  ofp <- readMutVar offRef
  unsafeIOToPrim $ unsafeWithForeignPtr ofp $ \p -> pokeElemOff (castPtr p) (n + 1) (fromIntegral used :: o)
  nullAt vb n
  writePrimArray meta 0 (n + 1)


{-# INLINABLE freezeVar #-}
freezeVar :: forall o m. (PrimMonad m, Offset o) => VarBuilder o (PrimState m) -> m ColumnArray
freezeVar (VarBuilder offs dat vb meta mk) = do
  n <- readPrimArray meta 0
  used <- readPrimArray meta 1
  ofp <- takeGrow offs
  dfp <- takeGrow dat
  v <- freezeValid vb n
  setPrimArray meta 0 2 0
  -- Restart with offset 0 in place for the next column.
  ensureGrow offs 0 (sizeOf (0 :: o))
  pure (mk v (VS.unsafeFromForeignPtr0 (castForeignPtr ofp) (n + 1)) (BSI.BS dfp used))


-- | Builder for a utf8 column (32-bit offsets) or large utf8 column (64-bit).
newtype Utf8Builder o s = Utf8Builder (VarBuilder o s)


-- | Builder for a binary column (32-bit offsets) or large binary column (64-bit).
newtype BinaryBuilder o s = BinaryBuilder (VarBuilder o s)


newUtf8Builder :: PrimMonad m => Int -> m (Utf8Builder Int32 (PrimState m))
newUtf8Builder hint = Utf8Builder <$> newVarBuilder ColUtf8 hint


newLargeUtf8Builder :: PrimMonad m => Int -> m (Utf8Builder Int64 (PrimState m))
newLargeUtf8Builder hint = Utf8Builder <$> newVarBuilder ColLargeUtf8 hint


newBinaryBuilder :: PrimMonad m => Int -> m (BinaryBuilder Int32 (PrimState m))
newBinaryBuilder hint = BinaryBuilder <$> newVarBuilder ColBinary hint


newLargeBinaryBuilder :: PrimMonad m => Int -> m (BinaryBuilder Int64 (PrimState m))
newLargeBinaryBuilder hint = BinaryBuilder <$> newVarBuilder ColLargeBinary hint


-- | Append the UTF-8 bytes of a 'Text' (one copy, no validation needed).
appendText :: (PrimMonad m, Offset o) => Utf8Builder o (PrimState m) -> Text -> m ()
appendText (Utf8Builder vb) t = appendVar vb (TF.lengthWord8 t) (TF.unsafeCopyToPtr t)
{-# INLINE appendText #-}


appendTextMaybe :: (PrimMonad m, Offset o) => Utf8Builder o (PrimState m) -> Maybe Text -> m ()
appendTextMaybe b = maybe (appendNull b) (appendText b)
{-# INLINE appendTextMaybe #-}


appendBytes :: (PrimMonad m, Offset o) => BinaryBuilder o (PrimState m) -> ByteString -> m ()
appendBytes (BinaryBuilder vb) bs =
  appendVar vb (BS.length bs) $ \dst -> BSU.unsafeUseAsCStringLen bs $ \(src, l) -> copyBytes dst (castPtr src) l
{-# INLINE appendBytes #-}


appendBytesMaybe :: (PrimMonad m, Offset o) => BinaryBuilder o (PrimState m) -> Maybe ByteString -> m ()
appendBytesMaybe b = maybe (appendNull b) (appendBytes b)
{-# INLINE appendBytesMaybe #-}


instance Offset o => ColumnBuilder (Utf8Builder o) where
  appendNull (Utf8Builder vb) = appendVarNull vb
  {-# INLINE appendNull #-}
  {-# INLINE builderLength #-}
  {-# INLINE freezeBuilder #-}
  builderLength (Utf8Builder (VarBuilder _ _ _ meta _)) = readPrimArray meta 0
  freezeBuilder (Utf8Builder vb) = freezeVar vb


instance Offset o => ColumnBuilder (BinaryBuilder o) where
  appendNull (BinaryBuilder vb) = appendVarNull vb
  {-# INLINE appendNull #-}
  {-# INLINE builderLength #-}
  {-# INLINE freezeBuilder #-}
  builderLength (BinaryBuilder (VarBuilder _ _ _ meta _)) = readPrimArray meta 0
  freezeBuilder (BinaryBuilder vb) = freezeVar vb
