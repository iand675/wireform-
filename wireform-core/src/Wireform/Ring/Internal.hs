{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Internal module exposing the unsafe machinery behind 'MagicRing'.

The phantom @s@ on 'MagicRing' acts like the @s@ on 'Control.Monad.ST.ST':
it is sealed inside 'withMagicRing' with a rank-2 @forall@, so values
that carry the @s@ — most importantly 'RingSlice' — cannot leak out
of the scope in which the ring is alive (and therefore cannot
dangle through a refill that overwrites the bytes they point at).

Code that genuinely needs the un-scoped variants ('newMagicRing',
'destroyMagicRing', the raw @ringBase@ pointer) lives here.
Day-to-day callers should prefer the safer surface in "Wireform.Ring".
-}
module Wireform.Ring.Internal (
  -- * The ring
  MagicRing (..),
  newMagicRing,
  destroyMagicRing,
  withMagicRing,
  ringBase,
  ringSize,
  ringMask,
  MagicRingException (..),

  -- * Slices
  RingSlice (..),
  ringSlice,
  ringSliceAtPos,
  ringSliceLength,
  ringSliceBase,
  withRingSlice,
  peekRingSliceByte,
  copyRingSlice,

  -- * Unsafe internals (no scoping guarantees)
  unsafeRingSliceFromPtr,
  unsafeRingScope,
) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peek, peekByteOff, poke, pokeByteOff)


------------------------------------------------------------------------
-- MagicRing
------------------------------------------------------------------------

{- | Opaque handle to a double-mapped ring buffer.

The phantom @s@ is /nominal/: it has no run-time content, only the
type-level role of preventing slices from escaping their ring's
scope (see 'withMagicRing' / 'RingSlice').
-}
data MagicRing s = MagicRing
  { mrBase :: {-# UNPACK #-} !(Ptr Word8)
  , mrSize :: {-# UNPACK #-} !Int
  }


type role MagicRing nominal


-- C struct layout: { void *base; size_t size; }
-- On Windows there's an extra HANDLE field, but we don't need it from
-- the Haskell side — hs_ring_destroy handles cleanup.
data CRing = CRing
  { crBase :: {-# UNPACK #-} !(Ptr Word8)
  , crSize :: {-# UNPACK #-} !CSize
  }


instance Storable CRing where
  sizeOf _ = sizeOf (undefined :: Ptr ()) + sizeOf (undefined :: CSize)


  -- conservative: ignores padding. On all target platforms
  -- both fields are pointer-sized and naturally aligned.
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    b <- peekByteOff p 0
    s <- peekByteOff p (sizeOf (undefined :: Ptr ()))
    pure (CRing b s)
  poke p (CRing b s) = do
    pokeByteOff p 0 b
    pokeByteOff p (sizeOf (undefined :: Ptr ())) s


foreign import capi unsafe "magic_ring.h hs_ring_create"
  c_ring_create :: CSize -> Ptr CRing -> IO CInt


foreign import capi unsafe "magic_ring.h hs_ring_destroy"
  c_ring_destroy :: Ptr CRing -> IO ()


data MagicRingException
  = MagicRingUnavailable !String
  deriving stock (Show, Typeable)


instance Exception MagicRingException


{- | Allocate a magic ring of at least the requested size (in bytes).
The actual size is rounded up to the nearest page-size multiple and
power of two.  Throws 'MagicRingUnavailable' on failure.

The returned ring is polymorphic in @s@: callers either pair it
with 'destroyMagicRing' manually (no scope safety) or, preferably,
use 'withMagicRing' which binds @s@ inside a rank-2 scope.
-}
newMagicRing :: Int -> IO (MagicRing s)
newMagicRing requested = alloca $ \p -> do
  poke p (CRing nullPtr 0)
  rc <- c_ring_create (fromIntegral requested) p
  if rc /= 0
    then
      throwIO
        ( MagicRingUnavailable $
            "hs_ring_create failed for requested size " <> show requested
        )
    else do
      cr <- peek p
      pure
        MagicRing
          { mrBase = crBase cr
          , mrSize = fromIntegral (crSize cr)
          }


{- | Release the ring's virtual mappings.  Call it exactly once per ring:
each call passes a fresh copy of the base pointer to C, so a second call
unmaps whatever has been mapped at that address since (callers that may
close twice need their own once-guard, as
"Wireform.Network.Transport.Duplex" does).
-}
destroyMagicRing :: MagicRing s -> IO ()
destroyMagicRing mr = alloca $ \p -> do
  poke p (CRing (mrBase mr) (fromIntegral (mrSize mr)))
  c_ring_destroy p


{- | Scoped allocation, modelled after 'Control.Monad.ST.runST'.

The body is rank-2: the @s@ inside the body is a fresh skolem that
cannot unify with anything outside the @forall@.  Any value carrying
that @s@ — slices, sub-buffers, anything we add later — therefore
cannot appear in the result type @a@ and cannot escape.

> withMagicRing 4096 $ \\ring -> do
>   let slice = ringSlice ring 0 16
>   copyRingSlice slice            -- OK, copy escapes as a fresh ByteString
>   pure slice                     -- type error: RingSlice s escapes
-}
withMagicRing :: Int -> (forall s. MagicRing s -> IO a) -> IO a
withMagicRing n action = bracket (newMagicRing n) destroyMagicRing action


-- | Base pointer to the start of the ring (first of the two mappings).
ringBase :: MagicRing s -> Ptr Word8
ringBase = mrBase
{-# INLINE ringBase #-}


-- | Physical size of the ring in bytes (N, not 2N).
ringSize :: MagicRing s -> Int
ringSize = mrSize
{-# INLINE ringSize #-}


{- | Bitmask for cheap @pos .&. ringMask@ indexing.
Always @ringSize - 1@ since the size is a power of two.
-}
ringMask :: MagicRing s -> Int
ringMask mr = mrSize mr - 1
{-# INLINE ringMask #-}


------------------------------------------------------------------------
-- RingSlice
------------------------------------------------------------------------

{- | A contiguous slice into a 'MagicRing'.

'RingSlice' is deliberately /not/ a 'ByteString': the only way to
materialise a 'ByteString' from one is 'copyRingSlice', which performs
a memcpy into freshly-allocated memory.  This makes the cost of
escaping the ring's scope visible at the call site and prevents
accidental retention of a pointer that subsequent refills will
overwrite.

The phantom @s@ ties the slice to its originating ring's scope;
because 'withMagicRing' seals @s@ inside a rank-2 @forall@, a slice
can never outlive the ring it came from when constructed through
the safe API.

The base pointer lies inside the first mapping (@[base, base + N)@)
but, by virtue of the double mapping, callers may read up to
'ringSliceLength' bytes contiguously from it without wrap logic.
-}
data RingSlice s = RingSlice
  { _rsPtr :: {-# UNPACK #-} !(Ptr Word8)
  , _rsLen :: {-# UNPACK #-} !Int
  }


type role RingSlice nominal


{- | Build a slice from a byte offset within the ring and a length.

The offset is taken modulo 'ringSize' so callers can pass an
absolute consumer position directly.  The length may exceed
@ringSize - (offset \`mod\` ringSize)@ — the double mapping makes
the read contiguous as long as @len <= ringSize@.
-}
ringSlice :: MagicRing s -> Int -> Int -> RingSlice s
ringSlice mr offset len =
  let !msk = mrSize mr - 1
      !off = offset .&. msk
      !p = mrBase mr `plusPtr` off
  in RingSlice p len
{-# INLINE ringSlice #-}


{- | Build a slice from an absolute (monotonic) producer/consumer
position and a length — the form the transport layer naturally
speaks.
-}
ringSliceAtPos :: MagicRing s -> Word64 -> Int -> RingSlice s
ringSliceAtPos mr pos len =
  let !msk = mrSize mr - 1
      !off = fromIntegral pos .&. msk
      !p = mrBase mr `plusPtr` off
  in RingSlice p len
{-# INLINE ringSliceAtPos #-}


-- | Slice length in bytes.
ringSliceLength :: RingSlice s -> Int
ringSliceLength (RingSlice _ n) = n
{-# INLINE ringSliceLength #-}


{- | Base pointer of the slice.

Exposed in @.Internal@ only.  The pointer is /not/ tagged with @s@,
so once you reach for it you are responsible for not retaining it
past the ring's lifetime.  Prefer 'withRingSlice' for scoped access.
-}
ringSliceBase :: RingSlice s -> Ptr Word8
ringSliceBase (RingSlice p _) = p
{-# INLINE ringSliceBase #-}


{- | Borrow the slice's pointer + length for an IO action.  The @s@
on 'RingSlice' guarantees the call site is inside the ring's scope;
the body is conventionally responsible for not stashing the pointer
somewhere it could outlive that scope.
-}
withRingSlice :: RingSlice s -> (Ptr Word8 -> Int -> IO a) -> IO a
withRingSlice (RingSlice p n) k = k p n
{-# INLINE withRingSlice #-}


-- | Read a single byte at an offset within the slice.  No bounds check.
peekRingSliceByte :: RingSlice s -> Int -> IO Word8
peekRingSliceByte (RingSlice p _) i = peek (p `plusPtr` i)
{-# INLINE peekRingSliceByte #-}


{- | The escape hatch.  Allocates a fresh 'ByteString' and memcpys the
slice's bytes into it.  The result has no dependency on the ring,
so it can leave the 'withMagicRing' scope freely.
-}
copyRingSlice :: RingSlice s -> IO ByteString
copyRingSlice (RingSlice p n)
  | n <= 0 = pure mempty
  | otherwise = BSI.create n $ \dst -> copyBytes dst p n
{-# INLINE copyRingSlice #-}


------------------------------------------------------------------------
-- Unsafe internals
------------------------------------------------------------------------

{- | Construct a 'RingSlice' directly from a raw pointer + length.

This is the back door used by the parser internals, which already
track bounds against the live ring.  It bypasses the scope check
because the @s@ is freely chosen by the caller; only call this
when you can guarantee, by other means, that the pointer is
inside a live ring.
-}
unsafeRingSliceFromPtr :: Ptr Word8 -> Int -> RingSlice s
unsafeRingSliceFromPtr = RingSlice
{-# INLINE unsafeRingSliceFromPtr #-}


{- | Re-tag a ring under a different @s@.  Like 'unsafeCoerce' for the
phantom — useful when wrapping a long-lived (unsafely-allocated)
ring in a scoped action.  Do not export from "Wireform.Ring".
-}
unsafeRingScope :: MagicRing s -> MagicRing s'
unsafeRingScope (MagicRing b sz) = MagicRing b sz
{-# INLINE unsafeRingScope #-}
