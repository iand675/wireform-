{-# LANGUAGE BangPatterns #-}

{- | Church-encoded sized builder: fuses size calculation with serialization.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

A 'SizedBuilder' carries both the computed byte size and the builder
in a single value. This avoids the need for a separate size-computation
pass when encoding submessages — the size is accumulated alongside
the builder as fields are appended.

'toByteString' allocates a single strict ByteString of exactly the
right size and fills it in one pass — no intermediate lazy chunks
or recopying.
-}
module Proto.Internal.SizedBuilder (
  -- * Core type
  SizedBuilder,

  -- * Construction
  empty,
  sized,

  -- * Running (strict ByteString — single allocation)
  toByteString,
  toByteStringFromBuilder,
  toBuilder,
  size,
  toLazyByteString,

  -- * Combinators
  withSubMessage,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Proto.Internal.Wire.Encode (putVarint, varintSize)
import Wireform.Builder qualified as B


-- | A builder that tracks its own byte size.
data SizedBuilder = SizedBuilder
  { sbSize :: {-# UNPACK #-} !Int
  , sbBuilder :: !B.Builder
  }


instance Semigroup SizedBuilder where
  SizedBuilder s1 b1 <> SizedBuilder s2 b2 =
    SizedBuilder (s1 + s2) (b1 <> b2)
  {-# INLINE (<>) #-}


instance Monoid SizedBuilder where
  mempty = SizedBuilder 0 mempty
  {-# INLINE mempty #-}


-- | The empty 'SizedBuilder' (zero size, empty builder).
empty :: SizedBuilder
empty = mempty
{-# INLINE empty #-}


-- | Construct a 'SizedBuilder' from a known byte size and a builder.
sized :: Int -> B.Builder -> SizedBuilder
sized = SizedBuilder
{-# INLINE sized #-}


-- | Extract the underlying builder, discarding the size.
toBuilder :: SizedBuilder -> B.Builder
toBuilder = sbBuilder
{-# INLINE toBuilder #-}


-- | Get the precomputed byte size.
size :: SizedBuilder -> Int
size = sbSize
{-# INLINE size #-}


{- | Produce a strict ByteString. Pre-allocates a buffer of exactly
the right size (from 'sbSize') — no reallocation, no IORef.
-}
toByteString :: SizedBuilder -> ByteString
toByteString (SizedBuilder sz bld) = B.toStrictByteStringExact sz bld
{-# INLINE toByteString #-}


{- | Produce a strict ByteString from a Builder when the exact size
is known. Pre-allocates a buffer of @sz@ bytes — no reallocation,
no IORef.
-}
toByteStringFromBuilder :: Int -> B.Builder -> ByteString
toByteStringFromBuilder = B.toStrictByteStringExact
{-# INLINE toByteStringFromBuilder #-}


-- | Produce a lazy 'BL.ByteString' from the builder.
toLazyByteString :: SizedBuilder -> BL.ByteString
toLazyByteString (SizedBuilder _sz bld) = B.toLazyByteString bld
{-# INLINE toLazyByteString #-}


-- | Wrap a 'SizedBuilder' as a length-delimited submessage (prepends the varint length prefix).
withSubMessage :: SizedBuilder -> SizedBuilder
withSubMessage sb =
  let !payloadSize = sbSize sb
      !lenPrefixBuilder = putVarint (fromIntegral payloadSize)
  in SizedBuilder
      { sbSize = varintSize (fromIntegral payloadSize) + payloadSize
      , sbBuilder = lenPrefixBuilder <> sbBuilder sb
      }
{-# INLINE withSubMessage #-}
