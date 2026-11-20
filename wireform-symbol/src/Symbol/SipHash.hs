{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Symbol.SipHash
Description : Keyed SipHash over byte slices, without copying them.

SipHash (Aumasson & Bernstein, 2012) is a keyed pseudo-random function: an
attacker who does not know the 128-bit key cannot choose inputs that
collide, which is what makes a hash table keyed by it safe to fill with
attacker-controlled strings (HTTP header names, say).

'sipHash13' is the 1-compression-round / 3-finalization-round variant Rust's
standard library and CPython (from 3.11) use for hash tables; 'sipHash24' is
the paper's conservative variant, used by the test suite against the
published vectors. Both read 8-byte words straight out of the source, a
'ByteArray#' slice or raw memory, so hashing never allocates.

Words are read in host byte order. On little-endian hosts (x86-64,
AArch64) that is the specified order and the outputs match the reference;
a big-endian host would get a different, equally keyed, function.
-}
module Symbol.SipHash
  ( SipKey (..)
  , newSipKey
  , Bytes (..)
  , bytesLength
  , sipHash13
  , sipHash24
  ) where

import Data.Bits (rotateL, unsafeShiftL, xor, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import GHC.Exts
import GHC.Word (Word64 (..), Word8 (..))
import System.Entropy (getEntropy)

-- | A 128-bit SipHash key.
data SipKey = SipKey {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving stock (Eq, Show)

-- | A key drawn from the operating system's entropy source.
newSipKey :: IO SipKey
newSipKey = do
  bs <- getEntropy 16
  let word i = foldr (\j acc -> (acc `unsafeShiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs (i + j))) 0 [7, 6 .. 0]
  if BS.length bs /= 16
    then ioError (userError "Symbol.SipHash.newSipKey: short read from the entropy source")
    else pure (SipKey (word 0) (word 8))

{- | Bytes to hash or compare, borrowed rather than copied.

'BytesAddr' points at memory the caller keeps alive (a 'BS.ByteString''s
buffer inside 'BSU.unsafeUseAsCStringLen', say) for as long as the value is
used.
-}
data Bytes
  = -- | array, byte offset, length
    BytesBA ByteArray# Int# Int#
  | -- | address, length
    BytesAddr Addr# Int#

bytesLength :: Bytes -> Int
bytesLength (BytesBA _ _ n) = I# n
bytesLength (BytesAddr _ n) = I# n
{-# INLINE bytesLength #-}

-- | The 8 bytes at offset @i@, host order, unaligned.
word64At :: Bytes -> Int -> Word64
word64At (BytesBA ba off _) (I# i) = W64# (indexWord8ArrayAsWord64# ba (off +# i))
word64At (BytesAddr a _) (I# i) = W64# (indexWord8OffAddrAsWord64# a i)
{-# INLINE word64At #-}

byteAt :: Bytes -> Int -> Word64
byteAt (BytesBA ba off _) (I# i) = fromIntegral (W8# (indexWord8Array# ba (off +# i)))
byteAt (BytesAddr a _) (I# i) = fromIntegral (W8# (indexWord8OffAddr# a i))
{-# INLINE byteAt #-}

-- | SipHash-1-3.
sipHash13 :: SipKey -> Bytes -> Word64
sipHash13 = sipHash 1 3
{-# INLINE sipHash13 #-}

-- | SipHash-2-4.
sipHash24 :: SipKey -> Bytes -> Word64
sipHash24 = sipHash 2 4
{-# INLINE sipHash24 #-}

data V = V {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

sipRound :: V -> V
sipRound (V v0 v1 v2 v3) =
  let a0 = v0 + v1
      a1 = rotateL v1 13 `xor` a0
      a0' = rotateL a0 32
      b2 = v2 + v3
      b3 = rotateL v3 16 `xor` b2
      c0 = a0' + b3
      c3 = rotateL b3 21 `xor` c0
      c2 = b2 + a1
      c1 = rotateL a1 17 `xor` c2
   in V c0 c1 (rotateL c2 32) c3
{-# INLINE sipRound #-}

rounds :: Int -> V -> V
rounds 1 v = sipRound v
rounds 2 v = sipRound (sipRound v)
rounds 3 v = sipRound (sipRound (sipRound v))
rounds 4 v = sipRound (sipRound (sipRound (sipRound v)))
rounds n v = rounds (n - 1) (sipRound v)
{-# INLINE rounds #-}

compress :: Int -> Word64 -> V -> V
compress c m (V v0 v1 v2 v3) = case rounds c (V v0 v1 v2 (v3 `xor` m)) of
  V w0 w1 w2 w3 -> V (w0 `xor` m) w1 w2 w3
{-# INLINE compress #-}

sipHash :: Int -> Int -> SipKey -> Bytes -> Word64
sipHash c d (SipKey k0 k1) src = finish (compress c lastBlock (go 0 v0))
 where
  !len = bytesLength src
  !full = len - (len `rem` 8)
  v0 =
    V
      (k0 `xor` 0x736f6d6570736575)
      (k1 `xor` 0x646f72616e646f6d)
      (k0 `xor` 0x6c7967656e657261)
      (k1 `xor` 0x7465646279746573)
  go !i !v
    | i >= full = v
    | otherwise = go (i + 8) (compress c (word64At src i) v)
  tailBytes !j !acc
    | j <= full = acc
    | otherwise = tailBytes (j - 1) ((acc `unsafeShiftL` 8) .|. byteAt src (j - 1))
  lastBlock = (fromIntegral len `unsafeShiftL` 56) .|. tailBytes len 0
  finish (V w0 w1 w2 w3) = case rounds d (V w0 w1 (w2 `xor` 0xff) w3) of
    V z0 z1 z2 z3 -> z0 `xor` z1 `xor` z2 `xor` z3
{-# INLINE sipHash #-}
