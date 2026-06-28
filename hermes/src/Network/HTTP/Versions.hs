{-# LANGUAGE PatternSynonyms #-}

{- | HTTP protocol versions — the canonical version type for the whole
wireform HTTP stack (@wireform-http1@ and @wireform-http@ both
re-export it).

A 'HTTPVersion' packs @major.minor@ into a single 'Word8' (4 bits
each), so it fits in one machine register. Comparison is
lexicographic on @(major, minor)@, i.e.
@HTTP0_9 < HTTP1_0 < HTTP1_1 < HTTP2 < HTTP3@. The pattern synonyms
cover every version the codebase speaks; arbitrary versions are
representable via 'mkVersion'.
-}
module Network.HTTP.Versions (
  HTTPVersion,
  mkVersion,
  versionMajor,
  versionMinor,
  versionToBytes,
  versionFromBytes,

  -- * Common versions
  pattern HTTP0_9,
  pattern HTTP1_0,
  pattern HTTP1_1,
  pattern HTTP2,
  pattern HTTP3,
) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)


-- | A packed @major.minor@ HTTP version.
newtype HTTPVersion = HTTPVersion Word8
  deriving stock (Eq, Generic)
  deriving newtype (Hashable, NFData)


instance Show HTTPVersion where
  showsPrec _ v =
    showString "mkVersion "
      . shows (versionMajor v)
      . showString " "
      . shows (versionMinor v)


instance Ord HTTPVersion where
  compare a b = case compare (versionMajor a) (versionMajor b) of
    EQ -> compare (versionMinor a) (versionMinor b)
    other -> other


{- | Build a 'HTTPVersion' from its @major.minor@ digits. Each
component must fit in 4 bits (0..15); larger values are saturated
to 15.
-}
{-# INLINE mkVersion #-}
mkVersion :: Word8 -> Word8 -> HTTPVersion
mkVersion major minor = HTTPVersion (sat major `shiftL` 4 .|. sat minor)
  where
    sat w = if w > 15 then 15 else w


{-# INLINE versionMajor #-}
versionMajor :: HTTPVersion -> Word8
versionMajor (HTTPVersion w) = w `shiftR` 4


{-# INLINE versionMinor #-}
versionMinor :: HTTPVersion -> Word8
versionMinor (HTTPVersion w) = w .&. 0x0F


-- | Render the canonical on-the-wire spelling of a 'HTTPVersion'.
versionToBytes :: HTTPVersion -> ByteString
versionToBytes v = case (versionMajor v, versionMinor v) of
  (0, 9) -> "HTTP/0.9"
  (1, 0) -> "HTTP/1.0"
  (1, 1) -> "HTTP/1.1"
  (2, 0) -> "HTTP/2"
  (3, 0) -> "HTTP/3"
  (mj, mn) ->
    "HTTP/" <> BS.singleton (digit mj) <> "." <> BS.singleton (digit mn)
  where
    digit n
      | n < 10 = 0x30 + n
      | otherwise = 0x3F -- '?'; only reachable if mkVersion's mask is bypassed


{- | Strict reverse of 'versionToBytes'. Only the canonical spellings
are recognised; everything else returns 'Nothing'.
-}
versionFromBytes :: ByteString -> Maybe HTTPVersion
versionFromBytes bs
  | bs == "HTTP/1.1" = Just HTTP1_1
  | bs == "HTTP/1.0" = Just HTTP1_0
  | bs == "HTTP/2" = Just HTTP2
  | bs == "HTTP/2.0" = Just HTTP2
  | bs == "HTTP/3" = Just HTTP3
  | bs == "HTTP/3.0" = Just HTTP3
  | bs == "HTTP/0.9" = Just HTTP0_9
  | otherwise = Nothing


pattern HTTP0_9 :: HTTPVersion
pattern HTTP0_9 = HTTPVersion 0x09


pattern HTTP1_0 :: HTTPVersion
pattern HTTP1_0 = HTTPVersion 0x10


pattern HTTP1_1 :: HTTPVersion
pattern HTTP1_1 = HTTPVersion 0x11


pattern HTTP2 :: HTTPVersion
pattern HTTP2 = HTTPVersion 0x20


pattern HTTP3 :: HTTPVersion
pattern HTTP3 = HTTPVersion 0x30
