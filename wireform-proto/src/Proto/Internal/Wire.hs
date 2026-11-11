{- | Wire format types and tag encoding/decoding.

The protobuf wire format uses a tag-length-value scheme where each field
is preceded by a tag encoding the field number and wire type.
-}
module Proto.Internal.Wire (
  -- * Wire types
  WireType (..),
  wireTypeFromTag,
  wireTypeToWord,

  -- * Tags
  Tag (..),
  makeTag,
  encodeTag,
  decodeTag,
  validTagWord,

  -- * Field key
  fieldTag,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word32, Word64)


-- | Protobuf wire types.
data WireType
  = WireVarint -- 0: int32, int64, uint32, uint64, sint32, sint64, bool, enum
  | Wire64Bit -- 1: fixed64, sfixed64, double
  | WireLengthDelimited -- 2: string, bytes, embedded messages, packed repeated fields
  | WireStartGroup -- 3: deprecated
  | WireEndGroup -- 4: deprecated
  | Wire32Bit -- 5: fixed32, sfixed32, float
  deriving stock (Show, Eq, Ord, Enum, Bounded)


-- | Extract the wire type from the low 3 bits of a tag value.
wireTypeFromTag :: Word32 -> Maybe WireType
wireTypeFromTag n = case n .&. 0x07 of
  0 -> Just WireVarint
  1 -> Just Wire64Bit
  2 -> Just WireLengthDelimited
  3 -> Just WireStartGroup
  4 -> Just WireEndGroup
  5 -> Just Wire32Bit
  _ -> Nothing


-- | Convert a 'WireType' to its numeric encoding (0-5).
wireTypeToWord :: WireType -> Word32
wireTypeToWord = \case
  WireVarint -> 0
  Wire64Bit -> 1
  WireLengthDelimited -> 2
  WireStartGroup -> 3
  WireEndGroup -> 4
  Wire32Bit -> 5


-- | A decoded tag: field number + wire type.
data Tag = Tag
  { tagFieldNumber :: {-# UNPACK #-} !Int
  , tagWireType :: !WireType
  }
  deriving stock (Show, Eq)


-- | Construct a tag from a field number and wire type.
makeTag :: Int -> WireType -> Tag
makeTag = Tag


-- | Encode a tag as a single Word64 value (for varint encoding).
encodeTag :: Tag -> Word64
encodeTag (Tag fn wt) =
  fromIntegral fn `shiftL` 3 .|. fromIntegral (wireTypeToWord wt)


-- | Check the field number and wire type before narrowing the tag.
validTagWord :: Word64 -> Bool
validTagWord w = w >= 8 && w <= fromIntegral (maxBound :: Word32) && w .&. 7 <= 5
{-# INLINE validTagWord #-}


-- | Decode a tag from a varint value.
decodeTag :: Word64 -> Maybe Tag
decodeTag w
  | validTagWord w = Tag (fromIntegral (w `shiftR` 3)) <$> wireTypeFromTag (fromIntegral w)
  | otherwise = Nothing


-- | Convenience: make the wire tag value for a given field number and wire type.
fieldTag :: Int -> WireType -> Word64
fieldTag fn wt = encodeTag (makeTag fn wt)
