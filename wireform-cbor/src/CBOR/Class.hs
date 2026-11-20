{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typeclass-based CBOR serialization with GHC Generics support.

Provides 'ToCBOR' and 'FromCBOR' typeclasses that can be derived
automatically for record types via @DeriveGeneric@. Records are encoded
as CBOR maps with field names as text-string keys.

@
{\-\# LANGUAGE DeriveGeneric \#-\}
import GHC.Generics (Generic)
import CBOR.Class

data Config = Config { host :: Text, port :: Int } deriving (Generic)
instance ToCBOR Config
instance FromCBOR Config

let bytes = encodeCBOR (Config \"localhost\" 8080)
let Right cfg = decodeCBOR bytes :: Either String Config
@
-}
module CBOR.Class (
  ToCBOR (..),
  FromCBOR (..),
  encodeCBOR,
  encodeCBORDirect,
  decodeCBOR,
  genericToEncoding,
  GToCBOR (..),
  GFromCBOR (..),
  GToCBOREncoding (..),
  GToCBOREncodingFields (..),
) where

import CBOR.Decode qualified as CD
import CBOR.Encode qualified as CE
import CBOR.Encoding (Encoding)
import CBOR.Encoding qualified as Enc
import CBOR.Internal (bignumMagnitude, integerValue)
import CBOR.Value qualified as CV
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as FProduct
import Data.Functor.Sum qualified as FSum
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Mon
import Data.Ord (Down (..))
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Semigroup qualified as Semi
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural (Natural)
import Wireform.Builder qualified as BB


{- | Conversion to CBOR.

Instances should provide 'toCBOR' (the AST conversion). For
performance-sensitive types they /should/ also provide
'toEncoding', which writes directly to a CBOR builder without
constructing an intermediate 'CV.Value'. The default
'toEncoding' implementation falls back to 'toCBOR'.
-}
class ToCBOR a where
  toCBOR :: a -> CV.Value
  default toCBOR :: (Generic a, GToCBOR (Rep a)) => a -> CV.Value
  toCBOR = gToCBOR . from


  {- | Direct-to-bytes encoding. The default delegates to 'toCBOR' for
  backwards compatibility, but instances that want to skip the
  intermediate 'CV.Value' allocation should override this. A
  'Generic'-driven default is available via 'genericToEncoding'
  (see 'GToCBOREncoding').
  -}
  toEncoding :: a -> Encoding
  toEncoding = valueToEncoding . toCBOR


class FromCBOR a where
  fromCBOR :: CV.Value -> Either String a
  default fromCBOR :: (Generic a, GFromCBOR (Rep a)) => CV.Value -> Either String a
  fromCBOR v = to <$> gFromCBOR v


{- | Encode via the AST. Equivalent to @CE.encode . toCBOR@; preserved
for backwards compatibility.
-}
encodeCBOR :: ToCBOR a => a -> ByteString
encodeCBOR = CE.encode . toCBOR


{- | Encode directly via 'toEncoding'. Avoids constructing an
intermediate 'CV.Value' when the instance provides a hand-written
or generically-derived 'toEncoding'.
-}
encodeCBORDirect :: ToCBOR a => a -> ByteString
encodeCBORDirect = Enc.encodingToByteString . toEncoding


{- | Generic 'toEncoding' implementation. Use as

> instance ToCBOR Foo where
>   toEncoding = genericToEncoding

to derive a record encoder that writes straight to a CBOR builder
without first allocating a 'CV.Value'.
-}
genericToEncoding :: (Generic a, GToCBOREncoding (Rep a)) => a -> Encoding
genericToEncoding = gToEncoding . from


decodeCBOR :: FromCBOR a => ByteString -> Either String a
decodeCBOR bs = CD.decode bs >>= fromCBOR


{- | Fallback used by the default 'toEncoding'. Walks an existing
'CV.Value' tree and produces a builder. Any value type that
'CBOR.Encoding' does not have a constructor for goes through
'CE.encode' so the bytes still match the AST encoder.
-}
valueToEncoding :: CV.Value -> Encoding
valueToEncoding v = case v of
  CV.UInt n -> Enc.unsignedInteger n
  CV.NInt n -> Enc.negativeInteger n
  CV.Bool b -> Enc.bool b
  CV.Null -> Enc.null_
  CV.Undefined -> Enc.undefined_
  CV.Float16 f -> Enc.float16 f
  CV.Float32 f -> Enc.float32 f
  CV.Float64 d -> Enc.float64 d
  CV.ByteString bs -> Enc.bytes bs
  CV.TextString t -> Enc.text t
  CV.Array vs -> Enc.array (V.map valueToEncoding vs)
  CV.Map kvs -> Enc.map_ (V.map (\(k, v') -> (valueToEncoding k, valueToEncoding v')) kvs)
  CV.Tag t inner -> Enc.tag t (valueToEncoding inner)
  CV.Simple _ -> Enc.Encoding (BB.byteString (CE.encode v))


-- Helper to convert a Haskell integer to CBOR UInt/NInt
intToCBOR :: Int64 -> CV.Value
intToCBOR n
  | n >= 0 = CV.UInt (fromIntegral n)
  | otherwise = CV.NInt (fromIntegral (negate n - 1))


cborToSigned :: forall a. (Bounded a, Integral a) => CV.Value -> Either String a
cborToSigned (CV.UInt n)
  | n <= fromIntegral (maxBound @a) = Right (fromIntegral n)
  | otherwise = Left "FromCBOR: integer out of range"
cborToSigned (CV.NInt n)
  | n <= fromIntegral (maxBound @a) = Right (-1 - fromIntegral n)
  | otherwise = Left "FromCBOR: integer out of range"
cborToSigned _ = Left "FromCBOR: expected UInt or NInt"
{-# INLINE cborToSigned #-}


cborToUnsigned :: forall a. (Bounded a, Integral a) => CV.Value -> Either String a
cborToUnsigned (CV.UInt n)
  | n <= fromIntegral (maxBound @a) = Right (fromIntegral n)
  | otherwise = Left "FromCBOR: unsigned integer out of range"
cborToUnsigned _ = Left "FromCBOR: expected UInt"
{-# INLINE cborToUnsigned #-}


instance ToCBOR Bool where
  toCBOR = CV.Bool
  toEncoding = Enc.bool


instance FromCBOR Bool where
  fromCBOR (CV.Bool b) = Right b
  fromCBOR _ = Left "FromCBOR Bool: expected Bool"


instance ToCBOR Int where
  toCBOR = intToCBOR . fromIntegral
  toEncoding = Enc.int


instance FromCBOR Int where
  fromCBOR = cborToSigned


instance ToCBOR Int8 where
  toCBOR = intToCBOR . fromIntegral
  toEncoding = Enc.int8


instance FromCBOR Int8 where
  fromCBOR = cborToSigned


instance ToCBOR Int16 where
  toCBOR = intToCBOR . fromIntegral
  toEncoding = Enc.int16


instance FromCBOR Int16 where
  fromCBOR = cborToSigned


instance ToCBOR Int32 where
  toCBOR = intToCBOR . fromIntegral
  toEncoding = Enc.int32


instance FromCBOR Int32 where
  fromCBOR = cborToSigned


instance ToCBOR Int64 where
  toCBOR = intToCBOR
  toEncoding = Enc.int64


instance FromCBOR Int64 where
  fromCBOR = cborToSigned


instance ToCBOR Word where
  toCBOR n = CV.UInt (fromIntegral n)
  toEncoding = Enc.word


instance FromCBOR Word where
  fromCBOR = cborToUnsigned


instance ToCBOR Word8 where
  toCBOR n = CV.UInt (fromIntegral n)
  toEncoding = Enc.word8


instance FromCBOR Word8 where
  fromCBOR = cborToUnsigned


instance ToCBOR Word16 where
  toCBOR n = CV.UInt (fromIntegral n)
  toEncoding = Enc.word16


instance FromCBOR Word16 where
  fromCBOR = cborToUnsigned


instance ToCBOR Word32 where
  toCBOR n = CV.UInt (fromIntegral n)
  toEncoding = Enc.word32


instance FromCBOR Word32 where
  fromCBOR = cborToUnsigned


instance ToCBOR Word64 where
  toCBOR = CV.UInt
  toEncoding = Enc.word64


instance FromCBOR Word64 where
  fromCBOR = cborToUnsigned


instance ToCBOR Float where
  toCBOR = CV.Float32
  toEncoding = Enc.float32


instance FromCBOR Float where
  fromCBOR (CV.Float32 f) = Right f
  fromCBOR (CV.Float16 f) = Right f
  fromCBOR (CV.Float64 d) = Right (realToFrac d)
  fromCBOR _ = Left "FromCBOR Float: expected Float"


instance ToCBOR Double where
  toCBOR = CV.Float64
  toEncoding = Enc.float64


instance FromCBOR Double where
  fromCBOR (CV.Float64 d) = Right d
  fromCBOR (CV.Float32 f) = Right (realToFrac f)
  fromCBOR (CV.Float16 f) = Right (realToFrac f)
  fromCBOR _ = Left "FromCBOR Double: expected Float"


instance ToCBOR Text where
  toCBOR = CV.TextString
  toEncoding = Enc.text


instance FromCBOR Text where
  fromCBOR (CV.TextString t) = Right t
  fromCBOR _ = Left "FromCBOR Text: expected TextString"


instance ToCBOR ByteString where
  toCBOR = CV.ByteString
  toEncoding = Enc.bytes


instance FromCBOR ByteString where
  fromCBOR (CV.ByteString bs) = Right bs
  fromCBOR _ = Left "FromCBOR ByteString: expected ByteString"


instance ToCBOR () where
  toCBOR () = CV.Null
  toEncoding () = Enc.null_


instance FromCBOR () where
  fromCBOR CV.Null = Right ()
  fromCBOR _ = Left "FromCBOR (): expected Null"


instance ToCBOR a => ToCBOR (Maybe a) where
  toCBOR Nothing = CV.Null
  toCBOR (Just x) = toCBOR x
  toEncoding Nothing = Enc.null_
  toEncoding (Just x) = toEncoding x


instance FromCBOR a => FromCBOR (Maybe a) where
  fromCBOR CV.Null = Right Nothing
  fromCBOR v = Just <$> fromCBOR v


instance ToCBOR a => ToCBOR [a] where
  toCBOR xs = CV.Array (V.fromList (map toCBOR xs))
  toEncoding xs = Enc.arrayList (fmap toEncoding xs)


instance FromCBOR a => FromCBOR [a] where
  fromCBOR (CV.Array vs) = traverse fromCBOR (V.toList vs)
  fromCBOR _ = Left "FromCBOR [a]: expected Array"


instance ToCBOR a => ToCBOR (Vector a) where
  toCBOR xs = CV.Array (V.map toCBOR xs)
  toEncoding xs = Enc.array (V.toList (V.map toEncoding xs))


instance FromCBOR a => FromCBOR (Vector a) where
  fromCBOR (CV.Array vs) = V.mapM fromCBOR vs
  fromCBOR _ = Left "FromCBOR Vector: expected Array"


instance (ToCBOR a, ToCBOR b) => ToCBOR (a, b) where
  toCBOR (a, b) = CV.Array (V.fromList [toCBOR a, toCBOR b])


instance (FromCBOR a, FromCBOR b) => FromCBOR (a, b) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = (,) <$> fromCBOR (vs V.! 0) <*> fromCBOR (vs V.! 1)
  fromCBOR _ = Left "FromCBOR (a,b): expected Array of length 2"


instance (ToCBOR k, ToCBOR v) => ToCBOR (Map k v) where
  toCBOR m = CV.Map (V.fromList [(toCBOR k, toCBOR v') | (k, v') <- Map.toList m])
  toEncoding m = Enc.mapList [(toEncoding k, toEncoding v') | (k, v') <- Map.toList m]


instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (Map k v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromCBOR _ = Left "FromCBOR Map: expected Map"


-- Aeson-parity instances ---------------------------------------------------

instance ToCBOR Integer where
  toCBOR = integerValue
  toEncoding = Enc.integer


instance FromCBOR Integer where
  fromCBOR (CV.UInt n) = Right (toInteger n)
  fromCBOR (CV.NInt n) = Right (-1 - toInteger n)
  fromCBOR (CV.Tag 2 (CV.ByteString magnitude)) = Right (bignumMagnitude magnitude)
  fromCBOR (CV.Tag 3 (CV.ByteString magnitude)) = Right (-1 - bignumMagnitude magnitude)
  fromCBOR _ = Left "FromCBOR Integer: expected an integer or bignum"


instance ToCBOR Natural where
  toCBOR = integerValue . toInteger
  toEncoding = Enc.integer . toInteger


instance FromCBOR Natural where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR (CV.Tag 2 (CV.ByteString magnitude)) = Right (fromInteger (bignumMagnitude magnitude))
  fromCBOR _ = Left "FromCBOR Natural: expected an unsigned integer or positive bignum"


instance ToCBOR TL.Text where
  toCBOR = CV.TextString . TL.toStrict


instance FromCBOR TL.Text where
  fromCBOR v = TL.fromStrict <$> fromCBOR v


instance ToCBOR BSL.ByteString where
  toCBOR = CV.ByteString . BSL.toStrict


instance FromCBOR BSL.ByteString where
  fromCBOR v = BSL.fromStrict <$> fromCBOR v


instance ToCBOR a => ToCBOR (NonEmpty a) where
  toCBOR = toCBOR . NE.toList


instance FromCBOR a => FromCBOR (NonEmpty a) where
  fromCBOR v = do
    xs <- fromCBOR v
    case xs of
      [] -> Left "FromCBOR NonEmpty: empty list"
      (y : ys) -> Right (y :| ys)


{- | 'Either' encodes as a tagged map with @"Left"@ or @"Right"@ keys
(mirrors aeson's Sum encoding).
-}
instance (ToCBOR a, ToCBOR b) => ToCBOR (Either a b) where
  toCBOR (Left x) = CV.Map (V.singleton (CV.TextString "Left", toCBOR x))
  toCBOR (Right x) = CV.Map (V.singleton (CV.TextString "Right", toCBOR x))


instance (FromCBOR a, FromCBOR b) => FromCBOR (Either a b) where
  fromCBOR (CV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (CV.TextString "Left", v) -> Left <$> fromCBOR v
        (CV.TextString "Right", v) -> Right <$> fromCBOR v
        _ -> Left "FromCBOR Either: expected Left/Right key"
  fromCBOR _ = Left "FromCBOR Either: expected single-key Map"


instance (Ord a, ToCBOR a) => ToCBOR (Set a) where
  toCBOR = CV.Array . V.fromList . fmap toCBOR . Set.toList


instance (Ord a, FromCBOR a) => FromCBOR (Set a) where
  fromCBOR v = Set.fromList <$> fromCBOR v


instance ToCBOR a => ToCBOR (Seq a) where
  toCBOR s = CV.Array (V.fromList (fmap toCBOR (foldr (:) [] s)))


instance FromCBOR a => FromCBOR (Seq a) where
  fromCBOR v = Seq.fromList <$> fromCBOR v


instance ToCBOR v => ToCBOR (IntMap v) where
  toCBOR m = CV.Map (V.fromList (fmap (\(k, v) -> (toCBOR k, toCBOR v)) (IntMap.toList m)))


instance FromCBOR v => FromCBOR (IntMap v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (IntMap.fromList pairs)
  fromCBOR _ = Left "FromCBOR IntMap: expected Map"


instance ToCBOR IntSet where
  toCBOR = CV.Array . V.fromList . fmap toCBOR . IntSet.toList


instance FromCBOR IntSet where
  fromCBOR v = IntSet.fromList <$> fromCBOR v


instance (Hashable k, ToCBOR k, ToCBOR v) => ToCBOR (HashMap k v) where
  toCBOR m = CV.Map (V.fromList (fmap (\(k, v) -> (toCBOR k, toCBOR v)) (HM.toList m)))


instance (Eq k, Hashable k, FromCBOR k, FromCBOR v) => FromCBOR (HashMap k v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromCBOR _ = Left "FromCBOR HashMap: expected Map"


instance (Hashable a, ToCBOR a) => ToCBOR (HashSet a) where
  toCBOR = CV.Array . V.fromList . fmap toCBOR . HS.toList


instance (Eq a, Hashable a, FromCBOR a) => FromCBOR (HashSet a) where
  fromCBOR v = HS.fromList <$> fromCBOR v


instance (ToCBOR a, ToCBOR b, ToCBOR c) => ToCBOR (a, b, c) where
  toCBOR (a, b, c) = CV.Array (V.fromList [toCBOR a, toCBOR b, toCBOR c])


instance (FromCBOR a, FromCBOR b, FromCBOR c) => FromCBOR (a, b, c) where
  fromCBOR (CV.Array vs)
    | V.length vs == 3 =
        (,,)
          <$> fromCBOR (vs V.! 0)
          <*> fromCBOR (vs V.! 1)
          <*> fromCBOR (vs V.! 2)
  fromCBOR _ = Left "FromCBOR (a,b,c): expected Array of length 3"


instance (ToCBOR a, ToCBOR b, ToCBOR c, ToCBOR d) => ToCBOR (a, b, c, d) where
  toCBOR (a, b, c, d) = CV.Array (V.fromList [toCBOR a, toCBOR b, toCBOR c, toCBOR d])


instance (FromCBOR a, FromCBOR b, FromCBOR c, FromCBOR d) => FromCBOR (a, b, c, d) where
  fromCBOR (CV.Array vs)
    | V.length vs == 4 =
        (,,,)
          <$> fromCBOR (vs V.! 0)
          <*> fromCBOR (vs V.! 1)
          <*> fromCBOR (vs V.! 2)
          <*> fromCBOR (vs V.! 3)
  fromCBOR _ = Left "FromCBOR (a,b,c,d): expected Array of length 4"


instance ToCBOR a => ToCBOR (Identity a) where
  toCBOR (Identity x) = toCBOR x


instance FromCBOR a => FromCBOR (Identity a) where
  fromCBOR v = Identity <$> fromCBOR v


instance ToCBOR a => ToCBOR (Const a b) where
  toCBOR (Const x) = toCBOR x


instance FromCBOR a => FromCBOR (Const a b) where
  fromCBOR v = Const <$> fromCBOR v


instance ToCBOR a => ToCBOR (Down a) where
  toCBOR (Down x) = toCBOR x


instance FromCBOR a => FromCBOR (Down a) where
  fromCBOR v = Down <$> fromCBOR v


instance ToCBOR Version where
  toCBOR = toCBOR . versionBranch


instance FromCBOR Version where
  fromCBOR v = makeVersion <$> fromCBOR v


instance (Integral a, ToCBOR a) => ToCBOR (Ratio a) where
  toCBOR r = CV.Array (V.fromList [toCBOR (numerator r), toCBOR (denominator r)])


instance (Integral a, FromCBOR a) => FromCBOR (Ratio a) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = do
        n <- fromCBOR (vs V.! 0)
        d <- fromCBOR (vs V.! 1)
        if d == 0
          then Left "FromCBOR Ratio: zero denominator"
          else Right (n % d)
  fromCBOR _ = Left "FromCBOR Ratio: expected Array of length 2"


-- Functor / monoid newtype instances --------------------------------------
--
-- Every newtype wraps its underlying value transparently, matching aeson's
-- convention.

instance ToCBOR a => ToCBOR (Mon.Sum a) where
  toCBOR = toCBOR . Mon.getSum
  toEncoding = toEncoding . Mon.getSum


instance FromCBOR a => FromCBOR (Mon.Sum a) where
  fromCBOR v = Mon.Sum <$> fromCBOR v


instance ToCBOR a => ToCBOR (Mon.Product a) where
  toCBOR = toCBOR . Mon.getProduct
  toEncoding = toEncoding . Mon.getProduct


instance FromCBOR a => FromCBOR (Mon.Product a) where
  fromCBOR v = Mon.Product <$> fromCBOR v


instance ToCBOR a => ToCBOR (Mon.Dual a) where
  toCBOR = toCBOR . Mon.getDual
  toEncoding = toEncoding . Mon.getDual


instance FromCBOR a => FromCBOR (Mon.Dual a) where
  fromCBOR v = Mon.Dual <$> fromCBOR v


instance ToCBOR Mon.All where
  toCBOR = toCBOR . Mon.getAll
  toEncoding = toEncoding . Mon.getAll


instance FromCBOR Mon.All where
  fromCBOR v = Mon.All <$> fromCBOR v


instance ToCBOR Mon.Any where
  toCBOR = toCBOR . Mon.getAny
  toEncoding = toEncoding . Mon.getAny


instance FromCBOR Mon.Any where
  fromCBOR v = Mon.Any <$> fromCBOR v


-- | 'Data.Monoid.First' encodes its inner 'Maybe'.
instance ToCBOR a => ToCBOR (Mon.First a) where
  toCBOR = toCBOR . Mon.getFirst
  toEncoding = toEncoding . Mon.getFirst


instance FromCBOR a => FromCBOR (Mon.First a) where
  fromCBOR v = Mon.First <$> fromCBOR v


instance ToCBOR a => ToCBOR (Mon.Last a) where
  toCBOR = toCBOR . Mon.getLast
  toEncoding = toEncoding . Mon.getLast


instance FromCBOR a => FromCBOR (Mon.Last a) where
  fromCBOR v = Mon.Last <$> fromCBOR v


instance ToCBOR a => ToCBOR (Semi.Min a) where
  toCBOR = toCBOR . Semi.getMin
  toEncoding = toEncoding . Semi.getMin


instance FromCBOR a => FromCBOR (Semi.Min a) where
  fromCBOR v = Semi.Min <$> fromCBOR v


instance ToCBOR a => ToCBOR (Semi.Max a) where
  toCBOR = toCBOR . Semi.getMax
  toEncoding = toEncoding . Semi.getMax


instance FromCBOR a => FromCBOR (Semi.Max a) where
  fromCBOR v = Semi.Max <$> fromCBOR v


instance ToCBOR a => ToCBOR (Semi.First a) where
  toCBOR = toCBOR . Semi.getFirst
  toEncoding = toEncoding . Semi.getFirst


instance FromCBOR a => FromCBOR (Semi.First a) where
  fromCBOR v = Semi.First <$> fromCBOR v


instance ToCBOR a => ToCBOR (Semi.Last a) where
  toCBOR = toCBOR . Semi.getLast
  toEncoding = toEncoding . Semi.getLast


instance FromCBOR a => FromCBOR (Semi.Last a) where
  fromCBOR v = Semi.Last <$> fromCBOR v


instance ToCBOR a => ToCBOR (Semi.WrappedMonoid a) where
  toCBOR = toCBOR . Semi.unwrapMonoid
  toEncoding = toEncoding . Semi.unwrapMonoid


instance FromCBOR a => FromCBOR (Semi.WrappedMonoid a) where
  fromCBOR v = Semi.WrapMonoid <$> fromCBOR v


{- | 'Semi.Arg' encodes as a two-element array of (key, value), matching
the 'Show' / 'aeson' shape.
-}
instance (ToCBOR a, ToCBOR b) => ToCBOR (Semi.Arg a b) where
  toCBOR (Semi.Arg a b) = CV.Array (V.fromList [toCBOR a, toCBOR b])
  toEncoding (Semi.Arg a b) = Enc.arrayList [toEncoding a, toEncoding b]


instance (FromCBOR a, FromCBOR b) => FromCBOR (Semi.Arg a b) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = Semi.Arg <$> fromCBOR (vs V.! 0) <*> fromCBOR (vs V.! 1)
  fromCBOR _ = Left "FromCBOR Arg: expected Array of length 2"


-- | 'Compose' is transparent: encodes its inner @f (g a)@.
instance ToCBOR (f (g a)) => ToCBOR (Compose f g a) where
  toCBOR = toCBOR . getCompose
  toEncoding = toEncoding . getCompose


instance FromCBOR (f (g a)) => FromCBOR (Compose f g a) where
  fromCBOR v = Compose <$> fromCBOR v


-- | 'Functor.Product' encodes as a 2-element array.
instance (ToCBOR (f a), ToCBOR (g a)) => ToCBOR (FProduct.Product f g a) where
  toCBOR (FProduct.Pair x y) = CV.Array (V.fromList [toCBOR x, toCBOR y])
  toEncoding (FProduct.Pair x y) = Enc.arrayList [toEncoding x, toEncoding y]


instance (FromCBOR (f a), FromCBOR (g a)) => FromCBOR (FProduct.Product f g a) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = FProduct.Pair <$> fromCBOR (vs V.! 0) <*> fromCBOR (vs V.! 1)
  fromCBOR _ = Left "FromCBOR Functor.Product: expected Array of length 2"


{- | 'Functor.Sum' encodes as a single-key map @{\"InL\":…}@ or
@{\"InR\":…}@.
-}
instance (ToCBOR (f a), ToCBOR (g a)) => ToCBOR (FSum.Sum f g a) where
  toCBOR (FSum.InL x) = CV.Map (V.singleton (CV.TextString "InL", toCBOR x))
  toCBOR (FSum.InR x) = CV.Map (V.singleton (CV.TextString "InR", toCBOR x))
  toEncoding (FSum.InL x) = Enc.mapList [(Enc.text "InL", toEncoding x)]
  toEncoding (FSum.InR x) = Enc.mapList [(Enc.text "InR", toEncoding x)]


instance (FromCBOR (f a), FromCBOR (g a)) => FromCBOR (FSum.Sum f g a) where
  fromCBOR (CV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (CV.TextString "InL", v) -> FSum.InL <$> fromCBOR v
        (CV.TextString "InR", v) -> FSum.InR <$> fromCBOR v
        _ -> Left "FromCBOR Functor.Sum: expected InL/InR key"
  fromCBOR _ = Left "FromCBOR Functor.Sum: expected single-key Map"


instance ToCBOR CV.Value where
  toCBOR = id


instance FromCBOR CV.Value where
  fromCBOR = Right


-- GHC.Generics support

class GToCBOR f where
  gToCBOR :: f p -> CV.Value


class GFromCBOR f where
  gFromCBOR :: CV.Value -> Either String (f p)


instance GToCBOR f => GToCBOR (M1 D c f) where
  gToCBOR (M1 x) = gToCBOR x


instance GFromCBOR f => GFromCBOR (M1 D c f) where
  gFromCBOR v = M1 <$> gFromCBOR v


instance (Constructor c, GToCBORFields f) => GToCBOR (M1 C c f) where
  gToCBOR (M1 x) =
    let fields = gToCBORFields x
    in CV.Map (V.fromList [(CV.TextString k, v) | (k, v) <- fields])


instance (Constructor c, GFromCBORFields f) => GFromCBOR (M1 C c f) where
  gFromCBOR (CV.Map kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromCBORFields lkup
  gFromCBOR _ = Left "GFromCBOR: expected Map for record type"


lookupField :: Text -> Vector (CV.Value, CV.Value) -> Maybe CV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (CV.TextString k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)


class GToCBORFields f where
  gToCBORFields :: f p -> [(Text, CV.Value)]


class GFromCBORFields f where
  gFromCBORFields :: (Text -> Maybe CV.Value) -> Either String (f p)


instance (GToCBORFields a, GToCBORFields b) => GToCBORFields (a :*: b) where
  gToCBORFields (a :*: b) = gToCBORFields a ++ gToCBORFields b


instance (GFromCBORFields a, GFromCBORFields b) => GFromCBORFields (a :*: b) where
  gFromCBORFields lkup = (:*:) <$> gFromCBORFields lkup <*> gFromCBORFields lkup


instance (Selector s, ToCBOR a) => GToCBORFields (M1 S s (K1 i a)) where
  gToCBORFields m@(M1 (K1 x)) = [(T.pack (selName m), toCBOR x)]


instance (Selector s, FromCBOR a) => GFromCBORFields (M1 S s (K1 i a)) where
  gFromCBORFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromCBOR: missing field " ++ T.unpack name
         Just v -> M1 . K1 <$> fromCBOR v


-- ---------------------------------------------------------------------------
-- Generic direct-to-bytes encoding (mirrors aeson's genericToEncoding).
-- ---------------------------------------------------------------------------

-- | Generic dispatch for a record's 'Encoding'.
class GToCBOREncoding f where
  gToEncoding :: f p -> Encoding


{- | Generic dispatch for a record's field-list 'Encoding'. Returns
the list of @(key, encoded value)@ pairs without first packing
them into a 'CV.Value'.
-}
class GToCBOREncodingFields f where
  gToEncodingFields :: f p -> [(Encoding, Encoding)]


instance GToCBOREncoding f => GToCBOREncoding (M1 D c f) where
  gToEncoding (M1 x) = gToEncoding x


instance (Constructor c, GToCBOREncodingFields f) => GToCBOREncoding (M1 C c f) where
  gToEncoding (M1 x) = Enc.mapList (gToEncodingFields x)


instance (GToCBOREncodingFields a, GToCBOREncodingFields b) => GToCBOREncodingFields (a :*: b) where
  gToEncodingFields (a :*: b) = gToEncodingFields a ++ gToEncodingFields b


instance (Selector s, ToCBOR a) => GToCBOREncodingFields (M1 S s (K1 i a)) where
  gToEncodingFields m@(M1 (K1 x)) = [(Enc.text (T.pack (selName m)), toEncoding x)]
