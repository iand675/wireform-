{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typeclass-based YAML serialization with GHC Generics support.
module YAML.Class (
  ToYAML (..),
  FromYAML (..),
  encodeYAML,
  encodeYAMLBS,
  decodeYAML,
  decodeYAMLBS,
  GToYAML (..),
  GFromYAML (..),
  validateFieldNames,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as FProduct
import Data.Functor.Sum qualified as FSum
import Data.HashSet qualified as HS
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
import Data.Text.Encoding qualified as TEnc
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Text.Lazy.Builder.RealFloat qualified as BF
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (double2Float, float2Double)
import GHC.Generics
import Numeric.Natural (Natural)
import YAML.Decode qualified as YD
import YAML.Encode qualified as YE
import YAML.Value qualified as YV


-- ---------------------------------------------------------------------------
-- Class
-- ---------------------------------------------------------------------------

class ToYAML a where
  toYAML :: a -> YV.Value
  default toYAML :: (Generic a, GToYAML (Rep a)) => a -> YV.Value
  toYAML = gToYAML . from


class FromYAML a where
  fromYAML :: YV.Value -> Either String a
  default fromYAML :: (Generic a, GFromYAML (Rep a)) => YV.Value -> Either String a
  fromYAML v = to <$> gFromYAML v


encodeYAML :: ToYAML a => a -> Text
encodeYAML = YE.encode . toYAML


encodeYAMLBS :: ToYAML a => a -> ByteString
encodeYAMLBS = YE.encodeBS . toYAML


decodeYAML :: FromYAML a => Text -> Either String a
decodeYAML t = YD.decode t >>= fromYAML


decodeYAMLBS :: FromYAML a => ByteString -> Either String a
decodeYAMLBS b = YD.decodeBS b >>= fromYAML


-- ---------------------------------------------------------------------------
-- Scalar instances
-- ---------------------------------------------------------------------------

instance ToYAML YV.Value where toYAML = id


instance FromYAML YV.Value where fromYAML = Right


instance ToYAML Text where
  toYAML = YV.YString


instance FromYAML Text where
  fromYAML v = case YV.unwrap v of
    YV.YString t -> Right t
    YV.YInt n -> Right (TL.toStrict (TB.toLazyText (BI.decimal n)))
    YV.YFloat d -> Right (TL.toStrict (TB.toLazyText (BF.realFloat d)))
    YV.YBool b -> Right (if b then "true" else "false")
    YV.YNull -> Right T.empty
    _ -> Left "FromYAML Text: expected scalar"


instance ToYAML Bool where toYAML = YV.YBool


instance FromYAML Bool where
  fromYAML v = case YV.unwrap v of
    YV.YBool b -> Right b
    _ -> Left "FromYAML Bool: expected YBool"


intToYAML :: Integral a => a -> YV.Value
intToYAML = YV.YInt . fromIntegral


intFrom :: forall a. (Integral a, Bounded a) => YV.Value -> Either String a
intFrom v = case YV.unwrap v of
  YV.YInt n
    | n >= toInteger (minBound :: a), n <= toInteger (maxBound :: a) -> Right (fromInteger n)
    | otherwise -> Left "FromYAML Int: integer out of range"
  _ -> Left "FromYAML Int: expected YInt"


instance ToYAML Int where toYAML = intToYAML


instance FromYAML Int where fromYAML = intFrom


instance ToYAML Int8 where toYAML = intToYAML


instance FromYAML Int8 where fromYAML = intFrom


instance ToYAML Int16 where toYAML = intToYAML


instance FromYAML Int16 where fromYAML = intFrom


instance ToYAML Int32 where toYAML = intToYAML


instance FromYAML Int32 where fromYAML = intFrom


instance ToYAML Int64 where toYAML = intToYAML


instance FromYAML Int64 where fromYAML = intFrom


instance ToYAML Word where toYAML = intToYAML


instance FromYAML Word where fromYAML = intFrom


instance ToYAML Word8 where toYAML = intToYAML


instance FromYAML Word8 where fromYAML = intFrom


instance ToYAML Word16 where toYAML = intToYAML


instance FromYAML Word16 where fromYAML = intFrom


instance ToYAML Word32 where toYAML = intToYAML


instance FromYAML Word32 where fromYAML = intFrom


instance ToYAML Word64 where toYAML = intToYAML


instance FromYAML Word64 where fromYAML = intFrom


instance ToYAML Integer where toYAML = YV.YInt


instance FromYAML Integer where
  fromYAML v = case YV.unwrap v of
    YV.YInt n -> Right n
    _ -> Left "FromYAML Integer: expected YInt"


instance ToYAML Natural where toYAML = YV.YInt . fromIntegral


instance FromYAML Natural where
  fromYAML v = case YV.unwrap v of
    YV.YInt n | n >= 0 -> Right (fromIntegral n)
    _ -> Left "FromYAML Natural: expected non-negative YInt"


instance ToYAML Double where toYAML = YV.YFloat


instance FromYAML Double where
  fromYAML v = case YV.unwrap v of
    YV.YFloat d -> Right d
    YV.YInt n -> Right (fromIntegral n)
    _ -> Left "FromYAML Double: expected YFloat"


instance ToYAML Float where toYAML = YV.YFloat . float2Double


instance FromYAML Float where
  fromYAML v = case YV.unwrap v of
    YV.YFloat d -> Right (double2Float d)
    YV.YInt n -> Right (fromIntegral n)
    _ -> Left "FromYAML Float: expected YFloat"


instance ToYAML Char where
  toYAML c = YV.YString (T.singleton c)


instance FromYAML Char where
  fromYAML v = case YV.unwrap v of
    YV.YString t | T.length t == 1 -> Right (T.head t)
    _ -> Left "FromYAML Char: expected single-character YString"


instance ToYAML TL.Text where
  toYAML = YV.YString . TL.toStrict


instance FromYAML TL.Text where
  fromYAML v = TL.fromStrict <$> fromYAML v


{- | Valid UTF-8 bytes use a text scalar. Arbitrary binary data uses a sequence
of byte-valued integers, so this pure instance is total and round-trippable.
-}
instance ToYAML ByteString where
  toYAML bytes = case TEnc.decodeUtf8' bytes of
    Right text -> YV.YString text
    Left _ -> YV.YSeq (V.generate (BS.length bytes) (YV.YInt . toInteger . BS.index bytes))


instance FromYAML ByteString where
  fromYAML v = case YV.unwrap v of
    YV.YString t -> Right (TEnc.encodeUtf8 t)
    YV.YSeq bytes -> BS.pack . V.toList <$> V.mapM (intFrom @Word8) bytes
    _ -> Left "FromYAML ByteString: expected text or a byte sequence"


instance ToYAML BSL.ByteString where
  toYAML = toYAML . BSL.toStrict


instance FromYAML BSL.ByteString where
  fromYAML v = BSL.fromStrict <$> fromYAML v


instance ToYAML () where
  toYAML () = YV.YSeq V.empty


instance FromYAML () where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs | V.null xs -> Right ()
    _ -> Left "FromYAML (): expected empty YSeq"


-- ---------------------------------------------------------------------------
-- Container instances
-- ---------------------------------------------------------------------------

instance ToYAML a => ToYAML [a] where
  toYAML xs = YV.YSeq (V.fromList (map toYAML xs))


instance FromYAML a => FromYAML [a] where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs -> traverse fromYAML (V.toList xs)
    _ -> Left "FromYAML [a]: expected YSeq"


instance ToYAML a => ToYAML (Vector a) where
  toYAML xs = YV.YSeq (V.map toYAML xs)


instance FromYAML a => FromYAML (Vector a) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs -> V.mapM fromYAML xs
    _ -> Left "FromYAML Vector: expected YSeq"


instance ToYAML a => ToYAML (Maybe a) where
  toYAML Nothing = YV.YNull
  toYAML (Just x) = toYAML x


instance FromYAML a => FromYAML (Maybe a) where
  fromYAML v = case YV.unwrap v of
    YV.YNull -> Right Nothing
    _ -> Just <$> fromYAML v


instance ToYAML a => ToYAML (NonEmpty a) where
  toYAML = toYAML . NE.toList


instance FromYAML a => FromYAML (NonEmpty a) where
  fromYAML v = do
    xs <- fromYAML v
    case xs of
      [] -> Left "FromYAML NonEmpty: empty list"
      (y : ys) -> Right (y :| ys)


instance (ToYAML a, ToYAML b) => ToYAML (Either a b) where
  toYAML (Left x) = YV.YMap (V.singleton (YV.YString "Left", toYAML x))
  toYAML (Right x) = YV.YMap (V.singleton (YV.YString "Right", toYAML x))


instance (FromYAML a, FromYAML b) => FromYAML (Either a b) where
  fromYAML v = case YV.unwrap v of
    YV.YMap kvs | V.length kvs == 1 -> case V.head kvs of
      (k, vv) -> case YV.unwrap k of
        YV.YString "Left" -> Left <$> fromYAML vv
        YV.YString "Right" -> Right <$> fromYAML vv
        _ -> Left "FromYAML Either: expected Left/Right key"
    _ -> Left "FromYAML Either: expected single-key YMap"


instance ToYAML a => ToYAML (Set a) where
  toYAML = YV.YSeq . V.fromList . fmap toYAML . Set.toList


instance (Ord a, FromYAML a) => FromYAML (Set a) where
  fromYAML v = Set.fromList <$> fromYAML v


instance ToYAML a => ToYAML (Seq a) where
  toYAML s = YV.YSeq (V.fromList (fmap toYAML (foldr (:) [] s)))


instance FromYAML a => FromYAML (Seq a) where
  fromYAML v = Seq.fromList <$> fromYAML v


instance ToYAML v => ToYAML (Map Text v) where
  toYAML m = YV.YMap (V.fromList (fmap mkPair (Map.toList m)))
    where
      mkPair (k, val) = (YV.YString k, toYAML val)


instance FromYAML v => FromYAML (Map Text v) where
  fromYAML v = case YV.unwrap v of
    YV.YMap kvs -> do
      V.foldM decodePair Map.empty kvs
      where
        decodePair values (k, val) = case YV.unwrap k of
          YV.YString s -> Map.alterF (\case Nothing -> Just <$> fromYAML val; Just _ -> Left "FromYAML Map: duplicate text key after unwrapping tags") s values
          _ -> Left "FromYAML (Map Text v): non-string key"
    _ -> Left "FromYAML (Map Text v): expected YMap"


instance ToYAML v => ToYAML (IntMap v) where
  toYAML m = YV.YMap (V.fromList (fmap mkPair (IntMap.toList m)))
    where
      mkPair (k, val) = (YV.YInt (fromIntegral k), toYAML val)


instance FromYAML v => FromYAML (IntMap v) where
  fromYAML v = case YV.unwrap v of
    YV.YMap kvs -> do
      V.foldM decodePair IntMap.empty kvs
      where
        decodePair values (k, val) = case YV.unwrap k of
          YV.YInt _ -> do
            key <- fromYAML k :: Either String Int
            IntMap.alterF (\case Nothing -> Just <$> fromYAML val; Just _ -> Left "FromYAML IntMap: duplicate integer key") key values
          _ -> Left "FromYAML IntMap: non-integer key"
    _ -> Left "FromYAML IntMap: expected YMap"


instance ToYAML IntSet where
  toYAML = YV.YSeq . V.fromList . fmap toYAML . IntSet.toList


instance FromYAML IntSet where
  fromYAML v = IntSet.fromList <$> fromYAML v


-- ---------------------------------------------------------------------------
-- Tuples
-- ---------------------------------------------------------------------------

instance (ToYAML a, ToYAML b) => ToYAML (a, b) where
  toYAML (a, b) = YV.YSeq (V.fromList [toYAML a, toYAML b])


instance (FromYAML a, FromYAML b) => FromYAML (a, b) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs
      | V.length xs == 2 ->
          (,) <$> fromYAML (xs V.! 0) <*> fromYAML (xs V.! 1)
    _ -> Left "FromYAML (a,b): expected YSeq of length 2"


instance (ToYAML a, ToYAML b, ToYAML c) => ToYAML (a, b, c) where
  toYAML (a, b, c) = YV.YSeq (V.fromList [toYAML a, toYAML b, toYAML c])


instance (FromYAML a, FromYAML b, FromYAML c) => FromYAML (a, b, c) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs
      | V.length xs == 3 ->
          (,,) <$> fromYAML (xs V.! 0) <*> fromYAML (xs V.! 1) <*> fromYAML (xs V.! 2)
    _ -> Left "FromYAML (a,b,c): expected YSeq of length 3"


instance (ToYAML a, ToYAML b, ToYAML c, ToYAML d) => ToYAML (a, b, c, d) where
  toYAML (a, b, c, d) =
    YV.YSeq (V.fromList [toYAML a, toYAML b, toYAML c, toYAML d])


instance (FromYAML a, FromYAML b, FromYAML c, FromYAML d) => FromYAML (a, b, c, d) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs
      | V.length xs == 4 ->
          (,,,)
            <$> fromYAML (xs V.! 0)
            <*> fromYAML (xs V.! 1)
            <*> fromYAML (xs V.! 2)
            <*> fromYAML (xs V.! 3)
    _ -> Left "FromYAML (a,b,c,d): expected YSeq of length 4"


-- ---------------------------------------------------------------------------
-- Functor / monoid newtypes
-- ---------------------------------------------------------------------------

instance ToYAML a => ToYAML (Identity a) where toYAML (Identity x) = toYAML x


instance FromYAML a => FromYAML (Identity a) where
  fromYAML v = Identity <$> fromYAML v


instance ToYAML a => ToYAML (Const a b) where toYAML (Const x) = toYAML x


instance FromYAML a => FromYAML (Const a b) where
  fromYAML v = Const <$> fromYAML v


instance ToYAML a => ToYAML (Down a) where toYAML (Down x) = toYAML x


instance FromYAML a => FromYAML (Down a) where
  fromYAML v = Down <$> fromYAML v


instance ToYAML Version where toYAML = toYAML . versionBranch


instance FromYAML Version where fromYAML v = makeVersion <$> fromYAML v


instance ToYAML a => ToYAML (Ratio a) where
  toYAML r = YV.YSeq (V.fromList [toYAML (numerator r), toYAML (denominator r)])


instance (Integral a, FromYAML a) => FromYAML (Ratio a) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs | V.length xs == 2 -> do
      n <- fromYAML (xs V.! 0)
      d <- fromYAML (xs V.! 1)
      if d == 0
        then Left "FromYAML Ratio: zero denominator"
        else Right (n % d)
    _ -> Left "FromYAML Ratio: expected YSeq of length 2"


instance ToYAML a => ToYAML (Mon.Sum a) where toYAML = toYAML . Mon.getSum


instance FromYAML a => FromYAML (Mon.Sum a) where
  fromYAML v = Mon.Sum <$> fromYAML v


instance ToYAML a => ToYAML (Mon.Product a) where toYAML = toYAML . Mon.getProduct


instance FromYAML a => FromYAML (Mon.Product a) where
  fromYAML v = Mon.Product <$> fromYAML v


instance ToYAML a => ToYAML (Mon.Dual a) where toYAML = toYAML . Mon.getDual


instance FromYAML a => FromYAML (Mon.Dual a) where
  fromYAML v = Mon.Dual <$> fromYAML v


instance ToYAML Mon.All where toYAML = toYAML . Mon.getAll


instance FromYAML Mon.All where fromYAML v = Mon.All <$> fromYAML v


instance ToYAML Mon.Any where toYAML = toYAML . Mon.getAny


instance FromYAML Mon.Any where fromYAML v = Mon.Any <$> fromYAML v


instance ToYAML a => ToYAML (Mon.First a) where toYAML = toYAML . Mon.getFirst


instance FromYAML a => FromYAML (Mon.First a) where
  fromYAML v = Mon.First <$> fromYAML v


instance ToYAML a => ToYAML (Mon.Last a) where toYAML = toYAML . Mon.getLast


instance FromYAML a => FromYAML (Mon.Last a) where
  fromYAML v = Mon.Last <$> fromYAML v


instance ToYAML a => ToYAML (Semi.Min a) where toYAML = toYAML . Semi.getMin


instance FromYAML a => FromYAML (Semi.Min a) where
  fromYAML v = Semi.Min <$> fromYAML v


instance ToYAML a => ToYAML (Semi.Max a) where toYAML = toYAML . Semi.getMax


instance FromYAML a => FromYAML (Semi.Max a) where
  fromYAML v = Semi.Max <$> fromYAML v


instance ToYAML a => ToYAML (Semi.First a) where toYAML = toYAML . Semi.getFirst


instance FromYAML a => FromYAML (Semi.First a) where
  fromYAML v = Semi.First <$> fromYAML v


instance ToYAML a => ToYAML (Semi.Last a) where toYAML = toYAML . Semi.getLast


instance FromYAML a => FromYAML (Semi.Last a) where
  fromYAML v = Semi.Last <$> fromYAML v


instance ToYAML a => ToYAML (Semi.WrappedMonoid a) where
  toYAML = toYAML . Semi.unwrapMonoid


instance FromYAML a => FromYAML (Semi.WrappedMonoid a) where
  fromYAML v = Semi.WrapMonoid <$> fromYAML v


instance (ToYAML a, ToYAML b) => ToYAML (Semi.Arg a b) where
  toYAML (Semi.Arg a b) = YV.YSeq (V.fromList [toYAML a, toYAML b])


instance (FromYAML a, FromYAML b) => FromYAML (Semi.Arg a b) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs
      | V.length xs == 2 ->
          Semi.Arg <$> fromYAML (xs V.! 0) <*> fromYAML (xs V.! 1)
    _ -> Left "FromYAML Arg: expected YSeq of length 2"


instance ToYAML (f (g a)) => ToYAML (Compose f g a) where
  toYAML = toYAML . getCompose


instance FromYAML (f (g a)) => FromYAML (Compose f g a) where
  fromYAML v = Compose <$> fromYAML v


instance (ToYAML (f a), ToYAML (g a)) => ToYAML (FProduct.Product f g a) where
  toYAML (FProduct.Pair x y) = YV.YSeq (V.fromList [toYAML x, toYAML y])


instance (FromYAML (f a), FromYAML (g a)) => FromYAML (FProduct.Product f g a) where
  fromYAML v = case YV.unwrap v of
    YV.YSeq xs
      | V.length xs == 2 ->
          FProduct.Pair <$> fromYAML (xs V.! 0) <*> fromYAML (xs V.! 1)
    _ -> Left "FromYAML Functor.Product: expected YSeq of length 2"


instance (ToYAML (f a), ToYAML (g a)) => ToYAML (FSum.Sum f g a) where
  toYAML (FSum.InL x) = YV.YMap (V.singleton (YV.YString "InL", toYAML x))
  toYAML (FSum.InR x) = YV.YMap (V.singleton (YV.YString "InR", toYAML x))


instance (FromYAML (f a), FromYAML (g a)) => FromYAML (FSum.Sum f g a) where
  fromYAML v = case YV.unwrap v of
    YV.YMap kvs | V.length kvs == 1 -> case V.head kvs of
      (k, val) -> case YV.unwrap k of
        YV.YString "InL" -> FSum.InL <$> fromYAML val
        YV.YString "InR" -> FSum.InR <$> fromYAML val
        _ -> Left "FromYAML Functor.Sum: expected InL/InR key"
    _ -> Left "FromYAML Functor.Sum: expected single-key YMap"


-- ---------------------------------------------------------------------------
-- GHC.Generics support
-- ---------------------------------------------------------------------------

class GToYAML f where
  gToYAML :: f p -> YV.Value


class GFromYAML f where
  gFromYAML :: YV.Value -> Either String (f p)


instance GToYAML f => GToYAML (M1 D c f) where
  gToYAML (M1 x) = gToYAML x


instance GFromYAML f => GFromYAML (M1 D c f) where
  gFromYAML v = M1 <$> gFromYAML v


instance GToYAMLFields f => GToYAML (M1 C c f) where
  gToYAML (M1 x) = YV.YMap (V.fromList (gToYAMLFields x))


instance GFromYAMLFields f => GFromYAML (M1 C c f) where
  gFromYAML v = case YV.unwrap v of
    YV.YMap kvs -> do
      validateFieldNames kvs
      let lkup nm = lookupField nm kvs
      M1 <$> gFromYAMLFields lkup
    _ -> Left "GFromYAML: expected YMap for record type"


{- | Tags can distinguish YAML mapping keys whose native Text field names are
equal. Reject that ambiguity before a record decoder chooses a field.
-}
validateFieldNames :: Vector (YV.Value, YV.Value) -> Either String ()
validateFieldNames fields = () <$ V.foldM add HS.empty fields
  where
    add names (key, _) = case YV.unwrap key of
      YV.YString name
        | HS.member name names -> Left "FromYAML: duplicate field name after unwrapping tags"
        | otherwise -> Right (HS.insert name names)
      _ -> Right names


lookupField :: Text -> Vector (YV.Value, YV.Value) -> Maybe YV.Value
lookupField nm kvs = go 0
  where
    !len = V.length kvs
    go !i
      | i >= len = Nothing
      | otherwise = case kvs V.! i of
          (k, val) -> case YV.unwrap k of
            YV.YString s | s == nm -> Just val
            _ -> go (i + 1)


class GToYAMLFields f where
  gToYAMLFields :: f p -> [(YV.Value, YV.Value)]


class GFromYAMLFields f where
  gFromYAMLFields :: (Text -> Maybe YV.Value) -> Either String (f p)


instance (GToYAMLFields a, GToYAMLFields b) => GToYAMLFields (a :*: b) where
  gToYAMLFields (a :*: b) = gToYAMLFields a ++ gToYAMLFields b


instance (GFromYAMLFields a, GFromYAMLFields b) => GFromYAMLFields (a :*: b) where
  gFromYAMLFields lkup = (:*:) <$> gFromYAMLFields lkup <*> gFromYAMLFields lkup


instance (Selector s, ToYAML a) => GToYAMLFields (M1 S s (K1 i a)) where
  gToYAMLFields m@(M1 (K1 x)) = [(YV.YString (T.pack (selName m)), toYAML x)]


instance (Selector s, FromYAML a) => GFromYAMLFields (M1 S s (K1 i a)) where
  gFromYAMLFields lkup =
    let nm = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup nm of
         Nothing -> Left $ "GFromYAML: missing field " ++ T.unpack nm
         Just v -> M1 . K1 <$> fromYAML v
