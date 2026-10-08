{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | @GHC.Generics@-backed 'Table' derivation for records whose
fields all have primitive Arrow encoders.

This module bridges "Arrow.Record"'s combinator API with
Haskell's 'Generic' machinery so that a record with
@deriving stock Generic@ can get an Arrow 'Table' instance in
one line:

@
data Trade = Trade { sym :: Text, qty :: Int32, note :: Maybe Text }
  deriving stock ('Generic')

tradeTable :: 'Table' Trade
tradeTable = 'genericTable'
@

Under the hood, 'genericTable' walks the 'Rep' to collect the
selector name + the 'HasEncoder' / 'HasDecoder' instance for
each field. Users who want a non-default representation for a
field type (e.g. a newtype wrapping @Int64@ that should
serialise as its underlying type) write a one-line
'HasEncoder' / 'HasDecoder' instance via
'Data.Functor.Contravariant.contramap' + 'fmap':

@
newtype UserId = UserId { unUserId :: Int64 }

instance 'HasEncoder' UserId where
  hasEncoder = 'contramap' unUserId 'int64E'
instance 'HasDecoder' UserId where
  hasDecoder = UserId \<$\> 'int64D'
@

For records that don't want to depend on 'Generic' (or that
want explicit column-name control), see "Arrow.Record.TH".
-}
module Arrow.Record.Generic (
  -- * Type classes for Generic deriving
  HasEncoder (..),
  HasDecoder (..),

  -- * Row-level (nested-record) classes
  HasRowEncoder (..),
  HasRowDecoder (..),

  -- * Deriver
  genericTable,
  genericRowEncoder,
  genericRowDecoder,

  -- * Re-exports for convenience
  Generic,
) where

import Arrow.Record (
  Decoder,
  Encoder,
  RowDecoder,
  RowEncoder,
  Table (..),
  binaryD,
  binaryE,
  boolD,
  boolE,
  columnD,
  doubleD,
  doubleE,
  fieldE,
  floatD,
  floatE,
  int16D,
  int16E,
  int32D,
  int32E,
  int64D,
  int64E,
  int8D,
  int8E,
  nullable,
  nullableD,
  utf8D,
  utf8E,
  word16D,
  word16E,
  word32D,
  word32E,
  word64D,
  word64E,
  word8D,
  word8E,
 )
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics


-- ============================================================
-- HasEncoder / HasDecoder
-- ============================================================

{- | Bridge class between Haskell primitive types and
"Arrow.Record" 'Encoder' values. Only needed when deriving a
'Table' with 'genericTable'; hand-written 'RowEncoder's don't
touch this class.
-}
class HasEncoder a where
  hasEncoder :: Encoder a


-- | Mirror of 'HasEncoder' on the decoding side.
class HasDecoder a where
  hasDecoder :: Decoder a


{- | Per-record class used when a nested record needs to be
embedded as an Arrow @ColStruct@ column.

Default implementation uses 'genericRowEncoder', so any
'Generic' record whose fields all have 'HasEncoder' or
'HasEncoder (Maybe a)' instances picks up the instance for
free:

@
data Address = Address { city :: Text, zip :: Text }
  deriving stock ('Generic')

instance 'HasRowEncoder' Address
instance 'HasRowDecoder' Address
@

Once the inner record has these, lift it into a parent
'RowEncoder' / 'RowDecoder' with @'structE' "addr" addr
'hasRowEncoder'@ / @'structD' "addr" 'hasRowDecoder'@.
-}
class HasRowEncoder a where
  hasRowEncoder :: RowEncoder a
  default hasRowEncoder
    :: (Generic a, GRowEncoder (Rep a)) => RowEncoder a
  hasRowEncoder = genericRowEncoder


-- | Mirror of 'HasRowEncoder' on the decoding side.
class HasRowDecoder a where
  hasRowDecoder :: RowDecoder a
  default hasRowDecoder
    :: (Generic a, GRowDecoder (Rep a)) => RowDecoder a
  hasRowDecoder = genericRowDecoder


-- Primitive instances just delegate to the corresponding
-- combinator from "Arrow.Record".

instance HasEncoder Int8 where hasEncoder = int8E


instance HasEncoder Int16 where hasEncoder = int16E


instance HasEncoder Int32 where hasEncoder = int32E


instance HasEncoder Int64 where hasEncoder = int64E


instance HasEncoder Word8 where hasEncoder = word8E


instance HasEncoder Word16 where hasEncoder = word16E


instance HasEncoder Word32 where hasEncoder = word32E


instance HasEncoder Word64 where hasEncoder = word64E


instance HasEncoder Float where hasEncoder = floatE


instance HasEncoder Double where hasEncoder = doubleE


instance HasEncoder Bool where hasEncoder = boolE


instance HasEncoder Text where hasEncoder = utf8E


instance HasEncoder ByteString where hasEncoder = binaryE


instance HasDecoder Int8 where hasDecoder = int8D


instance HasDecoder Int16 where hasDecoder = int16D


instance HasDecoder Int32 where hasDecoder = int32D


instance HasDecoder Int64 where hasDecoder = int64D


instance HasDecoder Word8 where hasDecoder = word8D


instance HasDecoder Word16 where hasDecoder = word16D


instance HasDecoder Word32 where hasDecoder = word32D


instance HasDecoder Word64 where hasDecoder = word64D


instance HasDecoder Float where hasDecoder = floatD


instance HasDecoder Double where hasDecoder = doubleD


instance HasDecoder Bool where hasDecoder = boolD


instance HasDecoder Text where hasDecoder = utf8D


instance HasDecoder ByteString where hasDecoder = binaryD


{- | @'Maybe' a@ lifts through 'nullable' / 'nullableD'. The
@{\-# OVERLAPPING #-\}@ is needed because the instance head
'HasEncoder (Maybe a)' overlaps with the base instance when
GHC considers the final record field type.
-}
instance {-# OVERLAPPING #-} HasEncoder a => HasEncoder (Maybe a) where
  hasEncoder = nullable hasEncoder
  {-# INLINE hasEncoder #-}


instance {-# OVERLAPPING #-} HasDecoder a => HasDecoder (Maybe a) where
  hasDecoder = nullableD hasDecoder
  {-# INLINE hasDecoder #-}


-- ============================================================
-- Generic derivation
-- ============================================================

{- | Derive a 'Table' for any record whose field types all have
'HasEncoder' + 'HasDecoder' instances. Column names come from
the record selector names.

@
data Trade = Trade { sym :: Text, qty :: Int32 } deriving (Generic)
tradeTable :: 'Table' Trade
tradeTable = 'genericTable'
@
-}
genericTable
  :: forall r
   . ( Generic r
     , GRowEncoder (Rep r)
     , GRowDecoder (Rep r)
     )
  => Table r
genericTable =
  Table
    { tableEncode = genericRowEncoder @r
    , tableDecode = genericRowDecoder @r
    }
{-# INLINE genericTable #-}


genericRowEncoder
  :: forall r
   . (Generic r, GRowEncoder (Rep r))
  => RowEncoder r
genericRowEncoder =
  contramap from (gRowEncoder :: RowEncoder (Rep r ()))
{-# INLINE genericRowEncoder #-}


genericRowDecoder
  :: forall r
   . (Generic r, GRowDecoder (Rep r))
  => RowDecoder r
genericRowDecoder =
  to <$> (gRowDecoder :: RowDecoder (Rep r ()))
{-# INLINE genericRowDecoder #-}


-- ============================================================
-- Generic Rep walkers
-- ============================================================

class GRowEncoder (f :: Type -> Type) where
  gRowEncoder :: RowEncoder (f p)


class GRowDecoder (f :: Type -> Type) where
  gRowDecoder :: RowDecoder (f p)


-- Datatype wrapper: transparent.
instance GRowEncoder f => GRowEncoder (M1 D m f) where
  gRowEncoder = unM1 `contramap` gRowEncoder
  {-# INLINE gRowEncoder #-}


instance GRowDecoder f => GRowDecoder (M1 D m f) where
  gRowDecoder = M1 <$> gRowDecoder
  {-# INLINE gRowDecoder #-}


-- Constructor wrapper: transparent.
instance GRowEncoder f => GRowEncoder (M1 C m f) where
  gRowEncoder = unM1 `contramap` gRowEncoder
  {-# INLINE gRowEncoder #-}


instance GRowDecoder f => GRowDecoder (M1 C m f) where
  gRowDecoder = M1 <$> gRowDecoder
  {-# INLINE gRowDecoder #-}


-- Product: concatenate via RowEncoder's Semigroup.
instance (GRowEncoder f, GRowEncoder g) => GRowEncoder (f :*: g) where
  gRowEncoder =
    ((\(l :*: _) -> l) `contramap` gRowEncoder)
      <> ((\(_ :*: r) -> r) `contramap` gRowEncoder)
  {-# INLINE gRowEncoder #-}


instance (GRowDecoder f, GRowDecoder g) => GRowDecoder (f :*: g) where
  gRowDecoder = (:*:) <$> gRowDecoder <*> gRowDecoder
  {-# INLINE gRowDecoder #-}


-- Record selector: pull the selector name via 'Selector' and
-- pair it with the field type's HasEncoder / HasDecoder instance.
instance (Selector s, HasEncoder a) => GRowEncoder (S1 s (Rec0 a)) where
  gRowEncoder =
    let selectorName = T.pack (selName (undefined :: S1 s (Rec0 a) p))
    in (unK1 . unM1)
         `contramap` fieldE selectorName id (hasEncoder :: Encoder a)
  {-# INLINE gRowEncoder #-}


instance (Selector s, HasDecoder a) => GRowDecoder (S1 s (Rec0 a)) where
  gRowDecoder =
    let selectorName = T.pack (selName (undefined :: S1 s (Rec0 a) p))
    in (M1 . K1) <$> columnD selectorName (hasDecoder :: Decoder a)
  {-# INLINE gRowDecoder #-}
