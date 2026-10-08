{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | @hasql@-shaped encoder / decoder combinators for Arrow's
columnar data model.

Five complementary abstractions:

* 'Encoder' @a@: column-level encoder, 'Contravariant'.
  Primitives 'int32E', 'utf8E', 'boolE', ... reshape with
  @'contramap' :: (a -> b) -> Encoder b -> Encoder a@ and
  @'nullable' :: Encoder a -> Encoder (Maybe a)@.

* 'Decoder' @a@: column-level decoder, 'Functor'. Mirror set
  of primitives ('int32D', ...) with @'nullableD'@.

* 'RowEncoder' @r@: record-level encoder. Combine
  'fieldE' calls via 'Semigroup' @<>@.

* 'RowDecoder' @r@: 'Applicative' row decoder. Build with
  @<$>@ + @<*>@ + 'columnD'.

* 'Table' @r@: pairs the two for round-trip use.

== Cost model

Encoding allocates one column buffer per field, sized for the row
count (fixed-width values and booleans are stored straight into their
slot; strings and bytes go through "Arrow.Column.Builder"), then
traverses the input records once, appending each record to every
column. Field selectors, 'contramap' and the 'nullable' unwrapping
are fused into that loop, so no intermediate vector and no per-row
heap object is allocated. Decoding binds each column once (type
check, dictionary handling) to a row reader, composes the readers
through the 'Applicative', and runs a single 'V.generate' over the
rows: the only per-row allocation is the record itself and its boxed
fields ('Text' fields copy their bytes once, 'ByteString' fields
alias the input column).

== Example

@
data Trade = Trade { sym :: Text, qty :: Int32, note :: Maybe Text }

tradeTable :: 'Table' Trade
tradeTable = 'table' enc dec
  where
    enc = 'fieldE' "sym"  sym  'utf8E'
       <> 'fieldE' "qty"  qty  'int32E'
       <> 'fieldE' "note" note ('nullable' 'utf8E')
    dec = Trade
        \<$\> 'columnD' "sym"  'utf8D'
        \<*\> 'columnD' "qty"  'int32D'
        \<*\> 'columnD' "note" ('nullableD' 'utf8D')

encoded = 'encodeTable' tradeTable tradesVec
@
-}
module Arrow.Record (
  -- * Column-level encoder
  Encoder,
  encoderType,
  encoderNullable,
  runEncoder,
  contramapE,
  nullable,

  -- ** Primitive encoders
  int8E,
  int16E,
  int32E,
  int64E,
  word8E,
  word16E,
  word32E,
  word64E,
  floatE,
  doubleE,
  boolE,
  utf8E,
  binaryE,
  date32E,
  timestampE,

  -- * Column-level decoder
  Decoder,
  decoderType,
  runDecoder,
  nullableD,

  -- ** Primitive decoders
  int8D,
  int16D,
  int32D,
  int64D,
  word8D,
  word16D,
  word32D,
  word64D,
  floatD,
  doubleD,
  boolD,
  utf8D,
  binaryD,
  date32D,
  timestampD,

  -- * Row-level encoder
  RowEncoder,
  rowEncoderFields,
  runRowEncoder,
  fieldE,
  structE,
  structEMaybe,

  -- * Row-level decoder
  RowDecoder,
  runRowDecoder,
  rowDecoderRequiredColumns,
  columnD,
  columnDWithDefault,
  structD,
  structDMaybe,

  -- * Column-name strategies
  NameStrategy (..),
  applyNameStrategy,

  -- * Table (round-trip pair)
  Table (..),
  table,
  tableSchema,
  tableRequiredColumns,
  encodeTable,
  decodeTable,

  -- * Subset / projection
  subsetTable,
  projectTable,
) where

import Arrow.Column (
  BoolArray (..),
  ColumnArray,
  ColumnBuilder (..),
  PrimArray (..),
  PrimType (..),
  anyBytesAt,
  anyTextAt,
  appendBytes,
  appendText,
  asBinary,
  asBool,
  asLargeBinary,
  asLargeUtf8,
  asPrim,
  asUtf8,
  bitAt,
  boolArrayAt,
  columnLength,
  columnTag,
  expandDictionary,
  fillerColumn,
  isValidAt,
  mkBitmap,
  mkBool,
  mkPrim,
  mkStruct,
  mkValidity,
  newBinaryBuilder,
  newUtf8Builder,
  nullCount,
  primColumn,
  unsafeBytesAt,
  unsafePrimAt,
  unsafeTextAt,
  validity,
  validityGenerate,
  pattern ColBinaryView,
  pattern ColDictionary,
  pattern ColFixedSizeBinary,
  pattern ColPrim,
  pattern ColStruct,
  pattern ColUtf8View,
 )
import Arrow.Column qualified as AC
import Arrow.Types (
  ArrowType (..),
  DateUnit (..),
  Endianness (..),
  Field (..),
  Precision (..),
  Schema (..),
  TimeUnit (..),
 )
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits (complement, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff, sizeOf)
import GHC.Exts (Int (I#), Int#)
import GHC.ForeignPtr (unsafeWithForeignPtr)


-- ============================================================
-- Encoder
-- ============================================================

{- | Serialises Haskell values as an Arrow column.

Pairs the Arrow type (used to populate the schema) with a column
sink: given the row count, it allocates the column's buffers and
returns an append action (indexed by row), a null-append action and a
freeze. 'contramap' composes the projection into the append action,
so a field selector runs inside the encoding loop and no intermediate
vector is built.
-}
data Encoder a = Encoder
  { encoderType :: ArrowType
  , encoderNullable :: Bool
  , encoderSink :: forall s. Int -> ST s (ColSink s a)
  }


{- | A column under construction, addressed by row index (rows are
appended in order @0, 1, ..@ exactly once each): append a value,
append a null, freeze.
-}
data ColSink s a = ColSink (Int# -> a -> ST s ()) (Int# -> ST s ()) (ST s ColumnArray)


-- | Encode a column with one row per input value.
runEncoder :: Encoder a -> V.Vector a -> ColumnArray
runEncoder e v = runST $ do
  ColSink push _ freeze <- encoderSink e (V.length v)
  V.imapM_ (\(I# i) x -> push i x) v
  freeze
{-# INLINE runEncoder #-}


instance Contravariant Encoder where
  contramap f (Encoder ty nu mk) =
    Encoder
      { encoderType = ty
      , encoderNullable = nu
      , encoderSink = \n -> do
          ColSink push pushNull freeze <- mk n
          pure (ColSink (\i x -> push i (f x)) pushNull freeze)
      }
  {-# INLINE contramap #-}


{- | Alias for 'contramap' that reads more naturally at call
sites.
-}
contramapE :: (a -> b) -> Encoder b -> Encoder a
contramapE = contramap
{-# INLINE contramapE #-}


{- | Lift an encoder to build a nullable Arrow column: 'Nothing'
rows are null. Arrow has no nested-null representation, so running
an encoder wrapped in 'nullable' twice is a runtime error.
-}
nullable :: Encoder a -> Encoder (Maybe a)
nullable e =
  Encoder
    { encoderType = encoderType e
    , encoderNullable = True
    , encoderSink = \n ->
        if encoderNullable e
          then
            errorWithoutStackTrace
              "Arrow.Record.nullable: Arrow has no nested-null \
              \representation; don't wrap 'nullable' twice"
          else do
            ColSink push pushNull freeze <- encoderSink e n
            let pushMaybe i = \case
                  Nothing -> pushNull i
                  Just x -> push i x
            pure (ColSink pushMaybe pushNull freeze)
    }
{-# INLINE nullable #-}


{- | Fixed-width encoder. The row count is known up front, so values
are stored straight into their slot of one pinned buffer (a single
store per row); nulls are recorded in a side mask and only turned
into a validity bitmap if one occurred.
-}
primE :: Storable a => ArrowType -> PrimType a -> Encoder a
primE ty t =
  Encoder
    { encoderType = ty
    , encoderNullable = False
    , encoderSink = \n -> do
        vals <- VSM.unsafeNew n
        nulls <- newNullMask n
        let pushNull i = do
              zeroSlot vals (I# i)
              markNull nulls (I# i)
            freeze = do
              v <- VS.unsafeFreeze vals
              valid <- freezeNullMask nulls n
              pure $ case valid of
                Nothing -> primColumn t v
                Just _ -> encoded "primE" (mkPrim t valid v)
        pure (ColSink (\i x -> VSM.unsafeWrite vals (I# i) x) pushNull freeze)
    }
{-# INLINE primE #-}


-- | Zero a null row's value slot (deterministic bytes under nulls).
zeroSlot :: forall s a. Storable a => VSM.MVector s a -> Int -> ST s ()
zeroSlot vals i = unsafeIOToST $ do
  let (fp, _) = VSM.unsafeToForeignPtr0 vals
      !w = sizeOf (undefined :: a)
  unsafeWithForeignPtr fp $ \p -> fillBytes (castPtr p `plusPtr` (i * w)) 0 w


-- | Unwrap a column built from rows that satisfy its invariants by construction.
encoded :: String -> Either String ColumnArray -> ColumnArray
encoded who = \case
  Right c -> c
  Left e -> errorWithoutStackTrace ("Arrow.Record." ++ who ++ ": " ++ e)


{- | Null rows seen so far: a zeroed bit per row (set = null) and the
null count.
-}
data NullMask s = NullMask (ForeignPtr Word8) (MutablePrimArray s Int)


newNullMask :: Int -> ST s (NullMask s)
newNullMask n = do
  bits <- newBits n
  count <- newPrimArray 1
  writePrimArray count 0 0
  pure (NullMask bits count)


markNull :: NullMask s -> Int -> ST s ()
markNull (NullMask bits count) i = do
  setBit' bits i
  c <- readPrimArray count 0
  writePrimArray count 0 (c + 1)
{-# INLINE markNull #-}


-- | The validity of @n@ rows: 'Nothing' when no row was null.
freezeNullMask :: NullMask s -> Int -> ST s (Maybe AC.Validity)
freezeNullMask (NullMask bits count) n = do
  c <- readPrimArray count 0
  if c == 0
    then pure Nothing
    else unsafeIOToST $ do
      let !len = bitBytes n
      valid <- BSI.mallocByteString len
      unsafeWithForeignPtr bits $ \src -> unsafeWithForeignPtr valid $ \dst ->
        let go j
              | j >= len = pure ()
              | otherwise = do
                  b <- peekByteOff src j :: IO Word8
                  pokeByteOff dst j (complement b)
                  go (j + 1)
        in go 0
      pure $ case mkBitmap (BSI.BS valid len) 0 n of
        Right bm -> mkValidity bm
        Left e -> errorWithoutStackTrace ("Arrow.Record: validity bitmap: " ++ e)


bitBytes :: Int -> Int
bitBytes n = (n + 7) `unsafeShiftR` 3
{-# INLINE bitBytes #-}


-- | A zeroed, pinned bitmap with room for @n@ bits.
newBits :: Int -> ST s (ForeignPtr Word8)
newBits n = unsafeIOToST $ do
  let !len = bitBytes n
  fp <- BSI.mallocByteString len
  unsafeWithForeignPtr fp $ \p -> fillBytes p 0 len
  pure fp


setBit' :: ForeignPtr Word8 -> Int -> ST s ()
setBit' bits i = unsafeIOToST $ unsafeWithForeignPtr bits $ \p -> do
  let !byte = i `unsafeShiftR` 3
  b <- peekByteOff p byte :: IO Word8
  pokeByteOff p byte (b .|. (1 `unsafeShiftL` (i .&. 7)))
{-# INLINE setBit' #-}


-- ============================================================
-- Primitive encoders
-- ============================================================

int8E :: Encoder Int8
int8E = primE (AInt 8 True) PInt8
{-# INLINE int8E #-}


int16E :: Encoder Int16
int16E = primE (AInt 16 True) PInt16
{-# INLINE int16E #-}


int32E :: Encoder Int32
int32E = primE (AInt 32 True) PInt32
{-# INLINE int32E #-}


int64E :: Encoder Int64
int64E = primE (AInt 64 True) PInt64
{-# INLINE int64E #-}


word8E :: Encoder Word8
word8E = primE (AInt 8 False) PUInt8
{-# INLINE word8E #-}


word16E :: Encoder Word16
word16E = primE (AInt 16 False) PUInt16
{-# INLINE word16E #-}


word32E :: Encoder Word32
word32E = primE (AInt 32 False) PUInt32
{-# INLINE word32E #-}


word64E :: Encoder Word64
word64E = primE (AInt 64 False) PUInt64
{-# INLINE word64E #-}


floatE :: Encoder Float
floatE = primE (AFloatingPoint Single) PFloat
{-# INLINE floatE #-}


doubleE :: Encoder Double
doubleE = primE (AFloatingPoint DoublePrecision) PDouble
{-# INLINE doubleE #-}


{- | One bit per row, set in place in a pinned bitmap sized for the
row count.
-}
boolE :: Encoder Bool
boolE =
  Encoder
    { encoderType = ABool
    , encoderNullable = False
    , encoderSink = \n -> do
        bits <- newBits n
        nulls <- newNullMask n
        let push i b = if b then setBit' bits (I# i) else pure ()
            freeze = do
              valid <- freezeNullMask nulls n
              pure $ case mkBitmap (BSI.BS bits (bitBytes n)) 0 n of
                Right bm -> encoded "boolE" (mkBool valid bm)
                Left e -> errorWithoutStackTrace ("Arrow.Record.boolE: " ++ e)
        pure (ColSink push (\i -> markNull nulls (I# i)) freeze)
    }
{-# INLINE boolE #-}


utf8E :: Encoder Text
utf8E = builderE AUtf8 newUtf8Builder appendText
{-# INLINE utf8E #-}


binaryE :: Encoder ByteString
binaryE = builderE ABinary newBinaryBuilder appendBytes
{-# INLINE binaryE #-}


{- | Var-length encoder over a growable column builder sized for the
row count (the builder appends offsets in order, so the row index is
not needed).
-}
builderE
  :: ColumnBuilder b
  => ArrowType
  -> (forall s. Int -> ST s (b s))
  -> (forall s. b s -> a -> ST s ())
  -> Encoder a
builderE ty new app =
  Encoder
    { encoderType = ty
    , encoderNullable = False
    , encoderSink = \n -> do
        b <- new n
        pure (ColSink (\_ x -> app b x) (\_ -> appendNull b) (freezeBuilder b))
    }
{-# INLINE builderE #-}


-- | Days since Unix epoch (INT32). Arrow logical @Date(DateDay)@.
date32E :: Encoder Int32
date32E = primE (ADate DateDay) PDate32
{-# INLINE date32E #-}


{- | Microseconds since Unix epoch (INT64, no timezone). Arrow
logical @Timestamp(Microsecond, None)@.
-}
timestampE :: Encoder Int64
timestampE = primE (ATimestamp Microsecond Nothing) PTimestamp
{-# INLINE timestampE #-}


-- ============================================================
-- Decoder
-- ============================================================

{- | A bound column (or row) reader: row index to value. The index is
unboxed so that calling a reader through an unknown closure passes it
in a register instead of allocating a box per row.
-}
type Reader a = Int# -> a


{- | Reads Haskell values out of an Arrow column.

A decoder binds a column once (type check, dictionary handling) and
yields a reader that indexes row @i@ straight out of the column's
buffers. 'nullableD' switches to the second binder, which yields a
@Maybe a@ reader and accepts columns with nulls.

Dictionary-encoded columns of the decoder's value type are accepted:
nullable decoders read the values through the keys without expanding
the dictionary; required decoders expand it first ('AC.expandDictionary'),
which keeps the plain-column path a single, fully specialisable branch.
-}
data Decoder a = Decoder
  { decoderType :: ArrowType
  , decoderBind :: ColumnArray -> Either String (Reader a)
  , decoderBindMaybe :: ColumnArray -> Either String (Reader (Maybe a))
  }


instance Functor Decoder where
  fmap f (Decoder ty b bm) =
    Decoder
      { decoderType = ty
      , decoderBind = \c -> case b c of
          Left e -> Left e
          Right g -> Right (\i -> f (g i))
      , decoderBindMaybe = \c -> case bm c of
          Left e -> Left e
          Right g -> Right (\i -> fmap f (g i))
      }
  {-# INLINE fmap #-}


-- | Decode a column without nulls into a vector of values.
runDecoder :: Decoder a -> ColumnArray -> Either String (V.Vector a)
runDecoder d col = case decoderBind d col of
  Left e -> Left e
  Right g -> Right (V.generate (columnLength col) (\(I# i) -> g i))


{- | Lift a 'Decoder' to read nullable columns: null rows become
'Nothing'. Arrow has no nested-null representation, so decoding
through a second 'nullableD' wrap returns 'Left'.
-}
nullableD :: Decoder a -> Decoder (Maybe a)
nullableD d =
  Decoder
    { decoderType = decoderType d
    , decoderBind = decoderBindMaybe d
    , decoderBindMaybe = \_ ->
        Left
          "Arrow.Record.nullableD: Arrow has no nested-null \
          \representation; don't wrap 'nullableD' twice"
    }
{-# INLINE nullableD #-}


{- | Build a decoder from binders for the plain (non-dictionary)
column shapes, adding dictionary support. Required decoders expand a
dictionary column first (so a key selecting a null value is a null
row, rejected like any other); nullable decoders read the values
through the keys without materialising anything.
-}
dictD
  :: ArrowType
  -> (ColumnArray -> Either String (Reader a))
  -> (ColumnArray -> Either String (Reader (Maybe a)))
  -> Decoder a
dictD ty req opt = Decoder ty bindReq bindOpt
  where
    bindReq col = case undictionary col of
      Left e -> Left e
      Right plain -> req plain
    bindOpt col = case col of
      ColDictionary _ keys vals -> do
        checkResolved keys vals
        gv <- opt vals
        g <- throughKeys keys gv
        Right $ case validity keys of
          Nothing -> g
          kv -> \i -> if isValidAt kv (I# i) then g i else Nothing
      _ -> opt col
{-# INLINE dictD #-}


{- | A dictionary column expanded to its value type; other columns
unchanged. Kept out of line so that a required decoder's binder calls
its plain-column reader once, letting GHC specialise the reader into
the row function.
-}
{-# NOINLINE undictionary #-}
undictionary :: ColumnArray -> Either String ColumnArray
undictionary col = case col of
  ColDictionary _ keys vals -> do
    checkResolved keys vals
    expandDictionary col
  _ -> Right col


{- | An unresolved dictionary column (values still the empty
placeholder) can only be read when every key is null.
-}
checkResolved :: ColumnArray -> ColumnArray -> Either String ()
checkResolved keys vals
  | columnLength vals == 0 && nullCount keys < columnLength keys =
      Left "Arrow.Record: dictionary column has unresolved (empty) values"
  | otherwise = Right ()


{- | Compose a value-row reader with a dictionary's keys. Keys are an
integer column at wire width; every valid key is in range of the
values (a 'ColDictionary' invariant, checked by 'checkResolved' for
the placeholder case). The result reads key slots without consulting
key validity; callers mask nulls.
-}
throughKeys :: forall b. ColumnArray -> Reader b -> Either String (Reader b)
throughKeys keys gv = case keys of
  ColPrim t _ ks -> case t of
    PInt8 -> Right (via ks)
    PInt16 -> Right (via ks)
    PInt32 -> Right (via ks)
    PInt64 -> Right (via ks)
    PUInt8 -> Right (via ks)
    PUInt16 -> Right (via ks)
    PUInt32 -> Right (via ks)
    PUInt64 -> Right (via ks)
    _ -> Left ("Arrow.Record: dictionary keys must be integers, got " ++ columnTag keys)
  _ -> Left ("Arrow.Record: dictionary keys must be integers, got " ++ columnTag keys)
  where
    via :: (Storable k, Integral k) => VS.Vector k -> Reader b
    via ks i = case fromIntegral (VS.unsafeIndex ks (I# i)) of I# k -> gv k
    {-# INLINE via #-}


expectErr :: String -> ColumnArray -> String
expectErr want got =
  "Arrow.Record: expected " ++ want ++ ", got " ++ columnTag got


nullsErr :: ColumnArray -> String
nullsErr col =
  "Arrow.Record: "
    ++ columnTag col
    ++ " column has "
    ++ show (nullCount col)
    ++ " null rows; decode it with nullableD"


{- | Fixed-width decoder: the column must carry the given tag
(dictionary columns of that value type are accepted, see 'dictD').
-}
primD :: Storable a => ArrowType -> PrimType a -> String -> Decoder a
primD ty t want = dictD ty req opt
  where
    req col = case asPrim t col of
      Just (PrimArray Nothing xs) -> Right (\i -> VS.unsafeIndex xs (I# i))
      Just _ -> Left (nullsErr col)
      Nothing -> Left (expectErr want col)
    opt col = case asPrim t col of
      Just (PrimArray Nothing xs) -> Right (\i -> let !x = VS.unsafeIndex xs (I# i) in Just x)
      Just arr -> Right $ \i -> case unsafePrimAt arr (I# i) of
        Nothing -> Nothing
        Just x -> x `seq` Just x
      Nothing -> Left (expectErr want col)
{-# INLINE primD #-}


-- ============================================================
-- Primitive decoders
-- ============================================================

int8D :: Decoder Int8
int8D = primD (AInt 8 True) PInt8 "ColInt8"
{-# INLINE int8D #-}


int16D :: Decoder Int16
int16D = primD (AInt 16 True) PInt16 "ColInt16"
{-# INLINE int16D #-}


int32D :: Decoder Int32
int32D = primD (AInt 32 True) PInt32 "ColInt32"
{-# INLINE int32D #-}


int64D :: Decoder Int64
int64D = primD (AInt 64 True) PInt64 "ColInt64"
{-# INLINE int64D #-}


word8D :: Decoder Word8
word8D = primD (AInt 8 False) PUInt8 "ColUInt8"
{-# INLINE word8D #-}


word16D :: Decoder Word16
word16D = primD (AInt 16 False) PUInt16 "ColUInt16"
{-# INLINE word16D #-}


word32D :: Decoder Word32
word32D = primD (AInt 32 False) PUInt32 "ColUInt32"
{-# INLINE word32D #-}


word64D :: Decoder Word64
word64D = primD (AInt 64 False) PUInt64 "ColUInt64"
{-# INLINE word64D #-}


floatD :: Decoder Float
floatD = primD (AFloatingPoint Single) PFloat "ColFloat"
{-# INLINE floatD #-}


doubleD :: Decoder Double
doubleD = primD (AFloatingPoint DoublePrecision) PDouble "ColDouble"
{-# INLINE doubleD #-}


boolD :: Decoder Bool
boolD = dictD ABool req opt
  where
    req col = case asBool col of
      Just (BoolArray Nothing bits) -> Right (\i -> bitAt bits (I# i))
      Just _ -> Left (nullsErr col)
      Nothing -> Left (expectErr "ColBool" col)
    opt col = case asBool col of
      Just (BoolArray Nothing bits) -> Right (\i -> let !b = bitAt bits (I# i) in Just b)
      Just arr -> Right (\i -> boolArrayAt arr (I# i))
      Nothing -> Left (expectErr "ColBool" col)
{-# INLINE boolD #-}


{- | Text from a utf8, large utf8 or utf8 view column; each row is
copied once into a fresh 'Text' (the column was validated as UTF-8
when it was built or decoded).
-}
utf8D :: Decoder Text
utf8D = dictD AUtf8 req opt
  where
    req col
      | Just arr <- asUtf8 col = noNulls col (\i -> orEmpty T.empty (unsafeTextAt arr (I# i)))
      | Just arr <- asLargeUtf8 col = noNulls col (\i -> orEmpty T.empty (unsafeTextAt arr (I# i)))
      | ColUtf8View {} <- col = noNulls col (\i -> orEmpty T.empty (anyTextAt col (I# i)))
      | otherwise = Left (expectErr "ColUtf8" col)
    opt col
      | Just arr <- asUtf8 col = Right (\i -> forceJust (unsafeTextAt arr (I# i)))
      | Just arr <- asLargeUtf8 col = Right (\i -> forceJust (unsafeTextAt arr (I# i)))
      | ColUtf8View {} <- col = Right (\i -> forceJust (anyTextAt col (I# i)))
      | otherwise = Left (expectErr "ColUtf8" col)
{-# INLINE utf8D #-}


-- | A required decoder's row reader, provided the column has no nulls.
noNulls :: ColumnArray -> Reader a -> Either String (Reader a)
noNulls col g
  | nullCount col > 0 = Left (nullsErr col)
  | otherwise = Right g
{-# INLINE noNulls #-}


{- | The value of a row known to be valid (the accessors only return
'Nothing' for null rows, which 'noNulls' excluded).
-}
orEmpty :: a -> Maybe a -> a
orEmpty def = \case
  Just x -> x
  Nothing -> def
{-# INLINE orEmpty #-}


-- | Evaluate the payload of a 'Just' so the row holds no thunk.
forceJust :: Maybe a -> Maybe a
forceJust = \case
  Nothing -> Nothing
  Just x -> x `seq` Just x
{-# INLINE forceJust #-}


{- | Bytes from a binary, large binary, binary view or fixed-size
binary column (utf8 columns are accepted too). Rows are zero-copy
slices that keep the column's buffer alive.
-}
binaryD :: Decoder ByteString
binaryD = dictD ABinary req opt
  where
    req col
      | Just arr <- asBinary col = noNulls col (\i -> orEmpty BS.empty (unsafeBytesAt arr (I# i)))
      | Just arr <- asLargeBinary col = noNulls col (\i -> orEmpty BS.empty (unsafeBytesAt arr (I# i)))
      | isViewOrFixed col = noNulls col (\i -> orEmpty BS.empty (anyBytesAt col (I# i)))
      | otherwise = Left (expectErr "ColBinary" col)
    opt col
      | Just arr <- asBinary col = Right (\i -> forceJust (unsafeBytesAt arr (I# i)))
      | Just arr <- asLargeBinary col = Right (\i -> forceJust (unsafeBytesAt arr (I# i)))
      | isViewOrFixed col = Right (\i -> forceJust (anyBytesAt col (I# i)))
      | otherwise = Left (expectErr "ColBinary" col)
{-# INLINE binaryD #-}


isViewOrFixed :: ColumnArray -> Bool
isViewOrFixed = \case
  ColBinaryView {} -> True
  ColUtf8View {} -> True
  ColFixedSizeBinary {} -> True
  _ -> False


date32D :: Decoder Int32
date32D = primD (ADate DateDay) PDate32 "ColDate32"
{-# INLINE date32D #-}


timestampD :: Decoder Int64
timestampD = primD (ATimestamp Microsecond Nothing) PTimestamp "ColTimestamp"
{-# INLINE timestampD #-}


-- ============================================================
-- RowEncoder
-- ============================================================

{- | A record-level encoder: produces one 'ColumnArray' per field,
plus the matching 'Field' list, from a 'V.Vector' of records.

'RowEncoder' is 'Contravariant' and a 'Semigroup' / 'Monoid'.
Combine 'fieldE' calls with @<>@:

@
enc = 'fieldE' "sym" sym utf8E <> 'fieldE' "qty" qty int32E
@
-}
data RowEncoder r = RowEncoder
  { rowEncoderFields :: [Field]
  -- ^ 'Field' entries in declaration order.
  , rowEncoderSink :: forall s x. V.Vector x -> (x -> r) -> ST s (RowSink s r)
  {- ^ Allocate one builder per field for the given input rows (seen
  through a projection, for encoders that need to look at every row
  up front) and return the per-row append action and the freeze.
  -}
  }


{- | Rows under construction: append record @i@ to every field's
builder, freeze every builder (one column per field).
-}
data RowSink s r = RowSink (Int# -> r -> ST s ()) (ST s [ColumnArray])


instance Contravariant RowEncoder where
  contramap f (RowEncoder fields mk) = RowEncoder fields $ \v g -> do
    RowSink push freeze <- mk v (\x -> f (g x))
    pure (RowSink (\i r -> push i (f r)) freeze)
  {-# INLINE contramap #-}


instance Semigroup (RowEncoder r) where
  RowEncoder fl ml <> RowEncoder fr mr = RowEncoder (fl ++ fr) $ \v g -> do
    RowSink pl zl <- ml v g
    RowSink pr zr <- mr v g
    pure (RowSink (\i r -> pl i r >> pr i r) ((++) <$> zl <*> zr))
  {-# INLINE (<>) #-}


instance Monoid (RowEncoder r) where
  mempty = RowEncoder [] (\_ _ -> pure (RowSink (\_ _ -> pure ()) (pure [])))
  {-# INLINE mempty #-}


{- | One 'ColumnArray' per field, parallel to 'rowEncoderFields'. The
rows are traversed once; each record is appended to every field's
builder.
-}
runRowEncoder :: RowEncoder r -> V.Vector r -> [ColumnArray]
runRowEncoder e v = runST $ do
  RowSink push freeze <- rowEncoderSink e v id
  V.imapM_ (\(I# i) r -> push i r) v
  freeze


leafField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
leafField name nu ty children =
  Field
    { fieldName = name
    , fieldNullable = nu
    , fieldType = ty
    , fieldChildren = children
    , fieldDictionary = Nothing
    , fieldMetadata = V.empty
    }


{- | Build a 'RowEncoder' for a single field: name + selector +
column encoder. The selector runs inside the encoder's builder loop.

@
fieldE "sym" tradeSym utf8E  :: RowEncoder Trade
@
-}
fieldE :: Text -> (r -> a) -> Encoder a -> RowEncoder r
fieldE name sel enc =
  RowEncoder [leafField name (encoderNullable enc) (encoderType enc) V.empty] $ \v _ -> do
    ColSink push _ freeze <- encoderSink enc (V.length v)
    pure (RowSink (\i r -> push i (sel r)) (fmap (: []) freeze))
{-# INLINE fieldE #-}


{- | Embed a nested record as a struct column.

Lifts a 'RowEncoder' for a child record type @c@ into a
'RowEncoder' for the parent @r@ that emits the child's
column tree under one named struct field. The struct's
children are exactly the child encoder's fields (in the
order they were declared with '<>').

@
data Address = Address { city :: Text, zip :: Text }
data Customer = Customer { name :: Text, addr :: Address }

addressEnc :: 'RowEncoder' Address
addressEnc = 'fieldE' "city" city utf8E
          <> 'fieldE' "zip"  zip  utf8E

customerEnc :: 'RowEncoder' Customer
customerEnc = 'fieldE'  "name" name  utf8E
           <> 'structE' "addr" addr  addressEnc
@
-}
structE :: Text -> (r -> c) -> RowEncoder c -> RowEncoder r
structE name sel inner =
  RowEncoder [leafField name False AStruct (V.fromList (rowEncoderFields inner))] $ \v g -> do
    RowSink push freeze <- rowEncoderSink inner v (\x -> sel (g x))
    pure $
      RowSink
        (\i r -> push i (sel r))
        (do cols <- freeze; pure [buildStruct "structE" (V.length v) Nothing (childNames inner) cols])
{-# INLINE structE #-}


{- | Like 'structE' but the parent rows are @Maybe c@: emits a
struct column with a validity bitmap. Child slots under a null
parent are arbitrary on the wire (Arrow spec, Layout.rst, "Struct
Layout"), so they repeat the first present row's value; if every
row is 'Nothing' the children are 'AC.fillerColumn' rows of the
child encoders' column shapes.

Pair with 'structDMaybe' on the read side.
-}
structEMaybe :: forall r c. Text -> (r -> Maybe c) -> RowEncoder c -> RowEncoder r
structEMaybe name sel inner =
  RowEncoder [leafField name True AStruct (V.fromList (rowEncoderFields inner))] $ \v g -> do
    let !n = V.length v
        pick x = sel (g x)
        assemble valid cols = [buildStruct "structEMaybe" n valid (childNames inner) cols]
    case V.find (isJust . pick) v >>= pick of
      Nothing -> do
        let !cols = map (fillerColumn n) (runRowEncoder inner (V.empty :: V.Vector c))
            !col = assemble (validityGenerate n (const False)) cols
        pure (RowSink (\_ _ -> pure ()) (pure col))
      Just present -> do
        let orPresent = fromMaybe present
            !valid = validityGenerate n (\i -> isJust (pick (V.unsafeIndex v i)))
        RowSink push freeze <- rowEncoderSink inner v (\x -> orPresent (pick x))
        pure (RowSink (\i r -> push i (orPresent (sel r))) (assemble valid <$> freeze))
{-# INLINE structEMaybe #-}


childNames :: RowEncoder c -> [Text]
childNames = map fieldName . rowEncoderFields


{- | Assemble an encoded struct. The children were built from the same
rows, so 'mkStruct' can only fail on a broken builder invariant.
-}
buildStruct :: String -> Int -> Maybe AC.Validity -> [Text] -> [ColumnArray] -> ColumnArray
buildStruct who n valid names cols =
  case mkStruct n valid (V.fromList (zip names cols)) of
    Right c -> c
    Left e -> errorWithoutStackTrace ("Arrow.Record." ++ who ++ ": " ++ e)


-- ============================================================
-- RowDecoder
-- ============================================================

{- | Row decoder. Looks up named columns in a
'V.Vector ColumnArray' (keyed by the schema's field names), binds
the matching 'Decoder' to each once, and composes the resulting
row readers.

'RowDecoder' is an 'Applicative': combine several 'columnD'
calls with @<$>@ + @<*>@ to build a record. Field values are
evaluated when the record is built (no thunk per field).
-}
data RowDecoder r = RowDecoder
  { rowDecoderRequiredColumns :: [Text]
  {- ^ Names of columns the decoder consults when run.
  Order matches first appearance in the applicative chain;
  duplicates removed. Useful for column projection: a
  caller can ask the source format to only materialise
  these columns rather than the whole record batch.
  -}
  , rowDecoderBind :: Int -> V.Vector Text -> V.Vector ColumnArray -> Either String (Reader r)
  {- ^ Given the number of rows that will be read, the column names
  and the columns (parallel), check and bind every column the
  decoder needs and return the row reader.
  -}
  }


instance Functor RowDecoder where
  fmap f (RowDecoder cs b) = RowDecoder cs $ \n names cols ->
    case b n names cols of
      Left e -> Left e
      Right g -> Right (\i -> let !x = g i in f x)
  {-# INLINE fmap #-}


instance Applicative RowDecoder where
  pure x = RowDecoder [] (\_ _ _ -> Right (\_ -> x))
  {-# INLINE pure #-}
  RowDecoder cF bF <*> RowDecoder cX bX = RowDecoder
    (mergeRequired cF cX)
    $ \n names cols -> case bF n names cols of
      Left e -> Left e
      Right gf -> case bX n names cols of
        Left e -> Left e
        Right gx -> Right (\i -> let !x = gx i in gf i x)
  {-# INLINE (<*>) #-}


{- | Order-preserving union of two 'rowDecoderRequiredColumns'
lists. Used by the Applicative instance to maintain the
"first appearance" order as decoders are combined.
-}
mergeRequired :: [Text] -> [Text] -> [Text]
mergeRequired xs ys = xs ++ filter (`notElem` xs) ys


{- | Decode a batch: field names and columns are parallel and every
column has the same number of rows.
-}
runRowDecoder :: RowDecoder r -> V.Vector Field -> V.Vector ColumnArray -> Either String (V.Vector r)
runRowDecoder d fields cols
  | V.length fields /= V.length cols =
      Left $
        "Arrow.Record: schema has "
          ++ show (V.length fields)
          ++ " fields but the batch has "
          ++ show (V.length cols)
          ++ " columns"
  | V.any ((/= n) . columnLength) cols =
      Left $
        "Arrow.Record: column lengths differ ("
          ++ show (V.toList (V.map columnLength cols))
          ++ ")"
  | otherwise = case rowDecoderBind d n (V.map fieldName fields) cols of
      Left e -> Left e
      Right g -> Right (V.generate n (\(I# i) -> g i))
  where
    !n = if V.null cols then 0 else columnLength (V.unsafeHead cols)


lookupColumn :: Text -> V.Vector Text -> V.Vector ColumnArray -> Maybe ColumnArray
lookupColumn name names cols = V.findIndex (== name) names >>= (cols V.!?)


-- | Bind a decoder to a column that must hold at least @n@ rows.
bindColumn :: String -> Text -> Decoder a -> Int -> ColumnArray -> Either String (Reader a)
bindColumn who name d n col
  | columnLength col < n =
      Left $
        prefix
          ++ "column has "
          ++ show (columnLength col)
          ++ " rows, expected "
          ++ show n
  | otherwise = case decoderBind d col of
      Left e -> Left (prefix ++ e)
      Right g -> Right g
  where
    prefix = "Arrow.Record." ++ who ++ " " ++ show name ++ ": "
{-# INLINE bindColumn #-}


{- | Decode the named column via the supplied 'Decoder'. Looks
the column up by 'Field' name in the schema the caller passes
to 'runRowDecoder'; returns 'Left' if the name isn't present.
-}
columnD :: Text -> Decoder a -> RowDecoder a
columnD name d = RowDecoder [name] $ \n names cols ->
  case lookupColumn name names cols of
    Nothing -> Left $ "Arrow.Record.columnD: no column named " ++ show name
    Just col -> bindColumn "columnD" name d n col
{-# INLINE columnD #-}


{- | Like 'columnD' but supplies a default value if the
column is missing from the source schema. Useful for
schema-evolution: an older Parquet file dropped a column
that the Haskell record still wants; instead of failing,
the decoder substitutes the default.

Decoding errors on a /present/ column still propagate
(e.g. wrong type); only "no such column" falls back.
-}
columnDWithDefault :: Text -> a -> Decoder a -> RowDecoder a
columnDWithDefault name def d = RowDecoder [name] $ \n names cols ->
  case lookupColumn name names cols of
    Nothing -> Right (\_ -> def)
    Just col -> bindColumn "columnDWithDefault" name d n col
{-# INLINE columnDWithDefault #-}


{- | Strategy for converting a record's selector name to its
on-the-wire column name. Mirrors the @renameStyle@ modifier
vocabulary in "Wireform.Derive".
-}
data NameStrategy
  = -- | Use the selector name unchanged.
    NameAsIs
  | {- | @userId@ → @user_id@. Inserts an underscore before any
    uppercase letter that follows a lowercase one and
    lower-cases the result.
    -}
    NameSnakeCase
  | {- | @user_id@ → @userId@. Drops underscores and
    upper-cases the following character.
    -}
    NameCamelCase
  | -- | @userId@ → @USER_ID@. snake-case + upper-case.
    NameUpperSnakeCase
  deriving (Show, Eq)


-- | Apply a 'NameStrategy' to a 'Text' selector name.
applyNameStrategy :: NameStrategy -> Text -> Text
applyNameStrategy NameAsIs = id
applyNameStrategy NameSnakeCase = T.toLower . toSnake
  where
    toSnake t = T.pack (go ' ' (T.unpack t))
    -- Walk char-by-char carrying the previous character so we
    -- can decide whether to insert an underscore before an
    -- uppercase letter:
    --
    --   * insert when the previous char was lowercase (the
    --     usual word-boundary case: userId -> user_id)
    --   * insert when the next char is lowercase /and/ the
    --     previous char was uppercase (acronym→word boundary:
    --     userIDValue -> user_id_value, where the _ before V
    --     comes from this rule)
    --
    -- Simple, deterministic, and matches what 'inflection' /
    -- ActiveSupport / serde do.
    go _ [] = []
    go prev (c : cs)
      | isUp c
      , isLow prev
          || ( isUp prev && case cs of
                 (n : _) -> isLow n
                 [] -> False
             ) =
          '_' : c : go c cs
      | otherwise = c : go c cs
    isUp c = c >= 'A' && c <= 'Z'
    isLow c = c >= 'a' && c <= 'z'
applyNameStrategy NameCamelCase = toCamel
  where
    toCamel t =
      let parts = T.splitOn (T.pack "_") t
      in case parts of
           [] -> T.empty
           (p : ps) -> T.concat (T.toLower p : map cap ps)
    cap t
      | T.null t = t
      | otherwise = T.cons (toUpper1 (T.head t)) (T.tail t)
    toUpper1 c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c
applyNameStrategy NameUpperSnakeCase =
  T.toUpper . applyNameStrategy NameSnakeCase


{- | Inverse of 'structE': decode a struct column at the given name as
a record using the supplied inner 'RowDecoder'. The inner decoder
sees the struct's children by name; the outer decoder threads the
nested record into the parent record's applicative chain like any
other column. A struct with null rows needs 'structDMaybe'.
-}
structD :: Text -> RowDecoder c -> RowDecoder c
structD name inner = RowDecoder [name] $ \n names cols ->
  bindStruct "structD" name inner n names cols $ \col v g -> case v of
    Nothing -> Right g
    Just _ -> Left $ "Arrow.Record.structD " ++ show name ++ ": " ++ nullsErr col
{-# INLINE structD #-}


{- | Like 'structD' but the struct may have null rows, which decode
to 'Nothing'. Structs without nulls are accepted too (every row
becomes 'Just').
-}
structDMaybe :: Text -> RowDecoder c -> RowDecoder (Maybe c)
structDMaybe name inner = RowDecoder [name] $ \n names cols ->
  bindStruct "structDMaybe" name inner n names cols $ \_ v g -> case v of
    Nothing -> Right (\i -> let !x = g i in Just x)
    Just _ -> Right (\i -> if isValidAt v (I# i) then let !x = g i in Just x else Nothing)
{-# INLINE structDMaybe #-}


{- | Find the struct column, bind the inner decoder to its children
(each holds at least as many rows as the struct), and hand the
struct's validity and the child row reader to the continuation.
-}
bindStruct
  :: String
  -> Text
  -> RowDecoder c
  -> Int
  -> V.Vector Text
  -> V.Vector ColumnArray
  -> (ColumnArray -> Maybe AC.Validity -> Reader c -> Either String (Reader r))
  -> Either String (Reader r)
bindStruct who name inner n names cols k =
  case lookupColumn name names cols of
    Nothing -> Left $ prefix ++ "no column named " ++ show name
    Just col -> case col of
      ColStruct m v children
        | m < n -> Left $ prefix ++ "struct has " ++ show m ++ " rows, expected " ++ show n
        | otherwise ->
            case rowDecoderBind inner n (V.map fst children) (V.map snd children) of
              Left e -> Left (prefix ++ e)
              Right g -> k col v g
      other -> Left $ prefix ++ "expected ColStruct, got " ++ columnTag other
  where
    prefix = "Arrow.Record." ++ who ++ " " ++ show name ++ ": "
{-# INLINE bindStruct #-}


-- ============================================================
-- Table
-- ============================================================

{- | Pairs a 'RowEncoder' with a 'RowDecoder' for one Haskell
record type. This is the handle you pass to the top-level
encode / decode helpers below.
-}
data Table r = Table
  { tableEncode :: RowEncoder r
  , tableDecode :: RowDecoder r
  }


{- | Smart constructor. Equivalent to @Table enc dec@ but reads
better in call-site positions.
-}
table :: RowEncoder r -> RowDecoder r -> Table r
table = Table
{-# INLINE table #-}


-- | Schema implied by the 'RowEncoder'.
tableSchema :: Table r -> Schema
tableSchema t =
  Schema
    { arrowFields = V.fromList (rowEncoderFields (tableEncode t))
    , arrowEndianness = Little
    , arrowMetadata = V.empty
    , arrowFeatures = V.empty
    }


{- | Names of the columns the 'Table''s decoder needs. Equivalent
to @'rowDecoderRequiredColumns' . 'tableDecode'@; surfaced
here so callers can drive column projection through a
'Table' without unpacking the inner 'RowDecoder'.
-}
tableRequiredColumns :: Table r -> [Text]
tableRequiredColumns = rowDecoderRequiredColumns . tableDecode


{- | Encode a vector of records as an Arrow batch + its schema.
The schema comes from 'tableSchema'; the batch is parallel to
'arrowFields' of that schema. Every column is built before the
pair is returned.
-}
encodeTable :: Table r -> V.Vector r -> (Schema, V.Vector ColumnArray)
encodeTable t rs =
  let !cols = V.fromList (runRowEncoder (tableEncode t) rs)
  in (tableSchema t, cols)


{- | Decode an Arrow batch into a vector of records. Looks up
columns by schema field name; returns 'Left' on missing
columns, type mismatches, nulls in a non-nullable decoder, or a
malformed batch (field and column counts or column lengths differ).
-}
decodeTable
  :: Table r
  -> Schema
  -> V.Vector ColumnArray
  -> Either String (V.Vector r)
decodeTable t sch = runRowDecoder (tableDecode t) (arrowFields sch)


-- ============================================================
-- Subset / projection
-- ============================================================

{- | Build a 'Table' for a subset of columns by name. The
resulting decoder ignores columns not in @keep@; the encoder
only emits the kept ones. Useful
for callers that have a single 'Table' and want to read or write
only a slice without writing a parallel @Table SubsetRecord@.

Returns 'Nothing' if any name in @keep@ isn't present in the
original table.
-}
subsetTable :: [Text] -> Table r -> Maybe (Table r)
subsetTable keep tbl = do
  let RowEncoder fields mk = tableEncode tbl
      byName nm = findIndex ((== nm) . fieldName) fields
  idxs <- traverse byName keep
  let !fieldVec = V.fromList fields
      pickCols cols = let !cv = V.fromList cols in map (V.unsafeIndex cv) idxs
  Just
    Table
      { tableEncode = RowEncoder (map (V.unsafeIndex fieldVec) idxs) $ \v g -> do
          RowSink push freeze <- mk v g
          pure (RowSink push (pickCols <$> freeze))
      , tableDecode = tableDecode tbl -- decoder looks columns up by name, so the subset is automatic
      }


{- | Project an existing batch by column name, in the order
listed. Returns 'Nothing' if any name is missing.

Together with @'subsetTable'@ this lets callers reuse one
'Table' definition across read paths that materialise
different column subsets.
-}
projectTable
  :: [Text]
  -> Schema
  -> V.Vector ColumnArray
  -> Maybe (Schema, V.Vector ColumnArray)
projectTable keep sch cols = do
  let !fields = arrowFields sch
  idxs <- V.fromList <$> traverse (\nm -> V.findIndex ((== nm) . fieldName) fields) keep
  newCols <- traverse (cols V.!?) idxs
  pure (sch {arrowFields = V.map (V.unsafeIndex fields) idxs}, newCols)
