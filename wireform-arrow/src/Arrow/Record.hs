{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | @hasql@-shaped encoder / decoder combinators for Arrow's
-- columnar data model.
--
-- Four complementary abstractions:
--
-- * 'Encoder' @a@ — column-level encoder, 'Contravariant'.
--   Primitives 'int32E', 'utf8E', 'boolE', … reshape with
--   @'contramap' :: (a -> b) -> Encoder b -> Encoder a@ and
--   @'nullable' :: Encoder a -> Encoder (Maybe a)@.
--
-- * 'Decoder' @a@ — column-level decoder, 'Functor'. Mirror set
--   of primitives ('int32D', …) with @'nullableD'@.
--
-- * 'RowEncoder' @r@ — record-level encoder. Combine
--   'fieldE' calls via 'Semigroup' @<>@.
--
-- * 'RowDecoder' @r@ — 'Applicative' row decoder. Build with
--   @<$>@ + @<*>@ + 'columnD'.
--
-- * 'Table' @r@ — pairs the two for round-trip use.
--
-- == Example
--
-- @
-- data Trade = Trade { sym :: Text, qty :: Int32, note :: Maybe Text }
--
-- tradeTable :: 'Table' Trade
-- tradeTable = 'table' enc dec
--   where
--     enc = 'fieldE' "sym"  sym  'utf8E'
--        <> 'fieldE' "qty"  qty  'int32E'
--        <> 'fieldE' "note" note ('nullable' 'utf8E')
--     dec = Trade
--         \<$\> 'columnD' "sym"  'utf8D'
--         \<*\> 'columnD' "qty"  'int32D'
--         \<*\> 'columnD' "note" ('nullableD' 'utf8D')
--
-- encoded = 'encodeTable' tradeTable tradesVec
-- @
module Arrow.Record
  ( -- * Column-level encoder
    Encoder
  , encoderType
  , encoderNullable
  , runEncoder
  , contramapE
  , nullable
    -- ** Primitive encoders
  , int8E, int16E, int32E, int64E
  , word8E, word16E, word32E, word64E
  , floatE, doubleE
  , boolE
  , utf8E
  , binaryE
  , date32E
  , timestampE
    -- * Column-level decoder
  , Decoder
  , decoderType
  , runDecoder
  , nullableD
    -- ** Primitive decoders
  , int8D, int16D, int32D, int64D
  , word8D, word16D, word32D, word64D
  , floatD, doubleD
  , boolD
  , utf8D
  , binaryD
  , date32D
  , timestampD
    -- * Row-level encoder
  , RowEncoder
  , rowEncoderFields
  , runRowEncoder
  , fieldE
  , structE
  , structEMaybe
    -- * Row-level decoder
  , RowDecoder
  , runRowDecoder
  , rowDecoderRequiredColumns
  , columnD
  , columnDWithDefault
  , structD
  , structDMaybe
    -- * Column-name strategies
  , NameStrategy (..)
  , applyNameStrategy
    -- * Table (round-trip pair)
  , Table (..)
  , table
  , tableSchema
  , tableRequiredColumns
  , encodeTable
  , decodeTable
    -- * Subset / projection
  , subsetTable
  , projectTable
  ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Columnar.Bit (Bit (..))
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Data.Maybe (fromMaybe, isJust)
import Arrow.Column (ColumnArray (..))
import Arrow.Types
  ( ArrowType (..)
  , DateUnit (..)
  , Endianness (..)
  , Field (..)
  , Precision (..)
  , Schema (..)
  , TimeUnit (..)
  )
-- AStruct is the constructor in ArrowType; explicit re-import
-- isn't needed because ArrowType (..) brings it in scope.

-- ============================================================
-- Encoder
-- ============================================================

-- | Serialises a vector of Haskell values as an Arrow column.
--
-- Pairs the Arrow type (used to populate the schema) with two
-- column-builder functions: one for required input, one for
-- 'Maybe'-wrapped input. Primitive encoders fill both;
-- 'contramap' threads a projection through both so a derived
-- encoder remains liftable via 'nullable'.
data Encoder a = Encoder
  { encoderType        :: !ArrowType
  , encoderNullable    :: !Bool
  , encoderRequired    :: !(V.Vector a -> ColumnArray)
  , encoderOptional    :: !(V.Vector (Maybe a) -> ColumnArray)
  }

-- | Encode a non-nullable column.
runEncoder :: Encoder a -> V.Vector a -> ColumnArray
runEncoder = encoderRequired

instance Contravariant Encoder where
  contramap f (Encoder ty nu req opt) = Encoder
    { encoderType      = ty
    , encoderNullable  = nu
    , encoderRequired  = req . V.map f
    , encoderOptional  = opt . V.map (fmap f)
    }

-- | Alias for 'contramap' that reads more naturally at call
-- sites.
contramapE :: (a -> b) -> Encoder b -> Encoder a
contramapE = contramap

-- | Lift an encoder to build a nullable Arrow column. The
-- inner encoder's @Maybe@-builder becomes the new required
-- builder; a second 'nullable' wrap is rejected at runtime
-- since Arrow has no nested-null representation.
nullable :: Encoder a -> Encoder (Maybe a)
nullable e = Encoder
  { encoderType      = encoderType e
  , encoderNullable  = True
  , encoderRequired  = encoderOptional e
  , encoderOptional  = \_ ->
      error "Arrow.Record.nullable: Arrow has no nested-null \
            \representation; don't wrap 'nullable' twice"
  }

-- Internal: primitive-encoder constructor.
mkE
  :: ArrowType
  -> (V.Vector a -> ColumnArray)
  -> (V.Vector (Maybe a) -> ColumnArray)
  -> Encoder a
mkE ty req opt = Encoder ty False req opt

-- ============================================================
-- Primitive encoders
-- ============================================================

-- All primitive encoders convert from a boxed @V.Vector a@
-- (the input shape every Encoder works with) into a packed
-- 'VS.Vector' for the constructor data, and into a
-- 'NullableView' for the nullable variant. The conversion
-- is one O(n) walk; the underlying storage is then
-- view-friendly (see 'docs/columnar-views-design.md').

int8E :: Encoder Int8
int8E = mkE (AInt 8 True) (ColInt8 . VS.convert) (ColInt8Maybe . AC.nvFromMaybeVector 0)

int16E :: Encoder Int16
int16E = mkE (AInt 16 True) (ColInt16 . VS.convert) (ColInt16Maybe . AC.nvFromMaybeVector 0)

int32E :: Encoder Int32
int32E = mkE (AInt 32 True) (ColInt32 . VS.convert) (ColInt32Maybe . AC.nvFromMaybeVector 0)

int64E :: Encoder Int64
int64E = mkE (AInt 64 True) (ColInt64 . VS.convert) (ColInt64Maybe . AC.nvFromMaybeVector 0)

word8E :: Encoder Word8
word8E = mkE (AInt 8 False) (ColUInt8 . VS.convert) (ColUInt8Maybe . AC.nvFromMaybeVector 0)

word16E :: Encoder Word16
word16E = mkE (AInt 16 False) (ColUInt16 . VS.convert) (ColUInt16Maybe . AC.nvFromMaybeVector 0)

word32E :: Encoder Word32
word32E = mkE (AInt 32 False) (ColUInt32 . VS.convert) (ColUInt32Maybe . AC.nvFromMaybeVector 0)

word64E :: Encoder Word64
word64E = mkE (AInt 64 False) (ColUInt64 . VS.convert) (ColUInt64Maybe . AC.nvFromMaybeVector 0)

floatE :: Encoder Float
floatE = mkE (AFloatingPoint Single) (ColFloat . VS.convert) (ColFloatMaybe . AC.nvFromMaybeVector 0)

doubleE :: Encoder Double
doubleE = mkE (AFloatingPoint DoublePrecision) (ColDouble . VS.convert) (ColDoubleMaybe . AC.nvFromMaybeVector 0)

boolE :: Encoder Bool
boolE = mkE ABool (ColBool . boolVecToBitVec) ColBoolMaybe

utf8E :: Encoder Text
utf8E = mkE AUtf8 (ColUtf8 . AV.utf8ViewFromVector) ColUtf8Maybe

binaryE :: Encoder ByteString
binaryE = mkE ABinary (ColBinary . AV.binaryViewFromVector) ColBinaryMaybe

-- | Days since Unix epoch (INT32). Arrow logical @Date(DateDay)@.
date32E :: Encoder Int32
date32E = mkE (ADate DateDay) (ColDate32 . VS.convert) (ColDate32Maybe . AC.nvFromMaybeVector 0)

-- | Microseconds since Unix epoch (INT64, no timezone). Arrow
-- logical @Timestamp(Microsecond, None)@.
timestampE :: Encoder Int64
timestampE = mkE (ATimestamp Microsecond Nothing) (ColTimestamp . VS.convert) (ColTimestampMaybe . AC.nvFromMaybeVector 0)

-- | Convert a boxed @V.Vector Bool@ into the packed
-- @VU.Vector Bit@ representation 'ColBool' now uses.
boolVecToBitVec :: V.Vector Bool -> VU.Vector Bit
boolVecToBitVec v = VU.generate (V.length v) (\i -> Bit (V.unsafeIndex v i))

-- ============================================================
-- Decoder
-- ============================================================

-- | Materialises a Haskell vector from an Arrow column.
--
-- The decoder stores two extractors: one for the non-nullable
-- 'ColumnArray' shape its 'decoderType' advertises, one for the
-- matching @Col*Maybe@. 'nullableD' flips to the second path so
-- 'fmap' composes through both in lockstep.
data Decoder a = Decoder
  { decoderType      :: !ArrowType
  , decoderRequired  :: !(ColumnArray -> Either String (V.Vector a))
  , decoderOptional  :: !(ColumnArray -> Either String (V.Vector (Maybe a)))
  }

instance Functor Decoder where
  fmap f (Decoder ty req opt) = Decoder
    { decoderType     = ty
    , decoderRequired = fmap (V.map f) . req
    , decoderOptional = fmap (V.map (fmap f)) . opt
    }

-- | Decode a non-nullable column into a vector of values.
runDecoder :: Decoder a -> ColumnArray -> Either String (V.Vector a)
runDecoder = decoderRequired

-- | Lift a 'Decoder' to recognise nullable columns. Wrapping
-- twice is a runtime error — Arrow has no nested-null
-- representation.
nullableD :: Decoder a -> Decoder (Maybe a)
nullableD d = Decoder
  { decoderType     = decoderType d
  , decoderRequired = decoderOptional d
  , decoderOptional = \_ ->
      Left "Arrow.Record.nullableD: Arrow has no nested-null \
            \representation; don't wrap 'nullableD' twice"
  }

-- Internal: primitive-decoder constructor.
mkD
  :: ArrowType
  -> (ColumnArray -> Either String (V.Vector a))
  -> (ColumnArray -> Either String (V.Vector (Maybe a)))
  -> Decoder a
mkD = Decoder

-- ============================================================
-- Primitive decoders
-- ============================================================

-- Each primitive decoder converts the packed VS.Vector
-- back to a boxed V.Vector for the consumer (the Decoder
-- API still hands out boxed vectors to consumers). The
-- nullable variant materialises the NullableView through
-- 'nvToMaybeVector'.

int8D :: Decoder Int8
int8D = mkD (AInt 8 True)
  (expectCol "ColInt8"      $ \case ColInt8      v -> Right (VS.convert v); o -> expectErr "ColInt8" o)
  (expectCol "ColInt8Maybe" $ \case ColInt8Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColInt8Maybe" o)

int16D :: Decoder Int16
int16D = mkD (AInt 16 True)
  (expectCol "ColInt16"      $ \case ColInt16      v -> Right (VS.convert v); o -> expectErr "ColInt16" o)
  (expectCol "ColInt16Maybe" $ \case ColInt16Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColInt16Maybe" o)

int32D :: Decoder Int32
int32D = mkD (AInt 32 True)
  (expectCol "ColInt32"      $ \case ColInt32      v -> Right (VS.convert v); o -> expectErr "ColInt32" o)
  (expectCol "ColInt32Maybe" $ \case ColInt32Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColInt32Maybe" o)

int64D :: Decoder Int64
int64D = mkD (AInt 64 True)
  (expectCol "ColInt64"      $ \case ColInt64      v -> Right (VS.convert v); o -> expectErr "ColInt64" o)
  (expectCol "ColInt64Maybe" $ \case ColInt64Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColInt64Maybe" o)

word8D :: Decoder Word8
word8D = mkD (AInt 8 False)
  (expectCol "ColUInt8"      $ \case ColUInt8      v -> Right (VS.convert v); o -> expectErr "ColUInt8" o)
  (expectCol "ColUInt8Maybe" $ \case ColUInt8Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColUInt8Maybe" o)

word16D :: Decoder Word16
word16D = mkD (AInt 16 False)
  (expectCol "ColUInt16"      $ \case ColUInt16      v -> Right (VS.convert v); o -> expectErr "ColUInt16" o)
  (expectCol "ColUInt16Maybe" $ \case ColUInt16Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColUInt16Maybe" o)

word32D :: Decoder Word32
word32D = mkD (AInt 32 False)
  (expectCol "ColUInt32"      $ \case ColUInt32      v -> Right (VS.convert v); o -> expectErr "ColUInt32" o)
  (expectCol "ColUInt32Maybe" $ \case ColUInt32Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColUInt32Maybe" o)

word64D :: Decoder Word64
word64D = mkD (AInt 64 False)
  (expectCol "ColUInt64"      $ \case ColUInt64      v -> Right (VS.convert v); o -> expectErr "ColUInt64" o)
  (expectCol "ColUInt64Maybe" $ \case ColUInt64Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColUInt64Maybe" o)

floatD :: Decoder Float
floatD = mkD (AFloatingPoint Single)
  (expectCol "ColFloat"      $ \case ColFloat      v -> Right (VS.convert v); o -> expectErr "ColFloat" o)
  (expectCol "ColFloatMaybe" $ \case ColFloatMaybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColFloatMaybe" o)

doubleD :: Decoder Double
doubleD = mkD (AFloatingPoint DoublePrecision)
  (expectCol "ColDouble"      $ \case ColDouble      v -> Right (VS.convert v); o -> expectErr "ColDouble" o)
  (expectCol "ColDoubleMaybe" $ \case ColDoubleMaybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColDoubleMaybe" o)

boolD :: Decoder Bool
boolD = mkD ABool
  (expectCol "ColBool"      $ \case ColBool      v -> Right (bitVecToBoolVec v); o -> expectErr "ColBool" o)
  (expectCol "ColBoolMaybe" $ \case ColBoolMaybe v -> Right v;                    o -> expectErr "ColBoolMaybe" o)

-- | Convert a packed @VU.Vector Bit@ back to the boxed
-- @V.Vector Bool@ representation the Decoder API exposes.
bitVecToBoolVec :: VU.Vector Bit -> V.Vector Bool
bitVecToBoolVec v = V.generate (VU.length v) (\i -> unBit (VU.unsafeIndex v i))

utf8D :: Decoder Text
utf8D = mkD AUtf8
  (expectCol "ColUtf8"      $ \case ColUtf8      v -> Right (AV.utf8ViewToVector v); o -> expectErr "ColUtf8" o)
  (expectCol "ColUtf8Maybe" $ \case ColUtf8Maybe v -> Right v; o -> expectErr "ColUtf8Maybe" o)

binaryD :: Decoder ByteString
binaryD = mkD ABinary
  (expectCol "ColBinary"      $ \case ColBinary      v -> Right (AV.binaryViewToVector v); o -> expectErr "ColBinary" o)
  (expectCol "ColBinaryMaybe" $ \case ColBinaryMaybe v -> Right v; o -> expectErr "ColBinaryMaybe" o)

date32D :: Decoder Int32
date32D = mkD (ADate DateDay)
  (expectCol "ColDate32"      $ \case ColDate32      v -> Right (VS.convert v); o -> expectErr "ColDate32" o)
  (expectCol "ColDate32Maybe" $ \case ColDate32Maybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColDate32Maybe" o)

timestampD :: Decoder Int64
timestampD = mkD (ATimestamp Microsecond Nothing)
  (expectCol "ColTimestamp"      $ \case ColTimestamp      v -> Right (VS.convert v); o -> expectErr "ColTimestamp" o)
  (expectCol "ColTimestampMaybe" $ \case ColTimestampMaybe v -> Right (AC.nvToMaybeVector v); o -> expectErr "ColTimestampMaybe" o)

-- Internal helpers shared by every primitive decoder.
expectCol :: String -> (ColumnArray -> Either String b) -> ColumnArray -> Either String b
expectCol _ k = k

expectErr :: String -> ColumnArray -> Either String a
expectErr want got =
  Left $ "Arrow.Record: expected " ++ want ++ ", got " ++ colTag got

colTag :: ColumnArray -> String
colTag = takeWhile (/= ' ') . show

-- ============================================================
-- RowEncoder
-- ============================================================

-- | A record-level encoder: produces a 'V.Vector ColumnArray' +
-- its 'Field' list from a 'V.Vector' of records.
--
-- 'RowEncoder' is 'Contravariant' and a 'Semigroup' / 'Monoid'.
-- Combine 'fieldE' calls with @<>@:
--
-- @
-- enc = 'fieldE' "sym" sym utf8E <> 'fieldE' "qty" qty int32E
-- @
data RowEncoder r = RowEncoder
  { rowEncoderFields :: ![Field]
    -- ^ 'Field' entries in declaration order.
  , runRowEncoder    :: !(V.Vector r -> [ColumnArray])
    -- ^ One 'ColumnArray' per field, parallel to
    -- 'rowEncoderFields'.
  }

instance Contravariant RowEncoder where
  contramap f (RowEncoder flds run) =
    RowEncoder flds (run . V.map f)

instance Semigroup (RowEncoder r) where
  RowEncoder fl rl <> RowEncoder fr rr =
    RowEncoder (fl ++ fr) (\v -> rl v ++ rr v)

instance Monoid (RowEncoder r) where
  mempty = RowEncoder [] (const [])

-- | Build a 'RowEncoder' for a single field: name + selector +
-- column encoder.
--
-- @
-- fieldE "sym" tradeSym utf8E  :: RowEncoder Trade
-- @
fieldE :: Text -> (r -> a) -> Encoder a -> RowEncoder r
fieldE name sel enc = RowEncoder
  { rowEncoderFields = [Field
      { fieldName       = name
      , fieldNullable   = encoderNullable enc
      , fieldType       = encoderType enc
      , fieldChildren   = V.empty
      , fieldDictionary = Nothing
      , fieldMetadata   = V.empty
      }]
  , runRowEncoder = \rs -> [runEncoder enc (V.map sel rs)]
  }

-- | Embed a nested record as a struct column.
--
-- Lifts a 'RowEncoder' for a child record type @c@ into a
-- 'RowEncoder' for the parent @r@ that emits the child's
-- column tree under one named @ColStruct@ field. The struct's
-- children are exactly the child encoder's fields (in the
-- order they were declared with '<>').
--
-- @
-- data Address = Address { city :: Text, zip :: Text }
-- data Customer = Customer { name :: Text, addr :: Address }
--
-- addressEnc :: 'RowEncoder' Address
-- addressEnc = 'fieldE' "city" city utf8E
--           <> 'fieldE' "zip"  zip  utf8E
--
-- customerEnc :: 'RowEncoder' Customer
-- customerEnc = 'fieldE'  "name" name  utf8E
--            <> 'structE' "addr" addr  addressEnc
-- @
structE :: Text -> (r -> c) -> RowEncoder c -> RowEncoder r
structE name sel inner = RowEncoder
  { rowEncoderFields = [Field
      { fieldName       = name
      , fieldNullable   = False
      , fieldType       = AStruct
      , fieldChildren   = V.fromList (rowEncoderFields inner)
      , fieldDictionary = Nothing
      , fieldMetadata   = V.empty
      }]
  , runRowEncoder = \rs ->
      let !innerCols = runRowEncoder inner (V.map sel rs)
          !childNames = map fieldName (rowEncoderFields inner)
          !named = V.fromList (zip childNames innerCols)
      in  [ColStruct named]
  }

-- | Like 'structE' but the parent rows are @Maybe c@: emits
-- a 'ColStructMaybe' with a top-level validity mask + child
-- columns. Child slots whose parent validity bit is unset are
-- arbitrary on the wire (Arrow spec, Layout.rst, "Struct
-- Layout") so we fill them by substituting the first present
-- row's value; if every row is 'Nothing' the children are
-- empty (the validity mask is all @False@ and consumers won't
-- index into them).
--
-- Pair with 'structDMaybe' on the read side. Together they
-- give @Maybe c@ a clean nested-record encoding without
-- requiring per-encoder children metadata on every primitive.
structEMaybe :: Text -> (r -> Maybe c) -> RowEncoder c -> RowEncoder r
structEMaybe name sel inner = RowEncoder
  { rowEncoderFields = [Field
      { fieldName       = name
      , fieldNullable   = True
      , fieldType       = AStruct
      , fieldChildren   = V.fromList (rowEncoderFields inner)
      , fieldDictionary = Nothing
      , fieldMetadata   = V.empty
      }]
  , runRowEncoder = \rs ->
      let !mvs   = V.map sel rs
          !valid = boolVecToBitVec (V.map isJust mvs)
          !cs    = case V.find isJust mvs of
                     Just (Just present) -> V.map (fromMaybe present) mvs
                     _                   -> V.empty
          !innerCols  = runRowEncoder inner cs
          !childNames = map fieldName (rowEncoderFields inner)
          !named      = V.fromList (zip childNames innerCols)
      in  [ColStructMaybe valid named]
  }

-- ============================================================
-- RowDecoder
-- ============================================================

-- | Row decoder. Looks up named columns in a
-- 'V.Vector ColumnArray' (keyed by the schema's field names) and
-- runs the matching 'Decoder' on each.
--
-- 'RowDecoder' is an 'Applicative': combine several 'columnD'
-- calls with @<$>@ + @<*>@ to build a record.
data RowDecoder r = RowDecoder
  { rowDecoderRequiredColumns :: ![Text]
    -- ^ Names of columns the decoder consults when run.
    -- Order matches first appearance in the applicative chain;
    -- duplicates removed. Useful for column projection: a
    -- caller can ask the source format to only materialise
    -- these columns rather than the whole record batch.
  , runRowDecoder :: !(V.Vector Field -> V.Vector ColumnArray -> Either String (V.Vector r))
  }

instance Functor RowDecoder where
  fmap f (RowDecoder cs0 run) = RowDecoder cs0 $ \fs cs ->
    V.map f <$> run fs cs

instance Applicative RowDecoder where
  pure x = RowDecoder [] $ \_fs cs ->
    -- Length comes from the first column; an empty batch yields
    -- V.empty. If callers need a fixed row count with no
    -- columns they can rely on 'pure' inside an outer
    -- 'liftA2'-chained RowDecoder that has at least one column.
    let !n = if V.null cs then 0 else columnLen (V.head cs)
    in  Right (V.replicate n x)
  RowDecoder cF runF <*> RowDecoder cX runX = RowDecoder
    (mergeRequired cF cX) $ \fs cs -> do
      fvec <- runF fs cs
      xvec <- runX fs cs
      if V.length fvec /= V.length xvec
        then Left $ "Arrow.Record.<*>: column length mismatch ("
                    ++ show (V.length fvec) ++ " vs " ++ show (V.length xvec) ++ ")"
        else Right (V.zipWith ($) fvec xvec)

-- | Order-preserving union of two 'rowDecoderRequiredColumns'
-- lists. Used by the Applicative instance to maintain the
-- "first appearance" order as decoders are combined.
mergeRequired :: [Text] -> [Text] -> [Text]
mergeRequired xs ys = xs ++ filter (`notElem` xs) ys

-- | Decode the named column via the supplied 'Decoder'. Looks
-- the column up by 'Field' name in the schema the caller passes
-- to 'runRowDecoder'; returns 'Left' if the name isn't present.
columnD :: Text -> Decoder a -> RowDecoder a
columnD name d = RowDecoder [name] $ \fs cs -> do
  idx <- case V.findIndex ((== name) . fieldName) fs of
    Just i  -> Right i
    Nothing -> Left $ "Arrow.Record.columnD: no column named " ++ show name
  let !col = V.unsafeIndex cs idx
  case runDecoder d col of
    Right vs -> Right vs
    Left  e  -> Left $ "Arrow.Record.columnD " ++ show name ++ ": " ++ e

-- | Like 'columnD' but supplies a default value if the
-- column is missing from the source schema. Useful for
-- schema-evolution: an older Parquet file dropped a column
-- that the Haskell record still wants; instead of failing,
-- the decoder substitutes the default.
--
-- Decoding errors on a /present/ column still propagate
-- (e.g. wrong type); only "no such column" falls back.
columnDWithDefault :: Text -> a -> Decoder a -> RowDecoder a
columnDWithDefault name def d = RowDecoder [name] $ \fs cs ->
  case V.findIndex ((== name) . fieldName) fs of
    Nothing ->
      -- Missing column: produce default for every row. Length
      -- comes from the first present column; an empty batch
      -- yields V.empty.
      let !n = if V.null cs then 0 else AC.columnLength (V.head cs)
      in  Right (V.replicate n def)
    Just idx ->
      let !col = V.unsafeIndex cs idx
      in case runDecoder d col of
           Right vs -> Right vs
           Left  e  -> Left $ "Arrow.Record.columnDWithDefault "
                                ++ show name ++ ": " ++ e

-- | Strategy for converting a record's selector name to its
-- on-the-wire column name. Mirrors the @renameStyle@ modifier
-- vocabulary in "Wireform.Derive".
data NameStrategy
  = NameAsIs
    -- ^ Use the selector name unchanged.
  | NameSnakeCase
    -- ^ @userId@ → @user_id@. Inserts an underscore before any
    -- uppercase letter that follows a lowercase one and
    -- lower-cases the result.
  | NameCamelCase
    -- ^ @user_id@ → @userId@. Drops underscores and
    -- upper-cases the following character.
  | NameUpperSnakeCase
    -- ^ @userId@ → @USER_ID@. snake-case + upper-case.
  deriving (Show, Eq)

-- | Apply a 'NameStrategy' to a 'Text' selector name.
applyNameStrategy :: NameStrategy -> Text -> Text
applyNameStrategy NameAsIs        = id
applyNameStrategy NameSnakeCase   = T.toLower . toSnake
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
    go _    []           = []
    go prev (c : cs)
      | isUp c
      , isLow prev
        || (isUp prev && case cs of
                           (n:_) -> isLow n
                           []    -> False)
      = '_' : c : go c cs
      | otherwise = c : go c cs
    isUp  c = c >= 'A' && c <= 'Z'
    isLow c = c >= 'a' && c <= 'z'
applyNameStrategy NameCamelCase   = toCamel
  where
    toCamel t =
      let parts = T.splitOn (T.pack "_") t
      in case parts of
           []      -> T.empty
           (p:ps)  -> T.concat (T.toLower p : map cap ps)
    cap t
      | T.null t  = t
      | otherwise = T.cons (toUpper1 (T.head t)) (T.tail t)
    toUpper1 c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c
applyNameStrategy NameUpperSnakeCase =
  T.toUpper . applyNameStrategy NameSnakeCase

-- | Inverse of 'structE': decode a 'ColStruct' column at the
-- given name as a record using the supplied inner 'RowDecoder'.
-- The inner decoder sees the struct's child fields + child
-- columns; the outer decoder threads the nested record into the
-- parent record's applicative chain like any other column.
structD :: Text -> RowDecoder c -> RowDecoder c
structD name inner = RowDecoder [name] $ \fs cs -> do
  idx <- case V.findIndex ((== name) . fieldName) fs of
    Just i  -> Right i
    Nothing -> Left $ "Arrow.Record.structD: no column named " ++ show name
  let !parentField = V.unsafeIndex fs idx
      !col         = V.unsafeIndex cs idx
  case col of
    ColStruct childCols -> do
      let !childFields = fieldChildren parentField
          !childCols'  = V.map snd childCols
      case runRowDecoder inner childFields childCols' of
        Right rs -> Right rs
        Left  e  -> Left $ "Arrow.Record.structD " ++ show name ++ ": " ++ e
    other ->
      Left $ "Arrow.Record.structD " ++ show name
              ++ ": expected ColStruct, got " ++ takeWhile (/= ' ') (show other)

-- | Like 'structD' but the column may be a 'ColStructMaybe' —
-- decodes per-row to @Maybe c@ honouring the parent validity
-- mask. Required-struct columns are accepted too (every row
-- becomes 'Just').
structDMaybe :: Text -> RowDecoder c -> RowDecoder (Maybe c)
structDMaybe name inner = RowDecoder [name] $ \fs cs -> do
  idx <- case V.findIndex ((== name) . fieldName) fs of
    Just i  -> Right i
    Nothing -> Left $ "Arrow.Record.structDMaybe: no column named " ++ show name
  let !parentField = V.unsafeIndex fs idx
      !col         = V.unsafeIndex cs idx
      !childFields = fieldChildren parentField
      mask vs valid =
        let !validBoxed = bitVecToBoolVec valid
        in if V.length vs /= V.length validBoxed
             then Left $ "Arrow.Record.structDMaybe " ++ show name
                   ++ ": child length " ++ show (V.length vs)
                   ++ " /= validity length " ++ show (V.length validBoxed)
             else Right $ V.zipWith
                    (\b v -> if b then Just v else Nothing) validBoxed vs
  case col of
    ColStruct childCols -> do
      let !childCols' = V.map snd childCols
      case runRowDecoder inner childFields childCols' of
        Right rs -> Right (V.map Just rs)
        Left  e  -> Left $ "Arrow.Record.structDMaybe " ++ show name ++ ": " ++ e
    ColStructMaybe valid childCols -> do
      let !childCols' = V.map snd childCols
      case runRowDecoder inner childFields childCols' of
        Right rs -> mask rs valid
        Left  e  -> Left $ "Arrow.Record.structDMaybe " ++ show name ++ ": " ++ e
    other ->
      Left $ "Arrow.Record.structDMaybe " ++ show name
        ++ ": expected ColStruct/ColStructMaybe, got "
        ++ takeWhile (/= ' ') (show other)

-- | Vector length of a 'ColumnArray'. Now delegates to
-- 'Arrow.Column.columnLength' (originally re-implemented here
-- to dodge a non-existent import cycle).
columnLen :: ColumnArray -> Int
columnLen = AC.columnLength

-- ============================================================
-- Table
-- ============================================================

-- | Pairs a 'RowEncoder' with a 'RowDecoder' for one Haskell
-- record type. This is the handle you pass to the top-level
-- encode / decode helpers below.
data Table r = Table
  { tableEncode :: !(RowEncoder r)
  , tableDecode :: !(RowDecoder r)
  }

-- | Smart constructor. Equivalent to @Table enc dec@ but reads
-- better in call-site positions.
table :: RowEncoder r -> RowDecoder r -> Table r
table = Table

-- | Schema implied by the 'RowEncoder'.
tableSchema :: Table r -> Schema
tableSchema t = Schema
  { arrowFields     = V.fromList (rowEncoderFields (tableEncode t))
  , arrowEndianness = Little
  , arrowMetadata   = V.empty
  , arrowFeatures = V.empty
  }

-- | Names of the columns the 'Table''s decoder needs. Equivalent
-- to @'rowDecoderRequiredColumns' . 'tableDecode'@; surfaced
-- here so callers can drive column projection through a
-- 'Table' without unpacking the inner 'RowDecoder'.
tableRequiredColumns :: Table r -> [Text]
tableRequiredColumns = rowDecoderRequiredColumns . tableDecode

-- | Encode a vector of records as an Arrow batch + its schema.
-- The schema comes from 'tableSchema'; the batch is parallel to
-- 'arrowFields' of that schema.
encodeTable :: Table r -> V.Vector r -> (Schema, V.Vector ColumnArray)
encodeTable t rs =
  ( tableSchema t
  , V.fromList (runRowEncoder (tableEncode t) rs)
  )

-- | Decode an Arrow batch into a vector of records. Looks up
-- columns by schema field name; returns 'Left' on missing
-- columns or type mismatches.
decodeTable
  :: Table r
  -> Schema
  -> V.Vector ColumnArray
  -> Either String (V.Vector r)
decodeTable t sch cs =
  runRowDecoder (tableDecode t) (arrowFields sch) cs

-- Map import is kept for future @byIndex@ variants.
_mapShim :: Map.Map Text Int
_mapShim = Map.empty

-- ============================================================
-- Subset / projection
-- ============================================================

-- | Build a 'Table' for a subset of columns by name. The
-- resulting decoder ignores columns not in @keep@; the encoder
-- only emits the kept ones. Useful for callers that have a
-- single 'Table' and want to read or write only a slice
-- without writing a parallel @Table SubsetRecord@.
--
-- Returns 'Nothing' if any name in @keep@ isn't present in the
-- original table.
subsetTable :: [Text] -> Table r -> Maybe (Table r)
subsetTable keep tbl =
  let !srcFields = rowEncoderFields (tableEncode tbl)
      keepIdx :: [Int]
      keepIdx = [ i | nm <- keep
                    , (i, f) <- zip [0..] srcFields
                    , fieldName f == nm ]
  in if length keepIdx /= length keep
       then Nothing
       else Just Table
         { tableEncode = subsetRowEncoder keepIdx (tableEncode tbl)
         , tableDecode = tableDecode tbl  -- decoder uses byName lookup so subset is automatic
         }

subsetRowEncoder :: [Int] -> RowEncoder r -> RowEncoder r
subsetRowEncoder keepIdx (RowEncoder fields0 run0) = RowEncoder
  { rowEncoderFields = [ fields0 !! i | i <- keepIdx ]
  , runRowEncoder    = \rs ->
      let !allCols = run0 rs
      in  [ allCols !! i | i <- keepIdx ]
  }

-- | Project an existing batch by column name, in the order
-- listed. Returns 'Nothing' if any name is missing.
--
-- Together with @'subsetTable'@ this lets callers reuse one
-- 'Table' definition across read paths that materialise
-- different column subsets.
projectTable
  :: [Text]
  -> Schema
  -> V.Vector ColumnArray
  -> Maybe (Schema, V.Vector ColumnArray)
projectTable keep sch cols = do
  let !nameToIdx = Map.fromList
        [ (fieldName f, i)
        | (i, f) <- V.toList (V.indexed (arrowFields sch))
        ]
  idxs <- traverse (`Map.lookup` nameToIdx) keep
  let !newFields = V.fromList
        [ V.unsafeIndex (arrowFields sch) i | i <- idxs ]
      !newCols   = V.fromList
        [ V.unsafeIndex cols i | i <- idxs ]
  pure (sch { arrowFields = newFields }, newCols)
