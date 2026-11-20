{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Delta.ScanPredicate (
  predicateColumns,
  Prepared,
  preparePredicate,
  prepareFilePruning,
  countRows,
  selectRows,
  RowSelection (..),
) where

import Arrow.Column qualified as AC
import Arrow.Column.Buffer (Decimal128 (..), unsafeBitAt, unsafeIsValidAt)
import Arrow.Types qualified as AT
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Data.Time (Day, LocalTime, defaultTimeLocale, diffDays, fromGregorian, parseTimeM)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Delta.DeletionVector (descriptorCardinality)
import Delta.Log (AddAction (addDeletionVector), AddStats (..), NullCount (..), effectiveTightBounds)
import Delta.Predicate
import Delta.ScanSchema (ScanSchema, logicalFileStatsForPruning, outputSchema, partitionValueReader)
import GHC.Exts (Int (I#), Int#)
import GHC.Float (float2Double)


data Truth = Match | Reject | Unknown
  deriving stock (Eq)


-- The row-index argument stays unboxed through the compiled predicate's
-- indirect calls, including the count-only loop.
type RowTest = Int# -> Truth


type Prepared = V.Vector AC.ColumnArray -> Either String RowTest


predicateColumns :: Predicate -> [Text]
predicateColumns gen = go gen []
  where
    go (Column name _ _) rest = name : rest
    go (And a b) rest = go a (go b rest)
    go (Or a b) rest = go a (go b rest)
    go (Not a) rest = go a rest
    go (Always _) rest = rest


preparePredicate :: AT.Schema -> Predicate -> Either String Prepared
preparePredicate schema = go
  where
    go (Always b) = Right (const (Right (\_ -> truth b)))
    go (Not input) = do
      prepare <- go input
      pure $ \columns -> do
        test <- prepare columns
        pure (\i -> notTruth (test i))
    go (And a b) = combine andTruth <$> go a <*> go b
    go (Or a b) = combine orTruth <$> go a <*> go b
    go (Column name path test) = do
      (field, indices) <- fieldPath (AT.arrowFields schema) (name : path)
      bind <- prepareTest (AT.fieldType field) test
      pure $ \columns -> do
        let (index, children) = indices
        (leaf, valid) <- columnPath (V.unsafeIndex columns index) children
        check <- bind leaf
        pure $ \i -> case test of
          IsNull -> truth (not (valid i))
          NullSafeEqual Nothing -> truth (not (valid i))
          NullSafeEqual (Just _) -> if valid i then check i else Reject
          _ -> if valid i then check i else Unknown
    combine :: (Truth -> Truth -> Truth) -> Prepared -> Prepared -> Prepared
    combine op left right columns = do
      a <- left columns
      b <- right columns
      pure (\i -> op (a i) (b i))


fieldPath :: V.Vector AT.Field -> [Text] -> Either String (AT.Field, (Int, [Int]))
fieldPath fields (name : rest) = do
  index <- maybe (Left ("Delta.Scan: predicate column " ++ show name ++ " is missing from the snapshot schema")) Right $
    V.findIndex ((== name) . AT.fieldName) fields
  let field = V.unsafeIndex fields index
  case rest of
    [] -> pure (field, (index, []))
    _ -> do
      unless (AT.fieldType field == AT.AStruct) (Left "Delta.Scan: a predicate path may descend only through structs")
      (target, (child, children)) <- fieldPath (AT.fieldChildren field) rest
      pure (target, (index, child : children))
fieldPath _ [] = Left "Delta.Scan: empty predicate column path"


columnPath :: AC.ColumnArray -> [Int] -> Either String (AC.ColumnArray, Int# -> Bool)
columnPath column [] = Right (column, present column)
columnPath parent@(AC.ColStruct _ _ columns) (index : rest) = do
  (leaf, valid) <- columnPath (snd (V.unsafeIndex columns index)) rest
  let parentValid = present parent
  pure (leaf, \i -> parentValid i && valid i)
columnPath _ _ = Left "Delta.Scan: predicate expected a struct column"


present :: AC.ColumnArray -> Int# -> Bool
present (AC.ColNull _) = \_ -> False
present c = let valid = AC.validity c in \i -> unsafeIsValidAt valid (I# i)
{-# INLINE present #-}


type Test = AC.ColumnArray -> Either String RowTest


prepareTest :: AT.ArrowType -> ColumnTest -> Either String Test
prepareTest _ IsNull = Right (const (Right (\_ -> Reject)))
prepareTest _ (NullSafeEqual Nothing) = prepareTest AT.ANull IsNull
prepareTest ty (NullSafeEqual (Just v)) = prepareTest ty (Compare Equal v)
prepareTest AT.ANull _ = Right (const (Right (\_ -> Unknown)))
prepareTest ty (Compare cmp v) = comparison ty cmp v
prepareTest ty (In values) = do
  tests <- traverse (traverse (comparison ty Equal)) values
  pure $ \column -> do
    bound <- traverse (traverse ($ column)) tests
    pure $ \i -> foldr (\test rest -> orTruth (maybe Unknown (\check -> check i) test) rest) Reject bound
prepareTest ty (Size cmp needle) = case ty of
  AT.AList -> sized
  AT.AMap _ -> sized
  _ -> Left "Delta.Scan: size requires an array or map field"
  where
    sized = Right $ \case
      AC.ColList _ offsets _ -> Right (testOffsets offsets)
      AC.ColMap _ offsets _ _ -> Right (testOffsets offsets)
      _ -> wrongColumn
    testOffsets offsets i# =
      let i = I# i#
          size = fromIntegral (VS.unsafeIndex offsets (i + 1) - VS.unsafeIndex offsets i)
      in truth (matches cmp (compare size needle))
prepareTest AT.AUtf8 (StringLength cmp needle) = Right $ \case
  AC.ColUtf8 _ offsets bytes -> Right $ \i# ->
    let i = I# i#
        start = fromIntegral (VS.unsafeIndex offsets i)
        end = fromIntegral (VS.unsafeIndex offsets (i + 1))
        -- The Arrow column has validated UTF-8 offsets and byte sequences.
        count !j !n
          | j == end = n
          | BSU.unsafeIndex bytes j .&. 0xc0 == 0x80 = count (j + 1) n
          | otherwise = count (j + 1) (n + 1)
    in truth (matches cmp (compare (count start 0) needle))
  _ -> wrongColumn
prepareTest _ (StringLength _ _) = Left "Delta.Scan: string length requires a string field"
prepareTest ty (Like patternText) = do
  pattern' <- likeTokens (T.unpack patternText)
  case ty of
    AT.AUtf8 -> Right $ \column -> pure (\i -> maybe Unknown (truth . likeMatches pattern') (AC.anyTextAt column (I# i)))
    AT.AInt _ True -> Right $ \column -> do
      readInt <- intAt column
      pure (\i -> truth (likeMatches pattern' (TL.toStrict (TB.toLazyText (TB.decimal (readInt i))))))
    AT.ABool -> Right $ \case
      AC.ColBool _ values ->
        let yes = truth (likeMatches pattern' "true")
            no = truth (likeMatches pattern' "false")
        in Right (\i -> if unsafeBitAt values (I# i) then yes else no)
      _ -> wrongColumn
    _ -> Left "Delta.Scan: LIKE requires a string, integer or boolean field"


data PruningTest
  = PartitionTest (AddAction -> Bool)
  | FileTest (AddAction -> Maybe AddStats -> Bool)


-- | A partition value and file bounds can each disprove a different arm
-- of an OR. Evaluate them in the same predicate, checking partitions first
-- and decoding statistics at most once per file.
prepareFilePruning
  :: (AddAction -> LocalTime -> Maybe Int)
  -> ScanSchema
  -> Predicate
  -> AddAction
  -> Bool
prepareFilePruning offset schema predicate = case go False predicate of
  Nothing -> const True
  Just (PartitionTest keep) -> keep
  Just (FileTest keep) -> \file -> keep file (either (const Nothing) id (logicalFileStatsForPruning schema file))
  where
    statsFields = AT.arrowFields (outputSchema schema)
    -- SQL's three-valued logic preserves De Morgan's laws. Push negation
    -- to the leaves before asking whether any row could match.
    go negated (Always value)
      | negated == value = Just (PartitionTest (const False))
      | otherwise = Nothing
    go negated (Not p) = go (not negated) p
    go negated (And a b) = branch negated (go negated a) (go negated b)
    go negated (Or a b) = branch (not negated) (go negated a) (go negated b)
    -- A null in the list prevents NOT IN from ever being true.
    -- A positive IN with no non-null choices also cannot match.
    go negated (Column _ _ (In values))
      | if negated then any isNothing values else not (any isJust values) =
          Just (PartitionTest (const False))
    go negated p@(Column name path test) = case partitionValueReader schema name of
      Just (field, readValue) -> do
        prepare <- either (const Nothing) Just $
          preparePredicate (AT.defaultSchema (V.singleton field)) (if negated then Not p else p)
        pure $ PartitionTest $ \file -> case readValue (offset file) file >>= prepare . V.singleton of
          Right check -> check 0# == Match
          Left _ -> True
      Nothing -> do
        keep <- dataTest negated name path test
        pure (FileTest (\file -> maybe True (keep file)))

    branch True left right = combine (||) <$> left <*> right
    branch False Nothing right = right
    branch False left Nothing = left
    branch False (Just left) (Just right) = Just (combine (&&) left right)
    combine op (PartitionTest a) (PartitionTest b) = PartitionTest (\file -> op (a file) (b file))
    combine op (PartitionTest a) (FileTest b) = FileTest (\file stats -> op (a file) (b file stats))
    combine op (FileTest a) (PartitionTest b) = FileTest (\file stats -> op (b file) (a file stats))
    combine op (FileTest a) (FileTest b) = FileTest (\file stats -> op (a file stats) (b file stats))

    dataTest negated name path IsNull = Just (prepareNullTest name path (not negated))
    dataTest negated name path (NullSafeEqual Nothing) = Just (prepareNullTest name path (not negated))
    -- Nulls match this predicate even when both bounds equal the literal.
    dataTest True _ _ (NullSafeEqual (Just _)) = Nothing
    dataTest negated name path test =
      let hasNonNull = prepareNullTest name path False
          lookupValue = lookupBound name path
          checkBounds = do
            (field, _) <- either (const Nothing) Just (fieldPath statsFields (name : path))
            boundTest (AT.fieldType field) negated test
      in Just $ case checkBounds of
        Nothing -> hasNonNull
        Just keeps -> \file stats -> hasNonNull file stats &&
          keeps (lookupValue (asMinValues stats)) (lookupValue (asMaxValues stats))
    opposite Equal = NotEqual
    opposite NotEqual = Equal
    opposite Less = GreaterEqual
    opposite LessEqual = Greater
    opposite Greater = LessEqual
    opposite GreaterEqual = Less
    -- Delta's nested counts describe the leaf over the file's rows,
    -- including nulls inherited from a parent struct.
    prepareNullTest name path wantNull file stats =
      case HM.lookup name (asNullCounts stats) >>= descendCount path of
        Just (NullCount 0) | wantNull -> False
        Just (NullCount count) | not wantNull -> case rowsForCounts file stats of
          Just rows -> count /= rows
          Nothing -> True
        _ -> True
    descendCount [] count = Just count
    descendCount (name : rest) (NestedNullCounts children) = HM.lookup name children >>= descendCount rest
    descendCount _ _ = Nothing
    rowsForCounts file stats = do
      physical <- asNumRecords stats
      if effectiveTightBounds stats
        then case addDeletionVector file of
          Nothing -> Just physical
          Just descriptor -> case descriptorCardinality descriptor of
            Right deleted | deleted <= physical -> Just (physical - deleted)
            _ -> Nothing
        else Just physical
    boundsTest negated literal = \case
      Compare op x -> Just (testLiteral (if negated then opposite op else op) (literal x))
      NullSafeEqual (Just x) -> Just (testLiteral Equal (literal x))
      In values
        | negated ->
            let orders = mapMaybe (>>= literal) values
            in Just $ \lo hi -> case (lo, hi) of
              (Just a, Just b) -> not (any (\order -> order a == EQ && order b == EQ) orders)
              _ -> True
        | otherwise ->
            let tests = map (testLiteral Equal . literal) (mapMaybe id values)
            in Just (\lo hi -> any (\test -> test lo hi) tests)
      _ -> Nothing
    testLiteral op candidate = case candidate of
      -- Absent endpoints act as negative and positive infinity.
      Just order -> \lo hi -> possible op (maybe LT order lo) (maybe GT order hi)
      Nothing -> \_ _ -> True
    possible Equal low high = low /= GT && high /= LT
    possible NotEqual low high = low /= EQ || high /= EQ
    possible Less low _ = low == LT
    possible LessEqual low _ = low /= GT
    possible Greater _ high = high == GT
    possible GreaterEqual _ high = high /= LT
    lookupBound name path bounds = HM.lookup name bounds >>= descend path
    descend [] value = Just value
    descend (name : rest) (Object fields) = KM.lookup (Key.fromText name) fields >>= descend rest
    descend _ _ = Nothing
    boundTest ty negated test = case ty of
      AT.AInt _ True -> numericBounds exactNumber
      AT.ADate AT.DateDay -> numericBounds dateNumber
      AT.ATimestamp AT.Microsecond _ -> numericBounds exactNumber
      AT.ADecimal _ _ -> prepareBounds decimalBound (fmap (flip compare) . exactNumber) negated test
      AT.AUtf8 -> prepareBounds textBound (\case
        PVText text -> let !bytes = TE.encodeUtf8 text in Just (\bound -> compare bound bytes)
        _ -> Nothing) negated test
      AT.ABool -> prepareBounds boolBound (\case
        PVBool value -> Just (\bound -> compare bound value)
        _ -> Nothing) negated test
      _ -> Nothing
      where
        numericBounds literal = prepareBounds intBound (fmap intOrder . literal) negated test
    prepareBounds bound literal negated test = do
      testBounds <- boundsTest negated literal test
      pure $ \lo hi -> case (lo >>= bound, hi >>= bound) of
        (Nothing, Nothing) -> True
        (Just a, Just b) | a > b -> True
        (lower, upper) -> testBounds lower upper
    intOrder = integralOrdering (toInteger (minBound :: Int64)) (toInteger (maxBound :: Int64)) fromInteger compare
    intBound (Number n) = toBoundedInteger @Int64 n
    intBound _ = Nothing
    decimalBound (Number n) = Just (toRational n)
    decimalBound _ = Nothing
    textBound (String text) = Just (TE.encodeUtf8 text)
    textBound _ = Nothing
    boolBound (Bool b) = Just b
    boolBound _ = Nothing


comparison :: AT.ArrowType -> Comparison -> PValue -> Either String Test
comparison ty cmp value = case ty of
  AT.ABool | PVBool needle <- value -> pure $ \case
    AC.ColBool _ values -> Right (\i -> check (compare (unsafeBitAt values (I# i)) needle))
    _ -> wrongColumn
  AT.AUtf8 | PVText needle <- value -> Right (compareBytes (TE.encodeUtf8 needle))
  AT.ABinary | PVBinary needle <- value -> Right (compareBytes needle)
  AT.AInt _ True | Just needle <- exactNumber value -> Right (compareInteger needle)
  AT.ADate AT.DateDay | Just needle <- dateNumber value -> Right (compareInteger needle)
  AT.ATimestamp AT.Microsecond _ | Just needle <- exactNumber value -> Right (compareInteger needle)
  AT.AInterval AT.YearMonth | Just needle <- exactNumber value -> Right (compareInteger needle)
  AT.ADuration AT.Microsecond | Just needle <- exactNumber value -> Right (compareInteger needle)
  AT.ADecimal precision scale | Just needle <- exactNumber value ->
    let raw = needle * fromInteger (10 ^ scale)
        magnitude = 10 ^ precision
        order = integralOrdering (1 - magnitude) (magnitude - 1) AC.decimal128FromInteger compareDecimal raw
    in Right $ \case
      AC.ColDecimal128 _ _ _ values -> Right (\i -> check (order (VS.unsafeIndex values (I# i))))
      _ -> wrongColumn
  AT.AFloatingPoint _ | Just needle <- floatingNumber value -> Right $ \case
    AC.ColFloat _ values -> Right (\i -> check (compareFloat (float2Double (VS.unsafeIndex values (I# i))) needle))
    AC.ColDouble _ values -> Right (\i -> check (compareFloat (VS.unsafeIndex values (I# i)) needle))
    _ -> wrongColumn
  _ -> Left ("Delta.Scan: incompatible predicate literal for " ++ show ty)
  where
    check = truth . matches cmp
    compareBytes needle column = case column of
      AC.ColUtf8 _ offsets bytes -> Right (compareAt offsets bytes)
      AC.ColBinary _ offsets bytes -> Right (compareAt offsets bytes)
      _ -> wrongColumn
      where
        compareAt offsets bytes i# =
          let i = I# i#
          in compareSlice offsets bytes i
        compareSlice offsets bytes i =
          let start = fromIntegral (VS.unsafeIndex offsets i)
              end = fromIntegral (VS.unsafeIndex offsets (i + 1))
          in check (compare (BS.take (end - start) (BS.drop start bytes)) needle)
    compareInteger needle column = do
      valueAt <- intAt column
      let order = integralOrdering (toInteger (minBound :: Int64)) (toInteger (maxBound :: Int64)) fromInteger compare needle
      pure (\i -> check (order (valueAt i)))


integralOrdering :: Integer -> Integer -> (Integer -> a) -> (a -> a -> Ordering) -> Rational -> a -> Ordering
integralOrdering lower upper convert order needle
  | whole < lower = const GT
  | whole > upper = const LT
  | otherwise =
      let !n = convert whole
          !fractional = needle /= fromInteger whole
      in \x -> case order x n of
        EQ | fractional -> LT
        result -> result
  where
    whole = floor needle
{-# INLINE integralOrdering #-}


compareDecimal :: Decimal128 -> Decimal128 -> Ordering
compareDecimal (Decimal128 lowA highA) (Decimal128 lowB highB) =
  case compare (fromIntegral highA :: Int64) (fromIntegral highB :: Int64) of
    EQ -> compare lowA lowB
    result -> result
{-# INLINE compareDecimal #-}


intAt :: AC.ColumnArray -> Either String (Int# -> Int64)
intAt = \case
  AC.ColInt8 _ v -> Right (\i -> fromIntegral (VS.unsafeIndex v (I# i)))
  AC.ColInt16 _ v -> Right (\i -> fromIntegral (VS.unsafeIndex v (I# i)))
  AC.ColInt32 _ v -> Right (\i -> fromIntegral (VS.unsafeIndex v (I# i)))
  AC.ColInt64 _ v -> Right (\i -> VS.unsafeIndex v (I# i))
  AC.ColDate32 _ v -> Right (\i -> fromIntegral (VS.unsafeIndex v (I# i)))
  AC.ColTimestamp _ v -> Right (\i -> VS.unsafeIndex v (I# i))
  AC.ColIntervalYearMonth _ v -> Right (\i -> fromIntegral (VS.unsafeIndex v (I# i)))
  AC.ColDuration _ v -> Right (\i -> VS.unsafeIndex v (I# i))
  _ -> wrongColumn


wrongColumn :: Either String a
wrongColumn = Left "Delta.Scan: predicate column does not match its logical Arrow type"


exactNumber :: PValue -> Maybe Rational
exactNumber (PVInt32 n) = Just (fromIntegral n)
exactNumber (PVInt64 n) = Just (fromIntegral n)
exactNumber (PVUnsigned n) = Just (fromIntegral n)
exactNumber (PVDecimal n) = Just n
exactNumber _ = Nothing


dateNumber :: PValue -> Maybe Rational
dateNumber (PVText date) = do
  day <- parseTimeM True defaultTimeLocale "%F" (T.unpack date) :: Maybe Day
  pure (fromIntegral (diffDays day (fromGregorian 1970 1 1)))
dateNumber value = exactNumber value


floatingNumber :: PValue -> Maybe Double
floatingNumber (PVFloat n) = Just (float2Double n)
floatingNumber (PVDouble n) = Just n
floatingNumber n = fromRational <$> exactNumber n


-- Spark SQL orders NaN above every other floating-point value, and treats
-- two NaNs and the two representations of zero as equal.
compareFloat :: Double -> Double -> Ordering
compareFloat a b
  | isNaN a = if isNaN b then EQ else GT
  | isNaN b = LT
  | otherwise = compare a b
{-# INLINE compareFloat #-}


matches :: Comparison -> Ordering -> Bool
matches Equal = (== EQ)
matches NotEqual = (/= EQ)
matches Less = (== LT)
matches LessEqual = (/= GT)
matches Greater = (== GT)
matches GreaterEqual = (/= LT)
{-# INLINE matches #-}


truth :: Bool -> Truth
truth True = Match
truth False = Reject
{-# INLINE truth #-}


notTruth :: Truth -> Truth
notTruth Match = Reject
notTruth Reject = Match
notTruth Unknown = Unknown
{-# INLINE notTruth #-}


andTruth :: Truth -> Truth -> Truth
andTruth Reject _ = Reject
andTruth Match b = b
andTruth Unknown Reject = Reject
andTruth Unknown _ = Unknown
{-# INLINE andTruth #-}


orTruth :: Truth -> Truth -> Truth
orTruth Match _ = Match
orTruth Reject b = b
orTruth Unknown Match = Match
orTruth Unknown _ = Unknown
{-# INLINE orTruth #-}


data LikeToken = Character !Char | One | Many


likeTokens :: [Char] -> Either String [LikeToken]
likeTokens [] = Right []
likeTokens ('%' : '%' : rest) = likeTokens ('%' : rest)
likeTokens ('%' : rest) = (Many :) <$> likeTokens rest
likeTokens ('_' : rest) = (One :) <$> likeTokens rest
likeTokens ('\\' : c : rest) | c == '\\' || c == '%' || c == '_' = (Character c :) <$> likeTokens rest
likeTokens ('\\' : _) = Left "Delta.Scan: malformed LIKE escape"
likeTokens (c : rest) = (Character c :) <$> likeTokens rest


likeMatches :: [LikeToken] -> Text -> Bool
likeMatches pattern' text = go pattern' text Nothing
  where
    go [Many] _ _ = True
    go (Many : rest) input _ = go rest input (Just (rest, input))
    go tokens input retry = case (tokens, T.uncons input) of
      ([], Nothing) -> True
      (One : rest, Just (_, chars)) -> go rest chars retry
      (Character expected : rest, Just (actual, chars)) | expected == actual -> go rest chars retry
      _ -> case retry of
        Just (rest, previous) | Just (_, chars) <- T.uncons previous -> go rest chars (Just (rest, chars))
        _ -> False


data RowSelection = All | None | Contiguous !Int !Int | Selected !(VS.Vector Int)


countRows :: RowTest -> Int -> Int
countRows test rows = go 0 0
  where
    go i@(I# i#) !count
      | i == rows = count
      | test i# == Match = go (i + 1) (count + 1)
      | otherwise = go (i + 1) count


-- Whole and contiguous matches can keep their Arrow buffers. For a
-- fragmented result, write positions directly into a storable vector.
selectRows :: RowTest -> Int -> RowSelection
selectRows test rows = findStart 0
  where
    matchesAt (I# i) = test i == Match
    findStart !i
      | i == rows = None
      | matchesAt i = findEnd i (i + 1)
      | otherwise = findStart (i + 1)
    findEnd !start !i
      | i == rows && start == 0 = All
      | i == rows = Contiguous start (i - start)
      | not (matchesAt i) = findNext start i (i + 1)
      | otherwise = findEnd start (i + 1)
    findNext !start !end !i
      | i == rows = Contiguous start (end - start)
      | matchesAt i = make start end i
      | otherwise = findNext start end (i + 1)
    make start end next = Selected $ VS.create $ do
      let fill vs !readAt !writeAt
            | readAt >= rows = pure (VSM.unsafeSlice 0 writeAt vs)
            | matchesAt readAt = VSM.unsafeWrite vs writeAt readAt >> fill vs (readAt + 1) (writeAt + 1)
            | otherwise = fill vs (readAt + 1) writeAt
      vs <- VSM.unsafeNew (rows - start)
      let prefix !i
            | i == end = do
                VSM.unsafeWrite vs (end - start) next
                fill vs (next + 1) (end - start + 1)
            | otherwise = VSM.unsafeWrite vs (i - start) i >> prefix (i + 1)
      prefix start
