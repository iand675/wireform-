{-# LANGUAGE OverloadedStrings #-}

{- | Properties of "FlatBuffers.Builder".

Random object trees (tables with scalar, string, vector, struct and
nested-table slots) are built with the original builder kept in
"Test.FlatBuffers.Builder.Reference" and with the current builder in
each field style: opaque writer closures (the 'B.Field'' pattern),
@'B.scalar' n (\\b -> prependX b v)@ (rewritten to data fields by the
builder's rules) and the data fields themselves ('B.scalarU8' ...).
Every build must produce the reference's bytes, and
"FlatBuffers.Reader" must read every value back from the result.
-}
module Test.FlatBuffers.Builder (tests) where

import Control.Monad (foldM, forM_, zipWithM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word8)
import FlatBuffers.Builder qualified as B
import FlatBuffers.Reader qualified as R
import Hedgehog (Gen, Property, PropertyT, annotate, evalEither, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.IO.Unsafe (unsafePerformIO)
import Test.FlatBuffers.Builder.Reference qualified as Ref
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "FlatBuffers.Builder" $ do
    it "matches the reference builder byte for byte in every field style" prop_matchesReference
    it "reads back every value it wrote" prop_readBack
    it "keeps objects aligned and the buffer a multiple of its largest alignment" prop_aligned
    it "survives regrowth from a 16-byte initial buffer" prop_tinyCapacity
    it "deduplicates identical vtables" $ do
      bs <- build Data Nothing [Just (VTab sameShape), Just (VVecTab (replicate 5 sameShape))]
      -- The six equal-shape tables share one vtable; the root has its own.
      countVTables bs `shouldBe` 2
    it "leaves a finished buffer untouched when building continues" $ do
      b <- B.newBuilderWithCapacity 16
      s <- B.writeString b "hello"
      t <- B.writeTable b [Just (B.voff s), Just (B.scalarI64 42)]
      bs <- B.finish b t
      let copy = BS.copy bs
      _ <- B.writeString b "a longer string that forces the buffer to regrow"
      _ <- B.writeTable b [Just (B.scalarI32 7)]
      bs `shouldBe` copy
  where
    sameShape = [Just (VI32 1), Nothing, Just (VI64 2)]


-- ---------------------------------------------------------------------------
-- Object model
-- ---------------------------------------------------------------------------

-- | A table slot's content.
data Val
  = VU8 Word8
  | VI16 Int16
  | VI32 Int32
  | VI64 Int64
  | VStruct Int64 Int64
  -- ^ inline 16-byte struct field
  | VStr Text
  | VTab [Maybe Val]
  | VVecI32 [Int32]
  | VVecI64 [Int64]
  | VVecStr [Text]
  | VVecTab [[Maybe Val]]
  | VVecStruct [(Int64, Int32)]
  -- ^ vector of 16-byte structs: i64, i32, 4 bytes of padding
  deriving stock (Show)


genTable :: Int -> Gen [Maybe Val]
genTable depth = Gen.list (Range.linear 0 9) (Gen.maybe (genVal depth))


genVal :: Int -> Gen Val
genVal depth =
  Gen.frequency $
    [ (3, VU8 <$> Gen.enumBounded)
    , (3, VI16 <$> Gen.enumBounded)
    , (3, VI32 <$> Gen.enumBounded)
    , (3, VI64 <$> Gen.enumBounded)
    , (1, VStruct <$> Gen.enumBounded <*> Gen.enumBounded)
    , (2, VStr <$> genText)
    , (1, VVecI32 <$> Gen.list (Range.linear 0 12) Gen.enumBounded)
    , (1, VVecI64 <$> Gen.list (Range.linear 0 12) Gen.enumBounded)
    , (1, VVecStr <$> Gen.list (Range.linear 0 6) genText)
    , (1, VVecStruct <$> Gen.list (Range.linear 0 6) ((,) <$> Gen.enumBounded <*> Gen.enumBounded))
    ]
      <> [ (2, VTab <$> genTable (depth - 1)) | depth > 0 ]
      <> [ (1, VVecTab <$> Gen.list (Range.linear 0 4) (genTable (depth - 1))) | depth > 0 ]


genText :: Gen Text
genText = Gen.text (Range.linear 0 40) Gen.unicode


-- ---------------------------------------------------------------------------
-- Building through either implementation
-- ---------------------------------------------------------------------------

-- | The builder operations a tree needs, so one walker drives both
-- implementations.
data Ops b f = Ops
  { opNew :: IO b
  , opFinish :: b -> Int -> IO ByteString
  , opTable :: b -> [Maybe f] -> IO Int
  , opString :: b -> Text -> IO Int
  , opOffsets :: b -> [Int] -> IO Int
  , opStructs :: b -> Int -> Int -> [b -> IO ()] -> IO Int
  , opVecI32 :: b -> [Int32] -> IO Int
  , opVecI64 :: b -> [Int64] -> IO Int
  , opVOff :: Int -> f
  , opU8 :: Word8 -> f
  , opI16 :: Int16 -> f
  , opI32 :: Int32 -> f
  , opI64 :: Int64 -> f
  , opStruct :: Int64 -> Int64 -> f
  , opPrependI32 :: b -> Int32 -> IO ()
  , opPrependI64 :: b -> Int64 -> IO ()
  , opPrependZeros :: b -> Int -> IO ()
  }


refOps :: Ops Ref.Builder Ref.Field'
refOps =
  Ops
    { opNew = Ref.newBuilder
    , opFinish = Ref.finish
    , opTable = Ref.writeTable
    , opString = Ref.writeString
    , opOffsets = Ref.writeVectorOfOffsets
    , opStructs = Ref.writeVectorOfStructs
    , opVecI32 = Ref.writeVectorInt32
    , opVecI64 = Ref.writeVectorInt64
    , opVOff = Ref.voff
    , opU8 = \v -> Ref.scalar 1 (\b -> Ref.prependU8 b v)
    , opI16 = \v -> Ref.scalar 2 (\b -> Ref.prependI16 b v)
    , opI32 = \v -> Ref.scalar 4 (\b -> Ref.prependI32 b v)
    , opI64 = \v -> Ref.scalar 8 (\b -> Ref.prependI64 b v)
    , opStruct = \x y -> Ref.struct 16 8 (\b -> Ref.prependI64 b y >> Ref.prependI64 b x)
    , opPrependI32 = Ref.prependI32
    , opPrependI64 = Ref.prependI64
    , opPrependZeros = \b n -> Ref.prependBS b (BS.replicate n 0)
    }


-- | How the current builder describes scalar fields.
data Style
  = -- | the 'B.Field'' pattern: a closure 'B.writeTable' must call
    Opaque
  | -- | @'B.scalar' n (\\b -> prependX b v)@, as "Arrow.FlatBufferIPC"
    -- writes it; the builder's rewrite rules turn it into data
    Scalar
  | -- | 'B.scalarU8' ... 'B.scalarI64'
    Data
  deriving stock (Show, Enum, Bounded)


newOps :: Style -> Maybe Int -> Ops B.Builder B.Field'
newOps style capacity =
  Ops
    { opNew = maybe B.newBuilder B.newBuilderWithCapacity capacity
    , opFinish = B.finish
    , opTable = B.writeTable
    , opString = B.writeString
    , opOffsets = B.writeVectorOfOffsets
    , opStructs = B.writeVectorOfStructs
    , opVecI32 = B.writeVectorInt32
    , opVecI64 = B.writeVectorInt64
    , opVOff = B.voff
    , opU8 = case style of
        Opaque -> \v -> B.Field' 1 (\b _ -> B.prependU8 b v)
        Scalar -> \v -> B.scalar 1 (\b -> B.prependU8 b v)
        Data -> B.scalarU8
    , opI16 = case style of
        Opaque -> \v -> B.Field' 2 (\b _ -> B.prependI16 b v)
        Scalar -> \v -> B.scalar 2 (\b -> B.prependI16 b v)
        Data -> B.scalarI16
    , opI32 = case style of
        Opaque -> \v -> B.Field' 4 (\b _ -> B.prependI32 b v)
        Scalar -> \v -> B.scalar 4 (\b -> B.prependI32 b v)
        Data -> B.scalarI32
    , opI64 = case style of
        Opaque -> \v -> B.Field' 8 (\b _ -> B.prependI64 b v)
        Scalar -> \v -> B.scalar 8 (\b -> B.prependI64 b v)
        Data -> B.scalarI64
    , opStruct = \x y -> B.struct 16 8 (\b -> B.prependI64 b y >> B.prependI64 b x)
    , opPrependI32 = B.prependI32
    , opPrependI64 = B.prependI64
    , opPrependZeros = \b n -> B.prependBS b (BS.replicate n 0)
    }


-- | Lay out a tree: every slot's out-of-line content first, then the table.
buildWith :: Ops b f -> [Maybe Val] -> IO ByteString
buildWith ops root = do
  b <- opNew ops
  t <- table b root
  opFinish ops b t
  where
    table b slots = do
      fields <- mapM (traverse (slot b)) slots
      opTable ops b fields

    slot b = \case
      VU8 v -> pure (opU8 ops v)
      VI16 v -> pure (opI16 ops v)
      VI32 v -> pure (opI32 ops v)
      VI64 v -> pure (opI64 ops v)
      VStruct x y -> pure (opStruct ops x y)
      VStr s -> opVOff ops <$> opString ops b s
      VTab ts -> opVOff ops <$> table b ts
      VVecI32 xs -> opVOff ops <$> opVecI32 ops b xs
      VVecI64 xs -> opVOff ops <$> opVecI64 ops b xs
      VVecStr ss -> do
        us <- mapM (opString ops b) ss
        opVOff ops <$> opOffsets ops b us
      VVecTab tss -> do
        us <- mapM (table b) tss
        opVOff ops <$> opOffsets ops b us
      VVecStruct es ->
        opVOff ops
          <$> opStructs
            ops
            b
            16
            8
            [ \bb -> opPrependZeros ops bb 4 >> opPrependI32 ops bb y >> opPrependI64 ops bb x
            | (x, y) <- es
            ]


build :: Style -> Maybe Int -> [Maybe Val] -> IO ByteString
build style capacity = buildWith (newOps style capacity)


buildPure :: Style -> Maybe Int -> [Maybe Val] -> ByteString
buildPure style capacity root = unsafePerformIO (build style capacity root)
{-# NOINLINE buildPure #-}


buildRef :: [Maybe Val] -> ByteString
buildRef root = unsafePerformIO (buildWith refOps root)
{-# NOINLINE buildRef #-}


-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_matchesReference :: Property
prop_matchesReference = property $ do
  root <- forAll (genTable 3)
  let expected = buildRef root
  forM_ [minBound .. maxBound] $ \style -> do
    annotate (show style)
    buildPure style Nothing root === expected


prop_readBack :: Property
prop_readBack = property $ do
  root <- forAll (genTable 3)
  let bs = buildPure Data Nothing root
  rootPos <- evalEither (R.followUOffset bs 0)
  checkTable bs rootPos root


prop_aligned :: Property
prop_aligned = property $ do
  root <- forAll (genTable 3)
  let bs = buildPure Data Nothing root
  rootPos <- evalEither (R.followUOffset bs 0)
  -- Inline 8-byte scalars and structs are 8-aligned, so a buffer
  -- holding any must be a multiple of 8; it is always a multiple of 4.
  (BS.length bs `mod` (if needs8 root then 8 else 4)) === 0
  checkAlignment bs rootPos root
  where
    needs8 = any (maybe False needs8Val)
    needs8Val = \case
      VI64 _ -> True
      VStruct _ _ -> True
      VVecI64 _ -> True
      VVecStruct _ -> True
      VTab ts -> needs8 ts
      VVecTab tss -> any needs8 tss
      _ -> False


prop_tinyCapacity :: Property
prop_tinyCapacity = property $ do
  root <- forAll (genTable 3)
  forM_ [minBound .. maxBound] $ \style ->
    buildPure style (Just 16) root === buildPure style Nothing root


-- ---------------------------------------------------------------------------
-- Reading back
-- ---------------------------------------------------------------------------

checkTable :: ByteString -> R.Pos -> [Maybe Val] -> PropertyT IO ()
checkTable bs pos slots = do
  slotAt <- evalEither (R.resolveTable bs pos)
  forM_ (zip [0 ..] slots) $ \(i, mv) -> case (mv, slotAt i) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just _) -> annotate ("absent slot " <> show i <> " is present") >> failure
    (Just _, Nothing) -> annotate ("present slot " <> show i <> " is absent") >> failure
    (Just v, Just p) -> checkVal bs p v
  -- Slots past the end of the list read as absent too.
  slotAt (length slots) === Nothing


checkVal :: ByteString -> R.Pos -> Val -> PropertyT IO ()
checkVal bs p = \case
  VU8 v -> evalEither (R.peekU8 bs p) >>= (=== v)
  VI16 v -> evalEither (R.peekI16 bs p) >>= (=== v)
  VI32 v -> evalEither (R.peekI32 bs p) >>= (=== v)
  VI64 v -> evalEither (R.peekI64 bs p) >>= (=== v)
  VStruct x y -> do
    evalEither (R.peekI64 bs p) >>= (=== x)
    evalEither (R.peekI64 bs (p + 8)) >>= (=== y)
  VStr s -> do
    target <- evalEither (R.followUOffset bs p)
    evalEither (R.readString bs target) >>= (=== s)
    -- NUL terminator after the bytes.
    n <- evalEither (R.vectorLength bs target)
    evalEither (R.peekU8 bs (target + 4 + n)) >>= (=== 0)
  VTab ts -> do
    target <- evalEither (R.followUOffset bs p)
    checkTable bs target ts
  VVecI32 xs -> do
    target <- evalEither (R.followUOffset bs p)
    evalEither (R.vectorLength bs target) >>= (=== length xs)
    zipWithM_ (\i x -> evalEither (R.peekI32 bs (R.vectorElementAt target 4 i)) >>= (=== x)) [0 ..] xs
  VVecI64 xs -> do
    target <- evalEither (R.followUOffset bs p)
    evalEither (R.readVectorInt64 bs target) >>= (=== xs)
  VVecStr ss -> do
    target <- evalEither (R.followUOffset bs p)
    ps <- evalEither (R.readVectorOfOffsets bs target)
    ts <- traverse (evalEither . R.readString bs) (V.toList ps)
    ts === ss
  VVecTab tss -> do
    target <- evalEither (R.followUOffset bs p)
    ps <- evalEither (R.readVectorOfOffsets bs target)
    V.length ps === length tss
    zipWithM_ (checkTable bs) (V.toList ps) tss
  VVecStruct es -> do
    target <- evalEither (R.followUOffset bs p)
    (n, ps) <- evalEither (R.readVectorOfStructs bs target 16)
    n === length es
    zipWithM_
      ( \q (x, y) -> do
          evalEither (R.peekI64 bs q) >>= (=== x)
          evalEither (R.peekI32 bs (q + 8)) >>= (=== y)
      )
      (V.toList ps)
      es


-- | Every inline scalar sits at a multiple of its size; strings and
-- offset vectors at a multiple of 4; i64 and struct vectors have
-- 8-aligned elements.
checkAlignment :: ByteString -> R.Pos -> [Maybe Val] -> PropertyT IO ()
checkAlignment bs pos slots = do
  (pos `mod` 4) === 0
  slotAt <- evalEither (R.resolveTable bs pos)
  forM_ (zip [0 ..] slots) $ \(i, mv) -> case (mv, slotAt i) of
    (Just v, Just p) -> do
      (p `mod` inlineSize v) === 0
      case v of
        VStr _ -> out p 4 0
        VTab ts -> do
          target <- evalEither (R.followUOffset bs p)
          checkAlignment bs target ts
        VVecI32 _ -> out p 4 0
        VVecI64 _ -> out p 8 4
        VVecStr _ -> out p 4 0
        VVecTab tss -> do
          target <- evalEither (R.followUOffset bs p)
          (target `mod` 4) === 0
          ps <- evalEither (R.readVectorOfOffsets bs target)
          zipWithM_ (checkAlignment bs) (V.toList ps) tss
        -- Only the length prefix's 4-byte alignment is checked: like
        -- the original builder, 'B.writeVectorOfStructs' aligns the
        -- whole object (prefix first) to the struct alignment.
        VVecStruct _ -> out p 4 0
        _ -> pure ()
    _ -> pure ()
  where
    -- The referenced object starts @rem'@ past a multiple of @align@.
    out p align rem' = do
      target <- evalEither (R.followUOffset bs p)
      (target `mod` align) === rem'
    inlineSize = \case
      VU8 _ -> 1
      VI16 _ -> 2
      VI32 _ -> 4
      VI64 _ -> 8
      VStruct _ _ -> 16
      _ -> 4


-- | Number of distinct vtables used by the root, the table in its slot
-- 0, and the tables in the vectors of its other slots.
countVTables :: ByteString -> Int
countVTables bs = either error length $ do
  rootPos <- R.followUOffset bs 0
  slotAt <- R.resolveTable bs rootPos
  nested <- maybe (Left "slot 0 absent") (R.followUOffset bs) (slotAt 0)
  vec <- maybe (Left "slot 1 absent") (R.followUOffset bs) (slotAt 1)
  elems <- R.readVectorOfOffsets bs vec
  foldM vtableOf [] (rootPos : nested : V.toList elems)
  where
    vtableOf seen pos = do
      soff <- R.peekI32 bs pos
      let vt = pos - fromIntegral soff
      Right (if vt `elem` seen then seen else vt : seen)

