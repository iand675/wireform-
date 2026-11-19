{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_, unless, when)
import Data.Hashable (Hashable (..))
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import GHC.IO (IO (..))
import Hashtable (Boxed, Storage, Strict, Unboxed)
import qualified Hashtable as H
import Hashtable.Internal
import Hashtable.Storage (Storage (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

------------------------------------------------------------------------
-- Adversarial keys

-- | Every key hashes to 0: one probe path for the whole table. This is the
-- shape of vector-hashtables issue #43.
newtype Collide = Collide Int deriving stock (Eq, Ord, Show)

instance Hashable Collide where
  hashWithSalt _ _ = 0
  hash _ = 0

-- | A boxed key with an ordinary hash.
newtype Weak' = Weak' Int deriving stock (Eq, Ord, Show)

instance Hashable Weak' where
  hashWithSalt s (Weak' k) = hashWithSalt s k

shouldReturnM :: (Show a, Eq a) => IO a -> IO a -> IO ()
shouldReturnM act want = do
  x <- act
  y <- want
  x `shouldBe` y

-- | Only five distinct hashes.
newtype Weak = Weak Int deriving stock (Eq, Ord, Show)

instance Hashable Weak where
  hashWithSalt s (Weak k) = hashWithSalt s (k `rem` 5)
  hash (Weak k) = k `rem` 5

------------------------------------------------------------------------
-- Structural invariants

{- | Everything the core promises about a table's bytes:

  * every full slot's tag is the tag of its key's hash;
  * every group strictly before a key's group on the key's probe path has
    the key's overflow bit set (the Bloom invariant lookups rely on);
  * no key is stored twice; size = number of full slots;
    size <= maxLoad <= 7/8 of capacity.
-}
invariants :: forall ks k vs v. (Hashable k, Ord k, Show k, Storage ks k) => Dictionary RealWorld ks k vs v -> IO [String]
invariants d = do
  Snapshot t <- readSnapshot d
  case t of
    Table mask ctrl keys _ -> do
      let groups = I# mask + 1
          byte :: Int -> IO Word
          byte off = IO $ \s -> case readWord8Array# ctrl (case off of I# o -> o) s of
            (# s1, b #) -> (# s1, W# (word8ToWord# b) #)
          readKey :: Int -> IO k
          readKey (I# i) = IO (readM keys i)
          meta (I# i) = IO $ \s -> case readIntArray# ctrl i s of (# s1, n #) -> (# s1, I# n #)
      gl <- meta 0
      ml <- meta 1
      let n = ml - gl
      errs <- newIORef []
      seen <- newIORef Map.empty
      let bad msg = modifyIORef' errs (msg :)
      forM_ [0 .. groups - 1] $ \g -> forM_ [0 .. 6] $ \j -> do
        tag <- byte (16 + 8 * g + j)
        when (tag /= 0) $ do
          k <- readKey (7 * g + j)
          modifyIORef' seen (Map.insertWith (+) k (1 :: Int))
          let w = W# (hashKey# mask k)
              want = case w of W# w# -> W# (tagOf# w#)
              step = fromIntegral (2 * tag + 1) :: Int
          when (want /= tag) $ bad ("tag mismatch for " <> show k)
          let walk h stride steps
                | h == g = pure ()
                | steps > groups = bad ("key " <> show k <> " not on its probe path")
                | otherwise = do
                    ofl <- byte (16 + 8 * h + 7)
                    let bit = 1 `unsafeShiftLW` fromIntegral (tag `rem` 8)
                    when (ofl `andW` bit == 0) $
                      bad ("missing overflow bit for " <> show k <> " in group " <> show h)
                    walk ((h + step) `rem` groups) stride (steps + 1 :: Int)
          walk (fromIntegral (w `andW` fromIntegral (groups - 1))) 0 0
      counts <- readIORef seen
      let full = sum (Map.elems counts)
      forM_ (Map.toList counts) $ \(k, c) -> when (c > 1) $ bad ("duplicate key " <> show k)
      when (full /= n) $ bad ("size " <> show n <> " /= full slots " <> show full)
      when (n > ml) $ bad "size exceeds maxLoad"
      when (ml > maxLoadFor groups) $ bad "maxLoad exceeds 7/8 of capacity"
      readIORef errs
 where
  unsafeShiftLW :: Word -> Int -> Word
  unsafeShiftLW (W# a) (I# b) = W# (uncheckedShiftL# a b)
  andW :: Word -> Word -> Word
  andW (W# a) (W# b) = W# (and# a b)


-- | Groups a lookup for an absent key visits before giving up.
missProbeGroups :: forall ks k vs v. Hashable k => Dictionary RealWorld ks k vs v -> k -> IO Int
missProbeGroups d k = do
  Snapshot t <- readSnapshot d
  case t of
    Table mask ctrl _ _ -> do
      let groups = I# mask + 1
          w = W# (hashKey# mask k)
          tag = case w of W# w# -> W# (tagOf# w#)
          bit = case tag of W# t# -> W# (uncheckedShiftL# 1## (word2Int# (and# t# 7##)))
          step = fromIntegral (2 * tag + 1) :: Int
          ofl (I# g) = IO $ \s -> case readWord8Array# ctrl (16# +# uncheckedIShiftL# g 3# +# 7#) s of
            (# s1, b #) -> (# s1, W# (word8ToWord# b) #)
          go h stride n
            | n >= groups = pure n
            | otherwise = do
                o <- ofl h
                if o `andW'` bit == 0
                  then pure n
                  else go ((h + step) `rem` groups) stride (n + 1)
      go (fromIntegral (w `andW'` fromIntegral (groups - 1))) 0 1
 where
  andW' :: Word -> Word -> Word
  andW' (W# a) (W# b) = W# (and# a b)

-- | Groups whose overflow byte is nonzero.
overflowedGroups :: Dictionary RealWorld ks k vs v -> IO Int
overflowedGroups d = do
  Snapshot t <- readSnapshot d
  case t of
    Table mask ctrl _ _ -> do
      let one (I# g) = IO $ \s -> case readWord8Array# ctrl (16# +# uncheckedIShiftL# g 3# +# 7#) s of
            (# s1, b #) -> (# s1, if isTrue# (eqWord# (word8ToWord# b) 0##) then 0 else 1 #)
      sum <$> mapM one [0 .. I# mask]

assertInvariants :: (Hashable k, Ord k, Show k, Storage ks k) => Dictionary RealWorld ks k vs v -> PropertyT IO ()
assertInvariants d = do
  errs <- evalIO (invariants d)
  unless (null errs) $ do
    annotateShow errs
    failure

groupCount :: Dictionary RealWorld ks k vs v -> IO Int
groupCount d = do
  Snapshot t <- readSnapshot d
  case t of Table mask _ _ _ -> pure (I# mask + 1)

------------------------------------------------------------------------
-- Model-based operation sequences

data Op k
  = Insert k Int
  | Delete k
  | Upsert k
  | AlterIns k Int
  | AlterDel k
  | AlterMIns k Int
  | AlterMDel k
  | InsertWith k Int
  | Lookup k
  | Reserve Int
  | CloneCheck k Int
  | Adjust k
  | UpdateOp k
  | Mutate k Int
  | InsertIfAbsent k Int
  deriving stock (Show)

genOps :: Gen k -> Gen [Op k]
genOps key =
  Gen.list (Range.linear 0 600) $
    Gen.frequency
      [ (10, Insert <$> key <*> val)
      , (8, Delete <$> key)
      , (4, Upsert <$> key)
      , (3, AlterIns <$> key <*> val)
      , (4, AlterDel <$> key)
      , (2, AlterMIns <$> key <*> val)
      , (2, AlterMDel <$> key)
      , (3, InsertWith <$> key <*> val)
      , (6, Lookup <$> key)
      , (1, Reserve <$> Gen.int (Range.linear 0 300))
      , (1, CloneCheck <$> key <*> val)
      , (3, Adjust <$> key)
      , (3, UpdateOp <$> key)
      , (3, Mutate <$> key <*> val)
      , (3, InsertIfAbsent <$> key <*> val)
      ]
 where
  val = Gen.int (Range.linear (-1000) 1000)

runModel ::
  forall ks k vs.
  (Hashable k, Ord k, Show k, Storage ks k, Storage vs Int) =>
  Dictionary RealWorld ks k vs Int -> [Op k] -> PropertyT IO ()
runModel t = go Map.empty
 where
  go model [] = do
    assertInvariants t
    xs <- evalIO (H.toList t)
    sort xs === Map.toList model
    n <- evalIO (H.size t)
    n === Map.size model
  go model (op : ops) = do
    model' <- step model op
    n <- evalIO (H.size t)
    n === Map.size model'
    go model' ops
  step model = \case
    Insert k v -> evalIO (H.insert t k v) >> pure (Map.insert k v model)
    Delete k -> evalIO (H.delete t k) >> pure (Map.delete k model)
    Upsert k -> evalIO (H.upsert t (maybe 1 (+ 1)) k) >> pure (Map.alter (Just . maybe 1 (+ 1)) k model)
    AlterIns k v -> evalIO (H.alter t (Just . maybe v (+ v)) k) >> pure (Map.alter (Just . maybe v (+ v)) k model)
    AlterDel k -> evalIO (H.alter t (const Nothing) k) >> pure (Map.delete k model)
    AlterMIns k v -> evalIO (H.alterM t (pure . Just . maybe v (subtract v)) k) >> pure (Map.alter (Just . maybe v (subtract v)) k model)
    AlterMDel k -> evalIO (H.alterM t (const (pure Nothing)) k) >> pure (Map.delete k model)
    InsertWith k v -> evalIO (H.insertWith (+) t k v) >> pure (Map.insertWith (+) k v model)
    Lookup k -> do
      r <- evalIO (H.lookup t k)
      r === Map.lookup k model
      m <- evalIO (H.member t k)
      m === Map.member k model
      dflt <- evalIO (H.findWithDefault t minBound k)
      dflt === Map.findWithDefault minBound k model
      i <- evalIO (H.findEntry t k)
      (i >= 0) === Map.member k model
      pure model
    Reserve n -> do
      evalIO (H.reserve t n)
      assertInvariants t
      pure model
    Adjust k -> evalIO (H.adjust t (* 3) k) >> pure (Map.adjust (* 3) k model)
    UpdateOp k -> do
      let f v = if even v then Nothing else Just (v + 1)
      evalIO (H.update t f k)
      pure (Map.update f k model)
    Mutate k v -> do
      -- delete if present and odd, bump if even, insert v if absent; report presence
      let f = \case
            Nothing -> (Just v, False)
            Just old -> (if odd old then Nothing else Just (old + v), True)
      present <- evalIO (H.mutate t f k)
      present === Map.member k model
      pure (Map.alter (fst . f) k model)
    InsertIfAbsent k v -> do
      inserted <- evalIO (H.insertIfAbsent t k v)
      inserted === not (Map.member k model)
      pure (Map.insertWith (\_ old -> old) k v model)
    CloneCheck k v -> do
      c <- evalIO (H.clone t)
      evalIO (H.insert t k v)
      cs <- evalIO (H.toList c)
      sort cs === Map.toList model
      assertInvariants c
      pure (Map.insert k v model)

modelProp ::
  forall ks k vs.
  (Hashable k, Ord k, Show k, Storage ks k, Storage vs Int) =>
  IO (Dictionary RealWorld ks k vs Int) -> Gen k -> Property
modelProp mk key = property $ do
  ops <- forAll (genOps key)
  t <- evalIO mk
  runModel t ops

------------------------------------------------------------------------
-- Tables under test

type IntTable = Dictionary RealWorld Unboxed Int Unboxed Int

main :: IO ()
main = sydTest $ do
  describe "model: random operation sequences agree with Data.Map" $ do
    it "Unboxed Int keys, small key space (heavy churn)" $
      modelProp @Unboxed @Int @Unboxed (H.new) (Gen.int (Range.linear 0 64))
    it "Unboxed Int keys, wide key space (growth)" $
      modelProp @Unboxed @Int @Unboxed (H.new) (Gen.int (Range.linear 0 100000))
    it "Strict keys with constant hash (vector-hashtables #43 shape)" $
      modelProp @Strict @Collide @Strict (H.new) (Collide <$> Gen.int (Range.linear 0 40))
    it "Boxed keys with five distinct hashes" $
      modelProp @Boxed @Weak @Boxed (H.new) (Weak <$> Gen.int (Range.linear 0 120))
    it "presized table" $
      modelProp @Unboxed @Int @Strict (H.initialize 500) (Gen.int (Range.linear 0 1000))
    it "large table (table-sized hash blocks), dense and sparse keys" $
      modelProp @Unboxed @Int @Unboxed (H.initialize 110000) (Gen.choice [Gen.int (Range.linear 0 300), Gen.int (Range.linear 0 maxBound)])

  describe "issue #43: deleting through alter keeps colliding keys" $ do
    it "alter (const Nothing) on the first of three colliding keys" $ do
      t <- H.new :: IO (Dictionary RealWorld Strict Collide Strict Int)
      forM_ [1, 2, 3] $ \i -> H.insert t (Collide i) i
      H.alter t (const Nothing) (Collide 1)
      (,) <$> H.lookup t (Collide 2) <*> H.lookup t (Collide 3) `shouldReturn` (Just 2, Just 3)
      H.insert t (Collide 3) 30
      H.size t `shouldReturn` 2
    it "every other key deleted through alter and alterM" $ do
      t <- H.new :: IO IntTable
      forM_ [0 .. 99999] $ \i -> H.insert t i i
      forM_ [0, 2 .. 99999] $ \i -> if i `rem` 4 == 0 then H.alter t (const Nothing) i else H.alterM t (const (pure Nothing)) i
      H.size t `shouldReturn` 50000
      forM_ [0 .. 99999] $ \i -> H.lookup t i >>= (`shouldBe` (if odd i then Just i else Nothing))
      invariants t `shouldReturn` []

  describe "churn" $ do
    it "insert/delete cycles at constant size do not grow the table without bound" $ do
      t <- H.new :: IO IntTable
      forM_ [0 .. 999] $ \i -> H.insert t i i
      g0 <- groupCount t
      forM_ [1000 .. 300999] $ \i -> do
        H.delete t (i - 1000)
        H.insert t i i
      H.size t `shouldReturn` 1000
      g1 <- groupCount t
      when (g1 > 4 * g0) $ expectationFailure ("table grew from " <> show g0 <> " to " <> show g1 <> " groups")
      forM_ [300000 .. 300999] $ \i -> H.lookup t i `shouldReturn` Just i
      H.lookup t 5 `shouldReturn` Nothing
      invariants t `shouldReturn` []
    it "high-load churn keeps miss probes short (overflow-drift accounting)" $ do
      t <- H.new :: IO IntTable
      -- 7/8 of 1024 groups' 7168 slots: the table sits at its load limit
      let live = maxLoadFor 1024 - 1
      forM_ [0 .. live - 1] $ \i -> H.insert t (i * 2) i
      forM_ [0 .. 400000 - 1] $ \i -> do
        H.delete t (i * 2)
        H.insert t ((i + live) * 2) i
      lens <- mapM (\i -> missProbeGroups t (i * 2 + 1)) [0 .. 9999]
      let mean = fromIntegral (sum lens) / 10000 :: Double
      putStrLn ("    mean miss probe groups after churn: " <> show mean)
      when (mean > 2.0) $ expectationFailure ("mean miss probe length drifted to " <> show mean)
      invariants t `shouldReturn` []
    it "large tables spread a dense key range with no overflow at all" $ do
      t <- H.initialize 200000 :: IO IntTable
      g <- groupCount t
      when (g < bigGroups) $ expectationFailure "table is below the large-table threshold"
      forM_ [0 .. 199999] $ \i -> H.insert t (i + 5000000) i
      ovf <- overflowedGroups t
      ovf `shouldBe` 0
      invariants t `shouldReturn` []
    it "constant-hash churn" $ do
      t <- H.new :: IO (Dictionary RealWorld Strict Collide Unboxed Int)
      forM_ [0 .. 49] $ \i -> H.insert t (Collide i) i
      forM_ [50 .. 5049] $ \i -> H.delete t (Collide (i - 50)) >> H.insert t (Collide i) i
      sort <$> H.toList t `shouldReturn` map (\i -> (Collide i, i)) [5000 .. 5049]
      invariants t `shouldReturn` []

  describe "strictness" $ do
    it "Strict: a throwing insert leaves the table untouched" $ do
      t <- H.new :: IO (Dictionary RealWorld Unboxed Int Strict Int)
      forM_ [1 .. 6] $ \i -> H.insert t i i -- table is now exactly at its load limit
      r1 <- try @ErrorCall (H.insert t 7 (error "boom"))
      either (const (pure ())) (const (expectationFailure "insert did not throw")) r1
      r2 <- try @ErrorCall (H.insert t 1 (error "boom"))
      either (const (pure ())) (const (expectationFailure "overwrite did not throw")) r2
      r3 <- try @ErrorCall (H.upsert t (const (error "boom")) 2)
      either (const (pure ())) (const (expectationFailure "upsert did not throw")) r3
      H.size t `shouldReturn` 6
      H.lookup t 7 `shouldReturn` Nothing
      H.lookup t 1 `shouldReturn` Just 1
      H.lookup t 2 `shouldReturn` Just 2
      invariants t `shouldReturn` []
    it "Boxed: values are not forced" $ do
      t <- H.new :: IO (Dictionary RealWorld Unboxed Int Boxed Int)
      H.insert t 1 (error "lazy")
      H.upsert t (maybe (error "lazier") id) 2
      H.size t `shouldReturn` 2
      H.member t 1 `shouldReturn` True

  describe "re-entrancy" $ do
    it "alterM whose callback grows the table" $ do
      t <- H.new :: IO IntTable
      H.insert t 0 0
      H.alterM t (\old -> do forM_ [1 .. 5000] (\i -> H.insert t i i); pure (fmap (+ 100) old)) 0
      H.lookup t 0 `shouldReturn` Just 100
      H.size t `shouldReturn` 5001
      invariants t `shouldReturn` []
    it "alterM whose callback deletes the key it is altering" $ do
      t <- H.new :: IO IntTable
      forM_ [0 .. 10] $ \i -> H.insert t i i
      H.alterM t (\old -> H.delete t 3 >> pure (fmap negate old)) 3
      H.lookup t 3 `shouldReturn` Just (-3)
      H.size t `shouldReturn` 11
    it "foldM whose callback deletes entries never sees a dead one" $ do
      t <- H.new :: IO (Dictionary RealWorld Strict Int Strict Int)
      forM_ [0 .. 2999] $ \i -> H.insert t i i
      deleted <- newIORef Set.empty
      _ <- H.foldM (\() k v -> do
                      _ <- evaluate k
                      _ <- evaluate v
                      dead <- Set.member k <$> readIORef deleted
                      when dead $ expectationFailure ("visited deleted key " <> show k)
                      let victim = (k * 7919) `rem` 3000
                      H.delete t victim
                      modifyIORef' deleted (Set.insert victim)) () t
      invariants t `shouldReturn` []

  describe "bulk API" $ do
    it "keys / values / toList / fromList agree" $ property $ do
      kvs <- forAll (Gen.list (Range.linear 0 2000) ((,) <$> Gen.int (Range.linear 0 5000) <*> Gen.int (Range.linear 0 9)))
      t <- evalIO (H.fromList kvs :: IO IntTable)
      let model = Map.fromList kvs
      ks <- evalIO (H.keys t :: IO (U.Vector Int))
      vs <- evalIO (H.values t :: IO (V.Vector Int))
      sort (zip (U.toList ks) (V.toList vs)) === Map.toList model
      xs <- evalIO (H.toList t)
      sort xs === Map.toList model
      s <- evalIO (H.foldM (\a _ v -> pure (a + v)) 0 t)
      s === sum (Map.elems model)
    it "set operations agree with Data.Map" $ property $ do
      let genMap = Map.fromList <$> Gen.list (Range.linear 0 400) ((,) <$> Gen.int (Range.linear 0 600) <*> Gen.int (Range.linear 0 99))
      ma <- forAll genMap
      mb <- forAll genMap
      a <- evalIO (H.fromList (Map.toList ma) :: IO IntTable)
      b <- evalIO (H.fromList (Map.toList mb) :: IO IntTable)
      let same name act want = do
            annotate name
            r <- evalIO act
            xs <- evalIO (H.toList r)
            sort xs === Map.toList want
            assertInvariants r
      same "union" (H.union a b) (Map.union ma mb)
      same "unionWith" (H.unionWith (-) a b) (Map.unionWith (-) ma mb)
      same "unionWith (swapped sizes)" (H.unionWith (-) b a) (Map.unionWith (-) mb ma)
      same "difference" (H.difference a b) (Map.difference ma mb)
      same "differenceWith" (H.differenceWith (\x y -> if even (x + y) then Just (x * y) else Nothing) a b)
        (Map.differenceWith (\x y -> if even (x + y) then Just (x * y) else Nothing) ma mb)
      same "intersection" (H.intersection a b) (Map.intersection ma mb)
      same "intersectionWith" (H.intersectionWith (-) a b) (Map.intersectionWith (-) ma mb)
      same "intersectionWith (swapped)" (H.intersectionWith (-) b a) (Map.intersectionWith (-) mb ma)
      -- inputs untouched
      xa <- evalIO (H.toList a)
      sort xa === Map.toList ma

  describe "bulk in-place updates" $ do
    it "mapInPlace / filterInPlace / clear / capacity / fromListWith agree with Data.Map" $ property $ do
      kvs <- forAll (Gen.list (Range.linear 0 3000) ((,) <$> Gen.int (Range.linear 0 4000) <*> Gen.int (Range.linear 0 99)))
      t <- evalIO (H.fromListWith (+) kvs :: IO (Dictionary RealWorld Unboxed Int Strict Int))
      let model = Map.fromListWith (+) kvs
      xs <- evalIO (H.toList t)
      sort xs === Map.toList model
      evalIO (H.mapInPlace t (\k v -> k - v))
      let model' = Map.mapWithKey (\k v -> k - v) model
      evalIO (H.filterInPlace t (\k v -> even (k + v)))
      let model'' = Map.filterWithKey (\k v -> even (k + v)) model'
      ys <- evalIO (H.toList t)
      sort ys === Map.toList model''
      assertInvariants t
      cap <- evalIO (H.capacity t)
      n <- evalIO (H.size t)
      diff n (<=) cap
      g <- evalIO (groupCount t)
      evalIO (H.clear t)
      evalIO (H.size t) >>= (=== 0)
      evalIO (groupCount t) >>= (=== g)
      evalIO (H.toList t) >>= (=== [])
      evalIO (H.insert t 1 1)
      evalIO (H.lookup t 1) >>= (=== Just 1)
      assertInvariants t
    it "filterInPlace with a constant-hash key never rehashes and keeps probe paths intact" $ do
      t <- H.new :: IO (Dictionary RealWorld Strict Collide Unboxed Int)
      forM_ [0 .. 59] $ \i -> H.insert t (Collide i) i
      g <- groupCount t
      H.filterInPlace t (\(Collide k) _ -> k `rem` 3 == 0)
      groupCount t `shouldReturn` g
      forM_ [0 .. 59] $ \i -> H.lookup t (Collide i) `shouldReturn` (if i `rem` 3 == 0 then Just i else Nothing)
      invariants t `shouldReturn` []

  describe "batched operations" $ do
    it "small tables (plain path) agree with Data.Map" $ property $ do
      ks <- forAll (Gen.list (Range.linear 0 2000) (Gen.int (Range.linear 0 3000)))
      let kv = U.fromList ks
      t <- evalIO (H.new :: IO IntTable)
      evalIO (H.insertBatch t kv (U.map (* 2) kv))
      evalIO (H.upsertBatch t (maybe 1 (+ 1)) kv)
      let model = Map.fromListWith (+) (map (\k -> (k, 1)) ks) `Map.union` Map.empty
          want = Map.mapWithKey (\k c -> 2 * k + c) model
      xs <- evalIO (H.toList t)
      sort xs === Map.toList want
      probes <- forAll (Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 4000)))
      r <- evalIO (H.lookupBatch t (-1) (U.fromList probes) :: IO (U.Vector Int))
      U.toList r === map (\k -> Map.findWithDefault (-1) k want) probes
    it "large tables (prefetch pipeline, growing mid-batch) agree with sequential ops" $ do
      let n = 200000
          ks = U.generate n (\i -> (i * 7919 + 13) `rem` 150000) -- duplicates on purpose
      t <- H.new :: IO IntTable
      H.insertBatch t ks (U.generate n id)
      ref <- H.new :: IO IntTable
      U.imapM_ (\i k -> H.insert ref k i) ks
      (sort <$> H.toList t) `shouldReturnM` (sort <$> H.toList ref)
      H.upsertBatch t (maybe 0 negate) ks
      U.mapM_ (H.upsert ref (maybe 0 negate)) ks
      (sort <$> H.toList t) `shouldReturnM` (sort <$> H.toList ref)
      probes <- pure (U.generate 50000 (\i -> i * 5))
      got <- H.lookupBatch t minBound probes :: IO (U.Vector Int)
      want <- U.mapM (H.findWithDefault ref minBound) probes
      got `shouldBe` want
      invariants t `shouldReturn` []
    it "upsertBatch that grows the table mid-pipeline re-finishes in-flight hashes" $ do
      -- start past the pipeline threshold, one insert short of the load limit
      let start = maxLoadFor 32768 - 1
      t <- H.initialize start :: IO IntTable
      ref <- H.initialize start :: IO IntTable
      g0 <- groupCount t
      when (g0 /= 32768) $ expectationFailure ("unexpected start size " <> show g0)
      forM_ [0 .. start - 1] $ \i -> H.insert t (i * 3) i >> H.insert ref (i * 3) i
      -- wide keys: the in-flight window must straddle hash-block boundaries, or
      -- a stale (old-mask) hash happens to equal the fresh one
      let ks = U.generate 150000 (\i -> ((i `rem` 120000) * 2654435761) `rem` 1099511627776)
      H.upsertBatch t (maybe 1 (+ 1)) ks
      U.mapM_ (H.upsert ref (maybe 1 (+ 1))) ks
      g1 <- groupCount t
      when (g1 <= g0) $ expectationFailure "table did not grow during the batch"
      (sort <$> H.toList t) `shouldReturnM` (sort <$> H.toList ref)
      invariants t `shouldReturn` []
    it "boxed columns through the prefetch pipeline" $ do
      let n = 100000
          ks = V.generate n (\i -> Weak' (i * 31))
      t <- H.new :: IO (Dictionary RealWorld Strict Weak' Strict Int)
      H.insertBatch t ks (V.generate n id)
      r <- H.lookupBatch t (-1) ks :: IO (V.Vector Int)
      r `shouldBe` V.generate n id
      invariants t `shouldReturn` []

  describe "freezing" $ do
    it "frozen lookups agree with the mutable table" $ property $ do
      kvs <- forAll (Gen.list (Range.linear 0 1500) ((,) <$> Gen.int (Range.linear 0 3000) <*> Gen.int (Range.linear 0 9)))
      t <- evalIO (H.fromList kvs :: IO (Dictionary RealWorld Unboxed Int Strict Int))
      f <- evalIO (H.freeze t)
      let model = Map.fromList kvs
      H.frozenSize f === Map.size model
      forM_ [0 .. 3000] $ \k -> do
        let i = H.findElem f k
        H.frozenLookup f k === Map.lookup k model
        H.frozenMember f k === Map.member k model
        when (i >= 0) $ (H.frozenKeyAt f i, H.frozenValueAt f i) === (k, model Map.! k)
      sort (H.frozenToList f) === Map.toList model
      -- thaw copies: mutating the thawed table leaves the frozen one alone
      t' <- evalIO (H.thaw f)
      evalIO (H.insert t' (-1) 0)
      H.frozenMember f (-1) === False
    it "unsafeFreeze then unsafeThaw round-trips" $ do
      t <- H.fromList (map (\i -> (i, i * 2)) [0 .. 999]) :: IO IntTable
      f <- H.unsafeFreeze t
      t' <- H.unsafeThaw f
      H.insert t' 1000 2000
      H.size t' `shouldReturn` 1001
      H.lookup t' 500 `shouldReturn` Just 1000
      invariants t' `shouldReturn` []
