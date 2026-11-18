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
          let w = W# (hashKey# k)
              want = case w of W# w# -> W# (tagOf# w#)
          when (want /= tag) $ bad ("tag mismatch for " <> show k)
          let walk h stride steps
                | h == g = pure ()
                | steps > groups = bad ("key " <> show k <> " not on its probe path")
                | otherwise = do
                    ofl <- byte (16 + 8 * h + 7)
                    let bit = 1 `unsafeShiftLW` fromIntegral (tag `rem` 8)
                    when (ofl `andW` bit == 0) $
                      bad ("missing overflow bit for " <> show k <> " in group " <> show h)
                    let stride' = stride + 1
                    walk ((h + stride') `rem` groups) stride' (steps + 1 :: Int)
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
          w = W# (hashKey# k)
          tag = case w of W# w# -> W# (tagOf# w#)
          bit = case tag of W# t# -> W# (uncheckedShiftL# 1## (word2Int# (and# t# 7##)))
          ofl (I# g) = IO $ \s -> case readWord8Array# ctrl (16# +# uncheckedIShiftL# g 3# +# 7#) s of
            (# s1, b #) -> (# s1, W# (word8ToWord# b) #)
          go h stride n
            | n >= groups = pure n
            | otherwise = do
                o <- ofl h
                if o `andW'` bit == 0
                  then pure n
                  else let stride' = stride + 1 in go ((h + stride') `rem` groups) stride' (n + 1)
      go (fromIntegral (w `andW'` fromIntegral (groups - 1))) 0 1
 where
  andW' :: Word -> Word -> Word
  andW' (W# a) (W# b) = W# (and# a b)

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
