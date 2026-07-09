{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- | The typed-row layer ("Lattice.Typed" + "Lattice.TH"): IDL-conforming
Haskell records over the protocol's dynamic row representation.

* __Wire forms__ — the generated 'LatticeValue' instances pin the §3.5.3
  canonical forms: wide integers as decimal strings, enums as bare strings
  (open enums round-tripping unknown spellings), sums as @{"$tag": …}@
  objects (open sums re-emitting an unknown tag's value identically).
* __Rows__ — 'toRowFields' emits exactly the stored fields (@maintained@
  in, computed and @on read@ out; absent optionals omitted, never null);
  'fromRowFields' inverts it and names the missing required field. A
  partial roundtrip preserves exactly the subset it was given.
* __Keys__ — a composite key renders comma-joined and parses back; a
  structurally impossible key text is 'Nothing'.
* __Loaders__ — 'loaders' adapts typed loaders to 'beLoad': canonical
  wire rows out, @lattice:internal@ for unregistered types, 'RowAbsent'
  for unparseable keys, 'checkLoaderCoverage' naming uncovered entities;
  E2E, a real origin answers a query wholly from typed loaders.
* __Memory__ — 'putEntityWith' stores the union of typed fields and
  backend-private extras under the row's own key, typed fields winning.
-}
module Test.Lattice.Typed (tests) where

import Control.Concurrent.STM (atomically)
import Data.Aeson qualified as A
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lattice.Backend (Backend (..), BackendFailure (..), EntityRow (..), LoadResult (..), Projection (..))
import Lattice.Backend.Memory (MemoryDb, MemoryHooks (..), defaultHooks, newMemoryDb, putEntityWith, readRow)
import Lattice.Schema (Schema)
import Lattice.TH (latticeTypesText)
import Lattice.Typed
import Lattice.Types (ArgName, FieldName, Ref (..))
import Test.Lattice.Fixtures (mustParseSchema, requireRight)
import Test.Lattice.Loop
import Test.Syd
import Test.Syd.Hedgehog ()


-- The fixture IDL: one splice generates the typed vocabulary AND persists
-- the source as @typedIdl@ for the runtime 'Schema' (a same-module
-- constant cannot feed the splice — GHC stage restriction — so the splice
-- is the single source of truth).
--
-- Sensor exercises a newtype Text key, an optional field, an I64 field, a
-- list of a closed enum, record/sum/open-enum/open-sum fields, a
-- @maintained@ derived field (stored), an argument-taking computed field
-- and an @on read@ derived field (both never row data). Reading has a
-- composite key over a newtype and an I64.
$( let idl =
        "schema typed.example.com\n\
        \\n\
        \newtype SensorId = Text\n\
        \\n\
        \enum Status closed = Active | Retired\n\
        \enum Channel open = Email | Sms\n\
        \\n\
        \data GeoPoint { lat: F64, lon: F64 }\n\
        \data Payment closed = Card { last4: Text, memo: Text? } | Cash\n\
        \data Signal open = Ping { note: Text } | Pong\n\
        \\n\
        \entity Sensor by id {\n\
        \  visible to all by default\n\
        \\n\
        \  id:           SensorId\n\
        \  label:        Text\n\
        \  nickname:     Text?\n\
        \  readingCount: I64\n\
        \  statuses:     [Status]\n\
        \  location:     GeoPoint\n\
        \  payment:      Payment\n\
        \  channel:      Channel\n\
        \  signal:       Signal\n\
        \\n\
        \  total: I64 derived reads own(readingCount) maintained\n\
        \  excerpt(len: I32 = 100): Text\n\
        \  summary: Text derived reads own(label) on read\n\
        \\n\
        \  fetch by id: public\n\
        \}\n\
        \\n\
        \entity Reading by (sensorId, seq) {\n\
        \  visible to all by default\n\
        \\n\
        \  sensorId: SensorId\n\
        \  seq:      I64\n\
        \  value:    F64\n\
        \  note:     Text?\n\
        \}\n\
        \\n\
        \get sensor(id: SensorId) of Sensor public\n"
    in (<>)
        <$> latticeTypesText idl
        <*> [d|
              typedIdl :: String
              typedIdl = idl
              |]
 )


typedSchema :: Schema
typedSchema = mustParseSchema (T.pack typedIdl)
{-# NOINLINE typedSchema #-}


tests :: Spec
tests =
  describe "typed rows" $ do
    describe "canonical wire values (§3.5.3)" $ do
      it "I64 renders as a decimal string and reads back leniently from a JSON number" $ do
        toWire (42 :: Int64) `shouldBe` A.String "42"
        toWire (-7 :: Int64) `shouldBe` A.String "-7"
        fromWire (A.String "42") `shouldBe` Right (42 :: Int64)
        fromWire (A.Number 42) `shouldBe` Right (42 :: Int64)

      it "a closed enum renders bare strings and rejects unknown spellings" $ do
        toWire Status'Active `shouldBe` A.String "Active"
        fromWire (A.String "Retired") `shouldBe` Right Status'Retired
        (fromWire (A.String "Weird") :: Either Text Status) `shouldSatisfy` isLeft

      it "an open enum round-trips an unknown spelling through Channel'Unknown" $ do
        fromWire (A.String "Email") `shouldBe` Right Channel'Email
        fromWire (A.String "Weird") `shouldBe` Right (Channel'Unknown "Weird")
        toWire (Channel'Unknown "Weird") `shouldBe` A.String "Weird"

      it "a sum renders {$tag, fields}, omitting an optional ctor field that is Nothing" $ do
        toWire (Payment'Card "4242" (Just "gift"))
          `shouldBe` A.object [("$tag", A.String "Card"), ("last4", A.String "4242"), ("memo", A.String "gift")]
        toWire (Payment'Card "4242" Nothing)
          `shouldBe` A.object [("$tag", A.String "Card"), ("last4", A.String "4242")]
        toWire Payment'Cash `shouldBe` A.object [("$tag", A.String "Cash")]
        fromWire (A.object [("$tag", A.String "Card"), ("last4", A.String "4242")])
          `shouldBe` Right (Payment'Card "4242" Nothing)
        (fromWire (A.object [("$tag", A.String "Voucher")]) :: Either Text Payment)
          `shouldSatisfy` isLeft

      it "an open sum carries an unknown tag through Signal'Unknown, re-emitting the value identically" $ do
        let raw = A.object [("$tag", A.String "Zap"), ("watts", A.Number 11)]
        fromWire raw `shouldBe` Right (Signal'Unknown raw)
        toWire (Signal'Unknown raw) `shouldBe` raw
        fromWire (A.object [("$tag", A.String "Ping"), ("note", A.String "hi")])
          `shouldBe` Right (Signal'Ping "hi")

    describe "entity rows" $ do
      it "a full row emits exactly the canonical stored fields; computed and on-read fields never appear" $
        toRowFields sensorFull `shouldBe` sensorRow

      it "a full row round-trips through its wire form" $
        fromRowFields sensorRow `shouldBe` Right sensorFull

      it "a row missing a required field decodes to a RowError naming the field" $
        case fromRowFields (Map.delete "label" sensorRow) :: Either RowError (Sensor Full) of
          Left e -> reField e `shouldBe` "label"
          Right row -> expectationFailure ("decoded a row with no label: " <> show row)

      it "a partial roundtrip preserves exactly the subset it was given" $ do
        let subset = Map.restrictKeys sensorRow (Set.fromList ["label", "payment", "readingCount"])
        partial <- requireRight (fromPartialRowFields subset :: Either RowError (Sensor Partial))
        toPartialRowFields partial `shouldBe` subset

      it "fromRowFields inverts toRowFields on generated full rows" $
        H.withTests 100 $
          H.property $ do
            row <- H.forAll genReading
            H.tripping row toRowFields fromRowFields

    describe "entity keys" $
      it "a composite key renders comma-joined and parses back; a wrong shape is Nothing" $ do
        let p = Proxy @Reading
            k = ReadingKey {readingKeySensorId = SensorId "s1", readingKeySeq = 7}
        renderEntityKey p k `shouldBe` "s1,7"
        parseEntityKey p "s1,7" `shouldBe` Just k
        parseEntityKey p "s1" `shouldBe` Nothing
        parseEntityKey p "s1,7,9" `shouldBe` Nothing
        parseEntityKey p "s1,x" `shouldBe` Nothing

    describe "loaders" $ do
      it "serves typed rows as canonical wire values; loader-omitted keys stay omitted" $ do
        res <- loaders [sensorLoader] "Sensor" ProjectAll ["s1", "gone", "never"]
        Map.lookup "s1" res
          `shouldBe` Just
            ( Right . RowFound . EntityRow "e7" $
                Map.fromList
                  [ ("id", A.String "s1")
                  , ("label", A.String "north gate")
                  , ("readingCount", A.String "42")
                  , ("statuses", A.toJSON (["Active", "Retired"] :: [Text]))
                  ]
            )
        Map.lookup "gone" res `shouldBe` Just (Right RowAbsent)
        Map.keys res `shouldBe` ["gone", "s1"]

      it "an unregistered entity type fails every key of its batch with lattice:internal" $ do
        res <- loaders [sensorLoader] "Reading" ProjectAll ["s1,1", "s1,2"]
        Map.map (either bfCode (const "unexpected success")) res
          `shouldBe` Map.fromList [("s1,1", "lattice:internal"), ("s1,2", "lattice:internal")]

      it "a structurally impossible key is RowAbsent and never reaches the loader" $ do
        -- readingLoader answers 'found' for EVERY key it is handed, so a
        -- malformed key leaking through would surface as RowFound here.
        res <- loaders [readingLoader] "Reading" ProjectAll ["s1", "s1,x", "s1,1,2"]
        res
          `shouldBe` Map.fromList
            [ ("s1", Right RowAbsent)
            , ("s1,x", Right RowAbsent)
            , ("s1,1,2", Right RowAbsent)
            ]
        ok <- loaders [readingLoader] "Reading" ProjectAll ["s1,7"]
        ok
          `shouldBe` Map.singleton
            "s1,7"
            ( Right . RowFound . EntityRow "e1" $
                Map.fromList
                  [ ("sensorId", A.String "s1")
                  , ("seq", A.String "7")
                  , ("value", A.Number 1.5)
                  ]
            )

      it "checkLoaderCoverage names the schema entities lacking loaders" $ do
        checkLoaderCoverage typedSchema [sensorLoader] `shouldBe` Left ["Reading"]
        checkLoaderCoverage typedSchema [sensorLoader, readingLoader] `shouldBe` Right ()

    describe "memory backend" $
      it "putEntityWith stores the union under the row's own key, typed fields winning collisions" $ do
        db <- newMemoryDb
        let edgeKeys = A.toJSON (["r1", "r2"] :: [Text])
        atomically
          ( putEntityWith
              db
              sensorFull
              (Map.fromList [("edgeKeys", edgeKeys), ("label", A.String "stale")])
          )
        row <- atomically (readRow db "Sensor" "s1")
        case row of
          RowFound (EntityRow _ fs) -> fs `shouldBe` Map.insert "edgeKeys" edgeKeys sensorRow
          other -> expectationFailure ("expected the seeded row, got: " <> show other)

    describe "end to end" $
      it "the executor answers a query wholly from typed loaders" $
        withLoop
          (loopSpec typedSchema)
            { lsHooks = defaultHooks {mhGetRoots = Map.fromList [("sensor", sensorRoot)]}
            , lsWrap = \b -> b {beLoad = loaders [sensorLoader, readingLoader]}
            }
          $ \loop ->
            clientFor loop id $ \lc -> do
              r <- runQuery lc "query { sensor(id: \"s1\") { label readingCount statuses } }" Map.empty
              -- A schemaless client leaves the get root as its wire array.
              nodes <- rootValue "sensor" r >>= asArray
              case nodes of
                [sensor] -> do
                  textField "label" sensor >>= (`shouldBe` "north gate")
                  -- The wide integer arrives in its §3.5.3 decimal-string form.
                  textField "readingCount" sensor >>= (`shouldBe` "42")
                  objectField "statuses" sensor
                    >>= asArray
                    >>= (`shouldBe` [A.String "Active", A.String "Retired"])
                _ -> expectationFailure ("expected exactly one sensor, got: " <> show nodes)


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sensorFull :: Sensor Full
sensorFull =
  Sensor
    { sensorId = SensorId "s1"
    , sensorLabel = "north gate"
    , sensorNickname = Nothing
    , sensorReadingCount = 42
    , sensorStatuses = [Status'Active, Status'Retired]
    , sensorLocation = GeoPoint {geoPointLat = 1.5, geoPointLon = -2.25}
    , sensorPayment = Payment'Card "4242" Nothing
    , sensorChannel = Channel'Email
    , sensorSignal = Signal'Ping "ok"
    , sensorTotal = 99
    }


-- | 'sensorFull' in its canonical wire form: every stored field in its
-- §3.5.3 spelling, the absent optional (@nickname@) omitted, the computed
-- (@excerpt@) and @on read@ (@summary@) fields structurally impossible.
sensorRow :: Map FieldName A.Value
sensorRow =
  Map.fromList
    [ ("channel", A.String "Email")
    , ("id", A.String "s1")
    , ("label", A.String "north gate")
    , ("location", A.object [("lat", A.Number 1.5), ("lon", A.Number (-2.25))])
    , ("payment", A.object [("$tag", A.String "Card"), ("last4", A.String "4242")])
    , ("readingCount", A.String "42")
    , ("signal", A.object [("$tag", A.String "Ping"), ("note", A.String "ok")])
    , ("statuses", A.toJSON (["Active", "Retired"] :: [Text]))
    , ("total", A.String "99")
    ]


-- | The projected shape a loader would return for @s1@: typed values only,
-- unfetched fields 'Nothing'.
sensorPartial :: Sensor Partial
sensorPartial =
  Sensor
    { sensorId = Just (SensorId "s1")
    , sensorLabel = Just "north gate"
    , sensorNickname = Nothing
    , sensorReadingCount = Just 42
    , sensorStatuses = Just [Status'Active, Status'Retired]
    , sensorLocation = Nothing
    , sensorPayment = Nothing
    , sensorChannel = Nothing
    , sensorSignal = Nothing
    , sensorTotal = Nothing
    }


-- | Serves @s1@, answers 'absent' for @gone@, and omits every other
-- key from its result map.
sensorLoader :: EntityLoader
sensorLoader = entityLoader @Sensor $ \_proj keys -> pure (Map.fromList (mapMaybe serve keys))
  where
    serve k
      | k == SensorId "s1" = Just (k, found "e7" sensorPartial)
      | k == SensorId "gone" = Just (k, absent)
      | otherwise = Nothing


-- | Answers every key it is handed (so only 'parseEntityKey' stands
-- between a malformed key text and a bogus 'RowFound').
readingLoader :: EntityLoader
readingLoader = entityLoader @Reading $ \_proj keys -> pure (Map.fromList (map serve keys))
  where
    serve k =
      ( k
      , found "e1" $
          Reading
            { readingSensorId = Just (readingKeySensorId k)
            , readingSeq = Just (readingKeySeq k)
            , readingValue = Just 1.5
            , readingNote = Nothing
            }
      )


-- | The @sensor@ get-root: resolve the @id@ argument to a 'Ref' (the
-- typed loaders do the actual reading).
sensorRoot :: MemoryDb -> Map ArgName A.Value -> IO (Maybe Ref)
sensorRoot _db args = pure $ case Map.lookup "id" args of
  Just (A.String k) -> Just (Ref "Sensor" k)
  _ -> Nothing


genReading :: H.Gen (Reading Full)
genReading = do
  sid <- Gen.text (Range.linear 1 12) Gen.alphaNum
  seqNo <- Gen.int64 Range.linearBounded
  val <- Gen.double (Range.linearFrac (-1.0e12) 1.0e12)
  note <- Gen.maybe (Gen.text (Range.linear 0 12) Gen.unicode)
  pure
    Reading
      { readingSensorId = SensorId sid
      , readingSeq = seqNo
      , readingValue = val
      , readingNote = note
      }
