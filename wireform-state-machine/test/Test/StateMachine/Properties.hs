-- | Hedgehog properties: configuration invariants hold after every step
-- of a random event sequence, and snapshot/restore through JSON is the
-- identity on machine state after any event prefix.
module Test.StateMachine.Properties (tests) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Property, annotateShow, evalEither, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import StateMachine hiding (context)
import StateMachine.Machine (Machine (..))

import Test.StateMachine.Support

{- | Compound nesting, a parallel state, and history, with every event
handled somewhere (and sometimes nowhere — drops are part of the walk).
-}
type PropChart =
  Chart
    "prop"
    Int
    ()
    '[ "A" ::: ()
     , "B" ::: ()
     , "C" ::: ()
     , "D" ::: ()
     , "E" ::: ()
     ]
    '[ Compound
        "m"
        "u"
        '[ State "u" '[On "A" ==> To "v" ! '["bump"], On "B" ==> To "w"]
         , State "v" '[On "A" ==> To "u" ! '["bump"]]
         , Compound
            "w"
            "w1"
            '[ State "w1" '[On "A" ==> To "w2" ! '["bump"]]
             , State "w2" '[]
             ]
            '[On "C" ==> To "u"]
         , Hist "mh"
         ]
        '[On "D" ==> To "par"]
     , Parallel
        "par"
        '[ Compound
            "ra"
            "a0"
            '[ State "a0" '[On "A" ==> To "a1" ! '["bump"]]
             , State "a1" '[On "B" ==> To "a0"]
             ]
            '[]
         , Compound
            "rb"
            "b0"
            '[ State "b0" '[On "C" ==> To "b1"]
             , State "b1" '[]
             ]
            '[]
         ]
        '[On "E" ==> To "mh", On "D" ==> To "m"]
     ]
    "m"

propImpl :: ChartImpl LogM PropChart
propImpl =
  chartImpl
    RNil
    (assign @"bump" (\c _ -> c + 1) :& RNil)
    RNil
    RNil
    (const ())

eventNames :: [Text]
eventNames = ["A", "B", "C", "D", "E"]

evOf :: Text -> StepEvent PropChart
evOf name = case name of
  "A" -> EvExternal (mkEvent_ @"A")
  "B" -> EvExternal (mkEvent_ @"B")
  "C" -> EvExternal (mkEvent_ @"C")
  "D" -> EvExternal (mkEvent_ @"D")
  "E" -> EvExternal (mkEvent_ @"E")
  other -> error ("unknown test event " <> show other)

-- | The machine after initialization and after each event, in order.
machinesAfter :: [Text] -> Either StepFault [Machine PropChart]
machinesAfter names = fst $ runLog $ do
  r0 <- initialize propImpl 0
  case r0 of
    Left f -> pure (Left f)
    Right s0 -> go (sMachine s0) [sMachine s0] names
 where
  go _ acc [] = pure (Right (reverse acc))
  go m acc (n : rest) = do
    r <- step propImpl m (evOf n)
    case r of
      Left f -> pure (Left f)
      Right s -> go (sMachine s) (sMachine s : acc) rest

{-------------------------------------------------------------------------------
  Invariants, stated against a hand-written copy of the chart's topology
  (independent of the implementation's own coherence checker)
-------------------------------------------------------------------------------}

parentOfName :: Map Text Text
parentOfName =
  Map.fromList
    [ ("u", "m")
    , ("v", "m")
    , ("w", "m")
    , ("mh", "m")
    , ("w1", "w")
    , ("w2", "w")
    , ("ra", "par")
    , ("rb", "par")
    , ("a0", "ra")
    , ("a1", "ra")
    , ("b0", "rb")
    , ("b1", "rb")
    ]

topLevel :: [Text]
topLevel = ["m", "par"]

compoundKids :: [(Text, [Text])]
compoundKids =
  [ ("m", ["u", "v", "w"])
  , ("w", ["w1", "w2"])
  , ("ra", ["a0", "a1"])
  , ("rb", ["b0", "b1"])
  ]

-- | Everything wrong with a configuration; empty means legal.
violations :: Set Text -> [Text]
violations cfg = topCheck <> closure <> compounds <> parallels <> histories
 where
  topCheck = case filter (`Set.member` cfg) topLevel of
    [_] -> []
    tops -> ["expected exactly one active top-level state, got " <> tshow tops]
  closure = concatMap missingParent (Set.toList cfg)
  missingParent n = case Map.lookup n parentOfName of
    Just p | not (Set.member p cfg) -> [n <> " is active without its parent " <> p]
    _ -> []
  compounds = concatMap oneChild compoundKids
  oneChild (c, kids)
    | not (Set.member c cfg) = []
    | otherwise = case filter (`Set.member` cfg) kids of
        [_] -> []
        act -> ["compound " <> c <> " has active children " <> tshow act]
  parallels
    | Set.member "par" cfg =
        concatMap
          (\r -> if Set.member r cfg then [] else ["parallel region " <> r <> " is not active"])
          ["ra", "rb"]
    | otherwise = []
  histories
    | Set.member "mh" cfg = ["history pseudo-state mh is active"]
    | otherwise = []
  tshow :: (Show a) => a -> Text
  tshow = T.pack . show

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_configInvariants :: Property
prop_configInvariants = property $ do
  names <- forAll (Gen.list (Range.linear 0 40) (Gen.element eventNames))
  case machinesAfter names of
    Left f -> annotateShow f >> failure
    Right ms -> mapM_ checkOne ms
 where
  checkOne m = do
    annotateShow (config m)
    violations (config m) === []

prop_snapshotRoundtrip :: Property
prop_snapshotRoundtrip = property $ do
  names <- forAll (Gen.list (Range.linear 0 30) (Gen.element eventNames))
  m <- case machinesAfter names of
    Left f -> annotateShow f >> failure
    Right ms -> pure (last ms)
  let snap = snapshot propImpl m
  snap' <- evalEither (Aeson.eitherDecode (Aeson.encode snap))
  snap' === snap
  r <- evalEither (restore propImpl snap')
  config (restoredMachine r) === config m
  mHistory (restoredMachine r) === mHistory m
  ctxOf (restoredMachine r) === ctxOf m
  isFinished (restoredMachine r) === isFinished m

tests :: Spec
tests = describe "properties" $ do
  it "configuration invariants hold after every step of a random event sequence" prop_configInvariants
  it "snapshot/restore through JSON is the identity after a random event prefix" prop_snapshotRoundtrip
