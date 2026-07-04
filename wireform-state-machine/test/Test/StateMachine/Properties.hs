{-# LANGUAGE TemplateHaskell #-}

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

data PropState = M | U | V | W | MH | W1 | W2 | Par | RA | RB | A0 | A1 | B0 | B1
data PropEvent = A | B | C | D | E
  deriving stock (Show, Enum, Bounded)
data PropAction = Bump

deriveKeyKind ''PropState
deriveKeyKind ''PropEvent
deriveKeyKind ''PropAction

{- | Compound nesting, a parallel state, and history, with every event
handled somewhere (and sometimes nowhere — drops are part of the walk).
-}
type PropChart :: ChartSpec PropState PropEvent NoKey PropAction NoKey NoKey NoKey
type PropChart =
  Chart
    "prop"
    Int
    ()
    '[ 'A ::: ()
     , 'B ::: ()
     , 'C ::: ()
     , 'D ::: ()
     , 'E ::: ()
     ]
    '[ Compound
        'M
        'U
        '[ State 'U '[On 'A ==> To 'V ! '[ 'Bump ], On 'B ==> To 'W]
         , State 'V '[On 'A ==> To 'U ! '[ 'Bump ]]
         , Compound
            'W
            'W1
            '[ State 'W1 '[On 'A ==> To 'W2 ! '[ 'Bump ]]
             , State 'W2 '[]
             ]
            '[On 'C ==> To 'U]
         , Hist 'MH
         ]
        '[On 'D ==> To 'Par]
     , Parallel
        'Par
        '[ Compound
            'RA
            'A0
            '[ State 'A0 '[On 'A ==> To 'A1 ! '[ 'Bump ]]
             , State 'A1 '[On 'B ==> To 'A0]
             ]
            '[]
         , Compound
            'RB
            'B0
            '[ State 'B0 '[On 'C ==> To 'B1]
             , State 'B1 '[]
             ]
            '[]
         ]
        '[On 'E ==> To 'MH, On 'D ==> To 'M]
     ]
    'M

propImpl :: ChartImpl LogM PropChart
propImpl =
  chartImpl
    RNil
    (assign @'Bump (\c _ -> c + 1) :& RNil)
    RNil
    RNil
    (const ())

eventKeys :: [PropEvent]
eventKeys = [minBound .. maxBound]

evOf :: PropEvent -> StepEvent PropChart
evOf e = case e of
  A -> EvExternal (mkEvent_ @'A)
  B -> EvExternal (mkEvent_ @'B)
  C -> EvExternal (mkEvent_ @'C)
  D -> EvExternal (mkEvent_ @'D)
  E -> EvExternal (mkEvent_ @'E)

-- | The machine after initialization and after each event, in order.
machinesAfter :: [PropEvent] -> Either StepFault [Machine PropChart]
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
    [ ("U", "M")
    , ("V", "M")
    , ("W", "M")
    , ("MH", "M")
    , ("W1", "W")
    , ("W2", "W")
    , ("RA", "Par")
    , ("RB", "Par")
    , ("A0", "RA")
    , ("A1", "RA")
    , ("B0", "RB")
    , ("B1", "RB")
    ]

topLevel :: [Text]
topLevel = ["M", "Par"]

compoundKids :: [(Text, [Text])]
compoundKids =
  [ ("M", ["U", "V", "W"])
  , ("W", ["W1", "W2"])
  , ("RA", ["A0", "A1"])
  , ("RB", ["B0", "B1"])
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
    | Set.member "Par" cfg =
        concatMap
          (\r -> if Set.member r cfg then [] else ["parallel region " <> r <> " is not active"])
          ["RA", "RB"]
    | otherwise = []
  histories
    | Set.member "MH" cfg = ["history pseudo-state MH is active"]
    | otherwise = []
  tshow :: (Show a) => a -> Text
  tshow = T.pack . show

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_configInvariants :: Property
prop_configInvariants = property $ do
  names <- forAll (Gen.list (Range.linear 0 40) (Gen.element eventKeys))
  case machinesAfter names of
    Left f -> annotateShow f >> failure
    Right ms -> mapM_ checkOne ms
 where
  checkOne m = do
    annotateShow (config m)
    violations (config m) === []

prop_snapshotRoundtrip :: Property
prop_snapshotRoundtrip = property $ do
  names <- forAll (Gen.list (Range.linear 0 30) (Gen.element eventKeys))
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
