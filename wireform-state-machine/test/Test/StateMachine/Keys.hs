{-# LANGUAGE TemplateHaskell #-}

{- | The singleton key layer itself: reify\/demote round trips between the
three representations of a key (type-level constructor, singleton, value),
wire-name parsing, singleton equality, and the machine queries that speak
key values ('matchesKey', 'activeKeys', 'availableEventKeys', 'mkEventS').
-}
module Test.StateMachine.Keys (tests) where

import Data.List (nub)
import Data.Type.Equality (testEquality)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Syd
import Test.Syd.Hedgehog ()

import StateMachine hiding (context)

import Test.StateMachine.Support

{-------------------------------------------------------------------------------
  A small chart exercising every key role
-------------------------------------------------------------------------------}

data KState = Idle | Busy | Won | Lost
  deriving stock (Show, Eq)
data KEvent = START | FINISH | ABORT
  deriving stock (Show, Eq)
data KGuard = Allowed
  deriving stock (Show, Eq)
data KAction = Note
  deriving stock (Show, Eq)
data KService = Work
  deriving stock (Show, Eq)
data KInvoke = TheJob
  deriving stock (Show, Eq)
data KOutput = Prize
  deriving stock (Show, Eq)

deriveKeyKind ''KState
deriveKeyKind ''KEvent
deriveKeyKind ''KGuard
deriveKeyKind ''KAction
deriveKeyKind ''KService
deriveKeyKind ''KInvoke
deriveKeyKind ''KOutput

type KeyChart :: ChartSpec KState KEvent KGuard KAction KService KInvoke KOutput
type KeyChart =
  Chart
    "keys"
    Bool
    ()
    '[ 'START ::: Int
     , 'FINISH ::: ()
     , 'ABORT ::: ()
     ]
    '[ State 'Idle '[On 'START ?: 'Allowed ==> To 'Busy ! '[ 'Note ]]
     , State
        'Busy
        '[ Invoke
            'TheJob
            'Work
            '[OnDone ==> To 'Won]
            '[OnError ==> To 'Lost]
         , On 'ABORT ==> To 'Idle
         ]
     , FinalWith 'Won 'Prize
     , State 'Lost '[On 'FINISH ==> To 'Idle]
     ]
    'Idle

keyImpl :: ChartImpl LogM KeyChart
keyImpl =
  chartImpl
    (mkGuard @'Allowed const :& RNil)
    (logAct @'Note :& RNil)
    (mkService @'Work (\_ _ -> pure (Right () :: Either () ())) :& RNil)
    (mkOutput @'Prize (\_ _ -> (42 :: Int)) :& RNil)
    (const ())

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

genState :: Property
genState = property $ do
  sk <- forAll (Gen.element (keyUniverse @KState))
  -- promote . demote = id (via the singleton inside SomeKey)
  withSomeKey sk (promoteKey . demoteKey) === sk
  -- parse . name = id
  parseKey (someKeyName sk) === Just sk

tests :: Spec
tests = describe "singleton keys" $ do
  describe "reify / demote round trips" $ do
    it "demote reifies a type-level key to its value" $ do
      demote @'Busy `shouldBe` Busy
      demote @'FINISH `shouldBe` FINISH

    it "keyNameOf and keyName agree with constructor spellings" $ do
      keyNameOf @'Idle `shouldBe` "Idle"
      keyName Lost `shouldBe` "Lost"
      someKeyName (promoteKey ABORT) `shouldBe` "ABORT"

    it "withKey promotes a value and hands back its singleton" $ do
      withKey Won (keyName . demoteKey) `shouldBe` "Won"

    it "promote/demote and parse/name are inverses on the whole universe" genState

    it "the universe lists every constructor once, in declaration order" $ do
      map someKeyName (keyUniverse @KState) `shouldBe` ["Idle", "Busy", "Won", "Lost"]
      let names = map someKeyName (keyUniverse @KEvent)
      names `shouldBe` nub names

    it "parseKey rejects names outside the kind" $ do
      parseKey @KState "START" `shouldBe` Nothing
      parseKey @KState "#root" `shouldBe` Nothing

  describe "singleton equality" $ do
    it "testEquality is reflexive on a key and refutes distinct keys" $ do
      case testEquality (skey @'Busy) (skey @'Busy) of
        Just _ -> pure ()
        Nothing -> expectationFailure "expected Busy ~ Busy"
      case testEquality (skey @'Busy) (skey @'Idle) of
        Just _ -> expectationFailure "expected Busy /~ Idle"
        Nothing -> pure ()

    it "SomeKey equality follows testEquality" $ do
      promoteKey Busy `shouldBe` promoteKey Busy
      promoteKey Busy `shouldNotBe` promoteKey Idle

  describe "machines speak key values" $ do
    it "matchesKey and activeKeys reify the configuration" $ do
      let m = boot keyImpl True
      matchesKey Idle m `shouldBe` True
      matchesKey Busy m `shouldBe` False
      activeKeys m `shouldBe` [Idle]

    it "matches accepts a promoted constructor" $ do
      let m = advance keyImpl (boot keyImpl True) (EvExternal (mkEvent @'START 3))
      matches @'Busy m `shouldBe` True

    it "activeKeys pattern-matches exhaustively over the state kind" $ do
      let m = advance keyImpl (boot keyImpl True) (EvExternal (mkEvent @'START 3))
          describe' k = case k of
            Idle -> "idle"
            Busy -> "busy"
            Won -> "won"
            Lost -> "lost"
      map describe' (activeKeys m) `shouldBe` ["busy" :: String]

    it "availableEventKeys lists the events that could fire" $ do
      let m0 = boot keyImpl True
      availableEventKeys keyImpl m0 `shouldBe` [START]

    it "a guarded transition still consults the registry" $ do
      let m = advance keyImpl (boot keyImpl False) (EvExternal (mkEvent @'START 3))
      matchesKey Idle m `shouldBe` True

  describe "singleton-valued events" $ do
    it "mkEventS builds the same event as mkEvent" $ do
      let (_, lg) =
            stepLog keyImpl (boot keyImpl True) (EvExternal (mkEventS (skey @'START) 3))
      lg `shouldBe` ["Note"]

    it "withKey + mkEventS dispatches on the singleton with typed payloads" $ do
      -- Reify an ABORT arriving as a value back to the type level and
      -- build its typed event; the payload type is refined per branch.
      let ev :: EventVal KeyChart
          ev = withKey ABORT $ \s -> case s of
            SSTART -> mkEventS s 9
            SFINISH -> mkEventS s ()
            SABORT -> mkEventS s ()
          m1 = advance keyImpl (boot keyImpl True) (EvExternal (mkEvent @'START 3))
          m2 = advance keyImpl m1 (EvExternal ev)
      matchesKey Idle m2 `shouldBe` True

    it "matchEvent recovers the typed payload via singleton equality" $ do
      let ev :: EventVal KeyChart
          ev = mkEvent @'START 7
      matchEvent @'START ev `shouldBe` Just 7
      matchEvent @'FINISH ev `shouldBe` Nothing
