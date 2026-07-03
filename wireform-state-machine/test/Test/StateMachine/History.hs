-- | History pseudo-states: shallow vs deep restoration, empty-history
-- defaults, and history surviving snapshot/restore.
module Test.StateMachine.History (tests) where

import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

{- | Two inner compounds under @m@; @x@ jumps across to @inner2@, whose
@u@ advances to @v@. @OUT@ exits @m@ (recording all three histories);
@away@ can re-enter through the shallow, deep, or defaulted history node.
-}
type HistChart =
  Chart
    "hist"
    ()
    ()
    '[ "ENTER" ::: ()
     , "SWITCH" ::: ()
     , "GO" ::: ()
     , "OUT" ::: ()
     , "BACK" ::: ()
     , "DEEPBACK" ::: ()
     , "DEFBACK" ::: ()
     ]
    '[ Compound
        "m"
        "inner1"
        '[ Compound "inner1" "x" '[State "x" '[On "SWITCH" ==> To "u"]] '[]
         , Compound
            "inner2"
            "u"
            '[ State "u" '[On "GO" ==> To "v"]
             , State "v" '[]
             ]
            '[]
         , Hist "h"
         , HistDeep "hd"
         , HistWith "hdef" 'Shallow "v"
         ]
        '[On "OUT" ==> To "away"]
     , State
        "away"
        '[ On "ENTER" ==> To "m"
         , On "BACK" ==> To "h"
         , On "DEEPBACK" ==> To "hd"
         , On "DEFBACK" ==> To "hdef"
         ]
     ]
    "away"

histImpl :: ChartImpl LogM HistChart
histImpl = chartImpl RNil RNil RNil RNil (const ())

-- | Boot, then drive into @inner2/v@ and back out: histories recorded as
-- shallow @inner2@, deep @v@.
recorded :: Machine HistChart
recorded =
  feed
    histImpl
    (boot histImpl ())
    [ EvExternal (mkEvent_ @"ENTER")
    , EvExternal (mkEvent_ @"SWITCH")
    , EvExternal (mkEvent_ @"GO")
    , EvExternal (mkEvent_ @"OUT")
    ]

tests :: Spec
tests = describe "history" $ do
  it "shallow history restores the last active immediate child; deeper compounds restart at their initial" $ do
    let m = advance histImpl recorded (EvExternal (mkEvent_ @"BACK"))
    -- inner2 was active (not inner1), but v's detail is forgotten: inner2
    -- restarts at its initial u.
    config m `shouldBe` Set.fromList ["m", "inner2", "u"]

  it "deep history restores the exact atomic configuration" $ do
    let m = advance histImpl recorded (EvExternal (mkEvent_ @"DEEPBACK"))
    config m `shouldBe` Set.fromList ["m", "inner2", "v"]

  it "empty history falls to the declared default target" $ do
    let m = advance histImpl (boot histImpl ()) (EvExternal (mkEvent_ @"DEFBACK"))
    config m `shouldBe` Set.fromList ["m", "inner2", "v"]

  it "empty history with no default falls to the parent's initial" $ do
    let m = advance histImpl (boot histImpl ()) (EvExternal (mkEvent_ @"BACK"))
    config m `shouldBe` Set.fromList ["m", "inner1", "x"]

  it "history survives a snapshot/restore roundtrip through JSON" $ do
    let snap = snapshot histImpl recorded
        decoded = either (error . ("snapshot did not decode: " <>)) id (Aeson.eitherDecode (Aeson.encode snap))
        restored = either (error . ("restore failed: " <>) . show) restoredMachine (restore histImpl decoded)
        shallow = advance histImpl restored (EvExternal (mkEvent_ @"BACK"))
        deep = advance histImpl restored (EvExternal (mkEvent_ @"DEEPBACK"))
    config shallow `shouldBe` Set.fromList ["m", "inner2", "u"]
    config deep `shouldBe` Set.fromList ["m", "inner2", "v"]
