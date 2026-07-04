{-# LANGUAGE TemplateHaskell #-}

-- | History pseudo-states: shallow vs deep restoration, empty-history
-- defaults, and history surviving snapshot/restore.
module Test.StateMachine.History (tests) where

import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

data HState = M | Inner1 | X | Inner2 | U | V | H | Hd | Hdef | Away
data HEvent = ENTER | SWITCH | GO | OUT | BACK | DEEPBACK | DEFBACK
deriveKeyKind ''HState
deriveKeyKind ''HEvent

{- | Two inner compounds under @M@; @X@ jumps across to @Inner2@, whose
@U@ advances to @V@. @OUT@ exits @M@ (recording all three histories);
@Away@ can re-enter through the shallow, deep, or defaulted history node.
-}
type HistChart :: ChartSpec HState HEvent NoKey NoKey NoKey NoKey NoKey
type HistChart =
  Chart
    "hist"
    ()
    ()
    '[ 'ENTER ::: ()
     , 'SWITCH ::: ()
     , 'GO ::: ()
     , 'OUT ::: ()
     , 'BACK ::: ()
     , 'DEEPBACK ::: ()
     , 'DEFBACK ::: ()
     ]
    '[ Compound
        'M
        'Inner1
        '[ Compound 'Inner1 'X '[State 'X '[On 'SWITCH ==> To 'U]] '[]
         , Compound
            'Inner2
            'U
            '[ State 'U '[On 'GO ==> To 'V]
             , State 'V '[]
             ]
            '[]
         , Hist 'H
         , HistDeep 'Hd
         , HistWith 'Hdef 'Shallow 'V
         ]
        '[On 'OUT ==> To 'Away]
     , State
        'Away
        '[ On 'ENTER ==> To 'M
         , On 'BACK ==> To 'H
         , On 'DEEPBACK ==> To 'Hd
         , On 'DEFBACK ==> To 'Hdef
         ]
     ]
    'Away

histImpl :: ChartImpl LogM HistChart
histImpl = chartImpl RNil RNil RNil RNil (const ())

-- | Boot, then drive into @Inner2/V@ and back out: histories recorded as
-- shallow @Inner2@, deep @V@.
recorded :: Machine HistChart
recorded =
  feed
    histImpl
    (boot histImpl ())
    [ EvExternal (mkEvent_ @'ENTER)
    , EvExternal (mkEvent_ @'SWITCH)
    , EvExternal (mkEvent_ @'GO)
    , EvExternal (mkEvent_ @'OUT)
    ]

tests :: Spec
tests = describe "history" $ do
  it "shallow history restores the last active immediate child; deeper compounds restart at their initial" $ do
    let m = advance histImpl recorded (EvExternal (mkEvent_ @'BACK))
    -- Inner2 was active (not Inner1), but V's detail is forgotten: Inner2
    -- restarts at its initial U.
    config m `shouldBe` Set.fromList ["M", "Inner2", "U"]

  it "deep history restores the exact atomic configuration" $ do
    let m = advance histImpl recorded (EvExternal (mkEvent_ @'DEEPBACK))
    config m `shouldBe` Set.fromList ["M", "Inner2", "V"]

  it "empty history falls to the declared default target" $ do
    let m = advance histImpl (boot histImpl ()) (EvExternal (mkEvent_ @'DEFBACK))
    config m `shouldBe` Set.fromList ["M", "Inner2", "V"]

  it "empty history with no default falls to the parent's initial" $ do
    let m = advance histImpl (boot histImpl ()) (EvExternal (mkEvent_ @'BACK))
    config m `shouldBe` Set.fromList ["M", "Inner1", "X"]

  it "history survives a snapshot/restore roundtrip through JSON" $ do
    let snap = snapshot histImpl recorded
        decoded = either (error . ("snapshot did not decode: " <>)) id (Aeson.eitherDecode (Aeson.encode snap))
        restored = either (error . ("restore failed: " <>) . show) restoredMachine (restore histImpl decoded)
        shallow = advance histImpl restored (EvExternal (mkEvent_ @'BACK))
        deep = advance histImpl restored (EvExternal (mkEvent_ @'DEEPBACK))
    config shallow `shouldBe` Set.fromList ["M", "Inner2", "U"]
    config deep `shouldBe` Set.fromList ["M", "Inner2", "V"]
