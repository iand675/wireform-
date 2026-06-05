{-# LANGUAGE OverloadedStrings #-}

module Test.EDI.Derive (tests) where

import qualified Data.Vector as V
import Test.Syd

import qualified EDI.Class as E
import qualified EDI.Value as EV

import Test.EDI.Derive.Instances ()
import Test.EDI.Derive.Types

tests :: Spec
tests = describe "EDI.Derive" $ sequence_
  [ recordTests
  , enumTests
  , sumTests
  ]

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "encodes a record as a renamed positional segment" $ do
      let value = NameSegment "ST" "Acme" "secret" (coercedCustomer "C-1") (Just "92")
      E.toEDI value `shouldBe`
        EV.Interchange
          EV.defaultSyntax
          ( V.singleton
              ( EV.Segment
                  "N1"
                  ( V.fromList
                      [ EV.Simple "ST"
                      , EV.Simple "Acme"
                      , EV.Simple "C-1"
                      , EV.Simple "92"
                      ]
                  )
              )
          )
  , it "round-trips and fills skipped fields from defaults" $ do
      let value = NameSegment "ST" "Acme" "secret" (coercedCustomer "C-1") Nothing
      E.fromEDI (E.toEDI value) `shouldBe`
        Right (NameSegment "ST" "Acme" defaultPrivate (coercedCustomer "C-1") Nothing)
  ]

enumTests :: Spec
enumTests = describe "enum" $ sequence_
  [ it "encodes constructor rename as segment tag" $
      E.toEDI Accepted `shouldBe`
        EV.Interchange EV.defaultSyntax (V.singleton (EV.Segment "A" V.empty))
  , it "round-trips enum tags" $ do
      E.fromEDI (E.toEDI Accepted) `shouldBe` Right Accepted
      E.fromEDI (E.toEDI Rejected) `shouldBe` Right Rejected
  ]

sumTests :: Spec
sumTests = describe "sum" $ sequence_
  [ it "encodes unary constructor as one segment element" $
      E.toEDI (ShipmentCreated "S-1") `shouldBe`
        EV.Interchange
          EV.defaultSyntax
          (V.singleton (EV.Segment "SC" (V.singleton (EV.Simple "S-1"))))
  , it "encodes nullary constructor as an empty segment" $
      E.toEDI ShipmentClosed `shouldBe`
        EV.Interchange EV.defaultSyntax (V.singleton (EV.Segment "SX" V.empty))
  , it "round-trips n-ary constructor elements" $
      E.fromEDI (E.toEDI (ShipmentMoved "S-1" 3)) `shouldBe`
        Right (ShipmentMoved "S-1" 3)
  ]
