{-# LANGUAGE OverloadedStrings #-}

module Test.EDI (ediTests) where

import Data.Vector qualified as V
import EDI
import EDI.EDIFACT qualified as EDIFACT
import Test.Syd


ediTests :: Spec
ediTests = describe "EDI package integration" $ do
  it "round-trips a segment through the public entry module" $ do
    let document = interchange defaultSyntax [segment "ST" [Simple "850", Simple "1"]]
    decode (encode document) `shouldBe` Right document
  it "retains the EDIFACT release boundary through typed decode" $ do
    let expected = Segment "UNB" (V.fromList [Simple "?", Simple "X"])
    (decodeEDI "UNB+??+X'" :: Either String Segment) `shouldBe` Right expected
  it "uses the EDIFACT-specific entry point for bare payload segments" $ do
    let expected =
          Interchange
            (EDIFACT.serviceStringSyntax EDIFACT.defaultServiceStringAdvice)
            (V.singleton (Segment "FTX" (V.fromList [Simple "?", Simple "X"])))
    EDIFACT.decodeEDIFACT "FTX+??+X'" `shouldBe` Right expected
