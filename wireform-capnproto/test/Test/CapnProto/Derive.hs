{-# LANGUAGE OverloadedStrings #-}

module Test.CapnProto.Derive (tests) where

import CapnProto.Decode qualified as CPD
import CapnProto.Derive
import CapnProto.Encode qualified as CPE
import CapnProto.Value qualified as CP
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Test.CapnProto.Derive.Instances ()
import Test.CapnProto.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "CapnProto.Derive" $
    sequence_
      [ recordTests
      , blobTests
      , profileTests
      , newtypeTests
      , coercedTests
      , enumTests
      , wireTests
      ]


recordTests :: Spec
recordTests =
  describe "record (mixed sections)" $
    sequence_
      [ it "splits scalars into data, Text into pointers" $ do
          let p = Position "origin" 3 7 (Just "home") "ignored"
          case toCapnProto p of
            CP.Struct dat ptrs -> do
              V.length dat `shouldBe` 2
              dat V.! 0 `shouldBe` CP.Int32 3
              dat V.! 1 `shouldBe` CP.Int32 7
              V.length ptrs `shouldBe` 2
              ptrs V.! 0 `shouldBe` CP.Text "origin"
              ptrs V.! 1 `shouldBe` CP.Text "home"
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "Nothing Maybe Text encodes as Void in pointer section" $ do
          let p = Position "none" 0 0 Nothing "ignored"
          case toCapnProto p of
            CP.Struct _ ptrs -> ptrs V.! 1 `shouldBe` CP.Void
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "round-trip reinstates skipped label from defaults" $ do
          let p = Position "origin" 3 7 (Just "home") "whatever"
          case fromCapnProto (toCapnProto p) of
            Right p' -> do
              posName p' `shouldBe` posName p
              posX p' `shouldBe` posX p
              posY p' `shouldBe` posY p
              posNote p' `shouldBe` posNote p
              posLabel p' `shouldBe` defaultLabel
            Left e -> expectationFailure e
      , it "round-trip with Nothing note" $ do
          let p = Position "none" 1 2 Nothing "whatever"
          case fromCapnProto (toCapnProto p) of
            Right p' -> do
              posNote p' `shouldBe` Nothing
              posLabel p' `shouldBe` defaultLabel
            Left e -> expectationFailure e
      ]


blobTests :: Spec
blobTests =
  describe "record (pointers only)" $
    sequence_
      [ it "Text + ByteString land in pointer section" $ do
          let b = Blob "payload" (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
          case toCapnProto b of
            CP.Struct dat ptrs -> do
              V.length dat `shouldBe` 0
              V.length ptrs `shouldBe` 2
              ptrs V.! 0 `shouldBe` CP.Text "payload"
              ptrs V.! 1 `shouldBe` CP.Data (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "round-trip" $ do
          let b = Blob "payload" (BS.pack [1, 2, 3])
          fromCapnProto (toCapnProto b) `shouldBe` Right b
      ]


profileTests :: Spec
profileTests =
  describe "record (data only)" $
    sequence_
      [ it "scalar-only record has empty pointer section" $ do
          let p = Profile 42 True 3.14
          case toCapnProto p of
            CP.Struct dat ptrs -> do
              V.length dat `shouldBe` 3
              dat V.! 0 `shouldBe` CP.Int32 42
              dat V.! 1 `shouldBe` CP.Bool True
              dat V.! 2 `shouldBe` CP.Float64 3.14
              V.length ptrs `shouldBe` 0
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "round-trip" $ do
          let p = Profile 7 False 2.71
          fromCapnProto (toCapnProto p) `shouldBe` Right p
      ]


newtypeTests :: Spec
newtypeTests =
  describe "newtype" $
    sequence_
      [ it "pass-through encodes to Int32" $
          toCapnProto (Tag 42) `shouldBe` CP.Int32 42
      , it "round-trip" $
          fromCapnProto (toCapnProto (Tag 7)) `shouldBe` Right (Tag 7)
      ]


coercedTests :: Spec
coercedTests =
  describe "coerced field" $
    sequence_
      [ it "newtype field encodes via inner Int32 repr" $ do
          let u = User (UserId 7) "alice"
          case toCapnProto u of
            CP.Struct dat ptrs -> do
              V.length dat `shouldBe` 1
              dat V.! 0 `shouldBe` CP.Int32 7
              V.length ptrs `shouldBe` 1
              ptrs V.! 0 `shouldBe` CP.Text "alice"
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "round-trip via coerce" $ do
          let u = User (UserId 99) "bob"
          fromCapnProto (toCapnProto u) `shouldBe` Right u
      ]


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red -> Enum 0" $ toCapnProto Red `shouldBe` CP.Enum 0
      , it "Green -> Enum 1" $ toCapnProto Green `shouldBe` CP.Enum 1
      , it "DarkBlue tag 42" $ toCapnProto DarkBlue `shouldBe` CP.Enum 42
      , it "round-trip" $
          mapM_ rt [Red, Green, DarkBlue]
      , it "unknown ordinal fails" $
          case fromCapnProto (CP.Enum 99) :: Either String Color of
            Left _ -> pure ()
            Right c -> expectationFailure ("unexpected " ++ show c)
      ]
  where
    rt :: Color -> IO ()
    rt c = fromCapnProto (toCapnProto c) `shouldBe` Right c


wireTests :: Spec
wireTests =
  describe "encode . decode" $
    sequence_
      [ it "matches the reference C++ encoding of a one-field UInt64 struct" $ do
          -- capnp encode Counter.capnp Counter, with (count = 42).
          let reference = BS.pack
                [ 0, 0, 0, 0, 2, 0, 0, 0
                , 0, 0, 0, 0, 1, 0, 0, 0
                , 42, 0, 0, 0, 0, 0, 0, 0
                ]
              counter = CP.Struct (V.singleton (CP.UInt64 42)) V.empty
          CPD.decode reference `shouldBe` Right counter
          CPE.encode counter `shouldBe` reference
      , it "keeps both pointer slots ahead of their payloads" $ do
          let blob = toCapnProto (Blob "x" (BS.pack [1, 2, 3]))
          CPD.decode (CPE.encode blob) `shouldBe` Right blob
      , it "distinguishes a zero-sized struct from a null pointer" $ do
          let empty = CP.Struct V.empty V.empty
          CPD.decode (CPE.encode empty) `shouldBe` Right empty
          CPD.decode (CPE.encode CP.Void) `shouldBe` Right CP.Void
      , it "Position decodes to Right" $
          case CPD.decode (CPE.encode (toCapnProto (Position "p" 1 2 (Just "n") "ignored"))) of
            Right _ -> pure ()
            Left e -> expectationFailure e
      , it "Profile decodes to Right" $
          case CPD.decode (CPE.encode (toCapnProto (Profile 9 True 0.5))) of
            Right _ -> pure ()
            Left e -> expectationFailure e
      ]
