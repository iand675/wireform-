{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.Partition (tests) where

import Avro.Value qualified as AV
import Data.Text (Text)
import Data.Vector qualified as V
import Iceberg.Expression qualified as E
import Iceberg.JSON qualified as J
import Iceberg.Partition
import Iceberg.Types
import Test.Syd


mkSF :: Int -> Text -> IcebergType -> StructField
mkSF i n t = StructField i n True t Nothing Nothing Nothing


schema :: Schema
schema =
  Schema
    { schemaId = 0
    , schemaFields =
        V.fromList
          [ mkSF 1 "id" TLong
          , mkSF 2 "name" TString
          ]
    , schemaIdentifierFieldIds = V.empty
    }


specBucket8 :: PartitionSpec
specBucket8 =
  PartitionSpec
    0
    ( V.singleton
        ( PartitionField
            { pfSourceIds = V.singleton 1
            , pfFieldId = 1000
            , pfName = "id_bucket"
            , pfTransform = Bucket 8
            }
        )
    )


specIdentity :: PartitionSpec
specIdentity =
  PartitionSpec
    0
    ( V.singleton
        ( PartitionField
            { pfSourceIds = V.singleton 1
            , pfFieldId = 1000
            , pfName = "id_part"
            , pfTransform = Identity
            }
        )
    )


tests :: Spec
tests =
  describe "Iceberg.Partition" $
    sequence_
      [ it "buildPartition Identity" $ do
          let lookupSrc 1 = Just (AV.Long 42)
              lookupSrc _ = Nothing
          case buildPartition specIdentity schema lookupSrc of
            Right (PartitionTuple t) -> V.toList t `shouldBe` [Just (AV.Long 42)]
            Left e -> expectationFailure (show e)
      , it "buildPartition Bucket(8) of long 42" $ do
          let lookupSrc 1 = Just (AV.Long 42)
              lookupSrc _ = Nothing
          case buildPartition specBucket8 schema lookupSrc of
            Right (PartitionTuple t) -> case V.toList t of
              [Just (AV.Int n)] -> (n >= 0 && n < 8) `shouldBe` True
              other -> expectationFailure ("unexpected partition: " ++ show other)
            Left e -> expectationFailure (show e)
      , it "inclusiveProject identity rewrites field name" $ do
          let expr = E.equal "id" (E.LLong 42)
              projected = inclusiveProject schema specIdentity expr
          case projected of
            E.EAnd _ _ -> pure () -- folded with True; either form is OK
            E.EPredicate p -> E.predField p `shouldBe` "id_part"
            other -> expectationFailure ("expected projected predicate, got: " ++ show other)
      , it "inclusiveProject Bucket(8) projects equal predicate" $ do
          let expr = E.equal "id" (E.LLong 42)
              projected = inclusiveProject schema specBucket8 expr
          case projected of
            E.EAnd _ (E.EPredicate p) -> E.predField p `shouldBe` "id_bucket"
            E.EPredicate p -> E.predField p `shouldBe` "id_bucket"
            other -> expectationFailure ("expected bucket predicate, got: " ++ show other)
      , it "keeps possible matches for negated equality with no identity partition" $ do
          let expr = E.not_ (E.equal "id" (E.LLong 42))
          inclusiveProject schema (PartitionSpec 0 V.empty) expr `shouldBe` E.true
          inclusiveProject schema specBucket8 expr `shouldBe` E.true
      , it "pushes negation through conjunctions and disjunctions before projection" $ do
          let idIsNull = E.isNull "id"
              nameIsNull = E.isNull "name"
          inclusiveProject schema specIdentity (E.not_ (E.and_ idIsNull nameIsNull)) `shouldBe` E.true
          inclusiveProject schema specIdentity (E.not_ (E.or_ idIsNull nameIsNull)) `shouldBe` E.notNull "id_part"
          inclusiveProject schema specIdentity (E.ENot (E.ENot idIsNull)) `shouldBe` E.isNull "id_part"
      , it "does not constrain source nullability through void or unknown transforms" $ do
          let specWith transform = specIdentity {psFields = V.map (\pf -> pf {pfTransform = transform}) (psFields specIdentity)}
          inclusiveProject schema (specWith Void) (E.notNull "id") `shouldBe` E.true
          inclusiveProject schema (specWith (UnknownTransform "future")) (E.isNull "id") `shouldBe` E.true
      , it "computes and projects a bucket partition on time" $ do
          let clockSchema = schema {schemaFields = V.singleton (mkSF 1 "clock" TTime)}
              bucket = V.map (\pf -> pf {pfTransform = Bucket 16}) (psFields specBucket8)
              spec = specBucket8 {psFields = bucket}
          buildPartition spec clockSchema (const (Just (AV.Long 34))) `shouldBe` Right (PartitionTuple (V.singleton (Just (AV.Int 3))))
          inclusiveProject clockSchema spec (E.equal "clock" (E.LLong 34)) `shouldBe` E.equal "id_bucket" (E.LInt 3)
      , -- V3 multi-arg transforms: a partition field with two source columns.
        it "V3: PartitionField round-trips through JSON with source-ids" $ do
          let pf =
                PartitionField
                  { pfSourceIds = V.fromList [1, 2]
                  , pfFieldId = 1000
                  , pfName = "joint_bucket"
                  , pfTransform = Bucket 16
                  }
              j = J.partitionFieldToJSON pf
          case J.partitionFieldFromJSON j of
            Right pf' -> pf' `shouldBe` pf
            Left e -> expectationFailure ("V3 multi-source-ids round-trip: " ++ e)
      , it "V2: PartitionField round-trips through JSON with source-id" $ do
          let pf =
                PartitionField
                  { pfSourceIds = V.singleton 1
                  , pfFieldId = 1000
                  , pfName = "id_bucket"
                  , pfTransform = Bucket 16
                  }
              j = J.partitionFieldToJSON pf
          case J.partitionFieldFromJSON j of
            Right pf' -> pf' `shouldBe` pf
            Left e -> expectationFailure ("V2 single source-id round-trip: " ++ e)
      ]
