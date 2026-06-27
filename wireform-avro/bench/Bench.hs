{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Microbench for wireform-avro encode + decode hot paths.

Avro is schema-driven: the wire codec takes an 'AvroType' alongside
the value, so we resolve the schema once (via the derived
'HasAvroSchema' instance) and reuse it across iterations, the same
way a real producer/consumer would.
-}
module Main (main) where

import Avro.Class (FromAvro (..), ToAvro (..))
import Avro.Decode (decodeAvro)
import Avro.Derive (avroSchema, deriveAvro)
import Avro.Encode (encodeAvro)
import Avro.Schema (AvroType)
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


$(deriveAvro ''Person)


personSchema :: AvroType
personSchema = avroSchema (Proxy :: Proxy Person)


{- | Schema for an Avro array of Person (HasAvroSchema [a] gives the
array wrapper; the Vector value encodes against the same shape).
-}
batchSchema :: AvroType
batchSchema = avroSchema (Proxy :: Proxy [Person])


small :: Person
small = Person "Alice" 30 "alice@example.com"


medium :: Vector Person
medium =
  V.fromList
    [ Person
      (T.pack ("user-" <> show i))
      (20 + i `mod` 50)
      (T.pack ("user" <> show i <> "@example.com"))
    | i <- [1 .. 100 :: Int]
    ]


encodeOne :: Person -> ByteString
encodeOne = encodeAvro personSchema . toAvro


encodeBatch :: Vector Person -> ByteString
encodeBatch = encodeAvro batchSchema . toAvro


decodeOne :: ByteString -> Either String Person
decodeOne bs = decodeAvro personSchema bs >>= fromAvro


decodeBatch :: ByteString -> Either String (Vector Person)
decodeBatch bs = decodeAvro batchSchema bs >>= fromAvro


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Person" $ nf encodeOne small
        , bench "[Person] x 100" $ nf encodeBatch medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeOne small)) $ \bs ->
            bench "Person" $ nf decodeOne bs
        , env (pure (encodeBatch medium)) $ \bs ->
            bench "[Person] x 100" $ nf decodeBatch bs
        ]
    ]
