{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Ion.Class
import Ion.Decode qualified as Decode
import Ion.Derive (deriveIon)
import Ion.Encode qualified as Encode
import Ion.Value qualified as I


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


deriveIon ''Person


small :: Person
small = Person "Alice" 30 "alice@example.com"


medium :: Vector Person
medium = V.generate 100 $ \index ->
  let i = index + 1
  in Person
       ("user-" <> number i)
       (20 + i `mod` 50)
       ("user" <> number i <> "@example.com")


number :: Int -> Text
number = TL.toStrict . TB.toLazyText . TBI.decimal


symbolHeavy :: I.Value
symbolHeavy = I.List $ V.generate 100 $ \index ->
  I.Annotation
    "event"
    ( I.Struct
        ( V.fromList
            [ ("name", I.String ("user-" <> number (index + 1)))
            , ("kind", I.Symbol "created")
            , ("tags", I.List (V.fromList [I.Symbol "created", I.Symbol "updated", I.Symbol "created"]))
            , ("unicode", I.Symbol "雪")
            ]
        )
    )


main :: IO ()
main =
  defaultMain
    [ env checked $ \ ~(one, many, symbols) ->
        bgroup
          "codec encode"
          [ bench "Person" $ nf Encode.encode one
          , bench "[Person] x 100" $ nf Encode.encode many
          , bench "symbols x 100" $ nf Encode.encode symbols
          ]
    , env (pure (Encode.encode (toIon small), Encode.encode (toIon medium), Encode.encode symbolHeavy)) $ \ ~(one, many, symbols) ->
        bgroup
          "codec decode"
          [ bench "Person" $ nf Decode.decode one
          , bench "[Person] x 100" $ nf Decode.decode many
          , bench "symbols x 100" $ nf Decode.decode symbols
          ]
    , bgroup
        "encode"
        [ bench "Person" $ nf encodeIon small
        , bench "[Person] x 100" $ nf encodeIon medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeIon small)) $ \bs ->
            bench "Person" $ nf (decodeIon :: ByteString -> Either String Person) bs
        , env (pure (encodeIon medium)) $ \bs ->
            bench "[Person] x 100" $ nf (decodeIon :: ByteString -> Either String (Vector Person)) bs
        ]
    ]


checked :: IO (I.Value, I.Value, I.Value)
checked = do
  let one = toIon small
      many = toIon medium
  unless
    (one == I.Struct (V.fromList [("personName", I.String "Alice"), ("personAge", I.Int 30), ("personEmail", I.String "alice@example.com")]))
    (fail "Ion derived benchmark record differs from the Rust fixture")
  mapM_
    ( \value ->
        unless
          (Decode.decode (Encode.encode value) == Right value)
          (fail "Ion benchmark fixture does not round-trip")
    )
    [one, many, symbolHeavy]
  pure (one, many, symbolHeavy)
