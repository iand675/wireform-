{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Criterion benchmark: wireform vs proto-lens (real generated code).

Wireform side uses @Proto.CodeGen@ output (@Proto.Bench.Wireform.Messages@)
including the sorted-tag @inOrderStage@ decode fast path. Regenerate that
module after editing @bench/compare/gen-wireform/Messages.proto@:

> cabal run regen-compare-bench-wireform

Run: cabal bench compare-bench
-}
module Main where

import Criterion.Main
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.ProtoLens (defMessage)
import Data.ProtoLens qualified as PLC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Lens.Family2 ((&), (.~))
import Proto.Bench qualified as PL
import Proto.Bench.Wireform.Messages qualified as W
import Proto.Bench_Fields qualified as F
import Proto qualified as H
import Proto qualified as H


encSmallH :: W.Small -> BS.ByteString
encSmallH = H.encodeMessage
{-# NOINLINE encSmallH #-}


encMediumH :: W.Medium -> BS.ByteString
encMediumH = H.encodeMessage
{-# NOINLINE encMediumH #-}


encNestedH :: W.WithNested -> BS.ByteString
encNestedH = H.encodeMessage
{-# NOINLINE encNestedH #-}


encRepH :: W.WithRepeated -> BS.ByteString
encRepH = H.encodeMessage
{-# NOINLINE encRepH #-}


main :: IO ()
main =
  defaultMain
    [ bgroup
        "Small"
        [ bgroup
            "encode"
            [ bench "wireform" $ nf encSmallH smallHS
            , bench "proto-lens" $ nf PLC.encodeMessage smallPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decSmallH smallBytes
            , bench "proto-lens" $ nf decSmallP smallBytes
            ]
        , bgroup
            "roundtrip"
            [ bench "wireform" $ nf rtSmallH smallHS
            , bench "proto-lens" $ nf rtSmallP smallPL
            ]
        ]
    , bgroup
        "Medium"
        [ bgroup
            "encode"
            [ bench "wireform" $ nf encMediumH mediumHS
            , bench "proto-lens" $ nf PLC.encodeMessage mediumPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decMediumH mediumBytes
            , bench "proto-lens" $ nf decMediumP mediumBytes
            ]
        , bgroup
            "roundtrip"
            [ bench "wireform" $ nf rtMediumH mediumHS
            , bench "proto-lens" $ nf rtMediumP mediumPL
            ]
        ]
    , bgroup
        "Nested"
        [ bgroup
            "encode"
            [ bench "wireform" $ nf encNestedH nestedHS
            , bench "proto-lens" $ nf PLC.encodeMessage nestedPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decNestedH nestedBytes
            , bench "proto-lens" $ nf decNestedP nestedBytes
            ]
        , bgroup
            "roundtrip"
            [ bench "wireform" $ nf rtNestedH nestedHS
            , bench "proto-lens" $ nf rtNestedP nestedPL
            ]
        ]
    , bgroup
        "Repeated"
        [ bgroup
            "encode"
            [ bench "wireform" $ nf encRepH repeatedHS
            , bench "proto-lens" $ nf PLC.encodeMessage repeatedPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decRepH repeatedBytes
            , bench "proto-lens" $ nf decRepP repeatedBytes
            ]
        ]
    ]


decSmallH :: BS.ByteString -> Either H.DecodeError W.Small
decSmallH = H.decodeMessage
{-# NOINLINE decSmallH #-}


decSmallP :: BS.ByteString -> Either String PL.Small
decSmallP = PLC.decodeMessage
{-# NOINLINE decSmallP #-}


rtSmallH :: W.Small -> Either H.DecodeError W.Small
rtSmallH m = H.decodeMessage (H.encodeMessage m)
{-# NOINLINE rtSmallH #-}


rtSmallP :: PL.Small -> Either String PL.Small
rtSmallP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtSmallP #-}


decMediumH :: BS.ByteString -> Either H.DecodeError W.Medium
decMediumH = H.decodeMessage
{-# NOINLINE decMediumH #-}


decMediumP :: BS.ByteString -> Either String PL.Medium
decMediumP = PLC.decodeMessage
{-# NOINLINE decMediumP #-}


rtMediumH :: W.Medium -> Either H.DecodeError W.Medium
rtMediumH m = H.decodeMessage (H.encodeMessage m)
{-# NOINLINE rtMediumH #-}


rtMediumP :: PL.Medium -> Either String PL.Medium
rtMediumP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtMediumP #-}


decNestedH :: BS.ByteString -> Either H.DecodeError W.WithNested
decNestedH = H.decodeMessage
{-# NOINLINE decNestedH #-}


decNestedP :: BS.ByteString -> Either String PL.WithNested
decNestedP = PLC.decodeMessage
{-# NOINLINE decNestedP #-}


rtNestedH :: W.WithNested -> Either H.DecodeError W.WithNested
rtNestedH m = H.decodeMessage (H.encodeMessage m)
{-# NOINLINE rtNestedH #-}


rtNestedP :: PL.WithNested -> Either String PL.WithNested
rtNestedP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtNestedP #-}


decRepH :: BS.ByteString -> Either H.DecodeError W.WithRepeated
decRepH = H.decodeMessage
{-# NOINLINE decRepH #-}


decRepP :: BS.ByteString -> Either String PL.WithRepeated
decRepP = PLC.decodeMessage
{-# NOINLINE decRepP #-}


-- wireform test values (PrefixedFields codegen; keep in sync with proto-lens fixtures)

smallHS :: W.Small
smallHS =
  W.Small
    { W.smallId = 42
    , W.smallName = "hello world"
    , W.smallActive = True
    , W.smallUnknownFields = []
    }


mediumHS :: W.Medium
mediumHS =
  W.Medium
    { W.mediumTitle = "benchmark title"
    , W.mediumCount = 100
    , W.mediumScore = 3.14159
    , W.mediumPayload = "payload\x00\x01\x02"
    , W.mediumEnabled = True
    , W.mediumTimestamp = 1708000000
    , W.mediumDescription = "a medium description"
    , W.mediumRatio = 0.75
    , W.mediumUnknownFields = []
    }


nestedHS :: W.WithNested
nestedHS =
  W.WithNested
    { W.withNestedId = 99
    , W.withNestedInner =
        Just
          ( W.Small
              { W.smallId = 1
              , W.smallName = "inner"
              , W.smallActive = True
              , W.smallUnknownFields = []
              }
          )
    , W.withNestedLabel = "outer label"
    , W.withNestedUnknownFields = []
    }


repeatedHS :: W.WithRepeated
repeatedHS =
  W.WithRepeated
    { W.withRepeatedValues = VU.fromList [1 .. 50]
    , W.withRepeatedTags = V.fromList (fmap (\i -> "tag_" <> T.pack (show i)) [1 .. 20 :: Int])
    , W.withRepeatedItems =
        V.fromList
          [ W.Small
              { W.smallId = fromIntegral i
              , W.smallName = "item" <> T.pack (show i)
              , W.smallActive = even i
              , W.smallUnknownFields = []
              }
          | i <- [1 .. 10 :: Int]
          ]
    , W.withRepeatedUnknownFields = []
    }


-- proto-lens test values (using the real generated field lenses)

smallPL :: PL.Small
smallPL =
  (defMessage :: PL.Small)
    & F.id .~ (42 :: Int64)
    & F.name .~ ("hello world" :: Text)
    & F.active .~ True


mediumPL :: PL.Medium
mediumPL =
  (defMessage :: PL.Medium)
    & F.title .~ ("benchmark title" :: Text)
    & F.count .~ (100 :: Int32)
    & F.score .~ (3.14159 :: Double)
    & F.payload .~ ("payload\x00\x01\x02" :: BS.ByteString)
    & F.enabled .~ True
    & F.timestamp .~ (1708000000 :: Int64)
    & F.description .~ ("a medium description" :: Text)
    & F.ratio .~ (0.75 :: Float)


nestedPL :: PL.WithNested
nestedPL =
  let inner =
        (defMessage :: PL.Small)
          & F.id .~ (1 :: Int64)
          & F.name .~ ("inner" :: Text)
          & F.active .~ True
  in (defMessage :: PL.WithNested)
      & F.id .~ (99 :: Int64)
      & F.inner .~ inner
      & F.label .~ ("outer label" :: Text)


repeatedPL :: PL.WithRepeated
repeatedPL =
  let mkItem :: Int -> PL.Small
      mkItem i =
        (defMessage :: PL.Small)
          & F.id .~ (fromIntegral i :: Int64)
          & F.name .~ (("item" <> T.pack (show i)) :: Text)
          & F.active .~ (even i :: Bool)
  in (defMessage :: PL.WithRepeated)
      & F.vec'values .~ VU.fromList ([1 .. 50] :: [Int32])
      & F.vec'tags .~ V.fromList (fmap (\i -> ("tag_" <> T.pack (show i)) :: Text) [1 .. 20 :: Int])
      & F.vec'items .~ V.fromList (fmap mkItem [1 .. 10])


-- Pre-encoded bytes (wireform canonical encoding for the fixtures above)
smallBytes, mediumBytes, nestedBytes, repeatedBytes :: BS.ByteString
smallBytes = encSmallH smallHS
mediumBytes = encMediumH mediumHS
nestedBytes = encNestedH nestedHS
repeatedBytes = encRepH repeatedHS
