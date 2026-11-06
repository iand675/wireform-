{-# LANGUAGE OverloadedStrings #-}

module SchemaBoundaryRegression (tests) where

import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Vector qualified as V
import Parquet.Arrow qualified as Arrow
import Parquet.Footer qualified as Footer
import Parquet.HighLevel qualified as High
import Parquet.Levels qualified as Levels
import Parquet.Read qualified as Read
import Parquet.Types qualified as P


check :: String -> Bool -> IO ()
check name ok = unless ok (fail ("Schema boundary regression: " ++ name))


node :: Text -> Maybe P.ParquetType -> Maybe P.Repetition -> Maybe Int -> P.SchemaElement
node name typ repetition count =
  P.SchemaElement
    { P.seName = name
    , P.seType = typ
    , P.seRepetition = repetition
    , P.seNumChildren = fromIntegral <$> count
    , P.seConvertedType = Nothing
    , P.seLogicalType = Nothing
    , P.seFieldId = Nothing
    , P.seTypeLength = Nothing
    }


root :: P.SchemaElement
root = node "schema" Nothing Nothing (Just 1)


leaf :: P.SchemaElement
leaf = node "x" (Just P.PTInt32) (Just P.Required) Nothing


tests :: IO ()
tests = do
  let group = node "g" Nothing (Just P.Optional) (Just 1)
      depth n = V.singleton root <> V.replicate n (node "g" Nothing (Just P.Required) (Just 1)) <> V.singleton leaf
      reject name schema path levelFailure = do
        let metadata = P.FileMetadata 1 schema 0 V.empty Nothing Nothing Nothing
            bytes = "PAR1" <> Footer.writeFooter metadata
            input = Read.ParquetFile bytes metadata
        check (name ++ " checked schema") (isLeft (Arrow.parquetFileArrowSchemaChecked input))
        check (name ++ " public file decode") (isLeft (High.decodeParquet High.defaultReadOptions bytes))
        if levelFailure then check (name ++ " selected path") (isLeft (Levels.maxLevelsForColumnPath schema (V.fromList path))) else pure ()
  reject "missing leaf repetition" (V.fromList [root, leaf {P.seRepetition = Nothing}]) ["x"] True
  reject "missing group repetition" (V.fromList [root, group {P.seRepetition = Nothing}, leaf]) ["g", "x"] True
  reject "missing nested leaf repetition" (V.fromList [root, group, leaf {P.seRepetition = Nothing}]) ["g", "x"] True
  forM_ [0, 1, -1] $ \count -> reject "primitive carrying num_children" (V.fromList [root, leaf {P.seNumChildren = Just count}]) ["x"] True
  reject "missing root count" (V.singleton root {P.seNumChildren = Nothing}) ["x"] True
  reject "negative root count" (V.fromList [root {P.seNumChildren = Just (-1)}, leaf]) ["x"] True
  reject "oversized root count" (V.fromList [root {P.seNumChildren = Just maxBound}, leaf]) ["x"] True
  reject "missing group count" (V.fromList [root, group {P.seNumChildren = Nothing}, leaf]) ["g", "x"] True
  reject "negative group count" (V.fromList [root, group {P.seNumChildren = Just (-1)}, leaf]) ["g", "x"] True
  reject "primitive root" (V.singleton leaf) ["x"] True
  reject "trailing physical nodes" (V.fromList [root, leaf, leaf {P.seName = "extra"}]) ["x"] False
  reject "malformed skipped sibling" (V.fromList [root {P.seNumChildren = Just 2}, leaf {P.seName = "bad", P.seRepetition = Nothing}, leaf]) ["x"] True
  reject "oversized selected depth" (depth 129) (replicate 129 "g" ++ ["x"]) False
  let skipped = V.singleton root {P.seNumChildren = Just 2} <> V.replicate 129 (node "g" Nothing (Just P.Required) (Just 1)) <> V.fromList [leaf {P.seName = "hidden"}, leaf]
  reject "oversized skipped depth" skipped ["x"] False
  -- Root markers occur in common producers but describe no parent relation.
  -- They must never add levels or make an otherwise valid root unreadable.
  forM_ [Nothing, Just P.Required, Just P.Optional, Just P.Repeated] $ \marker -> do
    let schema = V.fromList [root {P.seRepetition = marker}, group, leaf {P.seRepetition = Just P.Repeated}]
        metadata = P.FileMetadata 1 schema 0 V.empty Nothing Nothing Nothing
        bytes = "PAR1" <> Footer.writeFooter metadata
    check "valid root-marker compatibility" (isRight (High.decodeParquet High.defaultReadOptions bytes))
    check "root marker does not alter levels" (Levels.maxLevelsForColumnPath schema (V.fromList ["g", "x"]) == Right (1, 2))
  forM_ [0, 1, 127, 128] $ \n -> do
    let schema = depth n
        metadata = P.FileMetadata 1 schema 0 V.empty Nothing Nothing Nothing
        bytes = "PAR1" <> Footer.writeFooter metadata
    check "inclusive selected depth limit" (isRight (High.decodeParquet High.defaultReadOptions bytes))
    check "inclusive level-lookup depth limit" (Levels.maxLevelsForColumnPath schema (V.fromList (replicate n "g" ++ ["x"])) == Right (0, 0))
  let deep = depth 5000
      deepPath = V.fromList (replicate 5000 "g" ++ ["x"])
      deepSkipped = V.singleton root {P.seNumChildren = Just 2} <> V.replicate 5000 (node "g" Nothing (Just P.Required) (Just 1)) <> V.fromList [leaf {P.seName = "hidden"}, leaf]
  check "low-level selected lookup preserves deep valid schemas" (Levels.maxLevelsForColumnPath deep deepPath == Right (0, 0))
  check "skipped subtree scan is iterative at deep valid schemas" (Levels.maxLevelsForColumnPath deepSkipped (V.singleton "x") == Right (0, 0))
  check "empty input schema" (isLeft (Levels.maxLevelsForColumnPath V.empty (V.singleton "x")))
  check "empty path" (isLeft (Levels.maxLevelsForColumnPath (V.fromList [root, leaf]) V.empty))
  check "empty file bytes remain rejected" (isLeft (High.decodeParquet High.defaultReadOptions BS.empty))
