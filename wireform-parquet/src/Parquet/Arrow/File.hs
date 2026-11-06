{-# LANGUAGE OverloadedStrings #-}

{- | Compatibility with arrow-rs's experimental FILE extension. The extension
name is not yet a standardized canonical Arrow extension.
-}
module Parquet.Arrow.File (
  isFileField,
  fileSchemaFromField,
  fileMetadata,
  fieldIdMetadata,
) where

import Arrow.Types qualified as A
import Control.Monad (unless)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Parquet.File qualified as F


isFileField :: A.Field -> Bool
isFileField field = V.elem ("ARROW:extension:name", "arrow.parquet.file") (A.fieldMetadata field)


fileMetadata :: V.Vector (Text, Text)
fileMetadata = V.fromList [("ARROW:extension:name", "arrow.parquet.file"), ("ARROW:extension:metadata", "")]


fieldIdMetadata :: Maybe Int32 -> V.Vector (Text, Text)
fieldIdMetadata = maybe V.empty (V.singleton . ("PARQUET:field_id",) . TL.toStrict . TB.toLazyText . TBI.decimal)


fileSchemaFromField :: A.Field -> Either String F.FileSchema
fileSchemaFromField field = do
  let names = values "ARROW:extension:name" field
      parameters = values "ARROW:extension:metadata" field
  unless
    (names == V.singleton "arrow.parquet.file" && V.length parameters <= 1 && V.all T.null parameters)
    (Left "Parquet.Arrow: FILE extension expects a unique name and absent/empty metadata")
  unless
    (A.fieldType field == A.AStruct && A.fieldDictionary field == Nothing)
    (Left "Parquet.Arrow: FILE extension requires struct storage")
  ident <- fieldId field
  children <- V.mapM child (A.fieldChildren field)
  F.withFileSchemaFieldId ident <$> F.mkFileSchema children
  where
    child value = do
      kind <- case A.fieldName value of
        "uri" -> Right F.FileUri
        "offset" -> Right F.FileOffset
        "size" -> Right F.FileSize
        "content_type" -> Right F.FileContentType
        "checksum" -> Right F.FileChecksumField
        "inline" -> Right F.FileInline
        _ -> Left "Parquet.Arrow: unrecognized FILE field name"
      let ty = A.fieldType value
          matches = case kind of
            F.FileOffset -> ty == A.AInt 64 True
            F.FileSize -> ty == A.AInt 64 True
            F.FileInline -> ty `elem` [A.ABinary, A.ALargeBinary, A.ABinaryView]
            _ -> ty `elem` [A.AUtf8, A.ALargeUtf8, A.AUtf8View]
      unless
        (matches && A.fieldNullable value && V.null (A.fieldChildren value) && A.fieldDictionary value == Nothing)
        (Left "Parquet.Arrow: FILE children must be nullable primitives with their specified types")
      F.FileFieldSpec kind <$> fieldId value


values :: Text -> A.Field -> V.Vector Text
values key = V.map snd . V.filter ((== key) . fst) . A.fieldMetadata


fieldId :: A.Field -> Either String (Maybe Int32)
fieldId field = case V.toList (values "PARQUET:field_id" field) of
  [] -> Right Nothing
  [encoded] -> case TR.signed TR.decimal encoded :: Either String (Integer, Text) of
    Right (number, remainder)
      | T.null remainder && number >= fromIntegral (minBound :: Int32) && number <= fromIntegral (maxBound :: Int32) -> Right (Just (fromInteger number))
    _ -> Left "Parquet.Arrow: FILE field ID must fit a signed 32-bit integer"
  _ -> Left "Parquet.Arrow: duplicate FILE field ID metadata"
