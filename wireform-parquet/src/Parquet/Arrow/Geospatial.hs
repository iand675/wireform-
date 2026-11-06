{-# LANGUAGE OverloadedStrings #-}

-- | Translate geospatial semantics without changing the WKB storage bytes.
module Parquet.Arrow.Geospatial (toArrowMetadata, fromArrowField) where

import Arrow.Types qualified as A
import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Parquet.Types qualified as P


data CRSMetadata = CRSMetadata
  { crsValue :: !J.Value
  , crsHint :: !(Maybe Text)
  }


encodeJSON :: J.Value -> Text
encodeJSON = TE.decodeUtf8 . BL.toStrict . J.encode


objectJSON :: Text -> Either String J.Object
objectJSON value = case J.eitherDecodeStrict' (TE.encodeUtf8 value) of
  Right (J.Object object) -> Right object
  _ -> Left "Parquet.Arrow: CRS definition must be a JSON object"


resolveCRS :: P.FileMetadata -> Maybe Text -> Either String CRSMetadata
resolveCRS _ Nothing = Right (CRSMetadata (J.String "OGC:CRS84") (Just "authority_code"))
resolveCRS footer (Just raw)
  | Just name <- T.stripPrefix "projjson:" raw = do
      if T.null name then Left "Parquet.Arrow: empty PROJJSON metadata reference" else Right ()
      let entries = V.filter ((== TE.encodeUtf8 name) . P.kvKey) (maybe V.empty id (P.fmKeyValueMetadata footer))
      value <- case V.toList entries of
        [P.KeyValue _ (Just bytes)] -> either (const (Left "Parquet.Arrow: PROJJSON metadata is not UTF-8")) Right (TE.decodeUtf8' bytes)
        _ -> Left "Parquet.Arrow: missing, absent-valued or ambiguous PROJJSON metadata reference"
      object <- objectJSON value
      Right (CRSMetadata (J.Object object) (Just "projjson"))
  | Just identifier <- T.stripPrefix "srid:" raw =
      if T.null identifier then Left "Parquet.Arrow: empty SRID" else Right (CRSMetadata (J.String identifier) (Just "srid"))
  | T.null raw = Left "Parquet.Arrow: empty CRS does not identify a coordinate reference system"
  | otherwise = case J.eitherDecodeStrict' (TE.encodeUtf8 raw) of
      Right (J.Object object) -> Right (CRSMetadata (J.Object object) (Just "projjson"))
      _ -> Right (CRSMetadata (J.String raw) Nothing)


edgeName :: P.EdgeInterpolationAlgorithm -> Maybe Text
edgeName P.Spherical = Just "spherical"
edgeName P.Vincenty = Just "vincenty"
edgeName P.Thomas = Just "thomas"
edgeName P.Andoyer = Just "andoyer"
edgeName P.Karney = Just "karney"
edgeName P.UnrecognizedInterpolation {} = Nothing


parseEdge :: Text -> Either String P.EdgeInterpolationAlgorithm
parseEdge "spherical" = Right P.Spherical
parseEdge "vincenty" = Right P.Vincenty
parseEdge "thomas" = Right P.Thomas
parseEdge "andoyer" = Right P.Andoyer
parseEdge "karney" = Right P.Karney
parseEdge _ = Left "Parquet.Arrow: unsupported GeoArrow edge interpolation"


toArrowMetadata :: P.FileMetadata -> P.SchemaElement -> Either String (V.Vector (Text, Text))
toArrowMetadata footer element
  | P.seType element /= Just P.PTByteArray = Right V.empty
  | otherwise = case P.seLogicalType element of
      Just (P.LTGeometry parameters) -> build (P.geometryCrs parameters) Nothing
      Just (P.LTGeography parameters) -> case edgeName (maybe P.Spherical id (P.geographyAlgorithm parameters)) of
        -- A future algorithm cannot be labelled as planar or spherical WKB.
        Nothing -> Right V.empty
        Just edges -> build (P.geographyCrs parameters) (Just edges)
      _ -> Right V.empty
  where
    build :: Maybe Text -> Maybe Text -> Either String (V.Vector (Text, Text))
    build raw edges = do
      crs <- resolveCRS footer raw
      let fields =
            ["crs" .= crsValue crs]
              ++ maybe [] (\hint -> ["crs_type" .= hint]) (crsHint crs)
              ++ maybe [] (\value -> ["edges" .= value]) edges
      Right (V.fromList [("ARROW:extension:name", "geoarrow.wkb"), ("ARROW:extension:metadata", encodeJSON (J.Object (KM.fromList fields)))])


reservedField :: Text -> A.Field -> Either String (Maybe Text)
reservedField name field = case V.toList (V.filter ((== name) . fst) (A.fieldMetadata field)) of
  [] -> Right Nothing
  [(_, value)] -> Right (Just value)
  _ -> Left "Parquet.Arrow: duplicate Arrow extension metadata key"


optionalText :: Key -> J.Object -> Either String (Maybe Text)
optionalText name object = case KM.lookup name object of
  Nothing -> Right Nothing
  Just (J.String value) -> Right (Just value)
  _ -> Left "Parquet.Arrow: GeoArrow metadata field must be a string"


fromArrowField :: A.Field -> Either String (Maybe P.LogicalType)
fromArrowField field
  | not (V.any (\(key, value) -> key == "ARROW:extension:name" && "geoarrow." `T.isPrefixOf` value) (A.fieldMetadata field)) = Right Nothing
  | otherwise = do
      extension <- reservedField "ARROW:extension:name" field
      case extension of
        Just "geoarrow.wkb" -> convert
        Just name | "geoarrow." `T.isPrefixOf` name -> Left "Parquet.Arrow: only WKB GeoArrow storage can be written as Parquet geospatial data"
        _ -> Right Nothing
  where
    convert = do
      if A.fieldType field `elem` [A.ABinary, A.ALargeBinary]
        then Right ()
        else Left "Parquet.Arrow: GeoArrow WKB requires binary storage"
      encoded <- reservedField "ARROW:extension:metadata" field
      object <- case encoded of
        Nothing -> Right KM.empty
        Just value -> case J.eitherDecodeStrict' (TE.encodeUtf8 value) of
          Right (J.Object fields) -> Right fields
          _ -> Left "Parquet.Arrow: GeoArrow metadata must be a JSON object"
      hint <- optionalText "crs_type" object
      crs <- case KM.lookup "crs" object of
        Just (J.Object fields) | hint `elem` [Nothing, Just "projjson"] -> Right (encodeJSON (J.Object fields))
        Just (J.String value) | not (T.null value) -> case hint of
          Just "srid" -> Right ("srid:" <> value)
          Nothing -> literal value
          Just "authority_code" -> literal value
          Just "wkt2:2019" -> literal value
          _ -> Left "Parquet.Arrow: unsupported GeoArrow CRS representation"
        _ -> Left "Parquet.Arrow: unknown GeoArrow CRS cannot be replaced by Parquet's OGC:CRS84 default"
      edges <- optionalText "edges" object
      case edges of
        Nothing -> Right (Just (P.LTGeometry (P.GeometryParameters (Just crs))))
        Just value -> do
          algorithm <- parseEdge value
          Right (Just (P.LTGeography (P.GeographyParameters (Just crs) (Just algorithm))))
    literal value
      | "projjson:" `T.isPrefixOf` value = Left "Parquet.Arrow: GeoArrow CRS cannot reference missing file metadata"
      | otherwise = Right value
