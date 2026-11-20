{-# LANGUAGE OverloadedStrings #-}

module GeospatialRegression (tests) where

import Arrow.Types qualified as A
import Control.Monad (forM_, unless)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Parquet.Arrow qualified as Arrow
import Parquet.Read qualified as Read
import Parquet.Types qualified as P


fieldFor :: P.LogicalType -> Maybe (V.Vector P.KeyValue) -> Either String A.Field
fieldFor logical entries = do
  let root = P.SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
      leaf = P.SchemaElement "g" (Just P.Optional) (Just P.PTByteArray) Nothing Nothing (Just logical) Nothing Nothing
      footer = P.FileMetadata 1 (V.fromList [root, leaf]) 0 V.empty Nothing Nothing entries
  schema <- Arrow.parquetFileArrowSchemaChecked (Read.ParquetFile BS.empty footer)
  Right (V.head (A.arrowFields schema))


parameters :: A.Field -> Either String J.Object
parameters field = case V.find ((== "ARROW:extension:metadata") . fst) (A.fieldMetadata field) of
  Nothing -> Left "missing extension parameters"
  Just (_, encoded) -> case J.eitherDecodeStrict' (TE.encodeUtf8 encoded) of
    Right (J.Object fields) -> Right fields
    _ -> Left "extension parameters are not a JSON object"


logicalFor :: A.Field -> Either String (Maybe P.LogicalType)
logicalFor field = do
  let schema = A.Schema (V.singleton field) A.Little V.empty V.empty
  (elements, _) <- Arrow.arrowToParquetMixed schema []
  Right (P.seLogicalType (elements V.! 1))


geoField :: Text -> A.Field
geoField encoded =
  A.Field
    "g"
    True
    A.ABinary
    V.empty
    Nothing
    (V.fromList [("ARROW:extension:name", "geoarrow.wkb"), ("ARROW:extension:metadata", encoded)])


tests :: IO ()
tests = do
  let check label condition = unless condition (fail ("geospatial metadata: " ++ label))
      load result = either fail pure result
      crs = Just "EPSG:4326"
  planar <- load (fieldFor (P.LTGeometry P.defaultGeometryParameters) Nothing)
  object <- load (parameters planar)
  check
    "default CRS becomes explicit known CRS in GeoArrow"
    (KM.lookup "crs" object == Just (J.String "OGC:CRS84"))
  check "planar edges are omitted" (KM.lookup "edges" object == Nothing)
  check
    "canonical WKB extension retains binary storage"
    (A.fieldType planar == A.ABinary && V.elem ("ARROW:extension:name", "geoarrow.wkb") (A.fieldMetadata planar))
  forM_ [(P.Spherical, "spherical"), (P.Vincenty, "vincenty"), (P.Thomas, "thomas"), (P.Andoyer, "andoyer"), (P.Karney, "karney")] $ \(algorithm, name) -> do
    let expected = P.LTGeography (P.GeographyParameters crs (Just algorithm))
    field <- load (fieldFor expected Nothing)
    fields <- load (parameters field)
    check "all five edge algorithms keep their names" (KM.lookup "edges" fields == Just (J.String name))
    check "known geography metadata survives both directions" (logicalFor field == Right (Just expected))
  spherical <- load (fieldFor (P.LTGeography P.defaultGeographyParameters) Nothing >>= parameters)
  check "absent geography algorithm has spherical semantics" (KM.lookup "edges" spherical == Just (J.String "spherical"))
  let definition = "{\"type\":\"GeographicCRS\",\"name\":\"WGS 84\",\"datum\":{\"type\":\"GeodeticReferenceFrame\",\"name\":\"World Geodetic System 1984\",\"ellipsoid\":{\"name\":\"WGS 84\",\"semi_major_axis\":6378137,\"inverse_flattening\":298.257223563}},\"coordinate_system\":{\"subtype\":\"ellipsoidal\",\"axis\":[{\"name\":\"Longitude\",\"abbreviation\":\"lon\",\"direction\":\"east\",\"unit\":\"degree\"},{\"name\":\"Latitude\",\"abbreviation\":\"lat\",\"direction\":\"north\",\"unit\":\"degree\"}]}}"
      annotation = P.LTGeometry (P.GeometryParameters (Just "projjson:crs"))
      entry = P.KeyValue "crs" (Just (TE.encodeUtf8 definition))
  reference <- load (fieldFor annotation (Just (V.singleton entry)) >>= parameters)
  inline <- load (fieldFor (P.LTGeometry (P.GeometryParameters (Just definition))) Nothing >>= parameters)
  check "referenced PROJJSON resolves without an escaped JSON string" (KM.lookup "crs" reference == KM.lookup "crs" inline && case KM.lookup "crs" reference of Just J.Object {} -> True; _ -> False)
  forM_ [Nothing, Just V.empty, Just (V.singleton (P.KeyValue "crs" Nothing)), Just (V.fromList [entry, entry]), Just (V.singleton (P.KeyValue "crs" (Just "[]"))), Just (V.singleton (P.KeyValue "crs" (Just (BS.singleton 255))))] $ \entries ->
    check "bad PROJJSON references do not acquire guessed CRS" (isLeft (fieldFor annotation entries))
  srid <- load (fieldFor (P.LTGeometry (P.GeometryParameters (Just "srid:42"))) Nothing)
  sridFields <- load (parameters srid)
  check "SRID representation is disambiguated" (KM.lookup "crs" sridFields == Just (J.String "42") && KM.lookup "crs_type" sridFields == Just (J.String "srid"))
  check "SRID prefix is restored when writing" (logicalFor srid == Right (Just (P.LTGeometry (P.GeometryParameters (Just "srid:42")))))
  future <- load (fieldFor (P.LTGeography (P.GeographyParameters Nothing (Just (P.UnrecognizedInterpolation 99)))) Nothing)
  check "future algorithm stays physical binary without planar metadata" (A.fieldType future == A.ABinary && V.null (A.fieldMetadata future))
  forM_ ["{}", "{\"crs\":null}", "{\"crs\":\"\"}", "{\"crs\":\"EPSG:4326\",\"edges\":\"rhumb\"}", "{\"crs\":\"EPSG:4326\",\"crs_type\":\"future\"}", "[]", "{\"crs\":\"projjson:external\"}"] $ \encoded ->
    check "unsupported or unknown metadata fails instead of changing semantics" (isLeft (logicalFor (geoField encoded)))
  let known = geoField "{\"crs\":\"EPSG:3857\"}"
  check "large-binary WKB is supported" (logicalFor known {A.fieldType = A.ALargeBinary} == Right (Just (P.LTGeometry (P.GeometryParameters (Just "EPSG:3857")))))
  check "WKB annotation cannot be put on UTF-8 storage" (isLeft (logicalFor known {A.fieldType = A.AUtf8}))
  check "duplicate extension keys are not resolved arbitrarily" (isLeft (logicalFor known {A.fieldMetadata = V.snoc (A.fieldMetadata known) ("ARROW:extension:name", "arrow.json")}))
  putStrLn "GeoArrow metadata regression cases passed."
