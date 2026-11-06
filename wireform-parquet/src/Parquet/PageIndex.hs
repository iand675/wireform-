{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Apache Parquet page index: 'OffsetIndex' + 'ColumnIndex' (Thrift).

The page index tier (added in parquet-format 2.5) lets readers skip
entire pages without scanning column chunks. It is stored separately
from the row-group metadata, near the file footer:

* 'OffsetIndex' — per-page @(offset, compressed_page_size, first_row_index)@
  plus optional @unencoded_byte_array_data_bytes@ (size_statistics).
* 'ColumnIndex' — per-page @null_pages@, @min_values@, @max_values@,
  plus a 'BoundaryOrder' and optional null/level histograms.

'ColumnChunk' carries 'ccOffsetIndexOffset' / @offset_index_length@ and
'ccColumnIndexOffset' / @column_index_length@; this module reads/writes
the structures themselves with the Thrift Compact Protocol used by
"Parquet.Footer".
-}
module Parquet.PageIndex (
  -- * OffsetIndex
  encodeOffsetIndex,
  decodeOffsetIndex,
  readOffsetIndex,

  -- * ColumnIndex
  encodeColumnIndex,
  decodeColumnIndex,
  readColumnIndex,

  -- * Slicing
  columnChunkOffsetIndexSlice,
  columnChunkColumnIndexSlice,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text.Encoding qualified
import Data.Vector qualified as V
import Parquet.Encryption qualified as Encryption
import Parquet.Read (ParquetFile (..), decryptFileColumnModule, rawColumnChunkSlice)
import Parquet.Thrift.Schema
import Parquet.Types (
  ColumnChunk (..),
  ColumnIndex (..),
  ColumnMetadata (..),
  FileMetadata (..),
  OffsetIndex (..),
  PageLocation (..),
  RowGroup (..),
  boundaryOrderToInt,
  intToBoundaryOrder,
 )
import Thrift.Decode (decodeCompactBinaryStrings)
import Thrift.Encode (encodeCompact)
import Thrift.Value qualified as TV


-- ============================================================
-- OffsetIndex
-- ============================================================

-- | Serialize an 'OffsetIndex' in Thrift Compact Protocol.
encodeOffsetIndex :: OffsetIndex -> ByteString
encodeOffsetIndex = encodeCompact . offsetIndexToThrift


-- | Parse an 'OffsetIndex' from a Thrift Compact byte slice.
decodeOffsetIndex :: ByteString -> Either String OffsetIndex
decodeOffsetIndex bs = decodeCompactBinaryStrings bs >>= thriftToOffsetIndex


{- | Look up the 'OffsetIndex' for the column chunk at @(rowGroupIdx, columnIdx)@
if 'ccOffsetIndexOffset' / 'ccOffsetIndexLength' are populated and within
file bounds.
-}
readOffsetIndex
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe OffsetIndex)
readOffsetIndex pf rg col = do
  msl <- columnChunkOffsetIndexSlice pf rg col
  case msl of
    Nothing -> Right Nothing
    Just bs -> do
      index <- decodeOffsetIndex bs
      validateColumnOffsets pf rg col index
      Right (Just index)


{- | Slice a 'ParquetFile' to the bytes covering a column chunk's serialized
'OffsetIndex'. Returns 'Nothing' when the column chunk did not record one.
-}
columnChunkOffsetIndexSlice
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe ByteString)
columnChunkOffsetIndexSlice pf rgIdx colIdx = do
  cc <- selectColumnChunk pf rgIdx colIdx
  case (ccOffsetIndexOffset cc, ccOffsetIndexLength cc) of
    (Just off, Just len) -> do
      bytes <- sliceFile pf off len "offset index"
      Just <$> decryptFileColumnModule pf rgIdx colIdx Encryption.ModuleOffsetIndex bytes
    _ -> Right Nothing


-- ============================================================
-- ColumnIndex
-- ============================================================

-- | Serialize a 'ColumnIndex' in Thrift Compact Protocol.
encodeColumnIndex :: ColumnIndex -> ByteString
encodeColumnIndex = encodeCompact . columnIndexToThrift


-- | Parse a 'ColumnIndex' without interpreting opaque min/max bytes as text.
decodeColumnIndex :: ByteString -> Either String ColumnIndex
decodeColumnIndex bs = decodeCompactBinaryStrings bs >>= thriftToColumnIndex


{- | Look up the 'ColumnIndex' for the column chunk at @(rowGroupIdx,
columnIdx)@.
-}
readColumnIndex
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe ColumnIndex)
readColumnIndex pf rg col = do
  msl <- columnChunkColumnIndexSlice pf rg col
  case msl of
    Nothing -> Right Nothing
    Just bs -> Just <$> decodeColumnIndex bs


{- | Slice a 'ParquetFile' to the bytes covering a column chunk's serialized
'ColumnIndex'. Returns 'Nothing' when the column chunk did not record one.
-}
columnChunkColumnIndexSlice
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe ByteString)
columnChunkColumnIndexSlice pf rgIdx colIdx = do
  cc <- selectColumnChunk pf rgIdx colIdx
  case (ccColumnIndexOffset cc, ccColumnIndexLength cc) of
    (Just off, Just len) -> do
      bytes <- sliceFile pf off len "column index"
      Just <$> decryptFileColumnModule pf rgIdx colIdx Encryption.ModuleColumnIndex bytes
    _ -> Right Nothing


-- ============================================================
-- Internal: Thrift conversions
-- ============================================================

offsetIndexToThrift :: OffsetIndex -> TV.Value
offsetIndexToThrift oi =
  TV.Struct $
    V.fromList $
      concat
        [ [OffsetIndex_PageLocations (V.map pageLocationToThrift (oiPageLocations oi))]
        , optField
            (oiUnencodedByteArrayDataBytes oi)
            (OffsetIndex_UnencodedByteArrayDataBytes . V.map TV.I64)
        ]


pageLocationToThrift :: PageLocation -> TV.Value
pageLocationToThrift pl =
  TV.Struct $
    V.fromList
      [ PageLocation_Offset (plOffset pl)
      , PageLocation_CompressedPageSize (plCompressedPageSize pl)
      , PageLocation_FirstRowIndex (plFirstRowIndex pl)
      ]


thriftToOffsetIndex :: TV.Value -> Either String OffsetIndex
thriftToOffsetIndex (TV.Struct fields) = do
  let fm = V.toList fields
  pages <- case findField
    fm
    ( \case
        OffsetIndex_PageLocations xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM thriftToPageLocation vs
    Nothing -> Left "Parquet.PageIndex: missing page_locations"
  unencV <- case findField
    fm
    ( \case
        OffsetIndex_UnencodedByteArrayDataBytes xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> Just <$> V.mapM expectI64 vs
    Nothing -> Right Nothing
  Right
    OffsetIndex
      { oiPageLocations = pages
      , oiUnencodedByteArrayDataBytes = unencV
      }
thriftToOffsetIndex _ = Left "Parquet.PageIndex: expected struct for OffsetIndex"


thriftToPageLocation :: TV.Value -> Either String PageLocation
thriftToPageLocation (TV.Struct fields) = do
  let fm = V.toList fields
  off <- requireField fm "page_location.offset" $ \case
    PageLocation_Offset v -> Just v
    _ -> Nothing
  csz <- requireField fm "page_location.compressed_page_size" $ \case
    PageLocation_CompressedPageSize v -> Just v
    _ -> Nothing
  fri <- requireField fm "page_location.first_row_index" $ \case
    PageLocation_FirstRowIndex v -> Just v
    _ -> Nothing
  Right
    PageLocation
      { plOffset = off
      , plCompressedPageSize = csz
      , plFirstRowIndex = fri
      }
thriftToPageLocation _ =
  Left "Parquet.PageIndex: expected struct for PageLocation"


columnIndexToThrift :: ColumnIndex -> TV.Value
columnIndexToThrift ci =
  TV.Struct $
    V.fromList $
      concat
        [
          [ ColumnIndex_NullPages (V.map TV.Bool (ciNullPages ci))
          , ColumnIndex_MinValues (V.map TV.Binary (ciMinValues ci))
          , ColumnIndex_MaxValues (V.map TV.Binary (ciMaxValues ci))
          , ColumnIndex_BoundaryOrder (boundaryOrderToInt (ciBoundaryOrder ci))
          ]
        , optField
            (ciNullCounts ci)
            (ColumnIndex_NullCounts . V.map TV.I64)
        , optField
            (ciNanCounts ci)
            (ColumnIndex_NanCounts . V.map TV.I64)
        , optField
            (ciRepetitionLevelHistograms ci)
            (ColumnIndex_RepetitionLevelHistograms . V.map TV.I64)
        , optField
            (ciDefinitionLevelHistograms ci)
            (ColumnIndex_DefinitionLevelHistograms . V.map TV.I64)
        ]


thriftToColumnIndex :: TV.Value -> Either String ColumnIndex
thriftToColumnIndex (TV.Struct fields) = do
  let fm = V.toList fields
  nullPages <- case findField
    fm
    ( \case
        ColumnIndex_NullPages xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM expectBool vs
    Nothing -> Left "Parquet.PageIndex: missing null_pages"
  mins <- case findField
    fm
    ( \case
        ColumnIndex_MinValues xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM expectBinary vs
    Nothing -> Left "Parquet.PageIndex: missing min_values"
  maxs <- case findField
    fm
    ( \case
        ColumnIndex_MaxValues xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM expectBinary vs
    Nothing -> Left "Parquet.PageIndex: missing max_values"
  boInt <- requireField fm "boundary_order" $ \case
    ColumnIndex_BoundaryOrder v -> Just v
    _ -> Nothing
  bo <-
    maybe
      (Left $ "Parquet.PageIndex: invalid boundary_order " ++ show boInt)
      Right
      (intToBoundaryOrder boInt)
  ncv <- optListI64 fm $ \case
    ColumnIndex_NullCounts xs -> Just xs
    _ -> Nothing
  nanv <- optListI64 fm $ \case
    ColumnIndex_NanCounts xs -> Just xs
    _ -> Nothing
  repv <- optListI64 fm $ \case
    ColumnIndex_RepetitionLevelHistograms xs -> Just xs
    _ -> Nothing
  defv <- optListI64 fm $ \case
    ColumnIndex_DefinitionLevelHistograms xs -> Just xs
    _ -> Nothing
  let count = V.length nullPages
  if V.length mins /= count
    || V.length maxs /= count
    || maybe False ((/= count) . V.length) ncv
    || maybe False ((/= count) . V.length) nanv
    then Left "Parquet.PageIndex: per-page vector lengths differ"
    else Right ()
  if any (maybe False (V.any (< 0))) [ncv, nanv, repv, defv]
    then Left "Parquet.PageIndex: negative count or level frequency"
    else Right ()
  Right
    ColumnIndex
      { ciNullPages = nullPages
      , ciMinValues = mins
      , ciMaxValues = maxs
      , ciBoundaryOrder = bo
      , ciNullCounts = ncv
      , ciNanCounts = nanv
      , ciRepetitionLevelHistograms = repv
      , ciDefinitionLevelHistograms = defv
      }
thriftToColumnIndex _ =
  Left "Parquet.PageIndex: expected struct for ColumnIndex"


optListI64
  :: [(Int16, TV.Value)]
  -> ((Int16, TV.Value) -> Maybe (V.Vector TV.Value))
  -> Either String (Maybe (V.Vector Int64))
optListI64 fm probe = case findField fm probe of
  Just vs -> Just <$> V.mapM expectI64 vs
  Nothing -> Right Nothing


-- ============================================================
-- Helpers
-- ============================================================

selectColumnChunk
  :: ParquetFile -> Int -> Int -> Either String ColumnChunk
selectColumnChunk pf rgIdx colIdx = do
  let rgs = fmRowGroups (pfFooter pf)
  if rgIdx < 0 || rgIdx >= V.length rgs
    then Left "Parquet.PageIndex: row group index out of range"
    else do
      let cs = rgColumns (V.unsafeIndex rgs rgIdx)
      if colIdx < 0 || colIdx >= V.length cs
        then Left "Parquet.PageIndex: column index out of range"
        else do
          let chunk = V.unsafeIndex cs colIdx
          case ccFilePath chunk of
            Just path | path /= mempty -> Left "Parquet.PageIndex: external column-chunk files are not supported"
            _ -> Right chunk


-- Check the complete index, including entries the predicate will skip.
-- Data-page bytes are deliberately left untouched until a page is retained.
validateColumnOffsets :: ParquetFile -> Int -> Int -> OffsetIndex -> Either String ()
validateColumnOffsets pf groupIndex columnIndex index = do
  chunk <- selectColumnChunk pf groupIndex columnIndex
  metadata <- maybe (Left "Parquet.PageIndex: missing column metadata") Right (ccMetadata chunk)
  bytes <- rawColumnChunkSlice pf groupIndex columnIndex
  let group = fmRowGroups (pfFooter pf) V.! groupIndex
      rows = rgNumRows group
      locations = oiPageLocations index
      start = maybe (cmDataPageOffset metadata) (min (cmDataPageOffset metadata)) (cmDictionaryPageOffset metadata)
      size = if cmNumValues metadata == 0 then cmTotalCompressedSize metadata else fromIntegral (BS.length bytes)
      fileSize = fromIntegral (BS.length (pfBytes pf)) :: Int64
  if rows < 0 || start < 0 || start > fileSize || size < 0 || size > fileSize - start
    then Left "Parquet.PageIndex: invalid column bounds"
    else Right ()
  if V.null locations && rows /= 0
    then Left "Parquet.PageIndex: nonempty group has no indexed pages"
    else Right ()
  V.imapM_
    ( \i location -> do
        let offset = plOffset location
            pageSize = fromIntegral (plCompressedPageSize location) :: Int64
            row = plFirstRowIndex location
        if offset < start
          || offset > start + size
          || pageSize <= 0
          || pageSize > start + size - offset
          || row < 0
          || row >= rows && not (rows == 0 && i == 0)
          || i == 0 && (row /= 0 || offset /= cmDataPageOffset metadata)
          then Left "Parquet.PageIndex: page location lies outside its column/row range"
          else Right ()
        if i == 0
          then Right ()
          else do
            let previous = locations V.! (i - 1)
            if plOffset previous + fromIntegral (plCompressedPageSize previous) > offset
              || plFirstRowIndex previous >= row
              then Left "Parquet.PageIndex: unordered or overlapping page locations"
              else Right ()
    )
    locations
  case oiUnencodedByteArrayDataBytes index of
    Just sizes
      | V.length sizes /= V.length locations || V.any (< 0) sizes ->
          Left "Parquet.PageIndex: invalid per-page unencoded byte counts"
    _ -> Right ()


sliceFile
  :: ParquetFile
  -> Int64
  -> Int32
  -> String
  -> Either String ByteString
sliceFile pf off len what =
  let !o = fromIntegral off :: Int
      !l = fromIntegral len :: Int
      !bs = pfBytes pf
  in if o < 0 || l < 0 || o + l > BS.length bs
       then Left $ "Parquet.PageIndex: " ++ what ++ " slice out of bounds"
       else Right $! BS.take l (BS.drop o bs)


requireField
  :: [(Int16, TV.Value)]
  -> String
  -> ((Int16, TV.Value) -> Maybe a)
  -> Either String a
requireField fm name probe = case findField fm probe of
  Just v -> Right v
  Nothing -> Left $ "Parquet.PageIndex: missing or invalid field " ++ name


expectI64 :: TV.Value -> Either String Int64
expectI64 (TV.I64 v) = Right v
expectI64 v =
  Left $
    "Parquet.PageIndex: expected i64 but got " ++ show v


expectBool :: TV.Value -> Either String Bool
expectBool (TV.Bool b) = Right b
expectBool v =
  Left $
    "Parquet.PageIndex: expected bool but got " ++ show v


-- Thrift Compact transports binary and UTF-8 strings under the same
-- @TT_STRING@ wire type, so the decoder may surface either shape.
expectBinary :: TV.Value -> Either String ByteString
expectBinary (TV.Binary b) = Right b
expectBinary (TV.String t) =
  Right $! Data.Text.Encoding.encodeUtf8 t
expectBinary v =
  Left $
    "Parquet.PageIndex: expected binary but got " ++ show v
