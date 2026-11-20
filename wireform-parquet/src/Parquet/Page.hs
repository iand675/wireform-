{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Parquet page headers (Thrift compact protocol).

See @parquet.thrift@ (@PageHeader@, @DataPageHeader@, @DictionaryPageHeader@,
@DataPageHeaderV2@). Thrift field placement is mediated by the
pattern synonyms in "Parquet.Thrift.Schema".
-}
module Parquet.Page (
  PageHeader (..),
  PageType (..),
  DataPageHeader (..),
  DictionaryPageHeader (..),
  DataPageHeaderV2 (..),
  readPageHeaderAt,
  readPageHeaderCheckedAt,
  PageDetails (..),
  readPageDetailsAt,
  readPageDetailsCheckedAt,
  pageTypeTag,
  pageTypeIsDataPageV1,
  pageTypeIsDataPageV2,
  pageTypeIsDictionary,
  verifyPageChecksum,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64)
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import Foreign.C.Types (CUInt (..), CULong (..))
import Foreign.Ptr (Ptr, castPtr)
import Parquet.Thrift.Schema
import System.IO.Unsafe (unsafeDupablePerformIO)
import Thrift.Decode (decodeCompactFrom)
import Thrift.Value qualified as TV


{- | Discriminated union over Parquet page types. Mirrors the
@PageType@ enum in @parquet.thrift@ (DATA_PAGE=0, INDEX_PAGE=1,
DICTIONARY_PAGE=2, DATA_PAGE_V2=3) but each constructor carries the
nested struct that the spec couples with that variant — there is no
meaningful "DATA_PAGE without DataPageHeader" value on the wire, so
we don't represent one.
-}
data PageType
  = PtDataPage !DataPageHeader
  | PtIndexPage
  | PtDictionaryPage !DictionaryPageHeader
  | PtDataPageV2 !DataPageHeaderV2
  deriving stock (Show, Eq)


-- | The on-the-wire @PageType@ tag (Thrift @PageHeader.type@, field 1).
pageTypeTag :: PageType -> Int32
pageTypeTag = \case
  PtDataPage {} -> 0
  PtIndexPage -> 1
  PtDictionaryPage {} -> 2
  PtDataPageV2 {} -> 3


pageTypeIsDataPageV1 :: PageType -> Bool
pageTypeIsDataPageV1 PtDataPage {} = True
pageTypeIsDataPageV1 _ = False


pageTypeIsDataPageV2 :: PageType -> Bool
pageTypeIsDataPageV2 PtDataPageV2 {} = True
pageTypeIsDataPageV2 _ = False


pageTypeIsDictionary :: PageType -> Bool
pageTypeIsDictionary PtDictionaryPage {} = True
pageTypeIsDictionary _ = False


data PageHeader = PageHeader
  { phType :: !PageType
  , phUncompressedPageSize :: !(Maybe Int32)
  , phCompressedPageSize :: !(Maybe Int32)
  }
  deriving stock (Show, Eq)


-- | Nested @DataPageHeader@ (field 5 of @PageHeader@).
data DataPageHeader = DataPageHeader
  { dphNumValues :: !Int32
  , dphEncoding :: !Int32
  }
  deriving stock (Show, Eq)


-- | Nested @DictionaryPageHeader@ (field 7 of @PageHeader@).
data DictionaryPageHeader = DictionaryPageHeader
  { dictNumValues :: !Int32
  , dictEncoding :: !Int32
  }
  deriving stock (Show, Eq)


{- | Nested @DataPageHeaderV2@ (field 8 of @PageHeader@).
Parquet Thrift field IDs 1–7 of the @DataPageHeaderV2@ struct.
-}
data DataPageHeaderV2 = DataPageHeaderV2
  { dph2NumValues :: !Int32
  , dph2NumNulls :: !Int32
  , dph2NumRows :: !Int32
  , dph2Encoding :: !Int32
  , dph2DefLevelsLen :: !Int32
  , dph2RepLevelsLen :: !Int32
  , dph2IsCompressed :: !Bool
  }
  deriving stock (Show, Eq)


readPageHeaderAt :: ByteString -> Int -> Either String (PageHeader, Int)
readPageHeaderAt bs off = do
  details <- readPageDetailsAt bs off
  Right (pdHeader details, pdBodyOffset details)


data PageDetails = PageDetails
  { pdHeader :: !PageHeader
  , pdBodyOffset :: {-# UNPACK #-} !Int
  , pdDefinitionEncoding :: {-# UNPACK #-} !Int32
  , pdRepetitionEncoding :: {-# UNPACK #-} !Int32
  , pdChecksum :: !(Maybe Word32)
  }


readPageDetailsAt :: ByteString -> Int -> Either String PageDetails
readPageDetailsAt bs off
  | off < 0 || off >= BS.length bs = Left "Parquet.Page: header offset out of bounds"
  | otherwise = do
      (v, endOff) <- decodeCompactFrom bs off
      ph <- pageHeaderFromThrift v
      validatePageHeader ph
      checksum <- case v of
        TV.Struct fs ->
          V.foldM
            ( \seen field -> case field of
                PageHeader_Crc n -> case seen of
                  Nothing -> Right (Just (fromIntegral n))
                  Just _ -> Left "Parquet.Page: duplicate CRC field"
                (4, _) -> Left "Parquet.Page: CRC must be an i32"
                _ -> Right seen
            )
            Nothing
            fs
        _ -> Right Nothing
      let fields = case v of TV.Struct fs -> V.toList fs; _ -> []
          inner = maybe [] V.toList $ findField fields $ \case
            PageHeader_DataPageHeader fs -> Just fs
            _ -> Nothing
          definition = maybe 3 id $ findField inner $ \case
            DataPageHeader_DefinitionLevelEncoding n -> Just n
            _ -> Nothing
          repetition = maybe 3 id $ findField inner $ \case
            DataPageHeader_RepetitionLevelEncoding n -> Just n
            _ -> Nothing
      pure (PageDetails ph endOff definition repetition checksum)


{- | Read a complete page header and check the serialized (compressed) body.
Header-only callers, including encryption framing, use 'readPageHeaderAt'.
Every data reader uses this checked variant before decoding or decompressing.
-}
readPageHeaderCheckedAt :: ByteString -> Int -> Either String (PageHeader, Int)
readPageHeaderCheckedAt bs off = do
  details <- readPageDetailsCheckedAt bs off
  Right (pdHeader details, pdBodyOffset details)


readPageDetailsCheckedAt :: ByteString -> Int -> Either String PageDetails
readPageDetailsCheckedAt bs off = do
  details <- readPageDetailsAt bs off
  let body = pdBodyOffset details
  case phCompressedPageSize (pdHeader details) of
    Just size
      | fromIntegral size <= BS.length bs - body -> case pdChecksum details of
          Nothing -> Right details
          Just expected
            | pageCrc32 (BS.take (fromIntegral size) (BS.drop body bs)) == expected -> Right details
            | otherwise -> Left "Parquet.Page: CRC32 checksum mismatch"
    _ -> Left "Parquet.Page: truncated page body"


-- zlib's uInt length is safe here: the validated page size is a nonnegative
-- signed 32-bit integer, bounded below uInt's maximum on supported platforms.
-- The foreign call is deterministic and only reads the immutable bytes.
-- Only validated Parquet page bodies reach this worker, so their signed
-- 32-bit size fits zlib's unsigned-int byte count without narrowing overflow.
pageCrc32 :: ByteString -> Word32
pageCrc32 bytes = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bytes $ \(ptr, len) ->
    fromIntegral <$> c_crc32 0 (castPtr ptr) (fromIntegral len)
{-# NOINLINE pageCrc32 #-}


-- | Check the CRC of an on-disk page body, including encryption framing.
-- Enforce Parquet's signed 32-bit page-size bound before calling zlib.
verifyPageChecksum :: Word32 -> ByteString -> Either String ()
verifyPageChecksum expected bytes
  | BS.length bytes > fromIntegral (maxBound :: Int32) = Left "Parquet.Page: page body exceeds signed 32-bit size"
  | pageCrc32 bytes == expected = Right ()
  | otherwise = Left "Parquet.Page: CRC32 checksum mismatch"


foreign import ccall unsafe "crc32"
  c_crc32 :: CULong -> Ptr Word8 -> CUInt -> IO CULong


validatePageHeader :: PageHeader -> Either String ()
validatePageHeader header = do
  uncompressed <- size "uncompressed_page_size" (phUncompressedPageSize header)
  compressed <- size "compressed_page_size" (phCompressedPageSize header)
  case phType header of
    PtDataPage page
      | dphNumValues page < 0 -> Left "Parquet.Page: negative data-page value count"
    PtDictionaryPage page
      | dictNumValues page < 0 -> Left "Parquet.Page: negative dictionary value count"
    PtDataPageV2 page
      | dph2NumValues page < 0
          || dph2NumNulls page < 0
          || dph2NumRows page < 0
          || dph2DefLevelsLen page < 0
          || dph2RepLevelsLen page < 0 ->
          Left "Parquet.Page: negative V2 count or level length"
      | dph2NumNulls page > dph2NumValues page || dph2NumRows page > dph2NumValues page ->
          Left "Parquet.Page: inconsistent V2 row/null/value counts"
      | let levels = fromIntegral (dph2DefLevelsLen page) + fromIntegral (dph2RepLevelsLen page) :: Int64
      , levels > fromIntegral uncompressed || levels > fromIntegral compressed ->
          Left "Parquet.Page: V2 level lengths exceed page sizes"
    _ -> Right ()
  where
    size name = \case
      Just n | n >= 0 -> Right n
      _ -> Left ("Parquet.Page: missing or negative " ++ name)


pageHeaderFromThrift :: TV.Value -> Either String PageHeader
pageHeaderFromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  ty <- requireI32 fm "PageHeader.type" $ \case
    PageHeader_Type v -> Just v
    _ -> Nothing
  let unc = findField fm $ \case
        PageHeader_UncompressedSize v -> Just v
        _ -> Nothing
      comp = findField fm $ \case
        PageHeader_CompressedSize v -> Just v
        _ -> Nothing
  pageType <- case ty of
    0 -> case findField
      fm
      ( \case
          PageHeader_DataPageHeader fs -> Just fs
          _ -> Nothing
      ) of
      Just fs -> PtDataPage <$> dataPageHeaderFromThrift (TV.Struct fs)
      Nothing ->
        Left
          "Parquet.Page: DATA_PAGE without DataPageHeader (field 5)"
    1 -> Right PtIndexPage
    2 -> case findField
      fm
      ( \case
          PageHeader_DictionaryPageHeader fs -> Just fs
          _ -> Nothing
      ) of
      Just fs -> PtDictionaryPage <$> dictionaryPageHeaderFromThrift (TV.Struct fs)
      Nothing ->
        Left
          "Parquet.Page: DICTIONARY_PAGE without DictionaryPageHeader (field 7)"
    3 -> case findField
      fm
      ( \case
          PageHeader_DataPageHeaderV2 fs -> Just fs
          _ -> Nothing
      ) of
      Just fs -> PtDataPageV2 <$> dataPageHeaderV2FromThrift (TV.Struct fs)
      Nothing ->
        Left
          "Parquet.Page: DATA_PAGE_V2 without DataPageHeaderV2 (field 8)"
    _ -> Left ("Parquet.Page: unknown PageType tag " ++ show ty)
  pure
    PageHeader
      { phType = pageType
      , phUncompressedPageSize = unc
      , phCompressedPageSize = comp
      }
pageHeaderFromThrift _ = Left "Parquet.Page: expected PageHeader struct"


dataPageHeaderFromThrift :: TV.Value -> Either String DataPageHeader
dataPageHeaderFromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  n <- requireI32 fm "DataPageHeader.num_values" $ \case
    DataPageHeader_NumValues v -> Just v
    _ -> Nothing
  -- encoding defaults to 0 (PLAIN) when absent, per historic writers.
  let enc = maybe 0 id $ findField fm $ \case
        DataPageHeader_Encoding v -> Just v
        _ -> Nothing
  pure DataPageHeader {dphNumValues = n, dphEncoding = enc}
dataPageHeaderFromThrift _ = Left "Parquet.Page: expected DataPageHeader struct"


dictionaryPageHeaderFromThrift :: TV.Value -> Either String DictionaryPageHeader
dictionaryPageHeaderFromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  n <- requireI32 fm "DictionaryPageHeader.num_values" $ \case
    DictionaryPageHeader_NumValues v -> Just v
    _ -> Nothing
  let enc = maybe 0 id $ findField fm $ \case
        DictionaryPageHeader_Encoding v -> Just v
        _ -> Nothing
  pure DictionaryPageHeader {dictNumValues = n, dictEncoding = enc}
dictionaryPageHeaderFromThrift _ = Left "Parquet.Page: expected DictionaryPageHeader struct"


dataPageHeaderV2FromThrift :: TV.Value -> Either String DataPageHeaderV2
dataPageHeaderV2FromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  nv <- requireI32 fm "DataPageHeaderV2.num_values" $ \case
    DataPageHeaderV2_NumValues v -> Just v
    _ -> Nothing
  nn <- requireI32 fm "DataPageHeaderV2.num_nulls" $ \case
    DataPageHeaderV2_NumNulls v -> Just v
    _ -> Nothing
  nr <- requireI32 fm "DataPageHeaderV2.num_rows" $ \case
    DataPageHeaderV2_NumRows v -> Just v
    _ -> Nothing
  enc <- requireI32 fm "DataPageHeaderV2.encoding" $ \case
    DataPageHeaderV2_Encoding v -> Just v
    _ -> Nothing
  dl <- requireI32 fm "DataPageHeaderV2.definition_levels_byte_length" $ \case
    DataPageHeaderV2_DefinitionLevelsByteLength v -> Just v
    _ -> Nothing
  rl <- requireI32 fm "DataPageHeaderV2.repetition_levels_byte_length" $ \case
    DataPageHeaderV2_RepetitionLevelsByteLength v -> Just v
    _ -> Nothing
  -- is_compressed is optional; spec default is true.
  let isComp = maybe True id $ findField fm $ \case
        DataPageHeaderV2_IsCompressed b -> Just b
        _ -> Nothing
  pure
    DataPageHeaderV2
      { dph2NumValues = nv
      , dph2NumNulls = nn
      , dph2NumRows = nr
      , dph2Encoding = enc
      , dph2DefLevelsLen = dl
      , dph2RepLevelsLen = rl
      , dph2IsCompressed = isComp
      }
dataPageHeaderV2FromThrift _ = Left "Parquet.Page: expected DataPageHeaderV2 struct"


{- | Specialised @require@ for @Int32@ fields, producing a
@Parquet.Page@-flavoured error message.
-}
requireI32
  :: [(Int16, TV.Value)]
  -> String
  -> ((Int16, TV.Value) -> Maybe Int32)
  -> Either String Int32
requireI32 fm name probe = case findField fm probe of
  Just v -> Right v
  Nothing -> Left $ "Parquet.Page: missing or invalid field " ++ name
