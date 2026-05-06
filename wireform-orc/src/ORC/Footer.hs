{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Read/write Apache ORC file footer.
--
-- ORC file layout ends with:
--   [protobuf Footer] [protobuf PostScript] [1-byte postscript length]
--
-- The PostScript contains: footerLength, compression, compressionBlockSize,
-- version, and the magic string "ORC".
--
-- All @(fieldNum, wireType)@ tags are named once in "ORC.Proto.Schema";
-- this module only references them by pattern synonym so writer + reader
-- can't drift.
module ORC.Footer
  ( readORCFooter
  , writeORCFooter
  , readORCCompression
  , readORCCompressionBlockSize
  , orcMagic
    -- * ColumnStatistics codec
  , encodeColStats
  , decodeColStats
  ) where

import qualified Data.Bits as Bits
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import GHC.Float (castWord64ToDouble)

import ORC.Compress (decompressORCStreamSized)
import ORC.Proto.Schema
import ORC.Types

orcMagic :: ByteString
orcMagic = "ORC"

readORCFooter :: ByteString -> Either String ORCFooter
readORCFooter bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          let !magic = psMagic ps
              -- The PostScript's @magic@ field is optional in
              -- the spec (most modern writers, including the
              -- C++/pyarrow stack, omit it because the file's
              -- leading "ORC" header is canonical). Accept both
              -- @"ORC"@ and an empty string; reject anything
              -- else.
              !magicOk = BS.null magic || magic == orcMagic
          if not magicOk
            then Left $ "ORC.Footer: invalid magic (expected ORC, got " ++ show magic ++ ")"
            else do
              let !footerLen = fromIntegral (psFooterLength ps) :: Int
                  !footerStart = psStart - footerLen
              if footerLen <= 0 || footerStart < 0
                then Left "ORC.Footer: invalid footer length"
                else do
                  let !rawFooter = BSU.unsafeTake footerLen
                                     (BSU.unsafeDrop footerStart bs)
                  -- ORC's spec wraps the footer in the file's
                  -- compression envelope when @compression@ is
                  -- non-NONE. Decompress here so callers don't
                  -- have to.
                  ck <- maybe
                         (Left "ORC.Footer: unknown compression in postscript")
                         Right
                         (compressionFromInt (psCompression ps))
                  let !blk = max 1 (fromIntegral (psCompressionBlockSize ps) :: Int)
                  footerBytes <- decompressORCStreamSized blk ck rawFooter
                  decodeFooter footerBytes

-- | Extract the compression kind from the PostScript.
readORCCompression :: ByteString -> Either String CompressionKind
readORCCompression bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short for compression read"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          case compressionFromInt (psCompression ps) of
            Just ck -> Right ck
            Nothing -> Left $ "ORC.Footer: unknown compression kind " ++ show (psCompression ps)

-- | Extract the compressionBlockSize from the PostScript. This
-- is the maximum uncompressed length of any compression chunk
-- in the file (per ORC spec, 'ORCv1/index.html#postscript').
-- Decompressors that need to allocate an output buffer ahead
-- of time (LZ4, LZO) read this; chunks always decompress to
-- /at most/ this many bytes. Default in upstream writers is
-- 256 KiB; legal range is technically unbounded but values
-- above a few MiB are vanishingly rare.
readORCCompressionBlockSize :: ByteString -> Either String Int
readORCCompressionBlockSize bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short for compression-block-size read"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          Right (fromIntegral (psCompressionBlockSize ps))

writeORCFooter :: ORCFooter -> ByteString
writeORCFooter footer =
  let !footerBytes = encodeFooter footer
      !footerLen = BS.length footerBytes
      !psBytes = encodePostScript PostScript
        { psFooterLength = fromIntegral footerLen
        , psCompression = 0
        , psCompressionBlockSize = 262144
        , psVersion = V.fromList [0, 12]
        , psMagic = orcMagic
        }
      !psLen = BS.length psBytes
  in BL.toStrict $ B.toLazyByteString $
       B.byteString footerBytes
       <> B.byteString psBytes
       <> B.word8 (fromIntegral psLen)

-- Internal PostScript type
data PostScript = PostScript
  { psFooterLength         :: !Word64
  , psCompression          :: !Word64
  , psCompressionBlockSize :: !Word64
  , psVersion              :: !(Vector Word32)
  , psMagic                :: !ByteString
  }

------------------------------------------------------------------------
-- Protobuf encoding (manual, matching ORC .proto field numbers)
------------------------------------------------------------------------

encodeFooter :: ORCFooter -> ByteString
encodeFooter f = BL.toStrict $ B.toLazyByteString $ mconcat
  [ encodeVarintField Footer_HeaderLength  (orcHeaderLength f)
  , encodeVarintField Footer_ContentLength (orcContentLength f)
  , V.foldl' (\acc s -> acc <> encodeLengthDelim Footer_Stripes
                                 (encodeStripeInfo s))
      mempty (orcStripes f)
  , V.foldl' (\acc t -> acc <> encodeLengthDelim Footer_Types
                                 (encodeORCType t))
      mempty (orcTypes f)
  , V.foldl' (\acc (k, v) -> acc <> encodeLengthDelim Footer_Metadata
                                    (encodeMetadataEntry k v))
      mempty (orcMetadata f)
  , encodeVarintField Footer_NumberOfRows (orcNumberOfRows f)
  , V.foldl' (\acc s -> acc <> encodeLengthDelim Footer_Statistics
                                 (encodeColStats s))
      mempty (orcStatistics f)
  , case orcEncryption f of
      Nothing -> mempty
      Just (FooterEncryption encBs) ->
        encodeLengthDelim Footer_Encryption (B.byteString encBs)
  ]

encodeStripeInfo :: StripeInformation -> B.Builder
encodeStripeInfo si = mconcat
  [ encodeVarintField StripeInformation_Offset        (siOffset si)
  , encodeVarintField StripeInformation_IndexLength   (siIndexLength si)
  , encodeVarintField StripeInformation_DataLength    (siDataLength si)
  , encodeVarintField StripeInformation_FooterLength  (siFooterLength si)
  , encodeVarintField StripeInformation_NumberOfRows  (siNumberOfRows si)
  ]

encodeORCType :: ORCType -> B.Builder
encodeORCType ot = mconcat
  [ encodeVarintField ORCType_Kind
      (fromIntegral (typeKindToInt (otKind ot)) :: Word64)
  , V.foldl' (\acc st -> acc <> encodeVarintField ORCType_Subtypes
                                   (fromIntegral st :: Word64))
      mempty (otSubtypes ot)
  , V.foldl' (\acc fn -> acc <> encodeLengthDelim ORCType_FieldNames
                                   (B.byteString (TE.encodeUtf8 fn)))
      mempty (otFieldNames ot)
  ]

encodeMetadataEntry :: T.Text -> ByteString -> B.Builder
encodeMetadataEntry k v = mconcat
  [ encodeLengthDelim MetadataEntry_Name
      (B.byteString (TE.encodeUtf8 k))
  , encodeLengthDelim MetadataEntry_Value (B.byteString v)
  ]

encodeColStats :: ColumnStatistics -> B.Builder
encodeColStats cs = mconcat
  [ maybe mempty (encodeVarintField ColumnStatistics_NumberOfValues)
      (csNumberOfValues cs)
  , maybe mempty
      (\b -> encodeVarintField ColumnStatistics_HasNull
               (if b then 1 else 0 :: Word64))
      (csHasNull cs)
  , maybe mempty (encodeVarintField ColumnStatistics_BytesOnDisk)
      (csBytesOnDisk cs)
  , maybe mempty encodeStatsKind (csKind cs)
  ]

encodeStatsKind :: StatsKind -> B.Builder
encodeStatsKind = \case
  SkInt s    -> encodeLengthDelim ColumnStatistics_IntStatistics
                  (encodeIntStats s)
  SkDouble s -> encodeLengthDelim ColumnStatistics_DoubleStatistics
                  (encodeDoubleStats s)
  SkString s -> encodeLengthDelim ColumnStatistics_StringStatistics
                  (encodeStringStats s)
  SkBucket s -> encodeLengthDelim ColumnStatistics_BucketStatistics
                  (encodeBucketStats s)
  SkDecimal s -> encodeLengthDelim ColumnStatistics_DecimalStatistics
                  (encodeDecimalStats s)
  SkDate s -> encodeLengthDelim ColumnStatistics_DateStatistics
                  (encodeDateStats s)
  SkBinary s -> encodeLengthDelim ColumnStatistics_BinaryStatistics
                  (encodeBinaryStats s)
  SkTimestamp s -> encodeLengthDelim ColumnStatistics_TimestampStatistics
                  (encodeTimestampStats s)

encodeIntStats :: IntegerStatistics -> B.Builder
encodeIntStats s = mconcat
  [ maybe mempty (\v -> encodeVarintField IntegerStatistics_Minimum
                          (zigzag64 v)) (isMinimum s)
  , maybe mempty (\v -> encodeVarintField IntegerStatistics_Maximum
                          (zigzag64 v)) (isMaximum s)
  , maybe mempty (\v -> encodeVarintField IntegerStatistics_Sum
                          (zigzag64 v)) (isSum s)
  ]

zigzag64 :: Int64 -> Word64
zigzag64 n = fromIntegral ((n `Bits.shiftL` 1) `Bits.xor` (n `Bits.shiftR` 63))

unzigzag64 :: Word64 -> Int64
unzigzag64 v = fromIntegral ((v `Bits.shiftR` 1) `Bits.xor` negate (v Bits..&. 1))

encodeDoubleStats :: DoubleStatistics -> B.Builder
encodeDoubleStats s = mconcat
  [ maybe mempty (encodeFixedF64 DoubleStatistics_Minimum) (dsMinimum s)
  , maybe mempty (encodeFixedF64 DoubleStatistics_Maximum) (dsMaximum s)
  , maybe mempty (encodeFixedF64 DoubleStatistics_Sum)     (dsSum s)
  ]

encodeFixedF64 :: (Int, WireType) -> Double -> B.Builder
encodeFixedF64 (fn, wt) d =
  protoTagByte fn wt
    <> B.doubleLE d

encodeStringStats :: StringStatistics -> B.Builder
encodeStringStats s = mconcat
  [ maybe mempty (encodeLengthDelim StringStatistics_Minimum
                  . B.byteString . TE.encodeUtf8) (ssMinimum s)
  , maybe mempty (encodeLengthDelim StringStatistics_Maximum
                  . B.byteString . TE.encodeUtf8) (ssMaximum s)
  , maybe mempty (\v -> encodeVarintField StringStatistics_Sum
                          (zigzag64 v)) (ssSum s)
  , maybe mempty (encodeLengthDelim StringStatistics_LowerBound
                  . B.byteString . TE.encodeUtf8) (ssLowerBound s)
  , maybe mempty (encodeLengthDelim StringStatistics_UpperBound
                  . B.byteString . TE.encodeUtf8) (ssUpperBound s)
  ]

encodeBinaryStats :: BinaryStatistics -> B.Builder
encodeBinaryStats s =
  maybe mempty (\v -> encodeVarintField BinaryStatistics_Sum (zigzag64 v))
    (bsSum s)

encodeBucketStats :: BucketStatistics -> B.Builder
encodeBucketStats s =
  encodePackedVarintField BucketStatistics_Count
    (V.toList (bucketCounts s))

encodeDateStats :: DateStatistics -> B.Builder
encodeDateStats s = mconcat
  [ maybe mempty (\v -> encodeVarintField DateStatistics_Minimum
                          (zigzag64 v)) (dateMinimum s)
  , maybe mempty (\v -> encodeVarintField DateStatistics_Maximum
                          (zigzag64 v)) (dateMaximum s)
  ]

encodeTimestampStats :: TimestampStatistics -> B.Builder
encodeTimestampStats s = mconcat
  [ maybe mempty (\v -> encodeVarintField TimestampStatistics_Minimum
                          (zigzag64 v)) (tsMinimum s)
  , maybe mempty (\v -> encodeVarintField TimestampStatistics_Maximum
                          (zigzag64 v)) (tsMaximum s)
  , maybe mempty (\v -> encodeVarintField TimestampStatistics_MinimumUtc
                          (zigzag64 v)) (tsMinimumUtc s)
  , maybe mempty (\v -> encodeVarintField TimestampStatistics_MaximumUtc
                          (zigzag64 v)) (tsMaximumUtc s)
  ]

encodeDecimalStats :: DecimalStatistics -> B.Builder
encodeDecimalStats s = mconcat
  [ maybe mempty (encodeLengthDelim DecimalStatistics_Minimum
                  . B.byteString . TE.encodeUtf8) (decMinimum s)
  , maybe mempty (encodeLengthDelim DecimalStatistics_Maximum
                  . B.byteString . TE.encodeUtf8) (decMaximum s)
  , maybe mempty (encodeLengthDelim DecimalStatistics_Sum
                  . B.byteString . TE.encodeUtf8) (decSum s)
  ]

encodePostScript :: PostScript -> ByteString
encodePostScript ps = BL.toStrict $ B.toLazyByteString $ mconcat
  [ encodeVarintField PostScript_FooterLength         (psFooterLength ps)
  , encodeVarintField PostScript_Compression          (psCompression ps)
  , encodeVarintField PostScript_CompressionBlockSize (psCompressionBlockSize ps)
  , V.foldl' (\acc v -> acc <> encodeVarintField PostScript_Version
                                   (fromIntegral v :: Word64))
      mempty (psVersion ps)
  , encodeLengthDelim PostScript_Magic (B.byteString (psMagic ps))
  ]

------------------------------------------------------------------------
-- Protobuf decoding (manual varint + length-delimited)
------------------------------------------------------------------------

decodePostScript :: ByteString -> Either String PostScript
decodePostScript bs = do
  acc <- decodeMsg bs emptyAcc step
  Right (toPostScript acc)
  where
    -- Reverse-accumulating shadow record: the protobuf
    -- decoder is single-pass, so we build psVersion as a
    -- cons list and freeze once instead of V.snoc'ing on
    -- every entry (O(N^2) on long version vectors — small
    -- in practice but free to fix).
    emptyAcc = PostScriptAcc 0 0 0 [] BS.empty
    step ps = \case
      PostScript_FooterLength         -> ReadVarint $ \v -> ps { psaFooterLength = v }
      PostScript_Compression          -> ReadVarint $ \v -> ps { psaCompression = v }
      PostScript_CompressionBlockSize -> ReadVarint $ \v -> ps { psaCompressionBlockSize = v }
      PostScript_Version              -> ReadVarint $ \v ->
        ps { psaVersionRev = fromIntegral v : psaVersionRev ps }
      PostScript_Magic                -> ReadBytes  $ \v -> ps { psaMagic = v }
      _                               -> SkipUnknown
    toPostScript a = PostScript
      { psFooterLength         = psaFooterLength a
      , psCompression          = psaCompression a
      , psCompressionBlockSize = psaCompressionBlockSize a
      , psVersion              = V.fromList (reverse (psaVersionRev a))
      , psMagic                = psaMagic a
      }

-- Internal accumulator mirroring 'PostScript' but with a list
-- in place of the version 'Vector', so we don't pay O(N^2) for
-- V.snoc inside the decoder loop.
data PostScriptAcc = PostScriptAcc
  { psaFooterLength         :: !Word64
  , psaCompression          :: !Word64
  , psaCompressionBlockSize :: !Word64
  , psaVersionRev           :: ![Word32]
  , psaMagic                :: !ByteString
  }

decodeFooter :: ByteString -> Either String ORCFooter
decodeFooter bs = do
  acc <- decodeMsg bs emptyAcc step
  Right (finalize acc)
  where
    emptyAcc = FooterAcc 0 0 [] [] [] 0 [] Nothing
    step f = \case
      Footer_HeaderLength  -> ReadVarint $ \v -> f { faHeaderLength = v }
      Footer_ContentLength -> ReadVarint $ \v -> f { faContentLength = v }
      Footer_Stripes       -> ReadNested decodeStripeInfo $ \si ->
        f { faStripesRev = si : faStripesRev f }
      Footer_Types         -> ReadNested decodeORCType $ \t ->
        f { faTypesRev = t : faTypesRev f }
      Footer_Metadata      -> ReadNested decodeMetadataEntry $ \e ->
        f { faMetadataRev = e : faMetadataRev f }
      Footer_NumberOfRows  -> ReadVarint $ \v -> f { faNumberOfRows = v }
      Footer_Statistics    -> ReadNested decodeColStats $ \cs ->
        f { faStatisticsRev = cs : faStatisticsRev f }
      Footer_Encryption    -> ReadBytes $ \enc ->
        f { faEncryption = Just (FooterEncryption enc) }
      _                    -> SkipUnknown
    finalize a = ORCFooter
      { orcHeaderLength  = faHeaderLength a
      , orcContentLength = faContentLength a
      , orcStripes       = V.fromList (reverse (faStripesRev a))
      , orcTypes         = V.fromList (reverse (faTypesRev a))
      , orcMetadata      = V.fromList (reverse (faMetadataRev a))
      , orcNumberOfRows  = faNumberOfRows a
      , orcStatistics    = V.fromList (reverse (faStatisticsRev a))
      , orcEncryption    = faEncryption a
      }

-- Internal cons-list accumulator. Same fields as 'ORCFooter'
-- but with reverse lists in place of vectors so the decoder
-- doesn't pay O(N^2) for V.snoc per stripe / type / column
-- statistics entry.
data FooterAcc = FooterAcc
  { faHeaderLength   :: !Word64
  , faContentLength  :: !Word64
  , faStripesRev     :: ![StripeInformation]
  , faTypesRev       :: ![ORCType]
  , faMetadataRev    :: ![(T.Text, ByteString)]
  , faNumberOfRows   :: !Word64
  , faStatisticsRev  :: ![ColumnStatistics]
  , faEncryption     :: !(Maybe FooterEncryption)
  }

decodeStripeInfo :: ByteString -> Either String StripeInformation
decodeStripeInfo bs = decodeMsg bs (StripeInformation 0 0 0 0 0) step
  where
    step si = \case
      StripeInformation_Offset       -> ReadVarint $ \v -> si { siOffset = v }
      StripeInformation_IndexLength  -> ReadVarint $ \v -> si { siIndexLength = v }
      StripeInformation_DataLength   -> ReadVarint $ \v -> si { siDataLength = v }
      StripeInformation_FooterLength -> ReadVarint $ \v -> si { siFooterLength = v }
      StripeInformation_NumberOfRows -> ReadVarint $ \v -> si { siNumberOfRows = v }
      _                              -> SkipUnknown

decodeORCType :: ByteString -> Either String ORCType
decodeORCType bs = do
  acc <- decodeMsg bs emptyAcc step
  Right (toORCType acc)
  where
    emptyAcc = ORCTypeAcc TKBoolean [] []
    step ot = \case
      ORCType_Kind       -> ReadVarintE $ \v -> case intToTypeKind (fromIntegral v) of
        Just tk -> Right ot { otaKind = tk }
        Nothing -> Left $ "ORC.Footer: invalid TypeKind " ++ show v
      ORCType_Subtypes   -> ReadVarint $ \v ->
        ot { otaSubtypesRev = fromIntegral v : otaSubtypesRev ot }
      ORCType_FieldNames -> ReadBytesE $ \v -> case TE.decodeUtf8' v of
        Right t -> Right ot { otaFieldNamesRev = t : otaFieldNamesRev ot }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in field name"
      _                  -> SkipUnknown
    toORCType a = ORCType
      { otKind       = otaKind a
      , otSubtypes   = V.fromList (reverse (otaSubtypesRev a))
      , otFieldNames = V.fromList (reverse (otaFieldNamesRev a))
      }

data ORCTypeAcc = ORCTypeAcc
  { otaKind          :: !TypeKind
  , otaSubtypesRev   :: ![Word32]
  , otaFieldNamesRev :: ![T.Text]
  }

decodeMetadataEntry :: ByteString -> Either String (T.Text, ByteString)
decodeMetadataEntry bs = decodeMsg bs (T.empty, BS.empty) step
  where
    step (k, v) = \case
      MetadataEntry_Name  -> ReadBytesE $ \bs' -> case TE.decodeUtf8' bs' of
        Right t -> Right (t, v)
        Left _  -> Left "ORC.Footer: invalid UTF-8 in metadata key"
      MetadataEntry_Value -> ReadBytes  $ \bs' -> (k, bs')
      _                   -> SkipUnknown

decodeColStats :: ByteString -> Either String ColumnStatistics
decodeColStats bs =
  decodeMsg bs (ColumnStatistics Nothing Nothing Nothing Nothing) step
  where
    step cs = \case
      ColumnStatistics_NumberOfValues -> ReadVarint $ \v -> cs { csNumberOfValues = Just v }
      ColumnStatistics_HasNull        -> ReadVarint $ \v -> cs { csHasNull = Just (v /= 0) }
      ColumnStatistics_BytesOnDisk    -> ReadVarint $ \v -> cs { csBytesOnDisk = Just v }
      ColumnStatistics_IntStatistics  -> ReadNested decodeIntStats
                                          (\s -> cs { csKind = Just (SkInt s) })
      ColumnStatistics_DoubleStatistics -> ReadNested decodeDoubleStats
                                          (\s -> cs { csKind = Just (SkDouble s) })
      ColumnStatistics_StringStatistics -> ReadNested decodeStringStats
                                          (\s -> cs { csKind = Just (SkString s) })
      ColumnStatistics_BucketStatistics -> ReadNested decodeBucketStats
                                          (\s -> cs { csKind = Just (SkBucket s) })
      ColumnStatistics_DecimalStatistics -> ReadNested decodeDecimalStats
                                          (\s -> cs { csKind = Just (SkDecimal s) })
      ColumnStatistics_DateStatistics -> ReadNested decodeDateStats
                                          (\s -> cs { csKind = Just (SkDate s) })
      ColumnStatistics_BinaryStatistics -> ReadNested decodeBinaryStats
                                          (\s -> cs { csKind = Just (SkBinary s) })
      ColumnStatistics_TimestampStatistics -> ReadNested decodeTimestampStats
                                          (\s -> cs { csKind = Just (SkTimestamp s) })
      _                               -> SkipUnknown

decodeIntStats :: ByteString -> Either String IntegerStatistics
decodeIntStats bs = decodeMsg bs (IntegerStatistics Nothing Nothing Nothing) step
  where
    step s = \case
      IntegerStatistics_Minimum -> ReadVarint $ \v -> s { isMinimum = Just (unzigzag64 v) }
      IntegerStatistics_Maximum -> ReadVarint $ \v -> s { isMaximum = Just (unzigzag64 v) }
      IntegerStatistics_Sum     -> ReadVarint $ \v -> s { isSum     = Just (unzigzag64 v) }
      _                         -> SkipUnknown

decodeDoubleStats :: ByteString -> Either String DoubleStatistics
decodeDoubleStats bs = decodeMsg bs (DoubleStatistics Nothing Nothing Nothing) step
  where
    step s = \case
      DoubleStatistics_Minimum -> ReadFixed64 $ \w -> s { dsMinimum = Just (castWord64ToDouble w) }
      DoubleStatistics_Maximum -> ReadFixed64 $ \w -> s { dsMaximum = Just (castWord64ToDouble w) }
      DoubleStatistics_Sum     -> ReadFixed64 $ \w -> s { dsSum     = Just (castWord64ToDouble w) }
      _                        -> SkipUnknown

decodeStringStats :: ByteString -> Either String StringStatistics
decodeStringStats bs =
  decodeMsg bs (StringStatistics Nothing Nothing Nothing Nothing Nothing) step
  where
    step s = \case
      StringStatistics_Minimum    -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { ssMinimum = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in string-min stat"
      StringStatistics_Maximum    -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { ssMaximum = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in string-max stat"
      StringStatistics_Sum        -> ReadVarint $ \v -> s { ssSum = Just (unzigzag64 v) }
      StringStatistics_LowerBound -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { ssLowerBound = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in string-lower-bound stat"
      StringStatistics_UpperBound -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { ssUpperBound = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in string-upper-bound stat"
      _                           -> SkipUnknown

decodeBinaryStats :: ByteString -> Either String BinaryStatistics
decodeBinaryStats bs = decodeMsg bs (BinaryStatistics Nothing) step
  where
    step s = \case
      BinaryStatistics_Sum -> ReadVarint $ \v -> s { bsSum = Just (unzigzag64 v) }
      _                    -> SkipUnknown

decodeBucketStats :: ByteString -> Either String BucketStatistics
decodeBucketStats bs =
  decodeMsg bs (BucketStatistics V.empty) step
  where
    step s = \case
      BucketStatistics_Count -> ReadBytes $ \payload ->
        s { bucketCounts = bucketCounts s
                            <> V.fromList (decodePackedVarints payload)
          }
      _                      -> SkipUnknown

decodeDateStats :: ByteString -> Either String DateStatistics
decodeDateStats bs = decodeMsg bs (DateStatistics Nothing Nothing) step
  where
    step s = \case
      DateStatistics_Minimum -> ReadVarint $ \v -> s { dateMinimum = Just (unzigzag64 v) }
      DateStatistics_Maximum -> ReadVarint $ \v -> s { dateMaximum = Just (unzigzag64 v) }
      _                      -> SkipUnknown

decodeTimestampStats :: ByteString -> Either String TimestampStatistics
decodeTimestampStats bs =
  decodeMsg bs (TimestampStatistics Nothing Nothing Nothing Nothing) step
  where
    step s = \case
      TimestampStatistics_Minimum    -> ReadVarint $ \v -> s { tsMinimum = Just (unzigzag64 v) }
      TimestampStatistics_Maximum    -> ReadVarint $ \v -> s { tsMaximum = Just (unzigzag64 v) }
      TimestampStatistics_MinimumUtc -> ReadVarint $ \v -> s { tsMinimumUtc = Just (unzigzag64 v) }
      TimestampStatistics_MaximumUtc -> ReadVarint $ \v -> s { tsMaximumUtc = Just (unzigzag64 v) }
      _                              -> SkipUnknown

decodeDecimalStats :: ByteString -> Either String DecimalStatistics
decodeDecimalStats bs =
  decodeMsg bs (DecimalStatistics Nothing Nothing Nothing) step
  where
    step s = \case
      DecimalStatistics_Minimum -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { decMinimum = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in decimal-min stat"
      DecimalStatistics_Maximum -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { decMaximum = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in decimal-max stat"
      DecimalStatistics_Sum     -> ReadBytesE $ \b -> case TE.decodeUtf8' b of
        Right t -> Right s { decSum = Just t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in decimal-sum stat"
      _                         -> SkipUnknown

-- | Unpack a packed-varint payload into a list of @Word64@. Used
-- by the bucket-statistics decoder.
decodePackedVarints :: ByteString -> [Word64]
decodePackedVarints bs = go 0
  where
    !len = BS.length bs
    go !off
      | off >= len = []
      | otherwise = case readVarintRaw bs off len of
          Just (v, off') -> v : go off'
          Nothing -> []

readVarintRaw :: ByteString -> Int -> Int -> Maybe (Word64, Int)
readVarintRaw bs !off !len = go off 0 0
  where
    go !pos !val !shift
      | pos >= len = Nothing
      | shift > 63 = Nothing
      | otherwise =
          let !b = fromIntegral (BS.index bs pos) :: Word64
              !val' = val .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
               then Just (val', pos + 1)
               else go (pos + 1) val' (shift + 7)

-- Decoder primitives + DSL ('decodeMsg', 'FieldAction', 'getVarint',
-- 'getLenDelim', 'skipField') and encoder helpers
-- ('encodeVarintField', 'encodeLengthDelim') come from
-- "ORC.Proto.Schema"; those versions take the pattern-synonym
-- @(fieldNum, wireType)@ pair directly.
