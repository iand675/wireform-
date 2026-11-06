{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | The Parquet @FILE@ logical type. References retain the original strings
and distinguish absent values from empty inline bytes. Resolution here is a
pure description of the bytes to retrieve: it does not fetch URIs, interpret
relative paths, or verify checksums. Applications select their own transport,
URI policy and integrity checks. A URI always names an external object, even
when it names the Parquet file being read.
-}
module Parquet.File (
  FileField (..),
  FileFieldSpec (..),
  FileSchema,
  mkFileSchema,
  fileSchemaFields,
  fileSchemaFieldId,
  withFileSchemaFieldId,
  fileFieldName,
  fileSchemaElements,
  parseFileSchema,
  FileReference (..),
  emptyFileReference,
  validateFileReference,
  validateFileReferenceForSchema,
  FileLocation,
  fileLocationUri,
  fileLocationOffset,
  fileLocationSize,
  fileLocationEnd,
  FileSource (..),
  FileResolution (..),
  FileChecksum (..),
  FileDigestAlgorithm (..),
  parseFileChecksum,
  resolveFileReference,
  FileReader,
  compileFileReader,
  fileReferenceAt,
  fileReferenceAt#,
) where

import Arrow.Column qualified as A
import Control.Monad (unless, when)
import Data.Bits (bit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import GHC.Exts (Int (I#), Int#)
import Parquet.Types


data FileField = FileUri | FileOffset | FileSize | FileContentType | FileChecksumField | FileInline
  deriving (Eq, Ord, Show, Enum, Bounded)


data FileFieldSpec = FileFieldSpec
  { fileField :: !FileField
  , fileFieldId :: !(Maybe Int32)
  }
  deriving (Eq, Show)


{- | Validated, ordered fields. The cached mask avoids scanning the schema
for every row. An empty schema has no physical leaves and no valid non-null
references; null values are recovered from their containing row counts.
-}
data FileSchema = FileSchema !(V.Vector FileFieldSpec) {-# UNPACK #-} !Word8 !(Maybe Int32)
  deriving (Eq, Show)


mkFileSchema :: V.Vector FileFieldSpec -> Either String FileSchema
mkFileSchema fields = (\mask -> FileSchema fields mask Nothing) <$> V.foldM add 0 fields
  where
    add !mask field =
      let !flag = fieldBit (fileField field)
      in if mask .&. flag /= 0
           then Left "Parquet.File: duplicate FILE field"
           else Right (mask .|. flag)


fileSchemaFields :: FileSchema -> V.Vector FileFieldSpec
fileSchemaFields (FileSchema fields _ _) = fields


fileSchemaFieldId :: FileSchema -> Maybe Int32
fileSchemaFieldId (FileSchema _ _ ident) = ident


-- | Set the ID of the FILE group itself, independently of its child IDs.
withFileSchemaFieldId :: Maybe Int32 -> FileSchema -> FileSchema
withFileSchemaFieldId ident (FileSchema fields mask _) = FileSchema fields mask ident


fieldBit :: FileField -> Word8
fieldBit = bit . fromEnum
{-# INLINE fieldBit #-}


fileFieldName :: FileField -> Text
fileFieldName = \case
  FileUri -> "uri"
  FileOffset -> "offset"
  FileSize -> "size"
  FileContentType -> "content_type"
  FileChecksumField -> "checksum"
  FileInline -> "inline"


{- | Direct children, in schema order; the annotated group itself is emitted
by the enclosing writer. Every child is OPTIONAL, including locator fields.
-}
fileSchemaElements :: FileSchema -> V.Vector SchemaElement
fileSchemaElements = V.map element . fileSchemaFields
  where
    element spec =
      let field = fileField spec
          string = field == FileUri || field == FileContentType || field == FileChecksumField
          physical = if field == FileOffset || field == FileSize then PTInt64 else PTByteArray
      in SchemaElement
           { seName = fileFieldName field
           , seRepetition = Just Optional
           , seType = Just physical
           , seNumChildren = Nothing
           , seConvertedType = if string then Just CTUtf8 else Nothing
           , seLogicalType = if string then Just LTString else Nothing
           , seFieldId = fileFieldId spec
           , seTypeLength = Nothing
           }


{- | Validate a FILE group and its direct children. Names are case sensitive;
order and field IDs are preserved. Additional application metadata belongs
beside the FILE group, rather than inside it.
-}
parseFileSchema :: SchemaElement -> V.Vector SchemaElement -> Either String FileSchema
parseFileSchema group children = do
  unless
    ( seLogicalType group == Just LTFile
        && seType group == Nothing
        && seNumChildren group == Just (fromIntegral (V.length children))
        && seConvertedType group == Nothing
        && seTypeLength group == Nothing
    )
    (Left "Parquet.File: expected a FILE-annotated group")
  fields <- V.mapM child children
  withFileSchemaFieldId (seFieldId group) <$> mkFileSchema fields
  where
    child element = do
      kind <- case seName element of
        "uri" -> Right FileUri
        "offset" -> Right FileOffset
        "size" -> Right FileSize
        "content_type" -> Right FileContentType
        "checksum" -> Right FileChecksumField
        "inline" -> Right FileInline
        _ -> Left "Parquet.File: unknown FILE field (names are case sensitive)"
      let primitive = seNumChildren element == Nothing && seTypeLength element == Nothing
          plain = seLogicalType element == Nothing && seConvertedType element == Nothing
          string = case seLogicalType element of
            Just LTString -> seConvertedType element `elem` [Nothing, Just CTUtf8]
            Nothing -> seConvertedType element == Just CTUtf8
            _ -> False
          signed = case seLogicalType element of
            Just (LTInteger 64 True) -> seConvertedType element `elem` [Nothing, Just CTInt64]
            Nothing -> seConvertedType element `elem` [Nothing, Just CTInt64]
            _ -> False
          typeMatches = case kind of
            FileOffset -> seType element == Just PTInt64 && signed
            FileSize -> seType element == Just PTInt64 && signed
            FileInline -> seType element == Just PTByteArray && plain
            _ -> seType element == Just PTByteArray && string
      unless
        (primitive && seRepetition element == Just Optional && typeMatches)
        (Left "Parquet.File: FILE children must be OPTIONAL primitives with the specified types")
      Right (FileFieldSpec kind (seFieldId element))


data FileReference = FileReference
  { fileUri :: !(Maybe Text)
  , fileOffset :: !(Maybe Int64)
  , fileSize :: !(Maybe Int64)
  , fileContentType :: !(Maybe Text)
  , fileChecksum :: !(Maybe Text)
  , fileInline :: !(Maybe ByteString)
  }
  deriving (Eq, Show)


emptyFileReference :: FileReference
emptyFileReference = FileReference Nothing Nothing Nothing Nothing Nothing Nothing


{- | A validated external range. Offset and size are stored unsigned so even
@maxBound :: Int64@ plus @maxBound :: Int64@ has an exact exclusive end.
-}
data FileLocation = FileLocation
  { fileLocationUri :: !Text
  , fileLocationOffset :: {-# UNPACK #-} !Word64
  , fileLocationSize :: !(Maybe Word64)
  }
  deriving (Eq, Show)


fileLocationEnd :: FileLocation -> Maybe Word64
fileLocationEnd location = (fileLocationOffset location +) <$> fileLocationSize location


data FileSource = InlineBytes !ByteString | ExternalBytes !FileLocation
  deriving (Eq, Show)


data FileDigestAlgorithm = FileMD5 | FileCRC32 | FileCRC32C | FileSHA256
  deriving (Eq, Show)


{- | Parsed integrity metadata, not proof of verification. ETAG applies to the
whole external object; digests apply to the resolved range/inline bytes.
-}
data FileChecksum = FileETag !Text | FileDigest !FileDigestAlgorithm !Text
  deriving (Eq, Show)


data FileResolution = FileResolution
  { filePreferredSource :: !FileSource
  , fileExternalLocation :: !(Maybe FileLocation)
  , fileMediaType :: !Text
  , fileIntegrityToken :: !(Maybe FileChecksum)
  }
  deriving (Eq, Show)


nonempty :: Maybe Text -> Maybe Text
nonempty (Just value) | not (T.null value) = Just value
nonempty _ = Nothing
{-# INLINE nonempty #-}


{- | Unknown algorithms are ignored, as required by the FILE specification.
Known digest algorithms require their exact lowercase hexadecimal encoding.
-}
parseFileChecksum :: Maybe Text -> Either String (Maybe FileChecksum)
parseFileChecksum value = case nonempty value of
  Nothing -> Right Nothing
  Just token ->
    let (algorithm, colonDigest) = T.breakOn ":" token
        digest = T.drop 1 colonDigest
        hex count kind
          | T.length digest == count && T.all lowerHex digest = Right (Just (FileDigest kind digest))
          | otherwise = Left "Parquet.File: malformed known checksum digest"
    in if T.null colonDigest || T.null algorithm
         then Left "Parquet.File: checksum must have algorithm:digest form"
         else case algorithm of
           "ETAG" -> Right (Just (FileETag digest))
           "MD5" -> hex 32 FileMD5
           "CRC32" -> hex 8 FileCRC32
           "CRC32C" -> hex 8 FileCRC32C
           "SHA-256" -> hex 64 FileSHA256
           _ -> Right Nothing
  where
    lowerHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')


{- | Validate source/locator relationships. In particular, a present zero offset
still requires both URI and size. Empty inline bytes are a valid source.
-}
validateFileReference :: FileReference -> Either String ()
validateFileReference reference = do
  validateFileSources reference
  _ <- parseFileChecksum (fileChecksum reference)
  Right ()
{-# INLINE validateFileReference #-}


validateFileSources :: FileReference -> Either String ()
validateFileSources reference = do
  let uri = isJust (nonempty (fileUri reference))
      offset = fileOffset reference
      size = fileSize reference
  when
    (maybe False (< 0) offset || maybe False (< 0) size)
    (Left "Parquet.File: negative offset or size")
  when
    (isJust offset && (not uri || not (isJust size)))
    (Left "Parquet.File: offset requires URI and size")
  when
    (isJust size && not uri)
    (Left "Parquet.File: size requires URI")
  unless
    (uri || isJust (fileInline reference))
    (Left "Parquet.File: reference has neither inline bytes nor URI")
  Right ()
{-# INLINE validateFileSources #-}


{- | Reject values that would lose a present field when written using a subset
schema. This tests raw presence, so @Just ""@ is preserved rather than dropped.
-}
validateFileReferenceForSchema :: FileSchema -> FileReference -> Either String ()
validateFileReferenceForSchema (FileSchema _ mask _) reference = do
  let present field value = if isJust value then fieldBit field else 0
      used =
        present FileUri (fileUri reference)
          .|. present FileOffset (fileOffset reference)
          .|. present FileSize (fileSize reference)
          .|. present FileContentType (fileContentType reference)
          .|. present FileChecksumField (fileChecksum reference)
          .|. present FileInline (fileInline reference)
  unless (used .&. mask == used) (Left "Parquet.File: value contains a field omitted by its schema")
  validateFileReference reference
  -- Readers may choose either source without comparing them. A producer can
  -- nevertheless reject this locally provable disagreement without fetching.
  case (fileInline reference, fileSize reference) of
    (Just bytes, Just size) ->
      unless
        (fromIntegral (BS.length bytes) == size)
        (Left "Parquet.File: inline byte length disagrees with locator size")
    _ -> Right ()
{-# INLINE validateFileReferenceForSchema #-}


{- | Choose inline bytes when available, while retaining a valid external
alternative. Locator offset/size never slice the inline bytes.
-}
resolveFileReference :: FileReference -> Either String FileResolution
resolveFileReference reference = do
  validateFileSources reference
  integrity <- parseFileChecksum (fileChecksum reference)
  let location = (\uri -> FileLocation uri (maybe 0 fromIntegral (fileOffset reference)) (fromIntegral <$> fileSize reference)) <$> nonempty (fileUri reference)
  source <- case fileInline reference of
    Just bytes -> Right (InlineBytes bytes)
    Nothing -> maybe (Left "Parquet.File: missing source") (Right . ExternalBytes) location
  Right (FileResolution source location (maybe "application/octet-stream" id (nonempty (fileContentType reference))) integrity)


data TextColumn = TextColumn32 !(A.Utf8Array Int32) | TextColumn64 !(A.Utf8Array Int64) | TextColumnView !A.ColumnArray


data BinaryColumn = BinaryColumn32 !(A.BytesArray Int32) | BinaryColumn64 !(A.BytesArray Int64) | BinaryColumnView !A.ColumnArray


-- | Cached, named column views. Compile once per row group, not once per row.
data FileReader = FileReader
  { readerLength :: {-# UNPACK #-} !Int
  , readerValidity :: !(Maybe A.Validity)
  , readerUri :: !(Maybe TextColumn)
  , readerOffset :: !(Maybe (A.PrimArray Int64))
  , readerSize :: !(Maybe (A.PrimArray Int64))
  , readerContentType :: !(Maybe TextColumn)
  , readerChecksum :: !(Maybe TextColumn)
  , readerInline :: !(Maybe BinaryColumn)
  }


compileFileReader :: FileSchema -> A.ColumnArray -> Either String FileReader
compileFileReader schema column = case column of
  A.ColStruct n valid children -> do
    let specs = fileSchemaFields schema
        find field = snd <$> V.find ((== fileFieldName field) . fst) children
        expected name = V.any ((== name) . fileFieldName . fileField) specs
    unless
      ( V.length specs == V.length children
          && V.all (expected . fst) children
          && V.all (\spec -> V.length (V.filter ((== fileFieldName (fileField spec)) . fst) children) == 1) specs
      )
      (Left "Parquet.File: column fields do not match FILE schema")
    unless
      (V.all ((>= n) . A.columnLength . snd) children)
      (Left "Parquet.File: child column shorter than FILE group")
    uri <- traverse textColumn (find FileUri)
    offset <- traverse intColumn (find FileOffset)
    size <- traverse intColumn (find FileSize)
    content <- traverse textColumn (find FileContentType)
    checksum <- traverse textColumn (find FileChecksumField)
    inline <- traverse binaryColumn (find FileInline)
    Right (FileReader n valid uri offset size content checksum inline)
  _ -> Left "Parquet.File: expected a struct column"
  where
    textColumn value = case value of
      A.ColUtf8View {} -> Right (TextColumnView value)
      _ -> case A.asUtf8 value of
        Just array -> Right (TextColumn32 array)
        Nothing -> maybe (Left "Parquet.File: expected STRING column") (Right . TextColumn64) (A.asLargeUtf8 value)
    intColumn value = case value of
      A.ColInt64 valid values -> Right (A.PrimArray valid values)
      _ -> Left "Parquet.File: expected INT64 column"
    binaryColumn value = case value of
      A.ColBinary {} -> maybe (Left "Parquet.File: invalid BYTE_ARRAY column") (Right . BinaryColumn32) (A.asBinary value)
      A.ColLargeBinary {} -> maybe (Left "Parquet.File: invalid BYTE_ARRAY column") (Right . BinaryColumn64) (A.asLargeBinary value)
      A.ColBinaryView {} -> Right (BinaryColumnView value)
      _ -> Left "Parquet.File: expected BYTE_ARRAY column"


{- | Read a raw reference; invalid resolution metadata remains inspectable.
Call 'resolveFileReference' to validate it, or map its error to null as the
FILE specification permits. The containing struct's validity takes priority
over arbitrary values in its children beneath a null row.
-}
fileReferenceAt :: FileReader -> Int -> Either String (Maybe FileReference)
fileReferenceAt reader (I# index) = case fileReferenceAt# reader index of
  (# message | | #) -> Left message
  (# | reference | #) -> Right (Just reference)
  (# | | () #) -> Right Nothing


{- | The row-loop worker: error, present reference, or null. Avoids intermediate
boxed Either/Maybe results when composing with a nested encoder.
-}
fileReferenceAt# :: FileReader -> Int# -> (# String | FileReference | () #)
fileReferenceAt# reader index#
  | index < 0 || index >= readerLength reader = (# "Parquet.File: row index out of range" | | #)
  | not (A.isValidAt (readerValidity reader) index) = (# | | () #)
  | otherwise =
      let !reference =
            FileReference
              (readerUri reader >>= textAt)
              (readerOffset reader >>= (`A.primAt` index))
              (readerSize reader >>= (`A.primAt` index))
              (readerContentType reader >>= textAt)
              (readerChecksum reader >>= textAt)
              (readerInline reader >>= bytesAt)
      in (# | reference | #)
  where
    index = I# index#
    textAt (TextColumn32 array) = A.textAt array index
    textAt (TextColumn64 array) = A.textAt array index
    textAt (TextColumnView column) = A.anyTextAt column index
    bytesAt (BinaryColumn32 array) = A.bytesAt array index
    bytesAt (BinaryColumn64 array) = A.bytesAt array index
    bytesAt (BinaryColumnView column) = A.anyBytesAt column index
