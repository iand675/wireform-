{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Parquet page headers (Thrift compact protocol).
--
-- See @parquet.thrift@ (@PageHeader@, @DataPageHeader@, @DictionaryPageHeader@,
-- @DataPageHeaderV2@). Thrift field placement is mediated by the
-- pattern synonyms in "Parquet.Thrift.Schema".
module Parquet.Page
  ( PageHeader (..)
  , PageType (..)
  , DataPageHeader (..)
  , DictionaryPageHeader (..)
  , DataPageHeaderV2 (..)
  , readPageHeaderAt
  , pageTypeTag
  , pageTypeIsDataPageV1
  , pageTypeIsDataPageV2
  , pageTypeIsDictionary
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)

import Thrift.Wire

-- | Discriminated union over Parquet page types. Mirrors the
-- @PageType@ enum in @parquet.thrift@ (DATA_PAGE=0, INDEX_PAGE=1,
-- DICTIONARY_PAGE=2, DATA_PAGE_V2=3) but each constructor carries the
-- nested struct that the spec couples with that variant — there is no
-- meaningful "DATA_PAGE without DataPageHeader" value on the wire, so
-- we don't represent one.
data PageType
  = PtDataPage      !DataPageHeader
  | PtIndexPage
  | PtDictionaryPage !DictionaryPageHeader
  | PtDataPageV2    !DataPageHeaderV2
  deriving stock (Show, Eq)

-- | The on-the-wire @PageType@ tag (Thrift @PageHeader.type@, field 1).
pageTypeTag :: PageType -> Int32
pageTypeTag = \case
  PtDataPage{}       -> 0
  PtIndexPage        -> 1
  PtDictionaryPage{} -> 2
  PtDataPageV2{}     -> 3

pageTypeIsDataPageV1 :: PageType -> Bool
pageTypeIsDataPageV1 PtDataPage{} = True
pageTypeIsDataPageV1 _            = False

pageTypeIsDataPageV2 :: PageType -> Bool
pageTypeIsDataPageV2 PtDataPageV2{} = True
pageTypeIsDataPageV2 _              = False

pageTypeIsDictionary :: PageType -> Bool
pageTypeIsDictionary PtDictionaryPage{} = True
pageTypeIsDictionary _                  = False

data PageHeader = PageHeader
  { phType                 :: !PageType
  , phUncompressedPageSize :: !(Maybe Int32)
  , phCompressedPageSize   :: !(Maybe Int32)
  } deriving stock (Show, Eq)

-- | Nested @DataPageHeader@ (field 5 of @PageHeader@).
data DataPageHeader = DataPageHeader
  { dphNumValues :: !Int32
  , dphEncoding  :: !Int32
  } deriving stock (Show, Eq)

-- | Nested @DictionaryPageHeader@ (field 7 of @PageHeader@).
data DictionaryPageHeader = DictionaryPageHeader
  { dictNumValues :: !Int32
  , dictEncoding  :: !Int32
  } deriving stock (Show, Eq)

-- | Nested @DataPageHeaderV2@ (field 8 of @PageHeader@).
-- Parquet Thrift field IDs 1–7 of the @DataPageHeaderV2@ struct.
data DataPageHeaderV2 = DataPageHeaderV2
  { dph2NumValues    :: !Int32
  , dph2NumNulls     :: !Int32
  , dph2NumRows      :: !Int32
  , dph2Encoding     :: !Int32
  , dph2DefLevelsLen :: !Int32
  , dph2RepLevelsLen :: !Int32
  , dph2IsCompressed :: !Bool
  } deriving stock (Show, Eq)

-- | Direct Thrift-Compact decoder for 'PageHeader'.
--
-- Was previously a generic @decodeCompactFrom@ that built a full
-- @TV.Value@ AST then walked it. For files with many pages
-- (column chunks with K data pages → K calls per chunk) the
-- per-call AST cost (V.fromList, ~20 fields × allocation) was
-- a measurable slice of the read path.
--
-- This direct decoder walks the wire bytes once and writes
-- straight into a 'PageHeader' record. No 'TV.Value' is ever
-- allocated; nested @DataPageHeader@ / @DictionaryPageHeader@
-- / @DataPageHeaderV2@ sub-structs decode the same way.
-- Unknown / spec-optional fields (CRC, statistics) are
-- skipped via 'skipCompactValue'.
readPageHeaderAt :: ByteString -> Int -> Either String (PageHeader, Int)
readPageHeaderAt !bs !off0 =
  go off0 0 (-1) Nothing Nothing initialPayload
  where
    -- Sentinel for an as-yet-undecoded page-type tag. Final
    -- assembly fails if no PageType_Type field appeared.
    !initialPayload = NoBody

    go !off !lastFid !ty !mUnc !mComp !pld =
      case tCompDecodeFieldBegin bs off lastFid of
        Nothing -> Left "Parquet.Page: malformed PageHeader field header"
        Just (TT_STOP, _, off', _) -> assemble off' ty mUnc mComp pld
        Just (tt, fid, off', boolVal) -> case (fid, tt) of
          (1, TT_I32) -> step32 off' $ \v o' -> go o' fid v mUnc mComp pld
          (2, TT_I32) -> step32 off' $ \v o' -> go o' fid ty (Just v) mComp pld
          (3, TT_I32) -> step32 off' $ \v o' -> go o' fid ty mUnc (Just v) pld
          (5, TT_STRUCT) -> case decodeDataPageHeader bs off' of
            Left e -> Left e
            Right (dph, o') -> go o' fid ty mUnc mComp (DataBody dph)
          (7, TT_STRUCT) -> case decodeDictionaryPageHeader bs off' of
            Left e -> Left e
            Right (dph, o') -> go o' fid ty mUnc mComp (DictBody dph)
          (8, TT_STRUCT) -> case decodeDataPageHeaderV2 bs off' of
            Left e -> Left e
            Right (dph, o') -> go o' fid ty mUnc mComp (V2Body dph)
          _ -> case skipCompactValue tt boolVal bs off' of
            Nothing -> Left "Parquet.Page: failed to skip unknown PageHeader field"
            Just o' -> go o' fid ty mUnc mComp pld

    step32 off' cont = case tCompDecodeI32 bs off' of
      Nothing -> Left "Parquet.Page: malformed I32 field in PageHeader"
      Just (v, o') -> cont v o'

    assemble !endOff !ty !mUnc !mComp !pld
      | ty < 0 = Left "Parquet.Page: missing or invalid field PageHeader.type"
      | otherwise = do
          pageType <- case ty of
            0 -> case pld of
              DataBody dph -> Right (PtDataPage dph)
              _ -> Left "Parquet.Page: DATA_PAGE without DataPageHeader (field 5)"
            1 -> Right PtIndexPage
            2 -> case pld of
              DictBody dph -> Right (PtDictionaryPage dph)
              _ -> Left "Parquet.Page: DICTIONARY_PAGE without DictionaryPageHeader (field 7)"
            3 -> case pld of
              V2Body dph -> Right (PtDataPageV2 dph)
              _ -> Left "Parquet.Page: DATA_PAGE_V2 without DataPageHeaderV2 (field 8)"
            _ -> Left ("Parquet.Page: unknown PageType tag " ++ show ty)
          Right ( PageHeader
                    { phType = pageType
                    , phUncompressedPageSize = mUnc
                    , phCompressedPageSize = mComp
                    }
                , endOff
                )

-- Internal sum to track which (if any) sub-struct body has been
-- decoded so far. Avoids the boxed 'Maybe (Either ...)' shape.
data BodyAcc
  = NoBody
  | DataBody {-# UNPACK #-} !DataPageHeader
  | DictBody {-# UNPACK #-} !DictionaryPageHeader
  | V2Body   {-# UNPACK #-} !DataPageHeaderV2

decodeDataPageHeader :: ByteString -> Int -> Either String (DataPageHeader, Int)
decodeDataPageHeader !bs !off0 = go off0 0 (-1) 0
  where
    go !off !lastFid !nv !enc =
      case tCompDecodeFieldBegin bs off lastFid of
        Nothing -> Left "Parquet.Page: malformed DataPageHeader field header"
        Just (TT_STOP, _, off', _) ->
          if nv < 0
            then Left "Parquet.Page: missing field DataPageHeader.num_values"
            else Right (DataPageHeader { dphNumValues = nv, dphEncoding = enc }, off')
        Just (tt, fid, off', boolVal) -> case (fid, tt) of
          (1, TT_I32) -> case tCompDecodeI32 bs off' of
            Nothing -> Left "Parquet.Page: malformed DataPageHeader.num_values"
            Just (v, o') -> go o' fid v enc
          (2, TT_I32) -> case tCompDecodeI32 bs off' of
            Nothing -> Left "Parquet.Page: malformed DataPageHeader.encoding"
            Just (v, o') -> go o' fid nv v
          _ -> case skipCompactValue tt boolVal bs off' of
            Nothing -> Left "Parquet.Page: failed to skip unknown DataPageHeader field"
            Just o' -> go o' fid nv enc

decodeDictionaryPageHeader :: ByteString -> Int -> Either String (DictionaryPageHeader, Int)
decodeDictionaryPageHeader !bs !off0 = go off0 0 (-1) 0
  where
    go !off !lastFid !nv !enc =
      case tCompDecodeFieldBegin bs off lastFid of
        Nothing -> Left "Parquet.Page: malformed DictionaryPageHeader field header"
        Just (TT_STOP, _, off', _) ->
          if nv < 0
            then Left "Parquet.Page: missing field DictionaryPageHeader.num_values"
            else Right (DictionaryPageHeader { dictNumValues = nv, dictEncoding = enc }, off')
        Just (tt, fid, off', boolVal) -> case (fid, tt) of
          (1, TT_I32) -> case tCompDecodeI32 bs off' of
            Nothing -> Left "Parquet.Page: malformed DictionaryPageHeader.num_values"
            Just (v, o') -> go o' fid v enc
          (2, TT_I32) -> case tCompDecodeI32 bs off' of
            Nothing -> Left "Parquet.Page: malformed DictionaryPageHeader.encoding"
            Just (v, o') -> go o' fid nv v
          _ -> case skipCompactValue tt boolVal bs off' of
            Nothing -> Left "Parquet.Page: failed to skip unknown DictionaryPageHeader field"
            Just o' -> go o' fid nv enc

decodeDataPageHeaderV2 :: ByteString -> Int -> Either String (DataPageHeaderV2, Int)
decodeDataPageHeaderV2 !bs !off0 = go off0 0 (-1) (-1) (-1) (-1) (-1) (-1) True
  where
    -- Mandatory fields are -1-sentinelled; checked at STOP.
    go !off !lastFid !nv !nn !nr !enc !dl !rl !comp =
      case tCompDecodeFieldBegin bs off lastFid of
        Nothing -> Left "Parquet.Page: malformed DataPageHeaderV2 field header"
        Just (TT_STOP, _, off', _) ->
          let need x m =
                if x < 0
                  then Left ("Parquet.Page: missing field DataPageHeaderV2." ++ m)
                  else Right ()
          in do
            need nv  "num_values"
            need nn  "num_nulls"
            need nr  "num_rows"
            need enc "encoding"
            need dl  "definition_levels_byte_length"
            need rl  "repetition_levels_byte_length"
            Right ( DataPageHeaderV2
                      { dph2NumValues    = nv
                      , dph2NumNulls     = nn
                      , dph2NumRows      = nr
                      , dph2Encoding     = enc
                      , dph2DefLevelsLen = dl
                      , dph2RepLevelsLen = rl
                      , dph2IsCompressed = comp
                      }
                  , off'
                  )
        Just (tt, fid, off', boolVal) -> case (fid, tt) of
          (1, TT_I32) -> step32 off' fid (\v o -> go o fid v nn nr enc dl rl comp)
          (2, TT_I32) -> step32 off' fid (\v o -> go o fid nv v nr enc dl rl comp)
          (3, TT_I32) -> step32 off' fid (\v o -> go o fid nv nn v enc dl rl comp)
          (4, TT_I32) -> step32 off' fid (\v o -> go o fid nv nn nr v dl rl comp)
          (5, TT_I32) -> step32 off' fid (\v o -> go o fid nv nn nr enc v rl comp)
          (6, TT_I32) -> step32 off' fid (\v o -> go o fid nv nn nr enc dl v comp)
          (7, TT_BOOL) -> go off' fid nv nn nr enc dl rl boolVal
          _ -> case skipCompactValue tt boolVal bs off' of
            Nothing -> Left "Parquet.Page: failed to skip unknown DataPageHeaderV2 field"
            Just o' -> go o' fid nv nn nr enc dl rl comp

    step32 off' _ cont = case tCompDecodeI32 bs off' of
      Nothing -> Left "Parquet.Page: malformed I32 field in DataPageHeaderV2"
      Just (v, o') -> cont v o'

-- | Skip one compact-protocol value of the given type. Used to
-- step over fields the consumer doesn't need (CRC, Statistics).
-- Mirrors the @skipField@ helpers in @Thrift.Decode@ but
-- returns @Maybe Int@ instead of allocating a 'TV.Value'.
skipCompactValue :: ThriftType -> Bool -> ByteString -> Int -> Maybe Int
skipCompactValue !tt !_boolVal !bs !off = case tt of
  TT_STOP   -> Just off
  TT_BOOL   -> Just off  -- bool is encoded into the field header
  TT_BYTE   -> Just (off + 1)
  TT_I16    -> snd <$> tCompDecodeVarint bs off
  TT_I32    -> snd <$> tCompDecodeVarint bs off
  TT_I64    -> snd <$> tCompDecodeVarint bs off
  TT_DOUBLE -> Just (off + 8)
  TT_STRING -> case tCompDecodeVarint bs off of
    Nothing -> Nothing
    Just (lenW, o1) ->
      let !len = fromIntegral lenW :: Int
      in if len < 0 || o1 + len > BS.length bs
           then Nothing
           else Just (o1 + len)
  TT_STRUCT -> skipStruct bs off 0
  TT_LIST   -> case tCompDecodeListBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o1) -> skipNValues et (fromIntegral sz) bs o1
  TT_SET    -> case tCompDecodeSetBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o1) -> skipNValues et (fromIntegral sz) bs o1
  TT_MAP    -> case tCompDecodeMapBegin bs off of
    Nothing -> Nothing
    Just (kt, vt, sz, o1)
      | sz <= 0 -> Just o1
      | otherwise -> skipMapEntries kt vt (fromIntegral sz) bs o1
  TT_UUID   -> Just (off + 16)

skipStruct :: ByteString -> Int -> Int -> Maybe Int
skipStruct !bs !off !lastFidI =
  case tCompDecodeFieldBegin bs off (fromIntegral lastFidI) of
    Nothing -> Nothing
    Just (TT_STOP, _, off', _) -> Just off'
    Just (tt, fid, off', boolVal) -> case skipCompactValue tt boolVal bs off' of
      Nothing -> Nothing
      Just o' -> skipStruct bs o' (fromIntegral fid)

skipNValues :: ThriftType -> Int -> ByteString -> Int -> Maybe Int
skipNValues !tt !n !bs !off
  | n <= 0    = Just off
  | otherwise = case skipCompactValue tt False bs off of
      Nothing -> Nothing
      Just o' -> skipNValues tt (n - 1) bs o'

skipMapEntries :: ThriftType -> ThriftType -> Int -> ByteString -> Int -> Maybe Int
skipMapEntries !kt !vt !n !bs !off
  | n <= 0    = Just off
  | otherwise = case skipCompactValue kt False bs off of
      Nothing -> Nothing
      Just o1 -> case skipCompactValue vt False bs o1 of
        Nothing -> Nothing
        Just o2 -> skipMapEntries kt vt (n - 1) bs o2
