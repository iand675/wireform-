{-# LANGUAGE BangPatterns #-}
-- | Amazon Ion binary encoding.
--
-- Encodes an 'Ion.Value.Value' to the Amazon Ion 1.0 binary format. The
-- output begins with the Ion Binary Version Marker (BVM,
-- @0xE0 0x01 0x00 0xEA@). Whenever the value contains any user symbols
-- (i.e. 'Ion.Value.Symbol' values, struct field names, or annotation
-- names) we emit an @$ion_symbol_table@ annotation struct immediately
-- after the BVM and replace every textual symbol with its SID per the
-- spec. Pure-scalar payloads with no user symbols skip the table.
module Ion.Encode
  ( encode
  ) where

import Data.Bits (shiftR, (.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64)

import Wireform.Encode.Direct (directEncode)
import qualified Ion.Value as I
import Ion.SymbolTable (SymbolTable)
import qualified Ion.SymbolTable as ST

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

encode :: I.Value -> ByteString
encode !val =
  let !st = collectSymbols val ST.empty
      !hasUserSymbols = ST.size st > 0
      !ltsBs = if hasUserSymbols then encodeSymbolTable st else BS.empty
      !valBs = encodeValue st val
      !sz = 4 + BS.length ltsBs + BS.length valBs
  in directEncode sz $ \p _ -> do
       _ <- writeBVM p 0
       _ <- writeRawBytes p 4 ltsBs
       _ <- writeRawBytes p (4 + BS.length ltsBs) valBs
       pure sz
{-# NOINLINE encode #-}

------------------------------------------------------------------------
-- Symbol-table construction
------------------------------------------------------------------------

-- | Walk a value tree, interning every user symbol (Symbol, struct field
-- name, and annotation name) into the supplied table.
collectSymbols :: I.Value -> SymbolTable -> SymbolTable
collectSymbols !val !st = case val of
  I.Null            -> st
  I.Bool _          -> st
  I.Int _           -> st
  I.Float _         -> st
  I.String _        -> st
  I.Blob _          -> st
  I.Clob _          -> st
  I.List vs         -> V.foldl' (flip collectSymbols) st vs
  I.Struct kvs      ->
    V.foldl'
      (\acc (k, v) ->
        let (_, acc') = ST.internSymbol k acc
        in collectSymbols v acc')
      st kvs
  I.Symbol t        -> snd (ST.internSymbol t st)
  I.Annotation a v  ->
    let (_, st') = ST.internSymbol a st
    in collectSymbols v st'

------------------------------------------------------------------------
-- Symbol-table emission
--
-- An Ion local symbol table is encoded as:
--
--   Annotation(SID=3 \"$ion_symbol_table\", Struct { SID=7 \"symbols\": List
--   [ String s1, String s2, ... ] })
--
-- All identifiers used inside the LST are themselves *system* SIDs from
-- the predefined Ion 1.0 system symbol table.
------------------------------------------------------------------------

systemSidIonSymbolTable :: Int
systemSidIonSymbolTable = 3

systemSidSymbols :: Int
systemSidSymbols = 7

encodeSymbolTable :: SymbolTable -> ByteString
encodeSymbolTable st =
  let !syms = ST.symbols st
      !symsBs = encodeListOfStrings syms
      !structBs = encodeStructWithOneField systemSidSymbols symsBs
  in encodeAnnotationWith systemSidIonSymbolTable structBs

-- | Wrap a value in a single-annotation envelope.
-- Wire format:
--   typeNibble=14, len = sz(annot_len) + sz(annots) + sz(value)
--   <annot_length> = VarUInt of total annot bytes
--   <annot> = VarUInt SID
--   <value> = inner bytes
encodeAnnotationWith :: Int -> ByteString -> ByteString
encodeAnnotationWith !sid !innerBs =
  let !annBs = varUIntBytes sid
      !annsBs = annBs
      !annLenBs = varUIntBytes (BS.length annsBs)
      !payload = annLenBs <> annsBs <> innerBs
      !payloadLen = BS.length payload
      !td = tdBytes 0x0E payloadLen
  in td <> payload

-- | Struct (type 13) containing a single (sid, value) field.
-- Note: the spec supports an "ordered struct" form (D when len=1) but we
-- use the unordered form, where field names are VarUInt SIDs.
encodeStructWithOneField :: Int -> ByteString -> ByteString
encodeStructWithOneField !fieldSid !valBs =
  let !sidBs = varUIntBytes fieldSid
      !payload = sidBs <> valBs
      !td = tdBytes 0x0D (BS.length payload)
  in td <> payload

-- | A list of UTF-8 strings (type 11 + 0x80 string TDs).
encodeListOfStrings :: [T.Text] -> ByteString
encodeListOfStrings ts =
  let !payload = mconcat (map encodeString ts)
      !td = tdBytes 0x0B (BS.length payload)
  in td <> payload

encodeString :: T.Text -> ByteString
encodeString t =
  let !bs = TE.encodeUtf8 t
  in tdBytes 0x08 (BS.length bs) <> bs

------------------------------------------------------------------------
-- Value emission
--
-- Values use the user-symbol table built in 'collectSymbols' to translate
-- 'Symbol' / struct field names / annotation names into SIDs.
------------------------------------------------------------------------

encodeValue :: SymbolTable -> I.Value -> ByteString
encodeValue st = go
  where
    go = \case
      I.Null         -> BS.singleton 0x0F
      I.Bool b       -> BS.singleton (if b then 0x11 else 0x10)
      I.Int n        -> encodeIonInt n
      I.Float d      -> encodeIonFloat d
      I.String t     -> encodeString t
      I.Blob bs      -> tdBytes 0x0A (BS.length bs) <> bs
      I.Clob bs      -> tdBytes 0x09 (BS.length bs) <> bs
      I.List vs      ->
        let !payload = V.foldl' (\acc v -> acc <> go v) BS.empty vs
            !td = tdBytes 0x0B (BS.length payload)
        in td <> payload
      I.Struct kvs   ->
        let !payload = V.foldl'
              (\acc (k, v) ->
                let !sid = case ST.lookupSid k st of
                      Just s -> s
                      Nothing -> 0  -- system SID 0 = null; should never occur
                in acc <> varUIntBytes sid <> go v) BS.empty kvs
            !td = tdBytes 0x0D (BS.length payload)
        in td <> payload
      I.Symbol t     ->
        let !sid = case ST.lookupSid t st of
              Just s -> s
              Nothing -> 0
            !mag = fromIntegral sid :: Word64
            !nb = magnitudeBytes mag
        in if sid == 0
             then BS.singleton 0x70  -- $0 (symbol with unknown text)
             else tdBytes 0x07 nb <> magnitudeBytes' mag nb
      I.Annotation ann inner ->
        let !sid = case ST.lookupSid ann st of
              Just s -> s
              Nothing -> 0
        in encodeAnnotationWith sid (go inner)

encodeIonInt :: Integral a => a -> ByteString
encodeIonInt n
  | n == 0 = BS.singleton 0x20
  | n > 0  =
      let !mag = fromIntegral n :: Word64
          !nb = magnitudeBytes mag
      in tdBytes 0x02 nb <> magnitudeBytes' mag nb
  | otherwise =
      let !mag = fromIntegral (negate n) :: Word64
          !nb = magnitudeBytes mag
      in tdBytes 0x03 nb <> magnitudeBytes' mag nb

encodeIonFloat :: Double -> ByteString
encodeIonFloat d
  | d == 0    = BS.singleton 0x40
  | otherwise =
      let !w = castDoubleToWord64 d
      in tdBytes 0x04 8 <> be64Bytes w

------------------------------------------------------------------------
-- Wire helpers (pure ByteString flavours)
------------------------------------------------------------------------

-- | The type-descriptor prefix for a typed value of the given length.
-- @typeNibble@ is 0..15.  Lengths < 14 are inlined into the low nibble;
-- longer payloads use @0x0E@ + a VarUInt length.
tdBytes :: Word8 -> Int -> ByteString
tdBytes !tn !len
  | len < 14  = BS.singleton ((tn `shiftL` 4) .|. fromIntegral len)
  | otherwise = BS.cons ((tn `shiftL` 4) .|. 0x0E) (varUIntBytes len)

-- | VarUInt encoding (big-endian, 7-bit groups, MSB=1 marks the last byte).
varUIntBytes :: Int -> ByteString
varUIntBytes n0 =
  let !sz = varUIntSize n0
      step !i
        | i == sz - 1 = fromIntegral ((n0 `shiftR` (7 * (sz - 1 - i))) .&. 0x7F .|. 0x80)
        | otherwise   = fromIntegral ((n0 `shiftR` (7 * (sz - 1 - i))) .&. 0x7F)
  in BS.pack (map step [0 .. sz - 1])

varUIntSize :: Int -> Int
varUIntSize n
  | n <= 0x7F       = 1
  | n <= 0x3FFF     = 2
  | n <= 0x1FFFFF   = 3
  | n <= 0x0FFFFFFF = 4
  | otherwise       = 5

magnitudeBytes :: Word64 -> Int
magnitudeBytes n
  | n <= 0xFF             = 1
  | n <= 0xFFFF           = 2
  | n <= 0xFFFFFF         = 3
  | n <= 0xFFFFFFFF       = 4
  | n <= 0xFFFFFFFFFF     = 5
  | n <= 0xFFFFFFFFFFFF   = 6
  | n <= 0xFFFFFFFFFFFFFF = 7
  | otherwise             = 8

magnitudeBytes' :: Word64 -> Int -> ByteString
magnitudeBytes' !mag !nb = BS.pack $ map oneByte [nb - 1, nb - 2 .. 0]
  where
    oneByte i = fromIntegral ((mag `shiftR` (i * 8)) .&. 0xFF)

be64Bytes :: Word64 -> ByteString
be64Bytes !w = BS.pack
  [ fromIntegral ((w `shiftR` 56) .&. 0xFF)
  , fromIntegral ((w `shiftR` 48) .&. 0xFF)
  , fromIntegral ((w `shiftR` 40) .&. 0xFF)
  , fromIntegral ((w `shiftR` 32) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8)  .&. 0xFF)
  , fromIntegral (w               .&. 0xFF)
  ]

------------------------------------------------------------------------
-- Direct-buffer helpers used only for the BVM prefix
------------------------------------------------------------------------

writeBVM :: Ptr Word8 -> Int -> IO Int
writeBVM p off = do
  pokeByteOff p off       (0xE0 :: Word8)
  pokeByteOff p (off + 1) (0x01 :: Word8)
  pokeByteOff p (off + 2) (0x00 :: Word8)
  pokeByteOff p (off + 3) (0xEA :: Word8)
  pure $! off + 4

writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len

