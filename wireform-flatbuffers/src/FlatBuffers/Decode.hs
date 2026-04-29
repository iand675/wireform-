{-# LANGUAGE BangPatterns #-}
-- | Schema-driven FlatBuffers binary decoder.
--
-- FlatBuffers is a schema-driven format: the wire encoding does not
-- carry per-field type tags, so decoding requires a parsed @.fbs@
-- schema. This module accepts a 'FlatBuffersSchema' and uses it to:
--
--   * walk the table's vtable and apply the schema's field types to
--     each cell (no @int32@ / @uint32@ / @float@ / @uoffset@ ambiguity),
--   * follow @uoffset_t@ references to nested tables, strings, and
--     vectors, decoding them against their declared element types,
--   * apply schema-declared default values when a field's vtable cell
--     is zero,
--   * verify the buffer's @file_identifier@ against the schema's
--     declared identifier (if any).
module FlatBuffers.Decode
  ( decode
  , fileIdentifier
  , hasFileIdentifier
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified FlatBuffers.Value as F
import FlatBuffers.Schema
import FlatBuffers.Internal

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Decode a buffer against a schema's declared @root_type@. The
-- schema must declare a @root_type@ that names a table.  If the schema
-- declares a @file_identifier@, the buffer's bytes 4..7 must match it
-- (otherwise a 'Left' is returned).
decode :: FlatBuffersSchema -> ByteString -> Either String F.Value
decode !schema !bs = do
  rootDef <- rootTableDef schema
  let !env = mkEnv schema
  case schemaFileIdentifier schema of
    Just ident
      | BS.length bs >= 8
      , let got = BS.take 4 (BS.drop 4 bs)
            want = padIdent ident
      , got /= want ->
          Left $ "FlatBuffers.Decode: expected file_identifier "
              ++ show want ++ " but got " ++ show got
    _ -> Right ()
  if BS.length bs < 4
    then Left "FlatBuffers.Decode: input too short"
    else do
      let !rootOff = fromIntegral (readLE32 bs 0) :: Int
      if rootOff <= 0 || rootOff >= BS.length bs
        then Left "FlatBuffers.Decode: invalid root offset"
        else decodeTable env rootDef bs rootOff

-- | Read the 4-byte file identifier embedded immediately after the
-- root offset, if any.  Returns 'Nothing' for buffers shorter than 8
-- bytes.
fileIdentifier :: ByteString -> Maybe ByteString
fileIdentifier !bs
  | BS.length bs < 8 = Nothing
  | otherwise = Just (BS.take 4 (BS.drop 4 bs))

-- | @hasFileIdentifier expected bs@ is 'True' iff @bs@ has at least 8
-- bytes and bytes 4..7 match @expected@ (zero-padded or truncated to
-- four bytes).
hasFileIdentifier :: ByteString -> ByteString -> Bool
hasFileIdentifier !expected !bs
  | BS.length bs < 8 = False
  | otherwise =
      let !got = BS.take 4 (BS.drop 4 bs)
          !want = padIdent expected
      in got == want

------------------------------------------------------------------------
-- Table / field decode
------------------------------------------------------------------------

decodeTable :: Env -> TableDef -> ByteString -> Int -> Either String F.Value
decodeTable env td bs tableOff = do
  ensure bs tableOff 4
  let !soff = fromIntegral (readLE32S bs tableOff) :: Int
      !vtOff = tableOff - soff
  ensure bs vtOff 4
  let !vtSize  = fromIntegral (readLE16 bs vtOff)       :: Int
      !inlineSz = fromIntegral (readLE16 bs (vtOff + 2)) :: Int
  if vtSize < 4 || inlineSz < 4
    then Left "FlatBuffers.Decode: malformed vtable"
    else do
      let !nCellsAvail = (vtSize - 4) `div` 2
          fieldDefs    = V.toList (tdFields td)
      cells <- traverseN (length fieldDefs) $ \i ->
        if i < nCellsAvail
          then do
            ensure bs (vtOff + 4 + i * 2) 2
            Right (fromIntegral (readLE16 bs (vtOff + 4 + i * 2)) :: Int)
          else
            -- Fields declared in the schema but absent from this older
            -- vtable are treated as absent (forward-compatibility).
            Right 0
      fields <- traverseN (length fieldDefs) $ \i -> do
        let !cell = cells !! i
            !tf   = fieldDefs !! i
        if cell == 0
          then if isReferenceType env (tfType tf)
                 then Right Nothing
                 else Right (Just (defaultForField env tf))
          else do
            let !addr = tableOff + cell
            v <- decodeFieldValue env (tfType tf) bs addr
            Right (Just v)
      Right (F.VTable (V.fromList fields))

decodeFieldValue :: Env -> FBType -> ByteString -> Int -> Either String F.Value
decodeFieldValue env ty bs addr = case ty of
  FTBool   -> do ensure bs addr 1; Right (F.VBool (rdByte bs addr /= 0))
  FTByte   -> do ensure bs addr 1; Right (F.VInt8  (fromIntegral (rdByte bs addr) :: Int8))
  FTUByte  -> do ensure bs addr 1; Right (F.VWord8 (rdByte bs addr))
  FTShort  -> do ensure bs addr 2; Right (F.VInt16  (fromIntegral (readLE16 bs addr) :: Int16))
  FTUShort -> do ensure bs addr 2; Right (F.VWord16 (readLE16 bs addr))
  FTInt    -> do ensure bs addr 4; Right (F.VInt32  (fromIntegral (readLE32 bs addr) :: Int32))
  FTUInt   -> do ensure bs addr 4; Right (F.VWord32 (readLE32 bs addr))
  FTLong   -> do ensure bs addr 8; Right (F.VInt64  (fromIntegral (readLE64 bs addr) :: Int64))
  FTULong  -> do ensure bs addr 8; Right (F.VWord64 (readLE64 bs addr))
  FTFloat  -> do ensure bs addr 4; Right (F.VFloat  (castWord32ToFloat (readLE32 bs addr)))
  FTDouble -> do ensure bs addr 8; Right (F.VDouble (castWord64ToDouble (readLE64 bs addr)))
  FTString -> do
    ensure bs addr 4
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeString bs target
  FTVector elemTy -> do
    ensure bs addr 4
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeVector env elemTy bs target
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      ensure bs addr 4
      let !uoff = fromIntegral (readLE32 bs addr) :: Int
          !target = addr + uoff
      decodeTable env td bs target
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> decodeStruct env sd bs addr
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> decodeFieldValue env (fedUnderlyingType ed) bs addr
        Nothing -> Left $ "FlatBuffers.Decode: unknown named type "
                       ++ T.unpack name

decodeString :: ByteString -> Int -> Either String F.Value
decodeString bs off = do
  ensure bs off 4
  let !len = fromIntegral (readLE32 bs off) :: Int
  ensure bs (off + 4) (len + 1)  -- +1 for trailing NUL
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 4) bs)
  case TE.decodeUtf8' raw of
    Right t -> Right (F.VString t)
    Left  _ -> Left "FlatBuffers.Decode: invalid UTF-8 in string"

decodeVector :: Env -> FBType -> ByteString -> Int -> Either String F.Value
decodeVector env ety bs off = do
  ensure bs off 4
  let !cnt = fromIntegral (readLE32 bs off) :: Int
      !elemSz = inlineSizeOfType env ety
      !payloadStart = off + 4
  ensure bs payloadStart (cnt * elemSz)
  items <- traverseN cnt $ \i -> do
    let !addr = payloadStart + i * elemSz
    decodeVectorElem env ety bs addr
  Right (F.VVector (V.fromList items))

decodeVectorElem :: Env -> FBType -> ByteString -> Int -> Either String F.Value
decodeVectorElem env ety bs addr = case ety of
  FTString -> do
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeString bs target
  FTVector inner -> do
    let !uoff = fromIntegral (readLE32 bs addr) :: Int
        !target = addr + uoff
    decodeVector env inner bs target
  FTNamed name -> case Map.lookup name (envTables env) of
    Just td -> do
      let !uoff = fromIntegral (readLE32 bs addr) :: Int
          !target = addr + uoff
      decodeTable env td bs target
    Nothing -> case Map.lookup name (envStructs env) of
      Just sd -> decodeStruct env sd bs addr
      Nothing -> case Map.lookup name (envEnums env) of
        Just ed -> decodeFieldValue env (fedUnderlyingType ed) bs addr
        Nothing -> Left $ "FlatBuffers.Decode: unknown named element "
                       ++ T.unpack name
  scalar -> decodeFieldValue env scalar bs addr

decodeStruct :: Env -> FBStructDef -> ByteString -> Int -> Either String F.Value
decodeStruct env sd bs off = do
  let go !pos [] acc = Right (reverse acc, pos)
      go !pos ((_, fty) : rest) acc = do
        v <- decodeFieldValue env fty bs pos
        let !sz = inlineSizeOfType env fty
        go (pos + sz) rest (v : acc)
  (vs, _) <- go off (V.toList (fsdFields sd)) []
  Right (F.VStruct (V.fromList vs))

------------------------------------------------------------------------
-- Low-level reads
------------------------------------------------------------------------

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readLE16 :: ByteString -> Int -> Word16
readLE16 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE16 #-}

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE32 #-}

readLE32S :: ByteString -> Int -> Int32
readLE32S bs off = fromIntegral (readLE32 bs off)
{-# INLINE readLE32S #-}

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off < 0 || off + n > BS.length bs =
      Left "FlatBuffers.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}
