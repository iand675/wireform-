{-# LANGUAGE BangPatterns #-}

{- | Decode one Ion 1.0 binary user value, processing version markers and
local symbol tables. The dynamic 'I.Value' model supports a subset of Ion:
unsupported types and symbols whose text is unavailable return 'Left'.
-}
module Ion.Decode (decode) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Ion.Internal.Vector (manyVector)
import Ion.SymbolTable qualified as ST
import Ion.Value qualified as I
import Wireform.FFI (decodeTextFast)
import Wireform.Parser qualified as P
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Internal (Parser, Pure)


type D = Parser Pure String


-- Only the public boundary boxes the parser's success/failure result.
decode :: ByteString -> Either String I.Value
decode bs = case parseByteString document bs of
  Left e -> Left ("Ion.Decode: " <> show e)
  Right v -> Right v
{-# NOINLINE decode #-}


bvm :: D ()
bvm = P.byteString "\xe0\x01\x00\xea"


data Seen = NoValue | SeenValue !I.Value


data Top = UserValue !I.Value | LocalTable !ST.SymbolTable


document :: D I.Value
document = bvm >> top ST.systemTable NoValue
  where
    top !symbols !seen = do
      skipNOPs
      end <- P.atEnd
      if end
        then case seen of
          NoValue -> P.err "no user value after BVM"
          SeenValue v -> pure v
        else do
          td <- P.lookahead P.anyWord8
          if td == 0xe0
            then bvm >> top ST.systemTable seen
            else do
              item <-
                if td `shiftR` 4 == 14
                  then topAnnotation symbols
                  else UserValue <$> value symbols 0
              case item of
                LocalTable next -> top next seen
                UserValue (I.Symbol "$ion_1_0") -> top symbols seen
                UserValue v -> case seen of
                  NoValue -> top symbols (SeenValue v)
                  SeenValue _ -> P.err "multiple user values"


-- All lengths are nonnegative machine Ints. Check before shifting so an
-- attacker cannot wrap a length or SID into a small value.
varUInt :: D Int
varUInt = go 0
  where
    go !acc = P.withAnyWord8 $ \b -> do
      when (acc > maxBound `shiftR` 7) (P.err "VarUInt overflow")
      let !next = (acc `shiftL` 7) .|. fromIntegral (b .&. 0x7f)
      if b >= 0x80 then pure next else go next
{-# INLINE varUInt #-}


payloadLength :: Word8 -> D Int
payloadLength nibble
  | nibble < 14 = pure (fromIntegral nibble)
  | nibble == 14 = varUInt
  | otherwise = P.err "unexpected typed null"
{-# INLINE payloadLength #-}


-- NOPs do not create values, including inside lists. A struct NOP is
-- preceded by a field SID, which must be ignored even if it is unknown.
skipNOPs :: D ()
skipNOPs = do
  end <- P.atEnd
  unless end $ do
    td <- P.lookahead P.anyWord8
    when (td < 0x0f) $ do
      P.anyWord8_
      payloadLength td >>= P.skip
      skipNOPs


skipFieldNOPs :: D ()
skipFieldNOPs = do
  end <- P.atEnd
  unless end $ do
    td <- P.lookahead (varUInt >> P.anyWord8)
    when (td < 0x0f) $ do
      _ <- varUInt
      P.anyWord8_
      payloadLength td >>= P.skip
      skipFieldNOPs


resolve :: ST.SymbolTable -> Int -> D Text
resolve symbols sid = case ST.lookupSymbol symbols sid of
  Just text -> pure text
  Nothing -> P.err "symbol text is unavailable or SID is outside the current table"
{-# INLINE resolve #-}


magnitude :: Int -> D Word64
magnitude n
  | n > 8 = P.err "integer or symbol ID exceeds 64 bits"
  | otherwise = go n 0
  where
    go !remaining !acc
      | remaining == 0 = pure acc
      | otherwise = P.withAnyWord8 $ \b ->
          go (remaining - 1) ((acc `shiftL` 8) .|. fromIntegral b)
{-# INLINE magnitude #-}


symbol :: ST.SymbolTable -> Word8 -> D Text
symbol symbols nibble = do
  len <- payloadLength nibble
  sid <- magnitude len
  when (sid > fromIntegral (maxBound :: Int)) (P.err "symbol ID overflow")
  resolve symbols (fromIntegral sid)


utf8 :: Int -> D Text
utf8 n = do
  bytes <- P.takeBs n
  case decodeTextFast bytes of
    Left _ -> P.err "invalid UTF-8 string"
    Right text -> pure text
{-# INLINE utf8 #-}


value :: ST.SymbolTable -> Int -> D I.Value
value symbols depth = P.withAnyWord8 (valueTD symbols depth)


valueTD :: ST.SymbolTable -> Int -> Word8 -> D I.Value
valueTD symbols depth td
  | depth >= 512 = P.err "container nesting limit exceeded"
  | td == 0x0f = pure I.Null
  | nibble == 15 = P.err "typed null is not representable by Ion.Value"
  | otherwise = case td `shiftR` 4 of
      1 -> case nibble of
        0 -> pure (I.Bool False)
        1 -> pure (I.Bool True)
        _ -> P.err "invalid boolean descriptor"
      2 -> integer False
      3 -> integer True
      4 -> case nibble of
        0 -> pure (I.Float 0)
        4 -> I.Float . realToFrac <$> P.anyFloatbe
        8 -> I.Float <$> P.anyDoublebe
        _ -> P.err "invalid float descriptor"
      7 -> I.Symbol <$> symbol symbols nibble
      8 -> payloadLength nibble >>= fmap I.String . utf8
      9 -> payloadLength nibble >>= fmap I.Clob . P.takeBs
      10 -> payloadLength nibble >>= fmap I.Blob . P.takeBs
      11 -> do
        len <- payloadLength nibble
        I.List <$> P.isolate len (manyVector skipNOPs (value symbols (depth + 1)))
      13 -> do
        len <- structLength nibble
        I.Struct <$> P.isolate len (manyVector skipFieldNOPs field)
      14 -> do
        len <- payloadLength nibble
        P.isolate len $ do
          annotations <- annotationIDs symbols
          inner <- unwrapped symbols (depth + 1)
          pure (applyAnnotations annotations inner)
      _ -> P.err "unsupported Ion type or NOP where a value was required"
  where
    !nibble = td .&. 15
    integer negative = do
      len <- payloadLength nibble
      mag <- magnitude len
      let !limit = if negative then 0x8000000000000000 else 0x7fffffffffffffff
      when (mag > limit) (P.err "integer outside Int64 range")
      when (negative && mag == 0) (P.err "negative integer zero")
      pure (I.Int (if negative then negate (fromIntegral mag) else fromIntegral mag))
    field = do
      sid <- varUInt
      key <- resolve symbols sid
      v <- value symbols (depth + 1)
      pure (key, v)


-- Wrappers must enclose exactly one ordinary value: neither another
-- wrapper nor padding is legal. isolate also rejects extra wrapped bytes.
unwrapped :: ST.SymbolTable -> Int -> D I.Value
unwrapped symbols depth = P.withAnyWord8 $ \td ->
  if td `shiftR` 4 == 14 || td < 0x0f
    then P.err "annotation cannot wrap an annotation or NOP"
    else valueTD symbols depth td


structLength :: Word8 -> D Int
structLength 1 = do
  len <- varUInt
  when (len == 0) (P.err "ordered struct must be nonempty")
  pure len
structLength nibble = payloadLength nibble


annotationIDs :: ST.SymbolTable -> D (Vector Text)
annotationIDs symbols = do
  len <- varUInt
  when (len == 0) (P.err "annotation ID sequence must be nonempty")
  P.isolate len (manyVector (pure ()) (varUInt >>= resolve symbols))


applyAnnotations :: Vector Text -> I.Value -> I.Value
applyAnnotations annotations inner = V.foldr I.Annotation inner annotations


topAnnotation :: ST.SymbolTable -> D Top
topAnnotation symbols = P.withAnyWord8 $ \td -> do
  len <- payloadLength (td .&. 15)
  P.isolate len $ do
    annotations <- annotationIDs symbols
    innerTD <- P.lookahead P.anyWord8
    if V.unsafeHead annotations == "$ion_symbol_table" && innerTD `shiftR` 4 == 13
      then LocalTable <$> localTable symbols
      else UserValue . applyAnnotations annotations <$> unwrapped symbols 0


-- Metadata parsing retains slot positions and never interprets nonstring
-- symbol entries as text. Unknown fields are bounded-skipped, not decoded
-- into the smaller application Value model.
data Imports = Replace {-# UNPACK #-} !Int | Append


data TableFields = TableFields
  { slots :: !(Vector (Maybe Text))
  , imports :: !Imports
  , sawSymbols :: !Bool
  , sawImports :: !Bool
  }


localTable :: ST.SymbolTable -> D ST.SymbolTable
localTable previous = do
  fields <- P.withAnyWord8 $ \td ->
    if td == 0xdf
      then pure emptyFields
      else do
        len <- structLength (td .&. 15)
        P.isolate len (fieldsLoop emptyFields)
  case imports fields of
    Replace base -> do
      when (V.length (slots fields) > maxBound - base) (P.err "symbol table size overflow")
      pure (ST.SymbolTable base (slots fields))
    Append -> do
      let old = ST.localSymbols previous
          base = ST.localMinID previous
      when (V.length old > maxBound - base - V.length (slots fields)) (P.err "symbol table size overflow")
      pure (ST.SymbolTable base (old V.++ slots fields))
  where
    emptyFields = TableFields V.empty (Replace 10) False False
    fieldsLoop !fields = do
      skipFieldNOPs
      end <- P.atEnd
      if end
        then pure fields
        else do
          sid <- varUInt
          case ST.lookupSymbol previous sid of
            Just "symbols" -> do
              when (sawSymbols fields) (P.err "duplicate symbols field in local table")
              newSlots <- readSymbols previous
              fieldsLoop fields {slots = newSlots, sawSymbols = True}
            Just "imports" -> do
              when (sawImports fields) (P.err "duplicate imports field in local table")
              newImports <- readImports previous
              fieldsLoop fields {imports = newImports, sawImports = True}
            _ -> skipValue 0 >> fieldsLoop fields


readSymbols :: ST.SymbolTable -> D (Vector (Maybe Text))
readSymbols symbols = metadata symbols $ \td ->
  if td `shiftR` 4 == 11 && td .&. 15 /= 15
    then do
      len <- payloadLength (td .&. 15)
      P.isolate len (manyVector skipNOPs slot)
    else skipTD 0 td >> pure V.empty
  where
    slot = metadata symbols $ \td ->
      if td `shiftR` 4 == 8 && td .&. 15 /= 15
        then payloadLength (td .&. 15) >>= fmap Just . utf8
        else skipTD 0 td >> pure Nothing


readImports :: ST.SymbolTable -> D Imports
readImports symbols = metadata symbols $ \td -> case td `shiftR` 4 of
  7 | td .&. 15 /= 15 -> do
    text <- symbol symbols (td .&. 15)
    pure (if text == "$ion_symbol_table" then Append else Replace 10)
  11 | td .&. 15 /= 15 -> do
    len <- payloadLength (td .&. 15)
    sizes <- P.isolate len (manyVector skipNOPs (importSize symbols))
    Replace <$> V.foldM' addImport 10 sizes
  _ -> skipTD 0 td >> pure (Replace 10)
  where
    addImport !total !size
      | size > maxBound - total = P.err "shared imports overflow symbol ID range"
      | otherwise = pure (total + size)


data ImportFields = ImportFields
  { importName :: !Text
  , importMaxID :: {-# UNPACK #-} !Int64
  }


-- No catalog is supplied by this API. Imports with max_id reserve unknown
-- slots; imports without max_id must fail rather than silently renumbering
-- all following locals. Reservations do not allocate a vector of that size.
importSize :: ST.SymbolTable -> D Int
importSize symbols = metadata symbols $ \td ->
  if td `shiftR` 4 /= 13 || td == 0xdf
    then skipTD 0 td >> pure 0
    else do
      len <- structLength (td .&. 15)
      fields <- P.isolate len (go (ImportFields T.empty (-1)))
      if T.null (importName fields) || importName fields == "$ion"
        then pure 0
        else
          if importMaxID fields < 0
            then P.err "shared import requires max_id when no catalog is available"
            else
              if toInteger (importMaxID fields) > toInteger (maxBound :: Int)
                then P.err "import max_id overflow"
                else pure (fromIntegral (importMaxID fields))
  where
    go !fields = do
      skipFieldNOPs
      end <- P.atEnd
      if end
        then pure fields
        else do
          sid <- varUInt
          case ST.lookupSymbol symbols sid of
            Just "name" -> do
              text <- metadata symbols $ \td ->
                if td `shiftR` 4 == 8 && td /= 0x8f
                  then payloadLength (td .&. 15) >>= utf8
                  else skipTD 0 td >> pure T.empty
              go fields {importName = text}
            Just "max_id" -> do
              maxID <- metadata symbols $ \td ->
                if td `shiftR` 4 == 2 && td /= 0x2f
                  then do
                    v <- valueTD symbols 0 td
                    case v of
                      I.Int n -> pure n
                      _ -> P.err "invalid import max_id"
                  else skipTD 0 td >> pure (-1)
              go fields {importMaxID = maxID}
            _ -> skipValue 0 >> go fields


-- Annotations do not change a metadata value's underlying type. Retain
-- wrapper bounds while stripping them so annotated lists/strings are
-- interpreted exactly like their unannotated counterparts.
metadata :: ST.SymbolTable -> (Word8 -> D a) -> D a
metadata symbols parse = P.withAnyWord8 $ \td ->
  if td `shiftR` 4 /= 14
    then parse td
    else do
      len <- payloadLength (td .&. 15)
      P.isolate len $ do
        _ <- annotationIDs symbols
        P.withAnyWord8 $ \inner ->
          if inner `shiftR` 4 == 14 || inner < 0x0f
            then P.err "annotation cannot wrap an annotation or NOP"
            else parse inner


skipValue :: Int -> D ()
skipValue depth = P.withAnyWord8 (skipTD depth)


-- Skipped metadata may contain types that Value cannot represent. Their
-- explicit payload length still confines the skip to the enclosing field.
skipTD :: Int -> Word8 -> D ()
skipTD _ td
  | td `shiftR` 4 == 15 = P.err "reserved Ion type"
  | td `shiftR` 4 == 14 && (nibble < 3 || nibble == 15) = P.err "invalid annotation descriptor"
  | nibble == 15 = pure ()
  | td `shiftR` 4 == 1 = unless (nibble <= 1) (P.err "invalid boolean descriptor")
  | td `shiftR` 4 == 13 = structLength nibble >>= P.skip
  | otherwise = payloadLength nibble >>= P.skip
  where
    !nibble = td .&. 15
