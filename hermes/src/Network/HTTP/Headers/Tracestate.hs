{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
@tracestate@ request\/response header — W3C Trace Context.

Carries vendor-specific trace identification data as an ordered,
comma-separated list of opaque @key=value@ members:

@
tracestate  = list-member 0*31( OWS \",\" OWS list-member )
list-member = key \"=\" value / OWS
key         = simple-key / tenant-id \"@\" system-id
value       = 0*255(chr) nblk-chr
@

Members from repeated field lines are combined in received order.
Values retain leading and internal spaces. Blank members are accepted
and omitted from the canonical serialization.

Spec: <https://www.w3.org/TR/trace-context/#tracestate-header>.

See also: "Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.XCorrelationID".
-}
module Network.HTTP.Headers.Tracestate (
  Tracestate (..),
  TracestateMember (..),
  tracestateMemberList,
  tracestateParser,
  renderTracestate,
) where

import Data.Bits (complement, finiteBitSize, shiftL, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text.Short as ST
import Foreign.Ptr (plusPtr)
import GHC.Exts (Addr#, Int (I#), eqAddr#, indexWord8ArrayAsWord64#, indexWord8OffAddr#, indexWord8OffAddrAsWord64#, isTrue#, minusAddr#, plusAddr#, (<=#))
import GHC.Word (Word64 (W64#), Word8 (W8#))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTracestate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderSeparated, shortText)
import Wireform.Encode.Direct (dWord8, directEncode)
import Wireform.Parser.Internal (Parser (..), pattern Err#, pattern Fail#, pattern OK#)


-- | A single @key=value@ list member.
data TracestateMember = TracestateMember
  { tracestateKey :: !ST.ShortText
  , tracestateValue :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | The ordered @tracestate@ members, or a present but empty field.
data Tracestate
  = Tracestate {tracestateMembers :: !(NE.NonEmpty TracestateMember)}
  | TracestateEmpty
  deriving stock (Eq, Show)


instance KnownHeader Tracestate where
  type ParseFailure Tracestate = String
  type Cardinality Tracestate = 'ZeroOrOne
  type Direction Tracestate = 'RequestAndResponse


  parseFromHeaders _ (wire NE.:| []) = parseFieldLine "tracestate" tracestateParser wire
  parseFromHeaders _ fields
    | withinMemberLimit 32 (NE.toList fields) =
        parseListFieldLines "tracestate" tracestateList fields >>= verifyMembers
    | otherwise = Left "Tracestate exceeds 32 list members"


  renderToHeaders _ = renderTracestateField


  renderToHeadersChecked _ = encodeTracestateChecked


  headerName _ = hTracestate


-- | The ordered members of either a populated or an empty field.
tracestateMemberList :: Tracestate -> [TracestateMember]
tracestateMemberList TracestateEmpty = []
tracestateMemberList (Tracestate members) = NE.toList members


tracestateParser :: ParserT st String Tracestate
tracestateParser = tracestateList >>= either err pure . verifyMembers


tracestateList :: ParserT st String [TracestateMember]
tracestateList = ows *> members (32 :: Int)
  where
    comma = $(char ',') *> ows
    members 0 = err "Tracestate exceeds 32 list members"
    members slots = branch comma (members (slots - 1)) (populated <|> pure [])
      where
        populated = do
          member <- tracestateMember <* ows
          rest <- withOption comma (const (members (slots - 1))) (pure [])
          pure (member : rest)


-- Each line contributes one member even when blank. Commas inside values
-- are forbidden, so there is no quoting to account for when counting.
withinMemberLimit :: Int -> [ByteString] -> Bool
withinMemberLimit _ [] = True
withinMemberLimit slots (wire : rest) =
  let commas = BS.count 44 wire
  in commas < slots && withinMemberLimit (slots - commas - 1) rest


verifyMembers :: [TracestateMember] -> Either String Tracestate
verifyMembers [] = Right TracestateEmpty
verifyMembers (first : rest)
  | distinctKeys first rest = Right (Tracestate (first NE.:| rest))
  | otherwise = Left "Duplicate tracestate key"


distinctKeys :: TracestateMember -> [TracestateMember] -> Bool
distinctKeys _ [] = True
distinctKeys first [second] = tracestateKey first /= tracestateKey second
distinctKeys first rest = packedKeys IntSet.empty 0 (first : rest)
  where
    -- For short ASCII keys each byte fits into a machine integer. The
    -- leading 1 distinguishes keys of different lengths.
    packedKeys !seen !count [] = IntSet.size seen == count
    packedKeys !seen !count (entry : pending)
      | width <= (finiteBitSize (0 :: Int) - 1) `quot` 8 =
          packedKeys (IntSet.insert (packKey 0 1) seen) (count + 1) pending
      | otherwise = Set.size (Set.fromList (map tracestateKey (first : rest))) == 1 + length rest
      where
        bytes = ST.toShortByteString (tracestateKey entry)
        width = SBSI.length bytes
        packKey !offset !key
          | offset == width = key
          | otherwise = packKey (offset + 1) ((key `shiftL` 8) .|. fromIntegral (SBSI.unsafeIndex bytes offset))


tracestateMember :: ParserT st String TracestateMember
tracestateMember = TracestateMember <$> stateKey <*> ($(char '=') *> stateValue)


stateKey :: ParserT st String ST.ShortText
stateKey = shortASCIIFromParser_ $ Parser $ \_ end start st ->
  let invalid = (# st, Fail# #)
      byteAt addr = W8# (indexWord8OffAddr# addr 0#)
      simpleKey first stop =
        if lowerByte first && isTrue# (minusAddr# stop start <=# 256#)
          then (# st, OK# () stop #)
          else invalid
      systemId begin
        | not (isTrue# (eqAddr# begin end)) && lowerByte (byteAt begin) =
            let stop = scanStateRun keyByte end (plusAddr# begin 1#)
            in if isTrue# (minusAddr# stop begin <=# 14#)
                 then (# st, OK# () stop #)
                 else invalid
        | otherwise = invalid
  in if isTrue# (eqAddr# start end)
       then invalid
       else
         let first = byteAt start
         in if lowerByte first || digitByte first
              then
                let stop = scanStateRun keyByte end (plusAddr# start 1#)
                in if not (isTrue# (eqAddr# stop end)) && byteAt stop == 64
                     then
                       if isTrue# (minusAddr# stop start <=# 241#)
                         then systemId (plusAddr# stop 1#)
                         else invalid
                     else simpleKey first stop
              else invalid
{-# INLINE stateKey #-}


keyByte :: Word8 -> Bool
keyByte b = lowerByte b || digitByte b || b == 95 || b == 45 || b == 42 || b == 47
{-# INLINE keyByte #-}


lowerByte :: Word8 -> Bool
lowerByte b = b - 97 <= 25
{-# INLINE lowerByte #-}


digitByte :: Word8 -> Bool
digitByte b = b - 48 <= 9
{-# INLINE digitByte #-}


-- Capture just the value. The list parser's OWS consumes the trimmed
-- spaces, so there is no second ByteString slice to keep or copy.
stateValue :: ParserT st String ST.ShortText
stateValue = shortASCIIFromParser_ $ Parser $ \_ end start st ->
  let scan cur
        | isTrue# (8# <=# minusAddr# end cur)
        , valueWord (W64# (indexWord8OffAddrAsWord64# cur 0#)) =
            scan (plusAddr# cur 8#)
        | otherwise = scanStateRun valueByte end cur
      stop = scan start
      trim cur
        | isTrue# (eqAddr# cur start) = cur
        | W8# (indexWord8OffAddr# cur (-1#)) == 32 = trim (plusAddr# cur (-1#))
        | otherwise = cur
      trimmed = trim stop
  in if isTrue# (eqAddr# stop start)
       then (# st, Fail# #)
       else
         if isTrue# (eqAddr# trimmed start) || not (isTrue# (minusAddr# trimmed start <=# 256#))
           then (# st, Err# "Invalid tracestate value length" #)
           else (# st, OK# () trimmed #)
{-# INLINE stateValue #-}


valueByte :: Word8 -> Bool
valueByte b = b >= 32 && b <= 126 && b /= 44 && b /= 61
{-# INLINE valueByte #-}


-- Check printable ASCII in parallel, then exclude comma and equals.
-- The high bit bias prevents cross-byte borrows in the range comparisons.
valueWord :: Word64 -> Bool
valueWord bytes =
  rangeBits .&. complement bytes .&. high == high
    && excludes 0x2c2c_2c2c_2c2c_2c2c
    && excludes 0x3d3d_3d3d_3d3d_3d3d
  where
    high = 0x8080_8080_8080_8080
    low = bytes .&. complement high
    rangeBits = ((bytes .|. high) - 0x2020_2020_2020_2020) .&. (0xfefe_fefe_fefe_fefe - low)
    excludes repeated =
      let diff = bytes `xor` repeated
      in (diff - 0x0101_0101_0101_0101) .&. complement diff .&. high == 0
{-# INLINE valueWord #-}


scanStateRun :: (Word8 -> Bool) -> Addr# -> Addr# -> Addr#
scanStateRun predicate end = scan
  where
    scan cur
      | isTrue# (eqAddr# cur end) = cur
      | predicate (W8# (indexWord8OffAddr# cur 0#)) = scan (plusAddr# cur 1#)
      | otherwise = cur
{-# INLINE scanStateRun #-}


renderTracestate :: Tracestate -> M.Builder
renderTracestate = renderSeparated "," renderMember . tracestateMemberList
  where
    renderMember (TracestateMember key value) =
      shortText key <> M.char7 '=' <> shortText value


renderTracestateField :: Tracestate -> ByteString
renderTracestateField TracestateEmpty = BS.empty
renderTracestateField (Tracestate (first NE.:| rest)) = encodeTracestate size first rest
  where
    size = foldl' (\total entry -> addSize (addSize total 1) (memberSize entry)) (memberSize first) rest
    addSize = BSI.checkedAdd "tracestate"
    memberSize (TracestateMember key value) =
      addSize (addSize (SBSI.length (ST.toShortByteString key)) 1) (SBSI.length (ST.toShortByteString value))


encodeTracestateChecked :: Tracestate -> Maybe ByteString
encodeTracestateChecked TracestateEmpty = Just BS.empty
encodeTracestateChecked (Tracestate (first NE.:| rest)) = measure 32 0 first rest
  where
    -- At most 32 members of at most 513 bytes plus separators.
    measure :: Int -> Int -> TracestateMember -> [TracestateMember] -> Maybe ByteString
    measure !slots !size (TracestateMember key value) pending
      | slots > 0, validStateKey keyBytes, validStateValue valueBytes =
          let end = size + SBSI.length keyBytes + 1 + SBSI.length valueBytes
          in case pending of
               [] | distinctKeys first rest -> Just (encodeTracestate end first rest)
                  | otherwise -> Nothing
               next : more -> measure (slots - 1) (end + 1) next more
      | otherwise = Nothing
      where
        keyBytes = ST.toShortByteString key
        valueBytes = ST.toShortByteString value


validStateKey :: SBSI.ShortByteString -> Bool
validStateKey bytes = size > 0 && size <= 256 && (lowerByte first || digitByte first) && prefix 1
  where
    size = SBSI.length bytes
    at = SBSI.unsafeIndex bytes
    first = at 0
    prefix !offset
      | offset == size = lowerByte first
      | byte == 64 = offset <= 241 && system (offset + 1)
      | otherwise = keyByte byte && prefix (offset + 1)
      where
        byte = at offset
    system start = size > start && size - start <= 14 && lowerByte (at start)
      && scanCompactBytes keyByte bytes (start + 1)
{-# INLINE validStateKey #-}


validStateValue :: SBSI.ShortByteString -> Bool
validStateValue bytes@(SBSI.SBS array) =
  size > 0 && size <= 256 && SBSI.unsafeIndex bytes (size - 1) /= 32
    && (if size < 8 then scanCompactBytes valueByte bytes 0 else packed 0)
  where
    size = SBSI.length bytes
    lastWord = size - 8
    packed !offset
      | offset >= lastWord = check lastWord
      | otherwise = check offset && packed (offset + 8)
    check (I# offset) = valueWord (W64# (indexWord8ArrayAsWord64# array offset))
{-# INLINE validStateValue #-}


encodeTracestate :: Int -> TracestateMember -> [TracestateMember] -> ByteString
encodeTracestate size first rest = directEncode size $ \ptr offset ->
  writeMember ptr offset first >>= writeRest ptr rest
  where
    writeRest _ [] offset = pure offset
    writeRest ptr (entry : later) offset = do
      start <- dWord8 ptr offset 44
      end <- writeMember ptr start entry
      writeRest ptr later end

    writeMember ptr offset (TracestateMember key value) = do
      let keyBytes = ST.toShortByteString key
          valueBytes = ST.toShortByteString value
          keySize = SBSI.length keyBytes
          valueSize = SBSI.length valueBytes
      SBSI.copyToPtr keyBytes 0 (ptr `plusPtr` offset) keySize
      valueStart <- dWord8 ptr (offset + keySize) 61
      SBSI.copyToPtr valueBytes 0 (ptr `plusPtr` valueStart) valueSize
      pure (valueStart + valueSize)
