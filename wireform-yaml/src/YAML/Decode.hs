{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | YAML 1.2 decoder.

Parses a YAML stream into a 'YAML.Value.Stream' / 'Document' /
'Value' according to the YAML 1.2 specification, applying the
/core schema/ for plain-scalar resolution. Supports:

* Block-style mappings and sequences (with flexible indentation).
* Flow-style mappings (@{a: b}@) and sequences (@[a, b]@), nested.
* Plain, single-quoted, and double-quoted scalars (with all
  YAML 1.2 escapes for the latter, including @\\xNN@, @\\uNNNN@,
  @\\UNNNNNNNN@).
* Block literal (@|@) and block folded (@>@) scalars with all three
  chomping indicators (@-@, @+@, default-clip) and optional
  indentation indicators.
* Anchors (@&name@) and aliases (@*name@). Aliases are expanded
  inline so that downstream code can treat the result as a tree.
* Explicit tags (@!!str@, @!!int@, @!\<tag:yaml.org,2002:bool\>@,
  etc.) with the standard short-hand expansions for the
  @tag:yaml.org,2002:@ family.
* Comments, blank lines, the @---@ document-start and @...@
  document-end markers, and multi-document streams.
-}
module YAML.Decode (
  decode,
  decodeBS,
  decodeStream,
  decodeStreamBS,
  decodeDocuments,
  LocatedDocument (..),
  decodeLocatedDocuments,
  resolveExplicitTag,
  preprocess,
  splitMappingLine,
) where

import Control.Monad (unless, when)
import Control.Monad.State.Strict (StateT, evalStateT, get, lift, modify')
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isDigit, isHexDigit)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Unsafe.Char qualified as TU
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (
  ByteArray#,
  Int (..),
  Int#,
  clz#,
  ctz#,
  indexWord8ArrayAsWord64#,
  uncheckedIShiftRL#,
  word2Int#,
  word64ToWord#,
 )
import GHC.Float (castDoubleToWord64)
import GHC.Generics (Generic)
import GHC.Word (Word64 (..))
import YAML.Annotated qualified as A
import YAML.Internal (stripYaml, stripYamlEnd, stripYamlStart, wordsYaml)
import YAML.Value


-- ---------------------------------------------------------------------------
-- Fast byte-indexed Text access. The flow parser walks a buffer
-- one byte at a time looking for ASCII control characters
-- ('[', ']', '{', '}', ',', ':', '#', '\\1', '\\2'); doing that
-- through 'T.index' / 'T.length' is O(p) / O(N) per call because
-- Text positions are character indices and the implementation has
-- to walk UTF-8 bytes to convert. Operating on the underlying
-- byte array directly gives O(1) access without changing the
-- semantics for ASCII-only control bytes (multi-byte UTF-8
-- sequences in scalar content all start with a leading byte ≥
-- 0x80 that never matches our ASCII stoppers).
-- ---------------------------------------------------------------------------

-- | Byte length of a 'Text' (O(1)).
bLen :: Text -> Int
bLen (TI.Text _ _ l) = l
{-# INLINE bLen #-}


{- | Byte at position @p@ inside a 'Text' (O(1)). The caller is
responsible for keeping @p@ in @[0, bLen t)@.
-}
bAt :: Text -> Int -> Word8
bAt (TI.Text arr off _) p = TA.unsafeIndex arr (off + p)
{-# INLINE bAt #-}


{- | Slice @t@ to the bytes @[s, e)@ (O(1)). The caller must
ensure both ends sit on UTF-8 character boundaries.
-}
bSlice :: Text -> Int -> Int -> Text
bSlice (TI.Text arr off _) s e = TI.text arr (off + s) (max 0 (e - s))
{-# INLINE bSlice #-}


{- | Drop the first @p@ bytes of @t@ (O(1)). Caller must ensure
@p@ lies on a UTF-8 character boundary.
-}
bDrop :: Int -> Text -> Text
bDrop p (TI.Text arr off l) =
  let !p' = min p l
  in TI.text arr (off + p') (l - p')
{-# INLINE bDrop #-}


{- | Take the first @p@ bytes of @t@ (O(1)). Caller must ensure
@p@ lies on a UTF-8 character boundary.
-}
bTake :: Int -> Text -> Text
bTake p (TI.Text arr off l) =
  let !p' = max 0 (min p l)
  in TI.text arr off p'
{-# INLINE bTake #-}


-- ---------------------------------------------------------------------------
-- SWAR (SIMD-Within-A-Register) byte scanners.
--
-- Most of the parser's hot inner loops are looking for a single
-- ASCII byte (e.g. '#', '\\1', '\"') in a 'Text' body. The
-- Data.Text 'T.any' / 'T.findIndex' walk one /character/ at a
-- time via the UTF-8 stream interface; we can do much better by
-- treating the underlying byte array as 64-bit words and using
-- the classic "has-zero-byte" bit trick:
--
--   hasZeroByte x = ((x - 0x0101010101010101) & ~x & 0x8080808080808080) /= 0
--
-- After XOR-ing the input word with a broadcast of the target
-- byte, any byte equal to the target becomes zero, so a single
-- 64-bit operation tells us whether the eight bytes contain it.
-- ---------------------------------------------------------------------------

bcast64 :: Word8 -> Word64
bcast64 b = fromIntegral b * 0x0101010101010101
{-# INLINE bcast64 #-}


{- | True iff @x@ contains any zero byte. False-positive-free for
the boolean answer (the standard 'has-zero-byte' trick is
reliable for the binary "any zero?" question even though its
per-byte mask is not).
-}
hasZeroByte :: Word64 -> Bool
hasZeroByte x =
  ( (x - 0x0101010101010101)
      Bits..&. Bits.complement x
      Bits..&. 0x8080808080808080
  )
    /= 0
{-# INLINE hasZeroByte #-}


{- | Per-byte mask: returns @0x80@ in every byte position where
the input byte was @0x00@ and @0x00@ everywhere else. Robust
against the borrow-propagation false positives of the simpler
@(x-ones)&~x&hi@ formula (Hacker's Delight §6-1, "find a
specific byte").
-}
zeroByteMask :: Word64 -> Word64
zeroByteMask x =
  let !lo7 = 0x7F7F7F7F7F7F7F7F :: Word64
      !his = 0x8080808080808080 :: Word64
      !t = ((x Bits..&. lo7) + lo7) Bits..|. x
  in -- 'hi-bit set' iff one of:
     --   * x had bit 7 set (so byte was >= 0x80, i.e. not 0x00)
     --   * (low 7 bits of x) + 0x7F had bit 7 set (i.e. low 7
     --     bits weren't all zero, i.e. byte was non-zero in 1..127)
     -- Either way 'hi-bit set' means "non-zero". Inverting and
     -- ANDing with 'his' picks out the zero bytes only.
     Bits.complement t Bits..&. his
{-# INLINE zeroByteMask #-}


{- | Read 8 unaligned bytes from a byte array starting at byte
index @i@ as a single 'Word64'.
-}
indexWord64Unaligned :: ByteArray# -> Int -> Word64
indexWord64Unaligned ba# (I# i#) = W64# (indexWord8ArrayAsWord64# ba# i#)
{-# INLINE indexWord64Unaligned #-}


{- | True when @t@ contains the byte @c@. Pure byte-level scan
(no UTF-8 decoding) using a Word64-at-a-time loop.
-}
bAnyByte :: Word8 -> Text -> Bool
bAnyByte !c (TI.Text (TA.ByteArray ba#) off len) =
  let !pat = bcast64 c
      !endB = off + len
      goWord !i
        | i + 8 > endB = goByte i
        | otherwise =
            let !w = indexWord64Unaligned ba# i
                !xored = w `Bits.xor` pat
            in if hasZeroByte xored
                 then True
                 else goWord (i + 8)
      goByte !i
        | i >= endB = False
        | TA.unsafeIndex (TA.ByteArray ba#) i == c = True
        | otherwise = goByte (i + 1)
  in goWord off
{-# INLINE bAnyByte #-}


-- 0: printable ASCII with no CR; 1: the same with CR line breaks;
-- 2: Unicode or a potentially invalid control, requiring lexical validation.
-- The word loop avoids decoding ASCII as Unicode twice on ordinary documents.
asciiSourceStatus :: Text -> Int#
asciiSourceStatus (TI.Text array@(TA.ByteArray bytes#) offset size) =
  case wordLoop offset 0 of I# result -> result
  where
    end = offset + size
    wordLoop !position !status
      | position + 8 > end = byteLoop position end status
      | otherwise =
          let word = indexWord64Unaligned bytes# position
          in if word Bits..&. 0x8080808080808080 /= 0
               then 2
               else
                 if hasZeroByte (word Bits..&. 0xE0E0E0E0E0E0E0E0) || hasZeroByte (word `Bits.xor` 0x7F7F7F7F7F7F7F7F)
                   then case byteLoop position (position + 8) status of
                     2 -> 2
                     nextStatus -> wordLoop (position + 8) nextStatus
                   else wordLoop (position + 8) status
    byteLoop !position !limit !status
      | position >= limit = status
      | otherwise = case TA.unsafeIndex array position of
          13 -> byteLoop (position + 1) limit 1
          9 -> byteLoop (position + 1) limit status
          10 -> byteLoop (position + 1) limit status
          byte
            | byte >= 32 && byte < 127 -> byteLoop (position + 1) limit status
            | otherwise -> 2
{-# INLINE asciiSourceStatus #-}


{- | First matching byte in memory order. A native word load places the
first byte at opposite ends on little- and big-endian hosts. The target
byte order is a compile-time constant, so the unused branch disappears.
-}
wordFirstMatchByte :: Word64 -> Int
wordFirstMatchByte (W64# word#) = case targetByteOrder of
  LittleEndian -> I# (uncheckedIShiftRL# (word2Int# (ctz# (word64ToWord# word#))) 3#)
  BigEndian -> I# (uncheckedIShiftRL# (word2Int# (clz# (word64ToWord# word#))) 3#)
{-# INLINE wordFirstMatchByte #-}


{- | First index of /any/ of three bytes in @t@, or @-1@ when
none is present. Three robust SWAR scans run on the same
Word64 per iteration; the OR-combined per-byte mask is then
'ctz'-ed for the in-word offset of the first match.
-}
bFindAnyOf3 :: Word8 -> Word8 -> Word8 -> Text -> Int
bFindAnyOf3 !a !b !c (TI.Text (TA.ByteArray ba#) off len) =
  let !pa = bcast64 a
      !pb = bcast64 b
      !pc = bcast64 c
      !endB = off + len
      goWord !i
        | i + 8 > endB = goByte i
        | otherwise =
            let !w = indexWord64Unaligned ba# i
                !m =
                  zeroByteMask (w `Bits.xor` pa)
                    Bits..|. zeroByteMask (w `Bits.xor` pb)
                    Bits..|. zeroByteMask (w `Bits.xor` pc)
            in if m == 0
                 then goWord (i + 8)
                 else i + wordFirstMatchByte m - off
      goByte !i
        | i >= endB = -1
        | otherwise =
            let !x = TA.unsafeIndex (TA.ByteArray ba#) i
            in if x == a || x == b || x == c
                 then i - off
                 else goByte (i + 1)
  in goWord off
{-# INLINE bFindAnyOf3 #-}


-- | First index of either of two bytes. See 'bFindAnyOf3'.
bFindAnyOf2 :: Word8 -> Word8 -> Text -> Int
bFindAnyOf2 !a !b (TI.Text (TA.ByteArray ba#) off len) =
  let !pa = bcast64 a
      !pb = bcast64 b
      !endB = off + len
      goWord !i
        | i + 8 > endB = goByte i
        | otherwise =
            let !w = indexWord64Unaligned ba# i
                !m =
                  zeroByteMask (w `Bits.xor` pa)
                    Bits..|. zeroByteMask (w `Bits.xor` pb)
            in if m == 0
                 then goWord (i + 8)
                 else i + wordFirstMatchByte m - off
      goByte !i
        | i >= endB = -1
        | otherwise =
            let !x = TA.unsafeIndex (TA.ByteArray ba#) i
            in if x == a || x == b
                 then i - off
                 else goByte (i + 1)
  in goWord off
{-# INLINE bFindAnyOf2 #-}


-- | Word8 character literals of common ASCII control bytes.
w8Comma
  , w8Colon
  , w8LBrack
  , w8RBrack
  , w8LBrace
  , w8RBrace
  , w8DQuote
  , w8SQuote
  , w8Hash
  , w8Space
  , w8Tab
  , w8Quest
  , w8SOH
  , w8STX
  , w8Backslash
    :: Word8
w8Comma = 44
w8Colon = 58
w8LBrack = 91
w8RBrack = 93
w8LBrace = 123
w8RBrace = 125
w8DQuote = 34
w8SQuote = 39
w8Hash = 35
w8Space = 32
w8Tab = 9
w8Quest = 63
w8SOH = 1 -- '\\1'
w8STX = 2 -- '\\2'
w8Backslash = 92


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

decode :: Text -> Either String Value
decode t = case decodeDocuments t of
  Left err -> Left err
  Right [] -> Right YNull
  Right [d] -> Right (docBody d)
  Right _ -> Left "YAML: source contains multiple documents; use decodeStream"


decodeBS :: ByteString -> Either String Value
decodeBS bs = decodeSourceBS bs >>= decode


decodeStream :: Text -> Either String Stream
decodeStream t = (Stream . V.fromList) <$> decodeDocuments t


decodeStreamBS :: ByteString -> Either String Stream
decodeStreamBS bs = decodeSourceBS bs >>= decodeStream


-- The first bytes distinguish UTF-8, UTF-16 and UTF-32 even without a BOM.
-- Validate wide encodings before using text's partial decoding functions.
decodeSourceBS :: ByteString -> Either String Text
decodeSourceBS bytes
  | prefix [0, 0, 254, 255] || (len >= 4 && byte 0 == 0 && byte 1 == 0 && byte 2 == 0) = wide32 False
  | prefix [255, 254, 0, 0] || (len >= 4 && byte 1 == 0 && byte 2 == 0 && byte 3 == 0) = wide32 True
  | prefix [254, 255] || (len >= 2 && byte 0 == 0) = wide16 False
  | prefix [255, 254] || (len >= 2 && byte 1 == 0) = wide16 True
  | otherwise = either (Left . show) Right (TE.decodeUtf8' bytes)
  where
    len = BS.length bytes
    byte = BS.index bytes
    prefix = (`BS.isPrefixOf` bytes) . BS.pack
    unit :: Bool -> Int -> Int
    unit little i =
      let a = fromIntegral (byte i)
          b = fromIntegral (byte (i + 1))
      in if little then a + b * 256 else a * 256 + b
    wide16 little
      | len `rem` 2 /= 0 || not (valid16 0) = Left "YAML: invalid UTF-16 input"
      | otherwise = Right ((if little then TE.decodeUtf16LE else TE.decodeUtf16BE) bytes)
      where
        valid16 i
          | i >= len = True
          | u >= 0xD800 && u <= 0xDBFF = i + 3 < len && let low = unit little (i + 2) in low >= 0xDC00 && low <= 0xDFFF && valid16 (i + 4)
          | u >= 0xDC00 && u <= 0xDFFF = False
          | otherwise = valid16 (i + 2)
          where
            u = unit little i :: Int
    wide32 little
      | len `rem` 4 /= 0 || not (valid32 0) = Left "YAML: invalid UTF-32 input"
      | otherwise = Right ((if little then TE.decodeUtf32LE else TE.decodeUtf32BE) bytes)
      where
        valid32 i
          | i >= len = True
          | otherwise =
              let a = unit little i :: Int
                  b = unit little (i + 2)
                  cp = if little then a + b * 65536 else a * 65536 + b
              in cp <= 0x10FFFF && (cp < 0xD800 || cp > 0xDFFF) && valid32 (i + 4)


decodeDocuments :: Text -> Either String [Document]
decodeDocuments src = prepareSource src >>= parseStream . preprocess


prepareSource :: Text -> Either String Text
prepareSource src = case asciiSourceStatus src of
  0# -> Right src
  1# -> Right (normalizeLineBreaks src)
  _
    | T.any suspicious src && not (validRawCharacters (RawOutside True True) src) -> Left "YAML: invalid unescaped character in input"
    | otherwise -> Right (normalizeLineBreaks src)
  where
    suspicious c = invalidC0 c || c >= '\x7F' && c < '\xA0' && c /= '\x85' || c == '\xFFFE' || c == '\xFFFF'


invalidC0 :: Char -> Bool
invalidC0 c = c < ' ' && c /= '\t' && c /= '\n' && c /= '\r'


data RawContext = RawOutside !Bool !Bool | RawComment | RawSingle | RawDouble | RawEscape


-- Only needed when the source contains a non-printable code point. YAML's
-- JSON-compatible quoted scalars allow non-C0 code points that comments and
-- plain/block scalars forbid. The structural parser validates the latter too.
validRawCharacters :: RawContext -> Text -> Bool
validRawCharacters context source = case T.uncons source of
  Nothing -> True
  Just (c, rest)
    | invalidC0 c -> False
    | otherwise -> case context of
        RawDouble -> go (case c of '"' -> RawOutside False False; '\\' -> RawEscape; _ -> RawDouble) rest
        RawEscape -> go RawDouble rest
        RawSingle
          | c == '\'' -> case T.uncons rest of
              Just ('\'', after) -> go RawSingle after
              _ -> go (RawOutside False False) rest
          | otherwise -> go RawSingle rest
        RawComment -> printable c && go (if c == '\n' then RawOutside True True else RawComment) rest
        RawOutside canStart separated
          | not (printable c) -> False
          | c == '\n' || c == '\r' -> go (RawOutside True True) rest
          | canStart && c == '\xFEFF' -> go (RawOutside True separated) rest
          | c == ' ' || c == '\t' -> go (RawOutside canStart True) rest
          | canStart && c == '"' -> go RawDouble rest
          | canStart && c == '\'' -> go RawSingle rest
          | separated && c == '#' -> go RawComment rest
          | canStart && (c == '!' || c == '&') ->
              let (token, after) = T.break (\char -> char == ' ' || char == '\t' || char == '\n' || char == '\r') rest
              in T.all printable token && go (RawOutside True True) after
          | c `T.elem` "[,{" -> go (RawOutside True False) rest
          | canStart && c == '-', Just after <- T.stripPrefix "--" rest, followsSeparator after -> go (RawOutside True False) after
          | c == ':' && followsSeparator rest -> go (RawOutside True False) rest
          | canStart && c `T.elem` "-?" && followsSeparator rest -> go (RawOutside True False) rest
          | otherwise -> go (RawOutside False False) rest
  where
    go = validRawCharacters
    printable c = c == '\n' || c == '\r' || c == '\xFEFF' || validNonBreak c
    followsSeparator text = case T.uncons text of
      Nothing -> True
      Just (c, _) -> c `T.elem` " \t\n\r,]}"


validNonBreak :: Char -> Bool
validNonBreak c = (c == '\t' || c >= ' ' && c <= '\x7E' || c == '\x85' || c >= '\xA0' && c <= '\xFFFD' || c >= '\x10000') && c /= '\xFEFF'


normalizeLineBreaks :: Text -> Text
normalizeLineBreaks src
  | T.any (== '\r') src = T.replace "\r" "\n" (T.replace "\r\n" "\n" src)
  | otherwise = src


-- | A document and its original source slice, with a UTF-8 byte offset in the stream.
data LocatedDocument = LocatedDocument
  { locatedDocument :: !Document
  , locatedSource :: !Text
  , locatedOffset :: {-# UNPACK #-} !Int
  , locatedAnnotatedBody :: A.AValue
  -- ^ Presentation tree retaining immutable alias references for editing.
  }


data ParsedDocument = ParsedDocument
  { parsedDocument :: !Document
  , parsedStartLine :: {-# UNPACK #-} !Int
  , parsedAnnotated :: ~(Either String A.AValue)
  }


{- | Source slices come from the same parse as the document structure. Concatenating
the slices recovers the original stream, including comments and directives.
-}
decodeLocatedDocuments :: Text -> Either String [LocatedDocument]
decodeLocatedDocuments src = do
  prepared <- prepareSource src
  parsed <- parseStreamLocated (preprocess prepared)
  let bytes = TE.encodeUtf8 src
      starts = V.fromList (0 : lineStarts bytes 0)
      offset line = if line <= 1 then 0 else starts V.! (line - 1)
      locate [] = Right []
      locate (d : ds) = do
        annotations <- parsedAnnotated d
        others <- locate ds
        let begin = offset (parsedStartLine d)
            end = case ds of next : _ -> offset (parsedStartLine next); [] -> BS.length bytes
        pure (LocatedDocument (parsedDocument d) (TE.decodeUtf8 (BS.take (end - begin) (BS.drop begin bytes))) begin annotations : others)
  locate parsed
  where
    lineStarts bytes = go
      where
        len = BS.length bytes
        go i
          | i >= len = []
          | BS.index bytes i == 13 =
              let next = if i + 1 < len && BS.index bytes (i + 1) == 10 then i + 2 else i + 1
              in next : go next
          | BS.index bytes i == 10 = (i + 1) : go (i + 1)
          | otherwise = go (i + 1)


-- ---------------------------------------------------------------------------
-- Pre-processing: split into structured lines
-- ---------------------------------------------------------------------------

data PLine = PLine
  { lineNo :: {-# UNPACK #-} !Int
  , lineIndent :: {-# UNPACK #-} !Int
  , lineKind :: !LineKind
  , lineBody :: !Text
  {- ^ content after stripping indent
  AND trailing whitespace (the form
  most parser paths want)
  -}
  , lineRawBody :: !Text
  {- ^ content after stripping indent
  only — trailing whitespace kept,
  for block-scalar collection
  -}
  }
  deriving (Show)


data LineKind
  = LBlank
  | LComment
  | -- | @---@
    LDocStart
  | -- | @...@
    LDocEnd
  | -- | @%YAML 1.2@ etc.
    LDirective
  | LContent
  | LByteOrderMark
  deriving (Eq, Show)


preprocess :: Text -> [PLine]
preprocess (TI.Text arr@(TA.ByteArray ba#) off blen) =
  goLine 1 off
  where
    !endByte = off + blen

    -- A single line scan: SWAR-find the next '\\n', then dispatch
    -- to 'mkLine' which does the indent / strip-trailing-WS
    -- analysis as a small backward + forward pass on the line
    -- slice. Splitting "find newline" from "analyse line" lets
    -- the bulk content scan run 8 bytes at a time.
    goLine :: Int -> Int -> [PLine]
    goLine !lno !lineStart
      | lineStart >= endByte = []
      | otherwise =
          let !nlAbs = scanNewline lineStart
              -- 'nlAbs' is endByte if the trailing line has no
              -- terminating '\\n'. Either way 'mkLine' builds
              -- the PLine for [lineStart, nlAbs).
              !line = mkLine lno lineStart nlAbs
              !next = if nlAbs < endByte then nlAbs + 1 else endByte
          in line : goLine (lno + 1) next

    -- SWAR scan for byte 0x0A starting at @i@. Returns the
    -- absolute byte index of the first newline, or @endByte@
    -- when none is found in [i, endByte). Uses the robust
    -- 'zeroByteMask' so the per-byte position from 'ctz' is
    -- exact.
    scanNewline :: Int -> Int
    scanNewline !i
      | i + 8 > endByte = scanNewlineByte i
      | otherwise =
          let !w = indexWord64Unaligned ba# i
              !m = zeroByteMask (w `Bits.xor` 0x0A0A0A0A0A0A0A0A)
          in if m == 0
               then scanNewline (i + 8)
               else
                 let !off' = wordFirstMatchByte m
                     !cand = i + off'
                 in if cand < endByte then cand else endByte

    scanNewlineByte :: Int -> Int
    scanNewlineByte !i
      | i >= endByte = endByte
      | TA.unsafeIndex arr i == 10 = i
      | otherwise = scanNewlineByte (i + 1)

    -- Build a 'PLine' for the byte range [lineStart, lineEnd)
    -- (lineEnd points at the terminating '\\n' or 'endByte').
    -- Indentation is purely leading SPACE characters; tabs
    -- never count. Trailing '\\r', spaces and tabs are stripped
    -- from 'lineBody' but retained in 'lineRawBody'.
    mkLine :: Int -> Int -> Int -> PLine
    mkLine !lno !lineStart !lineEnd =
      let !endNoCR =
            if lineEnd > lineStart
              && TA.unsafeIndex arr (lineEnd - 1) == 13
              then lineEnd - 1
              else lineEnd
          !leadSp = countLeadingSpaces lineStart endNoCR
          !contentStart = lineStart + leadSp
      in if contentStart >= endNoCR
           then
             let !restLen = endNoCR - contentStart
                 !lineRawB =
                   if restLen == 0
                     then T.empty
                     else TI.text arr contentStart restLen
             in PLine lno leadSp LBlank T.empty lineRawB
           else
             let !rawEnd = stripTrailingWS endNoCR contentStart
                 !rawLen = endNoCR - contentStart
                 !bodyLen = rawEnd - contentStart
                 !lineRawB = TI.text arr contentStart rawLen
                 !lineB
                   | bodyLen == 0 = T.empty
                   | bodyLen == rawLen = lineRawB
                   | otherwise = TI.text arr contentStart bodyLen
                 !kind = if leadSp == 0 && T.isPrefixOf "\xFEFF" lineB then LByteOrderMark else classify lineB
             in PLine lno leadSp kind lineB lineRawB

    countLeadingSpaces :: Int -> Int -> Int
    countLeadingSpaces !s !e = go s
      where
        go !i
          | i >= e = i - s
          | TA.unsafeIndex arr i == 32 = go (i + 1)
          | otherwise = i - s

    -- Walk backward from @e@ down to @low@, returning the byte
    -- index just past the last non-(space|tab) byte. Used to
    -- strip trailing whitespace from 'lineBody' without the
    -- per-byte 'lastNonWS' tracking the previous code did
    -- during the forward scan.
    stripTrailingWS :: Int -> Int -> Int
    stripTrailingWS !e !low
      | e <= low = low
      | otherwise =
          let !c = TA.unsafeIndex arr (e - 1)
          in if c == 32 || c == 9
               then stripTrailingWS (e - 1) low
               else e


-- Top-level CAFs so 'classify' doesn't repack literals on every line.
dashesText, dashesSpace, dashesTab :: Text
dashesText = T.pack "---"
dashesSpace = T.pack "--- "
dashesTab = T.pack "---\t"


dotsText, dotsSpace, dotsTab :: Text
dotsText = T.pack "..."
dotsSpace = T.pack "... "
dotsTab = T.pack "...\t"


classify :: Text -> LineKind
classify t = case T.uncons t of
  Nothing -> LBlank
  Just (h, _)
    | h == '#' -> LComment
    | h == '-'
    , t == dashesText
        || T.isPrefixOf dashesSpace t
        || T.isPrefixOf dashesTab t ->
        LDocStart
    | h == '.'
    , t == dotsText
        || T.isPrefixOf dotsSpace t
        || T.isPrefixOf dotsTab t ->
        LDocEnd
    | h == '%' -> LDirective
    | otherwise -> LContent
{-# INLINE classify #-}


isSkippable :: PLine -> Bool
isSkippable l = case lineKind l of
  LBlank -> True
  LComment -> True
  LDirective -> True
  _ -> False
{-# INLINE isSkippable #-}


isSkippableNonDirective :: PLine -> Bool
isSkippableNonDirective l = case lineKind l of
  LBlank -> True
  LComment -> True
  _ -> False
{-# INLINE isSkippableNonDirective #-}


-- ---------------------------------------------------------------------------
-- Parser monad: pure ([PLine], Map Text Value) -> Either String (a, ...)
-- ---------------------------------------------------------------------------

{- | Result of one parse step. Avoids the @Either String (a, PS)@
/pair-of-pair/ shape used previously, which allocated two
cells (the 'Either' constructor plus the @(a, PS)@ tuple) on
every successful @>>=@. 'Result' uses a single constructor
carrying both the value and the next state, halving the per-
bind allocation on the success path. ('Err' carries a 'String'
which is rarely allocated since failure stops the chain.)
-}
data Result a
  = Ok !a !PS
  | Err !String


newtype P a = P {unP :: PS -> Result a}


{- | Mode flags packed into a single 'Int' so that 'modifyS'
doesn't have to copy individual 'Int' / 'Bool' fields when
entering / leaving a scope.

* bits  0..19: 'parentInd' (biased by 'parentBias' so the
  @-1@ sentinel fits in the unsigned slot, range
  @[-1, 0xFFFFE]@; YAML lines wider than ~1 M chars are
  not a real-world case).
* bits 20..29: 'parserDepth' (10 bits, range @[0, 1023]@).
* bit  30:    'inMapValue'
* bit  31:    'flowSpannedNewline'
-}
data PS = PS
  { psLines :: ![PLine]
  , psAnchors :: !(HashMap Text Text)
  {- ^ Source names point at immutable binding identities, so rebinding a name
  cannot change the size or value of an earlier alias.
  -}
  , psBindings :: !(HashMap Text AnchorBinding)
  , psNextBinding :: {-# UNPACK #-} !Int
  , psShortcuts :: !(Map Text Text)
  {- ^ %TAG shorthand prefixes ('!handle!' → expansion).
  Reset between documents.
  -}
  , psFlags :: {-# UNPACK #-} !Int
  }


{- | Hard-coded recursion depth limit. YAML documents legitimately
nesting more than 'maxParserDepth' containers are vanishingly
rare; the limit exists to prevent a deeply-nested adversarial
input from blowing the Haskell stack.
-}
maxParserDepth :: Int
maxParserDepth = 1024
{-# INLINE maxParserDepth #-}


{- | Hard-coded ceiling on the logical node count of any anchor
value or document body. Prevents adversarial alias-DAG inputs
(the YAML 'billion laughs' attack) from producing a 'Value'
whose naive traversal would never finish: the per-anchor
check in 'recordAnchor' fails before such a value is even
constructed.
-}
maxParseNodes :: Int
maxParseNodes = 10_000_000
{-# INLINE maxParseNodes #-}


parentBias :: Int
parentBias = 1


parentMask, depthMask, depthShift :: Int
parentMask = 0xFFFFF -- 20 bits, [0, 1 048 575]
depthShift = 20
depthMask = Bits.shiftL 0x3FF depthShift -- bits 20..29 (10 bits)


inMapValueBit, flowSpannedBit :: Int
inMapValueBit = 30
flowSpannedBit = 31


packFlags :: Int -> Bool -> Bool -> Int
packFlags pInd inMV fSpan =
  let !p = (pInd + parentBias) Bits..&. parentMask
      !mv = if inMV then Bits.bit inMapValueBit else 0
      !fs = if fSpan then Bits.bit flowSpannedBit else 0
  in p Bits..|. mv Bits..|. fs
{-# INLINE packFlags #-}


psParentInd :: PS -> Int
psParentInd s = (psFlags s Bits..&. parentMask) - parentBias
{-# INLINE psParentInd #-}


psInMapValue :: PS -> Bool
psInMapValue s = Bits.testBit (psFlags s) inMapValueBit
{-# INLINE psInMapValue #-}


psFlowSpannedNewline :: PS -> Bool
psFlowSpannedNewline s = Bits.testBit (psFlags s) flowSpannedBit
{-# INLINE psFlowSpannedNewline #-}


psDepth :: PS -> Int
psDepth s = Bits.shiftR (psFlags s) depthShift Bits..&. 0x3FF
{-# INLINE psDepth #-}


setParentInd :: Int -> PS -> PS
setParentInd !i s =
  s
    { psFlags =
        (psFlags s Bits..&. Bits.complement parentMask)
          Bits..|. ((i + parentBias) Bits..&. parentMask)
    }
{-# INLINE setParentInd #-}


setInMapValue :: Bool -> PS -> PS
setInMapValue True s = s {psFlags = Bits.setBit (psFlags s) inMapValueBit}
setInMapValue False s = s {psFlags = Bits.clearBit (psFlags s) inMapValueBit}
{-# INLINE setInMapValue #-}


setFlowSpanned :: Bool -> PS -> PS
setFlowSpanned True s = s {psFlags = Bits.setBit (psFlags s) flowSpannedBit}
setFlowSpanned False s = s {psFlags = Bits.clearBit (psFlags s) flowSpannedBit}
{-# INLINE setFlowSpanned #-}


setDepth :: Int -> PS -> PS
setDepth !d s =
  s
    { psFlags =
        (psFlags s Bits..&. Bits.complement depthMask)
          Bits..|. Bits.shiftL (d Bits..&. 0x3FF) depthShift
    }
{-# INLINE setDepth #-}


runP :: P a -> PS -> Either String (a, PS)
runP (P f) s = case f s of
  Ok a s' -> Right (a, s')
  Err e -> Left e
{-# INLINE runP #-}


instance Functor P where
  fmap f (P g) =
    P
      ( \s -> case g s of
          Ok a s' -> Ok (f a) s'
          Err e -> Err e
      )
  {-# INLINE fmap #-}


instance Applicative P where
  pure x = P (\s -> Ok x s)
  {-# INLINE pure #-}
  P pf <*> P px =
    P
      ( \s -> case pf s of
          Err e -> Err e
          Ok f s' -> case px s' of
            Err e -> Err e
            Ok x s'' -> Ok (f x) s''
      )
  {-# INLINE (<*>) #-}
  P g *> P h =
    P
      ( \s -> case g s of
          Err e -> Err e
          Ok _ s' -> h s'
      )
  {-# INLINE (*>) #-}


instance Monad P where
  P g >>= k =
    P
      ( \s -> case g s of
          Err e -> Err e
          Ok a s' -> unP (k a) s'
      )
  {-# INLINE (>>=) #-}


instance MonadFail P where
  fail = failP
  {-# INLINE fail #-}


failP :: String -> P a
failP msg = P (\_ -> Err msg)
{-# INLINE failP #-}


getS :: P PS
getS = P (\s -> Ok s s)
{-# INLINE getS #-}


modifyS :: (PS -> PS) -> P ()
modifyS f = P (\s -> Ok () (f s))
{-# INLINE modifyS #-}


getLines :: P [PLine]
getLines = P (\s -> Ok (psLines s) s)
{-# INLINE getLines #-}


setLines :: [PLine] -> P ()
setLines ls = P (\s -> Ok () (s {psLines = ls}))
{-# INLINE setLines #-}


popLine :: P (Maybe PLine)
popLine = P $ \s ->
  let go [] = (Nothing, [])
      go (x : rest)
        | isSkippable x = go rest
        | otherwise = (Just x, rest)
      (mx, ls') = go (psLines s)
  in Ok mx (s {psLines = ls'})
{-# INLINE popLine #-}


{- | Drop the next non-skippable line without allocating a
'Maybe' wrapper. Used at sites that have already peeked /
pattern-matched the head and only need to advance the cursor.
-}
dropLine :: P ()
dropLine = P $ \s ->
  let go [] = []
      go (x : rest)
        | isSkippable x = go rest
        | otherwise = rest
  in Ok () (s {psLines = go (psLines s)})
{-# INLINE dropLine #-}


peekLine :: P (Maybe PLine)
peekLine = P $ \s ->
  let go [] = Nothing
      go (x : xs)
        | isSkippable x = go xs
        | otherwise = Just x
  in Ok (go (psLines s)) s
{-# INLINE peekLine #-}


pushLine :: PLine -> P ()
pushLine l = P (\s -> Ok () (s {psLines = l : psLines s}))
{-# INLINE pushLine #-}


-- ---------------------------------------------------------------------------
-- Resource bounds
--
-- Defences against two well-known adversarial-input classes:
--
-- 1. /Stack overflow via deep nesting./ A '[[[[…]]]]'-style
--    input with hundreds of thousands of opening brackets
--    would blow the runtime stack. 'withDepth' bumps a counter
--    on every recursive descent and refuses to continue past
--    'maxParserDepth' (default 1024).
--
-- 2. /Alias-DAG \"billion laughs\"./ A small input
--
--    @
--      a: &a [1,1,1,1,1,1,1,1,1]
--      b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
--      …
--      i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]
--    @
--
--    yields a 'Value' whose physical representation is small
--    (sharing) but whose naive traversal walks 9^9 ~ 4×10^8
--    nodes. Pure Haskell sharing protects the parser itself
--    but downstream consumers (e.g. JSON projection) blow up.
--    'recordAnchor' computes the /logical/ expanded size of
--    every anchor at registration time using a memoised walk
--    seeded from previously-recorded anchors and refuses any
--    anchor whose expansion exceeds 'maxParseNodes'.
-- ---------------------------------------------------------------------------

{- | Run @action@ at one greater nesting depth. Fails before
entering when the depth would exceed 'maxParserDepth'.
The depth lives in the high bits of the packed 'psFlags' so
the bump / pop are bit-twiddles on a single 'Int' field, not
record updates of a separate strict 'Int' slot.
-}
withDepth :: P a -> P a
withDepth (P action) = P $ \s ->
  let !d0 = psDepth s
      !d = d0 + 1
  in if d >= maxParserDepth -- guard before the 10-bit depth slot wraps
       then Err "YAML: maximum nesting depth exceeded"
       else case action (setDepth d s) of
         Err e -> Err e
         Ok x s' -> Ok x (setDepth d0 s')
{-# INLINE withDepth #-}


-- Binding identities never occur in the source grammar. Internal references
-- use the distinct YAlias node, not scalar text or a mutable anchor name.
data AnchorBinding = AnchorBinding
  { bindingIdentity :: {-# UNPACK #-} !Int
  , bindingName :: !Anchor
  , bindingValue :: !Value
  , bindingSize :: {-# UNPACK #-} !Int
  }


sizeWithMemo :: HashMap Text AnchorBinding -> Value -> Int
sizeWithMemo bindings = go
  where
    add a b = min (maxParseNodes + 1) (a + b)
    go (YAlias identity) = maybe (maxParseNodes + 1) ((1 +) . bindingSize) (HM.lookup (unAnchor identity) bindings)
    go (YAnchored _ inner) = add 1 (go inner)
    go (YTagged _ inner) = add 1 (go inner)
    go (YSeq xs) = V.foldl' (\n v -> if n > maxParseNodes then n else add n (go v)) 1 xs
    go (YMap kvs) = V.foldl' (\n (k, v) -> if n > maxParseNodes then n else add n (add (go k) (go v))) 1 kvs
    go _ = 1


validAnchorName :: Text -> Bool
validAnchorName name = not (T.null name) && T.all (\c -> c > ' ' && validNonBreak c && not (c `T.elem` "[],{}")) name


recordAnchor :: Text -> Value -> P Anchor
recordAnchor name value = do
  unless (validAnchorName name) (failP "YAML: invalid or empty anchor name")
  when (hasAnchor value) (failP "YAML: node has more than one anchor")
  st <- getS
  let inner = value
      !size = sizeWithMemo (psBindings st) inner
      identity = "\0binding:" <> TL.toStrict (TB.toLazyText (BI.decimal (psNextBinding st)))
      binding = AnchorBinding (psNextBinding st) (Anchor name) inner size
  when (size > maxParseNodes) (failP "YAML: anchor expansion exceeds the node budget (possible billion-laughs alias DAG)")
  modifyS
    ( \state ->
        state
          { psAnchors = HM.insert name identity (psAnchors state)
          , psBindings = HM.insert identity binding (psBindings state)
          , psNextBinding = psNextBinding state + 1
          }
    )
  pure (Anchor identity)
  where
    hasAnchor (YAnchored _ _) = True
    hasAnchor (YAlias _) = True
    hasAnchor (YTagged _ inner) = hasAnchor inner
    hasAnchor _ = False


resolveAnchor :: Text -> P Value
resolveAnchor name = do
  unless (validAnchorName name) (failP "YAML: invalid or empty alias name")
  state <- getS
  case HM.lookup name (psAnchors state) of
    Just identity -> pure (YAlias (Anchor identity))
    Nothing -> failP ("YAML: alias *" ++ T.unpack name ++ " has no anchor")


resetAnchors :: P ()
resetAnchors = modifyS (\state -> state {psAnchors = HM.empty, psBindings = HM.empty, psNextBinding = 0})


-- Canonical keys distinguish scalar tags, normalize NaN/negative zero, and
-- ignore insertion order inside mapping-valued keys.
data Key
  = KNull
  | KBool !Bool
  | KInt !Integer
  | KFloat !Word64
  | KString !Text
  | KSeq ![Key]
  | KMap ![KeyEntry]
  | KTagged !Text !Key
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)


data KeyEntry = KeyEntry !Key !Key
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)


keyOf :: Value -> Key
keyOf = \case
  YNull -> KNull
  YBool b -> KBool b
  YInt n -> KInt n
  YFloat d -> KFloat (if isNaN d then 0x7ff8000000000000 else if d == 0 then 0 else castDoubleToWord64 d)
  YString t -> KString t
  YSeq xs -> KSeq (keyOf <$> V.toList xs)
  YMap kvs -> KMap (sort ((\(k, v) -> KeyEntry (keyOf k) (keyOf v)) <$> V.toList kvs))
  YAnchored _ v -> keyOf v
  YTagged (Tag t) v
    | t == "!" || t `elem` ["tag:yaml.org,2002:str", "tag:yaml.org,2002:int", "tag:yaml.org,2002:float", "tag:yaml.org,2002:bool", "tag:yaml.org,2002:null", "tag:yaml.org,2002:map", "tag:yaml.org,2002:seq"] -> keyOf v
    | otherwise -> KTagged t (keyOf v)
  YAlias a -> KTagged "unresolved-alias" (KString (unAnchor a))


-- Resolve each immutable binding at most once. Validation and merge expansion
-- run before exposing a value to downstream consumers.
finishDocument :: HashMap Text AnchorBinding -> Value -> Either String Value
finishDocument bindings body
  | sizeWithMemo bindings body > maxParseNodes = Left "YAML: document alias expansion exceeds the node budget"
  | otherwise = evalStateT (go body) HM.empty
  where
    go :: Value -> StateT (HashMap Text Value) (Either String) Value
    go value = case value of
      YAlias (Anchor identity) -> do
        cache <- get
        case HM.lookup identity cache of
          Just v -> pure v
          Nothing -> case HM.lookup identity bindings of
            Nothing -> lift (Left "YAML: unresolved alias")
            Just binding -> do
              inner <- go (bindingValue binding)
              let resolved = YAnchored (bindingName binding) inner
              modify' (HM.insert identity resolved)
              pure resolved
      YAnchored identity v -> case HM.lookup (unAnchor identity) bindings of
        Nothing -> lift (Left "YAML: unresolved anchor binding")
        Just binding -> YAnchored (bindingName binding) <$> go v
      YTagged t v -> YTagged t <$> go v
      YSeq xs -> YSeq <$> traverse go xs
      YMap kvs -> do
        entries <- traverse (\(k, v) -> (,) <$> go k <*> go v) kvs
        lift (finishMap entries)
      v -> pure v


-- Build the editable presentation graph only when source annotations are
-- requested. Alias targets share a memo, rather than expanding a DAG per use.
annotatePresentation :: HashMap Text AnchorBinding -> Value -> Either String A.AValue
annotatePresentation bindings body = evalStateT (go body) HM.empty
  where
    binding identity = maybe (lift (Left "YAML: unresolved presentation binding")) pure (HM.lookup (unAnchor identity) bindings)
    go :: Value -> StateT (HashMap Text A.AValue) (Either String) A.AValue
    go = \case
      YNull -> pure A.aNull
      YBool value -> pure (A.aBool value)
      YInt value -> pure (A.aInt value)
      YFloat value -> pure (A.aFloat value)
      YString value -> pure (A.aString value)
      YSeq values -> do
        items <- V.mapM go values
        pure A.aNull {A.avBody = A.ASeq items A.SeqBlock}
      YMap values -> do
        entries <- V.mapM (\(key, value) -> do k <- go key; v <- go value; pure (A.AMapEntry k v Nothing)) values
        pure A.aNull {A.avBody = A.AMap entries A.MapBlock}
      YTagged tag inner -> A.aTagged tag <$> go inner
      YAnchored identity inner -> do
        definition <- binding identity
        value <- go inner
        pure (A.aAnchored (bindingName definition) value) {A.avAnchorId = bindingIdentity definition}
      YAlias identity -> do
        definition <- binding identity
        memo <- get
        target <- case HM.lookup (unAnchor identity) memo of
          Just value -> pure value
          Nothing -> do
            value <- go (bindingValue definition)
            modify' (HM.insert (unAnchor identity) value)
            pure value
        pure A.aNull {A.avBody = A.AResolvedAlias (bindingIdentity definition) (bindingName definition) target}


finishMap :: V.Vector (Value, Value) -> Either String Value
finishMap entries = do
  seen <- V.foldM checkUnique HS.empty entries
  if not (V.any (isMerge . fst) entries)
    then pure (YMap entries)
    else do
      inherited <- concat <$> traverse mergeSource (V.toList (V.filter (isMerge . fst) entries))
      let explicit = V.filter (not . isMerge . fst) entries
          initial = HS.delete (KTagged "tag:yaml.org,2002:merge" (KString "<<")) seen
          append (known, acc) pair@(key, _)
            | HS.member (keyOf key) known = (known, acc)
            | otherwise = (HS.insert (keyOf key) known, pair : acc)
          (_, extra) = foldl' append (initial, []) inherited
      pure (YMap (explicit V.++ V.fromList (reverse extra)))
  where
    checkUnique seen (key, _)
      | HS.member (keyOf key) seen = Left "YAML: duplicate mapping key"
      | otherwise = Right (HS.insert (keyOf key) seen)
    isMerge (YAnchored _ key) = isMerge key
    isMerge (YTagged (Tag tag) _) = tag == "tag:yaml.org,2002:merge"
    isMerge _ = False
    mergeSource (_, value) = case unwrap value of
      YMap kvs -> Right (V.toList kvs)
      YSeq maps ->
        concat
          <$> traverse
            ( \v -> case unwrap v of
                YMap kvs -> Right (V.toList kvs)
                _ -> Left "YAML: merge sequence must contain mappings"
            )
            (V.toList maps)
      _ -> Left "YAML: merge value must be a mapping or sequence of mappings"


resetShortcuts :: P ()
resetShortcuts = modifyS (\s -> s {psShortcuts = Map.empty})


recordShortcut :: Text -> Text -> P ()
recordShortcut handle prefix =
  modifyS (\s -> s {psShortcuts = Map.insert handle prefix (psShortcuts s)})


{- | Run an action with 'psParentInd' temporarily set to @i@,
restoring the previous value afterwards.
-}
withParentInd :: Int -> P a -> P a
withParentInd !i action = do
  when (i < -1 || i >= parentMask) (failP "YAML: indentation exceeds the supported range")
  saved <- psParentInd <$> getS
  modifyS (setParentInd i)
  x <- action
  modifyS (setParentInd saved)
  pure x


-- Bit 32 is scoped to the direct scalar under an explicit tag. Containers
-- clear it before parsing their children; node properties retain it.
withRawScalar :: Bool -> P a -> P a
withRawScalar raw action = do
  saved <- psFlags <$> getS
  modifyS (\st -> st {psFlags = if raw then Bits.setBit (psFlags st) 32 else Bits.clearBit (psFlags st) 32})
  x <- action
  modifyS (\st -> st {psFlags = if Bits.testBit saved 32 then Bits.setBit (psFlags st) 32 else Bits.clearBit (psFlags st) 32})
  pure x


withSequenceItem :: Bool -> P a -> P a
withSequenceItem enabled action = do
  saved <- psFlags <$> getS
  modifyS (\s -> s {psFlags = (if enabled then Bits.setBit else Bits.clearBit) (psFlags s) 33})
  result <- action
  modifyS (\s -> s {psFlags = (if Bits.testBit saved 33 then Bits.setBit else Bits.clearBit) (psFlags s) 33})
  pure result


resolvePlainP :: Text -> P Value
resolvePlainP raw = do
  unless (T.null raw || plainStartAllowed False raw) (failP "YAML: invalid plain scalar indicator")
  unless (T.all (\c -> c == '\n' || validNonBreak c) raw) (failP "YAML: invalid character in plain scalar")
  flags <- psFlags <$> getS
  pure (if Bits.testBit flags 32 then YString raw else resolvePlain raw)


getParentInd :: P Int
getParentInd = psParentInd <$> getS


withInMapValue :: Bool -> P a -> P a
withInMapValue !b action = do
  saved <- psInMapValue <$> getS
  modifyS (setInMapValue b)
  x <- action
  modifyS (setInMapValue saved)
  pure x


getInMapValue :: P Bool
getInMapValue = psInMapValue <$> getS


lookupShortcut :: Text -> P (Maybe Text)
lookupShortcut handle = do
  s <- getS
  pure (Map.lookup handle (psShortcuts s))


-- ---------------------------------------------------------------------------
-- Stream / document
-- ---------------------------------------------------------------------------

parseStream :: [PLine] -> Either String [Document]
parseStream lns = fmap parsedDocument <$> parseStreamLocated lns


parseStreamLocated :: [PLine] -> Either String [ParsedDocument]
parseStreamLocated lns =
  case runP
    (loop True True)
    ( PS
        lns
        HM.empty
        HM.empty
        0
        Map.empty
        (packFlags 0 False False)
    ) of
    Left err -> Left err
    Right (ds, _) -> Right ds
  where
    loop !first !prevExplicitEnd = do
      ls <- getLines
      case dropWhile isSkippableNonDirective ls of
        [] -> pure []
        (l : rest) | lineKind l == LByteOrderMark -> do
          let raw = T.drop 1 (lineRawBody l)
              spaces = T.length (T.takeWhile (== ' ') raw)
              body = T.drop spaces raw
          setLines (l {lineIndent = spaces, lineBody = stripYamlEnd body, lineRawBody = body, lineKind = classify (stripYamlEnd body)} : rest)
          loop first prevExplicitEnd
        (l : _) -> do
          let canBeBare = first || prevExplicitEnd
              isExplicitStart = lineKind l == LDocStart
              isDirective = lineKind l == LDirective
          when (isDirective && not first && not prevExplicitEnd) $
            failP $
              "directive without preceding '...' marker (line "
                ++ show (lineNo l)
                ++ ")"
          unless (canBeBare || isExplicitStart || isDirective) $
            failP $
              "second document without '---' marker (line "
                ++ show (lineNo l)
                ++ ")"
          parsed <- parseDocument
          let d = parsedDocument parsed
          progress <- checkProgress (lineNo l)
          if progress
            then do
              ds <- loop (first && lineKind l == LDocEnd) (docExplicitEnd d)
              pure (if lineKind l == LDocEnd then ds else parsed {parsedStartLine = if first then 1 else case ls of { raw : _ -> lineNo raw; [] -> lineNo l }} : ds)
            else
              failP $ "stray content (line " ++ show (lineNo l) ++ ")"

    checkProgress prevLine = do
      ls <- getLines
      pure $ case dropWhile isSkippable ls of
        [] -> True
        (l : _) -> lineNo l /= prevLine


{- | Consume any leading directives ('%YAML ...', '%TAG ...') from
the line stream. Returns whether at least one directive was
present. Validates the directive syntax: '%YAML' takes a single
version token, '%TAG' takes exactly two arguments, and '%YAML'
can appear at most once per document.
-}
consumeDirectives :: P Bool
consumeDirectives = go False False
  where
    go !sawAny !sawYaml = do
      ls <- getLines
      case dropWhile isSkippableNonDirective ls of
        (l : rest) | lineKind l == LDirective -> do
          setLines rest
          let body = stripInlineComment (lineBody l)
              args = wordsYaml (T.drop 1 body) -- drop leading '%'
          case args of
            ("YAML" : ver : extra) -> do
              when sawYaml $
                failP
                  ( "duplicate %YAML directive (line "
                      ++ show (lineNo l)
                      ++ ")"
                  )
              when (not (null extra)) $
                failP
                  ( "extra words on %YAML directive (line "
                      ++ show (lineNo l)
                      ++ ")"
                  )
              when (not (validYamlVersion ver)) $
                failP
                  ( "invalid %YAML version "
                      ++ T.unpack ver
                      ++ " (line "
                      ++ show (lineNo l)
                      ++ ")"
                  )
              go True True
            ("TAG" : handle : prefix : []) -> do
              unless (validTagHandle handle && validURIText prefix) (failP "YAML: invalid TAG directive handle or prefix")
              shortcuts <- psShortcuts <$> getS
              when (Map.member handle shortcuts) (failP "YAML: duplicate TAG directive handle")
              recordShortcut handle prefix
              go True sawYaml
            ("TAG" : _) ->
              failP
                ( "malformed %TAG directive (line "
                    ++ show (lineNo l)
                    ++ ")"
                )
            ("YAML" : _) ->
              failP
                ( "malformed %YAML directive (line "
                    ++ show (lineNo l)
                    ++ ")"
                )
            _ -> go True sawYaml -- unknown / reserved directive
        _ -> pure sawAny


validYamlVersion :: Text -> Bool
validYamlVersion t = case T.splitOn (T.pack ".") t of
  [maj, min_]
    | T.dropWhile (== '0') maj == "1" && T.all isDigit_ min_
    , not (T.null maj) && not (T.null min_) ->
        True
  _ -> False
  where
    isDigit_ c = c >= '0' && c <= '9'


parseDocument :: P ParsedDocument
parseDocument = do
  -- %TAG shortcuts are scoped to a single document — clear any
  -- left over from the previous one before we parse the new
  -- prologue (spec §6.8.2).
  resetShortcuts
  hadDirective <- consumeDirectives
  ls0' <- getLines
  let nextSig = dropWhile isSkippableNonDirective ls0'
  (directives, ls1) <- case nextSig of
    (l : rest) | lineKind l == LDocStart -> do
      let body = lineBody l
          tail_ = let content = stripYamlStart (T.drop 3 body) in if T.isPrefixOf "#" content then T.empty else content
          isInlineBlockScalar = case T.uncons tail_ of
            Just ('|', _) -> True
            Just ('>', _) -> True
            _ -> False
          virtInd
            | isInlineBlockScalar = -1
            | otherwise = lineIndent l
      -- '--- &anchor a: b' (an anchor immediately followed by a
      -- mapping pair on the same line as '---') is invalid per
      -- the test suite (CXX2 / mapping-with-anchor-on-document-
      -- start-line).
      case T.uncons tail_ of
        Just ('&', restA) ->
          let (_anchor, afterAnchor) = takeAnchorName restA
              afterStripped = stripYamlStart afterAnchor
          in case findKeyValueSplit afterStripped of
               Just _ ->
                 failP $
                   "anchor immediately followed by mapping on '---' line (line "
                     ++ show (lineNo l)
                     ++ ")"
               Nothing -> pure ()
        _ -> pure ()
      pure $
        if T.null tail_
          then (True, rest)
          else
            ( True
            , PLine
                (lineNo l)
                virtInd
                LContent
                tail_
                tail_
                : rest
            )
    (l : _)
      | hadDirective ->
          failP
            ( "missing '---' after directive (line "
                ++ show (lineNo l)
                ++ ")"
            )
    []
      | hadDirective ->
          failP "directive without document"
    _ -> pure (False, ls0')
  setLines ls1
  resetAnchors
  rawBody <- parseDocBody
  bindings <- psBindings <$> getS
  body <- either failP pure (finishDocument bindings rawBody)
  ls2 <- getLines
  (explicitEnd, ls3) <- case dropWhile isSkippable ls2 of
    (l : rest) | lineKind l == LDocEnd -> do
      let extra = stripYamlStart (T.drop 3 (lineBody l))
          -- Strip an inline comment if any.
          extra' = stripYamlEnd $ case T.uncons extra of
            Just ('#', _) -> T.empty
            _ -> stripInlineComment extra
      unless (T.null extra') $
        failP $
          "trailing content after '...' marker: "
            ++ show extra
            ++ " (line "
            ++ show (lineNo l)
            ++ ")"
      pure (True, rest)
    _ -> pure (False, ls2)
  setLines ls3
  pure (ParsedDocument (Document directives explicitEnd body) 0 (annotatePresentation bindings rawBody))


parseDocBody :: P Value
parseDocBody = do
  mNext <- peekLine
  case mNext of
    Nothing -> pure YNull
    Just l
      | lineKind l == LDocStart -> pure YNull
      | lineKind l == LDocEnd -> pure YNull
      -- A bare top-level '|' / '>' block scalar at column 0 is
      -- the same as '--- |' / '--- >' for indent purposes — body
      -- lines may live at column 0 (parent < 0).
      | lineIndent l == 0
      , isBlockScalarStart (lineBody l) -> do
          modifyS
            ( \s -> case psLines s of
                (h : rs) -> s {psLines = h {lineIndent = -1} : rs}
                [] -> s
            )
          parseNode (-1)
      | otherwise -> parseNode (min 0 (lineIndent l))
  where
    isBlockScalarStart t = case T.uncons t of
      Just ('|', _) -> True
      Just ('>', _) -> True
      _ -> False


-- ---------------------------------------------------------------------------
-- Node dispatch
-- ---------------------------------------------------------------------------

-- | Parse a node whose left margin is at least @minInd@.
parseNode :: Int -> P Value
parseNode !minInd = withDepth $ do
  mNext <- peekLine
  case mNext of
    Nothing -> pure YNull
    Just l
      | lineKind l == LDocStart -> pure YNull
      | lineKind l == LDocEnd -> pure YNull
      | lineIndent l < minInd -> pure YNull
      | otherwise -> dispatch l


dispatch :: PLine -> P Value
dispatch l0 = do
  -- Strip leading TAB characters. They're not allowed as block
  -- indentation per spec §6.1 but real-world inputs use them as
  -- 'separation' whitespace between a structural marker and the
  -- following node — '\\t{}', '\\t- x', '\\t"…"' all parse OK
  -- in libfyaml etc.
  let body0 = lineBody l0
  case T.uncons body0 of
    Just ('\t', _) -> do
      let l =
            l0
              { lineBody = T.dropWhile (== '\t') body0
              , lineRawBody = T.dropWhile (== '\t') (lineRawBody l0)
              }
      modifyS
        ( \s -> case psLines s of
            (top : rs)
              | lineNo top == lineNo l0 ->
                  s {psLines = l : rs}
            _ -> s
        )
      dispatchOn l
    _ -> dispatchOn l0
  where
    dispatchOn l
      | let body = lineBody l
      , isSeqItem body || isExplicitKey body || case findKeyValueSplit body of Just _ -> True; Nothing -> False =
          withSequenceItem False (withRawScalar False (dispatchRaw l))
      | otherwise = dispatchRaw l
    dispatchRaw l = case T.uncons (lineBody l) of
      Just (h, _) -> case h of
        '!' -> parseTagged
        '&' -> parseAnchored
        '*' -> case findAliasKeySplit (lineBody l) of
          Just (aliasName, vRest) ->
            parseBlockMapAliasFirst
              (lineIndent l)
              aliasName
              vRest
          Nothing -> parseAlias
        '|' -> parseBlockScalar Literal
        '>' -> parseBlockScalar Folded
        '[' -> consumeFlowFromHead >>= maybeFlowAsBlockKey (lineIndent l)
        '{' -> consumeFlowFromHead >>= maybeFlowAsBlockKey (lineIndent l)
        '"' -> parseQuotedScalarLine '"' l
        '\'' -> parseQuotedScalarLine '\'' l
        _ -> parseBlockOrPlain l
      Nothing -> parseBlockOrPlain l


{- | Quoted scalars can be the entire node body, or the start of a
@key: \"…\"@ pair when the closing quote is followed by a colon. We
look ahead for a top-level @:@ after the closing quote and dispatch
to 'parseBlockMap' if found, otherwise consume the quoted scalar.

Quoted scalars may span multiple lines per YAML 1.2; if the close
quote is not on the same line we splice continuation lines into
the buffer until it is.
-}
parseQuotedScalarLine :: Char -> PLine -> P Value
parseQuotedScalarLine q l =
  -- A quoted key and quoted value also end with a quote. Only the
  -- absence of a colon can rule out a mapping without scanning it.
  let body = lineBody l
  in if not (bAnyByte w8Colon body)
       then doQuoted
       else case findKeyValueSplit body of
         Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
         Nothing -> doQuoted
  where
    doQuoted = do
      dropLine
      consumeQuotedAt
        q
        (lineIndent l)
        (preserveTrailingEscape (lineBody l) (lineRawBody l))


{- | If the trimmed line body ends in @\\@ that's not itself
escaped, take the next character /verbatim/ from the raw body
(so an escape argument like @\\<TAB>@ survives the trailing-WS
strip done by 'preprocess'). Any whitespace /after/ that escape
argument is still stripped.
-}
preserveTrailingEscape :: Text -> Text -> Text
preserveTrailingEscape stripped raw = case T.unsnoc stripped of
  Just (_, '\\')
    | not (endsEvenBackslashes stripped) ->
        -- Reach into raw at the position right after the trailing '\'.
        let idx = T.length stripped
        in if idx < T.length raw
             then stripped <> T.singleton (T.index raw idx)
             else stripped
  _ -> stripped
  where
    endsEvenBackslashes t =
      even (T.length (T.takeWhileEnd (== '\\') t))


{- | Like 'consumeQuoted' but the caller knows the indent of the
line that opened the quote; continuation lines must be at strictly
greater indent.
-}
consumeQuotedAt :: Char -> Int -> Text -> P Value
consumeQuotedAt q !openInd = go0 False
  where
    parser = case q of '"' -> parseDQ; _ -> parseSQ

    -- The very first attempt; no fold prefix has been emitted yet.
    go0 !multi !buf = case parser 0 buf of
      Just (v, p) -> finish multi v (bDrop p buf)
      Nothing -> readMore multi buf 0

    -- @blanks@ counts consecutive empty continuation lines we've
    -- absorbed since the last non-empty (or the opening) line.
    -- We pop the raw next line (not 'popLine', which would skip
    -- blank / comment lines — those are significant inside a
    -- multi-line quoted scalar).
    readMore _multi !buf !blanks = do
      ls <- getLines
      case ls of
        [] -> failP "YAML: unterminated quoted scalar"
        (l' : _)
          | lineKind l' == LDocStart || lineKind l' == LDocEnd ->
              failP $
                "document marker inside quoted scalar (line "
                  ++ show (lineNo l')
                  ++ ")"
        -- Continuations must be at /at least/ the same indent as
        -- the line that opened the quote. A line at lower indent
        -- belongs to the surrounding scope.
        (l' : _)
          | openInd > 0
          , lineKind l' == LContent
          , lineIndent l' < openInd ->
              failP $
                "wrong-indented quoted-scalar continuation (line "
                  ++ show (lineNo l')
                  ++ ")"
        (l' : rest) -> do
          setLines rest
          let body0 = lineBody l'
              raw = lineRawBody l'
              -- Use the raw body when the trimmed line body ends
              -- in '\\' so that '\\<TAB>' survives.
              body = preserveTrailingEscape body0 raw
              isBlank = T.null (stripYaml body)
              body' = T.dropWhile (\c -> c == ' ' || c == '\t') body
              -- DQ-only: a bare trailing backslash on the previous
              -- line eats the newline plus any leading whitespace
              -- on the next line (YAML 1.2 §5.7 / §7.5).
              endsWithEscape =
                q == '"'
                  && case T.unsnoc buf of
                    Just (_, '\\') -> not (endsEvenBackslashes buf)
                    _ -> False
          if isBlank
            then readMore True buf (blanks + 1)
            else
              let (buf', joined)
                    | endsWithEscape =
                        (T.init buf <> body', True)
                    | otherwise =
                        let joinSep
                              | blanks == 0 = tSpace
                              | otherwise = T.replicate blanks tNL
                        in (buf <> joinSep <> body', True)
              in joined `seq` case parser 0 buf' of
                   Just (v, p) -> finish True v (bDrop p buf')
                   Nothing -> readMore True buf' 0

    -- A run of trailing backslashes counts as "even" when it
    -- pairs up to "\\\\…", which means no escape at end.
    endsEvenBackslashes t = even (T.length (T.takeWhileEnd (== '\\') t))

    finish multi v rest =
      let trimmed = stripYamlStart rest
          -- Was there at least one whitespace char between the
          -- closing quote and 'rest'?
          hadSeparator = bLen trimmed < bLen rest
          stripped = case T.uncons trimmed of
            -- A '#' may only start a comment when preceded by
            -- whitespace; without one it's malformed.
            Just ('#', _)
              | hadSeparator -> T.empty
              | otherwise -> trimmed
            _ -> stripYamlEnd (stripInlineComment trimmed)
      in if T.null stripped
           then pure v
           else case T.uncons stripped of
             Just (c, _)
               | c == ':' && multi ->
                   failP "multi-line quoted scalar used as implicit key"
               -- A ':' trailing after the close quote means
               -- the scalar is acting as a key. That's only
               -- valid when this consumeQuoted call wasn't
               -- already inside a value position (openInd > 0
               -- means we're inside a parent block context's
               -- value). Otherwise treat the ':' as malformed
               -- (e.g. ZL4Z's "a: 'b': c").
               | c == ':' && openInd > 0 ->
                   failP $ "nested mapping after quoted scalar value"
               | c == ',' -> pushBack stripped
               | c == ':' -> pushBack stripped
               | c == ']' -> pushBack stripped
               | c == '}' -> pushBack stripped
               | otherwise ->
                   failP $
                     "trailing content after quoted scalar: "
                       ++ show stripped
             Nothing -> pure v
      where
        pushBack s = do
          pushLine (PLine 0 0 LContent s s)
          pure v


parseTagged :: P Value
parseTagged = do
  inValue <- getInMapValue
  inSequence <- (\s -> Bits.testBit (psFlags s) 33) <$> getS
  Just l <- popLine
  let (tg, rest) = breakOnSpace (lineBody l)
      after0 = stripYamlStart rest
      after = case T.uncons after0 of
        Just ('#', _) -> T.empty
        _ -> after0
  tag <- expandTagP (lineNo l) tg
  -- Tag tokens can contain URI characters (incl. ',') /inside/
  -- a verbatim '!<...>' wrapper, but a bare tag ('!!str' /
  -- '!foo') may not include ',' or flow indicators. Reject the
  -- bare-tag form when it contains them.
  let isVerbatim = case T.uncons (T.drop 1 tg) of
        Just ('<', _) -> True
        _ -> False
  unless isVerbatim $
    case T.find
      ( \c ->
          c == ','
            || c == '['
            || c == ']'
            || c == '{'
            || c == '}'
      )
      tg of
      Just c ->
        failP $
          "invalid tag character "
            ++ show c
            ++ " in tag "
            ++ show tg
            ++ " (line "
            ++ show (lineNo l)
            ++ ")"
      Nothing -> pure ()
  case findKeyValueSplit after of
    Just (keyText, valueRest) -> do
      key <- parsePropertyKey tag keyText
      pushLine l {lineBody = after, lineRawBody = after}
      parseBlockMapWithKey (lineIndent l) key valueRest
    Nothing ->
      if T.null after
        then do
          mNext <- peekLine
          case mNext of
            Just l2
              | lineIndent l2 > lineIndent l
                  || (lineIndent l2 == lineIndent l && not inValue)
                  || (not inSequence && lineIndent l2 == lineIndent l && isSeqItem (lineBody l2) && tag == Tag "tag:yaml.org,2002:seq") -> do
                  v <- withRawScalar True $ parseNode (lineIndent l2)
                  tagValue tag v
            Just l2
              | lineKind l2 == LContent
              , let body2 = lineBody l2
              , isBlockScalarHeadOrProp body2 -> do
                  -- Adjust the body line's stored indent to 0 so a
                  -- following block-scalar's auto-baseline picks up
                  -- content at any column > 0 (spec example 8.21).
                  modifyS
                    ( \s -> case psLines s of
                        (h : rs) -> s {psLines = h {lineIndent = 0} : rs}
                        _ -> s
                    )
                  v <- withRawScalar True $ parseNode 0
                  tagValue tag v
            _ -> tagValue tag YNull
        else do
          -- Update both 'lineBody' and 'lineRawBody' so that
          -- consumeQuoted's preserveTrailingEscape works on the
          -- right slice of the line.
          pushLine l {lineBody = after, lineRawBody = after}
          v <- withRawScalar True $ parseNode (lineIndent l)
          tagValue tag v


parsePropertyKey :: Tag -> Text -> P Value
parsePropertyKey tag text = do
  key <- parseRawKey text >>= recordAndResolveFlow
  tagValue tag key


parseRawKey :: Text -> P Value
parseRawKey text
  | T.null text = pure YNull
  | otherwise = case parseFlowValueWith True 0 0 text of
      Just (v, end) | T.null (stripYaml (bDrop end text)) -> pure v
      _ -> failP "YAML: invalid mapping key properties"


parseAnchored :: P Value
parseAnchored = do
  inMapValue <- getInMapValue
  -- Once we descend below the immediate mapping-value dispatch,
  -- further parseNode calls are NOT in mapping-value position.
  withInMapValue False (parseAnchoredImpl inMapValue)


parseAnchoredImpl :: Bool -> P Value
parseAnchoredImpl !inMapValue = do
  inSequence <- (\s -> Bits.testBit (psFlags s) 33) <$> getS
  parentIndent <- getParentInd
  Just l <- popLine
  let body = lineBody l
      (name, rest) = takeAnchorName (T.drop 1 body)
      after0 = stripYamlStart rest
      after = case T.uncons after0 of
        Just ('#', _) -> T.empty
        _ -> after0
  -- '&anchor - foo' / '&anchor ? key' / '&anchor : value' are
  -- invalid: an anchor cannot label a block-sequence / explicit
  -- mapping marker on the same line (the marker introduces its
  -- own nested structure with its own indentation requirements).
  case T.uncons after of
    Just ('-', rest1)
      | startsWithSeparator rest1 ->
          failP $
            "anchor immediately followed by block indicator '-' (line "
              ++ show (lineNo l)
              ++ ")"
    Just ('?', rest1)
      | startsWithSeparator rest1 ->
          failP $
            "anchor immediately followed by explicit-key marker (line "
              ++ show (lineNo l)
              ++ ")"
    -- '&anchor *alias' is invalid: an alias is itself a node
    -- reference and can't be re-labelled with a new anchor.
    Just ('*', _) ->
      failP $
        "anchor immediately followed by alias (line "
          ++ show (lineNo l)
          ++ ")"
    _ -> pure ()
  -- If the rest of the line introduces a mapping (i.e. there's a
  -- top-level @": "@ in the remainder), the anchor only binds to
  -- the /key/, not to the surrounding mapping. This matches the
  -- YAML 1.2 node-anchoring model where the anchor precedes the
  -- specific node it labels.
  case findKeyValueSplit after of
    Just (k, vRest) -> do
      keyVal <- parseRawKey k >>= recordAndResolveFlow
      identity <- recordAnchor name keyVal
      pushLine l {lineBody = after, lineRawBody = after}
      parseBlockMapWithKey (lineIndent l) (YAnchored identity keyVal) vRest
    Nothing -> do
      v <-
        if T.null after
          then do
            mNext <- peekLine
            case mNext of
              Just l2
                | lineIndent l2 > lineIndent l
                , isJustAnchorScalar (lineBody l2) -> do
                    -- '&outer' followed by '&inner scalar'
                    -- (no ':'/structural marker) is invalid:
                    -- two anchors on the same scalar node
                    -- (4JVG / scalar-value-with-two-anchors).
                    failP $
                      "node has two consecutive anchors (line "
                        ++ show (lineNo l2)
                        ++ ")"
              Just l2
                | lineIndent l2 > lineIndent l ->
                    parseNode (lineIndent l2)
              -- A bare anchor at the same column as a following
              -- block sequence binds to that sequence.
              Just l2
                | lineIndent l2 == lineIndent l
                , not inSequence
                , isSeqItem (lineBody l2) ->
                    parseBlockSeq (lineIndent l2)
              -- A bare anchor whose next line at the same
              -- column is another node-property line chains
              -- into that property; the whole stack labels
              -- the eventual node. EXCEPT when the anchor is
              -- itself the value of a mapping entry: a
              -- same-column property line then is a sibling
              -- (not a chained property), so the anchor refers
              -- to Null. See H7J7 (node-anchor-not-indented).
              Just l2
                | lineIndent l2 == lineIndent l
                , isNodePropertyLine (lineBody l2)
                , not inMapValue ->
                    parseNode (lineIndent l2)
              -- A bare anchor on its own line at column > 0
              -- binds to the next same-column content line.
              Just l2
                | lineIndent l2 == lineIndent l
                , lineIndent l > 0
                , lineKind l2 == LContent ->
                    parseNode (lineIndent l2)
              -- An anchor whose line is more-indented than the
              -- following block sequence binds to that
              -- sequence (e.g. 'seq:\\n &anchor\\n- a\\n- b').
              Just l2
                | lineKind l2 == LContent
                , lineIndent l2 < lineIndent l
                , lineIndent l2 >= parentIndent
                , isSeqItem (lineBody l2) ->
                    parseBlockSeq (lineIndent l2)
              -- At column 0 we only chain into a same-column
              -- /plain scalar/. In mapping-value position the
              -- same-column line is the next sibling.
              Just l2
                | lineIndent l2 == lineIndent l
                , lineIndent l == 0
                , lineKind l2 == LContent
                , not inMapValue ->
                    parseNode (lineIndent l2)
              _ -> pure YNull
          else do
            pushLine l {lineBody = after, lineRawBody = after}
            parseNode (lineIndent l)
      identity <- recordAnchor name v
      pure (YAnchored identity v)


{- | True when the line is shaped like '&anchor scalar' — i.e.
starts with an anchor whose remainder is a plain scalar (no
':' / sequence marker).
-}
isJustAnchorScalar :: Text -> Bool
isJustAnchorScalar t = case T.uncons t of
  Just ('&', rest) ->
    let (_name, after) = takeAnchorName rest
        body = stripYamlStart after
    in not (T.null body)
         && case findKeyValueSplit body of
           Just _ -> False
           Nothing ->
             not (isSeqItem body)
               && not (isExplicitKey body)
  _ -> False


{- | True when the line begins with a node-property indicator
('!' tag or '&' anchor) followed by separator / EOL.
-}
isNodePropertyLine :: Text -> Bool
isNodePropertyLine t = case T.uncons t of
  Just ('!', _) -> True
  Just ('&', _) -> True
  _ -> False


{- | True when the line begins with a block-scalar header indicator
('|' / '>') or a node-property indicator.
-}
isBlockScalarHeadOrProp :: Text -> Bool
isBlockScalarHeadOrProp t = case T.uncons t of
  Just ('|', _) -> True
  Just ('>', _) -> True
  Just ('!', _) -> True
  Just ('&', _) -> True
  _ -> False


parseAlias :: P Value
parseAlias = do
  Just l <- popLine
  let body = lineBody l
      (name, rest) = takeAnchorName (T.drop 1 body)
      after0 = stripYamlStart rest
      after = case T.uncons after0 of
        Just ('#', _) -> T.empty
        _ -> after0
  if T.null after
    then resolveAnchor name
    else do
      pushLine (PLine (lineNo l) (lineIndent l) LContent after after)
      resolveAnchor name


{- | Read an anchor / alias name. Colons are valid name characters;
a mapping delimiter after an anchor therefore needs separation
(@&name : value@).
-}
takeAnchorName :: Text -> (Text, Text)
takeAnchorName t = goT 0
  where
    !len = bLen t
    goT !i
      | i >= len = (t, T.empty)
      | otherwise =
          let !b = bAt t i
          in if isAnchorByte b
               then goT (i + 1)
               else (bTake i t, bDrop i t)


isAnchorByte :: Word8 -> Bool
isAnchorByte b =
  -- ASCII byte: reject the same chars as 'isAnchorChar'.
  -- Non-ASCII bytes (≥ 0x80, including UTF-8 leading and
  -- continuation bytes) are always part of the name.
  case b of
    44 -> False -- ','
    91 -> False -- '['
    93 -> False -- ']'
    123 -> False -- '{'
    125 -> False -- '}'
    32 -> False -- ' '
    9 -> False -- '\t'
    10 -> False -- '\n'
    13 -> False -- '\r'
    _ -> True
{-# INLINE isAnchorByte #-}


breakOnSpace :: Text -> (Text, Text)
breakOnSpace = T.break (\c -> c == ' ' || c == '\t')


-- ---------------------------------------------------------------------------
-- Flow style
-- ---------------------------------------------------------------------------

consumeFlowFromHead :: P Value
consumeFlowFromHead = do
  Just l <- popLine
  modifyS (setFlowSpanned False)
  inMV <- getInMapValue
  parent <- getParentInd
  -- Inside any block container (mapping value or non-zero
  -- parent ind), use the parent-+-1 column as the lower bound
  -- for flow continuations so we can detect tab-as-indent
  -- (Y79Y/003).
  let openInd
        | inMV = max 1 (parent + 1)
        | parent >= 0 && parent < ourInd = parent + 1
        | otherwise = -1
      ourInd = lineIndent l
  value <- consumeFlowAt openInd (lineBody l)
  modifyS
    ( \state ->
        state
          { psLines = case psLines state of
              next : rest | lineNo next == 0 -> next {lineIndent = ourInd} : rest
              other -> other
          }
    )
  pure value


{- | After a flow node has been consumed, see if there's a virtual
':' line waiting in the stream — if so, the flow node is the
/key/ of a block mapping. Continue parsing as a block mapping
with this flow value as the first key.
-}
maybeFlowAsBlockKey :: Int -> Value -> P Value
maybeFlowAsBlockKey !ind k = do
  mNext <- peekLine
  case mNext of
    Just l
      | lineIndent l == ind
      , let body = lineBody l
      , body == tColonStr
          || T.isPrefixOf tColonSpace body
          || T.isPrefixOf tColonTab body -> do
          spanned <- psFlowSpannedNewline <$> getS
          longKey <- (\s -> Bits.testBit (psFlags s) 34) <$> getS
          when longKey (failP "YAML: implicit mapping key exceeds 1024 characters")
          when spanned $
            failP $
              "flow node spanning newline used as block-mapping key (line "
                ++ show (lineNo l)
                ++ ")"
          dropLine
          let after =
                if body == tColonStr
                  then T.empty
                  else T.drop 2 body
          v <- parseImplicitMapValue ind after
          rest <- collectFlowMapEntries ind
          pure (YMap (V.fromList ((k, v) : rest)))
    _ -> pure k


{- | Collect more block-mapping entries after a flow-as-key entry.
Mirrors 'parseBlockMap.collect' but specialized to start from
an arbitrary state.
-}
collectFlowMapEntries :: Int -> P [(Value, Value)]
collectFlowMapEntries !ind = go []
  where
    go acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure (reverse acc)
        Just l
          | lineIndent l /= ind -> pure (reverse acc)
          | isSeqItem (lineBody l) -> pure (reverse acc)
          | startsWithTab (lineBody l) ->
              failP $
                "tab character used as indentation (line "
                  ++ show (lineNo l)
                  ++ ")"
          | isExplicitKey (lineBody l) -> do
              k <- readEntryKey
              v <- readEntryValue
              go ((k, v) : acc)
          | otherwise -> case findAliasKeySplit (lineBody l) of
              Just (aliasName, vRest) -> do
                dropLine
                k <- resolveAnchor aliasName
                v <- parseImplicitMapValue ind vRest
                go ((k, v) : acc)
              Nothing -> case findKeyValueSplit (lineBody l) of
                Just (k, vRest) -> do
                  dropLine
                  kv <- parseBlockKey k
                  v <- parseImplicitMapValue ind vRest
                  go ((kv, v) : acc)
                Nothing -> pure (reverse acc)

    -- (cheap inline of parseExplicitMap.readExplicitPart "?")
    readEntryKey = do
      Just l <- popLine
      let body = lineBody l
          afterMarker =
            if body == tQuestStr
              then T.empty
              else T.drop 1 body
          rest = stripYamlStart (T.drop 1 afterMarker)
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l -> parseNode (lineIndent l2)
            _ -> pure YNull
        else do
          pushLine (PLine (lineNo l) (lineIndent l + 2) LContent rest rest)
          parseNode (lineIndent l + 2)

    readEntryValue = do
      mPL <- peekLine
      case mPL of
        Just l
          | lineIndent l == ind
          , ( lineBody l == tColonStr
                || T.isPrefixOf tColonSpace (lineBody l)
                || T.isPrefixOf tColonTab (lineBody l)
            ) -> do
              Just l' <- popLine
              let body = lineBody l'
                  afterMarker =
                    if body == tColonStr
                      then T.empty
                      else T.drop 1 body
                  rest = stripYamlStart (T.drop 1 afterMarker)
              if T.null rest
                then do
                  mNext <- peekLine
                  case mNext of
                    Just l2
                      | lineIndent l2 > lineIndent l' ->
                          parseNode (lineIndent l2)
                    _ -> pure YNull
                else do
                  pushLine
                    ( PLine
                        (lineNo l')
                        (lineIndent l' + 2)
                        LContent
                        rest
                        rest
                    )
                  withParentInd (lineIndent l') (parseNode (lineIndent l' + 2))
        _ -> pure YNull


{- | Single-pass walk over a parsed flow value that both
/registers/ each embedded 'YAnchored' anchor and /resolves/
the alias nodes left by 'parseFlowAlias'.

The pass is bottom-up: when we encounter @&n inner@ we
resolve @inner@ first and only then call 'recordAnchor', so
the stored value is itself fully resolved. References to
@*name@ are then just looked up — no recursive walk into the
resolved value is needed, which both avoids the
alias-DAG-of-DAGs exponential blow-up that a separate
record-then-resolve approach would incur and naturally
detects forward / self-cycles (the anchor isn't in the
environment when we try to resolve into its own definition).
-}
recordAndResolveFlow :: Value -> P Value
recordAndResolveFlow = go
  where
    go v = case v of
      YAlias (Anchor name) -> resolveAnchor name
      YAnchored (Anchor n) inner -> do
        inner' <- go inner
        identity <- recordAnchor n inner'
        pure (YAnchored identity inner')
      YTagged (Tag spelling) inner -> do
        tag <- expandTagP 0 spelling
        normalized <- case resolveExplicitTag tag inner of
          Just value -> pure value
          Nothing -> failP "YAML: scalar or collection does not match its explicit tag"
        resolved <- go normalized
        recordTaggedAnchors tag resolved
        pure (YTagged tag resolved)
      YSeq xs -> YSeq <$> V.mapM go xs
      YMap kvs ->
        YMap
          <$> V.mapM
            (\(k, x) -> (,) <$> go k <*> go x)
            kvs
      _ -> pure v


consumeFlowAt :: Int -> Text -> P Value
consumeFlowAt !openInd = go
  where
    -- Strip end-of-line comments before adding a new chunk to the
    -- buffer. Flow nodes may contain comments between elements,
    -- per the YAML 1.2 grammar.
    stripFlowComment t = stripYamlEnd (stripComment True t)

    go buf0 = do
      state <- getS
      let buf = stripFlowComment buf0
      case scanFlow (psDepth state) buf of
        ScanComplete v rest -> do
          let longKey = T.length buf - T.length rest > 1024
          modifyS (\s -> s {psFlags = (if longKey then Bits.setBit else Bits.clearBit) (psFlags s) 34})
          v' <- recordAndResolveFlow v
          let !s = bDrop (skipFlowWS 0 rest) rest
          case T.uncons s of
            Nothing -> pure v'
            -- Trailing block-context indicators ('- ', ': ', '? ',
            -- bare '-' / ':' / '?') immediately after a flow node
            -- close are malformed (P2EQ /
            -- invalid-block-mapping-key-on-same-line-as-previous-key).
            Just (c, _)
              -- Trailing block indicators or bare text on the same
              -- line after a flow node close are malformed. Only a
              -- ':' / ',' / ']' / '}' terminator (the surrounding
              -- flow context's own punctuation) is acceptable here.
              | c == ',' || c == ':' || c == ']' || c == '}' -> do
                  pushLine (PLine 0 0 LContent s s)
                  pure v'
              | otherwise ->
                  failP $ "trailing content after flow node: " ++ show s
        ScanIncomplete -> do
          ls <- getLines
          case ls of
            [] -> failP "YAML: unterminated flow node"
            (l' : _)
              | lineKind l' == LDocStart || lineKind l' == LDocEnd ->
                  failP $
                    "document marker inside flow node (line "
                      ++ show (lineNo l')
                      ++ ")"
            (l' : _)
              | openInd > 0
              , lineKind l' == LContent
              , lineIndent l' < openInd ->
                  failP $
                    "wrong-indented flow continuation (line "
                      ++ show (lineNo l')
                      ++ ")"
            (l' : _)
              | openInd > 0
              , lineKind l' == LContent
              , lineIndent l' == 0
              , case T.uncons (lineBody l') of
                  Just ('\t', _) -> True
                  _ -> False ->
                  failP $
                    "tab as indentation in flow continuation (line "
                      ++ show (lineNo l')
                      ++ ")"
            (l' : rs) -> do
              setLines rs
              modifyS (setFlowSpanned True)
              -- Join with a sentinel character (\\1) instead of a
              -- plain space so downstream parsers can detect that
              -- a structural element spanned a newline (used for
              -- the implicit-key-followed-by-newline check).
              go (buf <> tSOH <> lineBody l')


data ScanResult
  = ScanComplete !Value !Text
  | ScanIncomplete


scanFlow :: Int -> Text -> ScanResult
scanFlow depth buf = case parseFlowValue depth 0 buf of
  Just (v, p) -> ScanComplete v (bDrop p buf)
  Nothing -> ScanIncomplete


parseFlowValue :: Int -> Int -> Text -> Maybe (Value, Int)
parseFlowValue = parseFlowValueWith False


parseFlowValueWith :: Bool -> Int -> Int -> Text -> Maybe (Value, Int)
parseFlowValueWith !raw !depth !p0 t =
  let !len = bLen t
      p = skipFlowWS p0 t
  in if depth >= maxParserDepth || p >= len
       then Nothing
       else case bAt t p of
         91 -> parseFlowSeq (depth + 1) (p + 1) t -- '['
         123 -> parseFlowMap (depth + 1) (p + 1) t -- '{'
         34 -> parseDQ p t -- '"'
         39 -> parseSQ p t -- '\''
         33 -> parseFlowTagged (depth + 1) p t -- '!'
         38 -> parseFlowAnchored raw (depth + 1) p t -- '&'
         42 -> parseFlowAlias p t -- '*'
         _ -> parseFlowPlainWith (if raw then YString else resolvePlain) p t


{- | Tagged node in flow context. Reads the tag token (everything
up to whitespace / flow stopper) and then optionally parses a
following node; @!!str@ alone is allowed and means "tagged
null".
-}
parseFlowTagged :: Int -> Int -> Text -> Maybe (Value, Int)
parseFlowTagged !depth !p t =
  let !len = bLen t
      goT !i
        | i >= len = i
        | otherwise =
            let !c = bAt t i
            in if c == w8Space
                 || c == w8Tab
                 || c == w8SOH
                 || c == w8Comma
                 || c == w8RBrack
                 || c == w8RBrace
                 then i
                 else goT (i + 1)
      !p1 = if p + 1 < len && bAt t (p + 1) == 60 then verbatim (p + 2) else goT p
      verbatim i
        | i >= len = i
        | bAt t i == 62 = i + 1
        | otherwise = verbatim (i + 1)
      tagText = bSlice t p p1
      tag = Tag tagText
      p2 = skipFlowWS p1 t
  in if p2 >= len
       then taggedFlow tag YNull p2
       else case bAt t p2 of
         44 -> taggedFlow tag YNull p1 -- ','
         93 -> taggedFlow tag YNull p1 -- ']'
         125 -> taggedFlow tag YNull p1 -- '}'
         58
           | colonIsSeparator (p2 + 1) t -> -- ':'
               taggedFlow tag YNull p1
         _ -> case parseFlowValueWith True depth p2 t of
           Just (v, p3) -> taggedFlow tag v p3
           Nothing -> taggedFlow tag YNull p2


{- | Anchor in flow context: read the anchor name and parse the
labelled value, wrapping it in 'YAnchored' so the post-pass
'recordFlowAnchors' picks it up and registers it with the
enclosing parser state.
-}
parseFlowAnchored :: Bool -> Int -> Int -> Text -> Maybe (Value, Int)
parseFlowAnchored !raw !depth !p t =
  let !len = bLen t
      goN !i
        | i >= len = i
        | otherwise =
            let !c = bAt t i
            in if c == w8Space
                 || c == w8Tab
                 || c == w8SOH
                 || c == w8Comma
                 || c == w8RBrack
                 || c == w8RBrace
                 then i
                 else goN (i + 1)
      !endName = goN (p + 1)
      name = bSlice t (p + 1) endName
      p2 = skipFlowWS endName t
  in if p2 >= len
       then Just (YAnchored (Anchor name) YNull, p2)
       else case bAt t p2 of
         44 -> Just (YAnchored (Anchor name) YNull, endName)
         93 -> Just (YAnchored (Anchor name) YNull, endName)
         125 -> Just (YAnchored (Anchor name) YNull, endName)
         58
           | colonIsSeparator (p2 + 1) t ->
               Just (YAnchored (Anchor name) YNull, endName)
         _ -> case parseFlowValueWith raw depth p2 t of
           Just (v, p3) -> Just (YAnchored (Anchor name) v, p3)
           Nothing -> Just (YAnchored (Anchor name) YNull, p2)


{- | Alias in flow context. A distinct constructor keeps user strings opaque
to the resolution pass, even when they contain escaped control characters.
-}
parseFlowAlias :: Int -> Text -> Maybe (Value, Int)
parseFlowAlias !p t =
  let !len = bLen t
      goN !i
        | i >= len = i
        | otherwise =
            let !c = bAt t i
            in if c == w8Space
                 || c == w8Tab
                 || c == w8SOH
                 || c == w8Comma
                 || c == w8RBrack
                 || c == w8RBrace
                 then i
                 else goN (i + 1)
      !p1 = goN (p + 1)
      name = bSlice t (p + 1) p1
  in Just (YAlias (Anchor name), p1)


parseFlowSeq :: Int -> Int -> Text -> Maybe (Value, Int)
parseFlowSeq !depth !p0 t =
  let !len = bLen t
      goV !p acc
        | p >= len = Nothing
        | bAt t p == w8RBrack = Just (YSeq (V.fromList (reverse acc)), p + 1)
        | bAt t p == w8Hash = Nothing
        | bAt t p == w8STX = Nothing
        | otherwise = case parseFlowEntry depth p t of
            Nothing -> Nothing
            Just (v, p1) ->
              -- Skip any '\\2' comment-break sentinels before
              -- looking for the next separator.
              let p2 = skipFlowWSAndCB p1 t
              in if p2 >= len
                   then Nothing
                   else case bAt t p2 of
                     44 ->
                       -- ','
                       let p3 = p2 + 1
                       in if p3 < len && bAt t p3 == w8Hash
                            then Nothing
                            else goV (skipFlowWS p3 t) (v : acc)
                     93 ->
                       -- ']'
                       Just (YSeq (V.fromList (reverse (v : acc))), p2 + 1)
                     _ -> Nothing
  in goV (skipFlowWS p0 t) []


{- | Skip whitespace and comment-break sentinels ('\\2'). Used
between flow elements where a comment may sit just before the
separator.
-}
skipFlowWSAndCB :: Int -> Text -> Int
skipFlowWSAndCB !p t =
  let !len = bLen t
      go !i
        | i >= len = i
        | otherwise = case bAt t i of
            32 -> go (i + 1)
            9 -> go (i + 1)
            1 -> go (i + 1)
            2 -> go (i + 1)
            _ -> i
  in go p
{-# INLINE skipFlowWSAndCB #-}


{- | A flow-sequence entry can be a single value or a one-pair
mapping (with @key: value@ syntax, or just @: value@ for an empty
key).
-}
parseFlowEntry :: Int -> Int -> Text -> Maybe (Value, Int)
parseFlowEntry depth = parseFlowEntry' depth False


{- | When the @explicit@ flag is set, the entry is being parsed
under a leading @?@ marker (explicit key); the
implicit-key-spans-newline check is skipped.
-}
parseFlowEntry' :: Int -> Bool -> Int -> Text -> Maybe (Value, Int)
parseFlowEntry' !depth !explicit !p0 t =
  let !len = bLen t
      p = skipFlowWS p0 t
  in if p >= len
       then Nothing
       else
         if bAt t p == w8Quest
           && p + 1 < len
           && ( bAt t (p + 1) == w8Space
                  || bAt t (p + 1) == w8Tab
                  || bAt t (p + 1) == w8SOH
              )
           then parseFlowEntry' depth True (skipFlowWS (p + 1) t) t
           else
             if bAt t p == w8Colon && colonIsSeparator (p + 1) t
               then
                 let p1 = skipFlowWS (p + 1) t
                 in if p1 >= len
                      then Nothing
                      else case bAt t p1 of
                        44 -> Just (YMap (V.singleton (YNull, YNull)), p1)
                        93 -> Just (YMap (V.singleton (YNull, YNull)), p1)
                        _ -> case parseFlowValue depth p1 t of
                          Nothing -> Just (YMap (V.singleton (YNull, YNull)), p1)
                          Just (v, p2) ->
                            Just (YMap (V.singleton (YNull, v)), p2)
               else
                 let !flowOpener = case bAt t p of
                       34 -> True -- '"'
                       39 -> True -- '\''
                       91 -> True -- '['
                       123 -> True -- '{'
                       _ -> False
                 in case parseFlowValue depth p t of
                      Nothing -> Nothing
                      Just (k, p1) ->
                        let p2 = skipFlowWS p1 t
                        in if p2 < len
                             && bAt t p2 == w8Colon
                             && ( flowOpener
                                    || colonIsSeparator (p2 + 1) t
                                )
                             then
                               -- For flow /sequences/, an implicit
                               -- key->value pair appearing inline
                               -- must have key and ':' on the same
                               -- line (spec §7.4.1).
                               let span_ = bSlice t p p2
                               in if not explicit
                                    && (bAnyByte w8SOH span_ || T.length span_ > 1024)
                                    then Nothing
                                    else case parseFlowValue depth (skipFlowWS (p2 + 1) t) t of
                                      Nothing -> Just (YMap (V.singleton (k, YNull)), p2 + 1)
                                      Just (v, p3) -> Just (YMap (V.singleton (k, v)), p3)
                             else Just (if explicit then YMap (V.singleton (k, YNull)) else k, p1)


{- | Whether a colon at position @p@ acts as a key/value separator
in flow context: only when the very next character is a flow
stopper or whitespace.
-}
colonIsSeparator :: Int -> Text -> Bool
colonIsSeparator !p t
  | p >= bLen t = True
  | otherwise = flowColonFollower (bAt t p)
{-# INLINE colonIsSeparator #-}


parseFlowMap :: Int -> Int -> Text -> Maybe (Value, Int)
parseFlowMap !depth !p0 t =
  let !len = bLen t
      goV !p acc
        | p >= len = Nothing
        | bAt t p == w8RBrace = Just (YMap (V.fromList (reverse acc)), p + 1)
        | bAt t p == w8Comma = Nothing
        | otherwise = do
            let explicit = bAt t p == w8Quest && colonIsSeparator (p + 1) t
                start = if explicit then skipFlowWS (p + 1) t else p
            (key, end) <-
              if start < len && (bAt t start == w8Colon || explicit && bAt t start `elem` [w8Comma, w8RBrace])
                then Just (YNull, start)
                else parseFlowValue depth start t
            let separator = skipFlowWS end t
            if separator < len && bAt t separator == w8Colon
              then
                let valueStart = skipFlowWS (separator + 1) t
                in if valueStart < len && bAt t valueStart `elem` [w8Comma, w8RBrace]
                     then finish valueStart key YNull acc
                     else do
                       (value, next) <- parseFlowValue depth valueStart t
                       finish next key value acc
              else finish separator key YNull acc
      finish !p key value acc =
        let next = skipFlowWS p t
        in if next >= len
             then Nothing
             else case bAt t next of
               44 -> goV (skipFlowWS (next + 1) t) ((key, value) : acc)
               125 -> Just (YMap (V.fromList (reverse ((key, value) : acc))), next + 1)
               _ -> Nothing
  in goV (skipFlowWS p0 t) []


parseDQ :: Int -> Text -> Maybe (Value, Int)
parseDQ !p0 t =
  let !len = bLen t
      -- Fast path: SWAR-scan for the first occurrence of '\"',
      -- '\\\\', or '\\1'. If the closing quote comes first
      -- (i.e. no escape or newline-sentinel before it) we slice
      -- the body out without further work.
      !startSlice = p0 + 1
      !rest = bSlice t startSlice len
      !idx = bFindAnyOf3 w8DQuote w8Backslash w8SOH rest
  in if idx < 0
       then Nothing -- no terminator at all
       else
         let !absIdx = startSlice + idx
             !c = bAt t absIdx
         in if c == w8DQuote
              then
                Just
                  ( YString (bSlice t startSlice absIdx)
                  , absIdx + 1
                  )
              else decodeQuoted True p0 absIdx t


parseSQ :: Int -> Text -> Maybe (Value, Int)
parseSQ !p0 t =
  let !len = bLen t
      !startSlice = p0 + 1
      !rest = bSlice t startSlice len
      -- SWAR-scan for the first '\\'' or '\\1' sentinel. A bare
      -- '\\'' is a closing quote; a doubled '\\'\\'' is the
      -- single-quoted escape and forces the slow path.
      !idx = bFindAnyOf2 w8SQuote w8SOH rest
  in if idx < 0
       then Nothing
       else
         let !absIdx = startSlice + idx
             !c = bAt t absIdx
         in if c == w8SQuote
              then
                if absIdx + 1 < len && bAt t (absIdx + 1) == w8SQuote
                  then decodeQuoted False p0 absIdx t -- '\\'\\'' escape
                  else
                    Just
                      ( YString (bSlice t startSlice absIdx)
                      , absIdx + 1
                      )
              else decodeQuoted False p0 absIdx t


-- Decode escapes into one UTF-8 buffer, copying ordinary runs in bulk. The
-- preliminary scan bounds allocation by this scalar, not the remaining flow
-- document; escaped strings in a long sequence therefore stay linear in size.
decodeQuoted :: Bool -> Int -> Int -> Text -> Maybe (Value, Int)
decodeQuoted double opening firstSpecial source@(TI.Text input inputOffset inputLength) = do
  closing <- findClosing firstSpecial
  let start = opening + 1
      rawLength = closing - start
      -- The largest expansion is \L or \P: two input bytes become three.
      extra = rawLength `quot` 2 + 1
  if rawLength > maxBound - extra
    then Nothing
    else
      let (array, result) = TA.run2 $ do
            output <- TA.new (rawLength + extra)
            TA.copyI (firstSpecial - start) output 0 input (inputOffset + start)
            let done size = TA.shrinkM output size >> pure (output, Just size)
                go !cursor !written
                  | cursor == closing = done written
                  | otherwise =
                      let index =
                            if double
                              then bFindAnyOf3 w8DQuote w8Backslash w8SOH (bSlice source cursor (closing + 1))
                              else bFindAnyOf2 w8SQuote w8SOH (bSlice source cursor (closing + 1))
                          position = cursor + index
                          runLength = position - cursor
                          next = written + runLength
                      in do
                           TA.copyI runLength output written input (inputOffset + cursor)
                           if position == closing
                             then done next
                             else case bAt source position of
                               92
                                 | double ->
                                     decodeDQEscape
                                       source
                                       (position + 1)
                                       (pure (output, Nothing))
                                       ( \char following -> do
                                           width <- TU.unsafeWrite output next char
                                           go following (next + width)
                                       )
                               39 | not double -> do
                                 TA.unsafeWrite output next w8SQuote
                                 go (position + 2) (next + 1)
                               _ -> do
                                 TA.unsafeWrite output next w8Space
                                 go (position + 1) (next + 1)
            go firstSpecial (firstSpecial - start)
      in case result of
           Nothing -> Nothing
           Just size -> Just (YString (TI.text array 0 size), closing + 1)
  where
    findClosing !cursor
      | cursor >= inputLength = Nothing
      | otherwise =
          let quote = if double then w8DQuote else w8SQuote
              index = bFindAnyOf2 quote quote (bDrop cursor source)
              position = cursor + index
          in if index < 0
               then Nothing
               else
                 if double && escapedQuote (position - 1) False
                   then findClosing (position + 1)
                   else
                     if not double && position + 1 < inputLength && bAt source (position + 1) == w8SQuote
                       then findClosing (position + 2)
                       else Just position
    -- Only quote candidates need escape parity. Scanning every backslash here
    -- repeats work that the decoding pass already performs for each escape.
    escapedQuote !position !oddRun
      | position >= 0 && bAt source position == w8Backslash = escapedQuote (position - 1) (not oddRun)
      | otherwise = oddRun


plainStartAllowed :: Bool -> Text -> Bool
plainStartAllowed flow text = case T.uncons text of
  Nothing -> False
  Just (first, rest)
    | first `T.elem` ",[]{}#&*!|>'\"%@`" -> False
    | first `T.elem` "-?:" -> case T.uncons rest of
        Nothing -> False
        Just (next, _) -> next /= ' ' && next /= '\t' && next /= '\n' && next /= '\1' && (not flow || not (next `T.elem` ",[]{}"))
    | otherwise -> True


parseFlowPlainWith :: (Text -> Value) -> Int -> Text -> Maybe (Value, Int)
parseFlowPlainWith resolve !p t =
  let !len = bLen t
      -- Walk the body looking for a stopper. Track in 'sawSOH'
      -- whether we crossed any '\\1' newline sentinel, so we
      -- can skip the (allocating) 'T.replace' fold below in
      -- the common case.
      scan !i !crossedNewline
        | i >= len = (i, crossedNewline)
        | otherwise =
            let !c = bAt t i
            in if isFlowStopByte c
                 || (c == w8Colon && colonStopByte i)
                 then (i, crossedNewline)
                 else scan (i + 1) (crossedNewline || c == w8SOH)
      colonStopByte !i =
        let i1 = i + 1
        in i1 >= len || flowColonFollower (bAt t i1)
      (!p', !sawSOH) = scan p False
      raw = bSlice t p p'
      folded
        | sawSOH = T.replace tSOH tSpace raw
        | otherwise = raw
      stripped = stripYamlEnd folded
  in if T.null stripped || not (plainStartAllowed True stripped) || not (T.all validNonBreak stripped)
       then Nothing
       else
         if stripped == tDashStr
           then Nothing
           else Just (resolve stripped, p')


-- ---------------------------------------------------------------------------
-- Constant-string CAFs.
--
-- 'T.pack \"…\"' is /not/ free even for a static literal — it
-- allocates the result 'Text' on every call unless GHC happens
-- to lift it. Hoisting frequently-used literals to top-level
-- saves a re-pack on every hot-loop iteration.
-- ---------------------------------------------------------------------------

tSOH, tSTX, tSpace, tNL :: Text
tSOH = T.singleton '\1'
tSTX = T.singleton '\2'
tSpace = T.singleton ' '
tNL = T.singleton '\n'


tDashStr :: Text
tDashStr = T.pack "-"


tQuestStr :: Text
tQuestStr = T.pack "?"


tColonStr, tColonSpace, tColonTab :: Text
tColonStr = T.pack ":"
tColonSpace = T.pack ": "
tColonTab = T.pack ":\t"


tYamlTagPrefix :: Text
tYamlTagPrefix = T.pack "tag:yaml.org,2002:"


{- | Bytes that terminate a flow-context plain scalar
(excluding the colon-with-follower case which is handled by
the caller).
-}
isFlowStopByte :: Word8 -> Bool
isFlowStopByte c =
  c == w8Comma
    || c == w8LBrack
    || c == w8RBrack
    || c == w8LBrace
    || c == w8RBrace
    || c == w8STX
{-# INLINE isFlowStopByte #-}


{- | A byte that, when it follows a ':' in flow context, makes
that ':' a key/value separator (and therefore a stop point
for a flow plain scalar).
-}
flowColonFollower :: Word8 -> Bool
flowColonFollower c =
  c == w8Space
    || c == w8Tab
    || c == w8SOH
    || c == w8Comma
    || c == w8RBrack
    || c == w8RBrace
{-# INLINE flowColonFollower #-}


skipFlowWS :: Int -> Text -> Int
skipFlowWS !p t =
  let !len = bLen t
      go !i
        | i >= len = i
        | otherwise = case bAt t i of
            32 -> go (i + 1) -- ' '
            9 -> go (i + 1) -- '\t'
            1 -> go (i + 1) -- '\1'
            2 -> go (i + 1) -- comment break
            _ -> i
  in go p
{-# INLINE skipFlowWS #-}


-- ---------------------------------------------------------------------------
-- Block style: dispatch from a line we haven't consumed yet.
-- ---------------------------------------------------------------------------

{- | A line starts a block-sequence entry if its body is exactly
@-@ or starts with @- @ / @-<TAB>@.
-}
isSeqItem :: Text -> Bool
isSeqItem b =
  case bLen b of
    0 -> False
    1 -> bAt b 0 == 45 -- '-'
    _ ->
      bAt b 0 == 45
        && let !c1 = bAt b 1
           in c1 == w8Space || c1 == w8Tab
{-# INLINE isSeqItem #-}


-- | Same shape for the explicit-key marker @?@.
isExplicitKey :: Text -> Bool
isExplicitKey b =
  case bLen b of
    0 -> False
    1 -> bAt b 0 == w8Quest
    _ ->
      bAt b 0 == w8Quest
        && let !c1 = bAt b 1
           in c1 == w8Space || c1 == w8Tab
{-# INLINE isExplicitKey #-}


{- | True when @b@ is exactly @\":\"@ or starts with @\": \"@ /
@\":\\t\"@. Used to recognise the value-continuation marker
after an explicit @?@ key.
-}
isColonMarker :: Text -> Bool
isColonMarker b =
  case bLen b of
    0 -> False
    1 -> bAt b 0 == w8Colon
    _ ->
      bAt b 0 == w8Colon
        && let !c1 = bAt b 1
           in c1 == w8Space || c1 == w8Tab
{-# INLINE isColonMarker #-}


parseBlockOrPlain :: PLine -> P Value
parseBlockOrPlain l
  | isSeqItem body = parseBlockSeq (lineIndent l)
  | isExplicitKey body = parseExplicitMap (lineIndent l)
  | otherwise = case findAliasKeySplit body of
      Just (aliasName, vRest) ->
        parseBlockMapAliasFirst
          (lineIndent l)
          aliasName
          vRest
      Nothing -> case findKeyValueSplit body of
        Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
        Nothing -> parsePlainScalar (lineIndent l) body
  where
    body = lineBody l


{- | Block mapping whose first key is an alias node (parsed via
'findAliasKeySplit'). Same shape as 'parseBlockMap' otherwise.
-}
parseBlockMapAliasFirst :: Int -> Text -> Text -> P Value
parseBlockMapAliasFirst !ind aliasName firstRest = do
  dropLine
  k0 <- resolveAnchor aliasName
  v0 <- parseImplicitMapValue ind firstRest
  rest <- collect [(k0, v0)]
  pure (YMap (V.fromList (reverse rest)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | isSeqItem (lineBody l) -> pure acc
          | isExplicitKey (lineBody l) -> pure acc
          | startsWithTab (lineBody l) ->
              failP $
                "tab character used as indentation (line "
                  ++ show (lineNo l)
                  ++ ")"
          | otherwise -> case findAliasKeySplit (lineBody l) of
              Just (a, vRest) -> do
                dropLine
                k <- resolveAnchor a
                v <- parseImplicitMapValue ind vRest
                collect ((k, v) : acc)
              Nothing -> case findKeyValueSplit (lineBody l) of
                Just (k, vRest) -> do
                  dropLine
                  v <- parseImplicitMapValue ind vRest
                  key <- parseBlockKey k
                  collect ((key, v) : acc)
                Nothing -> pure acc


-- ---------------------------------------------------------------------------
-- Block sequence
-- ---------------------------------------------------------------------------

parseBlockSeq :: Int -> P Value
parseBlockSeq !ind = collect [] >>= \xs -> pure (YSeq (V.fromList (reverse xs)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | not (isSeqItem (lineBody l)) ->
              pure acc
          | otherwise -> do
              v <- parseSeqItem ind
              collect (v : acc)


parseSeqItem :: Int -> P Value
parseSeqItem !ind = do
  Just l <- popLine
  let body = lineBody l
      after
        | body == "-" = T.empty
        | otherwise = T.drop 2 body
      after' = stripYamlStart after
  -- '-<TAB><INDICATOR>' or '- <TAB><INDICATOR>' (nested block
  -- marker reached via a tab in the indent column) is 'tab as
  -- indentation' per spec §6.1. The plain-scalar form
  -- '-<TAB>x' is fine because no further indent calculation
  -- happens; but '-\\t-' / '- \\t-' / '-\\t?' / etc. would set
  -- the nested block's indent to a tab-containing column.
  let separatorHasTab =
        T.any
          (== '\t')
          ( T.takeWhile
              (\c -> c == ' ' || c == '\t')
              (T.drop 1 body)
          )
  when (separatorHasTab && startsWithBlockIndicator after') $
    failP $
      "tab character used as indentation before nested block marker (line "
        ++ show (lineNo l)
        ++ ")"
  let isCommentOnly = case T.uncons after' of
        Just ('#', _) -> True
        _ -> False
  if isCommentOnly || T.null after'
    then do
      mNext <- peekLine
      case mNext of
        Just l2
          | lineIndent l2 > ind ->
              withParentInd ind (parseNode (lineIndent l2))
        _ -> pure YNull
    else do
      let isCarrier = case T.uncons after' of
            Just ('|', _) -> True
            Just ('>', _) -> True
            Just ('!', _) -> True
            Just ('&', _) -> True
            _ -> False
          isNestedBlock = case T.uncons after' of
            Just ('-', rest) -> startsWithSeparator rest
            Just ('?', rest) -> startsWithSeparator rest
            _ -> False
          extraWS = T.length after - T.length after'
          virtInd
            | isCarrier = ind
            -- For nested block constructs the actual
            -- column of the next dash / explicit-key marker
            -- is past the outer dash AND any extra
            -- separator spaces. Use it so collectors at
            -- that level match real-world inputs (A2M4).
            | isNestedBlock = ind + 2 + extraWS
            | otherwise = ind + 2
          virt = PLine (lineNo l) virtInd LContent after' after'
      pushLine virt
      withParentInd ind $
        withSequenceItem True $
          if T.isPrefixOf "!" after' || T.isPrefixOf "&" after'
            then withInMapValue True (parseNode virtInd)
            else parseNode virtInd


-- ---------------------------------------------------------------------------
-- Block mapping
-- ---------------------------------------------------------------------------

parseBlockMap :: Int -> Text -> Text -> P Value
parseBlockMap ind firstKey firstRest = do
  key <- parseBlockKey firstKey
  parseBlockMapWithKey ind key firstRest


parseBlockKey :: Text -> P Value
parseBlockKey raw
  | T.length raw > 1024 = failP "YAML: implicit mapping key exceeds 1024 characters"
  | T.null raw = pure YNull
  | otherwise = case T.uncons raw of
      Just ('"', _) -> quoted parseDQ
      Just ('\'', _) -> quoted parseSQ
      Just (c, _) | c == '!' || c == '&' || c == '*' || c == '[' || c == '{' -> parseRawKey raw >>= recordAndResolveFlow
      _ -> do
        unless ((plainStartAllowed False raw || raw `elem` ["-", "?", ":"]) && T.all validNonBreak raw) (failP "YAML: invalid plain mapping key")
        pure (resolvePlain raw)
  where
    quoted parser = case parser 0 raw of
      Just (key, end) | T.null (stripYaml (bDrop end raw)) -> pure key
      _ -> failP "YAML: invalid quoted mapping key"


parseBlockMapWithKey :: Int -> Value -> Text -> P Value
parseBlockMapWithKey !ind firstKey firstRest = do
  dropLine
  v0 <- parseImplicitMapValue ind firstRest
  rest <- collect [(firstKey, v0)]
  pure (YMap (V.fromList (reverse rest)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | otherwise ->
              let body = lineBody l
              in case T.uncons body of
                   Nothing -> pure acc
                   Just (h, _)
                     | h == '\t' ->
                         failP $
                           "tab character used as indentation (line "
                             ++ show (lineNo l)
                             ++ ")"
                     | h == '-'
                     , isSeqItem body ->
                         pure acc
                     | h == '?'
                     , isExplicitKey body -> do
                         k <- readExplicitPart "?"
                         v <- readExplicitValue
                         collect ((k, v) : acc)
                     | h == '*'
                     , Just (aliasName, vRest) <- findAliasKeySplit body -> do
                         dropLine
                         k <- resolveAnchor aliasName
                         v <- parseImplicitMapValue ind vRest
                         collect ((k, v) : acc)
                     | otherwise -> case findKeyValueSplit body of
                         Just (k, vRest) -> do
                           dropLine
                           let (anchors, k') = stripKeyProperties k
                           case T.uncons k' of
                             Just ('*', _)
                               | not (null anchors) ->
                                   failP $
                                     "anchor immediately followed by alias key (line "
                                       ++ show (lineNo l)
                                       ++ ")"
                             _ -> pure ()
                           kv <- parseBlockKey k
                           v <- parseImplicitMapValue ind vRest
                           collect ((kv, v) : acc)
                         Nothing -> pure acc

    readExplicitPart marker = do
      Just l <- popLine
      let body = lineBody l
          afterMarker =
            if body == marker
              then T.empty
              else T.drop 1 body
          rest0 = stripYamlStart (T.drop 1 afterMarker)
          rest = case T.uncons rest0 of
            Just ('#', _) -> T.empty
            _ -> rest0
      case T.uncons afterMarker of
        Just ('\t', _) ->
          failP $
            "tab character after explicit-key marker (line "
              ++ show (lineNo l)
              ++ ")"
        _ -> pure ()
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2
              | lineIndent l2 > lineIndent l ->
                  parseNode (lineIndent l2)
            Just l2
              | lineIndent l2 == lineIndent l
              , isSeqItem (lineBody l2) ->
                  parseBlockSeq (lineIndent l2)
            _ -> pure YNull
        else do
          let isBlockScalarHead = case T.uncons rest of
                Just ('|', _) -> True
                Just ('>', _) -> True
                _ -> False
              virtInd
                | isBlockScalarHead = lineIndent l
                | otherwise = lineIndent l + 2
          pushLine
            ( PLine
                (lineNo l)
                virtInd
                LContent
                rest
                rest
            )
          withParentInd (lineIndent l) (parseNode virtInd)

    readExplicitValue = do
      mPL <- peekLine
      case mPL of
        Just l
          | lineIndent l == ind
              && isColonMarker (lineBody l) ->
              readExplicitPart ":"
        _ -> pure YNull


startsWithTab :: Text -> Bool
startsWithTab t = case T.uncons t of
  Just ('\t', _) -> True
  _ -> False


-- | True when @t@ is empty or starts with a space / tab.
startsWithSeparator :: Text -> Bool
startsWithSeparator t = case T.uncons t of
  Nothing -> True
  Just (' ', _) -> True
  Just ('\t', _) -> True
  _ -> False


{- | True when @t@ begins with a block-context structural marker
('-' or '?' followed by space / tab / EOL).
-}
startsWithBlockIndicator :: Text -> Bool
startsWithBlockIndicator t = case T.uncons t of
  Just ('-', rest) -> isBlockSep rest
  Just ('?', rest) -> isBlockSep rest
  _ -> False
  where
    isBlockSep r = case T.uncons r of
      Nothing -> True
      Just (' ', _) -> True
      Just ('\t', _) -> True
      _ -> False


{- | Strip leading anchor / tag tokens (separated by spaces) from
a block-mapping key string. Returns the list of anchor names
encountered and the remainder text. Tags are dropped silently
(they don't change the key projection).
-}
stripKeyProperties :: Text -> ([Text], Text)
stripKeyProperties = go []
  where
    go acc t = case T.uncons (stripYamlStart t) of
      Just ('&', rest) ->
        let (name, after) = takeAnchorName rest
        in go (name : acc) after
      Just ('!', rest) ->
        let (_tg, after) = T.span (\c -> not (c == ' ' || c == '\t')) rest
        in go acc after
      _ -> (reverse acc, stripYamlStart t)


{- | Recognise @*alias : value@ style mapping entries where the key
is an alias node. Returns the alias name (without the @*@) and
the value text after the colon, or 'Nothing' when the line
doesn't have this shape.
-}
findAliasKeySplit :: Text -> Maybe (Text, Text)
findAliasKeySplit t = case T.uncons t of
  Just ('*', rest) ->
    let (name, after) = takeAnchorName rest
        afterTrim = stripYamlStart after
    in case T.uncons afterTrim of
         Just (':', tail_)
           | T.null tail_ || T.head tail_ == ' ' || T.head tail_ == '\t' ->
               Just (name, T.drop 1 afterTrim)
         _ -> Nothing
  _ -> Nothing


parseImplicitMapValue :: Int -> Text -> P Value
parseImplicitMapValue ind rest = withParentInd ind (withInMapValue True (withSequenceItem False (parseImplicitMapValueBody ind rest)))


parseImplicitMapValueBody :: Int -> Text -> P Value
parseImplicitMapValueBody !ind vRest =
  if T.null after
    then do
      mNext <- peekLine
      case mNext of
        Just l2
          | lineIndent l2 > ind -> do
              -- Strip leading tabs from the body, which YAML
              -- treats as additional whitespace (not part of
              -- the scalar text).
              let body' = T.dropWhile (== '\t') (lineBody l2)
              modifyS
                ( \s ->
                    s
                      { psLines = case psLines s of
                          (h : rs) -> h {lineBody = body'} : rs
                          [] -> []
                      }
                )
              parseNode (lineIndent l2)
          | lineIndent l2 == ind
              && (isSeqItem (lineBody l2)) ->
              parseBlockSeq ind
        _ -> pure YNull
    else case T.uncons after of
      Just (h, _)
        | h == '|' -> do
            pushLine (PLine 0 ind LContent after after)
            parseBlockScalar Literal
        | h == '>' -> do
            pushLine (PLine 0 ind LContent after after)
            parseBlockScalar Folded
        | h == '[' -> do
            pushLine (PLine 0 (ind + 2) LContent after after)
            Just l <- popLine
            consumeFlowAt (ind + 1) (lineBody l)
        | h == '{' -> do
            pushLine (PLine 0 (ind + 2) LContent after after)
            Just l <- popLine
            consumeFlowAt (ind + 1) (lineBody l)
        | h == '&' -> do
            pushLine (PLine 0 ind LContent after after)
            withInMapValue True parseAnchored
        | h == '*' -> do
            pushLine (PLine 0 (ind + 2) LContent after after)
            parseAlias
        | h == '!' -> do
            pushLine (PLine 0 ind LContent after after)
            parseTagged
        | h == '"' -> consumeQuotedAt '"' (ind + 1) after
        | h == '\'' -> consumeQuotedAt '\'' (ind + 1) after
        | h == '#' -> parseImplicitMapValueEmpty ind
        | otherwise -> goPlain
      Nothing -> goPlain
  where
    goPlain :: P Value
    goPlain = do
      -- Fast path: if the next physical line in the stream
      -- can't extend this plain scalar (lower indent than
      -- the value column, OR no next line at all), we can
      -- resolve directly without 'parsePlainScalar'.
      ls <- getLines
      let canExtend = case ls of
            (n : _) ->
              lineKind n == LContent
                && lineIndent n > ind
            _ -> False
          sclean = stripYamlEnd (stripInlineComment after)
          isUnambiguous =
            not canExtend
              && case findKeyValueSplit sclean of
                Just _ -> False
                Nothing -> True
      if isUnambiguous
        then resolvePlainP sclean
        else do
          pushLine (PLine 0 (ind + 1) LContent after after)
          withInMapValue True (parsePlainScalar (ind + 1) after)
    after = stripYamlStart vRest


parseImplicitMapValueEmpty :: Int -> P Value
parseImplicitMapValueEmpty !ind = do
  mNext <- peekLine
  case mNext of
    Just l2
      | lineIndent l2 > ind -> do
          let body' = T.dropWhile (== '\t') (lineBody l2)
          modifyS
            ( \s ->
                s
                  { psLines = case psLines s of
                      (h : rs) -> h {lineBody = body'} : rs
                      [] -> []
                  }
            )
          parseNode (lineIndent l2)
      | lineIndent l2 == ind && isSeqItem (lineBody l2) ->
          parseBlockSeq ind
    _ -> pure YNull


-- ---------------------------------------------------------------------------
-- Explicit-key mapping (?-form)
-- ---------------------------------------------------------------------------

parseExplicitMap :: Int -> P Value
parseExplicitMap !ind = collect [] >>= \kvs -> pure (YMap (V.fromList (reverse kvs)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | isExplicitKey (lineBody l) -> do
              k <- readExplicitPart "?"
              v <- readExplicitValue
              collect ((k, v) : acc)
          -- A bare ':' or ': value' / ':<TAB>value' line at the
          -- mapping indent is an entry with an implicit (null)
          -- key. Per spec §8.18, omitting the '?' is permitted.
          | isColonMarker (lineBody l) -> do
              v <- readExplicitPart ":"
              collect ((YNull, v) : acc)
          -- An ordinary 'key: value' implicit pair after a ?-form
          -- entry continues the same mapping (spec §8.18).
          | otherwise -> case findKeyValueSplit (lineBody l) of
              Just (k, vRest) -> do
                dropLine
                kv <- parseBlockKey k
                v <- parseImplicitMapValue ind vRest
                collect ((kv, v) : acc)
              Nothing -> pure acc

    readExplicitPart marker = do
      Just l <- popLine
      let body = lineBody l
          afterMarker =
            if body == marker
              then T.empty
              else T.drop 1 body
          rest0 = stripYamlStart (T.drop 1 afterMarker)
          -- '#' immediately after the explicit-key marker
          -- starts a comment; the value comes from the next
          -- continuation line.
          rest = case T.uncons rest0 of
            Just ('#', _) -> T.empty
            _ -> rest0
      case T.uncons afterMarker of
        Just ('\t', _) ->
          failP $
            "tab character after explicit-key marker (line "
              ++ show (lineNo l)
              ++ ")"
        _ -> pure ()
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l -> parseNode (lineIndent l2)
            Just l2
              | lineIndent l2 == lineIndent l
              , isSeqItem (lineBody l2) ->
                  parseBlockSeq (lineIndent l2)
            _ -> pure YNull
        else do
          let isBlockScalarHead = case T.uncons rest of
                Just ('|', _) -> True
                Just ('>', _) -> True
                _ -> False
              virtInd
                | isBlockScalarHead = lineIndent l
                | otherwise = lineIndent l + 2
          pushLine (PLine (lineNo l) virtInd LContent rest rest)
          withParentInd (lineIndent l) (parseNode virtInd)

    readExplicitValue = do
      mPL <- peekLine
      case mPL of
        Just l
          | lineIndent l == ind
              && isColonMarker (lineBody l) ->
              readExplicitPart ":"
        _ -> pure YNull


-- ---------------------------------------------------------------------------
-- Plain scalars (multi-line)
-- ---------------------------------------------------------------------------

parsePlainScalar :: Int -> Text -> P Value
parsePlainScalar !ind firstBody = do
  parentInd <- getParentInd
  inMapValue <- getInMapValue
  parsePlainScalarAt parentInd inMapValue ind firstBody


{- | Like 'parsePlainScalar' but with explicit parent indent and
"are we in a mapping-value position" flag.
-}
parsePlainScalarAt :: Int -> Bool -> Int -> Text -> P Value
parsePlainScalarAt !parentInd !inMapValue !baseIndArg firstBody = do
  unless (plainStartAllowed False firstBody) (failP "YAML: invalid plain scalar indicator")
  let !ind = baseIndArg
      !_p = parentInd
      !_m = inMapValue
  dropLine
  -- Fast path: a body with no '#' at all has no comment, so we
  -- skip the comparison probe entirely.
  let !hasHash = bAnyByte w8Hash firstBody
      !stripped =
        if hasHash
          then stripInlineComment firstBody
          else firstBody
      !first = stripYamlEnd stripped
      !hadComment =
        hasHash
          && bLen stripped < bLen (stripYamlEnd firstBody)
  -- A plain scalar may not contain ': ' (colon-space) in block
  -- context — that would form a nested mapping (spec §7.3.3).
  case findKeyValueSplit first of
    Just _ -> failP $ "nested mapping in plain scalar: " ++ show first
    _ -> pure ()
  -- A trailing comment on the first line of a plain scalar
  -- followed by a continuation line is malformed (the comment
  -- would silently break the scalar).
  when hadComment $ do
    ls' <- getLines
    case ls' of
      (l2 : _)
        | lineIndent l2 >= ind
        , lineKind l2 == LContent
        , not (isSeqItem (lineBody l2))
        , not (isExplicitKey (lineBody l2))
        , case findKeyValueSplit (lineBody l2) of
            Just _ -> False
            Nothing -> True ->
            failP $
              "comment between plain-scalar lines (line "
                ++ show (lineNo l2 - 1)
                ++ ")"
      _ -> pure ()
  rest <- collectFolds ind 0 []
  let !final = joinPlain (first : rest)
  resolvePlainP final
  where
    -- @blanks@ counts the run of consecutive blank lines we've
    -- absorbed since the last non-blank continuation line. The
    -- collected list interleaves non-blank line bodies with marker
    -- entries representing blank-line runs (encoded as the empty
    -- string preceded by a special sentinel, see joinPlain).
    --
    -- A continuation line is accepted when its indent is /strictly
    -- greater/ than the scalar's base indent and it doesn't look
    -- like a new collection entry (mapping key, seq item, explicit
    -- '?' key).
    collectFolds baseInd blanks acc = do
      ls <- getLines
      case ls of
        [] -> pure (reverse acc)
        (l : _)
          | lineKind l == LBlank ->
              do consumeOne; collectFolds baseInd (blanks + 1) acc
          | lineKind l == LComment
              && lineIndent l > baseInd ->
              do consumeOne; collectFolds baseInd blanks acc
          | (lineKind l == LContent || lineKind l == LDirective)
              -- Standard continuation: indent >= baseInd.
              -- Shallow continuation rules:
              --   * indent in (parentInd .. baseInd) range is OK
              --     for plain scalar content (UV7Q).
              --   * a '- ' shallow line (looks like nested seq) at
              --     the OUTERMOST seq (parentInd == 0, NOT inside
              --     a mapping value) also folds into the plain
              --     scalar (AB8U).
              && ( lineIndent l >= baseInd
                     || ( lineIndent l > parentInd
                            && not (isExplicitKey (lineBody l))
                            && ( not (isSeqItem (lineBody l))
                                   || (parentInd == 0 && not inMapValue)
                               )
                        )
                 )
              && not
                ( isSeqItem (lineBody l)
                    && lineIndent l >= baseInd
                )
              && not
                ( isExplicitKey (lineBody l)
                    && lineIndent l >= baseInd
                )
              && case findKeyValueSplit (lineBody l) of
                Just _ -> False
                Nothing -> True ->
              do
                -- For an LDirective line (begins with '%'), only
                -- accept it as scalar content if there's no later
                -- '---' / '...' marker that would make it a real
                -- directive for a subsequent document.
                accept <- case lineKind l of
                  LDirective -> do
                    ls' <- getLines
                    pure (not (any isMarker ls'))
                  _ -> pure True
                if not accept
                  then pure (reverse acc)
                  else do
                    consumeOne
                    let raw = lineBody l
                        s0 = stripYamlEnd (stripInlineComment raw)
                        s = T.dropWhile (\c -> c == ' ' || c == '\t') s0
                        hadComment = bLen s0 < bLen (stripYamlEnd raw)
                        prefix
                          | blanks == 0 = s
                          | otherwise = T.replicate blanks tNL <> s
                    when hadComment $ do
                      ls' <- getLines
                      case ls' of
                        (l2 : _)
                          | lineIndent l2 >= baseInd
                          , lineKind l2 == LContent
                          , not (isSeqItem (lineBody l2))
                          , not (isExplicitKey (lineBody l2))
                          , case findKeyValueSplit (lineBody l2) of
                              Just _ -> False
                              Nothing -> True ->
                              failP $
                                "comment between plain-scalar lines (line "
                                  ++ show (lineNo l)
                                  ++ ")"
                        _ -> pure ()
                    collectFolds baseInd 0 (prefix : acc)
          | otherwise -> pure (reverse acc)
      where
        isMarker l = lineKind l == LDocStart || lineKind l == LDocEnd

    consumeOne = do
      ls <- getLines
      case ls of
        (_ : xs) -> setLines xs
        [] -> pure ()

    -- Join the collected pieces; pieces that already start with a
    -- newline marker are joined with no separator.
    -- Most plain scalars are a single line; short-circuit
    -- those without building any intermediate list. For the
    -- multi-line case build the result in one 'T.concat'
    -- allocation rather than chaining '<>' (which is O(N^2) on
    -- the result text).
    joinPlain [] = T.empty
    joinPlain [x] = x
    joinPlain xs = T.concat (interleave xs)
      where
        interleave [] = []
        interleave [x] = [x]
        interleave (x : y : zs)
          | T.isPrefixOf tNL y = x : interleave (y : zs)
          | otherwise = x : tSpace : interleave (y : zs)


-- ---------------------------------------------------------------------------
-- Block scalars
-- ---------------------------------------------------------------------------

data Chomp = Strip | Clip | Keep deriving (Eq, Show)


data BlockKind = Literal | Folded deriving (Eq, Show)


parseBlockScalar :: BlockKind -> P Value
parseBlockScalar k = do
  Just l <- popLine
  let header = T.drop 1 (lineBody l) -- drop '|' or '>'
  (chomp, hint) <- case parseHeader header of
    Right h -> pure h
    Left err ->
      failP $
        "invalid block scalar header: "
          ++ err
          ++ " (line "
          ++ show (lineNo l)
          ++ ")"
  let explicitBase = (max 0 (lineIndent l) +) <$> hint
  body <- collectScalarLines (lineIndent l) explicitBase
  -- Per spec §8.1.1: a /leading/ blank line whose indent is
  -- greater than the first content line's indent is invalid
  -- (the missing indent indicator can't be recovered from
  -- blank lines alone).
  case explicitBase of
    Nothing ->
      let leadingBlanks = takeWhile (\(_, b) -> T.null b) body
          afterBlanks = dropWhile (\(_, b) -> T.null b) body
      in case afterBlanks of
           ((firstC, _) : _)
             | firstC >= 0
             , any (\(i, _) -> i > firstC) leadingBlanks ->
                 failP $
                   "block scalar baseline below earlier blank-line indent (line "
                     ++ show (lineNo l)
                     ++ ")"
           _ -> pure ()
    _ -> pure ()
  let bodyAdj = case nonEmptyContent explicitBase body of
        True -> body
        False -> map (\(i, b) -> if i < 0 then (i, b) else (-1, b)) body
      txt = case k of
        Literal -> joinLiteralAt explicitBase chomp bodyAdj
        Folded -> joinFoldedAt explicitBase chomp bodyAdj
  unless (case asciiSourceStatus txt of 0# -> True; _ -> T.all (\c -> c == '\n' || validNonBreak c) txt) (failP "YAML: invalid character in block scalar")
  pure (YString txt)
  where
    nonEmptyContent baseline = any (\(i, b) -> i >= 0 && (not (T.null b) || maybe False (i >) baseline))

    parseHeader :: Text -> Either String (Chomp, Maybe Int)
    parseHeader h0 =
      let
        -- Anything on the header line after a single '#' (with
        -- preceding whitespace) is a comment.
        h = stripInlineComment h0
        rest = T.unpack (stripYaml h)
        chompOf '-' = Strip
        chompOf '+' = Keep
        chompOf _ = Clip
        mkHint c
          | c >= '1' && c <= '9' = Right (Just (digitToInt c))
          -- '|0' is invalid (indent indicator must be 1..9).
          | c == '0' = Left "indent indicator must be 1..9"
          | otherwise = Left ("unexpected character " ++ [c])
      in
        case rest of
          [] -> Right (Clip, Nothing)
          [c]
            | c == '-' || c == '+' -> Right (chompOf c, Nothing)
            | isDigit c -> (\h_ -> (Clip, h_)) <$> mkHint c
            | otherwise -> Left ("unexpected character " ++ [c])
          [a, b]
            | (a == '-' || a == '+') && isDigit b ->
                (\h_ -> (chompOf a, h_)) <$> mkHint b
            | isDigit a && (b == '-' || b == '+') ->
                (\h_ -> (chompOf b, h_)) <$> mkHint a
            | otherwise -> Left ("unexpected header " ++ rest)
          _ -> Left ("unexpected header " ++ rest)


{- | Collect the body lines of a block scalar.

The semantics: blank / more-indented blank lines belong to the
scalar regardless of their column. Once we've seen a content
line, that line's indent /is/ the "base indent" of the scalar;
the scalar terminates on the first subsequent line whose indent
falls /at or below/ that base. Lines whose source classified as
@LComment@ but sit at indent > base are treated as scalar
content (the '#' is data); comments at base or shallower
terminate.
-}
collectScalarLines :: Int -> Maybe Int -> P [(Int, Text)]
collectScalarLines !parent !mExplicit = collect mExplicit []
  where
    -- @mBase@ is the established base indent (after the first
    -- content line, or pre-seeded by an explicit indent
    -- indicator). @acc@ is the reverse-accumulated body.
    collect mBase acc = do
      ls <- getLines
      case ls of
        [] -> pure (reverse acc)
        (l : _)
          | lineKind l == LBlank ->
              do
                let ind = lineIndent l
                    raw = lineRawBody l
                    hasTabs = not (T.null raw)
                -- A 'blank' line whose only content is a TAB
                -- and that sits at or below 'parent' before any
                -- content line establishes a baseline is using
                -- a tab as block-scalar indentation: invalid
                -- per spec §6.1 (Y79Y/000).
                case mBase of
                  Nothing
                    | hasTabs
                    , ind <= parent
                    , parent >= 0 ->
                        failP $
                          "tab character used as block-scalar indentation (line "
                            ++ show (lineNo l)
                            ++ ")"
                  _ -> pure ()
                _ <- consumeOne
                let isMoreIndented = case mBase of
                      Just b -> ind > b
                      Nothing -> ind > parent
                if isMoreIndented
                  then collect mBase ((ind, raw) : acc)
                  else
                    if hasTabs
                      then case mBase of
                        Just b
                          | ind >= b ->
                              collect mBase ((ind, raw) : acc)
                        _ -> collect mBase ((-1, T.empty) : acc)
                      else collect mBase ((-1, T.empty) : acc)
          | lineKind l == LDocStart || lineKind l == LDocEnd ->
              pure (reverse acc)
          | otherwise ->
              let ind = lineIndent l
                  inside = case mBase of
                    Just b -> ind >= b
                    Nothing -> ind > parent
              in if not inside
                   then pure (reverse acc)
                   else do
                     _ <- consumeOne
                     let mBase' = case mBase of
                           Just _ -> mBase
                           Nothing -> Just ind
                     collect mBase' ((ind, lineRawBody l) : acc)

    consumeOne = do
      ls <- getLines
      case ls of
        (_ : xs) -> setLines xs >> pure ()
        [] -> pure ()


-- Track the last content byte separately from trailing blank-line bytes, so
-- stripped/clipped scalars neither allocate nor retain the discarded suffix.
data LiteralExtent = LiteralExtent
  { literalBytes :: {-# UNPACK #-} !Int
  , literalContentEnd :: {-# UNPACK #-} !Int
  }


joinLiteralAt :: Maybe Int -> Chomp -> [(Int, Text)] -> Text
joinLiteralAt mExpl chomp xs =
  let baseInd = case mExpl of Just base -> base; Nothing -> minNonNegative xs
      measure !total !contentEnd [] = LiteralExtent total contentEnd
      measure !total !contentEnd ((indent, body) : rest) =
        let width = if indent < 0 then 0 else max 0 (indent - baseInd) + bLen body
            next = total + width + 1
        in measure next (if width == 0 then contentEnd else next - 1) rest
      extent = measure 0 0 xs
      size = case chomp of
        Keep -> literalBytes extent
        Strip -> literalContentEnd extent
        Clip -> if literalContentEnd extent == 0 then 0 else literalContentEnd extent + 1
      array = TA.run $ do
        output <- TA.new size
        let spacesAt !position !count
              | count <= 0 = pure ()
              | otherwise = TA.unsafeWrite output position w8Space >> spacesAt (position + 1) (count - 1)
            writeLines !_ [] = pure output
            writeLines !position _ | position >= size = pure output
            writeLines !position ((indent, body) : rest)
              | indent < 0 = TA.unsafeWrite output position 10 >> writeLines (position + 1) rest
              | otherwise = case body of
                  TI.Text input offset length_ -> do
                    let padding = max 0 (indent - baseInd)
                        content = position + padding
                        newline = content + length_
                    spacesAt position padding
                    TA.copyI length_ output content input offset
                    when (newline < size) (TA.unsafeWrite output newline 10)
                    writeLines (newline + 1) rest
        writeLines 0 xs
  in if size == 0 then T.empty else TI.text array 0 size


joinFoldedAt :: Maybe Int -> Chomp -> [(Int, Text)] -> Text
joinFoldedAt mExpl chomp xs =
  let baseInd = case mExpl of
        Just b -> b
        Nothing -> minNonNegative xs
      raw
        | null xs = T.empty
        | otherwise = T.concat (foldFirst xs baseInd) <> tNL
  in chompText chomp raw
  where
    isBlank (i, b) = i < 0 || T.null b

    -- A line is "more-indented" if its source column is past the
    -- base indent OR its body starts with whitespace (a leading
    -- tab counts).
    isMoreIndented bi (i, b) =
      i > bi
        || case T.uncons b of
          Just (' ', _) -> True
          Just ('\t', _) -> True
          _ -> False

    foldFirst [] _ = []
    foldFirst ((i, b) : rest) bi
      | isBlank (i, b) = tNL : foldAfterFirstBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              more = isMoreIndented bi (i, b)
          in txt : foldNext rest bi more

    -- Same as 'foldAfterBlank' but used when no content line has
    -- yet been seen — there's no "previous more-indented marker"
    -- to subtract, so a more-indented first content line gets
    -- /no/ extra preserved newline.
    foldAfterFirstBlank [] _ = []
    foldAfterFirstBlank ((i, b) : rest) bi
      | isBlank (i, b) = tNL : foldAfterFirstBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              nowMore = isMoreIndented bi (i, b)
          in txt : foldNext rest bi nowMore

    foldNext [] _ _ = []
    foldNext ((i, b) : rest) bi prevMore
      | isBlank (i, b) =
          -- A break right after a more-indented line is preserved
          -- as a literal newline; the upcoming blank emits another
          -- on top of that.
          let pre = if prevMore then [tNL] else []
          in pre ++ tNL : foldAfterBlank rest bi prevMore
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              nowMore = isMoreIndented bi (i, b)
              joinSep
                | prevMore || nowMore = tNL
                | otherwise = tSpace
          in joinSep : txt : foldNext rest bi nowMore

    -- @prevMore@ here refers to whether the line that opened the
    -- blank run was more-indented; when leaving a blank run we
    -- need to emit one more break if either side is more-indented.
    foldAfterBlank [] _ _ = []
    foldAfterBlank ((i, b) : rest) bi prevMore
      | isBlank (i, b) = tNL : foldAfterBlank rest bi prevMore
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) tSpace <> b
              nowMore = isMoreIndented bi (i, b)
              -- If we're leaving a blank-run into a more-indented
              -- line and the previous content line was /not/
              -- itself more-indented, the spec requires an extra
              -- preserved break.
              extra =
                if nowMore && not prevMore
                  then [tNL]
                  else []
          in extra ++ txt : foldNext rest bi nowMore


{- | Smallest indent from a non-blank line in the collected list,
or 0 if there are no non-blank lines. Blank lines (including
"more-indented blanks" we keep around for spacing) do not
contribute, since the spec defines the body indent as the indent
of the first non-empty line.
-}
minNonNegative :: [(Int, Text)] -> Int
minNonNegative = go Nothing
  where
    go acc [] = case acc of
      Nothing -> 0
      Just !n -> n
    go acc ((i, b) : rest)
      | i < 0 || T.null b = go acc rest
      | otherwise = case acc of
          Nothing -> go (Just i) rest
          Just !n
            | i < n -> go (Just i) rest
            | otherwise -> go (Just n) rest


chompText :: Chomp -> Text -> Text
chompText Strip = T.dropWhileEnd (== '\n')
chompText Keep = id
chompText Clip = \text ->
  let stripped = T.dropWhileEnd (== '\n') text
  in if T.null stripped then T.empty else stripped <> tNL


-- ---------------------------------------------------------------------------
-- Plain-scalar resolution per the YAML 1.2 core schema
-- ---------------------------------------------------------------------------

resolvePlain :: Text -> Value
resolvePlain raw
  | raw == "<<" = YTagged (Tag "tag:yaml.org,2002:merge") (YString "<<")
  | otherwise = case T.uncons raw of
      Nothing -> YString T.empty
      Just (h, _)
        | recognizedFirst h -> resolvePlain' raw
        | otherwise -> YString raw


{- | First-character check: filter for chars that could possibly
begin a YAML core-schema literal (null, bool, inf/nan, signed
digit, '+/-/.' or '~'). Skips the costly comparison cascade
for the common case of unquoted plain strings.
-}
recognizedFirst :: Char -> Bool
recognizedFirst c =
  c == 'n'
    || c == 'N'
    || c == 't'
    || c == 'T'
    || c == 'f'
    || c == 'F'
    || c == '~'
    || c == '.'
    || c == '+'
    || c == '-'
    || (c >= '0' && c <= '9')
{-# INLINE recognizedFirst #-}


resolvePlain' :: Text -> Value
resolvePlain' raw
  | raw == "null" || raw == "~" || raw == "Null" || raw == "NULL" = YNull
  | raw == "true" || raw == "True" || raw == "TRUE" = YBool True
  | raw == "false" || raw == "False" || raw == "FALSE" = YBool False
  | raw == ".inf"
      || raw == ".Inf"
      || raw == ".INF"
      || raw == "+.inf"
      || raw == "+.Inf"
      || raw == "+.INF" =
      YFloat (1 / 0)
  | raw == "-.inf" || raw == "-.Inf" || raw == "-.INF" = YFloat (-1 / 0)
  | raw == ".nan" || raw == ".NaN" || raw == ".NAN" = YFloat (0 / 0)
  | otherwise = case parseIntCore raw of
      Just n -> YInt n
      Nothing -> case parseFloatCore raw of
        Just d -> YFloat d
        Nothing -> YString raw


parseIntCore :: Text -> Maybe Integer
parseIntCore raw
  | Just body <- T.stripPrefix "0x" raw = digits 16 body
  | Just body <- T.stripPrefix "0o" raw = digits 8 body
  | otherwise = case T.uncons raw of
      Just ('+', rest) -> digits 10 rest
      Just ('-', rest) -> negate <$> digits 10 rest
      _ -> digits 10 raw
  where
    digits :: Word64 -> Text -> Maybe Integer
    digits base body
      | bLen body == 0 = Nothing
      | otherwise = small 0 0
      where
        !len = bLen body
        (!limit, !lastDigit) = maxBound `quotRem` base
        digitAt i =
          let byte = bAt body i
          in if byte >= 48 && byte <= 57
               then fromIntegral (byte - 48)
               else
                 if byte >= 65 && byte <= 70
                   then fromIntegral (byte - 55)
                   else
                     if byte >= 97 && byte <= 102
                       then fromIntegral (byte - 87)
                       else 255
        -- Inline the digit reader so its Word64 result is never heap-boxed per digit.
        {-# INLINE digitAt #-}
        small :: Word64 -> Int -> Maybe Integer
        small !acc !i
          | i == len = Just (toInteger acc)
          | otherwise =
              let !digit = digitAt i
              in if digit >= base
                   then Nothing
                   else
                     if acc > limit || (acc == limit && digit > lastDigit)
                       then big (toInteger acc) i
                       else small (acc * base + digit) (i + 1)
        big !acc !i
          | i == len = Just acc
          | otherwise =
              let !digit = digitAt i
              in if digit >= base
                   then Nothing
                   else big (acc * toInteger base + toInteger digit) (i + 1)
{-# INLINE parseIntCore #-}


parseFloatCore :: Text -> Maybe Double
parseFloatCore raw =
  let (sign, body) = case T.uncons raw of
        Just ('+', rest) -> (1, rest)
        Just ('-', rest) -> (-1, rest)
        _ -> (1, raw)
      (mantissa, exponentPart) = T.break (\c -> c == 'e' || c == 'E') body
      (whole, fraction) = T.break (== '.') mantissa
      digit c = c >= '0' && c <= '9'
      allDigits t = not (T.null t) && T.all digit t
      validMantissa =
        if T.null fraction
          then allDigits whole
          else
            T.all digit whole
              && T.all digit (T.drop 1 fraction)
              && (not (T.null whole) || T.length fraction > 1)
      exponentDigits = case T.uncons (T.drop 1 exponentPart) of
        Just ('+', rest) -> rest
        Just ('-', rest) -> rest
        _ -> T.drop 1 exponentPart
      validExponent = T.null exponentPart || allDigits exponentDigits
      digits = whole <> T.drop 1 fraction
  in if validMantissa && validExponent
       then do
         explicitExponent <- if T.null exponentPart then Just 0 else parseIntCore (T.drop 1 exponentPart)
         let decimalExponent = explicitExponent - toInteger (max 0 (bLen fraction - 1))
         if T.all (== '0') digits
           then Just (sign * 0)
           else
             if decimalExponent > 400
               then Just (sign * (1 / 0))
               else
                 if decimalExponent < negate (toInteger (bLen digits) + 400)
                   then Just (sign * 0)
                   else do
                     coefficient <- parseIntCore digits
                     Just (sign * Sci.toRealFloat (Sci.scientific coefficient (fromInteger decimalExponent)))
       else Nothing


-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

-- Apply a standard tag before downstream consumers unwrap its metadata.
resolveExplicitTag :: Tag -> Value -> Maybe Value
resolveExplicitTag tag (YAnchored anchor inner) = YAnchored anchor <$> resolveExplicitTag tag inner
resolveExplicitTag _ (YTagged _ _) = Nothing
resolveExplicitTag _ (YAlias _) = Nothing
resolveExplicitTag (Tag tag) v = case (tag, v) of
  ("!", YNull) -> Just (YString T.empty)
  ("!", _) -> Just v
  ("tag:yaml.org,2002:str", YNull) -> Just (YString T.empty)
  ("tag:yaml.org,2002:str", YString _) -> Just v
  ("tag:yaml.org,2002:merge", YString "<<") -> Just v
  ("tag:yaml.org,2002:int", YString raw) -> YInt <$> parseIntCore raw
  ("tag:yaml.org,2002:int", YInt _) -> Just v
  ("tag:yaml.org,2002:float", YString raw) -> case parseFloatCore raw of
    Just value -> Just (YFloat value)
    Nothing -> case resolvePlain raw of
      YFloat value -> Just (YFloat value)
      _ -> Nothing
  ("tag:yaml.org,2002:float", YFloat _) -> Just v
  ("tag:yaml.org,2002:float", YInt n) -> Just (YFloat (fromInteger n))
  ("tag:yaml.org,2002:bool", YString raw) -> case resolvePlain raw of
    YBool b -> Just (YBool b)
    _ -> Nothing
  ("tag:yaml.org,2002:bool", YBool _) -> Just v
  ("tag:yaml.org,2002:null", YNull) -> Just YNull
  ("tag:yaml.org,2002:null", YString raw)
    | T.null raw || resolvePlain raw == YNull -> Just YNull
  ("tag:yaml.org,2002:seq", YSeq _) -> Just v
  ("tag:yaml.org,2002:map", YMap _) -> Just v
  _
    | tag `elem` ["tag:yaml.org,2002:str", "tag:yaml.org,2002:int", "tag:yaml.org,2002:float", "tag:yaml.org,2002:bool", "tag:yaml.org,2002:null", "tag:yaml.org,2002:seq", "tag:yaml.org,2002:map", "tag:yaml.org,2002:merge"] -> Nothing
    | otherwise -> Just (case v of YNull -> YString T.empty; _ -> v)


tagValue :: Tag -> Value -> P Value
tagValue tag v = case resolveExplicitTag tag v of
  Just inner -> do
    recordTaggedAnchors tag inner
    pure (YTagged tag inner)
  Nothing -> failP "YAML: scalar or collection does not match its explicit tag"


-- A tag before an anchor labels the same node. Refresh its binding after
-- scalar resolution, so aliases inherit the resolved type and explicit tag.
recordTaggedAnchors :: Tag -> Value -> P ()
recordTaggedAnchors tag = go
  where
    go (YAnchored identity inner) = do
      state <- getS
      case HM.lookup (unAnchor identity) (psBindings state) of
        Nothing -> failP "YAML: unresolved tagged anchor"
        Just definition -> do
          let value = YTagged tag inner
              size = sizeWithMemo (psBindings state) value
          when (size > maxParseNodes) (failP "YAML: anchor expansion exceeds the node budget")
          modifyS (\s -> s {psBindings = HM.insert (unAnchor identity) definition {bindingValue = value, bindingSize = size} (psBindings s)})
    go _ = pure ()


taggedFlow :: Tag -> Value -> Int -> Maybe (Value, Int)
taggedFlow tag v offset = Just (YTagged tag v, offset)


expandTag :: Text -> Tag
expandTag t
  | T.isPrefixOf "!!" t =
      Tag (tYamlTagPrefix <> T.drop 2 t)
  | T.isPrefixOf "!<" t && T.isSuffixOf ">" t =
      Tag (T.init (T.drop 2 t))
  | otherwise = Tag t


{- | Resolve a tag token against the current %TAG shortcut map.
Refuses references to undefined shortcuts (spec §6.8.2).
-}
expandTagP :: Int -> Text -> P Tag
expandTagP lno token = do
  unless (validTagToken token) (failP "YAML: invalid tag spelling")
  tag@(Tag resolved) <- expandTagUncheckedP lno token
  unless (resolved == "!" || validResolvedTag resolved) (failP "YAML: tag must be a local tag or an absolute URI")
  pure tag


validTagHandle :: Text -> Bool
validTagHandle handle =
  handle == "!"
    || handle == "!!"
    || (T.length handle > 2 && T.head handle == '!' && T.last handle == '!' && T.all wordChar (T.init (T.tail handle)))
  where
    wordChar c = asciiLetter c || c >= '0' && c <= '9' || c == '-'


asciiLetter :: Char -> Bool
asciiLetter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'


-- Percent escapes are URI spelling, not scalar escapes. YAML 1.2.2 §5.6
-- requires that applications receive and compare these spellings unchanged.
validURIText :: Text -> Bool
validURIText text = not (T.null text) && go text
  where
    go rest = case T.uncons rest of
      Nothing -> True
      Just ('%', suffix) -> case T.uncons suffix of
        Just (a, more) -> case T.uncons more of
          Just (b, after) -> isHexDigit a && isHexDigit b && go after
          Nothing -> False
        Nothing -> False
      Just (c, suffix) -> (asciiLetter c || c >= '0' && c <= '9' || c `T.elem` "-#;/?:@&=+$,_.!~*'()[]") && go suffix


validResolvedTag :: Text -> Bool
validResolvedTag text
  | Just local <- T.stripPrefix "!" text = not (T.null local) && validURIText text
  | otherwise =
      let (scheme, rest) = T.break (== ':') text
      in not (T.null scheme) && asciiLetter (T.head scheme) && T.all (\c -> asciiLetter c || c >= '0' && c <= '9' || c `T.elem` "+-.") scheme && not (T.null rest) && validURIText text


validTagToken :: Text -> Bool
validTagToken token
  | token == "!" = True
  | Just body <- T.stripPrefix "!<" token = maybe False validResolvedTag (T.stripSuffix ">" body)
  | Just rest <- T.stripPrefix "!" token =
      let (handleBody, suffix) = T.break (== '!') rest
          (handle, name) = if T.null suffix then ("!", rest) else ("!" <> handleBody <> "!", T.drop 1 suffix)
      in validTagHandle handle && validURIText name && not (T.any (`T.elem` "!,[]{}") name)
  | otherwise = False


expandTagUncheckedP :: Int -> Text -> P Tag
expandTagUncheckedP lno t
  | T.isPrefixOf "!<" t && not (T.isSuffixOf ">" t) = failP "YAML: unterminated verbatim tag"
  -- '!!something' uses the implicit secondary handle '!!'.
  | T.isPrefixOf "!!" t = do
      shortcut <- lookupShortcut "!!"
      pure (maybe (expandTag t) (\prefix -> Tag (prefix <> T.drop 2 t)) shortcut)
  -- '!<verbatim>' is a verbatim tag.
  | T.isPrefixOf "!<" t && T.isSuffixOf ">" t = pure (expandTag t)
  -- '!handle!suffix' uses a primary or named shortcut.
  | T.isPrefixOf "!" t
  , let rest = T.drop 1 t
  , (handleBody, sfx) <- T.break (== '!') rest
  , not (T.null sfx)
  , let handle = T.cons '!' (T.snoc handleBody '!') =
      do
        ms <- lookupShortcut handle
        case ms of
          Just prefix -> pure (Tag (prefix <> T.drop 1 sfx))
          Nothing ->
            failP $
              "undefined %TAG shortcut "
                ++ show handle
                ++ " (line "
                ++ show lno
                ++ ")"
  | T.isPrefixOf "!" t
  , t /= "!" = do
      shortcut <- lookupShortcut "!"
      pure (maybe (expandTag t) (\prefix -> Tag (prefix <> T.drop 1 t)) shortcut)
  | otherwise = pure (expandTag t)


-- ---------------------------------------------------------------------------
-- @key: value@ split (top-level, respects quotes / brackets)
-- ---------------------------------------------------------------------------

{- | Walks the line in O(n) by reading the underlying UTF-8
bytes directly. Returns @Just (key, rest)@ when a top-level
@':'@ separator is found. The key/rest split is byte-position
based, which coincides with the character split when the line
is ASCII (the overwhelmingly common case for YAML keys).

For lines that contain non-ASCII multi-byte characters before
the separator, we fall back to the slower char-based split so
that 'T.take' / 'T.drop' produce correctly aligned slices.
-}

-- | Split a mapping header without discarding its quoted key spelling.
splitMappingLine :: Text -> Maybe (Text, Text)
splitMappingLine = findKeyValueSplit


findKeyValueSplit :: Text -> Maybe (Text, Text)
findKeyValueSplit (TI.Text arr off blen)
  | blen == 0 = Nothing
  | otherwise = goByte off 0 0 0 32
  where
    !endByte = off + blen
    -- 'goByte' uses byte indices into the underlying TA.Array.
    -- 'p' is the previous byte's code for atTokenStart.
    goByte
      :: Int
      -> Int
      -> Int
      -> Int
      -> Int
      -> Maybe (Text, Text)
    goByte !i !d !b !s !p
      | i >= endByte = Nothing
      | otherwise =
          let !w = TA.unsafeIndex arr i
              !c = toEnum (fromIntegral w) :: Char
              wi = fromIntegral w :: Int
          in case s of
               1 -> case c of
                 '"' -> goByte (i + 1) d b 0 wi
                 '\\'
                   | i + 1 < endByte ->
                       goByte (i + 2) d b 1 wi
                 _ -> goByte (i + 1) d b 1 wi
               2 -> case c of
                 '\'' -> goByte (i + 1) d b 0 wi
                 _ -> goByte (i + 1) d b 2 wi
               _ -> case c of
                 '"' | ts -> goByte (i + 1) d b 1 wi
                 '\'' | ts -> goByte (i + 1) d b 2 wi
                 '['
                   | ts || d > 0 || b > 0 ->
                       goByte (i + 1) (d + 1) b s wi
                 ']'
                   | d > 0 ->
                       goByte (i + 1) (d - 1) b s wi
                 '{'
                   | ts || d > 0 || b > 0 ->
                       goByte (i + 1) d (b + 1) s wi
                 '}'
                   | b > 0 ->
                       goByte (i + 1) d (b - 1) s wi
                 '#' | ts && d == 0 && b == 0 -> Nothing
                 ':'
                   | d == 0
                   , b == 0 ->
                       if i + 1 >= endByte
                         then Just (sliceKey i, T.empty)
                         else
                           let n = TA.unsafeIndex arr (i + 1)
                           in if n == 32 || n == 9
                                then
                                  Just
                                    ( sliceKey i
                                    , sliceTail (i + 1)
                                    )
                                else goByte (i + 1) d b s wi
                 _ -> goByte (i + 1) d b s wi
      where
        ts = p == 32 || p == 9

    -- Slice [off, i) from the underlying array (a zero-copy
    -- Text), strip trailing ASCII whitespace, then unquote.
    sliceKey i = stripYamlEnd (TI.text arr off (i - off))

    sliceTail i = TI.text arr i (endByte - i)
{-# INLINEABLE findKeyValueSplit #-}


-- ---------------------------------------------------------------------------
-- Inline-comment stripping (respects quotes)
-- ---------------------------------------------------------------------------

stripInlineComment :: Text -> Text
stripInlineComment = stripComment False


-- Flow buffers contain physical line sentinels. A comment ends a scalar but
-- must not discard the later lines already accumulated in that buffer.
stripComment :: Bool -> Text -> Text
stripComment flow t
  -- Fast path: if there's no '#' anywhere, return the input
  -- untouched (the common case for typical mapping values).
  | not (bAnyByte w8Hash t) = t
  | otherwise =
      -- Walk the underlying bytes, finding the first
      -- column-stopping '#' (preceded by ' ', '\\t', or '\\1' and
      -- not inside a quoted span). For an outer-context match,
      -- we either truncate at the preceding whitespace or, for
      -- a '\\1' match, splice in a '\\2' sentinel and skip the
      -- comment body.
      let !len = bLen t
          go !i !st
            | i >= len = bSlice t 0 i
            | otherwise = case st of
                Outer ->
                  let !c = bAt t i
                  in case c of
                       34 -> go (i + 1) InDQ -- '"'
                       39 -> go (i + 1) InSQ -- '\''
                       35
                         | (i == 0 && flow) || (i > 0 && let previous = bAt t (i - 1) in previous == w8Space || previous == w8Tab || previous == w8SOH) ->
                             if flow then spliceAt i else bSlice t 0 (max 0 (i - 1))
                         | otherwise -> go (i + 1) Outer
                       _ -> go (i + 1) Outer
                InDQ -> case bAt t i of
                  92
                    | i + 1 < len -> -- '\\' escape
                        go (i + 2) InDQ
                  34 -> go (i + 1) Outer
                  _ -> go (i + 1) InDQ
                InSQ ->
                  let !c = bAt t i
                  in if c == w8SQuote
                       then
                         if i + 1 < len && bAt t (i + 1) == w8SQuote
                           then go (i + 2) InSQ
                           else go (i + 1) Outer
                       else go (i + 1) InSQ
          spliceAt !commentStart =
            let prefix = bSlice t 0 commentStart
                afterComment = T.dropWhile (/= '\1') (bDrop (commentStart + 1) t)
            in prefix <> tSTX <> stripComment True afterComment
      in go 0 Outer


{-# INLINEABLE stripInlineComment #-}


data QState = Outer | InDQ | InSQ


-- ---------------------------------------------------------------------------
-- Double-quoted escape decoding
-- ---------------------------------------------------------------------------

decodeDQEscape :: Text -> Int -> r -> (Char -> Int -> r) -> r
decodeDQEscape source !position failure success
  | position >= bLen source = failure
  | otherwise = case bAt source position of
      48 -> done '\0'
      97 -> done '\a'
      98 -> done '\b'
      116 -> done '\t'
      9 -> done '\t'
      110 -> done '\n'
      118 -> done '\v'
      102 -> done '\f'
      114 -> done '\r'
      101 -> done '\x1B'
      32 -> done ' '
      34 -> done '"'
      47 -> done '/'
      92 -> done '\\'
      78 -> done '\x85'
      95 -> done '\xA0'
      76 -> done '\x2028'
      80 -> done '\x2029'
      120 -> readHex 2
      117 -> readHex 4
      85 -> readHex 8
      _ -> failure
  where
    done char = success char (position + 1)
    readHex width
      | position + 1 + width > bLen source = failure
      | otherwise = go (position + 1) 0
      where
        end = position + 1 + width
        go !offset !codepoint
          | codepoint > 0x10FFFF = failure
          | offset == end =
              if codepoint <= 0x10FFFF && not (codepoint >= 0xD800 && codepoint <= 0xDFFF)
                then success (chr codepoint) end
                else failure
          | otherwise =
              let byte = bAt source offset
                  digit
                    | byte >= 48 && byte <= 57 = fromIntegral (byte - 48)
                    | byte >= 65 && byte <= 70 = fromIntegral (byte - 55)
                    | byte >= 97 && byte <= 102 = fromIntegral (byte - 87)
                    | otherwise = 16
              in if digit == 16 then failure else go (offset + 1) (codepoint * 16 + digit)
{-# INLINE decodeDQEscape #-}
