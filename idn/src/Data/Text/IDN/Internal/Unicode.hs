{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Internal Unicode property lookups for IDN validation.
--
-- This module provides Unicode property lookups used internally for IDN
-- validation. For public API, see 'Data.Text.IDN'.
--
-- = Implementation Note: IntMap/IntSet vs CharSet
--
-- This module uses 'IntMap' and 'IntSet' from containers rather than 'CharSet'
-- from the charset package. Benchmarking showed that:
--
-- * Map lookups (codePointStatus, bidiClass, scriptOf, lookupContextRule)
--   are 2-23% slower with CharSet, as it provides no benefit for map operations
-- * Set membership tests (isCombiningMark, isVirama) show mixed results:
--     - CharSet is 36% faster for negative lookups (non-combining chars)
--     - CharSet is 2-6% slower for positive lookups and mixed workloads
-- * Overall: IntMap/IntSet is faster for our workload which is dominated by
--   map lookups rather than pure set membership tests
module Data.Text.IDN.Internal.Unicode
  ( -- * Property Lookup Functions
    codePointStatus
  , bidiClass
  , isCombiningMark
  , isVirama
  , scriptOf
  , lookupContextRule
  
    -- * Unicode Normalization
  , isNFCQuickCheck
  ) where

import Data.Text.IDN.Types
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import System.IO.Unsafe (unsafePerformIO)

-- Embed the Unicode data files at compile time
bidiClassData :: BS.ByteString
bidiClassData = $(embedFile "data/unicode/DerivedBidiClass.txt")

unicodeData :: BS.ByteString
unicodeData = $(embedFile "data/unicode/UnicodeData.txt")

joiningTypeData :: BS.ByteString
joiningTypeData = $(embedFile "data/unicode/DerivedJoiningType.txt")

scriptsData :: BS.ByteString
scriptsData = $(embedFile "data/unicode/Scripts.txt")

-- Parse data files once at module load time
{-# NOINLINE codePointStatusTable #-}
codePointStatusTable :: IntMap CodePointStatus
codePointStatusTable = deriveCodePointStatus (TE.decodeUtf8 unicodeData)

{-# NOINLINE bidiClassTable #-}
bidiClassTable :: IntMap BidiClass
bidiClassTable = unsafePerformIO $ parseBidiClasses (TE.decodeUtf8 bidiClassData)

{-# NOINLINE combiningMarksSet #-}
combiningMarksSet :: IntSet
combiningMarksSet = parseCombiningMarks (TE.decodeUtf8 unicodeData)

{-# NOINLINE viramasSet #-}
viramasSet :: IntSet
viramasSet = parseViramas (TE.decodeUtf8 joiningTypeData)

{-# NOINLINE scriptTable #-}
scriptTable :: IntMap Script
scriptTable = case parseScripts (TE.decodeUtf8 scriptsData) of
  Left err -> error $ "Failed to parse scripts: " ++ err
  Right m -> m

{-# NOINLINE contextRuleTable #-}
contextRuleTable :: IntMap ContextRule
contextRuleTable = IM.fromList $
  [ (0x200C, ZWNJRule)
  , (0x200D, ZWJRule)
  , (0x00B7, MiddleDotRule)
  , (0x0375, GreekKeraiaRule)
  , (0x05F3, HebrewGereshRule)
  , (0x05F4, HebrewGershayimRule)
  , (0x30FB, KatakanaMiddleDotRule)
  ] ++ [(cp, ArabicIndicDigitsRule) | cp <- [0x0660..0x0669]]
    ++ [(cp, ExtendedArabicIndicDigitsRule) | cp <- [0x06F0..0x06F9]]

-- Lookup functions
codePointStatus :: Char -> CodePointStatus
codePointStatus c = IM.findWithDefault PVALID (ord c) codePointStatusTable
{-# INLINE codePointStatus #-}

bidiClass :: Char -> BidiClass
bidiClass c = IM.findWithDefault L (ord c) bidiClassTable
{-# INLINE bidiClass #-}

isCombiningMark :: Char -> Bool
isCombiningMark c = IS.member (ord c) combiningMarksSet
{-# INLINE isCombiningMark #-}

isVirama :: Char -> Bool
isVirama c = IS.member (ord c) viramasSet
{-# INLINE isVirama #-}

scriptOf :: Char -> Maybe Script
scriptOf c = IM.lookup (ord c) scriptTable
{-# INLINE scriptOf #-}

lookupContextRule :: Char -> Maybe ContextRule
lookupContextRule c = IM.lookup (ord c) contextRuleTable
{-# INLINE lookupContextRule #-}

isNFCQuickCheck :: String -> Bool
isNFCQuickCheck = all checkChar
  where
    checkChar c = 
      let cp = ord c
      in cp < 0x0300 || (cp > 0x0370 && cp < 0x1AB0)
{-# INLINE isNFCQuickCheck #-}

-- Parsing functions
parseBidiClasses :: T.Text -> IO (IntMap BidiClass)
parseBidiClasses text = do
  let dataLines = filter (not . isComment) (T.lines text)
      entries = mapM parseBidiLine dataLines
  case entries of
    Left err -> error $ "Failed to parse bidi classes: " ++ err
    Right xs -> return $ IM.fromList (concat xs)

parseBidiLine :: T.Text -> Either String [(Int, BidiClass)]
parseBidiLine line = do
  let content = T.strip $ T.takeWhile (/= '#') line
  case T.words content of
    [] -> Right []
    [cpRange, ";", bidiClassStr] -> do
      (startCP, endCP) <- parseCodePointRange (T.strip cpRange)
      bidiClassVal <- parseBidiClass (T.strip bidiClassStr)
      return [(cp, bidiClassVal) | cp <- [startCP..endCP]]
    [cpRange, bidiClassStr] -> do  -- Handle without semicolon
      (startCP, endCP) <- parseCodePointRange (T.strip cpRange)
      bidiClassVal <- parseBidiClass (T.strip bidiClassStr)
      return [(cp, bidiClassVal) | cp <- [startCP..endCP]]
    _ -> Left $ "Invalid bidi line: " ++ T.unpack line

parseCodePointRange :: T.Text -> Either String (Int, Int)
parseCodePointRange t = case T.splitOn ".." t of
  [single] -> case TR.hexadecimal t of
    Right (cp, _) -> Right (cp, cp)
    Left err -> Left err
  [start, end] -> do
    (startCP, _) <- TR.hexadecimal start
    (endCP, _) <- TR.hexadecimal end
    return (startCP, endCP)
  _ -> Left $ "Invalid range: " ++ T.unpack t

parseBidiClass :: T.Text -> Either String BidiClass
parseBidiClass "L" = Right L
parseBidiClass "R" = Right R
parseBidiClass "AL" = Right AL
parseBidiClass "AN" = Right AN
parseBidiClass "EN" = Right EN
parseBidiClass "ES" = Right ES
parseBidiClass "ET" = Right ET
parseBidiClass "CS" = Right CS
parseBidiClass "NSM" = Right NSM
parseBidiClass "BN" = Right BN
parseBidiClass "B" = Right B
parseBidiClass "S" = Right S
parseBidiClass "WS" = Right WS
parseBidiClass "ON" = Right ON
parseBidiClass "LRE" = Right LRE
parseBidiClass "LRO" = Right LRO
parseBidiClass "RLE" = Right RLE
parseBidiClass "RLO" = Right RLO
parseBidiClass "PDF" = Right PDF
parseBidiClass "LRI" = Right LRI
parseBidiClass "RLI" = Right RLI
parseBidiClass "FSI" = Right FSI
parseBidiClass "PDI" = Right PDI
parseBidiClass t = Left $ "Unknown bidi class: " ++ T.unpack t

-- | Derive code point status according to RFC 5892
-- This implements the IDNA2008 derivation algorithm based on Unicode properties
deriveCodePointStatus :: T.Text -> IntMap CodePointStatus
deriveCodePointStatus text =
  let dataLines = filter (not . T.null) $ T.lines text
      entries = mapMaybe parseUnicodeDataLine dataLines
  in IM.fromList entries
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

data UnicodeEntry = UnicodeEntry
  { ueCodePoint :: Int
  , ueName :: T.Text
  , ueCategory :: T.Text
  , ueCombiningClass :: Int
  }

parseUnicodeDataLine :: T.Text -> Maybe (Int, CodePointStatus)
parseUnicodeDataLine line =
  case T.splitOn ";" line of
    (cpHex:name:cat:ccc:_rest) -> do
      (cp, _) <- either (const Nothing) Just $ TR.hexadecimal (T.strip cpHex)
      let category = T.strip cat
          status = deriveStatus cp category
      Just (cp, status)
    _ -> Nothing

-- | RFC 5892 derivation rules
deriveStatus :: Int -> T.Text -> CodePointStatus
deriveStatus cp cat
  -- Explicit CONTEXTJ code points
  | cp == 0x200C = CONTEXTJ  -- Zero Width Non-Joiner
  | cp == 0x200D = CONTEXTJ  -- Zero Width Joiner
  -- Explicit CONTEXTO code points
  | cp == 0x00B7 = CONTEXTO  -- Middle Dot
  | cp == 0x0375 = CONTEXTO  -- Greek Lower Numeral Sign (Keraia)
  | cp == 0x05F3 = CONTEXTO  -- Hebrew Punctuation Geresh
  | cp == 0x05F4 = CONTEXTO  -- Hebrew Punctuation Gershayim
  | cp == 0x30FB = CONTEXTO  -- Katakana Middle Dot
  | cp >= 0x0660 && cp <= 0x0669 = CONTEXTO  -- Arabic-Indic Digits
  | cp >= 0x06F0 && cp <= 0x06F9 = CONTEXTO  -- Extended Arabic-Indic Digits
  -- RFC 5892 Section 2.6: Exceptions (PVALID)
  | cp == 0x00DF = PVALID  -- LATIN SMALL LETTER SHARP S
  | cp == 0x03C2 = PVALID  -- GREEK SMALL LETTER FINAL SIGMA
  | cp == 0x06FD = PVALID  -- ARABIC SIGN SINDHI AMPERSAND
  | cp == 0x06FE = PVALID  -- ARABIC SIGN SINDHI POSTPOSITION MEN
  | cp == 0x0F0B = PVALID  -- TIBETAN MARK INTERSYLLABIC TSHEG
  | cp == 0x3007 = PVALID  -- IDEOGRAPHIC NUMBER ZERO
  -- RFC 5892 Section 2.6: Exceptions (DISALLOWED)
  | cp == 0x0640 = DISALLOWED  -- ARABIC TATWEEL
  | cp == 0x07FA = DISALLOWED  -- NKO LAJANYALAN
  | cp == 0x302E = DISALLOWED  -- HANGUL SINGLE DOT TONE MARK
  | cp == 0x302F = DISALLOWED  -- HANGUL DOUBLE DOT TONE MARK
  | cp == 0x3031 = DISALLOWED  -- VERTICAL KANA REPEAT MARK
  | cp == 0x3032 = DISALLOWED  -- VERTICAL KANA REPEAT WITH VOICED SOUND MARK
  | cp == 0x3033 = DISALLOWED  -- VERTICAL KANA REPEAT MARK UPPER HALF
  | cp == 0x3034 = DISALLOWED  -- VERTICAL KANA REPEAT WITH VOICED SOUND MARK UPPER HALF
  | cp == 0x3035 = DISALLOWED  -- VERTICAL KANA REPEAT MARK LOWER HALF
  | cp == 0x303B = DISALLOWED  -- VERTICAL IDEOGRAPHIC ITERATION MARK
  -- ASCII exceptions that are DISALLOWED (per RFC 5892)
  | cp `elem` [0x0020, 0x007F] = DISALLOWED  -- Space and DEL
  -- ASCII printable range (0x21-0x7E) is mostly PVALID
  | cp >= 0x0021 && cp <= 0x007E = PVALID
  -- Letters and marks are generally PVALID
  | cat `elem` ["Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc"] = PVALID
  -- Decimal numbers are PVALID
  | cat == "Nd" = PVALID
  -- Control characters and format characters are DISALLOWED
  | cat `elem` ["Cc", "Cf"] = DISALLOWED
  -- Unassigned
  | cat == "Cn" = UNASSIGNED
  -- Symbols, punctuation, separators, and other categories are DISALLOWED
  | cat `elem` ["Zs", "Zl", "Zp", "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po", 
                "Sm", "Sc", "Sk", "So", "Me", "No", "Nl"] = DISALLOWED
  -- Default to DISALLOWED for safety
  | otherwise = DISALLOWED

parseCombiningMarks :: T.Text -> IntSet
parseCombiningMarks text =
  let dataLines = filter (not . T.null) $ T.lines text
      marks = mapMaybe parseCombiningMarkLine dataLines
  in IS.fromList marks
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

parseCombiningMarkLine :: T.Text -> Maybe Int
parseCombiningMarkLine line =
  case T.splitOn ";" line of
    (cpHex:_name:cat:ccc:_rest) ->
      if T.strip cat == "Mn" || T.strip cat == "Mc" || T.strip cat == "Me" ||
         T.strip ccc /= "0"
      then case TR.hexadecimal (T.strip cpHex) of
        Right (cp, _) -> Just cp
        Left _ -> Nothing
      else Nothing
    _ -> Nothing

parseViramas :: T.Text -> IntSet
parseViramas text =
  let dataLines = filter (not . isComment) (T.lines text)
      viramas = mapMaybe parseViramaLine dataLines
  in IS.fromList viramas
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

parseViramaLine :: T.Text -> Maybe Int
parseViramaLine line =
  let content = T.strip $ T.takeWhile (/= '#') line
  in case T.splitOn ";" content of
    [cpRange, jType] | T.strip jType == "T" ->
      case parseCodePointRange (T.strip cpRange) of
        Right (cp, _) -> Just cp  -- Just take first of range
        Left _ -> Nothing
    _ -> Nothing

parseScripts :: T.Text -> Either String (IntMap Script)
parseScripts text = do
  let dataLines = filter (not . isComment) (T.lines text)
  entries <- mapM parseScriptLine dataLines
  return $ IM.fromList (concat entries)

parseScriptLine :: T.Text -> Either String [(Int, Script)]
parseScriptLine line =
  let content = T.strip $ T.takeWhile (/= '#') line
  in case T.splitOn ";" content of
    [] -> Right []
    [cpRange, scriptName] -> do
      (startCP, endCP) <- parseCodePointRange (T.strip cpRange)
      script <- parseScriptName (T.strip scriptName)
      return [(cp, script) | cp <- [startCP..endCP]]
    _ -> case T.words content of
      [] -> Right []
      [cpRange, scriptName] -> do
        (startCP, endCP) <- parseCodePointRange (T.strip cpRange)
        script <- parseScriptName (T.strip scriptName)
        return [(cp, script) | cp <- [startCP..endCP]]
      _ -> Left $ "Invalid script line: " ++ T.unpack line

parseScriptName :: T.Text -> Either String Script
parseScriptName "Arabic" = Right Arabic
parseScriptName "Hebrew" = Right Hebrew
parseScriptName "Greek" = Right Greek
parseScriptName "Hiragana" = Right Hiragana
parseScriptName "Katakana" = Right Katakana
parseScriptName "Han" = Right Han
parseScriptName "Hangul" = Right Hangul
parseScriptName "Latin" = Right Latin
parseScriptName "Cyrillic" = Right Cyrillic
parseScriptName "Devanagari" = Right Devanagari
parseScriptName "Bengali" = Right Bengali
parseScriptName "Gurmukhi" = Right Gurmukhi
parseScriptName "Gujarati" = Right Gujarati
parseScriptName "Oriya" = Right Oriya
parseScriptName "Tamil" = Right Tamil
parseScriptName "Telugu" = Right Telugu
parseScriptName "Kannada" = Right Kannada
parseScriptName "Malayalam" = Right Malayalam
parseScriptName "Thai" = Right Thai
parseScriptName "Lao" = Right Lao
parseScriptName "Tibetan" = Right Tibetan
parseScriptName "Myanmar" = Right Myanmar
parseScriptName "Georgian" = Right Georgian
parseScriptName _ = Right OtherScript

isComment :: T.Text -> Bool
isComment line = T.null (T.strip line) || "#" `T.isPrefixOf` T.strip line
