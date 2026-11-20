-- | Well-formed RFC 5646 tags, preserving their ASCII spelling.
module Network.HTTP.Grammar.LanguageTag (
  languageTagBytes,
  isLanguageTag,
) where

import Data.Bits (setBit, testBit, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Set as Set
import Data.Word (Word64, Word8)
import Network.HTTP.Grammar.Parser


-- | A language tag without a registry lookup or case normalization.
languageTagBytes :: ParserT st e ByteString
languageTagBytes = withByteString (skipSome (skipSatisfyAscii tagChar)) $ \() bytes ->
  if isLanguageTag bytes then pure bytes else failed
  where
    tagChar c = isLatinLetter c || isDigit c || c == '-'
{-# INLINE languageTagBytes #-}


isLanguageTag :: ByteString -> Bool
isLanguageTag bytes
  | simpleTag bytes = True
  | not (BS.elem 45 bytes) = False
  | otherwise = normalTag || Set.member (BS.map lowerAscii bytes) irregularTags
  where
    normalTag = case BS.split 45 bytes of
      [] -> False
      primary : subtags
        | BS.map lowerAscii primary == "x" -> privateUse subtags
        | isPrimary primary ->
            let afterExtlang = if BS.length primary <= 3 then dropExtlangs 3 subtags else subtags
                afterScript = dropOne (\tag -> BS.length tag == 4 && BS.all isLetter tag) afterExtlang
                afterRegion = dropOne isRegion afterScript
            in variants Set.empty afterRegion
        | otherwise -> False

    dropExtlangs :: Int -> [ByteString] -> [ByteString]
    dropExtlangs budget (tag : rest)
      | budget > 0 && BS.length tag == 3 && BS.all isLetter tag = dropExtlangs (budget - 1) rest
    dropExtlangs _ subtags = subtags

    isRegion tag = case BS.length tag of
      2 -> BS.all isLetter tag
      3 -> BS.all isNumber tag
      _ -> False

    variants seen (tag : rest)
      | isSubtag 5 tag || (BS.length tag == 4 && isNumber (BS.head tag) && BS.all isAlphanumeric tag) =
          let key = BS.map lowerAscii tag
          in not (Set.member key seen) && variants (Set.insert key seen) rest
    variants _ subtags = extensions 0 subtags

    extensions :: Word64 -> [ByteString] -> Bool
    extensions _ [] = True
    extensions seen (singleton : rest)
      | BS.length singleton /= 1 = False
      | byte == 120 = privateUse rest
      | isAlphanumeric byte && not (testBit seen bit) = case rest of
          tag : suffix | isSubtag 2 tag -> extensions (setBit seen bit) (dropWhile (isSubtag 2) suffix)
          _ -> False
      | otherwise = False
      where
        byte = lowerAscii (BS.head singleton)
        bit = fromIntegral (if isNumber byte then byte - 48 else byte - 97 + 10)

    privateUse [] = False
    privateUse subtags = all (isSubtag 1) subtags


-- The usual language, optional script and optional region need no subtag
-- list. The full grammar below this fast path also handles extensions.
simpleTag :: ByteString -> Bool
simpleTag bytes =
  primary >= 2 && (primary == end || (at primary == 45 && tailTag (primary + 1)))
  where
    end = BS.length bytes
    at = BSU.unsafeIndex bytes
    primary = letters 0
    limit = min end 8
    letters index
      | index < limit && isLetter (at index) = letters (index + 1)
      | otherwise = index
    tailTag start = region start || script start
    region start = case end - start of
      2 -> isLetter (at start) && isLetter (at (start + 1))
      3 -> isNumber (at start) && isNumber (at (start + 1)) && isNumber (at (start + 2))
      _ -> False
    script start =
      end - start >= 4
        && isLetter (at start)
        && isLetter (at (start + 1))
        && isLetter (at (start + 2))
        && isLetter (at (start + 3))
        && let after = start + 4 in after == end || (at after == 45 && region (after + 1))


isPrimary :: ByteString -> Bool
isPrimary bytes = BS.length bytes >= 2 && BS.length bytes <= 8 && BS.all isLetter bytes


isSubtag :: Int -> ByteString -> Bool
isSubtag minimumLength bytes =
  BS.length bytes >= minimumLength && BS.length bytes <= 8 && BS.all isAlphanumeric bytes


dropOne :: (a -> Bool) -> [a] -> [a]
dropOne predicate (value : rest) | predicate value = rest
dropOne _ values = values


isLetter, isNumber, isAlphanumeric :: Word8 -> Bool
isLetter w = let lower = w .|. 32 in lower >= 97 && lower <= 122
isNumber w = w >= 48 && w <= 57
isAlphanumeric w = isLetter w || isNumber w


lowerAscii :: Word8 -> Word8
lowerAscii w | w >= 65 && w <= 90 = w .|. 32
lowerAscii w = w


-- RFC 5646's regular grandfathered tags match the normal grammar.
irregularTags :: Set.Set ByteString
irregularTags =
  Set.fromList
    [ "en-gb-oed"
    , "i-ami"
    , "i-bnn"
    , "i-default"
    , "i-enochian"
    , "i-hak"
    , "i-klingon"
    , "i-lux"
    , "i-mingo"
    , "i-navajo"
    , "i-pwn"
    , "i-tao"
    , "i-tay"
    , "i-tsu"
    , "sgn-be-fr"
    , "sgn-be-nl"
    , "sgn-ch-de"
    ]
