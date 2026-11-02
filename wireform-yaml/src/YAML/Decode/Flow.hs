{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Align an already validated flow tree with its presentation tokens. This
pass never decides YAML semantics; an alignment failure leaves the normal
annotation fallback in charge.
-}
module YAML.Decode.Flow (annotateFlow) where

import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word8)
import YAML.Annotated


data Token = Token !Int !Int !Kind


data Kind = Atom | Quoted !Word8 | Property | Punct !Word8 | Note !Comment


annotateFlow :: Text -> AValue -> Maybe AValue
annotateFlow source original = do
  let bytes = TE.encodeUtf8 source
      body = dropPrologue 0 bytes
      tokens = lexFlow bytes body
      content = dropWhile isProperty tokens
  case content of
    Token _ _ (Punct c) : _ -> guard (c == 91 || c == 123)
    _ -> Nothing
  (result, rest) <- node tokens original
  let before = TE.decodeUtf8 (BS.take body bytes)
      comments = foldMap (\line -> case T.stripPrefix "#" (T.dropWhile (== ' ') line) of Just text -> [Comment (T.dropWhile (== ' ') text) 0]; Nothing -> []) (T.lines (T.replace "\r" "\n" before))
      trailing = case (avNodeSpan result, rest) of
        (Just (SrcSpan _ end), Token start _ (Note comment) : _)
          | not (BS.any (\byte -> byte == 10 || byte == 13) (BS.take (start - end) (BS.drop end bytes))) -> Just comment
        _ -> Nothing
  pure (leading comments result) {avTrivia = (avTrivia (leading comments result)) {triviaTrailing = trailing}}
  where
    isProperty (Token _ _ Property) = True
    isProperty (Token _ _ (Note _)) = True
    isProperty _ = False


dropPrologue :: Int -> BS.ByteString -> Int
dropPrologue !offset bytes
  | offset >= BS.length bytes = offset
  | space (BS.index bytes offset) = dropPrologue (offset + 1) bytes
  | BS.index bytes offset == 35 || BS.index bytes offset == 37 =
      dropPrologue (offset + BS.length (BS.takeWhile (\c -> c /= 10 && c /= 13) (BS.drop offset bytes))) bytes
  | BS.take 3 (BS.drop offset bytes) == "---" = dropPrologue (offset + 3) bytes
  | BS.take 3 (BS.drop offset bytes) == BS.pack [239, 187, 191] = dropPrologue (offset + 3) bytes
  | otherwise = offset


space :: Word8 -> Bool
space c = c == 32 || c == 9 || c == 10 || c == 13


lexFlow :: BS.ByteString -> Int -> [Token]
lexFlow bytes = go
  where
    size = BS.length bytes
    at = BS.index bytes
    slice a b = TE.decodeUtf8 (BS.take (b - a) (BS.drop a bytes))
    go !i
      | i >= size = []
      | space c = go (i + 1)
      | c == 35 =
          let end = untilByte (\b -> b == 10 || b == 13) (i + 1)
              comment = Comment (T.dropWhile (== ' ') (slice (i + 1) end)) 0
          in Token i end (Note comment) : go end
      | c == 34 || c == 39 =
          let end = quoted c (i + 1)
          in Token i end (Quoted c) : go end
      | c == 33 || c == 38 =
          let end =
                if c == 33 && i + 1 < size && at (i + 1) == 60
                  then min size (untilByte (== 62) (i + 2) + 1)
                  else untilByte (\b -> space b || delimiter b) (i + 1)
          in Token i end Property : go end
      | delimiter c || (c == 58 && colon i) || (c == 63 && (i + 1 == size || space (at (i + 1)))) = Token i (i + 1) (Punct c) : go (i + 1)
      | otherwise =
          let end = plain (i + 1)
              trimmed = trimEnd end
          in Token i trimmed Atom : go end
      where
        c = at i
    delimiter b = b == 91 || b == 93 || b == 123 || b == 125 || b == 44
    colon i = i + 1 == size || space (at (i + 1)) || delimiter (at (i + 1)) || previous (i - 1) `elem` [34, 39, 93, 125]
    previous i
      | i < 0 = 0
      | space (at i) = previous (i - 1)
      | otherwise = at i
    untilByte stop !i
      | i >= size || stop (at i) = i
      | otherwise = untilByte stop (i + 1)
    trimEnd !i
      | i > 0 && space (at (i - 1)) = trimEnd (i - 1)
      | otherwise = i
    plain !i
      | i >= size = i
      | delimiter (at i) = i
      | at i == 35 && space (at (i - 1)) = i
      | at i == 58 && (i + 1 == size || space (at (i + 1)) || delimiter (at (i + 1))) = i
      | otherwise = plain (i + 1)
    quoted q !i
      | i >= size = size
      | q == 34 && at i == 92 = quoted q (min size (i + 2))
      | at i == q = if q == 39 && i + 1 < size && at (i + 1) == q then quoted q (i + 2) else i + 1
      | otherwise = quoted q (i + 1)


node :: [Token] -> AValue -> Maybe (AValue, [Token])
node tokens original = do
  let (properties, content) = span property tokens
      bare = unwrap original
  (parsed, rest) <- bareNode content bare
  let notes = foldMap (\token -> case token of Token _ _ (Note comment) -> [comment]; _ -> []) properties
      result = leading notes (rewrap original parsed)
      start = case properties of Token a _ _ : _ -> a; [] -> maybe 0 srcStart (avNodeSpan parsed)
      propertyEnd = foldl (\end (Token _ next _) -> max end next) start properties
      -- Include the physical break after a property-only comment. Copying a
      -- span that ends inside that comment would swallow a fresh separator.
      emptyEnd = case (notes, rest) of (_ : _, Token offset _ _ : _) -> offset; _ -> propertyEnd
      finish = maybe emptyEnd srcEnd (avNodeSpan parsed)
  pure (result {avNodeSpan = if null properties then avNodeSpan parsed else Just (SrcSpan start finish)}, rest)
  where
    property (Token _ _ Property) = True
    property (Token _ _ (Note _)) = True
    property _ = False
    unwrap av = case avBody av of ATagged _ x -> unwrap x; AAnchored _ x -> unwrap x; _ -> av
    rewrap av inner = case avBody av of
      ATagged tag x -> av {avBody = ATagged tag (rewrap x inner), avNodeSpan = Nothing}
      AAnchored anchor x -> av {avBody = AAnchored anchor (rewrap x inner), avNodeSpan = Nothing}
      _ -> inner


bareNode :: [Token] -> AValue -> Maybe (AValue, [Token])
bareNode tokens original = case (tokens, avBody original) of
  (Token start _ (Punct 91) : rest, ASeq items _) -> do
    (values, trailing, ending) <- sequenceNodes rest (V.toList items)
    case ending of
      Token _ end (Punct 93) : more -> pure (original {avBody = ASeq (V.fromList values) SeqFlow, avNodeSpan = Just (SrcSpan start end), avTrailingText = notesText trailing}, more)
      _ -> Nothing
  (Token start _ (Punct 123) : rest, AMap entries _) -> do
    (values, trailing, ending) <- mappingNodes rest (V.toList entries)
    case ending of
      Token _ end (Punct 125) : more -> pure (original {avBody = AMap (V.fromList values) MapFlow, avNodeSpan = Just (SrcSpan start end), avTrailingText = notesText trailing}, more)
      _ -> Nothing
  (_, AMap entries _) | V.length entries == 1 -> do
    (entry, rest) <- pair tokens (V.head entries)
    let start = case tokens of Token offset _ _ : _ -> offset; [] -> 0
        end = case rest of Token offset _ _ : _ -> offset; [] -> maybe start srcEnd (avNodeSpan (amValue entry))
    pure (original {avBody = AMap (V.singleton entry) MapFlow, avNodeSpan = Just (SrcSpan start end)}, rest)
  (_, ANull) | boundary tokens -> pure (original {avNodeSpan = Nothing}, tokens)
  (_, AString text _) | T.null text, boundary tokens -> pure (original {avNodeSpan = Nothing}, tokens)
  (Token start end kind : rest, _)
    | scalar kind ->
        let body = case (avBody original, kind) of
              (AString text _, Quoted 34) -> AString text SSDoubleQuoted
              (AString text _, Quoted 39) -> AString text SSSingleQuoted
              (AString text _, _) -> AString text SSPlain
              (other, _) -> other
        in pure (original {avBody = body, avNodeSpan = Just (SrcSpan start end)}, rest)
  _ -> Nothing
  where
    scalar Atom = True
    scalar (Quoted _) = True
    scalar _ = False


boundary :: [Token] -> Bool
boundary [] = True
boundary (Token _ _ (Punct c) : _) = c == 44 || c == 93 || c == 125 || c == 58
boundary _ = False


takeNotes :: [Token] -> ([Comment], [Token])
takeNotes (Token _ _ (Note comment) : rest) = let (comments, more) = takeNotes rest in (comment : comments, more)
takeNotes tokens = ([], tokens)


notesText :: [Comment] -> Text
notesText = foldMap (\comment -> "# " <> commentText comment <> "\n")


leading :: [Comment] -> AValue -> AValue
leading comments av = av {avTrivia = (avTrivia av) {triviaLeading = comments <> triviaLeading (avTrivia av)}}


sequenceNodes :: [Token] -> [AValue] -> Maybe ([AValue], [Comment], [Token])
sequenceNodes tokens [] = let (comments, rest) = takeNotes tokens in pure ([], comments, rest)
sequenceNodes tokens (value : values) = do
  let (before, start) = takeNotes tokens
  (parsed, after) <- node start value
  let (trailing, separator) = takeNotes after
      decorated = leading (before <> trailing) parsed
  case separator of
    Token _ _ (Punct 44) : rest -> do
      (next, ending, more) <- sequenceNodes rest values
      pure (decorated : next, ending, more)
    _ -> do
      guard (null values)
      pure ([decorated], [], separator)


pair :: [Token] -> AMapEntry -> Maybe (AMapEntry, [Token])
pair tokens original = do
  let (before, content) = takeNotes tokens
      start = case content of Token _ _ (Punct 63) : rest -> rest; _ -> content
  (key, afterKey) <- node start (amKey original)
  let (middle, colon) = takeNotes afterKey
  (value, rest) <- case colon of
    Token _ _ (Punct 58) : afterColon -> do
      let (comments, valueStart) = takeNotes afterColon
      (result, more) <- node valueStart (amValue original)
      pure (leading comments result, more)
    _ -> case avBody (amValue original) of ANull -> pure (amValue original, colon); _ -> Nothing
  let (trailing, ending) = takeNotes rest
  pure (original {amKey = leading (before <> middle <> trailing) key, amValue = value, amTrailing = Nothing}, ending)


mappingNodes :: [Token] -> [AMapEntry] -> Maybe ([AMapEntry], [Comment], [Token])
mappingNodes tokens [] = let (comments, rest) = takeNotes tokens in pure ([], comments, rest)
mappingNodes tokens (entry : entries) = do
  (parsed, after) <- pair tokens entry
  case after of
    Token _ _ (Punct 44) : rest -> do
      (next, ending, more) <- mappingNodes rest entries
      pure (parsed : next, ending, more)
    _ -> do
      guard (null entries)
      pure ([parsed], [], after)
