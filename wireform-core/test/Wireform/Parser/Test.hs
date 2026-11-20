{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wireform.Parser.Test (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Int
import Data.Word
import Test.QuickCheck
import Test.Syd
import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Error
import Wireform.Parser.Internal (Pure, Stream)
import Wireform.Parser.Switch (switchAsciiCI)


type P = Parser Pure String


ok :: (Show a, Eq a) => Either (ParseError String) a -> a -> Expectation
ok (Right a) expected = a `shouldBe` expected
ok (Left e) _ = expectationFailure ("parse failed: " <> show e)


bad :: (Show a) => Either (ParseError String) a -> Expectation
bad (Left _) = pure ()
bad (Right a) = expectationFailure ("expected failure, got: " <> show a)


spec :: Spec
spec = describe "Parser" $ do
  it "folds ASCII letters in literal branches and leaves punctuation unchanged" $ do
    let parser =
          $( switchAsciiCI
               [|
                 case _ of
                   "CaChe-Age" -> pure (1 :: Int)
                   "{" -> pure 2
                   "_" -> pure 3
                 |]
           )
            <* eof
            :: P Int
    ok (parseByteString parser "CaCHE-aGE") 1
    ok (parseByteString parser "{") 2
    ok (parseByteString parser "_") 3
    bad (parseByteString parser "[")
    bad (parseByteString parser "CACHE")

  describe "byte primitives" $ do
    it "anyWord8" $
      ok (parseByteString (anyWord8 :: P Word8) "\x42") 0x42
    it "anyWord8 on empty" $
      bad (parseByteString (anyWord8 :: P Word8) "")
    it "anyWord16 native" $
      ok (parseByteString (anyWord16 :: P Word16) "\x01\x02") (if isLE then 0x0201 else 0x0102)
    it "anyWord32be" $
      ok (parseByteString (anyWord32be :: P Word32) "\x00\x00\x01\x00") 256
    it "anyWord32le" $
      ok (parseByteString (anyWord32le :: P Word32) "\x00\x01\x00\x00") 256
    it "anyWord64le" $
      ok (parseByteString (anyWord64le :: P Word64) "\x01\x00\x00\x00\x00\x00\x00\x00") 1
    it "anyWord64be" $
      ok (parseByteString (anyWord64be :: P Word64) "\x00\x00\x00\x00\x00\x00\x00\x01") 1

  describe "signed integers" $ do
    it "anyInt8" $
      ok (parseByteString (anyInt8 :: P Int8) "\xFF") (-1)
    it "anyInt16be" $
      ok (parseByteString (anyInt16be :: P Int16) "\xFF\xFE") (-2)
    it "anyInt32le" $
      ok (parseByteString (anyInt32le :: P Int32) "\xFE\xFF\xFF\xFF") (-2)

  describe "floating point" $ do
    it "anyFloatle parses 1.0" $
      ok (parseByteString (anyFloatle :: P Float) "\x00\x00\x80\x3F") 1.0
    it "anyDoublebe parses 1.0" $
      ok (parseByteString (anyDoublebe :: P Double) "\x3F\xF0\x00\x00\x00\x00\x00\x00") 1.0

  describe "sequencing" $ do
    it "two bytes" $ do
      let p = (,) <$> anyWord8 <*> anyWord8 :: P (Word8, Word8)
      ok (parseByteString p "\xAA\xBB") (0xAA, 0xBB)
    it "insufficient for sequence" $ do
      let p = (,) <$> anyWord8 <*> anyWord8 :: P (Word8, Word8)
      bad (parseByteString p "\xAA")
    it "three-way sequence" $ do
      let p = (,,) <$> anyWord8 <*> anyWord16be <*> anyWord32be :: P (Word8, Word16, Word32)
      ok (parseByteString p "\x01\x00\x02\x00\x00\x00\x03") (1, 2, 3)

  describe "alternatives" $ do
    it "second branch" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b") :: P String
      ok (parseByteString p "\x02") "b"
    it "first branch" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b") :: P String
      ok (parseByteString p "\x01") "a"
    it "both fail" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b") :: P String
      bad (parseByteString p "\x03")
    it "backtracking restores position" $ do
      let p = (word8 0x01 *> word8 0x99) <|> (word8 0x01 *> word8 0x02) :: P ()
      ok (parseByteString p "\x01\x02") ()
    it "three-way alternative" $ do
      let p = word8 0x01 <|> word8 0x02 <|> word8 0x03 :: P ()
      ok (parseByteString p "\x03") ()

  describe "cut/err" $ do
    it "cut converts Fail to Err" $ do
      let p = cut (word8 0x01 *> anyWord8 *> word8 0xFF) "bad" :: P ()
      case parseByteString p "\x01\x42\xAA" of
        Left (ParseErr _ e) -> e `shouldBe` "bad"
        other -> expectationFailure (show other)
    it "err produces Err" $ do
      let p = err "fatal" :: P ()
      case parseByteString p "x" of
        Left (ParseErr _ e) -> e `shouldBe` "fatal"
        other -> expectationFailure (show other)
    it "Err bypasses alternatives" $ do
      let p = (err "x" :: P ()) <|> pure ()
      case parseByteString p "x" of
        Left (ParseErr _ _) -> pure ()
        other -> expectationFailure (show other)
    it "try converts Err to Fail" $ do
      let p = try (err "x" :: P ()) <|> pure ()
      ok (parseByteString p "x") ()

  describe "withError" $ do
    it "catches and handles Err" $ do
      let p = withError (\e -> pure (e <> "!")) (err "boom") :: P String
      ok (parseByteString p "x") "boom!"

  describe "byte matching" $ do
    it "byteString match" $
      ok (parseByteString (byteString "hello" :: P ()) "hello") ()
    it "byteString mismatch" $
      bad (parseByteString (byteString "hello" :: P ()) "hxllo")
    it "byteString empty" $
      ok (parseByteString (byteString "" :: P ()) "anything") ()

  describe "takeBs" $ do
    it "takes n bytes" $
      ok (parseByteString (takeBs 3 :: P ByteString) "abcde") "abc"
    it "fails if short" $
      bad (parseByteString (takeBs 10 :: P ByteString) "abc")
    it "take 0 bytes" $
      ok (parseByteString (takeBs 0 :: P ByteString) "x") ""

  describe "skip" $ do
    it "skip and continue" $ do
      let p = skip 2 *> anyWord8 :: P Word8
      ok (parseByteString p "\x00\x00\x42") 0x42

  describe "takeRest" $ do
    it "consumes all" $
      ok (parseByteString (takeRest :: P ByteString) "hello") "hello"
    it "empty on empty" $
      ok (parseByteString (takeRest :: P ByteString) "") ""

  describe "eof" $ do
    it "succeeds at end" $
      ok (parseByteString (eof :: P ()) "") ()
    it "fails with remaining" $
      bad (parseByteString (eof :: P ()) "x")

  describe "atEnd / remaining" $ do
    it "atEnd true on empty" $
      ok (parseByteString (atEnd :: P Bool) "") True
    it "atEnd false with data" $
      ok (parseByteString (atEnd :: P Bool) "x") False
    it "remaining counts bytes" $
      ok (parseByteString (remaining :: P Int) "hello") 5

  describe "UTF-8" $ do
    it "ASCII char" $
      ok (parseByteString (anyAsciiChar :: P Char) "A") 'A'
    it "rejects non-ASCII" $
      bad (parseByteString (satisfyAscii (const True) :: P Char) "\xC3\xA9")
    it "2-byte UTF-8 (é)" $
      ok (parseByteString (anyChar :: P Char) "\xC3\xA9") '\x00E9'
    it "3-byte UTF-8 (€)" $
      ok (parseByteString (anyChar :: P Char) "\xE2\x82\xAC") '\x20AC'
    it "4-byte UTF-8 (😀)" $
      ok (parseByteString (anyChar :: P Char) "\xF0\x9F\x98\x80") '\x1F600'

  describe "satisfy" $ do
    it "satisfy matches" $
      ok (parseByteString (satisfy (== 'A') :: P Char) "A") 'A'
    it "satisfy rejects" $
      bad (parseByteString (satisfy (== 'A') :: P Char) "B")

  describe "ASCII decimal" $ do
    it "parses number" $
      ok (parseByteString (anyAsciiDecimalWord :: P Word) "12345x") 12345
    it "fails on non-digit" $
      bad (parseByteString (anyAsciiDecimalWord :: P Word) "abc")
    it "single digit" $
      ok (parseByteString (anyAsciiDecimalWord :: P Word) "0") 0

  describe "hex" $ do
    it "parses hex" $
      ok (parseByteString (anyAsciiHexWord :: P Word) "FF") 255
    it "mixed case" $
      ok (parseByteString (anyAsciiHexWord :: P Word) "aB") 0xAB

  describe "lookahead and negative lookahead" $ do
    it "lookahead does not consume" $ do
      let p = lookahead anyWord8 *> anyWord8 :: P Word8
      ok (parseByteString p "\x42") 0x42
    it "fails succeeds" $ do
      let p = fails (word8 0x01) :: P ()
      ok (parseByteString p "\x02") ()
    it "notFollowedBy" $ do
      let p = notFollowedBy (word8 0x01) :: P ()
      ok (parseByteString p "\x02") ()

  describe "many/some/many_/some_" $ do
    it "many_ then read" $ do
      let p = many_ (word8 0x41) *> anyWord8 :: P Word8
      ok (parseByteString p "\x41\x41\x42") 0x42
    it "many_ on empty" $
      ok (parseByteString (many_ (word8 0x41) *> eof :: P ()) "") ()
    it "many collects" $ do
      let p = many (word8 0x41 *> pure 'A') :: P [Char]
      ok (parseByteString p "\x41\x41\x42") "AA"
    it "some requires one" $
      bad (parseByteString (some (word8 0x41) :: P [()]) "\x42")

  describe "isolate" $ do
    it "isolate consumes exact bytes" $ do
      let p = isolate 3 (takeBs 3) :: P ByteString
      ok (parseByteString p "abcdef") "abc"
    it "isolate fails if inner underconsumed" $ do
      let p = isolate 3 (takeBs 2) :: P ByteString
      bad (parseByteString p "abcdef")

  describe "chainl" $ do
    it "left-associative chain" $ do
      let digit = anyAsciiDecimalInt :: P Int
          plus = word8 0x2B *> digit
          p = chainl (+) digit plus
      ok (parseByteString p "1+2+3x") 6

  describe "position and span" $ do
    it "getPos returns 0 at start" $ do
      ok (parseByteString (getPos :: P Pos) "hello") (Pos 0)
    it "getPos advances" $ do
      let p = skip 3 *> getPos :: P Pos
      ok (parseByteString p "hello") (Pos 3)
    it "byteStringOf captures consumed bytes" $ do
      let p = byteStringOf (skip 3) :: P ByteString
      ok (parseByteString p "hello") "hel"
    it "withSpan captures span" $ do
      let p = withSpan (skip 3) (\_ (Span s e) -> pure (subPos e s)) :: P Int
      ok (parseByteString p "hello") 3
    it "keeps a bounded window inside a sliced input" $ do
      let bytes = BS.drop 2 "xxabcd"
          window p = skip 1 *> withSpan (skip 1) (\_ span' -> inSpan span' p)
      ok (parseByteString (window (takeBs 1) :: P ByteString) bytes) "b"
      bad (parseByteString (window (takeBs 2) :: P ByteString) bytes)
      bad (parseByteString (window (byteString "bc") :: P ()) bytes)
      case parseByteString (window (ensureNOrEof 2 "past window") :: P ()) bytes of
        Left (ParseErr 0 "past window") -> pure ()
        result -> expectationFailure (show result)
      ok (parseByteString (window (skip 1) *> takeRest :: P ByteString) bytes) "cd"

  it "reports EOF when a streaming checkpoint is used in a whole input" $
    bad (parseByteString (coerce (checkpoint :: Parser Stream String ()) :: P ()) "abc")

  describe "skipBack" $ do
    it "skips backward and re-reads" $ do
      let p = do
            _ <- anyWord8
            _ <- anyWord8
            skipBack 2
            anyWord8 :: P Word8
      ok (parseByteString p "\xAA\xBB") 0xAA
    it "fails when skipping past start" $ do
      let p = skipBack 1 :: P ()
      bad (parseByteString p "x")
    it "skip forward then back" $ do
      let p = do
            skip 3
            skipBack 2
            anyWord8 :: P Word8
      -- "hello" -> skip 3 -> at 'l' -> back 2 -> at 'e'
      ok (parseByteString p "hello") (fromIntegral (fromEnum 'e'))

  describe "marks" $ do
    it "mark and restore" $ do
      let p = do
            m <- mark
            _ <- anyWord8
            _ <- anyWord8
            restore m
            anyWord8 :: P Word8
      ok (parseByteString p "\xAA\xBB") 0xAA
  where
    isLE :: Bool
    isLE =
      BS.pack [1, 0]
        == BS.pack
          ( let w = 1 :: Word16
            in [fromIntegral w, fromIntegral (w `div` 256)]
          )
