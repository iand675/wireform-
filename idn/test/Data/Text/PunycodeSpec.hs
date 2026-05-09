{-# LANGUAGE OverloadedStrings #-}

module Data.Text.PunycodeSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Punycode
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "encode" $ do
    it "encodes Arabic (Egyptian) domain" $ do
      encode "مصر" `shouldSatisfy` isRight
    
    it "encodes Chinese domain" $ do
      encode "中国" `shouldSatisfy` isRight
    
    it "encodes German domain with umlaut" $ do
      let result = encode "münchen"
      result `shouldBe` Right "mnchen-3ya"
    
    it "encodes Russian domain" $ do
      encode "испытание" `shouldSatisfy` isRight
    
    it "encodes Japanese (Hiragana) domain" $ do
      encode "ひらがな" `shouldSatisfy` isRight
    
    it "encodes Japanese (Katakana) domain" $ do
      encode "カタカナ" `shouldSatisfy` isRight
    
    it "encodes Korean domain" $ do
      encode "한국" `shouldSatisfy` isRight
    
    it "encodes Hindi domain" $ do
      encode "भारत" `shouldSatisfy` isRight
    
    it "encodes Greek domain" $ do
      encode "ελλάδα" `shouldSatisfy` isRight
    
    it "encodes Thai domain" $ do
      encode "ไทย" `shouldSatisfy` isRight
    
    it "encodes mixed ASCII and Unicode" $ do
      encode "hello世界" `shouldSatisfy` isRight
    
    it "returns text with delimiter for pure ASCII" $ do
      encode "example" `shouldBe` Right "example-"
      encode "test123" `shouldBe` Right "test123-"
    
    it "encodes emoji domains" $ do
      encode "😀" `shouldSatisfy` isRight
    
    it "handles empty string" $ do
      encode "" `shouldBe` Right ""
  
  describe "decode" $ do
    it "decodes German domain" $ do
      decode "mnchen-3ya" `shouldBe` Right "münchen"
    
    it "decodes pure ASCII with delimiter but no encoded part" $ do
      decode "test-" `shouldBe` Right "test"
    
    it "decodes Chinese domain" $ do
      let result = decode "fiq228c"  -- 中国
      result `shouldSatisfy` isRight
    
    it "decodes Arabic domain" $ do
      let result = decode "wgbl6c"  -- مصر
      result `shouldSatisfy` isRight
    
    it "handles empty string" $ do
      decode "" `shouldBe` Right ""
    
    it "rejects invalid Punycode" $ do
      -- '!' is not a valid base36 character
      decode "a-b-c-!" `shouldSatisfy` isLeft
    
    it "rejects non-basic characters before delimiter" $ do
      decode "münchen-test" `shouldSatisfy` isLeft
  
  describe "round-trip encoding/decoding" $ do
    it "round-trips German domain" $ do
      let original = "münchen"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips Chinese domain" $ do
      let original = "中国"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips Arabic domain" $ do
      let original = "مصر"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips Russian domain" $ do
      let original = "испытание"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips Japanese domain" $ do
      let original = "日本"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips Greek domain" $ do
      let original = "ελλάδα"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips mixed ASCII/Unicode" $ do
      let original = "hello世界"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err
    
    it "round-trips complex Unicode" $ do
      let original = "bücher"
      case encode original of
        Right encoded -> decode encoded `shouldBe` Right original
        Left err -> expectationFailure $ "Encoding failed: " ++ show err

  -- RFC 3492 test cases
  describe "RFC 3492 official test cases" $ do
    -- All test vectors from RFC 3492 Section 7.1
    it "(A) Arabic (Egyptian)" $ do
      encode "\x0644\x064A\x0647\x0645\x0627\x0628\x062A\x0643\x0644\x0645\x0648\x0634\x0639\x0631\x0628\x064A\x061F"
        `shouldBe` Right "egbpdaj6bu4bxfgehfvwxn"
    
    it "(B) Chinese (simplified)" $ do
      encode "\x4ED6\x4EEC\x4E3A\x4EC0\x4E48\x4E0D\x8BF4\x4E2D\x6587"
        `shouldBe` Right "ihqwcrb4cv8a8dqg056pqjye"
    
    it "(C) Chinese (traditional)" $ do
      encode "\x4ED6\x5011\x7232\x4EC0\x9EBD\x4E0D\x8AAA\x4E2D\x6587"
        `shouldBe` Right "ihqwctvzc91f659drss3x8bo0yb"
    
    it "(D) Czech" $ do
      encode "Pro\x010D\&prost\x011B\&nemluv\x00ED\x010D\&esky"
        `shouldBe` Right "Proprostnemluvesky-uyb24dma41a"
    
    it "(E) Hebrew" $ do
      encode "\x05DC\x05DE\x05D4\x05D4\x05DD\x05E4\x05E9\x05D5\x05D8\x05DC\x05D0\x05DE\x05D3\x05D1\x05E8\x05D9\x05DD\x05E2\x05D1\x05E8\x05D9\x05EA"
        `shouldBe` Right "4dbcagdahymbxekheh6e0a7fei0b"
    
    it "(F) Hindi (Devanagari)" $ do
      encode "\x092F\x0939\x0932\x094B\x0917\x0939\x093F\x0928\x094D\x0926\x0940\x0915\x094D\x092F\x094B\x0902\x0928\x0939\x0940\x0902\x092C\x094B\x0932\x0938\x0915\x0924\x0947\x0939\x0948\x0902"
        `shouldBe` Right "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd"
    
    it "(G) Japanese (kanji and hiragana)" $ do
      encode "\x306A\x305C\x307F\x3093\x306A\x65E5\x672C\x8A9E\x3092\x8A71\x3057\x3066\x304F\x308C\x306A\x3044\x306E\x304B"
        `shouldBe` Right "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"
    
    it "(H) Korean (Hangul syllables)" $ do
      encode "\xC138\xACC4\xC758\xBAA8\xB4E0\xC0AC\xB78C\xB4E4\xC774\xD55C\xAD6D\xC5B4\xB97C\xC774\xD574\xD55C\xB2E4\xBA74\xC5BC\xB9C8\xB098\xC88B\xC744\xAE4C"
        `shouldBe` Right "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c"
    
    it "(I) Russian (Cyrillic)" $ do
      encode "\x043F\x043E\x0447\x0435\x043C\x0443\x0436\x0435\x043E\x043D\x0438\x043D\x0435\x0433\x043E\x0432\x043E\x0440\x044F\x0442\x043F\x043E\x0440\x0443\x0441\x0441\x043A\x0438"
        `shouldBe` Right "b1abfaaepdrnnbgefbadotcwatmq2g4l"
    
    it "(J) Spanish" $ do
      encode "Porqu\x00E9nopuedensimplementehablarenEspa\x00F1ol"
        `shouldBe` Right "PorqunopuedensimplementehablarenEspaol-fmd56a"
    
    it "(K) Vietnamese" $ do
      encode "T\x1EA1isaoh\x1ECDkh\x00F4ngth\x1EC3\&ch\x1EC9n\x00F3iti\x1EBFngVi\x1EC7t"
        `shouldBe` Right "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g"
    
    it "(L) Japanese: 3年B組金八先生" $ do
      encode "3\x5E74\&B\x7D44\x91D1\x516B\x5148\x751F"
        `shouldBe` Right "3B-ww4c5e180e575a65lsy2b"
    
    it "(M) Japanese: 安室奈美恵-with-SUPER-MONKEYS" $ do
      encode "\x5B89\x5BA4\x5948\x7F8E\x6075-with-SUPER-MONKEYS"
        `shouldBe` Right "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n"
    
    it "(N) Japanese: Hello-Another-Way-それぞれの場所" $ do
      encode "Hello-Another-Way-\x305D\x308C\x305E\x308C\x306E\x5834\x6240"
        `shouldBe` Right "Hello-Another-Way--fc4qua05auwb3674vfr0b"
    
    it "(O) Japanese: ひとつ屋根の下2" $ do
      encode "\x3072\x3068\x3064\x5C4B\x6839\x306E\x4E0B\&2"
        `shouldBe` Right "2-u9tlzr9756bt3uc0v"
    
    it "(P) Japanese: MajiでKoiする5秒前" $ do
      encode "Maji\x3067Koi\x3059\x308B\&5\x79D2\x524D"
        `shouldBe` Right "MajiKoi5-783gue6qz075azm5e"
    
    it "(Q) Japanese: パフィーdeルンバ" $ do
      encode ("\x30D1\x30D5\x30A3\x30FC" <> "de" <> "\x30EB\x30F3\x30D0")
        `shouldBe` Right "de-jg4avhby1noc0d"
    
    it "(R) Japanese: そのスピードで" $ do
      encode "\x305D\x306E\x30B9\x30D4\x30FC\x30C9\x3067"
        `shouldBe` Right "d9juau41awczczp"
    
    it "(S) ASCII symbols: -> $1.00 <-" $ do
      encode "-> $1.00 <-"
        `shouldBe` Right "-> $1.00 <--"
    
    -- Round-trip tests for official vectors
    it "round-trips all official test cases" $ do
      let testVectors = 
            [ ("egbpdaj6bu4bxfgehfvwxn", "\x0644\x064A\x0647\x0645\x0627\x0628\x062A\x0643\x0644\x0645\x0648\x0634\x0639\x0631\x0628\x064A\x061F")
            , ("ihqwcrb4cv8a8dqg056pqjye", "\x4ED6\x4EEC\x4E3A\x4EC0\x4E48\x4E0D\x8BF4\x4E2D\x6587")
            , ("ihqwctvzc91f659drss3x8bo0yb", "\x4ED6\x5011\x7232\x4EC0\x9EBD\x4E0D\x8AAA\x4E2D\x6587")
            , ("Proprostnemluvesky-uyb24dma41a", "Pro\x010D\&prost\x011B\&nemluv\x00ED\x010D\&esky")
            , ("4dbcagdahymbxekheh6e0a7fei0b", "\x05DC\x05DE\x05D4\x05D4\x05DD\x05E4\x05E9\x05D5\x05D8\x05DC\x05D0\x05DE\x05D3\x05D1\x05E8\x05D9\x05DD\x05E2\x05D1\x05E8\x05D9\x05EA")
            ]
      forM_ testVectors $ \(encoded, unicode) -> do
        decode (T.pack encoded) `shouldBe` Right (T.pack unicode)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
