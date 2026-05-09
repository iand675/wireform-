{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDN
import Data.Text.Punycode
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)
import Control.DeepSeq (force)
import Control.Exception (evaluate)

-- | Deterministic generator runner
runGen :: Gen a -> a
runGen g = unGen g (mkQCGen 42) 30

-- | Generate a random ASCII label
genAsciiLabel :: Gen Text
genAsciiLabel = do
  len <- choose (5, 63)
  chars <- vectorOf len (elements ['a'..'z'])
  return $ T.pack chars

-- | Generate a random Latin-1 label (with some non-ASCII)
genLatin1Label :: Gen Text
genLatin1Label = do
  len <- choose (5, 20)
  -- mix of ascii and latin-1 supplements
  chars <- vectorOf len (elements (['a'..'z'] ++ ['\224'..'\255'])) 
  return $ T.pack chars

-- | Generate a random RTL label (Arabic/Hebrew)
genRTLLabel :: Gen Text
genRTLLabel = do
  len <- choose (3, 15)
  -- Arabic range roughly
  chars <- vectorOf len (elements ['\x0627'..'\x064A'])
  return $ T.pack chars

-- | Generate a random CJK label
genCJKLabel :: Gen Text
genCJKLabel = do
  len <- choose (2, 10)
  -- Common CJK range
  chars <- vectorOf len (elements ['\x4E00'..'\x4E50'])
  return $ T.pack chars

-- | Generate a random Emoji/Mixed label
genEmojiLabel :: Gen Text
genEmojiLabel = do
  let emojis = ["😀", "🚀", "fractal", "✨", "café", "こんにちは"]
  elements (map T.pack emojis)

-- | The Benchmark Corpus
corpus :: [(String, [Text])]
corpus = 
  [ ("ASCII", runGen $ vectorOf 100 genAsciiLabel)
  , ("Latin1", runGen $ vectorOf 100 genLatin1Label)
  , ("RTL", runGen $ vectorOf 100 genRTLLabel)
  , ("CJK", runGen $ vectorOf 100 genCJKLabel)
  , ("Emoji/Mixed", runGen $ vectorOf 50 genEmojiLabel)
  ]

-- | Pre-calculated Punycode versions for decoding benchmarks
encodedCorpus :: [(String, [Text])]
encodedCorpus = 
      [ (name, [ forceRes $ either (const t) ("xn--" <>) (encode t) | t <- texts ])
  | (name, texts) <- corpus
  ]
  where
    forceRes x = x `seq` x

-- | Extract just the punycode part (without xn--) for Punycode.decode benchmarks
punycodeOnlyCorpus :: [(String, [Text])]
punycodeOnlyCorpus =
  [ (name, [ forceRes $ if "xn--" `T.isPrefixOf` t then T.drop 4 t else t | t <- texts ])
  | (name, texts) <- encodedCorpus
  ]
  where
    forceRes x = x `seq` x

main :: IO ()
main = do
  -- Ensure corpus is evaluated
  _ <- evaluate (force corpus)
  _ <- evaluate (force encodedCorpus)
  
  defaultMain
    [ bgroup "toASCII" 
        [ bench name $ nf (map toASCII) texts
        | (name, texts) <- corpus
        ]
    , bgroup "toUnicode"
        [ bench name $ nf (map toUnicode) texts
        | (name, texts) <- encodedCorpus
        ]
    , bgroup "validateLabel"
        [ bench name $ nf (map validateLabel) texts
        | (name, texts) <- corpus
        ]
    , bgroup "Punycode.encode"
        [ bench name $ nf (map encode) texts
        | (name, texts) <- corpus
        ]
    , bgroup "Punycode.decode"
        [ bench name $ nf (map decode) texts
        | (name, texts) <- punycodeOnlyCorpus
        ]
    ]
