module Main (main) where

import CBOR.Encode (encode)
import CBOR.Stream qualified as S
import CBOR.Value qualified as C
import Codec.CBOR.Read qualified as Ref
import Codec.CBOR.Term qualified as Ref
import Control.Monad (unless)
import Control.Monad.ST (runST)
import Criterion.Main
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Vector qualified as V


data Fixture = Fixture
  { fixtureName :: !String
  , fixtureWire :: !BS.ByteString
  , fixtureCheck :: !Int
  }


main :: IO ()
main = defaultMain (map fixtureBench fixtures)


fixtures :: [Fixture]
fixtures =
  [ payload "bytes-8KiB" 8192
  , payload "bytes-32KiB" 32768
  , Fixture "indefinite-bytes-8KiB"
      (BS.concat (BS.singleton 0x5f : replicate 16 (encode (C.ByteString (BS.replicate 512 42))) ++ [BS.singleton 0xff]))
      (8192 + 42)
  , Fixture "array-512" (encode (C.Array (V.generate 512 (C.UInt . fromIntegral)))) (sum [0 .. 511])
  ]
  where
    payload name len =
      Fixture name (encode (C.ByteString (BS.replicate len 42))) (len + 42)


fixtureBench :: Fixture -> Benchmark
fixtureBench fixture =
  bgroup name
    [ feed "whole" [wire]
    , feed "feed-256" (fragment 256 wire)
    , feed "feed-16" (fragment 16 wire)
    ]
  where
    name = fixtureName fixture
    wire = fixtureWire fixture
    expected = fixtureCheck fixture
    feed group chunks =
      env
        (do
          unless (wireform chunks == expected && cborg chunks == expected) $
            fail ("Both decoders must produce the expected " ++ name)
          pure chunks
        )
        (\input -> bgroup group [bench "wireform" (nf wireform input), bench "cborg" (nf cborg input)])


fragment :: Int -> BS.ByteString -> [BS.ByteString]
fragment width bytes
  | BS.null bytes = []
  | otherwise = let (part, rest) = BS.splitAt width bytes in part : fragment width rest


wireform :: [BS.ByteString] -> Int
wireform [] = error "empty fixture"
wireform (first : rest) = go (S.streamDecode first) rest
  where
    go (S.Done value leftover) []
      | BS.null leftover = checkValue value
    go (S.Partial more) (chunk : chunks) = go (more chunk) chunks
    go step _ = error ("unexpected Wireform result: " ++ show step)

    checkValue (C.ByteString bytes) = BS.length bytes + fromIntegral (BS.last bytes)
    checkValue (C.Array items) = V.foldl' (\n item -> n + checkInteger item) 0 items
    checkValue _ = error "unexpected Wireform value"
    checkInteger (C.UInt word) = fromIntegral word
    checkInteger _ = error "unexpected Wireform array item"
{-# NOINLINE wireform #-}


cborg :: [BS.ByteString] -> Int
cborg chunks0 = runST (Ref.deserialiseIncremental Ref.decodeTerm >>= go chunks0)
  where
    go [] (Ref.Done leftover _ value)
      | BS.null leftover = pure (checkTerm value)
    go (chunk : chunks) (Ref.Partial more) = more (Just chunk) >>= go chunks
    go _ (Ref.Fail _ _ reason) = error (show reason)
    go _ _ = error "unexpected cborg result"

    checkTerm (Ref.TBytes bytes) = BS.length bytes + fromIntegral (BS.last bytes)
    checkTerm (Ref.TBytesI bytes) = fromIntegral (BL.length bytes) + fromIntegral (BL.last bytes)
    checkTerm (Ref.TList items) = sum (map checkInteger items)
    checkTerm _ = error "unexpected cborg value"
    checkInteger (Ref.TInt word) = word
    checkInteger _ = error "unexpected cborg array item"
{-# NOINLINE cborg #-}
