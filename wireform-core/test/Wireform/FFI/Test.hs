module Wireform.FFI.Test (spec) where

import Data.ByteString qualified as BS
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (nullPtr)
import Test.Syd
import Wireform.Encode.Direct qualified as Direct
import Wireform.FFI (compareBounds, validateArrowBuffers)
import Wireform.Hash qualified as Hash


{- | Negative counts and lengths must be rejected at the wrapper, not
widened: they cross the FFI as 'Data.Word.CSize'/'Int64', so a negative
'Int' would become a huge count and make the kernel read past the
caller's buffers.
-}
spec :: Spec
spec = describe "Wireform.FFI and direct encoding boundaries" $ do
  it "rejects a negative buffer count" $ do
    validateArrowBuffers nullPtr (-1) 8 `shouldBe` False

  it "rejects a negative body length" $ do
    validateArrowBuffers nullPtr 0 (-1) `shouldBe` False
    validateArrowBuffers nullPtr 1 (-1) `shouldBe` False

  it "rejects a negative bounds count with an empty mask" $ do
    compareBounds nullPtr (-1) 4 nullPtr `shouldBe` 0

  it "rejects a negative bound width with an empty mask" $ do
    compareBounds nullPtr 1 (-1) nullPtr `shouldBe` 0

  it "still accepts empty input" $ do
    validateArrowBuffers nullPtr 0 0 `shouldBe` True
    compareBounds nullPtr 0 0 nullPtr `shouldBe` 0

  it "keeps IO XXH64 byte-identical for empty, sliced, and block-boundary inputs" $ do
    empty <- Hash.xxh64IO 0 BS.empty
    empty `shouldBe` 0xef46db3751d8e999
    abc <- Hash.xxh64IO 0 (BS.pack [97, 98, 99])
    abc `shouldBe` 0x44bc2cf5ad770999
    mapM_
      ( \size -> do
          let payload = BS.pack (take size (cycle [0 .. 255]))
              sliced = BS.take size (BS.drop 3 (BS.replicate 3 0xa5 <> payload <> BS.replicate 7 0x5a))
          mapM_
            (\seed -> Hash.xxh64IO seed sliced >>= (`shouldBe` Hash.xxh64 seed payload))
            [0, 1, 0x123456789abcdef0, maxBound]
      )
      [0, 1, 7, 8, 15, 16, 31, 32, 33, 63, 64, 65, 257]

  it "writes raw varints across the 21-bit boundary without a field tag" $ do
    -- The published fallback used the field encoder with tag zero. That
    -- inserted an extra byte for values >= 2^21 (including 2 MiB lengths).
    let check value expected = allocaBytes 16 $ \ptr -> do
          fillBytes ptr 0xa5 16
          end <- Direct.dVarint ptr 3 value
          bytes <- peekArray 16 ptr
          end `shouldBe` (3 + length expected)
          bytes `shouldBe` (replicate 3 0xa5 ++ expected ++ replicate (13 - length expected) 0xa5)
    mapM_
      (uncurry check)
      [ (0x1fffff, [255, 255, 127])
      , (0x200000, [128, 128, 128, 1])
      , (0xfffffff, [255, 255, 255, 127])
      , (0x10000000, [128, 128, 128, 128, 1])
      , (maxBound, replicate 9 255 ++ [1])
      ]
