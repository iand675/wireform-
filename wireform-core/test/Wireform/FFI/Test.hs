module Wireform.FFI.Test (spec) where

import Foreign.Ptr (nullPtr)
import Test.Syd
import Wireform.FFI (compareBounds, validateArrowBuffers)


{- | Negative counts and lengths must be rejected at the wrapper, not
widened: they cross the FFI as 'Data.Word.CSize'/'Int64', so a negative
'Int' would become a huge count and make the kernel read past the
caller's buffers.
-}
spec :: Spec
spec = describe "Wireform.FFI negative arguments" $ do
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
