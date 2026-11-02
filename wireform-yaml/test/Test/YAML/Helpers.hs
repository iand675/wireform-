module Test.YAML.Helpers (
  assertRenderRoundtrip,
  assertDefaultRenderRoundtrip,
) where

import Test.Syd
import YAML.Decode qualified as D
import YAML.Pretty qualified as P
import YAML.Value (Value)


assertRenderRoundtrip :: P.RenderOptions -> Value -> IO ()
assertRenderRoundtrip opts value = D.decode (P.render opts value) `shouldBe` Right value


assertDefaultRenderRoundtrip :: Value -> IO ()
assertDefaultRenderRoundtrip value = D.decode (P.render P.defaultOptions value) `shouldBe` Right value
