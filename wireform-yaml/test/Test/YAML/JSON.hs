{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.JSON (tests) where

import Data.Aeson qualified as Aeson
import Test.Syd
import YAML.JSON qualified as J
import YAML.Value


tests :: Spec
tests = describe "json" $ do
  it "projects non-finite JSON values to null" $
    mapM_ (\d -> J.yamlToJSON (YFloat d) `shouldBe` Aeson.Null) [0 / 0, 1 / 0, -1 / 0]
