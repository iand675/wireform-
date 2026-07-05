{- | Compression contracts (spec §5.2): raw-DEFLATE round-trips with and
without the schema dictionary, the dictionary earns its bytes, and a
mismatched dictionary never silently yields the original text.
-}
module Test.Lattice.Compress (tests) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Lattice.Canonical (Compiled (..))
import Lattice.Compress (compressQuery, decompressQuery, schemaDictionary)
import Test.Lattice.Fixtures
import Test.Syd


tests :: Spec
tests =
  describe "Compression (§5.2)" $ do
    describe "§5.2 round-trips" $ do
      it "without a dictionary (dv omitted on inline URLs)" $ do
        t <- feedPageCanonical
        decompressQuery Nothing (compressQuery Nothing t) `shouldBe` Right t
        decompressQuery Nothing (compressQuery Nothing "") `shouldBe` Right ""
      it "with the schema dictionary" $ do
        t <- feedPageCanonical
        let dict = schemaDictionary blogSchema
        decompressQuery (Just dict) (compressQuery (Just dict) t) `shouldBe` Right t

    describe "§5.2 the dictionary earns its bytes" $ do
      it "dictionary compression beats bare compression on FeedPage" $ do
        t <- feedPageCanonical
        let with = BS.length (compressQuery (Just (schemaDictionary blogSchema)) t)
            without = BS.length (compressQuery Nothing t)
        with `shouldSatisfy` (< without)

    describe "§5.2 dictionaries are content-addressed for a reason" $ do
      it "decompressing under the wrong dictionary never yields the original" $ do
        t <- feedPageCanonical
        let compressed = compressQuery (Just (schemaDictionary blogSchema)) t
        decompressQuery (Just (schemaDictionary starwarsSchema)) compressed
          `shouldNotBe` Right t


feedPageCanonical :: IO Text
feedPageCanonical = compiledText <$> mustCompileWith blogSchema feedPageText
