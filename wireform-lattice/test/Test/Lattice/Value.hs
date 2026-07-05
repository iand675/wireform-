{- | Canonical value and hash contracts (spec §3.5.3, §5.1, §7.3): one
deterministic JSON rendering per value, pinned number forms, the canonical
field key, and the pinned content-address prefixes and truncation lengths.
-}
module Test.Lattice.Value (tests) where

import Data.Aeson qualified as A
import Data.Char (isAlphaNum)
import Data.Scientific qualified as Sci
import Data.Text qualified as T
import Lattice.Canonical (canonicalFieldKey)
import Lattice.Hash (
  dictHash,
  manifestEtagHash,
  planIdHash,
  queryHash,
  schemaHash,
 )
import Lattice.Value (canonicalJsonText)
import Test.Syd


tests :: Spec
tests =
  describe "Canonical values and hashes (§3.5.3)" $ do
    describe "§3.5.3 canonical JSON" $ do
      it "object keys sort by code point, no insignificant whitespace" $
        canonicalJsonText
          (A.object ["b" A..= (1 :: Int), "a" A..= (2 :: Int)])
          `shouldBe` "{\"a\":2,\"b\":1}"
      it "sorting is recursive" $
        canonicalJsonText
          (A.object ["z" A..= A.object ["b" A..= (1 :: Int), "a" A..= (2 :: Int)], "a" A..= True])
          `shouldBe` "{\"a\":true,\"z\":{\"a\":2,\"b\":1}}"

    describe "§3.5.3 pinned number rendering" $ do
      it "1.0 renders as the integer 1" $
        canonicalJsonText (A.Number (Sci.scientific 10 (-1))) `shouldBe` "1"
      it "0e5 normalizes to 0" $
        canonicalJsonText (A.Number (Sci.scientific 0 5)) `shouldBe` "0"
      it "-0 normalizes to 0" $
        canonicalJsonText (A.Number (Sci.fromFloatDigits (-0.0 :: Double))) `shouldBe` "0"
      it "the IEEE-safe bound 2^53-1 renders plain" $
        canonicalJsonText (A.Number 9007199254740991) `shouldBe` "9007199254740991"
      it "2^53+1 leaves the safe range but stays exact (scientific form)" $
        canonicalJsonText (A.Number 9007199254740993) `shouldBe` "9.007199254740993e15"

    describe "§4.1/§9.1 canonical field key" $ do
      it "no arguments renders the bare name" $
        canonicalFieldKey "name" [] `shouldBe` "name"
      it "arguments sort by name and render canonical JSON" $
        canonicalFieldKey
          "comments"
          [("first", A.Number 20), ("after", A.String "cur_x")]
          `shouldBe` "comments(after:\"cur_x\",first:20)"
      it "the doc example: avatarUrl(size:48)" $
        canonicalFieldKey "avatarUrl" [("size", A.Number 48)] `shouldBe` "avatarUrl(size:48)"

    describe "§5.1/§7.3 content-address prefixes and truncation lengths" $ do
      it "queryHash: 22 base64url chars (128-bit truncation)" $ do
        T.length (queryHash "query{me{name}}") `shouldBe` 22
        queryHash "query{me{name}}" `shouldSatisfy` T.all isB64Url
      it "schemaHash: 's' + 22 chars" $ do
        let h = schemaHash "schema x\n"
        T.length h `shouldBe` 23
        h `shouldSatisfy` T.isPrefixOf "s"
      it "planId: 'pl_' + 16 chars (96-bit truncation)" $ do
        let h = planIdHash "query{me{name}}" "entity User by id {}"
        T.length h `shouldBe` 19
        h `shouldSatisfy` T.isPrefixOf "pl_"
      it "manifest etag: 'm:' + 16 chars" $ do
        let h = manifestEtagHash "manifest input"
        T.length h `shouldBe` 18
        h `shouldSatisfy` T.isPrefixOf "m:"
      it "dictHash: 22 chars" $
        T.length (dictHash "dictionary bytes") `shouldBe` 22
      it "hashes are pure functions of their input" $ do
        queryHash "a" `shouldBe` queryHash "a"
        queryHash "a" `shouldNotBe` queryHash "b"
        planIdHash "q" "declsA" `shouldNotBe` planIdHash "q" "declsB"


isB64Url :: Char -> Bool
isB64Url c = isAlphaNum c || c == '-' || c == '_'
