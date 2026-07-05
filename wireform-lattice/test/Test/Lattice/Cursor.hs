{- | Cursor contracts (spec §3.2): deterministic session-free encoding, spec
retirement on keyset drift (§17.2), and malformed-input rejection.
-}
module Test.Lattice.Cursor (tests) where

import Data.Aeson qualified as A
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Lattice.Cursor
import Lattice.Schema (CountPolicy (..), CursorSpec (..), Direction (..))
import Test.Syd


tests :: Spec
tests =
  describe "Cursors (§3.2)" $ do
    describe "§3.2 encode/decode round-trip" $ do
      it "keyset values survive the trip under the generating spec" $ do
        let vals = [A.String "Luke Skywalker", A.Number 42]
        decodeCursor nameAsc (encodeCursor nameAsc vals)
          `shouldBe` Right (Cursor (specHashOf nameAsc) vals)
      it "cursors are deterministic: same values, same text" $ do
        let vals = [A.String "Han Solo"]
        encodeCursor nameAsc vals `shouldBe` encodeCursor nameAsc vals
        encodeCursor nameAsc vals `shouldSatisfy` T.isPrefixOf "cur_"

    describe "§3.2/§17.2 a changed CursorSpec retires old cursors" $ do
      it "a flipped keyset direction decodes as CursorRetired" $ do
        let cur = encodeCursor nameAsc [A.String "Leia Organa"]
        decodeCursor nameDesc cur `shouldBe` Left CursorRetired
      it "a different keyset column decodes as CursorRetired" $ do
        let cur = encodeCursor nameAsc [A.String "Leia Organa"]
        decodeCursor createdDesc cur `shouldBe` Left CursorRetired

    describe "§3.2 malformed cursors are malformed, not retired" $ do
      it "a missing cur_ prefix" $
        decodeCursor nameAsc "nope" `shouldBe` Left CursorMalformed
      it "invalid base64url after the prefix" $
        decodeCursor nameAsc "cur_%%%" `shouldBe` Left CursorMalformed
      it "a payload that is not a JSON array" $
        decodeCursor nameAsc "cur_eyJhIjoxfQ" `shouldBe` Left CursorMalformed


-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

nameAsc :: CursorSpec
nameAsc =
  CursorSpec
    { csKeyset = ("name", Asc) :| []
    , csDefaultPage = Just 10
    , csMaxPage = 50
    , csTotal = CountNone
    }


nameDesc :: CursorSpec
nameDesc = nameAsc {csKeyset = ("name", Desc) :| []}


createdDesc :: CursorSpec
createdDesc = nameAsc {csKeyset = ("createdAt", Desc) :| []}
