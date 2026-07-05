module Main (main) where

import Test.Syd (describe, sydTest)

import Test.Lattice.Canonical qualified
import Test.Lattice.Compress qualified
import Test.Lattice.Cursor qualified
import Test.Lattice.E2E qualified
import Test.Lattice.IDL qualified
import Test.Lattice.Plan qualified
import Test.Lattice.Query qualified
import Test.Lattice.Value qualified
import Test.Lattice.Wire qualified

main :: IO ()
main =
  sydTest $
    describe "wireform-lattice" $ do
      Test.Lattice.IDL.tests
      Test.Lattice.Query.tests
      Test.Lattice.Canonical.tests
      Test.Lattice.Plan.tests
      Test.Lattice.Wire.tests
      Test.Lattice.Cursor.tests
      Test.Lattice.Compress.tests
      Test.Lattice.Value.tests
      Test.Lattice.E2E.tests
