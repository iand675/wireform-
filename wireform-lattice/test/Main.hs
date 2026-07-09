module Main (main) where

import Test.Syd (describe, sydTest)

import Test.Lattice.Canonical qualified
import Test.Lattice.Compress qualified
import Test.Lattice.Compat qualified
import Test.Lattice.Consistency qualified
import Test.Lattice.Cursor qualified
import Test.Lattice.Digest qualified
import Test.Lattice.Derived qualified
import Test.Lattice.DigestE2E qualified
import Test.Lattice.E2E qualified
import Test.Lattice.Feed qualified
import Test.Lattice.Fusion qualified
import Test.Lattice.Gateway qualified
import Test.Lattice.Governance qualified
import Test.Lattice.IDL qualified
import Test.Lattice.Live qualified
import Test.Lattice.Plan qualified
import Test.Lattice.Nodes qualified
import Test.Lattice.Otel qualified
import Test.Lattice.Projection qualified
import Test.Lattice.Query qualified
import Test.Lattice.Registry qualified
import Test.Lattice.Typed qualified
import Test.Lattice.Value qualified
import Test.Lattice.Verbs qualified
import Test.Lattice.VerbsE2E qualified
import Test.Lattice.Wire qualified

main :: IO ()
main =
  sydTest $
    describe "wireform-lattice" $ do
      Test.Lattice.IDL.tests
      Test.Lattice.Verbs.tests
      Test.Lattice.Query.tests
      Test.Lattice.Canonical.tests
      Test.Lattice.Plan.tests
      Test.Lattice.Projection.tests
      Test.Lattice.Wire.tests
      Test.Lattice.Digest.tests
      Test.Lattice.Cursor.tests
      Test.Lattice.Compress.tests
      Test.Lattice.Value.tests
      Test.Lattice.E2E.tests
      Test.Lattice.Governance.tests
      Test.Lattice.VerbsE2E.tests
      Test.Lattice.DigestE2E.tests
      Test.Lattice.Derived.tests
      Test.Lattice.Registry.tests
      Test.Lattice.Compat.tests
      Test.Lattice.Consistency.tests
      Test.Lattice.Live.tests
      Test.Lattice.Feed.tests
      Test.Lattice.Nodes.tests
      Test.Lattice.Fusion.tests
      Test.Lattice.Gateway.tests
      Test.Lattice.Otel.tests
      Test.Lattice.Typed.tests
