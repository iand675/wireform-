module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.CodeGen (codeGenTests)
import Test.Collect (collectTests)
import Test.Compat (compatTests)
import Test.Hooks (hooksTests)
import Test.JSON (jsonTests)
import Test.Lens (lensTests)
import Test.Options (optionsTests)
import Test.Parser (parserTests)
import Test.PrintInspect (printInspectTests)
import Test.Resolver (resolverTests)
import Test.RingSink (ringSinkTests)
import Test.Roundtrip (roundtripTests)
import Test.Schema (schemaTests)
import Test.StreamCodec (streamCodecTests)
import Test.Syd
import Test.TDP (dynamicSchemaTests)
import Test.WellKnown (wellKnownTests)
import Test.WellKnownUtil (wellKnownUtilTests)
import Test.Wire (wireTests)


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest
    $ describe
      "wireform-proto"
    $ sequence_
      [ parserTests
      , wireTests
      , ringSinkTests
      , roundtripTests
      , codeGenTests
      , wellKnownTests
      , wellKnownUtilTests
      , printInspectTests
      , compatTests
      , schemaTests
      , optionsTests
      , lensTests
      , streamCodecTests
      , jsonTests
      , hooksTests
      , dynamicSchemaTests
      , resolverTests
      , collectTests
      ]
