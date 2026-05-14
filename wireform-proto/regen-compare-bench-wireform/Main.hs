{-# LANGUAGE OverloadedStrings #-}

{- | Regenerate wireform text-codegen output for @compare-bench@.

Reads @bench/compare/gen-wireform/Messages.proto@ and writes
@bench/compare/gen-wireform/Proto/Bench/Wireform/Messages.hs@ so the
Criterion harness exercises the same @Proto.CodeGen@ decode path as
production (including @inOrderStage@ fast paths for ascending tags).

Run from the @wireform-proto@ package root:

> cabal run regen-compare-bench-wireform
-}
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Proto.CodeGen (GenerateOpts (..), defaultGenerateOpts, generateModuleText, moduleNameForProto)
import Proto.IDL.Parser (parseProtoFile)
import Proto.IDL.Parser.Error (renderParseError)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

opts :: GenerateOpts
opts =
  defaultGenerateOpts
    { genModulePrefix = "Proto"
    }

-- | Locate the @wireform-proto@ package directory (monorepo-friendly).
findWireformProtoRoot :: IO FilePath
findWireformProtoRoot = do
  here <- getCurrentDirectory
  let candidates =
        here
          : (here </> "wireform-proto")
          : [takeDirectory here </> "wireform-proto" | takeDirectory here /= here]
  go candidates
 where
  go [] =
    fail "regen-compare-bench-wireform: wireform-proto.cabal not found (run from repo root or wireform-proto/)"
  go (dir : rest) = do
    ok <- doesFileExist (dir </> "wireform-proto.cabal")
    if ok then pure dir else go rest

main :: IO ()
main = do
  root <- findWireformProtoRoot
  let input = root </> "bench/compare/gen-wireform/Messages.proto"
      filePath = "Messages.proto"
  src <- TIO.readFile input
  case parseProtoFile filePath src of
    Left err -> do
      hPutStrLn stderr (renderParseError err)
      error "parseProtoFile failed"
    Right pf -> do
      let code = generateModuleText opts Map.empty filePath pf
          modPath =
            T.unpack
              ( T.map
                  (\c -> if c == '.' then '/' else c)
                  (moduleNameForProto opts filePath pf)
              )
          out = root </> "bench/compare/gen-wireform" </> modPath <> ".hs"
      createDirectoryIfMissing True (takeDirectory out)
      TIO.writeFile out code
      putStrLn ("Wrote " <> out)
