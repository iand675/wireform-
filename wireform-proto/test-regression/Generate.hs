{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- Run from wireform-proto: cabal exec -- runghc test-regression/Generate.hs
module Main where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Proto.CodeGen
import Proto.IDL.AST (importPath, protoImports)
import Proto.IDL.Parser
import Proto.IDL.Parser.Resolver (ResolvedProto (..), importClosure)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))


main :: IO ()
main = do
  files <- mapM readSchema ["edition2023", "edition2024", "integerMaps", "json_temporal", "json_collections", "packing_legacy", "groups", "edition_delimited", "extension_utf8", "rpc", "import_status", "import_scope", "my_file", "my__file", "google/protobuf/audit_mode", "google_import", "option_import"]
  optionSchemas <- mapM readSchema ["option_policy", "option_defaults"]
  let opts = defaultGenerateOpts {genModulePrefix = "Generated"}
      registry = buildTypeRegistry opts files
      schemas = Map.fromList (map (\(path, rp) -> (T.pack path, rpFile rp)) (optionSchemas <> files))
  mapM_ (generate opts registry schemas) files
  where
    readSchema name = do
      let file = name <> ".proto"
      source <- TIO.readFile ("test-regression/proto" </> file)
      pf <- either (fail . show) pure (parseProtoFile file source)
      pure (file, ResolvedProto pf file Map.empty)
    generate opts registry schemas (file, rp) = do
      let pf = rpFile rp
          output = "test-regression" </> T.unpack (T.replace "." "/" (moduleNameForProto opts file pf)) <> ".hs"
          imports = Map.restrictKeys schemas (importClosure schemas (const True) (map importPath (protoImports pf)))
      code <- either (fail . T.unpack) pure (generateModuleTextWithImports opts registry file imports pf)
      createDirectoryIfMissing True (takeDirectory output)
      TIO.writeFile output code
