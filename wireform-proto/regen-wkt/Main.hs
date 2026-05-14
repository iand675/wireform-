{- | Regenerate the well-known type modules from their .proto files.

The list of input @.proto@ files is given as @(modulePath, diskPath)@
pairs below; the *output* Haskell module name and filesystem path are
derived by 'Proto.CodeGen.moduleNameForProto', which respects the
@csharp_namespace@ option in each @.proto@. E.g.

    option csharp_namespace = "Google.Protobuf.WellKnownTypes";

in @timestamp.proto@ produces module
@Proto.Google.Protobuf.WellKnownTypes.Timestamp@ at
@src/Proto/Google/Protobuf/WellKnownTypes/Timestamp.hs@.

Before regenerating, this driver also sweeps any pre-existing stale
output files at *both* the legacy unprefixed paths (e.g.
@src/Proto/Google/Protobuf/Timestamp.hs@) and the new
@WellKnownTypes/@-prefixed paths, so a regen run is a clean slate.

Run from the @wireform-proto@ directory:

    cabal run regen-wkt
-}
module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Proto.CodeGen (
  GenerateOpts (..),
  buildTypeRegistry,
  builtinWellKnownTypes,
  defaultGenerateOpts,
  generateModuleText,
  moduleNameForProto,
 )
import Proto.IDL.Parser.Resolver (ResolvedProto (..), resolveProtoImports)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeDirectory, (</>))


-- | @(modulePath, diskPath)@ for every proto in the well-known-types
-- regen pass. @modulePath@ is the *import-relative* name used by other
-- protos (i.e. the path that would appear in an @import "..."@
-- directive); @diskPath@ is the filesystem location to read the proto
-- from.
protos :: [(FilePath, FilePath)]
protos =
  [ ("google/protobuf/timestamp.proto",       "data/proto/google/protobuf/timestamp.proto")
  , ("google/protobuf/duration.proto",        "data/proto/google/protobuf/duration.proto")
  , ("google/protobuf/empty.proto",           "data/proto/google/protobuf/empty.proto")
  , ("google/protobuf/field_mask.proto",      "data/proto/google/protobuf/field_mask.proto")
  , ("google/protobuf/source_context.proto",  "data/proto/google/protobuf/source_context.proto")
  , ("google/protobuf/any.proto",             "data/proto/google/protobuf/any.proto")
  , ("google/protobuf/wrappers.proto",        "data/proto/google/protobuf/wrappers.proto")
  , ("google/protobuf/struct.proto",          "data/proto/google/protobuf/struct.proto")
  -- type.proto and api.proto are intentionally excluded from this list.
  -- Both define messages with names that collide with Haskell built-ins:
  -- type.proto has a top-level message "Enum" which conflicts with
  -- Prelude.Enum (the typeclass), breaking 'deriving Enum' in the generated
  -- file. Including type.proto in the registry also causes descriptor.proto's
  -- nested FieldDescriptorProto.Type enum to be incorrectly resolved as
  -- google.protobuf.Type (from type.proto) instead of the local nested enum.
  -- The .proto files ship in data/proto/ for runtime use; they are not
  -- compiled as library modules.
  -- Reflection-time / plugin-protocol types. These ship the upstream
  -- @descriptor.proto@ and @compiler/plugin.proto@ unmodified; their
  -- @csharp_namespace@ options (@Google.Protobuf.Reflection@ and
  -- @Google.Protobuf.Compiler@ respectively) put them at
  -- @Proto.Google.Protobuf.Reflection.Descriptor@ and
  -- @Proto.Google.Protobuf.Compiler.Plugin@.
  , ("google/protobuf/descriptor.proto",      "data/proto/google/protobuf/descriptor.proto")
  , ("google/protobuf/compiler/plugin.proto", "data/proto/google/protobuf/compiler/plugin.proto")
  ]


-- | Legacy output paths (no @WellKnownTypes/@ segment). These are
-- the files the *old* hard-coded regen-wkt wrote to. We sweep them
-- on every regen so stale output never lingers.
legacyOutputs :: [FilePath]
legacyOutputs =
  [ "src/Proto/Google/Protobuf/Timestamp.hs"
  , "src/Proto/Google/Protobuf/Duration.hs"
  , "src/Proto/Google/Protobuf/Empty.hs"
  , "src/Proto/Google/Protobuf/FieldMask.hs"
  , "src/Proto/Google/Protobuf/SourceContext.hs"
  , "src/Proto/Google/Protobuf/Any.hs"
  , "src/Proto/Google/Protobuf/Wrappers.hs"
  , "src/Proto/Google/Protobuf/Struct.hs"
  -- Removed hand-written @Descriptor.hs@ — use @Reflection.Descriptor@.
  , "src/Proto/Google/Protobuf/Descriptor.hs"
  , "src/Proto/Google/Protobuf/Type.hs"
  ]


opts :: GenerateOpts
opts =
  defaultGenerateOpts
    { genModulePrefix = "Proto"
    }


-- | Convert a Haskell module name (e.g. @\"Proto.Google.Protobuf.WellKnownTypes.Timestamp\"@)
-- into a source-tree path (e.g. @\"src/Proto/Google/Protobuf/WellKnownTypes/Timestamp.hs\"@).
moduleNameToPath :: Text -> FilePath
moduleNameToPath mn =
  let parts = T.splitOn (T.pack ".") mn
      joined = T.unpack (T.intercalate (T.pack "/") parts)
   in "src" </> (joined ++ ".hs")


-- | Sweep a previously-generated file if it exists. Silent no-op
-- otherwise. We only delete files we know we wrote ourselves —
-- never anything outside the configured sweep list.
sweep :: FilePath -> IO ()
sweep p = do
  exists <- doesFileExist p
  if exists
    then do
      removeFile p
      putStrLn $ "  Removed stale " <> p
    else pure ()


main :: IO ()
main = do
  putStrLn "Sweeping legacy output paths..."
  mapM_ sweep legacyOutputs

  putStrLn ""
  putStrLn "Resolving proto imports..."
  resolved <- traverse resolveOne protos

  -- A *global* registry covering every proto in this regen pass.
  -- @builtinWellKnownTypes@ provides the eight WKT fallback entries
  -- (with the @csharp_namespace@-derived Haskell module names), but
  -- we *also* fold in entries for the in-pass files so a cross-file
  -- type reference such as @plugin.proto@ → @FileDescriptorProto@
  -- resolves to @Proto.Google.Protobuf.Reflection.Descriptor@.
  -- Without this the generator emits a bare unqualified type name and
  -- the build fails with "Not in scope".
  let globalReg =
        builtinWellKnownTypes
          <> buildTypeRegistry opts (fmap (\(modulePath, _, rp) -> (modulePath, rp)) resolved)

  putStrLn ""
  putStrLn "Regenerating well-known types..."
  mapM_
    ( \(modulePath, _diskPath, rp) -> do
        let modName = moduleNameForProto opts modulePath (rpFile rp)
            hsPath = moduleNameToPath modName
            code = generateModuleText opts globalReg modulePath (rpFile rp)
        createDirectoryIfMissing True (takeDirectory hsPath)
        TIO.writeFile hsPath code
        putStrLn $ "  Generated " <> hsPath <> " (module " <> T.unpack modName <> ")"
    )
    resolved

  putStrLn "Done."
  where
    resolveOne (modulePath, diskPath) = do
      result <- resolveProtoImports ["data/proto/", "data/", "."] diskPath
      case result of
        Left err -> error $ "Resolve error for " <> diskPath <> ": " <> show err
        Right rp -> pure (modulePath, diskPath, rp)
