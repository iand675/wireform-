{- | Cabal Setup.hs hook for automatic protobuf code generation.

== Basic usage

@
-- Setup.hs
import Distribution.Simple
import Proto.Setup

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \\args flags -> do
      protoGenPreBuildHook defaultProtoGenConfig
      preBuild simpleUserHooks args flags
  }
@

Then in your @.cabal@ file:

@
build-type: Custom

custom-setup
  setup-depends: base, wireform-proto, Cabal, directory, filepath, text

library
  hs-source-dirs: src, gen
@

== With codegen hooks

Register hooks via 'pgcHooks' to produce extra code based on proto attributes:

@
import Proto.Setup
import Proto.CodeGen.Hooks

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \\args flags -> do
      protoGenPreBuildHook defaultProtoGenConfig
        { pgcHooks = onMessageAttribute "audited" $ \\val ctx ->
            case val of
              CBool True -> ["-- audited: " \<> mhcHsTypeName ctx]
              _          -> []
        }
      preBuild simpleUserHooks args flags
  }
@
-}
module Proto.Setup (
  ProtoGenConfig (..),
  defaultProtoGenConfig,
  protoGenPreBuildHook,
  generateProtos,
  generateProtoFile,
) where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, unless)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Proto.CodeGen (
  GenerateOpts (..),
  buildTypeRegistry,
  defaultGenerateOpts,
  generateModuleTextWithImports,
  moduleNameForProto,
  restrictOptionImportTypes,
 )
import Proto.CodeGen.Hooks (CodeGenHooks, defaultCodeGenHooks)
import Proto.IDL.Parser.Resolver (ResolveError (..), ResolvedProto (..), resolveProtoImports, resolvedImportFiles, resolvedTypeDependencies)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  makeAbsolute,
 )
import System.FilePath (isRelative, makeRelative, normalise, takeDirectory, takeExtension, takeFileName, (<.>), (</>))


{- | Configuration for automatic protobuf code generation.

Use 'pgcHooks' to register codegen hooks that fire based on proto attributes:

@
import Proto.Setup
import Proto.CodeGen.Hooks

myConfig :: 'ProtoGenConfig'
myConfig = 'defaultProtoGenConfig'
  { 'pgcHooks' = myHooks
  }
@
-}
data ProtoGenConfig = ProtoGenConfig
  { pgcProtoDir :: FilePath
  -- ^ Directory containing @.proto@ source files.
  , pgcIncludeDirs :: [FilePath]
  -- ^ Additional directories to search when resolving @import@ statements.
  , pgcOutputDir :: FilePath
  -- ^ Output directory for generated Haskell modules.
  , pgcModulePrefix :: T.Text
  -- ^ Haskell module prefix for generated code (e.g. @\"Proto.Gen\"@).
  , pgcLazySub :: Bool
  -- ^ When 'True', generate lazy submessage decoders using 'Proto.Decode.LazyMessage'.
  , pgcHooks :: CodeGenHooks
  -- ^ Codegen hooks that fire based on proto attributes.
  }


-- | Sensible defaults: reads from @proto/@, writes to @gen/@, uses module prefix @Proto.Gen@.
defaultProtoGenConfig :: ProtoGenConfig
defaultProtoGenConfig =
  ProtoGenConfig
    { pgcProtoDir = "proto"
    , pgcIncludeDirs = []
    , pgcOutputDir = "gen"
    , pgcModulePrefix = "Proto.Gen"
    , pgcLazySub = False
    , pgcHooks = defaultCodeGenHooks
    }


{- | Pre-build hook: generate Haskell from all .proto files in pgcProtoDir.
Rewrites modules whose generated content differs from the existing output.
-}
protoGenPreBuildHook :: ProtoGenConfig -> IO ()
protoGenPreBuildHook = generateProtos


-- | Find and generate all .proto files.
generateProtos :: ProtoGenConfig -> IO ()
generateProtos cfg = do
  let protoDir = pgcProtoDir cfg
  exists <- doesDirectoryExist protoDir
  if not exists
    then putStrLn $ "[wireform] Proto directory not found: " <> protoDir
    else do
      protos <- sort <$> findProtoFiles protoDir
      unless (null protos) $
        putStrLn $
          "[wireform] Found " <> show (length protos) <> " .proto file(s) in " <> protoDir
      resolved <- traverse (resolveForGeneration cfg . (protoDir </>)) protos
      generateResolvedProtos cfg resolved


-- | Generate Haskell for one .proto file. Skips if output is up-to-date.
generateProtoFile :: ProtoGenConfig -> FilePath -> IO ()
generateProtoFile cfg protoPath = do
  resolved <- resolveForGeneration cfg protoPath
  generateResolvedProtos cfg [resolved]


resolveForGeneration :: ProtoGenConfig -> FilePath -> IO ResolvedProto
resolveForGeneration cfg path = do
  result <- resolveProtoImports (pgcProtoDir cfg : pgcIncludeDirs cfg) path
  either (fail . ("[wireform] " <>) . describeError) pure result
  where
    describeError (ParseError _ message) = message
    describeError (FileNotFound _ dependency directories) =
      path <> ": cannot find import " <> T.unpack dependency <> " in " <> show directories
    describeError (CircularImport chain) =
      "circular import: " <> T.unpack (T.intercalate " -> " chain)


generateResolvedProtos :: ProtoGenConfig -> [ResolvedProto] -> IO ()
generateResolvedProtos cfg resolved = do
  roots <- traverse (\dir -> (dir,) <$> makeAbsolute dir) (pgcProtoDir cfg : pgcIncludeDirs cfg)
  files <- Map.fromList <$> traverse identify (concatMap resolvedTypeDependencies resolved)
  let opts =
        defaultGenerateOpts
          { genModulePrefix = pgcModulePrefix cfg
          , genLazySubmessages = pgcLazySub cfg
          , genHooks = pgcHooks cfg
          }
      modulePath = sourceModulePath roots . rpPath
      output proto =
        pgcOutputDir cfg </> moduleToPath (moduleNameForProto opts (modulePath proto) (rpFile proto)) <.> "hs"
      outputs = Map.fromListWith (<>) (map (outputEntry output) (Map.elems files))
  forM_ (Map.elems outputs) $ \sources -> case sources of
    (first, dest) : (second, other) : _
      | dest == other ->
          fail ("[wireform] " <> first <> " and " <> second <> " both generate " <> dest)
      | otherwise ->
          fail ("[wireform] " <> first <> " and " <> second
            <> " generate paths differing only by case: " <> dest <> ", " <> other)
    _ -> pure ()
  prepared <- traverse (\proto -> do
    let importFiles = resolvedImportFiles proto
        pairs = map (\dependency -> (modulePath dependency, dependency)) (resolvedTypeDependencies proto)
    registry <- either (fail . T.unpack) pure $
      restrictOptionImportTypes importFiles (rpFile proto) (buildTypeRegistry opts pairs)
    code <- either (fail . T.unpack) pure $
      generateModuleTextWithImports opts registry (modulePath proto) importFiles (rpFile proto)
    pure (output proto, code)) resolved
  forM_ prepared $ \(outPath, code) -> do
    exists <- doesFileExist outPath
    unchanged <- if exists then (== code) <$> TIO.readFile outPath else pure False
    unless unchanged $ do
      createDirectoryIfMissing True (takeDirectory outPath)
      TIO.writeFile outPath code
      putStrLn $ "[wireform] Generated " <> outPath
  where
    outputEntry output proto =
      let dest = output proto
      in (T.toCaseFold (T.pack dest), [(rpPath proto, dest)])
    identify proto = do
      absolute <- makeAbsolute (rpPath proto)
      pure (absolute, proto)


-- Preserve the module names previously generated from relative paths. For an
-- absolute path, remove the source or include root before naming its module.
sourceModulePath :: [(FilePath, FilePath)] -> FilePath -> FilePath
sourceModulePath roots path
  | isRelative path = normalise path
  | otherwise = fromMaybe (takeFileName path) (listToMaybe (mapMaybe relativeToRoot roots))
  where
    relativeToRoot (configured, absolute)
      | isRelative relative =
          Just (if isRelative configured then normalise (configured </> relative) else relative)
      | otherwise = Nothing
      where
        relative = makeRelative absolute path


findProtoFiles :: FilePath -> IO [FilePath]
findProtoFiles root = go ""
  where
    go prefix = do
      let dir = root </> prefix
      entries <- listDirectory dir `catch` (\(_ :: IOException) -> pure [])
      fmap concat $ forM entries $ \entry -> do
        let rel = if null prefix then entry else prefix </> entry
            full = root </> rel
        isDir <- doesDirectoryExist full
        if isDir
          then go rel
          else pure [rel | takeExtension entry == ".proto"]


moduleToPath :: T.Text -> FilePath
moduleToPath = map (\c -> if c == '.' then '/' else c) . T.unpack
