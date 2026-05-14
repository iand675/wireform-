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
import Control.Monad (forM, forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Proto.CodeGen (
  GenerateOpts (..),
  TypeRegistry,
  defaultGenerateOpts,
  generateModuleText,
  moduleNameForProto,
 )
import Proto.CodeGen.Hooks (CodeGenHooks, defaultCodeGenHooks)
import Proto.IDL.AST (ProtoFile, ProtoFile' (..))
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getModificationTime,
  listDirectory,
 )
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))


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
  -- ^ When 'True', generate lazy submessage decoders using 'LazyMessage'.
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
Only regenerates when source is newer than output.
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
      protos <- findProtoFiles protoDir
      unless (null protos) $
        putStrLn $
          "[wireform] Found " <> show (length protos) <> " .proto file(s) in " <> protoDir
      forM_ protos $ \relPath ->
        generateProtoFile cfg (protoDir </> relPath)


-- | Generate Haskell for one .proto file. Skips if output is up-to-date.
generateProtoFile :: ProtoGenConfig -> FilePath -> IO ()
generateProtoFile cfg protoPath = do
  contents <- TIO.readFile protoPath
  case parseProtoFile protoPath contents of
    Left err ->
      putStrLn $ "[wireform] " <> renderParseError err
    Right pf -> do
      let opts =
            defaultGenerateOpts
              { genModulePrefix = pgcModulePrefix cfg
              , genLazySubmessages = pgcLazySub cfg
              , genHooks = pgcHooks cfg
              }
          emptyReg = Map.empty :: TypeRegistry
          code = generateModuleText opts emptyReg protoPath pf
          outPath = pgcOutputDir cfg </> moduleToPath opts protoPath pf <.> "hs"
      needsRegen <- checkStale protoPath outPath
      when needsRegen $ do
        createDirectoryIfMissing True (takeDirectory outPath)
        TIO.writeFile outPath code
        putStrLn $ "[wireform] Generated " <> outPath


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


checkStale :: FilePath -> FilePath -> IO Bool
checkStale src out = do
  outExists <- doesFileExist out
  if not outExists
    then pure True
    else do
      srcT <- getModificationTime src
      outT <- getModificationTime out
      pure (srcT > outT)


moduleToPath :: GenerateOpts -> FilePath -> ProtoFile -> FilePath
moduleToPath opts fp pf =
  let modName = T.unpack (moduleNameForProto opts fp pf)
  in fmap (\c -> if c == '.' then '/' else c) modName
