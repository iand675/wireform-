{- | Import resolution for .proto files with include directory support.

When a proto file contains @import "google/protobuf/timestamp.proto"@,
the resolver searches the include directories in order to find the
file, then parses it and transitively resolves its imports.
-}
module Proto.IDL.Parser.Resolver (
  -- * Configuration
  ResolveConfig (..),
  defaultResolveConfig,

  -- * Resolution
  resolveProtoFile,
  resolveProtoImports,
  ResolvedProto (..),
  ResolveError (..),

  -- * Bundled well-known types
  bundledIncludeDir,
  getBundledIncludeDir,
) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Paths_wireform_proto qualified
import Proto.IDL.AST
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))


-- | Configuration for proto file resolution.
data ResolveConfig = ResolveConfig
  { rcIncludeDirs :: ![FilePath]
  -- ^ Directories to search for imported .proto files, in order.
  -- The directory containing the importing file is always searched first.
  , rcBundledDir :: !(Maybe FilePath)
  -- ^ Path to bundled well-known proto files (google/protobuf/*.proto).
  -- If Nothing, uses the built-in bundled protos from the package.
  }
  deriving stock (Show, Eq)


-- | Default resolution config with no extra include directories and automatic bundled dir.
defaultResolveConfig :: ResolveConfig
defaultResolveConfig =
  ResolveConfig
    { rcIncludeDirs = []
    , rcBundledDir = Nothing
    }


-- | A fully resolved proto file with all its transitive imports.
data ResolvedProto = ResolvedProto
  { rpFile :: !ProtoFile
  , rpPath :: !FilePath
  , rpImports :: !(Map Text ResolvedProto)
  }
  deriving stock (Show)


-- | Errors that can occur during proto file resolution.
data ResolveError
  = -- | A proto file failed to parse.
    ParseError !FilePath !String
  | -- | An imported file could not be found in any include directory.
    FileNotFound !FilePath !Text ![FilePath]
  | -- | A circular import chain was detected.
    CircularImport ![Text]
  deriving stock (Show, Eq)


{- | The path to bundled well-known protobuf definitions shipped with this package.
This is a fallback for development; prefer 'getBundledIncludeDir' which uses
'Paths_wireform_proto' to locate the data-files at install time.
-}
bundledIncludeDir :: FilePath
bundledIncludeDir = "proto"


{- | Locate the bundled well-known .proto files using 'Paths_wireform_proto'.
The data-files are installed under the package data-dir; we look up
a known file and derive the root directory.
-}
getBundledIncludeDir :: IO FilePath
getBundledIncludeDir = do
  refFile <- Paths_wireform_proto.getDataFileName "proto/google/protobuf/timestamp.proto"
  let dir =
        takeDirectory (takeDirectory (takeDirectory (takeDirectory refFile)))
          </> "proto"
  exists <- doesFileExist refFile
  pure (if exists then dir else bundledIncludeDir)


-- | Resolve a proto file and all its transitive imports.
resolveProtoFile
  :: ResolveConfig
  -> FilePath
  -> IO (Either ResolveError ResolvedProto)
resolveProtoFile cfg path = do
  bundled <- bundledDirs cfg
  let cfg' = cfg {rcIncludeDirs = takeDirectory path : rcIncludeDirs cfg <> bundled}
  resolve cfg' Map.empty [] path


bundledDirs :: ResolveConfig -> IO [FilePath]
bundledDirs cfg = case rcBundledDir cfg of
  Just d -> pure [d]
  Nothing -> do
    dir <- getBundledIncludeDir
    pure [dir]


resolve
  :: ResolveConfig
  -> Map Text ResolvedProto
  -> [Text]
  -> FilePath
  -> IO (Either ResolveError ResolvedProto)
resolve cfg cache visiting path = do
  let pathT = T.pack path
  if pathT `elem` visiting
    then pure (Left (CircularImport (reverse (pathT : visiting))))
    else case Map.lookup pathT cache of
      Just resolved -> pure (Right resolved)
      Nothing -> do
        contents <- TIO.readFile path
        case parseProtoFile path contents of
          Left e -> pure (Left (ParseError path (renderParseError e)))
          Right pf -> do
            let imports = protoImports pf
            result <- foldM (resolveImport cfg (pathT : visiting)) (Right Map.empty) imports
            case result of
              Left e -> pure (Left e)
              Right importMap -> do
                -- For editions schemas, apply field_presence feature overrides so
                -- that EXPLICIT/IMPLICIT/LEGACY_REQUIRED are reflected in fieldLabel.
                -- This normalises the AST before it reaches any codegen path.
                let pf' = applyEditionFieldPresence pf
                    resolved =
                      ResolvedProto
                        { rpFile = pf'
                        , rpPath = path
                        , rpImports = importMap
                        }
                pure (Right resolved)


resolveImport
  :: ResolveConfig
  -> [Text]
  -> Either ResolveError (Map Text ResolvedProto)
  -> ImportDef
  -> IO (Either ResolveError (Map Text ResolvedProto))
resolveImport _ _ (Left e) _ = pure (Left e)
resolveImport cfg visiting (Right acc) imp = do
  let importPath = T.unpack (Proto.IDL.AST.importPath imp)
  found <- findFile (rcIncludeDirs cfg) importPath
  case found of
    Nothing -> pure (Left (FileNotFound importPath (Proto.IDL.AST.importPath imp) (rcIncludeDirs cfg)))
    Just fullPath -> do
      result <- resolve cfg acc visiting fullPath
      case result of
        Left e -> pure (Left e)
        Right resolved -> pure (Right (Map.insert (Proto.IDL.AST.importPath imp) resolved acc))


findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile [] _ = pure Nothing
findFile (dir : dirs) rel = do
  let full = dir </> rel
  exists <- doesFileExist full
  if exists
    then pure (Just full)
    else findFile dirs rel


-- | Resolve all imports from a proto file, returning the full import graph.
resolveProtoImports
  :: [FilePath]
  -- ^ Include directories
  -> FilePath
  -- ^ Proto file path
  -> IO (Either ResolveError ResolvedProto)
resolveProtoImports includeDirs =
  resolveProtoFile (defaultResolveConfig {rcIncludeDirs = includeDirs})
