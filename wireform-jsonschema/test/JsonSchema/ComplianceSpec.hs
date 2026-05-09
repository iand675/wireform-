{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module JsonSchema.ComplianceSpec (spec) where

import Test.Hspec
import JsonSchema
import JsonSchema.Validator (validateValueWithRegistry)
import JsonSchema.ReferenceLoader (ReferenceLoader, fileLoaderWithVersion, noOpLoader, makeLoader, registerScheme, registerDefaultLoader, emptyLoaderRegistry)
import JsonSchema.EmbeddedMetaschemas (standardMetaschemaLoader)
import JsonSchema.Types (buildRegistryWithExternalRefs, Schema(..), SchemaCore(..), splitUriFragment)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, FromJSON(..), (.:), (.=), eitherDecodeFileStrict, object)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory)
import System.FilePath ((</>), takeExtension, makeRelative, takeFileName, isAbsolute)
import Data.Maybe (Maybe(..), catMaybes)
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import System.IO (hPutStrLn, stderr)
import System.Environment (lookupEnv)

-- | Test suite file format
data TestSuiteFile = TestSuiteFile [TestGroup]
  deriving (Eq, Show, Generic)

instance FromJSON TestSuiteFile where
  parseJSON v = TestSuiteFile <$> parseJSON v

-- | A group of related tests
data TestGroup = TestGroup
  { groupDescription :: Text
  , groupSchema :: Value
  , groupTests :: [TestCase]
  }
  deriving (Eq, Show, Generic)

instance FromJSON TestGroup where
  parseJSON = Aeson.withObject "TestGroup" $ \o -> TestGroup
    <$> o .: "description"
    <*> o .: "schema"
    <*> o .: "tests"

-- | Individual test case
data TestCase = TestCase
  { testDescription :: Text
  , testData :: Value
  , testValid :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON TestCase where
  parseJSON = Aeson.withObject "TestCase" $ \o -> TestCase
    <$> o .: "description"
    <*> o .: "data"
    <*> o .: "valid"

-- | Reference loader for test suite remote schemas  
-- Uses the new registry-based approach for cleaner URI routing
testSuiteLoader :: JsonSchemaVersion -> ReferenceLoader
testSuiteLoader version =
  let registry = registerDefaultLoader (relativePath version)
               $ registerScheme "http" (localhostLoader version)
               $ registerScheme "https" metaschemaLoader
               $ emptyLoaderRegistry
  in makeLoader registry

-- | Loader for http://localhost:1234/... URIs (maps to local test suite files)
localhostLoader :: JsonSchemaVersion -> ReferenceLoader
localhostLoader version uri
  | T.isPrefixOf "http://example.com/" uri =
      -- Skip intentionally fake example URIs gracefully - return a minimal schema
      pure $ Right $ Schema
        { schemaVersion = Just version
        , schemaMetaschemaURI = Nothing
        , schemaId = Just uri
        , schemaCore = BooleanSchema True  -- Always valid schema
        , schemaVocabulary = Nothing
        , schemaExtensions = Map.empty
        , schemaRawKeywords = Map.empty
        }
  | T.isPrefixOf "http://localhost:1234/" uri =
      let (path, _) = splitUriFragment $ T.drop (T.length "http://localhost:1234/") uri
      in loadFromRemotes version path
  | otherwise =
      metaschemaLoader uri  -- Check for metaschemas

-- | Load from test-suite/json-schema-test-suite/remotes/ directory
loadFromRemotes :: JsonSchemaVersion -> Text -> IO (Either Text Schema)
loadFromRemotes version uri = do
  -- Strip fragment from URI (e.g., "draft7/name.json#/definitions/orNull" -> "draft7/name.json")
  let (path, _) = splitUriFragment uri
  let versionDir = case version of
        Draft04 -> "draft4"
        Draft06 -> "draft6"
        Draft07 -> "draft7"
        Draft201909 -> "draft2019-09"
        Draft202012 -> "draft2020-12"

      -- Check if path already includes version directory or v1
      pathIncludesVersion = any (\v -> T.isPrefixOf (T.pack v <> "/") path)
        ["draft4", "draft6", "draft7", "draft2019-09", "draft2020-12", "v1"]

      -- Extract filename from path (handle subdirectories)
      filename = T.pack $ takeFileName (T.unpack path)
      
      -- Try both with and without fractal-openapi/ prefix to support running from different directories
      -- For draft4/06/07, also try v1/ directory as those use the v1 test fixtures
      commonPath1 = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> T.unpack path
      commonPath2 = T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/" <> T.unpack path
      versionPath1 = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
      versionPath2 = T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
      v1Path1 = T.pack $ "test-suite/json-schema-test-suite/remotes/v1/" <> T.unpack path
      v1Path2 = T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/v1/" <> T.unpack path
      
      -- Also try paths with subdirectories preserved (e.g., "nested/foo-ref-string.json")
      -- by trying them in version directories
      subdirVersionPath1 = if T.any (== '/') path && not pathIncludesVersion
                           then Just $ T.pack $ "test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
                           else Nothing
      subdirVersionPath2 = if T.any (== '/') path && not pathIncludesVersion
                           then Just $ T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
                           else Nothing
      subdirV1Path1 = if T.any (== '/') path && not pathIncludesVersion && (version == Draft04 || version == Draft06 || version == Draft07)
                      then Just $ T.pack $ "test-suite/json-schema-test-suite/remotes/v1/" <> T.unpack path
                      else Nothing
      subdirV1Path2 = if T.any (== '/') path && not pathIncludesVersion && (version == Draft04 || version == Draft06 || version == Draft07)
                      then Just $ T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/v1/" <> T.unpack path
                      else Nothing

  -- Try paths with fallback: version-specific first, then v1 (for older drafts), then common
  -- Include subdirectory paths if applicable
  -- If path includes version directory, try it as-is first, then fallback to version-specific
  let pathsToTry = if pathIncludesVersion
                   then let basePaths = [commonPath1, commonPath2]
                            -- Also try in version-specific directories (in case path is wrong)
                            fallbackPaths = case version of
                                Draft04 -> [versionPath1, versionPath2, v1Path1, v1Path2]
                                Draft06 -> [versionPath1, versionPath2, v1Path1, v1Path2]
                                Draft07 -> [versionPath1, versionPath2, v1Path1, v1Path2]
                                _ -> [versionPath1, versionPath2]
                        in basePaths ++ fallbackPaths
                   else let basePaths = case version of
                                Draft04 -> [versionPath1, versionPath2, v1Path1, v1Path2, commonPath1, commonPath2]
                                Draft06 -> [versionPath1, versionPath2, v1Path1, v1Path2, commonPath1, commonPath2]
                                Draft07 -> [versionPath1, versionPath2, v1Path1, v1Path2, commonPath1, commonPath2]
                                _ -> [versionPath1, versionPath2, commonPath1, commonPath2]
                            subdirPaths = catMaybes [subdirVersionPath1, subdirVersionPath2, subdirV1Path1, subdirV1Path2]
                        in basePaths ++ subdirPaths

  -- First try direct paths
  result <- tryPaths path pathsToTry pathsToTry
  case result of
    Right schema -> pure $ Right schema
    Left _ ->
      -- If direct paths failed, try searching subdirectories
      -- First try searching with the full path preserved (in case it's in a subdirectory)
      -- Then fall back to searching for just the filename
      let searchName = if T.any (== '/') path
                      then T.pack $ takeFileName (T.unpack path)
                      else path
          searchPath = if T.any (== '/') path then path else searchName
      in do
        -- Try searching with full path first
        resultWithPath <- searchSubdirectoriesWithPath version versionDir searchPath
        case resultWithPath of
          Right schema -> pure $ Right schema
          Left _ -> searchSubdirectories version versionDir searchName
  where
    tryPaths originalPath allPaths [] = do
      let pathsList = T.intercalate ", " allPaths
      pure $ Left $ "Could not load schema from any path for: " <> originalPath <> " (tried: " <> pathsList <> ")"
    tryPaths originalPath allPaths (p:ps) = do
      -- Make path absolute if it's relative
      currentDir <- getCurrentDirectory
      let absPath = if isAbsolute (T.unpack p)
                   then T.unpack p
                   else currentDir </> T.unpack p
      -- Check if file exists before trying to load
      fileExists <- doesFileExist absPath
      if fileExists
      then do
        result <- fileLoaderWithVersion version (T.pack absPath)
        case result of
          Right schema -> pure $ Right schema
          Left err -> tryPaths originalPath allPaths ps  -- Try next path on error
      else tryPaths originalPath allPaths ps
    
    -- Search for file in subdirectories with path preserved
    searchSubdirectoriesWithPath :: JsonSchemaVersion -> String -> Text -> IO (Either Text Schema)
    searchSubdirectoriesWithPath v vDir fullPath = do
      let baseDirs = ["test-suite/json-schema-test-suite/remotes"
                     , "fractal-openapi/test-suite/json-schema-test-suite/remotes"]
          versionDirs = case v of
            Draft04 -> [vDir, "v1"]
            Draft06 -> [vDir, "v1"]
            Draft07 -> [vDir, "v1"]
            _ -> [vDir]
          -- Try root directories first (where baseUriChangeFolder/ files are), then version directories
          allDirs = baseDirs ++ [baseDir </> vDir' | baseDir <- baseDirs, vDir' <- versionDirs]
      
      -- Try each directory with the full path
      tryDirsWithPath allDirs
      where
        tryDirsWithPath [] = pure $ Left $ "Could not find " <> fullPath <> " in any subdirectory"
        tryDirsWithPath (dir:dirs) = do
          exists <- doesDirectoryExist dir
          if exists
          then do
            -- Try the full path directly
            let fullFilePath = dir </> T.unpack fullPath
            -- Make path absolute
            currentDir <- getCurrentDirectory
            let absFilePath = if isAbsolute fullFilePath
                             then fullFilePath
                             else currentDir </> fullFilePath
            fileExists <- doesFileExist absFilePath
            if fileExists
            then do
              result <- fileLoaderWithVersion v (T.pack absFilePath)
              case result of
                Right schema -> pure $ Right schema
                Left _ -> tryDirsWithPath dirs
            else tryDirsWithPath dirs
          else tryDirsWithPath dirs
    
    -- Search for file in subdirectories
    searchSubdirectories :: JsonSchemaVersion -> String -> Text -> IO (Either Text Schema)
    searchSubdirectories v vDir fname = do
      let baseDirs = ["test-suite/json-schema-test-suite/remotes"
                     , "fractal-openapi/test-suite/json-schema-test-suite/remotes"]
          versionDirs = case v of
            Draft04 -> [vDir, "v1"]
            Draft06 -> [vDir, "v1"]
            Draft07 -> [vDir, "v1"]
            _ -> [vDir]
          allDirs = [baseDir </> vDir' | baseDir <- baseDirs, vDir' <- versionDirs] ++
                    baseDirs
      
      -- Try each directory
      tryDirs allDirs
      where
        tryDirs [] = pure $ Left $ "Could not find " <> fname <> " in any subdirectory"
        tryDirs (dir:dirs) = do
          exists <- doesDirectoryExist dir
          if exists
          then do
            -- Search recursively in this directory
            found <- findFileRecursive dir (T.unpack fname)
            case found of
              Just filepath -> do
                -- Make path absolute
                currentDir <- getCurrentDirectory
                let absFilePath = if isAbsolute filepath
                                 then filepath
                                 else currentDir </> filepath
                result <- fileLoaderWithVersion v (T.pack absFilePath)
                case result of
                  Right schema -> pure $ Right schema
                  Left _ -> tryDirs dirs
              Nothing -> tryDirs dirs
          else tryDirs dirs
        
        findFileRecursive :: FilePath -> FilePath -> IO (Maybe FilePath)
        findFileRecursive dir targetFile = do
          entries <- listDirectory dir
          let checkEntry entry = do
                let fullPath = dir </> entry
                isDir <- doesDirectoryExist fullPath
                if isDir
                then findFileRecursive fullPath targetFile
                else if entry == targetFile
                     then pure $ Just fullPath
                     else pure Nothing
          results <- mapM checkEntry entries
          return $ foldr (<|>) Nothing results

-- | Loader for relative paths (no scheme)
relativePath :: JsonSchemaVersion -> ReferenceLoader
relativePath version uri
  | T.isPrefixOf "#" uri = noOpLoader uri  -- Fragment only, not a file reference
  | T.isPrefixOf "http://example.com/" uri = 
      -- Skip intentionally fake example URIs gracefully - return a minimal schema
      -- so registry building doesn't fail
      pure $ Right $ Schema
        { schemaVersion = Just version
        , schemaMetaschemaURI = Nothing
        , schemaId = Just uri
        , schemaCore = BooleanSchema True  -- Always valid schema
        , schemaVocabulary = Nothing
        , schemaExtensions = Map.empty
        , schemaRawKeywords = Map.empty
        }
  | otherwise = 
      let (path, _) = splitUriFragment uri
      in loadFromRemotes version path

-- | Loader for known metaschemas
-- Now uses embedded metaschemas with fallback to stubs
metaschemaLoader :: ReferenceLoader
metaschemaLoader uri = do
  -- First try embedded metaschemas
  embeddedResult <- standardMetaschemaLoader uri
  case embeddedResult of
    Right schema -> pure $ Right schema
    Left _ ->
      -- Fall back to legacy stub metaschemas for tests
      case normalizeMetaURI uri of
        Just "http://json-schema.org/draft-04/schema" ->
          pure $ Right draft04MetaSchema
        Just "http://json-schema.org/draft-06/schema" ->
          pure $ Right draft06MetaSchema
        Just "http://json-schema.org/draft-07/schema" ->
          pure $ Right draft07MetaSchema
        Just "https://json-schema.org/draft/2020-12/schema" ->
          pure $ Right draft202012MetaSchema
        Just "http://json-schema.org/draft/2019-09/schema" ->
          pure $ Right draft201909MetaSchema
        Just "https://json-schema.org/draft/2019-09/schema" ->
          pure $ Right draft201909MetaSchema
        _ ->
          pure $ Left $ "Unknown URI: " <> uri
  where
    normalizeMetaURI u =
      let base = T.takeWhile (/= '#') u
      in if T.null base then Nothing else Just base

-- | Minimal stub meta-schemas to support remote ref tests
draft04MetaSchema :: Schema
draft04MetaSchema =
  case parseSchemaWithVersion Draft04 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft-04 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "id" .= ("http://json-schema.org/draft-04/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

draft06MetaSchema :: Schema
draft06MetaSchema =
  case parseSchemaWithVersion Draft06 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft-06 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("http://json-schema.org/draft-06/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

draft07MetaSchema :: Schema
draft07MetaSchema =
  case parseSchema metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft-07 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("http://json-schema.org/draft-07/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

draft201909MetaSchema :: Schema
draft201909MetaSchema =
  case parseSchemaWithVersion Draft201909 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft 2019-09 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("http://json-schema.org/draft/2019-09/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

draft202012MetaSchema :: Schema
draft202012MetaSchema =
  case parseSchemaWithVersion Draft202012 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft 2020-12 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("https://json-schema.org/draft/2020-12/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

-- | Run a single test case
runTestCase :: JsonSchemaVersion -> FilePath -> TestGroup -> TestCase -> Expectation
runTestCase version filePath group testCase = do
  -- Parse the schema
  case parseSchemaWithVersion version (groupSchema group) of
    Left parseErr -> expectationFailure $
      "Schema parse failed: " <> T.unpack (groupDescription group)
      <> " - " <> show parseErr
    Right schema -> do
      -- Validate the test data (without external refs for now)
      -- TODO: Enable external ref loading once base URI tracking is complete
      let loader = testSuiteLoader version
      registryResult <- buildRegistryWithExternalRefs loader schema
      case registryResult of
        Left err ->
          expectationFailure $ T.unpack $
            T.unlines
              [ "Failed to build schema registry for " <> groupDescription group
              , "Error: " <> err
              ]
        Right registry -> do
          -- Enable format assertion for optional/format tests
          -- Enable content assertion for optional/content tests
          let isFormatTest = "optional/format" `T.isInfixOf` T.pack filePath
              isContentTest = "optional/content" `T.isInfixOf` T.pack filePath
              config = defaultValidationConfig
                { validationVersion = version
                , validationFormatAssertion = isFormatTest
                , validationContentAssertion = isContentTest
                }
              result = validateValueWithRegistry config registry schema (testData testCase)
              actualValid = isSuccess result
              expectedValid = testValid testCase
          when (actualValid /= expectedValid) $ do
            expectationFailure $ T.unpack $ T.unlines
              [ "Test failed: " <> testDescription testCase
              , "Group: " <> groupDescription group
              , "Expected: " <> if expectedValid then "valid" else "invalid"
              , "Actual: " <> if actualValid then "valid" else "invalid"
              , case result of
                  ValidationFailure errs -> "Errors: " <> T.pack (show $ unErrors errs)
                  ValidationSuccess _ -> "Validated successfully"
              ]

-- | Load and run tests from a file, creating individual it blocks
runTestFile :: JsonSchemaVersion -> FilePath -> Spec
runTestFile version filePath = describe fileLabel $ do
  result <- runIO $ eitherDecodeFileStrict filePath
  case result of
    Left err -> it ("Failed to load " <> fileLabel) $
      expectationFailure $ "JSON parse error: " <> err
    Right (TestSuiteFile groups) ->
      forM_ groups $ \group ->
        describe (T.unpack $ groupDescription group) $
          forM_ (groupTests group) $ \testCase ->
            it (T.unpack $ testDescription testCase) $
              runTestCase version filePath group testCase
  where
    -- Extract just the relative path for display (works with either path prefix)
    fileLabel =
      let withoutPrefix1 = makeRelative "test-suite/json-schema-test-suite/tests" filePath
          withoutPrefix2 = makeRelative "fractal-openapi/test-suite/json-schema-test-suite/tests" filePath
      in if length withoutPrefix1 < length withoutPrefix2
         then withoutPrefix1
         else withoutPrefix2

-- | Locate the JSON Schema official test suite. Honour the
-- @JSON_SCHEMA_TEST_SUITE@ env var first (a clone of
-- <https://github.com/json-schema-org/JSON-Schema-Test-Suite>), then
-- fall back to a vendored checkout under @wireform-jsonschema/@.
--
-- Returns 'Nothing' if no test suite is available so the surrounding
-- spec can skip silently — matching @wireform-toml@'s
-- @TOML_TEST_SUITE@ / @wireform-yaml@'s @YAML_TEST_SUITE@ contracts.
findTestSuiteRoot :: IO (Maybe FilePath)
findTestSuiteRoot = do
  envOverride <- lookupEnv "JSON_SCHEMA_TEST_SUITE"
  let candidates = case envOverride of
        Nothing -> defaultPaths
        Just p ->
          [ p
          , p </> "tests"
          , p </> "json-schema-test-suite/tests"
          ] ++ defaultPaths
  findFirst candidates
  where
    defaultPaths =
      [ "test-suite/json-schema-test-suite/tests"
      , "wireform-jsonschema/test-suite/json-schema-test-suite/tests"
      ]
    findFirst [] = pure Nothing
    findFirst (p:ps) = do
      exists <- doesDirectoryExist p
      if exists then pure (Just p) else findFirst ps

-- | Test files to exclude from the suite
-- These are tests that are intentionally skipped due to language-specific behavior
-- or limitations that are documented and acceptable
excludedTests :: [String]
excludedTests =
  [ "zeroTerminatedFloats.json"  -- Aeson cannot distinguish 1.0 from 1 in JSON
  ]

collectJsonFiles :: FilePath -> IO [FilePath]
collectJsonFiles dir = do
  entries <- listDirectory dir
  fmap concat $
    forM (sort entries) $ \entry -> do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then collectJsonFiles path
        else pure [path | takeExtension path == ".json" && entry `notElem` excludedTests]

spec :: Spec
spec = do
  describe "JSON Schema Official Test Suite" $ do
    mTestSuiteRoot <- runIO findTestSuiteRoot
    case mTestSuiteRoot of
      Nothing ->
        it "is skipped (set JSON_SCHEMA_TEST_SUITE to enable)" $
          pendingWith
            "Set JSON_SCHEMA_TEST_SUITE to a checkout of \
            \https://github.com/json-schema-org/JSON-Schema-Test-Suite \
            \to enable the official compliance suite."
      Just testSuiteRoot -> do
        it "loads test suite structure" $ do
          exists <- doesFileExist (testSuiteRoot </> "draft7/type.json")
          exists `shouldBe` True

        let versionDirs =
              [ (Draft04, "draft4")
              , (Draft06, "draft6")
              , (Draft07, "draft7")
              , (Draft201909, "draft2019-09")
              , (Draft202012, "draft2020-12")
              ]

        forM_ versionDirs $ \(version, dirName) -> do
          describe ("Draft " <> show version) $ do
            files <- runIO $ collectJsonFiles (testSuiteRoot </> dirName)
            forM_ files $ \file ->
              runTestFile version file
