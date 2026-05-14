module Test.Plugin (pluginTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Directory (findExecutable)
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Tasty
import Test.Tasty.HUnit

import Proto.CodeGen
import Proto (decodeMessage)
import Proto (encodeMessage)
import Proto.Google.Protobuf.Compiler.Plugin
import Proto.IDL.AST (ProtoFile)
import Proto.IDL.Descriptor (astToFileDescriptor, fileDescriptorToAST)
import Proto.IDL.Parser (parseProtoFile)
import Proto.IDL.Parser.Resolver (ResolvedProto (..))


testProtoSrc :: Text
testProtoSrc =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package test.plugin;"
    , ""
    , "enum Status {"
    , "  STATUS_UNSPECIFIED = 0;"
    , "  STATUS_ACTIVE = 1;"
    , "  STATUS_BANNED = 2;"
    , "}"
    , ""
    , "message Person {"
    , "  string name = 1;"
    , "  int32  age  = 2;"
    , "  Status status = 3;"
    , "  map<string,int32> scores = 4;"
    , "}"
    , ""
    , "message Team {"
    , "  string name = 1;"
    , "  repeated Person members = 2;"
    , "}"
    ]

testProtoPath :: FilePath
testProtoPath = "test/plugin/fixture.proto"


parseFixture :: IO ProtoFile
parseFixture =
  case parseProtoFile testProtoPath testProtoSrc of
    Left e -> assertFailure ("parse failed: " <> show e) >> error "unreachable"
    Right pf -> pure pf


pluginTests :: TestTree
pluginTests =
  testGroup
    "protoc-gen-wireform plugin"
    [ descriptorRoundTripTests
    , binaryPluginTest
    ]


{- | The plugin path goes:

  parse → FileDescriptorProto (astToFileDescriptor)
        → ProtoFile (fileDescriptorToAST)
        → generateModuleText

  The direct path (loadProto / Proto.Setup) goes:

  parse → generateModuleText

  These must produce identical output for the plugin to be a
  transparent substitute for the other code-generation entry points.
-}
descriptorRoundTripTests :: TestTree
descriptorRoundTripTests =
  testGroup
    "Descriptor round-trip (direct == via FileDescriptorProto)"
    [ testCase "enum and message structure preserved" $ do
        pf <- parseFixture
        let reg = buildTypeRegistry defaultGenerateOpts [(testProtoPath, ResolvedProto pf testProtoPath Map.empty)]
            directCode = generateModuleText defaultGenerateOpts reg testProtoPath pf
            fdp = astToFileDescriptor testProtoPath pf
            roundPf = fileDescriptorToAST fdp
            -- Use a registry built from the round-tripped file since its
            -- type names must match what the codegen will address.
            roundReg = buildTypeRegistry defaultGenerateOpts [(testProtoPath, ResolvedProto roundPf testProtoPath Map.empty)]
            roundCode = generateModuleText defaultGenerateOpts roundReg testProtoPath roundPf
        -- FileDescriptorProto stores messages and enums in separate lists so
        -- the top-level declaration order differs from the parsed source (all
        -- messages first, then all enums). Haskell modules are order-
        -- independent, so we compare sets of declarations rather than exact
        -- text. The key check is that the same data types and instances are
        -- present regardless of order.
        let directDecls = extractDecls directCode
            roundDecls  = extractDecls roundCode
        directDecls @?= roundDecls

    , testCase "expected declarations present" $ do
        pf <- parseFixture
        let reg = buildTypeRegistry defaultGenerateOpts [(testProtoPath, ResolvedProto pf testProtoPath Map.empty)]
            code = generateModuleText defaultGenerateOpts reg testProtoPath pf
        assertBool "data Person"  (T.isInfixOf "data Person"  code)
        assertBool "data Team"    (T.isInfixOf "data Team"    code)
        assertBool "data Status"  (T.isInfixOf "data Status"  code)
        assertBool "StatusActive" (T.isInfixOf "StatusActive" code)
        assertBool "MessageEncode instance for Person"  (T.isInfixOf "MessageEncode Person"  code)
        assertBool "MessageDecode instance for Person"  (T.isInfixOf "MessageDecode Person"  code)
        assertBool "map field (Map)" (T.isInfixOf "Map." code)

    , testCase "round-tripped code also has expected declarations" $ do
        pf <- parseFixture
        let fdp = astToFileDescriptor testProtoPath pf
            roundPf = fileDescriptorToAST fdp
            reg = buildTypeRegistry defaultGenerateOpts [(testProtoPath, ResolvedProto roundPf testProtoPath Map.empty)]
            code = generateModuleText defaultGenerateOpts reg testProtoPath roundPf
        assertBool "data Person (round-tripped)"  (T.isInfixOf "data Person"  code)
        assertBool "data Team (round-tripped)"    (T.isInfixOf "data Team"    code)
        assertBool "data Status (round-tripped)"  (T.isInfixOf "data Status"  code)
    ]


{- | End-to-end test for the 'protoc-gen-wireform' binary.

Constructs a 'CodeGeneratorRequest' from the test fixture, sends it
to the plugin binary on stdin, reads the 'CodeGeneratorResponse'
from stdout, and asserts the generated file content matches what
'generateModuleText' produces directly.

Skipped when the binary is not found in PATH. Run
@cabal build protoc-gen-wireform@ first, then add the binary to PATH
or point PATH at @$(cabal list-bin protoc-gen-wireform)@.
-}
binaryPluginTest :: TestTree
binaryPluginTest = testCase "End-to-end: binary stdin→stdout round-trip" $ do
  mBin <- findExecutable "protoc-gen-wireform"
  case mBin of
    Nothing ->
      putStrLn
        "\n  (skipped: protoc-gen-wireform not in PATH;\
        \ run 'cabal build wireform-proto:protoc-gen-wireform' then add to PATH)"
    Just binPath -> runBinaryTest binPath


runBinaryTest :: FilePath -> IO ()
runBinaryTest binPath = do
  pf <- parseFixture
  let fdp = astToFileDescriptor testProtoPath pf
      req =
        defaultCodeGeneratorRequest
          { codeGeneratorRequestFileToGenerate = V.singleton (T.pack testProtoPath)
          , codeGeneratorRequestProtoFile = V.singleton fdp
          }
      reqBytes = encodeMessage req
      reg = buildTypeRegistry defaultGenerateOpts [(testProtoPath, ResolvedProto pf testProtoPath Map.empty)]
      expected = generateModuleText defaultGenerateOpts reg testProtoPath pf

  resp <- invokePlugin binPath reqBytes

  case codeGeneratorResponseError resp of
    Just e -> assertFailure ("Plugin returned error: " <> T.unpack e)
    Nothing -> pure ()

  let files = V.toList (codeGeneratorResponseFile resp)
  case files of
    [] -> assertFailure "Plugin produced no output files"
    (f : _) -> do
      let actual = fromMaybe "" (codeGeneratorResponseFileContent f)
          expectedDecls = extractDecls expected
          actualDecls   = extractDecls actual
      when (expectedDecls /= actualDecls) $ do
        let diff = diffLines
              (T.unlines expectedDecls)
              (T.unlines actualDecls)
        assertFailure ("Plugin declarations differ from generateModuleText:\n" <> T.unpack diff)


invokePlugin :: FilePath -> ByteString -> IO CodeGeneratorResponse
invokePlugin binPath reqBytes = do
  let cp =
        (proc binPath [])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = Inherit
          }
  (Just inH, Just outH, Nothing, ph) <- createProcess cp
  hSetBinaryMode inH True
  hSetBinaryMode outH True
  -- Read stdout concurrently with writing stdin to prevent pipe buffer deadlock.
  resultVar <- newEmptyMVar
  _ <- forkIO $ do
    out <- BS.hGetContents outH
    putMVar resultVar out
  BS.hPut inH reqBytes
  hClose inH
  respBytes <- takeMVar resultVar
  _ <- waitForProcess ph
  case decodeMessage respBytes of
    Left err -> assertFailure ("Plugin response decode failed: " <> show err) >> error "unreachable"
    Right resp -> pure resp


-- | Extract the set of top-level declarations from generated Haskell source.
-- Lines starting with @data @, @newtype @, @type @, @instance @, or
-- @defaultFoo@ are collected and sorted. This normalises declaration
-- order differences while still catching missing or wrong declarations.
extractDecls :: Text -> [Text]
extractDecls src =
  let ls = T.lines src
      isDecl l =
        any (`T.isPrefixOf` l)
          ["data ", "newtype ", "type ", "instance ", "default"]
  in sort (filter isDecl ls)
  where
    sort = foldr insertSorted []
    insertSorted x [] = [x]
    insertSorted x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys


diffLines :: Text -> Text -> Text
diffLines expected actual =
  let expLines = T.lines expected
      actLines = T.lines actual
      pairs = zip3 [(1 :: Int) ..] (padTo (max (length expLines) (length actLines)) expLines) (padTo (max (length expLines) (length actLines)) actLines)
      diffs = filter (\(_, e, a) -> e /= a) pairs
      showDiff (n, e, a) = T.pack (show n) <> ":\n  expected: " <> e <> "\n  actual:   " <> a
  in if null diffs
       then "(no line differences found)"
       else T.intercalate "\n" (fmap showDiff (take 20 diffs))
            <> if length diffs > 20
                 then "\n... (" <> T.pack (show (length diffs - 20)) <> " more differing lines)"
                 else ""


padTo :: Int -> [Text] -> [Text]
padTo n xs = xs <> replicate (n - length xs) ""
