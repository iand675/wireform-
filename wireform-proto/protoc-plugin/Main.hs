{- | protoc plugin for wireform.

Usage: protoc --plugin=protoc-gen-wireform=./protoc-gen-wireform --wireform_out=gen/ foo.proto
-}
module Main where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.CodeGen
import Proto.Google.Protobuf.Compiler.Plugin
import Proto.Google.Protobuf.Compiler.Plugin.Util (pluginMain)
import Proto.Google.Protobuf.Reflection.Descriptor (FileDescriptorProto (..))
import Proto.IDL.Descriptor (fileDescriptorToAST)
import Proto.IDL.Parser.Resolver (ResolvedProto (..))


main :: IO ()
main = pluginMain handleRequest


handleRequest :: CodeGeneratorRequest -> IO CodeGeneratorResponse
handleRequest req = do
  let requestedFiles = V.toList (codeGeneratorRequestFileToGenerate req)
      allProtos = V.toList (codeGeneratorRequestProtoFile req)
      opts = parsePluginOpts (fromMaybe "" (codeGeneratorRequestParameter req))
  let resolvedPairs = fmap fdpToResolved allProtos
      typeReg = buildTypeRegistry opts resolvedPairs
      outputFiles = concatMap (generateForFile opts typeReg requestedFiles) allProtos
  pure
    defaultCodeGeneratorResponse
      { codeGeneratorResponseFile = V.fromList outputFiles
      , codeGeneratorResponseSupportedFeatures = Just 1
      }


parsePluginOpts :: T.Text -> GenerateOpts
parsePluginOpts _param = defaultGenerateOpts


fdpToResolved :: FileDescriptorProto -> (FilePath, ResolvedProto)
fdpToResolved fdp =
  let path = T.unpack (fromMaybe "" (fileDescriptorProtoName fdp))
      pf = fileDescriptorToAST fdp
  in ( path
     , ResolvedProto {rpFile = pf, rpPath = path, rpImports = Map.empty}
     )


generateForFile
  :: GenerateOpts
  -> TypeRegistry
  -> [T.Text]
  -> FileDescriptorProto
  -> [CodeGeneratorResponse'File]
generateForFile opts reg requestedFiles fdp =
  if fromMaybe "" (fileDescriptorProtoName fdp) `elem` requestedFiles
    then
      let filePath = T.unpack (fromMaybe "" (fileDescriptorProtoName fdp))
          pf = fileDescriptorToAST fdp
          moduleName = moduleNameForProto opts filePath pf
          outputPath = T.replace "." "/" moduleName <> ".hs"
          content = generateModuleText opts reg filePath pf
      in
        [ defaultCodeGeneratorResponse'File
            { codeGeneratorResponseFileName = Just outputPath
            , codeGeneratorResponseFileContent = Just content
            }
        ]
    else []
