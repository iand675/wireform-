{- | protoc plugin for wireform.

Usage: protoc --plugin=protoc-gen-wireform=./protoc-gen-wireform --wireform_out=gen/ foo.proto
-}
module Main where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.CodeGen
import Proto.Google.Protobuf.Compiler.Plugin
import Proto.Google.Protobuf.Descriptor
import Proto.IDL.AST (ImportModifier (ImportOption), importModifier)
import Proto.IDL.Descriptor (fileDescriptorToAST)
import Proto.IDL.Parser.Resolver (ResolvedProto (..), importClosure)


main :: IO ()
main = pluginMain handleRequest


handleRequest :: CodeGeneratorRequest -> IO CodeGeneratorResponse
handleRequest req = do
  let requestedFiles = V.toList (cgrFileToGenerate req)
      allProtos = V.toList (cgrProtoFile req)
      opts = parsePluginOpts (cgrParameter req)
      resolvedPairs = fmap fdpToResolved allProtos
      files = Map.fromList (map (\(path, resolved) -> (T.pack path, rpFile resolved)) resolvedPairs)
      typePaths = importClosure files ((/= Just ImportOption) . importModifier) requestedFiles
      typePairs = filter (\(path, _) -> Set.member (T.pack path) typePaths) resolvedPairs
      typeReg = buildTypeRegistry opts typePairs
      outputFiles = concatMap (generateForFile opts typeReg requestedFiles) allProtos
      conflict = moduleNameConflict opts (map (\(path, resolved) -> (path, rpFile resolved)) typePairs)
  pure $
    case conflict of
      Just message -> defaultCodeGeneratorResponse {cgrsError = message}
      Nothing ->
        defaultCodeGeneratorResponse
          { cgrsFile = V.fromList outputFiles
          , cgrsSupportedFeatures = 3
          , cgrsMinimumEdition = fromIntegral edition2023WireNumber
          , cgrsMaximumEdition = fromIntegral edition2024WireNumber
          }


parsePluginOpts :: T.Text -> GenerateOpts
parsePluginOpts _param = defaultGenerateOpts


fdpToResolved :: FileDescriptorProto -> (FilePath, ResolvedProto)
fdpToResolved fdp =
  let path = T.unpack (fdpName fdp)
      pf = fileDescriptorToAST fdp
  in ( path
     , ResolvedProto {rpFile = pf, rpPath = path, rpImports = Map.empty}
     )


generateForFile
  :: GenerateOpts
  -> TypeRegistry
  -> [T.Text]
  -> FileDescriptorProto
  -> [CodeGeneratorResponseFile]
generateForFile opts reg requestedFiles fdp =
  if fdpName fdp `elem` requestedFiles
    then
      let filePath = T.unpack (fdpName fdp)
          pf = fileDescriptorToAST fdp
          moduleName = moduleNameForProto opts filePath pf
          outputPath = T.replace "." "/" moduleName <> ".hs"
          content = generateModuleTextFromDescriptor opts reg fdp
      in [CodeGeneratorResponseFile outputPath content]
    else []
