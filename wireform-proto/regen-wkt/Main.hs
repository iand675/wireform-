{- | Regenerate the well-known type modules from their .proto files.

Run from the wireform-proto directory:

  cabal run regen-wkt
-}
module Main where

import Data.Text.IO qualified as TIO
import Proto.CodeGen (GenerateOpts (..), buildTypeRegistry, defaultGenerateOpts, generateModuleText)
import Proto.IDL.Parser.Resolver (ResolvedProto (..), resolveProtoImports)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)


-- (proto path relative to data/proto/, disk path for proto, output hs path)
protos :: [(FilePath, FilePath, FilePath)]
protos =
  [ ("google/protobuf/timestamp.proto", "data/proto/google/protobuf/timestamp.proto", "src/Proto/Google/Protobuf/Timestamp.hs")
  , ("google/protobuf/duration.proto", "data/proto/google/protobuf/duration.proto", "src/Proto/Google/Protobuf/Duration.hs")
  , ("google/protobuf/empty.proto", "data/proto/google/protobuf/empty.proto", "src/Proto/Google/Protobuf/Empty.hs")
  , ("google/protobuf/field_mask.proto", "data/proto/google/protobuf/field_mask.proto", "src/Proto/Google/Protobuf/FieldMask.hs")
  , ("google/protobuf/source_context.proto", "data/proto/google/protobuf/source_context.proto", "src/Proto/Google/Protobuf/SourceContext.hs")
  , ("google/protobuf/any.proto", "data/proto/google/protobuf/any.proto", "src/Proto/Google/Protobuf/Any.hs")
  , ("google/protobuf/wrappers.proto", "data/proto/google/protobuf/wrappers.proto", "src/Proto/Google/Protobuf/Wrappers.hs")
  , ("google/protobuf/struct.proto", "data/proto/google/protobuf/struct.proto", "src/Proto/Google/Protobuf/Struct.hs")
  ]


opts :: GenerateOpts
opts =
  defaultGenerateOpts
    { genModulePrefix = "Proto"
    }


main :: IO ()
main = do
  mapM_
    ( \(modulePath, diskPath, hsPath) -> do
        result <- resolveProtoImports ["data/proto/", "data/", "."] diskPath
        case result of
          Left err -> error $ "Resolve error for " <> diskPath <> ": " <> show err
          Right rp -> do
            -- Use modulePath (without data/proto/ prefix) so the generated
            -- module name is Proto.Google.Protobuf.Foo, not Proto.Data.Proto.Google.Protobuf.Foo
            let registry = buildTypeRegistry opts [(modulePath, rp)]
                code = generateModuleText opts registry modulePath (rpFile rp)
            createDirectoryIfMissing True (takeDirectory hsPath)
            TIO.writeFile hsPath code
            putStrLn $ "Generated " <> hsPath
    )
    protos

  putStrLn "Done."
