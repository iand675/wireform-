{- | Hand-written companion to 'Proto.Google.Protobuf.Compiler.Plugin'.

The @Plugin.hs@ module itself is *codegen output* from the upstream
@google\/protobuf\/compiler\/plugin.proto@ — pure data types plus
wire / JSON codecs, no IO. The @protoc@ side of the protocol (read
'CodeGeneratorRequest' off stdin, hand to a handler, write
'CodeGeneratorResponse' to stdout) is a hand-written wrapper because
it depends on @System.IO@ and isn't expressible in proto IDL. It
lives here, in the @Util@ sibling, so a regen pass over the @.proto@
file never clobbers it.
-}
module Proto.Google.Protobuf.Compiler.Plugin.Util
  ( -- * Plugin entry point
    pluginMain

    -- * Convenience constructors
  , makeErrorResponse
  , makeFileResponse
  , responseFile
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.IO (hSetBinaryMode, stderr, stdin, stdout)
import System.IO qualified as IO

import Proto (decodeMessage)
import Proto (encodeMessage)
import Proto.Google.Protobuf.Compiler.Plugin
  ( CodeGeneratorRequest
  , CodeGeneratorResponse
  , CodeGeneratorResponse'File
  , defaultCodeGeneratorResponse
  , defaultCodeGeneratorResponse'File
  )
import Proto.Google.Protobuf.Compiler.Plugin qualified as P


{- | Entry point for a @protoc@ plugin.

Reads a 'CodeGeneratorRequest' from @stdin@, hands it to the
supplied handler, and writes the resulting 'CodeGeneratorResponse'
back out to @stdout@. The handler runs in @IO@ so it can do
filesystem lookups, fetch external resources, etc., but the
top-level wire framing is exactly what @protoc@ expects from any
plugin executable.

If decoding the request fails — or the handler itself throws — the
exception is converted into a @CodeGeneratorResponse{ error = ... }@
on the wire. @protoc@'s convention is that plugin executables exit
with status 0 even when reporting a generation error; the error
message goes through the @error@ field. Genuine plumbing failures
(stdin closed, stdout broken pipe) propagate normally and end the
process.
-}
pluginMain :: (CodeGeneratorRequest -> IO CodeGeneratorResponse) -> IO ()
pluginMain handler = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  input <- BS.hGetContents stdin
  case decodeMessage input of
    Left err -> writeOut (makeErrorResponse (T.pack ("protoc-gen-wireform: decode error: " <> show err)))
    Right req -> do
      result <- try @SomeException (handler req)
      case result of
        Left exn -> writeOut (makeErrorResponse (T.pack ("protoc-gen-wireform: handler raised: " <> show exn)))
        Right resp -> writeOut resp
  where
    writeOut resp = BS.hPut stdout (encodeMessage resp)


-- | A 'CodeGeneratorResponse' carrying just an error message.
makeErrorResponse :: Text -> CodeGeneratorResponse
makeErrorResponse msg =
  defaultCodeGeneratorResponse {P.codeGeneratorResponseError = Just msg}


-- | A 'CodeGeneratorResponse' carrying the given output files.
makeFileResponse :: [CodeGeneratorResponse'File] -> CodeGeneratorResponse
makeFileResponse fs =
  defaultCodeGeneratorResponse {P.codeGeneratorResponseFile = V.fromList fs}


-- | Build a 'CodeGeneratorResponse'File' from a path and contents.
responseFile :: Text -> Text -> CodeGeneratorResponse'File
responseFile path contents =
  defaultCodeGeneratorResponse'File
    { P.codeGeneratorResponseFileName = Just path
    , P.codeGeneratorResponseFileContent = Just contents
    }
