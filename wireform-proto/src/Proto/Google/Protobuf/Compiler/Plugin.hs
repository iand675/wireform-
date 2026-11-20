{-# LANGUAGE BangPatterns #-}
-- | Haskell types for @google/protobuf/compiler/plugin.proto@.
--
-- These types define the interface between protoc and code generator plugins.
-- A plugin reads a 'CodeGeneratorRequest' from stdin and writes a
-- 'CodeGeneratorResponse' to stdout.
module Proto.Google.Protobuf.Compiler.Plugin
  ( -- * Request
    CodeGeneratorRequest (..)
  , defaultCodeGeneratorRequest

    -- * Response
  , CodeGeneratorResponse (..)
  , defaultCodeGeneratorResponse
  , CodeGeneratorResponseFile (..)
  , defaultCodeGeneratorResponseFile

    -- * Plugin entry point
  , pluginMain
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import System.IO (stdin, stdout, hSetBinaryMode)

import Proto.Encode
import Proto.Decode
import Proto.Internal.Wire (Tag(..))
import Proto.Google.Protobuf.Descriptor (FileDescriptorProto)

data CodeGeneratorRequest = CodeGeneratorRequest
  { cgrFileToGenerate    :: !(V.Vector Text)
  , cgrParameter         :: !Text
  , cgrProtoFile         :: !(V.Vector FileDescriptorProto)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultCodeGeneratorRequest :: CodeGeneratorRequest
defaultCodeGeneratorRequest = CodeGeneratorRequest V.empty "" V.empty

instance MessageEncode CodeGeneratorRequest where
  buildMessage cgr =
    V.foldl' (\a f -> a <> encodeFieldString 1 f) mempty (cgrFileToGenerate cgr) <>
    (if cgrParameter cgr == "" then mempty else encodeFieldString 2 (cgrParameter cgr)) <>
    V.foldl' (\a p -> a <> encodeFieldMessage 15 p) mempty (cgrProtoFile cgr)

instance MessageDecode CodeGeneratorRequest where
  messageDecoder = loop defaultCodeGeneratorRequest
    where
      loop !r = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure r
          UJust (Tag 1 _)  -> do v <- decodeFieldString; loop r { cgrFileToGenerate = V.snoc (cgrFileToGenerate r) v }
          UJust (Tag 2 _)  -> do v <- decodeFieldString; loop r { cgrParameter = v }
          UJust (Tag 15 _) -> do v <- decodeFieldMessage; loop r { cgrProtoFile = V.snoc (cgrProtoFile r) v }
          UJust (Tag _ wt) -> skipField wt >> loop r

data CodeGeneratorResponse = CodeGeneratorResponse
  { cgrsError   :: !Text
  , cgrsFile    :: !(V.Vector CodeGeneratorResponseFile)
  , cgrsSupportedFeatures :: !Int
  , cgrsMinimumEdition :: !Int32
  , cgrsMaximumEdition :: !Int32
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultCodeGeneratorResponse :: CodeGeneratorResponse
defaultCodeGeneratorResponse = CodeGeneratorResponse "" V.empty 0 0 0

instance MessageEncode CodeGeneratorResponse where
  buildMessage cgr =
    (if cgrsError cgr == "" then mempty else encodeFieldString 1 (cgrsError cgr)) <>
    (if cgrsSupportedFeatures cgr == 0 then mempty
     else encodeFieldVarint 2 (fromIntegral (cgrsSupportedFeatures cgr))) <>
    (if cgrsMinimumEdition cgr == 0 then mempty else encodeFieldVarint 3 (fromIntegral (cgrsMinimumEdition cgr))) <>
    (if cgrsMaximumEdition cgr == 0 then mempty else encodeFieldVarint 4 (fromIntegral (cgrsMaximumEdition cgr))) <>
    V.foldl' (\a f -> a <> encodeFieldMessage 15 f) mempty (cgrsFile cgr)

instance MessageDecode CodeGeneratorResponse where
  messageDecoder = loop defaultCodeGeneratorResponse
    where
      loop !r = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure r
          UJust (Tag 1 _)  -> do v <- decodeFieldString; loop r { cgrsError = v }
          UJust (Tag 2 _)  -> do v <- getVarint; loop r { cgrsSupportedFeatures = fromIntegral v }
          UJust (Tag 3 _)  -> do v <- getVarint; loop r { cgrsMinimumEdition = fromIntegral v }
          UJust (Tag 4 _)  -> do v <- getVarint; loop r { cgrsMaximumEdition = fromIntegral v }
          UJust (Tag 15 _) -> do v <- decodeFieldMessage; loop r { cgrsFile = V.snoc (cgrsFile r) v }
          UJust (Tag _ wt) -> skipField wt >> loop r

data CodeGeneratorResponseFile = CodeGeneratorResponseFile
  { cgrfName    :: !Text
  , cgrfContent :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultCodeGeneratorResponseFile :: CodeGeneratorResponseFile
defaultCodeGeneratorResponseFile = CodeGeneratorResponseFile "" ""

instance MessageEncode CodeGeneratorResponseFile where
  buildMessage f =
    (if cgrfName f == "" then mempty else encodeFieldString 1 (cgrfName f)) <>
    (if cgrfContent f == "" then mempty else encodeFieldString 15 (cgrfContent f))

instance MessageDecode CodeGeneratorResponseFile where
  messageDecoder = loop defaultCodeGeneratorResponseFile
    where
      loop !f = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure f
          UJust (Tag 1 _)  -> do v <- decodeFieldString; loop f { cgrfName = v }
          UJust (Tag 15 _) -> do v <- decodeFieldString; loop f { cgrfContent = v }
          UJust (Tag _ wt) -> skipField wt >> loop f

-- | Entry point for a protoc plugin.
-- Reads a CodeGeneratorRequest from stdin, applies the handler,
-- and writes the CodeGeneratorResponse to stdout.
pluginMain :: (CodeGeneratorRequest -> IO CodeGeneratorResponse) -> IO ()
pluginMain handler = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  input <- BS.hGetContents stdin
  case decodeMessage input of
    Left err -> do
      let resp = defaultCodeGeneratorResponse { cgrsError = T.pack (show err) }
      BS.hPut stdout (encodeMessage resp)
    Right req -> do
      resp <- handler req
      BS.hPut stdout (encodeMessage resp)
