-- | Run with: cabal run example-flatbuffers
module Main where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import FlatBuffers
import FlatBuffers.Parser (parseFlatBuffers)


main :: IO ()
main = case do
  schema <- parseFlatBuffers "table Example { name:string; count:int; values:[long]; } root_type Example;"
  let value = VTable (V.fromList [Just (VString "wireform"), Just (VInt32 42), Just (VVector (V.fromList [VInt64 10, VInt64 20]))])
  bytes <- encode schema value
  decoded <- decode schema bytes
  Right (bytes, decoded) of
  Left err -> fail err
  Right (bytes, decoded) -> do
    putStrLn ("Encoded " <> show (BS.length bytes) <> " bytes")
    print decoded
