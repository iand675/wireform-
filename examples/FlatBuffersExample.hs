-- | FlatBuffers is a schema-driven binary format. The encoder and
-- decoder both take a parsed @.fbs@ schema; the schema decides each
-- field's type, layout, and default value.
--
-- Run with: cabal run example-flatbuffers
module Main where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified FlatBuffers.Value  as FB
import qualified FlatBuffers.Parser as FBP
import qualified FlatBuffers.Encode as FBE
import qualified FlatBuffers.Decode as FBD

monsterSchema :: Text
monsterSchema = T.unlines
  [ "table Monster {"
  , "  name:string;"
  , "  hp:short = 100;"
  , "  mana:int = 150;"
  , "  inventory:[ubyte];"
  , "}"
  , "root_type Monster;"
  , "file_identifier \"MNST\";"
  ]

main :: IO ()
main = do
  schema <- case FBP.parseFlatBuffers monsterSchema of
    Right s -> pure s
    Left  e -> error ("schema parse failed: " ++ e)

  let monster = FB.VTable $ V.fromList
        [ Just (FB.VString "Goblin")
        , Just (FB.VInt16 50)
        , Nothing  -- mana defaults to 150
        , Just (FB.VVector (V.fromList
            [FB.VWord8 1, FB.VWord8 2, FB.VWord8 3]))
        ]

  case FBE.encode schema monster of
    Left  e  -> putStrLn $ "encode error: " ++ e
    Right bs -> do
      putStrLn $ "Encoded: " ++ show (BS.length bs) ++ " bytes"
      putStrLn $ "File identifier: " ++ show (BS.take 4 (BS.drop 4 bs))
      case FBD.decode schema bs of
        Right decoded -> putStrLn $ "Decoded: " ++ show decoded
        Left  err     -> putStrLn $ "decode error: " ++ err
