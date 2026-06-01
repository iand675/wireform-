module Main (main) where

import System.IO (hSetBuffering, BufferMode(..), stdout, hFlush)
import Interop.SelfTest (runSelfTest)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting conformance test..."
  hFlush stdout
  runSelfTest
