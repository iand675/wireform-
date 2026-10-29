
module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.Maybe (fromMaybe)
import GHC.Conc (setUncaughtExceptionHandler)
import System.IO
import Test.Syd

import GHC.Conc.Sync (threadLabel)

import Test.Prop.Dialogue                     qualified as Dialogue
import Test.Regression.Issue102               qualified as Issue102
import Test.Regression.Issue238               qualified as Issue238
import Test.Sanity.Any                        qualified as Any
import Test.Sanity.BrokenDeployments          qualified as BrokenDeployments
import Test.Sanity.Compression                qualified as Compression
import Test.Sanity.EndOfStream                qualified as EndOfStream
import Test.Sanity.Interop                    qualified as Interop
import Test.Sanity.NoIsLabel                  qualified as NoIsLabel
import Test.Sanity.Reclamation                qualified as Reclamation
import Test.Sanity.StreamingType.CustomFormat qualified as StreamingType.CustomFormat
import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming

main :: IO ()
main = do
    setUncaughtExceptionHandler uncaughtExceptionHandler

    sydTest $ describe "grapesy" $ sequence_ [
        describe "Sanity" $ sequence_ [
            EndOfStream.tests
          , describe "StreamingType" $ sequence_ [
                StreamingType.NonStreaming.tests
              , StreamingType.CustomFormat.tests
              ]
          , Compression.tests
          , Any.tests
          , Interop.tests
          , Reclamation.tests
          , BrokenDeployments.tests
          , NoIsLabel.tests
          ]
      , describe "Regression" $ sequence_ [
            Issue102.tests
          , Issue238.tests
          ]
      , describe "Prop" $ sequence_ [
            Dialogue.tests
          ]
      ]

uncaughtExceptionHandler :: SomeException -> IO ()
uncaughtExceptionHandler e = do
    tid    <- myThreadId
    mLabel :: Maybe String <- threadLabel tid
    hPutStrLn stderr $ concat [
         "Uncaught exception in "
      , show tid
      , " ("
      , fromMaybe "unlabelled" mLabel
      , "): "
      , displayException e
      ]
