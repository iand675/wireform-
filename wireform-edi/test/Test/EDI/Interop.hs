{-# LANGUAGE OverloadedStrings #-}

module Test.EDI.Interop (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import EDI.EDIFACT
import EDI.Value
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Syd


tests :: FilePath -> Spec
tests reference =
  describe "edifact-rs bidirectional interop" $
    mapM_ check fixtures
  where
    check (name, advice, segments) = it name $ withSystemTempDirectory "wireform-edifact-interop" $ \dir -> do
      let input = dir </> "wireform.edi"
          output = dir </> "rust.edi"
          doc = Interchange (serviceStringSyntax advice) segments
          expected = Interchange (serviceStringSyntax defaultServiceStringAdvice) segments
      encoded <- either expectationFailure pure (encodeEDIFACTBS advice doc)
      BS.writeFile input encoded
      (status, _, err) <- readProcessWithExitCode reference [input, output] ""
      case status of
        ExitFailure _ -> expectationFailure err
        ExitSuccess -> do
          rustBytes <- BS.readFile output
          decodeEDIFACTBS rustBytes `shouldBe` Right expected


fixtures :: [(String, ServiceStringAdvice, V.Vector Segment)]
fixtures =
  [
    ( "release before a structural boundary"
    , defaultServiceStringAdvice
    , V.singleton (Segment "FTX" (V.fromList [Simple "?", Simple "X"]))
    )
  ,
    ( "all structural delimiters, Unicode and trailing whitespace"
    , defaultServiceStringAdvice
    , V.singleton (Segment "FTX" (V.fromList [Simple "?:+' 雪é", Simple "last "]))
    )
  ,
    ( "composites and positional empty elements"
    , defaultServiceStringAdvice
    , V.singleton (Segment "FTX" (V.fromList [Simple "", Composite (V.fromList ["", "x:y", "?"]), Simple ""]))
    )
  ,
    ( "custom service advice is read and normalized"
    , ServiceStringAdvice ';' '|' ',' '!' (Just '*') '~'
    , V.singleton (Segment "FTX" (V.fromList [Simple "!;|~*?", Composite (V.fromList ["a", "b"])]))
    )
  ,
    ( "long release runs preserve subsequent fields"
    , defaultServiceStringAdvice
    , V.singleton (Segment "FTX" (V.fromList [Simple (T.replicate 10000 "?"), Simple "end"]))
    )
  ]
