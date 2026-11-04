{-# LANGUAGE OverloadedStrings #-}

-- Opt in with WIREFORM_ION_REFERENCE pointing at the binary built from
-- interop/ion-rs. Each case verifies Haskell -> official Rust -> Haskell,
-- including an independent Rust materialization and re-encoding step.
module Test.Ion.Interop (tests) where

import Data.ByteString qualified as BS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Vector qualified as V
import Ion.Decode (decode)
import Ion.Encode (encode)
import Ion.Value qualified as I
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Syd


tests :: FilePath -> Spec
tests reference =
  describe "official ion-rs bidirectional interop" $
    mapM_ check fixtures
  where
    check (name, value) = it name $ withSystemTempDirectory "wireform-ion-interop" $ \dir -> do
      let input = dir </> "wireform.ion"
          output = dir </> "rust.ion"
      BS.writeFile input (encode value)
      (status, _, err) <- readProcessWithExitCode reference [input, output] ""
      case status of
        ExitFailure _ -> expectationFailure err
        ExitSuccess -> do
          rustBytes <- BS.readFile output
          decode rustBytes `shouldBe` Right value


fixtures :: [(String, I.Value)]
fixtures =
  [
    ( "large symbol table and multi-byte SIDs"
    , I.List
        ( V.generate
            300
            (I.Symbol . TL.toStrict . TB.toLazyText . TBI.decimal)
        )
    )
  ,
    ( "system symbols"
    , I.List
        ( V.fromList
            ( map
                I.Symbol
                ["$ion", "$ion_1_0", "$ion_symbol_table", "name", "version", "imports", "symbols", "max_id", "$ion_shared_symbol_table"]
            )
        )
    )
  , ("empty and Unicode local symbols", I.List (V.fromList [I.Symbol "", I.Symbol "雪", I.Symbol "é", I.Symbol "雪"]))
  ,
    ( "fields, annotations and repeated symbols"
    , I.Annotation
        "event"
        ( I.Annotation
            "audit"
            ( I.Struct
                ( V.fromList
                    [ ("kind", I.Symbol "created")
                    , ("unicode", I.Symbol "雪")
                    , ("duplicate", I.Symbol "created")
                    , ("duplicate", I.List (V.singleton (I.Symbol "updated")))
                    ]
                )
            )
        )
    )
  ,
    ( "integer and floating point boundaries"
    , I.List
        ( V.fromList
            [I.Int minBound, I.Int maxBound, I.Int 0, I.Int (-1), I.Float (-0.0), I.Float (1 / 0), I.Float (-1 / 0), I.Float 1.5]
        )
    )
  ,
    ( "all supported scalar and container shapes"
    , I.List
        ( V.fromList
            [ I.Null
            , I.Bool True
            , I.Bool False
            , I.String "é雪\NUL"
            , I.Blob (BS.pack [0, 128, 255])
            , I.Clob (BS.pack [255, 254])
            , I.List V.empty
            , I.Struct V.empty
            , I.Struct (V.fromList [("", I.String ""), ("name", I.Int 42)])
            ]
        )
    )
  ]
