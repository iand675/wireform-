{-# LANGUAGE OverloadedStrings #-}

{- | Optional spec-conformance harness.

The official YAML test suite (https://github.com/yaml/yaml-test-suite)
ships ~400 test cases of the form

@
  <case-id>/in.yaml         -- raw YAML input
  <case-id>/error           -- present iff the input must fail
  <case-id>/in.json         -- equivalent JSON (where applicable)
  <case-id>/===             -- one-line label
@

We run every test we can find by reading the @YAML_TEST_SUITE@
environment variable and walking the directory. When the variable
is unset, the suite is skipped and a stub success test is emitted
so CI stays green out-of-the-box.

A built-in mini-suite (the cases in @wireform-yaml/test-data/yaml@)
always runs so that core compliance is exercised even without the
external suite.
-}
module Test.YAML.Conformance (tests) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM)
import Data.Aeson qualified as A
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, (</>))
import Test.Syd
import YAML.Decode qualified as YD
import YAML.Decode.Annotated qualified as YA
import YAML.Encode qualified as YE
import YAML.JSON qualified as YJ
import YAML.Pretty qualified as YP
import YAML.Value qualified as YV


tests :: IO Spec
tests = do
  builtin <- builtinSuite
  ext <- externalSuite
  pure (describe "conformance" $ sequence_ [builtin, ext])


-- ---------------------------------------------------------------------------
-- Built-in mini-suite
-- ---------------------------------------------------------------------------

{- | A handful of cases distilled from the YAML 1.2 spec that we
never want to regress on.
-}
builtinSuite :: IO Spec
builtinSuite =
  pure $
    describe "builtin" $
      sequence_
        [ caseOK
            "spec ex 2.1 seq of strings"
            ( T.unlines
                [ "- Mark McGwire"
                , "- Sammy Sosa"
                , "- Ken Griffey"
                ]
            )
            ( \v -> case v of
                YV.YSeq xs -> V.length xs `shouldBe` 3
                _ -> expectationFailure "expected sequence"
            )
        , caseOK
            "spec ex 2.2 mapping of scalars"
            ( T.unlines
                [ "hr:  65"
                , "avg: 0.278"
                , "rbi: 147"
                ]
            )
            ( \v -> do
                YV.lookupKey "hr" v `shouldBe` Just (YV.YInt 65)
                case YV.lookupKey "rbi" v of
                  Just (YV.YInt 147) -> pure ()
                  r -> expectationFailure (show r)
            )
        , caseOK
            "spec ex 7.1 alias nodes"
            ( T.unlines
                [ "First occurrence: &anchor Foo"
                , "Second occurrence: *anchor"
                ]
            )
            ( \v -> do
                fmap YV.unwrap (YV.lookupKey "First occurrence" v)
                  `shouldBe` Just (YV.YString "Foo")
                fmap YV.unwrap (YV.lookupKey "Second occurrence" v)
                  `shouldBe` Just (YV.YString "Foo")
            )
        , caseOK
            "spec ex 8.1 block scalar header"
            ( T.unlines
                [ "literal: |"
                , "  text"
                , "folded: >"
                , "  text"
                ]
            )
            ( \v -> do
                YV.lookupKey "literal" v `shouldBe` Just (YV.YString "text\n")
                YV.lookupKey "folded" v `shouldBe` Just (YV.YString "text\n")
            )
        , caseOK
            "spec ex 5.3 block sequence"
            ( T.unlines
                [ "block sequence:"
                , "  - one"
                , "  - two : three"
                ]
            )
            ( \v -> case YV.lookupKey "block sequence" v of
                Just (YV.YSeq xs) -> V.length xs `shouldBe` 2
                _ -> expectationFailure "expected nested sequence"
            )
        , caseOK
            "flow nested in block"
            ( T.unlines
                [ "flow: { a: 1, b: 2, c: [3, 4] }"
                ]
            )
            ( \v -> case YV.lookupKey "flow" v of
                Just (YV.YMap _) -> pure ()
                r -> expectationFailure (show r)
            )
        , caseOK
            "string preserves int-like value when quoted"
            "version: \"1.0\""
            (\v -> YV.lookupKey "version" v `shouldBe` Just (YV.YString "1.0"))
        ]


caseOK :: String -> T.Text -> (YV.Value -> IO ()) -> Spec
caseOK name src k = it name $
  case YD.decode src of
    Left e -> expectationFailure $ "decode failed: " ++ e
    Right v -> k v


-- ---------------------------------------------------------------------------
-- External suite (yaml-test-suite)
-- ---------------------------------------------------------------------------

externalSuite :: IO Spec
externalSuite = do
  mDir <- lookupEnv "YAML_TEST_SUITE"
  case mDir of
    Nothing -> pure (describe "yaml-test-suite (skipped, set YAML_TEST_SUITE)" $ sequence_ [])
    Just dir -> do
      exists <- doesDirectoryExist dir
      if not exists
        then pure (it "YAML_TEST_SUITE must exist" (expectationFailure ("missing conformance path: " ++ dir) :: IO ()))
        else do
          cases <- discoverCases dir
          pure
            ( describe
                ("yaml-test-suite (" ++ show (length cases) ++ " cases)")
                (if null cases then it "suite contains cases" (expectationFailure "no in.yaml cases found; use yaml-test-suite data branch" :: IO ()) else mapM_ (mkCase dir) cases)
            )


-- | Each case-id is a path relative to the test-suite root.
discoverCases :: FilePath -> IO [FilePath]
discoverCases root = walk root
  where
    -- "tags/" and "name/" are symlink farms grouping the same
    -- cases by category / human-readable name; skip them to
    -- avoid double-counting.
    isTagDir d = case reverse (splitPath d) of
      (last_ : _) ->
        let l = dropTrailingSlash last_
        in l == "tags" || l == "name"
      [] -> False

    splitPath = words . map (\c -> if c == '/' then ' ' else c)
    dropTrailingSlash s = case reverse s of
      ('/' : rs) -> reverse rs
      _ -> s

    walk d
      | isTagDir d = pure []
      | otherwise = do
          entries <- listDirectory d
          let absEntries = map (d </>) entries
          dirs <- filterM doesDirectoryExist absEntries
          hasIn <- doesFileExist (d </> "in.yaml")
          let here = if hasIn then [d] else []
          subs <- mapM walk dirs
          pure (here ++ concat subs)


mkCase :: FilePath -> FilePath -> Spec
mkCase _root caseDir = it caseDir $ do
  let inPath = caseDir </> "in.yaml"
      errPath = caseDir </> "error"
      jsonPath = caseDir </> "in.json"
  isErr <- doesFileExist errPath
  bytes <- BS.readFile inPath
  res <-
    try (evaluate (force (YD.decodeStreamBS bytes)))
      :: IO (Either SomeException (Either String YV.Stream))
  case (isErr, res) of
    (True, Right (Left _)) -> pure ()
    (True, Right (Right _)) -> expectationFailure "expected parse error, got success"
    (True, Left e) -> expectationFailure $ "exception instead of parse error: " ++ show e
    (False, Right (Right s))
      | takeFileName caseDir `elem` ["X38W", "2JQS"] -> expectationFailure "duplicate keys must fail composition"
      | otherwise -> do
          compareToExpectedJSON jsonPath s
          checkEmittedForms bytes s
    (False, Right (Left e))
      | takeFileName caseDir `elem` ["X38W", "2JQS"] ->
          -- Event-only fixtures with duplicate collection/null keys are valid
          -- parser events, but cannot form a mapping value with unique keys.
          e `shouldBe` "YAML: duplicate mapping key"
      | otherwise -> expectationFailure $ "decode failed: " ++ e
    (False, Left e) -> expectationFailure $ "exception: " ++ show e


-- Check every valid upstream value through each public renderer as well as the
-- decoder. JSON alone cannot detect corruption of collection-valued keys.
checkEmittedForms :: BS.ByteString -> YV.Stream -> IO ()
checkEmittedForms bytes stream = do
  traverse_
    check
    [ ("encode", YE.encodeStream stream)
    , ("pretty", YP.renderStream YP.defaultOptions stream)
    , ("compact", YP.renderStream YP.compactOptions stream)
    ]
  case TE.decodeUtf8' bytes of
    Left _ -> pure ()
    Right source -> case YA.decodeAnnotatedStream source of
      Left err -> expectationFailure err
      Right (annotated, original) -> check ("normalized annotations", YP.renderAnnotatedStream (YP.defaultOptions {YP.roPreserveOriginalText = False}) annotated (Just original))
  where
    expected = representation stream
    check (label, text) = case YD.decodeStream text of
      Left err -> expectationFailure (label <> ": " <> err <> "\n" <> T.unpack text)
      Right result -> representation result `shouldBe` expected
    representation = show . V.map (strip . YV.docBody) . YV.unStream
    strip = \case
      YV.YAnchored _ inner -> strip inner
      YV.YTagged _ inner -> strip inner
      YV.YSeq xs -> YV.YSeq (strip <$> xs)
      YV.YMap entries -> YV.YMap ((\(k, v) -> (strip k, strip v)) <$> entries)
      value -> value


{- | When the case ships an @in.json@ companion, compare it against
the JSON projection of every parsed document. Numeric and string
values stay distinct, and multi-document results are checked in full.
-}
compareToExpectedJSON :: FilePath -> YV.Stream -> IO ()
compareToExpectedJSON jsonPath stream = do
  hasJson <- doesFileExist jsonPath
  if not hasJson
    then pure ()
    else do
      raw <- BSL.readFile jsonPath
      case decodeJSONStream raw of
        Left err -> expectationFailure ("invalid expected JSON: " ++ err)
        Right expected ->
          fmap (YJ.yamlToJSON . YV.docBody) (V.toList (YV.unStream stream)) `shouldBe` expected


-- The upstream data branch stores one JSON value per YAML document, including
-- pretty-printed and scalar values. Decode the whole stream, retaining types.
decodeJSONStream :: BSL.ByteString -> Either String [A.Value]
decodeJSONStream bytes
  | BSL.null rest = Right []
  | otherwise = do
      (value, remaining) <- toEitherValue (lbsToTokens rest)
      (value :) <$> decodeJSONStream remaining
  where
    rest = BSL.dropWhile (\c -> c == 32 || c == 9 || c == 10 || c == 13) bytes
