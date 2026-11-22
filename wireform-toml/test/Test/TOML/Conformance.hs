{-# LANGUAGE OverloadedStrings #-}

{- | Optional conformance harness against
<https://github.com/toml-lang/toml-test the official TOML test suite>.

The toml-test suite ships hundreds of fixture cases of the form:

@
  tests/valid/\<group\>/\<name\>.toml    -- the input
  tests/valid/\<group\>/\<name\>.json    -- expected typed-JSON output
  tests/invalid/\<group\>/\<name\>.toml  -- input that must fail to parse
@

The companion @.json@ file uses a typed schema where every scalar
is wrapped in a @{ \"type\": ..., \"value\": ... }@ object so that
TOML's type information (especially @date-local@ vs @datetime@,
@integer@ vs @float@, etc.) is preserved.

This module exposes a single test tree that:

1. Walks @TOML_TEST_SUITE@ (or the @TOML_TEST_DIR@ env var) when
   set; otherwise reports a no-op skip group so CI stays green
   out-of-the-box.
2. For each @valid/@ case, runs 'TOML.Decode.decode' and structurally
   compares the result against the typed JSON.
3. For each @invalid/@ case, requires 'TOML.Decode.decode' to
   return 'Left' (or to throw any synchronous exception).

A mini built-in suite (drawn from the TOML 1.0 spec examples)
always runs so that core compliance is exercised even without
the external suite.
-}
module Test.TOML.Conformance (tests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath (
  dropExtension,
  takeDirectory,
  takeExtension,
  (</>),
 )
import TOML.Decode qualified as TD
import TOML.Value qualified as TV
import Test.Syd


-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

tests :: IO Spec
tests = do
  builtin <- builtinSuite
  ext <- externalSuite
  pure (describe "conformance" $ sequence_ [builtin, ext])


-- ---------------------------------------------------------------------------
-- Built-in mini-suite
-- ---------------------------------------------------------------------------

builtinSuite :: IO Spec
builtinSuite =
  pure $
    describe "builtin" $
      sequence_
        [ ok
            "spec key/value"
            "title = \"TOML Example\"\n"
            ( \v -> case lookupKey "title" v of
                Just (TV.TString t) -> t `shouldBe` T.pack "TOML Example"
                _ -> expectationFailure "expected title string"
            )
        , ok
            "spec table"
            ( T.unlines
                [ "[server]"
                , "host = \"localhost\""
                , "port = 8080"
                ]
            )
            ( \v -> case lookupKey "server" v of
                Just s -> do
                  case lookupKey "host" s of
                    Just (TV.TString t) -> t `shouldBe` T.pack "localhost"
                    _ -> expectationFailure "host"
                  case lookupKey "port" s of
                    Just (TV.TInteger n) -> n `shouldBe` 8080
                    _ -> expectationFailure "port"
                _ -> expectationFailure "expected server table"
            )
        , ok
            "spec inline table"
            "point = { x = 1, y = 2 }\n"
            ( \v -> case lookupKey "point" v of
                Just _ -> pure ()
                _ -> expectationFailure "expected point"
            )
        , ok
            "spec array of tables"
            ( T.unlines
                [ "[[products]]"
                , "name = \"Hammer\""
                , ""
                , "[[products]]"
                , "name = \"Nail\""
                ]
            )
            (\_ -> pure ())
        , it "rejects time zone suffixes on local times" $
            mapM_
              (\src -> TD.decode src `shouldSatisfy` isLeft)
              [ "time = 17:45:00Z"
              , "time = 17:45:00+02:00"
              , "time = 17:45z"
              , "time = 17:45-02:00"
              , "time = 17:45:00.125Z"
              ]
        , ok
            "accepts local times and dated offsets"
            (T.unlines ["local = 17:45", "fraction = 17:45:00.125", "offset = 1979-05-27T17:45:00.125+02:00", "utc = 1979-05-27T17:45Z"])
            (\v -> do
                lookupKey "local" v `shouldBe` Just (TV.TTime "17:45")
                lookupKey "fraction" v `shouldBe` Just (TV.TTime "17:45:00.125")
                lookupKey "offset" v `shouldBe` Just (TV.TDateTime "1979-05-27T17:45:00.125+02:00")
                lookupKey "utc" v `shouldBe` Just (TV.TDateTime "1979-05-27T17:45Z")
            )
        ]
  where
    ok name src k = it name $ case TD.decode src of
      Left e -> expectationFailure ("decode failed: " ++ e)
      Right v -> k v


lookupKey :: T.Text -> TV.Value -> Maybe TV.Value
lookupKey nm v = case v of
  TV.TTable kvs -> goV 0
    where
      !len = V.length kvs
      goV !i
        | i >= len = Nothing
        | otherwise = case kvs V.! i of
            (k, val)
              | k == nm -> Just val
              | otherwise -> goV (i + 1)
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- External suite (toml-test)
-- ---------------------------------------------------------------------------

externalSuite :: IO Spec
externalSuite = do
  mDir <- discoverSuiteDir
  case mDir of
    Nothing -> pure (describe "toml-test (skipped, set TOML_TEST_SUITE)" $ sequence_ [])
    Just dir -> do
      -- The official suite ships per-version manifest files
      -- @files-toml-1.0.0@ / @files-toml-1.1.0@. We default to the
      -- 1.1 manifest (the latest the decoder targets) so that
      -- version-only differences (e.g. \\xNN in basic strings,
      -- seconds-less times) don't show up as false-rejects.
      -- Override with @TOML_TEST_VERSION=1.0.0@ to test strict 1.0.
      ver <- fromMaybe "1.1.0" <$> lookupEnv "TOML_TEST_VERSION"
      let manifest = takeDirectory dir </> "tests" </> ("files-toml-" ++ ver)
      manifestExists <- doesFileExist manifest
      let manifest' =
            if manifestExists
              then manifest
              else dir </> ("files-toml-" ++ ver)
      m2Exists <- doesFileExist manifest'
      paths <-
        if m2Exists
          then readManifest dir manifest'
          else (++) <$> discoverValid dir <*> discoverInvalid dir
      let (validCases, invalidCases) =
            partitionPaths dir paths
          validCount = length validCases
          invalidCount = length invalidCases
      pure
        $ describe
          ( "toml-test ("
              ++ show validCount
              ++ " valid, "
              ++ show invalidCount
              ++ " invalid"
              ++ (if m2Exists then ", v" ++ ver else "")
              ++ ")"
          )
        $ sequence_
          [ describe "valid" (mapM_ mkValidCase validCases)
          , describe "invalid" (mapM_ mkInvalidCase invalidCases)
          ]


{- | Read a manifest file produced by toml-test. Each line is a
relative path under the suite root; we keep only the @.toml@
entries (the manifest also lists the companion @.json@ /
@.event@ fixtures).
-}
readManifest :: FilePath -> FilePath -> IO [FilePath]
readManifest root manifest = do
  bs <- BS.readFile manifest
  let txt = TE.decodeUtf8Lenient bs
      lns = filter (not . T.null) (T.lines txt)
      paths = map (\l -> root </> T.unpack (T.strip l)) lns
      tomls = filter (\p -> takeExtension p == ".toml") paths
  pure tomls


partitionPaths :: FilePath -> [FilePath] -> ([FilePath], [FilePath])
partitionPaths _root = goP ([], [])
  where
    goP (vs, is) [] = (reverse vs, reverse is)
    goP (vs, is) (p : rest)
      | "valid" `isPart` p = goP (p : vs, is) rest
      | "invalid" `isPart` p = goP (vs, p : is) rest
      | otherwise = goP (vs, is) rest
    isPart needle p = needle `elem` splitDirs p
    splitDirs = words . map (\c -> if c == '/' then ' ' else c)


{- | Look at @TOML_TEST_SUITE@ first, falling back to @TOML_TEST_DIR@.
The canonical layout has the actual cases under @\<root\>/tests/@,
so accept either pointer.
-}
discoverSuiteDir :: IO (Maybe FilePath)
discoverSuiteDir = do
  v1 <- lookupEnv "TOML_TEST_SUITE"
  v2 <- lookupEnv "TOML_TEST_DIR"
  let candidates = case (v1, v2) of
        (Just a, Just b) -> [a, b]
        (Just a, Nothing) -> [a]
        (Nothing, Just b) -> [b]
        (Nothing, Nothing) -> []
  pickExisting candidates
  where
    pickExisting [] = pure Nothing
    pickExisting (p : rest) = do
      ok <- doesDirectoryExist p
      if not ok
        then pickExisting rest
        else do
          -- If the user pointed at the repo root, descend into tests/
          subOk <- doesDirectoryExist (p </> "tests")
          pure (Just (if subOk then p </> "tests" else p))


discoverValid :: FilePath -> IO [FilePath]
discoverValid root = do
  let d = root </> "valid"
  exists <- doesDirectoryExist d
  if not exists
    then pure []
    else collectToml d


discoverInvalid :: FilePath -> IO [FilePath]
discoverInvalid root = do
  let d = root </> "invalid"
  exists <- doesDirectoryExist d
  if not exists
    then pure []
    else collectToml d


-- | Recursively find all @.toml@ files under a directory.
collectToml :: FilePath -> IO [FilePath]
collectToml = walk
  where
    walk d = do
      entries <- listDirectory d
      let absEntries = map (d </>) entries
      dirs <- filterM doesDirectoryExist absEntries
      files <- filterM doesFileExist absEntries
      let here = filter hasTomlExt files
      subs <- mapM walk dirs
      pure (here ++ concat subs)
    hasTomlExt :: FilePath -> Bool
    hasTomlExt f = takeExtension f == ".toml"


-- ---------------------------------------------------------------------------
-- Per-case drivers
-- ---------------------------------------------------------------------------

mkValidCase :: FilePath -> Spec
mkValidCase tomlPath = it tomlPath $ do
  bytes <- BS.readFile tomlPath
  res <-
    try (evaluate (TD.decodeBS bytes))
      :: IO (Either SomeException (Either String TV.Value))
  case res of
    Left e -> expectationFailure ("exception: " ++ show e)
    Right (Left e) -> expectationFailure ("decode failed: " ++ e)
    Right (Right v) -> do
      let jsonPath = dropExtension tomlPath ++ ".json"
      hasExpected <- doesFileExist jsonPath
      if not hasExpected
        then pure ()
        else do
          rawJSON <- BSL.readFile jsonPath
          case A.eitherDecode rawJSON of
            Left e -> expectationFailure ("malformed expected JSON: " ++ e)
            Right expected -> do
              let !actual = toTypedJSON v
              compareJSON actual expected


mkInvalidCase :: FilePath -> Spec
mkInvalidCase tomlPath = it tomlPath $ do
  bytes <- BS.readFile tomlPath
  res <-
    try (evaluate (TD.decodeBS bytes))
      :: IO (Either SomeException (Either String TV.Value))
  case res of
    Left _ -> pure () -- exception counts as fail
    Right (Left _) -> pure () -- expected
    Right (Right _) -> expectationFailure "expected parse error, got success"


{- | Compare actual decoded JSON against the expected typed JSON
from the toml-test suite. Floats / integers / datetime values are
compared by parse-equality on the string payload (matching the
behaviour of the upstream Go @toml-test@ runner) rather than by
byte-for-byte string equality.
-}
compareJSON :: A.Value -> A.Value -> IO ()
compareJSON a b
  | jsonEq a b = pure ()
  | otherwise = a `shouldBe` b


jsonEq :: A.Value -> A.Value -> Bool
jsonEq a b
  | Just (ta, va) <- typedScalar a
  , Just (tb, vb) <- typedScalar b
  , ta == tb =
      typedEq ta va vb
jsonEq (A.Object oA) (A.Object oB)
  | AKM.size oA /= AKM.size oB = False
  | otherwise = all matchKey (AKM.toList oA)
  where
    matchKey (k, va) = case AKM.lookup k oB of
      Just vb -> jsonEq va vb
      Nothing -> False
jsonEq (A.Array xs) (A.Array ys)
  | V.length xs /= V.length ys = False
  | otherwise = all (uncurry jsonEq) (zip (V.toList xs) (V.toList ys))
jsonEq a b = a == b


typedScalar :: A.Value -> Maybe (T.Text, T.Text)
typedScalar (A.Object o) = case ( AKM.lookup (AK.fromText (T.pack "type")) o
                                , AKM.lookup (AK.fromText (T.pack "value")) o
                                ) of
  (Just (A.String t), Just (A.String v)) -> Just (t, v)
  _ -> Nothing
typedScalar _ = Nothing


-- | Type-aware equality on scalar payloads.
typedEq :: T.Text -> T.Text -> T.Text -> Bool
typedEq ty va vb = case T.unpack ty of
  "float" -> case (parseFloat va, parseFloat vb) of
    (Just a, Just b) -> floatBitsEq a b
    _ -> va == vb
  "integer" -> case (reads (T.unpack va), reads (T.unpack vb)) of
    ([(a :: Integer, "")], [(b, "")]) -> a == b
    _ -> va == vb
  "datetime"
    | normaliseDT va == normaliseDT vb -> True
    | otherwise -> va == vb
  "datetime-local"
    | normaliseDT va == normaliseDT vb -> True
    | otherwise -> va == vb
  "time-local"
    | normaliseTime va == normaliseTime vb -> True
    | otherwise -> va == vb
  _ -> va == vb
  where
    normaliseTime t
      | T.length t == 5 = t <> T.pack ":00"
      | otherwise = t

    parseFloat t = case T.toLower t of
      "inf" -> Just (1 / 0 :: Double)
      "+inf" -> Just (1 / 0)
      "-inf" -> Just (-1 / 0)
      "nan" -> Just (0 / 0)
      "+nan" -> Just (0 / 0)
      "-nan" -> Just (negate (0 / 0))
      _ -> case reads (T.unpack t) :: [(Double, String)] of
        [(d, "")] -> Just d
        _ -> Nothing

    floatBitsEq a b
      | isNaN a && isNaN b = True
      | otherwise = a == b

    normaliseDT s =
      -- Canonicalise: T separator, Z (uppercase) for UTC, pad
      -- missing seconds with @:00@, and strip trailing zeros from
      -- fractional seconds so that @.6@ and @.600@ compare equal.
      let s1 = case T.splitAt 10 s of
            (d, rest) -> case T.uncons rest of
              Just (c, r) | c == ' ' || c == 't' -> d <> T.cons 'T' r
              _ -> s
          s2 = case T.unsnoc s1 of
            Just (rest, 'z') -> T.snoc rest 'Z'
            _ -> s1
          s3 = padDTSeconds s2
      in stripTrailingZerosInFrac s3

    -- Pad @YYYY-MM-DDTHH:MM(±|Z|.|<eol>)@ → insert @:00@.
    padDTSeconds t
      | T.length t < 16 = t
      | T.index t 13 /= ':' = t
      | not (digitsAt t 14 16) = t
      | T.length t == 16 = T.take 16 t <> T.pack ":00"
      | T.length t > 16
      , let nxt = T.index t 16
      , nxt == 'Z' || nxt == '+' || nxt == '-' =
          T.take 16 t <> T.pack ":00" <> T.drop 16 t
      | otherwise = t

    digitsAt t a b =
      and
        ( map
            ( \i ->
                i < T.length t
                  && let c = T.index t i
                     in c >= '0' && c <= '9'
            )
            [a .. b - 1]
        )

    stripTrailingZerosInFrac t = case T.breakOn (T.pack ".") t of
      (a, dotRest)
        | T.null dotRest -> t
        | otherwise ->
            let (frac, after) =
                  T.span
                    (\c -> c >= '0' && c <= '9')
                    (T.drop 1 dotRest)
                frac' = T.dropWhileEnd (== '0') frac
            in if T.null frac'
                 then a <> after
                 else a <> T.pack "." <> frac' <> after


-- ---------------------------------------------------------------------------
-- TOML.Value -> toml-test typed JSON
-- ---------------------------------------------------------------------------

{- | Convert a 'TV.Value' to the typed-JSON representation used by
the official toml-test suite. Scalars become
@{\"type\": ..., \"value\": ...}@; arrays become JSON arrays;
tables become JSON objects.
-}
toTypedJSON :: TV.Value -> A.Value
toTypedJSON = \case
  TV.TString t -> wrap "string" (A.String t)
  TV.TInteger n -> wrap "integer" (A.String (T.pack (show n)))
  TV.TBool b -> wrap "bool" (A.String (if b then "true" else "false"))
  TV.TFloat d -> wrap "float" (A.String (renderFloat d))
  TV.TDateTime t -> wrap (classifyDT t) (A.String (canonDT t))
  TV.TDate t -> wrap "date-local" (A.String t)
  TV.TTime t -> wrap "time-local" (A.String t)
  TV.TArray xs -> A.Array (V.map toTypedJSON xs)
  TV.TTable kvs ->
    A.Object (AKM.fromList (V.toList (V.map mkPair kvs)))
  where
    mkPair (k, val) = (AK.fromText k, toTypedJSON val)

    wrap :: T.Text -> A.Value -> A.Value
    wrap ty v =
      A.Object $
        AKM.fromList
          [ (AK.fromText "type", A.String ty)
          , (AK.fromText "value", v)
          ]


{- | The toml-test suite canonicalises floats: special values are
emitted as @inf@ / @-inf@ / @nan@; integer-valued floats are
printed without a fractional part (so @1.0@ becomes @1@); other
values use the default Haskell 'Double' rendering, with one
adjustment: a scientific exponent is rendered as @5e+22@ to
match the toml-test reference (Haskell's @show@ produces
@5.0e22@).
-}
renderFloat :: Double -> T.Text
renderFloat d
  | isNaN d = T.pack "nan"
  | isInfinite d && d > 0 = T.pack "inf"
  | isInfinite d = T.pack "-inf"
  | d == 0 =
      if isNegativeZero d
        then T.pack "-0"
        else T.pack "0"
  | fromIntegral (truncate d :: Integer) == d
      && abs d < 1e16 =
      T.pack (show (truncate d :: Integer))
  | otherwise = canonicalExp (T.pack (show d))


{- | Rewrite Haskell's @5.0e22@ as @5e+22@ to match the toml-test
canonical form. A bare integer mantissa keeps no trailing
@.0@; a non-negative exponent is prefixed with @+@.
-}
canonicalExp :: T.Text -> T.Text
canonicalExp t = case T.breakOn (T.pack "e") t of
  (mant, expPart)
    | T.null expPart -> t
    | otherwise ->
        let mant' = case T.unsnoc mant of
              Just (_, '0')
                | T.isSuffixOf (T.pack ".0") mant ->
                    T.dropEnd 2 mant
              _ -> mant
            expBody = T.drop 1 expPart
            expSigned = case T.uncons expBody of
              Just ('+', _) -> expBody
              Just ('-', _) -> expBody
              _ -> T.cons '+' expBody
        in mant' <> T.pack "e" <> expSigned


{- | The decoder returns 'TDateTime' as the source text. Distinguish
offset-bearing vs. local-only based on a trailing @Z@ / @+@ / @-@
in the time portion.
-}
classifyDT :: T.Text -> T.Text
classifyDT t
  | hasOffset t = T.pack "datetime"
  | otherwise = T.pack "datetime-local"
  where
    hasOffset s
      | T.length s < 11 = False
      | otherwise =
          let timepart = T.drop 11 s
              ends c =
                c == 'Z'
                  || c == 'z'
                  || c == '+'
                  || c == '-'
          in T.any ends timepart


{- | Canonicalise a TOML datetime to the toml-test reference form
(@YYYY-MM-DDTHH:MM:SS[.fff][Z|±HH:MM]@): replace a space
separator with @T@, uppercase a trailing @z@, and pad missing
seconds with @:00@.
-}
canonDT :: T.Text -> T.Text
canonDT t0 =
  let t1 = padSeconds (replaceSep (uppercaseZ t0))
  in t1
  where
    replaceSep t = case T.splitAt 10 t of
      (date, rest) -> case T.uncons rest of
        Just (' ', r) -> date <> T.cons 'T' r
        Just ('t', r) -> date <> T.cons 'T' r
        _ -> t

    uppercaseZ t = case T.unsnoc t of
      Just (rest, 'z') -> T.snoc rest 'Z'
      _ -> t

    -- @YYYY-MM-DDTHH:MM(±|Z|.|<eol>)@ → insert @:00@ between MM and
    -- the trailing portion.
    padSeconds t
      | T.length t < 16 = t
      | T.index t 13 /= ':' = t
      | not (hasMinSecShape t) = t
      | T.length t == 16 = T.take 16 t <> T.pack ":00"
      | T.length t > 16
      , let nxt = T.index t 16
      , nxt == 'Z' || nxt == '+' || nxt == '-' =
          T.take 16 t <> T.pack ":00" <> T.drop 16 t
      | otherwise = t
      where
        hasMinSecShape s =
          isDigitC (T.index s 11)
            && isDigitC (T.index s 12)
            && isDigitC (T.index s 14)
            && isDigitC (T.index s 15)
        isDigitC c = c >= '0' && c <= '9'
