{-# LANGUAGE OverloadedStrings #-}

{- | Runner for the upstream CEL conformance suite
(<https://github.com/google/cel-spec> @tests/simple/testdata/*.textproto@).

This is opt-in, matching the @TOML_TEST_SUITE@ / @YAML_TEST_SUITE@ pattern
used elsewhere in the monorepo: set @CEL_SPEC_DIR@ to a checkout of
@cel-spec@ (or directly to its @tests/simple/testdata@ directory) and run
the @wireform-cel-conformance@ test suite. Without the variable the runner
prints a notice and succeeds.

The runner parses the textproto @SimpleTestFile@ messages with a small
self-contained parser, decodes the expected @cel.expr.Value@ results and
variable bindings, evaluates each expression with this library, and reports
pass / skip / fail counts per file.

The runner lists files for unsupported extension libraries, checker tests,
and unknown tracking separately. Within included files, cases that require
unsupported features are counted as /skipped/. A checkout supplies protobuf
schemas for constructing and decoding its message fixtures.
-}
module Main (main) where

import CEL
import CEL.Value (celFieldPresent, equalUnknownFields, selectCelField)
import Control.Monad (forM, when)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isAlphaNum, isHexDigit)
import Data.List (intercalate, isSuffixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word8)
import FixtureProto
import GHC.Float (double2Float, float2Double)
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Any qualified as Any
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropTrailingPathSeparator, takeDirectory, (</>))
import Text.Read (readMaybe)


----------------------------------------------------------------------
-- Textproto AST + parser
----------------------------------------------------------------------

data Tok
  = TLBrace
  | TRBrace
  | TLBrack
  | TRBrack
  | TLAngle
  | TRAngle
  | TColon
  | TComma
  | TStr [Word8]
  | TAtom String
  deriving stock (Show, Eq)


tokenize :: String -> [Tok]
tokenize [] = []
tokenize (c : cs)
  | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' = tokenize cs
  | c == '#' = tokenize (dropWhile (/= '\n') cs)
  | c == '{' = TLBrace : tokenize cs
  | c == '}' = TRBrace : tokenize cs
  | c == '[' = TLBrack : tokenize cs
  | c == ']' = TRBrack : tokenize cs
  | c == '<' = TLAngle : tokenize cs
  | c == '>' = TRAngle : tokenize cs
  | c == ':' = TColon : tokenize cs
  | c == ',' = TComma : tokenize cs
  | c == '"' || c == '\'' = let (bytes, rest) = scanStr c cs in TStr bytes : tokenize rest
  | isAtomChar c = let (a, rest) = span isAtomChar (c : cs) in TAtom a : tokenize rest
  | otherwise = tokenize cs


isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c || c `elem` ("_+-./" :: String)


scanStr :: Char -> String -> ([Word8], String)
scanStr q = go []
  where
    go acc [] = (reverse acc, [])
    go acc (c : cs)
      | c == q = (reverse acc, cs)
      | c == '\\' = case esc cs of (bs, rest) -> go (reverse bs ++ acc) rest
      | otherwise = go (reverse (utf8 c) ++ acc) cs

    esc [] = ([], [])
    esc (e : rest) = case e of
      'a' -> ([0x07], rest)
      'b' -> ([0x08], rest)
      'f' -> ([0x0C], rest)
      'n' -> ([0x0A], rest)
      'r' -> ([0x0D], rest)
      't' -> ([0x09], rest)
      'v' -> ([0x0B], rest)
      '\\' -> ([0x5C], rest)
      '\'' -> ([0x27], rest)
      '"' -> ([0x22], rest)
      '?' -> ([0x3F], rest)
      '`' -> ([0x60], rest)
      'x' -> hexN 2 rest
      'X' -> hexN 2 rest
      'u' -> uni 4 rest
      'U' -> uni 8 rest
      _
        | e >= '0' && e <= '7' -> octal (e : rest)
        | otherwise -> (utf8 e, rest)

    hexN n s =
      let ds = takeWhile isHexDigit (take n s)
          consumed = length ds
      in ([fromIntegral (foldl' (\a d -> a * 16 + digitToInt d) 0 ds)], drop consumed s)
    octal s =
      let (ds, _) = splitAt 3 s
          octs = takeWhile (\d -> d >= '0' && d <= '7') (take 3 ds)
      in ([fromIntegral (foldl' (\a d -> a * 8 + digitToInt d) 0 octs)], drop (length octs) s)
    uni n s =
      let (ds, r) = splitAt n s
      in if length ds == n && all isHexDigit ds
           then (utf8 (chr (foldl' (\a d -> a * 16 + digitToInt d) 0 ds)), r)
           else (utf8 '?', s)


utf8 :: Char -> [Word8]
utf8 = BS.unpack . TE.encodeUtf8 . T.singleton


-- Parse a sequence of fields up to a closing brace/angle (or end of input).
parseFields :: [Tok] -> (Fields, [Tok])
parseFields = go []
  where
    go acc toks = case toks of
      [] -> (reverse acc, [])
      (TRBrace : rest) -> (reverse acc, rest)
      (TRAngle : rest) -> (reverse acc, rest)
      (TAtom name : rest) -> field name rest acc
      (TLBrack : TAtom name : TRBrack : rest) -> field name rest acc
      _ -> go acc (drop 1 toks) -- skip stray tokens defensively
    field name toks acc = case toks of
      (TColon : TLBrace : rest) -> sub name rest acc
      (TColon : TLAngle : rest) -> sub name rest acc
      (TColon : TLBrack : rest) -> bracketList name rest acc
      (TColon : rest) -> scalar name rest acc
      (TLBrace : rest) -> sub name rest acc
      (TLAngle : rest) -> sub name rest acc
      _ -> go acc toks

    sub name rest acc =
      let (fs, rest') = parseFields rest
      in go ((name, PMsg fs) : acc) rest'

    scalar name rest acc = case rest of
      (TStr bs : more) ->
        let (bs', more') = mergeStrs bs more
        in go ((name, PStr bs') : acc) more'
      (TAtom a : more) -> go ((name, PAtom a) : acc) more
      _ -> go acc rest

    bracketList name toks acc = case toks of
      (TRBrack : rest) -> go acc rest
      _ ->
        let (item, rest) = parseItem toks
            acc' = (name, item) : acc
        in case rest of
             (TComma : rest') -> bracketList name rest' acc'
             (TRBrack : rest') -> go acc' rest'
             _ -> go acc' rest

    parseItem toks = case toks of
      (TLBrace : rest) -> let (fs, r) = parseFields rest in (PMsg fs, r)
      (TLAngle : rest) -> let (fs, r) = parseFields rest in (PMsg fs, r)
      (TStr bs : more) -> let (bs', m) = mergeStrs bs more in (PStr bs', m)
      (TAtom a : more) -> (PAtom a, more)
      _ -> (PAtom "", drop 1 toks)

    mergeStrs bs (TStr more : rest) = mergeStrs (bs ++ more) rest
    mergeStrs bs rest = (bs, rest)


parseFile :: String -> Either String Fields
parseFile input = case parseFields (tokenize input) of
  (fields, []) -> Right fields
  _ -> Left "unexpected closing delimiter in textproto fixture"


----------------------------------------------------------------------
-- Field helpers
----------------------------------------------------------------------

field1 :: String -> Fields -> Maybe PtVal
field1 n fs = lookup n fs


fieldAll :: String -> Fields -> [PtVal]
fieldAll n fs = [v | (k, v) <- fs, k == n]


asText :: PtVal -> Maybe Text
asText (PStr bs) = Just (TE.decodeUtf8With lenient (BS.pack bs))
  where
    lenient _ _ = Just '\xFFFD'
asText (PAtom a) = Just (T.pack a)
asText _ = Nothing


asMsg :: PtVal -> Maybe Fields
asMsg (PMsg fs) = Just fs
asMsg _ = Nothing


----------------------------------------------------------------------
-- Decoding expected values / bindings
----------------------------------------------------------------------

-- Decode a cel.expr.Value message. 'Left' is a skip reason.
decodeValue :: Registry -> Fields -> Either String Value
decodeValue registry fs = case mapMaybe tryField fs of
  (v : _) -> v
  [] -> Left "empty Value message"
  where
    tryField (n, v) = case n of
      "null_value" -> Just (Right VNull)
      "bool_value" -> Just (decAtom v (\a -> VBool (a == "true")))
      "int64_value" -> Just (decInt v (VInt . fromInteger))
      "uint64_value" -> Just (decInt v (VUInt . fromInteger))
      "double_value" -> Just (decDouble v)
      "string_value" -> Just (decStr v VString)
      "bytes_value" -> Just (decBytes v)
      "type_value" -> Just (decType v)
      "enum_value" -> Just (decEnum v)
      "list_value" -> Just (decList v)
      "map_value" -> Just (decMap v)
      "object_value" -> Just (decObject v)
      _ -> Nothing

    decAtom (PAtom a) f = Right (f a)
    decAtom _ _ = Left "expected atom"

    decInt (PAtom a) f = case reads a of
      [(n, "")] -> Right (f (n :: Integer))
      _ -> Left ("bad integer: " <> a)
    decInt _ _ = Left "expected integer atom"

    decDouble (PAtom a) = case a of
      "inf" -> Right (VDouble (1 / 0))
      "-inf" -> Right (VDouble (-1 / 0))
      "nan" -> Right (VDouble (0 / 0))
      _ -> case reads a of
        [(d, "")] -> Right (VDouble d)
        _ -> Left ("bad double: " <> a)
    decDouble _ = Left "expected double atom"

    decStr (PStr bs) f = Right (f (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') (BS.pack bs)))
    decStr _ _ = Left "expected string"

    decBytes (PStr bs) = Right (VBytes (BS.pack bs))
    decBytes _ = Left "expected bytes"

    decObject (PMsg [(url, value)])
      | Just name <- T.stripPrefix "type.googleapis.com/" (T.pack url) = objectValue name value
    decObject _ = Left "object_value (protobuf message) unsupported"

    objectValue "google.protobuf.Duration" (PMsg fields) =
      VDuration <$> (Duration <$> number "seconds" fields <*> number "nanos" fields)
    objectValue "google.protobuf.Timestamp" (PMsg fields) =
      VTimestamp <$> (Timestamp <$> number "seconds" fields <*> number "nanos" fields)
    objectValue "google.protobuf.Value" value = decJsonValue value
    objectValue "google.protobuf.ListValue" value = decJsonList value
    objectValue "google.protobuf.Struct" value = decJsonStruct value
    objectValue "google.protobuf.Empty" (PMsg []) =
      Right (VMessage (CelMessage "google.protobuf.Empty" mempty))
    objectValue "google.protobuf.Any" value@(PMsg fields)
      | Just (PStr url) <- field1 "type_url" fields
      , Just (PStr payload) <- field1 "value" fields = do
          typeUrl <- first show (TE.decodeUtf8' (BS.pack url))
          first (T.unpack . errMsg) (resolveAny registry typeUrl (BS.pack payload))
      | otherwise = decObject value
    objectValue name (PMsg fields)
      | Just (kind, def) <- wrapperValueKind name = case fields of
          [] -> Right def
          [("value", value)] -> do
            decoded <- decodeValue registry [(kind, value)]
            Right $ case decoded of
              VDouble n | name == "google.protobuf.FloatValue" -> VDouble (float2Double (double2Float n))
              _ -> decoded
          _ -> Left "invalid wrapper field"
      | otherwise = decodeMessageFields registry objectValue name fields
    objectValue _ _ = Left "invalid protobuf object"

    decJsonValue (PMsg []) = Right VNull
    decJsonValue (PMsg [(kind, value)]) = case kind of
      "null_value" -> Right VNull
      "number_value" -> decDouble value
      "string_value" -> decStr value VString
      "bool_value" -> decAtom value (\a -> VBool (a == "true"))
      "list_value" -> decJsonList value
      "struct_value" -> decJsonStruct value
      _ -> Left "unsupported google.protobuf.Value kind"
    decJsonValue _ = Left "invalid google.protobuf.Value"

    decJsonList (PMsg fields) = VList . V.fromList <$> traverse decJsonValue (fieldAll "values" fields)
    decJsonList _ = Left "invalid google.protobuf.ListValue"

    decJsonStruct (PMsg fields) = do
      entries <- traverse decJsonEntry (fieldAll "fields" fields)
      buildMap entries
    decJsonStruct _ = Left "invalid google.protobuf.Struct"

    decJsonEntry (PMsg fields) = do
      key <- maybe (Right (VString "")) (\v -> decStr v VString) (field1 "key" fields)
      value <- maybe (Right VNull) decJsonValue (field1 "value" fields)
      Right (key, value)
    decJsonEntry _ = Left "invalid google.protobuf.Struct map entry"

    number :: (Num a, Read a) => String -> Fields -> Either String a
    number name fields = case field1 name fields of
      Nothing -> Right 0
      Just (PAtom atom) | Just n <- readMaybe atom -> Right n
      _ -> Left ("invalid object field " <> name)

    decType v = case asText v of
      Just t -> Right (VType (typeFromName t))
      Nothing -> Left "expected type name"

    decEnum (PMsg sub) = case field1 "value" sub of
      Just (PAtom a) | [(n, "")] <- reads a -> Right (VInt (fromInteger (n :: Integer)))
      Nothing -> Right (VInt 0) -- unset enum value defaults to 0
      _ -> Left "bad enum value"
    decEnum _ = Left "expected enum message"

    decList (PMsg sub) =
      let items = fieldAll "values" sub
      in case mapM (\pv -> asMsg pv >>= Just . decodeValue registry) items of
           Just rs -> VList . V.fromList <$> sequence rs
           Nothing -> Left "bad list element"
    decList _ = Left "expected list message"

    decMap (PMsg sub) =
      let entries = fieldAll "entries" sub
      in traverse decEntry entries >>= buildMap
    decMap _ = Left "expected map message"

    buildMap = either (Left . T.unpack) (Right . VMap) . celMap

    decEntry (PMsg e) = do
      kF <- maybe (Left "entry without key") Right (field1 "key" e >>= asMsg)
      vF <- maybe (Left "entry without value") Right (field1 "value" e >>= asMsg)
      k <- decodeValue registry kF
      val <- decodeValue registry vF
      Right (k, val)
    decEntry _ = Left "bad map entry"


wrapperValueKind :: Text -> Maybe (String, Value)
wrapperValueKind = \case
  "google.protobuf.BoolValue" -> Just ("bool_value", VBool False)
  "google.protobuf.BytesValue" -> Just ("bytes_value", VBytes BS.empty)
  "google.protobuf.StringValue" -> Just ("string_value", VString "")
  "google.protobuf.Int32Value" -> Just ("int64_value", VInt 0)
  "google.protobuf.Int64Value" -> Just ("int64_value", VInt 0)
  "google.protobuf.UInt32Value" -> Just ("uint64_value", VUInt 0)
  "google.protobuf.UInt64Value" -> Just ("uint64_value", VUInt 0)
  "google.protobuf.FloatValue" -> Just ("double_value", VDouble 0)
  "google.protobuf.DoubleValue" -> Just ("double_value", VDouble 0)
  _ -> Nothing


typeFromName :: Text -> CelType
typeFromName t = case t of
  "bool" -> TyBool
  "int" -> TyInt
  "uint" -> TyUInt
  "double" -> TyDouble
  "string" -> TyString
  "bytes" -> TyBytes
  "list" -> TyList
  "map" -> TyMap
  "null_type" -> TyNull
  "type" -> TyType
  "google.protobuf.Timestamp" -> TyTimestamp
  "google.protobuf.Duration" -> TyDuration
  _ -> TyMessage t


-- A binding's value is an ExprValue { value | error | unknown }.
decodeBinding :: Registry -> Fields -> Either String (Text, Value)
decodeBinding registry fs = do
  keyV <- maybe (Left "binding without key") Right (field1 "key" fs)
  key <- maybe (Left "binding key not text") Right (asText keyV)
  exprValue <- maybe (Left "binding without value") Right (field1 "value" fs >>= asMsg)
  case field1 "value" exprValue >>= asMsg of
    Just valueMsg -> do
      v <- decodeValue registry valueMsg
      Right (key, v)
    Nothing -> Left "binding is not a plain value (error/unknown)"


----------------------------------------------------------------------
-- Running a single test
----------------------------------------------------------------------

data Outcome = Pass | Skip String | Fail String


runTest :: Registry -> Env -> Fields -> Outcome
runTest registry baseEnv t
  | Just (PAtom "true") <- field1 "check_only" t = Skip "checker-only test"
  | any (isJust . flip field1 t) ["typed_result", "any_eval_errors", "unknown", "any_unknowns"] =
      Skip "unsupported result matcher"
  | otherwise = case field1 "expr" t >>= asText of
    Nothing -> Skip "no expr"
    Just expr ->
      let container = maybe "" id (field1 "container" t >>= asText)
          bindingResults = map (decodeBinding registry) (mapMaybe asMsg (fieldAll "bindings" t))
      in case sequence bindingResults of
           Left reason -> Skip reason
           Right binds ->
             let env = withContainer container (bindAll binds baseEnv)
                 result = run env expr
                 hasError = not (null (fieldAll "eval_error" t))
             in case (field1 "value" t, hasError) of
                  (Just valMsg, _) -> case asMsg valMsg of
                    Nothing -> Skip "value not a message"
                    Just vm -> case decodeValue registry vm of
                      Left reason -> Skip ("expected value: " <> reason)
                      Right expected -> expectedValueOutcome expr expected result
                  (Nothing, True) -> expectedErrorOutcome expr result
                  (Nothing, False) -> expectedValueOutcome expr (VBool True) result


expectedValueOutcome :: Text -> Value -> Either CelError Value -> Outcome
expectedValueOutcome expr expected = \case
  Right got
    | structEq got expected -> Pass
    | otherwise -> Fail (description <> show got <> expression)
  Left e
    | errKind e == ErrUnsupported -> Skip ("unsupported: " <> T.unpack (errMsg e))
    | otherwise -> Fail (description <> "error " <> show e <> expression)
  where
    description = "want " <> show expected <> ", got "
    expression = "  [" <> T.unpack expr <> "]"


expectedErrorOutcome :: Text -> Either CelError Value -> Outcome
expectedErrorOutcome expr = \case
  Left e
    | errKind e == ErrUnsupported -> Skip ("unsupported: " <> T.unpack (errMsg e))
    | otherwise -> Pass
  Right got -> Fail ("want error, got " <> show got <> "  [" <> T.unpack expr <> "]")


structEq :: Value -> Value -> Bool
structEq a@VAny {} b = either (const False) (\value -> structEq value b) (unpackValue a)
structEq a b@VAny {} = either (const False) (structEq a) (unpackValue b)
structEq a b = case (a, b) of
  (VNull, VNull) -> True
  (VBool x, VBool y) -> x == y
  (VInt x, VInt y) -> x == y
  (VUInt x, VUInt y) -> x == y
  (VDouble x, VDouble y) -> (isNaN x && isNaN y) || x == y
  (VString x, VString y) -> x == y
  (VBytes x, VBytes y) -> x == y
  (VType x, VType y) -> typeNameText x == typeNameText y
  (VDuration x, VDuration y) -> x == y
  (VTimestamp x, VTimestamp y) -> x == y
  (VList x, VList y) ->
    V.length x == V.length y && and (zipWith structEq (V.toList x) (V.toList y))
  (VMap x, VMap y) ->
    let ex = celMapEntries x
        ey = celMapEntries y
    in length ex == length ey
         && all (\(k, v) -> maybe False (structEq v) (celMapLookup k y)) ex
  (VMessage left@(CelMessage name x), VMessage right@(CelMessage other y)) ->
    let present field = if celFieldPresent field then Just (selectCelField field) else Nothing
        sameSelection (Right expected) (Right actual) = structEq expected actual
        sameSelection _ _ = False
        px = Map.mapMaybe present x
        py = Map.mapMaybe present y
    in name == other && equalUnknownFields (celMessageUnknownFields left) (celMessageUnknownFields right)
         && Map.size px == Map.size py && Map.isSubmapOfBy sameSelection px py
  _ -> False


----------------------------------------------------------------------
-- Collecting tests from a file
----------------------------------------------------------------------

collectTests :: Fields -> [Fields]
collectTests file =
  let sections = mapMaybe asMsg (fieldAll "section" file)
      sectionTests = concatMap (\s -> mapMaybe asMsg (fieldAll "test" s)) sections
      topTests = mapMaybe asMsg (fieldAll "test" file)
  in topTests ++ sectionTests


-- Files that exercise CEL extension libraries or checker-only behavior that
-- are out of scope for the core language definition this package targets.
excludedFiles :: [String]
excludedFiles =
  [ "bindings_ext.textproto"
  , "block_ext.textproto"
  , "encoders_ext.textproto"
  , "math_ext.textproto"
  , "lists_ext.textproto"
  , "network_ext.textproto"
  , "sets_ext.textproto"
  , "string_ext.textproto"
  , "proto2_ext.textproto"
  , "optionals.textproto"
  , "type_deduction.textproto"
  , "unknowns.textproto"
  ]


----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main = do
  verifyUnsupportedErrorIsSkipped
  verifyImplicitMatcher
  mDir <- lookupEnv "CEL_SPEC_DIR"
  case mDir of
    Nothing -> do
      putStrLn "CEL_SPEC_DIR not set; skipping upstream conformance suite."
      putStrLn "  Set CEL_SPEC_DIR to a cel-spec checkout to run it."
      exitSuccess
    Just dir -> do
      testdata <- resolveTestdata dir
      let root = takeDirectory (takeDirectory (takeDirectory (dropTrailingPathSeparator testdata)))
      loaded <- loadRegistry root
      registry <- case loaded of
        Just value -> do
          verifyAnyWireValues value
          verifyClosedEnumWireValues value
          verifyRequiredWireValues value
          pure value
        Nothing -> do
          putStrLn "Protobuf fixture schemas unavailable. Set CEL_SPEC_DIR to a full cel-spec checkout."
          pure emptyRegistry
      let baseEnv = fixtureEnv registry
      entries <- listDirectory testdata
      let fixtures = sortOn id (filter (".textproto" `isSuffixOf`) entries)
          isExcluded f = f `elem` excludedFiles || (f == "enums.textproto" && not (isJust loaded))
          excluded = filter isExcluded fixtures
          files = filter (not . isExcluded) fixtures
      when (not (null excluded)) $
        putStrLn ("Excluded fixture files: " <> intercalate ", " excluded)
      perFile <- forM files $ \f -> do
        contents <- readFile (testdata </> f)
        fields <- either (fail . ((f <> ": ") <>)) pure (parseFile contents)
        let tests = collectTests fields
            outcomes = map (runTest registry baseEnv) tests
            p = length [() | Pass <- outcomes]
            skips = [r | Skip r <- outcomes]
            fs = [r | Fail r <- outcomes]
        putStrLn $
          padRight 26 f
            <> "  pass="
            <> padLeft 4 (show p)
            <> "  skip="
            <> padLeft 4 (show (length skips))
            <> "  fail="
            <> padLeft 4 (show (length fs))
        mapM_ (\r -> putStrLn ("    FAIL: " <> r)) (take 12 fs)
        when (length fs > 12) $
          putStrLn ("    ... and " <> show (length fs - 12) <> " more failures")
        pure (p, skips, fs)
      let totP = sum [a | (a, _, _) <- perFile]
          allSkips = concat [b | (_, b, _) <- perFile]
          totF = sum [length c | (_, _, c) <- perFile]
      putStrLn "----------------------------------------------------------------"
      putStrLn "Skip reasons (feature not implemented by this library):"
      mapM_
        (\(reason, n) -> putStrLn ("  " <> padLeft 4 (show n) <> "  " <> reason))
        (tally (map skipCategory allSkips))
      putStrLn "----------------------------------------------------------------"
      putStrLn $
        "TOTAL  pass=" <> show totP <> "  skip=" <> show (length allSkips) <> "  fail=" <> show totF
      if totF == 0 then exitSuccess else exitFailure


-- An unsupported message constructor cannot verify the expected range error.
verifyUnsupportedErrorIsSkipped :: IO ()
verifyUnsupportedErrorIsSkipped =
  case expectedErrorOutcome "duration fixture" (Left (CelError ErrUnsupported "constructor unavailable")) of
    Skip _ -> pure ()
    _ -> fail "unsupported expected-error case must be reported as skipped"


verifyImplicitMatcher :: IO ()
verifyImplicitMatcher = do
  positive <- fixture "expr: 'true'"
  negative <- fixture "expr: 'false'"
  checker <- fixture "expr: 'false' check_only: true"
  unknown <- fixture "expr: 'true' unknown {}"
  case (positive, negative, checker, unknown) of
    (Pass, Fail _, Skip _, Skip _) -> pure ()
    _ -> fail "only an absent result matcher must imply boolean true"
  where
    fixture = either fail (pure . runTest emptyRegistry emptyEnv) . parseFile


verifyAnyWireValues :: Registry -> IO ()
verifyAnyWireValues registry = do
  let url = "type.googleapis.com/cel.expr.conformance.proto3.TestAllTypes"
      wire = "\xb8\x3e\x81\x00"
      pack name payload =
        let bytes = encodeMessage (Any.defaultAny {Any.anyTypeUrl = "type.googleapis.com/" <> name, Any.anyValue = payload})
        in BS.pack [0xa2, 0x06, fromIntegral (BS.length bytes)] <> bytes
      comparePayloads name x y expected = do
        left <- either (fail . show) pure (resolveAny registry url (pack name x))
        right <- either (fail . show) pure (resolveAny registry url (pack name y))
        let env = bindAll [("left", left), ("right", right)] (fixtureEnv registry)
        check env ("left " <> expected <> " right && left.single_any == right.single_any")
      check env source = case run env source of
        Right (VBool True) -> pure ()
        result -> fail (T.unpack source <> " returned " <> show result)
  case resolveAny registry url wire of
    Right (VMessage value) | celMessageUnknownFields value == wire -> pure ()
    _ -> fail "unpacking an Any must preserve unknown wire bytes"
  comparePayloads "google.protobuf.Int32Value" "\x08\x01" ("\x08\x01" <> wire) "!="
  comparePayloads "google.protobuf.Int32Value" "" "\x08\x00" "=="
  comparePayloads "google.protobuf.ListValue" "\x0a\x00" "\x0a\x02\x08\x00" "!="
  let env = withContainer "cel.expr.conformance.proto3" (fixtureEnv registry)
  packedList <- either (fail . show) pure $
    resolveAny registry url (pack "google.protobuf.ListValue" "\x0a\x09\x11\x00\x00\x00\x00\x00\x00\xf0\x3f")
  check (bind "packedList" packedList env) "packedList == TestAllTypes{single_any: [1]}"
  check env "type(TestAllTypes{single_any: [1]}.single_any[0]) == double"
  check env "TestAllTypes{single_any: {'data': b'foo', 'at': timestamp(1)}}.single_any == {'data': 'Zm9v', 'at': '1970-01-01T00:00:01Z'}"


verifyClosedEnumWireValues :: Registry -> IO ()
verifyClosedEnumWireValues registry = do
  let check syntax input unknown expression = do
        let url = "type.googleapis.com/cel.expr.conformance." <> syntax <> ".TestAllTypes"
        result <- either (fail . show) pure (resolveAny registry url input)
        case result of
          VMessage message | celMessageUnknownFields message == unknown -> pure ()
          _ -> fail ("unexpected unknown fields for " <> show result)
        case run (bind "m" result emptyEnv) expression of
          Right (VBool True) -> pure ()
          actual -> fail (T.unpack expression <> " returned " <> show actual)
      -- Field 24 is a closed enum in proto2 and an open enum in proto3.
      singular = "\xc0\x01\x63"
      oneof = "\xb0\x01\x63"
      packed = "\xa2\x03\x04\x00\x63\x01\x62"
      expanded = "\xa0\x03\x02"
      repeatedUnknown = "\xa0\x03\x63\xa0\x03\x62"
      validEntry = "\xc2\x04\x04\x08\x01\x10\x01"
      invalidEntry = "\xc2\x04\x04\x08\x01\x10\x63"
      knownLast = "\xc2\x04\x06\x08\x01\x10\x63\x10\x02"
      unknownLast = "\xc2\x04\x06\x08\x01\x10\x01\x10\x63"
  check "proto2" singular singular "!has(m.standalone_enum) && m.standalone_enum == 0"
  check "proto3" singular "" "has(m.standalone_enum) && m.standalone_enum == 99"
  check "proto2" ("\xaa\x01\x00" <> oneof) oneof
    "has(m.single_nested_message) && !has(m.single_nested_enum) && m.single_nested_enum == 1"
  check "proto2" (packed <> expanded) repeatedUnknown "m.repeated_nested_enum == [0, 1, 2]"
  check "proto2" (validEntry <> invalidEntry <> knownLast <> unknownLast) (invalidEntry <> unknownLast)
    "m.map_bool_enum == {true: 2}"
  -- NullValue is declared in proto3, even in the proto2 fixture.
  check "proto2" "\x98\x07\x63" "" "has(m.null_value) && m.null_value == null"


verifyRequiredWireValues :: Registry -> IO ()
verifyRequiredWireValues registry = do
  let url = "type.googleapis.com/cel.expr.conformance.proto2.TestRequired"
      env = withContainer "cel.expr.conformance.proto2" (fixtureEnv registry)
      rejected bytes = case resolveAny registry url bytes of
        Left err | errKind err == ErrConversion -> pure ()
        result -> fail ("expected missing required protobuf field, got " <> show result)
  rejected ""
  rejected "\x0a\x00"
  present <- either (fail . show) pure (resolveAny registry url "\x08\x00")
  case run (bind "m" present env) "has(m.required_int32) && m.required_int32 == 0 && !has(TestRequired{}.required_int32)" of
    Right (VBool True) -> pure ()
    result -> fail ("unexpected required field presence: " <> show result)


-- Group a skip reason into a coarse category (everything up to the first ':').
skipCategory :: String -> String
skipCategory = takeWhile (/= ':')


tally :: [String] -> [(String, Int)]
tally xs = sortOn (negate . snd) (foldr bump [] xs)
  where
    bump x acc = case lookup x acc of
      Just _ -> map (\(k, n) -> if k == x then (k, n + 1) else (k, n)) acc
      Nothing -> (x, 1) : acc


resolveTestdata :: FilePath -> IO FilePath
resolveTestdata dir = do
  let nested = dir </> "tests" </> "simple" </> "testdata"
  haveNested <- doesDirectoryExist nested
  pure (if haveNested then nested else dir)


padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '


padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s
