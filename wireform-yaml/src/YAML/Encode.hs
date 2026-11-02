{-# LANGUAGE BangPatterns #-}

{- | YAML 1.2 emitter.

Renders a 'YAML.Value.Value' (or a 'Document' / 'Stream') to its
block-style YAML representation, choosing flow style only for
empty containers and as a fallback. The output is /round-trippable/:
well-formed acyclic values with unique mapping keys retain their data through
'encode' and 'YAML.Decode.decode'. Decoding expands alias references; the
annotated surface retains their presentation identities for editing.

The emitter follows the YAML 1.2 core schema (so plain scalars that
happen to look like a bool / null / int / float are quoted to keep
their string identity).
-}
module YAML.Encode (
  encode,
  encodeBS,
  encodeDocument,
  encodeStream,
  safePlain,
  plainSyntaxSafe,
  buildTag,
  mustEscape,
  explicitKey,
) where

import Data.ByteString (ByteString)
import Data.Char (isControl, isDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (
  Builder,
  fromText,
  singleton,
  toLazyText,
 )
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Text.Lazy.Builder.RealFloat qualified as BF
import Data.Vector qualified as V
import YAML.Value


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Encode a single document body.
encode :: Value -> Text
encode v = TL.toStrict (toLazyText (buildDoc (Document False False v)))


encodeBS :: Value -> ByteString
encodeBS = TE.encodeUtf8 . encode


encodeDocument :: Document -> Text
encodeDocument = TL.toStrict . toLazyText . buildDoc


encodeStream :: Stream -> Text
encodeStream (Stream docs) =
  TL.toStrict (toLazyText (V.ifoldl' (\acc i d -> acc <> buildDoc (d {docDirectivesEnd = i > 0 || docDirectivesEnd d})) mempty docs))


-- ---------------------------------------------------------------------------
-- Document framing
-- ---------------------------------------------------------------------------

buildDoc :: Document -> Builder
buildDoc (Document directives explicitEnd body) =
  let !head_ = if directives then fromText "---\n" else mempty
      !body_ = buildTopLevel body
      !end_ = if explicitEnd then fromText "...\n" else mempty
  in head_ <> body_ <> end_


buildTopLevel :: Value -> Builder
buildTopLevel v = case v of
  YMap kvs
    | V.null kvs -> fromText "{}\n"
    | otherwise -> buildBlockMap 0 kvs
  YSeq xs
    | V.null xs -> fromText "[]\n"
    | otherwise -> buildBlockSeq 0 xs
  scalar -> buildScalar scalar <> singleton '\n'


-- ---------------------------------------------------------------------------
-- Block style
-- ---------------------------------------------------------------------------

buildBlockMap :: Int -> V.Vector (Value, Value) -> Builder
buildBlockMap !ind kvs = V.foldl' step mempty kvs
  where
    step !acc (k, v) = acc <> indent ind <> kvLine ind k v


kvLine :: Int -> Value -> Value -> Builder
kvLine !ind k v =
  let !keyB = buildKey ind k
      !inner = v
  in case inner of
       YMap kvs
         | V.null kvs -> keyB <> fromText ": {}\n"
         | otherwise ->
             keyB <> fromText ":\n" <> buildBlockMap (ind + 2) kvs
       YSeq xs
         | V.null xs -> keyB <> fromText ": []\n"
         | otherwise ->
             keyB <> fromText ":\n" <> buildBlockSeq ind xs
       _ ->
         keyB <> fromText ": " <> buildScalar inner <> singleton '\n'


buildBlockSeq :: Int -> V.Vector Value -> Builder
buildBlockSeq !ind xs = V.foldl' step mempty xs
  where
    step !acc x = acc <> indent ind <> seqLine ind x


seqLine :: Int -> Value -> Builder
seqLine !ind v =
  let !inner = v
  in case inner of
       YMap kvs
         | V.null kvs -> fromText "- {}\n"
         | otherwise -> fromText "- " <> firstKvLine (ind + 2) kvs
       YSeq ys
         | V.null ys -> fromText "- []\n"
         | otherwise ->
             -- Nested block sequences inside a block sequence are
             -- emitted in flow style. The compact "- - x" form is
             -- valid YAML but round-trips ambiguously through
             -- plain-scalar resolution.
             fromText "- ["
               <> commaSep (V.map buildFlow ys)
               <> fromText "]\n"
       _ ->
         fromText "- " <> buildScalar inner <> singleton '\n'


{- | Emit the first key-value pair on the same line as @-@, then the
rest at the deeper indent.
-}
firstKvLine :: Int -> V.Vector (Value, Value) -> Builder
firstKvLine !ind kvs
  | V.null kvs = mempty
  | otherwise =
      let (k, v) = V.head kvs
          rest = V.tail kvs
      in kvLine ind k v <> V.foldl' (\acc (k', v') -> acc <> indent ind <> kvLine ind k' v') mempty rest


indent :: Int -> Builder
indent !n
  | n <= 0 = mempty
  | otherwise = fromText (T.replicate n " ")


-- ---------------------------------------------------------------------------
-- Keys
-- ---------------------------------------------------------------------------

{- | Keys are rendered as scalars; complex (sequence / mapping) keys
are folded through the flow-style emitter inside an explicit-key
@?@ marker.
-}
buildKey :: Int -> Value -> Builder
buildKey ind key
  | explicitKey key = fromText "? " <> buildFlow key <> singleton '\n' <> indent ind
  | otherwise = buildScalar key


-- A conservative bound accounts for escape expansion as well as the 1024
-- character limit on an implicit key. Complex keys use the unrestricted form.
explicitKey :: Value -> Bool
explicitKey = \case
  YMap _ -> True
  YSeq _ -> True
  YTagged _ _ -> True
  YAnchored _ _ -> True
  YAlias _ -> True
  YString text -> T.length text > 100
  YInt number -> number >= 10 ^ (100 :: Int) || number <= negate (10 ^ (100 :: Int))
  _ -> False


-- ---------------------------------------------------------------------------
-- Scalar emission
-- ---------------------------------------------------------------------------

buildScalar :: Value -> Builder
buildScalar = \case
  YNull -> fromText "null"
  YBool True -> fromText "true"
  YBool False -> fromText "false"
  YInt n -> BI.decimal n
  YFloat d
    | isNaN d -> fromText ".nan"
    | isInfinite d && d > 0 -> fromText ".inf"
    | isInfinite d -> fromText "-.inf"
    | otherwise -> renderFloat d
  YString t -> renderString t
  v -> buildFlow v -- container fallthrough (shouldn't normally hit)


{- | Render a 'Double' so that decoding produces the same value and
it is unambiguously a YAML float (i.e. always contains @.@ or @e@).
-}
renderFloat :: Double -> Builder
renderFloat = BF.realFloat
{-# INLINE renderFloat #-}


-- ---------------------------------------------------------------------------
-- Flow style (used for keys + as fallback)
-- ---------------------------------------------------------------------------

buildFlow :: Value -> Builder
buildFlow v = case v of
  YTagged tag inner ->
    buildTag tag
      <> singleton ' '
      <> buildFlow inner
  YAnchored (Anchor name) inner -> singleton '&' <> fromText name <> singleton ' ' <> buildFlow inner
  YAlias (Anchor name) -> singleton '*' <> fromText name
  YMap kvs ->
    singleton '{' <> commaSep (V.map kvFlow kvs) <> singleton '}'
  YSeq xs ->
    singleton '[' <> commaSep (V.map buildFlow xs) <> singleton ']'
  scalar -> buildScalar scalar
  where
    kvFlow (k, val) = (if explicitKey k then fromText "? " else mempty) <> buildFlow k <> fromText ": " <> buildFlow val


{- | Emit a resolved tag without accidentally introducing a named handle.
Tags are URI spellings: percent escapes are retained as supplied.
-}
buildTag :: Tag -> Builder
buildTag (Tag tag)
  | tag == "!" = singleton '!'
  | Just suffix <- T.stripPrefix "tag:yaml.org,2002:" tag, shorthandSafe suffix = fromText "!!" <> fromText suffix
  | Just suffix <- T.stripPrefix "!" tag, shorthandSafe suffix = fromText tag
  | otherwise = fromText "!<" <> fromText tag <> singleton '>'
  where
    shorthandSafe suffix = not (T.null suffix) && not (T.any (`T.elem` "!,[]{}") suffix)


commaSep :: V.Vector Builder -> Builder
commaSep bs
  | V.null bs = mempty
  | otherwise = V.ifoldl' step mempty bs
  where
    step acc 0 b = acc <> b
    step acc _ b = acc <> fromText ", " <> b


-- ---------------------------------------------------------------------------
-- String quoting
-- ---------------------------------------------------------------------------

{- | Strategy: prefer plain when safe, otherwise single-quoted, only
fall back to double-quoted for strings containing characters that
must be escaped (control characters or single quotes that would
require doubling).
-}
renderString :: Text -> Builder
renderString t
  | T.null t = fromText "''"
  | needsDoubleQuote t = doubleQuoted t
  | safePlain t = fromText t
  | otherwise = singleQuoted t


doubleQuoted :: Text -> Builder
doubleQuoted t = singleton '"' <> T.foldr step (singleton '"') t
  where
    step c acc = escapeDQ c <> acc


escapeDQ :: Char -> Builder
escapeDQ c = case c of
  '"' -> fromText "\\\""
  '\\' -> fromText "\\\\"
  '\n' -> fromText "\\n"
  '\t' -> fromText "\\t"
  '\r' -> fromText "\\r"
  '\0' -> fromText "\\0"
  '\a' -> fromText "\\a"
  '\b' -> fromText "\\b"
  '\v' -> fromText "\\v"
  '\f' -> fromText "\\f"
  '\x1B' -> fromText "\\e"
  _
    | mustEscape c ->
        let cp = ord c
        in if cp <= 0xFF
             then fromText "\\x" <> hex 2 cp
             else
               if cp <= 0xFFFF
                 then fromText "\\u" <> hex 4 cp
                 else fromText "\\U" <> hex 8 cp
    | otherwise -> singleton c


hex :: Int -> Int -> Builder
hex width value =
  let digits number | number < 16 = 1 | otherwise = 1 + digits (number `quot` 16)
  in fromText (T.replicate (max 0 (width - digits value)) "0") <> BI.hexadecimal value


singleQuoted :: Text -> Builder
singleQuoted t =
  singleton '\'' <> fromText (T.replace "'" "''" t) <> singleton '\''


{- | A scalar can be emitted plain when:

* It's not parsed as a non-string scalar by the core schema
  (true / false / null / number / etc.).
* It does not contain any of the YAML indicator characters that
  would change the parse (@: { } [ ] , & * # ? | > ' " % @ \`@) in
  places that matter, or whitespace next to colons that would
  confuse the block scanner.
-}
safePlain :: Text -> Bool
safePlain t = not (parsesAsScalar t) && plainSyntaxSafe t


{- | Conservative syntax check shared with the pretty-printer. Schema-like
words may be unquoted only when the caller deliberately permits coercion.
-}
plainSyntaxSafe :: Text -> Bool
plainSyntaxSafe t
  | T.null t = False
  | t == "..." = False
  | startBad (T.head t) = False
  | endsWithSpace = False
  | T.any unsafePlainChar t = False
  | T.any mustEscape t = False
  | T.isInfixOf ": " t = False
  | T.isInfixOf ":\t" t = False
  | T.isSuffixOf ":" t = False
  | T.isInfixOf " #" t = False
  | otherwise = True
  where
    startBad c = case c of
      '!' -> True
      '&' -> True
      '*' -> True
      '?' -> True
      '|' -> True
      '>' -> True
      '\'' -> True
      '"' -> True
      '%' -> True
      '@' -> True
      '`' -> True
      '#' -> True
      ',' -> True
      '[' -> True
      ']' -> True
      '{' -> True
      '}' -> True
      ':' -> True
      '-' -> True
      ' ' -> True
      '\t' -> True
      _ -> False
    endsWithSpace = case T.unsnoc t of
      Just (_, c) -> c == ' ' || c == '\t'
      Nothing -> False


unsafePlainChar :: Char -> Bool
unsafePlainChar c =
  -- These cause parse ambiguity nearly anywhere in a plain scalar.
  c == ','
    || c == '['
    || c == ']'
    || c == '{'
    || c == '}'
    || c == '&'
    || c == '*'
    || c == '\n'
    || c == '\r'


{- | Conservative recogniser for strings the core schema would parse
as a non-string scalar. We must quote those when they appear as a
string value so the round-trip is faithful.
-}
parsesAsScalar :: Text -> Bool
parsesAsScalar t = case T.toLower t of
  "<<" -> True
  "null" -> True
  "~" -> True
  "" -> True
  "true" -> True
  "false" -> True
  "yes" -> True
  "no" -> True
  "on" -> True
  "off" -> True
  ".inf" -> True
  "+.inf" -> True
  "-.inf" -> True
  ".nan" -> True
  s ->
    looksLikeInt s || looksLikeFloat s


{- | Conservative integer recogniser matching the YAML 1.2 core
schema: optional sign, then decimal / hex (@0x@) / octal (@0o@).
-}
looksLikeInt :: Text -> Bool
looksLikeInt s = case T.uncons s of
  Just ('+', rest) -> goDec rest
  Just ('-', rest) -> goDec rest
  _ -> goDec s
  where
    goDec s'
      | T.isPrefixOf "0x" s' || T.isPrefixOf "0X" s' =
          let r = T.drop 2 s' in not (T.null r) && T.all isHex r
      | T.isPrefixOf "0o" s' || T.isPrefixOf "0O" s' =
          let r = T.drop 2 s' in not (T.null r) && T.all isOct r
      | otherwise = not (T.null s') && T.all isDigit s'
    isHex c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    isOct c = c >= '0' && c <= '7'


looksLikeFloat :: Text -> Bool
looksLikeFloat s =
  let s' = case T.uncons s of
        Just ('+', r) -> r
        Just ('-', r) -> r
        _ -> s
      hasDigit = T.any isDigit s'
      hasMark = T.any (\c -> c == '.' || c == 'e' || c == 'E') s'
      onlyOK =
        T.all
          ( \c ->
              isDigit c
                || c == '.'
                || c == 'e'
                || c == 'E'
                || c == '+'
                || c == '-'
          )
          s'
  in hasDigit && hasMark && onlyOK


needsDoubleQuote :: Text -> Bool
needsDoubleQuote = T.any badForSingle
  where
    badForSingle c =
      mustEscape c && c /= '\t'


mustEscape :: Char -> Bool
mustEscape c = isControl c || c == '\xFEFF' || c == '\xFFFE' || c == '\xFFFF'
