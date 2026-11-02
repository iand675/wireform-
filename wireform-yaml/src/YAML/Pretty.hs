{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pretty-printing for 'YAML.Value.Value' and
'YAML.Annotated.AValue' with configurable layout.

The fast emitter in "YAML.Encode" is biased toward producing
compact, canonical output. 'YAML.Pretty.render' / 'renderAnnotated'
here lets you wrap long flow collections, force quoting on
ambiguous scalars, switch between block and flow styles, etc.
-}
module YAML.Pretty (
  -- * Options
  RenderOptions (..),
  defaultOptions,
  compactOptions,

  -- * Style preferences
  StringStyleHint (..),

  -- * Render entry points
  render,
  renderDocument,
  renderStream,
  renderBS,
  renderAnnotated,
  renderAnnotatedDocument,
  renderAnnotatedStream,
) where

import Control.DeepSeq (NFData)
import Data.ByteString qualified as BS
import Data.Char (isControl, isPrint, ord)
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
import GHC.Generics (Generic)
import YAML.Annotated (
  ABody (..),
  ADocument (..),
  AMapEntry (..),
  AStream (..),
  AValue (..),
  Comment (..),
  MapStyle (..),
  ScalarStyle (..),
  SeqStyle (..),
  SrcSpan (..),
  Trivia (..),
  prepareForRendering,
 )
import YAML.Decode qualified as YD
import YAML.Encode qualified as YE
import YAML.Internal (stripYamlStart)
import YAML.Value (
  Anchor (..),
  Document (..),
  Stream (..),
  Tag (..),
  Value (..),
 )
import YAML.Value qualified as YV


-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | What style to prefer for fresh strings.
data StringStyleHint
  = -- | Plain when safe, double-quoted otherwise.
    HintAuto
  | HintAlwaysDouble
  | HintAlwaysSingle
  | -- | For multi-line strings.
    HintLiteralBlock
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Tunables for the pretty-printer.
data RenderOptions = RenderOptions
  { roIndent :: {-# UNPACK #-} !Int
  -- ^ Spaces per indent level. Default @2@.
  , roMaxLineWidth :: {-# UNPACK #-} !Int
  {- ^ Soft maximum line width, used to decide when to wrap
  long flow collections onto multiple lines and when to
  promote a long block scalar to a literal block. Default
  @100@. Set to 'maxBound' to disable wrapping.
  -}
  , roDefaultMapStyle :: !MapStyle
  -- ^ How fresh mappings render. Default 'MapBlock'.
  , roDefaultSeqStyle :: !SeqStyle
  -- ^ How fresh sequences render. Default 'SeqBlock'.
  , roStringStyleHint :: !StringStyleHint
  -- ^ How fresh strings render. Default 'HintAuto'.
  , roCompactSeqOfMap :: !Bool
  {- ^ Use the @- key: value@ compact form for mappings as
  sequence items. Default 'True'.
  -}
  , roQuoteAmbiguousScalars :: !Bool
  {- ^ Quote a fresh plain string when its content would parse
  back as another core-schema literal (number, bool, null).
  Default 'True'.
  -}
  , roDocStartMarker :: !Bool
  {- ^ Emit a leading @---@ on every document. Default 'False'
  for single-document streams; 'True' for multi-document
  streams (forced by 'renderStream').
  -}
  , roDocEndMarker :: !Bool
  -- ^ Emit a trailing @...@ marker. Default 'False'.
  , roTrailingNewline :: !Bool
  -- ^ Emit a final @\n@ at end of stream. Default 'True'.
  , roPreserveOriginalText :: !Bool
  {- ^ When 'True' (the default), unchanged sub-trees in an
  annotated render reuse their original source bytes
  verbatim. Set to 'False' to force re-emission everywhere
  (useful for normalising / canonicalising a document).
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Sensible defaults for human-readable output.
defaultOptions :: RenderOptions
defaultOptions =
  RenderOptions
    { roIndent = 2
    , roMaxLineWidth = 100
    , roDefaultMapStyle = MapBlock
    , roDefaultSeqStyle = SeqBlock
    , roStringStyleHint = HintAuto
    , roCompactSeqOfMap = True
    , roQuoteAmbiguousScalars = True
    , roDocStartMarker = False
    , roDocEndMarker = False
    , roTrailingNewline = True
    , roPreserveOriginalText = True
    }


{- | Compact emission: short lines, no padding, prefer flow when
it's shorter.
-}
compactOptions :: RenderOptions
compactOptions =
  defaultOptions
    { roIndent = 2
    , roMaxLineWidth = 200
    , roDefaultMapStyle = MapFlow
    , roDefaultSeqStyle = SeqFlow
    , roStringStyleHint = HintAuto
    , roCompactSeqOfMap = True
    , roTrailingNewline = False
    }


-- ---------------------------------------------------------------------------
-- Render entry points
-- ---------------------------------------------------------------------------

-- | Render a 'Value' to 'Text' using the given options.
render :: RenderOptions -> Value -> Text
render opts v = TL.toStrict (toLazyText (build (validIndent opts) v))


renderBS :: RenderOptions -> Value -> BS.ByteString
renderBS opts = TE.encodeUtf8 . render opts


renderDocument :: RenderOptions -> Document -> Text
renderDocument opts (Document start end body) =
  render (opts {roDocStartMarker = start || roDocStartMarker opts, roDocEndMarker = end || roDocEndMarker opts}) body


renderStream :: RenderOptions -> Stream -> Text
renderStream opts (Stream docs) = TL.toStrict (toLazyText (V.ifoldl' step mempty docs))
  where
    step acc i doc =
      acc
        <> fromText
          ( renderDocument
              (opts {roDocStartMarker = i > 0 || roDocStartMarker opts, roTrailingNewline = i < V.length docs - 1 || roTrailingNewline opts})
              doc
          )


{- | Render an 'AValue' tree. Uses the original source bytes for
sub-trees that haven't been modified (when
'roPreserveOriginalText' is on); falls back to the pretty-
printer otherwise.
-}
data SourceText = SourceText
  { originalText :: !Text
  , originalBytes :: !BS.ByteString
  }


sourceText :: Text -> SourceText
sourceText text = SourceText text (TE.encodeUtf8 text)


renderAnnotated :: RenderOptions -> AValue -> Maybe Text -> Text
renderAnnotated requested av original =
  let options = freshOptions requested
      body = buildAValue options (sourceText <$> original) 0 ChildOfDoc (prepareForRendering av) {avDirty = True}
      rendered = frameValue options body
  in preserveLineEndings requested original (TL.toStrict (toLazyText rendered))


renderAnnotatedDocument :: RenderOptions -> ADocument -> Maybe Text -> Text
renderAnnotatedDocument opts doc original = renderAnnotatedDocumentWithSource opts doc (sourceText <$> original)


renderAnnotatedDocumentWithSource :: RenderOptions -> ADocument -> Maybe SourceText -> Text
renderAnnotatedDocumentWithSource opts doc@(ADocument start end body _ original _ _) msrc
  | roPreserveOriginalText opts
  , not (avDirty body)
  , not (roDocStartMarker opts && not start)
  , not (roDocEndMarker opts && not end)
  , Just sp <- original
  , Just src <- msrc
  , Just text <- spanSlice src sp =
      text
  | otherwise = renderAnnotatedDocumentFresh opts doc msrc


renderAnnotatedDocumentFresh :: RenderOptions -> ADocument -> Maybe SourceText -> Text
renderAnnotatedDocumentFresh requested (ADocument start end originalBody leading original trailing directives) msrc =
  preserveLineEndings requested documentSource $
    TL.toStrict
      ( toLazyText
          ( finish
              ( foldMap (\line -> fromText line <> singleton '\n') directives
                  <> (if start || roDocStartMarker opts || not (null directives) then fromText "---\n" else mempty)
                  <> emitComments 0 leading
                  <> emittedBody
                  <> emitComments 0 trailing
                  <> (if end || roDocEndMarker opts then fromText "...\n" else mempty)
              )
          )
      )
  where
    documentSource =
      msrc >>= \source -> case original of
        Just span_ -> spanSlice source span_
        Nothing -> Just (originalText source)
    body = prepareForRendering originalBody
    opts = freshOptions requested
    rawBody = buildAValue opts msrc 0 ChildOfDoc (case original of Just _ -> body {avDirty = True}; Nothing -> body)
    emittedBody = if roTrailingNewline opts || end || roDocEndMarker opts || not (null trailing) then ensureNewline rawBody else rawBody
    finish = if roTrailingNewline opts then id else dropFinalNewline


-- Removing the final physical newline of a copied literal can change its value.
-- When regenerating without a final newline, emit scalars with escaped breaks.
freshOptions :: RenderOptions -> RenderOptions
freshOptions opts
  | roTrailingNewline opts = validIndent opts
  | otherwise = (validIndent opts) {roPreserveOriginalText = False}


validIndent :: RenderOptions -> RenderOptions
validIndent opts = opts {roIndent = max 1 (roIndent opts)}


preserveLineEndings :: RenderOptions -> Maybe Text -> Text -> Text
preserveLineEndings opts (Just source) output
  | roPreserveOriginalText opts
  , Just ending <- uniformEnding source =
      T.replace "\n" ending (T.replace "\r" "\n" (T.replace "\r\n" "\n" output))
  where
    uniformEnding text
      | T.isInfixOf "\r\n" text, not (T.any (\c -> c == '\r' || c == '\n') (T.replace "\r\n" "" text)) = Just "\r\n"
      | T.any (== '\r') text, not (T.any (== '\n') text) = Just "\r"
      | otherwise = Nothing
preserveLineEndings _ _ output = output


renderAnnotatedStream :: RenderOptions -> AStream -> Maybe Text -> Text
renderAnnotatedStream opts (AStream docs) msrc
  | V.null docs = if roPreserveOriginalText opts then maybe T.empty id msrc else T.empty
  | V.length docs == 1 =
      renderAnnotatedDocumentWithSource opts (V.head docs) cachedSource
  | otherwise = TL.toStrict (toLazyText (V.foldl' step mempty (V.indexed docs)))
  where
    cachedSource = sourceText <$> msrc
    step acc (i, d) =
      let docOpts = opts {roDocStartMarker = roDocStartMarker opts || (i > 0 && not (adExplicitEnd (docs V.! (i - 1)))), roTrailingNewline = i < V.length docs - 1 || roTrailingNewline opts}
          text = renderAnnotatedDocumentWithSource docOpts d cachedSource
          separator = if i < V.length docs - 1 && not (T.isSuffixOf "\n" text || T.isSuffixOf "\r" text) then singleton '\n' else mempty
      in acc <> fromText text <> separator


-- ---------------------------------------------------------------------------
-- Common building blocks
-- ---------------------------------------------------------------------------

build :: RenderOptions -> Value -> Builder
build opts v0 = frameValue opts (buildValue opts 0 ChildOfDoc v0)


frameValue :: RenderOptions -> Builder -> Builder
frameValue opts body =
  let body' = if roTrailingNewline opts || roDocEndMarker opts then ensureNewline body else body
      framed =
        (if roDocStartMarker opts then fromText "---\n" else mempty)
          <> body'
          <> (if roDocEndMarker opts then fromText "...\n" else mempty)
  in if roTrailingNewline opts then framed else dropFinalNewline framed


ensureNewline :: Builder -> Builder
ensureNewline body = if endsWithNL body then body else body <> singleton '\n'


dropFinalNewline :: Builder -> Builder
dropFinalNewline builder =
  let text = toLazyText builder
  in fromText (TL.toStrict (maybe text id (TL.stripSuffix "\n" text)))


-- A 'ChildContext' tells emitters how the value is being placed
-- (top-level, inside a block sequence item, inside a mapping
-- value at column N, etc.). It governs leading whitespace and
-- compact-form decisions.
data ChildContext
  = ChildOfDoc
  | {- | Same line as the mapping key's @:@; if the value fits
    inline we emit it after a single space, otherwise we go
    to the next line indented past the key.
    -}
    ChildOfMapValueInline
  | {- | Following a @-@ on the same line; treat similarly to
    'ChildOfMapValueInline'.
    -}
    ChildOfSeqItem


{- | Approximate the rendered length of a value to decide whether
it fits on one line. We don't need exactness — only enough to
make a flow / block / wrap decision.
-}
approxFlowLength :: Value -> Int
approxFlowLength = go
  where
    go = \case
      YNull -> 4
      YBool b -> if b then 4 else 5
      YInt n -> fromIntegral (TL.length (toLazyText (BI.decimal n)))
      YFloat d -> fromIntegral (TL.length (toLazyText (formatDouble d)))
      YString t -> 2 + T.length t -- worst case: quoted
      YSeq xs
        | V.null xs -> 2
        | otherwise ->
            2
              + V.length xs * 2
              + V.sum (V.map go xs)
      YMap kvs
        | V.null kvs -> 2
        | otherwise ->
            2
              + V.length kvs * 4
              + V.sum (V.map (\(k, v) -> go k + go v) kvs)
      YTagged _ x -> 6 + go x
      YAnchored _ x -> 6 + go x
      YAlias (Anchor name) -> 1 + T.length name
{-# INLINE approxFlowLength #-}


-- | Indent emission: @n@ spaces.
spaces :: Int -> Builder
spaces n
  | n <= 0 = mempty
  | otherwise = fromText (T.replicate n (T.singleton ' '))


{- | Does this builder, when forced, end with @\n@? We need only
a coarse approximation; if in doubt, return 'False' so the
caller emits a separator.
-}
endsWithNL :: Builder -> Bool
endsWithNL b = case TL.unsnoc (toLazyText b) of
  Just (_, '\n') -> True
  _ -> False


-- ---------------------------------------------------------------------------
-- Value rendering (un-annotated)
-- ---------------------------------------------------------------------------

buildValue :: RenderOptions -> Int -> ChildContext -> Value -> Builder
buildValue opts !ind ctx v = case v of
  YNull -> emitScalar opts ind ctx YNull
  YBool _ -> emitScalar opts ind ctx v
  YInt _ -> emitScalar opts ind ctx v
  YFloat _ -> emitScalar opts ind ctx v
  YString _ -> emitScalar opts ind ctx v
  YSeq xs
    | V.null xs -> contextSeparator ctx <> fromText "[]\n"
    | otherwise -> emitSeq opts ind ctx xs (roDefaultSeqStyle opts)
  YMap kvs
    | V.null kvs -> contextSeparator ctx <> fromText "{}\n"
    | otherwise -> emitMap opts ind ctx kvs (roDefaultMapStyle opts)
  property -> contextSeparator ctx <> emitFlowValue opts ind property <> singleton '\n'


{- | Builder used inline (after the colon / dash separator) on the
same line as the parent's marker.
-}
buildValueFlat :: RenderOptions -> Int -> Value -> Builder
buildValueFlat opts ind = buildValue opts ind ChildOfMapValueInline


contextSeparator :: ChildContext -> Builder
contextSeparator = \case
  ChildOfDoc -> mempty
  ChildOfMapValueInline -> singleton ' '
  ChildOfSeqItem -> mempty


-- ----- Sequences -----

emitSeq :: RenderOptions -> Int -> ChildContext -> V.Vector Value -> SeqStyle -> Builder
emitSeq opts ind ctx xs style
  | shouldFlowSeq opts ind xs style =
      contextSeparator ctx <> emitFlowSeq opts ind xs <> singleton '\n'
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          ChildOfSeqItem -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty xs
  where
    step acc x =
      acc
        <> spaces ind
        <> fromText "- "
        <> buildSeqItem opts (ind + 2) x


shouldFlowSeq :: RenderOptions -> Int -> V.Vector Value -> SeqStyle -> Bool
shouldFlowSeq opts ind xs style =
  case style of
    SeqFlow -> True
    SeqBlock ->
      ind + V.sum (V.map approxFlowLength xs) + 2 * V.length xs + 2
        <= roMaxLineWidth opts
        && V.all isLeaf xs
        -- only auto-flow when items are leaf scalars, avoid
        -- nesting flow inside block by accident
        && False -- block style stays block by default
  where
    isLeaf = \case
      YSeq {} -> False
      YMap {} -> False
      YTagged _ x -> isLeaf (YV.unwrap x)
      YAnchored _ x -> isLeaf (YV.unwrap x)
      _ -> True


buildSeqItem :: RenderOptions -> Int -> Value -> Builder
buildSeqItem opts ind v
  | roCompactSeqOfMap opts
  , roDefaultMapStyle opts == MapBlock
  , YMap kvs <- v
  , not (V.null kvs) =
      emitCompactSeqMap opts ind kvs
  | YSeq xs <- v
  , not (V.null xs) =
      singleton '\n' <> emitSeq opts ind ChildOfDoc xs (roDefaultSeqStyle opts)
  | otherwise = buildValue opts ind ChildOfSeqItem v


emitCompactSeqMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> Builder
emitCompactSeqMap opts ind kvs
  | V.null kvs = fromText "{}\n"
  | otherwise =
      let (k0, v0) = V.head kvs
          rest = V.tail kvs
      in emitMapEntry opts ind 0 k0 v0
           <> V.foldl' step mempty rest
  where
    step acc (k, v) = acc <> emitMapEntry opts ind ind k v


-- ----- Mappings -----

emitMap :: RenderOptions -> Int -> ChildContext -> V.Vector (Value, Value) -> MapStyle -> Builder
emitMap opts ind ctx kvs style
  | shouldFlowMap opts ind kvs style =
      contextSeparator ctx <> emitFlowMap opts ind kvs <> singleton '\n'
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          ChildOfSeqItem -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty kvs
  where
    step acc (k, v) = acc <> emitMapEntry opts ind ind k v


shouldFlowMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> MapStyle -> Bool
shouldFlowMap opts ind kvs = \case
  MapFlow -> True
  MapBlock ->
    ind
      + V.sum (V.map (\(k, v) -> approxFlowLength k + approxFlowLength v) kvs)
      + 4 * V.length kvs
      + 2
      <= roMaxLineWidth opts
      && False -- prefer block by default


emitMapEntry :: RenderOptions -> Int -> Int -> Value -> Value -> Builder
emitMapEntry opts childInd indentForLine k v =
  spaces indentForLine
    <> emitBlockKey opts childInd k
    <> fromText ":"
    <> buildValueFlat opts (childInd + roIndent opts) v


emitBlockKey :: RenderOptions -> Int -> Value -> Builder
emitBlockKey opts ind key
  | YE.explicitKey key = fromText "? " <> emitFlowValue opts (ind + 2) key <> singleton '\n' <> spaces ind
  | otherwise = emitMapKey opts ind key


emitMapKey :: RenderOptions -> Int -> Value -> Builder
emitMapKey opts ind k = case k of
  YString t -> emitPlainOrQuotedKey opts t
  YInt n -> BI.decimal n
  YBool b -> fromText (if b then "true" else "false")
  YNull -> fromText "null"
  YFloat d -> formatDouble d
  other -> emitFlowValue opts ind other


emitPlainOrQuotedKey :: RenderOptions -> Text -> Builder
emitPlainOrQuotedKey opts t
  | roStringStyleHint opts == HintAlwaysDouble = doubleQuoted t
  | roStringStyleHint opts == HintAlwaysSingle, not (T.any YE.mustEscape t) = singleQuoted t
  | needsQuoting opts t = doubleQuoted t
  | otherwise = fromText t


-- ----- Scalars -----

emitScalar :: RenderOptions -> Int -> ChildContext -> Value -> Builder
emitScalar opts ind ctx (YString text)
  | useLiteral opts text = contextSeparator ctx <> literalFor opts ind ctx text
emitScalar opts ind ctx v =
  contextSeparator ctx <> emitScalarBody opts ind v <> singleton '\n'


emitScalarBody :: RenderOptions -> Int -> Value -> Builder
emitScalarBody opts ind = \case
  YNull -> fromText "null"
  YBool b -> fromText (if b then "true" else "false")
  YInt n -> BI.decimal n
  YFloat d -> formatDouble d
  YString t -> emitString opts t
  v -> emitFlowValue opts ind v


emitString :: RenderOptions -> Text -> Builder
emitString opts t
  | T.any YE.mustEscape t = doubleQuoted t
  | otherwise = case roStringStyleHint opts of
      HintAlwaysDouble -> doubleQuoted t
      HintAlwaysSingle -> singleQuoted t
      HintLiteralBlock | needsBlock -> literalBlock t
      _ -> case classifyString opts t of
        KsPlain -> fromText t
        KsDouble -> doubleQuoted t
        KsSingle -> singleQuoted t
        KsLiteral -> literalBlock t
  where
    needsBlock = T.any (== '\n') t


data KeyStr = KsPlain | KsDouble | KsSingle | KsLiteral


classifyString :: RenderOptions -> Text -> KeyStr
classifyString opts t
  | T.null t = KsDouble
  | T.any (\c -> c == '\n' || c == '\r') t = KsDouble
  | hasUnsafeChar = KsDouble
  | parsesAsLiteral = KsDouble
  | otherwise = KsPlain
  where
    hasUnsafeChar =
      T.any
        (\c -> c == '"' || c == '\\' || isControl c)
        t

    parsesAsLiteral = needsQuoting opts t


needsQuoting :: RenderOptions -> Text -> Bool
needsQuoting opts text = not (YE.plainSyntaxSafe text) || (roQuoteAmbiguousScalars opts && not (YE.safePlain text))


useLiteral :: RenderOptions -> Text -> Bool
useLiteral opts text =
  T.any (== '\n') text
    && not (T.any (\c -> c == '\r' || (YE.mustEscape c && c /= '\n' && c /= '\t')) text)
    && roStringStyleHint opts `elem` [HintAuto, HintLiteralBlock]
    && roTrailingNewline opts


doubleQuoted :: Text -> Builder
doubleQuoted t = singleton '"' <> escapeDQ t <> singleton '"'


escapeDQ :: Text -> Builder
escapeDQ = T.foldr step mempty
  where
    step c acc = case c of
      '"' -> fromText "\\\"" <> acc
      '\\' -> fromText "\\\\" <> acc
      '\n' -> fromText "\\n" <> acc
      '\r' -> fromText "\\r" <> acc
      '\t' -> fromText "\\t" <> acc
      _
        | isControl c || not (isPrint c) ->
            hexEscape (ord c) <> acc
      _ -> singleton c <> acc


singleQuoted :: Text -> Builder
singleQuoted t = singleton '\'' <> fromText (T.replace "'" "''" t) <> singleton '\''


literalBlock :: Text -> Builder
literalBlock = literalBlockAt 0 2


literalFor :: RenderOptions -> Int -> ChildContext -> Text -> Builder
literalFor opts ind ctx text =
  let base = case ctx of
        ChildOfDoc -> 0
        ChildOfMapValueInline -> max 0 (ind - roIndent opts)
        ChildOfSeqItem -> max 0 (ind - 2)
      width = max 1 (min 9 (roIndent opts))
  in literalBlockAt base width text


literalBlockAt :: Int -> Int -> Text -> Builder
literalBlockAt base width text =
  let trailing = T.length (T.takeWhileEnd (== '\n') text)
      chomp
        | trailing == 0 = "-"
        | trailing > 1 || T.all (== '\n') text = "+"
        | otherwise = ""
      body = if trailing == 0 then text else T.dropEnd 1 text
      rows = if T.null text then [] else T.splitOn "\n" body
      needsHint = case T.uncons (T.dropWhile (== '\n') text) of
        Just (c, _) -> c == ' ' || c == '\t'
        Nothing -> False
  in singleton '|'
       <> (if needsHint then BI.decimal width else mempty)
       <> fromText chomp
       <> singleton '\n'
       <> foldMap (\line -> (if T.null line then mempty else spaces (base + width) <> fromText line) <> singleton '\n') rows


-- ----- Flow style -----

emitFlowValue :: RenderOptions -> Int -> Value -> Builder
emitFlowValue opts ind = \case
  YNull -> fromText "null"
  YBool b -> fromText (if b then "true" else "false")
  YInt n -> BI.decimal n
  YFloat d -> formatDouble d
  YString t -> emitFlowString opts t
  YSeq xs -> emitFlowSeq opts ind xs
  YMap kvs -> emitFlowMap opts ind kvs
  YTagged tg x ->
    tagPrefix tg <> singleton ' ' <> emitFlowValue opts ind x
  YAlias (Anchor n) -> singleton '*' <> fromText n
  YAnchored (Anchor n) x ->
    singleton '&'
      <> fromText n
      <> singleton ' '
      <> emitFlowValue opts ind x


emitFlowSeq :: RenderOptions -> Int -> V.Vector Value -> Builder
emitFlowSeq opts ind xs
  | V.null xs = fromText "[]"
  | otherwise =
      let approx =
            ind
              + 2
              + V.length xs * 2
              + V.sum (V.map approxFlowLength xs)
      in if approx <= roMaxLineWidth opts
           then
             singleton '['
               <> V.ifoldl' (\acc i x -> acc <> (if i == 0 then mempty else fromText ", ") <> emitFlowValue opts ind x) mempty xs
               <> singleton ']'
           else multiLineFlowSeq opts ind xs


multiLineFlowSeq :: RenderOptions -> Int -> V.Vector Value -> Builder
multiLineFlowSeq opts ind xs =
  fromText "[\n"
    <> V.foldl' step mempty (V.indexed xs)
    <> spaces ind
    <> singleton ']'
  where
    step acc (i, x) =
      acc
        <> spaces (ind + roIndent opts)
        <> emitFlowValue opts (ind + roIndent opts) x
        <> (if i < V.length xs - 1 then singleton ',' else mempty)
        <> singleton '\n'


emitFlowMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> Builder
emitFlowMap opts ind kvs
  | V.null kvs = fromText "{}"
  | otherwise =
      let approx =
            ind
              + 2
              + V.length kvs * 4
              + V.sum
                ( V.map
                    ( \(k, v) ->
                        approxFlowLength k + approxFlowLength v
                    )
                    kvs
                )
      in if approx <= roMaxLineWidth opts
           then
             singleton '{'
               <> V.ifoldl' (\acc i pair -> acc <> flowKV opts ind i pair) mempty kvs
               <> singleton '}'
           else multiLineFlowMap opts ind kvs


flowKV :: RenderOptions -> Int -> Int -> (Value, Value) -> Builder
flowKV opts ind i (k, v) =
  (if i == 0 then mempty else fromText ", ")
    <> (if YE.explicitKey k then fromText "? " else mempty)
    <> emitFlowValue opts ind k
    <> fromText ": "
    <> emitFlowValue opts ind v


multiLineFlowMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> Builder
multiLineFlowMap opts ind kvs =
  fromText "{\n"
    <> V.foldl' step mempty (V.indexed kvs)
    <> spaces ind
    <> singleton '}'
  where
    step acc (i, (k, v)) =
      acc
        <> spaces (ind + roIndent opts)
        <> (if YE.explicitKey k then fromText "? " else mempty)
        <> emitFlowValue opts (ind + roIndent opts) k
        <> fromText ": "
        <> emitFlowValue opts (ind + roIndent opts) v
        <> (if i < V.length kvs - 1 then singleton ',' else mempty)
        <> singleton '\n'


emitFlowString :: RenderOptions -> Text -> Builder
emitFlowString opts t = case classifyString opts t of
  KsPlain -> fromText t
  KsLiteral -> doubleQuoted t -- can't use literal in flow
  _ -> doubleQuoted t


formatDouble :: Double -> Builder
formatDouble d
  | isNaN d = fromText ".nan"
  | isInfinite d && d > 0 = fromText ".inf"
  | isInfinite d = fromText "-.inf"
  | otherwise = BF.realFloat d


tagPrefix :: Tag -> Builder
tagPrefix = YE.buildTag


-- ---------------------------------------------------------------------------
-- Annotated rendering (with verbatim fallback)
-- ---------------------------------------------------------------------------

buildAValue
  :: RenderOptions
  -> Maybe SourceText
  -> Int
  -> ChildContext
  -> AValue
  -> Builder
buildAValue opts msrc !ind ctx av
  | roPreserveOriginalText opts
  , not (avDirty av)
  , Just sp <- avSpan av
  , Just src <- msrc
  , Just slice <- spanSlice src sp =
      -- Verbatim copy of the original bytes (with the
      -- caller-side context separator).
      contextSeparator ctx <> fromText slice
  | otherwise =
      emitAComments ind (triviaLeading (avTrivia av))
        <> withFinalComment (triviaTrailing (avTrivia av)) (buildABody opts msrc ind ctx av)
        <> (if flowBody av then mempty else indentTrivia ind (avTrailingText av))


spanSlice :: SourceText -> SrcSpan -> Maybe Text
spanSlice source (SrcSpan s e)
  | s < 0 || e < s = Nothing
  | otherwise =
      let bs = originalBytes source
          n = BS.length bs
      in if e > n
           then Nothing
           else either (const Nothing) Just (TE.decodeUtf8' (BS.take (e - s) (BS.drop s bs)))


buildABody
  :: RenderOptions
  -> Maybe SourceText
  -> Int
  -> ChildContext
  -> AValue
  -> Builder
buildABody opts msrc ind ctx av = case avBody av of
  ANull -> emitScalar opts ind ctx YNull
  ABool b -> emitScalar opts ind ctx (YBool b)
  AInt n -> emitScalar opts ind ctx (YInt n)
  AFloat d -> emitScalar opts ind ctx (YFloat d)
  AString t s -> emitAString opts ind ctx t s
  ASeq _ SeqFlow -> contextSeparator ctx <> emitAFlowNode opts msrc ind av {avDirty = True} <> singleton '\n'
  AMap _ MapFlow -> contextSeparator ctx <> emitAFlowNode opts msrc ind av {avDirty = True} <> singleton '\n'
  ASeq xs s -> emitASeq opts msrc ind ctx xs s
  AMap es s -> emitAMap opts msrc ind ctx es s
  ATagged tg inner ->
    contextSeparator ctx
      <> tagPrefix tg
      <> buildAValue opts msrc ind ChildOfMapValueInline inner
  AAnchored (Anchor n) inner ->
    contextSeparator ctx
      <> singleton '&'
      <> fromText n
      <> buildAValue opts msrc ind ChildOfMapValueInline inner
  AAlias (Anchor n) ->
    contextSeparator ctx
      <> singleton '*'
      <> fromText n
      <> singleton '\n'
  AResolvedAlias _ (Anchor n) _ ->
    contextSeparator ctx <> singleton '*' <> fromText n <> singleton '\n'


flowBody :: AValue -> Bool
flowBody av = case avBody av of
  ASeq _ SeqFlow -> True
  AMap _ MapFlow -> True
  _ -> False


-- Flow children retain their own token spans: an entry span may also contain
-- a block key or sequence dash, which cannot be copied inside braces.
emitAFlowNode :: RenderOptions -> Maybe SourceText -> Int -> AValue -> Builder
emitAFlowNode opts source ind av
  | roPreserveOriginalText opts
  , not (avDirty av)
  , Just span_ <- avNodeSpan av
  , Just original <- source
  , Just text <- spanSlice original span_
  , not (T.any (\char -> char == '\n' || char == '\r') text) =
      fromText text
  | otherwise = case avBody av of
      AString text SSSingleQuoted | not (T.any YE.mustEscape text) -> singleQuoted text
      AString text SSDoubleQuoted -> doubleQuoted text
      AString text _ -> emitFlowString opts text
      ASeq values _ -> collection '[' ']' ((\value -> (avTrivia value, emitAFlowNode opts source childIndent value)) <$> values)
      AMap entries _ -> collection '{' '}' (entry <$> entries)
      ATagged tag inner -> tagPrefix tag <> singleton ' ' <> emitAFlowNode opts source ind inner
      AAnchored (Anchor name) inner -> singleton '&' <> fromText name <> singleton ' ' <> emitAFlowNode opts source ind inner
      AAlias (Anchor name) -> singleton '*' <> fromText name
      AResolvedAlias _ (Anchor name) _ -> singleton '*' <> fromText name
      _ -> emitFlowValue opts ind (astripAnnotationsForFlow av)
  where
    childIndent = ind + roIndent opts
    entry item =
      let key = amKey item
          value = amValue item
          trailing = case triviaTrailing (avTrivia value) of Just comment -> Just comment; Nothing -> amTrailing item
          trivia = (avTrivia key) {triviaLeading = triviaLeading (avTrivia key) <> triviaLeading (avTrivia value), triviaTrailing = trailing}
          explicit = YE.explicitKey (astripAnnotationsForFlow key)
      in (trivia, (if explicit then fromText "? " else mempty) <> emitAFlowNode opts source childIndent key <> fromText ": " <> emitAFlowNode opts source childIndent value)
    collection open close rows
      | V.null rows && T.null (avTrailingText av) = singleton open <> singleton close
      | multiline =
          singleton open
            <> singleton '\n'
            <> V.ifoldl' (\acc index (trivia, content) -> acc <> emitTrivia childIndent trivia <> withFinalComment (triviaTrailing trivia) (spaces childIndent <> content <> (if index + 1 < V.length rows then singleton ',' else mempty) <> singleton '\n')) mempty rows
            <> indentTrivia childIndent (avTrailingText av)
            <> spaces ind
            <> singleton close
      | otherwise = singleton open <> V.ifoldl' (\acc index (_, content) -> acc <> (if index == 0 then mempty else fromText ", ") <> content) mempty rows <> singleton close
      where
        multiline = not (T.null (avTrailingText av)) || V.any (\(trivia, content) -> not (null (triviaLeading trivia)) || triviaTrailing trivia /= Nothing || triviaBlankLines trivia > 0 || TL.any (== '\n') (toLazyText content)) rows


emitAString
  :: RenderOptions
  -> Int
  -> ChildContext
  -> Text
  -> ScalarStyle
  -> Builder
emitAString opts ind ctx text style
  | blockStyle && useLiteral (opts {roStringStyleHint = HintLiteralBlock}) text = contextSeparator ctx <> literalFor opts ind ctx text
  | otherwise = contextSeparator ctx <> styled <> singleton '\n'
  where
    blockStyle = case style of SSLiteral _ -> True; SSFolded _ -> True; _ -> False
    styled = case style of
      SSSingleQuoted | not (T.any YE.mustEscape text) -> singleQuoted text
      SSDoubleQuoted -> doubleQuoted text
      SSPlain | not (needsQuoting opts text) -> fromText text
      _ -> doubleQuoted text


emitASeq
  :: RenderOptions
  -> Maybe SourceText
  -> Int
  -> ChildContext
  -> V.Vector AValue
  -> SeqStyle
  -> Builder
emitASeq opts _msrc ind ctx xs SeqFlow =
  contextSeparator ctx
    <> emitFlowSeq opts ind (V.map astripAnnotationsForFlow xs)
    <> singleton '\n'
emitASeq opts msrc ind ctx xs SeqBlock
  | V.null xs =
      contextSeparator ctx <> fromText "[]\n"
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          ChildOfSeqItem -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty xs
  where
    step !acc x = case verbatimEntry opts msrc x of
      Just slice
        | sequenceEntry slice ->
            acc
              <> emitTrivia ind (avTrivia x)
              <> copyEntry ind ind slice
      _ ->
        acc
          <> emitTrivia ind (avTrivia x)
          <> withTrailingComment
            (triviaTrailing (avTrivia x))
            (spaces ind <> fromText "- " <> buildAValueSeqItem opts msrc (ind + 2) x {avDirty = True})


buildAValueSeqItem
  :: RenderOptions -> Maybe SourceText -> Int -> AValue -> Builder
buildAValueSeqItem opts msrc ind original
  | roCompactSeqOfMap opts
  , AMap es s <- avBody av
  , s == MapBlock
  , not (V.null es) =
      emitACompactSeqMap opts msrc ind es s
  | otherwise = buildAValue opts msrc ind ChildOfSeqItem av
  where
    av = original {avTrivia = (avTrivia original) {triviaLeading = [], triviaBlankLines = 0, triviaTrailing = Nothing}}


emitACompactSeqMap
  :: RenderOptions
  -> Maybe SourceText
  -> Int
  -> V.Vector AMapEntry
  -> MapStyle
  -> Builder
emitACompactSeqMap opts msrc ind es _ =
  let (e0, rest) = V.splitAt 1 es
  in V.foldl' (entryRow 0) mempty e0
       <> V.foldl' (entryRow ind) mempty rest
  where
    entryRow indForLine acc entry =
      let trivia = emitTrivia indForLine (avTrivia (amKey entry))
      in case verbatimEntry opts msrc (amValue entry) of
           Just slice | not (avDirty (amKey entry)), matchingKey (amKey entry) slice -> acc <> trivia <> copyEntry indForLine ind slice
           _ ->
             acc
               <> trivia
               <> withTrailingComment
                 (amTrailing entry)
                 ( spaces indForLine
                     <> emitAMapKey opts ind (amKey entry)
                     <> fromText ":"
                     <> buildAValue opts msrc (ind + roIndent opts) ChildOfMapValueInline (amValue entry) {avDirty = True}
                 )


emitAMap
  :: RenderOptions
  -> Maybe SourceText
  -> Int
  -> ChildContext
  -> V.Vector AMapEntry
  -> MapStyle
  -> Builder
emitAMap opts _msrc ind ctx es MapFlow =
  contextSeparator ctx
    <> emitFlowMap opts ind (V.map entryToPair es)
    <> singleton '\n'
  where
    entryToPair e =
      ( astripAnnotationsForFlow (amKey e)
      , astripAnnotationsForFlow (amValue e)
      )
emitAMap opts msrc ind ctx es MapBlock
  | V.null es = contextSeparator ctx <> fromText "{}\n"
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          ChildOfSeqItem -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty es
  where
    step !acc e =
      let kAv = amKey e
          vAv = amValue e
      in case verbatimEntry opts msrc vAv of
           Just slice
             | not (avDirty kAv)
             , matchingKey kAv slice ->
                 acc
                   <> emitTrivia ind (avTrivia kAv)
                   <> copyEntry ind ind slice
           _ ->
             acc
               <> emitTrivia ind (avTrivia kAv)
               <> withTrailingComment
                 (amTrailing e)
                 ( spaces ind
                     <> emitAMapKey opts ind kAv
                     <> fromText ":"
                     <> buildAValue opts msrc (ind + roIndent opts) ChildOfMapValueInline vAv {avDirty = True}
                 )


{- | Returns 'Just' the original entry slice (including its
trailing newline) when the entry's value has an unmodified
span and the renderer is configured to preserve original text.
-}
indentTrivia :: Int -> Text -> Builder
indentTrivia ind text =
  let rows = T.splitOn "\n" text
  in fromText (T.intercalate "\n" ((\row -> if T.null row then row else T.replicate ind " " <> row) <$> rows))


data SourceEntry = SourceEntry
  { entryText :: !Text
  , entryIndent :: {-# UNPACK #-} !Int
  , entryLeading :: {-# UNPACK #-} !Int
  }


sequenceEntry :: SourceEntry -> Bool
sequenceEntry entry =
  let text = stripYamlStart (entryText entry)
  in text == "-" || T.isPrefixOf "- " text || T.isPrefixOf "-\t" text || T.isPrefixOf "-\n" text || T.isPrefixOf "-\r" text


matchingKey :: AValue -> SourceEntry -> Bool
matchingKey key entry = case YD.splitMappingLine (T.takeWhile (\c -> c /= '\n' && c /= '\r') (stripYamlStart (entryText entry))) of
  Nothing -> False
  Just (raw, _) -> case avBody key of
    AString text _ | raw == text, YE.safePlain text -> True
    AResolvedAlias _ (Anchor name) _ -> raw == "*" <> name
    _ -> case YD.decode raw of
      Left _ -> False
      Right original -> stripAnchors original == stripAnchors (astripAnnotationsForFlow key)
  where
    stripAnchors (YAnchored _ inner) = stripAnchors inner
    stripAnchors (YTagged tag inner) = YTagged tag (stripAnchors inner)
    stripAnchors value = value


copyEntry :: Int -> Int -> SourceEntry -> Builder
copyEntry firstIndent targetIndent entry
  | sourceIndent == targetIndent = spaces firstIndent <> fromText (T.drop leading text)
  | otherwise =
      let rows = T.splitOn "\n" (T.replace "\r" "\n" (T.replace "\r\n" "\n" text))
          restRow row
            | T.null row = row
            | otherwise = T.replicate targetIndent " " <> T.drop (min sourceIndent (T.length (T.takeWhile (== ' ') row))) row
          adjusted = case rows of
            [] -> []
            first : rest -> (T.replicate firstIndent " " <> T.drop leading first) : (restRow <$> rest)
      in fromText (T.intercalate "\n" adjusted)
  where
    text = entryText entry
    sourceIndent = entryIndent entry
    leading = entryLeading entry


verbatimEntry :: RenderOptions -> Maybe SourceText -> AValue -> Maybe SourceEntry
verbatimEntry opts msrc av
  | roPreserveOriginalText opts
  , not (avDirty av)
  , Just sp <- avSpan av
  , Just source <- msrc
  , Just text <- spanSlice source sp =
      let findBreak !position
            | position < 0 = -1
            | let byte = BS.index (originalBytes source) position, byte == 10 || byte == 13 = position
            | otherwise = findBreak (position - 1)
          lastBreak = findBreak (srcStart sp - 1)
          column = srcStart sp - lastBreak - 1
          leading = T.length (T.takeWhile (== ' ') text)
      in Just (SourceEntry text (column + leading) leading)
  | otherwise = Nothing


emitAMapKey :: RenderOptions -> Int -> AValue -> Builder
emitAMapKey opts ind av
  | YE.explicitKey (astripAnnotationsForFlow av) = fromText "? " <> emitFlowValue opts (ind + 2) (astripAnnotationsForFlow av) <> singleton '\n' <> spaces ind
  | otherwise = case avBody av of
      AString t SSPlain -> emitPlainOrQuotedKey opts t
      AString t SSDoubleQuoted -> doubleQuoted t
      AString t SSSingleQuoted | not (T.any YE.mustEscape t) -> singleQuoted t
      AString t _ -> emitPlainOrQuotedKey opts t
      AInt n -> BI.decimal n
      ABool b -> fromText (if b then "true" else "false")
      ANull -> fromText "null"
      AFloat d -> formatDouble d
      _ -> emitFlowValue opts ind (astripAnnotationsForFlow av)


astripAnnotationsForFlow :: AValue -> Value
astripAnnotationsForFlow = astrip
  where
    astrip av = case avBody av of
      ANull -> YNull
      ABool b -> YBool b
      AInt n -> YInt n
      AFloat d -> YFloat d
      AString t _ -> YString t
      ASeq xs _ -> YSeq (V.map astrip xs)
      AMap es _ ->
        YMap
          ( V.map
              ( \e ->
                  ( astrip (amKey e)
                  , astrip (amValue e)
                  )
              )
              es
          )
      ATagged tg x -> YTagged tg (astrip x)
      AAnchored a x -> YAnchored a (astrip x)
      AAlias anchor -> YAlias anchor
      AResolvedAlias _ anchor _ -> YAlias anchor


withTrailingComment :: Maybe Comment -> Builder -> Builder
withTrailingComment Nothing row = row
withTrailingComment (Just comment) row =
  let (first, rest) = TL.breakOn "\n" (toLazyText row)
  in fromText (TL.toStrict first)
       <> fromText "  # "
       <> fromText (commentText comment)
       <> (if TL.null rest then singleton '\n' else fromText (TL.toStrict rest))


withFinalComment :: Maybe Comment -> Builder -> Builder
withFinalComment Nothing value = value
withFinalComment (Just comment) value =
  let text = toLazyText value
      body = case TL.unsnoc text of Just (prefix, '\n') -> prefix; _ -> text
  in fromText (TL.toStrict body) <> fromText "  # " <> fromText (commentText comment) <> singleton '\n'


emitAComments :: Int -> [Comment] -> Builder
emitAComments ind = foldMap (\comment -> spaces (max ind (commentColumn comment)) <> fromText "# " <> fromText (commentText comment) <> singleton '\n')


-- | Emit blank lines + leading comments before an entry.
emitTrivia :: Int -> Trivia -> Builder
emitTrivia ind tr =
  mconcat (replicate (triviaBlankLines tr) (singleton '\n'))
    <> emitAComments ind (triviaLeading tr)


emitComments :: Int -> [Comment] -> Builder
emitComments = emitAComments


-- Write escapes directly to the builder; supplementary scalars need eight digits.
hexEscape :: Int -> Builder
hexEscape codepoint =
  let width = if codepoint > 0xFFFF then 8 else 4
      prefix = if width == 8 then "\\U" else "\\u"
      digits n
        | n < 16 = 1
        | otherwise = 1 + digits (n `quot` 16)
  in fromText prefix <> fromText (T.replicate (width - digits codepoint) "0") <> BI.hexadecimal codepoint
