{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Annotation-preserving YAML decoder.

Parses YAML to 'AStream' / 'ADocument' / 'AValue' carrying:

* Source-byte spans for each top-level mapping entry / sequence
  item, so unmodified entries can be copied verbatim during
  re-emission.
* Comments and blank-line trivia attached to the entry that
  follows them.
* The original scalar style (plain / quoted / literal block /
  folded block) so that fresh edits keep the surrounding
  document's stylistic choices.

The structural parse delegates to "YAML.Decode" so spec
conformance stays in one place; this module layers a second
pass that walks the source text with a small line scanner.
-}
module YAML.Decode.Annotated (
  decodeAnnotated,
  decodeAnnotatedStream,
  decodeAnnotatedDocument,
) where

import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import YAML.Annotated (
  ABody (..),
  ADocument (..),
  AMapEntry (..),
  AStream (..),
  AValue (..),
  Chomping (..),
  Comment (..),
  MapStyle (..),
  ScalarStyle (..),
  SeqStyle (..),
  SrcSpan (..),
  Trivia (..),
 )
import YAML.Annotated qualified
import YAML.Decode qualified as YD
import YAML.Decode.Flow (annotateFlow)
import YAML.Internal (isYamlSpace, stripYaml, stripYamlStart, wordsYaml)
import YAML.Value (Document (..), Value (..))
import YAML.Value qualified


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

{- | Decode a single-document YAML source into an annotated tree.
The original source 'Text' is returned alongside; the renderer
needs it to copy unchanged sub-trees verbatim.
-}
decodeAnnotated :: Text -> Either String (ADocument, Text)
decodeAnnotated src = do
  (AStream docs, original) <- decodeAnnotatedStream src
  case V.toList docs of
    [d] -> pure (d, original)
    [] -> pure (ADocument False False (annotatePlain YNull) [] (Just (SrcSpan 0 (BS.length (TE.encodeUtf8 src)))) [] [], src)
    _ -> Left "decodeAnnotated: source contains multiple documents; use decodeAnnotatedStream"


-- | Decode documents with spans relative to the original stream.
decodeAnnotatedStream :: Text -> Either String (AStream, Text)
decodeAnnotatedStream src = do
  docs <- YD.decodeLocatedDocuments src
  pure (AStream (V.fromList (fmap annotateLocated docs)), src)
  where
    annotateLocated located =
      let offset = YD.locatedOffset located
          doc = annotateDocument (YD.locatedSource located) (YD.locatedDocument located) (YD.locatedAnnotatedBody located)
          shift (SrcSpan a b) = SrcSpan (a + offset) (b + offset)
          shiftValue av =
            av
              { avSpan = shift <$> avSpan av
              , avNodeSpan = shift <$> avNodeSpan av
              , avBody = case avBody av of
                  ASeq xs style -> ASeq (V.map shiftValue xs) style
                  AMap es style -> AMap (V.map (\e -> e {amKey = shiftValue (amKey e), amValue = shiftValue (amValue e)}) es) style
                  ATagged tag inner -> ATagged tag (shiftValue inner)
                  AAnchored anchor inner -> AAnchored anchor (shiftValue inner)
                  body -> body
              }
      in doc {adBody = shiftValue (adBody doc), adOriginalSpan = shift <$> adOriginalSpan doc}


decodeAnnotatedDocument :: Text -> Either String ADocument
decodeAnnotatedDocument src = fst <$> decodeAnnotated src


-- ---------------------------------------------------------------------------
-- Annotation pass
-- ---------------------------------------------------------------------------

-- | Wrap a parsed 'Document' with annotations.
annotateDocument :: Text -> Document -> AValue -> ADocument
annotateDocument src (Document start end body) presentation =
  let !srcLen = BS.length (TE.encodeUtf8 src)
      !lineIdx = buildLineIndex src
      !av = annotateBody src lineIdx presentation
      !av' = av {avSpan = Just (SrcSpan 0 srcLen)}
  in ADocument
       { adDirectivesEnd = start
       , adExplicitEnd = end
       , adBody = av'
       , adLeading = []
       , adOriginalSpan = Just (SrcSpan 0 srcLen)
       , adTrailing = trailingComments body src
       , adDirectives = filter (T.isPrefixOf "%") (takeWhile (\line -> T.null (stripYaml line) || T.isPrefixOf "#" line || T.isPrefixOf "%" line) (logicalLines src))
       }


-- Keep trailing standalone comments when an edit disables whole-document copying.
trailingComments :: Value -> Text -> [Comment]
trailingComments value src
  | isBlockScalar value = []
  | otherwise = reverse (go (reverse (logicalLines src)))
  where
    -- A root block scalar may contain comment-looking lines at column zero.
    -- Its header is the first content line, possibly preceded by properties.
    isBlockScalar (YTagged _ inner) = isBlockScalar inner
    isBlockScalar (YAnchored _ inner) = isBlockScalar inner
    isBlockScalar (YString _) = case dropWhile isPrologue (logicalLines src) of
      line : _ -> case dropWhile isProperty (wordsYaml line) of
        header : _ -> T.isPrefixOf "|" header || T.isPrefixOf ">" header
        [] -> False
      [] -> False
    isBlockScalar _ = False
    isProperty word = T.isPrefixOf "!" word || T.isPrefixOf "&" word
    isPrologue line =
      let stripped = stripYaml line
      in T.null stripped || T.isPrefixOf "#" stripped || T.isPrefixOf "%" stripped || stripped == "---"
    go [] = []
    go (line : rest)
      | T.null (stripYaml line) = go rest
      | stripYaml line == "..." = go rest
      | Just body <- T.stripPrefix "#" line =
          Comment (T.dropWhile (== ' ') body) 0 : go rest
      | otherwise = []


-- | Wrap top-level structures with per-entry spans + trivia.
annotateBody :: Text -> LineIndex -> AValue -> AValue
annotateBody src lineIdx value
  | Just annotated <- annotateFlow src value = annotated
  | otherwise = case avBody value of
      AMap pairs _
        | not (V.null pairs)
        , Just entries <- locateMapEntries src lineIdx ((\entry -> (amKey entry, amValue entry)) <$> V.toList pairs) ->
            value {avBody = AMap (V.fromList entries) MapBlock}
      ASeq items _
        | not (V.null items)
        , Just entries <- locateSeqItems src lineIdx (V.toList items) ->
            value {avBody = ASeq (V.fromList entries) SeqBlock}
      ATagged tag inner -> value {avBody = ATagged tag (annotateBody src lineIdx inner)}
      AAnchored anchor inner -> value {avBody = AAnchored anchor (annotateBody src lineIdx inner)}
      _ -> styleNode src value


-- ---------------------------------------------------------------------------
-- Trivial annotation (no source spans, default styles)
-- ---------------------------------------------------------------------------

annotatePlain :: Value -> AValue
annotatePlain v = AValue (bodyOf v) emptyTrivia Nothing Nothing "" (-1) False
  where
    bodyOf = \case
      YNull -> ANull
      YBool b -> ABool b
      YInt n -> AInt n
      YFloat d -> AFloat d
      YString t -> AString t (defaultScalarStyle t)
      YSeq xs -> ASeq (V.map annotatePlain xs) SeqBlock
      YMap kvs ->
        AMap
          ( V.map
              ( \(k, vv) ->
                  AMapEntry
                    (annotatePlain k)
                    (annotatePlain vv)
                    Nothing
              )
              kvs
          )
          MapBlock
      YTagged tg inner -> ATagged tg (annotatePlain inner)
      YAnchored a inner -> AAnchored a (annotatePlain inner)
      YAlias a -> AAlias a


emptyTrivia :: Trivia
emptyTrivia = Trivia [] 0 Nothing


defaultScalarStyle :: Text -> ScalarStyle
defaultScalarStyle t
  | T.any (\c -> c == '\n' || c == '\r') t = SSDoubleQuoted
  | otherwise = SSPlain


logicalLines :: Text -> [Text]
logicalLines = T.lines . T.replace "\r" "\n" . T.replace "\r\n" "\n"


propertyOnly :: Text -> Bool
propertyOnly source =
  let strip text
        | T.isPrefixOf "!" text || T.isPrefixOf "&" text = strip (stripYamlStart (T.dropWhile (\c -> c /= ' ' && c /= '\t') text))
        | otherwise = T.null text || T.isPrefixOf "#" text
  in (T.isPrefixOf "!" source || T.isPrefixOf "&" source) && strip source


-- ---------------------------------------------------------------------------
-- Line-byte index
-- ---------------------------------------------------------------------------

{- | A 'LineIndex' is a vector mapping 1-based line numbers to
their UTF-8 byte offsets in the source. Index 0 is unused; the
final element is the byte length of the source so that line
@i@ runs from @lineByte i@ inclusive to @lineByte (i + 1)@
exclusive.
-}
data LineIndex = LineIndex !(V.Vector Int)
  deriving (Show)


buildLineIndex :: Text -> LineIndex
buildLineIndex src =
  let bytes = TE.encodeUtf8 src
      size = BS.length bytes
      starts position
        | position >= size = []
        | BS.index bytes position == 13 =
            let next = if position + 1 < size && BS.index bytes (position + 1) == 10 then position + 2 else position + 1
            in next : starts next
        | BS.index bytes position == 10 = (position + 1) : starts (position + 1)
        | otherwise = starts (position + 1)
  in LineIndex (V.fromList (0 : starts 0 <> [size]))
{-# INLINE buildLineIndex #-}


lineRange :: LineIndex -> Int -> Maybe (Int, Int)
lineRange (LineIndex v) lno
  | lno < 1 || lno >= V.length v = Nothing
  | otherwise = Just (V.unsafeIndex v (lno - 1), V.unsafeIndex v lno)


-- ---------------------------------------------------------------------------
-- Top-level mapping entry locator
-- ---------------------------------------------------------------------------

{- | Walk the source in line order, matching each top-level
mapping key from the parsed structure to a line beginning at
column 0 with that key followed by @:@. For each entry we
record the byte span covering the key line + any continuation
lines (lines indented past column 0). Stand-alone comments and
blank lines that immediately precede the key are attached as
'triviaLeading' / 'triviaBlankLines'.

Returns 'Nothing' if the parsed structure can't be aligned with
the source on a 1:1 basis (e.g. mappings nested under @---@,
multi-document streams). The caller falls back to whole-doc
verbatim in that case.
-}
locateMapEntries
  :: Text
  -> LineIndex
  -> [(AValue, AValue)]
  -> Maybe [AMapEntry]
locateMapEntries src lineIdx kvs = go 1 [] 0 kvs
  where
    LineIndex starts = lineIdx
    !lineCount = V.length starts - 1

    -- 'pendingComments' / 'pendingBlanks' accumulate trivia
    -- waiting to attach to the next key line.
    go !lno !pendingComments !pendingBlanks remaining
      | lno > lineCount =
          if null remaining then Just [] else Nothing
      | otherwise =
          case lineSliceUtf8 src lineIdx lno of
            Nothing -> Nothing
            Just lt ->
              let stripped = stripYamlStart lt
                  indent = T.length lt - T.length stripped
              in -- Skip directive / doc-marker lines.
                 case T.uncons stripped of
                   _
                     | T.null stripped ->
                         go
                           (lno + 1)
                           pendingComments
                           (pendingBlanks + 1)
                           remaining
                   Just ('#', rest) ->
                     let cmt = Comment (stripCommentBody rest) indent
                     in go
                          (lno + 1)
                          (pendingComments ++ [cmt])
                          pendingBlanks
                          remaining
                   _ | propertyOnly stripped -> go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('%', _) ->
                     go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('-', rest)
                     | T.isPrefixOf "--" rest ->
                         go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('.', rest)
                     | T.isPrefixOf ".." rest ->
                         go (lno + 1) pendingComments pendingBlanks remaining
                   _
                     | indent /= 0 ->
                         -- A continuation line outside any entry: bail.
                         Nothing
                   _ ->
                     case remaining of
                       [] -> Nothing -- extra content past the parsed entries
                       ((kv, vv) : rest) ->
                         case matchKeyOnLine stripped kv of
                           Nothing -> Nothing
                           Just _ ->
                             -- Find the entry's span: from start
                             -- of key line through last non-blank
                             -- continuation line.
                             let !entryStartByte = V.unsafeIndex starts (lno - 1)
                                 (endLine, _eolCmt) =
                                   scanEntryLines src lineIdx (lno + 1)
                                 !entryEndByte =
                                   -- Line @endLine@ is the first
                                   -- line that does NOT belong to
                                   -- this entry (or @lineCount + 1@
                                   -- when none follows). Bytes
                                   -- BEFORE that line make up the
                                   -- entry.
                                   V.unsafeIndex
                                     starts
                                     (min (endLine - 1) (V.length starts - 1))
                                 trailingCmt =
                                   parseEolComment src lineIdx lno
                                 trivia =
                                   Trivia
                                     pendingComments
                                     pendingBlanks
                                     Nothing
                                 keyAv =
                                   (styleNode (maybe stripped fst (YD.splitMappingLine stripped)) kv)
                                     { avTrivia = trivia
                                     }
                                 valAv =
                                   annotateContainedValue
                                     src
                                     lineIdx
                                     vv
                                     ( Just
                                         ( SrcSpan
                                             entryStartByte
                                             entryEndByte
                                         )
                                     )
                                 entry =
                                   AMapEntry
                                     keyAv
                                     valAv
                                       { avSpan =
                                           Just
                                             ( SrcSpan
                                                 entryStartByte
                                                 entryEndByte
                                             )
                                       }
                                     trailingCmt
                             in (entry :)
                                  <$> go endLine [] 0 rest


-- ---------------------------------------------------------------------------
-- Top-level sequence item locator
-- ---------------------------------------------------------------------------

locateSeqItems
  :: Text -> LineIndex -> [AValue] -> Maybe [AValue]
locateSeqItems src lineIdx items = go 1 [] 0 items
  where
    LineIndex starts = lineIdx
    !lineCount = V.length starts - 1

    go !lno !pendingComments !pendingBlanks remaining
      | lno > lineCount =
          if null remaining then Just [] else Nothing
      | otherwise =
          case lineSliceUtf8 src lineIdx lno of
            Nothing -> Nothing
            Just lt ->
              let stripped = stripYamlStart lt
                  indent = T.length lt - T.length stripped
              in case T.uncons stripped of
                   _
                     | T.null stripped ->
                         go
                           (lno + 1)
                           pendingComments
                           (pendingBlanks + 1)
                           remaining
                   Just ('#', rest) ->
                     let cmt = Comment (stripCommentBody rest) indent
                     in go
                          (lno + 1)
                          (pendingComments ++ [cmt])
                          pendingBlanks
                          remaining
                   _ | propertyOnly stripped -> go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('%', _) ->
                     go (lno + 1) pendingComments pendingBlanks remaining
                   _ | stripped == "---" || stripped == "..." -> go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('-', after) | T.null after || T.head after == ' ' ->
                     case remaining of
                       [] -> Nothing
                       (vv : rest) ->
                         let !entryStartByte = V.unsafeIndex starts (lno - 1)
                             (endLine, _) =
                               scanEntryLines src lineIdx (lno + 1)
                             !entryEndByte =
                               V.unsafeIndex
                                 starts
                                 (min (endLine - 1) (V.length starts - 1))
                             av =
                               annotateContainedValue
                                 src
                                 lineIdx
                                 vv
                                 ( Just
                                     ( SrcSpan
                                         entryStartByte
                                         entryEndByte
                                     )
                                 )
                             av' =
                               av
                                 { avTrivia =
                                     Trivia
                                       pendingComments
                                       pendingBlanks
                                       (case avBody vv of AMap _ _ -> Nothing; ASeq _ _ -> Nothing; _ -> parseEolComment src lineIdx lno)
                                 , avSpan =
                                     Just
                                       ( SrcSpan
                                           entryStartByte
                                           entryEndByte
                                       )
                                 }
                         in (av' :) <$> go endLine [] 0 rest
                   _ -> Nothing
                     where
                       !_ = indent -- silence unused


{- | An entry's body extends until the next non-indented line.
Blank lines and stand-alone comments belong to the NEXT entry's
trivia, not this one.
-}
scanEntryLines :: Text -> LineIndex -> Int -> (Int, Maybe Comment)
scanEntryLines src lineIdx@(LineIndex starts) start = go start
  where
    !lineCount = V.length starts - 1
    scalarEntry = maybe False isScalarHeader (lineSliceUtf8 src lineIdx (start - 1))
    isScalarHeader source =
      let row = stripYamlStart source
          item = if T.isPrefixOf "- " row then T.drop 2 row else row
          value = maybe item snd (YD.splitMappingLine item)
          stripProps text = if T.isPrefixOf "!" text || T.isPrefixOf "&" text then stripProps (stripYamlStart (T.dropWhile (\c -> c /= ' ' && c /= '\t') text)) else text
      in case T.uncons (stripProps (stripYamlStart value)) of Just (c, _) -> c == '|' || c == '>'; Nothing -> False
    go !lno
      | lno > lineCount = (lineCount + 1, Nothing)
      | otherwise =
          case lineSliceUtf8 src lineIdx lno of
            Nothing -> (lno, Nothing)
            Just lt ->
              let stripped = stripYamlStart lt
                  indent = T.length lt - T.length stripped
                  laterIndented = lookAhead (lno + 1)
              in if (not (T.null stripped) && indent > 0) || (T.null stripped && (scalarEntry || laterIndented))
                   then go (lno + 1)
                   else (lno, Nothing)
    lookAhead line
      | line > lineCount = False
      | otherwise = case lineSliceUtf8 src lineIdx line of
          Just text
            | T.null (stripYaml text) -> lookAhead (line + 1)
            | otherwise -> T.length text > T.length (stripYamlStart text)
          Nothing -> False


-- Recursively annotate each entry using a dedented source view, then translate
-- its byte spans back into the original document. This preserves comments and
-- quoting at every block level, including compact sequence-of-mapping items.
annotateContainedValue :: Text -> LineIndex -> AValue -> Maybe SrcSpan -> AValue
annotateContainedValue src _ value Nothing = styleNode src value
annotateContainedValue src _ value span_@(Just (SrcSpan begin end)) =
  let chunk = TE.decodeUtf8 (BS.take (end - begin) (BS.drop begin (TE.encodeUtf8 src)))
      chunkIndex@(LineIndex originalStarts) = buildLineIndex chunk
      rows = (\line -> maybe "" id (lineSliceUtf8 chunk chunkIndex line)) <$> [1 .. V.length originalStarts - 1]
      first = case rows of line : _ -> line; [] -> ""
      stripped = stripYamlStart first
      isItem = stripped == "-" || T.isPrefixOf "- " stripped
      payload =
        if isItem
          then stripYamlStart (T.drop 1 stripped)
          else maybe stripped (stripYamlStart . snd) (YD.splitMappingLine stripped)
      firstRemoved = BS.length (TE.encodeUtf8 first) - BS.length (TE.encodeUtf8 payload)
      remainder = drop 1 rows
      significant = filter (\line -> not (T.null (stripYaml line)) && not (T.isPrefixOf "#" (stripYamlStart line))) remainder
      base = case significant of line : _ -> T.length line - T.length (stripYamlStart line); [] -> 0
      inline = not (T.null payload) && not (T.isPrefixOf "#" payload)
      -- A compact map starts immediately after a sequence dash. Otherwise
      -- inline content is a scalar/flow node; block children start next line.
      compact = isItem && inline && case avBody value of AMap _ _ -> True; _ -> False
      useFirst = inline || compact
      removed line = min base (T.length (T.takeWhile (== ' ') line))
      normalizedRows = (if useFirst then [payload] else []) <> ((\line -> T.drop (removed line) line) <$> remainder)
      rowNumbers = (if useFirst then [0] else []) <> [1 .. length rows - 1]
      prefixes = (if useFirst then [firstRemoved] else []) <> (removed <$> remainder)
      view = T.intercalate "\n" normalizedRows
      viewStarts = V.fromList (scanl (\offset line -> offset + BS.length (TE.encodeUtf8 line) + 1) 0 normalizedRows)
      sourceStarts = V.fromList ((begin +) . (originalStarts V.!) <$> rowNumbers)
      prefixBytes = V.fromList prefixes
      translate isEnd offset
        | V.null sourceStarts = begin
        | otherwise =
            let row = min (V.length sourceStarts - 1) (findRow offset 0 (V.length viewStarts - 1))
                column = offset - viewStarts V.! row
                prefix = if isEnd && column == 0 && row > 0 then 0 else prefixBytes V.! row
            in sourceStarts V.! row + prefix + column
      findRow offset low high
        | low >= high = low
        | otherwise =
            let middle = (low + high + 1) `quot` 2
            in if viewStarts V.! middle <= offset then findRow offset middle high else findRow offset low (middle - 1)
      shift (SrcSpan a b) = SrcSpan (translate False a) (translate True b)
      relocate av =
        av
          { avSpan = shift <$> avSpan av
          , avNodeSpan = shift <$> avNodeSpan av
          , avBody = case avBody av of
              AMap entries style -> AMap (V.map (\entry -> entry {amKey = relocate (amKey entry), amValue = relocate (amValue entry)}) entries) style
              ASeq items style -> ASeq (V.map relocate items) style
              ATagged tag inner -> ATagged tag (relocate inner)
              AAnchored anchor inner -> AAnchored anchor (relocate inner)
              body -> body
          }
      annotated = annotateBody view (buildLineIndex view) value
      childEnds = case avBody annotated of
        AMap entries _ -> srcEnd <$> V.toList (V.mapMaybe (avSpan . amValue) entries)
        ASeq items _ -> srcEnd <$> V.toList (V.mapMaybe avSpan items)
        _ -> []
      suffix = case childEnds of
        [] -> ""
        positions -> TE.decodeUtf8 (BS.drop (maximum positions) (TE.encodeUtf8 view))
  in (relocate annotated) {avSpan = span_, avTrailingText = if isFlow annotated then avTrailingText annotated else suffix}
  where
    isFlow av = case avBody av of
      AMap _ MapFlow -> True
      ASeq _ SeqFlow -> True
      ATagged _ inner -> isFlow inner
      AAnchored _ inner -> isFlow inner
      _ -> False


styleNode :: Text -> AValue -> AValue
styleNode source av = av {avBody = apply (avBody av)}
  where
    line = case filter (\text -> let t = stripYaml text in not (T.null t) && not (T.isPrefixOf "#" t) && not (T.isPrefixOf "%" t) && t /= "---") (logicalLines source) of
      first : _ -> stripYamlStart first
      [] -> ""
    content = stripProperties line
    stripProperties text
      | T.isPrefixOf "!" text || T.isPrefixOf "&" text = stripProperties (stripYamlStart (T.dropWhile (\c -> c /= ' ' && c /= '\t') text))
      | otherwise = text
    chomp
      | T.isInfixOf "-" content = Strip
      | T.isInfixOf "+" content = Keep
      | otherwise = Clip
    apply (AString text _) =
      AString
        text
        ( case T.uncons content of
            Just ('\'', _) -> SSSingleQuoted
            Just ('"', _) -> SSDoubleQuoted
            Just ('|', _) -> SSLiteral chomp
            Just ('>', _) -> SSFolded chomp
            _ -> defaultScalarStyle text
        )
    apply (AMap entries _) | T.isPrefixOf "{" content = AMap entries MapFlow
    apply (ASeq items _) | T.isPrefixOf "[" content = ASeq items SeqFlow
    apply (ATagged tag inner) = ATagged tag (styleNode content inner)
    apply (AAnchored anchor inner) = AAnchored anchor (styleNode content inner)
    apply body = body


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lineSliceUtf8 :: Text -> LineIndex -> Int -> Maybe Text
lineSliceUtf8 src lineIdx lno = do
  (s, e) <- lineRange lineIdx lno
  let bs = TE.encodeUtf8 src
  let raw = BS.take (max 0 (e - s)) (BS.drop s bs)
      bs' = case BS.unsnoc raw of
        Just (init', 0x0A) -> init'
        _ -> raw
      bs'' = case BS.unsnoc bs' of
        Just (init', 0x0D) -> init'
        _ -> bs'
  Right t <- pure (TE.decodeUtf8' bs'')
  pure t


{- | Match a top-level key name against the start of a stripped
line. Recognises plain / single-quoted / double-quoted scalar
keys; bails out for anything more exotic.
-}
matchKeyOnLine :: Text -> AValue -> Maybe Text
matchKeyOnLine line key = do
  (raw, _) <- YD.splitMappingLine line
  case avBody key of
    AResolvedAlias _ (YAML.Value.Anchor name) _ | raw == "*" <> name -> Just line
    _ -> do
      parsed <- either (const Nothing) Just (YD.decode raw)
      if unwrapKey parsed == unwrapKey (YAML.Annotated.astripAnnotations key) then Just line else Nothing
  where
    unwrapKey (YTagged _ value) = unwrapKey value
    unwrapKey (YAnchored _ value) = unwrapKey value
    unwrapKey value = value


stripCommentBody :: Text -> Text
stripCommentBody t = T.dropWhile (== ' ') t


parseEolComment :: Text -> LineIndex -> Int -> Maybe Comment
parseEolComment src lineIdx lno = do
  lt <- lineSliceUtf8 src lineIdx lno
  -- Find an unquoted '#' preceded by whitespace.
  let go !i !inSq !inDq
        | i >= T.length lt = Nothing
        | otherwise =
            let c = T.index lt i
            in case c of
                 '\'' | not inDq -> go (i + 1) (not inSq) inDq
                 '"' | not inSq -> go (i + 1) inSq (not inDq)
                 '#'
                   | not inSq
                   , not inDq
                   , i > 0
                   , isYamlSpace (T.index lt (i - 1)) ->
                       Just
                         ( Comment
                             (stripCommentBody (T.drop (i + 1) lt))
                             i
                         )
                 _ -> go (i + 1) inSq inDq
  go 0 False False


_isDigit :: Char -> Bool
_isDigit = isDigit
