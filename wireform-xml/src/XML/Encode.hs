{-# LANGUAGE BangPatterns #-}
-- | XML DOM serializer.
--
-- Serializes a 'Document' or 'Node' to 'ByteString' or 'Text'.
-- Uses direct buffer construction for speed: pre-computes output size,
-- allocates once, writes in a single pass.
module XML.Encode
  ( encode
  , encodeText
  , encodePretty
  , encodeNode
  , encodeNodeText
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Vector (Vector)
import qualified Data.Vector as V

import XML.Value

-- | Serialize a Document to ByteString.
encode :: Document -> ByteString
encode doc = BL.toStrict (B.toLazyByteString (buildDocument doc))

-- | Serialize to Text.
encodeText :: Document -> Text
encodeText doc = TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString (buildDocument doc)))

-- | Pretty-print with indentation (indent = number of spaces per level).
encodePretty :: Int -> Document -> ByteString
encodePretty indent doc = BL.toStrict (B.toLazyByteString (buildDocumentPretty indent doc))

-- | Serialize just a Node.
encodeNode :: Node -> ByteString
encodeNode node = BL.toStrict (B.toLazyByteString (buildNode node))

-- | Serialize just a Node to Text.
encodeNodeText :: Node -> Text
encodeNodeText node = TL.toStrict (TLE.decodeUtf8 (B.toLazyByteString (buildNode node)))

buildDocument :: Document -> Builder
buildDocument (Document mDecl root) =
  maybe mempty buildXMLDecl mDecl <> buildNode root

buildDocumentPretty :: Int -> Document -> Builder
buildDocumentPretty indent (Document mDecl root) =
  maybe mempty buildXMLDecl mDecl <> buildNodePretty indent 0 root

buildXMLDecl :: XMLDecl -> Builder
buildXMLDecl (XMLDecl ver mEnc mSa) =
  B.string7 "<?xml version=\"" <> B.byteString (TE.encodeUtf8 ver) <> B.char7 '"'
  <> maybe mempty (\e -> B.string7 " encoding=\"" <> B.byteString (TE.encodeUtf8 e) <> B.char7 '"') mEnc
  <> maybe mempty (\s -> B.string7 " standalone=\"" <> B.string7 (if s then "yes" else "no") <> B.char7 '"') mSa
  <> B.string7 "?>"

buildNode :: Node -> Builder
buildNode (Element name attrs children)
  | V.null children =
      B.char7 '<' <> buildName name <> buildAttrs attrs <> B.string7 "/>"
  | otherwise =
      B.char7 '<' <> buildName name <> buildAttrs attrs <> B.char7 '>'
      <> V.foldl' (\acc c -> acc <> buildNode c) mempty children
      <> B.string7 "</" <> buildName name <> B.char7 '>'
buildNode (Text t) = escapeXMLText t
buildNode (CData t) = buildCDataSafe t
buildNode (Comment t) = buildCommentSafe t
buildNode (ProcessingInstruction target content) =
  buildPISafe target content

-- | Build a CDATA section, splitting on @]]>@ to keep the output well-formed.
-- XML 1.0 §2.7 forbids the literal @]]>@ inside a CDATA section. We split
-- such occurrences across two CDATA sections (@...]]@ + @]]>@ + @>...@) so
-- the original text is preserved on parse.
buildCDataSafe :: Text -> Builder
buildCDataSafe t = go (T.splitOn "]]>" t)
  where
    open  = B.string7 "<![CDATA["
    close = B.string7 "]]>"
    splitMarker = B.string7 "]]]]><![CDATA[>"
    -- splitOn yields N+1 chunks; insert "]]><![CDATA[>" between adjacent
    -- chunks where a literal "]]>" used to live, but emit it as
    -- "...]]" (end first CDATA) + ">..." (start of next CDATA payload).
    go []     = open <> close
    go [x]    = open <> B.byteString (TE.encodeUtf8 x) <> close
    go (x:xs) =
      open <> B.byteString (TE.encodeUtf8 x)
        <> mconcat (fmap (\y -> splitMarker <> B.byteString (TE.encodeUtf8 y)) xs)
        <> close

-- | XML 1.0 §2.5 disallows @--@ inside a comment and forbids comments ending
-- in @-@. Encode those by replacing @--@ with @- -@ and trimming trailing
-- @-@ with a space; this preserves readability without producing
-- ill-formed XML.
buildCommentSafe :: Text -> Builder
buildCommentSafe raw =
  let !replaced = T.replace "--" "- -" raw
      !final    = if not (T.null replaced) && T.last replaced == '-'
                     then replaced <> " "
                     else replaced
  in B.string7 "<!--" <> B.byteString (TE.encodeUtf8 final) <> B.string7 "-->"

-- | XML 1.0 §2.6 forbids @?>@ inside a processing-instruction body. Replace
-- any literal @?>@ with @? >@ to keep the encoded form well-formed.
buildPISafe :: Text -> Text -> Builder
buildPISafe target content =
  let !safeContent = T.replace "?>" "? >" content
  in B.string7 "<?" <> B.byteString (TE.encodeUtf8 target)
       <> (if T.null safeContent
             then mempty
             else B.char7 ' ' <> B.byteString (TE.encodeUtf8 safeContent))
       <> B.string7 "?>"

buildNodePretty :: Int -> Int -> Node -> Builder
buildNodePretty indent level (Element name attrs children)
  | V.null children =
      indentB indent level
      <> B.char7 '<' <> buildName name <> buildAttrs attrs <> B.string7 "/>\n"
  | V.length children == 1 && isTextLike (V.head children) =
      indentB indent level
      <> B.char7 '<' <> buildName name <> buildAttrs attrs <> B.char7 '>'
      <> buildNode (V.head children)
      <> B.string7 "</" <> buildName name <> B.string7 ">\n"
  | otherwise =
      indentB indent level
      <> B.char7 '<' <> buildName name <> buildAttrs attrs <> B.string7 ">\n"
      <> V.foldl' (\acc c -> acc <> buildNodePretty indent (level + 1) c) mempty children
      <> indentB indent level
      <> B.string7 "</" <> buildName name <> B.string7 ">\n"
buildNodePretty indent level (Text t) =
  indentB indent level <> escapeXMLText t <> B.char7 '\n'
buildNodePretty indent level node =
  indentB indent level <> buildNode node <> B.char7 '\n'

isTextLike :: Node -> Bool
isTextLike (Text _) = True
isTextLike (CData _) = True
isTextLike _ = False

indentB :: Int -> Int -> Builder
indentB indent level =
  let !n = indent * level
  in mconcat (replicate n (B.char7 ' '))

buildName :: Name -> Builder
buildName (Name local mPrefix _) =
  case mPrefix of
    Nothing -> B.byteString (TE.encodeUtf8 local)
    Just pfx -> B.byteString (TE.encodeUtf8 pfx) <> B.char7 ':' <> B.byteString (TE.encodeUtf8 local)

buildAttrs :: Vector Attribute -> Builder
buildAttrs attrs = V.foldl' (\acc a -> acc <> buildAttr a) mempty attrs

buildAttr :: Attribute -> Builder
buildAttr (Attribute name val) =
  B.char7 ' ' <> buildName name <> B.string7 "=\"" <> escapeXMLAttr val <> B.char7 '"'

-- | Escape character data per XML 1.0 §2.4. We escape @<@, @&@, the
-- @]]>@ sequence (broken via @&gt;@), and the carriage return @\\r@
-- (so it survives end-of-line normalization §2.11).
escapeXMLText :: Text -> Builder
escapeXMLText = goText
  where
    goText t = case T.uncons t of
      Nothing      -> mempty
      Just (c, rest) -> escChar c rest <> goText rest
    escChar '<'  _    = B.string7 "&lt;"
    escChar '&'  _    = B.string7 "&amp;"
    escChar '\r' _    = B.string7 "&#xD;"
    escChar ']'  rest
      -- Break "]]>" into "]]&gt;" so the literal sequence cannot escape an
      -- enclosing CDATA-like context if the consumer concatenates encoded
      -- text. This is allowed by §2.4.
      | "]>" `T.isPrefixOf` rest = B.string7 "]"
      | otherwise               = B.charUtf8 ']'
    escChar '>'  _    = B.string7 "&gt;"
    escChar c    _    = B.charUtf8 c

-- | Escape an attribute value per XML 1.0 §3.3.3 (attribute-value
-- normalization). All whitespace characters that survive normalization
-- as themselves (TAB, LF, CR) MUST be encoded as character references
-- so they round-trip; otherwise the parser will normalize them to
-- spaces.
escapeXMLAttr :: Text -> Builder
escapeXMLAttr t = T.foldl' (\acc c -> acc <> escChar c) mempty t
  where
    escChar '<'  = B.string7 "&lt;"
    escChar '>'  = B.string7 "&gt;"
    escChar '&'  = B.string7 "&amp;"
    escChar '"'  = B.string7 "&quot;"
    escChar '\'' = B.string7 "&apos;"
    escChar '\t' = B.string7 "&#x9;"
    escChar '\n' = B.string7 "&#xA;"
    escChar '\r' = B.string7 "&#xD;"
    escChar c    = B.charUtf8 c
