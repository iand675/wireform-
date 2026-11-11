{-# LANGUAGE BangPatterns #-}

{- | Pretty-printing and exact-printing for protobuf ASTs.

Two modes:

* 'printProtoFile' — pretty-print a semantic AST with normalized
  formatting (2-space indent). Useful for code generation,
  formatting, and debugging.

* 'exactPrint' — reproduce the original source byte-for-byte for
  unmodified 'Parsed'-phase nodes, falling back to pretty-printing
  for modified or programmatically constructed nodes.
-}
module Proto.IDL.Print (
  -- * Pretty-printing (normalized formatting)
  printProtoFile,
  printTopLevel,
  printMessage,
  printEnum,
  printService,
  printField,
  printOption,
  printConstant,
  printProtoFloat,
  escapeProtoBytes,
  printComment,
  printCommentBlock,

  -- * Exact-printing (source-faithful)
  exactPrint,
  clearSpan,
) where

import Data.ByteString qualified as BS
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.RealFloat qualified as TBF
import Data.Set qualified as Set
import Data.Word (Word8)
import Proto.IDL.AST


-- | Render a complete proto file to source text.
printProtoFile :: ProtoFile -> Text
printProtoFile pf =
  T.intercalate "\n" $
    filter (not . T.null) $
      [printSyntax (protoSyntax pf)]
        <> maybe [] (\pkg -> ["", "package " <> pkg <> ";"]) (protoPackage pf)
        <> (if null (protoImports pf) then [] else "" : fmap printImport (protoImports pf))
        <> (if null (protoOptions pf) then [] else "" : fmap printTopLevelOption (protoOptions pf))
        <> concatMap render (protoTopLevels pf)
        <> [""]
  where
    groups = groupBodies (concatMap extensionFields (protoTopLevels pf)) (mapMaybe topMessage (protoTopLevels pf))
    extensionFields (TLExtend _ fields) = fields
    extensionFields _ = []
    topMessage (TLMessage message) = Just message
    topMessage _ = Nothing
    render (TLMessage message) | Map.member (msgName message) groups = []
    render definition = ["", printTopLevelWith groups definition]


printSyntax :: Syntax -> Text
printSyntax Proto2 = "syntax = \"proto2\";"
printSyntax Proto3 = "syntax = \"proto3\";"
printSyntax (Editions ed) = "edition = \"" <> editionName ed <> "\";"


printImport :: ImportDef -> Text
printImport (ImportDef _span modifier path) =
  "import " <> maybe "" printImportMod modifier <> "\"" <> path <> "\";"


printImportMod :: ImportModifier -> Text
printImportMod ImportPublic = "public "
printImportMod ImportWeak = "weak "
printImportMod ImportOption = "option "


printTopLevelOption :: OptionDef -> Text
printTopLevelOption opt = "option " <> printOptionAssignment opt <> ";"


-- | Render a top-level definition.
printTopLevel :: TopLevel -> Text
printTopLevel = printTopLevelWith Map.empty


printTopLevelWith :: Map.Map Text MessageDef -> TopLevel -> Text
printTopLevelWith groups = \case
  TLMessage msg -> printMessage 0 msg
  TLEnum ed -> printEnum 0 ed
  TLService svc -> printService svc
  TLExtend name fields ->
    "extend "
      <> name
      <> " {\n"
      <> foldMap (printFieldWithGroup groups 1) fields
      <> "}"
  TLOption opt -> "option " <> printOptionAssignment opt <> ";"
  TLComment cs -> printCommentBlock 0 cs


-- | Render a message definition at a given indentation depth.
printMessage :: Int -> MessageDef -> Text
printMessage depth msg =
  printDoc depth (msgDoc msg)
    <> indent depth
    <> "message "
    <> msgName msg
    <> " {\n"
    <> printMessageElements (depth + 1) (msgElements msg)
    <> indent depth
    <> "}"


printMessageElements :: Int -> [MessageElement] -> Text
printMessageElements depth elements = foldMap (printMessageElement groups depth) elements
  where
    groups = groupBodies (concatMap fields elements) (mapMaybe message elements)
    fields (MEField field) = [field]
    fields (MEExtend _ extensions) = extensions
    fields _ = []
    message (MEMessage child) = Just child
    message _ = Nothing


printMessageElement :: Map.Map Text MessageDef -> Int -> MessageElement -> Text
printMessageElement groups depth = \case
  MEField fd -> printFieldWithGroup groups depth fd
  MEEnum ed -> printEnum depth ed <> "\n"
  MEMessage msg
    | Map.member (msgName msg) groups -> ""
    | otherwise -> printMessage depth msg <> "\n"
  MEOneof od -> printOneof depth od <> "\n"
  MEMapField mf -> printDoc depth (mapDoc mf) <> indent depth <> printMapField mf <> "\n"
  MEReserved rd -> indent depth <> printReserved rd <> "\n"
  MEExtensions exs opts -> indent depth <> "extensions " <> printExtensionRanges exs <> printFieldOptions opts <> ";\n"
  MEExtend name fields ->
    indent depth
      <> "extend "
      <> name
      <> " {\n"
      <> foldMap (printFieldWithGroup groups (depth + 1)) fields
      <> indent depth
      <> "}\n"
  MEOption opt -> indent depth <> "option " <> printOptionAssignment opt <> ";\n"
  MEComment cs -> printCommentBlock depth cs <> "\n"


groupBodies :: [FieldDef] -> [MessageDef] -> Map.Map Text MessageDef
groupBodies fields = Map.fromList . mapMaybe groupBody
  where
    names = Set.fromList (mapMaybe legacyGroupName fields)
    groupBody message
      | Set.member (msgName message) names = Just (msgName message, message)
      | otherwise = Nothing


legacyGroupName :: FieldDef -> Maybe Text
legacyGroupName field
  | isLegacyGroupField field, FTNamed name <- fieldType field = Just (T.takeWhileEnd (/= '.') name)
  | otherwise = Nothing


printFieldWithGroup :: Map.Map Text MessageDef -> Int -> FieldDef -> Text
printFieldWithGroup groups depth field =
  printDoc depth (fieldDoc field) <> indent depth <> case legacyGroupName field >>= (`Map.lookup` groups) of
    Just body ->
      maybe "" (\label -> printLabel label <> " ") (fieldLabel field)
        <> "group " <> msgName body <> " = " <> intToText (unFieldNumber (fieldNumber field))
        <> printFieldOptions (fieldOptions field) <> " {\n"
        <> printMessageElements (depth + 1) (msgElements body) <> indent depth <> "}\n"
    Nothing -> printField field <> "\n"


-- | Render a field definition.
printField :: FieldDef -> Text
printField fd =
  maybe "" (\l -> printLabel l <> " ") (fieldLabel fd)
    <> printFieldType (fieldType fd)
    <> " "
    <> fieldName fd
    <> " = "
    <> intToText (unFieldNumber (fieldNumber fd))
    <> printFieldOptions (fieldOptions fd)
    <> ";"


printLabel :: FieldLabel -> Text
printLabel Optional = "optional"
printLabel Required = "required"
printLabel Repeated = "repeated"


printFieldType :: FieldType -> Text
printFieldType = \case
  FTScalar s -> printScalarType s
  FTNamed n -> n


printScalarType :: ScalarType -> Text
printScalarType = \case
  SDouble -> "double"
  SFloat -> "float"
  SInt32 -> "int32"
  SInt64 -> "int64"
  SUInt32 -> "uint32"
  SUInt64 -> "uint64"
  SSInt32 -> "sint32"
  SSInt64 -> "sint64"
  SFixed32 -> "fixed32"
  SFixed64 -> "fixed64"
  SSFixed32 -> "sfixed32"
  SSFixed64 -> "sfixed64"
  SBool -> "bool"
  SString -> "string"
  SBytes -> "bytes"


printMapField :: MapField -> Text
printMapField mf =
  "map<"
    <> printScalarType (mapKeyType mf)
    <> ", "
    <> printFieldType (mapValueType mf)
    <> "> "
    <> mapFieldName mf
    <> " = "
    <> intToText (unFieldNumber (mapFieldNum mf))
    <> printFieldOptions (mapOptions mf)
    <> ";"


printOneof :: Int -> OneofDef -> Text
printOneof depth od =
  printDoc depth (oneofDoc od)
    <> indent depth
    <> "oneof "
    <> oneofName od
    <> " {\n"
    <> T.concat (fmap (\opt -> indent (depth + 1) <> "option " <> printOptionAssignment opt <> ";\n") (oneofOptions od))
    <> T.concat (fmap (\f -> printDoc (depth + 1) (oneofFieldDoc f) <> indent (depth + 1) <> printOneofField f <> "\n") (oneofFields od))
    <> indent depth
    <> "}"


printOneofField :: OneofField -> Text
printOneofField f =
  printFieldType (oneofFieldType f)
    <> " "
    <> oneofFieldName f
    <> " = "
    <> intToText (unFieldNumber (oneofFieldNumber f))
    <> printFieldOptions (oneofFieldOptions f)
    <> ";"


printFieldOptions :: [OptionDef] -> Text
printFieldOptions [] = ""
printFieldOptions opts =
  " [" <> T.intercalate ", " (fmap printOptionAssignment opts) <> "]"


-- | Render an enum definition.
printEnum :: Int -> EnumDef -> Text
printEnum depth ed =
  printDoc depth (enumDoc ed)
    <> indent depth
    <> "enum "
    <> enumName ed
    <> " {\n"
    <> T.concat (fmap (\opt -> indent (depth + 1) <> "option " <> printOptionAssignment opt <> ";\n") (enumOptions ed))
    <> T.concat (fmap (\v -> printDoc (depth + 1) (evDoc v) <> indent (depth + 1) <> printEnumValue v <> "\n") (enumValues ed))
    <> indent depth
    <> "}"


printEnumValue :: EnumValue -> Text
printEnumValue ev =
  evName ev
    <> " = "
    <> intToText (evNumber ev)
    <> printFieldOptions (evOptions ev)
    <> ";"


-- | Render a service definition.
printService :: ServiceDef -> Text
printService svc =
  printDoc 0 (svcDoc svc)
    <> "service "
    <> svcName svc
    <> " {\n"
    <> T.concat (fmap (\opt -> indent 1 <> "option " <> printOptionAssignment opt <> ";\n") (svcOptions svc))
    <> T.concat (fmap (\rpc -> printDoc 1 (rpcDoc rpc) <> indent 1 <> printRpc rpc <> "\n") (svcRpcs svc))
    <> "}"


printRpc :: RpcDef -> Text
printRpc rpc =
  "rpc "
    <> rpcName rpc
    <> " ("
    <> printStream (rpcInputStr rpc)
    <> rpcInput rpc
    <> ")"
    <> " returns ("
    <> printStream (rpcOutputStr rpc)
    <> rpcOutput rpc
    <> ")"
    <> case rpcOptions rpc of
      [] -> ";"
      opts ->
        " {\n"
          <> T.concat (fmap (\opt -> indent 2 <> "option " <> printOptionAssignment opt <> ";\n") opts)
          <> indent 1
          <> "}"


printStream :: StreamQualifier -> Text
printStream NoStream = ""
printStream Streaming = "stream "


-- | Render an option assignment (name = value).
printOption :: OptionDef -> Text
printOption = printOptionAssignment


printOptionAssignment :: OptionDef -> Text
printOptionAssignment opt =
  printOptionName (optName opt) <> " = " <> printConstant (optValue opt)


printOptionName :: OptionName -> Text
printOptionName (OptionName parts) = T.intercalate "." (fmap printOptionNamePart parts)


printOptionNamePart :: OptionNamePart -> Text
printOptionNamePart = \case
  SimpleOption name -> name
  ExtensionOption name -> "(" <> name <> ")"


-- | Render a constant value.
printConstant :: Constant -> Text
printConstant = \case
  CIdent t -> t
  CSpelledInt spelling _ -> spelling
  CInt n -> integerToText n
  CFloat d -> printProtoFloat d
  CString s -> "\"" <> escapeProtoString s <> "\""
  CBytes bytes -> "\"" <> escapeProtoBytes bytes <> "\""
  CBool True -> "true"
  CBool False -> "false"
  CAggregate kvs -> "{ " <> T.intercalate " " (fmap printAggField kvs) <> " }"
  where
    printAggField (k, v) = k <> ": " <> printConstant v


-- | Format a protobuf floating-point literal using its native precision.
printProtoFloat :: RealFloat a => a -> Text
printProtoFloat number
  | isNaN number = "nan"
  | isInfinite number = if number < 0 then "-inf" else "inf"
  | otherwise = TL.toStrict (TB.toLazyText (TBF.realFloat number))


-- | C-escaped contents of a protobuf bytes literal, without delimiters.
escapeProtoBytes :: BS.ByteString -> Text
escapeProtoBytes = TL.toStrict . TB.toLazyText . BS.foldr (\byte rest -> byteEscape byte <> rest) mempty
  where
    byteEscape :: Word8 -> TB.Builder
    byteEscape 9 = "\\t"
    byteEscape 10 = "\\n"
    byteEscape 13 = "\\r"
    byteEscape 34 = "\\\""
    byteEscape 39 = "\\'"
    byteEscape 92 = "\\\\"
    byteEscape byte
      | byte >= 32 && byte <= 126 = TB.singleton (toEnum (fromIntegral byte))
      | otherwise =
          let (high, low) = fromIntegral byte `quotRem` 8
              (first, second) = high `quotRem` 8
          in "\\" <> digit first <> digit second <> digit low
    digit n = TB.singleton (toEnum (fromEnum '0' + n))


escapeProtoString :: Text -> Text
escapeProtoString = T.concatMap escChar
  where
    escChar '"' = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\r' = "\\r"
    escChar '\t' = "\\t"
    escChar '\0' = "\\000"
    escChar c = T.singleton c


printReserved :: ReservedDef -> Text
printReserved = \case
  ReservedNumbers ranges ->
    "reserved " <> T.intercalate ", " (fmap printReservedRange ranges) <> ";"
  ReservedNames names ->
    "reserved " <> T.intercalate ", " (fmap printReservedName names) <> ";"


printReservedName :: ReservedName -> Text
printReservedName = \case
  QuotedReservedName n -> "\"" <> escapeProtoString n <> "\""
  IdentReservedName n -> n


printReservedRange :: ReservedRange -> Text
printReservedRange = \case
  ReservedSingle n -> intToText n
  ReservedRange lo hi -> intToText lo <> " to " <> intToText hi


printExtensionRanges :: [ExtensionRange] -> Text
printExtensionRanges = T.intercalate ", " . fmap printExtRange
  where
    printExtRange er =
      intToText (extStart er)
        <> case extEnd er of
          ExtBoundNum n | n == extStart er -> ""
          ExtBoundNum n -> " to " <> intToText n
          ExtBoundMax -> " to max"


-- | Render a doc comment as proto // lines at a given indentation.
printDoc :: Int -> Maybe Text -> Text
printDoc _ Nothing = ""
printDoc depth (Just doc) =
  T.concat (fmap (\l -> indent depth <> "// " <> l <> "\n") (T.lines doc))


-- | Render a block of comments at a given indentation.
printCommentBlock :: Int -> [Comment] -> Text
printCommentBlock depth cs =
  T.concat (fmap (\c -> indent depth <> printComment c <> "\n") cs)


-- | Render a single comment.
printComment :: Comment -> Text
printComment (LineComment content) = "//" <> content
printComment (BlockComment content) = "/*" <> content <> "*/"


indent :: Int -> Text
indent n = T.replicate (n * 2) " "


intToText :: Int -> Text
intToText n
  | n < 0 = "-" <> intToText (negate n)
  | n < 10 = T.singleton (toEnum (n + 48))
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (toEnum (r + 48)) acc) q


integerToText :: Integer -> Text
integerToText n
  | n < 0 = "-" <> integerToText (negate n)
  | n < 10 = T.singleton (toEnum (fromIntegral n + 48))
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (toEnum (fromIntegral r + 48)) acc) q


-- -----------------------------------------------------------------------
-- Exact-printing
-- -----------------------------------------------------------------------

{- | Exact-print a parsed proto file.

If the file was parsed with 'Proto.IDL.Parser.parseProtoFileWithSpans'
and has not been modified, reproduces the original source
byte-for-byte. Modified nodes (with cleared spans) are
pretty-printed. Falls back to full pretty-print if no source
text is available.
-}
exactPrint :: ProtoFile' Parsed -> Text
exactPrint pf = case protoSource pf of
  Nothing -> printProtoFile (stripSpans pf)
  Just src -> reassemble src pf


{- | Clear a span, marking the node as modified so 'exactPrint'
will pretty-print it instead of using the original source.
-}
clearSpan :: Span -> Span
clearSpan _ = noSpan


data Region
  = Original {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | Replacement !Text


reassemble :: Text -> ProtoFile' Parsed -> Text
reassemble src pf =
  let regions = collectRegions pf
      sorted = sortBy (comparing regionStart) regions
  in emitRegions src 0 sorted


regionStart :: Region -> Int
regionStart (Original s _) = s
regionStart (Replacement _) = maxBound


emitRegions :: Text -> Int -> [Region] -> Text
emitRegions src !cursor [] = T.drop cursor src
emitRegions src !cursor (r : rs) = case r of
  Original start end ->
    let gap = sliceText cursor start src
        body = sliceText start end src
    in gap <> body <> emitRegions src end rs
  Replacement txt ->
    txt <> emitRegions src cursor rs


sliceText :: Int -> Int -> Text -> Text
sliceText start end src = T.take (end - start) (T.drop start src)


collectRegions :: ProtoFile' Parsed -> [Region]
collectRegions pf =
  concatMap importRegion (protoImports pf)
    <> concatMap optionRegion (protoOptions pf)
    <> concatMap topLevelRegion (protoTopLevels pf)


importRegion :: ImportDef' Parsed -> [Region]
importRegion imp = spanToRegion (importExt imp) Nothing


optionRegion :: OptionDef' Parsed -> [Region]
optionRegion opt = spanToRegion (optExt opt) Nothing


topLevelRegion :: TopLevel' Parsed -> [Region]
topLevelRegion tl = case tl of
  TLMessage m -> spanToRegion (msgExt m) (Just $ printTopLevel (TLMessage (stripMessage m)))
  TLEnum e -> spanToRegion (enumExt e) (Just $ printTopLevel (TLEnum (stripEnum e)))
  TLService s -> spanToRegion (svcExt s) (Just $ printTopLevel (TLService (stripService s)))
  TLExtend _ _ -> [Replacement (printTopLevel (stripTopLevel tl))]
  TLOption o -> spanToRegion (optExt o) Nothing
  TLComment _ -> [] -- Comment nodes are reconstructed from source gaps by exactPrint


spanToRegion :: Span -> Maybe Text -> [Region]
spanToRegion (Span (Just (SrcSpan s e))) _ = [Original s e]
spanToRegion (Span Nothing) (Just txt) = [Replacement txt]
spanToRegion (Span Nothing) Nothing = []
