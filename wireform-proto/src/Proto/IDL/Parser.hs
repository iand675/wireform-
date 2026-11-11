{- | Parser for the Protocol Buffers IDL (.proto files).

Supports proto2, proto3, and Editions (2023+) syntax including messages,
enums, services, oneofs, map fields, extensions, imports, packages,
and custom options.
-}
module Proto.IDL.Parser (
  parseProtoFile,
  parseProtoFileWithSpans,
  parseProto,
  Parser,
  renderParseError,
  renderParseErrors,
) where

import Control.Monad (void)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Void (Void)
import Proto.IDL.AST
import Proto.IDL.AST.Span (Span (..), SrcSpan (..), mkSpan)
import Proto.IDL.Parser.Error (renderParseError, renderParseErrors)
import Proto.IDL.Parser.Lexer
import Text.Megaparsec hiding (option)
import Text.Megaparsec.Char ()


-- | Get the doc comment for the current source position from the comment map.
getDoc :: CommentMap -> Parser (Maybe Text)
getDoc cm = do
  pos <- getSourcePos
  let line = unPos (sourceLine pos)
  pure (lookupDoc cm line)


{- | Parse a .proto file, returning a semantic AST (no source spans).
This is the standard entry point for codegen, analysis, etc.
-}
parseProtoFile :: FilePath -> Text -> Either (ParseErrorBundle Text Void) ProtoFile
parseProtoFile fp src = stripSpans <$> parseProtoFileWithSpans fp src


{- | Parse a .proto file, returning a 'Parsed'-phase AST with source
spans and the original source text. Use this for exactprint.
-}
parseProtoFileWithSpans :: FilePath -> Text -> Either (ParseErrorBundle Text Void) (ProtoFile' Parsed)
parseProtoFileWithSpans fp src =
  let cm = buildCommentMap src
      allComments = buildAllComments src
  in case parse (parseProto cm) fp src of
       Left e -> Left e
       Right pf ->
         let pf' = insertComments src allComments cm pf
         in Right pf' {protoSource = Just src}


-- | Core parser for a proto file given a pre-built comment map.
parseProto :: CommentMap -> Parser (ProtoFile' Parsed)
parseProto cm = do
  sc
  syn <- option Proto3 syntaxOrEdition
  stmts <- manyStmts (topLevelStmt cm syn)
  eof
  let pkg = firstJust (\case TLStmtPackage p -> Just p; _ -> Nothing) stmts
  let imps = concatMap (\case TLStmtImport i -> [i]; _ -> []) stmts
  let opts = concatMap (\case TLStmtOption o -> [o]; _ -> []) stmts
  let topDefs = concatMap (\case TLStmtTopLevel t -> [t]; TLStmtTopLevels ts -> ts; _ -> []) stmts
  validateOptionImports imps
  pure
    ProtoFile
      { protoSyntax = syn
      , protoPackage = pkg
      , protoImports = imps
      , protoOptions = opts
      , protoTopLevels = topDefs
      , protoSource = Nothing
      }


validateOptionImports :: [ImportDef' Parsed] -> Parser ()
validateOptionImports imports
  | any optionOnly imports = go Set.empty False imports
  | otherwise = pure ()
  where
    optionOnly imp = importModifier imp == Just ImportOption
    go _ _ [] = pure ()
    go seen afterOption (imp : rest)
      | afterOption && not (optionOnly imp) = atImport imp "ordinary imports must precede option imports"
      | Set.member (importPath imp) seen = atImport imp "import was listed twice"
      | otherwise = go (Set.insert (importPath imp) seen) (afterOption || optionOnly imp) rest
    atImport :: ImportDef' Parsed -> String -> Parser ()
    atImport imp message = do
      mapM_ (setOffset . spanStart) (unSpan (importExt imp))
      fail message


firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x : xs) = case f x of
  Just v -> Just v
  Nothing -> firstJust f xs


{- | Parse zero or more statements, tolerating empty statements (bare @;@).

The proto grammar models a stray semicolon as @emptyStatement = ";"@, which
@protoc@ accepts wherever a statement may appear. The most common case is the
trailing @;@ that follows a message, enum, or service block (e.g.
@message Foo { ... };@), but bare semicolons are also valid between and inside
declarations.
-}
manyStmts :: Parser a -> Parser [a]
manyStmts p = catMaybes <$> many ((Nothing <$ semi) <|> (Just <$> p))


data TLStmt
  = TLStmtPackage Text
  | TLStmtImport (ImportDef' Parsed)
  | TLStmtOption (OptionDef' Parsed)
  | TLStmtTopLevel (TopLevel' Parsed)
  | {- | One statement that expands to several top-level definitions, e.g. an
    @extend@ block containing a group (which hoists the group's message).
    -}
    TLStmtTopLevels [TopLevel' Parsed]


syntaxOrEdition :: Parser Syntax
syntaxOrEdition = syntaxDecl <|> editionDecl


syntaxDecl :: Parser Syntax
syntaxDecl = do
  reserved "syntax"
  equals
  s <- stringLiteral
  case s of
    "proto2" -> semi >> pure Proto2
    "proto3" -> semi >> pure Proto3
    _ ->
      fail
        ( "unknown syntax \""
            <> T.unpack s
            <> "\": expected \"proto2\" or \"proto3\""
        )


editionDecl :: Parser Syntax
editionDecl = do
  reserved "edition"
  equals
  ed <- stringLiteral
  semi
  pure (Editions (Edition ed))


topLevelStmt :: CommentMap -> Syntax -> Parser TLStmt
topLevelStmt cm syn = do
  start <- getOffset
  doc <- getDoc cm
  kw <- lookAhead (identifier <?> "top-level declaration (message, enum, service, import, package, or option)")
  case kw of
    "package" -> TLStmtPackage <$> packageDecl
    "import" -> TLStmtImport <$> importDecl syn start
    "option" -> TLStmtOption <$> optionDecl start
    "message" -> TLStmtTopLevel . TLMessage <$> messageDef cm doc start
    "enum" -> TLStmtTopLevel . TLEnum <$> enumDef cm doc start
    "service" -> TLStmtTopLevel . TLService <$> serviceDef cm doc start
    "extend" -> TLStmtTopLevels <$> extendDef cm
    _ ->
      fail
        ( "unexpected keyword '"
            <> T.unpack kw
            <> "', expected one of: message, enum, service, import, package, option, or extend"
        )


extendDef :: CommentMap -> Parser [TopLevel' Parsed]
extendDef cm = do
  reserved "extend"
  name <- fullIdent
  (fields, groupMsgs) <- extendBody cm
  -- A group inside an extend hoists its message definition to the enclosing
  -- (here, file) scope, matching how protoc models groups.
  pure (TLExtend name fields : fmap TLMessage groupMsgs)


{- | Parse the body of an @extend@ block: fields and (proto2) groups, plus
empty statements. Returns the extension fields and any group message
definitions that must be hoisted to the enclosing scope.
-}
extendBody :: CommentMap -> Parser ([FieldDef' Parsed], [MessageDef' Parsed])
extendBody cm = braces (mconcat <$> manyStmts (extendElem cm))


extendElem :: CommentMap -> Parser ([FieldDef' Parsed], [MessageDef' Parsed])
extendElem cm = do
  start <- getOffset
  doc <- getDoc cm
  lbl <- optional fieldLabelP
  isGroup <- option False (True <$ try (reserved "group"))
  if isGroup
    then do
      (nested, fld) <- groupParts cm doc lbl start
      pure ([fld], [nested])
    else do
      f <- fieldRest doc lbl start
      pure ([f], [])


packageDecl :: Parser Text
packageDecl = do
  reserved "package"
  pkg <- fullIdent
  semi
  pure pkg


importDecl :: Syntax -> Int -> Parser (ImportDef' Parsed)
importDecl syn start = do
  reserved "import"
  modifier <-
    optional $
      choice
        [ ImportPublic <$ reserved "public"
        , ImportWeak <$ reserved "weak"
        , do
            reserved "option"
            case syn of
              Editions (Edition edition)
                | Right (year, "") <- TR.decimal edition
                , year >= (2024 :: Integer) -> pure ImportOption
              _ -> fail "option import requires edition 2024 or later"
        ]
  path <- stringLiteral
  semi
  end <- getOffset
  pure ImportDef {importExt = mkSpan start end, importModifier = modifier, importPath = path}


optionDecl :: Int -> Parser (OptionDef' Parsed)
optionDecl start = do
  reserved "option"
  opt <- optionAssignment
  semi
  end <- getOffset
  pure opt {optExt = mkSpan start end}


optionAssignment :: Parser (OptionDef' Parsed)
optionAssignment = optionAssignmentWith (const constant)


optionAssignmentWith :: (OptionName -> Parser Constant) -> Parser (OptionDef' Parsed)
optionAssignmentWith valueParser = do
  start <- getOffset
  name <- optionName
  equals
  val <- valueParser name
  end <- getOffset
  pure OptionDef {optExt = mkSpan start end, optName = name, optValue = val}


optionName :: Parser OptionName
optionName = do
  first <- optionNamePart
  -- Parts after the first may also be extension names in parentheses,
  -- e.g. @option (a).(b).c = …@, which protoc accepts.
  rest <- many (symbol "." *> optionNamePart)
  pure OptionName {optNameParts = first : rest}


optionNamePart :: Parser OptionNamePart
optionNamePart =
  choice
    [ ExtensionOption <$> parens fullIdent
    , SimpleOption <$> identifier
    ]


constant :: Parser Constant
constant =
  choice
    [ CBool <$> try boolLiteral
    , quotedConstant
    , try (CFloat <$> floatLiteral)
    , try integerConstant
    , CAggregate <$> aggregateLiteral
    , CIdent <$> fullIdent
    ]
    <?> "constant value (string, number, boolean, identifier, or aggregate)"
  where
    quotedConstant = do
      bytes <- bytesLiteral
      pure (either (const (CBytes bytes)) CString (TE.decodeUtf8' bytes))
    integerConstant = do
      (spelling, number) <- intLiteralWithSpelling
      let digits = T.dropWhile (== '-') spelling
          signedZero = number == 0 && T.isPrefixOf "-" spelling
          nonDecimal = T.isPrefixOf "0" digits && T.compareLength digits 1 == GT
      pure (if signedZero || nonDecimal then CSpelledInt spelling number else CInt number)


aggregateLiteral :: Parser [(Text, Constant)]
aggregateLiteral = aggBody braces <|> aggBody angles
  where
    -- A message value may be delimited by braces or, in text format, angles.
    aggBody delim = delim (concat <$> many aggregateField)

    aggregateField :: Parser [(Text, Constant)]
    aggregateField = do
      key <- aggKey
      vals <- aggValue
      _ <- optional (comma <|> semi)
      pure (fmap ((,) key) vals)

    -- A field key is a bare identifier or a bracketed extension / Any-URL name
    -- (e.g. @[foo.bar]@ or @[type.googleapis.com/foo.Bar]@). The brackets are
    -- kept in the stored key so it round-trips.
    aggKey = aggExtKey <|> identifier
    aggExtKey = do
      void (symbol "[")
      d <- fullIdent
      suffix <- option "" ((\t -> "/" <> t) <$> (symbol "/" *> fullIdent))
      void (symbol "]")
      pure ("[" <> d <> suffix <> "]")

    -- "key: <scalar|list|message>" or the colon-less "key { … }" / "key < … >".
    aggValue =
      (symbol ":" *> (aggListValue <|> ((: []) <$> constant)))
        <|> ((: []) . CAggregate <$> aggregateLiteral)

    -- List values "[a, b, c]" desugar into repeated (key, value) pairs — the
    -- text-format equivalent of writing the key several times.
    aggListValue = brackets (constant `sepBy` comma)


messageDef :: CommentMap -> Maybe Text -> Int -> Parser (MessageDef' Parsed)
messageDef cm doc start = do
  reserved "message"
  name <- identifier <?> "message name"
  elems <- braces (concat <$> manyStmts (messageElement cm))
  end <- getOffset
  pure MessageDef {msgExt = mkSpan start end, msgDoc = doc, msgName = name, msgElements = elems}


{- | Parse one message-body element. Returns a list because a proto2 @group@
desugars into two elements (the nested message plus its field); every other
element yields a singleton.
-}
messageElement :: CommentMap -> Parser [MessageElement' Parsed]
messageElement cm = do
  start <- getOffset
  doc <- getDoc cm
  kw <- lookAhead ((identifier <|> symbol ".") <?> "message element (field, group, enum, message, oneof, map, option, reserved, or extensions)")
  case kw of
    "reserved" -> one (MEReserved <$> reservedDecl)
    "extensions" -> one (uncurry MEExtensions <$> extensionsDecl)
    "option" -> one (MEOption <$> optionDecl start)
    "enum" -> one (MEEnum <$> enumDef cm doc start)
    "message" -> one (MEMessage <$> messageDef cm doc start)
    "oneof" -> one (MEOneof <$> oneofDef cm doc start)
    "map" -> one (MEMapField <$> mapFieldDef cm doc start)
    "extend" -> nestedExtend cm
    _ -> fieldOrGroupDef cm doc start
  where
    one = fmap (: [])


{- | Parse a nested @extend@ block inside a message body. Yields the
'MEExtend' element plus any hoisted group messages (as sibling
'MEMessage' elements in the enclosing message).
-}
nestedExtend :: CommentMap -> Parser [MessageElement' Parsed]
nestedExtend cm = do
  reserved "extend"
  name <- fullIdent
  (fields, groupMsgs) <- extendBody cm
  pure (MEExtend name fields : fmap MEMessage groupMsgs)


-- | A field label (proto2 @optional@/@required@ or @repeated@).
fieldLabelP :: Parser FieldLabel
fieldLabelP =
  choice
    [ try (Optional <$ reserved "optional")
    , try (Required <$ reserved "required")
    , try (Repeated <$ reserved "repeated")
    ]
    <?> "field label (optional, required, or repeated)"


-- | Parse a field given its (already-consumed) label.
fieldRest :: Maybe Text -> Maybe FieldLabel -> Int -> Parser (FieldDef' Parsed)
fieldRest doc lbl start = do
  ft <- parseFieldType
  name <- identifier <?> "field name"
  equals
  num <- FieldNumber . fromIntegral <$> (intLiteral <?> "field number")
  opts <- fieldOptionsWith (optionAssignmentWith (fieldOptionValue ft))
  semi
  end <- getOffset
  pure
    FieldDef
      { fieldExt = mkSpan start end
      , fieldDoc = doc
      , fieldLabel = lbl
      , fieldType = ft
      , fieldName = name
      , fieldNumber = num
      , fieldOptions = opts
      }


fieldOptionValue :: FieldType -> OptionName -> Parser Constant
fieldOptionValue (FTScalar SBytes) (OptionName [SimpleOption "default"]) = CBytes <$> bytesLiteral
fieldOptionValue (FTScalar SString) (OptionName [SimpleOption "default"]) = CString <$> stringLiteral
fieldOptionValue (FTScalar SFloat) (OptionName [SimpleOption "default"]) = floatingDefault
fieldOptionValue (FTScalar SDouble) (OptionName [SimpleOption "default"]) = floatingDefault
fieldOptionValue _ _ = constant


floatingDefault :: Parser Constant
floatingDefault = do
  (source, value) <- match constant
  case value of
    CInt 0 | T.isPrefixOf "-" source -> pure (CFloat (-0.0))
    _ -> pure value


{- | Parse either a regular field or a proto2 @group@.

A group (@[label] group Name = N [opts] { body }@) is deprecated syntax that
is semantically a nested message plus a field of that message type. We desugar
it the way @protoc@ does: emit the nested message named @Name@ and a field
named after the lower-cased group name. The field retains the group marker
so the generators can emit matching start and end tags.
-}
fieldOrGroupDef :: CommentMap -> Maybe Text -> Int -> Parser [MessageElement' Parsed]
fieldOrGroupDef cm doc start = do
  lbl <- optional fieldLabelP
  isGroup <- option False (True <$ try (reserved "group"))
  if isGroup
    then (\(nested, fld) -> [MEMessage nested, MEField fld]) <$> groupParts cm doc lbl start
    else fmap ((: []) . MEField) (fieldRest doc lbl start)


{- | Parse a group body (the @group@ keyword has already been consumed) into
its constituent nested message and field, the way protoc models a group.
The @group@ field name is the lower-cased group name.
-}
groupParts :: CommentMap -> Maybe Text -> Maybe FieldLabel -> Int -> Parser (MessageDef' Parsed, FieldDef' Parsed)
groupParts cm doc lbl start = do
  name <- identifier <?> "group name"
  equals
  num <- FieldNumber . fromIntegral <$> (intLiteral <?> "field number")
  opts <- fieldOptionList
  elems <- braces (concat <$> manyStmts (messageElement cm))
  end <- getOffset
  let sp = mkSpan start end
      nested =
        MessageDef
          { msgExt = sp
          , msgDoc = doc
          , msgName = name
          , msgElements = elems
          }
      fld =
        GroupFieldDef
          { fieldExt = sp
          , fieldDoc = doc
          , fieldLabel = lbl
          , fieldType = FTNamed name
          , fieldName = T.toLower name
          , fieldNumber = num
          , fieldOptions = opts
          }
  pure (nested, fld)


parseFieldType :: Parser FieldType
parseFieldType =
  choice
    [ FTScalar SDouble <$ reserved "double"
    , FTScalar SFloat <$ reserved "float"
    , FTScalar SInt32 <$ reserved "int32"
    , FTScalar SInt64 <$ reserved "int64"
    , FTScalar SUInt32 <$ reserved "uint32"
    , FTScalar SUInt64 <$ reserved "uint64"
    , FTScalar SSInt32 <$ reserved "sint32"
    , FTScalar SSInt64 <$ reserved "sint64"
    , FTScalar SFixed32 <$ reserved "fixed32"
    , FTScalar SFixed64 <$ reserved "fixed64"
    , FTScalar SSFixed32 <$ reserved "sfixed32"
    , FTScalar SSFixed64 <$ reserved "sfixed64"
    , FTScalar SBool <$ reserved "bool"
    , FTScalar SString <$ reserved "string"
    , FTScalar SBytes <$ reserved "bytes"
    , FTNamed <$> fullIdent
    ]
    <?> "field type (double, float, int32, int64, string, bytes, bool, or message/enum name)"


mapFieldDef :: CommentMap -> Maybe Text -> Int -> Parser (MapField' Parsed)
mapFieldDef _cm doc start = do
  reserved "map"
  (kt, vt) <- angles $ do
    k <- scalarType
    comma
    v <- parseFieldType
    pure (k, v)
  name <- identifier
  equals
  num <- FieldNumber . fromIntegral <$> intLiteral
  opts <- fieldOptionList
  semi
  end <- getOffset
  pure
    MapField
      { mapExt = mkSpan start end
      , mapDoc = doc
      , mapKeyType = kt
      , mapValueType = vt
      , mapFieldName = name
      , mapFieldNum = num
      , mapOptions = opts
      }


scalarType :: Parser ScalarType
scalarType =
  choice
    [ SDouble <$ reserved "double"
    , SFloat <$ reserved "float"
    , SInt32 <$ reserved "int32"
    , SInt64 <$ reserved "int64"
    , SUInt32 <$ reserved "uint32"
    , SUInt64 <$ reserved "uint64"
    , SSInt32 <$ reserved "sint32"
    , SSInt64 <$ reserved "sint64"
    , SFixed32 <$ reserved "fixed32"
    , SFixed64 <$ reserved "fixed64"
    , SSFixed32 <$ reserved "sfixed32"
    , SSFixed64 <$ reserved "sfixed64"
    , SBool <$ reserved "bool"
    , SString <$ reserved "string"
    , SBytes <$ reserved "bytes"
    ]
    <?> "scalar type (double, float, int32, int64, uint32, uint64, sint32, sint64, fixed32, fixed64, sfixed32, sfixed64, bool, string, or bytes)"


oneofDef :: CommentMap -> Maybe Text -> Int -> Parser (OneofDef' Parsed)
oneofDef cm doc start = do
  reserved "oneof"
  name <- identifier
  (fields, opts) <- braces $ do
    items <- manyStmts (Left <$> try oneofOption <|> Right <$> oneofField cm)
    let os = concatMap (either (: []) (const [])) items
    let fs = concatMap (either (const []) (: [])) items
    pure (fs, os)
  _ <- optional semi
  end <- getOffset
  pure
    OneofDef
      { oneofExt = mkSpan start end
      , oneofDoc = doc
      , oneofName = name
      , oneofFields = fields
      , oneofOptions = opts
      }
  where
    oneofOption = do
      s <- getOffset
      optionDecl s
    oneofField cm' = do
      s <- getOffset
      doc' <- getDoc cm'
      ft <- parseFieldType
      name <- identifier
      equals
      num <- FieldNumber . fromIntegral <$> intLiteral
      opts <- fieldOptionList
      semi
      e <- getOffset
      pure
        OneofField
          { oneofFieldExt = mkSpan s e
          , oneofFieldDoc = doc'
          , oneofFieldType = ft
          , oneofFieldName = name
          , oneofFieldNumber = num
          , oneofFieldOptions = opts
          }


fieldOptionList :: Parser [OptionDef' Parsed]
fieldOptionList = fieldOptionsWith optionAssignment


fieldOptionsWith :: Parser (OptionDef' Parsed) -> Parser [OptionDef' Parsed]
fieldOptionsWith assignment = fromMaybe [] <$> optional (brackets (assignment `sepBy1` comma))


reservedDecl :: Parser ReservedDef
reservedDecl = do
  reserved "reserved"
  res <- try reservedNames <|> reservedNumbers
  semi
  pure res
  where
    -- proto2/proto3 use quoted names; editions (2023+) use bare identifiers.
    -- The spelling is retained so printing reproduces the right token.
    reservedNames = ReservedNames <$> (reservedName `sepBy1` comma)
    reservedName = (QuotedReservedName <$> stringLiteral) <|> (IdentReservedName <$> identifier)
    reservedNumbers = ReservedNumbers <$> (reservedRange `sepBy1` comma)
    reservedRange = do
      start <- fromIntegral <$> intLiteral
      end' <- optional (reserved "to" *> (Nothing <$ reserved "max" <|> Just . fromIntegral <$> intLiteral))
      case end' of
        Nothing -> pure (ReservedSingle start)
        Just Nothing -> pure (ReservedRange start 536870911) -- max field number
        Just (Just e) -> pure (ReservedRange start e)


extensionsDecl :: Parser ([ExtensionRange], [OptionDef' Parsed])
extensionsDecl = do
  reserved "extensions"
  ranges <- extensionRange `sepBy1` comma
  -- Optional trailing options, e.g. @[verification = UNVERIFIED]@ or an
  -- editions @[declaration = {…}]@ list; same bracket syntax as field options.
  opts <- fieldOptionList
  semi
  pure (ranges, opts)
  where
    extensionRange = do
      start <- fromIntegral <$> intLiteral
      end' <- optional (reserved "to" *> (ExtBoundMax <$ reserved "max" <|> ExtBoundNum . fromIntegral <$> intLiteral))
      pure
        ExtensionRange
          { extStart = start
          , extEnd = fromMaybe (ExtBoundNum start) end'
          }


data EnumItem = EIOption (OptionDef' Parsed) | EIValue (EnumValue' Parsed) | EIReserved


enumDef :: CommentMap -> Maybe Text -> Int -> Parser (EnumDef' Parsed)
enumDef cm doc start = do
  reserved "enum"
  name <- identifier
  items <- braces (manyStmts (enumItem cm))
  let vals = mapMaybe (\case EIValue v -> Just v; _ -> Nothing) items
      opts = mapMaybe (\case EIOption o -> Just o; _ -> Nothing) items
  end <- getOffset
  pure
    EnumDef
      { enumExt = mkSpan start end
      , enumDoc = doc
      , enumName = name
      , enumValues = vals
      , enumOptions = opts
      }


enumItem :: CommentMap -> Parser EnumItem
enumItem cm =
  choice
    [ EIOption <$> try (getOffset >>= optionDecl)
    , EIReserved <$ try enumReservedDecl
    , EIValue <$> enumValueDef cm
    ]
    <?> "enum value, option, or reserved declaration"


enumReservedDecl :: Parser ()
enumReservedDecl = do
  reserved "reserved"
  -- Either reserved names (quoted in proto2/proto3, bare identifiers in
  -- editions) or reserved numbers/ranges. Enum reserved ranges
  -- (@reserved 1 to 10;@, @reserved 5 to max;@) are accepted just like
  -- message reserved ranges; the values aren't retained in the AST.
  void (try ((stringLiteral <|> identifier) `sepBy1` comma)) <|> void (enumReservedRange `sepBy1` comma)
  semi
  where
    enumReservedRange = do
      _ <- intLiteral
      _ <- optional (reserved "to" *> (reserved "max" <|> void intLiteral))
      pure ()


enumValueDef :: CommentMap -> Parser (EnumValue' Parsed)
enumValueDef cm = do
  start <- getOffset
  doc <- getDoc cm
  name <- identifier
  equals
  num <- fromIntegral <$> intLiteral
  opts <- fieldOptionList
  semi
  end <- getOffset
  pure
    EnumValue
      { evExt = mkSpan start end
      , evDoc = doc
      , evName = name
      , evNumber = num
      , evOptions = opts
      }


serviceDef :: CommentMap -> Maybe Text -> Int -> Parser (ServiceDef' Parsed)
serviceDef cm doc start = do
  reserved "service"
  name <- identifier
  (rpcs, opts) <- braces $ do
    items <- manyStmts (Left <$> try (getOffset >>= optionDecl) <|> Right <$> rpcDef cm)
    pure
      ( concatMap (either (const []) (: [])) items
      , concatMap (either (: []) (const [])) items
      )
  end <- getOffset
  pure
    ServiceDef
      { svcExt = mkSpan start end
      , svcDoc = doc
      , svcName = name
      , svcRpcs = rpcs
      , svcOptions = opts
      }


rpcDef :: CommentMap -> Parser (RpcDef' Parsed)
rpcDef cm = do
  start <- getOffset
  doc <- getDoc cm
  reserved "rpc"
  name <- identifier
  void (symbol "(")
  inStream <- option NoStream (Streaming <$ reserved "stream")
  inType <- fullIdent
  void (symbol ")")
  reserved "returns"
  void (symbol "(")
  outStream <- option NoStream (Streaming <$ reserved "stream")
  outType <- fullIdent
  void (symbol ")")
  opts <- rpcBody <|> ([] <$ semi)
  _ <- optional semi
  end <- getOffset
  pure
    RpcDef
      { rpcExt = mkSpan start end
      , rpcDoc = doc
      , rpcName = name
      , rpcInput = inType
      , rpcInputStr = inStream
      , rpcOutput = outType
      , rpcOutputStr = outStream
      , rpcOptions = opts
      }
  where
    rpcBody = braces (manyStmts (getOffset >>= optionDecl))


-- -----------------------------------------------------------------------
-- Comment insertion post-pass
-- -----------------------------------------------------------------------

{- | Build a mapping from byte offset to 1-based line number.
Returns a function that converts a byte offset to its line number.
-}
offsetToLine :: Text -> Int -> Int
offsetToLine src offset =
  let prefix = T.take offset src
  in 1 + T.count "\n" prefix


{- | Compute the set of 1-based line numbers that are "claimed" as doc
comments by the CommentMap.  For each entry @(defLine, docText)@, the
comment lines that precede @defLine@ and were merged into @docText@
are claimed.
-}
claimedDocLines :: Text -> CommentMap -> IntSet
claimedDocLines src cm =
  let allLines = zip [1 ..] (T.lines src)
      defLines = IntMap.keys cm
  in IntSet.fromList (concatMap (claimedFor allLines) defLines)
  where
    claimedFor allLines defLine =
      -- Walk backwards from defLine-1, claiming comment and blank lines
      -- until we hit a non-comment, non-blank line.
      let preceding = reverse $ take (defLine - 1) allLines
      in claimBackward preceding []

    claimBackward [] acc = acc
    claimBackward ((ln, t) : rest) acc
      | isCommentLine t = claimBackward rest (ln : acc)
      | isBlankLine t = claimBackward rest acc -- blank lines between comments and def
      | otherwise = acc

    isCommentLine t = "//" `T.isPrefixOf` T.stripStart t
    isBlankLine t = T.null (T.strip t)


{- | Insert standalone comment nodes into a parsed proto file.
Comments that are NOT claimed as doc comments for any definition
become 'TLComment' (at top level) or 'MEComment' (inside messages).
-}
insertComments :: Text -> [LocComment] -> CommentMap -> ProtoFile' Parsed -> ProtoFile' Parsed
insertComments src allComments cm pf =
  let claimed = claimedDocLines src cm
      unclaimed = filter (\lc -> not (IntSet.member (lcLine lc) claimed)) allComments
      -- Insert unclaimed comments at the top level
      topLevels' = insertTopLevelComments src unclaimed (protoTopLevels pf)
  in pf {protoTopLevels = topLevels'}


-- | Get the start line of a top-level definition from its span.
topLevelStartLine :: Text -> TopLevel' Parsed -> Maybe Int
topLevelStartLine src = \case
  TLMessage m -> spanLine src (msgExt m)
  TLEnum e -> spanLine src (enumExt e)
  TLService s -> spanLine src (svcExt s)
  TLOption o -> spanLine src (optExt o)
  TLExtend _ _ -> Nothing -- extends don't have a span on the TLExtend itself
  TLComment _ -> Nothing


-- | Get the end line of a top-level definition from its span.
topLevelEndLine :: Text -> TopLevel' Parsed -> Maybe Int
topLevelEndLine src = \case
  TLMessage m -> spanEndLine src (msgExt m)
  TLEnum e -> spanEndLine src (enumExt e)
  TLService s -> spanEndLine src (svcExt s)
  TLOption o -> spanEndLine src (optExt o)
  TLExtend _ _ -> Nothing
  TLComment _ -> Nothing


spanLine :: Text -> Span -> Maybe Int
spanLine src (Span (Just (SrcSpan s _))) = Just (offsetToLine src s)
spanLine _ _ = Nothing


spanEndLine :: Text -> Span -> Maybe Int
spanEndLine src (Span (Just (SrcSpan _ e))) = Just (offsetToLine src (max 0 (e - 1)))
spanEndLine _ _ = Nothing


-- | Insert unclaimed comments between top-level definitions.
insertTopLevelComments :: Text -> [LocComment] -> [TopLevel' Parsed] -> [TopLevel' Parsed]
insertTopLevelComments _ [] tls = tls
insertTopLevelComments src unclaimed tls =
  let
    -- For each gap between definitions, find unclaimed comments that belong there
    result = go 0 tls
  in
    result
  where
    go :: Int -> [TopLevel' Parsed] -> [TopLevel' Parsed]
    go afterLine [] =
      -- Trailing comments after the last definition
      let trailing = filter (\lc -> lcLine lc > afterLine) unclaimed
          groups = groupConsecutive trailing
      in fmap (TLComment . fmap lcComment) groups
    go afterLine (tl : rest) =
      let startLine = fromMaybe maxBound (topLevelStartLine src tl)
          endLine = fromMaybe afterLine (topLevelEndLine src tl)
          -- Comments between afterLine and startLine that are unclaimed
          between = filter (\lc -> lcLine lc > afterLine && lcLine lc < startLine) unclaimed
          groups = groupConsecutive between
          commentNodes = fmap (TLComment . fmap lcComment) groups
          -- Also insert comments inside messages
          tl' = insertCommentsInTopLevel src unclaimed tl
      in commentNodes ++ [tl'] ++ go endLine rest


-- | Insert comments inside a top-level definition (recursively into messages).
insertCommentsInTopLevel :: Text -> [LocComment] -> TopLevel' Parsed -> TopLevel' Parsed
insertCommentsInTopLevel src unclaimed = \case
  TLMessage m -> TLMessage (insertCommentsInMessage src unclaimed m)
  other -> other


-- | Insert unclaimed comments between message elements.
insertCommentsInMessage :: Text -> [LocComment] -> MessageDef' Parsed -> MessageDef' Parsed
insertCommentsInMessage src unclaimed m =
  let msgStart = case msgExt m of
        Span (Just (SrcSpan s _)) -> offsetToLine src s
        _ -> 0
      msgEnd = case msgExt m of
        Span (Just (SrcSpan _ e)) -> offsetToLine src (max 0 (e - 1))
        _ -> maxBound
      -- Only consider unclaimed comments within the message's span
      relevant = filter (\lc -> lcLine lc > msgStart && lcLine lc < msgEnd) unclaimed
      elems' = insertMessageElementComments src relevant (msgElements m)
  in m {msgElements = elems'}


-- | Get the start/end line of a message element from its span.
msgElemStartLine :: Text -> MessageElement' Parsed -> Maybe Int
msgElemStartLine src = \case
  MEField f -> spanLine src (fieldExt f)
  MEEnum e -> spanLine src (enumExt e)
  MEMessage m -> spanLine src (msgExt m)
  MEOneof o -> spanLine src (oneofExt o)
  MEMapField mf -> spanLine src (mapExt mf)
  MEOption o -> spanLine src (optExt o)
  MEReserved _ -> Nothing
  MEExtensions _ _ -> Nothing
  MEExtend _ _ -> Nothing
  MEComment _ -> Nothing


msgElemEndLine :: Text -> MessageElement' Parsed -> Maybe Int
msgElemEndLine src = \case
  MEField f -> spanEndLine src (fieldExt f)
  MEEnum e -> spanEndLine src (enumExt e)
  MEMessage m -> spanEndLine src (msgExt m)
  MEOneof o -> spanEndLine src (oneofExt o)
  MEMapField mf -> spanEndLine src (mapExt mf)
  MEOption o -> spanEndLine src (optExt o)
  MEReserved _ -> Nothing
  MEExtensions _ _ -> Nothing
  MEExtend _ _ -> Nothing
  MEComment _ -> Nothing


insertMessageElementComments :: Text -> [LocComment] -> [MessageElement' Parsed] -> [MessageElement' Parsed]
insertMessageElementComments _ [] elems = elems
insertMessageElementComments src unclaimed elems = go 0 elems
  where
    go :: Int -> [MessageElement' Parsed] -> [MessageElement' Parsed]
    go afterLine [] =
      let trailing = filter (\lc -> lcLine lc > afterLine) unclaimed
          groups = groupConsecutive trailing
      in fmap (MEComment . fmap lcComment) groups
    go afterLine (el : rest) =
      let startLine = fromMaybe maxBound (msgElemStartLine src el)
          endLine = fromMaybe afterLine (msgElemEndLine src el)
          between = filter (\lc -> lcLine lc > afterLine && lcLine lc < startLine) unclaimed
          groups = groupConsecutive between
          commentNodes = fmap (MEComment . fmap lcComment) groups
          -- Recurse into nested messages
          el' = case el of
            MEMessage inner -> MEMessage (insertCommentsInMessage src unclaimed inner)
            _ -> el
      in commentNodes ++ [el'] ++ go endLine rest


{- | Group consecutive located comments (no gap of more than 1 line between them)
into blocks.
-}
groupConsecutive :: [LocComment] -> [[LocComment]]
groupConsecutive [] = []
groupConsecutive (lc : rest) = go [lc] (lcLine lc) rest
  where
    go acc _ [] = [reverse acc]
    go acc prevLine (x : xs)
      | lcLine x <= prevLine + 2 = go (x : acc) (lcLine x) xs -- allow 1 blank line gap
      | otherwise = reverse acc : go [x] (lcLine x) xs
