-- | Parser for Avro IDL (.avdl) files.
--
-- Avro IDL is a more ergonomic syntax for defining Avro schemas and protocols.
-- This module parses .avdl text into an intermediate 'AvroIDL' AST, which can
-- then be converted to the standard 'Avro.Schema.AvroType' /
-- 'Avro.Protocol.AvroProtocol' representations via "Avro.IDLConvert".
module Avro.IDL
  ( -- * Parsing
    parseAvroIDL
    -- * AST types
  , AvroIDL(..)
  , AvroIDLImport(..)
  , AvroIDLDecl(..)
  , AvroIDLField(..)
  , AvroIDLType(..)
  , AvroIDLMessage(..)
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- ---------------------------------------------------------------------------
-- AST
-- ---------------------------------------------------------------------------

data AvroIDL = AvroIDL
  { aidlNamespace    :: !(Maybe Text)
  , aidlProtocolName :: !Text
  , aidlImports      :: !(Vector AvroIDLImport)
  , aidlDeclarations :: !(Vector AvroIDLDecl)
  , aidlMessages     :: !(Vector AvroIDLMessage)
  } deriving stock (Show, Eq)

data AvroIDLImport
  = ImportIDL      !Text
  | ImportProtocol !Text
  | ImportSchema   !Text
  deriving stock (Show, Eq)

data AvroIDLDecl
  = IDLRecord !Text !(Vector AvroIDLField) !(Maybe Text) !(Vector Text)
  | IDLEnum   !Text !(Vector Text) !(Maybe Text)
  | IDLFixed  !Text !Int
  | IDLError  !Text !(Vector AvroIDLField) !(Maybe Text)
  deriving stock (Show, Eq)

data AvroIDLField = AvroIDLField
  { ifdType        :: !AvroIDLType
  , ifdName        :: !Text
  , ifdDefault     :: !(Maybe Text)
  , ifdAnnotations :: !(Vector (Text, Text))
  , ifdDoc         :: !(Maybe Text)
  , ifdOrder       :: !(Maybe Text)
  } deriving stock (Show, Eq)

data AvroIDLType
  = ITNull | ITBoolean | ITInt | ITLong | ITFloat | ITDouble | ITBytes | ITString
  | ITArray   !AvroIDLType
  | ITMap     !AvroIDLType
  | ITUnion   !(Vector AvroIDLType)
  | ITNamed   !Text
  | ITDecimal !Int !Int
  -- | @date@ — int with logical type @date@.
  | ITDate
  -- | @time_ms@ — int with logical type @time-millis@.
  | ITTimeMillis
  -- | @timestamp_ms@ — long with logical type @timestamp-millis@.
  | ITTimestampMillis
  -- | @local_timestamp_ms@ — long with logical type @local-timestamp-millis@.
  | ITLocalTimestampMillis
  -- | @uuid@ — string with logical type @uuid@.
  | ITUuid
  deriving stock (Show, Eq)

data AvroIDLMessage = AvroIDLMessage
  { imName   :: !Text
  , imReturn :: !AvroIDLType
  , imParams :: !(Vector (AvroIDLType, Text))
  , imOneway :: !Bool
  , imDoc    :: !(Maybe Text)
  , imErrors :: !(Vector Text)
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

parseAvroIDL :: Text -> Either String AvroIDL
parseAvroIDL input =
  case parse (sc *> pProtocol <* eof) "<avdl>" input of
    Left err -> Left (errorBundlePretty err)
    Right r  -> Right r

-- ---------------------------------------------------------------------------
-- Lexer helpers
-- ---------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = try $ do
      _ <- chunk "/*"
      nextChar <- lookAhead (optional anySingle)
      case nextChar of
        Just '*' -> empty
        _        -> void (manyTill anySingle (chunk "*/"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

semi :: Parser ()
semi = void (symbol ";")

comma :: Parser ()
comma = void (symbol ",")

-- ---------------------------------------------------------------------------
-- Doc comments
-- ---------------------------------------------------------------------------

pDocComment :: Parser (Maybe Text)
pDocComment = optional $ lexeme $ do
  _ <- chunk "/**"
  content <- manyTill anySingle (chunk "*/")
  pure (T.strip (T.pack content))

-- ---------------------------------------------------------------------------
-- Annotations
-- ---------------------------------------------------------------------------

data Annotation = Annotation !Text !Text
  deriving stock (Show, Eq)

pAnnotation :: Parser Annotation
pAnnotation = lexeme $ do
  _ <- char '@'
  name <- T.pack <$> some (alphaNumChar <|> char '_')
  _ <- char '('
  sc
  val <- pAnnotationValue
  sc
  _ <- char ')'
  pure (Annotation name val)

pAnnotationValue :: Parser Text
pAnnotationValue = pStringLit <|> pBracketedValue <|> pPlainValue
  where
    pBracketedValue = do
      _ <- char '['
      sc
      content <- manyTill anySingle (char ']')
      pure (T.pack ("[" ++ content ++ "]"))
    pPlainValue = T.pack <$> some (satisfy (\c -> c /= ')' && c /= '\n'))

pAnnotations :: Parser [Annotation]
pAnnotations = many pAnnotation

-- ---------------------------------------------------------------------------
-- String literal
-- ---------------------------------------------------------------------------

pStringLit :: Parser Text
pStringLit = lexeme $ do
  _ <- char '"'
  content <- manyTill L.charLiteral (char '"')
  pure (T.pack content)

-- ---------------------------------------------------------------------------
-- Identifier
-- ---------------------------------------------------------------------------

pIdentifier :: Parser Text
pIdentifier = lexeme $ do
  c <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '.')
  pure (T.pack (c : rest))

-- ---------------------------------------------------------------------------
-- Integer literal
-- ---------------------------------------------------------------------------

pIntLit :: Parser Int
pIntLit = lexeme L.decimal

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

pType :: Parser AvroIDLType
pType = choice
  [ pUnionType
  , pArrayType
  , pMapType
  , pDecimalType
  , pPrimitiveOrNamedType
  ]

pUnionType :: Parser AvroIDLType
pUnionType = do
  _ <- symbol "union"
  branches <- braces (pType `sepBy1` comma)
  pure (ITUnion (V.fromList branches))

pArrayType :: Parser AvroIDLType
pArrayType = do
  _ <- symbol "array"
  inner <- angles pType
  pure (ITArray inner)

pMapType :: Parser AvroIDLType
pMapType = do
  _ <- symbol "map"
  inner <- angles pType
  pure (ITMap inner)

pDecimalType :: Parser AvroIDLType
pDecimalType = do
  _ <- symbol "decimal"
  _ <- symbol "("
  prec <- pIntLit
  _ <- symbol ","
  scl <- pIntLit
  _ <- symbol ")"
  pure (ITDecimal prec scl)

pPrimitiveOrNamedType :: Parser AvroIDLType
pPrimitiveOrNamedType = do
  name <- pIdentifier
  pure $ case name of
    "null"               -> ITNull
    "boolean"            -> ITBoolean
    "int"                -> ITInt
    "long"               -> ITLong
    "float"              -> ITFloat
    "double"             -> ITDouble
    "bytes"              -> ITBytes
    "string"             -> ITString
    -- Avro IDL 1.11 logical-type keywords.
    "date"               -> ITDate
    "time_ms"            -> ITTimeMillis
    "timestamp_ms"       -> ITTimestampMillis
    "local_timestamp_ms" -> ITLocalTimestampMillis
    "uuid"               -> ITUuid
    other                -> ITNamed other

-- ---------------------------------------------------------------------------
-- Protocol
-- ---------------------------------------------------------------------------

pProtocol :: Parser AvroIDL
pProtocol = do
  anns <- pAnnotations
  let ns = extractNamespace anns
  _ <- symbol "protocol"
  name <- pIdentifier
  _ <- symbol "{"
  sc
  items <- many pProtocolItem
  _ <- symbol "}"
  let imports = [i | PIImport i    <- items]
      decls   = [d | PIDecl d      <- items]
      msgs    = [m | PIMessage m   <- items]
  pure AvroIDL
    { aidlNamespace    = ns
    , aidlProtocolName = name
    , aidlImports      = V.fromList imports
    , aidlDeclarations = V.fromList decls
    , aidlMessages     = V.fromList msgs
    }

extractNamespace :: [Annotation] -> Maybe Text
extractNamespace [] = Nothing
extractNamespace (Annotation "namespace" v : _) = Just (T.filter (/= '"') v)
extractNamespace (_ : rest) = extractNamespace rest

data ProtocolItem
  = PIImport  !AvroIDLImport
  | PIDecl    !AvroIDLDecl
  | PIMessage !AvroIDLMessage

pProtocolItem :: Parser ProtocolItem
pProtocolItem = choice
  [ PIImport  <$> pImport
  , try (PIDecl <$> pDecl)
  , PIMessage <$> pMessage
  ]

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

pImport :: Parser AvroIDLImport
pImport = do
  _ <- symbol "import"
  kind <- pIdentifier
  path <- pStringLit
  semi
  case kind of
    "idl"      -> pure (ImportIDL path)
    "protocol" -> pure (ImportProtocol path)
    "schema"   -> pure (ImportSchema path)
    _          -> fail $ "unknown import kind: " ++ T.unpack kind

-- ---------------------------------------------------------------------------
-- Declarations (record, enum, fixed, error)
-- ---------------------------------------------------------------------------

pDecl :: Parser AvroIDLDecl
pDecl = choice
  [ pRecordDecl
  , pEnumDecl
  , try pFixedDecl
  , pErrorDecl
  ]

pRecordDecl :: Parser AvroIDLDecl
pRecordDecl = do
  doc <- pDocComment
  anns <- pAnnotations
  let aliases = extractAliases anns
  _ <- symbol "record"
  name <- pIdentifier
  fields <- braces (many pField)
  pure (IDLRecord name (V.fromList fields) doc aliases)

pEnumDecl :: Parser AvroIDLDecl
pEnumDecl = do
  doc <- pDocComment
  _ <- pAnnotations
  _ <- symbol "enum"
  name <- pIdentifier
  syms <- braces (pIdentifier `sepBy1` comma)
  pure (IDLEnum name (V.fromList syms) doc)

pFixedDecl :: Parser AvroIDLDecl
pFixedDecl = do
  _ <- pAnnotations
  _ <- symbol "fixed"
  name <- pIdentifier
  sz <- parens pIntLit
  semi
  pure (IDLFixed name sz)

pErrorDecl :: Parser AvroIDLDecl
pErrorDecl = do
  doc <- pDocComment
  _ <- pAnnotations
  _ <- symbol "error"
  name <- pIdentifier
  fields <- braces (many pField)
  pure (IDLError name (V.fromList fields) doc)

-- ---------------------------------------------------------------------------
-- Fields
-- ---------------------------------------------------------------------------

pField :: Parser AvroIDLField
pField = do
  doc <- pDocComment
  fieldAnns <- pAnnotations
  let order = extractOrder fieldAnns
      annPairs = [(k, v) | Annotation k v <- fieldAnns
                          , k /= "order" && k /= "aliases"]
  ty <- pType
  name <- pIdentifier
  dflt <- optional (symbol "=" *> pDefaultValue)
  semi
  pure AvroIDLField
    { ifdType        = ty
    , ifdName        = name
    , ifdDefault     = dflt
    , ifdAnnotations = V.fromList annPairs
    , ifdDoc         = doc
    , ifdOrder       = order
    }

extractOrder :: [Annotation] -> Maybe Text
extractOrder [] = Nothing
extractOrder (Annotation "order" v : _) = Just (T.filter (/= '"') v)
extractOrder (_ : rest) = extractOrder rest

extractAliases :: [Annotation] -> Vector Text
extractAliases anns = case [v | Annotation "aliases" v <- anns] of
  []    -> V.empty
  (v:_) -> V.fromList $ parseAliasesValue v

parseAliasesValue :: Text -> [Text]
parseAliasesValue t =
  let stripped = T.strip t
      inner = case T.stripPrefix "[" stripped >>= T.stripSuffix "]" of
                Just s  -> s
                Nothing -> stripped
  in map (T.filter (/= '"') . T.strip) (T.splitOn "," inner)

-- ---------------------------------------------------------------------------
-- Default values
-- ---------------------------------------------------------------------------

pDefaultValue :: Parser Text
pDefaultValue = choice
  [ symbol "null"  *> pure "null"
  , symbol "true"  *> pure "true"
  , symbol "false" *> pure "false"
  , pDefaultString
  , symbol "["  *> symbol "]" *> pure "[]"
  , symbol "{"  *> symbol "}" *> pure "{}"
  , pDefaultNumber
  ]

pDefaultString :: Parser Text
pDefaultString = do
  s <- pStringLit
  pure (T.concat ["\"", s, "\""])

pDefaultNumber :: Parser Text
pDefaultNumber = lexeme $ do
  neg <- optional (char '-')
  digits <- some digitChar
  frac <- optional $ do
    _ <- char '.'
    ds <- some digitChar
    pure ('.' : ds)
  let numStr = maybe "" (: []) neg ++ digits ++ maybe "" id frac
  pure (T.pack numStr)

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

pMessage :: Parser AvroIDLMessage
pMessage = do
  doc <- pDocComment
  _ <- pAnnotations
  retTy <- pType
  name <- pIdentifier
  params <- parens (pParam `sepBy` comma)
  errs <- pThrowsClause <|> pure []
  ow <- option False (symbol "oneway" *> pure True)
  semi
  pure AvroIDLMessage
    { imName   = name
    , imReturn = retTy
    , imParams = V.fromList params
    , imOneway = ow
    , imDoc    = doc
    , imErrors = V.fromList errs
    }

pParam :: Parser (AvroIDLType, Text)
pParam = do
  _ <- pAnnotations
  ty <- pType
  name <- pIdentifier
  pure (ty, name)

pThrowsClause :: Parser [Text]
pThrowsClause = do
  _ <- symbol "throws"
  names <- pIdentifier `sepBy1` comma
  pure names
