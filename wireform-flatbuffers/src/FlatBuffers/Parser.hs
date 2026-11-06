{- | FlatBuffers schema parser.

Parses FlatBuffers schema definitions (@.fbs@ files) into a
'FlatBuffers.Schema.FlatBuffersSchema' AST using Megaparsec.
Supports tables, structs, enums, unions, namespaces, includes,
file identifiers, and root types.

@
table Monster {
  name:string;
  hp:int = 100;
  mana:short = 150;
}
@

@
import FlatBuffers.Parser (parseFlatBuffers)
let Right schema = parseFlatBuffers input
@
-}
module FlatBuffers.Parser (
  parseFlatBuffers,
) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void (Void)
import FlatBuffers.Schema
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


type Parser = Parsec Void Text


parseFlatBuffers :: Text -> Either String FlatBuffersSchema
parseFlatBuffers input =
  case parse (sc *> schemaP <* eof) "<fbs>" input of
    Left err -> Left (errorBundlePretty err)
    Right s -> Right s


--------------------------------------------------------------------------------
-- Whitespace / lexer helpers
--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)


braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")


identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_')
  pure (T.cons c rest)


dottedIdentifier :: Parser Text
dottedIdentifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_' || ch == '.')
  pure (T.cons c rest)


stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  void (char '"')
  content <- takeWhileP Nothing (/= '"')
  void (char '"')
  pure content


integerLiteral :: Parser Int64
integerLiteral = lexeme $ do
  sign <- optional (char '-')
  n <- (try (string "0x" *> L.hexadecimal) <|> L.decimal) :: Parser Integer
  let val = case sign of
        Just _ -> negate n
        Nothing -> n
  if val < toInteger (minBound :: Int64) || val > toInteger (maxBound :: Int64)
    then fail "enum integer exceeds the schema AST Int64 range"
    else pure (fromInteger val)


--------------------------------------------------------------------------------
-- Top-level schema
--------------------------------------------------------------------------------

schemaP :: Parser FlatBuffersSchema
schemaP = do
  items <- many topLevelP
  let ns = listToMaybe' [n | TLNamespace n <- items]
      incs = V.fromList [i | TLInclude i <- items]
      decls = V.fromList [d | TLDecl d <- items]
      root = listToMaybe' [r | TLRootType r <- items]
      fileId = listToMaybe' [f | TLFileIdentifier f <- items]
      fileExt = listToMaybe' [e | TLFileExtension e <- items]
      attrs = V.fromList [a | TLAttribute a <- items]
  pure
    FlatBuffersSchema
      { fbsNamespace = ns
      , fbsIncludes = incs
      , fbsDecls = decls
      , fbsRootType = root
      , fbsFileIdentifier = fileId
      , fbsFileExtension = fileExt
      , fbsAttributes = attrs
      }


listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x : _) = Just x


data TopLevel
  = TLNamespace !Text
  | TLInclude !Text
  | TLDecl !FBDeclaration
  | TLRootType !Text
  | TLFileIdentifier !Text
  | TLFileExtension !Text
  | TLAttribute !Text


topLevelP :: Parser TopLevel
topLevelP =
  choice
    [ TLNamespace <$> namespaceP
    , TLInclude <$> includeP
    , TLRootType <$> rootTypeP
    , TLFileIdentifier <$> fileIdentifierP
    , TLFileExtension <$> fileExtensionP
    , TLAttribute <$> attributeP
    , TLDecl <$> declP
    ]


--------------------------------------------------------------------------------
-- Directives
--------------------------------------------------------------------------------

namespaceP :: Parser Text
namespaceP = do
  reserved "namespace"
  ns <- dottedIdentifier
  void (symbol ";")
  pure ns


includeP :: Parser Text
includeP = do
  reserved "include"
  path <- stringLiteral
  void (symbol ";")
  pure path


rootTypeP :: Parser Text
rootTypeP = do
  void (symbol "root_type")
  name <- identifier
  void (symbol ";")
  pure name


fileIdentifierP :: Parser Text
fileIdentifierP = do
  void (symbol "file_identifier")
  ident <- stringLiteral
  void (symbol ";")
  pure ident


fileExtensionP :: Parser Text
fileExtensionP = do
  void (symbol "file_extension")
  ext <- stringLiteral
  void (symbol ";")
  pure ext


attributeP :: Parser Text
attributeP = do
  reserved "attribute"
  name <- stringLiteral
  void (symbol ";")
  pure name


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

declP :: Parser FBDeclaration
declP =
  choice
    [ FBTable <$> tableP
    , FBStruct <$> fbStructP
    , FBEnum <$> fbEnumP
    , FBUnion <$> fbUnionP
    ]


--------------------------------------------------------------------------------
-- Table
--------------------------------------------------------------------------------

tableP :: Parser TableDef
tableP = do
  reserved "table"
  name <- identifier
  fields <- braces (many tableFieldP)
  pure
    TableDef
      { tdName = name
      , tdFields = V.fromList fields
      }


metadataEntryP :: Parser (Text, Maybe Text)
metadataEntryP = do
  key <- identifier
  val <- optional (symbol ":" *> metadataValueP)
  pure (key, val)


metadataValueP :: Parser Text
metadataValueP = stringLiteral <|> lexeme (takeWhile1P Nothing (\c -> c /= ',' && c /= ')' && c /= ' ' && c /= '\n'))


fieldMetadataP :: Parser (V.Vector (Text, Maybe Text))
fieldMetadataP = do
  mMeta <- optional $ do
    void (symbol "(")
    entries <- metadataEntryP `sepBy` symbol ","
    void (symbol ")")
    pure entries
  pure $ V.fromList (maybe [] id mMeta)


tableFieldP :: Parser TableField
tableFieldP = do
  name <- identifier
  void (symbol ":")
  ty <- fbTypeP
  dflt <- optional (symbol "=" *> defaultValP)
  meta <- fieldMetadataP
  let depr = V.any (\(k, _) -> k == "deprecated") meta
  void (symbol ";")
  pure
    TableField
      { tfName = name
      , tfType = ty
      , tfDefault = dflt
      , tfDeprecated = depr
      , tfMetadata = meta
      }


defaultValP :: Parser Text
defaultValP = lexeme $ do
  c <- satisfy (\ch -> ch /= ';' && ch /= '(' && ch /= ' ' && ch /= '\n')
  rest <- takeWhileP Nothing (\ch -> ch /= ';' && ch /= '(' && ch /= ' ' && ch /= '\n')
  pure (T.cons c rest)


--------------------------------------------------------------------------------
-- Struct
--------------------------------------------------------------------------------

fbStructP :: Parser FBStructDef
fbStructP = do
  reserved "struct"
  name <- identifier
  fields <- braces (many structFieldP)
  pure
    FBStructDef
      { fsdName = name
      , fsdFields = V.fromList fields
      }


structFieldP :: Parser (Text, FBType)
structFieldP = do
  name <- identifier
  void (symbol ":")
  ty <- fbTypeP
  void (symbol ";")
  pure (name, ty)


--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

fbEnumP :: Parser FBEnumDef
fbEnumP = do
  reserved "enum"
  name <- identifier
  void (symbol ":")
  underlying <- fbTypeP
  vals <- braces (enumValP `sepEndBy` symbol ",")
  pure
    FBEnumDef
      { fedName = name
      , fedUnderlyingType = underlying
      , fedValues = V.fromList vals
      }


enumValP :: Parser (Text, Maybe Int64)
enumValP = do
  name <- identifier
  val <- optional (symbol "=" *> integerLiteral)
  pure (name, val)


--------------------------------------------------------------------------------
-- Union
--------------------------------------------------------------------------------

fbUnionP :: Parser FBUnionDef
fbUnionP = do
  reserved "union"
  name <- identifier
  members <- braces (identifier `sepEndBy` symbol ",")
  pure
    FBUnionDef
      { fudName = name
      , fudMembers = V.fromList members
      }


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

fbTypeP :: Parser FBType
fbTypeP =
  choice
    [ FTBool <$ reserved "bool"
    , FTByte <$ reserved "byte"
    , FTUByte <$ reserved "ubyte"
    , FTShort <$ reserved "short"
    , FTUShort <$ reserved "ushort"
    , FTInt <$ reserved "int"
    , FTUInt <$ reserved "uint"
    , FTLong <$ reserved "long"
    , FTULong <$ reserved "ulong"
    , FTFloat <$ reserved "float"
    , FTDouble <$ reserved "double"
    , FTString <$ reserved "string"
    , FTInt <$ reserved "int32"
    , FTUInt <$ reserved "uint32"
    , FTLong <$ reserved "int64"
    , FTULong <$ reserved "uint64"
    , FTShort <$ reserved "int16"
    , FTUShort <$ reserved "uint16"
    , FTByte <$ reserved "int8"
    , FTUByte <$ reserved "uint8"
    , FTFloat <$ reserved "float32"
    , FTDouble <$ reserved "float64"
    , vectorTypeP
    , FTNamed <$> identifier
    ]


vectorTypeP :: Parser FBType
vectorTypeP = do
  void (symbol "[")
  ty <- fbTypeP
  void (symbol "]")
  pure (FTVector ty)
