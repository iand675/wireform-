{- | Thrift IDL parser.

Parses Thrift IDL definitions (@.thrift@ files) into a
'Thrift.Schema.ThriftSchema' AST using Megaparsec. Supports structs,
unions, exceptions, enums, services, typedefs, constants, includes,
namespaces, and all Thrift type annotations.

@
struct Person {
  1: required string name;
  2: optional i32 age;
}
@

@
import Thrift.Parser (parseThrift)
let Right schema = parseThrift input
@
-}
module Thrift.Parser (
  parseThrift,
) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Thrift.Schema


type Parser = Parsec Void Text


-- | Parse Thrift IDL text into a 'ThriftSchema'.
parseThrift :: Text -> Either String ThriftSchema
parseThrift input =
  case parse (sc *> documentP <* eof) "<thrift>" input of
    Left err -> Left (errorBundlePretty err)
    Right s -> Right s


-- Whitespace and comments
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)


braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")


comma :: Parser ()
comma = void (symbol ",")


semicolon :: Parser ()
semicolon = void (symbol ";")


listSep :: Parser ()
listSep = comma <|> semicolon


optListSep :: Parser ()
optListSep = void (optional listSep)


identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_' || ch == '.')
  pure (T.cons c rest)


stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  delim <- char '"' <|> char '\''
  content <- takeWhileP Nothing (/= delim)
  void (char delim)
  pure content


integerLiteral :: Parser Int64
integerLiteral = lexeme $ do
  sign <- optional (char '-')
  n <- try (string "0x" *> L.hexadecimal) <|> L.decimal
  let val = case sign of
        Just _ -> negate n
        Nothing -> n
  pure val


-- Document
documentP :: Parser ThriftSchema
documentP = do
  _ <- many headerP
  defs <- many definitionP
  let structs = [s | DefStruct s <- defs]
      enums = [e | DefEnum e <- defs]
      typedefs = [t | DefTypedef t <- defs]
      consts = [c | DefConst c <- defs]
      services = [s | DefService s <- defs]
  pure
    ThriftSchema
      { tsStructs = structs
      , tsEnums = enums
      , tsTypedefs = typedefs
      , tsConsts = consts
      , tsServices = services
      }


data Definition
  = DefStruct ThriftStruct
  | DefEnum ThriftEnum
  | DefTypedef ThriftTypedef
  | DefConst ThriftConst
  | DefService ThriftService


headerP :: Parser ()
headerP = includeP <|> namespaceP <|> hashCommentP <|> cppIncludeP


hashCommentP :: Parser ()
hashCommentP = lexeme $ do
  void (char '#')
  void (takeWhileP Nothing (/= '\n'))


includeP :: Parser ()
includeP = do
  reserved "include"
  _ <- stringLiteral
  pure ()


cppIncludeP :: Parser ()
cppIncludeP = do
  reserved "cpp_include"
  _ <- stringLiteral
  pure ()


namespaceP :: Parser ()
namespaceP = do
  reserved "namespace"
  void (symbol "*" *> identifier) <|> namedScope
  where
    namedScope = do
      _ <- identifier
      _ <- identifier <|> stringLiteral
      void annotationsP


annotationPairP :: Parser (Text, Text)
annotationPairP = do
  key <- identifier
  val <- option "" (symbol "=" *> (stringLiteral <|> identifier))
  optListSep
  pure (key, val)


definitionP :: Parser Definition
definitionP =
  choice
    [ DefStruct <$> structP
    , DefStruct <$> unionP
    , DefStruct <$> exceptionP
    , DefEnum <$> enumP
    , DefService <$> serviceP
    , DefTypedef <$> typedefP
    , DefConst <$> constP
    ]


structP :: Parser ThriftStruct
structP = do
  reserved "struct"
  name <- identifier
  fields <- braces (many fieldP)
  anns <- annotationsP
  pure
    ThriftStruct
      { tsName = name
      , tsKind = StructNormal
      , tsFields = fields
      , tsAnnotations = anns
      }


unionP :: Parser ThriftStruct
unionP = do
  reserved "union"
  name <- identifier
  fields <- braces (many fieldP)
  anns <- annotationsP
  pure
    ThriftStruct
      { tsName = name
      , tsKind = StructUnion
      , tsFields = fields
      , tsAnnotations = anns
      }


exceptionP :: Parser ThriftStruct
exceptionP = do
  reserved "exception"
  name <- identifier
  fields <- braces (many fieldP)
  anns <- annotationsP
  pure
    ThriftStruct
      { tsName = name
      , tsKind = StructException
      , tsFields = fields
      , tsAnnotations = anns
      }


fieldP :: Parser ThriftField
fieldP = do
  fid <- integerLiteral
  void (symbol ":")
  req <- optional (requirednessP)
  ftype <- typeP
  fname <- identifier
  dflt <- optional (symbol "=" *> constValueP)
  optListSep
  anns <- annotationsP
  pure
    ThriftField
      { tfFieldId = fromIntegral fid
      , tfFieldName = fname
      , tfFieldType = ftype
      , tfRequiredness = fromMaybe Default req
      , tfDefault = dflt
      , tfAnnotations = anns
      }


requirednessP :: Parser Requiredness
requirednessP = (Required <$ reserved "required") <|> (Optional <$ reserved "optional")


typeP :: Parser ThriftType
typeP = (annotatableTypeP <* annotationsP) <|> (TStruct <$> identifier)


-- Type annotations configure the reference generators. Consume them in
-- their own position without treating them as annotations on the field.
annotatableTypeP :: Parser ThriftType
annotatableTypeP =
  choice
    [ TBool <$ reserved "bool"
    , TByte <$ (reserved "byte" <|> reserved "i8")
    , TI16 <$ reserved "i16"
    , TI32 <$ reserved "i32"
    , TI64 <$ reserved "i64"
    , TDouble <$ reserved "double"
    , TString <$ reserved "string"
    , TBinary <$ reserved "binary"
    , TUUID <$ reserved "uuid"
    , listTypeP
    , setTypeP
    , mapTypeP
    ]


listTypeP :: Parser ThriftType
listTypeP = do
  reserved "list"
  TList <$> angles typeP


setTypeP :: Parser ThriftType
setTypeP = do
  reserved "set"
  TSet <$> angles typeP


mapTypeP :: Parser ThriftType
mapTypeP = do
  reserved "map"
  angles $ do
    kt <- typeP
    void (symbol ",")
    vt <- typeP
    pure (TMap kt vt)


enumP :: Parser ThriftEnum
enumP = do
  reserved "enum"
  name <- identifier
  vals <- braces (many enumValueP)
  _ <- annotationsP
  let numbered = assignEnumValues vals 0
  pure ThriftEnum {teName = name, teValues = numbered}


enumValueP :: Parser (Text, Maybe Int32)
enumValueP = do
  name <- identifier
  val <- optional (symbol "=" *> (fromIntegral <$> integerLiteral))
  optListSep
  _ <- annotationsP
  pure (name, val)


assignEnumValues :: [(Text, Maybe Int32)] -> Int32 -> [(Text, Int32)]
assignEnumValues [] _ = []
assignEnumValues ((name, mval) : rest) nextVal =
  let val = fromMaybe nextVal mval
  in (name, val) : assignEnumValues rest (val + 1)


serviceP :: Parser ThriftService
serviceP = do
  reserved "service"
  name <- identifier
  extends <- optional (reserved "extends" *> identifier)
  methods <- braces (many methodP)
  _ <- annotationsP
  pure
    ThriftService
      { tsvName = name
      , tsvExtends = extends
      , tsvMethods = methods
      }


methodP :: Parser ThriftMethod
methodP = do
  ow <- option False (True <$ reserved "oneway")
  retType <- (Nothing <$ reserved "void") <|> (Just <$> typeP)
  name <- identifier
  params <- parens (many fieldP)
  throws <- option [] (reserved "throws" *> parens (many fieldP))
  optListSep
  _ <- annotationsP
  pure
    ThriftMethod
      { tmName = name
      , tmReturnType = retType
      , tmParams = params
      , tmThrows = throws
      , tmOneway = ow
      }


typedefP :: Parser ThriftTypedef
typedefP = do
  reserved "typedef"
  ty <- typeP
  name <- identifier
  _ <- annotationsP
  optListSep
  pure ThriftTypedef {ttName = name, ttType = ty}


constP :: Parser ThriftConst
constP = do
  reserved "const"
  ty <- typeP
  name <- identifier
  void (symbol "=")
  val <- constValueP
  optListSep
  pure ThriftConst {tcName = name, tcType = ty, tcValue = val}


constValueP :: Parser ThriftConstValue
constValueP =
  choice
    [ TCVBool True <$ reserved "true"
    , TCVBool False <$ reserved "false"
    , TCVString <$> stringLiteral
    , try constListP
    , try constMapP
    , try (TCVDouble <$> lexeme (L.signed (pure ()) L.float))
    , TCVInt <$> integerLiteral
    , TCVIdent <$> identifier
    ]


constListP :: Parser ThriftConstValue
constListP = do
  void (symbol "[")
  vals <- many (constValueP <* optListSep)
  void (symbol "]")
  pure (TCVList vals)


constMapP :: Parser ThriftConstValue
constMapP = do
  void (symbol "{")
  entries <- many (constMapEntry <* optListSep)
  void (symbol "}")
  pure (TCVMap entries)


constMapEntry :: Parser (ThriftConstValue, ThriftConstValue)
constMapEntry = do
  k <- constValueP
  void (symbol ":")
  v <- constValueP
  pure (k, v)


annotationsP :: Parser (V.Vector (Text, Text))
annotationsP = do
  mAnns <- optional $ parens (many annotationPairP)
  pure $ V.fromList (fromMaybe [] mAnns)
