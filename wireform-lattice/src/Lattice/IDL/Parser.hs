{-# LANGUAGE BangPatterns #-}

{- | Parser and elaborator for the Lattice IDL (spec §3.4): surface text to
the semantic model of "Lattice.Schema".

Elaboration also runs the schema-level checks: dangling type references,
unregistered claims in policies (§3.1), collections whose link field is
missing on the target, interface implementors missing declared fields,
write sets naming unknown collections, and @invalidates ⊇ writes@ (§11.4).

Link-field rule (spec clarification, ruled by Main 2026-07-05): a @has many@
(or field-backed @list@ root) link field must be a declared field of the
target when the target is a single entity. For interface and inline-union
targets the link field must be declared on /every/ member only when at least
one member declares it; a link field declared on no member is a
storage-level join column (resolution is backend-owned via @beChildren@)
and is allowed. The @starwars.lattice@ fixture's
@has many friends: Character by ownerId@ relies on this.

Schema fragment bodies are delegated to
'Lattice.Query.Parser.parseFragmentDef' (the query-language grammar, §4.8);
the IDL parser extracts the balanced-brace declaration text and reports the
query parser's errors at IDL line numbers.

Bounded collections with no explicit @max@ default to 100
(@'maxPageDefault' 'defaultBudgets'@, §3.6): the origin's real 'Budgets'
are not available at parse time, so the parser pins the protocol default.
-}
module Lattice.IDL.Parser (
  SchemaError (..),
  parseSchema,
) where

import Data.Aeson qualified as A
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (Scientific, scientific)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Query.AST (FragmentDefQ (..), QValue (..), TypeRefQ (..), VarDef (..))
import Lattice.Query.Parser (ParseError (..), parseFragmentDef)
import Lattice.Schema
import Lattice.Types
import Numeric.Natural (Natural)


data SchemaError = SchemaError
  { seLine :: Maybe Int
  , seMessage :: Text
  }
  deriving stock (Eq, Show)


-- | Parse and elaborate an IDL document. Parse errors abort with a single
-- error; elaboration collects every check failure.
parseSchema :: Text -> Either [SchemaError] Schema
parseSchema src = do
  toks <- singly (lexIdl src)
  decls <- singly (runP (pDecls src) toks)
  elaborate decls
  where
    singly :: Either SchemaError a -> Either [SchemaError] a
    singly = either (Left . (: [])) Right


-- ---------------------------------------------------------------------------
-- Lexer
-- ---------------------------------------------------------------------------

data TokKind
  = TkName Text
  | TkInt Integer
  | TkNum Scientific
  | TkStr Text
  | TkPunct Char
  deriving stock (Eq, Show)


data Tok = Tok
  { tokLine :: !Int
  , tokStart :: !Int
  -- ^ Code-point offset into the source (for fragment slice extraction).
  , tokEnd :: !Int
  , tokKind :: TokKind
  }


punctChars :: [Char]
punctChars = "{}()[]:,=|?.$@"


{- | Tokenize. @--@ and @#@ comments run to end of line. Identifiers admit
interior hyphens when followed by an alphanumeric (@best-effort@,
@all-or-nothing@); strings are JSON-style. @$@, @\@@, and floating-point
numbers only occur inside fragment bodies, which the declaration parser
skips over by brace counting, but the lexer must carry them.
-}
lexIdl :: Text -> Either SchemaError [Tok]
lexIdl src = go 1 0 (T.unpack src)
  where
    go :: Int -> Int -> String -> Either SchemaError [Tok]
    go !line !ix = \case
      [] -> Right []
      '\n' : cs -> go (line + 1) (ix + 1) cs
      c : cs
        | c == ' ' || c == '\t' || c == '\r' -> go line (ix + 1) cs
      '-' : '-' : cs ->
        let (com, rest) = break (== '\n') cs
         in go line (ix + 2 + length com) rest
      '#' : cs ->
        let (com, rest) = break (== '\n') cs
         in go line (ix + 1 + length com) rest
      '-' : c : cs
        | Char.isDigit c -> lexNumber line ix True (c : cs)
      c : cs
        | Char.isDigit c -> lexNumber line ix False (c : cs)
        | Char.isAlpha c || c == '_' ->
            let (body, rest) = identTail cs
             in emit line ix (1 + length body) (TkName (T.pack (c : body))) rest
        | c == '"' -> lexString line ix cs
        | c `elem` punctChars -> emit line ix 1 (TkPunct c) cs
        | otherwise ->
            Left (SchemaError (Just line) ("unexpected character " <> T.pack (show c)))

    emit :: Int -> Int -> Int -> TokKind -> String -> Either SchemaError [Tok]
    emit line ix n kind rest = (Tok line ix (ix + n) kind :) <$> go line (ix + n) rest

    identTail :: String -> (String, String)
    identTail = \case
      c : cs
        | Char.isAlphaNum c || c == '_' ->
            let (b, r) = identTail cs in (c : b, r)
      '-' : c : cs
        | Char.isAlphaNum c ->
            let (b, r) = identTail cs in ('-' : c : b, r)
      rest -> ("", rest)

    lexNumber :: Int -> Int -> Bool -> String -> Either SchemaError [Tok]
    lexNumber line ix neg s0 =
      let (ipart, r1) = List.span Char.isDigit s0
          (fpart, r2) = case r1 of
            '.' : d : ds
              | Char.isDigit d ->
                  let (f, r) = List.span Char.isDigit (d : ds) in (f, r)
            _ -> ("", r1)
          fracLen = if null fpart then 0 else 1 + length fpart
          (expChars, expVal) = case r2 of
            e : rest
              | e == 'e' || e == 'E' -> case rest of
                  '+' : d : _
                    | Char.isDigit d ->
                        let (f, _) = List.span Char.isDigit (drop 1 rest)
                         in (2 + length f, read f)
                  '-' : d : _
                    | Char.isDigit d ->
                        let (f, _) = List.span Char.isDigit (drop 1 rest)
                         in (2 + length f, negate (read f))
                  d : _
                    | Char.isDigit d ->
                        let (f, _) = List.span Char.isDigit rest
                         in (1 + length f, read f)
                  _ -> (0, 0)
            _ -> (0, 0 :: Integer)
          consumed = (if neg then 1 else 0) + length ipart + fracLen + expChars
          rest' = drop (length ipart + fracLen + expChars) s0
          sign = if neg then (-1) else 1
          kind
            | fracLen == 0 && expChars == 0 = TkInt (sign * read ipart)
            | otherwise =
                let coeff = sign * read (ipart <> fpart)
                    ex = fromInteger expVal - length fpart
                 in TkNum (scientific coeff ex)
       in emit line ix consumed kind rest'

    lexString :: Int -> Int -> String -> Either SchemaError [Tok]
    lexString line ix = loop 1 id
      where
        loop :: Int -> (String -> String) -> String -> Either SchemaError [Tok]
        loop !n acc = \case
          [] -> Left (SchemaError (Just line) "unterminated string literal")
          '"' : rest -> emit line ix (n + 1) (TkStr (T.pack (acc []))) rest
          '\n' : _ -> Left (SchemaError (Just line) "newline in string literal")
          '\\' : e : rest -> case escChar e of
            Just c -> loop (n + 2) (acc . (c :)) rest
            Nothing
              | e == 'u'
              , (h, rest') <- List.splitAt 4 rest
              , length h == 4
              , all Char.isHexDigit h ->
                  loop (n + 6) (acc . (Char.chr (hexVal h) :)) rest'
              | otherwise ->
                  Left (SchemaError (Just line) ("invalid string escape \\" <> T.singleton e))
          c : rest -> loop (n + 1) (acc . (c :)) rest

    escChar :: Char -> Maybe Char
    escChar = \case
      '"' -> Just '"'
      '\\' -> Just '\\'
      '/' -> Just '/'
      'n' -> Just '\n'
      't' -> Just '\t'
      'r' -> Just '\r'
      'b' -> Just '\b'
      'f' -> Just '\f'
      _ -> Nothing

    hexVal :: String -> Int
    hexVal = List.foldl' (\acc c -> acc * 16 + Char.digitToInt c) 0


-- ---------------------------------------------------------------------------
-- Parser core
-- ---------------------------------------------------------------------------

data PSt = PSt
  { psToks :: [Tok]
  , psLine :: !Int
  }


newtype P a = P {unwrapP :: PSt -> Either SchemaError (a, PSt)}


instance Functor P where
  fmap f (P g) = P (\s -> fmap (\(a, s') -> (f a, s')) (g s))


instance Applicative P where
  pure a = P (\s -> Right (a, s))
  P f <*> P g =
    P (\s -> f s >>= \(h, s') -> fmap (\(a, s'') -> (h a, s'')) (g s'))


instance Monad P where
  P g >>= f = P (\s -> g s >>= \(a, s') -> unwrapP (f a) s')


runP :: P a -> [Tok] -> Either SchemaError a
runP (P g) toks = fst <$> g (PSt toks 1)


pFail :: Int -> Text -> P a
pFail l msg = P (\_ -> Left (SchemaError (Just l) msg))


peekT :: P (Maybe Tok)
peekT = P (\s -> Right (listToMaybe (psToks s), s))


-- | The kind of the token @n@ positions ahead (0 = next), without consuming.
peekKindN :: Int -> P (Maybe TokKind)
peekKindN n = P (\s -> Right (tokKind <$> listToMaybe (drop n (psToks s)), s))


advanceT :: P ()
advanceT =
  P
    ( \s -> case psToks s of
        [] -> Right ((), s)
        t : ts -> Right ((), PSt ts (tokLine t))
    )


popT :: Text -> P Tok
popT expected =
  P
    ( \s -> case psToks s of
        [] ->
          Left
            (SchemaError (Just (psLine s)) ("unexpected end of input; expected " <> expected))
        t : ts -> Right (t, PSt ts (tokLine t))
    )


pAnyName :: Text -> P (Text, Int)
pAnyName what = do
  t <- popT what
  case tokKind t of
    TkName n -> pure (n, tokLine t)
    _ -> pFail (tokLine t) ("expected " <> what)


pNameIs :: Text -> P ()
pNameIs kw = do
  t <- popT ("`" <> kw <> "`")
  case tokKind t of
    TkName n | n == kw -> pure ()
    _ -> pFail (tokLine t) ("expected `" <> kw <> "`")


tryNameIs :: Text -> P Bool
tryNameIs kw =
  peekKindN 0 >>= \case
    Just (TkName n) | n == kw -> advanceT $> True
    _ -> pure False


pPunct :: Char -> P ()
pPunct c = do
  t <- popT ("`" <> T.singleton c <> "`")
  case tokKind t of
    TkPunct c' | c' == c -> pure ()
    _ -> pFail (tokLine t) ("expected `" <> T.singleton c <> "`")


tryPunct :: Char -> P Bool
tryPunct c =
  peekKindN 0 >>= \case
    Just (TkPunct c') | c' == c -> advanceT $> True
    _ -> pure False


pNat :: Text -> P Natural
pNat what = do
  t <- popT what
  case tokKind t of
    TkInt n | n >= 0 -> pure (fromInteger n)
    _ -> pFail (tokLine t) ("expected " <> what)


pInteger :: Text -> P Integer
pInteger what = do
  t <- popT what
  case tokKind t of
    TkInt n -> pure n
    _ -> pFail (tokLine t) ("expected " <> what)


-- | @a.b.c@ — used for the schema name and dotted collection names.
pDottedName :: Text -> P (Text, Int)
pDottedName what = do
  (n, l) <- pAnyName what
  go n l
  where
    go acc l = do
      d <- tryPunct '.'
      if d
        then do
          (n2, _) <- pAnyName "a name after `.`"
          go (acc <> "." <> n2) l
        else pure (acc, l)


-- | Commas are never syntax between block items; skip any.
skipCommas :: P ()
skipCommas = do
  c <- tryPunct ','
  if c then skipCommas else pure ()


isCapName :: Text -> Bool
isCapName t = case T.uncons t of
  Just (c, _) -> Char.isUpper c
  Nothing -> False


-- ---------------------------------------------------------------------------
-- Surface declarations
-- ---------------------------------------------------------------------------

data SDecl
  = SDSchema !Int Text
  | SDClaims [(Int, Text, FieldType)]
  | SDType !Int Text TypeDecl
  | SDInterface !Int Text [SItem]
  | SDEntity SEntity
  | SDFragment !Int Text FragmentDef
  | SDRoot SRoot
  | SDMutation SMutation


data SEntity = SEntity
  { sEntLine :: !Int
  , sEntName :: Text
  , sEntKey :: NonEmpty Text
  , sEntImpl :: [(Int, Text)]
  , sEntItems :: [SItem]
  }


data SItem
  = SIDefault !Int Policy
  | SIField !Int Text FieldDef
  | SIRelOne !Int Text (Int, NonEmpty Text) Text (Maybe Policy)
  | SIRelMany !Int Text (Int, NonEmpty Text) Text CollClauses (Maybe Policy)
  | SIFetch !Int (NonEmpty Text) Policy


data CollClauses = CollClauses
  { ccMax :: Maybe Natural
  , ccTrunc :: Bool
  , ccOrder :: [(Text, Direction)]
  , ccPage :: Maybe Natural
  , ccGrouped :: Maybe (NonEmpty Text)
  , ccAs :: Maybe Text
  }


emptyClauses :: CollClauses
emptyClauses = CollClauses Nothing False [] Nothing Nothing Nothing


data SRoot = SRoot
  { srLine :: !Int
  , srKind :: RootKind
  , srName :: Text
  , srParams :: [ArgDef]
  , srTarget :: (Int, NonEmpty Text)
  , srBy :: Maybe Text
  , srClauses :: CollClauses
  , srPolicy :: Policy
  }


data SMutation = SMutation
  { smLine :: !Int
  , smName :: Text
  , smParams :: [ArgDef]
  , smReturns :: Text
  , smAllow :: Maybe Policy
  , smWrites :: Maybe [WriteScopeDecl]
  , smInvalidates :: Maybe InvalidationSpec
  , smEffect :: Maybe EffectClass
  , smErrors :: Maybe (TypeName, Openness, NonEmpty Text)
  , smBatch :: Maybe BatchPolicy
  }


-- ---------------------------------------------------------------------------
-- Declaration parsing
-- ---------------------------------------------------------------------------

pDecls :: Text -> P [SDecl]
pDecls src = go
  where
    go =
      peekT >>= \case
        Nothing -> pure []
        Just t -> do
          d <- pDecl src t
          (d :) <$> go


pDecl :: Text -> Tok -> P SDecl
pDecl src t = do
  advanceT
  let l = tokLine t
  case tokKind t of
    TkName "schema" -> do
      (n, _) <- pDottedName "a schema name"
      pure (SDSchema l n)
    TkName "claims" -> SDClaims <$> pClaimsBlock
    TkName "newtype" -> pNewtypeDecl l
    TkName "enum" -> pEnumDecl l
    TkName "data" -> pDataDecl l
    TkName "interface" -> do
      (n, _) <- pAnyName "an interface name"
      pPunct '{'
      items <- pItems BodyInterface n
      pure (SDInterface l n items)
    TkName "entity" -> pEntityDecl l
    TkName "fragment" -> pFragmentDecl src t
    TkName "get" -> SDRoot <$> pRootDecl RootGet l
    TkName "list" -> SDRoot <$> pRootDecl RootList l
    TkName "mutation" -> SDMutation <$> pMutationDecl l
    _ -> pFail l "expected a declaration (schema, claims, newtype, enum, data, interface, entity, fragment, get, list, or mutation)"


pClaimsBlock :: P [(Int, Text, FieldType)]
pClaimsBlock = do
  pPunct '{'
  go
  where
    go = do
      skipCommas
      done <- tryPunct '}'
      if done
        then pure []
        else do
          (n, l) <- pAnyName "a claim name or `}`"
          pPunct ':'
          ty <- pType
          ((l, n, ty) :) <$> go


pNewtypeDecl :: Int -> P SDecl
pNewtypeDecl l = do
  (n, _) <- pAnyName "a type name"
  pPunct '='
  ty <- pType
  refs <- pRefinements
  pure (SDType l n (DeclNewtype ty refs))


-- | Refinements: only consumed when the parenthesized group opens with a
-- refinement keyword, so @newtype X = Foo@ followed by another declaration
-- never misfires.
pRefinements :: P [Refinement]
pRefinements = do
  k0 <- peekKindN 0
  k1 <- peekKindN 1
  case (k0, k1) of
    (Just (TkPunct '('), Just (TkName kw))
      | kw == "len" || kw == "min" || kw == "max" || kw == "match" -> do
          pPunct '('
          go
    _ -> pure []
  where
    go = do
      r <- pRefinement
      c <- tryPunct ','
      if c then (r :) <$> go else pPunct ')' $> [r]

    pRefinement = do
      (kw, l) <- pAnyName "a refinement (`len`, `min`, `max`, `match`)"
      case kw of
        "len" -> RefLen <$> pNat "a length"
        "min" -> RefMin <$> pInteger "a lower bound"
        "max" -> RefMax <$> pInteger "an upper bound"
        "match" -> do
          t <- popT "a regex string"
          case tokKind t of
            TkStr s -> pure (RefMatch s)
            _ -> pFail (tokLine t) "expected a regex string after `match`"
        _ -> pFail l ("unknown refinement `" <> kw <> "`")


pEnumDecl :: Int -> P SDecl
pEnumDecl l = do
  (n, _) <- pAnyName "an enum name"
  o <- pOpenness
  pPunct '='
  (c0, _) <- pAnyName "an enum constructor"
  cs <- pMore
  pure (SDType l n (DeclEnum o (c0 :| cs)))
  where
    pMore = do
      more <- tryPunct '|'
      if more
        then do
          (c, _) <- pAnyName "an enum constructor"
          (c :) <$> pMore
        else pure []


pDataDecl :: Int -> P SDecl
pDataDecl l = do
  (n, _) <- pAnyName "a type name"
  peekKindN 0 >>= \case
    Just (TkPunct '{') -> do
      fs <- pRecordBody
      pure (SDType l n (DeclRecord fs))
    _ -> do
      o <- pOpenness
      pPunct '='
      c0 <- pCtor
      cs <- pMore
      pure (SDType l n (DeclSum o (c0 :| cs)))
  where
    pMore = do
      more <- tryPunct '|'
      if more
        then do
          c <- pCtor
          (c :) <$> pMore
        else pure []

    pCtor = do
      (cn, _) <- pAnyName "a constructor name"
      fs <-
        peekKindN 0 >>= \case
          Just (TkPunct '{') -> pRecordBody
          _ -> pure []
      pure (Ctor cn fs)


-- | @{ a: T, b: U }@ — commas optional, empty body allowed.
pRecordBody :: P [(FieldName, FieldType)]
pRecordBody = do
  pPunct '{'
  go
  where
    go = do
      skipCommas
      done <- tryPunct '}'
      if done
        then pure []
        else do
          (n, l) <- pAnyName "a field name or `}`"
          pPunct ':'
          ty <- pType
          refuseRefinement l
          ((FieldName n, ty) :) <$> go

    -- Refinements have no slot in record/sum field types (they belong to
    -- newtype declarations, §3.5.2); reject rather than silently drop.
    refuseRefinement l = do
      k0 <- peekKindN 0
      k1 <- peekKindN 1
      case (k0, k1) of
        (Just (TkPunct '('), Just (TkName kw))
          | kw == "len" || kw == "min" || kw == "max" || kw == "match" ->
              pFail l "refinements are only allowed on newtype declarations; declare a refined newtype and use it here"
        _ -> pure ()


pOpenness :: P Openness
pOpenness = do
  t <- popT "`open` or `closed`"
  case tokKind t of
    TkName "open" -> pure Open
    TkName "closed" -> pure Closed
    _ -> pFail (tokLine t) "expected `open` or `closed`"


-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

primOf :: Text -> Maybe Prim
primOf = \case
  "Bool" -> Just PBool
  "Boolean" -> Just PBool
  "I8" -> Just PI8
  "I16" -> Just PI16
  "I32" -> Just PI32
  "I64" -> Just PI64
  "Int" -> Just PI32
  "W8" -> Just PW8
  "W16" -> Just PW16
  "W32" -> Just PW32
  "W64" -> Just PW64
  "Integer" -> Just PInteger
  "Decimal" -> Just PDecimal
  "F32" -> Just PF32
  "F64" -> Just PF64
  "Float" -> Just PF64
  "Text" -> Just PText
  "String" -> Just PText
  "Uuid" -> Just PUuid
  "Timestamp" -> Just PTimestamp
  "Date" -> Just PDate
  "TimeOfDay" -> Just PTimeOfDay
  "Duration" -> Just PDuration
  "Cursor" -> Just PCursor
  "Json" -> Just PJson
  _ -> Nothing


pType :: P FieldType
pType = pBasicType >>= opts
  where
    opts t = do
      q <- tryPunct '?'
      if q then opts (TOptional t) else pure t


{- | A type without trailing @?@: @Set T?@ is @(Set T)?@; parenthesize for
the other reading.
-}
pBasicType :: P FieldType
pBasicType = do
  t <- popT "a type"
  case tokKind t of
    TkPunct '[' -> do
      inner <- pType
      pPunct ']'
      pure (TList inner)
    TkPunct '(' -> do
      inner <- pType
      pPunct ')'
      pure inner
    TkName "Set" -> TSet <$> pBasicType
    TkName "Map" -> TMap <$> pBasicType <*> pBasicType
    TkName "Vec" -> do
      n <- pNat "a vector length"
      TVec n <$> pBasicType
    TkName "Bytes" ->
      peekKindN 0 >>= \case
        Just (TkInt n)
          | n >= 0 -> advanceT $> TPrim (PBytes (Just (fromInteger n)))
        _ -> pure (TPrim (PBytes Nothing))
    TkName "Bit" -> TPrim . PBit <$> pNat "a bit width"
    TkName n
      | Just p <- primOf n -> pure (TPrim p)
      | otherwise -> pure (TNamed (TypeName n))
    _ -> pFail (tokLine t) "expected a type"


-- ---------------------------------------------------------------------------
-- Values (argument defaults)
-- ---------------------------------------------------------------------------

pQValueLit :: P QValue
pQValueLit = do
  t <- popT "a literal value"
  case tokKind t of
    TkInt n -> pure (QInt n)
    TkNum s -> pure (QNum s)
    TkStr s -> pure (QString s)
    TkName "true" -> pure (QBool True)
    TkName "false" -> pure (QBool False)
    TkName n
      | isCapName n -> pure (QEnum n)
      | otherwise -> pFail (tokLine t) ("expected a literal value, saw `" <> n <> "`")
    TkPunct '[' -> do
      empty' <- tryPunct ']'
      if empty' then pure (QList []) else QList <$> go
    _ -> pFail (tokLine t) "expected a literal value"
  where
    go = do
      v <- pQValueLit
      c <- tryPunct ','
      if c then (v :) <$> go else pPunct ']' $> [v]


pParamList :: P [ArgDef]
pParamList = do
  pPunct '('
  empty' <- tryPunct ')'
  if empty' then pure [] else go
  where
    go = do
      a <- pArgDef
      c <- tryPunct ','
      if c then (a :) <$> go else pPunct ')' $> [a]

    pArgDef = do
      (n, _) <- pAnyName "an argument name"
      pPunct ':'
      ty <- pType
      eq <- tryPunct '='
      d <- if eq then Just <$> pQValueLit else pure Nothing
      pure ArgDef {adName = ArgName n, adType = ty, adDefault = d}


-- ---------------------------------------------------------------------------
-- Policies and predicates
-- ---------------------------------------------------------------------------

pPolicyFull :: Text -> P Policy
pPolicyFull what = do
  t <- popT what
  case tokKind t of
    TkName "public" -> pure Public
    TkName "private" -> pure Private
    TkName "visible" -> do
      pNameIs "when"
      RequiresClaims <$> pPredicates
    _ ->
      pFail
        (tokLine t)
        ("expected " <> what <> " (`public`, `private`, or `visible when ...`)")


{- | An optional trailing policy on a field or relationship. Guarded against
the next item being a default-visibility declaration (@private by default@,
@visible to all by default@).
-}
tryPolicy :: P (Maybe Policy)
tryPolicy = do
  k0 <- peekKindN 0
  k1 <- peekKindN 1
  case k0 of
    Just (TkName "public") -> advanceT $> Just Public
    Just (TkName "private")
      | Just (TkName "by") <- k1 -> pure Nothing
      | otherwise -> advanceT $> Just Private
    Just (TkName "visible")
      | Just (TkName "when") <- k1 -> do
          advanceT
          advanceT
          Just . RequiresClaims <$> pPredicates
      | otherwise -> pure Nothing
    _ -> pure Nothing


pPredicates :: P [ClaimPredicate]
pPredicates = do
  p <- pPredicate
  more <- tryNameIs "and"
  if more then (p :) <$> pPredicates else pure [p]


pPredicate :: P ClaimPredicate
pPredicate = do
  pNameIs "caller"
  pPunct '.'
  (c, _) <- pAnyName "a claim name"
  t <- popT "`=` or `in`"
  case tokKind t of
    TkPunct '=' -> ClaimPredicate (ClaimName c) <$> pRhs
    TkName "in" -> do
      pPunct '['
      vs <- pLits
      pure (ClaimPredicate (ClaimName c) (RhsOneOf vs))
    _ -> pFail (tokLine t) "expected `=` or `in` in a claim predicate"
  where
    pLits = do
      v <- pLiteral
      c <- tryPunct ','
      if c then (v :) <$> pLits else pPunct ']' $> [v]


{- | Predicate right-hand side: a bare lowercase name is a field of the
entity under inspection; a capitalized bare name is an enum literal; quoted
strings, numbers, and booleans are literals.
-}
pRhs :: P PredRhs
pRhs = do
  t <- popT "a predicate right-hand side"
  case tokKind t of
    TkStr s -> pure (RhsLiteral (A.String s))
    TkInt n -> pure (RhsLiteral (A.Number (fromInteger n)))
    TkNum s -> pure (RhsLiteral (A.Number s))
    TkName "true" -> pure (RhsLiteral (A.Bool True))
    TkName "false" -> pure (RhsLiteral (A.Bool False))
    TkName n
      | isCapName n -> pure (RhsLiteral (A.String n))
      | otherwise -> pure (RhsField (FieldName n))
    _ -> pFail (tokLine t) "expected a field name or literal"


pLiteral :: P A.Value
pLiteral = do
  t <- popT "a literal"
  case tokKind t of
    TkStr s -> pure (A.String s)
    TkInt n -> pure (A.Number (fromInteger n))
    TkNum s -> pure (A.Number s)
    TkName "true" -> pure (A.Bool True)
    TkName "false" -> pure (A.Bool False)
    TkName n
      | isCapName n -> pure (A.String n)
      | otherwise ->
          pFail (tokLine t) ("expected a literal; field references like `" <> n <> "` are not allowed in `in` lists")
    _ -> pFail (tokLine t) "expected a literal"


-- ---------------------------------------------------------------------------
-- Entity and interface bodies
-- ---------------------------------------------------------------------------

data BodyKind = BodyEntity | BodyInterface
  deriving stock (Eq)


pEntityDecl :: Int -> P SDecl
pEntityDecl l = do
  (n, _) <- pAnyName "an entity name"
  pNameIs "by"
  key <- pKeySpec
  impls <- pImplements
  pPunct '{'
  items <- pItems BodyEntity n
  pure (SDEntity (SEntity l n key impls items))
  where
    pImplements = do
      has <- tryNameIs "implements"
      if has then go else pure []
      where
        go = do
          (i, il) <- pAnyName "an interface name"
          c <- tryPunct ','
          if c then ((il, i) :) <$> go else pure [(il, i)]


-- | @id@ or @(orgId, seq)@.
pKeySpec :: P (NonEmpty Text)
pKeySpec = do
  paren <- tryPunct '('
  if paren
    then do
      (k0, _) <- pAnyName "a key field"
      ks <- go
      pure (k0 :| ks)
    else do
      (k, _) <- pAnyName "a key field"
      pure (k :| [])
  where
    go = do
      c <- tryPunct ','
      if c
        then do
          (k, _) <- pAnyName "a key field"
          (k :) <$> go
        else pPunct ')' $> []


pItems :: BodyKind -> Text -> P [SItem]
pItems bk owner = go
  where
    go = do
      skipCommas
      done <- tryPunct '}'
      if done
        then pure []
        else do
          it <- pItem
          (it :) <$> go

    pItem = do
      t <- popT "a field, relationship, or `}`"
      let l = tokLine t
      case tokKind t of
        TkName "visible"
          | bk == BodyEntity ->
              peekKindN 0 >>= \case
                Just (TkName "to") -> do
                  pNameIs "to"
                  pNameIs "all"
                  pNameIs "by"
                  pNameIs "default"
                  pure (SIDefault l Public)
                Just (TkName "when") -> do
                  pNameIs "when"
                  ps <- pPredicates
                  pNameIs "by"
                  pNameIs "default"
                  pure (SIDefault l (RequiresClaims ps))
                _ -> pFail l "expected `visible to all by default` or `visible when ... by default`"
        TkName "private"
          | bk == BodyEntity -> do
              pNameIs "by"
              pNameIs "default"
              pure (SIDefault l Private)
        TkName "has" -> pRelItem l
        TkName "fetch"
          | bk == BodyEntity -> do
              pNameIs "by"
              key <- pKeySpec
              pPunct ':'
              pol <- pPolicyFull "a fetch policy"
              pure (SIFetch l key pol)
        TkName n -> pFieldItem l n
        _ -> pFail l "expected a field, relationship, or `}`"

    pFieldItem l n = do
      args <-
        peekKindN 0 >>= \case
          Just (TkPunct '(') -> pParamList
          _ -> pure []
      pPunct ':'
      ty <- pType
      pol <- tryPolicy
      pure (SIField l n (FieldDef {fieldType = ty, fieldArgs = args, fieldPolicy = pol}))

    pRelItem l = do
      t <- popT "`one` or `many`"
      which <- case tokKind t of
        TkName "one" -> pure True
        TkName "many" -> pure False
        _ -> pFail (tokLine t) "expected `one` or `many` after `has`"
      (n, _) <- pAnyName "a relationship name"
      pPunct ':'
      tgt <- pTargetSurface
      pNameIs "by"
      (link, _) <- pAnyName "a link field"
      if which
        then do
          pol <- tryPolicy
          pure (SIRelOne l n tgt link pol)
        else do
          cls <- pCollClauses owner
          pol <- tryPolicy
          pure (SIRelMany l n tgt link cls pol)


-- | @T@ or @(A | B | C)@.
pTargetSurface :: P (Int, NonEmpty Text)
pTargetSurface = do
  t <- popT "a target type"
  case tokKind t of
    TkName n -> pure (tokLine t, n :| [])
    TkPunct '(' -> do
      (n0, _) <- pAnyName "a target type"
      ns <- go
      pure (tokLine t, n0 :| ns)
    _ -> pFail (tokLine t) "expected a target type or `(A | B)` union"
  where
    go = do
      bar <- tryPunct '|'
      if bar
        then do
          (n, _) <- pAnyName "a target type"
          (n :) <$> go
        else pPunct ')' $> []


{- | Collection clauses in any order: @max N [truncate]@, @ordered by f dir
[, ...]@, @page N@, @grouped by f [, ...]@, @as name@. Each keyword only
consumes when the following tokens match its shape, so a field named @max@
on the next line never misfires.
-}
pCollClauses :: Text -> P CollClauses
pCollClauses ctx = go emptyClauses
  where
    go cc = do
      k0 <- peekKindN 0
      k1 <- peekKindN 1
      k2 <- peekKindN 2
      case k0 of
        Just (TkName "max")
          | Just (TkInt _) <- k1 -> do
              dup (ccMax cc) "max"
              advanceT
              n <- pNat "a maximum"
              tr <- tryNameIs "truncate"
              go cc {ccMax = Just n, ccTrunc = ccTrunc cc || tr}
        Just (TkName "page")
          | Just (TkInt _) <- k1 -> do
              dup (ccPage cc) "page"
              advanceT
              n <- pNat "a page size"
              go cc {ccPage = Just n}
        Just (TkName "ordered")
          | Just (TkName "by") <- k1 -> do
              case ccOrder cc of
                [] -> pure ()
                _ -> failHere "duplicate `ordered by` clause"
              advanceT
              advanceT
              cols <- pOrderCols
              go cc {ccOrder = cols}
        Just (TkName "grouped")
          | Just (TkName "by") <- k1 -> do
              dup (ccGrouped cc) "grouped by"
              advanceT
              advanceT
              (f0, _) <- pAnyName "a grouping field"
              fs <- pMoreNames
              go cc {ccGrouped = Just (f0 :| fs)}
        Just (TkName "as")
          | Just (TkName _) <- k1
          , k2 /= Just (TkPunct ':') -> do
              dup (ccAs cc) "as"
              advanceT
              (n, _) <- pDottedName "a collection name"
              go cc {ccAs = Just n}
        _ -> pure cc

    dup :: Maybe a -> Text -> P ()
    dup Nothing _ = pure ()
    dup (Just _) what = failHere ("duplicate `" <> what <> "` clause")

    failHere msg =
      peekT >>= \case
        Just t -> pFail (tokLine t) (msg <> " in `" <> ctx <> "`")
        Nothing -> pFail 0 (msg <> " in `" <> ctx <> "`")

    pOrderCols = do
      (f, _) <- pAnyName "an ordering field"
      d <- pDirection
      c <- tryPunct ','
      if c then ((f, d) :) <$> pOrderCols else pure [(f, d)]

    pDirection = do
      t <- popT "`asc` or `desc`"
      case tokKind t of
        TkName "asc" -> pure Asc
        TkName "desc" -> pure Desc
        _ -> pFail (tokLine t) "expected `asc` or `desc`"

    pMoreNames = do
      c <- tryPunct ','
      if c
        then do
          (f, _) <- pAnyName "a grouping field"
          (f :) <$> pMoreNames
        else pure []


-- ---------------------------------------------------------------------------
-- Fragments
-- ---------------------------------------------------------------------------

{- | Extract the balanced-brace declaration text and delegate to the query
language parser ('parseFragmentDef'), reporting its errors at IDL lines.
-}
pFragmentDecl :: Text -> Tok -> P SDecl
pFragmentDecl src t0 = do
  skipToOpenBrace
  endOff <- balance (1 :: Int)
  let slice = codeSlice (tokStart t0) endOff
  case parseFragmentDef slice of
    Left pe ->
      let l = tokLine t0 + T.count "\n" (T.take (peOffset pe) slice)
       in pFail l ("in fragment: " <> peMessage pe)
    Right fd ->
      pure
        ( SDFragment
            (tokLine t0)
            (unFragmentName (fdName fd))
            (FragmentDef {fragOn = fdOn fd, fragParams = fdParams fd, fragSelection = fdSelection fd})
        )
  where
    codeSlice s e = T.take (e - s) (T.drop s src)

    skipToOpenBrace = do
      t <- popT "`{` in a fragment declaration"
      case tokKind t of
        TkPunct '{' -> pure ()
        _ -> skipToOpenBrace

    balance depth = do
      t <- popT "`}` closing a fragment"
      case tokKind t of
        TkPunct '{' -> balance (depth + 1)
        TkPunct '}'
          | depth == 1 -> pure (tokEnd t)
          | otherwise -> balance (depth - 1)
        _ -> balance depth


-- ---------------------------------------------------------------------------
-- Roots
-- ---------------------------------------------------------------------------

pRootDecl :: RootKind -> Int -> P SRoot
pRootDecl kind l = do
  (n, _) <- pAnyName "a root name"
  params <-
    peekKindN 0 >>= \case
      Just (TkPunct '(') -> pParamList
      _ -> pure []
  pNameIs "of"
  tgt <- pTargetSurface
  (by, cls) <- case kind of
    RootGet -> pure (Nothing, emptyClauses)
    RootList -> do
      hasBy <- tryNameIs "by"
      by <-
        if hasBy
          then Just . fst <$> pAnyName "a link field"
          else pure Nothing
      cls <- pCollClauses n
      pure (by, cls)
  pol <- pPolicyFull "a root policy"
  pure
    SRoot
      { srLine = l
      , srKind = kind
      , srName = n
      , srParams = params
      , srTarget = tgt
      , srBy = by
      , srClauses = cls
      , srPolicy = pol
      }


-- ---------------------------------------------------------------------------
-- Mutations
-- ---------------------------------------------------------------------------

pMutationDecl :: Int -> P SMutation
pMutationDecl l = do
  (n, _) <- pAnyName "a mutation name"
  params <-
    peekKindN 0 >>= \case
      Just (TkPunct '(') -> pParamList
      _ -> pure []
  pNameIs "returns"
  (ret, _) <- pAnyName "a return type"
  pPunct '{'
  loop
    SMutation
      { smLine = l
      , smName = n
      , smParams = params
      , smReturns = ret
      , smAllow = Nothing
      , smWrites = Nothing
      , smInvalidates = Nothing
      , smEffect = Nothing
      , smErrors = Nothing
      , smBatch = Nothing
      }
  where
    loop sm = do
      done <- tryPunct '}'
      if done
        then pure sm
        else do
          t <- popT "a mutation clause or `}`"
          let cl = tokLine t
          case tokKind t of
            TkName "allow" -> do
              dup cl (smAllow sm) "allow"
              g <- pAllow
              loop sm {smAllow = Just g}
            TkName "writes" -> do
              dup cl (smWrites sm) "writes"
              ws <- pWriteScopes
              loop sm {smWrites = Just ws}
            TkName "invalidates" -> do
              dup cl (smInvalidates sm) "invalidates"
              pNameIs "writes"
              c <- tryPunct ','
              inv <-
                if c
                  then WritesPlus <$> pWriteScopes
                  else pure ExactlyWrites
              loop sm {smInvalidates = Just inv}
            TkName "effect" -> do
              dup cl (smEffect sm) "effect"
              e <- pEffect
              loop sm {smEffect = Just e}
            TkName "errors" -> do
              dup cl (smErrors sm) "errors"
              (en, _) <- pAnyName "an error type name"
              o <- pOpenness
              pPunct '='
              (c0, _) <- pAnyName "an error constructor"
              cs <- pMoreCtors
              loop sm {smErrors = Just (TypeName en, o, c0 :| cs)}
            TkName "batch" -> do
              dup cl (smBatch sm) "batch"
              at <- pAtomicity
              pNameIs "max"
              mx <- pNat "a batch maximum"
              loop sm {smBatch = Just (BatchPolicy {bpAtomicity = at, bpMaxItems = mx})}
            _ ->
              pFail cl "expected a mutation clause (allow, writes, invalidates, effect, errors, batch) or `}`"

    dup :: Int -> Maybe a -> Text -> P ()
    dup _ Nothing _ = pure ()
    dup cl (Just _) what = pFail cl ("duplicate `" <> what <> "` clause")

    pAllow = do
      t <- popT "`public`, `private`, or `when`"
      case tokKind t of
        TkName "public" -> pure Public
        TkName "private" -> pure Private
        TkName "when" -> RequiresClaims <$> pPredicates
        _ -> pFail (tokLine t) "expected `public`, `private`, or `when ...` after `allow`"

    pEffect = do
      t <- popT "an effect class"
      case tokKind t of
        TkName "transactional" -> pure Transactional
        TkName "workflow" -> pure Workflow
        TkName "natural" ->
          peekKindN 0 >>= \case
            Just (TkStr _) -> do
              j <- popT "a justification"
              case tokKind j of
                TkStr s -> pure (NaturallyIdempotent s)
                _ -> pFail (tokLine j) "expected a justification string"
            _ -> pure (NaturallyIdempotent "")
        _ -> pFail (tokLine t) "expected `transactional`, `natural`, or `workflow`"

    pAtomicity = do
      t <- popT "`best-effort` or `all-or-nothing`"
      case tokKind t of
        TkName "best-effort" -> pure BestEffort
        TkName "all-or-nothing" -> pure AllOrNothing
        _ -> pFail (tokLine t) "expected `best-effort` or `all-or-nothing`"

    pMoreCtors = do
      more <- tryPunct '|'
      if more
        then do
          (c, _) <- pAnyName "an error constructor"
          (c :) <$> pMoreCtors
        else pure []


pWriteScopes :: P [WriteScopeDecl]
pWriteScopes = do
  s <- pWriteScope
  c <- tryPunct ','
  if c then (s :) <$> pWriteScopes else pure [s]


{- | @Post(post)@, @Review(new)@, @feed(Post.orgId)@, @reviews(episode)@.
A dotted or lowercase head is a collection; a capitalized undotted head is
an entity (collections are lowercase or dotted by construction: root names
and @Parent.field@ auto-names).
-}
pWriteScope :: P WriteScopeDecl
pWriteScope = do
  (h, hl) <- pDottedName "a write scope"
  let dotted = T.any (== '.') h
      entityHead = isCapName h && not dotted
  pPunct '('
  (i1, _) <- pAnyName "a write-scope key"
  dot <- tryPunct '.'
  scope <-
    if dot
      then do
        (f, _) <- pAnyName "a field name"
        pure (WCollection (CollectionName h) (GroupOfWritten (TypeName i1) (FieldName f)))
      else
        if entityHead
          then
            if i1 == "new"
              then pure (WEntity (TypeName h) KeyNew)
              else pure (WEntity (TypeName h) (KeyArg (ArgName i1)))
          else
            if i1 == "new"
              then pFail hl ("`new` is only valid in an entity write scope, not collection `" <> h <> "`")
              else pure (WCollection (CollectionName h) (GroupArg (ArgName i1)))
  pPunct ')'
  pure scope


-- ---------------------------------------------------------------------------
-- Elaboration
-- ---------------------------------------------------------------------------

-- | The default @max@ of a bounded collection when omitted (§3.6). The
-- origin's real 'Budgets' are unknown at parse time; this is the protocol
-- default ('maxPageDefault' of 'defaultBudgets').
defaultMax :: Natural
defaultMax = maxPageDefault defaultBudgets


err :: Int -> Text -> SchemaError
err l = SchemaError (Just l)


data EntInfo = EntInfo
  { eiLine :: !Int
  , eiKey :: NonEmpty Text
  , eiImpl :: [(Int, Text)]
  , eiFields :: Map FieldName FieldDef
  , eiFieldLines :: Map FieldName Int
  , eiRels :: [SItem]
  , eiDefaults :: [(Int, Policy)]
  , eiFetches :: [(Int, NonEmpty Text, Policy)]
  }


elaborate :: [SDecl] -> Either [SchemaError] Schema
elaborate decls =
  if null allErrs then Right schema else Left allErrs
  where
    -- ---- buckets --------------------------------------------------------
    schemaDs = mapMaybe (\case SDSchema l n -> Just (l, n); _ -> Nothing) decls
    claimDs = concatMap (\case SDClaims es -> es; _ -> []) decls
    typeDs = mapMaybe (\case SDType l n d -> Just (l, n, d); _ -> Nothing) decls
    ifaceDs = mapMaybe (\case SDInterface l n items -> Just (l, n, items); _ -> Nothing) decls
    entityDs = mapMaybe (\case SDEntity e -> Just e; _ -> Nothing) decls
    fragDs = mapMaybe (\case SDFragment l n f -> Just (l, n, f); _ -> Nothing) decls
    rootDs = mapMaybe (\case SDRoot r -> Just r; _ -> Nothing) decls
    mutDs = mapMaybe (\case SDMutation m -> Just m; _ -> Nothing) decls

    -- ---- schema name ----------------------------------------------------
    (sName, nameErrs) = case schemaDs of
      [] -> ("", [SchemaError Nothing "missing `schema <name>` declaration"])
      (_, n) : rest ->
        (n, map (\(l, _) -> err l "duplicate `schema` declaration") rest)

    -- ---- claims ---------------------------------------------------------
    (claims, claimDupErrs) =
      collectMap "claim" unClaimName (map (\(l, n, t) -> (l, ClaimName n, t)) claimDs)
    claimTyErrs =
      concatMap (\(l, n, t) -> tyRefErrs l ("claim `" <> n <> "`") t) claimDs

    -- ---- type/entity/interface namespaces --------------------------------
    (types, typeDupErrs) =
      collectMap "type" unTypeName (map (\(l, n, d) -> (l, TypeName n, d)) typeDs)
    typeBodyErrs = concatMap typeDeclErrs typeDs

    typeDeclErrs (l, n, d) =
      let ctx = "type `" <> n <> "`"
       in case d of
            DeclNewtype t _ -> tyRefErrs l ctx t
            DeclRecord fs -> concatMap (tyRefErrs l ctx . snd) fs
            DeclSum _ cs ->
              concatMap (\c -> concatMap (tyRefErrs l ctx . snd) (ctorFields c)) (NE.toList cs)
            DeclEnum _ _ -> []

    (entInfos, entErrs) = buildEntInfos entityDs
    (ifaceInfos, ifaceDupErrs) = buildIfaceInfos ifaceDs

    entityNames = Map.keysSet entInfos
    ifaceNames = Map.keysSet ifaceInfos

    typeNames = Set.map unTypeName (Map.keysSet types)

    crossDupErrs =
      concatMap
        ( \e ->
            if Set.member (sEntName e) typeNames
              then [err (sEntLine e) ("entity `" <> sEntName e <> "` duplicates a type declaration")]
              else []
        )
        entityDs
        <> concatMap
          ( \(l, n, _) ->
              if Set.member n typeNames || Set.member n entityNames
                then [err l ("interface `" <> n <> "` duplicates a type or entity name")]
                else []
          )
          ifaceDs

    -- iface name -> member entity names (from `implements`)
    implIndex :: Map Text (Set TypeName)
    implIndex =
      Map.foldrWithKey
        ( \en ei acc ->
            List.foldl'
              (\m (_, i) -> Map.insertWith Set.union i (Set.singleton (TypeName en)) m)
              acc
              (eiImpl ei)
        )
        (Map.map (const Set.empty) ifaceInfos)
        entInfos

    -- ---- type reference checking -----------------------------------------
    tyRefErrs :: Int -> Text -> FieldType -> [SchemaError]
    tyRefErrs l ctx = go
      where
        go = \case
          TPrim _ -> []
          TNamed (TypeName n)
            | Map.member (TypeName n) types -> []
            | Set.member n entityNames ->
                [err l ("entity type `" <> n <> "` cannot appear in a value-type position (in " <> ctx <> "); edges are relationships")]
            | Set.member n ifaceNames ->
                [err l ("interface `" <> n <> "` cannot appear in a value-type position (in " <> ctx <> ")")]
            | otherwise -> [err l ("unknown type `" <> n <> "` in " <> ctx)]
          TOptional t -> go t
          TList t -> go t
          TSet t -> go t
          TMap k v -> go k <> go v
          TVec _ t -> go t

    argErrs :: Int -> Text -> [ArgDef] -> [SchemaError]
    argErrs l ctx = concatMap (\a -> tyRefErrs l (ctx <> ", argument `" <> unArgName (adName a) <> "`") (adType a))

    -- ---- policy checking --------------------------------------------------
    -- Field-existence oracle differs per context (entity fields, target
    -- members, mutation params).
    policyErrs :: Int -> Text -> (Text -> Bool) -> Policy -> [SchemaError]
    policyErrs l ctx fieldOk = \case
      Public -> []
      Private -> []
      RequiresClaims ps -> concatMap predErrs ps
      where
        predErrs (ClaimPredicate c rhs) =
          claimErr c <> rhsErr rhs
        claimErr c =
          if Map.member c claims
            then []
            else [err l ("policy in " <> ctx <> " references unregistered claim `" <> unClaimName c <> "`")]
        rhsErr = \case
          RhsField (FieldName f)
            | fieldOk f -> []
            | otherwise -> [err l ("policy in " <> ctx <> " compares against unknown field `" <> f <> "`")]
          RhsLiteral _ -> []
          RhsOneOf _ -> []

    -- ---- target resolution -------------------------------------------------
    resolveTarget :: Text -> (Int, NonEmpty Text) -> (Target, [SchemaError])
    resolveTarget ctx (l, names) = case names of
      n :| []
        | Set.member n entityNames -> (TargetEntity (TypeName n), [])
        | Set.member n ifaceNames -> (TargetInterface (InterfaceName n), [])
        | otherwise ->
            (TargetEntity (TypeName n), [err l ("unknown target `" <> n <> "` in " <> ctx)])
      _ ->
        let bad =
              concatMap
                ( \n ->
                    if Set.member n entityNames
                      then []
                      else [err l ("union member `" <> n <> "` in " <> ctx <> " is not an entity")]
                )
                (NE.toList names)
         in (TargetUnion (fmap TypeName names), bad)

    -- Concrete member entities of a target, with their surface field maps.
    memberFields :: Target -> [(Text, Map FieldName FieldDef)]
    memberFields = \case
      TargetEntity (TypeName n) -> lookupOne n
      TargetUnion ns -> concatMap (\(TypeName n) -> lookupOne n) (NE.toList ns)
      TargetInterface (InterfaceName i) ->
        case Map.lookup i implIndex of
          Nothing -> []
          Just ms -> concatMap (\(TypeName n) -> lookupOne n) (Set.toList ms)
      where
        lookupOne n = case Map.lookup n entInfos of
          Nothing -> []
          Just ei -> [(n, eiFields ei)]

    hasField :: Map FieldName FieldDef -> Text -> Bool
    hasField m f = Map.member (FieldName f) m

    -- Link-field rule (see module haddock): strict on single entities;
    -- all-or-nothing on interface/union members.
    linkErrs :: Int -> Text -> Target -> FieldName -> [SchemaError]
    linkErrs l ctx tgt (FieldName link) =
      case tgt of
        TargetEntity _ -> case members of
          [(n, fm)]
            | not (hasField fm link) ->
                [err l ("link field `" <> link <> "` in " <> ctx <> " is not a declared field of `" <> n <> "`")]
          _ -> []
        _ ->
          let declaring = filter (\(_, fm) -> hasField fm link) members
           in if null declaring || length declaring == length members
                then []
                else
                  map
                    ( \(n, _) ->
                        err l ("link field `" <> link <> "` in " <> ctx <> " is declared on some target members but not on `" <> n <> "`")
                    )
                    (filter (\(_, fm) -> not (hasField fm link)) members)
      where
        members = memberFields tgt

    columnErrs :: Int -> Text -> Text -> Target -> [Text] -> [SchemaError]
    columnErrs l what ctx tgt cols =
      concatMap
        ( \(n, fm) ->
            concatMap
              ( \c ->
                  if hasField fm c
                    then []
                    else [err l (what <> " `" <> c <> "` in " <> ctx <> " is not a field of `" <> n <> "`")]
              )
              cols
        )
        (memberFields tgt)

    -- ---- collections ---------------------------------------------------------
    buildWindow :: Int -> Text -> CollClauses -> (Windowing, [SchemaError])
    buildWindow l ctx cc = case (ccOrder cc, ccPage cc) of
      ([], Nothing) ->
        ( Bounded (fromMaybe defaultMax (ccMax cc)) (if ccTrunc cc then Truncate else Overflow)
        , []
        )
      ([], Just _) ->
        ( Bounded (fromMaybe defaultMax (ccMax cc)) Overflow
        , [err l ("`page` in " <> ctx <> " requires an `ordered by` keyset")]
        )
      (o : os, _) ->
        ( Paginated
            CursorSpec
              { csKeyset = fmap (\(f, d) -> (FieldName f, d)) (o :| os)
              , csDefaultPage = ccPage cc
              , csMaxPage = fromMaybe defaultMax (ccMax cc)
              , csTotal = CountNone
              }
        , if ccTrunc cc
            then [err l ("`truncate` in " <> ctx <> " applies only to bounded collections")]
            else []
        )

    buildCollectionDef
      :: Int
      -> Text -- context for messages
      -> Target
      -> CollectionName
      -> Text -- link field
      -> NonEmpty Text -- default grouping
      -> CollClauses
      -> (CollectionDef, [SchemaError])
    buildCollectionDef l ctx tgt cname link defGroup cc =
      let (win, winErrs) = buildWindow l ctx cc
          grouping = fromMaybe defGroup (ccGrouped cc)
          groupedErrs = case ccGrouped cc of
            Nothing -> []
            Just gs -> columnErrs l "grouping field" ctx tgt (NE.toList gs)
          keysetErrs = case win of
            Paginated cs ->
              columnErrs l "keyset column" ctx tgt (map (unFieldName . fst) (NE.toList (csKeyset cs)))
            Bounded _ _ -> []
       in ( CollectionDef
              { colLink = FieldName link
              , colName = cname
              , colGrouping = fmap FieldName grouping
              , colWindow = win
              }
          , winErrs <> groupedErrs <> keysetErrs
          )

    -- ---- entities --------------------------------------------------------------
    entityResults :: [(TypeName, EntityDef, [(Int, CollectionName)], [SchemaError])]
    entityResults = map buildEntity (Map.toList entInfos)

    buildEntity :: (Text, EntInfo) -> (TypeName, EntityDef, [(Int, CollectionName)], [SchemaError])
    buildEntity (en, ei) =
      (TypeName en, def, colls, errs)
      where
        ownField = hasField (eiFields ei)
        ectx = "entity `" <> en <> "`"

        (defPol, defErrs) = case eiDefaults ei of
          [] ->
            (Public, [err (eiLine ei) (ectx <> " is missing its default visibility (`visible to all by default` or `private by default`)")])
          (l0, p) : rest ->
            ( p
            , map (\(l, _) -> err l ("duplicate default visibility in " <> ectx)) rest
                <> policyErrs l0 (ectx <> " default visibility") ownField p
            )

        keyErrs =
          concatMap
            ( \k ->
                if ownField k
                  then []
                  else [err (eiLine ei) (ectx <> " key field `" <> k <> "` is not declared")]
            )
            (NE.toList (eiKey ei))

        fieldErrs =
          Map.foldrWithKey
            ( \(FieldName f) fd acc ->
                let l = fromMaybe (eiLine ei) (Map.lookup (FieldName f) (eiFieldLines ei))
                    fctx = ectx <> ", field `" <> f <> "`"
                 in tyRefErrs l fctx (fieldType fd)
                      <> argErrs l fctx (fieldArgs fd)
                      <> maybe [] (policyErrs l fctx ownField) (fieldPolicy fd)
                      <> acc
            )
            []
            (eiFields ei)

        (rels, colls, relErrs) = List.foldl' stepRel (Map.empty, [], []) (eiRels ei)

        stepRel (m, cs, es) = \case
          SIRelOne l n tgtS byF pol ->
            let (tgt, tErrs) = resolveTarget (ectx <> ", relationship `" <> n <> "`") tgtS
                rctx = ectx <> ", relationship `" <> n <> "`"
                byErrs =
                  if ownField byF
                    then []
                    else [err l ("`has one` key field `" <> byF <> "` in " <> rctx <> " is not a declared field of `" <> en <> "`")]
                polErrs = maybe [] (policyErrs l rctx ownField) pol
                (m', dupE) = insertRel l n (ToOne {relTarget = tgt, relByField = FieldName byF, relPolicy = pol}) m
             in (m', cs, es <> tErrs <> byErrs <> polErrs <> dupE)
          SIRelMany l n tgtS link cc pol ->
            let rctx = ectx <> ", relationship `" <> n <> "`"
                (tgt, tErrs) = resolveTarget rctx tgtS
                cname = CollectionName (fromMaybe (en <> "." <> n) (ccAs cc))
                (cdef, cErrs) = buildCollectionDef l rctx tgt cname link (link :| []) cc
                lErrs = linkErrs l rctx tgt (FieldName link)
                polErrs = maybe [] (policyErrs l rctx ownField) pol
                (m', dupE) = insertRel l n (ToMany {relTarget = tgt, relCollection = cdef, relPolicy = pol}) m
             in (m', cs <> [(l, cname)], es <> tErrs <> cErrs <> lErrs <> polErrs <> dupE)
          _ -> (m, cs, es)

        insertRel l n rel m
          | Map.member (FieldName n) (eiFields ei) =
              (m, [err l ("relationship `" <> n <> "` in " <> ectx <> " duplicates a field name")])
          | Map.member (FieldName n) m =
              (m, [err l ("duplicate relationship `" <> n <> "` in " <> ectx)])
          | otherwise = (Map.insert (FieldName n) rel m, [])

        (fetchBy, fetchErrs) = case eiFetches ei of
          [] -> (Nothing, [])
          (l, keys, p) : rest ->
            ( Just p
            , (if NE.toList keys == NE.toList (eiKey ei) then [] else [err l ("`fetch by` key in " <> ectx <> " must match the entity key")])
                <> policyErrs l (ectx <> " fetch policy") ownField p
                <> map (\(l', _, _) -> err l' ("duplicate `fetch by` in " <> ectx)) rest
            )

        implErrs =
          concatMap
            ( \(l, i) ->
                if Set.member i ifaceNames
                  then implementsErrs l i
                  else [err l (ectx <> " implements unknown interface `" <> i <> "`")]
            )
            (eiImpl ei)

        implementsErrs l i = case Map.lookup i ifaceInfos of
          Nothing -> []
          Just (_, ifFields, ifRels) ->
            Map.foldrWithKey
              ( \(FieldName f) fd acc ->
                  case Map.lookup (FieldName f) (eiFields ei) of
                    Nothing ->
                      err l (ectx <> " implements `" <> i <> "` but does not declare its field `" <> f <> "`") : acc
                    Just efd
                      | fieldType efd /= fieldType fd || fieldArgs efd /= fieldArgs fd ->
                          err l (ectx <> " declares field `" <> f <> "` with a type incompatible with interface `" <> i <> "`") : acc
                      | otherwise -> acc
              )
              relImplErrs
              ifFields
            where
              relImplErrs =
                concatMap
                  ( \relItem -> case relItem of
                      SIRelOne _ n _ _ _ -> checkRelName n
                      SIRelMany _ n _ _ _ _ -> checkRelName n
                      _ -> []
                  )
                  ifRels
              checkRelName n =
                if any (relNamed n) (eiRels ei)
                  then []
                  else [err l (ectx <> " implements `" <> i <> "` but does not declare its relationship `" <> n <> "`")]
              relNamed n = \case
                SIRelOne _ n' _ _ _ -> n == n'
                SIRelMany _ n' _ _ _ _ -> n == n'
                _ -> False

        def =
          EntityDef
            { entityKey = fmap FieldName (eiKey ei)
            , entityDefaultPolicy = defPol
            , entityFields = eiFields ei
            , entityRels = rels
            , entityImplements = Set.fromList (map (InterfaceName . snd) (eiImpl ei))
            , entityFetchBy = fetchBy
            }
        errs = defErrs <> keyErrs <> fieldErrs <> relErrs <> fetchErrs <> implErrs

    entities :: Map TypeName EntityDef
    entities = Map.fromList (map (\(n, d, _, _) -> (n, d)) entityResults)

    -- ---- interfaces ------------------------------------------------------------
    ifaceResults :: [(InterfaceName, InterfaceDef, [SchemaError])]
    ifaceResults = map buildIface (Map.toList ifaceInfos)

    buildIface (n, (l, fields, relItems)) =
      (InterfaceName n, def, errs)
      where
        ictx = "interface `" <> n <> "`"
        ownField = hasField fields
        fieldErrs =
          Map.foldrWithKey
            ( \(FieldName f) fd acc ->
                let fctx = ictx <> ", field `" <> f <> "`"
                 in tyRefErrs l fctx (fieldType fd)
                      <> argErrs l fctx (fieldArgs fd)
                      <> maybe [] (policyErrs l fctx ownField) (fieldPolicy fd)
                      <> acc
            )
            []
            fields
        (rels, relErrs) = List.foldl' stepRel (Map.empty, []) relItems
        stepRel (m, es) = \case
          SIRelOne rl rn tgtS byF pol ->
            let rctx = ictx <> ", relationship `" <> rn <> "`"
                (tgt, tErrs) = resolveTarget rctx tgtS
                byErrs =
                  if ownField byF
                    then []
                    else [err rl ("`has one` key field `" <> byF <> "` in " <> rctx <> " is not a declared field of `" <> n <> "`")]
             in (Map.insert (FieldName rn) (ToOne {relTarget = tgt, relByField = FieldName byF, relPolicy = pol}) m, es <> tErrs <> byErrs)
          SIRelMany rl rn tgtS link cc pol ->
            let rctx = ictx <> ", relationship `" <> rn <> "`"
                (tgt, tErrs) = resolveTarget rctx tgtS
                cname = CollectionName (fromMaybe (n <> "." <> rn) (ccAs cc))
                (cdef, cErrs) = buildCollectionDef rl rctx tgt cname link (link :| []) cc
                lErrs = linkErrs rl rctx tgt (FieldName link)
             in (Map.insert (FieldName rn) (ToMany {relTarget = tgt, relCollection = cdef, relPolicy = pol}) m, es <> tErrs <> cErrs <> lErrs)
          _ -> (m, es)
        def =
          InterfaceDef
            { ifaceFields = fields
            , ifaceRels = rels
            , ifaceMemberSet = fromMaybe Set.empty (Map.lookup n implIndex)
            }
        errs = fieldErrs <> relErrs

    interfaces :: Map InterfaceName InterfaceDef
    interfaces = Map.fromList (map (\(n, d, _) -> (n, d)) ifaceResults)

    -- ---- fragments -------------------------------------------------------------
    (fragments, fragDupErrs) =
      collectMap "fragment" unFragmentName (map (\(l, n, f) -> (l, FragmentName n, f)) fragDs)
    fragErrs = concatMap checkFrag fragDs
    checkFrag (l, n, f) =
      let fctx = "fragment `" <> n <> "`"
          onErrs =
            if Set.member (fragOn f) entityNames || Set.member (fragOn f) ifaceNames
              then []
              else [err l (fctx <> " is declared on unknown type `" <> fragOn f <> "`")]
          paramErrs = concatMap checkParam (fragParams f)
          checkParam vd =
            let tn = trName (vdType vd)
             in if tn == "Bytes" || tn == "Bit" || primOf tn /= Nothing || Map.member (TypeName tn) types
                  then []
                  else [err l (fctx <> " parameter `$" <> unVarName (vdName vd) <> "` has unknown type `" <> tn <> "`")]
       in onErrs <> paramErrs

    -- ---- roots -----------------------------------------------------------------
    rootResults :: [(Int, RootName, RootDef, [(Int, CollectionName)], [SchemaError])]
    rootResults = map buildRoot rootDs

    buildRoot sr =
      (srLine sr, RootName (srName sr), def, colls, errs)
      where
        l = srLine sr
        rctx = (case srKind sr of RootGet -> "get root `"; RootList -> "list root `") <> srName sr <> "`"
        (tgt, tgtErrs) = resolveTarget rctx (srTarget sr)
        pErrs = argErrs l rctx (srParams sr)
        polErrs =
          policyErrs l rctx (\f -> all (\(_, fm) -> hasField fm f) (memberFields tgt) && not (null (memberFields tgt))) (srPolicy sr)

        (collM, colls, collErrs) = case srKind sr of
          RootGet -> (Nothing, [], [])
          RootList ->
            let cname = CollectionName (fromMaybe (srName sr) (ccAs (srClauses sr)))
                paramNames = map (unArgName . adName) (srParams sr)
             in case srBy sr of
                  Just link ->
                    let (cdef, cErrs) = buildCollectionDef l rctx tgt cname link (link :| []) (srClauses sr)
                        lErrs = linkErrs l rctx tgt (FieldName link)
                     in (Just cdef, [(l, cname)], cErrs <> lErrs)
                  Nothing -> case paramNames of
                    [] ->
                      let (cdef, cErrs) = buildCollectionDef l rctx tgt cname (srName sr) (srName sr :| []) (srClauses sr)
                       in ( Just cdef
                          , [(l, cname)]
                          , err l (rctx <> " needs `by <field>` or at least one parameter to define its collection grouping") : cErrs
                          )
                    p0 : ps ->
                      -- Parameter-backed list root: the parameters are the
                      -- grouping and the first parameter is the link.
                      let (cdef, cErrs) = buildCollectionDef l rctx tgt cname p0 (p0 :| ps) (srClauses sr)
                       in (Just cdef, [(l, cname)], cErrs)

        def =
          RootDef
            { rootKind = srKind sr
            , rootTarget = tgt
            , rootParams = srParams sr
            , rootCollection = collM
            , rootPolicy = srPolicy sr
            }
        errs = tgtErrs <> pErrs <> polErrs <> collErrs

    (roots, rootDupErrs) =
      collectMap "root" unRootName (map (\(l, n, d, _, _) -> (l, n, d)) rootResults)

    -- ---- collection registry ----------------------------------------------------
    allCollections :: [(Int, CollectionName)]
    allCollections =
      concatMap (\(_, _, cs, _) -> cs) entityResults
        <> concatMap (\(_, _, _, cs, _) -> cs) rootResults

    collectionNames :: Set CollectionName
    collectionNames = Set.fromList (map snd allCollections)

    collDupErrs =
      snd (List.foldl' step (Set.empty, []) allCollections)
      where
        step (seen, es) (l, c) =
          if Set.member c seen
            then (seen, es <> [err l ("duplicate collection name `" <> unCollectionName c <> "`")])
            else (Set.insert c seen, es)

    -- ---- mutations ---------------------------------------------------------------
    mutResults :: [(Int, MutationName, MutationDef, [SchemaError])]
    mutResults = map buildMutation mutDs

    buildMutation sm =
      (smLine sm, MutationName (smName sm), def, errs)
      where
        l = smLine sm
        mctx = "mutation `" <> smName sm <> "`"
        paramNames = Set.fromList (map (unArgName . adName) (smParams sm))
        pErrs = argErrs l mctx (smParams sm)

        required :: Maybe a -> a -> Text -> (a, [SchemaError])
        required (Just x) _ _ = (x, [])
        required Nothing d what = (d, [err l (mctx <> " is missing its `" <> what <> "` clause")])

        (guardPol, allowErrs0) = required (smAllow sm) Public "allow"
        (writes, writesErrs0) = required (smWrites sm) [] "writes"
        (inval, invalErrs0) = required (smInvalidates sm) ExactlyWrites "invalidates"
        (effect, effectErrs0) = required (smEffect sm) Transactional "effect"

        guardErrs = policyErrs l (mctx <> " guard") (`Set.member` paramNames) guardPol

        returnsErrs =
          if Set.member (smReturns sm) entityNames || Map.member (TypeName (smReturns sm)) types
            then []
            else [err l (mctx <> " returns unknown type `" <> smReturns sm <> "`")]

        scopeErrs = concatMap (writeScopeErrs l mctx paramNames) writes
        invalScopeErrs = case inval of
          ExactlyWrites -> []
          WritesPlus extra -> concatMap (writeScopeErrs l mctx paramNames) extra

        def =
          MutationDef
            { mutParams = smParams sm
            , mutGuard = guardPol
            , mutReturns = TypeName (smReturns sm)
            , mutWrites = writes
            , mutInvalidates = inval
            , mutEffect = effect
            , mutErrors = smErrors sm
            , mutBatch = smBatch sm
            }
        errs =
          pErrs
            <> allowErrs0
            <> writesErrs0
            <> invalErrs0
            <> effectErrs0
            <> guardErrs
            <> returnsErrs
            <> scopeErrs
            <> invalScopeErrs

    writeScopeErrs :: Int -> Text -> Set Text -> WriteScopeDecl -> [SchemaError]
    writeScopeErrs l mctx paramNames = \case
      WEntity (TypeName t) k ->
        ( if Set.member t entityNames
            then []
            else [err l (mctx <> " writes unknown entity `" <> t <> "`")]
        )
          <> case k of
            KeyNew -> []
            KeyArg (ArgName a) ->
              if Set.member a paramNames
                then []
                else [err l (mctx <> " write scope names unknown argument `" <> a <> "`")]
      WCollection c g ->
        ( if Set.member c collectionNames
            then []
            else [err l (mctx <> " writes unknown collection `" <> unCollectionName c <> "`")]
        )
          <> case g of
            GroupArg (ArgName a) ->
              if Set.member a paramNames
                then []
                else [err l (mctx <> " write scope names unknown argument `" <> a <> "`")]
            GroupOfWritten (TypeName t) (FieldName f) ->
              case Map.lookup t entInfos of
                Nothing -> [err l (mctx <> " write scope reads a field of unknown entity `" <> t <> "`")]
                Just ei ->
                  if hasField (eiFields ei) f
                    then []
                    else [err l (mctx <> " write scope reads unknown field `" <> t <> "." <> f <> "`")]

    (mutations, mutDupErrs) =
      collectMap "mutation" unMutationName (map (\(l, n, d, _) -> (l, n, d)) mutResults)

    -- ---- assembly ------------------------------------------------------------------
    schema =
      Schema
        { schemaName = sName
        , schemaClaims = claims
        , schemaTypes = types
        , schemaInterfaces = interfaces
        , schemaEntities = entities
        , schemaFragments = fragments
        , schemaRoots = roots
        , schemaMutations = mutations
        }

    allErrs =
      nameErrs
        <> claimDupErrs
        <> claimTyErrs
        <> typeDupErrs
        <> typeBodyErrs
        <> entErrs
        <> ifaceDupErrs
        <> crossDupErrs
        <> concatMap (\(_, _, _, es) -> es) entityResults
        <> concatMap (\(_, _, es) -> es) ifaceResults
        <> fragDupErrs
        <> fragErrs
        <> concatMap (\(_, _, _, _, es) -> es) rootResults
        <> rootDupErrs
        <> collDupErrs
        <> concatMap (\(_, _, _, es) -> es) mutResults
        <> mutDupErrs


-- | Insert with duplicate detection, keeping the first occurrence.
collectMap :: Ord k => Text -> (k -> Text) -> [(Int, k, v)] -> (Map k v, [SchemaError])
collectMap what render = List.foldl' step (Map.empty, [])
  where
    step (m, es) (l, k, v)
      | Map.member k m = (m, es <> [err l ("duplicate " <> what <> " `" <> render k <> "`")])
      | otherwise = (Map.insert k v m, es)


buildEntInfos :: [SEntity] -> (Map Text EntInfo, [SchemaError])
buildEntInfos = List.foldl' step (Map.empty, [])
  where
    step (m, es) e
      | Map.member (sEntName e) m =
          (m, es <> [err (sEntLine e) ("duplicate entity `" <> sEntName e <> "`")])
      | otherwise =
          let (info, fieldErrs) = mkInfo e
           in (Map.insert (sEntName e) info m, es <> fieldErrs)

    mkInfo e =
      let (fields, fieldLines, rels, defaults, fetches, dupErrs) =
            List.foldl' item (Map.empty, Map.empty, [], [], [], []) (sEntItems e)
          item (fm, flm, rs, ds, xs, des) = \case
            SIField l n fd
              | Map.member (FieldName n) fm ->
                  (fm, flm, rs, ds, xs, des <> [err l ("duplicate field `" <> n <> "` in entity `" <> sEntName e <> "`")])
              | otherwise ->
                  (Map.insert (FieldName n) fd fm, Map.insert (FieldName n) l flm, rs, ds, xs, des)
            it@(SIRelOne {}) -> (fm, flm, rs <> [it], ds, xs, des)
            it@(SIRelMany {}) -> (fm, flm, rs <> [it], ds, xs, des)
            SIDefault l p -> (fm, flm, rs, ds <> [(l, p)], xs, des)
            SIFetch l k p -> (fm, flm, rs, ds, xs <> [(l, k, p)], des)
       in ( EntInfo
              { eiLine = sEntLine e
              , eiKey = sEntKey e
              , eiImpl = sEntImpl e
              , eiFields = fields
              , eiFieldLines = fieldLines
              , eiRels = rels
              , eiDefaults = defaults
              , eiFetches = fetches
              }
          , dupErrs
          )


buildIfaceInfos
  :: [(Int, Text, [SItem])]
  -> (Map Text (Int, Map FieldName FieldDef, [SItem]), [SchemaError])
buildIfaceInfos = List.foldl' step (Map.empty, [])
  where
    step (m, es) (l, n, items)
      | Map.member n m = (m, es <> [err l ("duplicate interface `" <> n <> "`")])
      | otherwise =
          let (fields, rels, dupErrs) = List.foldl' item (Map.empty, [], []) items
              item (fm, rs, des) = \case
                SIField il fn fd
                  | Map.member (FieldName fn) fm ->
                      (fm, rs, des <> [err il ("duplicate field `" <> fn <> "` in interface `" <> n <> "`")])
                  | otherwise -> (Map.insert (FieldName fn) fd fm, rs, des)
                it@(SIRelOne {}) -> (fm, rs <> [it], des)
                it@(SIRelMany {}) -> (fm, rs <> [it], des)
                _ -> (fm, rs, des)
           in (Map.insert n (l, fields, rels) m, es <> dupErrs)
