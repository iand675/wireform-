{-# LANGUAGE BangPatterns #-}

{- | Parser and elaborator for the Lattice IDL (spec §3.4): surface text to
the semantic model of "Lattice.Schema".

Elaboration also runs the schema-level checks: dangling type references,
unregistered claims in policies (§3.1), collections whose link field is
missing on the target, interface implementors missing declared fields,
write sets naming unknown collections, @invalidates ⊇ writes@ (§11.4), and
co-key declarations (§3.8): unknown or chained bases, key-field
re-declarations, and stray @by@ clauses on @joins@/@refines@ entities.

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

Verb bindings (§11.7\/§11.8): a mutation block admits an
@as VERB \/e\/{Type}[\/{arg}] [last-writer-wins]@ clause (any clause order,
like every other clause; the canonical printer emits it after @effect@),
and the @batch@ clause admits a trailing collection binding
@batch \<atomicity\> max N as VERB \/e\/{Type}@. The two are disambiguated
by lookahead: an @as@ directly after @max N@ binds to the batch clause
only when its URL has __no__ key segment; @as VERB \/e\/T\/{arg}@ there is
left to parse as the mutation's singular @as@ clause.
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
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Scientific (Scientific, scientific)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time.Calendar (Day, fromGregorianValid)
import Lattice.Query.AST (FragmentDefQ (..), QValue (..), TypeRefQ (..), VarDef (..), fName, selectionFields)
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
  (decls, anns) <- singly (runPAnns (pDecls src) toks)
  elaborate anns decls
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
punctChars = "{}()[]:,=|?.$@+/"


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
  , psAnns :: [(Int, DeclPath, Ann)]
  -- ^ @\@break@\/@\@deprecated@ annotations, recorded at their parse sites
  -- (reverse order); elaboration validates and installs them.
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


-- | Run a parser, returning the collected annotations alongside the result.
runPAnns :: P a -> [Tok] -> Either SchemaError (a, [(Int, DeclPath, Ann)])
runPAnns (P g) toks = (\(a, s) -> (a, reverse (psAnns s))) <$> g (PSt toks 1 [])


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
        t : ts -> Right ((), s {psToks = ts, psLine = tokLine t})
    )


popT :: Text -> P Tok
popT expected =
  P
    ( \s -> case psToks s of
        [] ->
          Left
            (SchemaError (Just (psLine s)) ("unexpected end of input; expected " <> expected))
        t : ts -> Right (t, s {psToks = ts, psLine = tokLine t})
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
-- Annotations (§17.3 @break, §17.5 @deprecated)
-- ---------------------------------------------------------------------------

-- | A compatibility annotation, before elaboration attaches it to a site.
data Ann
  = AnnBreak Text
  | AnnDeprecated Deprecation


{- | Zero or more leading annotations: @\@break(approved: "…")@ \/
@\@deprecated(sunset: "YYYY-MM-DD", note: "…")@. Only consumed when the
token after @\@@ is one of the two annotation keywords, so nothing else
starting with @\@@ ever misfires.
-}
pAnnotations :: P [(Int, Ann)]
pAnnotations =
  peekT >>= \case
    Just t
      | TkPunct '@' <- tokKind t ->
          peekKindN 1 >>= \case
            Just (TkName kw)
              | kw == "break" || kw == "deprecated" -> do
                  a <- pAnnotation
                  (a :) <$> pAnnotations
            _ -> pure []
    _ -> pure []


pAnnotation :: P (Int, Ann)
pAnnotation = do
  pPunct '@'
  (kw, l) <- pAnyName "an annotation name"
  case kw of
    "break" -> do
      pPunct '('
      pNameIs "approved"
      pPunct ':'
      ticket <- pStringLit "an approval ticket string"
      pPunct ')'
      pure (l, AnnBreak ticket)
    "deprecated" -> do
      pPunct '('
      pNameIs "sunset"
      pPunct ':'
      ds <- pStringLit "a sunset date string"
      day <- case parseSunset ds of
        Just d -> pure d
        Nothing -> pFail l ("invalid sunset date `" <> ds <> "` (expected YYYY-MM-DD)")
      pPunct ','
      pNameIs "note"
      pPunct ':'
      note <- pStringLit "a deprecation note string"
      pPunct ')'
      pure (l, AnnDeprecated (Deprecation {depSunset = day, depNote = note}))
    _ -> pFail l "expected `break` or `deprecated`"


pStringLit :: Text -> P Text
pStringLit what = do
  t <- popT what
  case tokKind t of
    TkStr s -> pure s
    _ -> pFail (tokLine t) ("expected " <> what)


-- | @YYYY-MM-DD@, calendar-validated.
parseSunset :: Text -> Maybe Day
parseSunset t = case T.splitOn "-" t of
  [y, m, d] -> do
    (yy, mm, dd) <- (,,) <$> decimal y <*> decimal m <*> decimal d
    fromGregorianValid yy (fromInteger mm) (fromInteger dd)
  _ -> Nothing
  where
    decimal :: Text -> Maybe Integer
    decimal s = case TR.decimal s of
      Right (n, rest) | T.null rest -> Just n
      _ -> Nothing


-- | Record parsed annotations against their attachment site.
recordAnns :: DeclPath -> [(Int, Ann)] -> P ()
recordAnns p as =
  P (\s -> Right ((), s {psAnns = foldl (\acc (l, a) -> (l, p, a) : acc) (psAnns s) as}))


-- | The attachment site of a top-level declaration; the claims block has
-- no name to attach to.
declSite :: SDecl -> Maybe DeclPath
declSite = \case
  SDSchema _ _ -> Just OnSchema
  SDClaims _ -> Nothing
  SDType _ n _ -> Just (OnType (TypeName n))
  SDInterface _ n _ -> Just (OnInterface (InterfaceName n))
  SDEntity e -> Just (OnEntity (TypeName (sEntName e)))
  SDFragment _ n _ -> Just (OnFragment (FragmentName n))
  SDRoot r -> Just (OnRoot (RootName (srName r)))
  SDMutation m -> Just (OnMutation (MutationName (smName m)))
  SDExtend x -> Just (OnEntity (TypeName (sExtName x)))


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
  | SDExtend SExtend


data SEntity = SEntity
  { sEntLine :: !Int
  , sEntName :: Text
  , sEntKey :: SEntityKey
  , sEntImpl :: [(Int, Text)]
  , sEntItems :: [SItem]
  }


-- | An @extend entity@ block (§18.1): members declared on a foreign entity.
data SExtend = SExtend
  { sExtLine :: !Int
  , sExtName :: Text
  , sExtCoKey :: Maybe (CoKeyMode, Text)
  -- ^ An illegal @joins@\/@refines@ clause, recorded for fusion diagnostics.
  , sExtItems :: [SItem]
  }


-- | The key clause of an entity declaration: @by <keyspec>@, or a
-- co-key declaration @joins <Base>@ \/ @refines <Base>@ (§3.8).
data SEntityKey
  = SKeyBy (NonEmpty Text)
  | -- | Mode, base name, and whether the declaration illegally also
    -- carries a @by@ clause (rejected at elaboration).
    SKeyCo CoKeyMode Text !Bool


data SItem
  = SIDefault !Int Policy
  | SIField !Int Text FieldDef
  | SIRelOne !Int Text (Int, NonEmpty Text) Text Bool (Maybe Policy)
  | SIRelMany !Int Text (Int, NonEmpty Text) Text CollClauses (Maybe Policy)
  | SIFetch !Int (NonEmpty Text) Policy


data CollClauses = CollClauses
  { ccMin :: Maybe Natural
  , ccMax :: Maybe Natural
  , ccTrunc :: Bool
  , ccOrder :: [(Text, Direction)]
  , ccPage :: Maybe Natural
  , ccGrouped :: Maybe (NonEmpty Text)
  , ccAs :: Maybe Text
  }


emptyClauses :: CollClauses
emptyClauses = CollClauses Nothing Nothing False [] Nothing Nothing Nothing


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
  , smBinding :: Maybe (Int, VerbBinding)
  -- ^ The @as VERB \/e\/…@ clause and its line (§11.7).
  }


-- ---------------------------------------------------------------------------
-- Declaration parsing
-- ---------------------------------------------------------------------------

pDecls :: Text -> P [SDecl]
pDecls src = go
  where
    go = do
      anns <- pAnnotations
      peekT >>= \case
        Nothing -> case anns of
          [] -> pure []
          (l, _) : _ -> pFail l "dangling annotation: no declaration follows"
        Just t -> do
          d <- pDecl src t
          case (anns, declSite d) of
            ([], _) -> pure ()
            (_, Just site) -> recordAnns site anns
            ((l, _) : _, Nothing) -> pFail l "annotations are not allowed on the claims block"
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
    TkName "extend" -> pExtendDecl l
    _ -> pFail l "expected a declaration (schema, claims, newtype, enum, data, interface, entity, extend, fragment, get, list, or mutation)"


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
      plus <- tryPunct '+'
      pure (if plus then TList1 inner else TList inner)
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
  key <- pEntityKey
  impls <- pImplements
  pPunct '{'
  items <- pItems BodyEntity n
  pure (SDEntity (SEntity l n key impls items))
  where
    -- @by <keyspec>@, @joins <Base>@, or @refines <Base>@ — a stray @by@
    -- next to a co-key clause (either order) is recorded and rejected at
    -- elaboration, naming the entity.
    pEntityKey = do
      isBy <- tryNameIs "by"
      if isBy
        then do
          key <- pKeySpec
          co <- tryCoKey
          case co of
            Just (mode, base) -> pure (SKeyCo mode base True)
            Nothing -> pure (SKeyBy key)
        else do
          co <- tryCoKey
          case co of
            Nothing -> pFail l "expected `by`, `joins`, or `refines` after the entity name"
            Just (mode, base) -> do
              hasBy <- tryNameIs "by"
              if hasBy
                then pKeySpec $> SKeyCo mode base True
                else pure (SKeyCo mode base False)
    tryCoKey = do
      j <- tryNameIs "joins"
      if j
        then Just . (JoinsBase,) . fst <$> pAnyName "a base entity name"
        else do
          r <- tryNameIs "refines"
          if r
            then Just . (RefinesBase,) . fst <$> pAnyName "a base entity name"
            else pure Nothing
    pImplements = do
      has <- tryNameIs "implements"
      if has then go else pure []
      where
        go = do
          (i, il) <- pAnyName "an interface name"
          c <- tryPunct ','
          if c then ((il, i) :) <$> go else pure [(il, i)]


{- | @extend entity Foo { … }@ (§18.1): members this module declares on a
FOREIGN entity. The body grammar is the full entity-item grammar and the
header admits a @joins@\/@refines@ clause, deliberately: the spec pins
composition conflicts (an extension redeclaring the visibility default,
@fetch by@, or declaring a co-key) to fail FUSION, not module parse, so
the illegal clauses parse, are recorded in the model, and
'Lattice.Module.fuseModules' rejects them naming the offender. A @by@ key
clause has no such home — an extension never has a key of its own — and
key redeclaration is caught at fusion as a member collision with the
owner's key fields.
-}
pExtendDecl :: Int -> P SDecl
pExtendDecl l = do
  pNameIs "entity"
  (n, _) <- pAnyName "an entity name"
  co <- tryCoKeyClause
  pPunct '{'
  items <- pItems BodyEntity n
  pure (SDExtend (SExtend l n co items))
  where
    tryCoKeyClause = do
      j <- tryNameIs "joins"
      if j
        then Just . (JoinsBase,) . fst <$> pAnyName "a base entity name"
        else do
          r <- tryNameIs "refines"
          if r
            then Just . (RefinesBase,) . fst <$> pAnyName "a base entity name"
            else pure Nothing


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
      anns <- pAnnotations
      done <- tryPunct '}'
      if done
        then case anns of
          [] -> pure []
          (l, _) : _ -> pFail l "dangling annotation: no field or relationship follows"
        else do
          it <- pItem
          case (anns, itemSite it) of
            ([], _) -> pure ()
            (_, Just p) -> recordAnns p anns
            ((l, _) : _, Nothing) ->
              pFail l "annotations are only allowed on fields and relationships"
          (it :) <$> go

    itemSite = \case
      SIField _ n _ -> Just (site n)
      SIRelOne _ n _ _ _ _ -> Just (site n)
      SIRelMany _ n _ _ _ _ -> Just (site n)
      SIDefault _ _ -> Nothing
      SIFetch _ _ _ -> Nothing

    site n = case bk of
      BodyEntity -> OnEntityItem (TypeName owner) (FieldName n)
      BodyInterface -> OnIfaceItem (InterfaceName owner) (FieldName n)

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
      deriv <- tryDerivation
      pol <- tryPolicy
      pure
        ( SIField
            l
            n
            (FieldDef {fieldType = ty, fieldArgs = args, fieldPolicy = pol, fieldDerivation = deriv})
        )

    pRelItem l = do
      t <- popT "`one` or `many`"
      which <- case tokKind t of
        TkName "one" -> Just <$> tryPunct '?'
        TkName "many" -> pure Nothing
        _ -> pFail (tokLine t) "expected `one` or `many` after `has`"
      (n, _) <- pAnyName "a relationship name"
      pPunct ':'
      tgt <- pTargetSurface
      pNameIs "by"
      (link, _) <- pAnyName "a link field"
      case which of
        Just opt -> do
          pol <- tryPolicy
          pure (SIRelOne l n tgt link opt pol)
        Nothing -> do
          cls <- pCollClauses owner
          pol <- tryPolicy
          pure (SIRelMany l n tgt link cls pol)



{- | The derived-field clause (§3.7), canonical order:
@derived reads \<dep\>{, \<dep\>} [\@declassify(approved: \"…\")]
(on read | maintained)@. Only consumed when the next token is the
@derived@ keyword, so a policy or the next item never misfires.
-}
tryDerivation :: P (Maybe Derivation)
tryDerivation = do
  isDerived <- tryNameIs "derived"
  if not isDerived
    then pure Nothing
    else do
      pNameIs "reads"
      deps <- pDeps
      decl <- tryDeclassify
      mat <- pMaterialization
      pure (Just Derivation {derivReads = deps, derivMaterialize = mat, derivDeclassify = decl})


-- | One or more read-set deps, comma-separated.
pDeps :: P (NonEmpty Dep)
pDeps = do
  d <- pDep
  more <- tryPunct ','
  if more then (d NE.<|) <$> pDeps else pure (d :| [])


{- | One dep: @own(f1, f2)@, @\<edge\> ...Fragment@, or
@\<rel\> count\/sum(f)\/min(f)\/max(f)@. @own@ is only the keyword when a
parenthesized field list follows.
-}
pDep :: P Dep
pDep = do
  (n, _) <- pAnyName "a derived-field dep"
  isOwn <-
    if n == "own"
      then
        peekKindN 0 >>= \case
          Just (TkPunct '(') -> pure True
          _ -> pure False
      else pure False
  if isOwn
    then OwnFields <$> pOwnFields
    else
      peekKindN 0 >>= \case
        Just (TkPunct '.') -> do
          pPunct '.'
          pPunct '.'
          pPunct '.'
          (frag, _) <- pAnyName "a fragment name"
          pure (ViaEdge (FieldName n) (FragmentName frag))
        _ -> ViaCollection (FieldName n) <$> pAggregate


-- | @(f1, f2, …)@ after @own@.
pOwnFields :: P (NonEmpty FieldName)
pOwnFields = do
  pPunct '('
  (f0, _) <- pAnyName "an own dep field"
  rest <- go
  pure (FieldName f0 :| map FieldName rest)
  where
    go = do
      c <- tryPunct ','
      if c
        then do
          (f, _) <- pAnyName "an own dep field"
          (f :) <$> go
        else pPunct ')' $> []


pAggregate :: P Aggregate
pAggregate = do
  t <- popT "an aggregate (`count`, `sum(f)`, `min(f)`, `max(f)`)"
  case tokKind t of
    TkName "count" -> pure AggCount
    TkName "sum" -> AggSum <$> pAggField
    TkName "min" -> AggMin <$> pAggField
    TkName "max" -> AggMax <$> pAggField
    _ -> pFail (tokLine t) "expected `count`, `sum(field)`, `min(field)`, or `max(field)`"
  where
    pAggField = do
      pPunct '('
      (f, _) <- pAnyName "an aggregate field"
      pPunct ')'
      pure (FieldName f)


-- | @\@declassify(approved: \"…\")@ before the materialization keyword.
tryDeclassify :: P (Maybe Text)
tryDeclassify =
  peekT >>= \case
    Just t | TkPunct '@' <- tokKind t -> do
      advanceT
      pNameIs "declassify"
      pPunct '('
      pNameIs "approved"
      pPunct ':'
      s <- popT "the approval justification string"
      j <- case tokKind s of
        TkStr str -> pure str
        _ -> pFail (tokLine s) "expected a string after `approved:`"
      pPunct ')'
      pure (Just j)
    _ -> pure Nothing


pMaterialization :: P Materialization
pMaterialization = do
  t <- popT "`on read` or `maintained`"
  case tokKind t of
    TkName "on" -> pNameIs "read" $> OnRead
    TkName "maintained" -> pure Maintained
    _ -> pFail (tokLine t) "expected `on read` or `maintained`"

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
        Just (TkName "min")
          | Just (TkInt _) <- k1 -> do
              dup (ccMin cc) "min"
              advanceT
              n <- pNat "a minimum"
              nk0 <- peekKindN 0
              nk1 <- peekKindN 1
              case (nk0, nk1) of
                (Just (TkName "max"), Just (TkInt _)) -> go cc {ccMin = Just n}
                _ -> failHere "`min` must be immediately followed by `max`"
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
      , smBinding = Nothing
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
              bnd <- pBatchBinding
              loop sm {smBatch = Just (BatchPolicy {bpAtomicity = at, bpMaxItems = mx, bpBound = bnd})}
            TkName "as" -> do
              dup cl (smBinding sm) "as"
              b <- pVerbBinding
              loop sm {smBinding = Just (cl, b)}
            _ ->
              pFail cl "expected a mutation clause (allow, writes, invalidates, effect, errors, batch, as) or `}`"

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

    -- The verb of an @as@ binding: an HTTP method token.
    pBindVerb = do
      t <- popT "an HTTP verb (PUT, PATCH, DELETE, or POST)"
      case tokKind t of
        TkName "PUT" -> pure BindPut
        TkName "PATCH" -> pure BindPatch
        TkName "DELETE" -> pure BindDelete
        TkName "POST" -> pure BindCreate
        _ -> pFail (tokLine t) "expected `PUT`, `PATCH`, `DELETE`, or `POST` after `as`"

    -- @VERB /e/{Type}[/{arg}] [last-writer-wins]@ (§11.7).
    pVerbBinding = do
      v <- pBindVerb
      pPunct '/'
      pNameIs "e"
      pPunct '/'
      (ty, _) <- pAnyName "a bound entity type"
      keyed <- tryPunct '/'
      arg <-
        if keyed
          then do
            pPunct '{'
            (a, _) <- pAnyName "a key argument"
            pPunct '}'
            pure (Just (ArgName a))
          else pure Nothing
      lww <- tryNameIs "last-writer-wins"
      pure VerbBinding {vbVerb = v, vbTarget = TypeName ty, vbKeyArg = arg, vbLww = lww}

    -- The optional collection binding of a batch clause: @as VERB /e/{Type}@
    -- with NO key segment. An @as@ here is only consumed when the next seven
    -- tokens shape a collection URL; a keyed URL is left for the singular
    -- @as@ clause of the mutation block (clauses parse in any order).
    pBatchBinding = do
      k0 <- peekKindN 0
      k1 <- peekKindN 1
      k2 <- peekKindN 2
      k3 <- peekKindN 3
      k4 <- peekKindN 4
      k5 <- peekKindN 5
      k6 <- peekKindN 6
      let verbOf = \case
            Just (TkName "PUT") -> Just BindPut
            Just (TkName "PATCH") -> Just BindPatch
            Just (TkName "DELETE") -> Just BindDelete
            Just (TkName "POST") -> Just BindCreate
            _ -> Nothing
          tyOf = \case
            Just (TkName t) -> Just t
            _ -> Nothing
          collection =
            k0 == Just (TkName "as")
              && verbOf k1 /= Nothing
              && k2 == Just (TkPunct '/')
              && k3 == Just (TkName "e")
              && k4 == Just (TkPunct '/')
              && tyOf k5 /= Nothing
              && k6 /= Just (TkPunct '/')
      if not collection
        then pure Nothing
        else case (verbOf k1, tyOf k5) of
          (Just v, Just ty) -> do
            advanceT
            advanceT
            advanceT
            advanceT
            advanceT
            advanceT
            pure (Just (v, TypeName ty))
          _ -> pure Nothing


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
  , eiKeySpec :: SEntityKey
  -- ^ Surface key clause; co-keys are resolved by 'resolveCoKey'.
  , eiKey :: NonEmpty Text
  -- ^ Effective key: own @by@ spec, or the base's once resolved
  -- (placeholder until 'resolveCoKey' runs on a co-keyed entity).
  , eiCoKey :: Maybe CoKey
  -- ^ Set by 'resolveCoKey' for @joins@/@refines@ entities.
  , eiImpl :: [(Int, Text)]
  , eiFields :: Map FieldName FieldDef
  -- ^ Declared fields, plus the base's key 'FieldDef's once resolved.
  , eiFieldLines :: Map FieldName Int
  , eiRels :: [SItem]
  , eiDefaults :: [(Int, Policy)]
  , eiFetches :: [(Int, NonEmpty Text, Policy)]
  }


elaborate :: [(Int, DeclPath, Ann)] -> [SDecl] -> Either [SchemaError] Schema
elaborate anns decls =
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
    extendDs = mapMaybe (\case SDExtend x -> Just x; _ -> Nothing) decls

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

    (entInfos0, entErrs) = buildEntInfos entityDs
    (entInfos, coKeyErrs) =
      let resolved = Map.mapWithKey (resolveCoKey entInfos0) entInfos0
       in (Map.map fst resolved, concatMap snd (Map.elems resolved))
    (ifaceInfos, ifaceDupErrs) = buildIfaceInfos ifaceDs
    (extInfos, extDupErrs) = buildExtInfos extendDs

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
          TList t -> elemErrs t <> go t
          TList1 t -> elemErrs t <> go t
          TSet t -> go t
          TMap k v -> go k <> go v
          TVec _ t -> go t

        -- §3.5.2: element optionality is rejected — a list contains values.
        elemErrs = \case
          TOptional _ ->
            [err l ("optional element type in " <> ctx <> ": a list contains values — an element's absence is its absence from the list")]
          _ -> []

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

    -- Concrete member entities of a target, with their raw 'EntInfo's
    -- (derived-field checks read fields, lines, and default policies).
    memberEntInfos :: Target -> [(Text, EntInfo)]
    memberEntInfos tgt = mapMaybe (\n -> (,) n <$> Map.lookup n entInfos) names
      where
        names = case tgt of
          TargetEntity (TypeName n) -> [n]
          TargetUnion ns -> map unTypeName (NE.toList ns)
          TargetInterface (InterfaceName i) ->
            maybe [] (map unTypeName . Set.toList) (Map.lookup i implIndex)

    -- The effective default policy of a raw entity (first declaration wins,
    -- matching 'buildEntity').
    memberDefaultPol :: EntInfo -> Policy
    memberDefaultPol mi = case eiDefaults mi of
      (_, p) : _ -> p
      [] -> Public

    -- Is a field type numeric for sum/min/max aggregation (§3.7)? Resolves
    -- newtypes through 'types' (depth-bounded) and looks through optionals.
    numericType :: FieldType -> Bool
    numericType = goN (8 :: Int)
      where
        goN :: Int -> FieldType -> Bool
        goN fuel = \case
          TPrim p -> numericPrim p
          TOptional t -> goN fuel t
          TNamed n
            | fuel > 0
            , Just (DeclNewtype t _) <- Map.lookup n types ->
                goN (fuel - 1) t
          _ -> False
        numericPrim = \case
          PI8 -> True
          PI16 -> True
          PI32 -> True
          PI64 -> True
          PW8 -> True
          PW16 -> True
          PW32 -> True
          PW64 -> True
          PInteger -> True
          PDecimal -> True
          PF32 -> True
          PF64 -> True
          _ -> False

    -- The field a sum/min/max aggregate reads; 'Nothing' for @count@.
    aggregateField :: Aggregate -> Maybe FieldName
    aggregateField = \case
      AggCount -> Nothing
      AggSum f -> Just f
      AggMin f -> Just f
      AggMax f -> Just f

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
        ( Bounded minB maxB (if ccTrunc cc then Truncate else Overflow)
        , floorErrs
        )
      ([], Just _) ->
        ( Bounded minB maxB Overflow
        , err l ("`page` in " <> ctx <> " requires an `ordered by` keyset") : floorErrs
        )
      (o : os, _) ->
        ( Paginated
            CursorSpec
              { csKeyset = fmap (\(f, d) -> (FieldName f, d)) (o :| os)
              , csDefaultPage = ccPage cc
              , csMaxPage = maxB
              , csTotal = CountNone
              }
        , concat
            [ if ccTrunc cc
                then [err l ("`truncate` in " <> ctx <> " applies only to bounded collections")]
                else []
            , case ccMin cc of
                Just _ ->
                  [err l ("`min` in " <> ctx <> " applies only to bounded collections: an empty page is indistinguishable from end-of-pagination")]
                Nothing -> []
            ]
        )
      where
        minB = fromMaybe 0 (ccMin cc)
        maxB = fromMaybe defaultMax (ccMax cc)
        floorErrs =
          case ccMin cc of
            Just m
              | m > maxB ->
                  [err l ("`min " <> T.pack (show m) <> "` exceeds `max " <> T.pack (show maxB) <> "` in " <> ctx)]
            _ -> []

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
            Bounded {} -> []
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

        -- Inherited key fields of a co-keyed entity (§3.8): present by
        -- construction ('resolveCoKey'), and already checked in the base's
        -- own context — skipped below to avoid duplicate diagnostics.
        inheritedKeys = case eiCoKey ei of
          Nothing -> Set.empty
          Just _ -> Set.fromList (map FieldName (NE.toList (eiKey ei)))

        keyErrs =
          concatMap
            ( \k ->
                if ownField k || Set.member (FieldName k) inheritedKeys
                  then []
                  else [err (eiLine ei) (ectx <> " key field `" <> k <> "` is not declared")]
            )
            (NE.toList (eiKey ei))

        fieldErrs =
          Map.foldrWithKey
            ( \(FieldName f) fd acc ->
                let l = fromMaybe (eiLine ei) (Map.lookup (FieldName f) (eiFieldLines ei))
                    fctx = ectx <> ", field `" <> f <> "`"
                 in if Set.member (FieldName f) inheritedKeys
                      then acc
                      else
                        tyRefErrs l fctx (fieldType fd)
                          <> argErrs l fctx (fieldArgs fd)
                          <> maybe [] (policyErrs l fctx ownField) (fieldPolicy fd)
                          <> acc
            )
            []
            (eiFields ei)

        (rels, colls, relErrs) = List.foldl' stepRel (Map.empty, [], []) (eiRels ei)

        stepRel (m, cs, es) = \case
          SIRelOne l n tgtS byF opt pol ->
            let (tgt, tErrs) = resolveTarget (ectx <> ", relationship `" <> n <> "`") tgtS
                rctx = ectx <> ", relationship `" <> n <> "`"
                byErrs =
                  if ownField byF
                    then []
                    else [err l ("`has one` key field `" <> byF <> "` in " <> rctx <> " is not a declared field of `" <> en <> "`")]
                -- §3.4: an optional column cannot promise a required edge.
                optErrs = case Map.lookup (FieldName byF) (eiFields ei) of
                  Just fd
                    | not opt
                    , TOptional _ <- fieldType fd ->
                        [err l ("required `has one` in " <> rctx <> " reads the optional link column `" <> byF <> "`; an optional column cannot promise a required edge — declare `has one?` or make the column required")]
                  _ -> []
                polErrs = maybe [] (policyErrs l rctx ownField) pol
                (m', dupE) = insertRel l n (ToOne {relTarget = tgt, relByField = FieldName byF, relOptional = opt, relPolicy = pol}) m
             in (m', cs, es <> tErrs <> byErrs <> optErrs <> polErrs <> dupE)
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
                      SIRelOne _ n _ _ _ _ -> checkRelName n
                      SIRelMany _ n _ _ _ _ -> checkRelName n
                      _ -> []
                  )
                  ifRels
              checkRelName n =
                if any (relNamed n) (eiRels ei)
                  then []
                  else [err l (ectx <> " implements `" <> i <> "` but does not declare its relationship `" <> n <> "`")]
              relNamed n = \case
                SIRelOne _ n' _ _ _ _ -> n == n'
                SIRelMany _ n' _ _ _ _ -> n == n'
                _ -> False

        -- ---- derived fields (§3.7): shared checker ------------------------
        derivedErrs =
          derivedCheckErrs
            ectx
            (eiLine ei)
            (eiFieldLines ei)
            (NE.toList (eiKey ei))
            defPol
            (eiFields ei)
            rels

        def =
          EntityDef
            { entityKey = fmap FieldName (eiKey ei)
            , entityDefaultPolicy = defPol
            , entityFields = eiFields ei
            , entityRels = rels
            , entityImplements = Set.fromList (map (InterfaceName . snd) (eiImpl ei))
            , entityFetchBy = fetchBy
            , entityCoKey = eiCoKey ei
            }
        errs = defErrs <> keyErrs <> fieldErrs <> relErrs <> fetchErrs <> implErrs <> derivedErrs

    entities :: Map TypeName EntityDef
    entities = Map.fromList (map (\(n, d, _, _) -> (n, d)) entityResults)

    {- Derived-field checks (§3.7), shared between entity bodies and
    @extend entity@ blocks: the read set resolves against the declaring
    body's OWN fields and relationships. @keyFields@ is empty for
    extensions (they have no key of their own); @defPol@ is the effective
    default policy — extensions pass 'Public', the local approximation of
    the owner's (foreign, invisible) default; the fused elaboration
    re-checks flow against the real one. -}
    derivedCheckErrs
      :: Text
      -> Int
      -> Map FieldName Int
      -> [Text]
      -> Policy
      -> Map FieldName FieldDef
      -> Map FieldName RelationshipDef
      -> [SchemaError]
    derivedCheckErrs ectx declLine fieldLines keyFields defPol ownFields rels =
      Map.foldrWithKey
        ( \fn fd acc -> case fieldDerivation fd of
            Nothing -> acc
            Just d -> derivationErrs fn fd d <> acc
        )
        []
        ownFields
      where
        derivationErrs (FieldName f) fd d =
          let l = fromMaybe declLine (Map.lookup (FieldName f) fieldLines)
              dctx = ectx <> ", derived field `" <> f <> "`"
              keyErr =
                if f `elem` keyFields
                  then [err l (dctx <> " is a key field; key fields cannot be derived")]
                  else []
              argErr =
                if null (fieldArgs fd)
                  then []
                  else [err l (dctx <> " must not declare arguments")]
              derivedLvl = policyLevel (fromMaybe defPol (fieldPolicy fd))
              results = map (checkDep l dctx (derivMaterialize d)) (NE.toList (derivReads d))
              depErrs = concatMap snd results
              -- §3.7 information flow: the field's policy must dominate the
              -- join of its deps' policies along dep paths, unless the
              -- declaration declassifies.
              flowErr = case derivDeclassify d of
                Just _ -> []
                Nothing ->
                  let depsLvl = List.foldl' joinLevel LPublic (concatMap fst results)
                   in if joinLevel derivedLvl depsLvl == derivedLvl
                        then []
                        else
                          [ err
                              l
                              ( dctx
                                  <> ": declared policy does not dominate the join of its deps' policies (§3.7 information flow); add `@declassify(approved: \"…\")` to declassify deliberately"
                              )
                          ]
           in keyErr <> argErr <> depErrs <> flowErr

        -- One dep: its contribution to the dep-path policy join (empty when
        -- the dep is broken) and its check failures.
        checkDep l dctx mat = \case
          OwnFields fs ->
            let one (FieldName g) = case Map.lookup (FieldName g) ownFields of
                  Nothing -> ([], [err l (dctx <> ": unknown own dep field `" <> g <> "`")])
                  Just gfd
                    | isJust (fieldDerivation gfd) ->
                        ([], [err l (dctx <> ": own dep `" <> g <> "` is itself derived; derivation chaining is not supported")])
                    | not (null (fieldArgs gfd)) ->
                        ([], [err l (dctx <> ": own dep `" <> g <> "` is a computed field, not a stored field")])
                    | otherwise -> ([policyLevel (fromMaybe defPol (fieldPolicy gfd))], [])
                rs = map one (NE.toList fs)
             in (concatMap fst rs, concatMap snd rs)
          ViaEdge (FieldName e) fragN -> case Map.lookup (FieldName e) rels of
            Nothing -> ([], [err l (dctx <> ": unknown edge `" <> e <> "`")])
            Just ToMany {} ->
              ([], [err l (dctx <> ": edge dep `" <> e <> "` is not a to-one relationship")])
            Just rel ->
              let matErr = case mat of
                    Maintained ->
                      [err l (dctx <> ": maintained derivations cannot read through edges (no reverse index in v1); use `on read`")]
                    OnRead -> []
                  relLvl = policyLevel (fromMaybe Public (relPolicy rel))
               in case Map.lookup fragN fragments of
                    Nothing ->
                      ([relLvl], matErr <> [err l (dctx <> ": unknown fragment `" <> unFragmentName fragN <> "`")])
                    Just fdef ->
                      let okLabels = case relTarget rel of
                            TargetEntity (TypeName tn) -> [tn]
                            TargetInterface (InterfaceName i) -> [i]
                            TargetUnion ns -> map unTypeName (NE.toList ns)
                          onErr =
                            if fragOn fdef `elem` okLabels
                              then []
                              else
                                [ err
                                    l
                                    ( dctx
                                        <> ": fragment `"
                                        <> unFragmentName fragN
                                        <> "` is declared on `"
                                        <> fragOn fdef
                                        <> "`, not on the edge's target"
                                    )
                                ]
                          fragFields = map fName (selectionFields (fragSelection fdef))
                          onMember (mn, mi) =
                            let oneF g = case Map.lookup g (eiFields mi) of
                                  Nothing -> ([], [])
                                  Just gfd
                                    | isJust (fieldDerivation gfd) ->
                                        ( []
                                        , [ err
                                              l
                                              ( dctx
                                                  <> ": fragment field `"
                                                  <> unFieldName g
                                                  <> "` on `"
                                                  <> mn
                                                  <> "` is itself derived; derivation chaining is not supported"
                                              )
                                          ]
                                        )
                                    | otherwise ->
                                        ([policyLevel (fromMaybe (memberDefaultPol mi) (fieldPolicy gfd))], [])
                                frs = map oneF fragFields
                             in (concatMap fst frs, concatMap snd frs)
                          mrs = map onMember (memberEntInfos (relTarget rel))
                       in (relLvl : concatMap fst mrs, matErr <> onErr <> concatMap snd mrs)
          ViaCollection (FieldName r) agg -> case Map.lookup (FieldName r) rels of
            Nothing -> ([], [err l (dctx <> ": unknown collection `" <> r <> "`")])
            Just ToOne {} ->
              ([], [err l (dctx <> ": collection dep `" <> r <> "` is not a `has many` relationship")])
            Just rel ->
              let col = relCollection rel
                  relLvl = policyLevel (fromMaybe Public (relPolicy rel))
                  matErr = case mat of
                    Maintained
                      | colLink col `notElem` NE.toList (colGrouping col) ->
                          [ err
                              l
                              ( dctx
                                  <> ": maintained collection dep `"
                                  <> r
                                  <> "` must group by its link field (the owner key is recovered from the collection tag)"
                              )
                          ]
                    _ -> []
                  (aggLvls, aggErrs) = case aggregateField agg of
                    Nothing -> ([], [])
                    Just (FieldName g) ->
                      let one (mn, mi) = case Map.lookup (FieldName g) (eiFields mi) of
                            Nothing ->
                              ([], [err l (dctx <> ": aggregate field `" <> g <> "` is not a field of `" <> mn <> "`")])
                            Just gfd
                              | isJust (fieldDerivation gfd) ->
                                  ( []
                                  , [ err
                                        l
                                        ( dctx
                                            <> ": aggregate field `"
                                            <> g
                                            <> "` on `"
                                            <> mn
                                            <> "` is itself derived; derivation chaining is not supported"
                                        )
                                    ]
                                  )
                              | not (numericType (fieldType gfd)) ->
                                  ([], [err l (dctx <> ": aggregate field `" <> g <> "` on `" <> mn <> "` is not numeric")])
                              | otherwise ->
                                  ([policyLevel (fromMaybe (memberDefaultPol mi) (fieldPolicy gfd))], [])
                          rs = map one (memberEntInfos (relTarget rel))
                       in (concatMap fst rs, concatMap snd rs)
               in (relLvl : aggLvls, matErr <> aggErrs)

    -- ---- extensions (§18.1) -----------------------------------------------
    extensionResults :: [(TypeName, ExtensionDef, [(Int, CollectionName)], [SchemaError])]
    extensionResults = map buildExtension (Map.toList extInfos)

    buildExtension :: (Text, ExtInfo) -> (TypeName, ExtensionDef, [(Int, CollectionName)], [SchemaError])
    buildExtension (en, xi) = (TypeName en, def, colls, errs)
      where
        ectx = "extension of `" <> en <> "`"
        ownField = hasField (xiFields xi)

        -- The extended name must be FOREIGN: fusion resolves it. Extending
        -- a name this module itself declares is a local error — the
        -- members belong on the declaration.
        targetErrs
          | Set.member en entityNames =
              [err (xiLine xi) (ectx <> ": `" <> en <> "` is declared in this schema — declare the members on the entity instead of extending it")]
          | Set.member en typeNames =
              [err (xiLine xi) (ectx <> ": `" <> en <> "` is a value type, not an entity")]
          | Set.member en ifaceNames =
              [err (xiLine xi) (ectx <> ": `" <> en <> "` is an interface, not an entity")]
          | otherwise = []

        (defPolM, defErrs) = case xiDefaults xi of
          [] -> (Nothing, [])
          (l0, p) : rest ->
            ( Just p
            , map (\(l, _) -> err l ("duplicate default visibility in " <> ectx)) rest
                <> policyErrs l0 (ectx <> " default visibility") ownField p
            )

        fieldErrs =
          Map.foldrWithKey
            ( \(FieldName f) fd acc ->
                let l = fromMaybe (xiLine xi) (Map.lookup (FieldName f) (xiFieldLines xi))
                    fctx = ectx <> ", field `" <> f <> "`"
                 in tyRefErrs l fctx (fieldType fd)
                      <> argErrs l fctx (fieldArgs fd)
                      <> maybe [] (policyErrs l fctx ownField) (fieldPolicy fd)
                      <> acc
            )
            []
            (xiFields xi)

        (rels, colls, relErrs) = List.foldl' stepRel (Map.empty, [], []) (xiRels xi)

        stepRel (m, cs, es) = \case
          SIRelOne l n tgtS byF opt pol ->
            let rctx = ectx <> ", relationship `" <> n <> "`"
                (tgt, tErrs) = resolveTarget rctx tgtS
                byErrs =
                  if ownField byF
                    then []
                    else [err l ("`has one` key field `" <> byF <> "` in " <> rctx <> " is not a field this extension declares")]
                -- §3.4: an optional column cannot promise a required edge.
                optErrs = case Map.lookup (FieldName byF) (xiFields xi) of
                  Just fd
                    | not opt
                    , TOptional _ <- fieldType fd ->
                        [err l ("required `has one` in " <> rctx <> " reads the optional link column `" <> byF <> "`; an optional column cannot promise a required edge — declare `has one?` or make the column required")]
                  _ -> []
                polErrs = maybe [] (policyErrs l rctx ownField) pol
                (m', dupE) = insertRel l n (ToOne {relTarget = tgt, relByField = FieldName byF, relOptional = opt, relPolicy = pol}) m
             in (m', cs, es <> tErrs <> byErrs <> optErrs <> polErrs <> dupE)
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
          | Map.member (FieldName n) (xiFields xi) =
              (m, [err l ("relationship `" <> n <> "` in " <> ectx <> " duplicates a field name")])
          | Map.member (FieldName n) m =
              (m, [err l ("duplicate relationship `" <> n <> "` in " <> ectx)])
          | otherwise = (Map.insert (FieldName n) rel m, [])

        (fetchM, fetchErrs) = case xiFetches xi of
          [] -> (Nothing, [])
          (l, keys, p) : rest ->
            ( Just (fmap FieldName keys, p)
            , policyErrs l (ectx <> " fetch policy") ownField p
                <> map (\(l', _, _) -> err l' ("duplicate `fetch by` in " <> ectx)) rest
            )

        derivedErrs =
          derivedCheckErrs ectx (xiLine xi) (xiFieldLines xi) [] (fromMaybe Public defPolM) (xiFields xi) rels

        def =
          ExtensionDef
            { extFields = xiFields xi
            , extRels = rels
            , extDefaultPolicy = defPolM
            , extFetchBy = fetchM
            , extCoKey = (\(mo, b) -> CoKey (TypeName b) mo) <$> xiCoKey xi
            }
        errs = targetErrs <> defErrs <> fieldErrs <> relErrs <> fetchErrs <> derivedErrs

    extensions :: Map TypeName ExtensionDef
    extensions = Map.fromList (map (\(n, d, _, _) -> (n, d)) extensionResults)

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
                      <> ( if isJust (fieldDerivation fd)
                            then [err l (fctx <> ": interface fields cannot be derived (§3.7); derive on the implementing entities")]
                            else []
                         )
                      <> acc
            )
            []
            fields
        (rels, relErrs) = List.foldl' stepRel (Map.empty, []) relItems
        stepRel (m, es) = \case
          SIRelOne rl rn tgtS byF opt pol ->
            let rctx = ictx <> ", relationship `" <> rn <> "`"
                (tgt, tErrs) = resolveTarget rctx tgtS
                byErrs =
                  if ownField byF
                    then []
                    else [err rl ("`has one` key field `" <> byF <> "` in " <> rctx <> " is not a declared field of `" <> n <> "`")]
                -- §3.4: an optional column cannot promise a required edge.
                optErrs = case Map.lookup (FieldName byF) fields of
                  Just fd
                    | not opt
                    , TOptional _ <- fieldType fd ->
                        [err rl ("required `has one` in " <> rctx <> " reads the optional link column `" <> byF <> "`; an optional column cannot promise a required edge — declare `has one?` or make the column required")]
                  _ -> []
             in (Map.insert (FieldName rn) (ToOne {relTarget = tgt, relByField = FieldName byF, relOptional = opt, relPolicy = pol}) m, es <> tErrs <> byErrs <> optErrs)
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
        <> concatMap (\(_, _, cs, _) -> cs) extensionResults
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

        -- ---- verb binding (§11.7) ------------------------------------
        binding = snd <$> smBinding sm
        bl = maybe l fst (smBinding sm)

        nonKeyParams = case binding >>= vbKeyArg of
          Nothing -> smParams sm
          Just ka -> filter ((/= ka) . adName) (smParams sm)

        batchPatchBound =
          (fst <$> (smBatch sm >>= bpBound)) == Just BindPatch

        bindingErrs = case binding of
          Nothing -> []
          Just b ->
            let vt = bindVerbName (vbVerb b)
                tn = unTypeName (vbTarget b)
                targetInfo = Map.lookup tn entInfos
                targetErrs = case targetInfo of
                  Nothing -> [err bl (mctx <> " binds unknown entity `" <> tn <> "`")]
                  Just _ ->
                    if tn == smReturns sm
                      then []
                      else [err bl (mctx <> " binds `/e/" <> tn <> "` but returns `" <> smReturns sm <> "`; a verb binding targets the returned entity")]
                shapeErrs = case vbVerb b of
                  BindCreate ->
                    (case vbKeyArg b of
                       Nothing -> []
                       Just _ -> [err bl (mctx <> " POST binding must not name a key segment (creation binds the collection URL `/e/" <> tn <> "`)")])
                      <> (if vbLww b then [err bl (mctx <> " marks a POST binding last-writer-wins; the marker applies only to keyed bindings")] else [])
                  _ -> case vbKeyArg b of
                    Just _ -> []
                    Nothing -> [err bl (mctx <> " " <> vt <> " binding requires a key segment (`as " <> vt <> " /e/" <> tn <> "/{arg}`)")]
                keyErrs = case (vbKeyArg b, targetInfo) of
                  (Just (ArgName a), Just ei)
                    | not (Set.member a paramNames) ->
                        [err bl (mctx <> " binding names unknown argument `" <> a <> "`")]
                    | otherwise -> case eiKey ei of
                        kf :| [] ->
                          let keyTy = fieldType <$> Map.lookup (FieldName kf) (eiFields ei)
                              argTy = adType <$> List.find ((== ArgName a) . adName) (smParams sm)
                           in if keyTy /= Nothing && argTy == keyTy
                                then []
                                else [err bl (mctx <> " binding key argument `" <> a <> "` must have `" <> tn <> "`'s key type")]
                        _ -> [err bl (mctx <> " binds composite-keyed entity `" <> tn <> "`; verb bindings require a single-field key")]
                  _ -> []
                effectErrs = case vbVerb b of
                  BindPut -> naturalOnly vt
                  BindDelete -> naturalOnly vt
                  _ -> []
                naturalOnly v = case effect of
                  NaturallyIdempotent _ -> []
                  _ -> [err bl (mctx <> " binds " <> v <> " but its effect class is not `natural` (PUT and DELETE promise HTTP idempotency)")]
                arityErrs = case vbVerb b of
                  BindPut
                    | [_] <- nonKeyParams -> []
                    | otherwise -> [err bl (mctx <> " PUT binding requires exactly one non-key argument (the full replacement representation)")]
                  BindPatch
                    | [p] <- nonKeyParams -> patchRecordErrs p
                    | otherwise -> [err bl (mctx <> " PATCH binding requires exactly one non-key argument (the merge-patch record)")]
                  BindDelete
                    | null nonKeyParams -> []
                    | otherwise -> [err bl (mctx <> " DELETE binding admits no arguments besides the key")]
                  BindCreate
                    | [p] <- smParams sm -> createRecordErrs p
                    | otherwise -> [err bl (mctx <> " POST binding requires exactly one argument (the creation record)")]
                createRecordErrs p = case adType p of
                  TNamed rt | Just (DeclRecord _) <- Map.lookup rt types -> []
                  _ ->
                    [err bl (mctx <> " POST binding argument `" <> unArgName (adName p) <> "` must be a declared record type (the creation body is its bare fields, §11.8)")]
                patchRecordErrs p = case adType p of
                  TNamed rt | Just (DeclRecord fs) <- Map.lookup rt types ->
                    concatMap (patchFieldErrs (unTypeName rt)) fs
                  _ ->
                    [err bl (mctx <> " PATCH binding argument `" <> unArgName (adName p) <> "` must be a declared record type (merge-patch semantics)")]
                patchFieldErrs rt (FieldName f, ft) =
                  (case ft of
                     TOptional _ -> []
                     _ -> [err bl (mctx <> " PATCH binding record `" <> rt <> "` field `" <> f <> "` must be optional (merge-patch reapplication must be a no-op)")])
                    <> ( if batchPatchBound && Just f == keyFieldOfTarget
                           then [err bl (mctx <> " PATCH batch binding record `" <> rt <> "` field `" <> f <> "` collides with the bound entity's key field (batch items carry the key inline)")]
                           else []
                       )
                    <> ( if batchPatchBound && f == "key"
                           then [err bl (mctx <> " PATCH batch binding record `" <> rt <> "` field `key` collides with the batch item key")]
                           else []
                       )
                keyFieldOfTarget = case targetInfo of
                  Just ei | kf :| [] <- eiKey ei -> Just kf
                  _ -> Nothing
             in targetErrs <> shapeErrs <> keyErrs <> effectErrs <> arityErrs

        batchBindErrs = case smBatch sm >>= bpBound of
          Nothing -> []
          Just (v, ty) -> case binding of
            Nothing -> [err l (mctx <> " declares a batch collection binding but no `as` verb binding")]
            Just b
              | v == BindPut -> [err l (mctx <> " batch binding uses PUT; PUT never batches")]
              | otherwise ->
                  (if v /= vbVerb b then [err l (mctx <> " batch binding verb `" <> bindVerbName v <> "` must match the mutation's bound verb `" <> bindVerbName (vbVerb b) <> "`")] else [])
                    <> (if ty /= vbTarget b then [err l (mctx <> " batch binding must target `/e/" <> unTypeName (vbTarget b) <> "`, the bound entity")] else [])

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
            , mutBinding = binding
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
            <> bindingErrs
            <> batchBindErrs

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

    -- ---- binding URL-shape registry (§11.7: one mutation per verb + URL) ---------
    bindingShapes :: [(Int, MutationName, (BindVerb, TypeName, Bool))]
    bindingShapes =
      concatMap
        ( \(l, n, d, _) ->
            let single = case mutBinding d of
                  Nothing -> []
                  Just b -> [(vbVerb b, vbTarget b, vbKeyArg b /= Nothing)]
                batchB = case mutBatch d >>= bpBound of
                  Nothing -> []
                  Just (v, ty) -> [(v, ty, False)]
             in map (\s -> (l, n, s)) (Set.toList (Set.fromList (single <> batchB)))
        )
        mutResults

    bindDupErrs =
      snd (List.foldl' step (Map.empty, []) bindingShapes)
      where
        step (seen, es) (l, n, s@(v, ty, keyed)) = case Map.lookup s seen of
          Just prev ->
            ( seen
            , es
                <> [ err l $
                       "mutation `"
                         <> unMutationName n
                         <> "` and mutation `"
                         <> unMutationName prev
                         <> "` bind the same URL shape `"
                         <> bindVerbName v
                         <> " /e/"
                         <> unTypeName ty
                         <> (if keyed then "/{key}`" else "`")
                   ]
            )
          Nothing -> (Map.insert s n seen, es)

    -- ---- annotations (§17.3 @break, §17.5 @deprecated) -------------------
    (annBreaks, annDeprs, annErrs) = List.foldl' step (Map.empty, Map.empty, []) anns
      where
        step (bs, ds, es) (l, p, a) = case a of
          AnnBreak t
            | Map.member p bs -> (bs, ds, es <> [err l "duplicate @break annotation"])
            | otherwise -> (Map.insert p t bs, ds, es)
          AnnDeprecated d
            | not (deprecatable p) ->
                (bs, ds, es <> [err l "@deprecated is only allowed on fields, relationships, roots, and mutations"])
            | Map.member p ds -> (bs, ds, es <> [err l "duplicate @deprecated annotation"])
            | otherwise -> (bs, Map.insert p d ds, es)
        deprecatable = \case
          OnEntityItem _ _ -> True
          OnIfaceItem _ _ -> True
          OnRoot _ -> True
          OnMutation _ -> True
          _ -> False

    -- ---- assembly ------------------------------------------------------------------
    schema =
      Schema
        { schemaName = sName
        , schemaClaims = claims
        , schemaTypes = types
        , schemaInterfaces = interfaces
        , schemaEntities = entities
        , schemaExtensions = extensions
        , schemaFragments = fragments
        , schemaRoots = roots
        , schemaMutations = mutations
        , schemaBreaks = annBreaks
        , schemaDeprecations = annDeprs
        }

    allErrs =
      nameErrs
        <> claimDupErrs
        <> claimTyErrs
        <> typeDupErrs
        <> typeBodyErrs
        <> entErrs
        <> coKeyErrs
        <> ifaceDupErrs
        <> crossDupErrs
        <> concatMap (\(_, _, _, es) -> es) entityResults
        <> extDupErrs
        <> concatMap (\(_, _, _, es) -> es) extensionResults
        <> concatMap (\(_, _, es) -> es) ifaceResults
        <> fragDupErrs
        <> fragErrs
        <> concatMap (\(_, _, _, _, es) -> es) rootResults
        <> rootDupErrs
        <> collDupErrs
        <> concatMap (\(_, _, _, es) -> es) mutResults
        <> mutDupErrs
        <> bindDupErrs
        <> annErrs


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
            foldSItems ("entity `" <> sEntName e <> "`") (sEntItems e)
       in ( EntInfo
              { eiLine = sEntLine e
              , eiKeySpec = sEntKey e
              , eiKey = case sEntKey e of
                  SKeyBy ks -> ks
                  SKeyCo {} -> "id" :| []
              , eiCoKey = Nothing
              , eiImpl = sEntImpl e
              , eiFields = fields
              , eiFieldLines = fieldLines
              , eiRels = rels
              , eiDefaults = defaults
              , eiFetches = fetches
              }
          , dupErrs
          )


-- | Partition a body's items (fields, relationships, default-visibility
-- lines, @fetch by@ clauses), with duplicate-field detection.
foldSItems
  :: Text
  -> [SItem]
  -> ( Map FieldName FieldDef
     , Map FieldName Int
     , [SItem]
     , [(Int, Policy)]
     , [(Int, NonEmpty Text, Policy)]
     , [SchemaError]
     )
foldSItems ctx = List.foldl' item (Map.empty, Map.empty, [], [], [], [])
  where
    item (fm, flm, rs, ds, xs, des) = \case
      SIField l n fd
        | Map.member (FieldName n) fm ->
            (fm, flm, rs, ds, xs, des <> [err l ("duplicate field `" <> n <> "` in " <> ctx)])
        | otherwise ->
            (Map.insert (FieldName n) fd fm, Map.insert (FieldName n) l flm, rs, ds, xs, des)
      it@(SIRelOne {}) -> (fm, flm, rs <> [it], ds, xs, des)
      it@(SIRelMany {}) -> (fm, flm, rs <> [it], ds, xs, des)
      SIDefault l p -> (fm, flm, rs, ds <> [(l, p)], xs, des)
      SIFetch l k p -> (fm, flm, rs, ds, xs <> [(l, k, p)], des)


-- | The raw shape of one @extend entity@ block, pre-elaboration.
data ExtInfo = ExtInfo
  { xiLine :: !Int
  , xiCoKey :: Maybe (CoKeyMode, Text)
  , xiFields :: Map FieldName FieldDef
  , xiFieldLines :: Map FieldName Int
  , xiRels :: [SItem]
  , xiDefaults :: [(Int, Policy)]
  , xiFetches :: [(Int, NonEmpty Text, Policy)]
  }


buildExtInfos :: [SExtend] -> (Map Text ExtInfo, [SchemaError])
buildExtInfos = List.foldl' step (Map.empty, [])
  where
    step (m, es) x
      | Map.member (sExtName x) m =
          (m, es <> [err (sExtLine x) ("duplicate `extend entity " <> sExtName x <> "`")])
      | otherwise =
          let (fields, fieldLines, rels, defaults, fetches, dupErrs) =
                foldSItems ("extension of `" <> sExtName x <> "`") (sExtItems x)
           in ( Map.insert
                  (sExtName x)
                  ExtInfo
                    { xiLine = sExtLine x
                    , xiCoKey = sExtCoKey x
                    , xiFields = fields
                    , xiFieldLines = fieldLines
                    , xiRels = rels
                    , xiDefaults = defaults
                    , xiFetches = fetches
                    }
                  m
              , es <> dupErrs
              )


{- | Resolve a co-key declaration (§3.8) against the raw entity map:
install the base's key spec and key 'FieldDef's, and collect the co-key
checks — unknown base, chained co-keying, key-field re-declaration, and a
stray @by@ clause on a co-keyed declaration.
-}
resolveCoKey :: Map Text EntInfo -> Text -> EntInfo -> (EntInfo, [SchemaError])
resolveCoKey infos en ei = case eiKeySpec ei of
  SKeyBy _ -> (ei, [])
  SKeyCo mode base hasBy ->
    let kw = case mode of
          JoinsBase -> "joins"
          RefinesBase -> "refines"
        ectx = "entity `" <> en <> "` " <> kw <> " `" <> base <> "`"
        l = eiLine ei
        withCo e' = e' {eiCoKey = Just (CoKey (TypeName base) mode)}
        byErrs =
          if hasBy
            then [err l (ectx <> " and must not declare a `by` clause; the key is inherited from the base")]
            else []
     in case Map.lookup base infos of
          Nothing ->
            (withCo ei, byErrs <> [err l (ectx <> ", which is not a declared entity")])
          Just bi -> case eiKeySpec bi of
            SKeyCo {} ->
              (withCo ei, byErrs <> [err l (ectx <> ", which is itself co-keyed; declare every companion against the one base")])
            SKeyBy bkey ->
              let keyNames = Set.fromList (map FieldName (NE.toList bkey))
                  redeclErrs =
                    mapMaybe
                      ( \k ->
                          if Map.member (FieldName k) (eiFields ei)
                            then
                              Just
                                ( err
                                    (fromMaybe l (Map.lookup (FieldName k) (eiFieldLines ei)))
                                    (ectx <> " and re-declares its key field `" <> k <> "`; key fields are inherited")
                                )
                            else Nothing
                      )
                      (NE.toList bkey)
               in ( (withCo ei)
                      { eiKey = bkey
                      , eiFields = Map.union (eiFields ei) (Map.restrictKeys (eiFields bi) keyNames)
                      , eiFieldLines = Map.union (eiFieldLines ei) (Map.restrictKeys (eiFieldLines bi) keyNames)
                      }
                  , byErrs <> redeclErrs
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
