{- | Canonicalization (spec §5.1) and query identity.

Canonical form: defaults applied then erased; local fragments expanded and
selections sorted (fields by (name, canonical arguments), roots by name,
arguments by name, variables by name, inline-fragment alternatives by type
name); query name and comments erased; no insignificant whitespace; UTF-8,
NFC. Schema-fragment spreads survive as late-bound references.

The canonical text is itself a sentence of the §4.8 grammar (closure
property); 'compiledText' re-parses to 'compiledDoc'.

Pinned ordering inside one selection set: fields first (sorted by name,
then by canonically rendered arguments), then schema-fragment spreads (by
name, then arguments), then inline fragments (by concrete type name).
Identical selections merge: same field + arguments (+ @\@depth@) union
their selection sets recursively, identical spreads deduplicate, inline
fragments on one type merge their selections.

NFC note: names are ASCII by the grammar (§4.8), so the only place NFC
normalization acts is inside string literals. 'compileText' NFC-normalizes
the input before parsing and 'renderCanonical' NFC-normalizes the rendered
text as the final serialization step (§5.1 step 4), so hashing always sees
NFC bytes and the closure property holds exactly: the canonical text
re-parses to the canonical document, byte-identical string literals
included.
-}
module Lattice.Canonical (
  Compiled (..),
  compileText,
  compileDocument,
  expandImports,
  renderCanonical,

  -- * Canonical rendering primitives
  canonicalFieldKey,
  renderQValue,
  renderVarDef,
  renderSelectionSet,
) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.List (find, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Normalize (NormalizationMode (NFC), normalize)
import Lattice.Hash (queryHash)
import Lattice.Query.AST
import Lattice.Query.Parser (ParseError (..), parseDocument, parseImportFile)
import Lattice.Query.Validate (
  CompileError,
  ResolvedField (..),
  SelCtx (CtxUnion),
  compileRejected,
  fieldInContext,
  nodesListedTypes,
  nodesRootName,
  normalizeLimitArg,
  normalizeTypeAlias,
  targetContext,
  validateDocument,
 )
import Lattice.Schema
import Lattice.Types
import Lattice.Value (canonicalJsonText)


-- | A validated, canonicalized query.
data Compiled = Compiled
  { compiledDoc :: Document
  -- ^ The restricted (canonical) document: one anonymous query, no
  -- imports, no local fragment definitions.
  , compiledText :: Text
  -- ^ The canonical text — the query's identity (§5.1).
  , compiledHash :: Text
  -- ^ 'Lattice.Hash.queryHash' of 'compiledText'.
  }
  deriving stock (Eq, Show)


{- | Parse, validate (§4.8), and canonicalize (§5.1) query text. The text
must be import-free ('expandImports' resolves imports against their
file map first).
-}
compileText :: Schema -> Budgets -> Text -> Either CompileError Compiled
compileText schema budgets text = do
  doc <- case parseDocument (normalize NFC text) of
    Left e ->
      Left
        ( compileRejected
            ["parse error at offset " <> tshow (peOffset e) <> ": " <> peMessage e]
        )
    Right d -> Right d
  compileDocument schema budgets doc


{- | Validate and canonicalize an already-parsed (import-free) document.

Rule 7's unused-variable check runs on the pre-canonical document (inside
'validateDocument'), before default erasure: a variable whose only use
sits in an erased-default position still counts as used.
-}
compileDocument :: Schema -> Budgets -> Document -> Either CompileError Compiled
compileDocument schema budgets doc = do
  validateDocument schema budgets doc
  let canonDoc = canonicalize schema doc
      text = renderCanonical canonDoc
      bytes = BS.length (TE.encodeUtf8 text)
  if fromIntegral bytes > maxCanonicalBytes budgets
    then
      Left
        ( compileRejected
            [ "canonical text is "
                <> tshow bytes
                <> " bytes; maxCanonicalBytes is "
                <> tshow (maxCanonicalBytes budgets)
            ]
        )
    else
      Right
        Compiled
          { compiledDoc = canonDoc
          , compiledText = text
          , compiledHash = queryHash text
          }


{- | Splice imported fragment definitions into a document. The map is
import-path → file contents; nested imports resolve through the same map
(transitively; an import cycle is an error, as is a query definition in
an imported file, an unresolvable path, or a fragment-name collision
after expansion). Diamond imports splice once.
-}
expandImports :: Map Text Text -> Document -> Either Text Document
expandImports files doc = do
  (imported, _) <- go Set.empty Set.empty (docImports doc)
  let combined = docFragments doc <> imported
  case firstDup (map (unFragmentName . fdName) combined) of
    Just n -> Left ("duplicate fragment '" <> n <> "' after import expansion")
    Nothing ->
      Right doc {docImports = [], docFragments = combined}
  where
    go :: Set Text -> Set Text -> [Text] -> Either Text ([FragmentDefQ], Set Text)
    go _ done [] = Right ([], done)
    go stack done (p : ps)
      | p `Set.member` stack = Left ("import cycle involving \"" <> p <> "\"")
      | p `Set.member` done = go stack done ps
      | otherwise = do
          src <- case Map.lookup p files of
            Nothing -> Left ("unresolved import \"" <> p <> "\"")
            Just s -> Right s
          (imps, frags) <- case parseImportFile src of
            Left e ->
              Left
                ( "parse error in \""
                    <> p
                    <> "\" at offset "
                    <> tshow (peOffset e)
                    <> ": "
                    <> peMessage e
                )
            Right r -> Right r
          (nested, done') <- go (Set.insert p stack) (Set.insert p done) imps
          (rest, done'') <- go stack done' ps
          pure (nested <> frags <> rest, done'')
    firstDup = goDup Set.empty
      where
        goDup _ [] = Nothing
        goDup seen (n : ns)
          | n `Set.member` seen = Just n
          | otherwise = goDup (Set.insert n seen) ns


-- ---------------------------------------------------------------------------
-- Canonicalization (§5.1 steps 1–3)
-- ---------------------------------------------------------------------------

{- | Produce the restricted document. Only called on documents
'validateDocument' accepted (expansion relies on rule 8's acyclicity and
resolution; erasure relies on rule 6's declaredness).
-}
canonicalize :: Schema -> Document -> Document
canonicalize schema Document {..} =
  Document
    { docImports = []
    , docFragments = []
    , docQuery =
        QueryDef
          { qName = Nothing
          , qVars = sortOn vdName (map normVar (qVars docQuery))
          , qSelection = sortAndMerge (map eraseRoot (expandSel Map.empty (qSelection docQuery)))
          }
    }
  where
    localFrags :: Map FragmentName FragmentDefQ
    localFrags = Map.fromList (map (\f -> (fdName f, f)) docFragments)

    normVar vd =
      vd {vdType = (vdType vd) {trName = normalizeTypeAlias (trName (vdType vd))}}

    -- Step 2a: expand local fragment spreads inline, substituting
    -- parameter values (spread arguments over parameter defaults) into
    -- the body. Schema-fragment spreads survive with their arguments
    -- substituted and defaults erased.
    expandSel :: Map VarName QValue -> SelectionSet -> SelectionSet
    expandSel env = concatMap $ \case
      SField f ->
        [ SField
            f
              { fArgs = map (substArg env) (fArgs f)
              , fSelection = fmap (expandSel env) (fSelection f)
              }
        ]
      SInline t ss -> [SInline t (expandSel env ss)]
      SSpread n args ->
        let args' = map (substArg env) args
         in case Map.lookup n localFrags of
              Just lf -> expandSel (bindParams (fdParams lf) args') (fdSelection lf)
              Nothing -> case Map.lookup n (schemaFragments schema) of
                Just sf -> [SSpread n (eraseSpreadDefaults (fragParams sf) args')]
                Nothing -> [SSpread n args']

    bindParams :: [VarDef] -> [Argument] -> Map VarName QValue
    bindParams params args = Map.fromList (map bind params)
      where
        bind p =
          let given =
                find (\a -> unArgName (argName a) == unVarName (vdName p)) args
              v = case given of
                Just a -> argValue a
                Nothing -> case vdDefault p of
                  Just d -> d
                  -- A required parameter is never absent post-validation;
                  -- keep the body's variable to stay total.
                  Nothing -> QVar (vdName p)
           in (vdName p, v)

    substArg env a = a {argValue = substQ env (argValue a)}
    substQ env = \case
      QVar v -> Map.findWithDefault (QVar v) v env
      QList vs -> QList (map (substQ env) vs)
      other -> other

    eraseSpreadDefaults :: [VarDef] -> [Argument] -> [Argument]
    eraseSpreadDefaults params = filter keep
      where
        keep (Argument n v) =
          case find (\p -> unVarName (vdName p) == unArgName n) params of
            Just p | Just d <- vdDefault p -> v /= d
            _ -> True

    -- Step 1 (schema-directed): apply defaults for omitted arguments, then
    -- erase arguments equal to their default — one operation, since an
    -- omitted defaulted argument is already spelled canonically. Also
    -- erases pagination @first@ equal to the collection's default page and
    -- normalizes the @limit@ synonym.
    eraseRoot :: Selection -> Selection
    eraseRoot = \case
      SField f
        | Just rd <- Map.lookup (RootName (unFieldName (fName f))) (schemaRoots schema) ->
            let mcol = case (rootKind rd, rootCollection rd) of
                  (RootList, Just c) -> Just c
                  _ -> Nothing
                ctx = case targetContext schema (rootTarget rd) of
                  Right c -> Just c
                  Left _ -> Nothing
             in SField
                  f
                    { fArgs = eraseArgs (rootParams rd) mcol (fArgs f)
                    , fSelection = fmap (eraseSet ctx) (fSelection f)
                    }
        -- The implicit @nodes@ root (§14.4): no declared arguments to
        -- erase (@refs@ has no default), but the selection's field
        -- defaults erase per concrete type through the dispatch union,
        -- exactly as a declared union-target root would.
        | RootName (unFieldName (fName f)) == nodesRootName ->
            let ctx = do
                  sub <- fSelection f
                  CtxUnion <$> NE.nonEmpty (nodesListedTypes sub)
             in SField f {fSelection = fmap (eraseSet ctx) (fSelection f)}
      s -> s

    eraseSet mctx = case mctx of
      Nothing -> id
      Just ctx -> map (eraseInCtx ctx)

    eraseInCtx ctx = \case
      SField f -> case fieldInContext schema ctx (fName f) of
        Right (RScalar fd) -> SField f {fArgs = eraseArgs (fieldArgs fd) Nothing (fArgs f)}
        Right (RToOne target) ->
          SField f {fSelection = fmap (eraseSet (ctxOf target)) (fSelection f)}
        Right (RToMany target col) ->
          SField
            f
              { fArgs = eraseArgs [] (Just col) (fArgs f)
              , fSelection = fmap (eraseSet (ctxOf target)) (fSelection f)
              }
        Left _ -> SField f
      SInline t ss ->
        SInline t (eraseSet (ctxOf (TargetEntity t)) ss)
      SSpread n args -> case Map.lookup n (schemaFragments schema) of
        Just sf -> SSpread n (eraseSpreadDefaults (fragParams sf) args)
        Nothing -> SSpread n args

    ctxOf target = case targetContext schema target of
      Right c -> Just c
      Left _ -> Nothing

    eraseArgs :: [ArgDef] -> Maybe CollectionDef -> [Argument] -> [Argument]
    eraseArgs declared mcol rawArgs = filter keep args
      where
        paginated = case mcol of
          Just col | Paginated _ <- colWindow col -> True
          _ -> False
        args =
          if paginated
            then normalizeLimitArg (map adName declared) rawArgs
            else rawArgs
        keep (Argument n v) = case find (\ad -> adName ad == n) declared of
          Just ad | Just d <- adDefault ad -> v /= d
          Just _ -> True
          Nothing -> case mcol of
            Just col
              | Paginated cs <- colWindow col
              , n == ArgName "first"
              , Just dp <- csDefaultPage cs ->
                  v /= QInt (toInteger dp)
            _ -> True

    -- Step 2b: sort arguments, selections, and alternatives; merge
    -- identical selections (bottom-up, so merged sets are canonical too).
    sortAndMerge :: SelectionSet -> SelectionSet
    sortAndMerge = mergeAdjacent . sortOn selKey . map sortInner
      where
        sortInner = \case
          SField f ->
            SField
              f
                { fArgs = sortOn argName (fArgs f)
                , fSelection = fmap sortAndMerge (fSelection f)
                }
          SInline t ss -> SInline t (sortAndMerge ss)
          SSpread n args -> SSpread n (sortOn argName args)
        selKey :: Selection -> (Int, Text, Text, Int)
        selKey = \case
          SField f ->
            (0, unFieldName (fName f), renderArguments (fArgs f), maybe 0 id (fDepth f))
          SSpread n args -> (1, unFragmentName n, renderArguments args, 0)
          SInline t _ -> (2, unTypeName t, "", 0)
        mergeAdjacent (a : b : rest) = case mergeTwo a b of
          Just ab -> mergeAdjacent (ab : rest)
          Nothing -> a : mergeAdjacent (b : rest)
        mergeAdjacent xs = xs
        mergeTwo (SField a) (SField b)
          | fName a == fName b
          , fArgs a == fArgs b
          , fDepth a == fDepth b =
              Just (SField a {fSelection = unionSel (fSelection a) (fSelection b)})
        mergeTwo (SSpread n1 a1) (SSpread n2 a2)
          | n1 == n2, a1 == a2 = Just (SSpread n1 a1)
        mergeTwo (SInline t1 s1) (SInline t2 s2)
          | t1 == t2 = Just (SInline t1 (sortAndMerge (s1 <> s2)))
        mergeTwo _ _ = Nothing
        unionSel (Just x) (Just y) = Just (sortAndMerge (x <> y))
        unionSel (Just x) Nothing = Just x
        unionSel Nothing my = my


-- ---------------------------------------------------------------------------
-- Rendering (§5.1 step 4)
-- ---------------------------------------------------------------------------

{- | Render a canonical document as canonical text (§5.1 step 4).

Order-preserving: selections, arguments, and variables render exactly as
stored (the sorting of §5.1 step 2 is an AST transformation performed by
'compileDocument' before rendering). Only the query definition renders;
a restricted document has no imports and no local fragment definitions.
The result is NFC-normalized (module header) — the final serialization
step, so every hash of canonical text is over NFC bytes.
-}
renderCanonical :: Document -> Text
renderCanonical Document {docQuery = QueryDef {..}} =
  normalize NFC ("query" <> vars <> renderSelectionSet qSelection)
  where
    vars = case qVars of
      [] -> ""
      vs -> "(" <> T.intercalate "," (map renderVarDef vs) <> ")"


{- | The canonical wire field key (§4.1, §9.1): @avatarUrl(size:48)@,
@comments(first:20)@. Arguments sort by name and render their values as
'Lattice.Value.canonicalJsonText' (JSON strings for strings and enums,
pinned number rendering, @true@\/@false@, @[..]@ arrays). No arguments
renders the bare field name, no parentheses. Used by the planner, the
server, and clients; this is the one implementation.
-}
canonicalFieldKey :: FieldName -> [(ArgName, A.Value)] -> Text
canonicalFieldKey (FieldName f) args = case args of
  [] -> f
  _ ->
    f
      <> "("
      <> T.intercalate "," (map one (sortOn fst args))
      <> ")"
  where
    one (ArgName n, v) = n <> ":" <> canonicalJsonText v


{- | Canonical query-literal rendering of a value. Coincides byte-for-byte
with 'canonicalJsonText' on strings, numbers, booleans, and lists of
those; extends it with the two query-only forms: variables (@$name@) and
enum values (bare names).
-}
renderQValue :: QValue -> Text
renderQValue = \case
  QVar (VarName v) -> "$" <> v
  QEnum e -> e
  QList vs -> "[" <> T.intercalate "," (map renderQValue vs) <> "]"
  QInt i -> canonicalJsonText (A.Number (fromInteger i))
  QNum s -> canonicalJsonText (A.Number s)
  QString t -> canonicalJsonText (A.String t)
  QBool b -> canonicalJsonText (A.Bool b)


-- | @$name:Type@, @$name:Type?@, @$name:Type=default@ — no spaces.
renderVarDef :: VarDef -> Text
renderVarDef VarDef {..} =
  "$"
    <> unVarName vdName
    <> ":"
    <> trName vdType
    <> (if trOptional vdType then "?" else "")
    <> maybe "" (\v -> "=" <> renderQValue v) vdDefault


{- | @{f1 f2 edge{...}}@ — single space between selections, otherwise
minimal separators. Order-preserving (see 'renderCanonical').
-}
renderSelectionSet :: SelectionSet -> Text
renderSelectionSet ss =
  "{" <> T.intercalate " " (map renderSelection ss) <> "}"


renderSelection :: Selection -> Text
renderSelection = \case
  SField Field {..} ->
    unFieldName fName
      <> renderArguments fArgs
      <> maybe "" (\n -> "@depth(" <> T.pack (show n) <> ")") fDepth
      <> maybe "" renderSelectionSet fSelection
  SSpread (FragmentName n) args -> "..." <> n <> renderArguments args
  SInline (TypeName t) sub -> "... on " <> t <> renderSelectionSet sub


-- | @(a:1,b:"x")@ — order-preserving; empty renders nothing.
renderArguments :: [Argument] -> Text
renderArguments = \case
  [] -> ""
  as ->
    "("
      <> T.intercalate "," (map one as)
      <> ")"
  where
    one (Argument (ArgName n) v) = n <> ":" <> renderQValue v


tshow :: (Show a) => a -> Text
tshow = T.pack . show
