{- | Plan compilation (spec §7.3, §8.1): the authorization path join over the
query's traversal DAG, the per-slice partition, the plan id over pertinent
declarations, surrogate-key derivation, and the @explain@ rendering (§20.2).

The exported types are the execution contract consumed by
"Lattice.Server"; the compiler ('planQuery') fills them from a 'Compiled'
query.

Path-join rules (§8.1):

* the level of a root is its declared policy's level;
* levels propagate along edges: @level(child) = level(parent) ⊔ policy(edge)@;
* a field's emission level is @level(node) ⊔ policy(field)@;
* point-fetch masks slice at @nodesPolicy(T) ⊔ policy(field)@ (§6.7).

Implementation notes:

* An omitted edge policy contributes nothing to the join (it joins with
  'LPublic'); the target entity's default policy applies to /fields/, never
  to edge membership. @fetch by@ ('entityFetchBy') plays no part in query
  planning — it masks point fetches only.

* A @\@depth(n)@ edge keeps 'peDepth' and its 'peSelection' holds ONE level
  of the §4.8 rule-4 expansion: the enclosing selection set re-resolved at
  the edge's membership level. Because 'joinLevel' is idempotent, that
  level is a fixed point, and the recursive occurrence inside the expansion
  is tied back to the same node (a cyclic structure built through a lazy
  'MapL.singleton'). Consumers must not traverse a depth edge's
  'peSelection' unboundedly — in particular @show@ on a plan containing a
  @\@depth@ edge does not terminate; every traversal in this module either
  counts with 'peDepth' arithmetically or carries fuel/visited cutoffs.

* 'planPertinent' renders whole declarations (each touched entity, root,
  fragment, interface, and referenced named type) via "Lattice.IDL.Print",
  sorted by a @kind name@ key. This coarsens plan identity slightly against
  the §7.3 ideal (field-granular pertinence): an unselected field changing
  on a /touched/ entity moves the plan id, but untouched declarations never
  do.
-}
module Lattice.Plan (
  Plan (..),
  PlanRoot (..),
  TypedSelection (..),
  NodeSelection (..),
  PlanField (..),
  PlanEdge (..),
  BoundArg (..),
  planQuery,
  planSliceRecord,
  planProjections,
  explainJson,

  -- * Pagination argument names
  paginationArgNames,
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (find, foldl', partition, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as MapL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Scientific qualified as Sci
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Backend (Projection (..))
import Lattice.Canonical (Compiled (..), canonicalFieldKey)
import Lattice.Hash (planIdHash)
import Lattice.IDL.Print qualified as Print
import Lattice.Query.AST
import Lattice.Query.Validate (
  CompileError,
  compileRejected,
  isNodesRootDef,
  nodesListedTypes,
  nodesRootDef,
  nodesRootName,
 )
import Lattice.Schema
import Lattice.Types
import Lattice.Value (canonicalJsonText, qvalueToJson)
import Lattice.Wire (PlanRecord (..), SliceInfo (..))
import Numeric.Natural (Natural)


data Plan = Plan
  { planId :: Text
  -- ^ @pl_…@, hash over (canonical text, pertinent declarations).
  , planQueryHash :: Text
  , planCanonicalText :: Text
  , planVars :: [VarDef]
  , planRoots :: Map RootName PlanRoot
  , planSlices :: Map SliceName SliceInfo
  -- ^ Data slices only; a missing slice is empty.
  , planPertinent :: Text
  -- ^ Canonical rendering of the pertinent declarations (feeds 'planId').
  }
  deriving stock (Show)


data PlanRoot = PlanRoot
  { prDef :: RootDef
  , prArgs :: [(ArgName, BoundArg)]
  -- ^ Grouping-key and pagination arguments, canonical order.
  , prLevel :: Level
  , prSelection :: TypedSelection
  }
  deriving stock (Show)


-- | A selection resolved per concrete target type (interface dispatch, §4.4).
newtype TypedSelection = TypedSelection
  { tsPerType :: Map TypeName NodeSelection
  }
  deriving stock (Show)


data NodeSelection = NodeSelection
  { nsFields :: [PlanField]
  , nsEdges :: [PlanEdge]
  }
  deriving stock (Show)


data PlanField = PlanField
  { pfName :: FieldName
  , pfArgs :: [(ArgName, BoundArg)]
  , pfKey :: Text
  -- ^ Canonical wire key, e.g. @avatarUrl(size:48)@ (variables render
  -- at execution time; keys containing variables are completed then).
  , pfLevel :: Level
  -- ^ Emission level: node level ⊔ field policy.
  , pfDerivation :: Maybe Derivation
  -- ^ The field's declared derivation (§3.7); the executor resolves
  -- @on read@ read sets as hidden traversals, @maintained@ values read
  -- from the row like any stored field.
  }
  deriving stock (Show)


data PlanEdge = PlanEdge
  { peField :: FieldName
  , peRel :: RelationshipDef
  , peArgs :: [(ArgName, BoundArg)]
  , peKey :: Text
  , peLevel :: Level
  -- ^ Membership level of entities revealed through this edge.
  , peDepth :: Maybe Int
  -- ^ @\@depth(n)@ recursion, expanded by the executor.
  , peSelection :: TypedSelection
  }
  deriving stock (Show)


-- | An argument bound to a literal or left symbolic until request time.
data BoundArg
  = ArgLit A.Value
  | ArgVar VarName
  deriving stock (Eq, Show)


-- | The reserved pagination argument names (§3.6).
paginationArgNames :: [ArgName]
paginationArgNames = ["first", "after", "last", "before", "around"]


{- | Compile a canonicalized query against the schema: resolve every field
and edge, run the path join, derive slices and the plan id, and check the
static plan budgets (roots, depth, fan-out; §14.1).

Budget violations are structural and deterministic per canonical text, so
they reject as @400 lattice:compile-rejected@ (negatively cacheable,
§10.8); the @503 lattice:compile-budget@ code is reserved for compile
timeouts, which are not deterministic.
-}
planQuery :: Schema -> Budgets -> Compiled -> Either CompileError Plan
planQuery schema budgets compiled = do
  let queryDef = docQuery (compiledDoc compiled)
  rootFields <- extractRootFields (qSelection queryDef)
  let nRoots = length rootFields
  unless (fromIntegral nRoots <= maxRoots budgets) $
    Left
      ( compileRejected
          [ "query has "
              <> tshow nRoots
              <> " roots; the origin's maxRoots budget is "
              <> tshow (maxRoots budgets)
          ]
      )
  (rootPairs, touched) <- runPlanM (traverse (resolveRoot schema) rootFields) emptyTouched
  rootsMap <- uniqueRoots rootPairs
  let depth = plansDepth rootsMap
  unless (depth <= maxDepth budgets) $
    Left
      ( compileRejected
          [ "query traversal depth "
              <> tshow depth
              <> " (with @depth counting its full expansion) exceeds the origin's maxDepth budget "
              <> tshow (maxDepth budgets)
          ]
      )
  let rounds = planLoaderRounds rootsMap
  unless (fromIntegral (length rounds) <= maxRounds budgets) $
    Left
      ( compileRejected
          [ "query needs "
              <> tshow (length rounds)
              <> " loader rounds; the origin's maxRounds budget is "
              <> tshow (maxRounds budgets)
          ]
      )
  checkFanout budgets rounds
  let pertinent = renderPertinent schema touched
  Right
    Plan
      { planId = planIdHash (compiledText compiled) pertinent
      , planQueryHash = compiledHash compiled
      , planCanonicalText = compiledText compiled
      , planVars = qVars queryDef
      , planRoots = rootsMap
      , planSlices = deriveSlices budgets (nodesMembership schema rootsMap) rootsMap
      , planPertinent = pertinent
      }


-- | The @slice=plan@ wire record for a plan (§6.6).
planSliceRecord :: Plan -> PlanRecord
planSliceRecord p =
  PlanRecord
    { prQuery = planQueryHash p
    , prPlan = planId p
    , prSlices = planSlices p
    }


-- ---------------------------------------------------------------------------
-- The plan monad: Either with an accumulating touched-declaration set
-- ---------------------------------------------------------------------------

{- | The set of declarations a compilation read — the seed of the §7.3
pertinent-declaration closure.
-}
data Touched = Touched
  { tRoots :: Set RootName
  , tEntities :: Set TypeName
  , tInterfaces :: Set InterfaceName
  , tFragments :: Set FragmentName
  , tClaims :: Set ClaimName
  }


emptyTouched :: Touched
emptyTouched = Touched Set.empty Set.empty Set.empty Set.empty Set.empty


newtype PlanM a = PlanM {runPlanM :: Touched -> Either CompileError (a, Touched)}


instance Functor PlanM where
  fmap f (PlanM g) = PlanM $ \s -> fmap (\(a, s') -> (f a, s')) (g s)


instance Applicative PlanM where
  pure a = PlanM $ \s -> Right (a, s)
  PlanM mf <*> PlanM ma = PlanM $ \s -> do
    (f, s') <- mf s
    (a, s'') <- ma s'
    Right (f a, s'')


instance Monad PlanM where
  PlanM ma >>= f = PlanM $ \s -> do
    (a, s') <- ma s
    runPlanM (f a) s'


rejectM :: [Text] -> PlanM a
rejectM ds = PlanM $ \_ -> Left (compileRejected ds)


touching :: (Touched -> Touched) -> PlanM ()
touching f = PlanM $ \s -> Right ((), f s)


touchRoot :: RootName -> PlanM ()
touchRoot r = touching (\t -> t {tRoots = Set.insert r (tRoots t)})


touchEntity :: TypeName -> PlanM ()
touchEntity e = touching (\t -> t {tEntities = Set.insert e (tEntities t)})


touchInterface :: InterfaceName -> PlanM ()
touchInterface i = touching (\t -> t {tInterfaces = Set.insert i (tInterfaces t)})


touchFragment :: FragmentName -> PlanM ()
touchFragment f = touching (\t -> t {tFragments = Set.insert f (tFragments t)})


-- | Record the claims a policy's predicates range over (§7.3, §8.1).
touchPolicy :: Policy -> PlanM ()
touchPolicy = \case
  RequiresClaims preds ->
    touching (\t -> t {tClaims = foldl' (flip Set.insert) (tClaims t) (map cpClaim preds)})
  Public -> pure ()
  Private -> pure ()


touchTarget :: Target -> PlanM ()
touchTarget = \case
  TargetInterface i -> touchInterface i
  TargetEntity _ -> pure ()
  TargetUnion _ -> pure ()


-- ---------------------------------------------------------------------------
-- Root resolution
-- ---------------------------------------------------------------------------

extractRootFields :: SelectionSet -> Either CompileError [Field]
extractRootFields = traverse asField
  where
    asField = \case
      SField f -> Right f
      SInline ty _ ->
        Left
          ( compileRejected
              ["root selections must be root fields; found an inline fragment on " <> unTypeName ty]
          )
      SSpread n _ ->
        Left
          ( compileRejected
              ["root selections must be root fields; found a fragment spread ..." <> unFragmentName n]
          )


uniqueRoots :: [(RootName, PlanRoot)] -> Either CompileError (Map RootName PlanRoot)
uniqueRoots = foldM step Map.empty
  where
    step m (n, r)
      | Map.member n m = Left (compileRejected ["duplicate root field: " <> unRootName n])
      | otherwise = Right (Map.insert n r m)


resolveRoot :: Schema -> Field -> PlanM (RootName, PlanRoot)
resolveRoot schema f = do
  let rname = RootName (unFieldName (fName f))
  case Map.lookup rname (schemaRoots schema) of
    Just rd -> resolveDeclaredRoot schema f rname rd
    Nothing
      | rname == nodesRootName -> resolveNodesRoot schema f
      | otherwise -> rejectM ["unknown root: " <> unRootName rname]


resolveDeclaredRoot :: Schema -> Field -> RootName -> RootDef -> PlanM (RootName, PlanRoot)
resolveDeclaredRoot schema f rname rd = do
  touchRoot rname
  touchPolicy (rootPolicy rd)
  args <- bindArgs (fArgs f)
  sels <- case (fSelection f, fDepth f) of
    (Just s, Nothing) -> pure s
    (_, Just _) -> rejectM ["@depth is not valid on a root field: " <> unRootName rname]
    (Nothing, Nothing) -> rejectM ["root field requires a selection set: " <> unRootName rname]
  let lvl = policyLevel (rootPolicy rd)
  sel <- resolveTyped schema lvl (rootTarget rd) sels
  pure (rname, PlanRoot {prDef = rd, prArgs = args, prLevel = lvl, prSelection = sel})


{- | The implicit @nodes@ root (§14.4): dispatch union drawn from the
selection's inline fragments, each type resolved at its own membership
level — the level of its @fetch by@ policy ('entityFetchBy'), which is how
@fetchBy ⊔ field policy@ falls out of the ordinary path join. A type whose
@fetch by@ is absent (by-ref fetching forbidden) is validated and touched
(its declaration is pertinent: a fetch-by change must move the plan id) but
excluded from the per-type selection, so its refs never load and never
emit — indistinguishable from nonexistence.

'prLevel' is 'LPublic' (the root itself is reachable; membership is
per-type); 'planQuery' feeds the per-type membership levels into the slice
partition via 'nodesMembership'. The implicit root is never a schema
declaration, so it is not touched as a pertinent root — the canonical text
names it, and the per-type fetch-by policies travel with the touched
entity declarations.
-}
resolveNodesRoot :: Schema -> Field -> PlanM (RootName, PlanRoot)
resolveNodesRoot schema f = do
  args <- bindArgs (fArgs f)
  sels <- case (fSelection f, fDepth f) of
    (Just s, Nothing) -> pure s
    (_, Just _) -> rejectM ["@depth is not valid on a root field: nodes"]
    (Nothing, Nothing) -> rejectM ["root field requires a selection set: nodes"]
  ts <- case NE.nonEmpty (nodesListedTypes sels) of
    Just ts -> pure ts
    Nothing ->
      rejectM ["the nodes root dispatches per concrete type; select with inline fragments (§14.4)"]
  ents <- traverse (entityOf schema) (NE.toList ts)
  let typed = zip (NE.toList ts) ents
  checkFieldCoverage typed sels
  nodes <- traverse (resolveFetchable sels) typed
  pure
    ( nodesRootName
    , PlanRoot
        { prDef = nodesRootDef ts
        , prArgs = args
        , prLevel = LPublic
        , prSelection = TypedSelection (Map.fromList (catMaybes nodes))
        }
    )
  where
    resolveFetchable sels (t, ent) = do
      touchEntity t
      case entityFetchBy ent of
        Nothing -> pure Nothing
        Just pol -> do
          touchPolicy pol
          node <- resolveNode schema t ent (policyLevel pol) [] sels
          pure (Just (t, node))


{- | Per-root membership-level overrides for the slice partition: the
implicit @nodes@ root's membership is per type — each fetchable listed
type contributes its @fetch by@ policy's level. Declared roots are absent
from the map (their membership is 'prLevel').
-}
nodesMembership :: Schema -> Map RootName PlanRoot -> Map RootName [Level]
nodesMembership schema roots =
  Map.fromList
    [ (rn, lvls)
    | (rn, pr) <- Map.toList roots
    , isNodesRootDef (prDef pr)
    , let lvls =
            mapMaybe
              (\t -> policyLevel <$> (entityFetchBy =<< lookupEntity schema t))
              (Map.keys (tsPerType (prSelection pr)))
    ]


-- ---------------------------------------------------------------------------
-- Selection resolution (per concrete target type, §4.4)
-- ---------------------------------------------------------------------------

{- | In-flight @\@depth@ expansions: @(enclosing type, edge field, membership
level) → the node under construction@. Kept as an association list so the
tied node stays an unevaluated thunk until the knot closes.
-}
type DepthTies = [((TypeName, FieldName, Level), NodeSelection)]


resolveTyped :: Schema -> Level -> Target -> SelectionSet -> PlanM TypedSelection
resolveTyped schema lvl target sels = do
  touchTarget target
  let types = targetTypes schema target
  when (null types) $
    rejectM ["target resolves to no concrete entity types: " <> targetLabel target]
  ents <- traverse (entityOf schema) types
  let typed = zip types ents
  checkFieldCoverage typed sels
  nodes <-
    traverse
      ( \(t, ent) -> do
          touchEntity t
          node <- resolveNode schema t ent lvl [] sels
          pure (t, node)
      )
      typed
  pure (TypedSelection (Map.fromList nodes))


entityOf :: Schema -> TypeName -> PlanM EntityDef
entityOf schema t = case lookupEntity schema t of
  Just e -> pure e
  Nothing -> rejectM ["unknown entity type: " <> unTypeName t]


{- | Defensive re-check of §4.8 rule 5: every plain field must be declared by
at least one concrete target type (per-type resolution silently narrows to
the types that declare it, which is the §4.4 dispatch rule).
-}
checkFieldCoverage :: [(TypeName, EntityDef)] -> SelectionSet -> PlanM ()
checkFieldCoverage typed = traverse_ go
  where
    go = \case
      SField f
        | any (\(_, e) -> declaresField e (fName f)) typed -> pure ()
        | otherwise ->
            rejectM ["field " <> unFieldName (fName f) <> " is not declared by any target type"]
      SInline _ _ -> pure ()
      SSpread _ _ -> pure ()


resolveNode ::
  Schema -> TypeName -> EntityDef -> Level -> DepthTies -> SelectionSet -> PlanM NodeSelection
resolveNode schema t ent lvl ties sels = do
  flat <- flattenFor schema t ent Set.empty sels
  grouped <- groupFields flat
  items <- traverse (resolveItem schema t ent lvl ties sels) grouped
  let (fs, es) = partitionEithers items
  pure NodeSelection {nsFields = fs, nsEdges = es}


{- | Flatten one selection set to the plain fields that apply to the given
concrete type: inline fragments filter by type, schema-fragment spreads
expand (late-bound, §4.5) with their parameters substituted.
-}
flattenFor ::
  Schema -> TypeName -> EntityDef -> Set FragmentName -> SelectionSet -> PlanM [Field]
flattenFor schema t ent visited sels = concat <$> traverse go sels
  where
    go = \case
      SField f
        | declaresField ent (fName f) -> pure [f]
        | otherwise -> pure []
      SInline ty sub
        | inlineApplies ty -> flattenFor schema t ent visited sub
        | otherwise -> pure []
      SSpread name sargs -> do
        fdef <- case Map.lookup name (schemaFragments schema) of
          Just d -> pure d
          Nothing -> rejectM ["unknown schema fragment: " <> unFragmentName name]
        touchFragment name
        if fragApplies fdef
          then do
            when (Set.member name visited) $
              rejectM ["fragment spread cycle through " <> unFragmentName name]
            body <- substituteFragment name fdef sargs
            flattenFor schema t ent (Set.insert name visited) body
          else pure []
    inlineApplies ty =
      ty == t || Set.member (InterfaceName (unTypeName ty)) (entityImplements ent)
    fragApplies fdef =
      fragOn fdef == unTypeName t
        || Set.member (InterfaceName (fragOn fdef)) (entityImplements ent)


declaresField :: EntityDef -> FieldName -> Bool
declaresField ent n = Map.member n (entityFields ent) || Map.member n (entityRels ent)


{- | Substitute a schema fragment's parameters into its body: provided spread
arguments win, then declared defaults; an optional parameter without either
erases the arguments it appears in (omission is the only spelling of
absence, §4.8 rule 6).
-}
substituteFragment :: FragmentName -> FragmentDef -> [Argument] -> PlanM SelectionSet
substituteFragment name fdef sargs = do
  traverse_ checkKnown sargs
  binds <- traverse bindParam (fragParams fdef)
  pure (map (substSel binds) (fragSelection fdef))
  where
    bindParam vd = case lookupProvided (vdName vd) <|> vdDefault vd of
      Just v -> pure (vdName vd, Just v)
      Nothing
        | trOptional (vdType vd) -> pure (vdName vd, Nothing)
        | otherwise ->
            rejectM
              [ "fragment "
                  <> unFragmentName name
                  <> " requires argument "
                  <> unVarName (vdName vd)
              ]
    lookupProvided p =
      fmap argValue (find (\a -> unArgName (argName a) == unVarName p) sargs)
    checkKnown a =
      unless (any (\vd -> unVarName (vdName vd) == unArgName (argName a)) (fragParams fdef)) $
        rejectM
          [ "fragment "
              <> unFragmentName name
              <> " has no parameter "
              <> unArgName (argName a)
          ]


substSel :: [(VarName, Maybe QValue)] -> Selection -> Selection
substSel b = \case
  SField f ->
    SField
      f
        { fArgs = mapMaybe (substArg b) (fArgs f)
        , fSelection = fmap (map (substSel b)) (fSelection f)
        }
  SInline ty s -> SInline ty (map (substSel b) s)
  SSpread n as -> SSpread n (mapMaybe (substArg b) as)


substArg :: [(VarName, Maybe QValue)] -> Argument -> Maybe Argument
substArg b (Argument n v) = case v of
  QVar var -> case lookup var b of
    Just (Just v') -> Just (Argument n v')
    Just Nothing -> Nothing
    Nothing -> Just (Argument n (QVar var))
  QList vs -> Just (Argument n (QList (map (substListVal b) vs)))
  _ -> Just (Argument n v)


substListVal :: [(VarName, Maybe QValue)] -> QValue -> QValue
substListVal b = \case
  QVar var -> case lookup var b of
    Just (Just v') -> v'
    _ -> QVar var
  QList vs -> QList (map (substListVal b) vs)
  other -> other


{- | Merge duplicate selections of one field (reachable through overlapping
fragment expansions): identical @(name, arguments, depth)@ keys collapse,
edge subselections concatenate. The result is sorted by (name, canonical
arguments), matching canonical field order.
-}
groupFields :: [Field] -> PlanM [Field]
groupFields flat = Map.elems <$> foldM insert Map.empty flat
  where
    insert m f = do
      let key = (unFieldName (fName f), map argText (sortOn argName (fArgs f)), fDepth f)
      case Map.lookup key m of
        Nothing -> pure (Map.insert key f m)
        Just f0 -> do
          merged <- mergeField f0 f
          pure (Map.insert key merged m)
    argText (Argument a v) = unArgName a <> ":" <> renderKeyVal v
    mergeField f0 f1 = case (fSelection f0, fSelection f1) of
      (Nothing, Nothing) -> pure f0
      (Just s0, Just s1) -> pure f0 {fSelection = Just (s0 <> s1)}
      _ ->
        rejectM
          [ "field "
              <> unFieldName (fName f0)
              <> " is selected both with and without a selection set"
          ]


resolveItem ::
  Schema ->
  TypeName ->
  EntityDef ->
  Level ->
  DepthTies ->
  -- | The enclosing (raw) selection set, repeated by @\@depth@ (§4.8 rule 4).
  SelectionSet ->
  Field ->
  PlanM (Either PlanField PlanEdge)
resolveItem schema t ent lvl ties enclosing f =
  case lookupEntityRel ent (fName f) of
    Just rel -> Right <$> resolveEdge schema t ent lvl ties enclosing f rel
    Nothing -> case lookupEntityField ent (fName f) of
      Just fd -> Left <$> resolveScalar schema ent lvl f fd
      Nothing ->
        rejectM
          [ "field "
              <> unFieldName (fName f)
              <> " is not declared by entity "
              <> unTypeName t
          ]


resolveScalar :: Schema -> EntityDef -> Level -> Field -> FieldDef -> PlanM PlanField
resolveScalar schema ent lvl f fd = do
  when (isJust (fSelection f)) $
    rejectM ["scalar field takes no selection set: " <> unFieldName (fName f)]
  when (isJust (fDepth f)) $
    rejectM ["@depth is only valid on a self-targeting edge: " <> unFieldName (fName f)]
  args <- bindArgs (fArgs f)
  let pol = entityFieldPolicy ent fd
  touchPolicy pol
  -- §3.7 + §7.3: an @on read@ derivation's hidden traversals make the dep
  -- fragment and target declarations pertinent — a change to either must
  -- move the plan id. Maintained fields read only the row.
  case fieldDerivation fd of
    Just d | OnRead <- derivMaterialize d -> traverse_ touchDep (derivReads d)
    _ -> pure ()
  pure
    PlanField
      { pfName = fName f
      , pfArgs = args
      , pfKey = fieldKey (fName f) (fArgs f)
      , pfLevel = joinLevel lvl (policyLevel pol)
      , pfDerivation = fieldDerivation fd
      }
  where
    touchDep = \case
      OwnFields _ -> pure ()
      ViaEdge e frag -> do
        touchFragment frag
        touchDepRel e
      ViaCollection r _ -> touchDepRel r
    touchDepRel e = case lookupEntityRel ent e of
      Nothing -> pure ()
      Just rel -> do
        touchTarget (relTarget rel)
        traverse_ touchEntity (targetTypes schema (relTarget rel))
        touchPolicy (fromMaybe Public (relPolicy rel))


resolveEdge ::
  Schema ->
  TypeName ->
  EntityDef ->
  Level ->
  DepthTies ->
  SelectionSet ->
  Field ->
  RelationshipDef ->
  PlanM PlanEdge
resolveEdge schema t ent lvl ties enclosing f rel = do
  args <- bindArgs (fArgs f)
  traverse_ touchPolicy (relPolicy rel)
  let elvl = joinLevel lvl (maybe LPublic policyLevel (relPolicy rel))
      key = fieldKey (fName f) (fArgs f)
  case fDepth f of
    Nothing -> do
      sels <- case fSelection f of
        Just s -> pure s
        Nothing -> rejectM ["edge requires a selection set (or @depth): " <> unFieldName (fName f)]
      sub <- resolveTyped schema elvl (relTarget rel) sels
      pure
        PlanEdge
          { peField = fName f
          , peRel = rel
          , peArgs = args
          , peKey = key
          , peLevel = elvl
          , peDepth = Nothing
          , peSelection = sub
          }
    Just n -> do
      when (isJust (fSelection f)) $
        rejectM ["@depth cannot combine with a selection set: " <> unFieldName (fName f)]
      when (n < 1) $
        rejectM ["@depth requires a positive level count: " <> unFieldName (fName f)]
      case relTarget rel of
        TargetEntity t' | t' == t -> pure ()
        _ ->
          rejectM
            [ "@depth requires a self-targeting edge (the target of "
                <> unFieldName (fName f)
                <> " must be "
                <> unTypeName t
                <> ")"
            ]
      node <- case lookup (t, fName f, elvl) ties of
        Just tied -> pure tied
        Nothing -> resolveDepthNode schema t ent elvl ties (fName f) enclosing
      pure
        PlanEdge
          { peField = fName f
          , peRel = rel
          , peArgs = args
          , peKey = key
          , peLevel = elvl
          , peDepth = Just n
          , -- Built with the LAZY singleton: the tied node may still be
            -- under construction, and the strict field on 'peSelection'
            -- must not force it (see the module notes).
            peSelection = TypedSelection (MapL.singleton t node)
          }


{- | Resolve one level of a @\@depth@ expansion: the enclosing selection set
at the edge's membership level. That level is a join fixed point, so the
recursive occurrence inside resolves against the same node — the tie is
threaded lazily and closes the knot without forcing it.
-}
resolveDepthNode ::
  Schema -> TypeName -> EntityDef -> Level -> DepthTies -> FieldName -> SelectionSet -> PlanM NodeSelection
resolveDepthNode schema t ent elvl ties fname enclosing = PlanM $ \st ->
  let res = runPlanM (resolveNode schema t ent elvl ((key, node) : ties) enclosing) st
      key = (t, fname, elvl)
      node = case res of
        Right (n, _) -> n
        -- Never inspected: a Left aborts the whole plan before anything
        -- can look through the tie.
        Left _ -> NodeSelection [] []
  in res


-- ---------------------------------------------------------------------------
-- Argument binding and canonical field keys
-- ---------------------------------------------------------------------------

bindArgs :: [Argument] -> PlanM [(ArgName, BoundArg)]
bindArgs args = traverse bindOne (sortOn argName args)
  where
    bindOne (Argument n v) = case v of
      QVar var -> pure (n, ArgVar var)
      _ -> case qvalueToJson v of
        Just j -> pure (n, ArgLit j)
        Nothing ->
          rejectM
            [ "a variable inside a list literal is unsupported in argument position: "
                <> unArgName n
            ]


{- | The canonical wire field key (§4.1): literal-only argument lists render
through 'canonicalFieldKey'; variable-bearing ones render the same shape
with @$name@ placeholders, re-rendered by the executor after binding.
-}
fieldKey :: FieldName -> [Argument] -> Text
fieldKey n args =
  let sorted = sortOn argName args
      literal = traverse (\(Argument a v) -> (,) a <$> qvalueToJson v) sorted
  in case literal of
      Just pairs -> canonicalFieldKey n pairs
      Nothing ->
        unFieldName n
          <> "("
          <> T.intercalate "," (map renderOne sorted)
          <> ")"
  where
    renderOne (Argument a v) = unArgName a <> ":" <> renderKeyVal v


-- | Canonical query-literal rendering with @$name@ for variables.
renderKeyVal :: QValue -> Text
renderKeyVal = \case
  QVar v -> "$" <> unVarName v
  QList vs -> "[" <> T.intercalate "," (map renderKeyVal vs) <> "]"
  QInt i -> canonicalJsonText (A.Number (fromInteger i))
  QNum s -> canonicalJsonText (A.Number s)
  QString s -> canonicalJsonText (A.String s)
  QBool b -> canonicalJsonText (A.Bool b)
  QEnum e -> canonicalJsonText (A.String e)


-- ---------------------------------------------------------------------------
-- Slices (§8.1)
-- ---------------------------------------------------------------------------

{- | The per-slice partition: a slice exists exactly when some level in the
plan (root membership, edge membership, or field emission) lands in it;
the ctx slice's claim dependency is the union of every @Claims(S)@ level
in the plan; a slice's roots are those whose membership level lands in it.

@memberships@ overrides a root's membership levels (default
@[prLevel]@): the implicit @nodes@ root's membership is per type
('nodesMembership'), so it may land in several slices at once, and its
fetch-by claim gates join the ctx slice's claim dependency like any root
policy would.
-}
deriveSlices :: Budgets -> Map RootName [Level] -> Map RootName PlanRoot -> Map SliceName SliceInfo
deriveSlices budgets memberships roots =
  Map.fromList (map (\s -> (s, info s)) (Set.toList present))
  where
    fuel = fromIntegral (maxDepth budgets) + 2 :: Int
    membershipOf rn pr = Map.findWithDefault [prLevel pr] rn memberships
    allLevels =
      concatMap
        (\(rn, pr) -> membershipOf rn pr <> typedLevels fuel (prSelection pr))
        (Map.toList roots)
    present = Set.fromList (map sliceOfLevel allLevels)
    ctxClaims = Set.toAscList (Set.fromList (concatMap claimsOf allLevels))
    claimsOf = \case
      LClaims cs -> cs
      LPublic -> []
      LPrivate -> []
    rootsIn s =
      map fst (filter (\(rn, pr) -> s `elem` map sliceOfLevel (membershipOf rn pr)) (Map.toList roots))
    info s =
      SliceInfo
        { siClaims = if s == SliceCtx then ctxClaims else []
        , siRoots = rootsIn s
        }


-- Fuel bounds the traversal through @depth knots; levels are join fixed
-- points there, so the bounded walk still sees every distinct level.
typedLevels :: Int -> TypedSelection -> [Level]
typedLevels fuel (TypedSelection m)
  | fuel <= 0 = []
  | otherwise = concatMap (nodeLevels fuel) (Map.elems m)


nodeLevels :: Int -> NodeSelection -> [Level]
nodeLevels fuel (NodeSelection fs es) =
  map pfLevel fs
    <> concatMap (\e -> peLevel e : typedLevels (fuel - 1) (peSelection e)) es



-- ---------------------------------------------------------------------------
-- Load projections
-- ---------------------------------------------------------------------------

{- | Per-type load projections: for every entity type the plan can load,
the static upper bound on the stored fields the executor reads off that
type's rows ("Lattice.Backend"'s /Projections/ contract). This is the
introspection surface for "what data is this query asking for" — a SQL
backend renders it as its @SELECT@ column lists.

Included per type, mirroring the executor's row reads exactly:

* selected stored and @maintained@ fields ('Lattice.Server.Execute.emitField');
* 'RhsField' names of every policy evaluated against that type's rows —
  field emission policies, edge policies (parent row), and the @nodes@
  root's @fetch by@ row gates;
* @has one@ link fields ('relByField') of traversed edges, including the
  hidden links of @on read@ 'ViaEdge' deps;
* grouped-by override fields of scanned collections (surrogate-key
  derivation reads them off the parent row; the link field itself reads
  the parent /key/, not the row);
* @on read@ read sets: @own(…)@ fields on the owner, the dep fragment's
  top-level fields on each 'ViaEdge' target type, and 'ViaCollection'
  grouping overrides on the owner.

A selected field with declared arguments widens its type to 'ProjectAll':
'Lattice.Backend.beComputed' receives the whole row and declares no read
set. Types the plan never loads are absent; look up with a 'ProjectAll'
default.

\@depth\@ knots are cycle-broken per path (the tied expansion repeats the
enclosing selection, so one pass per @(type, edge)@ contributes every
field name; levels differ across tie instances but names do not).
-}
planProjections :: Schema -> Plan -> Map TypeName Projection
planProjections schema plan =
  Map.unionsWith (<>) (map rootProjections (Map.toList (planRoots plan)))
  where
    rootProjections (_, pr) =
      Map.unionsWith
        (<>)
        ( fetchByGates pr
            : map
              (\(t, node) -> projNode Set.empty t node)
              (Map.toList (tsPerType (prSelection pr)))
        )

    -- §14.4: a nodes job's fetch-by gate is evaluated against the loaded
    -- row when it carries 'RhsField' predicates.
    fetchByGates pr
      | isNodesRootDef (prDef pr) =
          Map.fromListWith
            (<>)
            ( map
                (\t -> (t, fieldsProj (maybe [] policyRowFields (entityFetchBy =<< lookupEntity schema t))))
                (Map.keys (tsPerType (prSelection pr)))
            )
      | otherwise = Map.empty

    projNode :: Set (TypeName, FieldName) -> TypeName -> NodeSelection -> Map TypeName Projection
    projNode seen t node = case lookupEntity schema t of
      Nothing -> Map.empty
      Just ent ->
        Map.unionsWith
          (<>)
          ( map (projField t ent) (nsFields node)
              <> map (projEdge seen t) (nsEdges node)
          )

    projField t ent pf = case lookupEntityField ent (pfName pf) of
      Nothing -> Map.empty
      Just fd ->
        let pol = here (policyRowFields (entityFieldPolicy ent fd))
            value = case fieldDerivation fd of
              Just d
                | OnRead <- derivMaterialize d ->
                    Map.unionsWith (<>) (map (projDep t ent) (NE.toList (derivReads d)))
              -- Maintained values read from the row like stored fields.
              _
                | null (fieldArgs fd) -> here [pfName pf]
                -- beComputed receives the whole row; no declared read set.
                | otherwise -> Map.singleton t ProjectAll
        in Map.unionWith (<>) pol value
      where
        here = Map.singleton t . fieldsProj

    projDep t ent = \case
      OwnFields ns -> Map.singleton t (fieldsProj (NE.toList ns))
      ViaEdge e frag -> case lookupEntityRel ent e of
        Just rel@ToOne {} ->
          Map.unionsWith
            (<>)
            ( Map.singleton t (fieldsProj [relByField rel])
                : map
                  (\tt -> Map.singleton tt (fieldsProj (fragmentTopFields frag)))
                  (targetTypes schema (relTarget rel))
            )
        _ -> Map.empty
      ViaCollection rf _ -> case lookupEntityRel ent rf of
        Just ToMany {relCollection = col} -> Map.singleton t (groupingProj col)
        _ -> Map.empty

    projEdge seen t pe =
      let parent =
            Map.singleton t . fieldsProj $
              policyRowFields (fromMaybe Public (relPolicy (peRel pe)))
                <> case peRel pe of
                  ToOne {relByField = byF} -> [byF]
                  ToMany {} -> []
          grouping = case peRel pe of
            ToOne {} -> Map.empty
            ToMany {relCollection = col} -> Map.singleton t (groupingProj col)
          -- A depth edge's expansion is cyclically tied; one pass per
          -- (enclosing type, edge) along a path sees every field name.
          recurse
            | isJust (peDepth pe) && Set.member (t, peField pe) seen = Map.empty
            | otherwise =
                let seen' = if isJust (peDepth pe) then Set.insert (t, peField pe) seen else seen
                in Map.unionsWith
                    (<>)
                    (map (\(tt, node) -> projNode seen' tt node) (Map.toList (tsPerType (peSelection pe))))
      in Map.unionsWith (<>) [parent, grouping, recurse]

    -- Surrogate-key grouping values read off the parent row for grouped-by
    -- overrides; the link field reads the parent's key component instead.
    groupingProj col = fieldsProj (NE.filter (/= colLink col) (colGrouping col))

    fragmentTopFields frag = case Map.lookup frag (schemaFragments schema) of
      Nothing -> []
      Just fdef -> map fName (selectionFields (fragSelection fdef))

    fieldsProj = ProjectFields . Set.fromList


-- | The 'RhsField' names a policy's predicates compare against.
policyRowFields :: Policy -> [FieldName]
policyRowFields = \case
  RequiresClaims preds -> mapMaybe rhsField preds
  Public -> []
  Private -> []
  where
    rhsField p = case cpRhs p of
      RhsField f -> Just f
      _ -> Nothing

-- ---------------------------------------------------------------------------
-- Budgets (§14.1): static depth, rounds, per-round fan-out
-- ---------------------------------------------------------------------------

{- | Static traversal depth: selection nesting with @\@depth(n)@ counting its
full @n@-level expansion. Computed arithmetically — depth edges are never
traversed (their expansion repeats the enclosing set, so it contributes
@n@ extra levels on top of the node's own base depth).
-}
plansDepth :: Map RootName PlanRoot -> Natural
plansDepth m = maximum0 (map (typedDepth . prSelection) (Map.elems m))


typedDepth :: TypedSelection -> Natural
typedDepth (TypedSelection m) = maximum0 (map nodeDepth (Map.elems m))


{- | A node's depth: its own level plus its deepest child. A field with an
@on read@ derivation whose read set leaves the row (§3.7 Planning) is a
hidden one-level traversal and counts like a leaf child.
-}
nodeDepth :: NodeSelection -> Natural
nodeDepth (NodeSelection fs es) =
  let (depthEdges, normalEdges) = partition (isJust . peDepth) es
      dHidden = if any hiddenDerivedField fs then 1 else 0
      dBase = 1 + maximum0 (dHidden : map (typedDepth . peSelection) normalEdges)
      dRec = maximum0 (mapMaybe (fmap fromIntegral . peDepth) depthEdges)
  in dBase + dRec


-- | Does this plan field resolve through hidden traversals (§3.7)?
hiddenDerivedField :: PlanField -> Bool
hiddenDerivedField pf = case pfDerivation pf of
  Just d -> derivMaterialize d == OnRead && derivationHidden d
  Nothing -> False


maximum0 :: [Natural] -> Natural
maximum0 = foldl' max 0


-- | One loader invocation bound within a round.
data LoaderInfo = LoaderInfo
  { liLoader :: Text
  -- ^ @Target via Type.edge@ / @Target via root@.
  , liCollection :: Maybe CollectionDef
  , liFanout :: Natural
  }


{- | The per-round loader bounds: round 0 scans the roots (sum of page
sizes), each deeper round multiplies by its collection's bound (§14.1).
Interface alternatives count fully (conservative sum across concrete
types). A @\@depth(n)@ edge contributes a geometric series over its @n@
levels, each level also repeating the enclosing set's non-recursive edges;
recursive occurrences inside the expansion are counted by that series and
never traversed structurally. An @on read@ derived field's hidden
traversals (§3.7 Planning) count fully: each 'ViaEdge' dep is one loader
at the parents' fan-out, each 'ViaCollection' dep one aggregate loader at
the same fan-out (set-in map-out, one value per parent), both in the same
round as the node's edge loads.
-}
planLoaderRounds :: Map RootName PlanRoot -> [[LoaderInfo]]
planLoaderRounds roots = mergeRounds (map rootRounds (Map.toList roots))
  where
    rootRounds (rname, pr) =
      let rd = prDef pr
          b0 = rootFanout rd (prArgs pr)
          l0 =
            LoaderInfo
              { liLoader = targetLabel (rootTarget rd) <> " via " <> unRootName rname
              , liCollection = rootCollection rd
              , liFanout = b0
              }
      in [l0] : typedRounds b0 (prSelection pr)

    typedRounds cnt (TypedSelection m) =
      mergeRounds (map (\(t, node) -> nodeRounds cnt t node) (Map.toList m))

    nodeRounds cnt t node =
      mergeRounds (derivedRounds cnt t node : map (edgeRounds cnt t) (nsEdges node))

    baseNodeRounds cnt t node =
      mergeRounds
        ( derivedRounds cnt t node
            : map (edgeRounds cnt t) (filter (isNothing . peDepth) (nsEdges node))
        )

    derivedRounds cnt t node =
      case concatMap (derivedLoaders cnt t) (nsFields node) of
        [] -> []
        ls -> [ls]

    derivedLoaders cnt t pf = case pfDerivation pf of
      Just d
        | OnRead <- derivMaterialize d ->
            mapMaybe (depLoader cnt t pf) (NE.toList (derivReads d))
      _ -> []

    depLoader cnt t pf dep =
      let label via =
            unTypeName t
              <> "."
              <> unFieldName (pfName pf)
              <> " derived via "
              <> unFieldName via
      in case dep of
          OwnFields _ -> Nothing
          ViaEdge e _ ->
            Just LoaderInfo {liLoader = label e, liCollection = Nothing, liFanout = cnt}
          ViaCollection r _ ->
            Just LoaderInfo {liLoader = label r, liCollection = Nothing, liFanout = cnt}

    edgeRounds cnt t e =
      let b = edgeBound e
      in case peDepth e of
          Nothing ->
            let c = cnt * b
            in [loaderFor t e c] : typedRounds c (peSelection e)
          Just n ->
            let node = depthNodeOf t e
                level i =
                  let c = cnt * b ^ i
                  in shiftRounds (i - 1) ([loaderFor t e c] : baseNodeRounds c t node)
            in mergeRounds (map level [1 .. n])

    loaderFor t e c =
      LoaderInfo
        { liLoader = edgeLabel t e
        , liCollection = edgeCollection e
        , liFanout = c
        }


depthNodeOf :: TypeName -> PlanEdge -> NodeSelection
depthNodeOf t e = fromMaybe (NodeSelection [] []) (Map.lookup t (tsPerType (peSelection e)))


mergeRounds :: [[[LoaderInfo]]] -> [[LoaderInfo]]
mergeRounds = foldr zipMerge []
  where
    zipMerge [] ys = ys
    zipMerge xs [] = xs
    zipMerge (x : xs) (y : ys) = (x <> y) : zipMerge xs ys


shiftRounds :: Int -> [[LoaderInfo]] -> [[LoaderInfo]]
shiftRounds k rs = replicate k [] <> rs


edgeBound :: PlanEdge -> Natural
edgeBound e = case peRel e of
  ToOne {} -> 1
  ToMany {relCollection = col} -> windowBound (colWindow col) (peArgs e)


edgeCollection :: PlanEdge -> Maybe CollectionDef
edgeCollection e = case peRel e of
  ToOne {} -> Nothing
  ToMany {relCollection = col} -> Just col


{- | A root's static round-0 fan-out. The implicit @nodes@ root's is the
literal @refs@ list's length when inline — over 'maxRoundFanout' this
rejects at compile time through 'checkFanout', the ordinary budget
rejection — and @1@ when variable-bound (the executor rejects an
over-budget list at binding time; deeper-round static bounds under this
approximation are a documented concession, the runtime fan-out guard being
authoritative).
-}
rootFanout :: RootDef -> [(ArgName, BoundArg)] -> Natural
rootFanout rd args
  | isNodesRootDef rd = case lookup "refs" args of
      Just (ArgLit (A.Array xs)) -> fromIntegral (length xs)
      _ -> 1
  | otherwise = case (rootKind rd, rootCollection rd) of
      (RootGet, _) -> 1
      (RootList, Just col) -> windowBound (colWindow col) args
      (RootList, Nothing) -> 1


{- | A collection's static cardinality bound: a bounded collection's @max@; a
paginated one's literal @first@/@last@ (clamped to @maxPage@), its
@defaultPage@ when traversed bare, or @maxPage@ when the page size is
variable-bound (§14.1).
-}
windowBound :: Windowing -> [(ArgName, BoundArg)] -> Natural
windowBound w args = case w of
  Bounded _ n _ -> n
  Paginated cs ->
    let cap = csMaxPage cs
    in case lookup "first" args <|> lookup "last" args of
        Just (ArgLit v) -> maybe cap (min cap) (naturalFromJson v)
        Just (ArgVar _) -> cap
        Nothing -> min cap (fromMaybe cap (csDefaultPage cs))


naturalFromJson :: A.Value -> Maybe Natural
naturalFromJson = \case
  A.Number n -> case Sci.floatingOrInteger n :: Either Double Integer of
    Right i | i >= 0 -> Just (fromInteger i)
    _ -> Nothing
  _ -> Nothing


checkFanout :: Budgets -> [[LoaderInfo]] -> Either CompileError ()
checkFanout budgets rounds = traverse_ check (zip [0 :: Int ..] rounds)
  where
    check (i, ls) =
      let total = sum (map liFanout ls)
      in unless (total <= maxRoundFanout budgets) $
          Left
            ( compileRejected
                [ "round "
                    <> tshow i
                    <> " has a fan-out bound of "
                    <> tshow total
                    <> ", exceeding the origin's maxRoundFanout budget "
                    <> tshow (maxRoundFanout budgets)
                ]
            )


targetLabel :: Target -> Text
targetLabel = \case
  TargetEntity t -> unTypeName t
  TargetInterface i -> unInterfaceName i
  TargetUnion ts -> T.intercalate "|" (map unTypeName (NE.toList ts))


edgeLabel :: TypeName -> PlanEdge -> Text
edgeLabel t e =
  targetLabel (relTarget (peRel e))
    <> " via "
    <> unTypeName t
    <> "."
    <> unFieldName (peField e)


-- ---------------------------------------------------------------------------
-- Pertinent declarations and plan identity (§7.3)
-- ---------------------------------------------------------------------------

{- | Render the pertinent-declaration set: whole declarations of every
touched entity, root, schema fragment, and interface, plus the closure of
named type declarations reachable from touched entities' fields, root and
fragment parameters, and referenced claims — each in canonical IDL form,
sorted by a @kind name@ key.

Touching a co-keyed entity (§3.8) makes its base pertinent too: the key
spec is inherited, so a base change must move every referencing plan id.
The reverse direction stays open — adding a refinement is additive and
leaves plans over the base untouched (§17.2).
-}
renderPertinent :: Schema -> Touched -> Text
renderPertinent schema Touched {..} =
  T.intercalate "\n" (map snd (sortOn fst entries))
  where
    entries =
      mapMaybe entityEntry (Set.toList touchedEntities)
        <> mapMaybe rootEntry (Set.toList tRoots)
        <> mapMaybe fragmentEntry (Set.toList tFragments)
        <> mapMaybe interfaceEntry (Set.toList tInterfaces)
        <> mapMaybe typeEntry (Set.toList pertinentTypes)
    entityEntry t =
      fmap
        (\d -> ("entity " <> unTypeName t, Print.printEntity t d))
        (Map.lookup t (schemaEntities schema))
    rootEntry r =
      fmap
        (\d -> ("root " <> unRootName r, Print.printRoot r d))
        (Map.lookup r (schemaRoots schema))
    fragmentEntry f =
      fmap
        (\d -> ("fragment " <> unFragmentName f, Print.printFragment f d))
        (Map.lookup f (schemaFragments schema))
    interfaceEntry i =
      fmap
        (\d -> ("interface " <> unInterfaceName i, Print.printInterface i d))
        (Map.lookup i (schemaInterfaces schema))
    typeEntry t =
      fmap
        (\d -> ("type " <> unTypeName t, Print.printTypeDecl t d))
        (Map.lookup t (schemaTypes schema))
    -- Touched entities plus the co-key base of each (§3.8).
    touchedEntities = Set.foldr addBase tEntities tEntities
    addBase t acc = case Map.lookup t (schemaEntities schema) of
      Just e | Just ck <- entityCoKey e -> Set.insert (ckBase ck) acc
      _ -> acc
    pertinentTypes = closeTypes schema seedTypes
    seedTypes =
      Set.unions
        [ Set.unions (map claimSeed (Set.toList tClaims))
        , Set.unions (map entitySeed (Set.toList touchedEntities))
        , Set.unions (map rootSeed (Set.toList tRoots))
        , Set.unions (map fragSeed (Set.toList tFragments))
        ]
    claimSeed c = maybe Set.empty namedIn (Map.lookup c (schemaClaims schema))
    entitySeed t = case Map.lookup t (schemaEntities schema) of
      Nothing -> Set.empty
      Just e -> Set.unions (map fieldDefSeed (Map.elems (entityFields e)))
    fieldDefSeed fd =
      namedIn (fieldType fd) <> Set.unions (map (namedIn . adType) (fieldArgs fd))
    rootSeed r = case Map.lookup r (schemaRoots schema) of
      Nothing -> Set.empty
      Just rd -> Set.unions (map (namedIn . adType) (rootParams rd))
    fragSeed f = case Map.lookup f (schemaFragments schema) of
      Nothing -> Set.empty
      Just fd -> Set.fromList (map (TypeName . trName . vdType) (fragParams fd))


-- | Named type declarations transitively reachable through 'schemaTypes'.
closeTypes :: Schema -> Set TypeName -> Set TypeName
closeTypes schema = go Set.empty . Set.toList
  where
    go acc [] = acc
    go acc (t : ts)
      | Set.member t acc = go acc ts
      | otherwise = case Map.lookup t (schemaTypes schema) of
          Nothing -> go acc ts
          Just d -> go (Set.insert t acc) (Set.toList (declNamed d) <> ts)
    declNamed = \case
      DeclNewtype ft _ -> namedIn ft
      DeclRecord fs -> Set.unions (map (namedIn . snd) fs)
      DeclSum _ cs -> Set.unions (map ctorNamed (NE.toList cs))
      DeclEnum _ _ -> Set.empty
    ctorNamed c = Set.unions (map (namedIn . snd) (ctorFields c))


namedIn :: FieldType -> Set TypeName
namedIn = \case
  TPrim _ -> Set.empty
  TNamed t -> Set.singleton t
  TOptional f -> namedIn f
  TList f -> namedIn f
  TList1 f -> namedIn f
  TSet f -> namedIn f
  TMap k v -> namedIn k <> namedIn v
  TVec _ f -> namedIn f


-- ---------------------------------------------------------------------------
-- Explain (§20.2)
-- ---------------------------------------------------------------------------

{- | One plan element in the explain document: path, concrete type (absent
for roots), join derivation, and joined level.
-}
data Element = Element Text (Maybe TypeName) Text Level


{- | One static traversal job for skeleton derivation, mirroring the
executor's per-entity work unit: concrete type, fan-out bound, node
selection, and remaining @\@depth@ fuel.
-}
data SkelJob = SkelJob TypeName Natural NodeSelection (Map FieldName Int)


-- | The @explain@ document (§20.2): path joins, slices, rounds, keys, budgets.
explainJson :: Schema -> Budgets -> Plan -> A.Value
explainJson schema budgets plan =
  A.object
    [ "plan" .= planId plan
    , "query" .= planQueryHash plan
    , "elements" .= map elementJson elements
    , "rounds" .= map roundJson indexedRounds
    , "surrogateKeys" .= map keyJson surrogates
    , "budgets" .= budgetsJson
    , "slices" .= slicesJson
    , "spans" .= skeleton
    , "projections" .= projectionsJson
    ]
  where
    rounds = planLoaderRounds (planRoots plan)
    indexedRounds = zip [0 :: Int ..] rounds

    -- Loader projections (§20.2): what each beLoad round may read per type.
    projectionsJson =
      A.object
        ( map
            ( \(t, proj) ->
                AK.fromText (unTypeName t) .= case proj of
                  ProjectAll -> A.String "*"
                  ProjectFields fs -> A.toJSON (map unFieldName (Set.toAscList fs))
            )
            (Map.toAscList (planProjections schema plan))
        )

    -- Elements: per root, the join derivation of every field and edge.
    elements = concatMap rootElements (Map.toList (planRoots plan))
    rootElements (rname, pr) =
      let seg = unRootName rname
          chain = seg <> "@root:" <> renderLevel (prLevel pr)
          rootEl =
            Element seg Nothing (chain <> " = " <> renderLevel (prLevel pr)) (prLevel pr)
      in rootEl : typedElements Set.empty seg chain (prSelection pr)
    typedElements visited path chain (TypedSelection m) =
      concatMap (\(t, node) -> nodeElements visited t path chain node) (Map.toList m)
    nodeElements visited t path chain node =
      map (fieldElement t path chain) (nsFields node)
        <> concatMap (edgeElements visited t path chain) (nsEdges node)
    fieldElement t path chain f =
      let pol = fieldPolicyOf t (pfName f)
          deriv =
            chain
              <> " ⊔ "
              <> unFieldName (pfName f)
              <> ":"
              <> renderLevel (policyLevel pol)
              <> " = "
              <> renderLevel (pfLevel f)
      in Element (path <> "." <> pfKey f) (Just t) deriv (pfLevel f)
    edgeElements visited t path chain e =
      let polLvl = maybe LPublic policyLevel (relPolicy (peRel e))
          chain' = chain <> " ⊔ " <> unFieldName (peField e) <> ":" <> renderLevel polLvl
          path' = path <> "." <> peKey e
          el = Element path' (Just t) (chain' <> " = " <> renderLevel (peLevel e)) (peLevel e)
          descend = case peDepth e of
            Nothing -> typedElements visited path' chain' (peSelection e)
            Just _
              -- One level of a @depth expansion is fully informative (its
              -- level is a join fixed point); the visited set cuts the knot.
              | Set.member (t, peField e) visited -> []
              | otherwise ->
                  typedElements (Set.insert (t, peField e) visited) path' chain' (peSelection e)
      in el : descend
    fieldPolicyOf t n = fromMaybe Public $ do
      e <- lookupEntity schema t
      fd <- lookupEntityField e n
      pure (entityFieldPolicy e fd)
    elementJson (Element p mt d lv) =
      A.object
        ( [ "path" .= p
          , "derivation" .= d
          , "slice" .= renderSlice (sliceOfLevel lv)
          ]
            <> maybe [] (\t -> ["type" .= unTypeName t]) mt
        )

    -- Rounds and loaders.
    roundJson (i, ls) = A.object ["round" .= i, "loaders" .= map loaderJson ls]
    loaderJson l =
      A.object
        ( ["loader" .= liLoader l, "fanout" .= liFanout l]
            <> maybe [] collectionPairs (liCollection l)
        )
    collectionPairs col =
      [ "collection" .= unCollectionName (colName col)
      , "grouping" .= map unFieldName (NE.toList (colGrouping col))
      ]

    -- Surrogate keys: the collections scanned, with their grouping keys.
    surrogates =
      Set.toAscList
        ( Set.fromList
            ( map
                (\c -> (unCollectionName (colName c), map unFieldName (NE.toList (colGrouping c))))
                (mapMaybe liCollection (concat rounds))
            )
        )
    keyJson (n, g) = A.object ["collection" .= n, "grouping" .= g]

    -- Budget consumption against the origin's published limits.
    budgetsJson =
      A.object
        [ "roots" .= usedLimit (fromIntegral (Map.size (planRoots plan))) (maxRoots budgets)
        , "depth" .= usedLimit (plansDepth (planRoots plan)) (maxDepth budgets)
        , "rounds" .= usedLimit (fromIntegral (length rounds)) (maxRounds budgets)
        , "roundFanout" .= usedLimit peakFanout (maxRoundFanout budgets)
        ]
    peakFanout = maximum0 (map (sum . map liFanout) rounds)
    usedLimit u l = A.object ["used" .= (u :: Natural), "limit" .= l]

    -- The slice partition, in the wire plan record's shape.
    slicesJson = A.object (map sliceEntry [SlicePub, SliceCtx, SlicePriv])
    sliceEntry s =
      AK.fromText (renderSlice s)
        .= maybe (A.Bool False) sliceInfoJson (Map.lookup s (planSlices plan))
    sliceInfoJson (SliceInfo cs rs) =
      A.object ["claims" .= map unClaimName cs, "roots" .= map unRootName rs]

    -- The expected span skeleton (§19.2): the @lattice.execute@ tree a
    -- live trace of this plan must match, derived by replaying the
    -- executor's round structure statically. Round k opens one entity
    -- load per concrete type its jobs reach (bare type name, ascending —
    -- 'loadRound' order) followed by one children fetch per @has many@
    -- edge occurrence (@Type.field@, ascending task key — 'runEdgeTask'
    -- order); fetched children and to-one targets (required ones probed
    -- even without a node selection) become round k+1's jobs. Root
    -- resolution and hidden derived-field loads open no spans. Loader
    -- names are attributes, never span names; @batch@ carries the static
    -- fan-out bound (runtime batch sizes are data facts, at most the
    -- bound).
    skeleton =
      [ A.object
          [ "name" .= ("lattice.execute" :: Text)
          , "children" .= zipWith spanRound [0 :: Int ..] (spanRounds initialJobs)
          ]
      ]
    initialJobs =
      [ SkelJob t (rootFanout (prDef pr) (prArgs pr)) node Map.empty
      | pr <- Map.elems (planRoots plan)
      , (t, node) <- Map.toAscList (tsPerType (prSelection pr))
      ]
    spanRounds [] = []
    spanRounds jobs =
      let loads = Map.fromListWith (+) [(t, c) | SkelJob t c _ _ <- jobs]
          steps = concatMap jobSteps jobs
          fetches = Map.fromListWith (+) (concatMap fst steps)
          kids = concatMap snd steps
      in (loads, fetches) : spanRounds kids
    jobSteps (SkelJob t cnt node fuel) = mapMaybe (skelEdge t cnt fuel) (nsEdges node)
    skelEdge t cnt fuel pe
      | Just n <- peDepth pe
      , Map.findWithDefault n (peField pe) fuel <= 0 =
          Nothing
      | otherwise =
          let fuel' = case peDepth pe of
                Nothing -> Map.empty
                Just n -> Map.insert (peField pe) (Map.findWithDefault n (peField pe) fuel - 1) fuel
              selTypes = tsPerType (peSelection pe)
              selJobs c = [SkelJob t' c node' fuel' | (t', node') <- Map.toAscList selTypes]
          in Just $ case peRel pe of
              ToOne {relTarget = tgt, relOptional = opt} ->
                ( []
                , selJobs cnt
                    <> [ SkelJob t' cnt (NodeSelection [] []) fuel'
                       | not opt
                       , t' <- targetTypes schema tgt
                       , not (Map.member t' selTypes)
                       ]
                )
              ToMany {} ->
                ([((t, peField pe, peKey pe), cnt)], selJobs (cnt * edgeBound pe))
    spanRound i (loads, fetches) =
      A.object
        [ "name" .= ("lattice.round[" <> tshow i <> "]")
        , "children"
            .= ( map (\(t, c) -> loadSpan (unTypeName t) c) (Map.toAscList loads)
                  <> map
                    (\((t, f, _), c) -> loadSpan (unTypeName t <> "." <> unFieldName f) c)
                    (Map.toAscList fetches)
               )
        ]
    loadSpan loader c =
      A.object
        [ "name" .= ("lattice.load" :: Text)
        , "loader" .= (loader :: Text)
        , "batch" .= (c :: Natural)
        ]


renderLevel :: Level -> Text
renderLevel = \case
  LPublic -> "Public"
  LClaims cs -> "Claims{" <> T.intercalate "," (map unClaimName cs) <> "}"
  LPrivate -> "Private"


tshow :: Show a => a -> Text
tshow = T.pack . show
