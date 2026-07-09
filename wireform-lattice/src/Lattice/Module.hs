{- | Schema modules and fusion (spec §18.1, §18.2): the topology-independent
composition algebra. Each module is a complete IDL document; fusion folds
@extend entity@ blocks into their owning entity's declaration, unions the
top-level namespaces, and yields ONE elaborated schema whose canonical text
is content-addressed like any schema.

== Determinism

'fuseModules' is order-insensitive: modules are sorted by name before
folding, and the semantic model's sorted maps do the rest, so any
permutation of the input yields a byte-identical 'fusedIdl' (and therefore
the same schema hash).

== Conflict rules (§18.1)

Composition conflicts fail fusion, not module parse:

* one owner per type — two modules declaring the same entity conflict even
  when the declarations are identical, because ownership routes loads;
  identical value-type\/@claims@ declarations DEDUPE (shared vocabulary),
  differing ones conflict;
* extensions may add stored fields, edges, and derived fields, and may NOT
  redeclare the owner's key fields, visibility default, @fetch by@, a
  co-key, or any member that already exists anywhere;
* mutations, roots, and fragments fuse by disjoint union; interfaces
  dedupe when their declared surface is identical (member sets union).

== In-process execution ('fuseBackends')

The fused 'Backend' routes each loader to the module that owns the
declaration it serves. A type with extension fields loads the owner's row
AND each extending module's row for the same key (the extending module's
'beLoad' serves the extended type name, returning only the columns it
owns) and merges the fields into ONE entity record whose @ver@ is the
owner's. This is the pinned in-process simplification of §18.1: the
gateway emits per-module records with per-module versions, but "in-process
fusion needs not even that". Collections and aggregates route to the
module owning the collection (extension edges to the extending module);
mutations route whole by name. 'beSnapshot' composes the per-module tokens
into a namespaced vector @mod1\/tok1,mod2\/tok2@ (sorted by module name) —
the in-process analogue of §18.4's @posts\/main="…"@ domain namespaces,
with the module name standing in for the upstream domain.
-}
module Lattice.Module (
  ModuleName (..),
  SchemaModule (..),
  FusionError (..),
  Fused (..),
  fuseModules,
  fuseBackends,
) where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Backend
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Schema
import Lattice.Types


-- | The name of a schema module ('smName'), the unit of ownership.
newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


-- | One module: a name and its complete IDL document (its own @schema@
-- line included). Modules parse independently: everything a module
-- mentions must be declared in it, except the entity names its
-- @extend entity@ blocks target.
data SchemaModule = SchemaModule
  { smName :: Text
  , smIdl :: Text
  }
  deriving stock (Eq, Show)


-- | A composition conflict (§18.1). Fusion collects every error rather
-- than stopping at the first.
data FusionError
  = -- | Two modules declare the same type. Entities conflict even when
    -- identical (ownership routes loads); value types conflict only when
    -- the declarations differ.
    FETwoOwners TypeName ModuleName ModuleName
  | -- | A field or edge of one entity contributed by two different
    -- modules (two extensions colliding).
    FEMemberConflict TypeName FieldName ModuleName ModuleName
  | -- | The same claim name declared with two different types: visibility
    -- payloads must mean one thing (§18.1).
    FEClaimConflict ClaimName ModuleName ModuleName
  | -- | The same directive name declared incompatibly by two modules
    -- (§3.9): the directive registry is shared vocabulary, like claims.
    FEDirectiveConflict DirectiveName ModuleName ModuleName
  | -- | A root, mutation, fragment, or interface name declared by two
    -- modules (first field is the declaration kind).
    FENameConflict Text Text ModuleName ModuleName
  | -- | An @extend entity@ block targets a type no module owns.
    FEUnknownExtendedType ModuleName TypeName
  | -- | An extension member collides with one of the owner's key fields.
    FEExtensionRedeclaresKey ModuleName TypeName FieldName
  | -- | An extension block carries a default-visibility line.
    FEExtensionRedeclaresDefault ModuleName TypeName
  | -- | An extension block carries a @fetch by@ clause.
    FEExtensionRedeclaresFetchBy ModuleName TypeName
  | -- | An extension member collides with an existing field or edge of
    -- the owner's declaration.
    FEExtensionRedeclaresMember ModuleName TypeName FieldName
  | -- | An extension block carries a @joins@\/@refines@ clause.
    FEExtensionDeclaresCoKey ModuleName TypeName
  | -- | The module's own IDL failed to parse or elaborate.
    FEModuleParse ModuleName [SchemaError]
  | -- | Two input modules share a name.
    FEDuplicateModuleName ModuleName
  | -- | The folded schema failed fused elaboration: a whole-schema check
    -- only visible once the pieces meet (e.g. §3.7 information flow of an
    -- extension's derived field against the owner's real default policy,
    -- or a cross-module collection-name collision).
    FEFusedSchemaInvalid [SchemaError]
  deriving stock (Eq, Show)


-- | The result of a successful fusion.
data Fused = Fused
  { fusedSchema :: Schema
  -- ^ One elaborated schema, extensions folded into their owners.
  , fusedIdl :: Text
  -- ^ The canonical fused IDL: one plain schema document (no @extend@
  -- blocks — provenance lives here in 'Fused', not in the text),
  -- content-addressed like any schema.
  , fusedOwner :: Map TypeName ModuleName
  -- ^ Entity ownership: which module's backend is authoritative for a
  -- type's rows, key, and versions.
  , fusedFieldOwner :: Map (TypeName, FieldName) ModuleName
  -- ^ Extension members ONLY (fields and edges): owner-declared members
  -- default to 'fusedOwner'.
  , fusedRootOwner :: Map RootName ModuleName
  -- ^ Routing for 'beGetRoot'\/'beListRoot' (implementation surface
  -- beyond the §18 record: roots are not 'TypeName'-keyed).
  , fusedMutationOwner :: Map MutationName ModuleName
  -- ^ Routing for 'beMutate' (§18.7: a mutation routes whole).
  }
  deriving stock (Eq, Show)


-- | Fuse modules into one schema. Order-insensitive: any permutation
-- yields a byte-identical 'fusedIdl'. All conflicts are collected.
fuseModules :: NonEmpty SchemaModule -> Either [FusionError] Fused
fuseModules mods0
  | not (null dupNameErrs) = Left dupNameErrs
  | not (null parseErrs) = Left parseErrs
  | not (null conflictErrs) = Left conflictErrs
  | otherwise = case parseSchema mergedIdl of
      Left es -> Left [FEFusedSchemaInvalid es]
      Right fused ->
        Right
          Fused
            { fusedSchema = fused
            , fusedIdl = canonicalIdl fused
            , fusedOwner = Map.map fst entityOwnership
            , fusedFieldOwner = fieldOwners
            , fusedRootOwner = Map.map fst rootU
            , fusedMutationOwner = Map.map fst mutU
            }
  where
    mods = List.sortOn smName (NE.toList mods0)

    dupNameErrs =
      map (FEDuplicateModuleName . ModuleName . NE.head) $
        filter ((> 1) . length) $
          NE.group (map smName mods)

    parsedResults =
      map (\m -> (ModuleName (smName m), parseSchema (smIdl m))) mods
    parseErrs = [FEModuleParse m es | (m, Left es) <- parsedResults]
    parsed = [(m, s) | (m, Right s) <- parsedResults]

    -- ---- ownership ------------------------------------------------------
    (entityOwnership, twoOwnerErrs) =
      strictDisjoint
        FETwoOwners
        [ (t, m, ed)
        | (m, s) <- parsed
        , (t, ed) <- Map.toAscList (schemaEntities s)
        ]

    -- Value types: identical declarations dedupe (shared vocabulary);
    -- differing ones are an ownership conflict.
    (typeU, typeErrs) =
      unionDedupe
        FETwoOwners
        [(t, m, d) | (m, s) <- parsed, (t, d) <- Map.toAscList (schemaTypes s)]

    -- Interfaces: identical declared surfaces dedupe, member sets union
    -- (membership is derived from entity @implements@ clauses and
    -- re-derived by the fused elaboration anyway).
    (ifaceU, ifaceErrs) = List.foldl' step (Map.empty, []) ifaceDecls
      where
        ifaceDecls =
          [(i, m, d) | (m, s) <- parsed, (i, d) <- Map.toAscList (schemaInterfaces s)]
        step (acc, es) (i, mn, d) = case Map.lookup i acc of
          Nothing -> (Map.insert i (mn, d) acc, es)
          Just (other, d0)
            | ifaceFields d0 == ifaceFields d && ifaceRels d0 == ifaceRels d ->
                ( Map.insert
                    i
                    (other, d0 {ifaceMemberSet = ifaceMemberSet d0 `Set.union` ifaceMemberSet d})
                    acc
                , es
                )
            | otherwise ->
                (acc, es <> [FENameConflict "interface" (unInterfaceName i) other mn])

    -- Claims: identical declarations dedupe; a claim with two types is a
    -- conflict (§18.1).
    (claimU, claimErrs) =
      unionDedupe
        FEClaimConflict
        [(c, m, t) | (m, s) <- parsed, (c, t) <- Map.toAscList (schemaClaims s)]

    -- Directives (§3.9): identical declarations dedupe (shared vocabulary),
    -- differing ones conflict — like claims.
    (directiveU, directiveErrs) =
      unionDedupe
        FEDirectiveConflict
        [(n, m, d) | (m, s) <- parsed, (n, d) <- Map.toAscList (schemaDirectiveDecls s)]

    -- Fragments, roots, mutations: strict disjoint union.
    (fragU, fragErrs) =
      strictDisjoint (FENameConflict "fragment" . unFragmentName) $
        [(f, m, d) | (m, s) <- parsed, (f, d) <- Map.toAscList (schemaFragments s)]
    (rootU, rootErrs) =
      strictDisjoint (FENameConflict "root" . unRootName) $
        [(r, m, d) | (m, s) <- parsed, (r, d) <- Map.toAscList (schemaRoots s)]
    (mutU, mutErrs) =
      strictDisjoint (FENameConflict "mutation" . unMutationName) $
        [(n, m, d) | (m, s) <- parsed, (n, d) <- Map.toAscList (schemaMutations s)]

    -- ---- extensions -----------------------------------------------------
    allExtensions =
      [ (m, t, x)
      | (m, s) <- parsed
      , (t, x) <- Map.toAscList (schemaExtensions s)
      ]

    extMembers :: ExtensionDef -> [FieldName]
    extMembers x = Map.keys (extFields x) <> Map.keys (extRels x)

    (fieldOwners, extErrs) = List.foldl' step (Map.empty, []) allExtensions
      where
        step (acc, es) (m, t, x) = case Map.lookup t entityOwnership of
          Nothing -> (acc, es <> [FEUnknownExtendedType m t])
          Just (_, ownerDef) ->
            let clauseErrs =
                  concat
                    [ [FEExtensionRedeclaresDefault m t | Just _ <- [extDefaultPolicy x]]
                    , [FEExtensionRedeclaresFetchBy m t | Just _ <- [extFetchBy x]]
                    , [FEExtensionDeclaresCoKey m t | Just _ <- [extCoKey x]]
                    ]
                keySet = Set.fromList (NE.toList (entityKey ownerDef))
                (acc', memberErrs) = List.foldl' member (acc, []) (extMembers x)
                member (a, ms) f
                  | Set.member f keySet = (a, ms <> [FEExtensionRedeclaresKey m t f])
                  | Map.member f (entityFields ownerDef) || Map.member f (entityRels ownerDef) =
                      (a, ms <> [FEExtensionRedeclaresMember m t f])
                  | Just other <- Map.lookup (t, f) a =
                      (a, ms <> [FEMemberConflict t f other m])
                  | otherwise = (Map.insert (t, f) m a, ms)
             in (acc', es <> clauseErrs <> memberErrs)

    conflictErrs =
      twoOwnerErrs <> typeErrs <> ifaceErrs <> claimErrs <> fragErrs <> rootErrs <> mutErrs <> extErrs <> directiveErrs

    -- ---- folding --------------------------------------------------------
    mergedEntities = Map.mapWithKey fold entityOwnership
      where
        fold t (_, ed) =
          ed
            { entityFields = entityFields ed `Map.union` extraFields t
            , entityRels = entityRels ed `Map.union` extraRels t
            }
        extraFields t =
          Map.unions [extFields x | (_, t', x) <- allExtensions, t' == t]
        extraRels t =
          Map.unions [extRels x | (_, t', x) <- allExtensions, t' == t]

    -- Sorted, deduplicated module schema names joined with "." — a split
    -- application whose modules share one schema name fuses to that name.
    mergedName =
      T.intercalate "." (map NE.head (NE.group (List.sort (map (schemaName . snd) parsed))))

    merged =
      Schema
        { schemaName = mergedName
        , schemaClaims = Map.map snd claimU
        , schemaTypes = Map.map snd typeU
        , schemaInterfaces = Map.map snd ifaceU
        , schemaEntities = mergedEntities
        , schemaExtensions = Map.empty
        , schemaFragments = Map.map snd fragU
        , schemaRoots = Map.map snd rootU
        , schemaMutations = Map.map snd mutU
        , schemaBreaks = Map.unions (map (schemaBreaks . snd) parsed)
        , schemaDeprecations = Map.unions (map (schemaDeprecations . snd) parsed)
        , schemaDirectiveDecls = Map.map snd directiveU
        , schemaDirectives = Map.unionsWith (<>) (map (schemaDirectives . snd) parsed)
        , schemaDescriptions = Map.unions (map (schemaDescriptions . snd) parsed)
        }

    -- The fused text is printed from the fold, then RE-ELABORATED: the
    -- whole-schema checks run once over the assembled surface, and the
    -- published text is a parse∘print fixpoint by construction.
    mergedIdl = canonicalIdl merged

    -- Union keyed by name, deduping identical payloads.
    unionDedupe
      :: (Ord k, Eq v)
      => (k -> ModuleName -> ModuleName -> FusionError)
      -> [(k, ModuleName, v)]
      -> (Map k (ModuleName, v), [FusionError])
    unionDedupe conflict = List.foldl' step (Map.empty, [])
      where
        step (acc, es) (k, mn, v) = case Map.lookup k acc of
          Nothing -> (Map.insert k (mn, v) acc, es)
          Just (other, v0)
            | v0 == v -> (acc, es)
            | otherwise -> (acc, es <> [conflict k other mn])

    strictDisjoint
      :: Ord k
      => (k -> ModuleName -> ModuleName -> FusionError)
      -> [(k, ModuleName, v)]
      -> (Map k (ModuleName, v), [FusionError])
    strictDisjoint conflict = List.foldl' step (Map.empty, [])
      where
        step (acc, es) (k, mn, v) = case Map.lookup k acc of
          Nothing -> (Map.insert k (mn, v) acc, es)
          Just (other, _) -> (acc, es <> [conflict k other mn])


{- | Synthesize the fused origin's 'Backend' from the per-module backends.
Routing is by declaration ownership (see the module haddock); a module
present in the fused schema but absent from the map fails its loads with
@lattice:internal@ rather than crashing.
-}
fuseBackends :: Map ModuleName Backend -> Fused -> Backend
fuseBackends backends fused =
  Backend
    { beSnapshot = snapshot
    , beGetRoot = \r args ->
        withRoot r (\b -> beGetRoot b r args) (pure (Left (missing "root")))
    , beListRoot = \r args w ->
        withRoot r (\b -> beListRoot b r args w) (pure (Left (missing "root")))
    , beChildren = children
    , beLoad = load
    , beComputed = \t f args row ->
        withField t f (\b -> beComputed b t f args row) (pure Nothing)
    , beMutate = mutate
    , beAggregate = \c agg gks ->
        case Map.lookup c collectionOwner >>= (`Map.lookup` backends) of
          Just b -> beAggregate b c agg gks
          Nothing -> pure (Left (missing "collection"))
    , beDerive = \t f deps ->
        withField t f (\b -> beDerive b t f deps) (pure Map.empty)
    , beStoreDerived = \t f vals ->
        withField t f (\b -> beStoreDerived b t f vals) (pure Map.empty)
    }
  where
    schema = fusedSchema fused

    missing :: Text -> BackendFailure
    missing what = internalError (Just ("no fused backend owns this " <> what))

    -- §18.4's namespaced snapshot vector, in-process form: one component
    -- per module backend, sorted by module name.
    snapshot = do
      toks <-
        traverse
          (\(ModuleName m, b) -> (\t -> m <> "/" <> t) <$> beSnapshot b)
          (Map.toAscList backends)
      pure (T.intercalate "," toks)

    typeOwner t = Map.lookup t (fusedOwner fused)
    fieldOwner t f = case Map.lookup (t, f) (fusedFieldOwner fused) of
      Just m -> Just m
      Nothing -> typeOwner t

    withField t f act fallback =
      case fieldOwner t f >>= (`Map.lookup` backends) of
        Just b -> act b
        Nothing -> fallback

    withRoot r act fallback =
      case Map.lookup r (fusedRootOwner fused) >>= (`Map.lookup` backends) of
        Just b -> act b
        Nothing -> fallback

    -- Modules contributing extension fields per type, in name order.
    extendersOf :: TypeName -> [ModuleName]
    extendersOf t =
      Set.toAscList . Set.fromList $
        [ m
        | ((t', _), m) <- Map.toAscList (fusedFieldOwner fused)
        , t' == t
        , Just m /= typeOwner t
        ]

    -- Owner row + extension rows, merged into ONE record: fields union,
    -- @ver@ = owner's (the pinned in-process simplification of §18.1). An
    -- absent or tombstoned owner row IS the result — extension rows
    -- cannot resurrect an entity. Any contributing load failure fails the
    -- key: a silently partial record would be indistinguishable from a
    -- complete one.
    load t proj keys = case typeOwner t >>= (`Map.lookup` backends) of
      Nothing -> pure (Map.fromList (map (\k -> (k, Left (missing "type"))) keys))
      Just ob -> do
        base <- beLoad ob t proj keys
        extra <-
          traverse
            (\m -> traverse (\b -> beLoad b t proj keys) (Map.lookup m backends))
            (extendersOf t)
        let mergeKey k r0 = List.foldl' (mergeOne k) r0 (catMaybes extra)
            mergeOne k acc extMap = case acc of
              Right (RowFound row) -> case Map.lookup k extMap of
                Just (Right (RowFound erow)) ->
                  Right (RowFound row {rowFields = rowFields row `Map.union` rowFields erow})
                Just (Left f) -> Left f
                _ -> acc
              _ -> acc
        pure (Map.mapWithKey mergeKey base)

    children t f parents w =
      case fieldOwner t f >>= (`Map.lookup` backends) of
        Just b -> beChildren b t f parents w
        Nothing ->
          pure (Map.fromList (map (\(r, _) -> (r, Left (missing "edge"))) parents))

    mutate n claims args pre =
      case Map.lookup n (fusedMutationOwner fused) >>= (`Map.lookup` backends) of
        Just b -> beMutate b n claims args pre
        Nothing -> pure (MutationFailed (missing "mutation"))

    -- Collection ownership: entity relationships route through the field
    -- owner (extension edges to the extending module), root collections
    -- through the root owner.
    collectionOwner :: Map CollectionName ModuleName
    collectionOwner = Map.fromList (entityCols <> rootCols)
      where
        entityCols =
          [ (colName (relCollection rel), m)
          | (t, ed) <- Map.toAscList (schemaEntities schema)
          , (f, rel) <- Map.toAscList (entityRels ed)
          , ToMany {} <- [rel]
          , Just m <- [fieldOwner t f]
          ]
        rootCols =
          [ (colName col, m)
          | (r, rd) <- Map.toAscList (schemaRoots schema)
          , Just col <- [rootCollection rd]
          , Just m <- [Map.lookup r (fusedRootOwner fused)]
          ]
