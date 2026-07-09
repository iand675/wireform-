{- | The semantic schema model (spec §3.1) plus the origin budgets (§14.1).

The IDL parser ("Lattice.IDL.Parser") elaborates surface text into this
model; the canonical IDL printer ("Lattice.IDL.Print") renders it back to
the schema's published, content-addressed form. Nothing here is
surface-syntax-shaped: field order is sorted maps, sugar is gone.

Implementation-driven IDL clarifications (folded back into the spec):

* Interfaces are declared in the IDL: an @interface@ declaration names its
  common fields, and entities opt in with @implements@. The semantic
  model's @interfaces :: Map InterfaceName (Set TypeName)@ is derived.
* Roots and relationships may target an inline union @(A | B)@, an
  anonymous interface with no common fields.
* A mutation write set may name @Type(new)@: an entity of that type created
  by the effect, whose key is only known at commit time.
* Co-keyed entities (§3.8): @entityCoKey@ records a @joins@/@refines@ base;
  the inherited key spec and key 'FieldDef's are copied in at elaboration,
  so 'entityKey' and 'entityFields' are always self-contained.
* Derived fields (§3.7): 'fieldDerivation' carries the declared read set,
  materialization, and optional declassification. 'ViaCollection' names the
  owning entity's @has many@ relationship /field/ (the surface spelling);
  the spec model's 'CollectionName' is @colName . relCollection@ of that
  relationship — the field name is stored so the canonical printer
  roundtrips the surface text.
-}
module Lattice.Schema (
  Schema (..),
  Deprecation (..),
  DeclPath (..),
  DirLocation (..),
  dirLocationName,
  parseDirLocation,
  DirectiveDef (..),
  DirectiveApp (..),
  reservedDirectiveNames,
  declPathLocation,
  InterfaceDef (..),
  EntityDef (..),
  ExtensionDef (..),
  emptyExtension,
  CoKey (..),
  CoKeyMode (..),
  FieldDef (..),
  Derivation (..),
  Dep (..),
  Aggregate (..),
  Materialization (..),
  derivationHidden,
  ArgDef (..),
  RelationshipDef (..),
  Target (..),
  targetTypes,
  CollectionDef (..),
  Windowing (..),
  OverflowPolicy (..),
  CursorSpec (..),
  Direction (..),
  CountPolicy (..),
  FragmentDef (..),
  RootDef (..),
  RootKind (..),
  MutationDef (..),
  WriteScopeDecl (..),
  KeyExprD (..),
  GroupExprD (..),
  EffectClass (..),
  InvalidationSpec (..),
  BatchPolicy (..),
  Atomicity (..),
  VerbBinding (..),
  BindVerb (..),
  bindVerbName,
  Budgets (..),
  defaultBudgets,

  -- * Lookups
  descriptionOf,
  lookupEntity,
  lookupEntityField,
  lookupEntityRel,
  entityFieldPolicy,
  sharedTruthFamily,
  interfaceMembers,
  schemaFragmentsOn,
  violatesList1,
) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Lattice.Query.AST (QValue, SelectionSet, VarDef)
import Lattice.Types
import Numeric.Natural (Natural)

data Schema = Schema
  { schemaName :: Text
  , schemaClaims :: Map ClaimName FieldType
  -- ^ The closed claim registry (§3.1).
  , schemaTypes :: Map TypeName TypeDecl
  -- ^ Declared value types: newtypes, records, sums, enums.
  , schemaInterfaces :: Map InterfaceName InterfaceDef
  , schemaEntities :: Map TypeName EntityDef
  , schemaExtensions :: Map TypeName ExtensionDef
  -- ^ @extend entity@ blocks (§18.1): members this module declares on a
  -- FOREIGN entity. Empty on a fused schema — fusion ("Lattice.Module")
  -- folds them into the owning entity's declaration.
  , schemaFragments :: Map FragmentName FragmentDef
  -- ^ Schema-declared fragments (late-bound, §4.5).
  , schemaRoots :: Map RootName RootDef
  , schemaMutations :: Map MutationName MutationDef
  , schemaBreaks :: Map DeclPath Text
  -- ^ @\@break(approved: "TICKET")@ override annotations (§17.3), keyed by
  -- the declaration they are attached to. Participate in the canonical IDL
  -- text but never in plan pertinence.
  , schemaDeprecations :: Map DeclPath Deprecation
  -- ^ @\@deprecated(sunset: …, note: …)@ metadata (§17.5), on fields,
  -- relationships, roots, and mutations.
  , schemaDirectiveDecls :: Map DirectiveName DirectiveDef
  -- ^ User-declared directives (§3.9): the closed directive registry a
  -- @directive \@name(…) on …@ line contributes. Part of the published
  -- canonical IDL (so of the schema hash) but never of plan pertinence.
  , schemaDirectives :: Map DeclPath [DirectiveApp]
  -- ^ Directive /applications/ (§3.9), keyed by the declaration they are
  -- written on and held in canonical order (by name, then canonical
  -- arguments). Metadata: like descriptions, they move the schema hash
  -- but never a plan id, and are non-breaking on every §17 axis.
  , schemaDescriptions :: Map DeclPath Text
  -- ^ Documentation strings (§3.9), one per declaration or item. Same
  -- hash\/pertinence\/compatibility treatment as directive applications;
  -- surfaced by codegen and tooling (the explorer's hover + completion).
  }
  deriving stock (Eq, Show)


{- | Deprecation metadata (§17.5): the element keeps serving, the metadata
appears in the published IDL document (so codegen warns), and the checker
passes its removal once past the sunset date.
-}
data Deprecation = Deprecation
  { depSunset :: Day
  , depNote :: Text
  }
  deriving stock (Eq, Ord, Show)


{- | An annotation attachment site: which declaration an IDL annotation
(@\@break@, @\@deprecated@) is written on. 'OnSchema' (the @schema@ line)
is the override site for removals of whole top-level declarations, which
have no surviving declaration of their own in the candidate (§17.3).
-}
data DeclPath
  = OnSchema
  | OnType TypeName
  | OnInterface InterfaceName
  | OnIfaceItem InterfaceName FieldName
  | OnEntity TypeName
  | OnEntityItem TypeName FieldName
  | OnFragment FragmentName
  | OnRoot RootName
  | OnMutation MutationName
  | OnDirective DirectiveName
  -- ^ A @directive@ declaration: carries only a description (no directive
  -- targets a directive declaration), so 'declPathLocation' rejects it.
  deriving stock (Eq, Ord, Show)


{- | A place a directive may be written (§3.9), the @on@ clause of a
@directive@ declaration. Entity\/interface fields and relationships are
distinct sites ('DLField' vs 'DLRelationship'), matching the IDL's
disjoint field and edge grammar.
-}
data DirLocation
  = DLSchema
  | DLType
  | DLInterface
  | DLEntity
  | DLField
  | DLRelationship
  | DLFragment
  | DLRoot
  | DLMutation
  deriving stock (Eq, Ord, Show, Enum, Bounded)


-- | The @on@-clause spelling of a location.
dirLocationName :: DirLocation -> Text
dirLocationName = \case
  DLSchema -> "SCHEMA"
  DLType -> "TYPE"
  DLInterface -> "INTERFACE"
  DLEntity -> "ENTITY"
  DLField -> "FIELD"
  DLRelationship -> "RELATIONSHIP"
  DLFragment -> "FRAGMENT"
  DLRoot -> "ROOT"
  DLMutation -> "MUTATION"


parseDirLocation :: Text -> Maybe DirLocation
parseDirLocation = \case
  "SCHEMA" -> Just DLSchema
  "TYPE" -> Just DLType
  "INTERFACE" -> Just DLInterface
  "ENTITY" -> Just DLEntity
  "FIELD" -> Just DLField
  "RELATIONSHIP" -> Just DLRelationship
  "FRAGMENT" -> Just DLFragment
  "ROOT" -> Just DLRoot
  "MUTATION" -> Just DLMutation
  _ -> Nothing


{- | A declared directive (§3.9): its argument signature, whether it may be
applied more than once at one site, and the set of locations it targets.
Argument order is the declaration order (like a field's arguments); it is
not semantic.
-}
data DirectiveDef = DirectiveDef
  { dirArgs :: [ArgDef]
  , dirRepeatable :: Bool
  , dirLocations :: Set DirLocation
  }
  deriving stock (Eq, Show)


{- | A directive application (§3.9): the directive name and its supplied
arguments, held sorted by argument name so the canonical rendering and
equality are order-insensitive.
-}
data DirectiveApp = DirectiveApp
  { daName :: DirectiveName
  , daArgs :: [(ArgName, QValue)]
  }
  deriving stock (Eq, Show)


{- | Directive names the protocol reserves for its built-in annotations
(§17.3 @\@break@, §17.5 @\@deprecated@, §3.7 @\@declassify@, §4.2
@\@depth@): a @directive@ declaration may not redeclare one.
-}
reservedDirectiveNames :: Set Text
reservedDirectiveNames = Set.fromList ["break", "deprecated", "declassify", "depth"]


{- | The directive location a declaration site occupies, or 'Nothing' when
no directive may target it (the @directive@ declaration itself, or an item
whose owner\/kind is unresolved). Entity and interface items resolve to
'DLField' or 'DLRelationship' by looking the member up on its owner.
-}
declPathLocation :: Schema -> DeclPath -> Maybe DirLocation
declPathLocation s = \case
  OnSchema -> Just DLSchema
  OnType _ -> Just DLType
  OnInterface _ -> Just DLInterface
  OnEntity _ -> Just DLEntity
  OnFragment _ -> Just DLFragment
  OnRoot _ -> Just DLRoot
  OnMutation _ -> Just DLMutation
  OnDirective _ -> Nothing
  OnEntityItem t f -> lookupEntity s t >>= itemLoc (entityFields) (entityRels) f
  OnIfaceItem i f ->
    Map.lookup i (schemaInterfaces s) >>= itemLoc ifaceFields ifaceRels f
  where
    itemLoc fields rels f rec
      | Map.member f (fields rec) = Just DLField
      | Map.member f (rels rec) = Just DLRelationship
      | otherwise = Nothing

{- | A declared interface: its common fields (each entity implementing the
interface must declare a compatible field or relationship of that name)
and the membership set, derived from @implements@ clauses.
-}
data InterfaceDef = InterfaceDef
  { ifaceFields :: Map FieldName FieldDef
  , ifaceRels :: Map FieldName RelationshipDef
  , ifaceMemberSet :: Set TypeName
  }
  deriving stock (Eq, Show)


data EntityDef = EntityDef
  { entityKey :: NonEmpty FieldName
  , entityDefaultPolicy :: Policy
  -- ^ @visible to all by default@ / @private by default@ baseline.
  , entityFields :: Map FieldName FieldDef
  , entityRels :: Map FieldName RelationshipDef
  , entityImplements :: Set InterfaceName
  , entityFetchBy :: Maybe Policy
  -- ^ The @nodes@ / point-fetch policy; 'Nothing' forbids by-ref fetching.
  , entityCoKey :: Maybe CoKey
  -- ^ Co-keyed entity (§3.8): the base whose key this entity inherits;
  -- 'Nothing' for ordinary entities.
  }
  deriving stock (Eq, Show)


{- | An @extend entity@ block (§18.1): stored fields, edges, and derived
fields one module declares on an entity another module owns. The block may
NOT redeclare the owner's key, visibility default, @fetch by@, co-key, or
any existing member — but "Lattice.IDL.Parser" is deliberately lenient
about the first four: the spec pins composition conflicts to FAIL FUSION
at deploy time (§18.1), so the illegal clauses parse, are recorded here,
and 'Lattice.Module.fuseModules' rejects them naming the offender.
-}
data ExtensionDef = ExtensionDef
  { extFields :: Map FieldName FieldDef
  , extRels :: Map FieldName RelationshipDef
  , extDefaultPolicy :: Maybe Policy
  -- ^ A @… by default@ line in the block: illegal, recorded for fusion.
  , extFetchBy :: Maybe (NonEmpty FieldName, Policy)
  -- ^ A @fetch by@ clause in the block: illegal, recorded for fusion.
  , extCoKey :: Maybe CoKey
  -- ^ A @joins@\/@refines@ clause on the block: illegal, recorded for fusion.
  }
  deriving stock (Eq, Show)


emptyExtension :: ExtensionDef
emptyExtension = ExtensionDef Map.empty Map.empty Nothing Nothing Nothing


-- | How a co-keyed entity couples to its base's record of truth (§3.8).
data CoKeyMode
  = -- | @joins@: adjacent truth — own @ver@, own surrogate keys, own lifecycle.
    JoinsBase
  | -- | @refines@: same truth — shared @ver@; writes mint keys for the whole
    -- family; deleting the base tombstones the family.
    RefinesBase
  deriving stock (Eq, Show)


-- | A @joins@/@refines@ declaration: the base entity and the coupling mode.
data CoKey = CoKey
  { ckBase :: TypeName
  , ckMode :: CoKeyMode
  }
  deriving stock (Eq, Show)


data FieldDef = FieldDef
  { fieldType :: FieldType
  , fieldArgs :: [ArgDef]
  -- ^ @avatarUrl(size: Int = 96)@ — declared arguments with defaults.
  , fieldPolicy :: Maybe Policy
  -- ^ 'Nothing' means the entity default applies.
  , fieldDerivation :: Maybe Derivation
  -- ^ @derived reads … on read \/ maintained@ (§3.7); 'Nothing' for
  -- ordinary stored fields.
  }
  deriving stock (Eq, Show)


{- | A derived field's declaration (§3.7): the read set (the dual of a
mutation's write set), how the value materializes, and the optional
audited declassification.
-}
data Derivation = Derivation
  { derivReads :: NonEmpty Dep
  , derivMaterialize :: Materialization
  , derivDeclassify :: Maybe Text
  -- ^ @\@declassify(approved: \"…\")@: the written justification; its
  -- presence skips the information-flow domination check (§3.7).
  }
  deriving stock (Eq, Show)


-- | One element of a derived field's read set (§3.7).
data Dep
  = -- | @own(f1, f2)@: stored fields of the owning entity.
    OwnFields (NonEmpty FieldName)
  | -- | @\<edge\> ...Fragment@: follow a declared to-one edge, read the
    -- schema fragment's fields off the target.
    ViaEdge FieldName FragmentName
  | -- | @\<rel\> count\/sum(f)\/min(f)\/max(f)@: aggregate over the named
    -- @has many@ relationship's collection.
    ViaCollection FieldName Aggregate
  deriving stock (Eq, Ord, Show)


-- | A set-in map-out collection aggregate (§3.7 Planning).
data Aggregate = AggCount | AggSum FieldName | AggMin FieldName | AggMax FieldName
  deriving stock (Eq, Ord, Show)


-- | When a derived value is computed (§3.7 Materialization).
data Materialization
  = -- | Computed in the plan on every read; witnessed by the response.
    OnRead
  | -- | Stored on the row, recomputed by the outbox relay; the ordinary
    -- @ver@ witnesses it.
    Maintained
  deriving stock (Eq, Show)


{- | Does an @on read@ derivation compile into hidden traversals (§3.7
Planning)? 'OwnFields'-only read sets resolve from the loaded row itself.
-}
derivationHidden :: Derivation -> Bool
derivationHidden d = any hidden (NE.toList (derivReads d))
  where
    hidden = \case
      OwnFields _ -> False
      ViaEdge _ _ -> True
      ViaCollection _ _ -> True


data ArgDef = ArgDef
  { adName :: ArgName
  , adType :: FieldType
  , adDefault :: Maybe QValueLit
  }
  deriving stock (Eq, Show)


-- | Literal default values reuse the query-language value type.
type QValueLit = QValue


data RelationshipDef
  = ToOne
      { relTarget :: Target
      , relByField :: FieldName
      -- ^ The key-holding field on /this/ entity.
      , relOptional :: Bool
      -- ^ 'False' for the bare @has one@ (exactly one: an unresolved target
      -- is an Edge-scoped @lattice:cardinality@ error, §3.4); 'True' for
      -- @has one?@ (zero or one: absence is legal and renders as absence).
      , relPolicy :: Maybe Policy
      }
  | ToMany
      { relTarget :: Target
      , relCollection :: CollectionDef
      , relPolicy :: Maybe Policy
      }
  deriving stock (Eq, Show)


-- | The target of a relationship or root.
data Target
  = TargetEntity TypeName
  | TargetInterface InterfaceName
  | -- | Inline union @(A | B)@: an anonymous interface with no common fields.
    TargetUnion (NonEmpty TypeName)
  deriving stock (Eq, Show)


-- | Concrete entity types a target may resolve to.
targetTypes :: Schema -> Target -> [TypeName]
targetTypes schema = \case
  TargetEntity t -> [t]
  TargetInterface i ->
    maybe [] (Set.toList . ifaceMemberSet) (Map.lookup i (schemaInterfaces schema))
  TargetUnion ts -> NE.toList ts


data CollectionDef = CollectionDef
  { colLink :: FieldName
  -- ^ The field on the /target/ that points back here (or the root's key).
  , colName :: CollectionName
  -- ^ Auto-derived (@Post.comments@, root name) or set with @as@.
  , colGrouping :: NonEmpty FieldName
  -- ^ Discriminant; defaults to @[colLink]@.
  , colWindow :: Windowing
  }
  deriving stock (Eq, Show)


data Windowing
  = -- | Whole set, no cursor: min and max cardinality plus the overflow
    -- policy. @min@ defaults to 0; a positive floor makes a short scan an
    -- Edge-scoped @lattice:collection-underflow@ error (§3.6).
    Bounded Natural Natural OverflowPolicy
  | Paginated CursorSpec
  deriving stock (Eq, Show)


data OverflowPolicy = Overflow | Truncate
  deriving stock (Eq, Show)


data CursorSpec = CursorSpec
  { csKeyset :: NonEmpty (FieldName, Direction)
  , csDefaultPage :: Maybe Natural
  , csMaxPage :: Natural
  , csTotal :: CountPolicy
  }
  deriving stock (Eq, Show)


data Direction = Asc | Desc
  deriving stock (Eq, Ord, Show)


data CountPolicy = CountNone | CountEstimate | CountExact
  deriving stock (Eq, Show)


-- | A schema-declared fragment (§4.5): late-bound, a pertinent declaration.
data FragmentDef = FragmentDef
  { fragOn :: Text
  -- ^ Entity type or interface name.
  , fragParams :: [VarDef]
  , fragSelection :: SelectionSet
  }
  deriving stock (Eq, Show)


data RootKind = RootGet | RootList
  deriving stock (Eq, Show)


data RootDef = RootDef
  { rootKind :: RootKind
  , rootTarget :: Target
  , rootParams :: [ArgDef]
  -- ^ @get hero(episode: Episode?)@ parameters / grouping-key arguments.
  , rootCollection :: Maybe CollectionDef
  -- ^ Present exactly when 'rootKind' is 'RootList'.
  , rootPolicy :: Policy
  }
  deriving stock (Eq, Show)


data MutationDef = MutationDef
  { mutParams :: [ArgDef]
  -- ^ The input record's fields.
  , mutGuard :: Policy
  , mutReturns :: TypeName
  , mutWrites :: [WriteScopeDecl]
  , mutInvalidates :: InvalidationSpec
  , mutEffect :: EffectClass
  , mutErrors :: Maybe (TypeName, Openness, NonEmpty Text)
  -- ^ Declared domain error sum (§9.4.2).
  , mutBatch :: Maybe BatchPolicy
  , mutBinding :: Maybe VerbBinding
  -- ^ Entity-space verb binding (§11.7); 'Nothing' = named @POST /m/{name}@ only.
  }
  deriving stock (Eq, Show)


data WriteScopeDecl
  = WEntity TypeName KeyExprD
  | WCollection CollectionName GroupExprD
  deriving stock (Eq, Show)


data KeyExprD
  = -- | @Post(post)@: key is the named input argument.
    KeyArg ArgName
  | -- | @Review(new)@: an entity created by the effect.
    KeyNew
  deriving stock (Eq, Show)


data GroupExprD
  = -- | @feed(Post.orgId)@: grouping value read off the written entity.
    GroupOfWritten TypeName FieldName
  | -- | @reviews(episode)@: grouping value is a named input argument.
    GroupArg ArgName
  deriving stock (Eq, Show)


data EffectClass
  = Transactional
  | NaturallyIdempotent Text
  -- ^ Carries the required written justification.
  | Workflow
  deriving stock (Eq, Show)


data InvalidationSpec
  = ExactlyWrites
  | WritesPlus [WriteScopeDecl]
  deriving stock (Eq, Show)


data BatchPolicy = BatchPolicy
  { bpAtomicity :: Atomicity
  , bpMaxItems :: Natural
  , bpBound :: Maybe (BindVerb, TypeName)
  -- ^ Bound-batch collection binding (§11.8): @batch … as VERB \/e\/{Type}@.
  -- Legal only alongside a singular 'VerbBinding' of the same verb and
  -- target ('BindPatch', 'BindCreate', or 'BindDelete'; PUT never batches).
  }
  deriving stock (Eq, Show)


data Atomicity = AllOrNothing | BestEffort
  deriving stock (Eq, Show)


{- | A mutation's entity-space wire spelling (§11.7): the verb of an
@as VERB \/e\/{Type}[\/{arg}]@ clause. 'BindCreate' is @POST@ to the
collection URL (the spec's @CREATE@); the keyed verbs carry the key
argument in the URL's final segment.
-}
data BindVerb = BindPut | BindPatch | BindDelete | BindCreate
  deriving stock (Eq, Ord, Show)


-- | The HTTP method token of a 'BindVerb', as printed in the IDL.
bindVerbName :: BindVerb -> Text
bindVerbName = \case
  BindPut -> "PUT"
  BindPatch -> "PATCH"
  BindDelete -> "DELETE"
  BindCreate -> "POST"


{- | A verb binding (§11.7): an additional entity-space wire spelling for a
mutation. The binding chooses the wire spelling only — guard, writes,
effect class, and invalidation apply identically to the named form.

'vbKeyArg' is the mutation argument bound by the URL's @{arg}@ segment;
'Nothing' is the collection form, which is 'BindCreate'-only. 'vbLww'
(@last-writer-wins@) suppresses the @428 Precondition Required@ demand on
the keyed verbs.
-}
data VerbBinding = VerbBinding
  { vbVerb :: BindVerb
  , vbTarget :: TypeName
  , vbKeyArg :: Maybe ArgName
  , vbLww :: Bool
  }
  deriving stock (Eq, Show)


-- | Origin budgets (§14.1), published in discovery.
data Budgets = Budgets
  { maxCanonicalBytes :: Natural
  , maxDepth :: Natural
  , maxRoots :: Natural
  , maxRounds :: Natural
  , maxRoundFanout :: Natural
  , maxSurrogateKeys :: Natural
  , maxBatchItems :: Natural
  , maxPageDefault :: Natural
  -- ^ The @max@ a bounded collection defaults to when omitted (§3.6).
  , coalesceWindowMs :: Natural
  }
  deriving stock (Eq, Show)


defaultBudgets :: Budgets
defaultBudgets =
  Budgets
    { maxCanonicalBytes = 65536
    , maxDepth = 12
    , maxRoots = 8
    , maxRounds = 8
    , maxRoundFanout = 10000
    , maxSurrogateKeys = 256
    , maxBatchItems = 500
    , maxPageDefault = 100
    , coalesceWindowMs = 5
    }


-- ---------------------------------------------------------------------------
-- Lookups
-- ---------------------------------------------------------------------------

{- | The documentation string (§3.9) attached to a declaration or item, if
any. The clean accessor over 'schemaDescriptions' for codegen and tooling:
@descriptionOf s (OnEntityItem "User" "email")@ is the doc comment written
on that field. Descriptions are metadata (they move the schema hash but
never a @planId@), so this never affects planning — only what codegen emits
and what the explorer shows in hover and completion.
-}
descriptionOf :: Schema -> DeclPath -> Maybe Text
descriptionOf s p = Map.lookup p (schemaDescriptions s)


lookupEntity :: Schema -> TypeName -> Maybe EntityDef
lookupEntity s t = Map.lookup t (schemaEntities s)


lookupEntityField :: EntityDef -> FieldName -> Maybe FieldDef
lookupEntityField e f = Map.lookup f (entityFields e)


lookupEntityRel :: EntityDef -> FieldName -> Maybe RelationshipDef
lookupEntityRel e f = Map.lookup f (entityRels e)


-- | The effective policy of a field, falling back to the entity default.
entityFieldPolicy :: EntityDef -> FieldDef -> Policy
entityFieldPolicy e fd = maybe (entityDefaultPolicy e) id (fieldPolicy fd)


{- | The types sharing one record of truth with the given type (§3.8): the
family base plus every @refines@ co-keyed entity of it. A refinement
resolves via its base; a @joins@ companion, an uncoupled entity, or an
unknown type is a singleton. The base is the head; refinements follow in
ascending name order.
-}
sharedTruthFamily :: Schema -> TypeName -> NonEmpty TypeName
sharedTruthFamily s t = case lookupEntity s t of
  Just e | Just (CoKey base RefinesBase) <- entityCoKey e -> familyOf base
  _ -> familyOf t
  where
    familyOf base = base :| Map.foldrWithKey (refinement base) [] (schemaEntities s)
    refinement base n e acc = case entityCoKey e of
      Just (CoKey b RefinesBase) | b == base -> n : acc
      _ -> acc


interfaceMembers :: Schema -> InterfaceName -> Set TypeName
interfaceMembers s i = maybe mempty ifaceMemberSet (Map.lookup i (schemaInterfaces s))


-- | Schema fragments declared on the given type (or interface) name.
schemaFragmentsOn :: Schema -> Text -> Map FragmentName FragmentDef
schemaFragmentsOn s ty = Map.filter (\f -> fragOn f == ty) (schemaFragments s)


{- | Does a wire value bind an empty array somewhere a nonempty list
(@[t]+@, 'TList1', §3.5.2) governs it? Structural, not a typechecker:
descends options, list\/set\/vector elements, and object-form map values,
resolving newtypes through 'schemaTypes' (depth-bounded against
pathological chains). Only the emptiness rule is checked; a value that
does not match its type's shape is someone else's diagnostic.
-}
violatesList1 :: Schema -> FieldType -> A.Value -> Bool
violatesList1 schema = go (8 :: Int)
  where
    go :: Int -> FieldType -> A.Value -> Bool
    go fuel ft v
      | fuel <= 0 = False
      | otherwise = case ft of
          TList1 t
            | A.Array xs <- v -> null xs || any (go fuel t) xs
          TList t
            | A.Array xs <- v -> any (go fuel t) xs
          TSet t
            | A.Array xs <- v -> any (go fuel t) xs
          TVec _ t
            | A.Array xs <- v -> any (go fuel t) xs
          TMap _ tv
            | A.Object o <- v -> any (go fuel tv) (KM.elems o)
          TOptional t
            | A.Null <- v -> False
            | otherwise -> go fuel t v
          TNamed n
            | Just (DeclNewtype t _) <- Map.lookup n (schemaTypes schema) ->
                go (fuel - 1) t v
          _ -> False
