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
-}
module Lattice.Schema (
  Schema (..),
  InterfaceDef (..),
  EntityDef (..),
  CoKey (..),
  CoKeyMode (..),
  FieldDef (..),
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
  Budgets (..),
  defaultBudgets,

  -- * Lookups
  lookupEntity,
  lookupEntityField,
  lookupEntityRel,
  entityFieldPolicy,
  sharedTruthFamily,
  interfaceMembers,
  schemaFragmentsOn,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
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
  , schemaFragments :: Map FragmentName FragmentDef
  -- ^ Schema-declared fragments (late-bound, §4.5).
  , schemaRoots :: Map RootName RootDef
  , schemaMutations :: Map MutationName MutationDef
  }
  deriving stock (Eq, Show)


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
  }
  deriving stock (Eq, Show)


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
  = Bounded Natural OverflowPolicy
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
  }
  deriving stock (Eq, Show)


data Atomicity = AllOrNothing | BestEffort
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
