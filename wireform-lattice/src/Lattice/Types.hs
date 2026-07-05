{- | Shared vocabulary for the Lattice protocol: names, entity references,
visibility policies and levels, the type language of spec §3.5, and claims.

Everything here is protocol vocabulary with no behavior beyond the
join-semilattice of visibility levels (§8.1) and policy evaluation at
emission time. The semantic schema model lives in "Lattice.Schema"; the
query AST in "Lattice.Query.AST".
-}
module Lattice.Types (
  -- * Names
  TypeName (..),
  FieldName (..),
  ArgName (..),
  VarName (..),
  RootName (..),
  MutationName (..),
  CollectionName (..),
  FragmentName (..),
  ClaimName (..),
  InterfaceName (..),

  -- * Entity references
  Ref (..),
  renderRef,
  parseRef,

  -- * Visibility
  Policy (..),
  ClaimPredicate (..),
  PredRhs (..),
  Level (..),
  joinLevel,
  policyLevel,
  Claims,
  SliceName (..),
  sliceOfLevel,
  renderSlice,
  parseSlice,

  -- * The type language (§3.5)
  FieldType (..),
  Prim (..),
  Openness (..),
  Refinement (..),
  TypeDecl (..),
  Ctor (..),

  -- * Snapshot tokens
  SnapshotToken,
) where

import Data.Aeson qualified as A
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric.Natural (Natural)


-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

newtype TypeName = TypeName {unTypeName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype FieldName = FieldName {unFieldName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype ArgName = ArgName {unArgName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype VarName = VarName {unVarName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype RootName = RootName {unRootName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype MutationName = MutationName {unMutationName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype CollectionName = CollectionName {unCollectionName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype FragmentName = FragmentName {unFragmentName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype ClaimName = ClaimName {unClaimName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


newtype InterfaceName = InterfaceName {unInterfaceName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)


-- ---------------------------------------------------------------------------
-- Entity references
-- ---------------------------------------------------------------------------

{- | A typed entity reference, @"Type:key"@ on the wire. The key part is the
canonical wire form of the entity's key field(s); composite keys join their
canonical forms with @","@.
-}
data Ref = Ref
  { refType :: TypeName
  , refKey :: Text
  }
  deriving stock (Eq, Ord, Show)


renderRef :: Ref -> Text
renderRef (Ref (TypeName t) k) = t <> ":" <> k


-- | Split at the first @:@. Type and key must both be nonempty.
parseRef :: Text -> Maybe Ref
parseRef t = case T.breakOn ":" t of
  (ty, rest)
    | not (T.null ty)
    , Just k <- T.stripPrefix ":" rest
    , not (T.null k) ->
        Just (Ref (TypeName ty) k)
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- Visibility
-- ---------------------------------------------------------------------------

{- | A visibility policy (§3.1). @RequiresClaims@ predicates range over the
schema's registered claims and fields of the entity under inspection; the
claim set is the policy's contribution to the slice's claim dependency.
-}
data Policy
  = Public
  | RequiresClaims [ClaimPredicate]
  | Private
  deriving stock (Eq, Ord, Show)


{- | One conjunct of a @visible when@ / @allow when@ clause:
@caller.org = orgId@ (claim equals entity field), @caller.role = Admin@
(claim equals literal), or @caller.role in [Editor, Admin]@.
-}
data ClaimPredicate = ClaimPredicate
  { cpClaim :: ClaimName
  , cpRhs :: PredRhs
  }
  deriving stock (Eq, Ord, Show)


data PredRhs
  = -- | Compare against a field of the entity under inspection (bare name).
    RhsField FieldName
  | -- | Compare against one literal (enum constructor or scalar).
    RhsLiteral A.Value
  | -- | @in@ a list of literals.
    RhsOneOf [A.Value]
  deriving stock (Eq, Ord, Show)


-- | The visibility join-semilattice: @Public < Claims(S) < Private@ (§8.1).
data Level
  = LPublic
  | LClaims [ClaimName]
  -- ^ Sorted, deduplicated claim-name set.
  | LPrivate
  deriving stock (Eq, Ord, Show)


joinLevel :: Level -> Level -> Level
joinLevel LPrivate _ = LPrivate
joinLevel _ LPrivate = LPrivate
joinLevel LPublic l = l
joinLevel l LPublic = l
joinLevel (LClaims a) (LClaims b) = LClaims (mergeAsc a b)
  where
    mergeAsc [] ys = ys
    mergeAsc xs [] = xs
    mergeAsc xxs@(x : xs) yys@(y : ys) = case compare x y of
      LT -> x : mergeAsc xs yys
      GT -> y : mergeAsc xxs ys
      EQ -> x : mergeAsc xs ys


-- | The level a policy contributes to the path join.
policyLevel :: Policy -> Level
policyLevel = \case
  Public -> LPublic
  RequiresClaims preds ->
    LClaims (dedupAsc (map cpClaim preds))
  Private -> LPrivate
  where
    dedupAsc = foldr insertAsc []
    insertAsc x [] = [x]
    insertAsc x yys@(y : ys) = case compare x y of
      LT -> x : yys
      EQ -> yys
      GT -> y : insertAsc x ys


-- | A caller's presented claims payload: claim name to canonical JSON value.
type Claims = Map ClaimName A.Value


-- | Authorization slices (§6.6): the three data slices plus the plan pseudo-slice.
data SliceName = SlicePub | SliceCtx | SlicePriv | SlicePlan
  deriving stock (Eq, Ord, Show)


sliceOfLevel :: Level -> SliceName
sliceOfLevel = \case
  LPublic -> SlicePub
  LClaims _ -> SliceCtx
  LPrivate -> SlicePriv


renderSlice :: SliceName -> Text
renderSlice = \case
  SlicePub -> "pub"
  SliceCtx -> "ctx"
  SlicePriv -> "priv"
  SlicePlan -> "plan"


parseSlice :: Text -> Maybe SliceName
parseSlice = \case
  "pub" -> Just SlicePub
  "ctx" -> Just SliceCtx
  "priv" -> Just SlicePriv
  "plan" -> Just SlicePlan
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- The type language (§3.5)
-- ---------------------------------------------------------------------------

{- | A field/argument/variable type. @TNamed@ references a declared newtype,
product, sum, or enum; entity types never appear in 'FieldType' position
(edges are relationships, a disjoint grammar position).
-}
data FieldType
  = TPrim Prim
  | TNamed TypeName
  | TOptional FieldType
  | TList FieldType
  | TSet FieldType
  | TMap FieldType FieldType
  | TVec Natural FieldType
  deriving stock (Eq, Ord, Show)


-- | Primitive types with pinned canonical wire forms (§3.5.3).
data Prim
  = PBool
  | PI8
  | PI16
  | PI32
  | PI64
  | PW8
  | PW16
  | PW32
  | PW64
  | PInteger
  | PDecimal
  | PF32
  | PF64
  | PText
  | PBytes (Maybe Natural)
  | PBit Natural
  | PUuid
  | PTimestamp
  | PDate
  | PTimeOfDay
  | PDuration
  | PCursor
  | PJson
  deriving stock (Eq, Ord, Show)


data Openness = Open | Closed
  deriving stock (Eq, Ord, Show)


-- | Refinement annotations on newtypes: validated at input boundaries, not structural.
data Refinement
  = RefLen Natural
  | RefMin Integer
  | RefMax Integer
  | RefMatch Text
  deriving stock (Eq, Ord, Show)


-- | A declared value type (§3.5.2).
data TypeDecl
  = DeclNewtype FieldType [Refinement]
  | DeclRecord [(FieldName, FieldType)]
  | DeclSum Openness (NonEmpty Ctor)
  | DeclEnum Openness (NonEmpty Text)
  deriving stock (Eq, Ord, Show)


-- | One constructor of a sum: a (possibly empty) record.
data Ctor = Ctor
  { ctorName :: Text
  , ctorFields :: [(FieldName, FieldType)]
  }
  deriving stock (Eq, Ord, Show)


{- | An opaque, per-domain-ordered snapshot token (§13.1). Comparison
semantics belong to the origin; the protocol only transports it.
-}
type SnapshotToken = Text
