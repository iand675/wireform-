{- | The pure schema-compatibility engine (spec §17): the four-axis change
taxonomy (§17.2), the check directions (§17.3), and corpus-aware checking
(§17.4) with deprecation gating (§17.5).

'diffSchemas' classifies a baseline→candidate diff along four independent
axes: compile compatibility, plan stability, semantic compatibility, and
cursor compatibility. A single edit may emit several 'Change's (an open
enum's non-append change is simultaneously a cursor break and a semantic
break, §17.2).

Overrides: a @\@break(approved: "TICKET")@ annotation in the /candidate/
IDL clears the breaks reported at its site ('chOverride' carries the
ticket). A removal has no surviving declaration of its own, so its
override site is the nearest surviving enclosure: a removed field or
relationship is overridden by @\@break@ on its entity (or interface), a
removed argument by @\@break@ on its surviving field\/root\/mutation, and
a removed top-level declaration by @\@break@ on the @schema@ declaration.

Corpus attribution is deliberately approximate: every corpus entry is
recompiled once under the newest baseline and once under the candidate;
an entry is attributed to a compile-axis break when it stops compiling
and mentions the change's subject token, to a plan-axis change when its
plan id moves and it mentions the token, and to a semantic\/cursor break
by token mention alone (those queries still compile — usage is the blast
radius). Identity-affecting default changes compile everywhere, so they
attribute by mention alone. @newestClient@ is the lexicographic maximum
of the attributed entries' advisory client builds.

'ServerForward' is the pure structural reverse diff (candidate-valid
queries must compile under the baseline), an approximation: the corpus
holds baseline traffic only, so forward breaks are reported structurally
without corpus weights attached to future-only queries.
-}
module Lattice.Compat (
  -- * Change taxonomy (§17.2)
  ChangeAxis (..),
  axisName,
  Change (..),
  diffSchemas,

  -- * Directions (§17.3)
  CheckMode (..),
  parseCheckMode,
  renderCheckMode,
  WindowUnit (..),
  Window (..),
  parseWindow,
  renderWindow,
  windowStart,

  -- * Corpus-aware checking (§17.4)
  LoggedSchema (..),
  CheckConfig (..),
  CorpusImpact (..),
  ReportedChange (..),
  Report (..),
  checkSchemas,
) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.List (foldl', isPrefixOf, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time (UTCTime (..))
import Data.Time.Calendar (addDays, addGregorianMonthsClip, addGregorianYearsClip)
import Lattice.Canonical (compileText)
import Lattice.IDL.Print (renderType)
import Lattice.Plan (Plan (..), planQuery)
import Lattice.Registry (CorpusEntry (..))
import Lattice.Schema
import Lattice.Types
import Numeric.Natural (Natural)


-- ---------------------------------------------------------------------------
-- Change taxonomy (§17.2)
-- ---------------------------------------------------------------------------

-- | The four independent compatibility axes of §17.2.
data ChangeAxis = AxisCompile | AxisPlan | AxisSemantic | AxisCursor
  deriving stock (Eq, Ord, Show)


-- | The report spelling of an axis.
axisName :: ChangeAxis -> Text
axisName = \case
  AxisCompile -> "compile"
  AxisPlan -> "plan"
  AxisSemantic -> "semantic"
  AxisCursor -> "cursor"


{- | One classified change. 'chSubject' is the bare dotted name of the
changed declaration (@Post@, @Post.ctr@, a root or mutation name);
'chDetail' names the kind of change. 'chOverride' is the approving ticket
of a candidate @\@break@ annotation covering the change, or the
deprecation-sunset note 'checkSchemas' attaches when gating a removal
(§17.5); an overridden break does not fail the check.
-}
data Change = Change
  { chAxis :: ChangeAxis
  , chBreaking :: Bool
  , chSubject :: Text
  , chDetail :: Text
  , chOverride :: Maybe Text
  }
  deriving stock (Eq, Show)


-- | A change plus the internal facts 'checkSchemas' needs: the baseline
-- path of a removal (deprecation gating) and whether the change is
-- identity-affecting (corpus attribution by mention, not recompile).
data RawChange = RawChange
  { rawChange :: Change
  , rawRemovedAt :: Maybe DeclPath
  , rawIdentity :: Bool
  }


{- | Diff baseline against candidate and classify every change (§17.2).
@\@break@ overrides are read from the candidate. Deterministically sorted
by (subject, axis, detail).
-}
diffSchemas :: Schema -> Schema -> [Change]
diffSchemas old new = sortChanges (map rawChange (diffRawWith new True old new))


sortChanges :: [Change] -> [Change]
sortChanges = sortOn (\c -> (chSubject c, chAxis c, chDetail c))


{- | The classifier. @annSrc@ is the schema whose @\@break@ annotations
resolve overrides (always the real candidate, even for the reversed
'ServerForward' diff); @deprInfo@ gates the informational
"newly deprecated" changes (suppressed on reversed diffs, where a
baseline-only deprecation is not news).
-}
diffRawWith :: Schema -> Bool -> Schema -> Schema -> [RawChange]
diffRawWith annSrc deprInfo old new =
  concat
    [ schemaNameD
    , claimsD
    , typesD
    , interfacesD
    , entitiesD
    , fragmentsD
    , rootsD
    , mutationsD
    , deprecationsD
    ]
  where
    -- ---- emission helpers ------------------------------------------------
    ov sites = listToMaybe (mapMaybe (\p -> Map.lookup p (schemaBreaks annSrc)) sites)
    chg sites axis brk subj det =
      RawChange (Change axis brk subj det (ov sites)) Nothing False
    rmv path sites axis subj det =
      RawChange (Change axis True subj det (ov sites)) (Just path) False
    idc sites subj det =
      RawChange (Change AxisCompile True subj det (ov sites)) Nothing True

    splitMap o n =
      ( Map.toAscList (Map.difference o n)
      , Map.toAscList (Map.difference n o)
      , Map.toAscList (Map.intersectionWith (,) o n)
      )

    -- ---- policies ----------------------------------------------------------
    -- Any policy change moves plan elements between slices (plan axis,
    -- reported per disturbed query, §17.2); a narrowing additionally turns
    -- visible fields into elisions (semantic break). Incomparable claim
    -- rewrites are conservatively narrowing.
    policyD sites subj what po pn
      | po == pn = []
      | otherwise =
          chg sites AxisPlan False subj (what <> " changed (plan-moving)")
            : [ chg sites AxisSemantic True subj (what <> " narrowed (visible data elides)")
              | policyNarrows po pn
              ]

    policyNarrows o n = case (o, n) of
      _ | o == n -> False
      (_, Public) -> False
      (Private, _) -> False
      (Public, _) -> True
      (RequiresClaims _, Private) -> True
      (RequiresClaims a, RequiresClaims b) -> a /= b

    -- ---- arguments ---------------------------------------------------------
    argsD sites subj what olds news =
      concat [removed, added, changed]
      where
        om = Map.fromList [(unArgName (adName a), a) | a <- olds]
        nm = Map.fromList [(unArgName (adName a), a) | a <- news]
        (ro, ao, co) = splitMap om nm
        removed =
          [ chg sites AxisCompile True subj (what <> " `" <> an <> "` removed")
          | (an, _) <- ro
          ]
        added =
          [ if optionalArg a
              then chg [] AxisCompile False subj (what <> " `" <> an <> "` added (optional/defaulted; additive)")
              else chg sites AxisCompile True subj (what <> " `" <> an <> "` added without a default")
          | (an, a) <- ao
          ]
        changed = concat [one an oa na | (an, (oa, na)) <- co]
        one an oa na =
          [ chg sites AxisCompile True subj
              (what <> " `" <> an <> "` type changed: " <> renderType (adType oa) <> " -> " <> renderType (adType na))
          | adType oa /= adType na
          ]
            <> case (adDefault oa, adDefault na) of
              (Just x, Just y)
                | x /= y ->
                    [idc sites subj (what <> " `" <> an <> "` default changed (identity-affecting, §5.1)")]
              (Just _, Nothing) ->
                [idc sites subj (what <> " `" <> an <> "` default removed (identity-affecting, §5.1)")]
              (Nothing, Just _) ->
                [chg [] AxisCompile False subj (what <> " `" <> an <> "` default added (additive)")]
              _ -> []
        optionalArg a = isJust (adDefault a) || isOptionalTy (adType a)
        isOptionalTy = \case
          TOptional _ -> True
          _ -> False

    -- ---- schema name -------------------------------------------------------
    schemaNameD =
      [ chg [] AxisPlan False "schema" ("schema renamed: " <> schemaName old <> " -> " <> schemaName new)
      | schemaName old /= schemaName new
      ]

    -- ---- claims (§17.2 plan axis: pertinent declarations) -------------------
    claimsD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaClaims old) (schemaClaims new)
        removed =
          [ chg [OnSchema] AxisPlan False (unClaimName c) "claim removed from the registry (ctx slices move)"
          | (c, _) <- ro
          ]
        added =
          [ chg [] AxisPlan False (unClaimName c) "claim added (additive)"
          | (c, _) <- ao
          ]
        changed =
          [ chg [OnSchema] AxisPlan False (unClaimName c)
              ("claim type changed (pertinent; plan-moving): " <> renderType a <> " -> " <> renderType b)
          | (c, (a, b)) <- co
          , a /= b
          ]

    -- ---- value types ---------------------------------------------------------
    typesD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaTypes old) (schemaTypes new)
        removed = [rmv (OnType t) [OnSchema] AxisCompile (unTypeName t) "type removed" | (t, _) <- ro]
        added = [chg [] AxisCompile False (unTypeName t) "type added (additive)" | (t, _) <- ao]
        changed = concat [typeD t a b | (t, (a, b)) <- co, a /= b]

    typeD t o n = case (o, n) of
      (DeclEnum op1 cs1, DeclEnum op2 cs2) ->
        [chg sites AxisSemantic True subj "enum openness changed" | op1 /= op2]
          <> enumD op1 (NE.toList cs1) (NE.toList cs2)
      (DeclNewtype t1 r1, DeclNewtype t2 r2) ->
        [ chg sites AxisCompile True subj
            ("newtype base changed: " <> renderType t1 <> " -> " <> renderType t2)
        | t1 /= t2
        ]
          <> [chg sites AxisSemantic True subj "newtype refinements changed" | r1 /= r2]
      (DeclRecord f1, DeclRecord f2) -> recordD (Map.fromList f1) (Map.fromList f2)
      (DeclSum o1 c1, DeclSum o2 c2) ->
        [chg sites AxisSemantic True subj "sum openness changed" | o1 /= o2]
          <> sumD o1 (NE.toList c1) (NE.toList c2)
      _ -> [chg sites AxisCompile True subj "type declaration kind changed"]
      where
        sites = [OnType t]
        subj = unTypeName t

        -- Enum declaration order is cursor-significant (§17.2): open enums
        -- are append-only; anything else is a cursor break AND a semantic
        -- break. Closed enums additionally break exhaustive matches even
        -- on append (§3.5.4 polarity, judged against the baseline promise).
        enumD op l1 l2
          | l1 == l2 = []
          | l1 `isPrefixOf` l2 = case op of
              Open -> [chg [] AxisSemantic False subj "enum extended (append; additive for open enums)"]
              Closed -> [chg sites AxisSemantic True subj "closed enum extended (exhaustive matches break)"]
          | otherwise =
              [ chg sites AxisCursor True subj "enum constructors changed non-append (declaration order is cursor-significant; open enums are append-only)"
              , chg sites AxisSemantic True subj "enum constructors changed non-append (ordered comparisons change meaning)"
              ]

        recordD f1 f2 =
          concat
            [ [ chg sites AxisCompile True subj ("record field `" <> unFieldName f <> "` removed")
              | (f, _) <- rf
              ]
            , [ chg sites AxisSemantic True subj
                  ("record field `" <> unFieldName f <> "` added (no default; polarity-dependent, §3.5.4)")
              | (f, _) <- af
              ]
            , [ chg sites AxisCompile True subj
                  ("record field `" <> unFieldName f <> "` type changed: " <> renderType a <> " -> " <> renderType b)
              | (f, (a, b)) <- cf
              , a /= b
              ]
            ]
          where
            (rf, af, cf) = splitMap f1 f2

        sumD op c1 c2 =
          concat
            [ [chg sites AxisCompile True subj ("constructor `" <> c <> "` removed") | (c, _) <- rc]
            , [ case op of
                  Open -> chg [] AxisSemantic False subj ("constructor `" <> c <> "` added (open sum; additive)")
                  Closed -> chg sites AxisSemantic True subj ("constructor `" <> c <> "` added (closed sum; exhaustive matches break)")
              | (c, _) <- ac
              ]
            , [ chg sites AxisCompile True subj ("constructor `" <> c <> "` fields changed")
              | (c, (a, b)) <- cc
              , a /= b
              ]
            ]
          where
            (rc, ac, cc) =
              splitMap
                (Map.fromList [(ctorName c, ctorFields c) | c <- c1])
                (Map.fromList [(ctorName c, ctorFields c) | c <- c2])

    -- ---- interfaces ------------------------------------------------------------
    interfacesD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaInterfaces old) (schemaInterfaces new)
        removed =
          [rmv (OnInterface i) [OnSchema] AxisCompile (unInterfaceName i) "interface removed" | (i, _) <- ro]
        added = [chg [] AxisCompile False (unInterfaceName i) "interface added (additive)" | (i, _) <- ao]
        changed = concat [ifaceD i a b | (i, (a, b)) <- co]

    ifaceD i o n =
      itemsD
        (unInterfaceName i)
        (OnIfaceItem i)
        [OnInterface i]
        (fromMaybe Public . fieldPolicy)
        (ifaceFields o)
        (ifaceFields n)
        (ifaceRels o)
        (ifaceRels n)

    -- ---- entities ---------------------------------------------------------------
    entitiesD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaEntities old) (schemaEntities new)
        removed = [rmv (OnEntity t) [OnSchema] AxisCompile (unTypeName t) "entity removed" | (t, _) <- ro]
        added = [chg [] AxisCompile False (unTypeName t) "entity added (additive)" | (t, _) <- ao]
        changed = concat [entityD t a b | (t, (a, b)) <- co]

    entityD t o n =
      concat
        [ [chg sites AxisCompile True subj "entity key changed" | entityKey o /= entityKey n]
        , coKeyD
        , policyD sites subj "default policy" (entityDefaultPolicy o) (entityDefaultPolicy n)
        , fetchByD
        , implementsD
        , itemsD subj (OnEntityItem t) sites effPol (entityFields o) (entityFields n) (entityRels o) (entityRels n)
        ]
      where
        subj = unTypeName t
        sites = [OnEntity t]
        effPol fd = fromMaybe (entityDefaultPolicy n) (fieldPolicy fd)
        coKeyD = case (entityCoKey o, entityCoKey n) of
          (a, b)
            | a == b -> []
          (Just _, Nothing) -> [chg sites AxisCompile True subj "co-key coupling removed (shared-truth family changes)"]
          (Nothing, Just _) -> [chg sites AxisCompile True subj "entity became co-keyed (key spec now inherited)"]
          _ -> [chg sites AxisCompile True subj "co-key base or mode changed"]
        fetchByD = case (entityFetchBy o, entityFetchBy n) of
          (a, b)
            | a == b -> []
          (Just _, Nothing) -> [chg sites AxisCompile True subj "point fetch (`fetch by`) removed"]
          (Nothing, Just _) -> [chg [] AxisCompile False subj "point fetch (`fetch by`) added (additive)"]
          (Just a, Just b) -> policyD sites subj "fetch policy" a b
          (Nothing, Nothing) -> []
        implementsD =
          [ chg sites AxisCompile True subj ("no longer implements `" <> unInterfaceName i <> "`")
          | i <- Set.toAscList (Set.difference (entityImplements o) (entityImplements n))
          ]
            <> [ chg [] AxisCompile False subj ("new interface implementor of `" <> unInterfaceName i <> "` (additive)")
               | i <- Set.toAscList (Set.difference (entityImplements n) (entityImplements o))
               ]

    -- Shared field/relationship item diff for entities and interfaces.
    itemsD owner itemPath ownerSites effPol of_ nf or_ nr =
      concat [fRemoved, fAdded, fChanged, rRemoved, rAdded, rChanged, crossed]
      where
        (rf, af, cf) = splitMap of_ nf
        (rr, ar, cr) = splitMap or_ nr
        subjOf f = owner <> "." <> unFieldName f
        fRemoved =
          [ rmv (itemPath f) (ownerSites <> [OnSchema]) AxisCompile (subjOf f) "field removed"
          | (f, _) <- rf
          , not (Map.member f nr) -- became a relationship: reported below
          ]
        fAdded =
          [ chg [] AxisCompile False (subjOf f) "field added (additive)"
          | (f, _) <- af
          , not (Map.member f or_)
          ]
        fChanged = concat [fieldD (itemPath f) (subjOf f) a b | (f, (a, b)) <- cf]
        rRemoved =
          [ rmv (itemPath f) (ownerSites <> [OnSchema]) AxisCompile (subjOf f) "relationship removed"
          | (f, _) <- rr
          , not (Map.member f nf)
          ]
        rAdded =
          [ chg [] AxisCompile False (subjOf f) "relationship added (additive)"
          | (f, _) <- ar
          , not (Map.member f of_)
          ]
        rChanged = concat [relD (itemPath f) (subjOf f) a b | (f, (a, b)) <- cr]
        crossed =
          [ chg [itemPath f] AxisCompile True (subjOf f) "field became a relationship (or vice versa)"
          | f <- Set.toAscList crossKeys
          ]
        crossKeys =
          Set.union
            (Set.intersection (Map.keysSet of_) (Map.keysSet nr))
            (Set.intersection (Map.keysSet or_) (Map.keysSet nf))

        fieldD site subj o n =
          concat
            [ [ chg [site] AxisCompile True subj
                  ("type changed: " <> renderType (fieldType o) <> " -> " <> renderType (fieldType n))
              | fieldType o /= fieldType n
              ]
            , argsD [site] subj "argument" (fieldArgs o) (fieldArgs n)
            , policyD [site] subj "policy" (effPol o) (effPol n)
            , [ chg [site] AxisPlan False subj "derivation changed (plan-moving)"
              | fieldDerivation o /= fieldDerivation n
              ]
            ]

    relD site subj o n = case (o, n) of
      (ToOne t1 b1 o1 p1, ToOne t2 b2 o2 p2) ->
        concat
          [ [chg [site] AxisCompile True subj "relationship target changed" | t1 /= t2]
          , [chg [site] AxisSemantic True subj "link field changed (join column moves)" | b1 /= b2]
          , [chg [site] AxisSemantic True subj "cardinality changed (`has one` <-> `has one?`)" | o1 /= o2]
          , policyD [site] subj "edge policy" (fromMaybe Public p1) (fromMaybe Public p2)
          ]
      (ToMany t1 c1 p1, ToMany t2 c2 p2) ->
        concat
          [ [chg [site] AxisCompile True subj "relationship target changed" | t1 /= t2]
          , collectionD site subj c1 c2
          , policyD [site] subj "edge policy" (fromMaybe Public p1) (fromMaybe Public p2)
          ]
      _ -> [chg [site] AxisCompile True subj "relationship cardinality changed (`has one` <-> `has many`)"]

    collectionD site subj o n =
      concat
        [ [chg [site] AxisSemantic True subj "collection link field changed (membership changes)" | colLink o /= colLink n]
        , [chg [site] AxisPlan False subj "collection renamed (cache grouping and write scopes move)" | colName o /= colName n]
        , [chg [site] AxisPlan False subj "collection grouping changed (surrogate keys move)" | colGrouping o /= colGrouping n]
        , windowD site subj (colWindow o) (colWindow n)
        ]

    windowD site subj o n = case (o, n) of
      _ | o == n -> []
      (Bounded mn1 mx1 op1, Bounded mn2 mx2 op2) ->
        concat
          [ [chg [site] AxisSemantic True subj "collection bound lowered (rows truncate or overflow)" | mx2 < mx1]
          , [chg [] AxisSemantic False subj "collection bound raised (additive)" | mx2 > mx1]
          , [chg [site] AxisSemantic True subj "collection minimum raised (short scans become errors)" | mn2 > mn1]
          , [chg [] AxisSemantic False subj "collection minimum lowered (additive)" | mn2 < mn1]
          , [chg [site] AxisSemantic True subj "overflow policy changed" | op1 /= op2]
          ]
      (Paginated c1, Paginated c2) ->
        concat
          [ [ chg [site] AxisCursor True subj
                "cursor keyset changed (outstanding cursors retire, 410 lattice:cursor-retired; result order changes)"
            | csKeyset c1 /= csKeyset c2
            ]
          , [ idc [site] subj "default page size changed (identity-affecting, §5.1)"
            | csDefaultPage c1 /= csDefaultPage c2
            ]
          , [chg [site] AxisSemantic True subj "max page lowered (larger pages rejected)" | csMaxPage c2 < csMaxPage c1]
          , [chg [] AxisSemantic False subj "max page raised (additive)" | csMaxPage c2 > csMaxPage c1]
          , countD (csTotal c1) (csTotal c2)
          ]
      _ -> [chg [site] AxisCursor True subj "windowing changed (bounded <-> paginated)"]
      where
        countD a b
          | a == b = []
          | countRank b < countRank a = [chg [site] AxisSemantic True subj "count policy weakened (totals disappear)"]
          | otherwise = [chg [] AxisSemantic False subj "count policy strengthened (additive)"]
        countRank = \case
          CountNone -> 0 :: Int
          CountEstimate -> 1
          CountExact -> 2

    -- ---- schema fragments (§17.2: removal breaks every referencing query) -----
    fragmentsD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaFragments old) (schemaFragments new)
        removed =
          [ rmv (OnFragment f) [OnSchema] AxisCompile (unFragmentName f)
              "schema fragment removed (breaks every referencing query)"
          | (f, _) <- ro
          ]
        added = [chg [] AxisCompile False (unFragmentName f) "schema fragment added (additive)" | (f, _) <- ao]
        changed =
          [ chg [OnFragment f] AxisCompile True (unFragmentName f)
              "schema fragment changed (late-bound: every referencing query changes shape)"
          | (f, (a, b)) <- co
          , a /= b
          ]

    -- ---- roots -----------------------------------------------------------------
    rootsD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaRoots old) (schemaRoots new)
        removed = [rmv (OnRoot r) [OnSchema] AxisCompile (unRootName r) "root removed" | (r, _) <- ro]
        added = [chg [] AxisCompile False (unRootName r) "root added (additive)" | (r, _) <- ao]
        changed = concat [rootD r a b | (r, (a, b)) <- co]

    rootD r o n =
      concat
        [ [chg sites AxisCompile True subj "root kind changed (get <-> list)" | rootKind o /= rootKind n]
        , [chg sites AxisCompile True subj "root target changed" | rootTarget o /= rootTarget n]
        , argsD sites subj "parameter" (rootParams o) (rootParams n)
        , policyD sites subj "root policy" (rootPolicy o) (rootPolicy n)
        , case (rootCollection o, rootCollection n) of
            (Just a, Just b) -> collectionD (OnRoot r) subj a b
            _ -> []
        ]
      where
        subj = unRootName r
        sites = [OnRoot r]

    -- ---- mutations ---------------------------------------------------------------
    mutationsD = concat [removed, added, changed]
      where
        (ro, ao, co) = splitMap (schemaMutations old) (schemaMutations new)
        removed = [rmv (OnMutation m) [OnSchema] AxisCompile (unMutationName m) "mutation removed" | (m, _) <- ro]
        added = [chg [] AxisCompile False (unMutationName m) "mutation added (additive)" | (m, _) <- ao]
        changed = concat [mutationD m a b | (m, (a, b)) <- co]

    mutationD m o n =
      concat
        [ argsD sites subj "input field" (mutParams o) (mutParams n)
        , [ chg sites AxisCompile True subj
              ("return type changed: " <> unTypeName (mutReturns o) <> " -> " <> unTypeName (mutReturns n))
          | mutReturns o /= mutReturns n
          ]
        , policyD sites subj "guard" (mutGuard o) (mutGuard n)
        , [chg sites AxisPlan False subj "write set changed (invalidation footprint moves)" | mutWrites o /= mutWrites n]
        , [chg sites AxisPlan False subj "invalidation set changed" | mutInvalidates o /= mutInvalidates n]
        , [chg sites AxisSemantic True subj "effect class changed (idempotency contract, §11.2)" | mutEffect o /= mutEffect n]
        , errorsD
        , batchD
        , bindingD
        ]
      where
        subj = unMutationName m
        sites = [OnMutation m]
        errorsD = case (mutErrors o, mutErrors n) of
          (a, b)
            | a == b -> []
          (Nothing, Just _) -> [chg [] AxisSemantic False subj "domain error sum declared (additive)"]
          (Just (t1, o1, cs1), Just (t2, o2, cs2))
            | t1 == t2
            , o1 == o2
            , o1 == Open
            , Set.fromList (NE.toList cs1) `Set.isSubsetOf` Set.fromList (NE.toList cs2) ->
                [chg [] AxisSemantic False subj "error sum extended (open; additive)"]
          _ -> [chg sites AxisSemantic True subj "declared error sum changed"]
        batchD = case (mutBatch o, mutBatch n) of
          (a, b)
            | a == b -> []
          (Nothing, Just _) -> [chg [] AxisSemantic False subj "batch declared (additive, §17.2)"]
          (Just _, Nothing) -> [chg sites AxisCompile True subj "batch removed (batch endpoint disappears)"]
          (Nothing, Nothing) -> []
          (Just b1, Just b2) ->
            concat
              [ [ chg sites AxisSemantic True subj
                    "batch atomicity changed (all-or-nothing <-> best-effort: partial failure changes meaning, §17.2)"
                | bpAtomicity b1 /= bpAtomicity b2
                ]
              , [ chg sites AxisSemantic True subj "batch maxItems lowered (larger batches rejected, §17.2)"
                | bpMaxItems b2 < bpMaxItems b1
                ]
              , [ chg [] AxisSemantic False subj "batch maxItems raised (additive, §17.2)"
                | bpMaxItems b2 > bpMaxItems b1
                ]
              , case (bpBound b1, bpBound b2) of
                  (x, y)
                    | x == y -> []
                  (Just _, Nothing) -> [chg sites AxisCompile True subj "bound-batch collection binding removed"]
                  (Nothing, Just _) -> [chg [] AxisCompile False subj "bound-batch collection binding added (additive)"]
                  _ -> [chg sites AxisCompile True subj "bound-batch collection binding changed"]
              ]
        bindingD = case (mutBinding o, mutBinding n) of
          (a, b)
            | a == b -> []
          (Just _, Nothing) -> [chg sites AxisCompile True subj "verb binding removed (entity-space URL disappears)"]
          (Nothing, Just _) -> [chg [] AxisCompile False subj "verb binding added (additive)"]
          (Nothing, Nothing) -> []
          (Just v1, Just v2)
            | vbVerb v1 == vbVerb v2
            , vbTarget v1 == vbTarget v2
            , vbKeyArg v1 == vbKeyArg v2 ->
                [chg sites AxisSemantic True subj "last-writer-wins changed (conditional-request demand, §11.7)"]
            | otherwise -> [chg sites AxisCompile True subj "verb binding changed"]

    -- ---- deprecation metadata (informational, §17.5) ---------------------------
    deprecationsD
      | not deprInfo = []
      | otherwise =
          [ chg [] AxisSemantic False (pathSubject p)
              ("deprecated (sunset " <> tshow (depSunset d) <> "): " <> depNote d)
          | (p, d) <- Map.toAscList (Map.difference (schemaDeprecations new) (schemaDeprecations old))
          ]


-- | The report subject of an annotation site.
pathSubject :: DeclPath -> Text
pathSubject = \case
  OnSchema -> "schema"
  OnType t -> unTypeName t
  OnInterface i -> unInterfaceName i
  OnIfaceItem i f -> unInterfaceName i <> "." <> unFieldName f
  OnEntity t -> unTypeName t
  OnEntityItem t f -> unTypeName t <> "." <> unFieldName f
  OnFragment f -> unFragmentName f
  OnRoot r -> unRootName r
  OnMutation m -> unMutationName m


-- ---------------------------------------------------------------------------
-- Directions (§17.3)
-- ---------------------------------------------------------------------------

-- | Client/server skew directions (§17.3).
data CheckMode = ClientBackward | ServerForward | Full
  deriving stock (Eq, Show)


-- | The @?mode=@ spellings: @client-backward[-transitive]@,
-- @server-forward[-transitive]@, @full[-transitive]@. The 'Bool' is the
-- transitive flag.
parseCheckMode :: Text -> Maybe (CheckMode, Bool)
parseCheckMode = \case
  "client-backward" -> Just (ClientBackward, False)
  "client-backward-transitive" -> Just (ClientBackward, True)
  "server-forward" -> Just (ServerForward, False)
  "server-forward-transitive" -> Just (ServerForward, True)
  "full" -> Just (Full, False)
  "full-transitive" -> Just (Full, True)
  _ -> Nothing


renderCheckMode :: CheckMode -> Text
renderCheckMode = \case
  ClientBackward -> "client-backward"
  ServerForward -> "server-forward"
  Full -> "full"


data WindowUnit = WinDays | WinMonths | WinYears
  deriving stock (Eq, Show)


-- | A deployment-log window: the client support horizon as a checker
-- parameter (§17.3).
data Window = Window
  { winCount :: Natural
  , winUnit :: WindowUnit
  }
  deriving stock (Eq, Show)


-- | The ISO-8601 period subset of §17.3: @P\<n\>D@, @P\<n\>M@, @P\<n\>Y@.
parseWindow :: Text -> Maybe Window
parseWindow t = do
  body <- T.stripPrefix "P" t
  (n, rest) <- either (const Nothing) Just (TR.decimal body)
  u <- case rest of
    "D" -> Just WinDays
    "M" -> Just WinMonths
    "Y" -> Just WinYears
    _ -> Nothing
  pure (Window n u)


renderWindow :: Window -> Text
renderWindow (Window n u) =
  "P" <> tshow n <> case u of
    WinDays -> "D"
    WinMonths -> "M"
    WinYears -> "Y"


-- | The window's lower bound, anchored at @now@ (months and years clip,
-- e.g. May 31 minus one month is April 30).
windowStart :: UTCTime -> Window -> UTCTime
windowStart now (Window n u) = now {utctDay = shift (utctDay now)}
  where
    k = negate (toInteger n)
    shift = case u of
      WinDays -> addDays k
      WinMonths -> addGregorianMonthsClip k
      WinYears -> addGregorianYearsClip k


-- ---------------------------------------------------------------------------
-- Corpus-aware checking (§17.4)
-- ---------------------------------------------------------------------------

-- | One deployment-log entry, parsed: the content address ('lsHash', the
-- canonical text's schema hash) and the elaborated schema.
data LoggedSchema = LoggedSchema
  { lsDeployedAt :: UTCTime
  , lsHash :: Text
  , lsSchema :: Schema
  }


data CheckConfig = CheckConfig
  { ccMode :: CheckMode
  , ccTransitive :: Bool
  -- ^ Check against every logged schema in the window, not only the
  -- newest (§17.3). Without a 'ccWindow' the window is unbounded.
  , ccWindow :: Maybe Window
  , ccNow :: UTCTime
  -- ^ Anchors the transitive window and gates deprecated removals (§17.5).
  , ccBudgets :: Budgets
  -- ^ Budgets for corpus recompilation; the origin's own, ideally.
  }


-- | The corpus blast radius attributed to one change (§17.4).
data CorpusImpact = CorpusImpact
  { ciTexts :: [Text]
  , ciAggregateHits :: Natural
  , ciNewestClient :: Maybe Text
  }
  deriving stock (Eq, Show)


data ReportedChange = ReportedChange
  { rcChange :: Change
  , rcCorpus :: CorpusImpact
  }
  deriving stock (Eq, Show)


-- | The §17.4 report: @pass@ iff no unoverridden breaking change.
data Report = Report
  { repPass :: Bool
  , repMode :: CheckMode
  , repTransitive :: Bool
  , repBaselines :: [Text]
  -- ^ Hashes of every baseline the candidate was checked against.
  , repChanges :: [ReportedChange]
  }
  deriving stock (Show)


instance A.ToJSON CorpusImpact where
  toJSON ci =
    A.object
      [ "texts" .= ciTexts ci
      , "aggregateHits" .= ciAggregateHits ci
      , "newestClient" .= ciNewestClient ci
      ]


instance A.ToJSON ReportedChange where
  toJSON (ReportedChange c ci) =
    A.object
      [ "axis" .= axisName (chAxis c)
      , "subject" .= chSubject c
      , "detail" .= chDetail c
      , "breaking" .= chBreaking c
      , "override" .= chOverride c
      , "corpus" .= ci
      ]


instance A.ToJSON Report where
  toJSON r =
    A.object
      [ "pass" .= repPass r
      , "mode" .= renderCheckMode (repMode r)
      , "transitive" .= repTransitive r
      , "baselines" .= repBaselines r
      , "changes" .= repChanges r
      ]


{- | Check a candidate schema against the deployment log and corpus
(§17.3–§17.5). Baselines: the newest logged schema, plus — transitively —
every other logged schema within the window. An empty log passes
vacuously (nothing to be compatible with). Removals of elements the
baseline had @\@deprecated@ pass once past sunset regardless of corpus
('chOverride' records the gate); before sunset, ordinary corpus rules
apply. The corpus is recompiled against the newest baseline and the
candidate once, whatever the mode.
-}
checkSchemas :: CheckConfig -> [LoggedSchema] -> [CorpusEntry] -> Schema -> Report
checkSchemas cfg logged corpus cand =
  Report
    { repPass = all passes reported
    , repMode = ccMode cfg
    , repTransitive = ccTransitive cfg
    , repBaselines = map lsHash baselines
    , repChanges = reported
    }
  where
    baselines = case sortOn (Down . lsDeployedAt) logged of
      [] -> []
      newest : rest
        | not (ccTransitive cfg) -> [newest]
        | otherwise ->
            newest : case ccWindow cfg of
              Nothing -> rest
              Just w -> filter (\ls -> lsDeployedAt ls >= windowStart (ccNow cfg) w) rest

    infos = map (entryInfo (lsSchema <$> listToMaybe baselines)) corpus

    raws = dedupe (concatMap perBaseline baselines)
    perBaseline b = case ccMode cfg of
      ClientBackward -> backward b
      ServerForward -> forward b
      Full -> backward b <> forward b
    backward b = map (gate (lsSchema b)) (diffRawWith cand True (lsSchema b) cand)
    forward b = map fwdTag (diffRawWith cand False cand (lsSchema b))
    fwdTag rc =
      rc {rawChange = (rawChange rc) {chDetail = "(server-forward) " <> chDetail (rawChange rc)}}

    -- §17.5: a breaking removal of a baseline-deprecated element passes
    -- once past sunset; the override text records the gate.
    gate base rc
      | c <- rawChange rc
      , chBreaking c
      , isNothing (chOverride c)
      , Just p <- rawRemovedAt rc
      , Just d <- Map.lookup p (schemaDeprecations base)
      , depSunset d <= utctDay (ccNow cfg) =
          rc {rawChange = c {chOverride = Just ("deprecated (sunset " <> tshow (depSunset d) <> ") — past sunset")}}
      | otherwise = rc

    -- One report row per (axis, subject, detail); an unresolved break from
    -- any baseline wins over a resolved duplicate from another.
    dedupe = Map.elems . foldl' step Map.empty
      where
        step m rc = Map.insertWith prefer (key rc) rc m
        key rc = let c = rawChange rc in (chAxis c, chSubject c, chDetail c)
        prefer neu alt
          | unresolved alt = alt
          | otherwise = neu
        unresolved rc = let c = rawChange rc in chBreaking c && isNothing (chOverride c)

    sortedRaws = sortOn (\rc -> let c = rawChange rc in (chSubject c, chAxis c, chDetail c)) raws
    reported = map (\rc -> ReportedChange (rawChange rc) (impactFor rc)) sortedRaws
    passes (ReportedChange c _) = not (chBreaking c) || isJust (chOverride c)

    -- Recompile one corpus entry under the newest baseline and the
    -- candidate; 'planId' movement is the plan-axis signal.
    entryInfo mbase e = EntryInfo e broke moved
      where
        planOf s =
          either (const Nothing) (Just . planId) $
            compileText s (ccBudgets cfg) (ceText e) >>= planQuery s (ccBudgets cfg)
        candP = planOf cand
        baseP = mbase >>= planOf
        broke = isJust baseP && isNothing candP
        moved = case (baseP, candP) of
          (Just a, Just b) -> a /= b
          _ -> False

    impactFor rc = CorpusImpact texts hits newest
      where
        c = rawChange rc
        token = case reverse (T.splitOn "." (chSubject c)) of
          t : _ | not (T.null t) -> t
          _ -> chSubject c
        mentions ei = token `T.isInfixOf` ceText (eiEntry ei)
        affected = case chAxis c of
          AxisCompile
            | rawIdentity rc -> filter mentions infos
            | chBreaking c -> filter (\ei -> eiBroke ei && mentions ei) infos
            | otherwise -> []
          AxisPlan -> filter (\ei -> eiMoved ei && mentions ei) infos
          _
            | chBreaking c -> filter mentions infos
            | otherwise -> []
        texts = map (ceText . eiEntry) affected
        hits = sum (map (ceHits . eiEntry) affected)
        newest = case concatMap (ceClients . eiEntry) affected of
          [] -> Nothing
          cs -> Just (maximum cs)


-- | A corpus entry's recompile fate under baseline and candidate.
data EntryInfo = EntryInfo
  { eiEntry :: CorpusEntry
  , eiBroke :: Bool
  , eiMoved :: Bool
  }


tshow :: Show a => a -> Text
tshow = T.pack . show
