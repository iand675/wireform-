{- | Static validation (spec §4.8's eight rules) and the compile-rejection
vocabulary shared by validation, canonicalization, and planning.

Rejections for structural reasons are deterministic per canonical text and
therefore negatively cacheable (§10.8, §14.1).

Also home to the selection-context machinery ('SelCtx', 'fieldInContext')
shared with "Lattice.Canonical" (default erasure walks the same typed
tree) and available to the planner.

Validation expects an import-free document ('Lattice.Canonical.expandImports'
runs first); a document with unresolved imports is rejected outright.
-}
module Lattice.Query.Validate (
  CompileError (..),
  compileRejected,
  budgetExceeded,
  validateDocument,

  -- * Selection contexts (shared with canonicalization and planning)
  SelCtx (..),
  ResolvedField (..),
  targetContext,
  namedContext,
  fieldInContext,

  -- * The implicit @nodes@ root (§14.4)
  nodesRootName,
  nodesRefsArg,
  nodesRootDef,
  isNodesRootDef,
  nodesListedTypes,

  -- * Surface normalization helpers
  normalizeTypeAlias,
  normalizeLimitArg,
) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Query.AST
import Lattice.Schema
import Lattice.Types
import Numeric.Natural (Natural)


-- | A whole-request compile failure (problem type + diagnostics).
data CompileError = CompileError
  { ceCode :: Text
  -- ^ @lattice:compile-rejected@, @lattice:compile-budget@,
  -- @lattice:fragment-shadow@ …
  , ceStatus :: Int
  , ceDiagnostics :: [Text]
  }
  deriving stock (Eq, Show)


compileRejected :: [Text] -> CompileError
compileRejected = CompileError "lattice:compile-rejected" 400


budgetExceeded :: [Text] -> CompileError
budgetExceeded = CompileError "lattice:compile-budget" 503


-- ---------------------------------------------------------------------------
-- Selection contexts
-- ---------------------------------------------------------------------------

{- | The type a selection set selects against: a concrete entity, a declared
interface, or an inline union (an anonymous interface with no declared
fields).
-}
data SelCtx
  = CtxEntity TypeName EntityDef
  | CtxIface InterfaceName InterfaceDef
  | CtxUnion (NonEmpty TypeName)


ctxDescribe :: SelCtx -> Text
ctxDescribe = \case
  CtxEntity (TypeName t) _ -> t
  CtxIface (InterfaceName i) _ -> i
  CtxUnion ts -> "(" <> joinBar (NE.toList ts) <> ")"
  where
    joinBar = foldr1 (\a b -> a <> " | " <> b) . map unTypeName


-- | A field resolved against a 'SelCtx'.
data ResolvedField
  = RScalar FieldDef
  | RToOne Target
  | -- | The 'CollectionDef' is a representative: for union contexts the
    -- members' collections agree on link, grouping, and windowing (checked
    -- by 'fieldInContext'); 'colName' may differ and must not be relied on.
    RToMany Target CollectionDef


-- | Resolve a relationship/root target to a selection context.
targetContext :: Schema -> Target -> Either Text SelCtx
targetContext schema = \case
  TargetEntity t ->
    case lookupEntity schema t of
      Just ed -> Right (CtxEntity t ed)
      Nothing -> Left ("unknown entity type '" <> unTypeName t <> "'")
  TargetInterface i ->
    case Map.lookup i (schemaInterfaces schema) of
      Just idf -> Right (CtxIface i idf)
      Nothing -> Left ("unknown interface '" <> unInterfaceName i <> "'")
  TargetUnion ts -> Right (CtxUnion ts)


-- | Resolve a bare name (a fragment's @on@ type) to a selection context.
namedContext :: Schema -> Text -> Either Text SelCtx
namedContext schema nm
  | Just ed <- lookupEntity schema (TypeName nm) = Right (CtxEntity (TypeName nm) ed)
  | Just idf <- Map.lookup (InterfaceName nm) (schemaInterfaces schema) =
      Right (CtxIface (InterfaceName nm) idf)
  | otherwise = Left ("'" <> nm <> "' names neither an entity nor an interface")


{- | Resolve a field name against a context. Interface contexts see only
interface-declared fields (§4.8 rule 5). Union contexts see the fields
every member declares compatibly: identical scalar types and argument
lists, or relationships agreeing on target, link, grouping, and
windowing (this is what lets @hero { name friends(first: 10) { … } }@
select through an inline union).
-}
fieldInContext :: Schema -> SelCtx -> FieldName -> Either Text ResolvedField
fieldInContext schema ctx fn = case ctx of
  CtxEntity t ed -> entityField t ed
  CtxIface i idf ->
    case Map.lookup fn (ifaceFields idf) of
      Just fd -> Right (RScalar fd)
      Nothing -> case Map.lookup fn (ifaceRels idf) of
        Just rel -> Right (relResolved rel)
        Nothing ->
          Left
            ( "field '"
                <> unFieldName fn
                <> "' is not declared by interface '"
                <> unInterfaceName i
                <> "' (§4.8 rule 5)"
            )
  CtxUnion ts -> do
    members <- traverse memberField (NE.toList ts)
    unify members
  where
    entityField t ed =
      case lookupEntityField ed fn of
        Just fd -> Right (RScalar fd)
        Nothing -> case lookupEntityRel ed fn of
          Just rel -> Right (relResolved rel)
          Nothing ->
            Left
              ( "entity '"
                  <> unTypeName t
                  <> "' has no field '"
                  <> unFieldName fn
                  <> "'"
              )
    memberField t = case lookupEntity schema t of
      Nothing -> Left ("unknown entity type '" <> unTypeName t <> "'")
      Just ed -> entityField t ed
    relResolved = \case
      ToOne {relTarget} -> RToOne relTarget
      ToMany {relTarget, relCollection} -> RToMany relTarget relCollection
    unify [] = Left ("field '" <> unFieldName fn <> "' resolved against an empty union")
    unify (r : rs) =
      let ok = all (agrees r) rs
       in if ok
            then Right r
            else
              Left
                ( "field '"
                    <> unFieldName fn
                    <> "' is not declared compatibly by every member of "
                    <> ctxDescribe ctx
                )
    agrees (RScalar a) (RScalar b) =
      fieldType a == fieldType b && fieldArgs a == fieldArgs b
    agrees (RToOne a) (RToOne b) = a == b
    agrees (RToMany ta ca) (RToMany tb cb) =
      ta == tb
        && colLink ca == colLink cb
        && colGrouping ca == colGrouping cb
        && colWindow ca == colWindow cb
    agrees _ _ = False


-- ---------------------------------------------------------------------------
-- The implicit @nodes@ root (§14.4)
-- ---------------------------------------------------------------------------

{- | Every origin serves the protocol-level batched entity root
@nodes(refs: [EntityRef])@ (§14.4). It is never declared in the IDL; the
validator and planner inject it, so it exists exactly when the schema does
NOT declare a root of the same name (a declared @nodes@ root shadows the
implicit one).

The wire form of an @EntityRef@ is the ordinary ref string @\"Type:key\"@:
inline as string literals, or bound from one variable declared
@$refs:EntityRef@ (the protocol-level type name; there is no list spelling
in variable declarations, and the binding is a JSON array of ref strings).
Selection dispatches per concrete type with inline fragments exactly like
an interface target; the dispatch union is the set of types the selection
names ('nodesListedTypes').
-}
nodesRootName :: RootName
nodesRootName = RootName "nodes"


-- | The single declared argument of the implicit root: @refs@, required.
nodesRefsArg :: ArgDef
nodesRefsArg =
  ArgDef
    { adName = "refs"
    , adType = TNamed (TypeName "EntityRef")
    , adDefault = Nothing
    }


{- | The synthesized §14.4 'RootDef' over the selection's dispatch union.
'rootPolicy' is 'Public': the root itself is reachable by anyone; access is
gated per entity type by 'entityFetchBy' ('Nothing' forbids), which joins
into the path exactly like a root policy would — per type.
-}
nodesRootDef :: NonEmpty TypeName -> RootDef
nodesRootDef ts =
  RootDef
    { rootKind = RootGet
    , rootTarget = TargetUnion ts
    , rootParams = [nodesRefsArg]
    , rootCollection = Nothing
    , rootPolicy = Public
    }


{- | Is this 'RootDef' the synthesized implicit @nodes@ root? The @refs@
parameter's protocol-level @EntityRef@ type cannot be declared in an IDL
(unknown named types are rejected at elaboration), so the parameter list
is a reliable marker.
-}
isNodesRootDef :: RootDef -> Bool
isNodesRootDef rd = rootKind rd == RootGet && rootParams rd == [nodesRefsArg]


{- | The concrete types a @nodes@ selection dispatches over: its top-level
inline-fragment alternatives, first occurrence order, deduplicated.
-}
nodesListedTypes :: SelectionSet -> [TypeName]
nodesListedTypes sels = dedup Set.empty [t | SInline t _ <- sels]
  where
    dedup _ [] = []
    dedup seen (t : rest)
      | Set.member t seen = dedup seen rest
      | otherwise = t : dedup (Set.insert t seen) rest


-- ---------------------------------------------------------------------------
-- Surface normalization
-- ---------------------------------------------------------------------------

{- | GraphQL-familiar spellings accepted for the canonical prim names
(§3.4): the canonical text always uses the right-hand names.
-}
normalizeTypeAlias :: Text -> Text
normalizeTypeAlias = \case
  "Int" -> "I32"
  "String" -> "Text"
  "Float" -> "F64"
  "Boolean" -> "Bool"
  t -> t


{- | §4.2 spells the forward page-size argument @limit@; rule 6 (and the
rest of the spec) names it @first@. @limit@ is accepted as a surface
synonym on paginated collections and normalized to @first@ — unless the
field genuinely declares an argument named @limit@ (the declared names
are the first parameter).
-}
normalizeLimitArg :: [ArgName] -> [Argument] -> [Argument]
normalizeLimitArg declared args
  | ArgName "limit" `elem` declared = args
  | otherwise = map rename args
  where
    rename a
      | argName a == ArgName "limit" = a {argName = ArgName "first"}
      | otherwise = a


-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

-- | Accumulated diagnostics and the set of variables seen in use.
type W = ([Text], Set VarName)


diag :: Text -> W
diag t = ([t], Set.empty)


reservedNames :: [Text]
reservedNames = ["query", "fragment", "import", "on", "true", "false"]


-- | URL parameter names a query variable must not collide with (§6.1/§6.6).
reservedUrlParams :: [Text]
reservedUrlParams = ["p", "slice", "vc", "project", "live", "d", "dv", "intent"]


pageArgNames :: [ArgName]
pageArgNames = ["first", "last", "after", "before", "around"]


{- | Check static rules 1–8 (§4.8) against the schema: single query
(structural in 'Document'), reserved names, \@depth placement,
scalar\/edge selection shapes, declared-arguments-only (with
pagination-argument exclusivity), variable use\/declaration agreement,
spread resolution and acyclicity, and fragment shadowing — plus the
pre-plan budgets (root count, traversal depth including \@depth
expansion) and the reserved-URL-parameter rule for variable names.
-}
validateDocument :: Schema -> Budgets -> Document -> Either CompileError ()
validateDocument schema budgets Document {..} = do
  checkImports
  checkShadow
  case allDiags of
    [] -> Right ()
    ds -> Left (compileRejected ds)
  where
    checkImports = case docImports of
      [] -> Right ()
      _ ->
        Left
          ( compileRejected
              ["document has unresolved imports; expand imports before compiling"]
          )

    checkShadow =
      case mapMaybe shadowOf docFragments of
        [] -> Right ()
        ds -> Left (CompileError "lattice:fragment-shadow" 400 ds)
    shadowOf f = case Map.lookup (fdName f) (schemaFragments schema) of
      Just sf
        | fragOn sf == fdOn f ->
            Just
              ( "local fragment '"
                  <> unFragmentName (fdName f)
                  <> "' shadows the schema fragment of the same name on '"
                  <> fdOn f
                  <> "' (§4.5)"
              )
      _ -> Nothing

    localFrags :: Map FragmentName FragmentDefQ
    localFrags =
      foldl' (\m f -> Map.insertWith (\_ old -> old) (fdName f) f m) Map.empty docFragments

    dupFragDiags =
      dupDiags "fragment" (map (unFragmentName . fdName) docFragments)

    allDiags =
      dupFragDiags
        <> reservedValueDiags
        <> urlParamDiags
        <> varDeclDiags
        <> cycleDiags
        <> fst queryW
        <> unusedVarDiags
        <> fragmentDiags
        <> budgetDiags

    -- Rule 2 re-check on the AST (the parser already enforces this for
    -- parsed text; programmatically built documents get the same rules).
    reservedValueDiags =
      concatMap fragNameDiag docFragments
        <> concatMap (valueDiags . vdDefault) (qVars docQuery)
        <> concatMap (\f -> concatMap (valueDiags . vdDefault) (fdParams f)) docFragments
        <> selValues (qSelection docQuery)
        <> concatMap (selValues . fdSelection) docFragments
    fragNameDiag f
      | unFragmentName (fdName f) `elem` reservedNames =
          [ "reserved name '"
              <> unFragmentName (fdName f)
              <> "' cannot be used as a fragment name (§4.8 rule 2)"
          ]
      | otherwise = []
    selValues = concatMap $ \case
      SField f ->
        concatMap (valueDiags . Just . argValue) (fArgs f)
          <> maybe [] selValues (fSelection f)
      SInline _ ss -> selValues ss
      SSpread _ args -> concatMap (valueDiags . Just . argValue) args
    valueDiags = \case
      Nothing -> []
      Just v -> go v
      where
        go = \case
          QEnum e
            | e `elem` reservedNames ->
                ["reserved name '" <> e <> "' cannot be used as an enum value (§4.8 rule 2)"]
            | otherwise -> []
          QVar (VarName v)
            | v `elem` reservedNames ->
                ["reserved name '" <> v <> "' cannot be used as a variable name (§4.8 rule 2)"]
            | otherwise -> []
          QList vs -> concatMap go vs
          _ -> []

    -- Variable names must not collide with reserved URL parameters, and
    -- variable defaults must be constant.
    urlParamDiags = concatMap urlDiag (qVars docQuery)
    urlDiag vd
      | unVarName (vdName vd) `elem` reservedUrlParams =
          [ "variable '$"
              <> unVarName (vdName vd)
              <> "' collides with a reserved URL parameter name (p, slice, vc, project, live, d, dv, intent)"
          ]
      | otherwise = []

    varDeclDiags =
      dupDiags "variable" (map (unVarName . vdName) (qVars docQuery))
        <> concatMap (constDefaultDiag "variable") (qVars docQuery)
        <> concatMap
          (\f -> concatMap (constDefaultDiag ("parameter of fragment '" <> unFragmentName (fdName f) <> "'")) (fdParams f))
          docFragments
    constDefaultDiag what vd = case vdDefault vd of
      Just v
        | containsVar v ->
            [ "default of "
                <> what
                <> " '$"
                <> unVarName (vdName vd)
                <> "' must be a constant value"
            ]
      _ -> []
    containsVar = \case
      QVar _ -> True
      QList vs -> any containsVar vs
      _ -> False

    -- Rule 8: spread graph acyclicity over local fragments.
    cycleDiags = case findCycle of
      Nothing -> []
      Just n ->
        ["fragment spread cycle involving '" <> unFragmentName n <> "' (§4.8 rule 8)"]
    findCycle = goAll (Map.keys localFrags) Set.empty
      where
        goAll [] _ = Nothing
        goAll (n : ns) done
          | n `Set.member` done = goAll ns done
          | otherwise = case visit n Set.empty done of
              Left bad -> Just bad
              Right done' -> goAll ns done'
        visit n stack done
          | n `Set.member` stack = Left n
          | n `Set.member` done = Right done
          | otherwise =
              case Map.lookup n localFrags of
                Nothing -> Right (Set.insert n done)
                Just f ->
                  let succs = localSpreads (fdSelection f)
                      step acc s = acc >>= visit s (Set.insert n stack)
                   in Set.insert n <$> foldl' step (Right done) succs
        localSpreads = concatMap $ \case
          SField f -> maybe [] localSpreads (fSelection f)
          SInline _ ss -> localSpreads ss
          SSpread s _ -> if Map.member s localFrags then [s] else []

    spreadsResolve = cycleFree && allResolve (qSelection docQuery) && all (allResolve . fdSelection) docFragments
    cycleFree = null cycleDiags
    allResolve = all $ \case
      SField f -> maybe True allResolve (fSelection f)
      SInline _ ss -> allResolve ss
      SSpread n _ ->
        Map.member n localFrags || Map.member n (schemaFragments schema)

    -- The typed walk over the query selection (roots) and each local
    -- fragment body (in its own @on@ context, with its own parameters).
    queryEnv =
      VarEnv "the query" (Map.fromList (map (\v -> (vdName v, v)) (qVars docQuery)))

    queryW =
      mconcat (map walkRoot (qSelection docQuery))

    unusedVarDiags = concatMap unusedOf (qVars docQuery)
      where
        used = snd queryW
        unusedOf vd
          | vdName vd `Set.member` used = []
          | otherwise =
              [ "variable '$"
                  <> unVarName (vdName vd)
                  <> "' is declared but never used (§4.8 rule 7)"
              ]

    fragmentDiags = concatMap fragW docFragments
    fragW f =
      let fname = unFragmentName (fdName f)
          env =
            VarEnv
              ("fragment '" <> fname <> "'")
              (Map.fromList (map (\v -> (vdName v, v)) (fdParams f)))
          dups = dupDiags ("parameter of fragment '" <> fname <> "'") (map (unVarName . vdName) (fdParams f))
       in case namedContext schema (fdOn f) of
            Left e -> dups <> ["fragment '" <> fname <> "': " <> e]
            Right ctx ->
              let (ds, used) = walkSet env ctx (fdSelection f)
                  unusedP vd
                    | vdName vd `Set.member` used = []
                    | otherwise =
                        [ "parameter '$"
                            <> unVarName (vdName vd)
                            <> "' of fragment '"
                            <> fname
                            <> "' is declared but never used"
                        ]
               in dups <> ds <> concatMap unusedP (fdParams f)

    -- Budgets checkable before canonicalization (§14.1): root count and
    -- traversal depth, @depth expansion and spreads counted fully.
    budgetDiags = rootBudget <> depthBudget
    rootBudget =
      let n = length (qSelection docQuery)
       in if fromIntegral n > maxRoots budgets
            then
              [ "query has "
                  <> tshow n
                  <> " roots; maxRoots is "
                  <> tshow (maxRoots budgets)
              ]
            else []
    depthBudget
      | not spreadsResolve = []
      | otherwise =
          let d = depthOfSet Set.empty (qSelection docQuery)
           in if d > maxDepth budgets
                then
                  [ "traversal depth "
                      <> tshow d
                      <> " (including @depth expansion) exceeds maxDepth "
                      <> tshow (maxDepth budgets)
                  ]
                else []

    depthOfSet :: Set FragmentName -> SelectionSet -> Natural
    depthOfSet active ss = extra + base
      where
        extra = sum (map extraOf ss)
        extraOf = \case
          SField f | Just n <- fDepth f, n > 0 -> fromIntegral n
          _ -> 0
        base = maximum (1 : map baseOf ss)
        baseOf = \case
          SField f -> case (fDepth f, fSelection f) of
            (Just _, _) -> 1
            (_, Just sub) -> 1 + depthOfSet active sub
            _ -> 1
          SInline _ sub -> depthOfSet active sub
          SSpread n _
            | n `Set.member` active -> 1
            | Just f <- Map.lookup n localFrags ->
                depthOfSet (Set.insert n active) (fdSelection f)
            | Just sf <- Map.lookup n (schemaFragments schema) ->
                depthOfSet (Set.insert n active) (fragSelection sf)
            | otherwise -> 1

    -- ---------------------------------------------------------------
    -- Root fields
    -- ---------------------------------------------------------------

    walkRoot :: Selection -> W
    walkRoot = \case
      SInline {} -> diag "top-level selections must be root fields, not inline fragments"
      SSpread {} -> diag "top-level selections must be root fields, not fragment spreads"
      SField f ->
        let rn = unFieldName (fName f)
         in case Map.lookup (RootName rn) (schemaRoots schema) of
              Just rd -> rootField f rd
              Nothing
                | RootName rn == nodesRootName -> nodesRoot f
                | otherwise -> diag ("unknown root field '" <> rn <> "'")

    rootField :: Field -> RootDef -> W
    rootField f rd =
      let rn = unFieldName (fName f)
          desc = "root '" <> rn <> "'"
          depthD = case fDepth f of
            Nothing -> mempty
            Just _ -> diag ("@depth cannot appear on " <> desc <> " (§4.8 rule 4)")
          argsW = case (rootKind rd, rootCollection rd) of
            (RootList, Just col) ->
              checkArgs
                queryEnv
                ArgTarget
                  { atDesc = desc
                  , atDeclared = rootParams rd
                  , atCollection = Just (rootTarget rd, col, True)
                  }
                (fArgs f)
            _ ->
              checkArgs
                queryEnv
                ArgTarget
                  { atDesc = desc
                  , atDeclared = rootParams rd
                  , atCollection = Nothing
                  }
                (fArgs f)
          subW = case fSelection f of
            Nothing
              | Nothing <- fDepth f ->
                  diag (desc <> " requires a selection set (§4.8 rule 5)")
              | otherwise -> mempty
            Just sub -> case targetContext schema (rootTarget rd) of
              Left e -> diag (desc <> ": " <> e)
              Right ctx -> walkSet queryEnv ctx sub
       in depthD <> argsW <> subW

    -- The implicit @nodes@ root (§14.4): exactly the declared-root rules
    -- against the synthesized definition, except that the dispatch union
    -- is drawn from the selection itself — the types its inline fragments
    -- name. A selection with no inline fragments has no union to check
    -- bare fields against and is rejected.
    nodesRoot :: Field -> W
    nodesRoot f =
      let desc = "root 'nodes'"
          depthD = case fDepth f of
            Nothing -> mempty
            Just _ -> diag ("@depth cannot appear on " <> desc <> " (§4.8 rule 4)")
          argsW =
            checkArgs
              queryEnv
              ArgTarget
                { atDesc = desc
                , atDeclared = [nodesRefsArg]
                , atCollection = Nothing
                }
              (fArgs f)
          subW = case fSelection f of
            Nothing -> diag (desc <> " requires a selection set (§4.8 rule 5)")
            Just sub -> case NE.nonEmpty (nodesListedTypes sub) of
              Nothing ->
                diag
                  ( desc
                      <> " dispatches per concrete type; select with inline fragments (§14.4)"
                  )
              Just ts -> walkSet queryEnv (CtxUnion ts) sub
       in depthD <> argsW <> subW

    -- ---------------------------------------------------------------
    -- Selection sets
    -- ---------------------------------------------------------------

    walkSet :: VarEnv -> SelCtx -> SelectionSet -> W
    walkSet env ctx = mconcat . map (walkSel env ctx)

    walkSel :: VarEnv -> SelCtx -> Selection -> W
    walkSel env ctx = \case
      SField f -> case fieldInContext schema ctx (fName f) of
        Left e -> diag e
        Right rf -> checkField env ctx f rf
      SInline t sub -> inlineFrag env ctx t sub
      SSpread n args -> spread env ctx n args

    inlineFrag :: VarEnv -> SelCtx -> TypeName -> SelectionSet -> W
    inlineFrag env ctx t sub =
      let recur = case lookupEntity schema t of
            Nothing -> diag ("unknown entity type '" <> unTypeName t <> "' in inline fragment")
            Just ed -> walkSet env (CtxEntity t ed) sub
       in case ctx of
            CtxEntity e _
              | e == t -> recur
              | otherwise ->
                  diag
                    ( "inline fragment on '"
                        <> unTypeName t
                        <> "' inside a selection on '"
                        <> unTypeName e
                        <> "'"
                    )
            CtxIface i idf
              | t `Set.member` ifaceMemberSet idf -> recur
              | otherwise ->
                  diag
                    ( "'"
                        <> unTypeName t
                        <> "' does not implement interface '"
                        <> unInterfaceName i
                        <> "' (§4.8 rule 5)"
                    )
            CtxUnion ts
              | t `elem` NE.toList ts -> recur
              | otherwise ->
                  diag
                    ( "'"
                        <> unTypeName t
                        <> "' is not a member of "
                        <> ctxDescribe ctx
                    )

    spread :: VarEnv -> SelCtx -> FragmentName -> [Argument] -> W
    spread env ctx n args =
      case Map.lookup n localFrags of
        Just f -> spreadTo env ctx n args (fdOn f) (fdParams f)
        Nothing -> case Map.lookup n (schemaFragments schema) of
          Just sf -> spreadTo env ctx n args (fragOn sf) (fragParams sf)
          Nothing ->
            diag
              ( "fragment '"
                  <> unFragmentName n
                  <> "' is not defined in the document or the schema (§4.8 rule 8)"
              )

    spreadTo :: VarEnv -> SelCtx -> FragmentName -> [Argument] -> Text -> [VarDef] -> W
    spreadTo env ctx n args onTy params =
      let fname = unFragmentName n
          compatible = case ctx of
            CtxEntity t _ ->
              onTy == unTypeName t || implementsIface t onTy
            CtxIface i _ -> onTy == unInterfaceName i
            CtxUnion ts -> all (\t -> implementsIface t onTy) (NE.toList ts)
          compatD =
            if compatible
              then mempty
              else
                diag
                  ( "fragment '"
                      <> fname
                      <> "' on '"
                      <> onTy
                      <> "' cannot be spread inside a selection on '"
                      <> ctxDescribe ctx
                      <> "' (§4.8 rule 8)"
                  )
       in compatD <> checkSpreadArgs env fname args params

    implementsIface :: TypeName -> Text -> Bool
    implementsIface t iname =
      case Map.lookup (InterfaceName iname) (schemaInterfaces schema) of
        Just idf -> t `Set.member` ifaceMemberSet idf
        Nothing -> False

    checkSpreadArgs :: VarEnv -> Text -> [Argument] -> [VarDef] -> W
    checkSpreadArgs env fname args params =
      let paramMap = Map.fromList (map (\p -> (unVarName (vdName p), p)) params)
          dups = (dupDiags ("argument of fragment '" <> fname <> "'") (map (unArgName . argName) args), Set.empty)
          perArg (Argument (ArgName an) v) =
            case Map.lookup an paramMap of
              Nothing ->
                diag
                  ( "fragment '"
                      <> fname
                      <> "' has no parameter '"
                      <> an
                      <> "'"
                  )
              Just p ->
                varUsesRef
                  env
                  ("argument '" <> an <> "' of fragment '" <> fname <> "'")
                  (vdType p)
                  (isJust (vdDefault p))
                  v
          missing p =
            let required = not (trOptional (vdType p)) && not (isJust (vdDefault p))
                given = any (\a -> unArgName (argName a) == unVarName (vdName p)) args
             in if required && not given
                  then
                    [ "fragment '"
                        <> fname
                        <> "' requires parameter '$"
                        <> unVarName (vdName p)
                        <> "'"
                    ]
                  else []
       in dups <> mconcat (map perArg args) <> (concatMap missing params, Set.empty)

    -- ---------------------------------------------------------------
    -- Fields
    -- ---------------------------------------------------------------

    checkField :: VarEnv -> SelCtx -> Field -> ResolvedField -> W
    checkField env ctx f rf =
      let fname = unFieldName (fName f)
          desc = "field '" <> fname <> "' of " <> ctxDescribe ctx
       in case rf of
            RScalar fd ->
              let selD = case fSelection f of
                    Just _ -> diag ("scalar " <> desc <> " takes no selection set (§4.8 rule 5)")
                    Nothing -> mempty
                  depD = case fDepth f of
                    Just _ -> diag ("@depth cannot appear on scalar " <> desc <> " (§4.8 rule 4)")
                    Nothing -> mempty
                  argsW =
                    checkArgs
                      env
                      ArgTarget {atDesc = desc, atDeclared = fieldArgs fd, atCollection = Nothing}
                      (fArgs f)
               in selD <> depD <> argsW
            RToOne target ->
              let argsW =
                    checkArgs
                      env
                      ArgTarget {atDesc = desc, atDeclared = [], atCollection = Nothing}
                      (fArgs f)
               in argsW <> edgeShape env ctx f desc target
            RToMany target col ->
              let argsW =
                    checkArgs
                      env
                      ArgTarget
                        { atDesc = desc
                        , atDeclared = []
                        , atCollection = Just (target, col, False)
                        }
                      (fArgs f)
               in argsW <> edgeShape env ctx f desc target

    -- Rule 4 (@depth placement) and rule 5 (edges require a selection set
    -- or @depth), then the recursive walk into the edge's target.
    edgeShape :: VarEnv -> SelCtx -> Field -> Text -> Target -> W
    edgeShape env ctx f desc target = case fDepth f of
      Just n ->
        let selD = case fSelection f of
              Just _ -> diag (desc <> " carries @depth and must not carry a selection set (§4.8 rule 4)")
              Nothing -> mempty
            posD =
              if n < 1
                then diag ("@depth on " <> desc <> " must be a positive integer")
                else mempty
            tgtD =
              if depthTargetOk ctx target
                then mempty
                else
                  diag
                    ( desc
                        <> " carries @depth but does not target the enclosing selection's type '"
                        <> ctxDescribe ctx
                        <> "' (§4.8 rule 4)"
                    )
         in selD <> posD <> tgtD
      Nothing -> case fSelection f of
        Nothing -> diag ("edge " <> desc <> " requires a selection set (§4.8 rule 5)")
        Just sub -> case targetContext schema target of
          Left e -> diag (desc <> ": " <> e)
          Right ctx' -> walkSet env ctx' sub

    depthTargetOk :: SelCtx -> Target -> Bool
    depthTargetOk ctx target = case (ctx, target) of
      (CtxEntity t _, TargetEntity t') -> t == t'
      (CtxIface i _, TargetInterface i') -> i == i'
      (CtxUnion ts, TargetUnion ts') ->
        Set.fromList (NE.toList ts) == Set.fromList (NE.toList ts')
      _ -> False

    -- ---------------------------------------------------------------
    -- Arguments (rule 6) and variable uses (rule 7)
    -- ---------------------------------------------------------------

    checkArgs :: VarEnv -> ArgTarget -> [Argument] -> W
    checkArgs env ArgTarget {..} rawArgs =
      let paginated = case atCollection of
            Just (_, col, _) | Paginated _ <- colWindow col -> True
            _ -> False
          args =
            if paginated
              then normalizeLimitArg (map adName atDeclared) rawArgs
              else rawArgs
          names = map argName args
          dups = dupDiags ("argument of " <> atDesc) (map unArgName names)
          declaredMap = Map.fromList (map (\ad -> (adName ad, ad)) atDeclared)
          groupSlots = groupingSlots
          perArg (Argument an v) =
            case Map.lookup an declaredMap of
              Just ad ->
                let site = "argument '" <> unArgName an <> "' of " <> atDesc
                 in varUsesField env site (adType ad) (isJust (adDefault ad)) v
                      <> (emptyList1Diags schema site (adType ad) v, Set.empty)
              Nothing
                | paginated && an `elem` pageArgNames ->
                    varUsesField
                      env
                      ("argument '" <> unArgName an <> "' of " <> atDesc)
                      (pageArgType an)
                      True
                      v
                | Just ft <- Map.lookup an groupSlots ->
                    varUsesField
                      env
                      ("argument '" <> unArgName an <> "' of " <> atDesc)
                      ft
                      True
                      v
                | bounded && an `elem` pageArgNames ->
                    diag
                      ( atDesc
                          <> " is a bounded collection and takes no pagination arguments (§3.6, §4.8 rule 6)"
                      )
                | otherwise ->
                    diag
                      ( "argument '"
                          <> unArgName an
                          <> "' of "
                          <> atDesc
                          <> " is not declared (§4.8 rule 6)"
                      )
          bounded = case atCollection of
            Just (_, col, _) | Bounded {} <- colWindow col -> True
            _ -> False
          groupingSlots :: Map ArgName FieldType
          groupingSlots = case atCollection of
            Nothing -> Map.empty
            Just (target, col, isRoot) ->
              let linked = colLink col
                  offered =
                    filter
                      (\g -> isRoot || g /= linked)
                      (NE.toList (colGrouping col))
                  notDeclared g = not (Map.member (ArgName (unFieldName g)) declaredMap)
                  slotOf g = case targetContext schema target of
                    Left _ -> Nothing
                    Right tctx -> case fieldInContext schema tctx g of
                      Right (RScalar fd) -> Just (ArgName (unFieldName g), fieldType fd)
                      _ -> Nothing
               in Map.fromList (mapMaybe slotOf (filter notDeclared offered))
          required ad =
            not (isOptionalType (adType ad)) && not (isJust (adDefault ad))
          missing ad =
            if required ad && not (adName ad `elem` names)
              then
                [ "required argument '"
                    <> unArgName (adName ad)
                    <> "' of "
                    <> atDesc
                    <> " is missing"
                ]
              else []
          has n = ArgName n `elem` names
          pageRules
            | not paginated = []
            | otherwise =
                concat
                  [ if has "first" && has "last"
                      then ["'first' and 'last' are mutually exclusive on " <> atDesc <> " (§4.8 rule 6)"]
                      else []
                  , if has "after" && not (has "first")
                      then ["'after' requires 'first' on " <> atDesc <> " (§4.8 rule 6)"]
                      else []
                  , if has "before" && not (has "last")
                      then ["'before' requires 'last' on " <> atDesc <> " (§4.8 rule 6)"]
                      else []
                  , if has "around" && (has "first" || has "last" || has "after" || has "before")
                      then ["'around' is exclusive of the other pagination arguments on " <> atDesc <> " (§4.8 rule 6)"]
                      else []
                  , defaultPageRule
                  ]
          defaultPageRule = case atCollection of
            Just (_, col, _)
              | Paginated cs <- colWindow col
              , Nothing <- csDefaultPage cs
              , not (has "first" || has "last" || has "around") ->
                  [ atDesc
                      <> " is paginated with no default page and requires a page argument (§4.2)"
                  ]
            _ -> []
       in (dups, Set.empty)
            <> mconcat (map perArg args)
            <> (concatMap missing atDeclared <> pageRules, Set.empty)

    pageArgType :: ArgName -> FieldType
    pageArgType = \case
      "first" -> TPrim PI32
      "last" -> TPrim PI32
      _ -> TPrim PCursor

    -- Variable uses against a schema 'FieldType' slot.
    varUsesField :: VarEnv -> Text -> FieldType -> Bool -> QValue -> W
    varUsesField env site ft omittable = go ft
      where
        go t = \case
          QVar v -> useVar env site (fieldTypeName t) omittable v
          QList vs -> mconcat (map (go (elemType t)) vs)
          _ -> mempty
        elemType t = case stripOptional t of
          TList e -> e
          TList1 e -> e
          TSet e -> e
          TVec _ e -> e
          other -> other

    -- Variable uses against a fragment-parameter 'TypeRefQ' slot.
    varUsesRef :: VarEnv -> Text -> TypeRefQ -> Bool -> QValue -> W
    varUsesRef env site tr omittable = go
      where
        slot = Just (normalizeTypeAlias (trName tr), trOptional tr)
        go = \case
          QVar v -> useVar env site slot omittable v
          QList vs -> mconcat (map go vs)
          _ -> mempty

    useVar :: VarEnv -> Text -> Maybe (Text, Bool) -> Bool -> VarName -> W
    useVar VarEnv {..} site slot omittable v@(VarName vn) =
      ( case Map.lookup v veDefs of
          Nothing ->
            [ "variable '$"
                <> vn
                <> "' used at "
                <> site
                <> " is not declared by "
                <> veWho
                <> " (§4.8 rule 7)"
            ]
          Just vd -> case slot of
            Nothing ->
              [site <> " has a type that cannot be bound from a variable"]
            Just (slotName, slotOptional) ->
              let vtn = normalizeTypeAlias (trName (vdType vd))
                  nameD =
                    if vtn == slotName
                      then []
                      else
                        [ "variable '$"
                            <> vn
                            <> "' has type '"
                            <> vtn
                            <> "' but "
                            <> site
                            <> " expects '"
                            <> slotName
                            <> "' (§4.8 rule 7)"
                        ]
                  optD =
                    if trOptional (vdType vd)
                      && not (slotOptional || omittable)
                      && not (isJust (vdDefault vd))
                      then
                        [ "optional variable '$"
                            <> vn
                            <> "' cannot bind the required "
                            <> site
                        ]
                      else []
               in nameD <> optD
      , Set.singleton v
      )


-- | The variable environment a selection is checked in.
data VarEnv = VarEnv
  { veWho :: Text
  , veDefs :: Map VarName VarDef
  }


-- | What a field/root offers its arguments: declared 'ArgDef's plus, for
-- collection-bearing positions, the collection (target, definition, and
-- whether this is a root — roots may pass the link/grouping key, edges
-- have it bound by traversal).
data ArgTarget = ArgTarget
  { atDesc :: Text
  , atDeclared :: [ArgDef]
  , atCollection :: Maybe (Target, CollectionDef, Bool)
  }


-- ---------------------------------------------------------------------------
-- Type-name helpers
-- ---------------------------------------------------------------------------

isOptionalType :: FieldType -> Bool
isOptionalType = \case
  TOptional _ -> True
  _ -> False


stripOptional :: FieldType -> FieldType
stripOptional = \case
  TOptional t -> t
  t -> t


{- | An empty list literal bound where a nonempty list (@[t]+@, §3.5.2)
governs it: compile-rejected, like any other argument shape violation.
Structural walk over the literal, descending list-like elements and
resolving newtypes (depth-bounded); variable /values/ are checked at bind
time instead, since a literal never sees them.
-}
emptyList1Diags :: Schema -> Text -> FieldType -> QValue -> [Text]
emptyList1Diags schema site = go (8 :: Int)
  where
    go :: Int -> FieldType -> QValue -> [Text]
    go fuel ft v
      | fuel <= 0 = []
      | otherwise = case (ft, v) of
          (TOptional t, _) -> go fuel t v
          (TList1 _, QList []) ->
            [site <> " is an empty list, but its type is a nonempty list ([t]+)"]
          (TList1 e, QList vs) -> concatMap (go fuel e) vs
          (TList e, QList vs) -> concatMap (go fuel e) vs
          (TSet e, QList vs) -> concatMap (go fuel e) vs
          (TVec _ e, QList vs) -> concatMap (go fuel e) vs
          (TNamed tn, _)
            | Just (DeclNewtype t _) <- Map.lookup tn (schemaTypes schema) ->
                go (fuel - 1) t v
          _ -> []


{- | The (canonical name, optionality) of a type as it appears in a variable
declaration, or 'Nothing' for shapes with no @TypeRef@ spelling
(lists, sets, maps, vectors).
-}
fieldTypeName :: FieldType -> Maybe (Text, Bool)
fieldTypeName = \case
  TOptional t -> fmap (\(n, _) -> (n, True)) (baseName t)
  t -> baseName t
  where
    baseName = \case
      TPrim p -> Just (primName p, False)
      TNamed (TypeName n) -> Just (n, False)
      _ -> Nothing


primName :: Prim -> Text
primName = \case
  PBool -> "Bool"
  PI8 -> "I8"
  PI16 -> "I16"
  PI32 -> "I32"
  PI64 -> "I64"
  PW8 -> "W8"
  PW16 -> "W16"
  PW32 -> "W32"
  PW64 -> "W64"
  PInteger -> "Integer"
  PDecimal -> "Decimal"
  PF32 -> "F32"
  PF64 -> "F64"
  PText -> "Text"
  PBytes _ -> "Bytes"
  PBit _ -> "Bit"
  PUuid -> "Uuid"
  PTimestamp -> "Timestamp"
  PDate -> "Date"
  PTimeOfDay -> "TimeOfDay"
  PDuration -> "Duration"
  PCursor -> "Cursor"
  PJson -> "Json"


-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

dupDiags :: Text -> [Text] -> [Text]
dupDiags what = go Set.empty
  where
    go _ [] = []
    go seen (n : ns)
      | n `Set.member` seen = ("duplicate " <> what <> " '" <> n <> "'") : go seen ns
      | otherwise = go (Set.insert n seen) ns


tshow :: (Show a) => a -> Text
tshow = T.pack . show
