{- | Canonical IDL printer (spec §3.4, §7.1): sorted declarations, normalized
whitespace. The canonical text is the schema's published, content-addressed
form (@/schema/{schemaHash}@) and the input to 'Lattice.Hash.schemaHash'.

Also renders the per-query pertinent-declaration subset that feeds plan
identity (§7.3).

Canonical layout: the @schema@ line, the @claims@ block, then type
declarations, interfaces, entities, fragments, roots, and mutations, each
group sorted by name, one blank line between declarations, 2-space indent
inside braces. Within an entity: the default-visibility line, fields sorted
by name, relationships sorted by name, then @fetch by@. List-shaped model
fields (declaration-ordered record fields, argument lists, fragment
parameters and selections) print in stored order, so
@'Lattice.IDL.Parser.parseSchema' . 'canonicalIdl'@ is the identity on
parsed schemas.
-}
module Lattice.IDL.Print (
  canonicalIdl,
  printEntity,
  printExtension,
  printRoot,
  printMutation,
  printFragment,
  printTypeDecl,
  printInterface,
  renderType,
) where

import Data.Aeson qualified as A
import Data.Char qualified as Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Canonical (renderQValue, renderSelectionSet, renderVarDef)
import Lattice.Schema
import Lattice.Types
import Numeric (showHex)


canonicalIdl :: Schema -> Text
canonicalIdl s =
  T.intercalate "\n\n" (schemaLine : blocks) <> "\n"
  where
    ann = annPrefix s
    schemaLine = ann OnSchema "\n" <> "schema " <> schemaName s
    blocks =
      claimsBlocks
        <> directiveBlocks
        <> map (\(n, d) -> ann (OnType n) "\n" <> printTypeDecl n d) (Map.toAscList (schemaTypes s))
        <> map (\(n, d) -> ann (OnInterface n) "\n" <> printInterfaceIn s n d) (Map.toAscList (schemaInterfaces s))
        <> map (\(n, d) -> ann (OnEntity n) "\n" <> printEntityIn s n d) (Map.toAscList (schemaEntities s))
        <> map (\(n, d) -> ann (OnEntity n) "\n" <> printExtensionIn s n d) (Map.toAscList (schemaExtensions s))
        <> map (\(n, d) -> ann (OnFragment n) "\n" <> printFragment n d) (Map.toAscList (schemaFragments s))
        <> map (\(n, d) -> ann (OnRoot n) "\n" <> printRoot n d) (Map.toAscList (schemaRoots s))
        <> map (\(n, d) -> ann (OnMutation n) "\n" <> printMutation n d) (Map.toAscList (schemaMutations s))
    claimsBlocks
      | Map.null (schemaClaims s) = []
      | otherwise = [printClaims (schemaClaims s)]
    directiveBlocks =
      map
        (\(n, d) -> ann (OnDirective n) "\n" <> renderDirectiveDecl n d)
        (Map.toAscList (schemaDirectiveDecls s))

    printClaims cm =
      block "claims {" (map claimLine (Map.toAscList cm))
    claimLine (ClaimName c, t) = c <> ": " <> renderType t


{- | The canonical rendering of a site's annotations (§17.3 @\@break@,
§17.5 @\@deprecated@), each followed by @sep@: a newline before a
top-level declaration, a space before an entity\/interface item. Empty
when the site carries none. Annotations participate in the canonical IDL
text (they are part of the published document) but never in plan
pertinence ("Lattice.Plan" prints declarations without them).
-}
annPrefix :: Schema -> DeclPath -> Text -> Text
annPrefix s p sep =
  descPrefix
    <> dirPrefix
    <> maybe "" (\t -> "@break(approved: " <> jsonString t <> ")" <> sep) (Map.lookup p (schemaBreaks s))
    <> maybe "" (\d -> renderDeprecation d <> sep) (Map.lookup p (schemaDeprecations s))
  where
    descPrefix = maybe "" (\t -> jsonString t <> sep) (Map.lookup p (schemaDescriptions s))
    dirPrefix = foldMap (\a -> renderDirectiveApp a <> sep) (Map.findWithDefault [] p (schemaDirectives s))


renderDeprecation :: Deprecation -> Text
renderDeprecation d =
  "@deprecated(sunset: \""
    <> T.pack (show (depSunset d))
    <> "\", note: "
    <> jsonString (depNote d)
    <> ")"


{- | Canonical @directive \@name(params) [repeatable] on LOC | LOC@ line
(§3.9). Locations print in the fixed 'DirLocation' order; parameters in
declaration order like any argument list.
-}
renderDirectiveDecl :: DirectiveName -> DirectiveDef -> Text
renderDirectiveDecl (DirectiveName n) d =
  "directive @"
    <> n
    <> renderArgDefs (dirArgs d)
    <> (if dirRepeatable d then " repeatable" else "")
    <> " on "
    <> T.intercalate " | " (map dirLocationName (Set.toAscList (dirLocations d)))


-- | A directive application: @\@name@ or @\@name(a: v, b: v)@ (arguments
-- in canonical, name-sorted order — the model already stores them sorted).
renderDirectiveApp :: DirectiveApp -> Text
renderDirectiveApp (DirectiveApp (DirectiveName n) args) = "@" <> n <> renderArgs args
  where
    renderArgs [] = ""
    renderArgs as = "(" <> T.intercalate ", " (map one as) <> ")"
    one (ArgName a, v) = a <> ": " <> renderQValue v



{- | A schema carrying no annotations: the un-annotated printers
('printEntity', 'printInterface') delegate through it, so plan pertinence
(§7.3) never includes annotation text and plan ids are stable under
@\@break@\/@\@deprecated@ churn.
-}
bare :: Schema
bare =
  Schema
    { schemaName = ""
    , schemaClaims = Map.empty
    , schemaTypes = Map.empty
    , schemaInterfaces = Map.empty
    , schemaEntities = Map.empty
    , schemaExtensions = Map.empty
    , schemaFragments = Map.empty
    , schemaRoots = Map.empty
    , schemaMutations = Map.empty
    , schemaBreaks = Map.empty
    , schemaDeprecations = Map.empty
    , schemaDirectiveDecls = Map.empty
    , schemaDirectives = Map.empty
    , schemaDescriptions = Map.empty
    }

printTypeDecl :: TypeName -> TypeDecl -> Text
printTypeDecl (TypeName n) = \case
  DeclNewtype t refs -> "newtype " <> n <> " = " <> renderType t <> renderRefs refs
  DeclRecord fs -> "data " <> n <> " " <> recordBody fs
  DeclSum o cs ->
    "data " <> n <> " " <> renderOpenness o <> " = "
      <> T.intercalate " | " (map ctor (NE.toList cs))
  DeclEnum o cs ->
    "enum " <> n <> " " <> renderOpenness o <> " = "
      <> T.intercalate " | " (NE.toList cs)
  where
    ctor (Ctor cn []) = cn
    ctor (Ctor cn fs) = cn <> " " <> recordBody fs
    recordBody [] = "{}"
    recordBody fs =
      "{ " <> T.intercalate ", " (map (\(FieldName f, t) -> f <> ": " <> renderType t) fs) <> " }"

    renderRefs [] = ""
    renderRefs rs = "(" <> T.intercalate ", " (map renderRef' rs) <> ")"
    renderRef' = \case
      RefLen k -> "len " <> tshow k
      RefMin i -> "min " <> tshow i
      RefMax i -> "max " <> tshow i
      RefMatch re -> "match " <> jsonString re


printInterface :: InterfaceName -> InterfaceDef -> Text
printInterface = printInterfaceIn bare


-- | 'printInterface' with the schema's annotations rendered on items.
printInterfaceIn :: Schema -> InterfaceName -> InterfaceDef -> Text
printInterfaceIn s iname@(InterfaceName n) i =
  block
    ("interface " <> n <> " {")
    ( map (\(f, fd) -> itemAnn f <> fieldLine (f, fd)) (Map.toAscList (ifaceFields i))
        <> map relLine (Map.toAscList (ifaceRels i))
    )
  where
    itemAnn f = annPrefix s (OnIfaceItem iname f) " "
    relLine (f, r) = itemAnn f <> renderRel (defaultCollName n f) f r


printEntity :: TypeName -> EntityDef -> Text
printEntity = printEntityIn bare


-- | 'printEntity' with the schema's annotations rendered on items.
printEntityIn :: Schema -> TypeName -> EntityDef -> Text
printEntityIn s tname@(TypeName n) e =
  block
    ("entity " <> n <> keyClause <> impls <> " {")
    (defaultLine : fieldLines <> relLines <> fetchLines)
  where
    -- A co-keyed entity (§3.8) declares no @by@ clause; its key fields are
    -- inherited from the base and omitted from the printed body so
    -- parse ∘ print round-trips.
    keyClause = case entityCoKey e of
      Nothing -> " by " <> renderKey (entityKey e)
      Just (CoKey (TypeName b) JoinsBase) -> " joins " <> b
      Just (CoKey (TypeName b) RefinesBase) -> " refines " <> b

    impls
      | Set.null (entityImplements e) = ""
      | otherwise =
          " implements "
            <> T.intercalate ", " (map unInterfaceName (Set.toAscList (entityImplements e)))

    defaultLine = case entityDefaultPolicy e of
      Public -> "visible to all by default"
      Private -> "private by default"
      RequiresClaims ps -> "visible when " <> renderPreds ps <> " by default"

    bodyFields = case entityCoKey e of
      Nothing -> entityFields e
      Just _ ->
        Map.withoutKeys
          (entityFields e)
          (Set.fromList (NE.toList (entityKey e)))
    fieldLines = map (\(f, fd) -> itemAnn f <> fieldLine (f, fd)) (Map.toAscList bodyFields)
    relLines =
      map (\(f, r) -> itemAnn f <> renderRel (defaultCollName n f) f r) (Map.toAscList (entityRels e))
    itemAnn f = annPrefix s (OnEntityItem tname f) " "
    fetchLines = case entityFetchBy e of
      Nothing -> []
      Just p -> ["fetch by " <> renderKey (entityKey e) <> ": " <> renderPolicy p]

    renderKey (FieldName k :| []) = k
    renderKey ks = "(" <> T.intercalate ", " (map unFieldName (NE.toList ks)) <> ")"


printExtension :: TypeName -> ExtensionDef -> Text
printExtension = printExtensionIn bare


{- | Canonical @extend entity@ block (§18.1). Body layout mirrors an entity:
the (illegal, fusion-rejected) default-visibility line if present, fields
sorted by name, relationships sorted by name, then the (illegal) @fetch by@.
Extension edges auto-name their collections off the EXTENDED type
(@Post.reactions@), matching where fusion folds them.
-}
printExtensionIn :: Schema -> TypeName -> ExtensionDef -> Text
printExtensionIn s tname@(TypeName n) x =
  block
    ("extend entity " <> n <> coClause <> " {")
    (defaultLines <> fieldLines <> relLines <> fetchLines)
  where
    coClause = case extCoKey x of
      Nothing -> ""
      Just (CoKey (TypeName b) JoinsBase) -> " joins " <> b
      Just (CoKey (TypeName b) RefinesBase) -> " refines " <> b

    defaultLines = case extDefaultPolicy x of
      Nothing -> []
      Just Public -> ["visible to all by default"]
      Just Private -> ["private by default"]
      Just (RequiresClaims ps) -> ["visible when " <> renderPreds ps <> " by default"]

    fieldLines = map (\(f, fd) -> itemAnn f <> fieldLine (f, fd)) (Map.toAscList (extFields x))
    relLines =
      map (\(f, r) -> itemAnn f <> renderRel (defaultCollName n f) f r) (Map.toAscList (extRels x))
    itemAnn f = annPrefix s (OnEntityItem tname f) " "

    fetchLines = case extFetchBy x of
      Nothing -> []
      Just (ks, p) -> ["fetch by " <> renderKeySpec ks <> ": " <> renderPolicy p]

    renderKeySpec (FieldName k :| []) = k
    renderKeySpec ks = "(" <> T.intercalate ", " (map unFieldName (NE.toList ks)) <> ")"


printRoot :: RootName -> RootDef -> Text
printRoot (RootName n) r = case rootKind r of
  RootGet ->
    "get " <> n <> renderArgDefs (rootParams r) <> " of " <> renderTarget (rootTarget r)
      <> " "
      <> renderPolicy (rootPolicy r)
  RootList ->
    "list " <> n <> renderArgDefs (rootParams r) <> " of " <> renderTarget (rootTarget r)
      <> collClauses
      <> " "
      <> renderPolicy (rootPolicy r)
  where
    collClauses = case rootCollection r of
      Nothing -> ""
      Just col ->
        let paramNames = map (unArgName . adName) (rootParams r)
            paramBacked = case paramNames of
              [] -> False
              p0 : _ -> unFieldName (colLink col) == p0
            defGroup = case (paramBacked, paramNames) of
              (True, p0 : ps) -> fmap FieldName (p0 :| ps)
              _ -> colLink col :| []
            byClause = if paramBacked then "" else " by " <> unFieldName (colLink col)
         in byClause
              <> renderWindow (colWindow col)
              <> groupedClause defGroup col
              <> asClause (CollectionName n) col


printFragment :: FragmentName -> FragmentDef -> Text
printFragment (FragmentName n) f =
  "fragment " <> n <> params <> " on " <> fragOn f <> " " <> renderSelectionSet (fragSelection f)
  where
    params = case fragParams f of
      [] -> ""
      vs -> "(" <> T.intercalate "," (map renderVarDef vs) <> ")"


printMutation :: MutationName -> MutationDef -> Text
printMutation (MutationName n) m =
  block
    ("mutation " <> n <> renderArgDefs (mutParams m) <> " returns " <> unTypeName (mutReturns m) <> " {")
    (allowLine : writesLine : invalidatesLine : effectLine : (asLines <> errorsLines <> batchLines))
  where
    allowLine = case mutGuard m of
      Public -> "allow public"
      Private -> "allow private"
      RequiresClaims ps -> "allow when " <> renderPreds ps
    writesLine = "writes " <> renderScopes (mutWrites m)
    invalidatesLine = case mutInvalidates m of
      ExactlyWrites -> "invalidates writes"
      WritesPlus extra -> "invalidates writes, " <> renderScopes extra
    effectLine = case mutEffect m of
      Transactional -> "effect transactional"
      NaturallyIdempotent j
        | T.null j -> "effect natural"
        | otherwise -> "effect natural " <> jsonString j
      Workflow -> "effect workflow"
    errorsLines = case mutErrors m of
      Nothing -> []
      Just (TypeName en, o, cs) ->
        ["errors " <> en <> " " <> renderOpenness o <> " = " <> T.intercalate " | " (NE.toList cs)]
    -- Canonical clause order (parse order is free): the singular binding
    -- prints after @effect@, the batch binding as a suffix of @batch@.
    asLines = case mutBinding m of
      Nothing -> []
      Just b ->
        [ "as "
            <> bindVerbName (vbVerb b)
            <> " /e/"
            <> unTypeName (vbTarget b)
            <> maybe "" (\(ArgName a) -> "/{" <> a <> "}") (vbKeyArg b)
            <> (if vbLww b then " last-writer-wins" else "")
        ]
    batchLines = case mutBatch m of
      Nothing -> []
      Just bp ->
        let at = case bpAtomicity bp of
              BestEffort -> "best-effort"
              AllOrNothing -> "all-or-nothing"
            bound = case bpBound bp of
              Nothing -> ""
              Just (v, ty) -> " as " <> bindVerbName v <> " /e/" <> unTypeName ty
         in ["batch " <> at <> " max " <> tshow (bpMaxItems bp) <> bound]

    renderScopes = T.intercalate ", " . map renderScope
    renderScope = \case
      WEntity (TypeName t) KeyNew -> t <> "(new)"
      WEntity (TypeName t) (KeyArg (ArgName a)) -> t <> "(" <> a <> ")"
      WCollection (CollectionName c) (GroupArg (ArgName a)) -> c <> "(" <> a <> ")"
      WCollection (CollectionName c) (GroupOfWritten (TypeName t) (FieldName f)) ->
        c <> "(" <> t <> "." <> f <> ")"


-- ---------------------------------------------------------------------------
-- Shared rendering
-- ---------------------------------------------------------------------------

-- | A braced block: header, 2-space-indented lines, closing brace.
block :: Text -> [Text] -> Text
block header body =
  header <> "\n" <> T.intercalate "\n" (map ("  " <>) body) <> "\n}"


tshow :: Show a => a -> Text
tshow = T.pack . show


renderOpenness :: Openness -> Text
renderOpenness = \case
  Open -> "open"
  Closed -> "closed"


defaultCollName :: Text -> FieldName -> CollectionName
defaultCollName owner (FieldName f) = CollectionName (owner <> "." <> f)


{- | Canonical field line. The derived clause (§3.7) prints in the pinned
clause order — @derived reads \<dep\>{, \<dep\>}
[\@declassify(approved: \"…\")] (on read | maintained)@ — between the type
and the policy, matching the parser's only accepted order; fields without
a derivation print byte-identically to before the clause existed.
-}
fieldLine :: (FieldName, FieldDef) -> Text
fieldLine (FieldName f, fd) =
  f <> renderArgDefs (fieldArgs fd) <> ": " <> renderType (fieldType fd)
    <> maybe "" renderDerivation (fieldDerivation fd)
    <> maybe "" (\p -> " " <> renderPolicy p) (fieldPolicy fd)


renderDerivation :: Derivation -> Text
renderDerivation d =
  " derived reads "
    <> T.intercalate ", " (map renderDep (NE.toList (derivReads d)))
    <> maybe "" (\j -> " @declassify(approved: " <> jsonString j <> ")") (derivDeclassify d)
    <> case derivMaterialize d of
      OnRead -> " on read"
      Maintained -> " maintained"


renderDep :: Dep -> Text
renderDep = \case
  OwnFields fs -> "own(" <> T.intercalate ", " (map unFieldName (NE.toList fs)) <> ")"
  ViaEdge e frag -> unFieldName e <> " ..." <> unFragmentName frag
  ViaCollection r agg -> unFieldName r <> " " <> renderAggregate agg


renderAggregate :: Aggregate -> Text
renderAggregate = \case
  AggCount -> "count"
  AggSum f -> "sum(" <> unFieldName f <> ")"
  AggMin f -> "min(" <> unFieldName f <> ")"
  AggMax f -> "max(" <> unFieldName f <> ")"


renderArgDefs :: [ArgDef] -> Text
renderArgDefs [] = ""
renderArgDefs as = "(" <> T.intercalate ", " (map one as) <> ")"
  where
    one a =
      unArgName (adName a) <> ": " <> renderType (adType a)
        <> maybe "" (\v -> " = " <> renderQValue v) (adDefault a)


renderRel :: CollectionName -> FieldName -> RelationshipDef -> Text
renderRel autoName (FieldName f) = \case
  ToOne tgt (FieldName byF) opt pol ->
    "has one" <> (if opt then "? " else " ") <> f <> ": " <> renderTarget tgt <> " by " <> byF <> polSuffix pol
  ToMany tgt col pol ->
    "has many " <> f <> ": " <> renderTarget tgt
      <> " by "
      <> unFieldName (colLink col)
      <> renderWindow (colWindow col)
      <> groupedClause (colLink col :| []) col
      <> asClause autoName col
      <> polSuffix pol
  where
    polSuffix = maybe "" (\p -> " " <> renderPolicy p)


renderWindow :: Windowing -> Text
renderWindow = \case
  Bounded minN maxN op ->
    (if minN > 0 then " min " <> tshow minN else "")
      <> " max "
      <> tshow maxN
      <> case op of
        Truncate -> " truncate"
        Overflow -> ""
  Paginated cs ->
    " ordered by "
      <> T.intercalate ", " (map col (NE.toList (csKeyset cs)))
      <> maybe "" (\p -> " page " <> tshow p) (csDefaultPage cs)
      <> " max "
      <> tshow (csMaxPage cs)
  where
    col (FieldName f, d) = f <> " " <> case d of
      Asc -> "asc"
      Desc -> "desc"


-- | @grouped by ...@ when the grouping differs from its default.
groupedClause :: NonEmpty FieldName -> CollectionDef -> Text
groupedClause defGroup col
  | colGrouping col == defGroup = ""
  | otherwise =
      " grouped by " <> T.intercalate ", " (map unFieldName (NE.toList (colGrouping col)))


-- | @as name@ when the collection name differs from its auto-derived default.
asClause :: CollectionName -> CollectionDef -> Text
asClause autoName col
  | colName col == autoName = ""
  | otherwise = " as " <> unCollectionName (colName col)


renderTarget :: Target -> Text
renderTarget = \case
  TargetEntity (TypeName t) -> t
  TargetInterface (InterfaceName i) -> i
  TargetUnion ts -> "(" <> T.intercalate " | " (map unTypeName (NE.toList ts)) <> ")"


renderPolicy :: Policy -> Text
renderPolicy = \case
  Public -> "public"
  Private -> "private"
  RequiresClaims ps -> "visible when " <> renderPreds ps


renderPreds :: [ClaimPredicate] -> Text
renderPreds = T.intercalate " and " . map one
  where
    one (ClaimPredicate (ClaimName c) rhs) =
      "caller." <> c <> case rhs of
        RhsField (FieldName f) -> " = " <> f
        RhsLiteral v -> " = " <> renderLit v
        RhsOneOf vs -> " in [" <> T.intercalate ", " (map renderLit vs) <> "]"


{- | A predicate literal: capitalized-identifier strings print bare (enum
literals); everything else prints as JSON. Arrays and objects cannot arise
from parsed schemas and print for diagnostics only.
-}
renderLit :: A.Value -> Text
renderLit = \case
  A.String s
    | isBareEnum s -> s
    | otherwise -> jsonString s
  A.Number n -> renderNumber n
  A.Bool True -> "true"
  A.Bool False -> "false"
  A.Null -> "null"
  v -> tshow v
  where
    isBareEnum s = case T.uncons s of
      Just (c, rest) ->
        Char.isUpper c && T.all (\x -> Char.isAlphaNum x || x == '_') rest
      Nothing -> False


renderNumber :: Scientific -> Text
renderNumber n = case Sci.floatingOrInteger n :: Either Double Integer of
  Right i -> tshow i
  Left _ -> tshow n


renderType :: FieldType -> Text
renderType = \case
  TPrim p -> renderPrim p
  TNamed (TypeName n) -> n
  TOptional t -> renderType t <> "?"
  TList t -> "[" <> renderType t <> "]"
  TList1 t -> "[" <> renderType t <> "]+"
  TSet t -> "Set " <> renderTypeArg t
  TMap k v -> "Map " <> renderTypeArg k <> " " <> renderTypeArg v
  TVec n t -> "Vec " <> tshow n <> " " <> renderTypeArg t


-- | Argument position of @Set@/@Map@/@Vec@: parenthesize anything that is
-- not self-delimiting under juxtaposition.
renderTypeArg :: FieldType -> Text
renderTypeArg t
  | selfDelim t = renderType t
  | otherwise = "(" <> renderType t <> ")"
  where
    selfDelim = \case
      TPrim (PBytes (Just _)) -> False
      TPrim (PBit _) -> False
      TPrim _ -> True
      TNamed _ -> True
      TList _ -> True
      _ -> False


renderPrim :: Prim -> Text
renderPrim = \case
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
  PBytes Nothing -> "Bytes"
  PBytes (Just n) -> "Bytes " <> tshow n
  PBit n -> "Bit " <> tshow n
  PUuid -> "Uuid"
  PTimestamp -> "Timestamp"
  PDate -> "Date"
  PTimeOfDay -> "TimeOfDay"
  PDuration -> "Duration"
  PCursor -> "Cursor"
  PJson -> "Json"


-- | JSON-escape a string literal (RFC 8259 §7).
jsonString :: Text -> Text
jsonString t = "\"" <> T.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      '\b' -> "\\b"
      '\f' -> "\\f"
      c
        | c < ' ' ->
            let h = T.pack (showHex (Char.ord c) "")
             in "\\u" <> T.replicate (4 - T.length h) "0" <> h
        | otherwise -> T.singleton c
