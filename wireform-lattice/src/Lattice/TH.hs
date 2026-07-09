{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{- | IDL-to-Haskell codegen: generate typed value types and entity records
(the "Lattice.Typed" vocabulary) from a Lattice schema at compile time.

@
{\-# LANGUAGE DataKinds, StandaloneDeriving, TemplateHaskell, TypeFamilies #-\}
module My.Rows where
import Lattice.TH (latticeTypes)
$(latticeTypes "schema/api.lattice")
@

The splice parses the IDL with the real parser ("Lattice.IDL.Parser") and
emits, in one declaration group:

* one Haskell declaration per declared value type —
  @newtype HumanId = HumanId Text@ (selector @unHumanId@),
  @data Episode = Episode'NewHope | …@ (open enums and sums gain an
  @…'Unknown@ catch-all carrying the raw wire spelling\/value, §3.5.4),
  @data Point = Point { pointX :: …, … }@ for records, and one positional
  constructor per sum alternative (argument order = the constructor's
  field declaration order) — each with a 'LatticeValue' instance pinning
  the §3.5.3 canonical wire form (sums as @{"$tag": …}@, enums as bare
  strings);
* one higher-kinded record per entity —
  @data Human f = Human { humanId :: Field f 'Req HumanId, … }@ —
  covering the STORED row fields (argument-taking computed fields and
  @on read@ derived fields are not row data and are omitted; @maintained@
  fields are included), with @Show@\/@Eq@ for both row shapes and a
  'LatticeEntity' instance (row conversion + typed keys; a composite key
  generates an @\<Entity\>Key@ record).

Field selectors are prefixed with the owning type's name
(@humanHomePlanet@), so generated records never collide. Refinement
annotations (@len@\/@min@\/@max@\/@match@) validate at protocol input
boundaries, not here; the typed layer trusts the backend it types.
@Vec n t@ maps to a plain list (length is a boundary check), and @Uuid@\/
@Duration@\/@Cursor@ ride as their canonical text.

The splice site needs @DataKinds@, @TypeFamilies@, @StandaloneDeriving@,
and @TemplateHaskell@. Generated code references only exported names
("Lattice.Typed", "Lattice.Types", "Lattice.Value", @containers@, @text@,
@aeson@), so no extra imports are required at the splice site.
-}
module Lattice.TH (
  latticeTypes,
  latticeTypesText,
) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Word (Word16, Word32, Word64, Word8)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.Schema
import Lattice.Typed
import Lattice.Types
import Lattice.Value (renderScalarKey)


-- | Generate types from an IDL file (registered as a compile dependency,
-- so editing the schema recompiles the splice site).
latticeTypes :: FilePath -> Q [Dec]
latticeTypes fp = do
  addDependentFile fp
  src <- runIO (TIO.readFile fp)
  latticeTypesFromSource src


-- | Generate types from inline IDL text (fixtures, tests).
latticeTypesText :: String -> Q [Dec]
latticeTypesText = latticeTypesFromSource . T.pack


latticeTypesFromSource :: Text -> Q [Dec]
latticeTypesFromSource src = case parseSchema src of
  Left errs -> fail ("latticeTypes: IDL failed to parse:\n" <> unlines (map renderErr errs))
  Right schema -> do
    valueDecs <- concat <$> traverse (uncurry (genTypeDecl schema)) (Map.toAscList (schemaTypes schema))
    entityDecs <- concat <$> traverse (uncurry (genEntity schema)) (Map.toAscList (schemaEntities schema))
    pure (valueDecs <> entityDecs)
  where
    renderErr (SchemaError ln msg) =
      maybe "" (\l -> "line " <> show l <> ": ") ln <> T.unpack msg


-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

tyName :: TypeName -> Name
tyName = mkName . T.unpack . unTypeName


-- | @Human@ + @homePlanet@ -> @humanHomePlanet@.
selName :: Text -> FieldName -> Name
selName owner f = mkName (lowerFirst (T.unpack owner) <> upperFirst (T.unpack (unFieldName f)))


-- | @Episode@ + @NewHope@ -> @Episode'NewHope@ (sums and enums).
ctorN :: Text -> Text -> Name
ctorN owner c = mkName (T.unpack owner <> "'" <> T.unpack c)


unknownCtorN :: Text -> Name
unknownCtorN owner = mkName (T.unpack owner <> "'Unknown")


lowerFirst :: String -> String
lowerFirst = \case
  [] -> []
  c : cs -> toLower c : cs


upperFirst :: String -> String
upperFirst = \case
  [] -> []
  c : cs -> toUpper c : cs


-- | A monomorphic 'Text' literal (no splice-site @OverloadedStrings@ needed).
txtE :: Text -> Q Exp
txtE t = [|T.pack $(pure (LitE (StringL (T.unpack t))))|]


-- | @FieldName "…"@ as an expression.
fieldNameE :: FieldName -> Q Exp
fieldNameE f = [|FieldName $(txtE (unFieldName f))|]


fieldBang :: Bang
fieldBang = Bang NoSourceUnpackedness NoSourceStrictness


plainDeriv :: [Name] -> DerivClause
plainDeriv ns = DerivClause Nothing (map ConT ns)


plainBndr :: Name -> TyVarBndr BndrVis
#if MIN_VERSION_template_haskell(2,21,0)
plainBndr n = PlainTV n BndrReq
#else
plainBndr n = PlainTV n ()
#endif


#if !MIN_VERSION_template_haskell(2,21,0)
type BndrVis = ()
#endif


-- ---------------------------------------------------------------------------
-- FieldType -> Haskell type
-- ---------------------------------------------------------------------------

fieldTypeT :: FieldType -> Q Type
fieldTypeT = \case
  TPrim p -> primT p
  TNamed n -> pure (ConT (tyName n))
  TOptional t -> [t|Maybe $(fieldTypeT t)|]
  TList t -> [t|[$(fieldTypeT t)]|]
  TList1 t -> [t|NonEmpty $(fieldTypeT t)|]
  TSet t -> [t|Set $(fieldTypeT t)|]
  TMap k v -> [t|Map $(fieldTypeT k) $(fieldTypeT v)|]
  -- Fixed length is checked at protocol input boundaries, not structurally.
  TVec _ t -> [t|[$(fieldTypeT t)]|]


primT :: Prim -> Q Type
primT = \case
  PBool -> [t|Bool|]
  PI8 -> [t|Int8|]
  PI16 -> [t|Int16|]
  PI32 -> [t|Int32|]
  PI64 -> [t|Int64|]
  PW8 -> [t|Word8|]
  PW16 -> [t|Word16|]
  PW32 -> [t|Word32|]
  PW64 -> [t|Word64|]
  PInteger -> [t|Integer|]
  PDecimal -> [t|Scientific|]
  PF32 -> [t|Float|]
  PF64 -> [t|Double|]
  PText -> [t|Text|]
  PBytes _ -> [t|ByteString|]
  PBit _ -> [t|ByteString|]
  -- Uuid, Duration, and Cursor ride as their canonical text (§3.5.3).
  PUuid -> [t|Text|]
  PDuration -> [t|Text|]
  PCursor -> [t|Text|]
  PTimestamp -> [t|UTCTime|]
  PDate -> [t|Day|]
  PTimeOfDay -> [t|TimeOfDay|]
  PJson -> [t|A.Value|]


-- ---------------------------------------------------------------------------
-- Value type declarations
-- ---------------------------------------------------------------------------

genTypeDecl :: Schema -> TypeName -> TypeDecl -> Q [Dec]
genTypeDecl _schema n = \case
  DeclNewtype inner _refinements -> genNewtype n inner
  DeclEnum openness ctors -> genEnum n openness ctors
  DeclRecord fields -> genRecord n fields
  DeclSum openness ctors -> genSum n openness ctors


latticeValueInst :: Name -> [(Name, Exp)] -> Dec
latticeValueInst name methods =
  InstanceD
    Nothing
    []
    (AppT (ConT ''LatticeValue) (ConT name))
    (map (\(m, e) -> ValD (VarP m) (NormalB e) []) methods)


genNewtype :: TypeName -> FieldType -> Q [Dec]
genNewtype n inner = do
  innerT <- fieldTypeT inner
  let name = tyName n
      un = mkName ("un" <> T.unpack (unTypeName n))
      dec =
        NewtypeD
          []
          name
          []
          Nothing
          (RecC name [(un, fieldBang, innerT)])
          [plainDeriv [''Eq, ''Ord, ''Show]]
  toW <- [|toWire . $(varE un)|]
  fromW <- [|fmap $(conE name) . fromWire|]
  tf <- [|mapTextForm $(conE name) $(varE un) <$> textForm|]
  pure [dec, latticeValueInst name [('toWire, toW), ('fromWire, fromW), ('textForm, tf)]]


genEnum :: TypeName -> Openness -> NonEmpty Text -> Q [Dec]
genEnum n openness ctors = do
  let name = tyName n
      owner = unTypeName n
      known = map (\c -> NormalC (ctorN owner c) []) (NE.toList ctors)
      unknown = case openness of
        Open -> [NormalC (unknownCtorN owner) [(fieldBang, ConT ''Text)]]
        Closed -> []
      dec = DataD [] name [] Nothing (known <> unknown) [plainDeriv [''Eq, ''Ord, ''Show]]
  render <- enumRenderE owner openness (NE.toList ctors)
  parse <- enumParseE owner openness (NE.toList ctors)
  toW <- [|A.String . $(pure render)|]
  fromW <-
    [|
      \case
        A.String t -> $(pure parse) t
        _ -> Left ($(txtE ("not an enum string for " <> owner)))
      |]
  tf <- [|Just (TextForm $(pure render) $(pure parse))|]
  pure [dec, latticeValueInst name [('toWire, toW), ('fromWire, fromW), ('textForm, tf)]]


-- | @\case Episode'NewHope -> "NewHope"; …; Episode'Unknown t -> t@
enumRenderE :: Text -> Openness -> [Text] -> Q Exp
enumRenderE owner openness ctors = do
  t <- newName "t"
  knowns <-
    traverse
      (\c -> Match (ConP (ctorN owner c) [] []) . NormalB <$> txtE c <*> pure [])
      ctors
  let unknowns = case openness of
        Open -> [Match (ConP (unknownCtorN owner) [] [VarP t]) (NormalB (VarE t)) []]
        Closed -> []
  pure (LamCaseE (knowns <> unknowns))


-- | @\t -> case t of "NewHope" -> Right Episode'NewHope; …@ with the
-- open\/closed fallback.
enumParseE :: Text -> Openness -> [Text] -> Q Exp
enumParseE owner openness ctors = do
  t <- newName "t"
  matches <-
    traverse
      ( \c -> do
          guard' <- [|$(varE t) == $(txtE c)|]
          body <- [|Right $(conE (ctorN owner c))|]
          pure (GuardedB [(NormalG guard', body)])
      )
      ctors
  fallback <- case openness of
    Open -> [|Right ($(conE (unknownCtorN owner)) $(varE t))|]
    Closed -> [|Left ($(txtE ("unknown " <> owner <> " constructor: ")) <> $(varE t))|]
  let clauses = map (\b -> Match WildP b []) matches <> [Match WildP (NormalB fallback) []]
  pure (LamE [VarP t] (CaseE (TupE []) clauses))


genRecord :: TypeName -> [(FieldName, FieldType)] -> Q [Dec]
genRecord n fields = do
  let name = tyName n
      owner = unTypeName n
  varBangs <-
    traverse (\(f, ft) -> (,,) (selName owner f) fieldBang <$> fieldTypeT ft) fields
  let dec = DataD [] name [] Nothing [RecC name varBangs] [plainDeriv [''Eq, ''Show]]
  toW <- objectToWireE owner Nothing fields
  fromW <- recordFromWireE name owner fields
  pure [dec, latticeValueInst name [('toWire, toW), ('fromWire, fromW)]]


{- | @\r -> A.Object …@ over a record's selectors (or a sum constructor's
positional variables when @tagged@ carries the ctor tag and var names).
Optional 'Nothing' fields are omitted (§4.8 rule 6).
-}
objectToWireE :: Text -> Maybe (Text, [Name]) -> [(FieldName, FieldType)] -> Q Exp
objectToWireE owner tagged fields = do
  r <- newName "r"
  let accessor f i = case tagged of
        Nothing -> pure (AppE (VarE (selName owner f)) (VarE r))
        Just (_, vars) -> pure (VarE (vars !! i))
  entries <-
    traverse
      ( \((f, ft), i) -> do
          sel <- accessor f i
          case ft of
            TOptional _ -> [|fmap (\v -> ($(txtE (unFieldName f)), toWire v)) $(pure sel)|]
            _ -> [|Just ($(txtE (unFieldName f)), toWire $(pure sel))|]
      )
      (zip fields [0 ..])
  tagEntry <- case tagged of
    Nothing -> pure []
    Just (tag, _) -> do
      e <- [|Just ($(txtE "$tag"), A.String $(txtE tag))|]
      pure [e]
  body <-
    [|
      A.Object
        (KM.fromList (map (\(k, v) -> (AK.fromText k, v)) (catMaybes $(pure (ListE (tagEntry <> entries))))))
      |]
  let pat = case tagged of
        Nothing -> VarP r
        Just _ -> WildP
  pure (LamE [pat] body)


-- | The row-fields map of an object's members, for the shared decoder.
objectFieldsE :: Name -> Q Exp
objectFieldsE km =
  [|Map.fromList (map (\(k, v) -> (FieldName (AK.toText k), v)) (KM.toList $(varE km)))|]


recordFromWireE :: Name -> Text -> [(FieldName, FieldType)] -> Q Exp
recordFromWireE name owner fields = do
  v <- newName "v"
  km <- newName "km"
  o <- newName "o"
  inner <- fieldsFromMapE (ConE name) (map (fmap isOptional) fields) o
  toMap <- objectFieldsE km
  wrapped <- [|either (Left . rowErrorText) Right $(pure inner)|]
  notObj <- [|Left $(txtE ("not a record object for " <> owner))|]
  pure $
    LamE [VarP v] $
      CaseE
        (VarE v)
        [ Match
            (ConP 'A.Object [] [VarP km])
            (NormalB (LetE [ValD (VarP o) (NormalB toMap) []] wrapped))
            []
        , Match WildP (NormalB notObj) []
        ]


genSum :: TypeName -> Openness -> NonEmpty Ctor -> Q [Dec]
genSum n openness ctors = do
  let name = tyName n
      owner = unTypeName n
  known <-
    traverse
      ( \c ->
          NormalC (ctorN owner (ctorName c))
            <$> traverse (\(_, ft) -> (,) fieldBang <$> fieldTypeT ft) (ctorFields c)
      )
      (NE.toList ctors)
  let unknown = case openness of
        Open -> [NormalC (unknownCtorN owner) [(fieldBang, ConT ''A.Value)]]
        Closed -> []
      dec = DataD [] name [] Nothing (known <> unknown) [plainDeriv [''Eq, ''Show]]
  toW <- sumToWireE owner openness (NE.toList ctors)
  fromW <- sumFromWireE owner openness (NE.toList ctors)
  pure [dec, latticeValueInst name [('toWire, toW), ('fromWire, fromW)]]


-- | Sums serialize as @{"$tag": "Ctor", …fields}@, fields in canonical
-- (sorted-at-emission) order; the Unknown case re-emits its raw value.
sumToWireE :: Text -> Openness -> [Ctor] -> Q Exp
sumToWireE owner openness ctors = do
  matches <- traverse one ctors
  unknowns <- case openness of
    Open -> do
      v <- newName "v"
      pure [Match (ConP (unknownCtorN owner) [] [VarP v]) (NormalB (VarE v)) []]
    Closed -> pure []
  pure (LamCaseE (matches <> unknowns))
  where
    one c = do
      args <- traverse (\i -> newName ("x" <> show i)) [1 .. length (ctorFields c)]
      body <- objectToWireE owner (Just (ctorName c, args)) (ctorFields c)
      -- objectToWireE with a tag ignores its lambda argument; apply to ().
      pure
        ( Match
            (ConP (ctorN owner (ctorName c)) [] (map VarP args))
            (NormalB (AppE body (TupE [])))
            []
        )


sumFromWireE :: Text -> Openness -> [Ctor] -> Q Exp
sumFromWireE owner openness ctors = do
  v <- newName "v"
  km <- newName "km"
  o <- newName "o"
  tagN <- newName "tag"
  branches <-
    traverse
      ( \c -> do
          inner <- fieldsFromMapE (ConE (ctorN owner (ctorName c))) (map (fmap isOptional) (ctorFields c)) o
          guard' <- [|$(varE tagN) == $(txtE (ctorName c))|]
          body <- [|either (Left . rowErrorText) Right $(pure inner)|]
          pure (Match WildP (GuardedB [(NormalG guard', body)]) [])
      )
      ctors
  fallback <- case openness of
    Open -> [|Right ($(conE (unknownCtorN owner)) $(varE v))|]
    Closed -> [|Left ($(txtE ("unknown $tag constructor for " <> owner <> ": ")) <> $(varE tagN))|]
  toMap <- objectFieldsE km
  lookupTag <- [|KM.lookup (AK.fromText $(txtE "$tag")) $(varE km)|]
  missingTag <- [|Left $(txtE ("missing $tag for " <> owner))|]
  notObj <- [|Left $(txtE ("not a tagged object for " <> owner))|]
  let tagCase =
        CaseE
          lookupTag
          [ Match
              (ConP 'Just [] [ConP 'A.String [] [VarP tagN]])
              (NormalB (CaseE (TupE []) (branches <> [Match WildP (NormalB fallback) []])))
              []
          , Match WildP (NormalB missingTag) []
          ]
  pure $
    LamE [VarP v] $
      CaseE
        (VarE v)
        [ Match
            (ConP 'A.Object [] [VarP km])
            (NormalB (LetE [ValD (VarP o) (NormalB toMap) []] tagCase))
            []
        , Match WildP (NormalB notObj) []
        ]


{- | Applicative decode chain
@Ctor \<$\> requiredField "a" m \<*\> optionalField "b" m …@ over a
@Map FieldName Value@ bound to the given name.
-}
fieldsFromMapE :: Exp -> [(FieldName, Bool)] -> Name -> Q Exp
fieldsFromMapE ctor fields m = do
  parts <-
    traverse
      ( \(f, opt) ->
          if opt
            then [|optionalField $(fieldNameE f) $(varE m)|]
            else [|requiredField $(fieldNameE f) $(varE m)|]
      )
      fields
  pure $ case parts of
    [] -> AppE (ConE 'Right) ctor
    p0 : rest ->
      foldl
        (\acc p -> InfixE (Just acc) (VarE '(<*>)) (Just p))
        (InfixE (Just ctor) (VarE '(<$>)) (Just p0))
        rest


-- ---------------------------------------------------------------------------
-- Entities
-- ---------------------------------------------------------------------------

-- | The entity's stored row fields: everything but computed
-- (argument-taking) and @on read@ derived fields (§3.7).
rowFieldsOf :: EntityDef -> [(FieldName, FieldDef)]
rowFieldsOf ent = filter stored (Map.toAscList (entityFields ent))
  where
    stored (_, fd) =
      null (fieldArgs fd)
        && case fieldDerivation fd of
          Just d -> derivMaterialize d /= OnRead
          Nothing -> True


genEntity :: Schema -> TypeName -> EntityDef -> Q [Dec]
genEntity schema n ent = do
  let name = tyName n
      owner = unTypeName n
      fields = rowFieldsOf ent
      fVar = mkName "f"
  varBangs <-
    traverse
      ( \(f, fd) -> do
          let (opt, innerFt) = splitOptional (fieldType fd)
          inner <- fieldTypeT innerFt
          let t =
                ConT ''Field
                  `AppT` VarT fVar
                  `AppT` PromotedT (if opt then 'Opt else 'Req)
                  `AppT` inner
          pure (selName owner f, fieldBang, t)
      )
      fields
  let dec = DataD [] name [plainBndr fVar] Nothing [RecC name varBangs] []
      derivs =
        map
          (\(cls, shape) -> StandaloneDerivD Nothing [] (AppT (ConT cls) (AppT (ConT name) (ConT shape))))
          [(''Show, ''Full), (''Show, ''Partial), (''Eq, ''Full), (''Eq, ''Partial)]
  (keyDecs, keyT, keyImpl) <- genKey schema n ent
  inst <- entityInstance n ent keyT keyImpl
  pure ([dec] <> derivs <> keyDecs <> [inst])


-- | Which key implementation the instance methods use.
data KeyImpl
  = -- | Single key field: its selector and scalar wire shape.
    SingleKey FieldName ScalarKeyForm
  | -- | Composite: generated key record with per-component shapes.
    CompositeKey Name [(FieldName, ScalarKeyForm)]


{- | The typed key: the key field's type for single-field keys, a generated
@\<Entity\>Key@ record for composite keys.
-}
genKey :: Schema -> TypeName -> EntityDef -> Q ([Dec], Type, KeyImpl)
genKey schema n ent = case entityKey ent of
  kf NE.:| [] -> do
    ft <- keyFieldType kf
    t <- fieldTypeT ft
    pure ([], t, SingleKey kf (scalarFormOf schema ft))
  ks -> do
    let owner = unTypeName n
        keyName = mkName (T.unpack owner <> "Key")
    comps <-
      traverse
        ( \kf -> do
            ft <- keyFieldType kf
            t <- fieldTypeT ft
            pure ((selName (owner <> "Key") kf, fieldBang, t), (kf, scalarFormOf schema ft))
        )
        (NE.toList ks)
    let dec =
          DataD [] keyName [] Nothing [RecC keyName (map fst comps)] [plainDeriv [''Eq, ''Ord, ''Show]]
    pure ([dec], ConT keyName, CompositeKey keyName (map snd comps))
  where
    keyFieldType kf = case Map.lookup kf (entityFields ent) of
      Just fd -> pure (fieldType fd)
      Nothing ->
        fail
          ( "latticeTypes: key field not declared on "
              <> T.unpack (unTypeName n)
              <> ": "
              <> T.unpack (unFieldName kf)
          )


-- | Resolve a key component's wire shape through newtypes (for
-- 'scalarKeyFromText'): string-formed, number-formed, or boolean.
scalarFormOf :: Schema -> FieldType -> ScalarKeyForm
scalarFormOf schema = go (8 :: Int)
  where
    go fuel = \case
      TPrim p -> primForm p
      TNamed t
        | fuel > 0
        , Just (DeclNewtype inner _) <- Map.lookup t (schemaTypes schema) ->
            go (fuel - 1) inner
      -- Enums are string-formed; anything else has no scalar key form and
      -- was rejected upstream — default to text.
      _ -> KeyText
    primForm = \case
      PBool -> KeyBool
      PI8 -> KeyNumber
      PI16 -> KeyNumber
      PI32 -> KeyNumber
      PW8 -> KeyNumber
      PW16 -> KeyNumber
      PW32 -> KeyNumber
      PF32 -> KeyNumber
      PF64 -> KeyNumber
      _ -> KeyText


entityInstance :: TypeName -> EntityDef -> Type -> KeyImpl -> Q Dec
entityInstance n ent keyT keyImpl = do
  let name = tyName n
      owner = unTypeName n
      fields = rowFieldsOf ent
  toRowE <- rowsToMapE owner fields Fullish
  toPartialE <- rowsToMapE owner fields Partialish
  fromRowE <- rowsFromMapE name owner fields Fullish
  fromPartialE <- rowsFromMapE name owner fields Partialish
  renderK <- renderKeyE owner keyImpl
  parseK <- parseKeyE keyImpl
  rowK <- rowKeyE owner keyImpl
  keyFieldsE <-
    [|
      NE.fromList $(ListE <$> traverse fieldNameE (NE.toList (entityKey ent)))
      |]
  nameE <- [|TypeName $(txtE (unTypeName n))|]
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''LatticeEntity) (ConT name))
      [ TySynInstD (TySynEqn Nothing (AppT (ConT ''EntityKey) (ConT name)) keyT)
      , ValD (VarP 'entityName) (NormalB (LamE [WildP] nameE)) []
      , ValD (VarP 'entityKeyFields) (NormalB (LamE [WildP] keyFieldsE)) []
      , ValD (VarP 'renderEntityKey) (NormalB (LamE [WildP] renderK)) []
      , ValD (VarP 'parseEntityKey) (NormalB (LamE [WildP] parseK)) []
      , ValD (VarP 'fullRowKey) (NormalB rowK) []
      , ValD (VarP 'toRowFields) (NormalB toRowE) []
      , ValD (VarP 'toPartialRowFields) (NormalB toPartialE) []
      , ValD (VarP 'fromRowFields) (NormalB fromRowE) []
      , ValD (VarP 'fromPartialRowFields) (NormalB fromPartialE) []
      ]


data Shapeish = Fullish | Partialish


{- | @\r -> Map.fromList (catMaybes […])@: required 'Full' fields always
present, everything else present-when-'Just', absent fields omitted.
-}
rowsToMapE :: Text -> [(FieldName, FieldDef)] -> Shapeish -> Q Exp
rowsToMapE owner fields shape = do
  r <- newName "r"
  entries <-
    traverse
      ( \(f, fd) -> do
          let sel = pure (AppE (VarE (selName owner f)) (VarE r))
              isBare = case shape of
                Fullish -> not (isOptional (fieldType fd))
                Partialish -> False
          if isBare
            then [|Just ($(fieldNameE f), toWire $(sel))|]
            else [|fmap (\v -> ($(fieldNameE f), toWire v)) $(sel)|]
      )
      fields
  body <- [|Map.fromList (catMaybes $(pure (ListE entries)))|]
  pure (LamE [VarP r] body)


rowsFromMapE :: Name -> Text -> [(FieldName, FieldDef)] -> Shapeish -> Q Exp
rowsFromMapE name owner fields shape = do
  m <- newName "m"
  let fieldsOf =
        map
          ( \(f, fd) ->
              ( f
              , case shape of
                  Fullish -> isOptional (fieldType fd)
                  Partialish -> True
              )
          )
          fields
  inner <- fieldsFromMapE (ConE name) fieldsOf m
  let _ = owner
  pure (LamE [VarP m] inner)


renderKeyE :: Text -> KeyImpl -> Q Exp
renderKeyE owner = \case
  SingleKey _ _ -> [|renderScalarKey . toWire|]
  CompositeKey _ comps -> do
    k <- newName "k"
    parts <-
      traverse
        (\(f, _) -> [|renderScalarKey (toWire ($(varE (selName (owner <> "Key") f)) $(varE k)))|])
        comps
    body <- [|T.intercalate $(txtE ",") $(pure (ListE parts))|]
    pure (LamE [VarP k] body)


parseKeyE :: KeyImpl -> Q Exp
parseKeyE = \case
  SingleKey _ form ->
    [|\t -> scalarKeyFromText $(formE form) t >>= either (const Nothing) Just . fromWire|]
  CompositeKey keyName comps -> do
    t <- newName "t"
    partNames <- traverse (\i -> newName ("p" <> show i)) [1 .. length comps]
    partExprs <-
      traverse
        ( \((_, form), p) ->
            [|scalarKeyFromText $(formE form) $(varE p) >>= either (const Nothing) Just . fromWire|]
        )
        (zip comps partNames)
    let chain = case partExprs of
          [] -> AppE (ConE 'Just) (ConE keyName)
          p0 : rest ->
            foldl
              (\acc p -> InfixE (Just acc) (VarE '(<*>)) (Just p))
              (InfixE (Just (ConE keyName)) (VarE '(<$>)) (Just p0))
              rest
    splitE <- [|T.splitOn $(txtE ",") $(varE t)|]
    fallthrough <- [|Nothing|]
    pure $
      LamE [VarP t] $
        CaseE
          splitE
          [ Match (ListP (map VarP partNames)) (NormalB chain) []
          , Match WildP (NormalB fallthrough) []
          ]


formE :: ScalarKeyForm -> Q Exp
formE = \case
  KeyText -> [|KeyText|]
  KeyNumber -> [|KeyNumber|]
  KeyBool -> [|KeyBool|]


rowKeyE :: Text -> KeyImpl -> Q Exp
rowKeyE owner = \case
  SingleKey f _ -> pure (VarE (selName owner f))
  CompositeKey keyName comps -> do
    r <- newName "r"
    let args = map (\(f, _) -> AppE (VarE (selName owner f)) (VarE r)) comps
    pure (LamE [VarP r] (foldl AppE (ConE keyName) args))


-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

isOptional :: FieldType -> Bool
isOptional = \case
  TOptional _ -> True
  _ -> False


splitOptional :: FieldType -> (Bool, FieldType)
splitOptional = \case
  TOptional t -> (True, t)
  t -> (False, t)
