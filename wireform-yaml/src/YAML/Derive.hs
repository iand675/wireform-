{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for YAML
'YAML.Class.ToYAML' / 'YAML.Class.FromYAML' instances.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — YAML mapping with text-string keys.
* 'TypeShapeEnum'    — YAML scalar 'YV.YString' carrying the
  (possibly renamed) constructor name.
* 'TypeShapeSum'     — YAML mapping
  @{ tag: \"Ctor\", contents: ... }@; nullary constructors omit
  the @contents@ key entirely (mirroring the TOML / EDN derivers).

Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
'defaults', 'optional', 'coerced'.
-}
module YAML.Derive (
  deriveYAML,
  deriveToYAML,
  deriveFromYAML,
) where

import Data.Coerce (Coercible, coerce)
import Data.Foldable (foldlM)
import Data.List (nub)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo
import YAML.Class qualified as Y
import YAML.Value qualified as YV


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveYAML :: Name -> Q [Dec]
deriveYAML nm = (++) <$> deriveToYAML nm <*> deriveFromYAML nm


deriveToYAML :: Name -> Q [Dec]
deriveToYAML nm = do
  ti <- reifyTypeInfo nm
  context <- instanceContext ''Y.ToYAML ti
  body <- toYAMLBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          context
          (AppT (ConT ''Y.ToYAML) typ)
          [FunD 'Y.toYAML [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromYAML :: Name -> Q [Dec]
deriveFromYAML nm = do
  ti <- reifyTypeInfo nm
  context <- instanceContext ''Y.FromYAML ti
  body <- fromYAMLBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          context
          (AppT (ConT ''Y.FromYAML) typ)
          [FunD 'Y.fromYAML [Clause [] (NormalB body) []]]
  pure [decl]


-- Constrain the variables used by serialized fields. An applied type variable
-- (f a) needs a constraint on the application, rather than the higher-kinded f.
instanceContext :: Name -> TypeInfo -> Q Cxt
instanceContext cls ti = do
  constraints <- case typeInfoShape ti of
    TypeShapeNewtype constructor -> pure (concatMap ordinary (conInfoFields constructor))
    TypeShapeRecord constructor -> concat <$> traverse fieldConstraints (conInfoFields constructor)
    TypeShapeEnum _ -> pure []
    TypeShapeSum constructors -> concat <$> traverse (\constructor -> if conInfoIsRecord constructor then concat <$> traverse fieldConstraints (conInfoFields constructor) else pure (concatMap ordinary (conInfoFields constructor))) constructors
  pure (nub (typeInfoContext ti <> constraints))
  where
    ordinary = (AppT (ConT cls) <$>) . variables . fieldInfoType
    fieldConstraints field@(FieldInfo selector typ) = case selector of
      Nothing -> pure (ordinary field)
      Just name -> do
        modifier <- reifyModifierInfoFor backendYAML name
        pure $
          if miSkip modifier
            then []
            else case miCoerce modifier of
              Nothing -> ordinary field
              Just target
                | null (variables typ) -> []
                | otherwise ->
                    let wireType = if miRequired modifier == Just False then AppT (ConT ''Maybe) (ConT target) else ConT target
                    in [AppT (AppT (ConT ''Coercible) typ) wireType]
    variables typ
      | variableHead typ = [typ]
    variables (AppT left right) = variables left <> variables right
    variables (SigT typ _) = variables typ
    variables (ParensT typ) = variables typ
    variables _ = []
    variableHead (VarT _) = True
    variableHead (AppT typ _) = variableHead typ
    variableHead _ = False


-- ---------------------------------------------------------------------------
-- ToYAML
-- ---------------------------------------------------------------------------

toYAMLBody :: TypeInfo -> Q Exp
toYAMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toYAMLNewtype c
  TypeShapeRecord c -> toYAMLRecord c
  TypeShapeEnum cs -> toYAMLEnum cs
  TypeShapeSum cs -> toYAMLSum cs


toYAMLNewtype :: ConInfo -> Q Exp
toYAMLNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|Y.toYAML ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|Y.toYAML $(varE x)|]
  _ -> fail "YAML.Derive: newtype must have exactly one field"


toYAMLRecord :: ConInfo -> Q Exp
toYAMLRecord c = do
  x <- newName "x"
  pairs <- recordToYAMLPairs (varE x) c
  lamE [varP x] [|YV.YMap (V.fromList $(pure pairs))|]


recordToYAMLPairs :: Q Exp -> ConInfo -> Q Exp
recordToYAMLPairs varExp c = do
  pairExpss <- mapM (toYAMLField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toYAMLField :: Q Exp -> FieldInfo -> Q [Exp]
toYAMLField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendYAML selName
  if miSkip mi
    then pure []
    else do
      let selBase = Text.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|Y.toYAML $getter|]
            Just target -> [|Y.toYAML $(sigE [|coerce $getter|] (pure (if miRequired mi == Just False then AppT (ConT ''Maybe) (ConT target) else ConT target)))|]
      pair <- [|(YV.YString $(pure keyExp), $encoded)|]
      pure [pair]


toYAMLEnum :: [ConInfo] -> Q Exp
toYAMLEnum cs = do
  v <- newName "v"
  matches <- mapM enumToYAMLMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToYAMLMatch :: ConInfo -> Q Match
enumToYAMLMatch c = do
  mi <- reifyModifierInfoFor backendYAML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  body <- [|YV.YString $(pure keyExp)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toYAMLSum :: [ConInfo] -> Q Exp
toYAMLSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToYAML cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToYAML :: ConInfo -> Q Match
sumCtorToYAML c
  | conInfoIsRecord c
  , not (null (conInfoFields c)) = do
      modifier <- reifyModifierInfoFor backendYAML (conInfoName c)
      tag <- renderWireKey modifier (Text.pack (nameBase (conInfoName c)))
      record <- newName "record"
      pairs <- recordToYAMLPairs (varE record) c
      body <- [|YV.YMap (V.fromList [(YV.YString "tag", YV.YString $(pure tag)), (YV.YString "contents", YV.YMap (V.fromList $(pure pairs)))])|]
      let pattern_ = AsP record (ConP (conInfoName c) [] (replicate (length (conInfoFields c)) WildP))
      pure (Match pattern_ (NormalB body) [])
  | otherwise = sumCtorToYAMLPositional c


sumCtorToYAMLPositional :: ConInfo -> Q Match
sumCtorToYAMLPositional c = do
  mi <- reifyModifierInfoFor backendYAML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  body <- case fieldNames of
    [] ->
      [|
        YV.YMap
          ( V.fromList
              [ (YV.YString (Text.pack "tag"), YV.YString $(pure keyExp))
              ]
          )
        |]
    [n] ->
      [|
        YV.YMap
          ( V.fromList
              [ (YV.YString (Text.pack "tag"), YV.YString $(pure keyExp))
              , (YV.YString (Text.pack "contents"), Y.toYAML $(varE n))
              ]
          )
        |]
    ns ->
      [|
        YV.YMap
          ( V.fromList
              [ (YV.YString (Text.pack "tag"), YV.YString $(pure keyExp))
              ,
                ( YV.YString (Text.pack "contents")
                , YV.YSeq
                    ( V.fromList
                        $(pure (ListE (map (AppE (VarE 'Y.toYAML) . VarE) ns)))
                    )
                )
              ]
          )
        |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromYAML
-- ---------------------------------------------------------------------------

fromYAMLBody :: TypeInfo -> Q Exp
fromYAMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromYAMLNewtype c
  TypeShapeRecord c -> fromYAMLRecord c
  TypeShapeEnum cs -> fromYAMLEnum cs
  TypeShapeSum cs -> fromYAMLSum cs


fromYAMLNewtype :: ConInfo -> Q Exp
fromYAMLNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . Y.fromYAML|]
  _ -> fail "YAML.Derive: newtype must have exactly one field"


fromYAMLRecord :: ConInfo -> Q Exp
fromYAMLRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  let unwrapped = AppE (VarE 'YV.unwrap) (VarE v)
  lamE
    [varP v]
    ( caseE
        (pure unwrapped)
        [ match
            (conP 'YV.YMap [varP kvs])
            (normalB [|Y.validateFieldNames $(varE kvs) >> $(pure bodyE)|])
            []
        , match
            wildP
            ( normalB
                [|Left "YAML.Derive: expected YMap for record type"|]
            )
            []
        ]
    )


recordParser :: Name -> ConInfo -> Q Exp
recordParser kvs c = do
  let conName = conInfoName c
      fields = conInfoFields c
  case fields of
    [] -> [|Right $(conE conName)|]
    (f0 : fs) -> do
      e0 <- fieldParser kvs f0
      hd <- [|$(conE conName) <$> $(pure e0)|]
      foldlM
        ( \acc f -> do
            ef <- fieldParser kvs f
            [|$(pure acc) <*> $(pure ef)|]
        )
        hd
        fs


fieldParser :: Name -> FieldInfo -> Q Exp
fieldParser kvs (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendYAML selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "YAML.Derive: missing 'defaults' for skipped field "
                ++ $(litE (stringL (nameBase selName)))
            )
          |]
    else do
      let selBase = Text.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let isOptional = miRequired mi == Just False
          targetType target = if isOptional then AppT (ConT ''Maybe) (ConT target) else ConT target
      missing <- case miDefaults mi of
        Just def -> [|Right $(varE def)|]
        Nothing | isOptional -> [|Right Nothing|]
        Nothing -> [|Left ("YAML.Derive: missing field " ++ Text.unpack $(pure keyExp))|]
      input <- newName "input"
      present <- case miCoerce mi of
        Nothing -> [|Y.fromYAML $(varE input)|]
        Just target -> [|coerce <$> $(sigE [|Y.fromYAML $(varE input)|] (pure (AppT (AppT (ConT ''Either) (ConT ''String)) (targetType target))))|]
      [|
        case lookupYAMLField $(pure keyExp) $(varE kvs) of
          Nothing -> $(pure missing)
          Just $(varP input) -> $(pure present)
        |]


-- | Linear scan for a key in a 'YV.YMap'.
lookupYAMLField
  :: Text.Text
  -> V.Vector (YV.Value, YV.Value)
  -> Maybe YV.Value
lookupYAMLField nm kvs = V.foldr step Nothing kvs
  where
    step (k, val) acc = case YV.unwrap k of
      YV.YString s | s == nm -> Just val
      _ -> acc


fromYAMLEnum :: [ConInfo] -> Q Exp
fromYAMLEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            ( AppE
                (VarE 'mappend)
                (LitE (StringL "YAML.Derive: unknown enum value "))
            )
            (AppE (VarE 'show) (VarE s))
        )
      multi =
        MultiIfE
          ( map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
              ++ [(NormalG (ConE 'True), AppE (ConE 'Left) (snd fallback))]
          )
      unwrapped = AppE (VarE 'YV.unwrap) (VarE v)
  lamE
    [varP v]
    ( caseE
        (pure unwrapped)
        [ match
            (conP 'YV.YString [varP s])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "YAML.Derive: enum expected YString"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendYAML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromYAMLSum :: [ConInfo] -> Q Exp
fromYAMLSum cs = do
  v <- newName "v"
  kvs <- newName "kvs"
  tagVar <- newName "tag"
  cVar <- newName "c"
  branches <- mapM (sumDispatch tagVar cVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            (ConE 'Left)
            ( AppE
                ( AppE
                    (VarE 'mappend)
                    (LitE (StringL "YAML.Derive: unknown sum tag "))
                )
                (AppE (VarE 'show) (VarE tagVar))
            )
        )
      multi = MultiIfE (branches ++ [fallback])
      unwrapped = AppE (VarE 'YV.unwrap) (VarE v)
  lamE
    [varP v]
    ( caseE
        (pure unwrapped)
        [ match
            (conP 'YV.YMap [varP kvs])
            ( normalB
                [|
                  do
                    Y.validateFieldNames $(varE kvs)
                    $(varP tagVar) <-
                      case lookupYAMLField (Text.pack "tag") $(varE kvs) of
                        Just t -> case YV.unwrap t of
                          YV.YString s -> Right s
                          _ -> Left "YAML.Derive: sum 'tag' not a string"
                        _ -> Left "YAML.Derive: sum missing 'tag'"
                    let $(varP cVar) =
                          lookupYAMLField (Text.pack "contents") $(varE kvs)
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "YAML.Derive: sum expected YMap"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendYAML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <-
    if conInfoIsRecord c && not (null (conInfoFields c))
      then recordSumParser cVar c
      else case conInfoFields c of
        [] ->
          let conNameStr = nameBase (conInfoName c)
          in [|
               case $(varE cVar) of
                 Nothing -> Right $(conE (conInfoName c))
                 Just _ ->
                   Left
                     ( "YAML.Derive: "
                         ++ conNameStr
                         ++ " is nullary; unexpected 'contents'"
                     )
               |]
        [_one] ->
          let conNameStr = nameBase (conInfoName c)
          in [|
               case $(varE cVar) of
                 Just inner -> fmap $(conE (conInfoName c)) (Y.fromYAML inner)
                 Nothing ->
                   Left
                     ( "YAML.Derive: "
                         ++ conNameStr
                         ++ " missing 'contents'"
                     )
               |]
        many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


recordSumParser :: Name -> ConInfo -> Q Exp
recordSumParser contents constructor = do
  fields <- newName "fields"
  parser <- recordParser fields constructor
  [|
    do
      $(varP fields) <- case $(varE contents) of
        Nothing -> Right V.empty
        Just inner -> case YV.unwrap inner of
          YV.YMap entries -> Right entries
          _ -> Left "YAML.Derive: record constructor contents must be a mapping"
      Y.validateFieldNames $(varE fields)
      $(pure parser)
    |]


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|Y.fromYAML ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
  hd <- do
    e0 <- parseI 0
    [|$(conE conName) <$> $(pure e0)|]
  body <-
    foldlM
      ( \acc i -> do
          ei <- parseI i
          [|$(pure acc) <*> $(pure ei)|]
      )
      hd
      [1 .. arity - 1]
  let conNameStr = nameBase conName
      arityStr = show arity
  [|
    case $(varE cVar) of
      Just inner -> case YV.unwrap inner of
        YV.YSeq $(varP arr)
          | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
              $(pure body)
          | otherwise ->
              Left
                ( "YAML.Derive: "
                    ++ conNameStr
                    ++ " expected "
                    ++ arityStr
                    ++ " contents, got "
                    ++ show (V.length $(varE arr))
                )
        _ ->
          Left
            ( "YAML.Derive: "
                ++ conNameStr
                ++ " expected YSeq contents"
            )
      Nothing ->
        Left
          ( "YAML.Derive: "
              ++ conNameStr
              ++ " missing 'contents'"
          )
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "YAML.Derive: cannot derive YAML for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
