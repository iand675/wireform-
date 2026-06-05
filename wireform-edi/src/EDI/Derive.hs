{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotation-driven Template Haskell deriver for EDI.
--
-- Records encode as one segment whose tag is the constructor name (honouring
-- constructor rename annotations) and whose fields are positional elements.
-- Sum constructors encode as one segment tagged by the constructor name.
module EDI.Derive
  ( deriveEDI
  , deriveToEDI
  , deriveFromEDI
  ) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Haskell.TH

import qualified EDI.Class as E
import qualified EDI.Value as EV
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

deriveEDI :: Name -> Q [Dec]
deriveEDI nm = (++) <$> deriveToEDI nm <*> deriveFromEDI nm

deriveToEDI :: Name -> Q [Dec]
deriveToEDI nm = do
  ti <- reifyTypeInfo nm
  body <- toEDIBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''E.ToEDI) typ)
          [FunD 'E.toEDI [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromEDI :: Name -> Q [Dec]
deriveFromEDI nm = do
  ti <- reifyTypeInfo nm
  body <- fromEDIBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''E.FromEDI) typ)
          [FunD 'E.fromEDI [Clause [] (NormalB body) []]]
  pure [decl]

toEDIBody :: TypeInfo -> Q Exp
toEDIBody ti =
  case typeInfoShape ti of
    TypeShapeNewtype c -> toEDINewtype c
    TypeShapeRecord c -> toEDIRecord c
    TypeShapeEnum cs -> toEDIEnum cs
    TypeShapeSum cs -> toEDISum cs

toEDINewtype :: ConInfo -> Q Exp
toEDINewtype c =
  case conInfoFields c of
    [FieldInfo (Just sel) _] -> do
      x <- newName "x"
      lamE [varP x] [| E.toEDI ($(varE sel) $(varE x)) |]
    [FieldInfo Nothing _] -> do
      x <- newName "x"
      lamE [conP (conInfoName c) [varP x]] [| E.toEDI $(varE x) |]
    _ -> fail "EDI.Derive: newtype must have exactly one field"

toEDIRecord :: ConInfo -> Q Exp
toEDIRecord c = do
  x <- newName "x"
  tag <- segmentTagFor c
  elems <- recordElements (varE x) c
  lamE
    [varP x]
    [| E.singletonInterchange
         (EV.Segment $(pure tag) (V.fromList $(pure elems))) |]

recordElements :: Q Exp -> ConInfo -> Q Exp
recordElements varExp c = do
  elemss <- mapM (fieldElement varExp) (conInfoFields c)
  pure (ListE (concat elemss))

fieldElement :: Q Exp -> FieldInfo -> Q [Exp]
fieldElement varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendEDI selName
  if miSkip mi
    then pure []
    else do
      let getter = appE (varE selName) varExp
          encoded =
            case miCoerce mi of
              Nothing -> [| E.toEDIElement $getter |]
              Just _ -> [| E.toEDIElement (coerce $getter) |]
      elemExp <- encoded
      pure [elemExp]

toEDIEnum :: [ConInfo] -> Q Exp
toEDIEnum cs = do
  v <- newName "v"
  matches <- mapM enumToEDIMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

enumToEDIMatch :: ConInfo -> Q Match
enumToEDIMatch c = do
  tag <- segmentTagFor c
  body <- [| E.singletonInterchange (EV.Segment $(pure tag) V.empty) |]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])

toEDISum :: [ConInfo] -> Q Exp
toEDISum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToEDI cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

sumCtorToEDI :: ConInfo -> Q Match
sumCtorToEDI c = do
  tag <- segmentTagFor c
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      elemList = ListE (map (AppE (VarE 'E.toEDIElement) . VarE) fieldNames)
  body <- [| E.singletonInterchange
              (EV.Segment $(pure tag) (V.fromList $(pure elemList))) |]
  pure (Match pat (NormalB body) [])

fromEDIBody :: TypeInfo -> Q Exp
fromEDIBody ti =
  case typeInfoShape ti of
    TypeShapeNewtype c -> fromEDINewtype c
    TypeShapeRecord c -> fromEDIRecord c
    TypeShapeEnum cs -> fromEDIEnum cs
    TypeShapeSum cs -> fromEDISum cs

fromEDINewtype :: ConInfo -> Q Exp
fromEDINewtype c =
  case conInfoFields c of
    [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . E.fromEDI |]
    _ -> fail "EDI.Derive: newtype must have exactly one field"

fromEDIRecord :: ConInfo -> Q Exp
fromEDIRecord c = do
  doc <- newName "doc"
  seg <- newName "seg"
  tag <- segmentTagFor c
  parser <- recordParser seg c
  lamE
    [varP doc]
    [| case firstSegment $(varE doc) of
         Nothing -> Left "EDI.Derive: expected one segment"
         Just $(varP seg)
           | EV.segmentTag $(varE seg) == $(pure tag) -> $(pure parser)
           | otherwise ->
               Left ("EDI.Derive: expected segment "
                     ++ T.unpack $(pure tag)
                     ++ ", got "
                     ++ T.unpack (EV.segmentTag $(varE seg))) |]

recordParser :: Name -> ConInfo -> Q Exp
recordParser seg c =
  case conInfoFields c of
    [] -> [| Right $(conE (conInfoName c)) |]
    fields -> buildSequence seg (conInfoName c) fields

buildSequence :: Name -> Name -> [FieldInfo] -> Q Exp
buildSequence seg conName = go 0 []
  where
    go _ acc [] = do
      let assemble = foldl (\fn arg -> AppE fn (VarE arg)) (ConE conName) (reverse acc)
      [| Right $(pure assemble) |]
    go pos acc (f : fs) = do
      vName <- newName "v"
      (elemParser, advance) <- fieldParser seg pos f
      rest <- go (pos + advance) (vName : acc) fs
      [| $(pure elemParser) >>= \$(varP vName) -> $(pure rest) |]

fieldParser :: Name -> Int -> FieldInfo -> Q (Exp, Int)
fieldParser seg pos (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendEDI selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> do
        e <- [| Right $(varE defNm) |]
        pure (e, 0)
      Nothing -> do
        e <- [| Left $(litE (stringL ("EDI.Derive: missing 'defaults' for skipped field " ++ nameBase selName))) |]
        pure (e, 0)
    else do
      let posLit = litE (integerL (fromIntegral pos))
          base =
            [| case elementAt $posLit $(varE seg) of
                 Nothing -> Left ("EDI.Derive: segment missing element at index "
                                  ++ show ($posLit :: Int))
                 Just elemValue -> E.fromEDIElement elemValue |]
      e <- case miCoerce mi of
        Nothing -> base
        Just _ -> [| fmap coerce $base |]
      pure (e, 1)

fromEDIEnum :: [ConInfo] -> Q Exp
fromEDIEnum cs = do
  doc <- newName "doc"
  seg <- newName "seg"
  tag <- newName "tag"
  branches <- mapM (enumDispatch tag) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            (ConE 'Left)
            (AppE
              (AppE (VarE 'mappend) (LitE (StringL "EDI.Derive: unknown enum segment ")))
              (AppE (VarE 'T.unpack) (VarE tag)))
        )
      multi = MultiIfE (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches ++ [fallback])
  lamE
    [varP doc]
    [| case firstSegment $(varE doc) of
         Nothing -> Left "EDI.Derive: expected one enum segment"
         Just $(varP seg) ->
           let $(varP tag) = EV.segmentTag $(varE seg)
           in $(pure multi) |]

enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch tagVar c = do
  tag <- segmentTagFor c
  pure (NormalG (InfixE (Just (VarE tagVar)) (VarE '(==)) (Just tag)), ConE (conInfoName c))

fromEDISum :: [ConInfo] -> Q Exp
fromEDISum cs = do
  doc <- newName "doc"
  seg <- newName "seg"
  tag <- newName "tag"
  branches <- mapM (sumDispatch tag seg) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            (ConE 'Left)
            (AppE
              (AppE (VarE 'mappend) (LitE (StringL "EDI.Derive: unknown sum segment ")))
              (AppE (VarE 'T.unpack) (VarE tag)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE
    [varP doc]
    [| case firstSegment $(varE doc) of
         Nothing -> Left "EDI.Derive: expected one sum segment"
         Just $(varP seg) ->
           let $(varP tag) = EV.segmentTag $(varE seg)
           in $(pure multi) |]

sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar segVar c = do
  tag <- segmentTagFor c
  body <- sumBody segVar (conInfoName c) (length (conInfoFields c))
  pure (NormalG (InfixE (Just (VarE tagVar)) (VarE '(==)) (Just tag)), body)

sumBody :: Name -> Name -> Int -> Q Exp
sumBody seg conName arity =
  build 0 []
  where
    build ix acc
      | ix >= arity = do
          let assemble = foldl (\fn arg -> AppE fn (VarE arg)) (ConE conName) (reverse acc)
          [| Right $(pure assemble) |]
      | otherwise = do
          value <- newName "value"
          let posLit = litE (integerL (fromIntegral ix))
              parser =
                [| case elementAt $posLit $(varE seg) of
                     Nothing -> Left ("EDI.Derive: segment missing element at index "
                                      ++ show ($posLit :: Int))
                     Just elemValue -> E.fromEDIElement elemValue |]
          rest <- build (ix + 1) (value : acc)
          [| $parser >>= \$(varP value) -> $(pure rest) |]

segmentTagFor :: ConInfo -> Q Exp
segmentTagFor c = do
  mi <- reifyModifierInfoFor backendEDI (conInfoName c)
  renderWireKey mi (T.pack (nameBase (conInfoName c)))

firstSegment :: EV.Interchange -> Maybe EV.Segment
firstSegment doc = fst <$> V.uncons (EV.interchangeSegments doc)

elementAt :: Int -> EV.Segment -> Maybe EV.Element
elementAt ix seg = EV.segmentElements seg V.!? ix

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "EDI.Derive: cannot derive EDI for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
