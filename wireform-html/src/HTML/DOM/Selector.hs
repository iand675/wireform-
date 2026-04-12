{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | CSS selector matching against 'ElementIndex' and zipper 'Node's (internal to @HTML.DOM@).
module HTML.DOM.Selector
  ( matchesSelectorIdx
  , matchesComplexIdx
  , matchesCompoundIdx
  , matchContextIdx
  , decomposeComplex
  , matchesSelector
  , matchesComplex
  , matchesCompound
  , matchCompoundFlat
  , matchSubFlat
  , matchComplexFlat
  ) where
import Data.Primitive.PrimArray (indexPrimArray)
import Data.Primitive.SmallArray
  ( SmallArray, sizeofSmallArray, indexSmallArray
  )
import Data.Text (Text)
import qualified Data.Text as T

import HTML.DOM.Index (ElementIndex(..))
import HTML.DOM.Zipper
  ( Crumb(..), Node(..)
  , childNodes, nextSibling, prevSibling, parentNode
  )
import qualified HTML.Selector as Sel
import HTML.Value (HTMLNode(..), HTMLAttribute(..))

-- ---------------------------------------------------------------------------
-- Shared tag+attrs-only subselector matching
-- ---------------------------------------------------------------------------

isAttributeSubSel :: Sel.SubSel -> Bool
isAttributeSubSel = \case
  Sel.SelId _ -> True
  Sel.SelClass _ -> True
  Sel.SelAttrExists _ -> True
  Sel.SelAttrExact {} -> True
  Sel.SelAttrPrefix {} -> True
  Sel.SelAttrSuffix {} -> True
  Sel.SelAttrContains {} -> True
  Sel.SelAttrWord {} -> True
  Sel.SelAttrHyphen {} -> True
  _ -> False

{-# INLINE matchSubCommon #-}
matchSubCommon :: Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Maybe Bool
matchSubCommon _tag attrs sub
  | isAttributeSubSel sub = Just (Sel.matchSub attrs sub)
matchSubCommon _ _ Sel.SelNeverMatch = Just False
matchSubCommon tag attrs Sel.SelChecked =
  Just $
    (tag == "input" && Sel.attrExists "checked" attrs)
    || (tag == "option" && Sel.attrExists "selected" attrs)
matchSubCommon tag attrs Sel.SelRequired =
  Just $ isFormInputElement tag && Sel.attrExists "required" attrs
matchSubCommon tag attrs Sel.SelOptional =
  Just $ isFormInputElement tag && not (Sel.attrExists "required" attrs)
matchSubCommon tag attrs Sel.SelDefault = Just $ matchDefaultIdx tag attrs
matchSubCommon tag attrs Sel.SelPlaceholderShown =
  Just $
    (tag == "input" || tag == "textarea")
    && Sel.attrExists "placeholder" attrs
    && not (hasNonEmptyValue attrs)
matchSubCommon tag attrs Sel.SelIndeterminate =
  Just $ tag == "input" && matchIndeterminate attrs
matchSubCommon tag attrs Sel.SelLink =
  Just $
    (tag == "a" || tag == "area" || tag == "link") && Sel.attrExists "href" attrs
matchSubCommon _ _ Sel.SelDefined = Just True
matchSubCommon _ _ Sel.SelTarget = Just False
matchSubCommon _ _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Index-based matching
-- ---------------------------------------------------------------------------

matchesSelectorIdx :: Sel.Selector -> ElementIndex -> Int -> Bool
matchesSelectorIdx (Sel.Selector [cs]) idx i = matchesComplexIdx idx i cs
matchesSelectorIdx (Sel.Selector complexSels) idx i =
  any (matchesComplexIdx idx i) complexSels

matchesComplexIdx :: ElementIndex -> Int -> Sel.ComplexSelector -> Bool
matchesComplexIdx idx i (Sel.ComplexSelector compound []) =
  matchesCompoundIdx idx i compound
matchesComplexIdx idx i cs =
  let !(subject, ctx) = decomposeComplex cs
  in matchesCompoundIdx idx i subject && matchContextIdx idx i ctx
{-# INLINE matchesComplexIdx #-}

matchesCompoundIdx :: ElementIndex -> Int -> Sel.CompoundSelector -> Bool
matchesCompoundIdx idx i (Sel.CompoundSelector mtype subs) =
  case indexSmallArray (eiNodes idx) i of
    HTMLElement tag attrs _ ->
      Sel.matchType mtype tag && allSubsIdx idx i tag attrs subs
    _ -> False
{-# INLINE matchesCompoundIdx #-}

allSubsIdx :: ElementIndex -> Int -> Text -> SmallArray HTMLAttribute -> [Sel.SubSel] -> Bool
allSubsIdx _ _ !_ !_ [] = True
allSubsIdx idx i tag attrs (s : rest) =
  matchSubIdx idx i tag attrs s && allSubsIdx idx i tag attrs rest
{-# INLINE allSubsIdx #-}

matchSubIdx :: ElementIndex -> Int -> Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Bool
matchSubIdx idx i tag attrs sub =
  case matchSubCommon tag attrs sub of
    Just b -> b
    Nothing -> case sub of
      Sel.SelFirstChild ->
        indexPrimArray (eiElemPos idx) i == 1
      Sel.SelLastChild ->
        indexPrimArray (eiElemPos idx) i == indexPrimArray (eiElemCnt idx) i
      Sel.SelOnlyChild ->
        indexPrimArray (eiElemCnt idx) i == 1
      Sel.SelNthChild a b ->
        Sel.nthMatch a b (fromIntegral (indexPrimArray (eiElemPos idx) i))
      Sel.SelNthLastChild a b ->
        let !pos = indexPrimArray (eiElemPos idx) i
            !cnt = indexPrimArray (eiElemCnt idx) i
        in Sel.nthMatch a b (fromIntegral (cnt - pos + 1))

      Sel.SelFirstOfType -> typeIndexIdx idx i tag == 1
      Sel.SelLastOfType -> typeIndexFromEndIdx idx i tag == 1
      Sel.SelOnlyOfType -> typeIndexIdx idx i tag == 1 && typeIndexFromEndIdx idx i tag == 1
      Sel.SelNthOfType a b -> Sel.nthMatch a b (typeIndexIdx idx i tag)
      Sel.SelNthLastOfType a b -> Sel.nthMatch a b (typeIndexFromEndIdx idx i tag)

      Sel.SelEmpty ->
        case indexSmallArray (eiNodes idx) i of
          HTMLElement _ _ children -> allCommentsOnly children 0 (sizeofSmallArray children)
          _ -> False
      Sel.SelRoot ->
        indexPrimArray (eiParent idx) i < 0

      Sel.SelNot sel -> not (matchesSelectorIdx sel idx i)
      Sel.SelIs sel -> matchesSelectorIdx sel idx i
      Sel.SelWhere sel -> matchesSelectorIdx sel idx i
      Sel.SelHas rels ->
        any (uncurry (matchHasRelIdx idx i)) rels

      Sel.SelNthChildOf a b sel ->
        matchesSelectorIdx sel idx i
        && Sel.nthMatch a b (nthChildOfIdx idx i sel True)
      Sel.SelNthLastChildOf a b sel ->
        matchesSelectorIdx sel idx i
        && Sel.nthMatch a b (nthChildOfIdx idx i sel False)

      Sel.SelScope ->
        indexPrimArray (eiParent idx) i < 0
      Sel.SelBlank ->
        case indexSmallArray (eiNodes idx) i of
          HTMLElement _ _ children -> allBlankChildren children 0 (sizeofSmallArray children)
          _ -> False
      Sel.SelDir dir' -> matchDirIdx idx i dir'

      Sel.SelEnabled -> isFormElement tag && not (isActuallyDisabledIdx idx i tag attrs)
      Sel.SelDisabled -> isFormElement tag && isActuallyDisabledIdx idx i tag attrs
      Sel.SelReadOnly ->
        not (isFormInputElement tag)
        || Sel.attrExists "readonly" attrs
        || isActuallyDisabledIdx idx i tag attrs
      Sel.SelReadWrite ->
        isFormInputElement tag
        && not (Sel.attrExists "readonly" attrs)
        && not (isActuallyDisabledIdx idx i tag attrs)

      Sel.SelLang langTags -> matchLangIdx idx i langTags

      s -> Sel.matchSub attrs s
{-# INLINE matchSubIdx #-}

allCommentsOnly :: SmallArray HTMLNode -> Int -> Int -> Bool
allCommentsOnly !children !i !n
  | i >= n = True
  | HTMLComment {} <- indexSmallArray children i = allCommentsOnly children (i + 1) n
  | otherwise = False

typeIndexIdx :: ElementIndex -> Int -> Text -> Int
typeIndexIdx idx i tag = go 1 (fromIntegral (indexPrimArray (eiPrevElem idx) i) :: Int)
  where
    go !n prev
      | prev < 0 = n
      | HTMLElement t _ _ <- indexSmallArray (eiNodes idx) prev
      , t == tag = go (n + 1) (fromIntegral (indexPrimArray (eiPrevElem idx) prev))
      | otherwise = go n (fromIntegral (indexPrimArray (eiPrevElem idx) prev))

typeIndexFromEndIdx :: ElementIndex -> Int -> Text -> Int
typeIndexFromEndIdx idx i tag = go 1 (fromIntegral (indexPrimArray (eiNextElem idx) i) :: Int)
  where
    go !n nxt
      | nxt < 0 = n
      | HTMLElement t _ _ <- indexSmallArray (eiNodes idx) nxt
      , t == tag = go (n + 1) (fromIntegral (indexPrimArray (eiNextElem idx) nxt))
      | otherwise = go n (fromIntegral (indexPrimArray (eiNextElem idx) nxt))

matchContextIdx :: ElementIndex -> Int -> [(Sel.Combinator, Sel.CompoundSelector)] -> Bool
matchContextIdx _ _ [] = True
matchContextIdx idx i ((Sel.Descendant, comp) : rest) =
  anyAncestorIdx idx i (\j -> matchesCompoundIdx idx j comp && matchContextIdx idx j rest)
matchContextIdx idx i ((Sel.Child, comp) : rest) =
  let !pidx = fromIntegral (indexPrimArray (eiParent idx) i) :: Int
  in pidx >= 0 && matchesCompoundIdx idx pidx comp && matchContextIdx idx pidx rest
matchContextIdx idx i ((Sel.AdjacentSibling, comp) : rest) =
  let !prev = fromIntegral (indexPrimArray (eiPrevElem idx) i) :: Int
  in prev >= 0 && matchesCompoundIdx idx prev comp && matchContextIdx idx prev rest
matchContextIdx idx i ((Sel.GeneralSibling, comp) : rest) =
  anyPrevSibIdx idx i (\j -> matchesCompoundIdx idx j comp && matchContextIdx idx j rest)

anyAncestorIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyAncestorIdx idx !i f =
  let !pidx = fromIntegral (indexPrimArray (eiParent idx) i) :: Int
  in pidx >= 0 && (f pidx || anyAncestorIdx idx pidx f)

anyPrevSibIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyPrevSibIdx idx !i f =
  let !prev = fromIntegral (indexPrimArray (eiPrevElem idx) i) :: Int
  in prev >= 0 && (f prev || anyPrevSibIdx idx prev f)

anyDescendantIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyDescendantIdx idx !i f = go (i + 1)
  where
    !n = eiCount idx
    go !j
      | j >= n = False
      | not (isDescendantOf idx j i) = False
      | f j = True
      | otherwise = go (j + 1)

isDescendantOf :: ElementIndex -> Int -> Int -> Bool
isDescendantOf idx !j !i =
  j > i && j < fromIntegral (indexPrimArray (eiSubEnd idx) i)
{-# INLINE isDescendantOf #-}

matchLangIdx :: ElementIndex -> Int -> [Text] -> Bool
matchLangIdx idx !i langTags = go i
  where
    go !j
      | j < 0 = False
      | HTMLElement _ attrs _ <- indexSmallArray (eiNodes idx) j
      , Just val <- Sel.findAttr "lang" attrs =
          let !lval = T.toLower val
          in any (\lt -> lval == lt || T.isPrefixOf (lt <> "-") lval) langTags
      | otherwise = go (fromIntegral (indexPrimArray (eiParent idx) j))

decomposeComplex :: Sel.ComplexSelector -> (Sel.CompoundSelector, [(Sel.Combinator, Sel.CompoundSelector)])
decomposeComplex (Sel.ComplexSelector hd []) = (hd, [])
decomposeComplex (Sel.ComplexSelector hd tl) = go hd tl []
  where
    go !prev ((comb, comp) : rest) acc = go comp rest ((comb, prev) : acc)
    go !subj [] acc = (subj, acc)
{-# INLINE decomposeComplex #-}

matchesComplex :: Sel.ComplexSelector -> Node -> Bool
matchesComplex cs node =
  let (subject, context) = decomposeComplex cs
  in matchesCompound subject node && matchContext context node

matchContext :: [(Sel.Combinator, Sel.CompoundSelector)] -> Node -> Bool
matchContext [] _ = True
matchContext ((Sel.Descendant, comp) : rest) n =
  anyAncestor (\anc -> matchesCompound comp anc && matchContext rest anc) n
matchContext ((Sel.Child, comp) : rest) n =
  case parentNode n of
    Just p  -> matchesCompound comp p && matchContext rest p
    Nothing -> False
matchContext ((Sel.AdjacentSibling, comp) : rest) n =
  case prevElementSibling n of
    Just ps -> matchesCompound comp ps && matchContext rest ps
    Nothing -> False
matchContext ((Sel.GeneralSibling, comp) : rest) n =
  anyPrevElementSibling (\sib -> matchesCompound comp sib && matchContext rest sib) n

prevElementSibling :: Node -> Maybe Node
prevElementSibling n = case prevSibling n of
  Nothing -> Nothing
  Just s | isElement s -> Just s
         | otherwise -> prevElementSibling s

matchesCompound :: Sel.CompoundSelector -> Node -> Bool
matchesCompound (Sel.CompoundSelector mtype subs) node@(Node raw _) = case raw of
  HTMLElement tag attrs _ ->
    Sel.matchType mtype tag && all (matchSubDOM node tag attrs) subs
  _ -> False

matchSubDOM :: Node -> Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Bool
matchSubDOM node tag attrs sub =
  case matchSubCommon tag attrs sub of
    Just b -> b
    Nothing -> case sub of
      Sel.SelFirstChild -> isFirstElementChild node
      Sel.SelLastChild -> isLastElementChild node
      Sel.SelOnlyChild -> isFirstElementChild node && isLastElementChild node
      Sel.SelFirstOfType -> isFirstOfType tag node
      Sel.SelLastOfType -> isLastOfType tag node
      Sel.SelOnlyOfType -> isFirstOfType tag node && isLastOfType tag node
      Sel.SelNthChild a b -> Sel.nthMatch a b (elementIndex node)
      Sel.SelNthLastChild a b -> Sel.nthMatch a b (elementIndexFromEnd node)
      Sel.SelNthOfType a b -> Sel.nthMatch a b (typeIndex tag node)
      Sel.SelNthLastOfType a b -> Sel.nthMatch a b (typeIndexFromEnd tag node)
      Sel.SelEmpty -> isEmptyElement node
      Sel.SelRoot -> isRootNode node

      Sel.SelNot sel -> not (matchesSelector sel node)
      Sel.SelIs sel -> matchesSelector sel node
      Sel.SelWhere sel -> matchesSelector sel node
      Sel.SelHas rels ->
        any (uncurry (matchHasRel node)) rels

      Sel.SelNthChildOf a b sel ->
        matchesSelector sel node
        && Sel.nthMatch a b (nthChildOfNode node sel True)
      Sel.SelNthLastChildOf a b sel ->
        matchesSelector sel node
        && Sel.nthMatch a b (nthChildOfNode node sel False)

      Sel.SelScope -> isRootNode node
      Sel.SelBlank -> isBlankElement node
      Sel.SelDir dir' -> matchDir dir' node

      Sel.SelEnabled -> isFormElement tag && not (isActuallyDisabledDOM node tag attrs)
      Sel.SelDisabled -> isFormElement tag && isActuallyDisabledDOM node tag attrs
      Sel.SelReadOnly ->
        not (isFormInputElement tag)
        || Sel.attrExists "readonly" attrs
        || isActuallyDisabledDOM node tag attrs
      Sel.SelReadWrite ->
        isFormInputElement tag
        && not (Sel.attrExists "readonly" attrs)
        && not (isActuallyDisabledDOM node tag attrs)

      Sel.SelLang langTags -> matchLang langTags node

      Sel.SelNeverMatch -> False

      s -> Sel.matchSub attrs s

elementIndex :: Node -> Int
elementIndex = go 1
  where
    go !i node = case prevSibling node of
      Nothing -> i
      Just s -> if isElement s then go (i + 1) s else go i s

elementIndexFromEnd :: Node -> Int
elementIndexFromEnd = go 1
  where
    go !i node = case nextSibling node of
      Nothing -> i
      Just s -> if isElement s then go (i + 1) s else go i s

typeIndex :: Text -> Node -> Int
typeIndex t = go 1
  where
    go !i node = case prevSibling node of
      Nothing -> i
      Just s -> if nodeTagName s == Just t then go (i + 1) s else go i s

typeIndexFromEnd :: Text -> Node -> Int
typeIndexFromEnd t = go 1
  where
    go !i node = case nextSibling node of
      Nothing -> i
      Just s -> if nodeTagName s == Just t then go (i + 1) s else go i s

isFirstElementChild :: Node -> Bool
isFirstElementChild n = case prevSibling n of
  Nothing -> True
  Just s -> not (isElement s) && isFirstElementChild s

isLastElementChild :: Node -> Bool
isLastElementChild n = case nextSibling n of
  Nothing -> True
  Just s -> not (isElement s) && isLastElementChild s

isFirstOfType :: Text -> Node -> Bool
isFirstOfType t n = case prevSibling n of
  Nothing -> True
  Just s -> nodeTagName s /= Just t && isFirstOfType t s

isLastOfType :: Text -> Node -> Bool
isLastOfType t n = case nextSibling n of
  Nothing -> True
  Just s -> nodeTagName s /= Just t && isLastOfType t s

isEmptyElement :: Node -> Bool
isEmptyElement node = all isNotContentChild (childNodes node)
  where
    isNotContentChild (Node (HTMLComment _) _) = True
    isNotContentChild _ = False

isElement :: Node -> Bool
isElement (Node (HTMLElement {}) _) = True
isElement _ = False

nodeTagName :: Node -> Maybe Text
nodeTagName (Node (HTMLElement t _ _) _) = Just t
nodeTagName _ = Nothing

isRootNode :: Node -> Bool
isRootNode (Node _ []) = True
isRootNode (Node _ (Crumb {} : _)) = False

isFormElement :: Text -> Bool
isFormElement t =
  t == "input" || t == "select" || t == "textarea"
  || t == "button" || t == "fieldset"

isFormInputElement :: Text -> Bool
isFormInputElement t = t == "input" || t == "select" || t == "textarea"

isActuallyDisabledIdx :: ElementIndex -> Int -> Text -> SmallArray HTMLAttribute -> Bool
isActuallyDisabledIdx idx i tag attrs
  | tag == "fieldset" = Sel.attrExists "disabled" attrs
  | Sel.attrExists "disabled" attrs = True
  | otherwise = hasDisabledFieldsetAncestorIdx idx i

hasDisabledFieldsetAncestorIdx :: ElementIndex -> Int -> Bool
hasDisabledFieldsetAncestorIdx idx !i = go i
  where
    go !j =
      let !pj = fromIntegral (indexPrimArray (eiParent idx) j) :: Int
      in pj >= 0 && case indexSmallArray (eiNodes idx) pj of
           HTMLElement "fieldset" pattrs _ ->
             if Sel.attrExists "disabled" pattrs
             then not (isInsideFirstLegendIdx idx j pj)
             else go pj
           HTMLElement {} -> go pj
           _ -> False

isInsideFirstLegendIdx :: ElementIndex -> Int -> Int -> Bool
isInsideFirstLegendIdx idx !j !fsi = findFirstLegend (fsi + 1)
  where
    !fsEnd = fromIntegral (indexPrimArray (eiSubEnd idx) fsi) :: Int
    findFirstLegend !k
      | k >= fsEnd = False
      | HTMLElement "legend" _ _ <- indexSmallArray (eiNodes idx) k
      , fromIntegral (indexPrimArray (eiParent idx) k) == (fsi :: Int) =
          let !legEnd = fromIntegral (indexPrimArray (eiSubEnd idx) k) :: Int
          in j >= k && j < legEnd
      | HTMLElement {} <- indexSmallArray (eiNodes idx) k
      , fromIntegral (indexPrimArray (eiParent idx) k) == (fsi :: Int) =
          findFirstLegend (fromIntegral (indexPrimArray (eiSubEnd idx) k))
      | otherwise = findFirstLegend (k + 1)

isActuallyDisabledDOM :: Node -> Text -> SmallArray HTMLAttribute -> Bool
isActuallyDisabledDOM _ tag attrs
  | tag == "fieldset" = Sel.attrExists "disabled" attrs
  | Sel.attrExists "disabled" attrs = True
isActuallyDisabledDOM (Node _ crumbs) _ _ = checkFieldsetCrumbs crumbs False

checkFieldsetCrumbs :: [Crumb] -> Bool -> Bool
checkFieldsetCrumbs [] _ = False
checkFieldsetCrumbs (Crumb ptag pattrs pchildren _ : rest) wasInLegend
  | ptag == "fieldset" && Sel.attrExists "disabled" pattrs =
      if wasInLegend && isFirstLegendChild pchildren
      then checkFieldsetCrumbs rest False
      else True
  | ptag == "legend" = checkFieldsetCrumbs rest True
  | otherwise = checkFieldsetCrumbs rest False

isFirstLegendChild :: SmallArray HTMLNode -> Bool
isFirstLegendChild children = go 0
  where
    !n = sizeofSmallArray children
    go !i
      | i >= n = False
      | HTMLElement "legend" _ _ <- indexSmallArray children i = True
      | HTMLElement {} <- indexSmallArray children i = False
      | otherwise = go (i + 1)

hasNonEmptyValue :: SmallArray HTMLAttribute -> Bool
hasNonEmptyValue attrs = case Sel.findAttr "value" attrs of
  Just v -> not (T.null v)
  Nothing -> False

matchDefaultIdx :: Text -> SmallArray HTMLAttribute -> Bool
matchDefaultIdx tag attrs
  | tag == "button" || (tag == "input" && isSubmitType attrs) =
      True
  | tag == "option" = Sel.attrExists "selected" attrs
  | tag == "input" && isCheckable attrs = Sel.attrExists "checked" attrs
  | otherwise = False

isSubmitType :: SmallArray HTMLAttribute -> Bool
isSubmitType attrs = case Sel.findAttr "type" attrs of
  Nothing -> True
  Just v -> T.toLower v == "submit"

isCheckable :: SmallArray HTMLAttribute -> Bool
isCheckable attrs = case Sel.findAttr "type" attrs of
  Nothing -> False
  Just v -> let !t = T.toLower v in t == "checkbox" || t == "radio"

matchIndeterminate :: SmallArray HTMLAttribute -> Bool
matchIndeterminate attrs = case Sel.findAttr "type" attrs of
  Nothing -> False
  Just v ->
    let !t = T.toLower v
    in (t == "checkbox" || t == "radio") && not (Sel.attrExists "checked" attrs)

allBlankChildren :: SmallArray HTMLNode -> Int -> Int -> Bool
allBlankChildren !children !i !n
  | i >= n = True
  | HTMLComment {} <- indexSmallArray children i = allBlankChildren children (i + 1) n
  | HTMLText t <- indexSmallArray children i = T.all isWSChar t && allBlankChildren children (i + 1) n
  | otherwise = False

isWSChar :: Char -> Bool
isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'
{-# INLINE isWSChar #-}

isBlankElement :: Node -> Bool
isBlankElement node = all isBlankChild (childNodes node)
  where
    isBlankChild (Node (HTMLComment _) _) = True
    isBlankChild (Node (HTMLText t) _) = T.all isWSChar t
    isBlankChild _ = False

matchDirIdx :: ElementIndex -> Int -> Text -> Bool
matchDirIdx idx !i dir' = go i
  where
    go !j
      | j < 0 = dir' == "ltr"
      | HTMLElement _ attrs _ <- indexSmallArray (eiNodes idx) j
      , Just val <- Sel.findAttr "dir" attrs
      , isValidDir val = T.toLower val == dir'
      | otherwise = go (fromIntegral (indexPrimArray (eiParent idx) j))

matchDir :: Text -> Node -> Bool
matchDir dir' node = case getInheritedDir node of
  Nothing -> dir' == "ltr"
  Just val -> T.toLower val == dir'

isValidDir :: Text -> Bool
isValidDir v = let !lv = T.toLower v in lv == "ltr" || lv == "rtl" || lv == "auto"

getInheritedDir :: Node -> Maybe Text
getInheritedDir (Node raw crumbs) = case raw of
  HTMLElement _ attrs _ ->
    case Sel.findAttr "dir" attrs of
      Just v | isValidDir v -> Just v
      _ -> getInheritedDir' crumbs
  _ -> getInheritedDir' crumbs
  where
    getInheritedDir' [] = Nothing
    getInheritedDir' (Crumb _ attrs _ _ : rest) =
      case Sel.findAttr "dir" attrs of
        Just v | isValidDir v -> Just v
        _ -> getInheritedDir' rest

matchHasRelIdx :: ElementIndex -> Int -> Sel.Combinator -> Sel.ComplexSelector -> Bool
matchHasRelIdx idx i comb (Sel.ComplexSelector compound chain) =
  hasCandidateIdx idx i comb (\j ->
    matchesCompoundIdx idx j compound && matchHasChainIdx idx j chain)

matchHasChainIdx :: ElementIndex -> Int -> [(Sel.Combinator, Sel.CompoundSelector)] -> Bool
matchHasChainIdx _ _ [] = True
matchHasChainIdx idx i ((comb, compound) : rest) =
  hasCandidateIdx idx i comb (\j ->
    matchesCompoundIdx idx j compound && matchHasChainIdx idx j rest)

hasCandidateIdx :: ElementIndex -> Int -> Sel.Combinator -> (Int -> Bool) -> Bool
hasCandidateIdx idx i comb f = case comb of
  Sel.Descendant -> anyDescendantIdx idx i f
  Sel.Child -> anyChildIdx idx i f
  Sel.AdjacentSibling ->
    let !nxt = fromIntegral (indexPrimArray (eiNextElem idx) i) :: Int
    in nxt >= 0 && f nxt
  Sel.GeneralSibling -> anyNextSiblingIdx idx i f

anyChildIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyChildIdx idx i f =
  let !subEnd = fromIntegral (indexPrimArray (eiSubEnd idx) i) :: Int
  in go (i + 1) subEnd
  where
    go !j !end
      | j >= end = False
      | HTMLElement {} <- indexSmallArray (eiNodes idx) j
      , fromIntegral (indexPrimArray (eiParent idx) j) == (i :: Int) =
          f j || go (fromIntegral (indexPrimArray (eiSubEnd idx) j)) end
      | otherwise = go (j + 1) end

anyNextSiblingIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyNextSiblingIdx idx i f = go (fromIntegral (indexPrimArray (eiNextElem idx) i) :: Int)
  where
    go !j
      | j < 0 = False
      | f j = True
      | otherwise = go (fromIntegral (indexPrimArray (eiNextElem idx) j))

matchHasRel :: Node -> Sel.Combinator -> Sel.ComplexSelector -> Bool
matchHasRel node comb (Sel.ComplexSelector compound chain) =
  hasCandidateDOM node comb (\n ->
    matchesCompound compound n && matchHasChainDOM n chain)

matchHasChainDOM :: Node -> [(Sel.Combinator, Sel.CompoundSelector)] -> Bool
matchHasChainDOM _ [] = True
matchHasChainDOM node ((comb, compound) : rest) =
  hasCandidateDOM node comb (\n ->
    matchesCompound compound n && matchHasChainDOM n rest)

hasCandidateDOM :: Node -> Sel.Combinator -> (Node -> Bool) -> Bool
hasCandidateDOM node comb f = case comb of
  Sel.Descendant -> anyDescendant f node
  Sel.Child -> any (\c -> isElement c && f c) (childNodes node)
  Sel.AdjacentSibling -> case nextElementSibling node of
    Nothing -> False
    Just s -> f s
  Sel.GeneralSibling -> anyNextElementSibling f node

nextElementSibling :: Node -> Maybe Node
nextElementSibling n = case nextSibling n of
  Nothing -> Nothing
  Just s | isElement s -> Just s
         | otherwise -> nextElementSibling s

anyNextElementSibling :: (Node -> Bool) -> Node -> Bool
anyNextElementSibling f n = case nextSibling n of
  Nothing -> False
  Just s -> (isElement s && f s) || anyNextElementSibling f s

nthChildOfIdx :: ElementIndex -> Int -> Sel.Selector -> Bool -> Int
nthChildOfIdx idx i (Sel.Selector sels) forward =
  let !sibArr = if forward then eiPrevElem idx else eiNextElem idx
      go !n !j
        | j < 0 = n
        | HTMLElement {} <- indexSmallArray (eiNodes idx) j
        , any (matchesComplexIdx idx j) sels =
            go (n + 1) (fromIntegral (indexPrimArray sibArr j))
        | otherwise =
            go n (fromIntegral (indexPrimArray sibArr j))
  in go 1 (fromIntegral (indexPrimArray sibArr i) :: Int)

nthChildOfNode :: Node -> Sel.Selector -> Bool -> Int
nthChildOfNode n (Sel.Selector sels) forward = go 1 (step n)
  where
    step = if forward then prevSibling else nextSibling
    go !i Nothing = i
    go !i (Just s)
      | isElement s && any (\cs -> matchesComplex cs s) sels = go (i + 1) (step s)
      | otherwise = go i (step s)

matchLang :: [Text] -> Node -> Bool
matchLang langTags node = case getInheritedLang node of
  Nothing -> False
  Just val ->
    let !lval = T.toLower val
    in any (\lt -> lval == lt || T.isPrefixOf (lt <> "-") lval) langTags

getInheritedLang :: Node -> Maybe Text
getInheritedLang (Node raw crumbs) = case raw of
  HTMLElement _ attrs _ ->
    case Sel.findAttr "lang" attrs of
      Just v -> Just v
      Nothing -> getInheritedLang' crumbs
  _ -> getInheritedLang' crumbs
  where
    getInheritedLang' [] = Nothing
    getInheritedLang' (Crumb _ attrs _ _ : rest) =
      case Sel.findAttr "lang" attrs of
        Just v -> Just v
        Nothing -> getInheritedLang' rest

anyDescendant :: (Node -> Bool) -> Node -> Bool
anyDescendant f node = any (\c -> f c || anyDescendant f c) (childNodes node)

anyAncestor :: (Node -> Bool) -> Node -> Bool
anyAncestor f n = case parentNode n of
  Nothing -> False
  Just p  -> f p || anyAncestor f p

anyPrevElementSibling :: (Node -> Bool) -> Node -> Bool
anyPrevElementSibling f n = case prevSibling n of
  Nothing -> False
  Just s | isElement s -> f s || anyPrevElementSibling f s
         | otherwise -> anyPrevElementSibling f s

matchesSelector :: Sel.Selector -> Node -> Bool
matchesSelector (Sel.Selector [cs]) node = matchesComplex cs node
matchesSelector (Sel.Selector complexSels) node =
  any (\cs -> matchesComplex cs node) complexSels
{-# INLINE matchesSelector #-}

matchCompoundFlat :: Sel.CompoundSelector -> Text -> SmallArray HTMLAttribute -> Bool
matchCompoundFlat (Sel.CompoundSelector mtype subs) tag attrs =
  Sel.matchType mtype tag && allSubsFlat tag attrs subs
{-# INLINE matchCompoundFlat #-}

allSubsFlat :: Text -> SmallArray HTMLAttribute -> [Sel.SubSel] -> Bool
allSubsFlat !_ !_ [] = True
allSubsFlat tag attrs (s : rest) = matchSubFlat tag attrs s && allSubsFlat tag attrs rest
{-# INLINE allSubsFlat #-}

matchSubFlat :: Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Bool
matchSubFlat tag attrs sub =
  case matchSubCommon tag attrs sub of
    Just b -> b
    Nothing -> case sub of
      Sel.SelNot (Sel.Selector [cs]) -> not (matchComplexFlat tag attrs cs)
      Sel.SelNot (Sel.Selector sels) -> not (any (matchComplexFlat tag attrs) sels)
      Sel.SelIs (Sel.Selector [cs]) -> matchComplexFlat tag attrs cs
      Sel.SelIs (Sel.Selector sels) -> any (matchComplexFlat tag attrs) sels
      Sel.SelWhere (Sel.Selector [cs]) -> matchComplexFlat tag attrs cs
      Sel.SelWhere (Sel.Selector sels) -> any (matchComplexFlat tag attrs) sels
      s -> Sel.matchSub attrs s
{-# INLINE matchSubFlat #-}

matchComplexFlat :: Text -> SmallArray HTMLAttribute -> Sel.ComplexSelector -> Bool
matchComplexFlat tag attrs (Sel.ComplexSelector compound []) =
  matchCompoundFlat compound tag attrs
matchComplexFlat _ _ _ = False
{-# INLINE matchComplexFlat #-}
