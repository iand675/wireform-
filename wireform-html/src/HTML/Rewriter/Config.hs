{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HTML.Rewriter.Config where

import Control.Monad.ST (runST)
import Data.List (foldl')
import Data.Primitive.ByteArray (
  ByteArray,
  fillByteArray,
  indexByteArray,
  newByteArray,
  unsafeFreezeByteArray,
  writeByteArray,
  )
import Data.Primitive.SmallArray (
  SmallArray,
  copySmallArray,
  emptySmallArray,
  indexSmallArray,
  newSmallArray,
  runSmallArray,
  sizeofSmallArray,
  smallArrayFromList,
 )
import Data.Text (Text)
import GHC.Exts (Addr#)
import HTML.Rewriter.Mutations (
  CommentRef,
  DoctypeRef,
  ElementRef,
  EndTagRef,
  TextChunkRef,
 )
import HTML.Rewriter.StackFrame (StackFrame (..))
import HTML.TagId (TagId (..), tagIdFromText)
import HTML.Selector (
  Combinator (..),
  ComplexSelector (..),
  CompoundSelector (..),
  Selector (..),
  SelectorError (..),
  SubSel (..),
  TypeSel (..),
  hasClassWord,
  isClassOnlyCompound,
  isRewriterCompatible,
  matchCompound,
  matchType,
 )
import HTML.Value (HTMLAttribute (..))

data HandlerEntry
  = HElement !Selector !(ElementRef -> IO ())
  | HText !Selector !(TextChunkRef -> IO ())
  | HEndTag !Selector !(EndTagRef -> IO ())


data GlobalHandlers = GlobalHandlers
  { ghComment :: ![CommentRef -> IO ()]
  , ghDoctype :: ![DoctypeRef -> IO ()]
  }


data RewriterConfig = RewriterConfig
  { rcHandlers :: ![HandlerEntry]
  , rcGlobal :: !GlobalHandlers
  }


newtype RewriterBuilder a = RewriterBuilder
  {unRB :: RewriterConfig -> (a, RewriterConfig)}


instance Functor RewriterBuilder where
  fmap f (RewriterBuilder g) = RewriterBuilder $ \c ->
    let (a, c') = g c in (f a, c')


instance Applicative RewriterBuilder where
  pure a = RewriterBuilder (a,)
  RewriterBuilder f <*> RewriterBuilder g = RewriterBuilder $ \c ->
    let (fab, c1) = f c
        (a, c2) = g c1
    in (fab a, c2)


instance Monad RewriterBuilder where
  RewriterBuilder m >>= k = RewriterBuilder $ \c ->
    let (a, c1) = m c
    in unRB (k a) c1


onElement :: Selector -> (ElementRef -> IO ()) -> RewriterBuilder ()
onElement sel handler = RewriterBuilder $ \c ->
  ((), c {rcHandlers = rcHandlers c ++ [HElement sel handler]})


onText :: Selector -> (TextChunkRef -> IO ()) -> RewriterBuilder ()
onText sel handler = RewriterBuilder $ \c ->
  ((), c {rcHandlers = rcHandlers c ++ [HText sel handler]})


onComment :: (CommentRef -> IO ()) -> RewriterBuilder ()
onComment handler = RewriterBuilder $ \c ->
  let g = rcGlobal c
  in ((), c {rcGlobal = g {ghComment = ghComment g ++ [handler]}})


onDoctype :: (DoctypeRef -> IO ()) -> RewriterBuilder ()
onDoctype handler = RewriterBuilder $ \c ->
  let g = rcGlobal c
  in ((), c {rcGlobal = g {ghDoctype = ghDoctype g ++ [handler]}})


onEndTag :: Selector -> (EndTagRef -> IO ()) -> RewriterBuilder ()
onEndTag sel handler = RewriterBuilder $ \c ->
  ((), c {rcHandlers = rcHandlers c ++ [HEndTag sel handler]})


-- ---------------------------------------------------------------------------
-- Rewriter (compiled configuration)
-- ---------------------------------------------------------------------------

-- | Pre-decomposed selector: subject + ancestor context chain.
-- Computed once at build time to avoid per-match allocation.
data DecomposedSel = DecomposedSel
  { dsSubject :: !CompoundSelector
  , dsContext :: ![(Combinator, CompoundSelector)]
  }


data CompiledHandler
  = CHElement !(SmallArray DecomposedSel) !(ElementRef -> IO ())
  | CHText !(SmallArray DecomposedSel) !(TextChunkRef -> IO ())
  | CHEndTag !(SmallArray DecomposedSel) !(EndTagRef -> IO ())


data Rewriter = Rewriter
  { rwHandlers :: !(SmallArray CompiledHandler)
  , rwComment :: !(SmallArray (CommentRef -> IO ()))
  , rwDoctype :: !(SmallArray (DoctypeRef -> IO ()))
  , rwHasText :: !Bool
  , rwHasElement :: !Bool
  , rwHasEndTag :: !Bool
  , rwTagFilter :: !(TagId -> Bool)
  , rwContextNeedsAttrs :: !Bool
  , rwNeedsStack :: !Bool
  , rwNeedsContextStack :: !Bool
  , rwTextSelectors :: !(SmallArray DecomposedSel)
  , rwClassOnly :: !Bool
  }


isNoopRewriter :: Rewriter -> Bool
isNoopRewriter rw = sizeofSmallArray (rwHandlers rw) == 0
                 && sizeofSmallArray (rwComment rw) == 0
                 && sizeofSmallArray (rwDoctype rw) == 0
{-# INLINE isNoopRewriter #-}


hasTextHandlers :: Rewriter -> Bool
hasTextHandlers = rwHasText
{-# INLINE hasTextHandlers #-}


hasElementHandlers :: Rewriter -> Bool
hasElementHandlers = rwHasElement
{-# INLINE hasElementHandlers #-}
{- | Check if a complex selector matches at the current position.
The parser stores selectors left-to-right: "div > span" becomes
@ComplexSelector div [(Child, span)]@. CSS matching is right-to-left:
the subject (rightmost compound) must match the current element, then
ancestors are checked working backwards through the chain.
-}
matchAtPosition :: DecomposedSel -> [StackFrame] -> Text -> SmallArray HTMLAttribute -> Bool
matchAtPosition ds stack tag attrs =
  matchCompound (dsSubject ds) tag attrs && matchContext (dsContext ds) stack
{-# INLINE matchAtPosition #-}


{- | Extract the subject (rightmost compound) and the ancestor chain.
"div > span" → DecomposedSel span [(Child, div)]
"a b > c" → DecomposedSel c [(Child, b), (Descendant, a)]
Computed once at build time.
-}
decomposeComplex :: ComplexSelector -> DecomposedSel
decomposeComplex (ComplexSelector hd []) = DecomposedSel hd []
decomposeComplex (ComplexSelector hd tl) = go hd tl []
  where
    go !prev ((comb, comp) : rest) acc = go comp rest ((comb, prev) : acc)
    go !subj [] acc = DecomposedSel subj acc


-- | Walk the ancestor chain against the element stack.
matchContext :: [(Combinator, CompoundSelector)] -> [StackFrame] -> Bool
matchContext [] _ = True
matchContext ((Child, comp) : rest) frames =
  case frames of
    (StackFrame t a _ _ : frames') ->
      matchCompound comp t a && matchContext rest frames'
    [] -> False
matchContext ((Descendant, comp) : rest) frames =
  scanAncestors frames
  where
    scanAncestors [] = False
    scanAncestors (StackFrame t a _ _ : frames') =
      (matchCompound comp t a && matchContext rest frames') || scanAncestors frames'
matchContext ((AdjacentSibling, _) : _) _ = False
matchContext ((GeneralSibling, _) : _) _ = False
-- | Single-pass: run all matching element handlers, return True if any matched.
runElementHandlers :: Rewriter -> [StackFrame] -> Text -> SmallArray HTMLAttribute -> ElementRef -> IO Bool
runElementHandlers rw stack tag attrs er = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHElement decomposed handler ->
            if matchAnyDecomposed decomposed stack tag attrs
              then handler er >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runElementHandlers #-}


-- | Single-pass: run all matching end-tag handlers, return True if any matched.
runEndTagHandlers :: Rewriter -> [StackFrame] -> Text -> EndTagRef -> IO Bool
runEndTagHandlers rw stack name etr = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHEndTag decomposed handler ->
            if matchAnyEndTag decomposed stack name
              then handler etr >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runEndTagHandlers #-}


-- | O(1) check: does any text handler have an ancestor match at the current stack?
-- Uses the cached sfTextMatch flag computed when each frame was pushed.
anyTextAncestorMatches :: [StackFrame] -> Bool
anyTextAncestorMatches [] = False
anyTextAncestorMatches (sf : _) = sfTextMatch sf
{-# INLINE anyTextAncestorMatches #-}


-- | Single-pass: run all matching text handlers, return True if any matched.
-- Walks the ancestor stack to determine which text handlers apply.
runTextHandlers :: Rewriter -> [StackFrame] -> TextChunkRef -> IO Bool
runTextHandlers rw stack tr = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHText decomposed handler ->
            if anyAncestorMatches decomposed stack
              then handler tr >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)

    anyAncestorMatches _ [] = False
    anyAncestorMatches !sels (StackFrame tag attrs _ _ : frames) =
      matchAnyDecomposed sels frames tag attrs || anyAncestorMatches sels frames
{-# INLINE runTextHandlers #-}

-- | Fire all text handlers unconditionally. Used when
-- rwNeedsContextStack=False and the depth-indexed text mask already
-- confirmed that a matching ancestor exists.
runTextHandlersAll :: Rewriter -> TextChunkRef -> IO Bool
runTextHandlersAll rw tr = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHText _ handler -> handler tr >> go True (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runTextHandlersAll #-}


matchAnyDecomposed :: SmallArray DecomposedSel -> [StackFrame] -> Text -> SmallArray HTMLAttribute -> Bool
matchAnyDecomposed !arr stack tag attrs = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | matchAtPosition (indexSmallArray arr i) stack tag attrs = True
      | otherwise = go (i + 1)
{-# INLINE matchAnyDecomposed #-}


-- | Run element handlers using class-only byte matching (no SmallArray attrs).
runElementHandlersClass :: Rewriter -> Text -> Addr# -> Int -> Int -> ElementRef -> IO Bool
runElementHandlersClass rw tag addr# classOff classLen er = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHElement decomposed handler ->
            if matchAnyDecomposedClass decomposed tag addr# classOff classLen
              then handler er >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runElementHandlersClass #-}




-- | Class-only decomposed selector matching at byte level.
matchAnyDecomposedClass :: SmallArray DecomposedSel -> Text -> Addr# -> Int -> Int -> Bool
matchAnyDecomposedClass !arr tag addr# classOff classLen = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | matchDecomposedClass (indexSmallArray arr i) = True
      | otherwise = go (i + 1)
    matchDecomposedClass (DecomposedSel (CompoundSelector mtype subs) _ctx) =
      matchType mtype tag && allSubsClass subs
    allSubsClass [] = True
    allSubsClass (SelClass c : rest) = hasClassWord c addr# classOff classLen && allSubsClass rest
    allSubsClass _ = False
{-# INLINE matchAnyDecomposedClass #-}


matchAnyEndTag :: SmallArray DecomposedSel -> [StackFrame] -> Text -> Bool
matchAnyEndTag !arr stack name = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | matchEndPositionD (indexSmallArray arr i) stack name = True
      | otherwise = go (i + 1)
{-# INLINE matchAnyEndTag #-}


matchEndPositionD :: DecomposedSel -> [StackFrame] -> Text -> Bool
matchEndPositionD ds stack name =
  let currentAttrs = case stack of
        (StackFrame _ a _ _ : _) -> a
        [] -> mempty
  in matchCompoundForEnd (dsSubject ds) name currentAttrs && matchContext (dsContext ds) stack


matchCompoundForEnd :: CompoundSelector -> Text -> SmallArray HTMLAttribute -> Bool
matchCompoundForEnd (CompoundSelector mtype _subs) name _attrs =
  case mtype of
    Nothing -> True
    Just TypeUniversal -> True
    Just (TypeTag t) -> t == name

buildRewriter :: RewriterBuilder () -> Either SelectorError Rewriter
buildRewriter builder =
  let emptyConfig = RewriterConfig [] (GlobalHandlers [] [])
      ((), cfg) = unRB builder emptyConfig
  in compileConfig cfg


compileConfig :: RewriterConfig -> Either SelectorError Rewriter
compileConfig cfg = do
  handlersList <- mapM compileHandler (rcHandlers cfg)
  let !handlers = smallArrayFromList handlersList
      !hasUniversal = anyHasUniversal handlersList
      !tagTids = collectTagIds handlersList
      !tagFilter
        | hasUniversal = const True
        | null tagTids = const False
        | otherwise =
            let !ba = buildTagFilterBA False tagTids
            in \tid -> indexByteArray ba (fromEnum tid) /= (0 :: Int)
      !ctxAttrs = contextNeedsAttrs handlersList
      !hasText = any isTextHandler handlersList
      !hasEndTag = any isEndTagHandler handlersList
      !hasContext = any handlerHasContext handlersList
      !needsStack = hasText || hasContext || hasEndTag
      !textSels = concatTextSelectors handlersList
  Right
    Rewriter
      { rwHandlers = handlers
      , rwComment = smallArrayFromList (ghComment (rcGlobal cfg))
      , rwDoctype = smallArrayFromList (ghDoctype (rcGlobal cfg))
      , rwHasText = hasText
      , rwHasElement = any isElementHandler handlersList
      , rwHasEndTag = hasEndTag
      , rwTagFilter = tagFilter
      , rwContextNeedsAttrs = ctxAttrs
      , rwNeedsStack = needsStack
      , rwNeedsContextStack = hasContext || hasEndTag
      , rwTextSelectors = textSels
      , rwClassOnly = allClassOnly handlersList textSels && not ctxAttrs
      }
  where
    isTextHandler (CHText _ _) = True
    isTextHandler _ = False
    isElementHandler (CHElement _ _) = True
    isElementHandler _ = False
    isEndTagHandler (CHEndTag _ _) = True
    isEndTagHandler _ = False
    compileHandler (HElement sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHElement (smallArrayFromList (map decomposeComplex cs)) handler)
    compileHandler (HText sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHText (smallArrayFromList (map decomposeComplex cs)) handler)
    compileHandler (HEndTag sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHEndTag (smallArrayFromList (map decomposeComplex cs)) handler)

    allClassOnly :: [CompiledHandler] -> SmallArray DecomposedSel -> Bool
    allClassOnly hs textSelsArr =
      all handlerClassOnly hs && allDecomposedClassOnly textSelsArr
      where
        handlerClassOnly (CHElement ds _) = allDecomposedClassOnly ds
        handlerClassOnly (CHText ds _) = allDecomposedClassOnly ds
        handlerClassOnly (CHEndTag _ _) = False
        allDecomposedClassOnly :: SmallArray DecomposedSel -> Bool
        allDecomposedClassOnly ds = go2 0
          where
            !n = sizeofSmallArray ds
            go2 !i
              | i >= n = True
              | DecomposedSel subj ctx <- indexSmallArray ds i =
                  isClassOnlyCompound subj && null ctx && go2 (i + 1)

    anyHasUniversal :: [CompiledHandler] -> Bool
    anyHasUniversal = any handlerUniversal
      where
        handlerUniversal (CHElement ds _) = any decomposedUniversal ds
        handlerUniversal (CHText ds _) = any decomposedUniversal ds
        handlerUniversal (CHEndTag ds _) = any decomposedUniversal ds
        decomposedUniversal (DecomposedSel subj _) = compoundUniversal subj
        compoundUniversal (CompoundSelector Nothing _) = True
        compoundUniversal (CompoundSelector (Just TypeUniversal) _) = True
        compoundUniversal _ = False

    collectTagIds :: [CompiledHandler] -> [TagId]
    collectTagIds = concatMap handlerTagIds
      where
        handlerTagIds (CHElement ds _) = concatMap decomposedTids ds
        handlerTagIds (CHText ds _) = concatMap decomposedTids ds
        handlerTagIds (CHEndTag ds _) = concatMap decomposedTids ds
        decomposedTids (DecomposedSel subj ctx) =
          compoundTids subj ++ concatMap (compoundTids . snd) ctx
        compoundTids (CompoundSelector (Just (TypeTag t)) _) =
          [tagIdFromText t]
        compoundTids _ = []

    contextNeedsAttrs :: [CompiledHandler] -> Bool
    contextNeedsAttrs = any handlerCtxAttrs
      where
        handlerCtxAttrs (CHElement ds _) = any dsCtxAttrs ds
        handlerCtxAttrs (CHText ds _) = any dsCtxAttrs ds
        handlerCtxAttrs (CHEndTag ds _) = any dsCtxAttrs ds
        dsCtxAttrs (DecomposedSel _ ctx) = any (compoundHasSubs . snd) ctx
        compoundHasSubs (CompoundSelector _ subs) = not (null subs)

    handlerHasContext :: CompiledHandler -> Bool
    handlerHasContext (CHElement ds _) = any dsHasCtx ds
    handlerHasContext (CHText ds _) = any dsHasCtx ds
    handlerHasContext (CHEndTag ds _) = any dsHasCtx ds

    dsHasCtx :: DecomposedSel -> Bool
    dsHasCtx (DecomposedSel _ ctx) = not (null ctx)

    concatTextSelectors :: [CompiledHandler] -> SmallArray DecomposedSel
    concatTextSelectors hs =
      let !totalLen = foldl' (\n h -> case h of CHText ds _ -> n + sizeofSmallArray ds; _ -> n) 0 hs
      in if totalLen == 0
        then emptySmallArray
        else runSmallArray $ do
          ma <- newSmallArray totalLen (error "unreachable")
          let fill !_ [] = pure ()
              fill !off (CHText ds _ : rest) = do
                let !dsLen = sizeofSmallArray ds
                copySmallArray ma off ds 0 dsLen
                fill (off + dsLen) rest
              fill !off (_ : rest) = fill off rest
          fill 0 hs
          pure ma

    validateRewriter sel =
      if isRewriterCompatible sel
        then Right ()
        else Left (UnsupportedSelector "selector uses DOM-only features (pseudo-classes or sibling combinators)")


buildTagFilterBA :: Bool -> [TagId] -> ByteArray
buildTagFilterBA hasUniversal tagTids = runST $ do
  let !maxIdx = fromEnum (maxBound :: TagId)
      !slots = maxIdx + 1
  mba <- newByteArray (slots * 8)
  if hasUniversal
    then do
      let fillAll !i
            | i >= slots = pure ()
            | otherwise = writeByteArray mba i (1 :: Int) >> fillAll (i + 1)
      fillAll 0
    else do
      fillByteArray mba 0 (slots * 8) 0
      mapM_ (\tid -> writeByteArray mba (fromEnum tid) (1 :: Int)) tagTids
  unsafeFreezeByteArray mba
{-# NOINLINE buildTagFilterBA #-}
