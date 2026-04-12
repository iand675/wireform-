{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | DOM-style API for parsed HTML documents.
--
-- Provides a zipper-based 'Node' type that supports parent\/sibling
-- navigation over the immutable 'HTMLNode' tree, an incremental parser,
-- CSS selector queries, and HTML serialization.
--
-- The underlying 'HTMLNode' tree is never copied or mutated — 'Node'
-- values carry a list of 'Crumb's (breadcrumbs) that record the path
-- from the root, enabling O(1) parent\/sibling access.
module HTML.DOM
  ( -- * Types
    Document
  , Node
  , NodeType(..)
    -- * Parsing
  , parseDocument
    -- * Incremental parsing
  , Parser
  , newParser
  , feedParser
  , finishParser
    -- * Streaming tree construction
  , TreeEvent(..)
  , Step(..)
  , streamHTML
  , streamHTMLEvents
  , streamHTMLRaw
  , streamHTMLEventsRaw
  , StreamParser
  , newStreamParser
  , feedChunk
  , feedChunkEvents
  , finishStream
  , finishStreamEvents
    -- * Document access
  , documentDoctype
  , documentElement
  , documentHTML
    -- * Tree traversal
  , childNodes
  , firstChild
  , lastChild
  , nextSibling
  , prevSibling
  , parentNode
    -- * Node inspection
  , nodeName
  , nodeType
  , rawNode
  , textContent
  , tagName
  , getAttribute
  , getAttributes
  , hasAttribute
  , classList
    -- * Serialization
  , innerHTML
  , outerHTML
  , serialize
  , serializeDocument
    -- * Construction
  , rootNode
    -- * CSS selectors
  , querySelector
  , querySelectorAll
  , querySelectorAllSel
  , querySelectorAllDoc
  , matches
  , closest
  , findFirstComplex
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import Data.Primitive.PrimArray
  ( PrimArray
  , newPrimArray, writePrimArray, unsafeFreezePrimArray, indexPrimArray, sizeofPrimArray
  , shrinkMutablePrimArray
  )
import Data.Primitive.SmallArray (SmallArray, sizeofSmallArray, indexSmallArray)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import HTML.Encode (buildNode, buildDocument)
import HTML.DOM.Index (ElementIndex(..), buildElementIndex)
import HTML.DOM.Selector
  ( decomposeComplex
  , matchCompoundFlat
  , matchContextIdx
  , matchesComplex
  , matchesCompoundIdx
  , matchesSelector
  , matchesSelectorIdx
  )
import HTML.DOM.Zipper
  ( Crumb(..), Node(..), NodeType(..), rootNode
  , childNodes, firstChild, lastChild, nextSibling, prevSibling, parentNode
  , nodeName, nodeType, rawNode, textContent, tagName
  , getAttribute, getAttributes, hasAttribute, classList
  )
import HTML.Parse
  ( parseHTML, Token(TEOF)
  , TreeBuilder, newTreeBuilder, newTreeBuilderWith
  , processToken, tokenizeBSIO
  , freeTreeBuilder, drainTreeBuilderStack
  , tbGetEvents, tbResetEvents, tokenizeRawEventsIO
  )
import qualified HTML.Parse as P (buildDocument)
import qualified HTML.Selector as Sel
import HTML.Value
  ( HTMLDocument(..), HTMLNode(..), Doctype(..), TreeEvent(..)
  )

import Control.Monad.ST (runST)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A parsed HTML document with a precomputed element index for fast
-- CSS selector queries.
data Document = Document !HTMLDocument !ElementIndex

instance Show Document where
  show (Document doc _) = "Document " ++ show doc

instance Eq Document where
  Document a _ == Document b _ = a == b

-- | Extract the raw 'HTMLDocument' from a 'Document'.
documentHTML :: Document -> HTMLDocument
documentHTML (Document doc _) = doc

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a complete HTML document from a 'ByteString'.
-- Builds a pre-order element index for fast CSS selector queries.
parseDocument :: ByteString -> Document
parseDocument bs =
  let !doc = parseHTML bs
      !idx = buildElementIndex (htmlRoot doc)
  in Document doc idx
  where
    htmlRoot (HTMLDocument _ root) = root

-- ---------------------------------------------------------------------------
-- Incremental parsing
-- ---------------------------------------------------------------------------

-- | Opaque incremental parser state.
--
-- Chunks fed via 'feedParser' are tokenized and fed to the HTML5
-- tree builder immediately (no buffering). The tree builder state
-- persists across chunks. Incomplete tag fragments at chunk
-- boundaries are carried over to the next 'feedParser' call.
data Parser = Parser
  { _parserTB :: !TreeBuilder
  , _parserLeftover :: !(IORef ByteString)
  }

-- | Create a fresh incremental parser.
newParser :: IO Parser
newParser = do
  tb <- newTreeBuilder Nothing
  lo <- newIORef BS.empty
  pure (Parser tb lo)

-- | Feed a chunk of HTML bytes to the parser.
-- The chunk is tokenized and processed by the tree builder immediately.
feedParser :: Parser -> ByteString -> IO ()
feedParser (Parser tb loRef) chunk = do
  prev <- readIORef loRef
  let !combined = if BS.null prev then chunk else prev <> chunk
      !splitPt = findSafeBreak combined
      !toProcess = BS.take splitPt combined
      !remainder = BS.drop splitPt combined
  writeIORef loRef remainder
  when (not (BS.null toProcess)) $
    tokenizeBSIO toProcess 0 (BS.length toProcess) 0 False tb

-- | Finalize the parser and extract the document.
--
-- Processes any remaining leftover bytes, sends EOF to the tree
-- builder, and constructs the immutable document with element index.
-- After calling this, the 'Parser' should not be reused.
finishParser :: Parser -> IO Document
finishParser (Parser tb loRef) = do
  lo <- readIORef loRef
  writeIORef loRef BS.empty
  when (not (BS.null lo)) $
    tokenizeBSIO lo 0 (BS.length lo) 0 False tb
  processToken tb TEOF
  doc <- P.buildDocument tb
  freeTreeBuilder tb
  let !idx = buildElementIndex (htmlRoot doc)
  pure $! Document doc idx
  where
    htmlRoot (HTMLDocument _ root) = root

-- ---------------------------------------------------------------------------
-- Streaming tree construction
-- ---------------------------------------------------------------------------

-- | A step in a pull-based event stream.
--
-- Produced by 'streamHTML', 'feedChunk', and 'finishStream'. Consume
-- by pattern-matching on 'Yield' and recursing into the tail.
data Step a = Yield !a (Step a) | Done

-- | Stream tree-construction events from a complete HTML document.
--
-- Events are buffered in a growable array during tree building and
-- returned as a 'Step' chain. All parsing completes before the first
-- event is yielded, so this is suitable for chunk-at-a-time processing
-- rather than event-by-event interleaving with parsing.
--
-- @
-- step0 <- streamHTML html
-- let go Done = pure ()
--     go (Yield evt resume) = do
--       processEvent evt
--       resume >>= go
-- go step0
-- @
streamHTML :: ByteString -> IO (Step TreeEvent)
streamHTML bs = do
  frozen <- streamHTMLEvents bs
  pure $! arrayToStep frozen 0 (sizeofSmallArray frozen)

-- | Like 'streamHTML' but returns a compact 'SmallArray' instead of
-- a 'Step' chain. Avoids per-event thunk allocation.
streamHTMLEvents :: ByteString -> IO (SmallArray TreeEvent)
streamHTMLEvents bs = do
  tb <- newTreeBuilderWith True False 2048 Nothing
  tokenizeBSIO bs 0 (BS.length bs) 0 False tb
  processToken tb TEOF
  drainTreeBuilderStack tb
  frozen <- tbGetEvents tb
  freeTreeBuilder tb
  pure frozen

-- | Raw streaming: bypasses the HTML5 tree construction algorithm. Tokens
-- are converted directly to 'TreeEvent's without implicit element insertion,
-- auto-closing, or adoption agency processing. Much faster but not
-- spec-correct for malformed HTML.
streamHTMLRaw :: ByteString -> IO (Step TreeEvent)
streamHTMLRaw bs = do
  frozen <- streamHTMLEventsRaw bs
  pure $! arrayToStep frozen 0 (sizeofSmallArray frozen)

-- | Like 'streamHTMLRaw' but returns a compact 'SmallArray'.
streamHTMLEventsRaw :: ByteString -> IO (SmallArray TreeEvent)
streamHTMLEventsRaw = tokenizeRawEventsIO


-- | Incremental streaming parser.
--
-- Feed chunks of HTML via 'feedChunk'. Each call returns a 'Step' stream
-- of events produced by that chunk. The tree builder state persists
-- across chunks. Call 'finishStream' to signal end-of-input.
data StreamParser = StreamParser
  { _spTreeBuilder :: !TreeBuilder
  , _spLeftover :: !(IORef ByteString)
  }

-- | Create a new incremental streaming parser.
newStreamParser :: IO StreamParser
newStreamParser = do
  tb <- newTreeBuilderWith True False 256 Nothing
  lo <- newIORef BS.empty
  pure (StreamParser tb lo)

-- | Feed a chunk of HTML. Returns a 'Step' stream of tree events
-- produced from complete tokens in the chunk. Incomplete tag fragments
-- at the end are carried over to the next 'feedChunk' call.
feedChunk :: StreamParser -> ByteString -> IO (Step TreeEvent)
feedChunk sp chunk = do
  frozen <- feedChunkEvents sp chunk
  pure $! arrayToStep frozen 0 (sizeofSmallArray frozen)

-- | Like 'feedChunk' but returns a 'SmallArray' directly.
feedChunkEvents :: StreamParser -> ByteString -> IO (SmallArray TreeEvent)
feedChunkEvents (StreamParser tb loRef) chunk = do
  prev <- readIORef loRef
  let !combined = if BS.null prev then chunk else prev <> chunk
      !splitPt = findSafeBreak combined
      !toProcess = BS.take splitPt combined
      !remainder = BS.drop splitPt combined
  writeIORef loRef remainder
  if BS.null toProcess
    then pure mempty
    else do
      tbResetEvents tb
      tokenizeBSIO toProcess 0 (BS.length toProcess) 0 False tb
      tbGetEvents tb

-- | Signal end-of-input. Returns final events (implicit close tags, etc.)
-- and frees the tree builder. The 'StreamParser' must not be reused.
finishStream :: StreamParser -> IO (Step TreeEvent)
finishStream sp = do
  frozen <- finishStreamEvents sp
  pure $! arrayToStep frozen 0 (sizeofSmallArray frozen)

-- | Like 'finishStream' but returns a 'SmallArray' directly.
finishStreamEvents :: StreamParser -> IO (SmallArray TreeEvent)
finishStreamEvents (StreamParser tb loRef) = do
  lo <- readIORef loRef
  writeIORef loRef BS.empty
  tbResetEvents tb
  when (not (BS.null lo)) $
    tokenizeBSIO lo 0 (BS.length lo) 0 False tb
  processToken tb TEOF
  drainTreeBuilderStack tb
  frozen <- tbGetEvents tb
  freeTreeBuilder tb
  pure frozen


arrayToStep :: SmallArray TreeEvent -> Int -> Int -> Step TreeEvent
arrayToStep !arr !i !len
  | i >= len = Done
  | otherwise = Yield (indexSmallArray arr i) (arrayToStep arr (i + 1) len)


findSafeBreak :: ByteString -> Int
findSafeBreak !bs = go (BS.length bs - 1)
  where
    go !i
      | i < 0 = BS.length bs
      | otherwise = case BS.index bs i of
          0x3C -> i
          0x3E -> BS.length bs
          _ -> go (i - 1)

when :: Bool -> IO () -> IO ()
when True act = act
when False _ = pure ()
{-# INLINE when #-}


-- ---------------------------------------------------------------------------
-- Document access
-- ---------------------------------------------------------------------------

-- | The document's doctype declaration, if present.
documentDoctype :: Document -> Maybe Doctype
documentDoctype (Document (HTMLDocument mdt _) _) = mdt

-- | The root element of the document as a navigable 'Node'.
documentElement :: Document -> Node
documentElement (Document (HTMLDocument _ root) _) = rootNode root

-- ---------------------------------------------------------------------------
-- Serialization
-- ---------------------------------------------------------------------------

-- | Serialize a node and its descendants to an HTML 'BB.Builder'.
serialize :: Node -> BB.Builder
serialize (Node raw _) = buildNode raw

-- | Serialize a full document (including doctype) to an HTML 'BB.Builder'.
serializeDocument :: Document -> BB.Builder
serializeDocument (Document doc _) = buildDocument doc

-- | Inner HTML of an element as 'Text' (serialized child nodes).
-- Returns 'T.empty' for non-element nodes.
innerHTML :: Node -> Text
innerHTML (Node (HTMLElement _ _ children) _) =
  TE.decodeUtf8 $! BL.toStrict $! BB.toLazyByteString $!
    foldMap buildNode children
innerHTML _ = T.empty

-- | Outer HTML of a node as 'Text' (the node itself, serialized).
outerHTML :: Node -> Text
outerHTML (Node raw _) =
  TE.decodeUtf8 $! BL.toStrict $! BB.toLazyByteString $! buildNode raw

-- ---------------------------------------------------------------------------
-- Crumb reconstruction
-- ---------------------------------------------------------------------------

-- Reconstruct crumb stack by walking parent pointers.
-- The list is built recursively so that the immediate parent crumb is
-- at the head (matching the zipper convention). The tail is lazy —
-- ancestor crumbs are only allocated when navigation actually reaches them.
rebuildCrumbs :: ElementIndex -> Int -> [Crumb]
rebuildCrumbs idx = go
  where
    go !i =
      let !pidx = fromIntegral (indexPrimArray (eiParent idx) i) :: Int
      in if pidx < 0
         then []
         else case indexSmallArray (eiNodes idx) pidx of
           HTMLElement ptag pattrs pchildren ->
             let !ri = fromIntegral (indexPrimArray (eiRawChild idx) i) :: Int
             in Crumb ptag pattrs pchildren ri : go pidx
           _ -> []
{-# INLINE rebuildCrumbs #-}

-- ---------------------------------------------------------------------------
-- CSS selectors (backed by HTML.Selector)
-- ---------------------------------------------------------------------------

-- | Find the first descendant matching a CSS selector (not self).
--
-- Per the DOM spec, 'querySelector' on an Element searches descendants
-- only. Returns 'Nothing' if the selector is invalid or no match.
querySelector :: Node -> Text -> Maybe Node
querySelector root selText =
  case Sel.parseSelector selText of
    Left _  -> Nothing
    Right sel -> case querySelectorAllSel sel root of
      []    -> Nothing
      (x:_) -> Just x

-- | Find all descendants matching a CSS selector string (not self).
-- Returns results in document order, deduplicated when multiple
-- comma-separated branches match the same node.
querySelectorAll :: Node -> Text -> [Node]
querySelectorAll root selText =
  case Sel.parseSelector selText of
    Left _  -> []
    Right sel -> querySelectorAllSel sel root


-- | Like 'querySelectorAll' but takes a pre-parsed 'Sel.Selector',
-- avoiding repeated parsing in hot loops. Searches descendants only
-- (not self), per the DOM spec for Element.querySelectorAll.
querySelectorAllSel :: Sel.Selector -> Node -> [Node]
querySelectorAllSel sel@(Sel.Selector complexSels) (Node raw crumbs) =
  case raw of
    HTMLElement tag attrs children ->
      forEachChild tag attrs children crumbs 0 (sizeofSmallArray children) []
    _ -> []
  where
    forEachChild !ptag !pattrs !pchildren !pcrumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray pchildren i
              !nextRest = forEachChild ptag pattrs pchildren pcrumbs (i + 1) n rest
          in case child of
               HTMLElement {} ->
                 let !crumbs' = Crumb ptag pattrs pchildren i : pcrumbs
                 in dispatchChild child crumbs' nextRest
               _ -> nextRest
    dispatchChild childRaw childCrumbs rest = case complexSels of
      [cs] -> dispatchSingle cs childRaw childCrumbs rest
      _ | all isFlatSingle complexSels ->
            collectFlatMulti (map extractCompound complexSels) childRaw childCrumbs rest
        | otherwise ->
            collectGeneral sel childRaw childCrumbs rest
    dispatchSingle (Sel.ComplexSelector compound []) r c rest
      | Sel.isFlatCompound compound = collectFlat compound r c rest
    dispatchSingle cs r c rest =
      let !(subject, ctx) = decomposeComplex cs
      in if Sel.isFlatCompound subject && canFastCtx ctx
         then collectFastCtx subject ctx r c rest
         else collectGeneral sel r c rest
    canFastCtx = all pairOk
      where
        pairOk (Sel.AdjacentSibling, _) = False
        pairOk (Sel.GeneralSibling, _) = False
        pairOk (_, comp) = Sel.isFlatCompound comp
    isFlatSingle (Sel.ComplexSelector c []) = Sel.isFlatCompound c
    isFlatSingle _ = False
    extractCompound (Sel.ComplexSelector c _) = c

-- | Like 'querySelectorAllSel' but uses the document's precomputed element
-- index for O(1) structural pseudo-class evaluation and optional tag-index
-- candidate reduction. Returns results in document order.
querySelectorAllDoc :: Sel.Selector -> Document -> [Node]
querySelectorAllDoc sel@(Sel.Selector complexSels) (Document _ idx) =
  case complexSels of
    [cs] -> dispatchIdx cs
    _    -> scanAllIdx sel idx
  where
    dispatchIdx cs =
      let !(subject, ctx) = decomposeComplex cs
      in case ctx of
        [] -> scanCompoundIdx subject idx
        _  -> scanComplexIdx subject ctx idx

-- Intersect two sorted PrimArray Int32s.
intersectSorted :: PrimArray Int32 -> PrimArray Int32 -> PrimArray Int32
intersectSorted a b = runST $ do
  let !na = sizeofPrimArray a
      !nb = sizeofPrimArray b
  out <- newPrimArray (min na nb)
  let go !ia !ib !k
        | ia >= na || ib >= nb = pure k
        | otherwise =
            let !va = indexPrimArray a ia
                !vb = indexPrimArray b ib
            in case compare va vb of
              LT -> go (ia + 1) ib k
              GT -> go ia (ib + 1) k
              EQ -> do
                writePrimArray out k va
                go (ia + 1) (ib + 1) (k + 1)
  !k <- go 0 0 0
  shrinkMutablePrimArray out k
  unsafeFreezePrimArray out

-- Resolve candidate set for a compound selector. When both tag and class
-- indices are available, intersects them so that type + class matching can
-- be skipped per-element.
data CandidateSet
  = PreMatched !(PrimArray Int32)
  | NeedFilter !(PrimArray Int32) !Sel.CompoundSelector

resolveCandidates :: Sel.CompoundSelector -> ElementIndex -> Maybe CandidateSet
resolveCandidates compound idx =
  let mTag = Sel.compoundType compound >>= \t -> HM.lookup t (eiByTag idx)
      mCls = Sel.compoundClass compound >>= \c -> HM.lookup c (eiByClass idx)
  in case (mTag, mCls) of
    (Just tagArr, Just clsArr) ->
      let !inter = intersectSorted tagArr clsArr
      in case Sel.compoundSubsWithoutClass compound of
        Just [] -> Just (PreMatched inter)
        Just rest -> Just (NeedFilter inter (Sel.CompoundSelector Nothing rest))
        Nothing -> Just (NeedFilter inter compound)
    (Just tagArr, Nothing) ->
      case stripType compound of
        Sel.CompoundSelector _ [] -> Just (PreMatched tagArr)
        rest -> Just (NeedFilter tagArr rest)
    (Nothing, Just clsArr) ->
      Just (NeedFilter clsArr (stripClass compound))
    (Nothing, Nothing) -> Nothing
  where
    stripType (Sel.CompoundSelector _ subs) = Sel.CompoundSelector Nothing subs
    stripClass c = case Sel.compoundSubsWithoutClass c of
      Just rest -> Sel.CompoundSelector (Sel.compoundType c >>= \t -> Just (Sel.TypeTag t)) rest
      Nothing -> c
{-# INLINE resolveCandidates #-}

-- Build nodes from an array of known-matching indices (iterate backwards for
-- correct document order without reverse).
buildNodesRev :: ElementIndex -> PrimArray Int32 -> Int -> [Node] -> [Node]
buildNodesRev idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
          !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
      in buildNodesRev idx arr (j - 1) (node : acc)

scanCompoundIdx :: Sel.CompoundSelector -> ElementIndex -> [Node]
scanCompoundIdx compound idx =
  case resolveCandidates compound idx of
    Just (PreMatched arr) ->
      buildNodesRev idx arr (sizeofPrimArray arr - 1) []
    Just (NeedFilter arr rest) ->
      filterCandidatesRev rest idx arr (sizeofPrimArray arr - 1) []
    Nothing -> scanAllFwd compound idx 0 (eiCount idx)

filterCandidatesRev :: Sel.CompoundSelector -> ElementIndex -> PrimArray Int32
                    -> Int -> [Node] -> [Node]
filterCandidatesRev compound idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
      in if matchesCompoundIdx idx i compound
         then let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
              in filterCandidatesRev compound idx arr (j - 1) (node : acc)
         else filterCandidatesRev compound idx arr (j - 1) acc

scanAllFwd :: Sel.CompoundSelector -> ElementIndex -> Int -> Int -> [Node]
scanAllFwd compound idx !start !n = go (n - 1) []
  where
    go !i acc
      | i < start = acc
      | matchesCompoundIdx idx i compound =
          let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
          in go (i - 1) (node : acc)
      | otherwise = go (i - 1) acc

-- Eagerly resolve a CandidateSet to a PrimArray of definite matches.
materializeCandidates :: CandidateSet -> ElementIndex -> PrimArray Int32
materializeCandidates (PreMatched arr) _ = arr
materializeCandidates (NeedFilter arr compound) idx = runST $ do
  let !n = sizeofPrimArray arr
  out <- newPrimArray n
  let go !j !k
        | j >= n = pure k
        | otherwise =
            let !i = fromIntegral (indexPrimArray arr j) :: Int
            in if matchesCompoundIdx idx i compound
               then writePrimArray out k (indexPrimArray arr j) >> go (j + 1) (k + 1)
               else go (j + 1) k
  !k <- go 0 0
  shrinkMutablePrimArray out k
  unsafeFreezePrimArray out

scanComplexIdx :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
               -> ElementIndex -> [Node]
scanComplexIdx subject ctx idx =
  case ctx of
    [(Sel.Descendant, ctxCompound)]
      | Just subCS  <- resolveCandidates subject idx
      , Just ctxCS  <- resolveCandidates ctxCompound idx ->
          let !ctxArr = materializeCandidates ctxCS idx
          in mergeDescendantNodes subCS ctxArr idx
    _ -> case resolveCandidates subject idx of
      Just (PreMatched arr) ->
        filterContextRev ctx idx arr (sizeofPrimArray arr - 1) []
      Just (NeedFilter arr rest) ->
        filterComplexRev rest ctx idx arr (sizeofPrimArray arr - 1) []
      Nothing -> scanAllComplexFwd subject ctx idx 0 (eiCount idx)

-- Strict pair for merge stack entries, avoiding boxed (Int32, Int32) tuples.
data CtxEntry = CtxEntry {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32

-- O(m+k) merge scan for descendant combinator. Subject candidates and context
-- candidates are both sorted in document order. Uses eiSubEnd for subtree
-- containment: context c is an ancestor of subject s iff c < s < eiSubEnd[c].
mergeDescendantNodes :: CandidateSet -> PrimArray Int32 -> ElementIndex -> [Node]
mergeDescendantNodes subjectCS contextArr idx =
  let (!subArr, !mCompound) = case subjectCS of
        PreMatched arr   -> (arr, Nothing)
        NeedFilter arr c -> (arr, Just c)
      !nSub = sizeofPrimArray subArr
      !nCtx = sizeofPrimArray contextArr
      !subEnd = eiSubEnd idx
      !nodes = eiNodes idx
      goFwd !si !ci !stk !acc
        | si >= nSub = acc
        | otherwise =
            let !s = indexPrimArray subArr si
                (!ci', !stk') = pushCtx ci stk s
                !stk'' = popExpired stk' s
            in case stk'' of
              [] -> goFwd (si + 1) ci' stk'' acc
              _  ->
                let !sI = fromIntegral s :: Int
                    keep = case mCompound of
                      Nothing -> True
                      Just c  -> matchesCompoundIdx idx sI c
                in if keep
                   then let !node = Node (indexSmallArray nodes sI) (rebuildCrumbs idx sI)
                        in goFwd (si + 1) ci' stk'' (node : acc)
                   else goFwd (si + 1) ci' stk''  acc
      pushCtx !ci !stk !s
        | ci >= nCtx = (ci, stk)
        | otherwise =
            let !c = indexPrimArray contextArr ci
            in if c < s
               then pushCtx (ci + 1) (CtxEntry c (indexPrimArray subEnd (fromIntegral c)) : stk) s
               else (ci, stk)
      popExpired [] _ = []
      popExpired stk@(CtxEntry _ end : rest) !s
        | end <= s  = popExpired rest s
        | otherwise = stk
  in reverse (goFwd 0 0 [] [])

-- Subject is pre-matched; only check context combinators.
filterContextRev :: [(Sel.Combinator, Sel.CompoundSelector)]
                 -> ElementIndex -> PrimArray Int32 -> Int -> [Node] -> [Node]
filterContextRev ctx idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
      in if matchContextIdx idx i ctx
         then let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
              in filterContextRev ctx idx arr (j - 1) (node : acc)
         else filterContextRev ctx idx arr (j - 1) acc

filterComplexRev :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
                 -> ElementIndex -> PrimArray Int32 -> Int -> [Node] -> [Node]
filterComplexRev subject ctx idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
      in if matchesCompoundIdx idx i subject && matchContextIdx idx i ctx
         then let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
              in filterComplexRev subject ctx idx arr (j - 1) (node : acc)
         else filterComplexRev subject ctx idx arr (j - 1) acc

scanAllComplexFwd :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
                  -> ElementIndex -> Int -> Int -> [Node]
scanAllComplexFwd subject ctx idx !start !n = go (n - 1) []
  where
    go !i acc
      | i < start = acc
      | matchesCompoundIdx idx i subject && matchContextIdx idx i ctx =
          let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
          in go (i - 1) (node : acc)
      | otherwise = go (i - 1) acc

-- Full scan fallback for multi-selector (comma-separated).
scanAllIdx :: Sel.Selector -> ElementIndex -> [Node]
scanAllIdx sel idx = go (eiCount idx - 1) []
  where
    go !i acc
      | i < 0 = acc
      | matchesSelectorIdx sel idx i =
          let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
          in go (i - 1) (node : acc)
      | otherwise = go (i - 1) acc

-- ---------------------------------------------------------------------------
-- Optimized traversals
-- ---------------------------------------------------------------------------

-- Flat traversal for selectors that are a single compound with no
-- combinators and no structural pseudo-classes. Matches tag + attrs
-- directly, only constructing Node for results. Text nodes are skipped
-- entirely (no Crumb allocation).
collectFlat :: Sel.CompoundSelector -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectFlat compound = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if matchCompoundFlat compound tag attrs
         then Node raw crumbs : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest


-- Flat traversal for multiple flat compound selectors (comma-separated).
collectFlatMulti :: [Sel.CompoundSelector] -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectFlatMulti compounds = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if anyMatch tag attrs compounds
         then Node raw crumbs : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest
    anyMatch !_ !_ [] = False
    anyMatch tag attrs (c:cs) = matchCompoundFlat c tag attrs || anyMatch tag attrs cs


-- Fast-context traversal: subject and ancestor compounds are all flat
-- (no structural pseudos), combinators are Descendant/Child only.
-- Matches subject against tag+attrs, ancestor compounds against crumbs
-- directly — never reconstructing Node for context matching.
collectFastCtx :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
               -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectFastCtx subject ctx = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if matchCompoundFlat subject tag attrs && matchCtxCrumbs ctx crumbs
         then Node raw crumbs : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest

matchCtxCrumbs :: [(Sel.Combinator, Sel.CompoundSelector)] -> [Crumb] -> Bool
matchCtxCrumbs [] _ = True
matchCtxCrumbs _ [] = False
matchCtxCrumbs ((Sel.Descendant, comp) : rest) crumbs = anyCrumb comp rest crumbs
matchCtxCrumbs ((Sel.Child, comp) : rest) (Crumb tag attrs _ _ : parentCrumbs) =
  matchCompoundFlat comp tag attrs && matchCtxCrumbs rest parentCrumbs
matchCtxCrumbs _ _ = False

anyCrumb :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)] -> [Crumb] -> Bool
anyCrumb _ _ [] = False
anyCrumb comp rest (Crumb tag attrs _ _ : parentCrumbs) =
  (matchCompoundFlat comp tag attrs && matchCtxCrumbs rest parentCrumbs)
  || anyCrumb comp rest parentCrumbs


-- General traversal using difference lists and direct SmallArray indexing.
-- Used when the selector needs full tree context (structural pseudos,
-- sibling combinators). Text nodes are still skipped.
collectGeneral :: Sel.Selector -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectGeneral sel = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !node = Node raw crumbs
          !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if matchesSelector sel node
         then node : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs

-- | Depth-first search for the first element matching a complex selector.
findFirstComplex :: Sel.ComplexSelector -> Node -> Maybe Node
findFirstComplex cs node
  | matchesComplex cs node = Just node
  | otherwise = firstJust (findFirstComplex cs) (childNodes node)

-- | Test whether a node matches a CSS selector string (Element.matches()).
matches :: Node -> Text -> Bool
matches node selText =
  case Sel.parseSelector selText of
    Left _  -> False
    Right sel -> matchesSelector sel node

-- | Return the closest ancestor (or self) matching a CSS selector
-- string (Element.closest()).
closest :: Node -> Text -> Maybe Node
closest node selText =
  case Sel.parseSelector selText of
    Left _  -> Nothing
    Right sel -> go sel node
  where
    go sel n
      | matchesSelector sel n = Just n
      | otherwise = case parentNode n of
          Nothing -> Nothing
          Just p  -> go sel p
