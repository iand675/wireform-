{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

{- | CSS Selectors Level 4 parser and matching engine.

Supports CSS selectors for both DOM queries and the streaming rewriter:

__Both modes__: type, universal, ID, class, attribute selectors
(existence, exact, prefix, suffix, substring, word, hyphen; @i@/@s@ flags;
HTML enumerated attributes are case-insensitive by default),
namespace prefixes (parsed and discarded — HTML5 has no element namespaces),
descendant and child combinators, comma groups,
CSS escape sequences in identifiers and strings.

__DOM only__: adjacent sibling (@+@), general sibling (@~@),
@:first-child@, @:last-child@, @:only-child@,
@:first-of-type@, @:last-of-type@, @:only-of-type@,
@:nth-child(An+B)@, @:nth-last-child(An+B)@,
@:nth-child(An+B of S)@, @:nth-last-child(An+B of S)@,
@:nth-of-type@, @:nth-last-of-type@,
@:not()@, @:is()@ (forgiving), @:where()@ (forgiving), @:has()@ (relative selectors),
@:empty@, @:blank@, @:root@, @:scope@, @:defined@, @:target@, @:dir()@,
@:enabled@, @:disabled@, @:checked@, @:required@, @:optional@,
@:read-only@, @:read-write@, @:default@, @:placeholder-shown@, @:indeterminate@,
@:link@, @:any-link@, @:lang()@ (multi-argument).

Dynamic pseudo-classes (@:hover@, @:active@, @:focus@, @:focus-within@,
@:focus-visible@, @:visited@) parse successfully but never match in the
static DOM.
-}
module HTML.Selector (
  -- * Types
  Selector (..),
  ComplexSelector (..),
  CompoundSelector (..),
  Combinator (..),
  TypeSel (..),
  SubSel (..),
  SelectorError (..),

  -- * Parsing
  parseSelector,

  -- * Matching (simple / rewriter mode)
  matchCompound,
  matchType,
  matchSub,
  nthMatch,

  -- * Querying
  hasPseudoClasses,
  isRewriterCompatible,
  isFlatCompound,
  isClassOnlyCompound,
  hasClassWord,

  -- * Utilities
  findAttr,
  attrExists,
  withAttr,
  compoundType,
  compoundClass,
  compoundSubsWithoutClass,
) where

import Data.Array.Byte (ByteArray (..))
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal (Text (..))
import Data.Word (Word8)
import GHC.Exts (Addr#, ByteArray#, Int#, compareByteArrays#, indexWord8Array#, indexWord8OffAddr#, isTrue#, (==#))
import GHC.Int (Int (..))
import GHC.Word (Word8 (..))
import HTML.Value (HTMLAttribute (..))


-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Parsed CSS selector: comma-separated list of complex selectors.
newtype Selector = Selector [ComplexSelector]
  deriving (Show, Eq)


{- | Chain of compound selectors linked by combinators.
The first compound is the leftmost (ancestor-side in descendant selectors).
-}
data ComplexSelector
  = ComplexSelector
      !CompoundSelector
      ![(Combinator, CompoundSelector)]
  deriving (Show, Eq)


data Combinator
  = Descendant
  | Child
  | AdjacentSibling
  | GeneralSibling
  deriving (Show, Eq, Ord, Enum, Bounded)


data CompoundSelector
  = CompoundSelector
      !(Maybe TypeSel)
      ![SubSel]
  deriving (Show, Eq)


data TypeSel
  = TypeTag !Text
  | TypeUniversal
  deriving (Show, Eq)


data SubSel
  = SelId !Text
  | SelClass !Text
  | SelAttrExists !Text
  | SelAttrExact !Text !Text !Bool     -- ^ name, value, case-insensitive flag
  | SelAttrPrefix !Text !Text !Bool
  | SelAttrSuffix !Text !Text !Bool
  | SelAttrContains !Text !Text !Bool
  | SelAttrWord !Text !Text !Bool
  | SelAttrHyphen !Text !Text !Bool
  | SelNot !Selector
  | SelHas ![(Combinator, ComplexSelector)]
  | SelIs !Selector
  | SelWhere !Selector
  | SelFirstChild
  | SelLastChild
  | SelOnlyChild
  | SelFirstOfType
  | SelLastOfType
  | SelOnlyOfType
  | SelNthChild !Int !Int
  | SelNthLastChild !Int !Int
  | SelNthOfType !Int !Int
  | SelNthLastOfType !Int !Int
  | SelNthChildOf !Int !Int !Selector    -- ^ @:nth-child(An+B of S)@
  | SelNthLastChildOf !Int !Int !Selector
  | SelEmpty
  | SelRoot
  | SelScope
  | SelDefined
  | SelBlank
  | SelDir !Text
  | SelTarget
  | SelEnabled
  | SelDisabled
  | SelChecked
  | SelRequired
  | SelOptional
  | SelReadOnly
  | SelReadWrite
  | SelDefault
  | SelPlaceholderShown
  | SelIndeterminate
  | SelLink
  | SelLang ![Text]                      -- ^ One or more BCP 47 tags
  | SelNeverMatch
  deriving (Show, Eq)


data SelectorError
  = SelectorSyntaxError !Int !Text
  | UnsupportedSelector !Text
  deriving (Show, Eq)


-- ---------------------------------------------------------------------------
-- Querying
-- ---------------------------------------------------------------------------

{- | Whether any compound in the selector uses pseudo-classes
(DOM-only features not available in rewriter mode).
-}
hasPseudoClasses :: Selector -> Bool
hasPseudoClasses (Selector cs) = any complexHasPseudo cs
  where
    complexHasPseudo (ComplexSelector hd tl) =
      compoundHasPseudo hd || any (compoundHasPseudo . snd) tl
    compoundHasPseudo (CompoundSelector _ subs) = any subIsPseudo subs
    subIsPseudo (SelId _) = False
    subIsPseudo (SelClass _) = False
    subIsPseudo (SelAttrExists _) = False
    subIsPseudo (SelAttrExact {}) = False
    subIsPseudo (SelAttrPrefix {}) = False
    subIsPseudo (SelAttrSuffix {}) = False
    subIsPseudo (SelAttrContains {}) = False
    subIsPseudo (SelAttrWord {}) = False
    subIsPseudo (SelAttrHyphen {}) = False
    subIsPseudo _ = True


{- | Whether the selector can be used in rewriter (streaming) mode.
Rejects sibling combinators and pseudo-classes.
-}
isRewriterCompatible :: Selector -> Bool
isRewriterCompatible (Selector cs) = all complexOk cs
  where
    complexOk (ComplexSelector hd tl) =
      compoundOk hd && all pairOk tl
    pairOk (AdjacentSibling, _) = False
    pairOk (GeneralSibling, _) = False
    pairOk (_, c) = compoundOk c
    compoundOk (CompoundSelector _ subs) = all subOkForRewriter subs
    subOkForRewriter (SelId _) = True
    subOkForRewriter (SelClass _) = True
    subOkForRewriter (SelAttrExists _) = True
    subOkForRewriter (SelAttrExact {}) = True
    subOkForRewriter (SelAttrPrefix {}) = True
    subOkForRewriter (SelAttrSuffix {}) = True
    subOkForRewriter (SelAttrContains {}) = True
    subOkForRewriter (SelAttrWord {}) = True
    subOkForRewriter (SelAttrHyphen {}) = True
    subOkForRewriter _ = False


-- | Whether a compound selector can be matched using only tag name and
-- attributes — no tree context or structural pseudo-classes needed.
-- Logical pseudo-classes (:not, :is, :where) are flat when their inner
-- selectors are also flat.
isFlatCompound :: CompoundSelector -> Bool
isFlatCompound (CompoundSelector _ subs) = all isFlatSub subs
{-# INLINE isFlatCompound #-}

isFlatSub :: SubSel -> Bool
isFlatSub (SelId _) = True
isFlatSub (SelClass _) = True
isFlatSub (SelAttrExists _) = True
isFlatSub (SelAttrExact {}) = True
isFlatSub (SelAttrPrefix {}) = True
isFlatSub (SelAttrSuffix {}) = True
isFlatSub (SelAttrContains {}) = True
isFlatSub (SelAttrWord {}) = True
isFlatSub (SelAttrHyphen {}) = True
isFlatSub (SelNot (Selector cs)) = all isFlatComplex cs
isFlatSub (SelIs (Selector cs)) = all isFlatComplex cs
isFlatSub (SelWhere (Selector cs)) = all isFlatComplex cs
isFlatSub SelChecked = True
isFlatSub SelRequired = True
isFlatSub SelOptional = True
isFlatSub SelDefault = True
isFlatSub SelPlaceholderShown = True
isFlatSub SelIndeterminate = True
isFlatSub SelLink = True
isFlatSub SelDefined = True
isFlatSub SelNeverMatch = True
isFlatSub _ = False

isFlatComplex :: ComplexSelector -> Bool
isFlatComplex (ComplexSelector c []) = isFlatCompound c
isFlatComplex _ = False


-- | Extract the concrete tag name from a compound selector, if present.
-- Returns 'Nothing' for universal or absent type selectors.
compoundType :: CompoundSelector -> Maybe Text
compoundType (CompoundSelector (Just (TypeTag t)) _) = Just t
compoundType _ = Nothing
{-# INLINE compoundType #-}

-- | Extract the first class name from a compound selector's sub-selectors.
compoundClass :: CompoundSelector -> Maybe Text
compoundClass (CompoundSelector _ subs) = go subs
  where
    go [] = Nothing
    go (SelClass c : _) = Just c
    go (_ : rest) = go rest
{-# INLINE compoundClass #-}

-- | Return sub-selectors with the first SelClass removed (the one used for
-- index lookup). Returns Nothing if no SelClass was present.
compoundSubsWithoutClass :: CompoundSelector -> Maybe [SubSel]
compoundSubsWithoutClass (CompoundSelector _ subs) = go [] subs
  where
    go _ [] = Nothing
    go acc (SelClass _ : rest) = Just (reverse acc ++ rest)
    go acc (s : rest) = go (s : acc) rest
{-# INLINE compoundSubsWithoutClass #-}

-- | True when the compound selector can be fully decided using only
-- the tag name and class attribute value (no id, no arbitrary attrs).
isClassOnlyCompound :: CompoundSelector -> Bool
isClassOnlyCompound (CompoundSelector _ subs) = all isClassSub subs
  where
    isClassSub (SelClass _) = True
    isClassSub _ = False
{-# INLINE isClassOnlyCompound #-}

-- | Byte-level class word match using Addr# (for scanClassAndSkip results).
-- Checks if the needle Text word appears whitespace-delimited in the raw
-- bytes at addr# starting at offset for len bytes.
hasClassWord :: Text -> Addr# -> Int -> Int -> Bool
hasClassWord (Text (ByteArray needleBA#) nOff nLen) addr# !off !len
  | nLen == 0 = False
  | nLen > len = False
  | otherwise = scan off
  where
    !end = off + len
    rd :: Int -> Word8
    rd (I# i#) = W8# (indexWord8OffAddr# addr# i#)
    {-# INLINE rd #-}

    scan !i
      | i >= end = False
      | rd i == 0x20 = scan (i + 1)
      | otherwise =
          let !wEnd = findSpace i
              !wLen = wEnd - i
          in (wLen == nLen && bytesMatch nOff i nLen) || scan wEnd
    findSpace !i
      | i >= end = end
      | rd i == 0x20 = i
      | otherwise = findSpace (i + 1)
    bytesMatch !_ !_ 0 = True
    bytesMatch !bo !ao !n =
      W8# (indexWord8Array# needleBA# (toInt# bo)) == W8# (indexWord8OffAddr# addr# (toInt# ao))
      && bytesMatch (bo + 1) (ao + 1) (n - 1)
    toInt# :: Int -> Int#
    toInt# (I# i) = i
{-# INLINE hasClassWord #-}


-- ---------------------------------------------------------------------------
-- Matching
-- ---------------------------------------------------------------------------

{- | Match a compound selector against an element's tag and attributes.
Handles type, ID, class, and attribute selectors.
Pseudo-classes are /not/ checked (they need tree context);
use 'isRewriterCompatible' to reject selectors that need them.
-}
matchCompound :: CompoundSelector -> Text -> SmallArray HTMLAttribute -> Bool
matchCompound (CompoundSelector mtype subs) tag attrs =
  matchType mtype tag && allSubs attrs subs
{-# INLINE matchCompound #-}


allSubs :: SmallArray HTMLAttribute -> [SubSel] -> Bool
allSubs !_ [] = True
allSubs attrs (s : rest) = matchSub attrs s && allSubs attrs rest
{-# INLINE allSubs #-}


matchType :: Maybe TypeSel -> Text -> Bool
matchType Nothing _ = True
matchType (Just TypeUniversal) _ = True
matchType (Just (TypeTag t)) tag = t == tag
{-# INLINE matchType #-}


matchSub :: SmallArray HTMLAttribute -> SubSel -> Bool
matchSub attrs = \case
  SelId i -> withAttr "id" attrs False (== i)
  SelClass c -> withAttr "class" attrs False (hasWord c)
  SelAttrExists n -> attrExists n attrs
  SelAttrExact n v ci -> withAttr n attrs False (attrEq ci v)
  SelAttrPrefix n v ci
    | T.null v -> False
    | otherwise -> withAttr n attrs False (attrPfx ci v)
  SelAttrSuffix n v ci
    | T.null v -> False
    | otherwise -> withAttr n attrs False (attrSfx ci v)
  SelAttrContains n v ci
    | T.null v -> False
    | otherwise -> withAttr n attrs False (attrInf ci v)
  SelAttrWord n v ci -> withAttr n attrs False (hasWordCI ci v)
  SelAttrHyphen n v ci -> withAttr n attrs False (\av ->
    attrEq ci v av || attrPfx ci (v <> "-") av)
  -- Pseudo-classes return True in flat mode (rewriter rejects them at build time;
  -- DOM matching uses matchSubDOM instead)
  _ -> True
{-# INLINE matchSub #-}

attrEq :: Bool -> Text -> Text -> Bool
attrEq False a b = a == b
attrEq True a b = T.toCaseFold a == T.toCaseFold b
{-# INLINE attrEq #-}

attrPfx :: Bool -> Text -> Text -> Bool
attrPfx False p t = T.isPrefixOf p t
attrPfx True p t = T.isPrefixOf (T.toCaseFold p) (T.toCaseFold t)
{-# INLINE attrPfx #-}

attrSfx :: Bool -> Text -> Text -> Bool
attrSfx False s t = T.isSuffixOf s t
attrSfx True s t = T.isSuffixOf (T.toCaseFold s) (T.toCaseFold t)
{-# INLINE attrSfx #-}

attrInf :: Bool -> Text -> Text -> Bool
attrInf False s t = T.isInfixOf s t
attrInf True s t = T.isInfixOf (T.toCaseFold s) (T.toCaseFold t)
{-# INLINE attrInf #-}

hasWordCI :: Bool -> Text -> Text -> Bool
hasWordCI False w t = hasWord w t
hasWordCI True w t = hasWord (T.toCaseFold w) (T.toCaseFold t)
{-# INLINE hasWordCI #-}


-- | Test whether a 1-based index matches An+B.
nthMatch :: Int -> Int -> Int -> Bool
nthMatch !a !b !index
  | a == 0 = index == b
  | otherwise =
      let !diff = index - b
      in diff `rem` a == 0 && diff `quot` a >= 0
{-# INLINE nthMatch #-}


-- | CPS-style attribute lookup. Avoids Maybe box allocation.
withAttr :: Text -> SmallArray HTMLAttribute -> a -> (Text -> a) -> a
withAttr name attrs def f = go 0
  where
    !n = sizeofSmallArray attrs
    go !i
      | i >= n = def
      | HTMLAttribute k v <- indexSmallArray attrs i =
          if k == name then f v else go (i + 1)
{-# INLINE withAttr #-}


findAttr :: Text -> SmallArray HTMLAttribute -> Maybe Text
findAttr name attrs = go 0
  where
    !n = sizeofSmallArray attrs
    go !i
      | i >= n = Nothing
      | HTMLAttribute k v <- indexSmallArray attrs i, k == name = Just v
      | otherwise = go (i + 1)
{-# INLINE findAttr #-}


attrExists :: Text -> SmallArray HTMLAttribute -> Bool
attrExists name attrs = go 0
  where
    !n = sizeofSmallArray attrs
    go !i
      | i >= n = False
      | HTMLAttribute k _ <- indexSmallArray attrs i, k == name = True
      | otherwise = go (i + 1)
{-# INLINE attrExists #-}


-- | Check if a word appears in a space-separated list.
-- Uses byte-level comparison to avoid allocating intermediate Text slices.
hasWord :: Text -> Text -> Bool
hasWord (Text (ByteArray needleBA#) nOff nLen) (Text (ByteArray hayBA#) hOff hLen)
  | nLen == 0 = False
  | otherwise = scan hOff
  where
    !hEnd = hOff + hLen
    scan !i
      | i >= hEnd = False
      | W8# (indexWord8Array# hayBA# (toInt# i)) == 0x20 = scan (i + 1)
      | otherwise =
          let !wEnd = findSpace i
              !wLen = wEnd - i
          in (wLen == nLen && bytesEqual hayBA# i needleBA# nOff nLen) || scan wEnd
    findSpace !i
      | i >= hEnd = hEnd
      | W8# (indexWord8Array# hayBA# (toInt# i)) == 0x20 = i
      | otherwise = findSpace (i + 1)
{-# INLINE hasWord #-}

toInt# :: Int -> Int#
toInt# (I# i) = i
{-# INLINE toInt# #-}

bytesEqual :: ByteArray# -> Int -> ByteArray# -> Int -> Int -> Bool
bytesEqual a# !aOff b# !bOff !n =
  isTrue# (compareByteArrays# a# (toInt# aOff) b# (toInt# bOff) (toInt# n) ==# 0#)
{-# INLINE bytesEqual #-}


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

data P = P !Text !Int


pOff :: P -> Int
pOff (P _ n) = n
{-# INLINE pOff #-}


peek :: P -> Maybe Char
peek (P t _) = case T.uncons t of
  Just (c, _) -> Just c
  Nothing -> Nothing
{-# INLINE peek #-}


advance :: P -> P
advance (P t n) = P (T.drop 1 t) (n + 1)
{-# INLINE advance #-}


skipWS :: P -> P
skipWS p@(P t n) =
  let !ws = T.length (T.takeWhile isWSChar t)
  in if ws == 0 then p else P (T.drop ws t) (n + ws)
  where
    isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'


atEnd :: P -> Bool
atEnd (P t _) = T.null t
{-# INLINE atEnd #-}


err :: P -> Text -> Either SelectorError a
err (P _ n) msg = Left (SelectorSyntaxError n msg)


-- | Parse a CSS selector string.
parseSelector :: Text -> Either SelectorError Selector
parseSelector input = do
  let !p0 = skipWS (P input 0)
  if atEnd p0
    then err p0 "empty selector"
    else do
      (sels, p1) <- parseSelectorList p0
      let !p2 = skipWS p1
      if atEnd p2
        then Right (Selector sels)
        else err p2 ("unexpected: " <> T.take 20 (pRest p2))
  where
    pRest (P t _) = t


parseSelectorList :: P -> Either SelectorError ([ComplexSelector], P)
parseSelectorList p = do
  (first, p1) <- parseComplexSelector p
  loop [first] p1
  where
    loop !acc pp = do
      let !pp' = skipWS pp
      case peek pp' of
        Just ',' -> do
          let !pp'' = skipWS (advance pp')
          (next, pp3) <- parseComplexSelector pp''
          loop (next : acc) pp3
        _ -> Right (reverse acc, pp')


parseComplexSelector :: P -> Either SelectorError (ComplexSelector, P)
parseComplexSelector p = do
  (hd, p1) <- parseCompoundSelector p
  (tl, p2) <- parseComplexTail p1
  Right (ComplexSelector hd tl, p2)


parseComplexTail :: P -> Either SelectorError ([(Combinator, CompoundSelector)], P)
parseComplexTail p =
  case parseCombinator p of
    Just (comb, p2) -> do
      (compound, p3) <- parseCompoundSelector p2
      (more, p4) <- parseComplexTail p3
      Right ((comb, compound) : more, p4)
    Nothing -> Right ([], p)


parseCombinator :: P -> Maybe (Combinator, P)
parseCombinator p =
  let !p' = skipWS p
      !hadWS = pOff p' > pOff p
  in case peek p' of
      Just '>' -> Just (Child, skipWS (advance p'))
      Just '+' -> Just (AdjacentSibling, skipWS (advance p'))
      Just '~' -> Just (GeneralSibling, skipWS (advance p'))
      Just c | hadWS && isCompoundStart c -> Just (Descendant, p')
      _ -> Nothing


isCompoundStart :: Char -> Bool
isCompoundStart c =
  isIdentStart c || c == '*' || c == '.' || c == '#' || c == '[' || c == ':' || c == '|'


parseCompoundSelector :: P -> Either SelectorError (CompoundSelector, P)
parseCompoundSelector p = do
  let !p' = skipWS p
  case peek p' of
    Just '*' ->
      case peek (advance p') of
        Just '|' -> do
          -- *|E or *|* — skip namespace prefix, parse type selector
          let !p2 = advance (advance p')
          case peek p2 of
            Just '*' -> do
              (subs, p3) <- parseSubSelectors (advance p2)
              Right (CompoundSelector (Just TypeUniversal) subs, p3)
            Just c | isIdentStart c -> do
              (ident, p3) <- readIdent p2
              (subs, p4) <- parseSubSelectors p3
              Right (CompoundSelector (Just (TypeTag (T.toLower ident))) subs, p4)
            _ -> do
              (subs, p3) <- parseSubSelectors p2
              Right (CompoundSelector (Just TypeUniversal) subs, p3)
        _ -> do
          (subs, p3) <- parseSubSelectors (advance p')
          Right (CompoundSelector (Just TypeUniversal) subs, p3)
    Just '|' -> do
      -- |E — no-namespace prefix; in HTML all elements are in no namespace
      let !p2 = advance p'
      case peek p2 of
        Just '*' -> do
          (subs, p3) <- parseSubSelectors (advance p2)
          Right (CompoundSelector (Just TypeUniversal) subs, p3)
        Just c | isIdentStart c -> do
          (ident, p3) <- readIdent p2
          (subs, p4) <- parseSubSelectors p3
          Right (CompoundSelector (Just (TypeTag (T.toLower ident))) subs, p4)
        _ -> err p2 "expected element name after |"
    Just c | isIdentStart c -> do
      (ident, p'') <- readIdent p'
      case peek p'' of
        Just '|' -> do
          -- ns|E — skip namespace prefix
          let !p2 = advance p''
          case peek p2 of
            Just '*' -> do
              (subs, p3) <- parseSubSelectors (advance p2)
              Right (CompoundSelector (Just TypeUniversal) subs, p3)
            Just ci | isIdentStart ci -> do
              (ident2, p3) <- readIdent p2
              (subs, p4) <- parseSubSelectors p3
              Right (CompoundSelector (Just (TypeTag (T.toLower ident2))) subs, p4)
            _ -> do
              (subs, p3) <- parseSubSelectors p2
              Right (CompoundSelector (Just TypeUniversal) subs, p3)
        _ -> do
          (subs, p3) <- parseSubSelectors p''
          Right (CompoundSelector (Just (TypeTag (T.toLower ident))) subs, p3)
    Just c | c == '.' || c == '#' || c == '[' || c == ':' -> do
      (subs, p3) <- parseSubSelectors p'
      Right (CompoundSelector Nothing subs, p3)
    Just c -> err p' ("unexpected '" <> T.singleton c <> "' in selector")
    Nothing -> err p' "unexpected end of selector"


parseSubSelectors :: P -> Either SelectorError ([SubSel], P)
parseSubSelectors p =
  case peek p of
    Just '.' -> do
      (cls, p') <- readIdent (advance p)
      (more, p'') <- parseSubSelectors p'
      Right (SelClass cls : more, p'')
    Just '#' -> do
      (ident, p') <- readIdent (advance p)
      (more, p'') <- parseSubSelectors p'
      Right (SelId ident : more, p'')
    Just '[' -> do
      (attr, p') <- parseAttrSelector (skipWS (advance p))
      (more, p'') <- parseSubSelectors p'
      Right (attr : more, p'')
    Just ':' -> do
      (pseudo, p') <- parsePseudo (advance p)
      (more, p'') <- parseSubSelectors p'
      Right (pseudo : more, p'')
    _ -> Right ([], p)


-- ---------------------------------------------------------------------------
-- Attribute selector
-- ---------------------------------------------------------------------------

parseAttrSelector :: P -> Either SelectorError (SubSel, P)
parseAttrSelector p = do
  -- Handle namespace prefixes: [|attr], [*|attr], [ns|attr]
  -- In HTML, all attributes are in no namespace, so *|attr and |attr
  -- are equivalent to plain [attr].
  p0 <- case peek p of
    Just '|' -> Right (advance p)
    Just '*' -> case peek (advance p) of
      Just '|' -> Right (advance (advance p))
      _ -> Right p
    _ -> do
      (name, p1) <- readIdent p
      case peek p1 of
        Just '|' -> Right (advance p1)
        _ -> Right p
  (name, p1) <- readIdent p0
  let !lname = T.toLower name
      !p2 = skipWS p1
  case peek p2 of
    Just ']' -> Right (SelAttrExists lname, advance p2)
    Just '=' -> do
      (val, p3) <- readAttrValue (skipWS (advance p2))
      (ci, p3a) <- parseCIFlag lname p3
      p4 <- expectChar ']' (skipWS p3a)
      Right (SelAttrExact lname val ci, p4)
    Just '^' -> parseAttrOp SelAttrPrefix lname p2
    Just '$' -> parseAttrOp SelAttrSuffix lname p2
    Just '*' -> parseAttrOp SelAttrContains lname p2
    Just '~' -> parseAttrOp SelAttrWord lname p2
    Just '|' -> parseAttrOp SelAttrHyphen lname p2
    _ -> err p2 "expected ] or attribute operator"


parseAttrOp :: (Text -> Text -> Bool -> SubSel) -> Text -> P -> Either SelectorError (SubSel, P)
parseAttrOp ctor name p = do
  let !p1 = advance p
  case peek p1 of
    Just '=' -> do
      (val, p2) <- readAttrValue (skipWS (advance p1))
      (ci, p2a) <- parseCIFlag name p2
      p3 <- expectChar ']' (skipWS p2a)
      Right (ctor name val ci, p3)
    _ -> err p "expected = after operator"


-- | Parse optional case-sensitivity flag. When absent, HTML enumerated
-- attributes default to case-insensitive per the HTML spec.
parseCIFlag :: Text -> P -> Either SelectorError (Bool, P)
parseCIFlag attrName p0 =
  let !p = skipWS p0
  in case peek p of
    Just 'i' -> Right (True, advance p)
    Just 'I' -> Right (True, advance p)
    Just 's' -> Right (False, advance p)
    Just 'S' -> Right (False, advance p)
    _ -> Right (htmlCaseInsensitiveAttr attrName, p)

-- | HTML attributes whose values are always compared case-insensitively
-- in selectors, per the HTML and Selectors Level 4 specs.
htmlCaseInsensitiveAttr :: Text -> Bool
htmlCaseInsensitiveAttr n =
  n == "type" || n == "dir" || n == "lang" || n == "rel"
  || n == "target" || n == "method" || n == "enctype"
  || n == "accept-charset" || n == "http-equiv"
  || n == "shape" || n == "scope" || n == "align" || n == "valign"
  || n == "frame" || n == "rules" || n == "scrolling"
  || n == "clear" || n == "media" || n == "step"
  || n == "wrap" || n == "kind" || n == "loading"
  || n == "decoding" || n == "crossorigin" || n == "referrerpolicy"
  || n == "formmethod" || n == "formenctype" || n == "formtarget"
  || n == "autocomplete" || n == "inputmode" || n == "translate"
  || n == "draggable" || n == "spellcheck" || n == "contenteditable"


readAttrValue :: P -> Either SelectorError (Text, P)
readAttrValue p = case peek p of
  Just '"' -> readString '"' (advance p)
  Just '\'' -> readString '\'' (advance p)
  Just c | isIdentStart c || isDigit c -> readIdent p
  _ -> err p "expected attribute value"


-- ---------------------------------------------------------------------------
-- Pseudo-class
-- ---------------------------------------------------------------------------

parsePseudo :: P -> Either SelectorError (SubSel, P)
parsePseudo p =
  case peek p of
    Just ':' -> do
      -- :: pseudo-element — parse and reject
      (name, p1) <- readIdent (advance p)
      Left (UnsupportedSelector ("::" <> name))
    _ -> do
      (name, p1) <- readIdent p
      case T.toLower name of
        "first-child" -> Right (SelFirstChild, p1)
        "last-child" -> Right (SelLastChild, p1)
        "only-child" -> Right (SelOnlyChild, p1)
        "first-of-type" -> Right (SelFirstOfType, p1)
        "last-of-type" -> Right (SelLastOfType, p1)
        "only-of-type" -> Right (SelOnlyOfType, p1)
        "empty" -> Right (SelEmpty, p1)
        "root" -> Right (SelRoot, p1)
        "not" -> parseFunctionalSelector SelNot p1
        "is" -> parseFunctionalSelector SelIs p1
        "matches" -> parseFunctionalSelector SelIs p1
        "where" -> parseFunctionalSelector SelWhere p1
        "has" -> parseHasSelector p1
        "nth-child" -> parseNthPseudoOf SelNthChild SelNthChildOf p1
        "nth-last-child" -> parseNthPseudoOf SelNthLastChild SelNthLastChildOf p1
        "nth-of-type" -> parseNthPseudo SelNthOfType p1
        "nth-last-of-type" -> parseNthPseudo SelNthLastOfType p1
        -- Structural
        "scope" -> Right (SelScope, p1)
        "defined" -> Right (SelDefined, p1)
        "blank" -> Right (SelBlank, p1)
        "target" -> Right (SelTarget, p1)
        -- Form pseudo-classes
        "enabled" -> Right (SelEnabled, p1)
        "disabled" -> Right (SelDisabled, p1)
        "checked" -> Right (SelChecked, p1)
        "required" -> Right (SelRequired, p1)
        "optional" -> Right (SelOptional, p1)
        "read-only" -> Right (SelReadOnly, p1)
        "read-write" -> Right (SelReadWrite, p1)
        "default" -> Right (SelDefault, p1)
        "placeholder-shown" -> Right (SelPlaceholderShown, p1)
        "indeterminate" -> Right (SelIndeterminate, p1)
        -- Link
        "link" -> Right (SelLink, p1)
        "any-link" -> Right (SelLink, p1)
        -- Dynamic pseudo-classes: parse but never match in static DOM
        "hover" -> Right (SelNeverMatch, p1)
        "active" -> Right (SelNeverMatch, p1)
        "focus" -> Right (SelNeverMatch, p1)
        "focus-within" -> Right (SelNeverMatch, p1)
        "focus-visible" -> Right (SelNeverMatch, p1)
        "visited" -> Right (SelNeverMatch, p1)
        -- Language / direction
        "lang" -> parseLangPseudo p1
        "dir" -> parseDirPseudo p1
        -- Legacy single-colon pseudo-elements — parse and reject
        "before" -> Left (UnsupportedSelector "::before")
        "after" -> Left (UnsupportedSelector "::after")
        "first-line" -> Left (UnsupportedSelector "::first-line")
        "first-letter" -> Left (UnsupportedSelector "::first-letter")
        other -> Left (UnsupportedSelector other)


-- | Parse forgiving selector list for :is() / :where().
-- Invalid branches are silently dropped per Selectors Level 4.
parseFunctionalSelector :: (Selector -> SubSel) -> P -> Either SelectorError (SubSel, P)
parseFunctionalSelector ctor p1 = do
  p2 <- expectChar '(' p1
  (inner, p3) <- parseForgivingSelectorList (skipWS p2)
  p4 <- expectChar ')' (skipWS p3)
  Right (ctor (Selector inner), p4)

-- | Parse a forgiving selector list: each branch that fails to parse is
-- silently dropped rather than causing the whole selector to fail.
parseForgivingSelectorList :: P -> Either SelectorError ([ComplexSelector], P)
parseForgivingSelectorList p = go p []
  where
    go !p' !acc =
      case parseComplexSelector (skipWS p') of
        Right (cs, p'') ->
          let !p''' = skipWS p''
          in case peek p''' of
            Just ',' -> go (advance p''') (cs : acc)
            _ -> Right (reverse (cs : acc), p''')
        Left _ -> skipToNextBranch p' acc
    skipToNextBranch !p' !acc =
      case peek p' of
        Nothing -> Right (reverse acc, p')
        Just ')' -> Right (reverse acc, p')
        Just ',' -> go (advance p') acc
        _ -> skipToNextBranch (advance p') acc


parseNthPseudo :: (Int -> Int -> SubSel) -> P -> Either SelectorError (SubSel, P)
parseNthPseudo ctor p1 = do
  p2 <- expectChar '(' p1
  (a, b, p3) <- parseNthExpr (skipWS p2)
  p4 <- expectChar ')' (skipWS p3)
  Right (ctor a b, p4)

-- | Parse :nth-child / :nth-last-child with optional "of S" clause.
parseNthPseudoOf :: (Int -> Int -> SubSel)
                 -> (Int -> Int -> Selector -> SubSel)
                 -> P -> Either SelectorError (SubSel, P)
parseNthPseudoOf plainCtor ofCtor p1 = do
  p2 <- expectChar '(' p1
  (a, b, p3) <- parseNthExpr (skipWS p2)
  let !p3' = skipWS p3
  if peekWord p3' "of"
    then do
      let !p4 = skipWS (skipN 2 p3')
      (sels, p5) <- parseForgivingSelectorList p4
      p6 <- expectChar ')' (skipWS p5)
      Right (ofCtor a b (Selector sels), p6)
    else do
      p4 <- expectChar ')' p3'
      Right (plainCtor a b, p4)

-- | Parse :has() with a relative selector list.
parseHasSelector :: P -> Either SelectorError (SubSel, P)
parseHasSelector p1 = do
  p2 <- expectChar '(' p1
  (rels, p3) <- parseRelativeSelectorList (skipWS p2)
  p4 <- expectChar ')' (skipWS p3)
  Right (SelHas rels, p4)

-- | Parse comma-separated relative selectors (each may start with a combinator).
parseRelativeSelectorList :: P -> Either SelectorError ([(Combinator, ComplexSelector)], P)
parseRelativeSelectorList p = go p []
  where
    go !p' !acc = do
      (rel, p'') <- parseRelativeSelector (skipWS p')
      let !p''' = skipWS p''
      case peek p''' of
        Just ',' -> go (advance p''') (rel : acc)
        _ -> Right (reverse (rel : acc), p''')

-- | Parse one relative selector: optional leading combinator + complex selector.
parseRelativeSelector :: P -> Either SelectorError ((Combinator, ComplexSelector), P)
parseRelativeSelector p =
  let !p' = skipWS p
  in case peek p' of
    Just '>' -> do
      (cs, p'') <- parseComplexSelector (skipWS (advance p'))
      Right ((Child, cs), p'')
    Just '+' -> do
      (cs, p'') <- parseComplexSelector (skipWS (advance p'))
      Right ((AdjacentSibling, cs), p'')
    Just '~' -> do
      (cs, p'') <- parseComplexSelector (skipWS (advance p'))
      Right ((GeneralSibling, cs), p'')
    _ -> do
      (cs, p'') <- parseComplexSelector p'
      Right ((Descendant, cs), p'')

-- | Parse :lang() with one or more comma-separated language tags.
parseLangPseudo :: P -> Either SelectorError (SubSel, P)
parseLangPseudo p1 = do
  p2 <- expectChar '(' p1
  (tags, p3) <- parseLangTags (skipWS p2) []
  p4 <- expectChar ')' (skipWS p3)
  Right (SelLang tags, p4)
  where
    parseLangTags !p !acc = do
      (tag', p') <- readLangTag (skipWS p)
      let !t = T.toLower tag'
          !p'' = skipWS p'
      case peek p'' of
        Just ',' -> parseLangTags (advance p'') (t : acc)
        _ -> Right (reverse (t : acc), p'')

-- | Parse :dir(ltr) or :dir(rtl).
parseDirPseudo :: P -> Either SelectorError (SubSel, P)
parseDirPseudo p1 = do
  p2 <- expectChar '(' p1
  (dir', p3) <- readIdent (skipWS p2)
  p4 <- expectChar ')' (skipWS p3)
  Right (SelDir (T.toLower dir'), p4)

-- | Check if the next characters match a keyword (case-insensitive),
-- followed by whitespace or a non-ident character.
peekWord :: P -> Text -> Bool
peekWord (P t _) kw =
  let !n = T.length kw
      !prefix = T.take n t
  in T.toCaseFold prefix == T.toCaseFold kw
     && (T.length t <= n || let c = T.index t n in not (isAlphaNum c || c == '_' || c == '-'))

-- | Advance parser by n characters.
skipN :: Int -> P -> P
skipN !n (P t pos) = P (T.drop n t) (pos + n)


-- ---------------------------------------------------------------------------
-- nth expression: an+b
-- ---------------------------------------------------------------------------

parseNthExpr :: P -> Either SelectorError (Int, Int, P)
parseNthExpr p =
  let !p' = skipWS p
  in case peek p' of
      Nothing -> err p' "expected nth expression"
      Just c
        | c == 'o' || c == 'O' -> do
            (w, p1) <- readIdent p'
            if T.toLower w == "odd"
              then Right (2, 1, p1)
              else err p' ("unexpected '" <> w <> "' in :nth-child")
        | c == 'e' || c == 'E' -> do
            (w, p1) <- readIdent p'
            if T.toLower w == "even"
              then Right (2, 0, p1)
              else err p' ("unexpected '" <> w <> "' in :nth-child")
        | c == 'n' || c == 'N' -> do
            let !p1 = advance p'
            parseNthB 1 p1
        | c == '-' -> do
            let !p1 = advance p'
            case peek p1 of
              Just cn | cn == 'n' || cn == 'N' -> parseNthB (-1) (advance p1)
              _ -> do
                (num, p2) <- readInt p1
                Right (0, negate num, p2)
        | c == '+' -> do
            let !p1 = advance p'
            case peek p1 of
              Just cn | cn == 'n' || cn == 'N' -> parseNthB 1 (advance p1)
              _ -> do
                (num, p2) <- readInt p1
                Right (0, num, p2)
        | isDigit c -> do
            (num, p1) <- readInt p'
            case peek p1 of
              Just cn | cn == 'n' || cn == 'N' -> parseNthB num (advance p1)
              _ -> Right (0, num, p1)
        | otherwise -> err p' "expected nth expression"


parseNthB :: Int -> P -> Either SelectorError (Int, Int, P)
parseNthB a p =
  let !p' = skipWS p
  in case peek p' of
      Just '+' -> do
        (b, p1) <- readInt (skipWS (advance p'))
        Right (a, b, p1)
      Just '-' -> do
        (b, p1) <- readInt (skipWS (advance p'))
        Right (a, negate b, p1)
      _ -> Right (a, 0, p')


readInt :: P -> Either SelectorError (Int, P)
readInt p@(P t n) =
  let digits = T.takeWhile isDigit t
      !dlen = T.length digits
  in if dlen == 0
      then err p "expected integer"
      else Right (textToInt digits, P (T.drop dlen t) (n + dlen))


textToInt :: Text -> Int
textToInt = T.foldl' (\acc c -> acc * 10 + digitToInt c) 0


-- ---------------------------------------------------------------------------
-- Lexer primitives
-- ---------------------------------------------------------------------------

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_' || c == '-' || c > '\x7F'
{-# INLINE isIdentStart #-}


isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '-' || c > '\x7F'
{-# INLINE isIdentChar #-}


readIdent :: P -> Either SelectorError (Text, P)
readIdent p@(P t _) =
  let !ident = T.takeWhile isIdentChar t
      !len = T.length ident
  in case T.uncons (T.drop len t) of
      Just ('\\', _) -> readIdentEsc p
      _ | len == 0 ->
            case T.uncons t of
              Just ('\\', _) -> readIdentEsc p
              _ -> err p "expected identifier"
        | otherwise -> Right (ident, advanceN len p)
  where
    advanceN !k (P tt nn) = P (T.drop k tt) (nn + k)
    {-# INLINE advanceN #-}


readIdentEsc :: P -> Either SelectorError (Text, P)
readIdentEsc p0 = go p0 []
  where
    go p acc = case peek p of
      Just '\\' -> do
        (c, p') <- readEscape (advance p)
        go p' (c : acc)
      Just c | isIdentChar c ->
        let (chunk, p') = spanIdent p
        in go p' (chunk : acc)
      _ | null acc -> err p0 "expected identifier"
        | otherwise -> Right (T.concat (reverse acc), p)
    spanIdent (P t nn) =
      let chunk = T.takeWhile isIdentChar t
          !len = T.length chunk
      in (chunk, P (T.drop len t) (nn + len))


readLangTag :: P -> Either SelectorError (Text, P)
readLangTag p@(P t n) =
  let tag = T.takeWhile (\c -> isAlphaNum c || c == '-' || c == '_') t
      !len = T.length tag
  in if len == 0
      then err p "expected language tag"
      else Right (tag, P (T.drop len t) (n + len))


readEscape :: P -> Either SelectorError (Text, P)
readEscape p@(P t n) = case T.uncons t of
  Nothing -> Right ("\\", p)
  Just (c, _)
    | isHexDigit c ->
        let hex = T.takeWhile isHexDigit (T.take 6 t)
            !hlen = T.length hex
            !cp = T.foldl' (\acc h -> acc * 16 + hexVal h) 0 hex
            !p1 = P (T.drop hlen t) (n + hlen)
            !p2 = case peek p1 of
              Just ' '  -> advance p1
              Just '\t' -> advance p1
              Just '\n' -> advance p1
              Just '\r' -> advance p1
              Just '\f' -> advance p1
              _ -> p1
        in Right (T.singleton (toEnum cp), p2)
    | c == '\n' || c == '\r' || c == '\f' -> Right (T.singleton c, advance p)
    | otherwise -> Right (T.singleton c, advance p)
  where
    isHexDigit ch = isDigit ch || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
    hexVal ch
      | isDigit ch = fromEnum ch - fromEnum '0'
      | ch >= 'a' && ch <= 'f' = fromEnum ch - fromEnum 'a' + 10
      | otherwise = fromEnum ch - fromEnum 'A' + 10


readString :: Char -> P -> Either SelectorError (Text, P)
readString quote p0 = go p0 []
  where
    go p@(P t n) acc = case T.uncons t of
      Nothing -> Left (SelectorSyntaxError n "unterminated string")
      Just (c, _)
        | c == quote -> Right (T.concat (reverse acc), advance p)
        | c == '\\' -> do
            (esc, p') <- readEscape (advance p)
            go p' (esc : acc)
        | otherwise ->
            let (chunk, rest) = T.break (\ch -> ch == quote || ch == '\\') t
                !clen = T.length chunk
            in go (P rest (n + clen)) (chunk : acc)


expectChar :: Char -> P -> Either SelectorError P
expectChar c p = case peek p of
  Just c' | c' == c -> Right (advance p)
  Just c' -> err p ("expected '" <> T.singleton c <> "' but got '" <> T.singleton c' <> "'")
  Nothing -> err p ("expected '" <> T.singleton c <> "' but got end of input")
