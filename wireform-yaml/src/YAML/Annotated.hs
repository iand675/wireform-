{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Annotated YAML representation that preserves enough source-level
information to round-trip a parsed document /verbatim/, while still
allowing programmatic modification.

The intended workflow is:

@
Right (adoc, original) <- 'YAML.Decode.Annotated.decodeAnnotated' yamlText
let adoc' = adoc { 'adBody' = 'updateAt' [\"server\", \"port\"] ('aInt' 8081) ('adBody' adoc) }
let yamlText' = 'YAML.Pretty.renderAnnotatedDocument' 'YAML.Pretty.defaultOptions' adoc' (Just original)
@

The renderer copies original source bytes for sub-trees that haven't
been modified (preserving comments, blank lines, quote style,
block-vs-flow choice, indentation, etc.) and falls back to the
pretty-printer's settings for fresh content.
-}
module YAML.Annotated (
  -- * Annotated value
  AValue (..),
  ABody (..),
  AMapEntry (..),
  Trivia (..),
  Comment (..),
  SrcSpan (..),

  -- * Style
  ScalarStyle (..),
  Chomping (..),
  SeqStyle (..),
  MapStyle (..),

  -- * Document / stream
  ADocument (..),
  AStream (..),

  -- * Construction (un-annotated, ready for fresh emission)
  aNull,
  aBool,
  aInt,
  aFloat,
  aString,
  aStringStyled,
  aSeq,
  aMap,
  aTagged,
  aAnchored,

  -- * Querying
  abody,
  astripAnnotations,
  isDirty,
  markDirty,
  prepareForRendering,

  -- * Modification helpers
  setKey,
  updateKey,
  deleteKey,
  appendKey,
  updateAt,
  setIndex,
  appendItem,
  deleteIndex,
) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, evalState, get, modify')
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import YAML.Value (Anchor (..), Tag (..))
import YAML.Value qualified as YV


-- ---------------------------------------------------------------------------
-- Source spans + trivia
-- ---------------------------------------------------------------------------

{- | A half-open byte range @[start, end)@ in the original source
'Text' (encoded as UTF-8 and indexed by byte position so the
offsets are stable across text-encoding boundaries).
-}
data SrcSpan = SrcSpan
  { srcStart :: {-# UNPACK #-} !Int
  , srcEnd :: {-# UNPACK #-} !Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


{- | A single comment token: a leading @#@ on its own line, or an
end-of-line comment trailing a value.
-}
data Comment = Comment
  { commentText :: !Text
  {- ^ The text after the @#@, without the @#@ itself or any
  indenting whitespace. May contain trailing whitespace.
  -}
  , commentColumn :: {-# UNPACK #-} !Int
  {- ^ Column where the @#@ sat in the source, useful for
  preserving indentation when re-emitting fresh content
  around the comment.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Whitespace / comments / blank-lines that decorate a value.
data Trivia = Trivia
  { triviaLeading :: ![Comment]
  {- ^ Stand-alone comments on lines /before/ this value
  (between the previous structural token and this one).
  -}
  , triviaBlankLines :: {-# UNPACK #-} !Int
  {- ^ Number of blank lines immediately preceding the value
  (after any leading comments).
  -}
  , triviaTrailing :: !(Maybe Comment)
  {- ^ End-of-line comment on the same line as the value, if
  any.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Trivia with no comments and no blank lines.
emptyTrivia :: Trivia
emptyTrivia = Trivia [] 0 Nothing


-- ---------------------------------------------------------------------------
-- Style
-- ---------------------------------------------------------------------------

-- | How a scalar was (or should be) emitted.
data ScalarStyle
  = -- | @value@ — unquoted plain scalar.
    SSPlain
  | -- | @\"value\"@ — double-quoted; supports YAML escapes.
    SSDoubleQuoted
  | -- | @\'value\'@ — single-quoted.
    SSSingleQuoted
  | -- | @|@ — literal block scalar.
    SSLiteral !Chomping
  | -- | @>@ — folded block scalar.
    SSFolded !Chomping
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Chomping indicator for block scalars.
data Chomping = Clip | Strip | Keep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | How a sequence was (or should be) emitted.
data SeqStyle = SeqBlock | SeqFlow
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | How a mapping was (or should be) emitted.
data MapStyle = MapBlock | MapFlow
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- ---------------------------------------------------------------------------
-- The annotated tree
-- ---------------------------------------------------------------------------

{- | Annotated YAML value.

Carries enough information for the renderer to reproduce the
original source byte-for-byte when nothing has been modified
('avSpan' present and 'avDirty' False) and to fall back to the
'YAML.Pretty.RenderOptions' pretty-printer otherwise.
-}
data AValue = AValue
  { avBody :: !ABody
  , avTrivia :: !Trivia
  , avSpan :: !(Maybe SrcSpan)
  {- ^ The half-open byte range in the original source that
  produced this value, if any. 'Nothing' means the value
  was constructed fresh and has no original-source backing.
  -}
  , avNodeSpan :: !(Maybe SrcSpan)
  -- ^ Exact flow-node token range, excluding its surrounding entry and trivia.
  , avTrailingText :: !Text
  -- ^ Container-ending trivia retained when children are edited.
  , avAnchorId :: {-# UNPACK #-} !Int
  -- ^ Original binding identity on an anchor definition; negative for fresh nodes.
  , avDirty :: !Bool
  {- ^ Set to 'True' when a sub-tree has been modified; the
  renderer falls back to fresh emission for any node with
  @'avDirty' == True@ even if 'avSpan' is present.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | The actual structural body of an 'AValue'.
data ABody
  = ANull
  | ABool !Bool
  | AInt !Integer
  | AFloat !Double
  | AString !Text !ScalarStyle
  | ASeq !(Vector AValue) !SeqStyle
  | AMap !(Vector AMapEntry) !MapStyle
  | ATagged !Tag !AValue
  | AAnchored !Anchor !AValue
  | {- | A @*alias@ reference. The annotated layer keeps it
    explicit; projection to 'YAML.Value.Value' preserves the
    reference as 'YAML.Value.YAlias'.
    -}
    AAlias !Anchor
  | {- | An original alias with its immutable binding identity and resolved
    target. The target permits safe materialization if its definition is deleted
    or moved after this reference during an edit.
    -}
    AResolvedAlias {-# UNPACK #-} !Int !Anchor !AValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | A mapping entry.

The @amTrailing@ comment is the eol comment on the value line
(if any); per-entry trivia for blank lines and stand-alone
comments lives on the @amKey@'s 'avTrivia'.
-}
data AMapEntry = AMapEntry
  { amKey :: !AValue
  , amValue :: !AValue
  , amTrailing :: !(Maybe Comment)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- ---------------------------------------------------------------------------
-- Documents / streams
-- ---------------------------------------------------------------------------

-- | An annotated document.
data ADocument = ADocument
  { adDirectivesEnd :: !Bool
  , adExplicitEnd :: !Bool
  , adBody :: !AValue
  , adLeading :: ![Comment]
  {- ^ Comments before the document body (after any preceding
  @---@ marker).
  -}
  , adOriginalSpan :: !(Maybe SrcSpan)
  -- ^ Original document bytes, including framing and trailing trivia.
  , adTrailing :: ![Comment]
  {- ^ Comments after the body (before any @...@ marker or the
  next document).
  -}
  , adDirectives :: ![Text]
  -- ^ Original directive lines, retained when edited content is re-emitted.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | An annotated multi-document stream.
newtype AStream = AStream {aStreamDocs :: Vector ADocument}
  deriving stock (Show, Eq, Generic)
  deriving newtype (NFData)


-- ---------------------------------------------------------------------------
-- Construction (fresh, un-annotated)
-- ---------------------------------------------------------------------------

freshAValue :: ABody -> AValue
freshAValue b = AValue b emptyTrivia Nothing Nothing "" (-1) False


aNull :: AValue
aNull = freshAValue ANull


aBool :: Bool -> AValue
aBool b = freshAValue (ABool b)


aInt :: Integer -> AValue
aInt n = freshAValue (AInt n)


aFloat :: Double -> AValue
aFloat d = freshAValue (AFloat d)


{- | Construct a string in the renderer's default scalar style
(typically plain unless the body would parse back as some other
core-schema literal, in which case the renderer will quote it).
-}
aString :: Text -> AValue
aString t = freshAValue (AString t SSPlain)


-- | Construct a string with an explicit scalar style.
aStringStyled :: Text -> ScalarStyle -> AValue
aStringStyled t s = freshAValue (AString t s)


aSeq :: [AValue] -> AValue
aSeq xs = freshAValue (ASeq (V.fromList xs) SeqBlock)


aMap :: [(AValue, AValue)] -> AValue
aMap kvs =
  freshAValue
    (AMap (V.fromList ((\(k, v) -> AMapEntry k v Nothing) <$> kvs)) MapBlock)


aTagged :: Tag -> AValue -> AValue
aTagged t v = freshAValue (ATagged t v)


aAnchored :: Anchor -> AValue -> AValue
aAnchored a v = freshAValue (AAnchored a v)


-- ---------------------------------------------------------------------------
-- Querying
-- ---------------------------------------------------------------------------

abody :: AValue -> ABody
abody = avBody


isDirty :: AValue -> Bool
isDirty = avDirty


{- | Mark a value as dirty so the renderer will re-emit it from
scratch (using 'YAML.Pretty.RenderOptions') instead of copying
the original source bytes.
-}
markDirty :: AValue -> AValue
markDirty v = v {avDirty = True}
{-# INLINE markDirty #-}


{- | Preserve references to definitions that still precede them. References to
deleted or displaced definitions become independent copies of their targets;
immutable identities keep legal anchor-name rebinding unambiguous.
-}
prepareForRendering :: AValue -> AValue
prepareForRendering root
  | not (hasAliases root) = root
  | otherwise = evalState (go root) HM.empty
  where
    definitions = collect Nothing HM.empty root
    hasAliases value = case avBody value of
      AResolvedAlias _ _ _ -> True
      ATagged _ inner -> hasAliases inner
      AAnchored _ inner -> hasAliases inner
      ASeq items _ -> V.any hasAliases items
      AMap entries _ -> V.any (\entry -> hasAliases (amKey entry) || hasAliases (amValue entry)) entries
      _ -> False
    collect outerTag found value = case avBody value of
      AAnchored _ inner ->
        let target = maybe inner (`aTagged` inner) outerTag
            updated = if avAnchorId value < 0 then found else HM.insert (avAnchorId value) target found
        in collect Nothing updated inner
      ATagged tag inner -> collect (Just tag) found inner
      ASeq items _ -> V.foldl' (collect Nothing) found items
      AMap entries _ -> V.foldl' (\acc entry -> collect Nothing (collect Nothing acc (amKey entry)) (amValue entry)) found entries
      _ -> found
    go :: AValue -> State (HM.HashMap Text Int) AValue
    go value = case avBody value of
      AResolvedAlias identity anchor snapshot -> do
        active <- get
        if HM.lookup (unAnchor anchor) active == Just identity
          then pure value
          else pure (materialize HS.empty (HM.lookupDefault snapshot identity definitions)) {avTrivia = avTrivia value}
      AAnchored anchor inner -> do
        modify' (HM.insert (unAnchor anchor) (avAnchorId value))
        updated <- go inner
        pure value {avBody = AAnchored anchor updated, avDirty = avDirty value || avDirty updated}
      ATagged tag inner -> do
        updated <- go inner
        pure value {avBody = ATagged tag updated, avDirty = avDirty value || avDirty updated}
      ASeq items style -> do
        updated <- V.mapM go items
        pure value {avBody = ASeq updated style, avDirty = avDirty value || V.any avDirty updated}
      AMap entries style -> do
        updated <- V.mapM (\entry -> do key <- go (amKey entry); item <- go (amValue entry); pure entry {amKey = key, amValue = item}) entries
        pure value {avBody = AMap updated style, avDirty = avDirty value || V.any (\entry -> avDirty (amKey entry) || avDirty (amValue entry)) updated}
      _ -> pure value
    materialize visited value = case avBody value of
      AAnchored _ inner -> materialize visited inner
      AResolvedAlias identity _ snapshot ->
        let target = if HS.member identity visited then snapshot else HM.lookupDefault snapshot identity definitions
        in materialize (HS.insert identity visited) target
      body ->
        value
          { avSpan = Nothing
          , avDirty = True
          , avAnchorId = -1
          , avBody = case body of
              ATagged tag inner -> ATagged tag (materialize visited inner)
              ASeq items style -> ASeq (materialize visited <$> items) style
              AMap entries style -> AMap ((\entry -> entry {amKey = materialize visited (amKey entry), amValue = materialize visited (amValue entry)}) <$> entries) style
              other -> other
          }


{- | Project an 'AValue' down to the unannotated 'YAML.Value.Value'
representation. Explicit aliases project to 'YAML.Value.YAlias'.
-}
astripAnnotations :: AValue -> YV.Value
astripAnnotations a = case avBody a of
  ANull -> YV.YNull
  ABool b -> YV.YBool b
  AInt n -> YV.YInt n
  AFloat d -> YV.YFloat d
  AString t _ -> YV.YString t
  ASeq xs _ -> YV.YSeq (V.map astripAnnotations xs)
  AMap es _ ->
    YV.YMap
      ( V.map
          ( \(AMapEntry k v _) ->
              (astripAnnotations k, astripAnnotations v)
          )
          es
      )
  ATagged tg v -> YV.YTagged tg (astripAnnotations v)
  AAnchored an v -> YV.YAnchored an (astripAnnotations v)
  AAlias anchor -> YV.YAlias anchor
  AResolvedAlias _ anchor _ -> YV.YAlias anchor


-- ---------------------------------------------------------------------------
-- Modification helpers
-- ---------------------------------------------------------------------------

{- | Replace the value at the given mapping key. If the key isn't
present the original mapping is returned unchanged. Marks the
mapping (and only the mapping) as dirty.
-}
setKey :: Text -> AValue -> AValue -> AValue
setKey k newV m = updateKey k (const (Just newV)) m


{- | Update the value at the given mapping key. Returning 'Nothing'
from the function deletes the entry; returning 'Just' replaces
it. If the key isn't present the original mapping is returned.
-}
updateKey :: Text -> (AValue -> Maybe AValue) -> AValue -> AValue
updateKey k f m = case avBody m of
  AMap entries style ->
    if V.any (matchesKey k . amKey) entries
      then markDirty m {avBody = AMap (V.mapMaybe updateEntry entries) style}
      else m
  ATagged tag inner -> modifyInner (ATagged tag) (updateKey k f) m inner
  AAnchored anchor inner -> modifyInner (AAnchored anchor) (updateKey k f) m inner
  AResolvedAlias _ _ target -> editAlias (updateKey k f) m target
  _ -> m
  where
    updateEntry entry
      | matchesKey k (amKey entry) = (\value -> entry {amValue = preserveStyle (amValue entry) value}) <$> f (amValue entry)
      | otherwise = Just entry


matchesKey :: Text -> AValue -> Bool
matchesKey key value = case avBody value of
  AString text _ -> text == key
  ATagged _ inner -> matchesKey key inner
  AAnchored _ inner -> matchesKey key inner
  AResolvedAlias _ _ target -> matchesKey key target
  _ -> False


modifyInner :: (AValue -> ABody) -> (AValue -> AValue) -> AValue -> AValue -> AValue
modifyInner wrap f outer inner =
  let changed = f inner
  in if avDirty changed then markDirty outer {avBody = wrap changed} else outer


editAlias :: (AValue -> AValue) -> AValue -> AValue -> AValue
editAlias change reference target =
  let updated = change target
  in if avDirty updated then markDirty updated {avTrivia = avTrivia reference, avSpan = Nothing} else reference


preserveStyle :: AValue -> AValue -> AValue
preserveStyle old new =
  let styled = case (avBody old, avBody new) of
        (AString _ style, AString text SSPlain) -> new {avBody = AString text style}
        (AAnchored anchor inner, _) | not (hasAnchor new) -> (aAnchored anchor (preserveStyle inner (detachSelf (avAnchorId old) new))) {avAnchorId = avAnchorId old}
        (AAnchored anchor _, _) -> inheritIdentity anchor (avAnchorId old) new
        (ATagged _ inner, _) -> preserveStyle inner new
        _ -> new
  in markDirty styled {avTrivia = avTrivia old, avTrailingText = avTrailingText old}
  where
    hasAnchor value = case avBody value of
      AAnchored _ _ -> True
      ATagged _ inner -> hasAnchor inner
      _ -> False
    inheritIdentity anchor identity value = case avBody value of
      AAnchored name _ | name == anchor -> value {avAnchorId = identity}
      ATagged tag inner -> value {avBody = ATagged tag (inheritIdentity anchor identity inner)}
      _ -> value
    detachSelf identity value
      | not (dependsOn identity value) = value
      | otherwise = case avBody value of
          AResolvedAlias _ _ target -> detachSelf identity target
          ATagged tag inner -> markDirty value {avBody = ATagged tag (detachSelf identity inner)}
          AAnchored anchor inner -> markDirty value {avBody = AAnchored anchor (detachSelf identity inner)}
          ASeq items style -> markDirty value {avBody = ASeq (detachSelf identity <$> items) style}
          AMap entries style -> markDirty value {avBody = AMap ((\entry -> entry {amKey = detachSelf identity (amKey entry), amValue = detachSelf identity (amValue entry)}) <$> entries) style}
          _ -> value
    dependsOn identity value = case avBody value of
      AResolvedAlias original _ target -> original == identity || dependsOn identity target
      ATagged _ inner -> dependsOn identity inner
      AAnchored _ inner -> dependsOn identity inner
      ASeq items _ -> V.any (dependsOn identity) items
      AMap entries _ -> V.any (\entry -> dependsOn identity (amKey entry) || dependsOn identity (amValue entry)) entries
      _ -> False


-- | Delete a key from a mapping. No-op if absent.
deleteKey :: Text -> AValue -> AValue
deleteKey k m = updateKey k (const Nothing) m


{- | Append a key-value pair to the end of a mapping. If the key
already exists the existing value is left in place; use
'setKey' to replace.
-}
appendKey :: Text -> AValue -> AValue -> AValue
appendKey k v m = case avBody m of
  AMap entries style
    | V.any (matchesKey k . amKey) entries -> m
    | otherwise ->
        let entry =
              AMapEntry
                (markDirty (aString k))
                (markDirty v)
                Nothing
        in markDirty
             m
               { avBody = AMap (V.snoc entries entry) style
               }
  ATagged tag inner -> modifyInner (ATagged tag) (appendKey k v) m inner
  AAnchored anchor inner -> modifyInner (AAnchored anchor) (appendKey k v) m inner
  AResolvedAlias _ _ target -> editAlias (appendKey k v) m target
  _ -> m


{- | Walk a path of mapping keys and apply 'setKey' at the leaf.
The intermediate mappings are also marked dirty so the renderer
knows their layout might need re-flowing.
-}
updateAt :: [Text] -> AValue -> AValue -> AValue
updateAt [] _ m = m
updateAt [k] v m = setKey k v m
updateAt (k : ks) v m = updateKey k (Just . updateAt ks v) m


{- | Replace an item in a sequence. If the index is out of bounds
the sequence is returned unchanged.
-}
setIndex :: Int -> AValue -> AValue -> AValue
setIndex i x s = case avBody s of
  ASeq items style
    | i >= 0
    , i < V.length items ->
        markDirty
          s
            { avBody = ASeq (items V.// [(i, preserveStyle (items V.! i) x)]) style
            }
  ATagged tag inner -> modifyInner (ATagged tag) (setIndex i x) s inner
  AAnchored anchor inner -> modifyInner (AAnchored anchor) (setIndex i x) s inner
  AResolvedAlias _ _ target -> editAlias (setIndex i x) s target
  _ -> s


-- | Append an item to the end of a sequence.
appendItem :: AValue -> AValue -> AValue
appendItem x s = case avBody s of
  ASeq items style ->
    markDirty
      s
        { avBody = ASeq (V.snoc items (markDirty x)) style
        }
  ATagged tag inner -> modifyInner (ATagged tag) (appendItem x) s inner
  AAnchored anchor inner -> modifyInner (AAnchored anchor) (appendItem x) s inner
  AResolvedAlias _ _ target -> editAlias (appendItem x) s target
  _ -> s


-- | Remove an item from a sequence.
deleteIndex :: Int -> AValue -> AValue
deleteIndex i s = case avBody s of
  ASeq items style
    | i >= 0
    , i < V.length items ->
        let items' = V.ifilter (\j _ -> j /= i) items
        in markDirty s {avBody = ASeq items' style}
  ATagged tag inner -> modifyInner (ATagged tag) (deleteIndex i) s inner
  AAnchored anchor inner -> modifyInner (AAnchored anchor) (deleteIndex i) s inner
  AResolvedAlias _ _ target -> editAlias (deleteIndex i) s target
  _ -> s
