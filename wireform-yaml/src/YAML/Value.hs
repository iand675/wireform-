{- | YAML 1.2 value representation.

The 'Value' ADT models the YAML 1.2 representation graph:

* Scalars corresponding to the YAML 1.2 /core schema/:
  'YNull', 'YBool', 'YInt', 'YFloat', and 'YString'.
* 'YSeq' for sequences (\"YAML lists\").
* 'YMap' for mappings (\"YAML dicts\"). Insertion order is preserved
  in the underlying 'Vector'; YAML mappings are unordered in the
  data model but downstream consumers (round-tripping in particular)
  typically want the source order.
* 'YAnchored' and 'YTagged' wrap sub-values with anchor identifiers and
  tag URIs so that anchor / alias relationships and explicit tags
  round-trip cleanly. An un-anchored, untagged scalar lives
  directly as 'YString' / 'YInt' / etc. without a wrapper.

A 'Document' carries the optional directives end marker and
optional document end marker so that emitters can faithfully
preserve multi-document streams.
-}
module YAML.Value (
  Value (..),
  Document (..),
  Stream (..),
  Tag (..),
  Anchor (..),

  -- * Construction helpers
  null_,
  bool,
  int,
  float,
  string,
  seq_,
  mapping,

  -- * Accessors
  unwrap,
  lookupKey,
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


-- | An anchor identifier, e.g. the @ref@ in @&ref@.
newtype Anchor = Anchor {unAnchor :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NFData)


{- | A YAML tag URI, e.g. @tag:yaml.org,2002:str@. Short-hand tags
(@!!str@) are resolved to their full URI form during decoding.
-}
newtype Tag = Tag {unTag :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NFData)


{- | YAML representation node.

Integers have arbitrary precision. Presentation details such as quoting
and comments live in 'YAML.Annotated.AValue', rather than this data model.
-}
data Value
  = YNull
  | YBool !Bool
  | YInt !Integer
  | YFloat !Double
  | YString !Text
  | YSeq !(Vector Value)
  | YMap !(Vector (Value, Value))
  | -- | A node carrying an explicit YAML tag.
    YTagged !Tag !Value
  | {- | A node introduced with @&anchor@. Subsequent @*anchor@
    references in the source resolve to the same logical node;
    decoders expand aliases while retaining anchor metadata on the
    definition and on each resolved reference.
    -}
    YAnchored !Anchor !Value
  | -- | An alias reference. Decoders resolve these before returning a value.
    YAlias !Anchor
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A single YAML document within a stream.
data Document = Document
  { docDirectivesEnd :: !Bool
  -- ^ @True@ when a @---@ marker preceded the body in the source.
  , docExplicitEnd :: !Bool
  -- ^ @True@ when a @...@ marker followed the body in the source.
  , docBody :: !Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A YAML stream is a sequence of documents.
newtype Stream = Stream {unStream :: Vector Document}
  deriving stock (Show, Eq, Generic)
  deriving newtype (NFData)


-- ---------------------------------------------------------------------------
-- Construction helpers
-- ---------------------------------------------------------------------------

null_ :: Value
null_ = YNull


bool :: Bool -> Value
bool = YBool


int :: Integer -> Value
int = YInt


float :: Double -> Value
float = YFloat


string :: Text -> Value
string = YString


seq_ :: [Value] -> Value
seq_ = YSeq . V.fromList


mapping :: [(Value, Value)] -> Value
mapping = YMap . V.fromList


-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | Strip outer 'YAnchored' / 'YTagged' wrappers.
unwrap :: Value -> Value
unwrap = go
  where
    go (YAnchored _ v) = go v
    go (YTagged _ v) = go v
    go v = v


{- | Lookup by 'YString' key in a 'YMap'. Skips wrappers on both the
node we're searching and on its keys. Returns the first match in
source order.
-}
lookupKey :: Text -> Value -> Maybe Value
lookupKey k v = case unwrap v of
  YMap kvs -> goV 0
    where
      !len = V.length kvs
      goV !i
        | i >= len = Nothing
        | otherwise = case kvs V.! i of
            (kk, vv) -> case unwrap kk of
              YString k' | k' == k -> Just vv
              _ -> goV (i + 1)
  _ -> Nothing
