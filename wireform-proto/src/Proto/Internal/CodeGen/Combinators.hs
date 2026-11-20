{- | Shared helpers for the code-generation pipeline.

__THIS MODULE IS INTERNAL TO wireform.  DO NOT DEPEND ON IT.__
-}
module Proto.Internal.CodeGen.Combinators (
  -- * Text → Doc helpers
  txt,
  tshow,

  -- * Structural helpers
  braceBlock,
  instanceHead,
  chooseGeneratedName,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter


{- | Emit a 'Text' literal into the generated Haskell source.
Replaces the ubiquitous @pretty ("..." :: Text)@ pattern.
-}
txt :: Text -> Doc ann
txt = pretty
{-# INLINE txt #-}


-- | @T.pack . show@ — convenient for embedding numbers / enum tags.
tshow :: Show a => a -> Text
tshow = T.pack . show
{-# INLINE tshow #-}


{- | Format a list of field docs as a brace-delimited block:

@
{ field1
, field2
}
@
-}
braceBlock :: [Doc ann] -> Doc ann
braceBlock [] = txt "{ }"
braceBlock (f : fs) =
  vsep (txt "{ " <> f : fmap (\x -> txt ", " <> x) fs)
    <> line
    <> txt "}"


-- | @instance C T where@
instanceHead :: Text -> Text -> Doc ann
instanceHead cls ty = txt "instance " <> txt cls <> txt " " <> txt ty <> txt " where"


-- | Keep the original binding when available and disambiguate collisions.
chooseGeneratedName :: Set Text -> Text -> Text -> Text
chooseGeneratedName used name suffix
  | Set.notMember name used = name
  | otherwise = unused (name <> "''" <> suffix)
  where
    unused candidate
      | Set.member candidate used = unused (candidate <> "'")
      | otherwise = candidate
