{- |
RFC 2295 §8.6 @Variant-Vary@ records the request field names that select
the variant data in a choice response. It carries either a lone wildcard
or an ordered nonempty list of field names. The original spellings are
retained for rendering.

Spec: <https://www.rfc-editor.org/rfc/rfc2295.html#section-8.6>
-}
module Network.HTTP.Headers.VariantVary (
  VariantVary (..),
  variantVaryFieldNames,
  variantVaryParser,
  renderVariantVary,
) where

import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAsciiUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, hVariantVary, unsafeUnknownHeaderFromBytestring)
import qualified Network.HTTP.Headers.Mason as M


data VariantVary
  = VariantVaryAll
  | VariantVaryFields !(NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


{- | Return case-folded HTTP names. The wildcard has no finite list.
Invalid names supplied via the raw constructor yield 'Left'.
-}
variantVaryFieldNames :: VariantVary -> Either String (Maybe (NonEmpty HeaderFieldName))
variantVaryFieldNames VariantVaryAll = Right Nothing
variantVaryFieldNames (VariantVaryFields fields) = Just <$> traverse name fields
  where
    name raw
      | BSC.null bytes || bytes == "*" || not (BSC.all isTokenChar bytes) = Left "Invalid Variant-Vary field name"
      | otherwise = Right (unsafeUnknownHeaderFromBytestring folded)
      where
        bytes = ST.toByteString raw
        folded
          | BSC.any isAsciiUpper bytes = BSC.map toLower bytes
          | otherwise = bytes


instance KnownHeader VariantVary where
  type ParseFailure VariantVary = String
  type Cardinality VariantVary = 'ZeroOrOne
  type Direction VariantVary = 'Response


  parseFromHeaders _ ("*" :| []) = Right VariantVaryAll
  parseFromHeaders _ fields =
    parseListFieldLines "Variant-Vary" (httpList fieldName) fields >>= variantVaryFromList


  renderToHeaders _ VariantVaryAll = "*"
  renderToHeaders _ fields = M.toStrictByteString (renderVariantVary fields)


  headerName _ = hVariantVary


variantVaryParser :: ParserT st String VariantVary
variantVaryParser = httpList fieldName >>= either err pure . variantVaryFromList


variantVaryFromList :: [ST.ShortText] -> Either String VariantVary
variantVaryFromList ["*"] = Right VariantVaryAll
variantVaryFromList fields
  | "*" `elem` fields = Left "Variant-Vary wildcard must stand alone"
  | otherwise = VariantVaryFields <$> requireNonEmptyList "Variant-Vary" fields


renderVariantVary :: VariantVary -> M.Builder
renderVariantVary VariantVaryAll = M.char7 '*'
renderVariantVary (VariantVaryFields (first :| rest)) = shortText first <> foldMap ((", " <>) . shortText) rest
{-# INLINE renderVariantVary #-}
