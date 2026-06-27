{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2295 §8.6 @Variant-Vary@ — response header used in a choice response
to record vary information that applies to the variant data rather than
to the response as a whole.

== Grammar

@
Variant-Vary = \"Variant-Vary\" \":\" ( \"*\" | 1#field-name )
@

The literal @\"*\"@ surfaces as 'VariantVaryAll'; otherwise the value is
a non-empty list of header field-name tokens.

Spec: <https://www.rfc-editor.org/rfc/rfc2295#section-8.6>

See also: "Network.HTTP.Headers.Alternates", "Network.HTTP.Headers.TCN", "Network.HTTP.Headers.Negotiate", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.VariantVary (
  VariantVary (..),
  variantVaryParser,
  renderVariantVary,
) where

import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hVariantVary)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | @VariantVaryAll@ corresponds to the literal @\"*\"@; all other values
surface as a non-empty list of field-name tokens.
-}
data VariantVary
  = VariantVaryAll
  | VariantVaryFields !(NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


instance KnownHeader VariantVary where
  type ParseFailure VariantVary = String
  type Cardinality VariantVary = 'ZeroOrOne
  type Direction VariantVary = 'Response


  parseFromHeaders _ headers = case runParser variantVaryParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Variant-Vary: " <> show leftover)
    Fail -> Left "Failed to parse Variant-Vary header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderVariantVary


  headerName _ = hVariantVary


variantVaryParser :: ParserT st String VariantVary
variantVaryParser = ows *> (allVariants <|> fields)
  where
    allVariants = VariantVaryAll <$ $(char '*')
    fields = do
      first <- fieldName
      rest <- many (ows *> $(char ',') *> ows *> fieldName)
      pure (VariantVaryFields (first :| rest))


renderVariantVary :: VariantVary -> M.Builder
renderVariantVary = \case
  VariantVaryAll -> M.char7 '*'
  VariantVaryFields fs -> M.intersperse ", " (map R.shortText (NE.toList fs))
