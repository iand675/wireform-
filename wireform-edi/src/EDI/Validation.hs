-- | Generic EDI structural validation.
module EDI.Validation
  ( ValidationError(..)
  , validateInterchange
  , validationErrors
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import EDI.Value

data ValidationError
  = InvalidSyntax !String
  | EmptyInterchange
  | EmptySegmentTag !Int
  | ElementContainsDelimiter !Int !Int !Char
  | EmptyCompositeElement !Int !Int
  deriving stock (Show, Eq, Ord)

validateInterchange :: Interchange -> Either (Vector ValidationError) ()
validateInterchange doc =
  let errs = validationErrors doc
  in if V.null errs
       then Right ()
       else Left errs

validationErrors :: Interchange -> Vector ValidationError
validationErrors doc =
  V.fromList (syntaxErrors <> emptyErrors <> segmentErrors)
  where
    syn = interchangeSyntax doc
    segments = interchangeSegments doc
    syntaxErrors =
      case validateSyntax syn of
        Right () -> []
        Left err -> [InvalidSyntax err]
    emptyErrors =
      if V.null segments
        then [EmptyInterchange]
        else []
    segmentErrors =
      concat (V.ifoldr (\ix seg acc -> validateSegment syn ix seg : acc) [] segments)

validateSegment :: Syntax -> Int -> Segment -> [ValidationError]
validateSegment syn ix seg =
  tagErrors <> concat (V.ifoldr (\elemIx elemValue acc -> validateElement syn (segmentTag seg) ix elemIx elemValue : acc) [] (segmentElements seg))
  where
    tagErrors =
      if T.null (segmentTag seg)
        then [EmptySegmentTag ix]
        else delimiterErrors syn ix (-1) (segmentTag seg)

validateElement :: Syntax -> Text -> Int -> Int -> Element -> [ValidationError]
validateElement _ "ISA" _ elemIx (Simple _)
  | elemIx == 10 || elemIx == 15 = []
validateElement syn _ segIx elemIx (Simple t) =
  simpleDelimiterErrors syn segIx elemIx t
validateElement syn _ segIx elemIx (Composite parts)
  | V.null parts = [EmptyCompositeElement segIx elemIx]
  | otherwise =
      concat (V.foldr (\part acc -> delimiterErrors syn segIx elemIx part : acc) [] parts)

simpleDelimiterErrors :: Syntax -> Int -> Int -> Text -> [ValidationError]
simpleDelimiterErrors syn segIx elemIx t =
  foldr step [] delimiters
  where
    delimiters =
      case repetitionSeparator syn of
        Nothing -> [elementSeparator syn, segmentTerminator syn]
        Just r -> [elementSeparator syn, segmentTerminator syn, r]
    step delim acc =
      if T.any (== delim) t
        then ElementContainsDelimiter segIx elemIx delim : acc
        else acc

delimiterErrors :: Syntax -> Int -> Int -> Text -> [ValidationError]
delimiterErrors syn segIx elemIx t =
  foldr step [] delimiters
  where
    delimiters =
      case repetitionSeparator syn of
        Nothing ->
          [ elementSeparator syn
          , componentSeparator syn
          , segmentTerminator syn
          ]
        Just r ->
          [ elementSeparator syn
          , componentSeparator syn
          , segmentTerminator syn
          , r
          ]
    step delim acc =
      if T.any (== delim) t
        then ElementContainsDelimiter segIx elemIx delim : acc
        else acc
