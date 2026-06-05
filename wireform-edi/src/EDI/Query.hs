-- | Segment and element lookup helpers for dynamic EDI values.
module EDI.Query
  ( segmentAt
  , segmentsByTag
  , firstSegmentByTag
  , requireSegmentByTag
  , segmentHasTag
  , elementAt
  , requireElement
  , elementText
  , elementTextAt
  , requireElementText
  , compositeAt
  , countSegments
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import EDI.Value

segmentAt :: Int -> Interchange -> Maybe Segment
segmentAt ix doc = interchangeSegments doc V.!? ix

segmentsByTag :: Text -> Interchange -> Vector Segment
segmentsByTag tag doc =
  V.filter (segmentHasTag tag) (interchangeSegments doc)

firstSegmentByTag :: Text -> Interchange -> Maybe Segment
firstSegmentByTag tag doc =
  V.find (segmentHasTag tag) (interchangeSegments doc)

requireSegmentByTag :: Text -> Interchange -> Either String Segment
requireSegmentByTag tag doc =
  case firstSegmentByTag tag doc of
    Just seg -> Right seg
    Nothing -> Left ("EDI.Query: missing segment " <> T.unpack tag)

segmentHasTag :: Text -> Segment -> Bool
segmentHasTag tag seg = segmentTag seg == tag

elementAt :: Int -> Segment -> Maybe Element
elementAt ix seg = segmentElements seg V.!? ix

requireElement :: Int -> Segment -> Either String Element
requireElement ix seg =
  case elementAt ix seg of
    Just elemValue -> Right elemValue
    Nothing ->
      Left
        ( "EDI.Query: segment "
            <> T.unpack (segmentTag seg)
            <> " missing element "
            <> show (ix + 1)
        )

elementText :: Element -> Either String Text
elementText (Simple t) = Right t
elementText (Composite _) = Left "EDI.Query: expected simple element"

elementTextAt :: Int -> Segment -> Maybe Text
elementTextAt ix seg =
  case elementAt ix seg of
    Just (Simple t) -> Just t
    _ -> Nothing

requireElementText :: Int -> Segment -> Either String Text
requireElementText ix seg = requireElement ix seg >>= elementText

compositeAt :: Int -> Segment -> Maybe (Vector Text)
compositeAt ix seg =
  case elementAt ix seg of
    Just (Composite parts) -> Just parts
    _ -> Nothing

countSegments :: Text -> Interchange -> Int
countSegments tag = V.length . segmentsByTag tag
