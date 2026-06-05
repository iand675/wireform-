-- | Builder wrapper used by EDI encoders.
module EDI.Encoding
  ( Encoding(..)
  , encodingToText
  , text
  , char
  , element
  , segment
  , interchange
  ) where

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)

import EDI.Value (Element, Interchange, Segment, Syntax)
import qualified EDI.Encode as Encode

newtype Encoding = Encoding { getEncoding :: Builder }
  deriving newtype (Semigroup, Monoid)

encodingToText :: Encoding -> Text
encodingToText = toStrict . toLazyText . getEncoding

text :: Text -> Encoding
text = Encoding . fromText

char :: Char -> Encoding
char = Encoding . singleton

element :: Syntax -> Element -> Encoding
element syn = Encoding . Encode.buildElement syn

segment :: Syntax -> Segment -> Encoding
segment syn = Encoding . Encode.buildSegment syn

interchange :: Interchange -> Encoding
interchange = Encoding . Encode.buildInterchange
