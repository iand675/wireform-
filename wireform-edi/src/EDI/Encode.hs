-- | EDI text encoding.
module EDI.Encode
  ( encode
  , encodeWithSyntax
  , encodeBS
  , buildInterchange
  , buildSegment
  , buildElement
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Vector qualified as V

import EDI.Value

-- | Render an interchange to 'Text'.
encode :: Interchange -> Text
encode = toStrict . toLazyText . buildInterchange

-- | Render segments using a supplied syntax.
encodeWithSyntax :: Syntax -> V.Vector Segment -> Text
encodeWithSyntax syn segments =
  encode (Interchange syn segments)

-- | Render an interchange as UTF-8 bytes.
encodeBS :: Interchange -> ByteString
encodeBS = TE.encodeUtf8 . encode

buildInterchange :: Interchange -> Builder
buildInterchange doc =
  let syn = interchangeSyntax doc
  in V.foldMap (buildSegment syn) (interchangeSegments doc)

buildSegment :: Syntax -> Segment -> Builder
buildSegment syn seg =
  fromText (segmentTag seg)
    <> buildElements syn (segmentElements seg)
    <> singleton (segmentTerminator syn)

buildElements :: Syntax -> V.Vector Element -> Builder
buildElements syn elems =
  V.foldMap
    (\elemValue -> singleton (elementSeparator syn) <> buildElement syn elemValue)
    elems

buildElement :: Syntax -> Element -> Builder
buildElement _ (Simple t) = fromText t
buildElement syn (Composite parts) =
  V.ifoldl'
    ( \acc i part ->
        if i == 0
          then fromText part
          else acc <> singleton (componentSeparator syn) <> fromText part
    )
    mempty
    parts
