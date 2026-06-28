{- |
Backwards-compatible re-export of the general HTTP wire-grammar
renderers, plus the one header-/policy/-specific renderer that does not
belong in the grammar substrate.

The general RFC 8941 renderers now live in
"Network.HTTP.Grammar.Rendering"; new code should import from there
directly. What stays here is 'fitHeaderSplitsToCommas', which depends on
'HeaderSettings' and 'HeaderFieldName' and is therefore genuinely
header-level (it is about fitting a rendered value into framed header
lines, not about the wire grammar of the value itself).
-}
module Network.HTTP.Headers.Rendering.Util (
  module Network.HTTP.Grammar.Rendering,
  fitHeaderSplitsToCommas,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.HTTP.Grammar.Rendering
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, toText)
import Network.HTTP.Headers.Settings (HeaderSettings, maxHeaderSize)


{- | Given maximum header size, a field name, and a field value, split the field value into a list of field values
that when emitted will each fit within the maximum header size when combined with the field name, and separated by a colon.

This function is best effort: if a comma-separated value is too large to fit within the maximum header size, it will be emitted as a singleton list.

If the field value is already small enough to fit within the maximum header size, it will be returned as a singleton list.

Multiple message-header fields with the same field-name MAY be present in a message if and only if the entire field-value
for that header field is defined as a comma-separated list [i.e., #(values)]. It MUST be possible to combine the
multiple header fields into one "field-name: field-value" pair, without changing the semantics of the message, by appending each subsequent field-value to the first,
each separated by a comma. The order in which header fields with the same field-name are received is therefore significant
to the interpretation of the combined field value, and thus a proxy MUST NOT change the order of these field values when a message is forwarded

The one exception to this rule is the "Set-Cookie" header field, which does not follow the list construct and SHOULD be handled as a
special case.
-}
fitHeaderSplitsToCommas :: HeaderSettings -> HeaderFieldName -> ByteString -> [ByteString]
fitHeaderSplitsToCommas hsettings hfield bs = go bs
  where
    chunkSize = maxHeaderSize hsettings - T.length (toText hfield) - 1
    go headerContents =
      if B.length headerContents <= chunkSize
        then [headerContents]
        else
          let (chunk, rest) = B.splitAt chunkSize headerContents
              lastFittingComma = B.findIndexEnd (== 0x2C {- comma -}) chunk
              -- If there is no comma in the chunk, we can't split it, so we find the first comma we can and split there.
              firstFittingComma = case lastFittingComma of
                Nothing -> case B.findIndex (== 0x2C {- comma -}) bs of
                  Nothing -> [bs] -- No comma in the whole header, so we can't split it.
                  Just i -> B.take (i - 1) bs : go (B.drop (i + 1) bs)
                Just i -> B.take (i - 1) bs : go (B.drop (i + 1) bs)
          in firstFittingComma
