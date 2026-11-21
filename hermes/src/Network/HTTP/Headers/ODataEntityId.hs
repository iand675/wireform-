{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

{- |
@OData-EntityId@ — a response header returned for a create or upsert request
that completes with @204 No Content@, carrying the entity-id (an IRI) of the
affected entity so the client can reference it without a representation body.

== Grammar

@
OData-EntityId = IRI-reference ; nonempty
@

Absolute and relative references follow RFC 3987. Parsing preserves their
spelling. Rendering maps raw Unicode characters to UTF-8 percent escapes,
as required for URI wire output. Existing escapes retain their spelling.

Spec: OData Version 4.01 Part 1: Protocol §4.1 and §8.3.4
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataEntityId>

See also: "Network.HTTP.Headers.ODataVersion", "Network.HTTP.Headers.ODataMaxVersion",
"Network.HTTP.Headers.PreferenceApplied", "Network.HTTP.Headers.Location".
-}
module Network.HTTP.Headers.ODataEntityId (
  ODataEntityId (..),
  oDataEntityIdParser,
  renderODataEntityId,
) where

import Data.Bits (popCount, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (Int (..), indexWord8ArrayAsWord64#)
import GHC.Word (Word64 (W64#))
import Network.HTTP.Grammar.URIReference (checkedURIField, iriReference, iriReferenceBytes, outboundURIReferenceBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataEntityId)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The entity-id (an IRI) carried by an @OData-EntityId@ response header.
newtype ODataEntityId = ODataEntityId {oDataEntityId :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ODataEntityId where
  type ParseFailure ODataEntityId = String
  type Cardinality ODataEntityId = 'ZeroOrOne
  type Direction ODataEntityId = 'Response


  parseFromHeaders _ = parseSingleFieldLine "OData-EntityId" oDataEntityIdParser


  renderToHeaders _ = renderEntityIdWith SBS.fromShort id . oDataEntityId


  renderToHeadersChecked _ (ODataEntityId iri)
    | nonAscii == 0 = checkedURIField outboundReference wire
    | otherwise = do
        _ <- checkedURIField (nonEmptyReference iriReferenceBytes) wire
        checkedURIField outboundReference (escapeUnicode bytes nonAscii)
    where
      bytes = ST.toShortByteString iri
      wire = SBS.fromShort bytes
      nonAscii = countNonAscii bytes
      outboundReference = nonEmptyReference outboundURIReferenceBytes
      nonEmptyReference syntax = withSpan syntax $ \() (Span start end) ->
        if start == end then failed else pure ()


  headerName _ = hODataEntityId


oDataEntityIdParser :: ParserT st String ODataEntityId
oDataEntityIdParser = withSpan iriReference $ \value (Span start end) ->
  if start == end then failed else pure (ODataEntityId value)


renderODataEntityId :: ODataEntityId -> M.Builder
renderODataEntityId = renderEntityIdWith M.shortByteString M.byteString . oDataEntityId


renderEntityIdWith :: (SBS.ShortByteString -> a) -> (ByteString -> a) -> ST.ShortText -> a
renderEntityIdWith ascii encoded iri
  | nonAscii == 0 = ascii bytes
  | otherwise = encoded (escapeUnicode bytes nonAscii)
  where
    bytes = ST.toShortByteString iri
    nonAscii = countNonAscii bytes
{-# INLINE renderEntityIdWith #-}


-- Test the high bit in each byte. Full words are read only within the array.
countNonAscii :: SBS.ShortByteString -> Int
countNonAscii bytes@(SBSI.SBS array) = go 0 0
  where
    size = SBS.length bytes
    go !offset !count
      | offset <= size - 8 =
          case offset of
            I# i ->
              let word = W64# (indexWord8ArrayAsWord64# array i)
              in go (offset + 8) (count + popCount (word .&. 0x8080808080808080))
      | offset < size =
          go (offset + 1) (count + fromEnum (SBSI.unsafeIndex bytes offset >= 0x80))
      | otherwise = count


escapeUnicode :: SBS.ShortByteString -> Int -> ByteString
escapeUnicode bytes nonAscii = BSI.unsafeCreate (size + 2 * nonAscii) $ \target -> do
  let copy !source !dest
        | source == size = pure ()
        | otherwise = do
            let byte = SBSI.unsafeIndex bytes source
            if byte < 0x80
              then pokeByteOff target dest byte *> copy (source + 1) (dest + 1)
              else do
                pokeByteOff target dest (0x25 :: Word8)
                pokeByteOff target (dest + 1) (hex (byte `shiftR` 4))
                pokeByteOff target (dest + 2) (hex (byte .&. 15))
                copy (source + 1) (dest + 3)
  copy 0 0
  where
    size = SBS.length bytes
    hex digit = if digit < 10 then 48 + digit else 55 + digit
