{-# LANGUAGE DataKinds #-}
{- | Content-address hashing for the Lattice protocol.

Every content address in Lattice is a truncation of a BLAKE3 digest,
rendered as unpadded base64url (RFC 4648 §5). The truncation lengths are
pinned here so that every implementation agrees byte-for-byte:

* 'queryHash' — 16 bytes (128 bits) of @BLAKE3(canonical query text)@,
  22 base64url characters. This is the @{queryHash}@ in @/q/{queryHash}@.
* 'schemaHash' — 16 bytes of @BLAKE3(canonical IDL text)@, prefixed @"s"@.
* 'planId' — 12 bytes (96 bits) of @BLAKE3(canonical text <> 0x00 <> pertinent
  declarations)@, prefixed @"pl_"@.
* 'manifestEtag' — 12 bytes of BLAKE3 over the manifest input, prefixed @"m:"@.
* 'dictHash' — 16 bytes of @BLAKE3(dictionary bytes)@.
-}
module Lattice.Hash (
  blake3,
  b64url,
  queryHash,
  schemaHash,
  planIdHash,
  manifestEtagHash,
  dictHash,
  cursorSpecHash,
) where

import BLAKE3 qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Internal qualified as BSI
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke)


{- | Full 32-byte BLAKE3 digest of the input.

blake3-0.3's 'BLAKE3.Digest' lives in the @ram@ byte-array class hierarchy
(a @memory@ fork with colliding module names), so we extract the bytes
through the 'Foreign.Storable' instance instead of a class conversion.
-}
blake3 :: ByteString -> ByteString
blake3 bs = BSI.unsafeCreate 32 $ \p -> poke (castPtr p) digest
  where
    digest = BLAKE3.hash @32 Nothing [bs] :: BLAKE3.Digest 32


-- | Unpadded base64url.
b64url :: ByteString -> Text
b64url = TE.decodeUtf8 . B64U.encodeUnpadded


-- | 128-bit truncated BLAKE3 of the canonical query text, base64url.
queryHash :: Text -> Text
queryHash = b64url . BS.take 16 . blake3 . TE.encodeUtf8


-- | Content address of a canonical IDL document: @s@ + 128-bit BLAKE3.
schemaHash :: Text -> Text
schemaHash t = "s" <> b64url (BS.take 16 (blake3 (TE.encodeUtf8 t)))


{- | Plan id over the canonical query text and the canonical rendering of
the query's pertinent schema declarations: @pl_@ + 96-bit BLAKE3.
-}
planIdHash :: Text -> Text -> Text
planIdHash canonicalText pertinent =
  "pl_"
    <> b64url
      (BS.take 12 (blake3 (TE.encodeUtf8 canonicalText <> BS.singleton 0 <> TE.encodeUtf8 pertinent)))


-- | Weak manifest etag body: @m:@ + 96-bit BLAKE3 of the manifest input.
manifestEtagHash :: ByteString -> Text
manifestEtagHash bs = "m:" <> b64url (BS.take 12 (blake3 bs))


-- | Content address of a compression dictionary (16-byte truncation).
dictHash :: ByteString -> Text
dictHash = b64url . BS.take 16 . blake3


-- | 4-byte hash of a canonical 'CursorSpec' rendering, embedded in cursors.
cursorSpecHash :: Text -> ByteString
cursorSpecHash = BS.take 4 . blake3 . TE.encodeUtf8
