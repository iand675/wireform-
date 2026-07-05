{- | Query-text compression for URL embedding (spec §5.2): raw DEFLATE at a
pinned parameterization (level 9, 32 KiB window), optionally primed with a
schema-derived shared dictionary.

@dv@ MAY be omitted on inline-form URLs, meaning no dictionary — required
for encoders without dictionary support (browser @CompressionStream@); an
implementation-driven clarification folded back into the spec.

The dictionary is a deterministic function of the schema: the sorted
identifiers (type, field, root, fragment names) joined by single spaces,
followed by common query syntax tokens. Dictionaries are content-addressed
by 'Lattice.Hash.dictHash' and immutable.
-}
module Lattice.Compress (
  Dictionary,
  schemaDictionary,
  compressQuery,
  decompressQuery,
) where

import Codec.Compression.Zlib.Internal qualified as Z
import Codec.Compression.Zlib.Raw qualified as Raw
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lattice.Schema
import Lattice.Types


type Dictionary = ByteString


{- | Deterministic schema-derived dictionary (§5.2).

Exact construction: the set of every type, interface, entity, field (and
relationship), root, schema-fragment, mutation, and claim name in the
schema, deduplicated and sorted ascending by code point, joined with
single spaces, followed by one more space and the literal syntax primer
@"query fragment on true false first after last before around \@depth($:){}"@.
The primer sits last because DEFLATE prefers recent dictionary bytes and
the primer tokens appear in every query. UTF-8 encoded (names are ASCII
by the grammars). Content-addressed by 'Lattice.Hash.dictHash'; immutable.
-}
schemaDictionary :: Schema -> Dictionary
schemaDictionary Schema {..} =
  TE.encodeUtf8 (T.intercalate " " (Set.toAscList names <> [primer]))
  where
    primer :: Text
    primer = "query fragment on true false first after last before around @depth($:){}"
    names =
      Set.unions
        [ keySet unTypeName schemaTypes
        , keySet unInterfaceName schemaInterfaces
        , keySet unTypeName schemaEntities
        , keySet unFragmentName schemaFragments
        , keySet unRootName schemaRoots
        , keySet unMutationName schemaMutations
        , keySet unClaimName schemaClaims
        , foldMap entityFieldNames (Map.elems schemaEntities)
        , foldMap ifaceFieldNames (Map.elems schemaInterfaces)
        ]
    keySet :: (k -> Text) -> Map k v -> Set Text
    keySet f = Set.fromList . map f . Map.keys
    entityFieldNames ed =
      keySet unFieldName (entityFields ed) <> keySet unFieldName (entityRels ed)
    ifaceFieldNames idf =
      keySet unFieldName (ifaceFields idf) <> keySet unFieldName (ifaceRels idf)


compressQuery :: Maybe Dictionary -> Text -> ByteString
compressQuery mDict t =
  BL.toStrict $
    Raw.compressWith
      Raw.defaultCompressParams
        { Z.compressLevel = Z.bestCompression
        , Z.compressDictionary = mDict
        }
      (BL.fromStrict (TE.encodeUtf8 t))


{- | Inverse of 'compressQuery'.

zlib (the C library and this binding) only installs an inflate dictionary
on the zlib-format @Z_NEED_DICT@ signal, which raw streams never emit, so
naive raw inflation of a dictionary-compressed stream fails with
"invalid distance too far back". Instead the dictionary is spliced in as
deflate itself would have seen it: prepend non-final /stored/ blocks
containing the dictionary bytes (stored blocks are byte-aligned, so plain
concatenation with the real stream is a valid raw deflate stream), inflate
without any dictionary — the window primes itself by decompressing the
prefix — and drop the dictionary-length prefix from the output. The wire
bytes remain exactly the pinned raw-DEFLATE parameterization (§5.2).
-}
decompressQuery :: Maybe Dictionary -> ByteString -> Either Text Text
decompressQuery mDict bs =
  case Z.foldDecompressStreamWithInput
    (\chunk rest -> (BL.fromStrict chunk <>) <$> rest)
    (\_leftover -> Right BL.empty)
    (Left . T.pack . show)
    (Z.decompressST Z.rawFormat Z.defaultDecompressParams)
    (BL.fromStrict input) of
    Left e -> Left e
    Right out ->
      either
        (Left . T.pack . show)
        Right
        (TE.decodeUtf8' (BL.toStrict (BL.drop prefixLen out)))
  where
    input = case mDict of
      Nothing -> bs
      Just dict -> storedBlocks dict <> bs
    prefixLen = case mDict of
      Nothing -> 0
      Just dict -> fromIntegral (BS.length dict)


{- | Encode bytes as non-final stored deflate blocks (BFINAL=0, BTYPE=00):
one byte of zero header bits, then little-endian LEN and NLEN (= one's
complement of LEN), then the raw bytes; at most 65535 bytes per block.
-}
storedBlocks :: ByteString -> ByteString
storedBlocks = BS.concat . map block . chunksOf
  where
    chunksOf b
      | BS.null b = []
      | otherwise =
          let (c, rest) = BS.splitAt 65535 b
           in c : chunksOf rest
    block c =
      let len = BS.length c
          nlen = 65535 - len
          lo n = fromIntegral (n `mod` 256)
          hi n = fromIntegral (n `div` 256)
       in BS.pack [0x00, lo len, hi len, lo nlen, hi nlen] <> c
