{- | Cursor encoding (spec §3.2).

A cursor is deterministic, session-free, and derivable by any holder of the
keyset column values. The pinned layout (an implementation-driven
clarification folded back into the spec):

@
cursor = "cur_" <> base64url(canonicalJson([specHashB64, v1, …, vN]))
@

where @specHashB64@ is the base64url of the first 4 bytes of
@BLAKE3(canonical CursorSpec rendering)@ and @v1…vN@ are the item's keyset
column values in canonical wire form. Presenting a cursor whose embedded
spec hash does not match the collection's current 'CursorSpec' is rejected
@410 lattice:cursor-retired@.
-}
module Lattice.Cursor (
  Cursor (..),
  specHashOf,
  encodeCursor,
  decodeCursor,
  CursorError (..),
) where

import Data.Aeson qualified as A
import Data.ByteString.Base64.URL qualified as B64U
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Lattice.Hash (b64url, cursorSpecHash)
import Lattice.Schema (CursorSpec (..), Direction (..))
import Lattice.Types (FieldName (..))
import Lattice.Value (canonicalJson)


-- | A decoded cursor: the generating spec's hash and the keyset column values.
data Cursor = Cursor
  { curSpec :: Text
  -- ^ base64url of the 4-byte spec hash.
  , curValues :: [A.Value]
  }
  deriving stock (Eq, Show)


data CursorError
  = CursorMalformed
  | -- | Spec hash mismatch: the pagination spec changed (§17.2).
    CursorRetired
  deriving stock (Eq, Show)


{- | The canonical rendering of a 'CursorSpec' that feeds the embedded hash:
@field dir,field dir,…|pageDefault|maxPage@.
-}
specHashOf :: CursorSpec -> Text
specHashOf CursorSpec {..} =
  b64url . cursorSpecHash $
    T.intercalate
      ","
      [ unFieldName f <> " " <> renderDir d
      | (f, d) <- NE.toList csKeyset
      ]
  where
    renderDir Asc = "asc"
    renderDir Desc = "desc"


encodeCursor :: CursorSpec -> [A.Value] -> Text
encodeCursor spec vals =
  "cur_"
    <> TE.decodeUtf8
      ( B64U.encodeUnpadded
          (canonicalJson (A.Array (V.fromList (A.String (specHashOf spec) : vals))))
      )


decodeCursor :: CursorSpec -> Text -> Either CursorError Cursor
decodeCursor spec t = do
  body <- maybe (Left CursorMalformed) Right (T.stripPrefix "cur_" t)
  bs <- either (const (Left CursorMalformed)) Right (B64U.decodeUnpadded (TE.encodeUtf8 body))
  arr <- maybe (Left CursorMalformed) Right (A.decodeStrict bs)
  case arr of
    A.Array v | Just (A.String h) <- v V.!? 0 -> do
      let cur = Cursor h (drop 1 (V.toList v))
      if h == specHashOf spec
        then Right cur
        else Left CursorRetired
    _ -> Left CursorMalformed
