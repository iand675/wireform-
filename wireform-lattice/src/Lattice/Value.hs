{- | Canonical JSON and canonical wire forms (spec §3.5.3).

Lattice pins one deterministic rendering per value so that etags, cursors,
claims payloads, and URL-bound variables hash identically across
implementations:

* Object keys sort by Unicode code point.
* No insignificant whitespace.
* Integral numbers inside the IEEE-safe range render as plain JSON numbers;
  @I64@\/@W64@\/@Integer@\/@Decimal@ render as decimal strings at their
  declaring sites (schema-directed, not value-directed).
* Doubles render shortest-round-trip; @NaN@\/@Infinity@ are unrepresentable;
  @-0@ normalizes to @0@.
-}
module Lattice.Value (
  canonicalJson,
  canonicalJsonText,
  valueToUrlParam,
  urlParamToValue,
  qvalueToJson,
  jsonToQValue,
  renderScalarKey,
) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Lattice.Query.AST (QValue (..))


-- | Deterministic JSON bytes: sorted keys, minimal whitespace, pinned numbers.
canonicalJson :: A.Value -> ByteString
canonicalJson = BL.toStrict . BB.toLazyByteString . go
  where
    go :: A.Value -> BB.Builder
    go = \case
      A.Null -> "null"
      A.Bool True -> "true"
      A.Bool False -> "false"
      A.Number n -> BB.byteString (renderNumber n)
      A.String t -> string t
      A.Array xs -> "[" <> commas (map go (V.toList xs)) <> "]"
      A.Object o ->
        "{"
          <> commas
            [ string (AK.toText k) <> ":" <> go v
            | (k, v) <- sortOn (AK.toText . fst) (KM.toList o)
            ]
          <> "}"
    commas [] = mempty
    commas [x] = x
    commas (x : xs) = x <> "," <> commas xs
    string t = BB.lazyByteString (A.encode (A.String t))


canonicalJsonText :: A.Value -> Text
canonicalJsonText = TE.decodeUtf8 . canonicalJson


{- | Pinned number rendering: integral 'Scientific's inside the IEEE-safe
range render without exponent or fraction; everything else renders via
the shortest scientific form with a lowercase @e@.
-}
renderNumber :: Scientific -> ByteString
renderNumber n
  | Just i <- toSafeInt = BL.toStrict (BB.toLazyByteString (BB.integerDec i))
  | otherwise =
      -- Shortest form via scientific's standard rendering.
      TE.encodeUtf8 (T.pack (Sci.formatScientific Sci.Generic Nothing normalized))
  where
    normalized = Sci.normalize (if n == 0 then 0 else n)
    toSafeInt = case Sci.floatingOrInteger @Double normalized of
      Right i | abs i <= 9007199254740991 -> Just i
      _ -> Nothing


{- | Bind a value into a URL query parameter (§3.5.3): strings raw, scalars
as their canonical literal text, composite values base64url of their
canonical JSON, prefixed @"b64:"@ so the two encodings never collide.
Percent-encoding is the transport layer's job, not ours.
-}
valueToUrlParam :: A.Value -> Text
valueToUrlParam = \case
  A.String t -> t
  A.Bool True -> "true"
  A.Bool False -> "false"
  A.Number n -> TE.decodeUtf8 (renderNumber n)
  v -> "b64:" <> TE.decodeUtf8 (B64U.encodeUnpadded (canonicalJson v))


{- | Inverse of 'valueToUrlParam' for a parameter expected to have the given
shape. @expectString@ callers (Text\/Uuid\/enums\/cursors) take the text
verbatim; otherwise numbers and booleans are recognized, and @b64:@
payloads decode as canonical JSON.
-}
urlParamToValue ::
  -- | Treat as a string type (no literal interpretation)?
  Bool ->
  Text ->
  Either Text A.Value
urlParamToValue expectString t
  | Just b64 <- T.stripPrefix "b64:" t =
      case B64U.decodeUnpadded (TE.encodeUtf8 b64) of
        Left e -> Left (T.pack e)
        Right bs -> maybe (Left "invalid b64 JSON payload") Right (A.decodeStrict bs)
  | expectString = Right (A.String t)
  | t == "true" = Right (A.Bool True)
  | t == "false" = Right (A.Bool False)
  | Right (n, rest) <- TR.signed TR.rational t
  , T.null rest =
      Right (A.Number n)
  | otherwise = Right (A.String t)


-- | Query-language literal to JSON (enums render as bare strings).
qvalueToJson :: QValue -> Maybe A.Value
qvalueToJson = \case
  QVar _ -> Nothing
  QInt i -> Just (A.Number (fromInteger i))
  QNum s -> Just (A.Number s)
  QString t -> Just (A.String t)
  QBool b -> Just (A.Bool b)
  QEnum e -> Just (A.String e)
  QList vs -> A.Array . V.fromList <$> traverse qvalueToJson vs


-- | JSON to a query-language literal (for default-erasure comparison).
jsonToQValue :: A.Value -> Maybe QValue
jsonToQValue = \case
  A.String t -> Just (QString t)
  A.Bool b -> Just (QBool b)
  A.Number n -> Just $ case Sci.floatingOrInteger @Double n of
    Right i -> QInt i
    Left _ -> QNum n
  A.Array vs -> QList <$> traverse jsonToQValue (V.toList vs)
  _ -> Nothing


{- | Render a scalar as key text for a 'Lattice.Types.Ref' (the part after
@:@). Composite keys join with @,@ at the call site.
-}
renderScalarKey :: A.Value -> Text
renderScalarKey = \case
  A.String t -> t
  A.Number n -> TE.decodeUtf8 (renderNumber n)
  A.Bool True -> "true"
  A.Bool False -> "false"
  v -> canonicalJsonText v
