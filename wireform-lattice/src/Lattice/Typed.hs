{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typed rows for backend authors: IDL-conforming Haskell records over the
protocol's dynamic row representation (@Map FieldName Value@).

Loaders written against 'Lattice.Backend.Backend' otherwise traffic in raw
'A.Value' maps and must remember each field's §3.5.3 canonical wire form by
convention. This module (with its codegen half, "Lattice.TH") replaces that
convention with types:

* "Lattice.TH"'s @latticeTypes@ splice generates, from the schema IDL, one
  Haskell type per declared value type (newtypes, enums, sums, records) and
  one record per entity — so a loader author constructs @Post { postTitle =
  …, postRank = … }@ and never sees JSON.
* The generated instances of 'LatticeValue' and 'LatticeEntity' do the
  @Map FieldName Value@ conversion internally, emitting exactly the §3.5.3
  canonical wire forms (wide integers as decimal strings, bytes as unpadded
  base64url, timestamps as RFC 3339 @Z@, sums as @{"$tag": …}@, …).
* 'loaders' adapts a set of per-entity typed loaders into the dynamic
  'Lattice.Backend.beLoad' signature.

== Row shapes: 'Full' and 'Partial'

Entity records are higher-kinded over a row shape @f@:

@
data Post f = Post
  { postId    :: Field f 'False PostId  -- key: required
  , postTitle :: Field f 'False Text    -- required field
  , postTag   :: Field f 'True  Text    -- optional field (@Text?@)
  }
@

* @Post 'Full'@ — every required field present ('Field' reduces to @a@),
  optional fields @Maybe a@. The shape for writes and fully-loaded rows.
* @Post 'Partial'@ — every field @Maybe a@. The shape loaders return:
  'Nothing' means the field is absent from the row — whether because the
  'Projection' did not ask for it or because the row genuinely lacks it.
  Those collapse deliberately: on the wire, omission is the only spelling
  of absence (§4.8 rule 6), so a partial row cannot and need not
  distinguish them.

Backend-private row fields (e.g. edge-backing link lists no query ever
selects) are NOT part of the generated records — they are storage the
backend self-serves (see the /Projections/ contract in "Lattice.Backend").
'Lattice.Backend.Memory.putEntityWith' lets a seeded row carry such
extras alongside its typed fields.

== What generated rows exclude

@on read@ derived fields (never on a row, §3.7) and argument-taking
computed fields (served by 'Lattice.Backend.beComputed') are not row
fields and are omitted from entity records. @maintained@ derived fields
are stored and included.
-}
module Lattice.Typed (
  -- * Row shapes
  Full,
  Partial,
  Field,
  Optionality (..),

  -- * Canonical wire values (§3.5.3)
  LatticeValue (..),
  TextForm (..),
  mapTextForm,

  -- * Entities
  LatticeEntity (..),
  RowError (..),
  fromEntityRow,

  -- * Loaders
  EntityLoader,
  entityLoader,
  Loaded,
  found,
  absent,
  tombstone,
  loadFailed,
  loaders,
  checkLoaderCoverage,

  -- * Support for generated code
  ScalarKeyForm (..),
  scalarKeyFromText,
  requiredField,
  optionalField,
  rowErrorText,
) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64U
import Data.Foldable (toList)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (TimeOfDay)
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Vector qualified as V
import Lattice.Backend (BackendFailure, EntityRow (..), LoadResult (..), Projection, internalError)
import Lattice.Schema (Schema, schemaEntities)
import Lattice.Types (FieldName (..), Ref, TypeName (..), parseRef, renderRef)
import Lattice.Value (canonicalJson)


-- ---------------------------------------------------------------------------
-- Row shapes
-- ---------------------------------------------------------------------------

-- | The row shape of writes and fully-loaded rows: required fields bare.
data Full


-- | The row shape loaders return: every field 'Maybe' (absent = not on the
-- row, whether unfetched or genuinely missing).
data Partial


-- | Whether a field is required or optional in the schema — the middle
-- parameter of 'Field'. A named kind rather than a bare 'Bool', so field
-- declarations read @Field f 'Req a@ \/ @Field f 'Opt a@ instead of
-- @Field f 'True a@ \/ @Field f 'False a@.
data Optionality = Req | Opt


{- | One entity field's type at a row shape @f@. The 'Optionality' is the
field's declared IDL optionality (@t?@ is @''Opt'@), erased into the same
'Maybe' under 'Partial'.

* @Field 'Full' ''Req' a = a@ — required, present on a whole row.
* @Field 'Full' ''Opt' a = Maybe a@ — declared-optional field.
* @Field 'Partial' o a = Maybe a@ — any field of a loaded\/projected row.
-}
type family Field f (o :: Optionality) a where
  Field Full 'Req a = a
  Field Full 'Opt a = Maybe a
  Field Partial o a = Maybe a


-- ---------------------------------------------------------------------------
-- Canonical wire values
-- ---------------------------------------------------------------------------

{- | A bidirectional canonical-text form, for types whose §3.5.3 wire form
is a JSON string ('Text', enums, newtypes of these). Drives two things:
@Map k v@ collapsing to a JSON object (exactly when the key has a
'TextForm'), and entity-key rendering.
-}
data TextForm a = TextForm
  { renderText :: a -> Text
  , parseText :: Text -> Either Text a
  }


-- | Adapt a 'TextForm' through an isomorphism (generated newtypes use this).
mapTextForm :: (a -> b) -> (b -> a) -> TextForm a -> TextForm b
mapTextForm f g (TextForm r p) = TextForm (r . g) (fmap f . p)


{- | A value with a pinned §3.5.3 canonical wire form.

'toWire' MUST produce the canonical form — row values are emitted to
clients as-is, so a non-canonical value is a wire-conformance bug.
'fromWire' accepts the canonical form (and, for the wide integers, a JSON
number leniently).
-}
class LatticeValue a where
  toWire :: a -> A.Value
  fromWire :: A.Value -> Either Text a


  -- | 'Just' exactly when the canonical wire form is a JSON string.
  textForm :: Maybe (TextForm a)
  textForm = Nothing


instance LatticeValue Bool where
  toWire = A.Bool
  fromWire = \case
    A.Bool b -> Right b
    v -> wrong "Bool" v


-- Small integers: JSON numbers (§3.5.3).
instance LatticeValue Int8 where
  toWire = A.Number . fromIntegral
  fromWire = boundedFromNumber "I8"


instance LatticeValue Int16 where
  toWire = A.Number . fromIntegral
  fromWire = boundedFromNumber "I16"


instance LatticeValue Int32 where
  toWire = A.Number . fromIntegral
  fromWire = boundedFromNumber "I32"


instance LatticeValue Word8 where
  toWire = A.Number . fromIntegral
  fromWire = boundedFromNumber "W8"


instance LatticeValue Word16 where
  toWire = A.Number . fromIntegral
  fromWire = boundedFromNumber "W16"


instance LatticeValue Word32 where
  toWire = A.Number . fromIntegral
  fromWire = boundedFromNumber "W32"


-- Wide integers: decimal strings (IEEE-safe range; §3.5.3).
instance LatticeValue Int64 where
  toWire = A.String . tshow
  fromWire = wideFromWire "I64"
  textForm = Just (TextForm tshow (parseSignedDecimal "I64"))


instance LatticeValue Word64 where
  toWire = A.String . tshow
  fromWire = wideFromWire "W64"
  textForm = Just (TextForm tshow (parseSignedDecimal "W64"))


instance LatticeValue Integer where
  toWire = A.String . tshow
  fromWire = wideFromWire "Integer"
  textForm = Just (TextForm tshow (parseSignedDecimal "Integer"))


{- | @Decimal@: normalized decimal string (no exponent, no superfluous
zeros; §3.5.3).
-}
instance LatticeValue Scientific where
  toWire = A.String . decimalText
  fromWire = \case
    A.String t -> parseDecimal t
    A.Number n -> Right n
    v -> wrong "Decimal" v
  textForm = Just (TextForm decimalText parseDecimal)


{- | @F32@\/@F64@: JSON numbers. NaN and infinities are not representable
on the wire (§3.5.3); 'toWire' rejects them with a runtime 'error' naming
the bug (a backend producing NaN has no valid row to emit).
-}
instance LatticeValue Double where
  toWire d
    | isNaN d || isInfinite d = error "Lattice.Typed: NaN/Infinity has no canonical wire form (§3.5.3)"
    | otherwise = A.Number (normalizeZero (Sci.fromFloatDigits d))
  fromWire = \case
    A.Number n -> Right (Sci.toRealFloat n)
    v -> wrong "F64" v


instance LatticeValue Float where
  toWire f
    | isNaN f || isInfinite f = error "Lattice.Typed: NaN/Infinity has no canonical wire form (§3.5.3)"
    | otherwise = A.Number (normalizeZero (Sci.fromFloatDigits f))
  fromWire = \case
    A.Number n -> Right (Sci.toRealFloat n)
    v -> wrong "F32" v


instance LatticeValue Text where
  toWire = A.String
  fromWire = \case
    A.String t -> Right t
    v -> wrong "Text" v
  textForm = Just (TextForm id Right)


-- | @Bytes@: unpadded base64url (§3.5.3).
instance LatticeValue ByteString where
  toWire = A.String . TE.decodeUtf8 . B64U.encodeUnpadded
  fromWire = \case
    A.String t -> either (Left . T.pack) Right (B64U.decodeUnpadded (TE.encodeUtf8 t))
    v -> wrong "Bytes" v
  textForm =
    Just
      ( TextForm
          (TE.decodeUtf8 . B64U.encodeUnpadded)
          (either (Left . T.pack) Right . B64U.decodeUnpadded . TE.encodeUtf8)
      )


-- | @Timestamp@: RFC 3339, UTC @Z@ only, minimal fractional digits (§3.5.3).
instance LatticeValue UTCTime where
  toWire = A.String . T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
  fromWire = \case
    A.String t -> case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack t) of
      Just u -> Right u
      Nothing -> Left ("not an RFC 3339 UTC timestamp: " <> t)
    v -> wrong "Timestamp" v
  textForm =
    Just
      ( TextForm
          (T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ")
          ( \t -> case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack t) of
              Just u -> Right u
              Nothing -> Left ("not an RFC 3339 UTC timestamp: " <> t)
          )
      )


instance LatticeValue Day where
  toWire = A.String . T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
  fromWire = \case
    A.String t -> case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
      Just d -> Right d
      Nothing -> Left ("not an ISO 8601 date: " <> t)
    v -> wrong "Date" v
  textForm =
    Just
      ( TextForm
          (T.pack . formatTime defaultTimeLocale "%Y-%m-%d")
          ( \t -> case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
              Just d -> Right d
              Nothing -> Left ("not an ISO 8601 date: " <> t)
          )
      )


instance LatticeValue TimeOfDay where
  toWire = A.String . T.pack . formatTime defaultTimeLocale "%H:%M:%S%Q"
  fromWire = \case
    A.String t -> case parseTimeM False defaultTimeLocale "%H:%M:%S%Q" (T.unpack t) of
      Just d -> Right d
      Nothing -> Left ("not an ISO 8601 time of day: " <> t)
    v -> wrong "TimeOfDay" v


-- | @Json@: the escape hatch; carried as-is.
instance LatticeValue A.Value where
  toWire = id
  fromWire = Right


-- | @\"Type:key\"@ typed entity references (link fields commonly hold these).
instance LatticeValue Ref where
  toWire = A.String . renderRef
  fromWire = \case
    A.String t -> maybe (Left ("not a Type:key ref: " <> t)) Right (parseRef t)
    v -> wrong "Ref" v
  textForm = Just (TextForm renderRef (\t -> maybe (Left ("not a Type:key ref: " <> t)) Right (parseRef t)))


{- | A NESTED optional (e.g. @[t?]@): 'Nothing' renders as JSON @null@.
Field-level optionality never reaches this instance — absent fields are
omitted from the row entirely (§4.8 rule 6); the generated converters
handle that above the value layer.
-}
instance LatticeValue a => LatticeValue (Maybe a) where
  toWire = maybe A.Null toWire
  fromWire = \case
    A.Null -> Right Nothing
    v -> Just <$> fromWire v


instance LatticeValue a => LatticeValue [a] where
  toWire = A.Array . V.fromList . map toWire
  fromWire = \case
    A.Array xs -> traverse fromWire (toList xs)
    v -> wrong "list" v


instance LatticeValue a => LatticeValue (NonEmpty a) where
  toWire = toWire . NE.toList
  fromWire v =
    fromWire v >>= \case
      [] -> Left "empty array where a nonempty list ([t]+) governs"
      x : xs -> Right (x NE.:| xs)


-- | @Set t@: elements sorted by canonical encoding (§3.5.3).
instance (Ord a, LatticeValue a) => LatticeValue (Set a) where
  toWire = toWire . sortOn (canonicalJson . toWire) . Set.toList
  fromWire v = Set.fromList <$> fromWire v


{- | @Map k v@: a JSON object exactly when the key's canonical form is a
string (the key has a 'TextForm'); otherwise an array of @[k, v]@ pairs
sorted by the key's canonical encoding (§3.5.3).
-}
instance (Ord k, LatticeValue k, LatticeValue v) => LatticeValue (Map k v) where
  toWire m = case textForm of
    Just tf ->
      A.Object . KM.fromList . map (\(k, v) -> (AK.fromText (renderText tf k), toWire v)) $
        Map.toList m
    Nothing ->
      toWire
        ( map (\(k, v) -> [toWire k, toWire v])
            . sortOn (canonicalJson . toWire . fst)
            $ Map.toList m
        )
  fromWire val = case textForm of
    Just tf -> case val of
      A.Object o ->
        Map.fromList
          <$> traverse
            (\(k, v) -> (,) <$> parseText tf (AK.toText k) <*> fromWire v)
            (KM.toList o)
      v -> wrong "map (object form)" v
    Nothing -> case val of
      A.Array _ -> do
        pairs <- fromWire val
        Map.fromList <$> traverse pair pairs
      v -> wrong "map (pair form)" v
    where
      pair = \case
        [k, v] -> (,) <$> fromWire k <*> fromWire v
        _ -> Left "map pair is not a 2-element array"


-- ---------------------------------------------------------------------------
-- Entities
-- ---------------------------------------------------------------------------

-- | A malformed row field: which field, and why.
data RowError = RowError
  { reField :: FieldName
  , reMessage :: Text
  }
  deriving stock (Eq, Show)


{- | An entity type generated by "Lattice.TH": conversion between the typed
records and the protocol's dynamic row fields, plus the key vocabulary.
-}
class Ord (EntityKey e) => LatticeEntity e where
  -- | The typed key: the key field's type, or a generated record for
  -- composite keys.
  type EntityKey e


  entityName :: Proxy e -> TypeName


  -- | The key spec (@by …@), for diagnostics and validation.
  entityKeyFields :: Proxy e -> NonEmpty FieldName


  -- | The wire key text: canonical form(s) of the key field(s), composite
  -- components joined with @\",\"@ (matching 'Lattice.Types.Ref').
  renderEntityKey :: Proxy e -> EntityKey e -> Text


  parseEntityKey :: Proxy e -> Text -> Maybe (EntityKey e)


  -- | A full row always carries its key.
  fullRowKey :: e Full -> EntityKey e


  toRowFields :: e Full -> Map FieldName A.Value


  toPartialRowFields :: e Partial -> Map FieldName A.Value


  fromRowFields :: Map FieldName A.Value -> Either RowError (e Full)


  fromPartialRowFields :: Map FieldName A.Value -> Either RowError (e Partial)


-- | Read an 'EntityRow' (e.g. a 'Lattice.Backend.beChildren' parent) into
-- typed partial fields. The version stays on the 'EntityRow' the caller
-- already holds.
fromEntityRow :: LatticeEntity e => EntityRow -> Either RowError (e Partial)
fromEntityRow (EntityRow _ fs) = fromPartialRowFields fs


-- ---------------------------------------------------------------------------
-- Loaders
-- ---------------------------------------------------------------------------

{- | What one key loaded to. Build it with 'found', 'absent', 'tombstone',
or 'loadFailed' — never a raw constructor. A key a loader omits from its
result map is treated as 'absent'.
-}
data Loaded e
  = Found Text (e Partial)
  | Absent
  | Tombstone Text
  | LoadFailed BackendFailure


-- | A row: its version token and its (possibly projected) typed fields.
found :: Text -> e Partial -> Loaded e
found = Found


-- | No such row — the same fact as a key that never existed.
absent :: Loaded e
absent = Absent


-- | The row was deleted; the tombstone version (@t:\<n\>@) is emitted.
tombstone :: Text -> Loaded e
tombstone = Tombstone


-- | This key's load failed (a scoped 'BackendFailure', §9.4.2).
loadFailed :: BackendFailure -> Loaded e
loadFailed = LoadFailed


-- | One entity's batched loader, existentially packaged for dispatch.
data EntityLoader
  = forall e.
    LatticeEntity e =>
    EntityLoader
      (Proxy e)
      (Projection -> [EntityKey e] -> IO (Map (EntityKey e) (Loaded e)))


{- | Package one entity's loader. The loader is batched exactly like
'Lattice.Backend.beLoad' (set-in, map-out) and receives the plan's
requested 'Projection'; it may ignore it (over-fetch is always correct)
or narrow its storage read to the projected fields. It returns typed
rows keyed by the entity's key type, built with 'found' \/ 'absent' \/
'tombstone' \/ 'loadFailed'.
-}
entityLoader ::
  forall e.
  LatticeEntity e =>
  (Projection -> [EntityKey e] -> IO (Map (EntityKey e) (Loaded e))) ->
  EntityLoader
entityLoader = EntityLoader (Proxy @e)


{- | Combine per-entity loaders into a 'Lattice.Backend.beLoad'; the
@Map FieldName Value@ representation stays internal.

> backend = someBackend { beLoad = loaders [postLoader, authorLoader] }

* A type with no registered loader fails its whole batch with
  @lattice:internal@ (a wiring bug, surfaced loudly as Entity-scoped
  errors — pair with 'checkLoaderCoverage' at startup).
* A key that does not parse as the entity's key type names nothing and
  loads 'absent' (indistinguishable from nonexistence, which is what a
  structurally impossible key is).
* Keys the loader omits from its result map are 'absent' too.
-}
loaders ::
  [EntityLoader] ->
  TypeName ->
  Projection ->
  [Text] ->
  IO (Map Text (Either BackendFailure LoadResult))
loaders ls = \ty proj keys -> case Map.lookup ty table of
  Nothing ->
    pure . Map.fromList $
      map (\k -> (k, Left (internalError (Just ("no loader for entity type " <> unTypeName ty))))) keys
  Just run -> run proj keys
  where
    table = Map.fromList (map entry ls)
    entry (EntityLoader (p :: Proxy e) load) = (entityName p, run)
      where
        run proj keys = do
          let parsed = map (\k -> (k, parseEntityKey p k)) keys
              wanted = mapMaybe snd parsed
          results <- if null wanted then pure Map.empty else load proj wanted
          pure . Map.fromList $
            concatMap
              ( \(rawKey, mtk) -> case mtk of
                  Nothing -> [(rawKey, Right RowAbsent)]
                  Just tk -> case Map.lookup tk results of
                    Nothing -> []
                    Just res -> [(rawKey, resolve res)]
              )
              parsed
        resolve :: Loaded e -> Either BackendFailure LoadResult
        resolve = \case
          Found ver fs -> Right (RowFound (EntityRow ver (toPartialRowFields fs)))
          Absent -> Right RowAbsent
          Tombstone v -> Right (RowTombstone v)
          LoadFailed bf -> Left bf


{- | Startup completeness: every entity the schema declares has a loader.
'Left' names the missing types. Run this once when wiring the
'Lattice.Backend.Backend'; 'loaders' itself degrades per batch.
-}
checkLoaderCoverage :: Schema -> [EntityLoader] -> Either [TypeName] ()
checkLoaderCoverage schema ls =
  case filter (`Set.notMember` provided) (Map.keys (schemaEntities schema)) of
    [] -> Right ()
    missing -> Left missing
  where
    provided = Set.fromList (map (\(EntityLoader p _) -> entityName p) ls)


-- ---------------------------------------------------------------------------
-- Support for generated code
-- ---------------------------------------------------------------------------

{- | How an entity-key component round-trips through the wire key text
(chosen at codegen time from the key field's resolved primitive): the key
text is 'Lattice.Value.renderScalarKey' of the canonical wire form, so
string-formed types embed directly, number-formed parse back through
'A.Number', booleans through @true@\/@false@.
-}
data ScalarKeyForm = KeyText | KeyNumber | KeyBool
  deriving stock (Eq, Show)


-- | Reconstruct the wire form of one key component from its key text.
scalarKeyFromText :: ScalarKeyForm -> Text -> Maybe A.Value
scalarKeyFromText form t = case form of
  KeyText -> Just (A.String t)
  KeyNumber -> case TR.signed TR.rational (T.strip t) of
    Right (d :: Double, rest) | T.null rest -> Just (A.Number (Sci.fromFloatDigits d))
    _ -> Nothing
  KeyBool -> case t of
    "true" -> Just (A.Bool True)
    "false" -> Just (A.Bool False)
    _ -> Nothing


-- | Decode a required row field (generated @fromRowFields@ bodies).
requiredField :: LatticeValue a => FieldName -> Map FieldName A.Value -> Either RowError a
requiredField fn m = case Map.lookup fn m of
  Nothing -> Left (RowError fn "required field absent from row")
  Just v -> either (Left . RowError fn) Right (fromWire v)


-- | Decode an optional (or partial) row field.
optionalField :: LatticeValue a => FieldName -> Map FieldName A.Value -> Either RowError (Maybe a)
optionalField fn m = case Map.lookup fn m of
  Nothing -> Right Nothing
  Just v -> either (Left . RowError fn) (Right . Just) (fromWire v)


-- | Render a 'RowError' as the value-level error text (generated
-- record\/sum decoders flatten nested field errors through this).
rowErrorText :: RowError -> Text
rowErrorText (RowError f m) = unFieldName f <> ": " <> m


-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

wrong :: Text -> A.Value -> Either Text a
wrong what v = Left ("not a canonical " <> what <> ": " <> T.pack (constrOf v))
  where
    constrOf = \case
      A.Object _ -> "object"
      A.Array _ -> "array"
      A.String _ -> "string"
      A.Number _ -> "number"
      A.Bool _ -> "bool"
      A.Null -> "null"


tshow :: Show a => a -> Text
tshow = T.pack . show


boundedFromNumber :: forall a. (Integral a, Bounded a) => Text -> A.Value -> Either Text a
boundedFromNumber what = \case
  A.Number n -> case Sci.toBoundedInteger n of
    Just i -> Right i
    Nothing -> Left (what <> " out of range or not integral")
  v -> wrong what v


wideFromWire :: Integral a => Text -> A.Value -> Either Text a
wideFromWire what = \case
  A.String t -> parseSignedDecimal what t
  -- Lenient read: a JSON number in the safe range is unambiguous.
  A.Number n -> case Sci.floatingOrInteger @Double n of
    Right i -> Right (fromInteger i)
    Left _ -> Left (what <> " is not integral")
  v -> wrong what v


parseSignedDecimal :: Integral a => Text -> Text -> Either Text a
parseSignedDecimal what t = case TR.signed TR.decimal t of
  Right (i, rest) | T.null rest -> Right i
  _ -> Left ("not a decimal " <> what <> " string: " <> t)


-- | Normalized decimal text: no exponent, no superfluous zeros (§3.5.3).
decimalText :: Scientific -> Text
decimalText n =
  let full = T.pack (Sci.formatScientific Sci.Fixed Nothing n)
  in if T.any (== '.') full
      then
        let trimmed = T.dropWhileEnd (== '0') full
        in if T.last trimmed == '.' then T.init trimmed else trimmed
      else full


parseDecimal :: Text -> Either Text Scientific
parseDecimal t = case TR.signed TR.rational t of
  Right (n, rest) | T.null rest -> Right n
  _ -> Left ("not a decimal string: " <> t)


-- | @-0@ normalizes to @0@ (§3.5.3).
normalizeZero :: Scientific -> Scientific
normalizeZero n = if n == 0 then 0 else n
