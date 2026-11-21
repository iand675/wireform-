{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Network.HTTP.Headers (
  KnownHeader (..),
  HeaderCardinality (..),
  HeaderIsRequestOrResponse (..),
  HeaderMap,
  headerMapToMap,
  headerMapFromList,
  headerMapToList,
  lookupHeader,
  lookupHeaderWith,
  deleteHeader,
  lookupRawHeader,
  insertRawHeader,
  alterRawHeader,
  renderHeaderLines,
  setHeader,
  setRawHeader,
  HeaderSettings,
  HeaderMessage (..),
  defaultHeaderSettings,
  maxHeaderSize,
  headerMessage,
  withHeaderDateReference,
  withCaseInsensitiveDates,
  withHeaderMessage,
  HeaderRenderingResult,
  HeaderRenderingResultToNonEmpty (..),
) where

import qualified Data.ByteString as BS
import Data.CaseInsensitive (CI, foldedCase)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.NonNull
import Data.Typeable
import Data.Vector (Vector)
import Network.HTTP.Headers.HeaderFieldName
import Network.HTTP.Headers.Settings


-- data Header = Header
--   { headerName :: {-# UNPACK #-} !HeaderFieldName
--   , headerValue :: {-# UNPACK #-} !ByteString
--   }

newtype HeaderMap = HeaderMap
  { headerMapToMap :: HashMap HeaderFieldName (NonEmpty BS.ByteString)
  {- ^ Internal invariant: fields are stored in reverse order. They're reversed when
  they're looked up.

  TODO: Benchmark against using Seq instead of NonEmpty.
  -}
  }
  deriving stock (Eq)


instance Show HeaderMap where
  showsPrec n x = ("headerMapFromList " ++) . showsPrec n (headerMapToList x)


lookupRawHeader :: HeaderFieldName -> HeaderMap -> Maybe (NonEmpty BS.ByteString)
lookupRawHeader name (HeaderMap m) = fmap reverseValues $ name `Map.lookup` m
{-# INLINE lookupRawHeader #-}


lookupHeader :: forall a. KnownHeader a => HeaderMap -> Either (ParseFailure a) (Maybe a)
lookupHeader = lookupHeaderWith defaultHeaderSettings
{-# INLINE lookupHeader #-}


-- | Look up a typed field with explicit decoding settings.
lookupHeaderWith :: forall a. KnownHeader a => HeaderSettings -> HeaderMap -> Either (ParseFailure a) (Maybe a)
lookupHeaderWith settings m = traverse (parseFromHeaders settings) (lookupRawHeader (headerName $ Proxy @a) m)
{-# INLINE lookupHeaderWith #-}


insertRawHeader :: HeaderFieldName -> BS.ByteString -> HeaderMap -> HeaderMap
insertRawHeader name value (HeaderMap m) = HeaderMap $ Map.alter f name m
  where
    f Nothing = Just $ value NE.:| []
    f (Just (x NE.:| xs)) = Just $ value NE.:| (x : xs)


deleteHeader :: HeaderFieldName -> HeaderMap -> HeaderMap
deleteHeader name (HeaderMap m) = HeaderMap $ Map.delete name m


alterRawHeader :: HeaderFieldName -> (Maybe (NonEmpty BS.ByteString) -> Maybe (NonEmpty BS.ByteString)) -> HeaderMap -> HeaderMap
alterRawHeader name f (HeaderMap m) = HeaderMap $ Map.alter (fmap reverseValues . f . fmap reverseValues) name m


-- | Render the field lines to send, omitting absent or rejected values.
renderHeaderLines :: forall a. (KnownHeader a, HeaderRenderingResultToNonEmpty (Cardinality a)) => HeaderSettings -> a -> Maybe (NonEmpty BS.ByteString)
renderHeaderLines settings a
  | omitHeaderValue a = Nothing
  | otherwise = renderToHeadersChecked settings a >>= headerRenderingResultToNonEmptyMaybe @(Cardinality a)
{-# INLINE renderHeaderLines #-}


{- | Store all rendered field lines in order. An explicitly omitted
value clears the named field. Other renderings with no lines leave the
map alone.
-}
setHeader :: forall a. (KnownHeader a, HeaderRenderingResultToNonEmpty (Cardinality a)) => a -> HeaderMap -> HeaderMap
setHeader a m
  | omitHeaderValue a = deleteHeader name m
  | otherwise =
      case renderToHeadersChecked defaultHeaderSettings a >>= headerRenderingResultToNonEmptyMaybe @(Cardinality a) of
        Nothing -> m
        Just values -> setRawHeader name values m
  where
    name = headerName (Proxy @a)
{-# INLINE setHeader #-}


setRawHeader :: HeaderFieldName -> NonEmpty BS.ByteString -> HeaderMap -> HeaderMap
setRawHeader name value (HeaderMap m) = HeaderMap $ Map.insert name (reverseValues value) m


reverseValues :: NonEmpty a -> NonEmpty a
reverseValues value@(_ NE.:| []) = value
reverseValues value = NE.reverse value
{-# INLINE reverseValues #-}


{- | Build a 'HeaderMap' from a list of headers.

This is the format used by the @http-types@ and @wai@, so it's useful for
integrating with those libraries.
-}
headerMapFromList :: [(CI BS.ByteString, BS.ByteString)] -> HeaderMap
headerMapFromList = HeaderMap . foldl' add Map.empty
  where
    add m (name, value) = Map.insertWith (<>) (unsafeUnknownHeaderFromBytestring (foldedCase name)) (NE.singleton value) m


-- | Preserve field-line order within each name. Order across names is unspecified.
headerMapToList :: HeaderMap -> [(CI BS.ByteString, BS.ByteString)]
headerMapToList (HeaderMap m) = Map.foldrWithKey prepend [] m
  where
    prepend name values rest =
      let ciName = toCIByteString name
      in foldl' (\out value -> (ciName, value) : out) rest values


data HeaderCardinality = ZeroOrOne | One | ZeroOrMore | OneOrMore


data HeaderIsRequestOrResponse = Request | Response | RequestAndResponse


-- type family HeaderRenderingResult1 (f :: HeaderCardinality) :: Type -> Type where
--   HeaderRenderingResult1 ZeroOrOne = Maybe
--   HeaderRenderingResult1 ZeroOrMore = []
--   HeaderRenderingResult1 One = Identity
--   HeaderRenderingResult1 OneOrMore = NonEmpty

type family HeaderRenderingResult (f :: HeaderCardinality) :: Type where
  HeaderRenderingResult ZeroOrOne = BS.ByteString
  HeaderRenderingResult ZeroOrMore = [BS.ByteString]
  HeaderRenderingResult One = BS.ByteString
  HeaderRenderingResult OneOrMore = NonNull (Vector BS.ByteString)


class HeaderRenderingResultToNonEmpty (f :: HeaderCardinality) where
  headerRenderingResultToNonEmpty :: HeaderRenderingResult f -> NonEmpty BS.ByteString


  -- | Convert without assuming that the renderer emits a field line.
  headerRenderingResultToNonEmptyMaybe :: HeaderRenderingResult f -> Maybe (NonEmpty BS.ByteString)
  headerRenderingResultToNonEmptyMaybe = Just . headerRenderingResultToNonEmpty @f


instance HeaderRenderingResultToNonEmpty ZeroOrOne where
  headerRenderingResultToNonEmpty = NE.singleton


instance HeaderRenderingResultToNonEmpty ZeroOrMore where
  headerRenderingResultToNonEmpty = NE.fromList
  headerRenderingResultToNonEmptyMaybe = NE.nonEmpty


instance HeaderRenderingResultToNonEmpty One where
  headerRenderingResultToNonEmpty = NE.singleton


instance HeaderRenderingResultToNonEmpty OneOrMore where
  headerRenderingResultToNonEmpty = toNonEmpty


-- addHeader :: Header -> HeaderMap -> HeaderMap
-- addHeader (Header name value) (HeaderMap m) = HeaderMap $ Map.alter f name m
--   where
--     f Nothing = Just $ Reverse $ value :| []
--     f (Just (Reverse (x :| xs))) = Just $ Reverse $ value :| (x : xs)

-- lookupHeader :: HeaderFieldName -> HeaderMap -> Maybe (NonEmpty ByteString)
-- lookupHeader name (HeaderMap m) = fmap getReverse $ Map.lookup name m

{- | A typeclass for parsing and rendering well-known HTTP headers.

One header type can represent both request and response fields.
Instances with different grammars for each direction can use
'headerMessage' when the caller supplies explicit decoding settings.

When defining a new instance, you should generally use a newtype wrapper
or brand new data type
-}
class Typeable a => KnownHeader a where
  {- | The type of errors that can occur when parsing a given header.

  When possible, it is recommended to use a more specific type than 'String'.
  -}
  type ParseFailure a :: Type


  type Cardinality a :: HeaderCardinality
  type Direction a :: HeaderIsRequestOrResponse


  {- | Parse a value from one or more headers with the same header name.

  Some headers like 'Set-Cookie' can be split across multiple headers,
  so we must accommodate that.

  The order of the headers is preserved relative to how they were received.

  This function won't be called if the header is not present in the request.
  -}
  parseFromHeaders :: HeaderSettings -> NonEmpty BS.ByteString -> Either (ParseFailure a) a


  {- | Render the value in the representation selected by its 'Cardinality'.

  Use 'renderHeaderLines' when emitting a field section so that
  field-specific omission and rejection rules also apply.
  -}
  renderToHeaders :: HeaderSettings -> a -> HeaderRenderingResult (Cardinality a)


  {- | Render a value for emission. 'Nothing' rejects the value and keeps
  existing field lines when called by 'setHeader'. Explicit omission is
  controlled by 'omitHeaderValue'.
  -}
  renderToHeadersChecked :: HeaderSettings -> a -> Maybe (HeaderRenderingResult (Cardinality a))
  renderToHeadersChecked settings value = Just (renderToHeaders settings value)
  {-# INLINE renderToHeadersChecked #-}


  {- | Whether the value means that the field is intentionally absent.
  This also lets 'setHeader' clear any previous field of the same name.
  -}
  omitHeaderValue :: a -> Bool
  omitHeaderValue _ = False
  {-# INLINE omitHeaderValue #-}


  headerName :: proxy a -> HeaderFieldName
