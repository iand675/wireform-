{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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
  deleteHeader,
  lookupRawHeader,
  insertRawHeader,
  alterRawHeader,
  setHeader,
  setRawHeader,
  HeaderSettings,
  defaultHeaderSettings,
  maxHeaderSize,
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
import Data.MonoTraversable
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
  -- ^ Internal invariant: fields are stored in reverse order. They're reversed when
  --   they're looked up.
  --
  --   TODO: Benchmark against using Seq instead of NonEmpty.
  }
  deriving stock (Eq)


instance Show HeaderMap where
  showsPrec n x = ("headerMapFromList " ++) . showsPrec n (headerMapToList x)


lookupRawHeader :: HeaderFieldName -> HeaderMap -> Maybe (NonEmpty BS.ByteString)
lookupRawHeader name (HeaderMap m) = fmap NE.reverse $ name `Map.lookup` m


lookupHeader :: forall a. KnownHeader a => HeaderMap -> Either (ParseFailure a) (Maybe a)
lookupHeader m = case lookupRawHeader (headerName $ Proxy @a) m of
  Nothing -> Right Nothing
  Just entries -> case parseFromHeaders defaultHeaderSettings (NE.reverse entries) of
    Left e -> Left e
    Right a -> Right $ Just a


insertRawHeader :: HeaderFieldName -> BS.ByteString -> HeaderMap -> HeaderMap
insertRawHeader name value (HeaderMap m) = HeaderMap $ Map.alter f name m
  where
    f Nothing = Just $ value NE.:| []
    f (Just (x NE.:| xs)) = Just $ value NE.:| (x : xs)


deleteHeader :: HeaderFieldName -> HeaderMap -> HeaderMap
deleteHeader name (HeaderMap m) = HeaderMap $ Map.delete name m


alterRawHeader :: HeaderFieldName -> (Maybe (NonEmpty BS.ByteString) -> Maybe (NonEmpty BS.ByteString)) -> HeaderMap -> HeaderMap
alterRawHeader name f (HeaderMap m) = HeaderMap $ Map.alter (fmap NE.reverse . f . fmap NE.reverse) name m


setHeader :: forall a. (KnownHeader a, MonoFoldable (HeaderRenderingResult (Cardinality a)), Element (HeaderRenderingResult (Cardinality a)) ~ BS.ByteString) => a -> HeaderMap -> HeaderMap
setHeader a (HeaderMap m) = case otoList $ renderToHeaders defaultHeaderSettings a of
  [] -> HeaderMap m
  (x : xs) -> HeaderMap $ Map.insert (headerName (Proxy :: Proxy a)) (NE.reverse (x NE.:| xs)) m


setRawHeader :: HeaderFieldName -> NonEmpty BS.ByteString -> HeaderMap -> HeaderMap
setRawHeader name value (HeaderMap m) = HeaderMap $ Map.insert name (NE.reverse value) m


{- | Build a 'HeaderMap' from a list of headers.

This is the format used by the @http-types@ and @wai@, so it's useful for
integrating with those libraries.
-}
headerMapFromList :: [(CI BS.ByteString, BS.ByteString)] -> HeaderMap
headerMapFromList = foldr f (HeaderMap mempty)
  where
    f (name, value) (HeaderMap m) = HeaderMap $ Map.alter g (unsafeUnknownHeaderFromBytestring $ foldedCase name) m
      where
        g Nothing = Just $ value NE.:| []
        g (Just (x NE.:| xs)) = Just $ value NE.:| (x : xs)


headerMapToList :: HeaderMap -> [(CI BS.ByteString, BS.ByteString)]
headerMapToList (HeaderMap m) = concatMap f $ Map.toList m
  where
    f (name, values) =
      let ciName = toCIByteString name
      in map (ciName,) $ NE.toList values


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


instance HeaderRenderingResultToNonEmpty ZeroOrOne where
  headerRenderingResultToNonEmpty = NE.singleton


instance HeaderRenderingResultToNonEmpty ZeroOrMore where
  headerRenderingResultToNonEmpty = NE.fromList


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

This type class doesn't distinguish between request and response headers
because many headers can be used in both requests and responses.

For headers with different semantics in requests and responses, you should
either use newtype wrappers or perform additional validation in your
application logic.

When defining a new instance, you should generally use a newtype wrapper
or brand new data type
-}
class Typeable a => KnownHeader a where
  -- | The type of errors that can occur when parsing a given header.
  --
  --   When possible, it is recommended to use a more specific type than 'String'.
  type ParseFailure a :: Type


  type Cardinality a :: HeaderCardinality
  type Direction a :: HeaderIsRequestOrResponse


  -- | Parse a value from one or more headers with the same header name.
  --
  --   Some headers like 'Set-Cookie' can be split across multiple headers,
  --   so we must accommodate that.
  --
  --   The order of the headers is preserved relative to how they were received.
  --
  --   This function won't be called if the header is not present in the request.
  parseFromHeaders :: HeaderSettings -> NonEmpty BS.ByteString -> Either (ParseFailure a) a


  -- | Similarly to 'parseFromHeaders', we might need to render a value to multiple headers
  --   with the same header name.
  --
  --   If the header can be safely omitted, return '[]'. Otherwise, return a non-empty list of headers.
  renderToHeaders :: HeaderSettings -> a -> HeaderRenderingResult (Cardinality a)


  headerName :: proxy a -> HeaderFieldName
