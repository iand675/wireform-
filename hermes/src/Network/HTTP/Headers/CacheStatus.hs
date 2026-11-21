{-# LANGUAGE ScopedTypeVariables #-}

{- |
@Cache-Status@ is a response header by which each cache on the response path
records how it handled the request — whether the response was a hit, was
forwarded toward the origin, was stored, or was collapsed with another
in-flight request. It is an RFC 8941 structured field whose value is a list of
entries, one per cache, each identified by the cache's name and annotated with
parameters such as @hit@, @fwd@, @fwd-status@, @ttl@, and @key@, giving
operators end-to-end visibility into caching behaviour along the path.

Spec: <https://www.rfc-editor.org/rfc/rfc9211>

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Age", "Network.HTTP.Headers.Via", "Network.HTTP.Headers.ProxyStatus".
-}
module Network.HTTP.Headers.CacheStatus (
  CacheStatus (..),
  CacheStatusEntry (..),
  CacheStatusCommonFields (..),
  Forward (..),
  Hit (..),
  ForwardInfo (..),
  effectiveForwardStatus,
  FailureAccum,
  cacheStatusParser,
  renderCacheStatus,
  renderCacheStatusChecked,
) where

import qualified Data.ByteString.Short as SBS
import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Short (ShortText, toShortByteString)
import Data.Validation (Validation (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCacheStatus)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Parsing.Util as P
import qualified Network.HTTP.Headers.Rendering.Util as R


{-
To aid debugging (both by humans and automated tools), HTTP caches often append header fields to a response explaining how they handled the request. Unfortunately, the semantics of these header fields are often unclear, and both the semantics and syntax used vary between implementations.

This specification defines a new HTTP response header field, "Cache-Status", for this purpose with standardized syntax and semantics.
-}
newtype CacheStatus = CacheStatus
  { cacheStatus :: [CacheStatusEntry]
  }
  deriving stock (Eq, Show)


data CacheStatusEntry = CacheStatusEntry
  { cacheStatusIdentifier :: {-# UNPACK #-} !(Either P.RFC8941Token P.RFC8941String)
  , cacheStatusHitOrForward :: !(Either Hit ForwardInfo)
  , cacheStatusCommonFields :: !CacheStatusCommonFields
  }
  deriving stock (Eq, Show)


data CacheStatusCommonFields = CacheStatusCommonFields
  { cacheStatusTtl :: !(Maybe Int)
  , cacheStatusKey :: !(Maybe P.RFC8941String)
  , cacheStatusDetail :: !(Maybe (Either P.RFC8941Token P.RFC8941String))
  , cacheStatusUnknownParameters :: ![(ShortText, Maybe P.ItemValue)]
  -- ^ Extension parameters and forwarding-only parameters on members without @fwd@.
  }
  deriving stock (Eq, Show)


{- | When present, indicates that the request went forward towards the origin

The most specific reason known to the cache SHOULD be used, to the extent that it is possible to implement.
-}
data Forward
  = -- | The cache was configured to not handle this request.
    Bypass
  | -- | The request method's semantics require the request to be forwarded.
    Method
  | -- | The cache did not contain any responses that matched the request URI.
    UriMiss
  | {- | The cache contained a response that matched the request URI, but it could not select a response based
    upon this request's header fields and stored Vary header fields.
    -}
    VaryMiss
  | {- | The cache did not contain any responses that could be used to satisfy this request
    (to be used when an implementation cannot distinguish between uri-miss and vary-miss).
    -}
    Miss
  | {- | The cache was able to select a fresh response for the request, but the request's
    semantics (e.g., Cache-Control request directives) did not allow its use.
    -}
    Request
  | -- | The cache was able to select a response for the request, but it was stale.
    Stale
  | -- | The cache was able to select a partial response for the request, but it did not contain all of the requested ranges (or the request was for the complete response).
    Partial
  | UnknownForward !ShortText
  deriving stock (Eq, Show, Ord)


data ForwardInfo = ForwardInfo
  { forwardType :: !Forward
  , forwardStatus :: !(Maybe Int)
  , forwardStored :: !(Maybe Bool)
  , forwardCollapsed :: !(Maybe Bool)
  }
  deriving stock (Eq, Show)


-- | The next hop's status, defaulting to the enclosing response status.
effectiveForwardStatus :: Int -> ForwardInfo -> Int
effectiveForwardStatus responseStatus = fromMaybe responseStatus . forwardStatus


data Hit = Hit | NoHit | HitUnspecified
  deriving stock (Eq, Show)


type FailureAccum = NE.NonEmpty (Maybe ShortText, String)


instance KnownHeader CacheStatus where
  type ParseFailure CacheStatus = FailureAccum
  type Cardinality CacheStatus = 'ZeroOrMore
  type Direction CacheStatus = 'Response


  parseFromHeaders _ headers = case P.runParser cacheStatusParser (P.combineStructuredFieldLines headers) of
    P.OK cs "" -> Right cs
    P.OK _ rest -> Left $ pure (Nothing, "Unconsumed input after parsing Cache-Status header: " <> show rest)
    P.Fail -> Left $ pure (Nothing, "Failed to parse Cache-Status header")
    P.Err e -> Left e


  renderToHeaders _ (CacheStatus []) = []
  renderToHeaders _ value = either (const []) (pure . M.toStrictByteString) (renderCacheStatusChecked value)


  omitHeaderValue = null . cacheStatus
  {-# INLINE omitHeaderValue #-}


  headerName _ = hCacheStatus


cacheStatusEntryParser :: forall st. P.ParserT st FailureAccum (Validation FailureAccum CacheStatusEntry)
cacheStatusEntryParser = do
  ident <- P.structuredFieldTokenOrString
  parameters <- P.embedError P.structuredFieldParameters $ \msg -> P.err (pure (Nothing, msg))
  let debugName = either P.unsafeToRFC8941Token P.unsafeToRFC8941String ident
      forwarded = lookupParameter "fwd" parameters
  pure $ CacheStatusEntry ident <$> vHitOrForward debugName forwarded parameters <*> vCommonFields debugName (isJust forwarded) parameters
  where
    vHitOrForward :: ShortText -> Maybe P.ItemValue -> [P.StructuredFieldParameter] -> Validation FailureAccum (Either Hit ForwardInfo)
    vHitOrForward k forwarded mps = case (lookupParameter "hit" mps, forwarded) of
      (Just (P.Boolean b), Nothing) -> Success $ Left (if b then Hit else NoHit)
      (Just _, Nothing) -> Failure $ pure (Nothing, show k <> " 'hit' parameter must be a boolean")
      (Nothing, Just (P.Token t)) -> Right <$> vForward k (P.unsafeToRFC8941Token t) mps
      (Nothing, Just _) -> Failure $ pure (Nothing, show k <> " 'fwd' parameter must be a token")
      (Just _, Just _) -> Failure $ pure (Nothing, show k <> " cannot have both 'hit' and 'fwd' parameters")
      (Nothing, Nothing) -> Success $ Left HitUnspecified
    vForward :: ShortText -> ShortText -> [P.StructuredFieldParameter] -> Validation FailureAccum ForwardInfo
    vForward k t mps = ForwardInfo <$> vForwardType <*> vForwardStatus <*> vForwardStored <*> vForwardCollapsed
      where
        vForwardType = case t of
          "bypass" -> pure Bypass
          "method" -> pure Method
          "uri-miss" -> pure UriMiss
          "vary-miss" -> pure VaryMiss
          "miss" -> pure Miss
          "request" -> pure Network.HTTP.Headers.CacheStatus.Request
          "stale" -> pure Stale
          "partial" -> pure Partial
          _ -> pure $ UnknownForward t
        vForwardStatus = case lookupParameter "fwd-status" mps of
          Just (P.Integer i)
            | validForwardStatus i -> pure $ Just i
          Just _ -> Failure $ pure (Just k, "fwd-status must be an HTTP status code from 100 through 599")
          Nothing -> pure Nothing
        vForwardStored = optionalBoolean "stored"
        vForwardCollapsed = optionalBoolean "collapsed"
        optionalBoolean name = case lookupParameter name mps of
          Nothing -> pure Nothing
          Just (P.Boolean b) -> pure (Just b)
          Just _ -> Failure $ pure (Just k, show name <> " must be a boolean")
    vCommonFields k forwarded mps = CacheStatusCommonFields <$> vTtl <*> vKey <*> vDetail <*> vUnknownParameters
      where
        vTtl = case lookupParameter "ttl" mps of
          Just (P.Integer i) -> pure $ Just i
          Just _ -> Failure $ pure (Just k, "ttl must be an integer")
          _ -> pure Nothing
        vKey = case lookupParameter "key" mps of
          Just (P.String s) -> pure $ Just s
          Just _ -> Failure $ pure (Just k, "key must be a string")
          _ -> pure Nothing
        vDetail = case lookupParameter "detail" mps of
          Just (P.String s) -> pure $ Just $ Right s
          Just (P.Token s) -> pure $ Just $ Left s
          Just _ -> Failure $ pure (Just k, "detail must be a string or token")
          _ -> pure Nothing
        vUnknownParameters =
          pure (P.structuredFieldParameterPairs (filter (not . projectedParameter forwarded . P.structuredParameterKey) mps))


projectedParameter :: Bool -> ShortText -> Bool
projectedParameter forwarded key = case SBS.length (toShortByteString key) of
  3 -> key == "hit" || key == "fwd" || key == "ttl" || key == "key"
  6 -> key == "detail" || (forwarded && key == "stored")
  9 -> forwarded && key == "collapsed"
  10 -> forwarded && key == "fwd-status"
  _ -> False
{-# INLINE projectedParameter #-}


lookupParameter :: ShortText -> [P.StructuredFieldParameter] -> Maybe P.ItemValue
lookupParameter _ [] = Nothing
lookupParameter key (P.StructuredFieldParameter name value : rest)
  | key == name = Just value
  | otherwise = lookupParameter key rest


cacheStatusParser :: forall st. P.ParserT st FailureAccum CacheStatus
cacheStatusParser = do
  entries <- P.structuredFieldList cacheStatusEntryParser
  case sequenceA entries of
    Failure acc -> P.err acc
    Success es -> pure $ CacheStatus es


validForwardStatus :: Int -> Bool
validForwardStatus status = status >= 100 && status <= 599
{-# INLINE validForwardStatus #-}


-- | Render the field value. An invalid member produces no bytes.
renderCacheStatus :: CacheStatus -> M.Builder
renderCacheStatus = either (const mempty) id . renderCacheStatusChecked


{- | Validate all entries before emitting the list, including the surviving
extension parameters after ordered replacement.
-}
renderCacheStatusChecked :: CacheStatus -> Either String M.Builder
renderCacheStatusChecked (CacheStatus entries)
  | all validCacheStatusEntry entries = Right (R.renderSeparated ", " renderCacheStatusEntry entries)
  | otherwise = Left "Invalid Cache-Status member or parameter"
{-# INLINE renderCacheStatusChecked #-}


validCacheStatusEntry :: CacheStatusEntry -> Bool
validCacheStatusEntry (CacheStatusEntry identifier hitOrForward common) =
  validTokenOrString identifier
    && validForward hitOrForward
    && validCommonFields (isRight hitOrForward) common
  where
    validTokenOrString = either P.isStructuredFieldToken P.isStructuredFieldString

    validForward (Left _) = True
    validForward (Right (ForwardInfo reason status _ _)) =
      validReason reason && maybe True validForwardStatus status
    validReason (UnknownForward reason) = P.isStructuredFieldToken (P.RFC8941Token reason)
    validReason _ = True

    validCommonFields forwarded (CacheStatusCommonFields ttl key detail parameters) =
      maybe True (P.isStructuredFieldBareItem . P.Integer) ttl
        && maybe True P.isStructuredFieldString key
        && maybe True validTokenOrString detail
        && all (validExtension forwarded) (P.orderedFieldEntries fst parameters)
    validExtension forwarded (key, value) =
      projectedParameter forwarded key
        || (P.isStructuredFieldKey key && maybe True P.isStructuredFieldBareItem value)


renderCacheStatusEntry :: CacheStatusEntry -> M.Builder
renderCacheStatusEntry (CacheStatusEntry k hf cf) =
  either R.rfc8941Token R.rfc8941String k
    <> renderHitOrForward hf
    <> renderCommonFields (isRight hf) cf


renderHitOrForward :: Either Hit ForwardInfo -> M.Builder
renderHitOrForward = \case
  Left Hit -> ";hit"
  Left NoHit -> ";hit=?0"
  Left HitUnspecified -> mempty
  Right (ForwardInfo t s st c) ->
    R.rfc8941Parameter R.ExcludeIfEmpty renderForwardType "fwd" (Just t)
      <> renderForwardStatus s
      <> renderForwardStored st
      <> renderForwardCollapsed c


renderForwardType :: Forward -> M.Builder
renderForwardType =
  R.shortText . \case
    Bypass -> "bypass"
    Method -> "method"
    UriMiss -> "uri-miss"
    VaryMiss -> "vary-miss"
    Miss -> "miss"
    Network.HTTP.Headers.CacheStatus.Request -> "request"
    Stale -> "stale"
    Partial -> "partial"
    UnknownForward t -> t


renderForwardStatus :: Maybe Int -> M.Builder
renderForwardStatus = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941Integer "fwd-status"


renderForwardStored :: Maybe Bool -> M.Builder
renderForwardStored = renderBooleanParameter "stored"


renderForwardCollapsed :: Maybe Bool -> M.Builder
renderForwardCollapsed = renderBooleanParameter "collapsed"


renderBooleanParameter :: ShortText -> Maybe Bool -> M.Builder
renderBooleanParameter _ Nothing = mempty
renderBooleanParameter name (Just True) = M.char7 ';' <> R.shortText name
renderBooleanParameter name (Just False) = M.char7 ';' <> R.shortText name <> "=?0"


renderTtl :: Maybe Int -> M.Builder
renderTtl = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941Integer "ttl"


renderKey :: Maybe P.RFC8941String -> M.Builder
renderKey = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941String "key"


renderDetail :: Maybe (Either P.RFC8941Token P.RFC8941String) -> M.Builder
renderDetail = R.rfc8941Parameter R.ExcludeIfEmpty (either R.rfc8941Token R.rfc8941String) "detail"


renderCommonFields :: Bool -> CacheStatusCommonFields -> M.Builder
renderCommonFields forwarded (CacheStatusCommonFields ttl key detail parameters) =
  renderTtl ttl
    <> renderKey key
    <> renderDetail detail
    <> renderExtensions forwarded (P.orderedFieldEntries fst parameters)


renderExtensions :: Bool -> [(ShortText, Maybe P.ItemValue)] -> M.Builder
renderExtensions _ [] = mempty
renderExtensions forwarded ((name, value) : rest)
  | projectedParameter forwarded name = renderExtensions forwarded rest
  | otherwise =
      R.renderStructuredFieldParameter name (fromMaybe (P.Boolean True) value)
        <> renderExtensions forwarded rest
