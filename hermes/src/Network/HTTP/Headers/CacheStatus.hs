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
  FailureAccum,
  cacheStatusParser,
  renderCacheStatus,
) where

import Control.Monad.Combinators
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
-- Importing 'Data.Validation' unqualified would bring its
-- 'either' Iso (validation 1.2+) into scope alongside
-- 'Prelude.either'. The latter is what the renderers below want,
-- so we expose only the constructors and the Validation type.

import Data.Text.Short (ShortText)
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
  | -- | The cache contained a response that matched the request URI, but it could not select a response based
    --     upon this request's header fields and stored Vary header fields.
    VaryMiss
  | -- | The cache did not contain any responses that could be used to satisfy this request
    --     (to be used when an implementation cannot distinguish between uri-miss and vary-miss).
    Miss
  | -- | The cache was able to select a fresh response for the request, but the request's
    --     semantics (e.g., Cache-Control request directives) did not allow its use.
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


data Hit = Hit
  deriving stock (Eq, Show)


type FailureAccum = NE.NonEmpty (Maybe ShortText, String)


instance KnownHeader CacheStatus where
  type ParseFailure CacheStatus = FailureAccum
  type Cardinality CacheStatus = 'ZeroOrMore
  type Direction CacheStatus = 'Response


  parseFromHeaders _ headers = case P.runParser cacheStatusParser (B.intercalate ", " $ NE.toList headers) of
    P.OK cs "" -> Right cs
    P.OK _ rest -> Left $ pure (Nothing, "Unconsumed input after parsing Cache-Status header: " <> show rest)
    P.Fail -> Left $ pure (Nothing, "Failed to parse Cache-Status header")
    P.Err e -> Left e


  renderToHeaders _ = pure . M.toStrictByteString . renderCacheStatus


  headerName _ = hCacheStatus


unvalidatedCacheStatusEntryParser
  :: forall st
   . P.ParserT st FailureAccum (Either P.RFC8941Token P.RFC8941String, [(ShortText, Maybe P.ItemValue)])
unvalidatedCacheStatusEntryParser = do
  ident <- eitherP P.rfc8941Token P.rfc8941String
  params <- P.embedError P.rfc8941Parameters $ \msg -> P.err $ pure (Nothing, msg)
  pure (ident, params)


cacheStatusEntryParser :: forall st. P.ParserT st FailureAccum (Validation FailureAccum CacheStatusEntry)
cacheStatusEntryParser = do
  (k, ps) <- unvalidatedCacheStatusEntryParser
  let mps :: M.Map ShortText (Maybe P.ItemValue)
      mps = M.fromList ps
      debugName = either P.unsafeToRFC8941Token P.unsafeToRFC8941String k
  pure $ CacheStatusEntry k <$> vHitOrForward debugName mps <*> vCommonFields debugName mps
  where
    vHitOrForward :: ShortText -> M.Map ShortText (Maybe P.ItemValue) -> Validation FailureAccum (Either Hit ForwardInfo)
    vHitOrForward k mps = case (M.lookup "hit" mps, M.lookup "fwd" mps) of
      (Just _, Nothing) -> Success $ Left Hit
      (Nothing, Just (Just (P.Token t))) -> Right <$> vForward k (P.unsafeToRFC8941Token t) mps
      (Nothing, Just _) -> Failure $ pure (Nothing, show k <> " 'fwd' parameter must be a token")
      (Just _, Just _) -> Failure $ pure (Nothing, show k <> " cannot have both 'hit' and 'fwd' parameters")
      (Nothing, Nothing) -> Failure $ pure (Nothing, show k <> " must have either 'hit' or 'fwd' parameter")
    vForward :: ShortText -> ShortText -> M.Map ShortText (Maybe P.ItemValue) -> Validation FailureAccum ForwardInfo
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
        vForwardStatus = case M.lookup "fwd-status" mps of
          Just (Just (P.Integer i)) -> pure $ Just i
          Just (Just _) -> Failure $ pure (Just k, "fwd-status must be an integer")
          Just Nothing -> Failure $ pure (Just k, "fwd-status if provided must have a value")
          Nothing -> pure Nothing
        vForwardStored = case M.lookup "stored" mps of
          Just (Just (P.Boolean b)) -> pure $ Just b
          Just (Just _) -> Failure $ pure (Just k, "stored must be a boolean")
          _ -> pure Nothing
        vForwardCollapsed = case M.lookup "collapsed" mps of
          Just (Just (P.Boolean b)) -> pure $ Just b
          Just (Just _) -> Failure $ pure (Just k, "collapsed must be a boolean")
          _ -> pure Nothing
    vCommonFields k mps = CacheStatusCommonFields <$> vTtl <*> vKey <*> vDetail <*> vUnknownParameters
      where
        vTtl = case M.lookup "ttl" mps of
          Just (Just (P.Integer i)) -> pure $ Just i
          Just (Just _) -> Failure $ pure (Just k, "ttl must be an integer")
          _ -> pure Nothing
        vKey = case M.lookup "key" mps of
          Just (Just (P.String s)) -> pure $ Just s
          Just (Just _) -> Failure $ pure (Just k, "key must be a string")
          _ -> pure Nothing
        vDetail = case M.lookup "detail" mps of
          Just (Just (P.String s)) -> pure $ Just $ Right s
          Just (Just (P.Token s)) -> pure $ Just $ Left s
          Just (Just _) -> Failure $ pure (Just k, "detail must be a string or token")
          _ -> pure Nothing
        vUnknownParameters =
          pure $
            M.toList $
              M.filterWithKey (\k' _ -> k' `notElem` ["hit", "fwd", "fwd-status", "stored", "collapsed", "ttl", "key", "detail"]) mps


cacheStatusParser :: forall st. P.ParserT st FailureAccum CacheStatus
cacheStatusParser = do
  entries <- P.rfc8941List cacheStatusEntryParser
  case sequenceA entries of
    Failure acc -> P.err acc
    Success es -> pure $ CacheStatus es


renderCacheStatus :: CacheStatus -> M.Builder
renderCacheStatus (CacheStatus entries) = M.intersperse ", " $ renderCacheStatusEntry <$> entries


renderCacheStatusEntry :: CacheStatusEntry -> M.Builder
renderCacheStatusEntry (CacheStatusEntry k hf cf) =
  either R.rfc8941Token R.rfc8941String k
    <> renderHitOrForward hf
    <> renderCommonFields cf


renderHitOrForward :: Either Hit ForwardInfo -> M.Builder
renderHitOrForward = \case
  Left Hit -> R.rfc8941Parameter R.IncludeIfEmpty R.rfc8941Binary "hit" Nothing
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
renderForwardStored = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941Boolean "stored"


renderForwardCollapsed :: Maybe Bool -> M.Builder
renderForwardCollapsed = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941Boolean "collapsed"


renderTtl :: Maybe Int -> M.Builder
renderTtl = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941Integer "ttl"


renderKey :: Maybe P.RFC8941String -> M.Builder
renderKey = R.rfc8941Parameter R.ExcludeIfEmpty R.rfc8941String "key"


renderDetail :: Maybe (Either P.RFC8941Token P.RFC8941String) -> M.Builder
renderDetail = R.rfc8941Parameter R.ExcludeIfEmpty (either R.rfc8941Token R.rfc8941String) "detail"


renderUnknownParameter :: (ShortText, Maybe P.ItemValue) -> M.Builder
renderUnknownParameter (k, mv) =
  R.shortText k
    <> case mv of
      Nothing -> mempty
      Just v -> M.char7 '=' <> R.rfc8941ItemValue v


renderCommonFields :: CacheStatusCommonFields -> M.Builder
renderCommonFields (CacheStatusCommonFields ttl key detail ups) =
  renderTtl ttl
    <> renderKey key
    <> renderDetail detail
    <> M.intersperse " " (renderUnknownParameter <$> ups)
