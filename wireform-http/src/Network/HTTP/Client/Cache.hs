{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | A private (per-client) RFC 9111 HTTP cache.

This is the cache surface that the audit called out as missing
(§1.1). It implements the subset of RFC 9111 that applies to a
non-shared user-agent cache:

* Storage of GET \/ HEAD responses keyed by URI (scheme, host,
  port, path+query).
* Freshness from @Cache-Control: max-age@ then @Expires@, with
  @immutable@ limited to the freshness lifetime.
* Conditional revalidation via @ETag@ → @If-None-Match@ and
  @Last-Modified@ → @If-Modified-Since@; on @304 Not Modified@
  the stored body is replayed.
* Evaluates caller-provided @If-None-Match@ and @If-Modified-Since@
  against reusable 200 responses and can return a local @304@.
* An upstream @HEAD 200@ can refresh a matching cached GET or
  retire it when the stored representation cannot be confirmed.
* Honors @no-store@ on either the request or the response.
* Honors @no-cache@ (forces revalidation) and @must-revalidate@
  (refuses to serve stale).
* Applies request @max-age@, @min-fresh@, and @max-stale@ to cache hits.
  An @only-if-cached@ request receives an eligible entry or a local 504.
* Adds an @Age@ header to cache hits per RFC 9111 §4.2.3.

== Scope: private cache

Shared-cache directives (@public@, @s-maxage@, @proxy-revalidate@)
are parsed and stored but not consulted; this module is designed
for the per-client use case.

== Vary-keyed secondary cache keys (RFC 9111 §4.1)

When a response carries a @Vary@ header the cache stores which
request-header values were in play at store time.  On a subsequent
request the stored entry is a hit only if the current request's
headers match for every name listed in @Vary@.  A @Vary: *@ response
is never stored (it opts out of caching entirely).

== Invalidation after writes (RFC 9111 §4.4)

A 2xx or 3xx response to an unsafe method invalidates cached GET and HEAD
responses for its target. Valid singleton @Location@ and @Content-Location@
references also invalidate matching targets within the same origin.

== stale-while-revalidate (RFC 5861 §3)

When a response carries @Cache-Control: stale-while-revalidate=N@
and the request allows stale reuse within the N-second window, the cache
serves the stale response immediately. Requests that permit contacting
the origin also trigger a background revalidation.

== stale-if-error (RFC 5861 §4)

When the request or response carries @Cache-Control: stale-if-error=N@
and revalidation fails within the allowed window, the cache can serve
the stale entry. If both specify a window, the shorter one applies.

== Pluggable storage

The cache decouples its decision logic (what's cacheable, when to
revalidate, how to apply Cache-Control) from its persistence
('CacheStore'). 'CacheStore' is a record-of-functions over
'CacheKey' and 'CacheEntry':

@
data 'CacheStore' = 'CacheStore'
  { 'csLookup' :: 'CacheKey' -> IO (Maybe 'CacheEntry')
  , 'csInsert' :: 'CacheKey' -> 'CacheEntry' -> IO ()
  , 'csDelete' :: 'CacheKey' -> IO ()
  , 'csClear'  :: IO ()
  , 'csSize'   :: IO Int
  }
@

The shipped 'newCache' uses a 'TVar'-backed 'HM.HashMap' with soft
entry-count eviction. 'newCacheWith' accepts a caller-supplied
'CacheStore'. 'newSynchronizedCacheWith' coordinates publication and
invalidation among requests sharing one cache instance.

== Usage

@
cache <- 'newCache' 'defaultCacheConfig'
'withClient' 'defaultClientConfig'
  { 'ccExtra' = ['withCache' cache] } $ \\transport -> ...
@

The cache middleware is composed /below/ the auth and tracing
middlewares so that cache hits skip token-injection latency and
tracing sees cache-vs-network status. Compose /above/ the
decompression middleware so cache hits replay the stored decoded bytes
directly.
-}
module Network.HTTP.Client.Cache (
  -- * Cache
  Cache,
  CacheConfig (..),
  defaultCacheConfig,
  newCache,
  newCacheWith,
  newSynchronizedCacheWith,
  clearCache,

  -- * Storage backend
  CacheStore (..),
  newInMemoryStore,

  -- * Cache primitives (for custom stores)
  CacheKey (..),
  CacheEntry (..),

  -- * Middleware
  withCache,

  -- * Inspection
  cacheSize,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (mask, onException, throwIO)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.Containers.ListUtils (nubOrd)
import Data.Fixed (Fixed (MkFixed))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable, hashWithSalt)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isNothing)
import Data.Text.Short qualified as ShortText
import Data.Time.Clock (
  NominalDiffTime,
  UTCTime (..),
  addUTCTime,
  diffTimeToPicoseconds,
  diffUTCTime,
  getCurrentTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import Data.Word (Word32, Word64)
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Conditional qualified as Conditional
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Transport (Middleware, Transport (..))
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.Age qualified as Age
import Network.HTTP.Headers.CacheControl qualified as HCC
import Network.HTTP.Headers.ContentLength qualified as ContentLength
import Network.HTTP.Headers.ContentLocation qualified as ContentLocation
import Network.HTTP.Headers.Date qualified as Date
import Network.HTTP.Headers.ETag qualified as ETag
import Network.HTTP.Headers.Expires qualified as Expires
import Network.HTTP.Headers.HeaderFieldName qualified as FieldName
import Network.HTTP.Headers.IfModifiedSince qualified as IfModifiedSince
import Network.HTTP.Headers.IfNoneMatch qualified as IfNoneMatch
import Network.HTTP.Headers.LastModified qualified as LastModified
import Network.HTTP.Headers.Location qualified as Location
import Network.HTTP.Headers.Pragma qualified as Pragma
import Network.HTTP.Headers.TransferEncoding qualified as TransferEncoding
import Network.HTTP.Headers.Vary qualified as Vary
import Network.HTTP.Internal.Validation (isBodyDigestField)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import UnliftIO.Exception (tryAny)


-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data CacheConfig = CacheConfig
  { ccMaxEntries :: !Int
  {- ^ Soft cap on the number of stored entries, honoured by
  'newInMemoryStore'. Custom 'CacheStore' backends decide
  their own eviction policy and may ignore this. Default
  1024.
  -}
  , ccMaxBodyBytes :: !Int
  {- ^ Maximum size of a response body the cache will store, in
  bytes. Larger responses pass through untouched. Default
  1 MiB.
  -}
  }
  deriving stock (Eq, Show)


defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { ccMaxEntries = 1024
    , ccMaxBodyBytes = 1024 * 1024
    }


-- ---------------------------------------------------------------------------
-- Primitives (exposed so custom CacheStores can serialise them)
-- ---------------------------------------------------------------------------

{- | The primary cache key (scheme, host, port, method, path+query).
Vary-derived secondary key matching is done at the entry level —
'lookupEntry' filters stored entries by comparing the request's
header values against 'ceVaryFields' (RFC 9111 §4.1 second step).
-}
data CacheKey = CacheKey
  { ckScheme :: !WURI.Scheme
  , ckHost :: !ByteString
  , ckPort :: !Int
  , ckMethod :: !M.Method
  , ckPath :: !ByteString
  }
  deriving stock (Eq, Show)


instance Hashable CacheKey where
  hashWithSalt s k =
    hashWithSalt
      s
      ( case ckScheme k of WURI.SchemeHttp -> (0 :: Int); WURI.SchemeHttps -> 1
      , ckHost k
      , ckPort k
      , M.fromMethod (ckMethod k)
      , ckPath k
      )


{- | One stored response plus the metadata needed to make freshness
and revalidation decisions later.
-}
data CacheEntry = CacheEntry
  { ceStatus :: !S.Status
  , ceHeaders :: ![(H.HeaderName, H.HeaderValue)]
  , ceBody :: !ByteString
  , ceRequestTime :: !UTCTime
  -- ^ Time the exchange that produced these stored headers began.
  , ceResponseTime :: !UTCTime
  -- ^ Header receipt time, before buffering the body. Used for RFC 9111 §4.2.3.
  , ceMaxAge :: !(Maybe NominalDiffTime)
  , ceExpiresAt :: !(Maybe UTCTime)
  , ceImmutable :: !Bool
  , ceMustRevalidate :: !Bool
  , ceNoCache :: !Bool
  {- ^ Response was tagged @no-cache@: stored but forces
  revalidation on every reuse.
  -}
  , ceVaryFields :: ![(H.HeaderName, H.HeaderValue)]
  {- ^ Snapshot of the original request's field lines for each header
  named in the response's @Vary@ field (RFC 9111 §4.1). Custom stores
  should treat the list as opaque and preserve its rows verbatim.
  Legacy snapshots for responses with selecting fields are cache misses.
  -}
  , ceStaleWhileRevalidate :: !(Maybe NominalDiffTime)
  {- ^ @stale-while-revalidate@ delta from RFC 5861 §3.  When the
  entry is stale but within this window the cache serves the
  stale response and triggers a background revalidation.
  -}
  , ceStaleIfError :: !(Maybe NominalDiffTime)
  {- ^ @stale-if-error@ delta from RFC 5861 §4.  When revalidation
  fails and the entry is within this window, the stale entry is
  returned rather than propagating the error.
  -}
  }
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Pluggable storage backend
-- ---------------------------------------------------------------------------

{- | A record-of-functions abstraction over the cache's
persistence layer.  Bundled implementations:

* 'newInMemoryStore' — 'TVar' + 'Data.HashMap.Strict.HashMap'
  with a soft entry-count cap.

Custom backends (Redis, disk, SQLite, …) implement their own
'CacheStore' and pass it to 'newCacheWith'. The 'IO' actions
must be safe to call concurrently from multiple threads —
this module makes no internal locking attempt.
-}
data CacheStore = CacheStore
  { csLookup :: !(CacheKey -> IO (Maybe CacheEntry))
  -- ^ Return the entry for @key@, or 'Nothing' if absent.
  , csInsert :: !(CacheKey -> CacheEntry -> IO ())
  {- ^ Store the entry, replacing any existing one for @key@.
  The store is responsible for any eviction needed to
  stay within its configured limits.
  -}
  , csDelete :: !(CacheKey -> IO ())
  -- ^ Remove @key@ if present. No-op when absent.
  , csClear :: !(IO ())
  -- ^ Drop all stored entries.
  , csSize :: !(IO Int)
  {- ^ Current entry count. Used by 'cacheSize' for
  diagnostics; cheap if possible but not required to be
  constant-time.
  -}
  }


{- | Allocate the bundled 'TVar' \/ 'HM.HashMap' store with a soft
cap on the entry count. When the cap is exceeded, the
'csInsert' implementation drops enough entries to bring it
back to the limit; the dropped set is unspecified (effectively
'HM.HashMap' iteration order), which is acceptable for a
best-effort fast-path cache.
-}
newInMemoryStore :: Int -> IO CacheStore
newInMemoryStore cap = do
  ref <- newTVarIO HM.empty
  pure CacheStore
    { csLookup = memoryLookup ref
    , csInsert = \k entry -> atomically (modifyTVar' ref (insertWithinCap cap k entry))
    , csDelete = \k -> atomically $ modifyTVar' ref (HM.delete k)
    , csClear = atomically $ writeTVar ref HM.empty
    , csSize = HM.size <$> readTVarIO ref
    }


insertWithinCap :: Int -> CacheKey -> a -> HM.HashMap CacheKey a -> HM.HashMap CacheKey a
insertWithinCap cap key entry entries =
  let inserted = HM.insert key entry entries
      excess = HM.size inserted - cap
  in if excess > 0 then HM.fromList (drop excess (HM.toList inserted)) else inserted


memoryLookup :: TVar (HM.HashMap CacheKey a) -> CacheKey -> IO (Maybe a)
memoryLookup ref key = HM.lookup key <$> readTVarIO ref


-- ---------------------------------------------------------------------------
-- Cache
-- ---------------------------------------------------------------------------

data Cache = Cache
  { cacheLookup :: !(CacheKey -> IO (Maybe StoredEntry))
  , cacheGeneration :: !(IO Word64)
  , cacheUpdate :: !(Word64 -> CacheKey -> Maybe CacheEntry -> Maybe CacheEntry -> IO ())
  -- ^ The first entry is the expected snapshot for a revalidation.
  , cacheInvalidate :: !([CacheKey] -> IO ())
  , cacheClear :: !(IO ())
  , cacheCount :: !(IO Int)
  , cacheConfig :: !CacheConfig
  }


{- | Allocate a 'Cache' backed by the bundled in-memory store
sized via 'ccMaxEntries'.
-}
newCache :: CacheConfig -> IO Cache
newCache cfg = do
  ref <- newTVarIO HM.empty
  generation <- newTVarIO 0
  let prepare entry =
        let timing = initialTiming entry
        in timing `seq` StoredEntry entry timing
      update started key expected change = atomically $ do
        current <- readTVar generation
        when (current == started) $ modifyTVar' ref $ \entries ->
          if updateStillApplies expected (seEntry <$> HM.lookup key entries)
            then change entries
            else entries
      invalidate change = atomically $ do
        modifyTVar' generation (+ 1)
        modifyTVar' ref change
  pure Cache
    { cacheLookup = memoryLookup ref
    , cacheGeneration = readTVarIO generation
    , cacheUpdate = \started key expected -> \case
        Nothing -> update started key expected (HM.delete key)
        Just entry ->
          let stored = prepare entry
          in stored `seq` update started key expected (insertWithinCap (ccMaxEntries cfg) key stored)
    , cacheInvalidate = \keys -> invalidate (\entries -> foldl' (flip HM.delete) entries keys)
    , cacheClear = invalidate (const HM.empty)
    , cacheCount = HM.size <$> readTVarIO ref
    , cacheConfig = cfg
    }


{- | Build a 'Cache' from a caller-supplied 'CacheStore'. The
'CacheConfig' still controls cache-level policy
('ccMaxBodyBytes' bounds the body size accepted on store);
'ccMaxEntries' is up to the backend to honour (the bundled
'newInMemoryStore' does, third-party stores may not).

Operations use the store callbacks directly. Use 'newSynchronizedCacheWith'
to make revalidation checks and writes atomic within one cache instance
and to order invalidation against publication.
-}
newCacheWith :: CacheStore -> CacheConfig -> Cache
newCacheWith store cfg = Cache
  { cacheLookup = \key -> (>>= describeLoadedEntry) <$> csLookup store key
  , cacheGeneration = pure 0
  , cacheUpdate = \_ key expected entry -> do
      applies <- case expected of
        Nothing -> pure True
        Just _ -> updateStillApplies expected <$> csLookup store key
      when applies (maybe (csDelete store key) (csInsert store key) entry)
  , cacheInvalidate = mapM_ (csDelete store)
  , cacheClear = csClear store
  , cacheCount = csSize store
  , cacheConfig = cfg
  }


{- | Coordinate invalidation and publication for a caller-supplied store.
All writers using this cache share the same gate. Writers using separate
cache instances or processes need coordination at the storage layer.
-}
newSynchronizedCacheWith :: CacheStore -> CacheConfig -> IO Cache
newSynchronizedCacheWith store cfg = do
  generation <- newIORef 0
  gate <- newMVar ()
  let base = newCacheWith store cfg
      locked :: IO a -> IO a
      locked action = withMVar gate (\() -> action)
      invalidate action = locked $ do
        atomicModifyIORef' generation (\n -> (n + 1, ()))
        action
  pure base
    { cacheLookup = locked . cacheLookup base
    , cacheGeneration = readIORef generation
    , cacheUpdate = \started key expected entry -> locked $ do
        current <- readIORef generation
        when (current == started) (cacheUpdate base started key expected entry)
    , cacheInvalidate = invalidate . cacheInvalidate base
    , cacheClear = invalidate (cacheClear base)
    }


updateStillApplies :: Maybe CacheEntry -> Maybe CacheEntry -> Bool
updateStillApplies Nothing _ = True
updateStillApplies (Just expected) (Just current) =
  ceResponseTime expected == ceResponseTime current
    && ceRequestTime expected == ceRequestTime current
    && expected == current
updateStillApplies (Just _) Nothing = False


clearCache :: Cache -> IO ()
clearCache = cacheClear


cacheSize :: Cache -> IO Int
cacheSize = cacheCount


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

{- | Cache middleware. On each request:

1. For a bodyless GET or HEAD without an origin-only precondition or
   a range, build a cache key from the request URI + method.
2. Reuse an eligible entry with an @Age@ header, subject to the
   request's Cache-Control constraints and any stale allowance.
   Evaluate caller conditions against an eligible 200 response.
3. Else if a cached entry has a valid validator, attach conditional
   headers and call the inner transport. A matching 304 refreshes the
   stored metadata and reuses the cached body.
4. When the request requires the origin, @only-if-cached@ returns a
   local 504. Otherwise, call the transport and store eligible responses.
-}
withCache :: Cache -> Middleware IO
withCache cache inner = Transport $ \req -> do
  let policy = requestCachePolicy (WReq.headers req)
      mode = cacheRequestMode req
      contactOrigin action
        | crOnlyIfCached policy = pure cacheOnlyMiss
        | otherwise = action
      bypass
        | not (crNoStore policy)
        , WReq.method req == M.mHead
        , BSm.knownSize (WReq.body req) == Just 0
        , Just key <- keyOfRequest req = do
            generation <- cacheGeneration cache
            trRaw <$> sendCacheTimed cache generation key inner req
        | otherwise = forwardWithInvalidation cache inner req
  if crNoStore policy || mode == BypassCache
    then contactOrigin bypass
    else case keyOfRequest req of
      Nothing -> contactOrigin (sendRaw inner req)
      Just key -> do
        generation <- cacheGeneration cache
        mEntry <- lookupEntry cache key (WReq.headers req)
        case mEntry of
          Nothing -> contactOrigin $
            if mode == Unconditional
              then sendCacheTimed cache generation key inner req >>= maybeStoreAndReturn cache generation key Nothing req
              else bypass
          Just stored -> do
            now <- getCurrentTime
            let entry = seEntry stored
                timing = entryTiming now stored
                decision = cacheDecision policy entry timing
            if mode == Unconditional
              then case decision of
                UseCache -> replayHit timing entry
                UseCacheAndRevalidate -> do
                  unless (crOnlyIfCached policy) $
                    void (forkIO (revalidateSilent cache inner req generation key entry))
                  replayHit timing entry
                ContactOrigin -> contactOrigin (revalidateOrReplace cache inner req policy generation key stored)
              else case decision of
                ContactOrigin -> contactOrigin $
                  if ceStatus entry == S.status200
                    && cachedNotModified now timing entry (WReq.headers req) == Right True
                    then revalidateCallerCondition cache generation key inner req entry
                    else bypass
                _
                  | ceStatus entry == S.status200
                  , Right unchanged <- cachedNotModified now timing entry (WReq.headers req) -> do
                      case decision of
                        UseCacheAndRevalidate
                          | not (crOnlyIfCached policy)
                          , let fields = filter (not . cacheConditionField . fst) (WReq.headers req)
                          , varyMatch fields entry ->
                              void (forkIO (revalidateSilent cache inner req{WReq.headers = fields} generation key entry))
                        _ -> pure ()
                      if unchanged then pure (notModifiedResponse timing entry) else replayHit timing entry
                  | otherwise -> contactOrigin bypass


cacheOnlyMiss :: RawResponse
cacheOnlyMiss = RawResponse
  { Resp.statusCode = S.status504
  , Resp.headers = [(H.hContentLength, "0")]
  , Resp.bodyPopper = pure BS.empty
  , Resp.protocolInfo = HTTP1_1
  }


-- | Invalidate after the origin returns a successful write or redirect.
-- Once the response is received, apply the mutation before returning it.
forwardWithInvalidation :: Cache -> Transport IO -> WReq.Request BSm.BodyStream -> IO RawResponse
forwardWithInvalidation cache inner req
  | M.isSafe (WReq.method req) = sendRaw inner req
  | otherwise = mask $ \restore -> do
      reply <- restore (sendRaw inner req)
      let code = S.statusCode (Resp.statusCode reply)
      when (code >= 200 && code < 400) $
        case WURI.renderRequestURI (WReq.requestURI req) of
          Left _ -> pure ()
          Right uri -> cacheInvalidate cache (mutationKeys uri (Resp.headers reply))
            `onException` discardReply reply
      pure reply


discardReply :: RawResponse -> IO ()
discardReply reply = void $ tryAny $ case Resp.protocolInfo reply of
  HTTP2 _ -> Resp.cancelStream reply
  _ -> BSm.drainPopper (Resp.bodyPopper reply)


mutationKeys :: WURI.URI -> H.Headers -> [CacheKey]
mutationKeys uri headers = addMethods base (maybe contentKeys (`addMethods` contentKeys) location)
  where
    base = keyOfURI M.mGet uri
    fields = H.stripHopByHop headers
    location = relatedKey H.hLocation Location.locationUri
    contentKeys = case relatedKey H.hContentLocation ContentLocation.contentLocationUri of
      Just key | Just key /= location -> addMethods key []
      _ -> []
    sameOrigin key = ckScheme key == ckScheme base && ckHost key == ckHost base && ckPort key == ckPort base
    addMethods key rest = key : key{ckMethod = M.mHead} : rest
    relatedKey :: Typed.KnownHeader a => H.HeaderName -> (a -> ShortText.ShortText) -> Maybe CacheKey
    relatedKey name project = do
      wire <- either (const Nothing) id (H.lookupSingletonHeader name fields)
      field <- either (const Nothing) Just (Typed.parseFromHeaders Typed.defaultHeaderSettings (wire :| []))
      target <- WURI.resolveHttpReference (Just uri) (ShortText.toByteString (project field))
      let key = keyOfURI M.mGet target
      if sameOrigin key && ckPath key /= ckPath base then Just key else Nothing


-- ---------------------------------------------------------------------------
-- Hit / miss paths
-- ---------------------------------------------------------------------------

-- | Stamp an 'Age' header and rebuild the popper for a cache hit.
replayHit :: EntryTiming -> CacheEntry -> IO RawResponse
replayHit timing entry = do
  popper <- BSm.popperFromStrict (ceBody entry)
  pure
    RawResponse
      { Resp.statusCode = ceStatus entry
      , Resp.headers = replayHeaders timing entry
      , Resp.bodyPopper = popper
      , Resp.protocolInfo = HTTP1_1
      }


replayHeaders :: EntryTiming -> CacheEntry -> H.Headers
replayHeaders timing entry = (H.hAge, renderAgeField (etAge timing)) : case etDate timing of
  Just _ -> H.deleteHeader H.hAge fields
  Nothing -> (H.hDate, renderDateField (ceResponseTime entry)) : filter (not . isTimingField . fst) fields
  where
    fields = ceHeaders entry
{-# INLINE replayHeaders #-}


-- | Metadata for a locally satisfied conditional request (RFC 9110 §15.4.5).
notModifiedResponse :: EntryTiming -> CacheEntry -> RawResponse
notModifiedResponse timing entry =
  RawResponse S.status304 (filter (retained . fst) (replayHeaders timing entry)) (pure BS.empty) HTTP1_1
  where
    retained name =
      isTimingField name || name == H.hETag || name == H.hLastModified || name == H.hVary
        || name == H.hCacheControl || name == H.hExpires || name == H.hContentLocation


{- | Cached entry exists but is stale or marked no-cache. Issue a
conditional request; on 304 replay the cached body with the new
response's headers merged on top.
-}
revalidateOrReplace
  :: Cache
  -> Transport IO
  -> WReq.Request BSm.BodyStream
  -> CacheRequestPolicy
  -> Word64
  -> CacheKey
  -> StoredEntry
  -> IO RawResponse
revalidateOrReplace cache inner req policy generation key stored = do
  result <- tryAny (sendRevalidation (sendCacheTimed cache generation key inner) req entry)
  case result of
    Left err -> do
      now <- getCurrentTime
      case staleFallback now of
        Just timing -> replayHit timing entry
        Nothing -> throwIO err
    Right (Validated sent timed) -> do
      let reply = trRaw timed
      BSm.drainPopper (Resp.bodyPopper reply)
      freshened <- refreshCachedEntry cache generation key sent entry timed (Resp.headers reply)
      now <- getCurrentTime
      replayHit (entryTiming now (describeEntry freshened)) freshened
    Right (Refetched sent timed)
      | staleErrorStatus (Resp.statusCode reply)
      , Just timing <- staleFallback (trReceivedAt timed) -> do
          BSm.drainPopper (Resp.bodyPopper reply)
          replayHit timing entry
      | otherwise -> maybeStoreAndReturn cache generation key (Just entry) sent timed
      where
        reply = trRaw timed
  where
    entry = seEntry stored
    staleFallback now = do
      window <- minOptional (crStaleIfError policy) (ceStaleIfError entry)
      let timing = entryTiming now stored
      if staleIfErrorAllowed policy entry timing && withinStaleWindow window timing
        then Just timing
        else Nothing


{- | Background revalidation for stale-while-revalidate. Errors are
swallowed since the caller already received the stale response.
-}
revalidateSilent
  :: Cache
  -> Transport IO
  -> WReq.Request BSm.BodyStream
  -> Word64
  -> CacheKey
  -> CacheEntry
  -> IO ()
revalidateSilent cache inner req generation key entry = do
  _ <- tryAny $ do
    result <- sendRevalidation (sendCacheTimed cache generation key inner) req entry
    case result of
      Validated sent timed -> do
        let reply = trRaw timed
        BSm.drainPopper (Resp.bodyPopper reply)
        void (refreshCachedEntry cache generation key sent entry timed (Resp.headers reply))
      Refetched sent timed -> do
        buffered <- bufferForCache cache generation key (Just entry) sent timed
        case buffered of
          Nothing -> BSm.drainPopper (Resp.bodyPopper (trRaw timed))
          Just _ -> pure ()
  pure ()


data RevalidationReply
  = Validated !(WReq.Request BSm.BodyStream) !TimedResponse
  | Refetched !(WReq.Request BSm.BodyStream) !TimedResponse


data TimedResponse = TimedResponse
  { trSentAt :: !UTCTime
  , trReceivedAt :: !UTCTime
  , trRaw :: !RawResponse
  }


-- | Refresh the selected snapshot while forwarding the caller's own 304.
revalidateCallerCondition
  :: Cache
  -> Word64
  -> CacheKey
  -> Transport IO
  -> WReq.Request BSm.BodyStream
  -> CacheEntry
  -> IO RawResponse
revalidateCallerCondition cache generation key inner req entry = do
  timed <- sendCacheTimed cache generation key inner req
  let reply = trRaw timed
      fields = Resp.headers reply
      usable = H.stripHopByHop fields
      validatorField name = name == H.hETag || name == H.hLastModified
  when
    ( Resp.statusCode reply == S.status304
        && cacheMetadataRetained validatorField fields usable
        && callerResponseValidates entry (trReceivedAt timed) usable
    ) $
      void (refreshCachedEntry cache generation key req entry timed usable)
        `onException` discardReply reply
  pure reply


-- | Identify the one stored variant this cache keeps for a primary key,
-- using the validators in the 304 (RFC 9111 §4.3.4).
callerResponseValidates :: CacheEntry -> UTCTime -> H.Headers -> Bool
callerResponseValidates stored now fields = case (readETag fields, readLastModified now fields) of
  (Right (Just tag), Right _) -> case readETag (ceHeaders stored) of
    Right (Just previous) -> case tag of
      ETag.StrongETagBytes{} -> ETag.strongMatch tag previous
      ETag.WeakETagBytes{} -> ETag.weakMatch tag previous
    _ -> False
  (Right Nothing, Right (Just date)) -> storedDate == Right (Just date)
  (Right Nothing, Right Nothing) ->
    readETag (ceHeaders stored) == Right Nothing && storedDate == Right Nothing
  _ -> False
  where
    storedDate = readLastModified (ceResponseTime stored) (ceHeaders stored)


sendTimed :: Transport IO -> WReq.Request BSm.BodyStream -> IO TimedResponse
sendTimed inner req = do
  sentAt <- getCurrentTime
  reply <- sendRaw inner req
  receivedAt <- getCurrentTime
  pure (TimedResponse sentAt receivedAt reply)


-- | Observe only the GET variant selected by the outbound HEAD. Take its
-- snapshot before the exchange so a late reply cannot displace newer data.
sendCacheTimed
  :: Cache
  -> Word64
  -> CacheKey
  -> Transport IO
  -> WReq.Request BSm.BodyStream
  -> IO TimedResponse
sendCacheTimed cache generation key inner req
  | ckMethod key /= M.mHead = sendTimed inner req
  | otherwise = do
      let getKey = key{ckMethod = M.mGet}
      selected <- lookupEntry cache getKey (WReq.headers req)
      timed <- sendTimed inner req
      let reply = trRaw timed
      case selected of
        Just stored | Resp.statusCode reply == S.status200 ->
          freshenFromHead cache generation getKey req (seEntry stored) timed
            `onException` discardReply reply
        _ -> pure ()
      pure timed


freshenFromHead
  :: Cache
  -> Word64
  -> CacheKey
  -> WReq.Request BSm.BodyStream
  -> CacheEntry
  -> TimedResponse
  -> IO ()
freshenFromHead cache generation key req entry timed
  | ceStatus entry == S.status200
  , cacheMetadataRetained validationField originHeaders usableHeaders
  , headMatchesRepresentation (trReceivedAt timed) entry usableHeaders =
      void (refreshCachedEntry cache generation key req entry timed usableHeaders)
  | otherwise = cacheUpdate cache generation key (Just entry) Nothing
  where
    originHeaders = Resp.headers (trRaw timed)
    usableHeaders = H.stripHopByHop originHeaders
    validationField name = name == H.hETag || name == H.hLastModified || name == H.hContentLength


-- A matching validator is needed before replacing GET metadata. A length
-- reported by HEAD also needs to match the stored representation's length.
headMatchesRepresentation :: UTCTime -> CacheEntry -> H.Headers -> Bool
headMatchesRepresentation received entry fields =
  case (readETag fields, readLastModified received fields, readLength fields) of
    (Right tag, Right modified, Right len) ->
      not (isNothing tag && isNothing modified)
        && matches tag (readETag stored)
        && matches modified (readLastModified (ceResponseTime entry) stored)
        && matches len (readLength stored)
    _ -> False
  where
    stored = ceHeaders entry
    readLength headers = H.lookupSingletonHeader H.hContentLength headers >>=
      traverse (Typed.parseFromHeaders @ContentLength.ContentLength Typed.defaultHeaderSettings . (:| []))
    matches Nothing _ = True
    matches (Just expected) actual = actual == Right (Just expected)


-- | Send a cache-owned validation request. If the 304 cannot identify the
-- stored representation, drain it before sending the original bodyless
-- request once. Use the actual sent headers when building the next Vary key.
sendRevalidation
  :: (WReq.Request BSm.BodyStream -> IO TimedResponse)
  -> WReq.Request BSm.BodyStream
  -> CacheEntry
  -> IO RevalidationReply
sendRevalidation send req entry = case validatorsForVariant entry of
  CacheValidators Nothing Nothing -> fetchOriginal
  validators -> do
    let sent = attachValidators validators req
    timed <- send sent
    let reply = trRaw timed
    case S.statusCode (Resp.statusCode reply) of
      304
        | cacheMetadataRetained (const False) originHeaders usableHeaders
        , responseValidates validators (trReceivedAt timed) usableHeaders ->
            pure (Validated sent timed{trRaw = reply{Resp.headers = usableHeaders}})
        | otherwise -> do
            BSm.drainPopper (Resp.bodyPopper reply)
            fetchOriginal
        where
          originHeaders = Resp.headers reply
          usableHeaders = H.stripHopByHop originHeaders
      _ -> pure (Refetched sent timed)
  where
    fetchOriginal = Refetched req <$> send req


refreshCachedEntry
  :: Cache
  -> Word64
  -> CacheKey
  -> WReq.Request BSm.BodyStream
  -> CacheEntry
  -> TimedResponse
  -> H.Headers
  -> IO CacheEntry
refreshCachedEntry cache generation key req entry timed fresh = do
  let merged = mergeStoredHeaders (ceHeaders entry) fresh
      directives = parseCacheControl merged
      build = buildEntry timed (ceStatus entry) merged (ceBody entry) directives
  case extractVaryFields (WReq.headers req) merged of
    Just fields | HCC.NoStore `notElem` directives -> do
      let freshened = build fields
      cacheUpdate cache generation key (Just entry) (Just freshened)
      pure freshened
    _ -> do
      cacheUpdate cache generation key (Just entry) Nothing
      pure (build [])


{- | Either store the response and pass it through, or pass it
through unchanged when it's not storable.
-}
maybeStoreAndReturn
  :: Cache
  -> Word64
  -> CacheKey
  -> Maybe CacheEntry
  -> WReq.Request BSm.BodyStream
  -> TimedResponse
  -> IO RawResponse
maybeStoreAndReturn cache generation key previous req timed = do
  let raw = trRaw timed
  buffered <- bufferForCache cache generation key previous req timed
  case buffered of
    Nothing -> pure raw
    Just body -> do
      popper <- BSm.popperFromStrict body
      pure raw {Resp.bodyPopper = popper}


-- | Buffer a candidate for storage. 'Nothing' leaves the response popper
-- untouched. A full replacement retires the revalidated entry even when
-- storage policy or the size cap prevents storing the new response.
bufferForCache
  :: Cache
  -> Word64
  -> CacheKey
  -> Maybe CacheEntry
  -> WReq.Request BSm.BodyStream
  -> TimedResponse
  -> IO (Maybe ByteString)
bufferForCache cache generation key previous req timed
  | responseIsStorable raw
  , Just hdrs <- cacheResponseHeaders (Resp.headers raw)
  , let directives = parseCacheControl hdrs
  , HCC.NoStore `notElem` directives
  , Just fields <- extractVaryFields (WReq.headers req) hdrs = do
      body <- BSm.popperBytes (Resp.bodyPopper raw)
      if BS.length body <= ccMaxBodyBytes (cacheConfig cache)
        then cacheUpdate cache generation key previous (Just (buildEntry timed (Resp.statusCode raw) hdrs body directives fields))
        else retirePrevious
      pure (Just body)
  | code >= 200 && code < 500 && code /= 206 && code /= 304 = do
      retirePrevious `onException` discardReply raw
      pure Nothing
  | otherwise = pure Nothing
  where
    raw = trRaw timed
    code = S.statusCode (Resp.statusCode raw)
    retirePrevious = case previous of
      Nothing -> pure ()
      Just _ -> cacheUpdate cache generation key previous Nothing


-- | The HTTP/1 transport removes chunk framing. Additional transfer
-- codings can still describe the bytes delivered by the body popper.
cacheResponseHeaders :: H.Headers -> Maybe H.Headers
cacheResponseHeaders headers
  | not (any (H.isHopByHop . fst) headers) = Just headers
  | not (cacheMetadataRetained (const False) headers retained) = Nothing
  | otherwise = case H.lookupHeaders H.hTransferEncoding headers of
      [] -> Just retained
      ["chunked"] -> dechunked
      wire : rest ->
        case Typed.parseFromHeaders Typed.defaultHeaderSettings (wire :| rest) of
          Right coding@(TransferEncoding.TransferEncoding (_ :| []))
            | TransferEncoding.transferEncodingEndsInChunked coding -> dechunked
          _ -> Nothing
  where
    retained = H.stripHopByHop headers
    dechunked
      | H.hasHeader H.hContentLength retained = Just (H.deleteHeader H.hContentLength retained)
      | otherwise = Just retained


-- A Connection option on representation or reuse metadata makes the
-- response unsuitable for storing or freshening a cached representation.
cacheMetadataRetained :: (H.HeaderName -> Bool) -> H.Headers -> H.Headers -> Bool
cacheMetadataRetained additionallyRequired original retained = not (H.hasHeader H.hConnection original && any lost original)
  where
    lost (name, _) =
      ( name == H.hContentEncoding || name == H.hContentRange
          || name == H.hCacheControl || name == H.hVary || name == H.hExpires
          || isTimingField name || additionallyRequired name
      ) && not (H.hasHeader name retained)


-- ---------------------------------------------------------------------------
-- Storage helpers
-- ---------------------------------------------------------------------------

lookupEntry
  :: Cache
  -> CacheKey
  -> H.Headers
  -> IO (Maybe StoredEntry)
lookupEntry cache key headers = do
  mEntry <- cacheLookup cache key
  pure $ case mEntry of
    Just stored | varyMatch headers (seEntry stored) -> mEntry
    _ -> Nothing


{- | Returns 'True' iff the request's headers satisfy the Vary
secondary key stored in the entry (RFC 9111 §4.1).
-}
varyMatch :: H.Headers -> CacheEntry -> Bool
varyMatch reqHdrs entry = case ceVaryFields entry of
  -- An older cache entry could have ignored selecting names on later lines.
  [] -> varyNames (ceHeaders entry) == Just []
  (name, _) : fields -> BS.null (CI.foldedCase name) && matchFields fields
  where
    matchFields [] = True
    matchFields ((name, value) : fields)
      | value == absentVaryField = noField name reqHdrs && matchFields fields
      | otherwise = matchLines name value fields reqHdrs

    matchLines _ _ _ [] = False
    matchLines name expected fields ((requestName, value) : rest)
      | requestName /= name = matchLines name expected fields rest
      | value /= expected = False
      | otherwise = case fields of
          (nextName, nextValue) : remaining
            | nextName == name -> matchLines name nextValue remaining rest
          _ -> noField name rest && matchFields fields

    noField _ [] = True
    noField name ((requestName, _) : rest) = name /= requestName && noField name rest


keyOfRequest :: WReq.Request BSm.BodyStream -> Maybe CacheKey
keyOfRequest req = case WURI.renderRequestURI (WReq.requestURI req) of
  Left _ -> Nothing
  Right uri -> Just (keyOfURI (WReq.method req) uri)


keyOfURI :: M.Method -> WURI.URI -> CacheKey
keyOfURI method uri = CacheKey
  { ckScheme = WURI.uriScheme uri
  , ckHost = CI.foldCase (WURI.uriHost uri)
  , ckPort = WURI.uriPort uri
  , ckMethod = method
  , ckPath = WURI.uriPathAndQuery uri
  }


-- | Recompute the metadata whenever the stored field lines change.
buildEntry
  :: TimedResponse
  -> S.Status
  -> H.Headers
  -> ByteString
  -> [HCC.CacheControlDirective]
  -> [(H.HeaderName, H.HeaderValue)]
  -> CacheEntry
buildEntry timed status hdrs body directives varyFields =
  let maxAge =
        case [s | HCC.MaxAge s <- directives] of
          (s : _) -> Just (fromIntegral s :: NominalDiffTime)
          [] -> Nothing
      received = trReceivedAt timed
      expires = either (const Nothing) (fmap Expires.expires) $
        readTimedField H.hExpires Typed.parseFromHeaders received hdrs
      storedHeaders = case responseDate received hdrs of
        Just _ -> hdrs
        Nothing -> H.insertHeader H.hDate (renderDateField received) hdrs
      immutable = HCC.Immutable `elem` directives
      mustRevalidate = HCC.MustRevalidate `elem` directives
      noCacheTag = any isNoCache directives
      swr =
        case [s | HCC.StaleWhileRevalidate s <- directives] of
          (s : _) -> Just (fromIntegral s :: NominalDiffTime)
          [] -> Nothing
      sie =
        case [s | HCC.StaleIfError s <- directives] of
          (s : _) -> Just (fromIntegral s :: NominalDiffTime)
          [] -> Nothing
  in CacheEntry
         { ceStatus = status
         , ceHeaders = storedHeaders
         , ceBody = body
         , ceRequestTime = trSentAt timed
         , ceResponseTime = received
         , ceMaxAge = maxAge
         , ceExpiresAt = expires
         , ceImmutable = immutable
         , ceMustRevalidate = mustRevalidate
         , ceNoCache = noCacheTag
         , ceVaryFields = varyFields
         , ceStaleWhileRevalidate = swr
         , ceStaleIfError = sie
         }
  where
    isNoCache (HCC.NoCache _) = True
    isNoCache _ = False


-- | Build a secondary key from the request field lines named by @Vary@.
extractVaryFields
  :: H.Headers
  -> H.Headers
  -> Maybe [(H.HeaderName, H.HeaderValue)]
extractVaryFields reqHdrs respHdrs = do
  names <- varyNames respHdrs
  fields <- traverse snapshot (nubOrd names)
  pure (varySnapshotMarker : concat fields)
  where
    snapshot name = case H.lookupHeaders name reqHdrs of
      [] -> Just [(name, absentVaryField)]
      values -> traverse (fieldLine name) values
    fieldLine name value
      | BS.notElem 0 value = Just (name, value)
      | otherwise = Nothing


-- A wire field name cannot be empty. The initial row distinguishes current
-- snapshots from older cache entries.
varySnapshotMarker :: H.Header
varySnapshotMarker = ("", "")


-- A present field has one row per line, grouped by name. NUL represents an
-- absent field and cannot occur in a wire field value.
absentVaryField :: ByteString
absentVaryField = "\0"


varyNames :: H.Headers -> Maybe [H.HeaderName]
varyNames = findFirst
  where
    findFirst [] = Just []
    findFirst ((name, value) : rest)
      | name /= H.hVary = findFirst rest
      | otherwise =
          case Typed.parseFromHeaders Typed.defaultHeaderSettings (value :| H.lookupHeaders H.hVary rest) of
            Right Vary.VaryEmpty -> Just []
            Right (Vary.Vary directives) -> traverse selectingName (NE.toList directives)
            Left _ -> Nothing
    selectingName Vary.VaryAll = Nothing
    selectingName (Vary.VaryHeader name) = Just (FieldName.toCIByteString name)


-- ---------------------------------------------------------------------------
-- Freshness
-- ---------------------------------------------------------------------------

data EntryTiming = EntryTiming
  { etDate :: !(Maybe UTCTime)
  , etAge :: !NominalDiffTime
  , etLifetime :: !NominalDiffTime
  }


-- | A decoded snapshot belongs to the exact CacheEntry returned by the
-- store. A custom backend leaves it lazy until the Vary check succeeds.
data StoredEntry = StoredEntry
  { seEntry :: !CacheEntry
  , seTiming :: ~EntryTiming
  }


describeEntry :: CacheEntry -> StoredEntry
describeEntry entry = StoredEntry entry (initialTiming entry)


describeLoadedEntry :: CacheEntry -> Maybe StoredEntry
describeLoadedEntry entry
  -- Older backends can have freshness or Vary snapshots derived from
  -- connection-scoped fields. Rebuild those entries from the origin.
  | any (H.isHopByHop . fst) (ceHeaders entry) = Nothing
  | otherwise = Just (describeEntry entry)


-- | Use the conservative initial age from RFC 9111 §4.2.3. The same
-- current age drives freshness, stale extension windows and Age on replay.
entryTiming :: UTCTime -> StoredEntry -> EntryTiming
entryTiming now stored = initial{etAge = etAge initial + residentTime}
  where
    initial = seTiming stored
    residentTime = max 0 (utcDifference now (ceResponseTime (seEntry stored)))


initialTiming :: CacheEntry -> EntryTiming
initialTiming entry = EntryTiming date age lifetime
  where
    received = ceResponseTime entry
    fields = ceHeaders entry
    date = responseDate received fields
    origin = fromMaybe received date
    apparentAge = max 0 (utcDifference received origin)
    responseDelay = max 0 (utcDifference received (ceRequestTime entry))
    age = max apparentAge (incomingAge fields + responseDelay)
    lifetime = case ceMaxAge entry of
      Just limit -> limit
      Nothing -> maybe 0 (`utcDifference` origin) (ceExpiresAt entry)


-- | On the same UTC day, the day offsets cancel. Clamp leap seconds as
-- diffUTCTime does when converting to nominal time.
utcDifference :: UTCTime -> UTCTime -> NominalDiffTime
utcDifference a@(UTCTime dayA timeA) b@(UTCTime dayB timeB)
  | dayA == dayB = secondsToNominalDiffTime (MkFixed (nominalPicos timeA - nominalPicos timeB))
  | otherwise = diffUTCTime a b
  where
    nominalPicos = min (86400 * picosecondsPerSecond) . diffTimeToPicoseconds
{-# INLINE utcDifference #-}


picosecondsPerSecond :: Integer
picosecondsPerSecond = 1000000000000


data CacheDecision = UseCache | UseCacheAndRevalidate | ContactOrigin


cacheDecision :: CacheRequestPolicy -> CacheEntry -> EntryTiming -> CacheDecision
cacheDecision policy entry timing
  | requiresValidation policy entry timing = ContactOrigin
  | etAge timing < etLifetime timing = UseCache
  | ceMustRevalidate entry = ContactOrigin
  | otherwise = case crMaxStale policy of
      AnyStaleness -> UseCache
      StaleUpTo limit
        | staleness timing <= limit -> UseCache
        | otherwise -> ContactOrigin
      NoStaleAllowance
        | isNothing (crMaxAge policy)
        , Just window <- ceStaleWhileRevalidate entry
        , withinStaleWindow window timing -> UseCacheAndRevalidate
        | otherwise -> ContactOrigin


requiresValidation :: CacheRequestPolicy -> CacheEntry -> EntryTiming -> Bool
requiresValidation policy entry timing =
  crNoCache policy || ceNoCache entry
    || maybe False (etAge timing >) (crMaxAge policy)
    || maybe False lacksFreshness (crMinFresh policy)
  where
    lacksFreshness minimumFresh = remaining <= 0 || remaining < minimumFresh
    remaining = etLifetime timing - etAge timing


staleIfErrorAllowed :: CacheRequestPolicy -> CacheEntry -> EntryTiming -> Bool
staleIfErrorAllowed policy entry timing =
  not (ceMustRevalidate entry || requiresValidation policy entry timing)
    && case crMaxStale policy of
      AnyStaleness -> True
      StaleUpTo limit -> staleness timing <= limit
      NoStaleAllowance -> isNothing (crMaxAge policy)


staleness :: EntryTiming -> NominalDiffTime
staleness timing = etAge timing - etLifetime timing


withinStaleWindow :: NominalDiffTime -> EntryTiming -> Bool
withinStaleWindow window timing =
  window > 0 && elapsed >= 0 && elapsed <= window
  where
    elapsed = staleness timing


staleErrorStatus :: S.Status -> Bool
staleErrorStatus status = case S.statusCode status of
  500 -> True
  502 -> True
  503 -> True
  504 -> True
  _ -> False


incomingAge :: H.Headers -> NominalDiffTime
incomingAge hdrs = case H.lookupHeader H.hAge hdrs of
  Nothing -> 0
  Just value -> case Typed.parseFromHeaders Typed.defaultHeaderSettings (value :| []) of
    Right (Age.Age seconds) -> fromIntegral seconds
    Left _ -> 0


responseDate :: UTCTime -> H.Headers -> Maybe UTCTime
responseDate received = either (const Nothing) (fmap Date.date) . readTimedField H.hDate Typed.parseFromHeaders received


renderAgeField :: NominalDiffTime -> ByteString
renderAgeField age
  | picos < picosecondsPerSecond = "0"
  | otherwise = Typed.renderToHeaders Typed.defaultHeaderSettings (Age.Age seconds)
  where
    MkFixed picos = nominalDiffTimeToSeconds age
    seconds = fromInteger (min (toInteger (maxBound :: Word32)) (picos `quot` picosecondsPerSecond))


renderDateField :: UTCTime -> ByteString
renderDateField = Typed.renderToHeaders Typed.defaultHeaderSettings . Date.Date


isTimingField :: H.HeaderName -> Bool
isTimingField name = name == H.hAge || name == H.hDate


-- ---------------------------------------------------------------------------
-- Header / directive helpers
-- ---------------------------------------------------------------------------

cacheableMethod :: M.Method -> Bool
cacheableMethod m = m == M.mGet || m == M.mHead


data CacheRequestMode = Unconditional | EvaluateConditions | BypassCache
  deriving stock (Eq)


-- | Only the origin handles write, range, WebDAV and extension preconditions.
cacheRequestMode :: WReq.Request BSm.BodyStream -> CacheRequestMode
cacheRequestMode req
  | not (cacheableMethod (WReq.method req)) || BSm.knownSize (WReq.body req) /= Just 0 = BypassCache
  | otherwise = scan Unconditional (WReq.headers req)
  where
    scan !mode [] = mode
    scan !mode ((name, _):rest)
      | len < 2 = scan mode rest
      | otherwise = case BSU.unsafeIndex folded 0 of
          0x69 -- "if" or "if-"
            | BSU.unsafeIndex folded 1 == 0x66
            , len == 2 || BSU.unsafeIndex folded 2 == 0x2d ->
                if cacheConditionField name then scan EvaluateConditions rest else BypassCache
          0x72 | folded == "range" -> BypassCache
          _ -> scan mode rest
      where
        folded = CI.foldedCase name
        len = BS.length folded


cacheConditionField :: H.HeaderName -> Bool
cacheConditionField name = name == H.hIfNoneMatch || name == H.hIfModifiedSince


-- | Evaluate the applicable condition for a reusable 200 response.
-- A malformed entity-tag list is passed through to the origin. RFC 9110
-- calls for invalid or repeated If-Modified-Since dates to be ignored.
cachedNotModified :: UTCTime -> EntryTiming -> CacheEntry -> H.Headers -> Either String Bool
cachedNotModified now timing entry fields = case H.lookupHeaders H.hIfNoneMatch fields of
  first : rest -> do
    tagList <- Typed.parseFromHeaders Typed.defaultHeaderSettings (first :| rest)
    pure $ case tagList of
      IfNoneMatch.IfNoneMatchAny -> True
      IfNoneMatch.IfNoneMatchTags tags -> case readETag stored of
        Right (Just tag) -> any (ETag.weakMatch tag) tags
        _ -> False
      IfNoneMatch.IfNoneMatchNoTags -> False
  [] -> pure $ case readTimedField H.hIfModifiedSince Typed.parseFromHeaders now fields of
    Right (Just (IfModifiedSince.IfModifiedSince since)) -> case readLastModified received stored of
      Right (Just modified) -> modified <= since
      _ -> case etDate timing of
        Just date -> date <= since
        -- The synthesized Date represents receipt rounded to wire precision.
        Nothing -> received < addUTCTime 1 since
    _ -> False
  where
    received = ceResponseTime entry
    stored = ceHeaders entry


data StalenessLimit
  = NoStaleAllowance
  | AnyStaleness
  | StaleUpTo !NominalDiffTime


data CacheRequestPolicy = CacheRequestPolicy
  { crNoStore :: !Bool
  , crNoCache :: !Bool
  , crOnlyIfCached :: !Bool
  , crMaxAge :: !(Maybe NominalDiffTime)
  , crMinFresh :: !(Maybe NominalDiffTime)
  , crMaxStale :: !StalenessLimit
  , crStaleIfError :: !(Maybe NominalDiffTime)
  }


defaultRequestPolicy :: CacheRequestPolicy
defaultRequestPolicy = CacheRequestPolicy False False False Nothing Nothing NoStaleAllowance Nothing


-- | One scan for the common request without cache instructions. Only
-- consult legacy Pragma when every Cache-Control field is absent.
requestCachePolicy :: H.Headers -> CacheRequestPolicy
requestCachePolicy = findPolicy
  where
    findPolicy [] = defaultRequestPolicy
    findPolicy ((name, value) : rest) = case BS.length folded of
      13 | BSU.unsafeIndex folded 0 == 0x63, folded == "cache-control" ->
        readControl value (H.lookupHeaders H.hCacheControl rest)
      6 | BSU.unsafeIndex folded 0 == 0x70, folded == "pragma" ->
        case H.lookupHeaders H.hCacheControl rest of
          first : later -> readControl first later
          []
            | any pragmaNoCache (value : H.lookupHeaders pragmaName rest) -> defaultRequestPolicy{crNoCache = True}
            | otherwise -> defaultRequestPolicy
      _ -> findPolicy rest
      where
        folded = CI.foldedCase name
    pragmaName = "pragma"
    readControl first = foldl' applyControl (applyControl defaultRequestPolicy first)
    applyControl policy = foldl' applyRequestDirective policy . cacheControlField


pragmaNoCache :: H.HeaderValue -> Bool
pragmaNoCache value
  | value == "no-cache" = True
  | otherwise = case Typed.parseFromHeaders Typed.defaultHeaderSettings (value :| []) of
      Right (Pragma.Pragma directives) -> any isNoCache directives
      Left _ -> False
  where
    isNoCache directive =
      isNothing (Pragma.pragmaValue directive)
        && CI.foldCase (ShortText.toByteString (Pragma.pragmaName directive)) == "no-cache"


applyRequestDirective :: CacheRequestPolicy -> HCC.CacheControlDirective -> CacheRequestPolicy
applyRequestDirective policy = \case
  HCC.NoStore -> policy{crNoStore = True}
  HCC.NoCache _ -> policy{crNoCache = True}
  HCC.OnlyIfCached -> policy{crOnlyIfCached = True}
  HCC.MaxAge limit -> policy{crMaxAge = minOptional (Just (fromIntegral limit)) (crMaxAge policy)}
  HCC.MinFresh limit ->
    let requested = fromIntegral limit
    in policy{crMinFresh = Just (maybe requested (max requested) (crMinFresh policy))}
  HCC.MaxStale limit -> policy{crMaxStale = restrictStaleness limit (crMaxStale policy)}
  HCC.StaleIfError limit -> policy{crStaleIfError = minOptional (Just (fromIntegral limit)) (crStaleIfError policy)}
  _ -> policy
  where
    restrictStaleness Nothing NoStaleAllowance = AnyStaleness
    restrictStaleness Nothing existing = existing
    restrictStaleness (Just limit) (StaleUpTo existing) = StaleUpTo (min (fromIntegral limit) existing)
    restrictStaleness (Just limit) _ = StaleUpTo (fromIntegral limit)


minOptional :: Ord a => Maybe a -> Maybe a -> Maybe a
minOptional (Just x) (Just y) = Just (min x y)
minOptional x Nothing = x
minOptional Nothing y = y


{- | RFC 9111 §3: storable by default for 200, 203, 204, 300, 301,
308, 404, 405, 410, 414, 501. We're conservative and only
store the codes that the audit list cares about (200, 203, 301,
410) — the others rarely benefit a non-shared cache.
-}
responseIsStorable :: RawResponse -> Bool
responseIsStorable raw =
  S.statusCode (Resp.statusCode raw) `elem` [200, 203, 301, 410]


parseCacheControl :: [(H.HeaderName, H.HeaderValue)] -> [HCC.CacheControlDirective]
parseCacheControl hdrs = concatMap cacheControlField (H.lookupHeaders H.hCacheControl hdrs)


cacheControlField :: H.HeaderValue -> [HCC.CacheControlDirective]
cacheControlField = either (const []) id . HCC.parseCacheControlDirectives


data CacheValidators = CacheValidators
  { cvTag :: !(Maybe CachedETag)
  , cvModified :: !(Maybe UTCTime)
  }


data CachedETag = StrongValidator !ByteString | WeakValidator !ByteString


cachedETag :: ETag.EntityTag -> CachedETag
cachedETag tag = case tag of
  ETag.StrongETagBytes{} -> StrongValidator wire
  ETag.WeakETagBytes{} -> WeakValidator wire
  where
    wire = Conditional.renderETag tag


-- | Cache-owned requests start without conditions. Keep a condition absent
-- when its field selects the variant. If the entry has a usable ETag,
-- validation requires sending that tag. The date can be omitted independently.
validatorsForVariant :: CacheEntry -> CacheValidators
validatorsForVariant entry
  | Just _ <- tag, selectsBy H.hIfNoneMatch = CacheValidators Nothing Nothing
  | otherwise = CacheValidators tag date
  where
    fields = ceHeaders entry
    tag = cachedETag <$> usable (readETag fields)
    date
      | selectsBy H.hIfModifiedSince = Nothing
      | otherwise = usable (readLastModified (ceResponseTime entry) fields)
    selectsBy name = any ((== name) . fst) (ceVaryFields entry)
    usable = either (const Nothing) id


readETag :: H.Headers -> Either String (Maybe ETag.EntityTag)
readETag hdrs = H.lookupSingletonHeader H.hETag hdrs >>= traverse (fmap ETag.etag . ETag.parseETag)


readLastModified :: UTCTime -> H.Headers -> Either String (Maybe UTCTime)
readLastModified now = fmap (fmap LastModified.lastModified) . readTimedField H.hLastModified Typed.parseFromHeaders now


readTimedField
  :: H.HeaderName
  -> (Typed.HeaderSettings -> NonEmpty ByteString -> Either String a)
  -> UTCTime
  -> H.Headers
  -> Either String (Maybe a)
readTimedField name parse now hdrs = H.lookupSingletonHeader name hdrs >>= traverse (parse settings . (:| []))
  where
    settings = Typed.withHeaderDateReference now (Typed.withCaseInsensitiveDates Typed.defaultHeaderSettings)
{-# INLINE readTimedField #-}


attachValidators :: CacheValidators -> WReq.Request a -> WReq.Request a
attachValidators validators req = maybe withDate addTag (cvTag validators)
  where
    withDate = maybe req (\date -> Conditional.ifModifiedSince date req) (cvModified validators)
    addTag (StrongValidator wire) = WReq.setHeader H.hIfNoneMatch wire withDate
    addTag (WeakValidator wire) = WReq.setHeader H.hIfNoneMatch wire withDate


-- | RFC 9111 §4.3.4 selects the stored response by strong ETag first,
-- then weak ETag. A matching modification date can validate a request
-- made with If-Modified-Since alone.
responseValidates :: CacheValidators -> UTCTime -> H.Headers -> Bool
responseValidates stored now hdrs = case H.lookupSingletonHeader H.hETag hdrs of
  Right (Just fresh) -> maybe False (\tag -> matchesETag tag fresh) (cvTag stored) && validDate
  Right Nothing -> isNothing (cvTag stored) && case readLastModified now hdrs of
    Right (Just date) -> cvModified stored == Just date
    _ -> False
  Left _ -> False
  where
    validDate = case readLastModified now hdrs of
      Right _ -> True
      Left _ -> False


-- | A match against the typed validator's complete wire spelling also
-- validates the candidate's grammar, without copying its opaque bytes.
-- Only a weak response tag can match a tag of the opposite strength.
matchesETag :: CachedETag -> ByteString -> Bool
matchesETag tag value = case tag of
  WeakValidator wire -> candidate == wire
  StrongValidator wire -> candidate == wire ||
    (BS.length candidate - BS.length wire == 2 && BS.stripPrefix "W/" candidate == Just wire)
  where
    candidate = BS.dropWhileEnd isOWS (BS.dropWhile isOWS value)
    isOWS byte = byte == 0x20 || byte == 0x09


{- | RFC 9111 §3.2: refresh the cached entry by replacing each eligible
field carried by a successful conditional or HEAD response. Content-Length,
Content-Encoding, and Content-Range describe the stored bytes and
keep their stored values, including absence after decompression.
Digest fields likewise remain tied to the bytes in the cache. Age and
Date belong to the new exchange, so old values are dropped even when omitted.
-}
mergeStoredHeaders
  :: [(H.HeaderName, H.HeaderValue)]
  -> [(H.HeaderName, H.HeaderValue)]
  -> [(H.HeaderName, H.HeaderValue)]
mergeStoredHeaders cached fresh =
  let updates = filter (\(n, _) -> n /= H.hContentLength && n /= H.hContentEncoding && n /= H.hContentRange && not (isBodyDigestField n)) fresh
      kept = filter (\(n, _) -> not (isTimingField n) && not (H.hasHeader n updates)) cached
  in kept <> updates
