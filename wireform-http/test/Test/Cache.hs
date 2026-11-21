{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for "Network.HTTP.Client.Cache". Focused on the
'CacheStore' contract and on the behaviours that distinguish the
RFC 9111 cache from a passthrough middleware (freshness, ETag
revalidation, no-store / no-cache).

The tests build their own mock transport that records every
upstream call into an 'IORef'; that lets us assert exactly when
the cache hit vs missed.
-}
module Test.Cache (tests) where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, throwIO)
import Control.Monad (replicateM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower, toUpper)
import Data.IORef
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Cache
import Network.HTTP.Client.Compression (withDecompression)
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport (Transport (..))
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.HttpDate (formatHttpDate)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

{- | A canned response factory: each call to 'mkTransport' takes a
list of @(status, headers, body)@ tuples and returns a
'Transport' that returns them in order, recording every call
into a counter.
-}
mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef Int, IORef [Request BSm.BodyStream])
mkTransport canned = do
  responsesRef <- newIORef canned
  callsRef <- newIORef (0 :: Int)
  requestsRef <- newIORef ([] :: [Request BSm.BodyStream])
  let go = Transport $ \req -> do
        atomicModifyIORef' callsRef (\n -> (n + 1, ()))
        atomicModifyIORef' requestsRef (\xs -> (req : xs, ()))
        next <- atomicModifyIORef' responsesRef $ \rs -> case rs of
          (r : rest) -> (rest, r)
          [] -> ([], (S.status500, [], "no more canned responses"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (go, callsRef, requestsRef)


makeRequest :: ByteString -> IO (Request BSm.BodyStream)
makeRequest pathBs =
  case WURI.parseTemplate (BS8.unpack ("http://example.com" <> pathBs)) of
    Left err -> error ("makeRequest: bad URI: " <> show err)
    Right t -> prepareRequest [] (get t)


drainBody :: RawResponse -> IO ByteString
drainBody = BSm.popperBytes . Resp.bodyPopper


-- ---------------------------------------------------------------------------
-- Hit + miss accounting
-- ---------------------------------------------------------------------------

unit_fresh_hits :: Spec
unit_fresh_hits = it "fresh response satisfies subsequent reads" $ do
  let payload = "hello"
  (t, calls, _) <-
    mkTransport
      [
        ( S.status200
        ,
          [ (H.hCacheControl, "max-age=60")
          , (H.hContentLength, BS8.pack (show (BS.length payload)))
          ]
        , payload
        )
      ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  r1 <- makeRequest "/a" >>= withC
  body1 <- drainBody r1
  r2 <- makeRequest "/a" >>= withC
  body2 <- drainBody r2
  n <- readIORef calls
  n `shouldBe` 1
  body1 `shouldBe` payload
  body2 `shouldBe` payload
  size <- cacheSize cache
  size `shouldBe` 1


unit_replays_end_to_end_headers :: Spec
unit_replays_end_to_end_headers = it "stores end-to-end headers and replays the dechunked body" $ do
  let fields =
        [ (H.hCacheControl, "max-age=60")
        , (H.hConnection, "keep-alive, X-First")
        , ("cOnNeCtIoN", "\tX-Second ")
        , ("Keep-Alive", "timeout=5")
        , ("X-First", "one")
        , ("X-Second", "two")
        , (H.hTransferEncoding, "ChUnKeD")
        , (H.hContentLength, "50")
        , (H.hTrailer, "Content-Digest")
        , ("X-End-To-End", "original")
        , ("X-End-To-End", "second")
        ]
  (t, calls, _) <- mkTransport [(S.status200, fields, "dechunked")]
  store <- newInMemoryStore 4
  inserted <- newIORef Nothing
  let observing = store{csInsert = \key entry -> writeIORef inserted (Just entry) >> csInsert store key entry}
      cache = newCacheWith observing defaultCacheConfig
  req <- makeRequest "/hop-fields"
  let fetch = sendRaw (withCache cache t) req
      expected = [(H.hCacheControl, "max-age=60"), ("X-End-To-End", "original"), ("X-End-To-End", "second")]
      withoutTiming = filter (\(name, _) -> name /= H.hAge && name /= H.hDate)
  original <- fetch
  Resp.headers original `shouldBe` fields
  drainBody original `shouldReturn` "dechunked"
  fmap (withoutTiming . ceHeaders) <$> readIORef inserted `shouldReturn` Just expected
  replay <- fetch
  withoutTiming (Resp.headers replay) `shouldBe` expected
  drainBody replay `shouldReturn` "dechunked"
  readIORef calls `shouldReturn` 1


unit_bypasses_unusable_response_metadata :: Spec
unit_bypasses_unusable_response_metadata = it "passes through unsupported transfer codings and connection-scoped representation metadata" $ do
  let check fields = do
        let headers = (H.hCacheControl, "max-age=60") : fields
        (t, calls, _) <- mkTransport [(S.status200, headers, "first"), (S.status200, [], "second")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/unusable-response-fields"
        let fetch = sendRaw (withCache cache t) req
        original <- fetch
        Resp.headers original `shouldBe` headers
        drainBody original `shouldReturn` "first"
        cacheSize cache `shouldReturn` 0
        (fetch >>= drainBody) `shouldReturn` "second"
        readIORef calls `shouldReturn` 2
  check [(H.hTransferEncoding, "gzip"), (H.hTransferEncoding, "chunked")]
  check [(H.hConnection, "Content-Encoding"), (H.hContentEncoding, "gzip")]
  check [(H.hConnection, "Vary"), (H.hVary, "Accept-Language")]
  check [(H.hConnection, "Cache-Control"), (H.hCacheControl, "no-store")]


unit_bypasses_old_hop_fields :: Spec
unit_bypasses_old_hop_fields = it "refreshes persisted entries with connection-specific headers" $ do
  let fresh = [(H.hCacheControl, "max-age=60")]
  (t, calls, _) <- mkTransport [(S.status200, fresh, "old"), (S.status200, fresh, "new")]
  store <- newInMemoryStore 4
  req <- makeRequest "/old-hop-fields"
  let fetch backend = sendRaw (withCache (newCacheWith backend defaultCacheConfig) t) req >>= drainBody
      oldStore = store{csLookup = \key -> fmap addOldFields <$> csLookup store key}
      addOldFields entry = entry{ceHeaders = (H.hConnection, "X-Old") : ("X-Old", "retained") : ceHeaders entry}
  fetch store `shouldReturn` "old"
  fetch oldStore `shouldReturn` "new"
  fetch store `shouldReturn` "new"
  readIORef calls `shouldReturn` 2


unit_cache_date_casing :: Spec
unit_cache_date_casing = it "interprets cache dates with ASCII case folding" $ do
  now <- getCurrentTime
  let oldDate = (H.hDate, BS8.map toLower (formatHttpDate (addUTCTime (-120) now)))
      expires = (H.hExpires, BS8.map toUpper (formatHttpDate (addUTCTime 120 now)))
  (t, calls, _) <- mkTransport [(S.status200, [oldDate, expires], "cached"), (S.status200, [], "origin")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/folded-cache-dates"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "cached"
  reply <- fetch
  drainBody reply `shouldReturn` "cached"
  expectAgeBetween 120 149 reply
  readIORef calls `shouldReturn` 1
  expectReplacement [oldDate, (H.hCacheControl, "max-age=60")]


unit_replays_corrected_age :: Spec
unit_replays_corrected_age = it "includes upstream Age, response delay and resident time on a cache hit" $ do
  let headers = [(H.hCacheControl, "max-age=300"), (H.hAge, "10, 200"), (H.hAge, "999")]
  (t, calls, _) <- mkTransport [(S.status200, headers, "cached")]
  store <- newInMemoryStore 4
  let shifted = store{csInsert = insertEntry}
      insertEntry key entry = csInsert store key entry
        { ceRequestTime = addUTCTime (-40) (ceRequestTime entry)
        , ceResponseTime = addUTCTime (-15) (ceResponseTime entry)
        }
      cache = newCacheWith shifted defaultCacheConfig
  req <- makeRequest "/corrected-age"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "cached"
  reply <- fetch
  drainBody reply `shouldReturn` "cached"
  readIORef calls `shouldReturn` 1
  expectAgeBetween 50 59 reply
  length (H.lookupHeaders H.hDate (Resp.headers reply)) `shouldBe` 1


unit_keeps_request_and_receipt_times :: Spec
unit_keeps_request_and_receipt_times = it "records the request and header receipt times on misses and validation" $ do
  let etag = (H.hETag, "\"old\"")
      stale = [(H.hCacheControl, "max-age=0"), etag]
      update = [(H.hCacheControl, "max-age=60"), etag]
  (inner, _, _) <- mkTransport [(S.status200, stale, "cached"), (S.status304, update, "")]
  marks <- newIORef Nothing
  recorded <- newIORef Nothing
  let t = Transport $ \req -> do
        start <- getCurrentTime
        reply <- sendRaw inner req
        finish <- clockAfter start
        writeIORef marks (Just (start, finish))
        pure reply
  store <- newInMemoryStore 4
  let observing = store{csInsert = \key entry -> writeIORef recorded (Just entry) >> csInsert store key entry}
      cache = newCacheWith observing defaultCacheConfig
      checkTimes = do
        seen <- readIORef marks
        stored <- readIORef recorded
        case (seen, stored) of
          (Just (start, finish), Just entry) -> do
            (ceRequestTime entry <= start) `shouldBe` True
            (ceResponseTime entry >= finish) `shouldBe` True
          _ -> expectationFailure "Missing exchange timestamps"
  req <- makeRequest "/age-exchange-times"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "cached"
  checkTimes
  fetch `shouldReturn` "cached"
  checkTimes
  where
    clockAfter :: UTCTime -> IO UTCTime
    clockAfter start = do
      now <- getCurrentTime
      if now > start then pure now else clockAfter start


unit_age_and_date_can_expire_a_response :: Spec
unit_age_and_date_can_expire_a_response = it "uses both upstream Age and the origin Date when checking freshness" $ do
  expectReplacement [(H.hCacheControl, "max-age=30"), (H.hAge, "90")]
  expectReplacement [(H.hCacheControl, "max-age=30"), (H.hDate, "Sun, 06 Nov 1994 08:49:37 GMT")]
  expectReplacement [(H.hCacheControl, "max-age=0, immutable")]


unit_expires_relative_to_date :: Spec
unit_expires_relative_to_date = it "uses Expires relative to Date and treats duplicate Expires as stale" $ do
  now <- getCurrentTime
  let date = formatHttpDate (addUTCTime 3600 now)
      expires = formatHttpDate (addUTCTime 3630 now)
  expectReplacement [(H.hDate, date), (H.hExpires, expires), (H.hAge, "60")]
  expectReplacement [(H.hExpires, expires), (H.hExpires, expires)]


unit_apparent_age_on_hit :: Spec
unit_apparent_age_on_hit = it "replays the greater apparent age while the explicit expiration is still in the future" $ do
  now <- getCurrentTime
  let date = formatHttpDate (addUTCTime (-90) now)
      expires = formatHttpDate (addUTCTime 60 now)
      headers = [(H.hDate, date), (H.hExpires, expires), (H.hAge, "10")]
  (t, calls, _) <- mkTransport [(S.status200, headers, "cached")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/apparent-age"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "cached"
  reply <- fetch
  drainBody reply `shouldReturn` "cached"
  expectAgeBetween 90 99 reply
  readIORef calls `shouldReturn` 1


unit_bad_age_does_not_use_later_lines :: Spec
unit_bad_age_does_not_use_later_lines = it "ignores an invalid first Age member and repairs an invalid Date on replay" $ do
  let headers = [(H.hCacheControl, "max-age=60"), (H.hAge, "bad"), (H.hAge, "999"), (H.hDate, "bad")]
  (t, calls, _) <- mkTransport [(S.status200, headers, "cached")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/bad-age-date"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "cached"
  reply <- fetch
  drainBody reply `shouldReturn` "cached"
  expectAgeBetween 0 9 reply
  case H.lookupHeaders H.hDate (Resp.headers reply) of
    [value] -> BS.length value `shouldBe` 29
    _ -> expectationFailure "Expected one corrected Date"
  readIORef calls `shouldReturn` 1


unit_validation_replaces_age :: Spec
unit_validation_replaces_age = it "restarts age using the validation response and drops an inherited Age or Date" $ do
  let etag = (H.hETag, "\"old\"")
      old = [(H.hCacheControl, "max-age=30"), (H.hAge, "500"), (H.hDate, "Sun, 06 Nov 1994 08:49:37 GMT"), etag]
      check updatedAge expectedLower expectedUpper = do
        let update = [(H.hCacheControl, "max-age=60"), etag] <> updatedAge
        (t, calls, _) <- mkTransport [(S.status200, old, "cached"), (S.status304, update, "")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/validated-age"
        let fetch = sendRaw (withCache cache t) req
        (fetch >>= drainBody) `shouldReturn` "cached"
        validated <- fetch
        drainBody validated `shouldReturn` "cached"
        expectAgeBetween expectedLower expectedUpper validated
        case H.lookupHeaders H.hDate (Resp.headers validated) of
          [value] -> (value /= "Sun, 06 Nov 1994 08:49:37 GMT") `shouldBe` True
          _ -> expectationFailure "Expected a validation Date"
        (fetch >>= drainBody) `shouldReturn` "cached"
        readIORef calls `shouldReturn` 2
  check [] 0 9
  check [(H.hAge, "3")] 3 12


unit_age_saturation_on_replay :: Spec
unit_age_saturation_on_replay = it "saturates a revalidated Age before rendering it" $ do
  let etag = (H.hETag, "\"old\"")
      old = [(H.hCacheControl, "max-age=0"), etag]
      update = [(H.hAge, "99999999999999999999"), etag]
  (t, calls, _) <- mkTransport [(S.status200, old, "cached"), (S.status304, update, "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/saturated-age"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "cached"
  reply <- fetch
  drainBody reply `shouldReturn` "cached"
  expectAgeBetween 4294967295 4294967295 reply
  readIORef calls `shouldReturn` 2


unit_age_limits_stale_use :: Spec
unit_age_limits_stale_use = it "checks stale windows against corrected age and obeys restrictive directives" $ do
  expectReplacement [(H.hCacheControl, "max-age=60, stale-while-revalidate=10"), (H.hAge, "90")]
  let check directive age code expected = do
        let cached = [(H.hCacheControl, directive), (H.hAge, age)]
        (t, calls, _) <- mkTransport [(S.status200, cached, "cached"), (code, [], "origin")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/age-stale-window"
        let fetch = sendRaw (withCache cache t) req >>= drainBody
        fetch `shouldReturn` "cached"
        fetch `shouldReturn` expected
        readIORef calls `shouldReturn` 2
  check "max-age=30, stale-if-error=20" "40" (S.Status 503) "cached"
  check "max-age=30, stale-if-error=20" "60" (S.Status 503) "origin"
  check "max-age=0, stale-if-error=60, stale-while-revalidate=60, must-revalidate" "5" (S.Status 503) "origin"
  check "max-age=0, stale-if-error=60, stale-while-revalidate=60, no-cache" "5" (S.Status 503) "origin"
  check "max-age=0, stale-if-error=60" "5" (S.Status 501) "origin"


expectReplacement :: H.Headers -> IO ()
expectReplacement initialFields = do
  (t, calls, _) <- mkTransport [(S.status200, initialFields, "cached"), (S.status200, [], "origin")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/age-replacement"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "cached"
  fetch `shouldReturn` "origin"
  readIORef calls `shouldReturn` 2


expectAgeBetween :: Integer -> Integer -> RawResponse -> IO ()
expectAgeBetween lower upper reply = case H.lookupHeaders H.hAge (Resp.headers reply) of
  [value] -> case BS8.readInteger value of
    Just (age, rest) -> (BS.null rest && age >= lower && age <= upper) `shouldBe` True
    _ -> expectationFailure "Expected decimal Age"
  _ -> expectationFailure "Expected one Age header"


unit_no_store_response :: Spec
unit_no_store_response = it "no-store on response is not cached" $ do
  let payload = "secret"
  (t, calls, _) <-
    mkTransport
      [ (S.status200, [(H.hCacheControl, "max-age=60"), (H.hCacheControl, " \t, No-StOrE ,")], payload)
      , (S.status200, [(H.hCacheControl, "no-store, max-age=60")], payload)
      ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  _ <- makeRequest "/x" >>= withC >>= drainBody
  _ <- makeRequest "/x" >>= withC >>= drainBody
  n <- readIORef calls
  n `shouldBe` 2
  size <- cacheSize cache
  size `shouldBe` 0


unit_only_if_cached :: Spec
unit_only_if_cached = it "serves an eligible cached response or returns a local 504" $ do
  let fields = [(H.hCacheControl, "max-age=60"), (H.hAge, "15"), (H.hVary, "X-Theme")]
  (t, calls, _) <- mkTransport [(S.status200, fields, "light")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/only-if-cached"
  let fetch hdrs = sendRaw (withCache cache t) req{WReq.headers = hdrs}
      only = [(H.hCacheControl, "only-if-cached")]
      light = ("X-Theme", "light")
      expects504 hdrs = do
        reply <- fetch hdrs
        Resp.statusCode reply `shouldBe` S.status504
        drainBody reply `shouldReturn` ""
  expects504 (light : only)
  readIORef calls `shouldReturn` 0
  (fetch [light] >>= drainBody) `shouldReturn` "light"
  (fetch (light : only) >>= drainBody) `shouldReturn` "light"
  expects504 (("X-Theme", "dark") : only)
  expects504 (light : (H.hRange, "bytes=0-1") : only)
  unchanged <- fetch (light : (H.hIfNoneMatch, "*") : only)
  Resp.statusCode unchanged `shouldBe` S.status304
  drainBody unchanged `shouldReturn` ""
  expects504 [light, (H.hCacheControl, "only-if-cached, no-cache")]
  expects504 [light, (H.hCacheControl, "only-if-cached, no-store")]
  expects504 [light, (H.hCacheControl, "only-if-cached, max-age=10")]
  expects504 [light, (H.hCacheControl, "only-if-cached, min-fresh=50")]
  readIORef calls `shouldReturn` 1


unit_request_age_limits :: Spec
unit_request_age_limits = it "checks request max-age and min-fresh against corrected age" $ do
  let response = "max-age=60"
  checkCacheReuse response "20" [(H.hCacheControl, "max-age=30")] "cached"
  checkCacheReuse response "20" [(H.hCacheControl, "max-age=10")] "origin"
  checkCacheReuse response "20" [(H.hCacheControl, "min-fresh=30")] "cached"
  checkCacheReuse response "20" [(H.hCacheControl, "min-fresh=50")] "origin"
  checkCacheReuse response "20" [(H.hCacheControl, "max-age=30, max-age=10")] "origin"
  checkCacheReuse response "20"
    [(H.hCacheControl, "min-fresh=50"), (H.hCacheControl, "min-fresh=1")]
    "origin"
  let tag = "\"old\""
      initial = [(H.hCacheControl, response), (H.hAge, "20"), (H.hETag, tag)]
      update = [(H.hCacheControl, response), (H.hETag, tag)]
  (t, calls, sent) <- mkTransport [(S.status200, initial, "cached"), (S.status304, update, "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/request-age-validation"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
  fetch req `shouldReturn` "cached"
  fetch (WReq.setHeader H.hCacheControl "max-age=0" req) `shouldReturn` "cached"
  readIORef calls `shouldReturn` 2
  map (H.lookupHeader H.hIfNoneMatch . WReq.headers) <$> readIORef sent
    `shouldReturn` [Just tag, Nothing]


unit_request_stale_limits :: Spec
unit_request_stale_limits = it "applies request max-stale and respects stricter request and response constraints" $ do
  let response = "max-age=30"
      swr = "max-age=30, stale-while-revalidate=120"
      request control = [(H.hCacheControl, control)]
      check = checkCacheReuse response "70"
  check (request "max-stale") "cached"
  check (request "max-stale=60") "cached"
  check (request "max-stale=10") "origin"
  check (request "max-stale, max-stale=10") "origin"
  check (request "max-stale=10, max-stale") "origin"
  check (request "max-stale=60, min-fresh=0") "origin"
  check (request "max-stale, max-age=50") "origin"
  check (request "max-stale, no-cache") "origin"
  checkCacheReuse swr "70" (request "max-age=100") "origin"
  checkCacheReuse swr "70" (request "max-stale=10") "origin"
  checkCacheReuse "max-age=30, must-revalidate" "70" (request "max-stale") "origin"
  checkCacheReuse "max-age=30, no-cache" "70" (request "max-stale") "origin"


unit_only_if_cached_stale :: Spec
unit_only_if_cached_stale = it "can serve an allowed stale response to a cache-only request" $ do
  let request = [(H.hCacheControl, "only-if-cached, max-stale=60")]
  checkCacheReuse "max-age=30" "70" request "cached"
  checkCacheReuse "max-age=30, stale-while-revalidate=60" "70" [(H.hCacheControl, "only-if-cached")] "cached"


unit_request_pragma :: Spec
unit_request_pragma = it "uses legacy Pragma no-cache only in the absence of Cache-Control" $ do
  let check = checkCacheReuse "max-age=90" "15"
      pragma = [("Pragma", "x-extension=some-token"), ("Pragma", "No-CaChE")]
  check pragma "origin"
  check (pragma <> [(H.hCacheControl, "")]) "cached"
  check (pragma <> [(H.hCacheControl, "max-age=60")]) "cached"
  check [("Pragma", "no-cache=extension")] "cached"


unit_request_no_store :: Spec
unit_request_no_store = it "honours request no-store without replacing an existing entry" $ do
  let fields = [(H.hCacheControl, "max-age=60")]
  (t, calls, _) <- mkTransport
    [(S.status200, fields, "first bypass"), (S.status200, fields, "cached"), (S.status200, fields, "second bypass")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/request-no-store"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
      bypass = fetch (WReq.setHeader H.hCacheControl "no-store" req)
  bypass `shouldReturn` "first bypass"
  cacheSize cache `shouldReturn` 0
  fetch req `shouldReturn` "cached"
  bypass `shouldReturn` "second bypass"
  fetch req `shouldReturn` "cached"
  readIORef calls `shouldReturn` 3


unit_request_stale_if_error :: Spec
unit_request_stale_if_error = it "allows and limits stale-if-error on an individual request" $ do
  let check response control expected = do
        let fields = [(H.hCacheControl, response), (H.hAge, "60")]
        (t, calls, _) <- mkTransport [(S.status200, fields, "cached"), (S.status503, [], "unavailable")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/request-error-window"
        let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
        fetch req `shouldReturn` "cached"
        fetch (WReq.setHeader H.hCacheControl control req) `shouldReturn` expected
        readIORef calls `shouldReturn` 2
  check "max-age=30" "stale-if-error=60" "cached"
  check "max-age=30" "stale-if-error=10, stale-if-error=60" "unavailable"
  check "max-age=30, stale-if-error=90" "stale-if-error=10" "unavailable"
  check "max-age=30, stale-if-error=10" "stale-if-error=90" "unavailable"
  check "max-age=30, must-revalidate" "stale-if-error=90" "unavailable"
  check "max-age=30, stale-if-error=90" "stale-if-error=90, min-fresh=1" "unavailable"


checkCacheReuse :: H.HeaderValue -> H.HeaderValue -> H.Headers -> ByteString -> IO ()
checkCacheReuse responseControl age requestFields expected = do
  let initial = [(H.hCacheControl, responseControl), (H.hAge, age)]
  (t, calls, _) <- mkTransport [(S.status200, initial, "cached"), (S.status200, [], "origin")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/request-cache-policy"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
  fetch req `shouldReturn` "cached"
  fetch req{WReq.headers = requestFields} `shouldReturn` expected
  readIORef calls `shouldReturn` (if expected == "cached" then 1 else 2)


unit_mutation_invalidates_target :: Spec
unit_mutation_invalidates_target = it "invalidates the target's GET and HEAD after a successful unsafe request" $ do
  let fresh = [(H.hCacheControl, "max-age=60")]
  (t, calls, _) <- mkTransport
    [ (S.status200, fresh, "old GET"), (S.status200, fresh, "old HEAD")
    , (S.status200, fresh, "other query"), (S.status303, [], "changed")
    , (S.status200, fresh, "new GET"), (S.status200, fresh, "new HEAD")
    ]
  store <- newInMemoryStore 8
  let cache = newCacheWith store defaultCacheConfig
      fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
  target <- makeRequest "/mutating?variant=one"
  other <- makeRequest "/mutating?variant=two"
  let headReq = target{WReq.method = M.mHead}
      writeReq = WReq.setHeader H.hCacheControl "no-store" target{WReq.method = M.mPost}
  fetch target `shouldReturn` "old GET"
  fetch headReq `shouldReturn` "old HEAD"
  fetch other `shouldReturn` "other query"
  fetch writeReq `shouldReturn` "changed"
  cacheSize cache `shouldReturn` 1
  fetch target `shouldReturn` "new GET"
  fetch headReq `shouldReturn` "new HEAD"
  fetch other `shouldReturn` "other query"
  readIORef calls `shouldReturn` 6


unit_mutation_safe_and_error :: Spec
unit_mutation_safe_and_error = it "keeps entries after safe methods and failed writes, and invalidates for unknown methods" $ do
  let fresh = [(H.hCacheControl, "max-age=60")]
  (t, calls, _) <- mkTransport
    [ (S.status200, fresh, "cached"), (S.status200, [], "options")
    , (S.status200, [], "query"), (S.status200, [], "report")
    , (S.status500, [], "failed write"), (S.status200, [], "extension write")
    , (S.status200, fresh, "new")
    ]
  cache <- newCache defaultCacheConfig
  target <- makeRequest "/method-safety"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
      useMethod method = fetch target{WReq.method = method}
  fetch target `shouldReturn` "cached"
  useMethod M.mOptions `shouldReturn` "options"
  useMethod M.mQuery `shouldReturn` "query"
  useMethod M.mReport `shouldReturn` "report"
  useMethod M.mPut `shouldReturn` "failed write"
  fetch target `shouldReturn` "cached"
  useMethod "EXTEND" `shouldReturn` "extension write"
  fetch target `shouldReturn` "new"
  readIORef calls `shouldReturn` 7


unit_mutation_invalidates_related :: Spec
unit_mutation_invalidates_related = it "resolves typed Location and Content-Location on the same origin" $ do
  let fresh = [(H.hCacheControl, "max-age=60")]
      fields = [(H.hLocation, "../assets/report?rev=2#top"), (H.hContentLocation, "?view=current")]
  (t, calls, _) <- mkTransport
    [ (S.status200, fresh, "old report"), (S.status200, fresh, "old current")
    , (S.status200, fields, "changed"), (S.status200, fresh, "new report")
    , (S.status200, fresh, "new current")
    ]
  cache <- newCache defaultCacheConfig
  report <- makeRequest "/assets/report?rev=2"
  current <- makeRequest "/edit/record?view=current"
  target <- makeRequest "/edit/record?view=previous"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
  fetch report `shouldReturn` "old report"
  fetch current `shouldReturn` "old current"
  fetch target{WReq.method = M.mPut} `shouldReturn` "changed"
  fetch report `shouldReturn` "new report"
  fetch current `shouldReturn` "new current"
  readIORef calls `shouldReturn` 5


unit_mutation_distinguishes_empty_query :: Spec
unit_mutation_distinguishes_empty_query = it "keeps absent and empty queries in distinct cache entries" $ do
  let fresh = [(H.hCacheControl, "max-age=60")]
  (t, calls, _) <- mkTransport
    [ (S.status200, fresh, "without query")
    , (S.status200, fresh, "with empty query")
    , (S.status200, [(H.hContentLocation, "../record?")], "changed")
    , (S.status200, fresh, "updated empty query")
    ]
  cache <- newCache defaultCacheConfig
  noQuery <- makeRequest "/record"
  emptyQuery <- makeRequest "/record?"
  target <- makeRequest "/edit/record"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
  fetch noQuery `shouldReturn` "without query"
  fetch emptyQuery `shouldReturn` "with empty query"
  fetch target{WReq.method = M.mPut} `shouldReturn` "changed"
  fetch noQuery `shouldReturn` "without query"
  fetch emptyQuery `shouldReturn` "updated empty query"
  readIORef calls `shouldReturn` 4


unit_mutation_ignores_bad_references :: Spec
unit_mutation_ignores_bad_references = it "ignores ambiguous fields and different origins while accepting host casing and default ports" $ do
  let fresh = [(H.hCacheControl, "max-age=60")]
      mutations =
        [ [(H.hLocation, "https://example.com/side"), (H.hContentLocation, "//example.com:8080/side")]
        , [(H.hLocation, "/side"), (H.hLocation, "/wrong"), (H.hContentLocation, "/side#fragment")]
        , [(H.hLocation, "http://example.com:18446744073709551696/side")]
        , [(H.hLocation, "http://example.com:80/side"), (H.hConnection, "location")]
        , [(H.hLocation, "HTTP://EXAMPLE.COM:80/side#ok")]
        ]
  (t, calls, _) <- mkTransport $ [(S.status200, fresh, "cached")] <>
    map (\fields -> (S.status200, fields, "changed")) mutations <> [(S.status200, fresh, "new")]
  cache <- newCache defaultCacheConfig
  side <- makeRequest "/side"
  target <- makeRequest "/do-write"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
      mutate = fetch target{WReq.method = M.mPatch}
  fetch side `shouldReturn` "cached"
  replicateM_ 4 $ do
    mutate `shouldReturn` "changed"
    fetch side `shouldReturn` "cached"
  mutate `shouldReturn` "changed"
  fetch side `shouldReturn` "new"
  readIORef calls `shouldReturn` 7


unit_mutation_blocks_late_publication :: Spec
unit_mutation_blocks_late_publication = it "keeps a response already in flight from repopulating an invalidated key" $ do
  newCache defaultCacheConfig >>= \cache -> verify cache False
  newCache defaultCacheConfig >>= \cache -> verify cache True
  store <- newInMemoryStore 8
  newSynchronizedCacheWith store defaultCacheConfig >>= \cache -> verify cache False
  where
    verify cache validating = do
      let tag = (H.hETag, "\"old\"")
          fresh = [(H.hCacheControl, "max-age=60"), tag]
          cold = [(S.status200, fresh, "earlier")]
          warm = [(S.status200, [(H.hCacheControl, "max-age=0"), tag], "earlier"), (S.status304, fresh, "")]
          gateAt = if validating then 1 else 0
      (inner, calls, _) <- mkTransport $
        (if validating then warm else cold) <> [(S.status200, [], "changed"), (S.status200, fresh, "later")]
      readStarted <- newEmptyMVar
      continueRead <- newEmptyMVar
      result <- newEmptyMVar
      firstRead <- newIORef True
      let transport = Transport $ \request -> do
            n <- readIORef calls
            reply <- sendRaw inner request
            if n == gateAt
              then pure reply{Resp.bodyPopper = do
                pause <- atomicModifyIORef' firstRead (\first -> (False, first))
                when pause (putMVar readStarted () >> takeMVar continueRead)
                Resp.bodyPopper reply}
              else pure reply
      target <- makeRequest "/in-flight"
      let fetch candidate = sendRaw (withCache cache transport) candidate >>= drainBody
      when validating (fetch target `shouldReturn` "earlier")
      _ <- forkFinally (fetch target) (putMVar result)
      takeMVar readStarted
      (fetch target{WReq.method = M.mPost} `shouldReturn` "changed")
        `finally` putMVar continueRead ()
      (takeMVar result >>= either throwIO pure) `shouldReturn` "earlier"
      cacheSize cache `shouldReturn` 0
      fetch target `shouldReturn` "later"
      readIORef calls `shouldReturn` (gateAt + 3)


unit_etag_revalidation :: Spec
unit_etag_revalidation = it "stale entry revalidates with ETag and replays on 304" $ do
  let payload = "etag-body"
      etag = "\"v1\""
  (t, calls, reqsRef) <-
    mkTransport
      [
        ( S.status200
        ,
          [ (H.hCacheControl, "max-age=0")
          , (H.hETag, etag)
          ]
        , payload
        )
      ,
        ( S.status304
        , [(H.hETag, etag)]
        , ""
        )
      ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  _ <- makeRequest "/e" >>= withC >>= drainBody
  r2 <- makeRequest "/e" >>= withC
  body2 <- drainBody r2
  n <- readIORef calls
  n `shouldBe` 2
  -- The replayed response is the cached body, not the 304's empty body.
  body2 `shouldBe` payload
  -- The second request should have carried If-None-Match.
  reqs <- readIORef reqsRef
  let recent = head reqs -- newest first
      hadInm = case lookup H.hIfNoneMatch (WReq.headers recent) of
        Just v -> v == etag
        Nothing -> False
  (hadInm) `shouldBe` True


unit_forward_request_preconditions :: Spec
unit_forward_request_preconditions = it "forwards origin-only preconditions and ranges when a cached response exists" $ do
  let stored age = [(H.hCacheControl, age), (H.hETag, "\"stored\"")]
      callerFields = [(H.hIfNoneMatch, "\"stored\""), ("If-Match", "\"client\"")]
      rangeFields = [(H.hRange, "bytes=0-1"), (H.hIfRange, "\"client\"")]
  (t, calls, sent) <-
    mkTransport
      [ (S.status200, stored "max-age=60", "fresh")
      , (S.Status 412, [], "origin precondition")
      , (S.status200, stored "max-age=0", "stale")
      , (S.Status 206, [], "ab")
      ]
  cache <- newCache defaultCacheConfig
  freshReq <- makeRequest "/client-condition"
  staleReq <- makeRequest "/client-range"
  let fetch = sendRaw (withCache cache t)
  (fetch freshReq >>= drainBody) `shouldReturn` "fresh"
  conditional <- fetch freshReq{WReq.headers = callerFields}
  Resp.statusCode conditional `shouldBe` S.Status 412
  drainBody conditional `shouldReturn` "origin precondition"
  (fetch staleReq >>= drainBody) `shouldReturn` "stale"
  (fetch staleReq{WReq.headers = rangeFields} >>= drainBody) `shouldReturn` "ab"
  readIORef calls `shouldReturn` 4
  map WReq.headers . take 3 <$> readIORef sent `shouldReturn` [rangeFields, WReq.headers staleReq, callerFields]


unit_cached_entity_tag_conditions :: Spec
unit_cached_entity_tag_conditions = it "evaluates repeated If-None-Match with weak byte comparison on the selected variant" $ do
  now <- getCurrentTime
  let opaqueTag = "\"id-" <> BS.singleton 0xFF <> "\""
      current = formatHttpDate now
      metadata =
        [ (H.hETag, "W/" <> opaqueTag)
        , (H.hDate, current)
        , (H.hCacheControl, "max-age=60")
        , (H.hExpires, formatHttpDate (addUTCTime 60 now))
        , (H.hVary, "Accept-Language")
        , (H.hContentLocation, "/en")
        ]
      fields = metadata <> [(H.hContentLength, "5"), (H.hContentType, "text/plain"), ("Content-Digest", "sha-256=:AQ==:")]
      english = (H.hAcceptLanguage, "en")
      inm value = (H.hIfNoneMatch, value)
      ims = (H.hIfModifiedSince, current)
  (t, calls, _) <- mkTransport [(S.status200, fields, "hello")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/cached-tag"
  let fetch headers = sendRaw (withCache cache t) req{WReq.headers = headers}
  (fetch [english] >>= drainBody) `shouldReturn` "hello"
  matched <- fetch [english, inm "\"other\"", inm (" \t" <> opaqueTag), ims, (H.hCacheControl, "only-if-cached")]
  Resp.statusCode matched `shouldBe` S.status304
  drainBody matched `shouldReturn` ""
  Resp.rawResponseTrailers matched `shouldReturn` []
  H.hasHeader H.hAge (Resp.headers matched) `shouldBe` True
  H.deleteHeader H.hAge (Resp.headers matched) `shouldBe` metadata
  wildcard <- fetch [english, inm "*"]
  Resp.statusCode wildcard `shouldBe` S.status304
  drainBody wildcard `shouldReturn` ""
  let expectBody tag = do
        reply <- fetch [english, ims, inm tag]
        Resp.statusCode reply `shouldBe` S.status200
        drainBody reply `shouldReturn` "hello"
  expectBody "\"other\""
  expectBody ""
  missed <- fetch [(H.hAcceptLanguage, "fr"), inm "*", (H.hCacheControl, "only-if-cached")]
  Resp.statusCode missed `shouldBe` S.status504
  readIORef calls `shouldReturn` 1


unit_cached_date_conditions :: Spec
unit_cached_date_conditions = it "evaluates If-Modified-Since against Last-Modified, Date, or the stored receipt time" $ do
  now <- getCurrentTime
  let current = formatHttpDate now
      modified = formatHttpDate (addUTCTime (-60) now)
      earlier = formatHttpDate (addUTCTime (-61) now)
      check method extra date = do
        (t, calls, _) <- mkTransport [(S.status200, (H.hCacheControl, "max-age=60") : extra, "")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/cached-date"
        let fetch fields = sendRaw (withCache cache t) req{WReq.method = method, WReq.headers = fields}
        _ <- fetch []
        hit <- fetch []
        let toCondition captured = [(H.hIfModifiedSince, captured)]
        threshold <- case date of
          Just value -> pure value
          Nothing -> case H.lookupHeader H.hDate (Resp.headers hit) of
            Just value -> pure value
            Nothing -> expectationFailure "Expected a synthesized Date"
        matched <- fetch (toCondition threshold)
        Resp.statusCode matched `shouldBe` S.status304
        drainBody matched `shouldReturn` ""
        mapM_ (\fields -> fetch fields >>= \reply -> Resp.statusCode reply `shouldBe` S.status200)
          [toCondition earlier, toCondition "bad date", toCondition threshold <> toCondition threshold]
        readIORef calls `shouldReturn` 1
  check M.mGet [(H.hDate, current), (H.hLastModified, modified)] (Just modified)
  check M.mHead [(H.hDate, current)] (Just current)
  check M.mGet [] Nothing


unit_forward_conditional_cache_misses :: Spec
unit_forward_conditional_cache_misses = it "forwards the caller's exact validators when the selected entry cannot be reused" $ do
  let own = [(H.hIfNoneMatch, "\"client\""), (H.hIfNoneMatch, "W/\"other\"")]
      inherit = [(H.hETag, "\"stored\"")]
      check cached headers = do
        (t, calls, sent) <- mkTransport (cached <> [(S.status304, inherit, "")])
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/forward-validator"
        let fetch = sendRaw (withCache cache t)
        mapM_ (const (fetch req >>= drainBody)) cached
        reply <- fetch req{WReq.headers = headers}
        Resp.statusCode reply `shouldBe` S.status304
        Resp.headers reply `shouldBe` inherit
        drainBody reply `shouldReturn` ""
        readIORef calls `shouldReturn` length cached + 1
        map WReq.headers . take 1 <$> readIORef sent `shouldReturn` [headers]
      stored freshness = [(S.status200, (H.hCacheControl, freshness) : inherit, "stored")]
  check [] own
  check (stored "max-age=0") own
  check (stored "max-age=60") (own <> [(H.hCacheControl, "no-cache")])
  check (stored "max-age=60") (own <> [(H.hCacheControl, "no-store")])
  check (stored "max-age=60") [(H.hIfNoneMatch, "* , \"other\"")]
  check [(S.status203, (H.hCacheControl, "max-age=60") : inherit, "transformed")] own


unit_caller_304_updates_cache :: Spec
unit_caller_304_updates_cache = it "freshens the selected snapshot from an identifying caller 304" $ do
  let tag value = (H.hETag, value)
      date = (H.hLastModified, "Sun, 06 Nov 1994 08:49:37 GMT")
      named = [(H.hIfNoneMatch, "\"other\""), (H.hIfNoneMatch, "W/\"stored\"")]
      check staleMetadata conditions freshMetadata updated = do
        let stale = (H.hCacheControl, "max-age=0") : (H.hContentLength, "4") : staleMetadata
            fresh = (H.hCacheControl, "max-age=60") : ("X-Metadata", "fresh") : freshMetadata
        (inner, calls, sent) <- mkTransport [(S.status200, stale, "body"), (S.status304, fresh, "")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/caller-304"
        let fetch = sendRaw (withCache cache inner)
        (fetch req >>= drainBody) `shouldReturn` "body"
        reply <- fetch req{WReq.headers = conditions}
        Resp.statusCode reply `shouldBe` S.status304
        Resp.headers reply `shouldBe` fresh
        drainBody reply `shouldReturn` ""
        map WReq.headers . take 1 <$> readIORef sent `shouldReturn` [conditions]
        hit <- fetch (WReq.setHeader H.hCacheControl "only-if-cached" req)
        if updated
          then do
            Resp.statusCode hit `shouldBe` S.status200
            H.lookupHeader "X-Metadata" (Resp.headers hit) `shouldBe` Just "fresh"
            H.lookupHeader H.hContentLength (Resp.headers hit) `shouldBe` Just "4"
            drainBody hit `shouldReturn` "body"
          else Resp.statusCode hit `shouldBe` S.status504
        readIORef calls `shouldReturn` 2
      strong = tag "\"stored\""
      weak = tag "W/\"stored\""
  check [strong] named [strong, (H.hContentLength, "999")] True
  check [strong] named [weak] True
  check [weak] named [strong] False
  check [strong] named [tag "\"other\""] False
  check [strong] named [strong, weak] False
  check [strong] named [strong, (H.hConnection, "ETag")] False
  check [strong] named [] False
  check [date] [(H.hIfModifiedSince, snd date)] [date] True
  check [] [(H.hIfNoneMatch, "*")] [] True
  let vary = (H.hVary, "Accept-Language")
  check [strong, vary] (named <> [(H.hAcceptLanguage, "fr")]) [strong] False


unit_late_caller_304_preserves_newer_entry :: Spec
unit_late_caller_304_preserves_newer_entry = it "keeps a response stored while a caller 304 was in flight" $ do
  let stale = [(H.hCacheControl, "max-age=0"), (H.hETag, "\"old\"")]
      refreshed = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"old\"")]
      latest = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"new\"")]
  (inner, calls, _) <- mkTransport [(S.status200, stale, "old"), (S.status304, refreshed, ""), (S.status200, latest, "new")]
  started <- newEmptyMVar
  release <- newEmptyMVar
  finished <- newEmptyMVar
  let peer = Transport $ \request -> do
        reply <- sendRaw inner request
        when (H.hasHeader "X-Caller" (WReq.headers request)) (putMVar started () >> takeMVar release)
        pure reply
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/late-caller-304"
  let fetch = sendRaw (withCache cache peer)
      conditional = req{WReq.headers = [("X-Caller", "1"), (H.hIfNoneMatch, "\"old\"")]}
  (fetch req >>= drainBody) `shouldReturn` "old"
  _ <- forkFinally (fetch conditional) (putMVar finished)
  (takeMVar started >> ((fetch req >>= drainBody) `shouldReturn` "new")) `finally` putMVar release ()
  reply <- takeMVar finished >>= either throwIO pure
  Resp.statusCode reply `shouldBe` S.status304
  drainBody reply `shouldReturn` ""
  (fetch req >>= drainBody) `shouldReturn` "new"
  readIORef calls `shouldReturn` 3


unit_conditional_background_validation :: Spec
unit_conditional_background_validation = it "serves conditional stale-while-revalidate hits with cache-owned background validators" $ do
  let tag = (H.hETag, "\"stored\"")
      check variant = do
        let stale = [(H.hCacheControl, "max-age=0, stale-while-revalidate=60"), tag] <> variant
        received <- newEmptyMVar
        first <- newIORef True
        let peer = Transport $ \req -> do
              initial <- atomicModifyIORef' first (\value -> (False, value))
              if initial
                then do
                  body <- BSm.popperFromStrict "stored"
                  pure (RawResponse S.status200 stale body HTTP1_1)
                else do
                  putMVar received (WReq.headers req)
                  pure (RawResponse S.status304 [tag, (H.hCacheControl, "max-age=60")] (pure "") HTTP1_1)
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/stale-conditional"
        let fetch = sendRaw (withCache cache peer)
        (fetch req >>= drainBody) `shouldReturn` "stored"
        reply <- fetch req{WReq.headers = [(H.hIfNoneMatch, "*")]}
        Resp.statusCode reply `shouldBe` S.status304
        drainBody reply `shouldReturn` ""
        fields <- takeMVar received
        H.lookupHeaders H.hIfNoneMatch fields `shouldBe` ["\"stored\""]
        H.lookupHeaders H.hIfModifiedSince fields `shouldBe` []
  check []
  check [(H.hVary, "If-Modified-Since"), (H.hLastModified, "Sun, 06 Nov 1994 08:49:37 GMT")]


unit_unmatched_304 :: Spec
unit_unmatched_304 = it "retries once after mismatched, missing or invalid validation metadata" $ do
  let date = (H.hLastModified, "Sun, 06 Nov 1994 08:49:37 GMT")
      check storedTag returned = do
        let stale = [(H.hCacheControl, "max-age=0"), (H.hETag, storedTag), date]
            fresh = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"replacement\"")]
        (t, calls, sent) <-
          mkTransport [(S.status200, stale, "old"), (S.status304, returned, ""), (S.status200, fresh, "new")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/retry-validator"
        let fetch = sendRaw (withCache cache t) req >>= drainBody
        fetch `shouldReturn` "old"
        fetch `shouldReturn` "new"
        fetch `shouldReturn` "new"
        readIORef calls `shouldReturn` 3
        map (H.lookupHeader H.hIfNoneMatch . WReq.headers) <$> readIORef sent
          `shouldReturn` [Nothing, Just storedTag, Nothing]
  check "\"old\"" [(H.hETag, "\"different\""), date]
  check "W/\"old\"" [(H.hETag, "\"old\"")]
  check "\"old\"" [(H.hETag, "\"old\""), (H.hETag, "\"old\"")]
  check "\"old\"" [(H.hETag, "unquoted")]
  check "\"old\"" [(H.hETag, "\"old\""), (H.hConnection, "etag")]
  check "\"old\"" [(H.hETag, "\"old\""), (H.hConnection, "cache-control"), (H.hCacheControl, "no-store")]
  check "\"old\"" [(H.hETag, "\"old\""), (H.hLastModified, "bad date")]
  check "\"old\"" [date]
  check "\"old\"" []


unit_validation_fallback_once :: Spec
unit_validation_fallback_once = it "forwards a second 304 after one unconditional retry" $ do
  let stale = [(H.hCacheControl, "max-age=0"), (H.hETag, "\"old\"")]
      mismatch = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"other\"")]
  (t, calls, sent) <- mkTransport [(S.status200, stale, "old"), (S.status304, mismatch, ""), (S.status304, [], "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/retry-once"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "old"
  retry <- fetch
  Resp.statusCode retry `shouldBe` S.status304
  drainBody retry `shouldReturn` ""
  readIORef calls `shouldReturn` 3
  map (H.lookupHeader H.hIfNoneMatch . WReq.headers) <$> readIORef sent
    `shouldReturn` [Nothing, Just "\"old\"", Nothing]


unit_weak_etag_revalidation :: Spec
unit_weak_etag_revalidation = it "uses weak comparison for a weak response ETag and preserves opaque bytes" $ do
  let tag = BS.pack [34, 255, 34]
      weakTag = "W/" <> tag
      stale = [(H.hCacheControl, "max-age=0"), (H.hETag, tag)]
      update = [(H.hCacheControl, "max-age=60"), (H.hETag, weakTag)]
  (t, calls, sent) <- mkTransport [(S.status200, stale, "old"), (S.status304, update, "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/weak-validator"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "old"
  reused <- fetch
  H.lookupHeader H.hETag (Resp.headers reused) `shouldBe` Just weakTag
  drainBody reused `shouldReturn` "old"
  (fetch >>= drainBody) `shouldReturn` "old"
  readIORef calls `shouldReturn` 2
  map (H.lookupHeader H.hIfNoneMatch . WReq.headers) <$> readIORef sent `shouldReturn` [Just tag, Nothing]


unit_last_modified_revalidation :: Spec
unit_last_modified_revalidation = it "revalidates Last-Modified by its typed timestamp and generates a canonical date" $ do
  let canonical = "Sun, 06 Nov 1994 08:49:37 GMT"
      check original revised = do
        let stale = [(H.hCacheControl, "max-age=0"), (H.hLastModified, original)]
            update = [(H.hCacheControl, "max-age=60"), (H.hLastModified, revised)]
        (t, calls, sent) <- mkTransport [(S.status200, stale, "old"), (S.status304, update, "")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/date-validator"
        let fetch = sendRaw (withCache cache t) req >>= drainBody
        fetch `shouldReturn` "old"
        fetch `shouldReturn` "old"
        fetch `shouldReturn` "old"
        readIORef calls `shouldReturn` 2
        map (H.lookupHeader H.hIfModifiedSince . WReq.headers) <$> readIORef sent `shouldReturn` [Just canonical, Nothing]
  check "Sun Nov  6 08:49:37 1994" canonical
  check "SUN nov  6 08:49:37 1994" "sUn, 06 nOV 1994 08:49:37 gMt"


unit_invalid_stored_validators :: Spec
unit_invalid_stored_validators = it "omits invalid stored validators and leaves an unsolicited 304 intact" $ do
  let stale =
        [ (H.hCacheControl, "max-age=0")
        , (H.hETag, "\"old\"")
        , (H.hETag, "\"old\"")
        , (H.hLastModified, "invalid date")
        ]
  (t, calls, sent) <- mkTransport [(S.status200, stale, "old"), (S.status304, [], "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/invalid-validator"
  let fetch = sendRaw (withCache cache t) req
  (fetch >>= drainBody) `shouldReturn` "old"
  reply <- fetch
  Resp.statusCode reply `shouldBe` S.status304
  drainBody reply `shouldReturn` ""
  readIORef calls `shouldReturn` 2
  map WReq.headers <$> readIORef sent `shouldReturn` replicate 2 (WReq.headers req)


unit_cache_bypasses_request_body :: Spec
unit_cache_bypasses_request_body = it "bypasses the cache for a GET carrying a streamed body" $ do
  let hdrs = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"stored\"")]
  (t, calls, _) <- mkTransport [(S.status200, hdrs, "bodyless"), (S.status200, hdrs, "with body")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/get-body"
  streamed <- BSm.streamFromStrict "input"
  let fetch candidate = sendRaw (withCache cache t) candidate >>= drainBody
  fetch req `shouldReturn` "bodyless"
  fetch req{WReq.body = streamed} `shouldReturn` "with body"
  fetch req `shouldReturn` "bodyless"
  readIORef calls `shouldReturn` 2


unit_vary_repeated_response_fields :: Spec
unit_vary_repeated_response_fields = it "uses every Vary field line and every nominated request value" $ do
  let headers = [(H.hCacheControl, "max-age=60"), (H.hVary, "Accept"), (H.hVary, "X-Flavour")]
  (t, calls, _) <- mkTransport [(S.status200, headers, "vanilla"), (S.status200, headers, "chocolate")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/vary-lines"
  let fetch flavour =
        sendRaw (withCache cache t) req{WReq.headers = [(H.hAccept, "text/plain"), ("x-flavour", flavour)]}
          >>= drainBody
  fetch "vanilla" `shouldReturn` "vanilla"
  fetch "vanilla" `shouldReturn` "vanilla"
  fetch "chocolate" `shouldReturn` "chocolate"
  readIORef calls `shouldReturn` 2

unit_vary_request_lines :: Spec
unit_vary_request_lines = it "compares all request lines and keeps absence distinct from an empty line" $ do
  let headers = [(H.hCacheControl, "max-age=60"), (H.hVary, "X-Key")]
  (t, calls, _) <-
    mkTransport (map (\body -> (S.status200, headers, body)) ["absent", "empty", "pair one", "pair two"])
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/vary-request-lines"
  let fetch values =
        sendRaw (withCache cache t) req{WReq.headers = map (\value -> ("x-key", value)) values}
          >>= drainBody
  fetch [] `shouldReturn` "absent"
  fetch [] `shouldReturn` "absent"
  fetch [""] `shouldReturn` "empty"
  fetch [""] `shouldReturn` "empty"
  fetch ["same", "first"] `shouldReturn` "pair one"
  fetch ["same", "first"] `shouldReturn` "pair one"
  fetch ["same", "second"] `shouldReturn` "pair two"
  readIORef calls `shouldReturn` 4

unit_vary_unusable :: Spec
unit_vary_unusable = it "skips responses with a wildcard or malformed Vary on a later line" $ do
  let headers value = [(H.hCacheControl, "max-age=60"), (H.hVary, "Accept"), (H.hVary, value)]
  (t, calls, _) <-
    mkTransport
      [ (S.status200, headers "*", "wildcard")
      , (S.status200, headers "bad name", "malformed")
      , (S.status200, [], "other")
      ]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/vary-invalid"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "wildcard"
  fetch `shouldReturn` "malformed"
  cacheSize cache `shouldReturn` 0
  fetch `shouldReturn` "other"
  readIORef calls `shouldReturn` 3

unit_vary_legacy_cache_entry :: Spec
unit_vary_legacy_cache_entry = it "treats a stored Vary entry with a legacy snapshot as a miss" $ do
  let headers = [(H.hCacheControl, "max-age=60"), (H.hVary, ""), (H.hVary, "X-Format")]
  (t, calls, _) <- mkTransport [(S.status200, headers, "old"), (S.status200, headers, "new")]
  inner <- newInMemoryStore 4
  let legacy = inner{csInsert = \key entry -> csInsert inner key entry{ceVaryFields = []}}
      cache = newCacheWith legacy defaultCacheConfig
  req <- makeRequest "/vary-old"
  let fetch v = sendRaw (withCache cache t) req{WReq.headers = [("x-format", v)]} >>= drainBody
  fetch "first" `shouldReturn` "old"
  fetch "second" `shouldReturn` "new"
  readIORef calls `shouldReturn` 2

unit_revalidated_vary_and_freshness :: Spec
unit_revalidated_vary_and_freshness = it "rebuilds Vary and freshness after 304 and keeps stored Content-Length" $ do
  let etag = (H.hETag, "\"v1\"")
      stale = [(H.hCacheControl, "max-age=0"), (H.hContentLength, "5"), etag]
      update =
        [ (H.hCacheControl, "max-age=60")
        , (H.hVary, "X-Format")
        , (H.hContentLength, "0")
        , (H.hConnection, "x-hop-one")
        , (H.hConnection, "x-hop-two")
        , ("x-hop-one", "first hop field")
        , ("x-hop-two", "second hop field")
        , etag
        ]
  (t, calls, _) <-
    mkTransport [(S.status200, stale, "first"), (S.status304, update, ""), (S.status200, [], "second")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/vary-freshened"
  let fetch format = sendRaw (withCache cache t) req{WReq.headers = [("x-format", format)]}
  (fetch "a" >>= drainBody) `shouldReturn` "first"
  revalidated <- fetch "a"
  let refreshedHeaders = Resp.headers revalidated
  H.lookupHeader H.hContentLength refreshedHeaders `shouldBe` Just "5"
  map (\name -> H.lookupHeader name refreshedHeaders) [H.hConnection, "x-hop-one", "x-hop-two"]
    `shouldBe` [Nothing, Nothing, Nothing]
  drainBody revalidated `shouldReturn` "first"
  (fetch "a" >>= drainBody) `shouldReturn` "first"
  readIORef calls `shouldReturn` 2
  (fetch "b" >>= drainBody) `shouldReturn` "second"
  readIORef calls `shouldReturn` 3

unit_revalidated_decoded_body :: Spec
unit_revalidated_decoded_body = it "keeps decoded cache bytes and their headers aligned after 304" $ do
  let payload = "decoded bytes"
      coded = BSL.toStrict (GZip.compress (BSL.fromStrict payload))
      size = BS8.pack (show (BS.length coded))
      tag = (H.hETag, "\"encoded-v1\"")
      stale = [(H.hCacheControl, "max-age=0"), (H.hContentEncoding, "gzip"), (H.hContentLength, size), tag]
      update = [(H.hCacheControl, "max-age=60"), ("content-encoding", "gzip"), (H.hContentLength, size), tag] <> validationDigests
  (t, calls, _) <- mkTransport [(S.status200, stale, coded), (S.status304, update, "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/decoded-304"
  let fetch = sendRaw (withCache cache (withDecompression t)) req
      check response = do
        H.lookupHeader H.hContentEncoding (Resp.headers response) `shouldBe` Nothing
        H.lookupHeader H.hContentLength (Resp.headers response) `shouldBe` Nothing
        map (\(name, _) -> H.lookupHeader name (Resp.headers response)) validationDigests `shouldBe` replicate (length validationDigests) Nothing
        drainBody response `shouldReturn` payload
  fetch >>= check
  refreshed <- fetch
  H.lookupHeader H.hCacheControl (Resp.headers refreshed) `shouldBe` Just "max-age=60"
  check refreshed
  fetch >>= check
  readIORef calls `shouldReturn` 2


unit_revalidated_encoded_body :: Spec
unit_revalidated_encoded_body = it "keeps the coding chain and framing of the stored bytes after 304" $ do
  let payload = BSL.toStrict (Brotli.compress (GZip.compress "stored representation"))
      size = BS8.pack (show (BS.length payload))
      tag = (H.hETag, "W/\"v1\"")
      oldDigest = "sha-256=:RK/0qy18MlBSVnWgjwz6lZEWjP/lF5HF9bvEF8FabDg=:"
      stale = [(H.hCacheControl, "max-age=0"), (H.hContentLength, size), (H.hContentEncoding, "gzip"), ("content-encoding", "br"), ("Content-Digest", oldDigest), tag]
      update = [(H.hCacheControl, "max-age=60"), (H.hContentEncoding, "zstd"), (H.hContentLength, "1"), (H.hContentRange, "bytes 1-1/12"), (H.hContentType, "application/octet-stream"), tag] <> validationDigests
  (t, calls, _) <- mkTransport [(S.status200, stale, payload), (S.status304, update, "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/encoded-304"
  let fetch = sendRaw (withCache cache t) req
      check response = do
        let fields = Resp.headers response
        H.lookupHeaders H.hContentEncoding fields `shouldBe` ["gzip", "br"]
        H.lookupHeader H.hContentLength fields `shouldBe` Just size
        H.lookupHeader H.hContentRange fields `shouldBe` Nothing
        H.lookupHeader H.hContentType fields `shouldBe` Just "application/octet-stream"
        H.lookupHeader "Content-Digest" fields `shouldBe` Just oldDigest
        H.lookupHeader "Repr-Digest" fields `shouldBe` Nothing
        drainBody response `shouldReturn` payload
  (fetch >>= drainBody) `shouldReturn` payload
  fetch >>= check
  fetch >>= check
  readIORef calls `shouldReturn` 2

validationDigests :: H.Headers
validationDigests =
  [ ("content-digest", "sha-256=:47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=:")
  , ("repr-digest", "sha-256=:d435Qo+nKZ+gLcUHn7GQtQ72hiBVAgqoLsZnZPiTGPk=:")
  , ("digest", "md5=1B2M2Y8AsgTpgAmY7PhCfg==")
  , ("content-md5", "1B2M2Y8AsgTpgAmY7PhCfg==")
  ]

unit_revalidated_vary_wildcard :: Spec
unit_revalidated_vary_wildcard = it "evicts a validated response whose new Vary contains a wildcard" $ do
  let etag = (H.hETag, "\"v1\"")
      stale = [(H.hCacheControl, "max-age=0"), etag]
      update = [(H.hCacheControl, "max-age=60"), (H.hVary, "*"), etag]
  (t, calls, _) <-
    mkTransport [(S.status200, stale, "first"), (S.status304, update, ""), (S.status200, [], "second")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/vary-freshened-all"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "first"
  fetch `shouldReturn` "first"
  cacheSize cache `shouldReturn` 0
  fetch `shouldReturn` "second"
  readIORef calls `shouldReturn` 3


unit_revalidated_no_store :: Spec
unit_revalidated_no_store = it "evicts a validated response updated with no-store" $ do
  let etag = (H.hETag, "\"v1\"")
      stale = [(H.hCacheControl, "max-age=0"), etag]
      update = [etag, (H.hCacheControl, "max-age=60"), (H.hCacheControl, "no-store")]
  (t, calls, _) <-
    mkTransport [(S.status200, stale, "original"), (S.status304, update, ""), (S.status200, [], "new")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/revalidate-no-store"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "original"
  fetch `shouldReturn` "original"
  cacheSize cache `shouldReturn` 0
  fetch `shouldReturn` "new"
  readIORef calls `shouldReturn` 3


unit_head_freshens_get :: Spec
unit_head_freshens_get = it "freshens a matching GET from HEAD while retaining the stored body metadata" $ do
  let tag = (H.hETag, "\"same\"")
      modified = (H.hLastModified, "Sun, 06 Nov 1994 08:49:37 GMT")
      digest = ("Content-Digest", "sha-256=:d435Qo+nKZ+gLcUHn7GQtQ72hiBVAgqoLsZnZPiTGPk=:")
      old = [(H.hCacheControl, "max-age=0"), (H.hContentLength, "4"), (H.hContentType, "text/plain"), tag, modified, digest]
      revised =
        [ (H.hCacheControl, "max-age=60")
        , (H.hContentLength, "04")
        , (H.hContentType, "application/octet-stream")
        , tag
        , (H.hLastModified, "Sun Nov  6 08:49:37 1994")
        , ("Content-Digest", "sha-256=:47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=:")
        ]
      check headRequest condition = do
        (t, calls, sent) <- mkTransport [(S.status200, old, "body"), (S.status200, revised, "")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/freshen-from-head"
        let fetch = sendRaw (withCache cache t)
        (fetch req >>= drainBody) `shouldReturn` "body"
        headReply <- fetch (headRequest req{WReq.method = M.mHead})
        Resp.headers headReply `shouldBe` revised
        drainBody headReply `shouldReturn` ""
        getReply <- fetch req
        H.lookupHeader H.hContentType (Resp.headers getReply) `shouldBe` Just "application/octet-stream"
        H.lookupHeader H.hContentLength (Resp.headers getReply) `shouldBe` Just "4"
        H.lookupHeader "Content-Digest" (Resp.headers getReply) `shouldBe` Just (snd digest)
        drainBody getReply `shouldReturn` "body"
        readIORef calls `shouldReturn` 2
        map (H.lookupHeader H.hIfNoneMatch . WReq.headers) <$> readIORef sent
          `shouldReturn` [condition, Nothing]
  check id Nothing
  check (WReq.setHeader H.hIfNoneMatch "\"caller\"") (Just "\"caller\"")


unit_head_retires_get :: Spec
unit_head_retires_get = it "retires a GET whose HEAD validators or length cannot confirm the stored representation" $ do
  let tag = (H.hETag, "\"stored\"")
      date = (H.hLastModified, "Sun, 06 Nov 1994 08:49:37 GMT")
      old = [(H.hCacheControl, "max-age=60"), (H.hContentLength, "4"), tag, date]
      check fields = do
        (t, calls, _) <- mkTransport [(S.status200, old, "body"), (S.status200, fields, ""), (S.status200, [], "replacement")]
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/retire-from-head"
        let fetch request = sendRaw (withCache cache t) request >>= drainBody
        fetch req `shouldReturn` "body"
        fetch req{WReq.method = M.mHead} `shouldReturn` ""
        fetch req `shouldReturn` "replacement"
        readIORef calls `shouldReturn` 3
  check [(H.hETag, "\"changed\""), (H.hContentLength, "4")]
  check [tag, (H.hLastModified, "Mon, 07 Nov 1994 08:49:37 GMT")]
  check [tag, (H.hContentLength, "5")]
  check [tag, (H.hContentLength, "4"), (H.hContentLength, "4")]
  check [(H.hContentLength, "4")]
  check [tag, (H.hCacheControl, "no-store")]


unit_head_preserves_get_variant :: Spec
unit_head_preserves_get_variant = it "limits HEAD metadata updates to the variant it selected" $ do
  let old = [(H.hCacheControl, "max-age=60"), (H.hVary, "Accept-Language"), (H.hETag, "\"english\"")]
      revised = [(H.hCacheControl, "max-age=60"), (H.hVary, "Accept-Language"), (H.hETag, "\"french\"")]
  (t, calls, _) <- mkTransport [(S.status200, old, "hello"), (S.status200, revised, ""), (S.status200, revised, "")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/head-variant"
  let en = WReq.setHeader H.hAcceptLanguage "en" req
      fr = WReq.setHeader H.hAcceptLanguage "fr" req{WReq.method = M.mHead}
      bypass = WReq.setHeader H.hCacheControl "no-store" en{WReq.method = M.mHead}
      fetch request = sendRaw (withCache cache t) request >>= drainBody
  fetch en `shouldReturn` "hello"
  fetch fr `shouldReturn` ""
  fetch en `shouldReturn` "hello"
  fetch bypass `shouldReturn` ""
  fetch en `shouldReturn` "hello"
  readIORef calls `shouldReturn` 3


unit_late_head_preserves_newer_get :: Spec
unit_late_head_preserves_newer_get = it "keeps a GET published while HEAD was in flight" $ do
  let fresh = (H.hCacheControl, "max-age=60")
      old = [fresh, (H.hETag, "\"old\"")]
      revised = [fresh, (H.hETag, "\"head\"")]
      newest = [fresh, (H.hETag, "\"newest\"")]
  (inner, calls, _) <- mkTransport [(S.status200, old, "old"), (S.status200, revised, ""), (S.status200, newest, "latest")]
  started <- newEmptyMVar
  release <- newEmptyMVar
  finished <- newEmptyMVar
  let t = Transport $ \request -> do
        reply <- sendRaw inner request
        when (WReq.method request == M.mHead) (putMVar started () >> takeMVar release)
        pure reply
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/late-head"
  let fetch request = sendRaw (withCache cache t) request >>= drainBody
  fetch req `shouldReturn` "old"
  _ <- forkFinally (fetch req{WReq.method = M.mHead}) (putMVar finished)
  (takeMVar started >> (fetch (WReq.setHeader H.hCacheControl "no-cache" req) `shouldReturn` "latest"))
    `finally` putMVar release ()
  (takeMVar finished >>= either throwIO pure) `shouldReturn` ""
  fetch req `shouldReturn` "latest"
  readIORef calls `shouldReturn` 3


unit_uncacheable_replacement :: Spec
unit_uncacheable_replacement = it "retires the stale entry when the full replacement cannot be stored" $ do
  let old = [(H.hCacheControl, "max-age=0"), (H.hETag, "\"old\"")]
      check status fields payload = do
        (t, calls, _) <- mkTransport [(S.status200, old, "old"), (status, fields, payload)]
        cache <- newCache defaultCacheConfig{ccMaxBodyBytes = 8}
        req <- makeRequest "/uncacheable-replacement"
        let fetch = sendRaw (withCache cache t)
            cachedOnly = WReq.setHeader H.hCacheControl "only-if-cached, max-stale" req
        (fetch req >>= drainBody) `shouldReturn` "old"
        (fetch req >>= drainBody) `shouldReturn` payload
        cacheSize cache `shouldReturn` 0
        reply <- fetch cachedOnly
        Resp.statusCode reply `shouldBe` S.status504
        readIORef calls `shouldReturn` 2
  check S.status200 [(H.hCacheControl, "no-store")] "new"
  check S.status200 [(H.hVary, "*")] "new"
  check S.status200 [(H.hCacheControl, "max-age=60")] "more than eight bytes"
  check S.status404 [] "gone"


unit_failed_validation_keeps_entry :: Spec
unit_failed_validation_keeps_entry = it "keeps the stored response after a failed validation" $ do
  let old = [(H.hCacheControl, "max-age=0"), (H.hETag, "\"old\"")]
  (t, calls, _) <- mkTransport [(S.status200, old, "old"), (S.status503, [], "busy")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/failed-validation-preserves-entry"
  let fetch = sendRaw (withCache cache t)
      cachedOnly = WReq.setHeader H.hCacheControl "only-if-cached, max-stale" req
  (fetch req >>= drainBody) `shouldReturn` "old"
  (fetch req >>= drainBody) `shouldReturn` "busy"
  (fetch cachedOnly >>= drainBody) `shouldReturn` "old"
  readIORef calls `shouldReturn` 2


unit_concurrent_revalidation_preserves_newer :: Spec
unit_concurrent_revalidation_preserves_newer = it "commits revalidation only against the entry it read" $ do
  let old = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"old\"")]
      refreshed = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"new\"")]
      check makeCache slowReply slowBody = do
        (inner, calls, _) <- mkTransport [(S.status200, old, "old"), slowReply, (S.status200, refreshed, "new")]
        started <- newEmptyMVar
        release <- newEmptyMVar
        finished <- newEmptyMVar
        let t = Transport $ \request -> do
              reply <- sendRaw inner request
              n <- readIORef calls
              when (n == 2) (putMVar started () >> takeMVar release)
              pure reply
        cache <- makeCache
        req <- makeRequest "/raced-revalidation"
        let fetch request = sendRaw (withCache cache t) request >>= drainBody
            validate = fetch (WReq.setHeader H.hCacheControl "no-cache" req)
        fetch req `shouldReturn` "old"
        _ <- forkFinally validate (putMVar finished)
        (takeMVar started >> (validate `shouldReturn` "new")) `finally` putMVar release ()
        (takeMVar finished >>= either throwIO pure) `shouldReturn` slowBody
        fetch req `shouldReturn` "new"
        readIORef calls `shouldReturn` 3
  let runCases makeCache = do
        check makeCache (S.status304, old, "") "old"
        check makeCache (S.status200, [(H.hCacheControl, "max-age=60"), (H.hETag, "\"superseded\"")], "superseded") "superseded"
        check makeCache (S.status200, [(H.hCacheControl, "no-store")], "uncached") "uncached"
  runCases (newCache defaultCacheConfig)
  runCases (newInMemoryStore 4 >>= \store -> newSynchronizedCacheWith store defaultCacheConfig)


unit_background_uncacheable_replacement :: Spec
unit_background_uncacheable_replacement = it "retires the stale entry when background revalidation returns no-store" $ do
  let old = [(H.hCacheControl, "max-age=0, stale-while-revalidate=60"), (H.hAge, "1"), (H.hETag, "\"old\"")]
      replacement = [(H.hCacheControl, "no-store")]
  drained <- newEmptyMVar
  (inner, calls, _) <- mkTransport [(S.status200, old, "old"), (S.status200, replacement, "new")]
  let t = Transport $ \req -> do
        reply <- sendRaw inner req
        pure $ if H.lookupHeader H.hCacheControl (Resp.headers reply) == Just "no-store"
          then reply{Resp.bodyPopper = do
                       bytes <- Resp.bodyPopper reply
                       when (BS.null bytes) (putMVar drained ())
                       pure bytes}
          else reply
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/background-uncacheable"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "old"
  fetch `shouldReturn` "old"
  takeMVar drained
  cacheSize cache `shouldReturn` 0
  readIORef calls `shouldReturn` 2


unit_revalidated_vary_uses_sent_request :: Spec
unit_revalidated_vary_uses_sent_request = it "keys a revalidated response on the field lines sent upstream" $ do
  let etag = (H.hETag, "\"v1\"")
      stale = [(H.hCacheControl, "max-age=0"), etag]
      update = [etag, (H.hCacheControl, "max-age=60"), (H.hVary, "If-None-Match")]
  (t, calls, _) <-
    mkTransport [(S.status200, stale, "original"), (S.status304, update, ""), (S.status200, [], "new")]
  cache <- newCache defaultCacheConfig
  req <- makeRequest "/revalidate-request-fields"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "original"
  fetch `shouldReturn` "original"
  fetch `shouldReturn` "new"
  readIORef calls `shouldReturn` 3


unit_validators_preserve_vary :: Spec
unit_validators_preserve_vary = it "keeps foreground validation on the same Vary selection" $ do
  let tag = (H.hETag, "\"v1\"")
      date = (H.hLastModified, "Sun, 06 Nov 1994 08:49:37 GMT")
      tagCondition = (H.hIfNoneMatch, snd tag)
      dateCondition = (H.hIfModifiedSince, snd date)
      language = (H.hAcceptLanguage, "en")
      check varying validators expected = do
        let vary = [(H.hVary, "Accept-Language"), (H.hVary, varying)]
            stale = (H.hCacheControl, "max-age=0") : vary <> validators
            fresh = (H.hCacheControl, "max-age=60") : vary <> validators
            unchanged = not (null expected)
            wantBody = if unchanged then "original" else "replacement"
        count <- newIORef (0 :: Int)
        let send = Transport $ \req -> do
              n <- atomicModifyIORef' count (\old -> (old + 1, old))
              let fields = filter ((/= H.hAcceptLanguage) . fst) (WReq.headers req)
                  (status, headers, bytes)
                    | n == 0 = (S.status200, stale, "original")
                    | fields /= expected = (S.status200, fresh, "another variant")
                    | unchanged = (S.status304, fresh, "")
                    | otherwise = (S.status200, fresh, "replacement")
              body <- BSm.popperFromStrict bytes
              pure (RawResponse status headers body HTTP1_1)
        cache <- newCache defaultCacheConfig
        req <- makeRequest "/vary-validator"
        let fetch = sendRaw (withCache cache send) req{WReq.headers = [language]} >>= drainBody
        fetch `shouldReturn` "original"
        fetch `shouldReturn` wantBody
        fetch `shouldReturn` wantBody
        readIORef count `shouldReturn` 2
  check "iF-nOnE-mAtCh" [tag, date] []
  check "If-Modified-Since" [date] []
  check "If-Modified-Since" [tag, date] [tagCondition]
  check "If-None-Match" [date] [dateCondition]


unit_background_validators_preserve_vary :: Spec
unit_background_validators_preserve_vary = it "refreshes a stale Vary variant without adding its selecting header" $ do
  let tag = (H.hETag, "\"stored\"")
      vary = (H.hVary, "If-None-Match")
      stale = [(H.hCacheControl, "max-age=0, stale-while-revalidate=60"), vary, tag]
      fresh = [(H.hCacheControl, "max-age=60"), vary, tag]
  (peer, calls, sent) <- mkTransport [(S.status200, stale, "old"), (S.status200, fresh, "replacement")]
  store <- newInMemoryStore 4
  inserted <- newEmptyMVar
  let notifying = store{csInsert = \key entry -> csInsert store key entry >> putMVar inserted entry}
      cache = newCacheWith notifying defaultCacheConfig
  req <- makeRequest "/vary-background-validator"
  let fetch = sendRaw (withCache cache peer) req >>= drainBody
  fetch `shouldReturn` "old"
  _ <- takeMVar inserted
  fetch `shouldReturn` "old"
  refreshed <- takeMVar inserted
  ceBody refreshed `shouldBe` "replacement"
  (map WReq.headers <$> readIORef sent) `shouldReturn` replicate 2 (WReq.headers req)
  fetch `shouldReturn` "replacement"
  readIORef calls `shouldReturn` 2


unit_background_revalidated_vary :: Spec
unit_background_revalidated_vary = it "refreshes Vary and freshness after a background 304" $ do
  let etag = (H.hETag, "\"v1\"")
      stale = [(H.hCacheControl, "max-age=0, stale-while-revalidate=60"), etag]
      update = [etag, (H.hCacheControl, "max-age=60"), (H.hVary, "X-Mode")]
  (t, calls, _) <-
    mkTransport [(S.status200, stale, "cached"), (S.status304, update, ""), (S.status200, [], "uncached")]
  store <- newInMemoryStore 4
  inserted <- newEmptyMVar
  let notifying = store{csInsert = insertEntry}
      insertEntry key entry = do
        let stored =
              if ceMaxAge entry == Just 0
                then entry{ceMaxAge = Just (-1)}
                else entry
        csInsert store key stored
        putMVar inserted stored
      cache = newCacheWith notifying defaultCacheConfig
  req <- makeRequest "/vary-background"
  let fetch mode = sendRaw (withCache cache t) req{WReq.headers = [("x-mode", mode)]} >>= drainBody
  fetch "a" `shouldReturn` "cached"
  _ <- takeMVar inserted
  fetch "a" `shouldReturn` "cached"
  refreshed <- takeMVar inserted
  ceMaxAge refreshed `shouldBe` Just 60
  fetch "a" `shouldReturn` "cached"
  readIORef calls `shouldReturn` 2
  fetch "b" `shouldReturn` "uncached"
  readIORef calls `shouldReturn` 3


unit_background_unmatched_304 :: Spec
unit_background_unmatched_304 = it "drains an unmatched background 304 before fetching a replacement" $ do
  let stale = [(H.hCacheControl, "max-age=0, stale-while-revalidate=60"), (H.hETag, "\"old\"")]
      update = [(H.hCacheControl, "max-age=60"), (H.hETag, "\"new\"")]
  (inner, calls, sent) <- mkTransport [(S.status200, stale, "old"), (S.status304, update, ""), (S.status200, update, "new")]
  drained <- newIORef False
  let t = Transport $ \req -> do
        n <- readIORef calls
        if n == 2 then readIORef drained `shouldReturn` True else pure ()
        reply <- sendRaw inner req
        pure $ if Resp.statusCode reply == S.status304
          then reply{Resp.bodyPopper = Resp.bodyPopper reply <* writeIORef drained True}
          else reply
  store <- newInMemoryStore 4
  inserted <- newEmptyMVar
  let notifying = store{csInsert = insertEntry}
      insertEntry key entry = do
        let stored = if ceMaxAge entry == Just 0 then entry{ceMaxAge = Just (-1)} else entry
        csInsert store key stored
        putMVar inserted stored
      cache = newCacheWith notifying defaultCacheConfig
  req <- makeRequest "/background-retry"
  let fetch = sendRaw (withCache cache t) req >>= drainBody
  fetch `shouldReturn` "old"
  _ <- takeMVar inserted
  fetch `shouldReturn` "old"
  refreshed <- takeMVar inserted
  ceBody refreshed `shouldBe` "new"
  fetch `shouldReturn` "new"
  readIORef calls `shouldReturn` 3
  map (H.lookupHeader H.hIfNoneMatch . WReq.headers) <$> readIORef sent
    `shouldReturn` [Nothing, Just "\"old\"", Nothing]


-- ---------------------------------------------------------------------------
-- Pluggable store
-- ---------------------------------------------------------------------------

{- | A custom 'CacheStore' that records every operation it sees.
Used to verify the cache routes its writes / reads through
'CacheStore' instead of touching internal state.
-}
mkAuditedStore :: IO (CacheStore, IORef [String])
mkAuditedStore = do
  inner <- newInMemoryStore 64
  audit <- newIORef ([] :: [String])
  let log_ msg = atomicModifyIORef' audit (\xs -> (msg : xs, ()))
  pure
    ( CacheStore
        { csLookup = \k -> do
            log_ ("lookup " <> show (ckPath k))
            csLookup inner k
        , csInsert = \k v -> do
            log_ ("insert " <> show (ckPath k))
            csInsert inner k v
        , csDelete = \k -> do
            log_ ("delete " <> show (ckPath k))
            csDelete inner k
        , csClear = do
            log_ "clear"
            csClear inner
        , csSize = csSize inner
        }
    , audit
    )


unit_custom_store_is_used :: Spec
unit_custom_store_is_used = it "withCache routes through a custom CacheStore" $ do
  let payload = "audit-body"
  (t, _, _) <-
    mkTransport
      [
        ( S.status200
        , [(H.hCacheControl, "max-age=60"), (H.hContentLength, BS8.pack (show (BS.length payload)))]
        , payload
        )
      ]
  (store, audit) <- mkAuditedStore
  let cache = newCacheWith store defaultCacheConfig
      withC = sendRaw (withCache cache t)
  _ <- makeRequest "/p1" >>= withC >>= drainBody
  _ <- makeRequest "/p1" >>= withC >>= drainBody
  log_ <- reverse <$> readIORef audit
  -- Expected pattern: lookup (miss), insert (after store), lookup (hit).
  let summary = filter (\s -> "lookup" `isPrefix` s || "insert" `isPrefix` s) log_
  (length summary) `shouldBe` 3
  ("lookup" `isPrefix` head summary) `shouldBe` True
  ("insert" `isPrefix` (summary !! 1)) `shouldBe` True
  ("lookup" `isPrefix` (summary !! 2)) `shouldBe` True
  where
    isPrefix p s = take (length p) s == p


unit_custom_store_isolated :: Spec
unit_custom_store_isolated = it "two caches with separate stores don't share state" $ do
  let payload = "isolation-body"
  (t, calls, _) <-
    mkTransport
      [
        ( S.status200
        , [(H.hCacheControl, "max-age=60")]
        , payload
        )
      ,
        ( S.status200
        , [(H.hCacheControl, "max-age=60")]
        , payload
        )
      ]
  storeA <- newInMemoryStore 16
  storeB <- newInMemoryStore 16
  let cacheA = newCacheWith storeA defaultCacheConfig
      cacheB = newCacheWith storeB defaultCacheConfig
  _ <- makeRequest "/iso" >>= sendRaw (withCache cacheA t) >>= drainBody
  _ <- makeRequest "/iso" >>= sendRaw (withCache cacheB t) >>= drainBody
  n <- readIORef calls
  n `shouldBe` 2


unit_in_memory_eviction :: Spec
unit_in_memory_eviction = it "in-memory store honours the entry cap" $ do
  let body = "x"
  (t, _, _) <-
    mkTransport
      [ (S.status200, [(H.hCacheControl, "max-age=60")], body)
      | _ <- [(1 :: Int) .. 6]
      ]
  let cfg = defaultCacheConfig {ccMaxEntries = 3}
  cache <- newCache cfg
  let withC = sendRaw (withCache cache t)
  -- Insert 5 entries into a 3-slot cache.
  mapM_
    (\i -> makeRequest (BS8.pack ("/" <> show i)) >>= withC >>= drainBody)
    [(1 :: Int) .. 5]
  size <- cacheSize cache
  (if (size <= 3) then pure () else expectationFailure ("size " <> show size <> " > cap"))


{- | Verify that 'clearCache' on the Cache wrapper actually calls
the store's 'csClear' (not just the in-memory map).
-}
unit_clear_threads_through :: Spec
unit_clear_threads_through = it "clearCache invokes csClear on the store" $ do
  cleared <- newIORef (0 :: Int)
  inner <- newInMemoryStore 16
  let store =
        CacheStore
          { csLookup = csLookup inner
          , csInsert = csInsert inner
          , csDelete = csDelete inner
          , csClear = atomicModifyIORef' cleared (\n -> (n + 1, ())) >> csClear inner
          , csSize = csSize inner
          }
      cache = newCacheWith store defaultCacheConfig
  clearCache cache
  clearCache cache
  n <- readIORef cleared
  n `shouldBe` 2


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Cache" $
    sequence_
      [ unit_fresh_hits
      , unit_replays_end_to_end_headers
      , unit_bypasses_unusable_response_metadata
      , unit_bypasses_old_hop_fields
      , unit_cache_date_casing
      , unit_replays_corrected_age
      , unit_keeps_request_and_receipt_times
      , unit_age_and_date_can_expire_a_response
      , unit_expires_relative_to_date
      , unit_apparent_age_on_hit
      , unit_bad_age_does_not_use_later_lines
      , unit_validation_replaces_age
      , unit_age_saturation_on_replay
      , unit_age_limits_stale_use
      , unit_no_store_response
      , unit_only_if_cached
      , unit_request_age_limits
      , unit_request_stale_limits
      , unit_only_if_cached_stale
      , unit_request_pragma
      , unit_request_no_store
      , unit_request_stale_if_error
      , unit_mutation_invalidates_target
      , unit_mutation_safe_and_error
      , unit_mutation_invalidates_related
      , unit_mutation_distinguishes_empty_query
      , unit_mutation_ignores_bad_references
      , unit_mutation_blocks_late_publication
      , unit_etag_revalidation
      , unit_forward_request_preconditions
      , unit_cached_entity_tag_conditions
      , unit_cached_date_conditions
      , unit_forward_conditional_cache_misses
      , unit_caller_304_updates_cache
      , unit_late_caller_304_preserves_newer_entry
      , unit_conditional_background_validation
      , unit_unmatched_304
      , unit_validation_fallback_once
      , unit_weak_etag_revalidation
      , unit_last_modified_revalidation
      , unit_invalid_stored_validators
      , unit_cache_bypasses_request_body
      , unit_vary_repeated_response_fields
      , unit_vary_request_lines
      , unit_vary_unusable
      , unit_vary_legacy_cache_entry
      , unit_revalidated_vary_and_freshness
      , unit_revalidated_decoded_body
      , unit_revalidated_encoded_body
      , unit_revalidated_vary_wildcard
      , unit_revalidated_no_store
      , unit_head_freshens_get
      , unit_head_retires_get
      , unit_head_preserves_get_variant
      , unit_late_head_preserves_newer_get
      , unit_uncacheable_replacement
      , unit_failed_validation_keeps_entry
      , unit_concurrent_revalidation_preserves_newer
      , unit_background_uncacheable_replacement
      , unit_revalidated_vary_uses_sent_request
      , unit_validators_preserve_vary
      , unit_background_validators_preserve_vary
      , unit_background_revalidated_vary
      , unit_background_unmatched_304
      , unit_custom_store_is_used
      , unit_custom_store_isolated
      , unit_in_memory_eviction
      , unit_clear_threads_through
      ]
