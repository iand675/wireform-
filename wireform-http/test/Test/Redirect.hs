{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for "Network.HTTP.Client.Redirect" — the redirect-following
middleware. Covers:

* 301 / 302 rewriting POST to GET (rpRewriteToGet default).
* 303 follows with GET or HEAD.
* 307 / 308 preserve method.
* Cross-origin redirect strips Host, Authorization and Cookie.
* Loop detection on normalised URI key.
* Hop count exceeded.
-}
module Test.Redirect (tests) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Middleware (millis, seconds)
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Redirect
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Mock transport that returns a scripted sequence
-- ---------------------------------------------------------------------------

mkScripted
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef [Request BSm.BodyStream])
mkScripted script = do
  scriptRef <- newIORef script
  reqsRef <- newIORef ([] :: [Request BSm.BodyStream])
  let t = Transport $ \req -> do
        atomicModifyIORef' reqsRef (\xs -> (req : xs, ()))
        next <- atomicModifyIORef' scriptRef $ \s -> case s of
          (x : rest) -> (rest, x)
          [] -> ([], (S.status500, [], "exhausted"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          Resp.RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (t, reqsRef)


mkRequest :: String -> M.Method -> IO (Request BSm.BodyStream)
mkRequest url meth = case WURI.parseTemplate url of
  Left e -> error (show e)
  Right t -> do
    r <- prepareRequest [] (get t)
    pure r {WReq.method = meth}


-- ---------------------------------------------------------------------------
-- 3xx mechanics
-- ---------------------------------------------------------------------------

unit_302_to_get :: Spec
unit_302_to_get = it "302 rewrites POST to GET by default" $ do
  (t, reqs) <-
    mkScripted
      [ (S.status302, [(H.hLocation, "http://example.com/landing")], "")
      , (S.status200, [], "landed")
      ]
  req <- mkRequest "http://example.com/start" M.mPost
  _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs <- reverse <$> readIORef reqs
  (length xs) `shouldBe` 2
  (WReq.method (xs !! 0)) `shouldBe` M.mPost
  -- After the 302 the method should have been rewritten to GET.
  (WReq.method (xs !! 1)) `shouldBe` M.mGet


unit_307_preserves_method :: Spec
unit_307_preserves_method = it "307 preserves the method and body" $ do
  (t, reqs) <-
    mkScripted
      [ (S.status307, [(H.hLocation, "http://example.com/again")], "")
      , (S.status200, [], "ok")
      ]
  req <- mkRequest "http://example.com/start" M.mPost
  _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
  xs <- reverse <$> readIORef reqs
  (length xs) `shouldBe` 2
  (WReq.method (xs !! 0)) `shouldBe` M.mPost
  (WReq.method (xs !! 1)) `shouldBe` M.mPost


unit_rejects_invalid_locations :: Spec
unit_rejects_invalid_locations = it "returns redirects with invalid or repeated Location fields and leaves the body readable" $ do
  let candidates =
        [ [(H.hLocation, "/landing"), (H.hLocation, "/landing")]
        , [(H.hLocation, "/not a URI")]
        , [(H.hLocation, "mailto:other@example.com")]
        , [(H.hLocation, "http://example.com:65536/landing")]
        , [(H.hConnection, "keep-alive"), (H.hLocation, "/landing"), (H.hConnection, "upgrade, LoCaTiOn")]
        ]
  req <- mkRequest "http://example.com/start" M.mGet
  forM_ candidates $ \fields -> do
    (t, reqs) <- mkScripted [(S.status302, fields, "original"), (S.status200, [], "unexpected")]
    raw <- sendRaw (withRedirects defaultRedirectPolicy t) req
    Resp.statusCode raw `shouldBe` S.status302
    BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "original"
    (length <$> readIORef reqs) `shouldReturn` 1


unit_resolves_location_references :: Spec
unit_resolves_location_references = it "resolves query-only Location fields and inherits fragments only when omitted" $ do
  let step loc = (S.status302, [(H.hLocation, loc)], "redirect")
  (t, reqs) <- mkScripted
    [ step "?p=2", step "?", step "../done#", step "HTTP://EXAMPLE.COM:80/final"
    , (S.status200, [], "landed")
    ]
  req <- mkRequest "http://example.com/a/b?p=1#original" M.mGet
  raw <- sendRaw (withRedirects defaultRedirectPolicy t) req
  BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"
  requests <- drop 1 . reverse <$> readIORef reqs
  let targets r = do
        uri <- WURI.renderRequestURI (WReq.requestURI r)
        pure (WURI.uriPathAndQuery uri, WURI.renderURI uri)
  traverse targets requests `shouldBe` Right
    [ ("/a/b?p=2", "http://example.com/a/b?p=2#original")
    , ("/a/b?", "http://example.com/a/b?#original")
    , ("/done", "http://example.com/done#")
    , ("/final", "http://EXAMPLE.COM:80/final#")
    ]


unit_waits_for_redirect_delay :: Spec
unit_waits_for_redirect_delay = it "drains each redirect and waits before issuing the next request" $ do
  let date = "Thu, 01 Jan 2026 00:00:01 GMT"
  (inner, reqs) <- mkScripted
    [ (S.status302, [(H.hLocation, "/second"), (H.hRetryAfter, " \t1 \t")], "first redirect")
    , (S.status307, [(H.hLocation, "/final"), (H.hRetryAfter, date)], "second redirect")
    , (S.status200, [], "landed")
    ]
  waited <- newIORef []
  drained <- newIORef 0
  clockReads <- newIORef (0 :: Int)
  let transport = Transport $ \request -> do
        response <- sendRaw inner request
        pure response{Resp.bodyPopper = do
          chunk <- Resp.bodyPopper response
          if BS8.null chunk then modifyIORef' drained (+ 1) else pure ()
          pure chunk}
      clock = modifyIORef' clockReads (+ 1) >> pure (UTCTime (fromGregorian 2026 1 1) 0.75000075)
      wait micros = do
        dispatched <- length <$> readIORef reqs
        readIORef drained `shouldReturn` dispatched
        modifyIORef' waited (micros :)
  req <- mkRequest "http://example.com/start" M.mGet
  raw <- sendRaw (withRedirectsUsing clock wait (millis 1250) defaultRedirectPolicy transport) req
  readIORef waited `shouldReturn` [250_000, 1_000_000]
  readIORef clockReads `shouldReturn` 1
  (length <$> readIORef reqs) `shouldReturn` 3
  BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"


unit_preserves_redirect_above_wait_budget :: Spec
unit_preserves_redirect_above_wait_budget = it "returns an untouched redirect when its minimum exceeds the remaining budget" $ do
  let first = (S.status302, [(H.hLocation, "/second"), (H.hRetryAfter, "1")], "first redirect")
      second delay = (S.status307, [(H.hLocation, "/final"), (H.hRetryAfter, delay)], "still waiting")
      final = (S.status200, [], "landed")
      clock = pure (UTCTime (fromGregorian 2026 1 1) 0)
  req <- mkRequest "http://example.com/start" M.mGet
  forM_ ["2", "Thu, 01 Jan 2026 00:00:02 GMT", BS8.pack (show (maxBound :: Word)), BS8.replicate 4096 '9'] $ \delay -> do
    (inner, reqs) <- mkScripted [first, second delay, final]
    waits <- newIORef []
    let wait micros = modifyIORef' waits (micros :)
    raw <- sendRaw (withRedirectsUsing clock wait (seconds 2) defaultRedirectPolicy inner) req
    Resp.statusCode raw `shouldBe` S.status307
    Resp.headers raw `shouldBe` [(H.hLocation, "/final"), (H.hRetryAfter, delay)]
    BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "still waiting"
    (length <$> readIORef reqs) `shouldReturn` 2
    readIORef waits `shouldReturn` [1_000_000]
  (inner, reqs) <- mkScripted [second "6", final]
  raw <- sendRaw (withRedirects defaultRedirectPolicy inner) req
  Resp.statusCode raw `shouldBe` S.status307
  BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "still waiting"
  (length <$> readIORef reqs) `shouldReturn` 1


unit_ignores_unusable_redirect_delays :: Spec
unit_ignores_unusable_redirect_delays = it "follows without waiting for unusable, zero, or past redirect delays" $ do
  let field value = [(H.hRetryAfter, value)]
      clock = pure (UTCTime (fromGregorian 2026 1 1) 0)
      wait _ = expectationFailure "This redirect should be immediate"
      candidates =
        [ (S.status302, [])
        , (S.status302, field "0")
        , (S.status302, field "+100")
        , (S.status302, field "10" <> field "10")
        , (S.status302, field "10" <> [(H.hConnection, "retry-after")])
        , (S.status302, field "Sun, 06 Nov 1994 08:49:37 GMT")
        , (S.status201, field "10")
        ]
      policy = defaultRedirectPolicy{rpFollowOn = \status -> status == S.status302 || status == S.status201}
  req <- mkRequest "http://example.com/start" M.mGet
  forM_ candidates $ \(status, fields) -> do
    (inner, _) <- mkScripted [(status, (H.hLocation, "/done") : fields, ""), (S.status200, [], "landed")]
    raw <- sendRaw (withRedirectsUsing clock wait (seconds 0) policy inner) req
    BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"


unit_preserves_literal_target :: Spec
unit_preserves_literal_target = it "follows a network-path Location with its literal encoded path and query" $ do
  (t, reqs) <- mkScripted
    [ (S.status302, [(H.hLocation, "//example.net:8443/a,b/%7Bpart%7D?q=1,2&x=%2F")], "")
    , (S.status200, [], "landed")
    ]
  req <- mkRequest "https://example.com/start" M.mGet
  raw <- sendRaw (withRedirects defaultRedirectPolicy t) req
  BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"
  requests <- drop 1 . reverse <$> readIORef reqs
  map (WURI.requestURIToText . WReq.requestURI) requests `shouldBe`
    ["https://example.net:8443/a,b/%7Bpart%7D?q=1,2&x=%2F"]


unit_requires_base_for_relative_location :: Spec
unit_requires_base_for_relative_location = it "follows an absolute Location when the transport hid the base URI" $ do
  let success = (S.status200, [], "landed")
      step loc = (S.status302, [(H.hLocation, loc)], "original")
  req <- mkRequest "/start" M.mGet
  forM_ ["/landing", "//example.net/landing"] $ \loc -> do
    (t, reqs) <- mkScripted [step loc, success]
    raw <- sendRaw (withRedirects defaultRedirectPolicy t) req
    Resp.statusCode raw `shouldBe` S.status302
    BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "original"
    (length <$> readIORef reqs) `shouldReturn` 1
  (t, reqs) <- mkScripted [step "https://example.net/landing", success]
  raw <- sendRaw (withRedirects defaultRedirectPolicy t) req
  BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"
  requests <- drop 1 . reverse <$> readIORef reqs
  map (WURI.requestURIToText . WReq.requestURI) requests `shouldBe` ["https://example.net/landing"]


unit_preserves_non_post_redirects :: Spec
unit_preserves_non_post_redirects = it "preserves PATCH on a 302 and HEAD on a 303" $ do
  let check status method payload expected = do
        (t, reqs) <- mkScripted [(status, [(H.hLocation, "/landing")], ""), (S.status200, [], "ok")]
        req <- mkRequest "http://example.com/start" method
        body <- BSm.streamFromStrict payload
        _ <- sendRaw (withRedirects defaultRedirectPolicy t) req{WReq.body = body}
        sent <- reverse <$> readIORef reqs
        map WReq.method sent `shouldBe` [method, method]
        traverse (BSm.bodyStreamBytes . WReq.body) sent `shouldReturn` [payload, expected]
  check S.status302 M.mPatch "PATCH body" "PATCH body"
  check S.status303 M.mHead "" ""


unit_removes_content_fields_on_redirect :: Spec
unit_removes_content_fields_on_redirect = it "drops content metadata when a 303 turns a same-target POST into GET" $ do
  let contentFields =
        [ H.hContentEncoding, H.hContentLanguage, H.hContentLocation, H.hContentType
        , H.hContentLength, H.hContentRange, H.hTransferEncoding, H.hTrailer, H.hExpect
        , H.hLastModified, "Digest", "Content-Digest", "Repr-Digest", "Content-Disposition", "Content-MD5"
        ]
  (t, reqs) <- mkScripted [(S.status303, [(H.hLocation, "/submit")], ""), (S.status200, [], "ok")]
  req <- mkRequest "http://example.com/submit" M.mPost
  body <- BSm.streamFromStrict "submission"
  _ <- sendRaw (withRedirects defaultRedirectPolicy t)
    req{WReq.body = body, WReq.headers = (H.hAccept, "text/plain") : map (\name -> (name, "metadata")) contentFields}
  sent <- reverse <$> readIORef reqs
  map WReq.method sent `shouldBe` [M.mPost, M.mGet]
  case sent of
    [_, follow] -> do
      WReq.headers follow `shouldBe` [(H.hAccept, "text/plain")]
      BSm.bodyStreamBytes (WReq.body follow) `shouldReturn` ""
    _ -> expectationFailure "Expected one redirected request"


-- ---------------------------------------------------------------------------
-- Cross-origin credential stripping
-- ---------------------------------------------------------------------------

unit_strips_auth_cross_origin :: Spec
unit_strips_auth_cross_origin = it
  "Authorization is dropped on cross-origin redirect"
  $ do
    (t, reqs) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://attacker.example/x")], "")
        , (S.status200, [], "")
        ]
    req0 <- mkRequest "http://api.example.com/" M.mGet
    let req =
          req0
            { WReq.headers =
                H.insertHeader H.hAuthorization "Bearer secret" (WReq.headers req0)
            }
    _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
    xs <- reverse <$> readIORef reqs
    let r2 = xs !! 1
    (H.lookupHeader H.hAuthorization (WReq.headers r2)) `shouldBe` Nothing


unit_strips_cookie_cross_origin :: Spec
unit_strips_cookie_cross_origin = it
  "Cookie is dropped on cross-origin redirect"
  $ do
    (t, reqs) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://attacker.example/x")], "")
        , (S.status200, [], "")
        ]
    req0 <- mkRequest "http://api.example.com/" M.mGet
    let req =
          req0
            { WReq.headers =
                H.insertHeader H.hCookie "session=abc; flavour=oatmeal" (WReq.headers req0)
            }
    _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
    xs <- reverse <$> readIORef reqs
    let r2 = xs !! 1
    (H.lookupHeader H.hCookie (WReq.headers r2)) `shouldBe` Nothing


unit_keeps_auth_same_origin :: Spec
unit_keeps_auth_same_origin = it
  "Authorization is kept on same-origin redirect"
  $ do
    (t, reqs) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://api.example.com/landing")], "")
        , (S.status200, [], "")
        ]
    req0 <- mkRequest "http://api.example.com/" M.mGet
    let req =
          req0
            { WReq.headers =
                H.insertHeader H.hAuthorization "Bearer secret" (WReq.headers req0)
            }
    _ <- sendRaw (withRedirects defaultRedirectPolicy t) req
    xs <- reverse <$> readIORef reqs
    let r2 = xs !! 1
    (H.lookupHeader H.hAuthorization (WReq.headers r2)) `shouldBe` (Just "Bearer secret")


unit_uses_effective_redirect_origin :: Spec
unit_uses_effective_redirect_origin = it "compares origin casing and ports, and rebuilds Host when the origin changes" $ do
  let initial = [(H.hAuthorization, "Test credentials"), (H.hCookie, "session=probe"), (H.hHost, "example.com")]
      check loc expected = do
        (t, seen) <- mkScripted [(S.status307, [(H.hLocation, loc)], ""), (S.status200, [], "ok")]
        req <- mkRequest "http://example.com/start" M.mGet
        _ <- sendRaw (withRedirects defaultRedirectPolicy t) req{WReq.headers = initial}
        (map WReq.headers . reverse <$> readIORef seen) `shouldReturn` [initial, expected]
  check "HTTP://EXAMPLE.COM:80/landing" initial
  check "http://example.com:8080/landing" []


unit_conditional_ranges :: Spec
unit_conditional_ranges = describe "conditional ranges" $ do
  it "drops both fields when the redirect changes the target" $ do
    forM_ ["/other", "?version=2", "?", "/%73tart", "https://example.com/start", "http://example.com:8080/start", "http://mirror.example/start"] $ \location ->
      check M.mGet S.status307 location conditional [unrelated]
  it "keeps both fields when only the fragment or origin spelling changes" $
    check M.mGet S.status307 "HTTP://EXAMPLE.COM:80/start#chapter" conditional conditional
  it "keeps an unconditional range across targets" $
    check M.mGet S.status307 "http://mirror.example/start" [range, unrelated] [range, unrelated]
  it "drops both fields when the redirect rewrites the method at the same target" $
    check M.mPost S.status303 "#chapter" conditional [unrelated]
  where
    range = (H.hRange, "bytes=12-")
    unrelated = ("X-Keep", "value")
    conditional = [range, unrelated, ("if-range", "\"revision-1\"")]
    check method status location initial expected = do
      (transport, seen) <- mkScripted [(status, [(H.hLocation, location)], "redirect"), (S.status200, [], "landed")]
      req <- mkRequest "http://example.com/start" method
      let policy = defaultRedirectPolicy{rpDetectLoops = False}
      raw <- sendRaw (withRedirects policy transport) req{WReq.headers = initial}
      BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"
      (map WReq.headers . reverse <$> readIORef seen) `shouldReturn` [initial, expected]


-- ---------------------------------------------------------------------------
-- Loop + hop count
-- ---------------------------------------------------------------------------

unit_detects_loop :: Spec
unit_detects_loop = it
  "rpDetectLoops raises RedirectLoop on revisited URI"
  $ do
    -- Two URIs bouncing between each other. The first hop goes to
    -- /b, the second redirects back to /a. The third would revisit
    -- /b — the loop detector should trip then.
    (t, _) <-
      mkScripted
        [ (S.status302, [(H.hLocation, "http://example.com/b")], "")
        , (S.status302, [(H.hLocation, "http://example.com/a")], "")
        , (S.status302, [(H.hLocation, "http://example.com/b")], "")
        ]
    req <- mkRequest "http://example.com/a" M.mGet
    r <- try (sendRaw (withRedirects defaultRedirectPolicy t) req)
    case r of
      Left (_ :: RedirectLoop) -> pure ()
      Right _ -> error "expected RedirectLoop"


unit_too_many_redirects :: Spec
unit_too_many_redirects = it
  "rpMaxRedirects raises TooManyRedirects"
  $ do
    -- Six redirects in a row, then a 200.  Policy caps at 3 hops.
    let script =
          [ (S.status302, [(H.hLocation, BS8.pack ("http://example.com/" <> show i))], "")
          | i <- [(1 :: Int) .. 6]
          ]
            <> [(S.status200, [], "")]
    (t, _) <- mkScripted script
    let policy =
          defaultRedirectPolicy
            { rpMaxRedirects = 3
            , rpDetectLoops = False -- so we see TooManyRedirects not RedirectLoop
            }
    req <- mkRequest "http://example.com/0" M.mGet
    r <- try (sendRaw (withRedirects policy t) req)
    case r of
      Left (_ :: TooManyRedirects) -> pure ()
      Right _ -> error "expected TooManyRedirects"


unit_no_loop_no_redirect :: Spec
unit_no_loop_no_redirect = it
  "non-redirect response passes through untouched"
  $ do
    (t, reqs) <- mkScripted [(S.status200, [], "hello")]
    req <- mkRequest "http://example.com/x" M.mGet
    r <- sendRaw (withRedirects defaultRedirectPolicy t) req
    (S.statusCode (Resp.statusCode r) == 200) `shouldBe` True
    xs <- readIORef reqs
    (length xs) `shouldBe` 1


unit_loop_uses_request_target :: Spec
unit_loop_uses_request_target = it "detects a first-hop loop across case, default port and fragment changes" $ do
  (t, reqs) <- mkScripted [(S.status302, [(H.hLocation, "HTTP://EXAMPLE.COM:80/path#other")], "")]
  req <- mkRequest "http://example.com/path#base" M.mGet
  r <- try (sendRaw (withRedirects defaultRedirectPolicy t) req)
  case r of
    Left (RedirectLoop visited) -> visited `shouldBe` ["http://example.com/path", "http://example.com/path"]
    Right _ -> expectationFailure "Expected a redirect loop"
  (length <$> readIORef reqs) `shouldReturn` 1


unit_hop_limit_counts_transitions :: Spec
unit_hop_limit_counts_transitions = it "allows a terminal response at the configured hop limit" $ do
  let limits n = defaultRedirectPolicy{rpMaxRedirects = n}
      first = (S.status302, [(H.hLocation, "/landing")], "redirect")
      success = (S.status200, [], "landed")
      check n plan expectedCalls = do
        (t, reqs) <- mkScripted plan
        req <- mkRequest "http://example.com/start" M.mGet
        raw <- sendRaw (withRedirects (limits n) t) req
        BSm.popperBytes (Resp.bodyPopper raw) `shouldReturn` "landed"
        (length <$> readIORef reqs) `shouldReturn` expectedCalls
  check 0 [success] 1
  check 1 [first, success] 2
  (t, reqs) <- mkScripted [first, (S.status302, [(H.hLocation, "/beyond")], "redirect")]
  req <- mkRequest "http://example.com/start" M.mGet
  r <- try (sendRaw (withRedirects (limits 1) t) req)
  case r of
    Left (TooManyRedirects visited) ->
      visited `shouldBe` ["http://example.com/start", "http://example.com/landing", "http://example.com/beyond"]
    Right _ -> expectationFailure "Expected the hop limit to stop the redirect"
  (length <$> readIORef reqs) `shouldReturn` 2


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Network.HTTP.Client.Redirect" $
    sequence_
      [ unit_302_to_get
      , unit_307_preserves_method
      , unit_rejects_invalid_locations
      , unit_resolves_location_references
      , unit_waits_for_redirect_delay
      , unit_preserves_redirect_above_wait_budget
      , unit_ignores_unusable_redirect_delays
      , unit_preserves_literal_target
      , unit_requires_base_for_relative_location
      , unit_preserves_non_post_redirects
      , unit_removes_content_fields_on_redirect
      , unit_strips_auth_cross_origin
      , unit_strips_cookie_cross_origin
      , unit_keeps_auth_same_origin
      , unit_uses_effective_redirect_origin
      , unit_conditional_ranges
      , unit_detects_loop
      , unit_too_many_redirects
      , unit_no_loop_no_redirect
      , unit_loop_uses_request_target
      , unit_hop_limit_counts_transitions
      ]
