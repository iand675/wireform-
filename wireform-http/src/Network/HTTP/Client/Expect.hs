{-# LANGUAGE OverloadedStrings #-}

{- | @Expect: 100-continue@ helpers (RFC 9110 §10.1.1).

== Two-stage send (§1.2 audit fix)

'withExpectContinue' sets the @Expect: 100-continue@ header on
qualifying requests. The HTTP/1.1 and HTTP/2 clients send the head and
wait for @100 Continue@ or a final response, with a one-second default.
A continue or a timeout releases the body. An early final response
returns without running the body producer.
-}
module Network.HTTP.Client.Expect (
  -- * Configuration
  ExpectConfig (..),
  defaultExpectConfig,

  -- * Middleware
  withExpectContinue,

  -- * Helpers
  expect100ContinueHeader,
  hasExpectContinueHeader,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Client.BodyStream (knownSize)
import Network.HTTP.Client.Middleware (Duration, seconds, toMicros)
import Network.HTTP.Client.Protocol (withContinueTimeoutMicros)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Transport
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.ContentLength (ContentLength (..))
import Network.HTTP.Headers.Expect (hasContinueExpectation)
import Network.HTTP.Types.Header qualified as H


-- | Configuration for the @Expect: 100-continue@ middleware.
data ExpectConfig = ExpectConfig
  { expectMinBodyBytes :: !Int
  {- ^ Attach @Expect: 100-continue@ when the declared or known
  content length is at least this many bytes.
  Skipping the protocol for tiny bodies is the
  industry-standard heuristic; the round-trip cost dominates
  the savings below ~1\u201316 KiB.
  -}
  , expectTimeout :: !Duration
  {- ^ How long to wait for an interim 100 Continue before sending
  the body anyway (RFC 9110 \u00a710.1.1: \"a client SHOULD NOT
  wait for an indefinite period\"). Defaults to one second.
  A zero timeout lets the transport send the content immediately.
  -}
  }


defaultExpectConfig :: ExpectConfig
defaultExpectConfig =
  ExpectConfig
    { expectMinBodyBytes = 1024
    , expectTimeout = seconds 1
    }


{- | Attach @Expect: 100-continue@ to outgoing requests with
nonempty content that meets the size threshold, or with an unknown
content length. An existing bare expectation uses the configured
timeout too.
-}
withExpectContinue :: ExpectConfig -> Middleware IO
withExpectContinue cfg inner = Transport $ \req ->
  let hdrs = WReq.headers req
      size = knownSize (WReq.body req)
      minimumBytes = max 0 (expectMinBodyBytes cfg)
      qualifies = size /= Just 0 && case H.lookupHeaders H.hContentLength hdrs of
        [] -> maybe True (\n -> n > 0 && n >= fromIntegral minimumBytes) size
        first : rest -> case Typed.parseFromHeaders Typed.defaultHeaderSettings (first :| rest) of
          Right (ContentLength n) -> n > 0 && n >= fromIntegral minimumBytes
          Left _ -> False
      prepared
        | hasExpectContinueHeader hdrs = withTimeoutHint req
        | qualifies = withTimeoutHint (WReq.addHeader H.hExpect expect100ContinueHeader req)
        | otherwise = req
  in sendRaw inner prepared
  where
    withTimeoutHint = WReq.addProtocolHint (withContinueTimeoutMicros (toMicros (expectTimeout cfg)))


expect100ContinueHeader :: ByteString
expect100ContinueHeader = "100-continue"


-- | The combined @Expect@ list contains a bare @100-continue@.
hasExpectContinueHeader :: H.Headers -> Bool
hasExpectContinueHeader = hasContinueExpectation . H.lookupHeaders H.hExpect
