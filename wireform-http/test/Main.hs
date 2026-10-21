module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.AuthChallenge qualified
import Test.Cache qualified
import Test.Client qualified
import Test.ConcurrencyStress qualified
import Test.Conditional qualified
import Test.Cookies qualified
import Test.Digest qualified
import Test.Http1EdgeCases qualified
import Test.Http1Integration qualified
import Test.Http2EdgeCases qualified
import Test.Http2Integration qualified
import Test.IDN qualified
import Test.Negotiation qualified
import Test.Proxy qualified
import Test.Range qualified
import Test.Redirect qualified
import Test.Retry qualified
import Test.SSE qualified
import Test.SSEIntegration qualified
import Test.SSEReconnect qualified
import Test.Syd
import Test.UrlDecode qualified
import Test.VersionTypes qualified


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-http" $
      sequence_
        [ Test.VersionTypes.tests
        , Test.Negotiation.tests
        , Test.Http1Integration.tests
        , Test.Http1EdgeCases.tests
        , Test.Http2Integration.tests
        , Test.Http2EdgeCases.tests
        , Test.ConcurrencyStress.tests
        , Test.Client.tests
        , Test.SSE.tests
        , Test.SSEIntegration.tests
        , Test.UrlDecode.tests
        , Test.IDN.tests
        , Test.AuthChallenge.tests
        , Test.Cache.tests
        , Test.Conditional.tests
        , Test.Cookies.tests
        , Test.Digest.tests
        , Test.Proxy.tests
        , Test.Range.tests
        , Test.Redirect.tests
        , Test.Retry.tests
        , Test.SSEReconnect.tests
        ]
