{-# LANGUAGE OverloadedStrings #-}

{- | Redirect-following middleware (RFC 9110 \u00a715.4).

The middleware follows a redirect with a valid singleton @Location@
up to the configured hop limit. It buffers the request body for
method-preserving redirects. A 303 follows with GET or HEAD, and the
'rpRewriteToGet' policy controls POST after 301 and 302.

Cross-origin hops have @Authorization@ stripped by default to avoid
leaking credentials to a redirect target the caller didn't authorise.
Conditional range requests lose both @Range@ and @If-Range@ when the
target resource or request method changes.

The middleware operates against the wire-level
@'Request' 'BodyStream'@ shape (i.e. it sits below 'send'\\'s body
encoding in the stack) so it can preserve the materialised request
bytes across hops.
-}
module Network.HTTP.Client.Redirect (
  -- * Configuration
  RedirectPolicy (..),
  defaultRedirectPolicy,

  -- * Middleware
  withRedirects,
  withRedirectsWithin,
  withRedirectsUsing,

  -- * Errors
  TooManyRedirects (..),
  RedirectLoop (..),
) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.HashSet qualified as HS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.Encoding qualified as TE
import Data.Text.Short qualified as ShortText
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Client.BodyStream (
  bodyStreamBytes,
  drainPopper,
  streamFromStrict,
 )
import Network.HTTP.Client.Middleware (Duration, seconds, toMicros)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport (Middleware, Transport (..))
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.Location qualified as Location
import Network.HTTP.Internal.RetryAfter (RetryDelay (..), retryAfterWithin)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Network.URI.Template qualified as Template


-- ---------------------------------------------------------------------------
-- Policy
-- ---------------------------------------------------------------------------

data RedirectPolicy = RedirectPolicy
  { rpMaxRedirects :: !Int
  {- ^ Maximum number of redirect transitions. A further eligible redirect
  throws 'TooManyRedirects'.
  -}
  , rpRewriteToGet :: !Bool
  {- ^ When 'True', @301@ and @302@ rewrite @POST@ to @GET@ and
  drop the body, following the historical browser behaviour.
  -}
  , rpStripAuthCrossOrigin :: !Bool
  {- ^ Drop @Authorization@ on cross-origin hops. @True@ by default
  (it's how curl behaves with @--location@; sttp, requests,
  and reqwest all do the same).
  -}
  , rpStripCookieCrossOrigin :: !Bool
  {- ^ Drop the request-side @Cookie@ header on cross-origin hops.
  @True@ by default. Cross-origin cookie leakage is at least
  as severe as 'Authorization', and the cookie jar
  middleware re-attaches the right cookies for the new
  target on the next hop anyway.
  -}
  , rpDetectLoops :: !Bool
  {- ^ Reject redirect loops when a request method and target recur.
  @True@ by default. This catches
  misconfigured servers that bounce indefinitely between two
  URIs without ever exceeding 'rpMaxRedirects'.
  -}
  , rpFollowOn :: !(S.Status -> Bool)
  {- ^ Which statuses to follow. Defaults to 301\/302\/303\/307\/308;
  override to e.g. include 300 (Multiple Choices) when the
  server reliably returns a Location for it.
  -}
  }


defaultRedirectPolicy :: RedirectPolicy
defaultRedirectPolicy =
  RedirectPolicy
    { rpMaxRedirects = 10
    , rpRewriteToGet = True
    , rpStripAuthCrossOrigin = True
    , rpStripCookieCrossOrigin = True
    , rpDetectLoops = True
    , rpFollowOn = isRedirectStatus
    }


isRedirectStatus :: S.Status -> Bool
isRedirectStatus s = case S.statusCode s of
  301 -> True
  302 -> True
  303 -> True
  307 -> True
  308 -> True
  _ -> False


-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data TooManyRedirects = TooManyRedirects
  { trVisited :: ![ByteString]
  {- ^ The normalized targets in order, including the proposed target
  that would exceed the limit.
  -}
  }
  deriving stock (Show)


instance Exception TooManyRedirects


{- | Thrown when 'rpDetectLoops' spots a repeated method and target.
The @rlVisited@ list gives the normalized targets in order. The last
entry identifies the repeated target.
-}
data RedirectLoop = RedirectLoop {rlVisited :: ![ByteString]}
  deriving stock (Show)


instance Exception RedirectLoop


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | Follow redirects with a cumulative five-second Retry-After budget.
withRedirects :: RedirectPolicy -> Middleware IO
withRedirects = withRedirectsWithin (seconds 5)


{- | Limit the cumulative time spent waiting for Retry-After on 3xx
responses. If a minimum wait exceeds the remaining budget, return
that response with its body still readable. A zero budget follows
redirects that require no wait.
-}
withRedirectsWithin :: Duration -> RedirectPolicy -> Middleware IO
withRedirectsWithin = withRedirectsUsing getCurrentTime threadDelay


-- | Supply a wall clock and a wait operation that accepts microseconds.
withRedirectsUsing :: IO UTCTime -> (Int -> IO ()) -> Duration -> RedirectPolicy -> Middleware IO
withRedirectsUsing getTime wait budget policy inner = Transport $ \req0 -> do
  buffered <- bodyStreamBytes (WReq.body req0)
  let current = either (const Nothing) Just (WURI.renderRequestURI (WReq.requestURI req0))
      visited = maybe [] (\uri -> [redirectHop (WReq.method req0) uri]) current
  go visited (rpMaxRedirects policy) (max 0 (toMicros budget)) current req0 buffered
  where
    go visited remaining remainingWait current req payload = do
      body <- streamFromStrict payload
      raw <- sendRaw inner req{WReq.body = body}
      if rpFollowOn policy (statusCode raw)
        then case redirectTarget current (headers raw) of
          Nothing -> pure raw
          Just (RedirectTarget resolved target) -> do
            let code = S.statusCode (statusCode raw)
                method = WReq.method req
                dropBody =
                  code == 303
                    || (rpRewriteToGet policy && method == M.mPost && (code == 301 || code == 302))
                nextMethod = if dropBody && method /= M.mHead then M.mGet else method
                next = redirectHop nextMethod resolved
                chain = next : visited
                crossOrigin = maybe True (not . sameOrigin resolved) current
                fields = WReq.headers req
                nextFields
                  | H.hasHeader H.hIfRange fields
                  , crossOrigin || nextMethod /= method || maybe True (not . samePathAndQuery resolved) current =
                      filter (\(name, _) -> name /= H.hRange && name /= H.hIfRange) fields
                  | otherwise = fields
                rewritten =
                  req
                    { WReq.requestURI = target
                    , WReq.method = nextMethod
                    , WReq.headers = redirectHeaders policy crossOrigin dropBody nextFields
                    }
                nextRequest = if dropBody then WReq.withoutRequestTrailers rewritten else rewritten
                follow micros = do
                  drainPopper (bodyPopper raw)
                  when (remaining <= 0) $
                    throwIO (TooManyRedirects (map hopTarget (reverse chain)))
                  when (rpDetectLoops policy && next `elem` visited) $
                    throwIO (RedirectLoop (map hopTarget (reverse chain)))
                  when (micros > 0) (wait micros)
                  go chain (remaining - 1) (remainingWait - micros) (Just resolved)
                    nextRequest (if dropBody then BS.empty else payload)
            delay <- if code >= 300 && code < 400
              then retryAfterWithin getTime remainingWait (headers raw)
              else pure NoRetryDelay
            case delay of
              NoRetryDelay -> follow 0
              RetryDelay micros -> follow micros
              RetryDelayAboveLimit -> pure raw
        else pure raw


data RedirectTarget = RedirectTarget !WURI.URI !WURI.RequestURI


redirectTarget :: Maybe WURI.URI -> H.Headers -> Maybe RedirectTarget
redirectTarget base fields = do
  wire <- either (const Nothing) id (H.lookupSingletonHeader H.hLocation endToEnd)
  Location.Location location <- either (const Nothing) Just $
    Typed.parseFromHeaders Typed.defaultHeaderSettings (wire :| [])
  let reference = ShortText.toByteString location
  resolved <- WURI.resolveHttpReference base reference
  let target = resolved
        { WURI.uriFragmentComponent = WURI.uriFragmentComponent resolved <|> (base >>= WURI.uriFragmentComponent)
        }
      template = Template.UriTemplate (pure (Template.Literal (TE.decodeUtf8 (WURI.renderURI target))))
  pure (RedirectTarget target (WURI.templateURI template))
  where
    -- Location is end-to-end unless Connection explicitly names it.
    endToEnd = if H.hasHeader H.hConnection fields then H.stripHopByHop fields else fields


data RedirectHop = RedirectHop
  { hopMethod :: !M.Method
  , hopTarget :: !ByteString
  }
  deriving stock (Eq)


redirectHop :: M.Method -> WURI.URI -> RedirectHop
redirectHop method uri =
  RedirectHop
    { hopMethod = method
    , hopTarget =
        WURI.renderURI (WURI.normalizeURI (WURI.withoutUserinfo uri{WURI.uriFragmentComponent = Nothing}))
    }


sameOrigin :: WURI.URI -> WURI.URI -> Bool
sameOrigin a b =
  WURI.uriScheme a == WURI.uriScheme b
    && WURI.uriPort a == WURI.uriPort b
    && CI.foldCase (WURI.uriHost a) == CI.foldCase (WURI.uriHost b)


-- Compare the path and query as sent on the wire, without the fragment.
-- Loop detection also canonicalizes percent escapes and dot segments.
samePathAndQuery :: WURI.URI -> WURI.URI -> Bool
samePathAndQuery a b =
  path a == path b && WURI.uriQueryComponent a == WURI.uriQueryComponent b
  where
    path u
      | BS.null (WURI.uriPath u) = "/"
      | otherwise = WURI.uriPath u


redirectHeaders :: RedirectPolicy -> Bool -> Bool -> H.Headers -> H.Headers
redirectHeaders policy crossOrigin dropBody fields
  | crossOrigin || dropBody = filter keep fields
  | otherwise = fields
  where
    keep (name, _) =
      not (dropBody && HS.member name contentFields)
        && not (crossOrigin && originSpecific name)
    originSpecific name =
      name == H.hHost
        || (rpStripAuthCrossOrigin policy && name == H.hAuthorization)
        || (rpStripCookieCrossOrigin policy && name == H.hCookie)


contentFields :: HS.HashSet H.HeaderName
contentFields =
  HS.fromList
    [ H.hContentEncoding, H.hContentLanguage, H.hContentLocation, H.hContentType
    , H.hContentLength, H.hContentRange, H.hTransferEncoding, H.hTrailer, H.hExpect
    , H.hLastModified, "Digest", "Content-Digest", "Repr-Digest", "Content-Disposition", "Content-MD5"
    ]
