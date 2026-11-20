{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 401 \/ 407 challenge handling (RFC 9110 \u00a711).

Three pieces:

* 'parseAuthChallenges' \/ 'AuthChallenge': lex a
  @WWW-Authenticate@ \/ @Proxy-Authenticate@ header value into a list
  of @scheme@ + parameter pairs. The actual lexing delegates to
  hermes's 'Network.HTTP.Headers.Authorization.credentialsParser',
  which parses the RFC 9110 \u00a711.4 challenge grammar (auth-scheme
  + token68 \/ auth-param list) into a structured 'Credentials'
  value; we just project that into the wireform 'AuthChallenge'
  shape that fits the rest of the client API (case-insensitive
  scheme \/ name comparisons over flat 'ByteString's).
* 'withChallengeAuth' middleware: when the inner transport returns
  @401 Unauthorized@ (or @407 Proxy Authentication Required@) and a
  matching challenge is present, the supplied callback gets a chance
  to compute a follow-up @Authorization@ header and the request is
  re-issued. If the callback returns 'Nothing', the 401 \/ 407
  response is returned to the caller verbatim.
* 'basicChallengeResponder' covers RFC 7617 Basic auth: it matches
  challenges by realm and emits the right header.

Digest is intentionally not implemented \u2014 it's no longer
recommended (its MD5 \/ SHA-256 transcript is brittle in practice
and prone to downgrade) and the wireform stack would rather call
out a missing implementation than ship a half-baked one. The
plumbing here is the substrate for a Digest add-on package if
anyone wants to write one.

== Multi-challenge values

A single @WWW-Authenticate@ header value can carry several
challenges separated by commas. The grammar is ambiguous without
scheme-aware look-ahead because the comma between challenges is
indistinguishable from the comma between auth-params of the same
challenge. The scheme-aware splitter lives in hermes as
'Hermes.WWWAuthenticate.challengesParser'; this module is a thin
projection over its output into the case-insensitive 'ByteString'
vocabulary the rest of the client uses.

The middleware combines repeated field lines in their original order
before parsing them, as required by RFC 9110 §5.3.
-}
module Network.HTTP.Client.AuthChallenge (
  -- * Challenges
  AuthChallenge (..),
  parseAuthChallenges,

  -- * Middleware
  ChallengeResponder,
  withChallengeAuth,
  withProxyChallengeAuth,

  -- * Built-in responders
  basicChallengeResponder,
  bearerChallengeResponder,

  -- * Request-aware variants
  RequestChallengeResponder,
  withRequestChallengeAuth,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Short qualified as ST
import Network.HTTP.Client.BodyStream (BodyStream, drainPopper)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Transport
import Network.HTTP.Headers.Authorization qualified as HAuth
import Network.HTTP.Headers.WWWAuthenticate qualified as HWA
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S


-- ---------------------------------------------------------------------------
-- Challenge data
-- ---------------------------------------------------------------------------

{- | A parsed authentication challenge. Mirrors hermes's 'Credentials'
shape projected onto the case-insensitive 'ByteString' vocabulary
the rest of the client uses.
-}
data AuthChallenge = AuthChallenge
  { acScheme :: !(CI ByteString)
  {- ^ Scheme, e.g. @\"Basic\"@. Compared case-insensitively per
  RFC 9110 \u00a711.4.
  -}
  , acParams :: ![(CI ByteString, ByteString)]
  {- ^ Parameter list, e.g. @[(\"realm\", \"example\")]@. Names
  are compared case-insensitively; values are the unquoted
  bytes (RFC 7230 quoted-string backslash escapes are already
  resolved by the hermes parser).
  -}
  , acToken68 :: !(Maybe ByteString)
  {- ^ Bearer-style schemes carry a single @token68@ form instead
  of a parameter list (e.g. @Negotiate ABC...@).
  -}
  }
  deriving stock (Eq, Show)


{- | Parse a single @WWW-Authenticate@ \/ @Proxy-Authenticate@
header value into the list of challenges it carries. Multiple
challenges in the same value are disambiguated from
intra-challenge auth-param commas by
'Hermes.WWWAuthenticate.challengesParser' (RFC 9110 §11.6.1).
Returns @[]@ if the value doesn't parse.
-}
parseAuthChallenges :: ByteString -> [AuthChallenge]
parseAuthChallenges raw = parseAuthChallengeFields [raw]


parseAuthChallengeFields :: [ByteString] -> [AuthChallenge]
parseAuthChallengeFields [] = []
parseAuthChallengeFields (first : rest) =
  either (const []) (map fromHermesChallenge) $
    HWA.parseChallengeFieldLines "Authentication challenge" (first :| rest)


{- | Project hermes's structured 'HWA.AuthChallenge' onto the
wireform shape (case-insensitive scheme \/ param names, raw
bytes for values).
-}
fromHermesChallenge :: HWA.AuthChallenge -> AuthChallenge
fromHermesChallenge ch =
  AuthChallenge
    { acScheme = CI.mk (schemeBytes (HWA.challengeScheme ch))
    , acParams = case HWA.challengeContents ch of
        HWA.ChallengeParams ps ->
          [(CI.mk (shortToBytes k), HAuth.credentialParamBytes v) | (k, v) <- ps]
        _ -> []
    , acToken68 = case HWA.challengeContents ch of
        HWA.ChallengeToken68 bs -> Just bs
        _ -> Nothing
    }
  where
    schemeBytes (HAuth.AuthScheme s) = shortToBytes s
    shortToBytes = ST.toByteString


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

{- | A user-supplied function that turns a list of challenges into an
@Authorization@ (or @Proxy-Authorization@) header value, or
'Nothing' if it can't satisfy any of them.
-}
type ChallengeResponder = [AuthChallenge] -> IO (Maybe ByteString)


{- | Re-issue a request with an @Authorization@ header when the inner
transport returns @401@ with a parseable challenge that the
responder accepts. Limits to a single retry per request \u2014
multi-leg negotiations (Negotiate, NTLM) need a per-scheme
middleware; this one is the common case.
-}
withChallengeAuth :: ChallengeResponder -> Middleware IO
withChallengeAuth = challengeAuth 401 H.hAuthorization H.hWWWAuthenticate


withProxyChallengeAuth :: ChallengeResponder -> Middleware IO
withProxyChallengeAuth = challengeAuth 407 H.hProxyAuthorization H.hProxyAuthenticate


challengeAuth
  :: Int
  -- ^ Status code that triggers a retry (401 or 407).
  -> H.HeaderName
  -- ^ Header to set on the retry (Authorization \/ Proxy-Authorization).
  -> H.HeaderName
  -- ^ Challenge header on the response (WWW-Authenticate \/ Proxy-Authenticate).
  -> ChallengeResponder
  -> Middleware IO
challengeAuth triggerCode authHdr challengeHdr responder inner = Transport $ \req -> do
  raw <- sendRaw inner req
  let code = fromIntegral (S.statusCode (Resp.statusCode raw)) :: Int
  if code /= triggerCode
    then pure raw
    else do
      let challenges =
            parseAuthChallengeFields (H.lookupHeaders challengeHdr (Resp.headers raw))
      case challenges of
        [] -> pure raw
        cs -> do
          mAuth <- responder cs
          case mAuth of
            Nothing -> pure raw
            Just header -> do
              -- Drain the failure response so the connection / mock
              -- can advance.
              drainPopper (Resp.bodyPopper raw)
              let req' =
                    req
                      { WReq.headers =
                          H.insertHeader authHdr header (WReq.headers req)
                      }
              sendRaw inner req'


-- ---------------------------------------------------------------------------
-- Request-aware variant
-- ---------------------------------------------------------------------------

{- | Like 'ChallengeResponder' but sees the outgoing request, so it
can read the method, request-target, body length, etc. when
constructing the auth header.  This is what Digest needs
(RFC 7616 §3.4 — the @response@ digest is keyed on the request
method + URI), and is the right shape for any other scheme
whose answer is request-specific.

The body type is fixed to 'BodyStream' because that's the body
shape the middleware layer sees (per 'Transport'); responders
that don't care about the body should pattern-match on it as
@_@.
-}
type RequestChallengeResponder =
  WReq.Request BodyStream
  -> [AuthChallenge]
  -> IO (Maybe ByteString)


{- | Middleware variant of 'withChallengeAuth' that hands the
responder the request being retried.  Wire your Digest
responder up through this; for Basic \/ Bearer the simpler
'withChallengeAuth' is enough.
-}
withRequestChallengeAuth :: RequestChallengeResponder -> Middleware IO
withRequestChallengeAuth responder inner = Transport $ \req -> do
  raw <- sendRaw inner req
  let code = fromIntegral (S.statusCode (Resp.statusCode raw)) :: Int
  if code /= 401
    then pure raw
    else do
      let challenges =
            parseAuthChallengeFields (H.lookupHeaders H.hWWWAuthenticate (Resp.headers raw))
      case challenges of
        [] -> pure raw
        cs -> do
          mAuth <- responder req cs
          case mAuth of
            Nothing -> pure raw
            Just header -> do
              drainPopper (Resp.bodyPopper raw)
              let req' =
                    req
                      { WReq.headers =
                          H.insertHeader H.hAuthorization header (WReq.headers req)
                      }
              sendRaw inner req'


-- ---------------------------------------------------------------------------
-- Basic
-- ---------------------------------------------------------------------------

{- | A 'ChallengeResponder' that fulfils RFC 7617 Basic challenges
when the supplied per-realm credentials match.
-}
basicChallengeResponder
  :: (ByteString -> Maybe (ByteString, ByteString))
  -- ^ realm \u2192 (user, password). Pass @const Nothing@ to refuse.
  -> ChallengeResponder
basicChallengeResponder lookupCreds challenges =
  pure $
    firstJust
      [ case lookupCreds realm of
          Just (u, p) -> Just ("Basic " <> B64.encode (u <> ":" <> p))
          Nothing -> Nothing
      | ch <- challenges
      , acScheme ch == basic
      , let realm = case lookup (CI.mk "realm") (acParams ch) of
              Just r -> r
              Nothing -> ""
      ]
  where
    basic = CI.mk "Basic"


firstJust :: [Maybe a] -> Maybe a
firstJust = go
  where
    go [] = Nothing
    go (Just x : _) = Just x
    go (Nothing : r) = go r


-- ---------------------------------------------------------------------------
-- Bearer (RFC 6750)
-- ---------------------------------------------------------------------------

{- | A 'ChallengeResponder' for RFC 6750 @Bearer@ challenges. The
callback is consulted with each challenge's @realm@ (empty
'ByteString' if absent) and is expected to return the
caller's access token; the responder then emits
@Authorization: Bearer \<token\>@.

Pass @const Nothing@ to refuse all Bearer challenges (useful
when stacking multiple responders so a Bearer-then-Basic
combination falls through to Basic when no token is provisioned
for a given realm).
-}
bearerChallengeResponder
  :: (ByteString -> Maybe ByteString)
  -- ^ realm → access token.
  -> ChallengeResponder
bearerChallengeResponder lookupToken challenges =
  pure $
    firstJust
      [ case lookupToken realm of
          Just tok -> Just ("Bearer " <> tok)
          Nothing -> Nothing
      | ch <- challenges
      , acScheme ch == bearer
      , let realm = case lookup (CI.mk "realm") (acParams ch) of
              Just r -> r
              Nothing -> ""
      ]
  where
    bearer = CI.mk "Bearer"
