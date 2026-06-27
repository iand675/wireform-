{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{- |
@WWW-Authenticate@ — the response header an origin server sends on a
@401 Unauthorized@ response to challenge the client for credentials,
naming one or more acceptable authentication schemes (e.g. @Basic@,
@Bearer@, @Digest@) and their parameters (such as @realm@). The client
answers with a matching @Authorization@ request header.

== Grammar (RFC 9110 §11.6.1)

@
WWW-Authenticate = #challenge
challenge        = auth-scheme [ 1*SP ( token68 / #auth-param ) ]
auth-scheme      = token
auth-param       = token BWS \"=\" BWS ( token / quoted-string )
token68          = 1*( ALPHA / DIGIT / \"-\" / \".\" / \"_\" / \"~\"
                       / \"+\" / \"/\" ) *\"=\"
@

Where @#rule@ is the comma-separated list operator (RFC 9110
§5.6.1).  Both the outer challenge list and the inner
@#auth-param@ list use comma separation, which means a naïve
@split-on-comma@ approach is incorrect — it would carve a single
multi-parameter challenge into one \"challenge\" per parameter.

The parser here disambiguates the two roles of the comma by
peeking after each candidate separator: an @auth-param@ is
@token BWS \"=\"@ (a value follows), whereas a new @auth-scheme@
is @token 1*SP@ (a fresh challenge starts).  When the look-ahead
shows an @\"=\"@ the comma is consumed as an auth-param
separator; otherwise the comma is left for the outer challenge
list to consume.

== Module surface

* 'AuthChallenge' is the structured challenge type
  (scheme + 'ChallengeContents').  This replaces the older
  \"scheme + raw rest as ShortText\" representation that did not
  carry enough structure to round-trip RFC-compliantly.
* 'ChallengeContents' distinguishes the three on-the-wire shapes:
  bare scheme, @token68@, and @auth-param@ list.
* 'WWWAuthenticate' is a list of challenges with a 'KnownHeader'
  instance that joins multi-line headers per RFC 9110 §5.3.
* The @auth-scheme@ \/ @auth-param@ vocabulary is shared with
  "Network.HTTP.Headers.Authorization" so the request and
  response sides speak the same parameter types.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.1>

See also: "Network.HTTP.Headers.Authorization",
"Network.HTTP.Headers.ProxyAuthenticate",
"Network.HTTP.Headers.OptionalWWWAuthenticate",
"Network.HTTP.Headers.AuthenticationInfo".
-}
module Network.HTTP.Headers.WWWAuthenticate (
  -- * Types
  WWWAuthenticate (..),
  AuthChallenge (..),
  ChallengeContents (..),

  -- * Parsing
  wwwAuthenticateParser,
  challengesParser,
  challengeParser,

  -- * Rendering
  renderWWWAuthenticate,
  renderAuthChallenge,

  -- * Re-exports
  AuthScheme (..),
  CredentialParam (..),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (
  AuthScheme (..),
  CredentialParam (..),
 )
import Network.HTTP.Headers.HeaderFieldName (hWWWAuthenticate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A single challenge: an authentication scheme plus its content,
which is either nothing (bare scheme), a @token68@ payload, or
a list of @auth-param@s.
-}
data AuthChallenge = AuthChallenge
  { challengeScheme :: !AuthScheme
  , challengeContents :: !ChallengeContents
  }
  deriving stock (Eq, Show)


-- | The shape of a challenge's payload.
data ChallengeContents
  = -- | Just the scheme (rare; some legacy schemes can omit
    --     everything after the scheme name).
    ChallengeBare
  | -- | Single @token68@ payload (used for schemes like @Bearer@
    --     that may carry a single opaque token, though the
    --     challenge form usually still provides parameters).
    ChallengeToken68 !ByteString
  | -- | @auth-param@ list, in document order.  Parameter names are
    --     case-insensitive per RFC 9110 §11.2; we preserve the
    --     on-the-wire spelling so renderers can round-trip.
    ChallengeParams ![(ShortText, CredentialParam)]
  deriving stock (Eq, Show)


{- | @WWW-Authenticate@ header value: zero-or-more challenges.
An empty list is technically out-of-grammar (RFC 9110 requires
at least one challenge on 401), but parsing leniently here
lets callers report \"server sent something we couldn't make
sense of\" rather than throw.
-}
newtype WWWAuthenticate = WWWAuthenticate
  { authChallenges :: [AuthChallenge]
  }
  deriving stock (Eq, Show)


instance KnownHeader WWWAuthenticate where
  type ParseFailure WWWAuthenticate = String
  type Cardinality WWWAuthenticate = 'ZeroOrMore
  type Direction WWWAuthenticate = 'Response


  parseFromHeaders _ headers = do
    challenges <- traverse parseOne (NE.toList headers)
    pure (WWWAuthenticate (concat challenges))
    where
      parseOne hdr = case runParser challengesParser hdr of
        OK cs leftover
          | B.null (dropOws leftover) -> Right cs
          | otherwise ->
              Left ("Unconsumed input after parsing WWW-Authenticate: " <> show leftover)
        Fail -> Left "Failed to parse WWW-Authenticate header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ (WWWAuthenticate cs) =
    [M.toStrictByteString (renderWWWAuthenticate (WWWAuthenticate cs))]


  headerName _ = hWWWAuthenticate


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

{- | Top-level parser for the @WWW-Authenticate@ value.  Returns
the challenge list directly so it can be reused for
@Proxy-Authenticate@ (same grammar per RFC 9110 §11.7.1).
-}
wwwAuthenticateParser :: ParserT st String WWWAuthenticate
wwwAuthenticateParser = WWWAuthenticate <$> challengesParser


{- | Parse a comma-separated list of challenges with scheme-aware
splitting.  Tolerates the RFC 9110 §5.6.1 \"empty list element\"
form (leading \/ trailing \/ stacked commas).
-}
challengesParser :: ParserT st String [AuthChallenge]
challengesParser = do
  ows
  _ <- skipMany (ows *> $(char ','))
  ows
  first <- challengeParser
  rest <- many continueChallenge
  ows
  _ <- skipMany (ows *> $(char ','))
  ows
  pure (first : rest)
  where
    continueChallenge = do
      ows
      $(char ',')
      ows
      _ <- skipMany (ows *> $(char ','))
      ows
      challengeParser


{- | Parse a single challenge: an @auth-scheme@ optionally followed
by SP+ and then a @token68@ or an @auth-param@ list.
-}
challengeParser :: ParserT st String AuthChallenge
challengeParser = do
  scheme <- AuthScheme <$> rfc9110Token
  contents <- challengePayload <|> pure ChallengeBare
  pure AuthChallenge {challengeScheme = scheme, challengeContents = contents}
  where
    challengePayload = do
      _ <- skipSome $(char ' ')
      ows
      (ChallengeParams <$> authParamList)
        <|> (ChallengeToken68 <$> token68Parser)


{- | The auth-param list, scheme-aware: stops at any @\",\"@ that
looks like the start of a new challenge rather than another
auth-param.
-}
authParamList :: ParserT st String [(ShortText, CredentialParam)]
authParamList = do
  first <- authParamP
  rest <- many continueParam
  pure (first : rest)
  where
    continueParam = do
      lookahead $ do
        ows
        $(char ',')
        ows
        _ <- rfc9110Token
        bws
        $(char '=')
      ows
      $(char ',')
      ows
      authParamP

    authParamP = do
      key <- rfc9110Token
      bws
      $(char '=')
      bws
      val <-
        (CredentialParamString <$> rfc8941String)
          <|> (CredentialParamToken <$> rfc9110Token)
      pure (key, val)


{- | RFC 9110 §11.4: @token68 = 1*tchar68 *\"=\"@ where the
@tchar68@ set is the unreserved URI chars plus a few more.
-}
token68Parser :: ParserT st e ByteString
token68Parser =
  byteStringOf $ do
    skipSome (skipSatisfyAscii (`CharSet.member` token68Chars))
    skipMany (skipSatisfyAscii (== '='))
  where
    token68Chars =
      CharSet.fromList (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']) <> "-._~+/"


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderWWWAuthenticate :: WWWAuthenticate -> M.Builder
renderWWWAuthenticate (WWWAuthenticate cs) =
  M.intersperse ", " (map renderAuthChallenge cs)


renderAuthChallenge :: AuthChallenge -> M.Builder
renderAuthChallenge (AuthChallenge (AuthScheme scheme) contents) =
  case contents of
    ChallengeBare -> R.shortText scheme
    ChallengeToken68 t -> R.shortText scheme <> M.char7 ' ' <> M.byteString t
    ChallengeParams [] -> R.shortText scheme
    ChallengeParams ps ->
      R.shortText scheme
        <> M.char7 ' '
        <> M.intersperse ", " (map renderParam ps)
  where
    renderParam (k, v) =
      R.shortText k <> M.char7 '=' <> case v of
        CredentialParamToken t -> R.shortText t
        CredentialParamString s -> R.rfc8941String s
