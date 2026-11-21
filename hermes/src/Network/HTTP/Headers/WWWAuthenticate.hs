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
  parseChallengeFieldLines,

  -- * Rendering
  renderWWWAuthenticate,
  renderAuthChallenge,
  encodeAuthChallengesChecked,

  -- * Re-exports
  AuthScheme (..),
  CredentialParam (..),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (
  AuthScheme (..),
  CredentialContents (..),
  CredentialParam (..),
  Credentials (Credentials),
  authParamParser,
  authParamsUnique,
  encodeCredentialEntriesChecked,
  renderAuthParam,
  token68Parser,
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
  = {- | Just the scheme (rare; some legacy schemes can omit
    everything after the scheme name).
    -}
    ChallengeBare
  | {- | Single @token68@ payload (used for schemes like @Bearer@
    that may carry a single opaque token, though the
    challenge form usually still provides parameters).
    -}
    ChallengeToken68 !ByteString
  | {- | @auth-param@ list, in document order.  Parameter names are
    case-insensitive per RFC 9110 §11.2; we preserve the
    on-the-wire spelling so renderers can round-trip.
    -}
    ChallengeParams ![(ShortText, CredentialParam)]
  deriving stock (Eq, Show)


{- | @WWW-Authenticate@ header value. The field grammar permits an empty
list, while a 401 response is required to contain at least one challenge.
-}
newtype WWWAuthenticate = WWWAuthenticate
  { authChallenges :: [AuthChallenge]
  }
  deriving stock (Eq, Show)


instance KnownHeader WWWAuthenticate where
  type ParseFailure WWWAuthenticate = String
  type Cardinality WWWAuthenticate = 'ZeroOrMore
  type Direction WWWAuthenticate = 'Response


  parseFromHeaders _ = fmap WWWAuthenticate . parseChallengeFieldLines "WWW-Authenticate"


  renderToHeaders _ (WWWAuthenticate cs) =
    [M.toStrictByteString (renderWWWAuthenticate (WWWAuthenticate cs))]


  renderToHeadersChecked _ = fmap pure . encodeAuthChallengesChecked . authChallenges


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
challengesParser = httpList challengeParser


{- | Challenge and parameter separators share a comma. Interpret repeated
lines as one value so a parameter can continue a challenge on the next line.
-}
parseChallengeFieldLines :: String -> NonEmpty ByteString -> Either String [AuthChallenge]
parseChallengeFieldLines name (first :| rest) =
  parseFieldLine name challengesParser $
    case rest of
      [] -> first
      _ -> B.intercalate "," (first : rest)


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
      skipSome $(char ' ')
      (token <* lookahead (ows *> ($(char ',') <|> eof)))
        <|> (ChallengeParams <$> authParamList)
        <|> token
    token = ChallengeToken68 <$> token68Parser


{- | The auth-param list, scheme-aware: stops at any @\",\"@ that
looks like the start of a new challenge rather than another
auth-param.
-}
authParamList :: ParserT st String [(ShortText, CredentialParam)]
authParamList = do
  skipMany comma
  first <- authParamParser
  rest <- many (comma *> skipMany comma *> authParamParser)
  let params = first : rest
  if authParamsUnique params then pure params else err "Duplicate authentication parameter"
  where
    comma = ows *> $(char ',') *> ows


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderWWWAuthenticate :: WWWAuthenticate -> M.Builder
renderWWWAuthenticate (WWWAuthenticate cs) =
  M.intersperse ", " (map renderAuthChallenge cs)


encodeAuthChallengesChecked :: [AuthChallenge] -> Maybe ByteString
encodeAuthChallengesChecked = encodeCredentialEntriesChecked asCredentials
  where
    asCredentials (AuthChallenge scheme contents) = Credentials scheme $ case contents of
      ChallengeBare -> CredentialBare
      ChallengeToken68 token -> CredentialToken token
      ChallengeParams [] -> CredentialBare
      ChallengeParams (first : rest) -> CredentialParams (first :| rest)


renderAuthChallenge :: AuthChallenge -> M.Builder
renderAuthChallenge (AuthChallenge (AuthScheme scheme) contents) =
  case contents of
    ChallengeBare -> R.shortText scheme
    ChallengeToken68 t -> R.shortText scheme <> M.char7 ' ' <> M.byteString t
    ChallengeParams [] -> R.shortText scheme
    ChallengeParams ps ->
      R.shortText scheme
        <> M.char7 ' '
        <> M.intersperse ", " (map renderAuthParam ps)
