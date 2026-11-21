{- |
@Optional-WWW-Authenticate@ — an experimental response header (RFC 8053)
that advertises authentication challenges a client /may/ use, without the
@401 Unauthorized@ semantics of @WWW-Authenticate@. It lets a resource be
served to anonymous clients while still inviting authentication, and is
therefore sent on non-401 (typically @200@) responses. It shares
@WWW-Authenticate@'s challenge grammar (RFC 9110 §11.6.1) exactly, so the
challenge parser and renderer from "Network.HTTP.Headers.WWWAuthenticate"
are reused verbatim.

@
Optional-WWW-Authenticate = 1#challenge
@

Spec: <https://www.rfc-editor.org/rfc/rfc8053#section-3>

See also: "Network.HTTP.Headers.WWWAuthenticate",
"Network.HTTP.Headers.AuthenticationControl",
"Network.HTTP.Headers.ProxyAuthenticate", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.OptionalWWWAuthenticate (
  -- * Type
  OptionalWWWAuthenticate (..),

  -- * Parsing
  optionalWWWAuthenticateParser,

  -- * Rendering
  renderOptionalWWWAuthenticate,

  -- * Re-exports
  AuthChallenge (..),
  ChallengeContents (..),
) where

import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOptionalWWWAuthenticate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.WWWAuthenticate (
  AuthChallenge (..),
  ChallengeContents (..),
  challengesParser,
  encodeAuthChallengesChecked,
  parseChallengeFieldLines,
  renderAuthChallenge,
 )


-- | @Optional-WWW-Authenticate@ value containing advertised challenges.
newtype OptionalWWWAuthenticate = OptionalWWWAuthenticate
  { optionalAuthChallenges :: [AuthChallenge]
  }
  deriving stock (Eq, Show)


instance KnownHeader OptionalWWWAuthenticate where
  type ParseFailure OptionalWWWAuthenticate = String
  type Cardinality OptionalWWWAuthenticate = 'ZeroOrMore
  type Direction OptionalWWWAuthenticate = 'Response


  parseFromHeaders _ fields = parseChallengeFieldLines "Optional-WWW-Authenticate" fields >>= requireChallenges


  renderToHeaders _ (OptionalWWWAuthenticate []) = []
  renderToHeaders _ value = [M.toStrictByteString (renderOptionalWWWAuthenticate value)]


  omitHeaderValue = null . optionalAuthChallenges
  {-# INLINE omitHeaderValue #-}


  renderToHeadersChecked _ = fmap pure . encodeAuthChallengesChecked . optionalAuthChallenges


  headerName _ = hOptionalWWWAuthenticate


optionalWWWAuthenticateParser :: ParserT st String OptionalWWWAuthenticate
optionalWWWAuthenticateParser = challengesParser >>= either err pure . requireChallenges


requireChallenges :: [AuthChallenge] -> Either String OptionalWWWAuthenticate
requireChallenges [] = Left "Optional-WWW-Authenticate requires a challenge"
requireChallenges cs = Right (OptionalWWWAuthenticate cs)


renderOptionalWWWAuthenticate :: OptionalWWWAuthenticate -> M.Builder
renderOptionalWWWAuthenticate (OptionalWWWAuthenticate cs) =
  M.intersperse ", " (map renderAuthChallenge cs)
