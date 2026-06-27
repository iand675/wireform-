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

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOptionalWWWAuthenticate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.WWWAuthenticate (
  AuthChallenge (..),
  ChallengeContents (..),
  challengesParser,
  renderAuthChallenge,
 )


-- | @Optional-WWW-Authenticate@ value: zero-or-more advertised challenges.
newtype OptionalWWWAuthenticate = OptionalWWWAuthenticate
  { optionalAuthChallenges :: [AuthChallenge]
  }
  deriving stock (Eq, Show)


instance KnownHeader OptionalWWWAuthenticate where
  type ParseFailure OptionalWWWAuthenticate = String
  type Cardinality OptionalWWWAuthenticate = 'ZeroOrMore
  type Direction OptionalWWWAuthenticate = 'Response


  parseFromHeaders _ headers = do
    challenges <- traverse parseOne (NE.toList headers)
    pure (OptionalWWWAuthenticate (concat challenges))
    where
      parseOne hdr = case runParser challengesParser hdr of
        OK cs leftover
          | B.null (dropOws leftover) -> Right cs
          | otherwise ->
              Left ("Unconsumed input after parsing Optional-WWW-Authenticate: " <> show leftover)
        Fail -> Left "Failed to parse Optional-WWW-Authenticate header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ (OptionalWWWAuthenticate cs) =
    [M.toStrictByteString (renderOptionalWWWAuthenticate (OptionalWWWAuthenticate cs))]


  headerName _ = hOptionalWWWAuthenticate


optionalWWWAuthenticateParser :: ParserT st String OptionalWWWAuthenticate
optionalWWWAuthenticateParser = OptionalWWWAuthenticate <$> challengesParser


renderOptionalWWWAuthenticate :: OptionalWWWAuthenticate -> M.Builder
renderOptionalWWWAuthenticate (OptionalWWWAuthenticate cs) =
  M.intersperse ", " (map renderAuthChallenge cs)
