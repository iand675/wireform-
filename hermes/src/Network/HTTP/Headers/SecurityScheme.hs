{- |
@Security-Scheme@ — obsolete header from Secure HTTP (S-HTTP) used to negotiate
the security enhancements in force. S-HTTP was moved to Historic status and the
value is host-specific with no live, interoperable grammar, so it is preserved
verbatim as an opaque string.

Spec: <https://datatracker.ietf.org/doc/html/rfc2660 RFC 2660: The Secure HyperText Transfer Protocol> (S-HTTP; Historic).

See also: "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.StrictTransportSecurity", "Network.HTTP.Headers.WWWAuthenticate".
-}
module Network.HTTP.Headers.SecurityScheme (
  SecurityScheme (..),
  securitySchemeParser,
  renderSecurityScheme,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecurityScheme)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Security-Scheme@ value.
newtype SecurityScheme = SecurityScheme {securitySchemeValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SecurityScheme where
  type ParseFailure SecurityScheme = String
  type Cardinality SecurityScheme = 'ZeroOrOne
  type Direction SecurityScheme = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser securitySchemeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Security-Scheme header: " <> show rest
    Fail -> Left "Failed to parse Security-Scheme header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecurityScheme


  headerName _ = hSecurityScheme


securitySchemeParser :: ParserT st String SecurityScheme
securitySchemeParser = SecurityScheme <$> takeRestShortText


renderSecurityScheme :: SecurityScheme -> M.Builder
renderSecurityScheme = shortText . securitySchemeValue
