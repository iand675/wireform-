{- |
@X-Powered-By@ — de-facto response header advertising the technology stack
(framework, language, or server software) powering the origin server, e.g.
@PHP/8.2.0@ or @Express@.

This header is __not__ IANA-registered and its value is entirely
opaque / host-specific with no agreed grammar, so (per the depth guidance) it
is preserved verbatim as a raw 'ST.ShortText' rather than given a fabricated
structure.

Spec: no formal specification (de-facto); see the OWASP Secure Headers project,
<https://owasp.org/www-project-secure-headers/>.

See also: "Network.HTTP.Headers.Server", "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.XPoweredBy (
  XPoweredBy (..),
  xPoweredByParser,
  renderXPoweredBy,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXPoweredBy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | An @X-Powered-By@ value: an opaque product / technology string.
newtype XPoweredBy = XPoweredBy {poweredBy :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XPoweredBy where
  type ParseFailure XPoweredBy = String
  type Cardinality XPoweredBy = 'ZeroOrOne
  type Direction XPoweredBy = 'Response


  parseFromHeaders _ headers = case runParser xPoweredByParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Powered-By header: " <> show rest
    Fail -> Left "Failed to parse X-Powered-By header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXPoweredBy


  headerName _ = hXPoweredBy


xPoweredByParser :: ParserT st String XPoweredBy
xPoweredByParser = XPoweredBy <$> takeRestShortText


renderXPoweredBy :: XPoweredBy -> M.Builder
renderXPoweredBy (XPoweredBy v) = shortText v
