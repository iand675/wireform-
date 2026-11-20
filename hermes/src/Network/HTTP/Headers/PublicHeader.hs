{- |
@Public@ — obsolete HTTP/1.1 (RFC 2068) response header advertising the full
set of request methods supported by the server. It has a simple, live token-list
grammar, so it is parsed structurally into a non-empty list of method tokens.

@
Public = "Public" ":" 1#method
@

Spec: <https://datatracker.ietf.org/doc/html/rfc2068#section-14.35 RFC 2068 §14.35> (dropped in RFC 2616).

See also: "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.Safe", "Network.HTTP.Headers.URI".
-}
module Network.HTTP.Headers.PublicHeader (
  PublicHeader (..),
  publicHeaderParser,
  renderPublicHeader,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPublicHeader)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedText, renderSeparated, shortText)


-- | A non-empty list of method tokens advertised by the server.
newtype PublicHeader = PublicHeader {publicMethods :: NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader PublicHeader where
  type ParseFailure PublicHeader = String
  type Cardinality PublicHeader = 'ZeroOrOne
  type Direction PublicHeader = 'Response


  parseFromHeaders _ (wire :| []) = parseFieldLine "Public" publicHeaderParser wire
  parseFromHeaders _ fields =
    PublicHeader <$> (parseListFieldLines "Public" (httpList rfc9110Token) fields >>= requireNonEmptyList "Public")


  renderToHeaders _ = encodeCommaSeparatedText . publicMethods


  headerName _ = hPublicHeader


publicHeaderParser :: ParserT st String PublicHeader
publicHeaderParser = PublicHeader <$> httpList1 "Public" rfc9110Token


renderPublicHeader :: PublicHeader -> M.Builder
renderPublicHeader (PublicHeader (first :| rest)) =
  renderSeparated ", " shortText (first : rest)
