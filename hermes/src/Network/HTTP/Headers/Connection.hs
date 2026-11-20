{- |
RFC 9110 §7.6.1 @Connection@ — a hop-by-hop control field listing
connection options for the current connection. Each option is either a
directive governing the connection itself (notably @close@, and the
legacy @keep-alive@) or the name of another header field that applies
only to this hop and must be stripped by intermediaries before
forwarding. It is both a request and response header and is forbidden in
HTTP/2 and HTTP/3.

== Grammar

@
Connection        = #connection-option
connection-option = token
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1>

See also: "Network.HTTP.Headers.Close", "Network.HTTP.Headers.KeepAlive",
"Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.TE".
-}
module Network.HTTP.Headers.Connection (
  Connection (..),
  connectionHasClose,
  connectionParser,
  renderConnection,
) where

import qualified Data.CaseInsensitive as CI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hConnection)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedText, renderSeparated, shortText)


-- | Connection header options (e.g., "keep-alive", "close" ...)
data Connection
  = Connection {connectionOptions :: NE.NonEmpty ST.ShortText}
  | ConnectionEmpty
  deriving stock (Eq, Show)


instance KnownHeader Connection where
  type ParseFailure Connection = String
  type Cardinality Connection = 'ZeroOrOne
  type Direction Connection = 'RequestAndResponse


  parseFromHeaders _ = fmap fromOptions . parseListFieldLines "Connection" (httpList rfc9110Token)


  renderToHeaders _ ConnectionEmpty = ""
  renderToHeaders _ (Connection opts) = encodeCommaSeparatedText opts


  headerName _ = hConnection


connectionParser :: ParserT st String Connection
connectionParser = fromOptions <$> httpList rfc9110Token


-- | The case-insensitive HTTP/1.1 connection tear-down option.
connectionHasClose :: Connection -> Bool
connectionHasClose ConnectionEmpty = False
connectionHasClose (Connection opts) = any closes opts
  where
    closes opt = opt == "close" || CI.foldCase (ST.toByteString opt) == "close"


fromOptions :: [ST.ShortText] -> Connection
fromOptions = maybe ConnectionEmpty Connection . NE.nonEmpty


renderConnection :: Connection -> M.Builder
renderConnection ConnectionEmpty = mempty
renderConnection (Connection opts) = renderSeparated ", " shortText (NE.toList opts)
