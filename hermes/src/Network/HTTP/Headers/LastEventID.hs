{- |
The @Last-Event-ID@ request header lets a Server-Sent Events client resume a
stream after the connection drops: on reconnection the browser replays the
@id@ of the last event it received so the server can pick up where it left
off. The value is an opaque, server-defined string with no further internal
structure, so it is preserved verbatim.

Spec (WHATWG HTML, Server-sent events): <https://html.spec.whatwg.org/multipage/server-sent-events.html#the-last-event-id-header>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.KeepAlive", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.LastEventID (
  LastEventID (..),
  lastEventIDParser,
  renderLastEventID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLastEventID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The opaque identifier of the last event a Server-Sent Events client saw.
newtype LastEventID = LastEventID {lastEventID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader LastEventID where
  type ParseFailure LastEventID = String
  type Cardinality LastEventID = 'ZeroOrOne
  type Direction LastEventID = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser lastEventIDParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing Last-Event-ID header: " <> show rest
      Fail -> Left "Failed to parse Last-Event-ID header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderLastEventID


  headerName _ = hLastEventID


lastEventIDParser :: ParserT st String LastEventID
lastEventIDParser = LastEventID <$> takeRestShortText


renderLastEventID :: LastEventID -> M.Builder
renderLastEventID (LastEventID i) = shortText i
