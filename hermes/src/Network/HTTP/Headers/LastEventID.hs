{- |
The @Last-Event-ID@ request header lets a Server-Sent Events client resume a
stream after the connection drops: on reconnection the browser replays the
@id@ of the last event it received so the server can pick up where it left
off. The field preserves UTF-8 event identifiers verbatim. NUL, LF and CR
are excluded from the value.

Spec (WHATWG HTML, Server-sent events): <https://html.spec.whatwg.org/multipage/server-sent-events.html#the-last-event-id-header>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.KeepAlive", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.LastEventID (
  LastEventID (..),
  lastEventIDParser,
  renderLastEventID,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLastEventID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Parser (err, takeRest)


-- | The opaque identifier of the last event a Server-Sent Events client saw.
newtype LastEventID = LastEventID {lastEventID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader LastEventID where
  type ParseFailure LastEventID = String
  type Cardinality LastEventID = 'ZeroOrOne
  type Direction LastEventID = 'Request


  parseFromHeaders _ (wire :| []) = decodeLastEventID wire
  parseFromHeaders _ _ = Left "Multiple Last-Event-ID field lines"


  renderToHeaders _ = ST.toByteString . lastEventID


  headerName _ = hLastEventID


lastEventIDParser :: ParserT st String LastEventID
lastEventIDParser = takeRest >>= either err pure . decodeLastEventID


decodeLastEventID :: ByteString -> Either String LastEventID
decodeLastEventID wire
  | BS.any isSpecial wire =
      if BS.elem 0 wire || BS.elem 10 wire || BS.elem 13 wire
        then Left "Forbidden byte in Last-Event-ID header"
        else case ST.fromByteString wire of
          Nothing -> Left "Invalid UTF-8 in Last-Event-ID header"
          Just value -> Right $! LastEventID value
  | otherwise = Right $! LastEventID (STU.fromByteStringUnsafe wire)
  where
    isSpecial byte = byte >= 128 || byte == 0 || byte == 10 || byte == 13
{-# INLINE decodeLastEventID #-}


renderLastEventID :: LastEventID -> M.Builder
renderLastEventID (LastEventID i) = shortText i
