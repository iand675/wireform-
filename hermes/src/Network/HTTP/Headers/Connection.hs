{-# LANGUAGE TemplateHaskell #-}

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
  connectionParser,
  renderConnection,
) where

import Control.Monad.Combinators (sepBy1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hConnection)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Connection header options (e.g., "keep-alive", "close" ...)
newtype Connection = Connection {connectionOptions :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Connection where
  type ParseFailure Connection = String
  type Cardinality Connection = 'ZeroOrOne
  type Direction Connection = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser connectionParser $ NE.head headers of
    OK conn "" -> Right conn
    OK _ rest -> Left $ "Unconsumed input after parsing Connection header: " <> show rest
    Fail -> Left "Failed to parse Connection header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderConnection


  headerName _ = hConnection


connectionParser :: ParserT st String Connection
connectionParser = do
  firstOpt <- rfc9110Token
  rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
  pure $ Connection (firstOpt NE.:| rest)


renderConnection :: Connection -> M.Builder
renderConnection (Connection opts) = M.intersperse ", " $ map shortText $ NE.toList opts
