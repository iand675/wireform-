{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 6455 §11.3.4 @Sec-WebSocket-Protocol@ — names the application
subprotocol(s) used over the WebSocket connection. A client request
offers a comma-separated, preference-ordered list of subprotocol
tokens; a server response echoes the single token it selected.

== Grammar

@
Sec-WebSocket-Protocol = 1#token
@

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4>

See also: "Network.HTTP.Headers.SecWebSocketExtensions", "Network.HTTP.Headers.SecWebSocketVersion", "Network.HTTP.Headers.SecWebSocketKey", "Network.HTTP.Headers.SecWebSocketAccept", "Network.HTTP.Headers.ALPN", "Network.HTTP.Headers.Upgrade".
-}
module Network.HTTP.Headers.SecWebSocketProtocol (
  SecWebSocketProtocol (..),
  secWebSocketProtocolParser,
  renderSecWebSocketProtocol,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketProtocol)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A non-empty, preference-ordered list of subprotocol tokens.
newtype SecWebSocketProtocol = SecWebSocketProtocol {secWebSocketProtocols :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketProtocol where
  type ParseFailure SecWebSocketProtocol = String
  type Cardinality SecWebSocketProtocol = 'ZeroOrOne
  type Direction SecWebSocketProtocol = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser secWebSocketProtocolParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Sec-WebSocket-Protocol: " <> show rest)
    Fail -> Left "Failed to parse Sec-WebSocket-Protocol header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketProtocol


  headerName _ = hSecWebSocketProtocol


secWebSocketProtocolParser :: ParserT st String SecWebSocketProtocol
secWebSocketProtocolParser = do
  ows
  first <- rfc9110Token
  rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
  ows
  pure (SecWebSocketProtocol (first NE.:| rest))


renderSecWebSocketProtocol :: SecWebSocketProtocol -> M.Builder
renderSecWebSocketProtocol (SecWebSocketProtocol ps) =
  M.intersperse ", " (map shortText (NE.toList ps))
