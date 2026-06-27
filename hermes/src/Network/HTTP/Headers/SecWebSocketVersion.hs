{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 6455 §11.3.5 @Sec-WebSocket-Version@ — carries the WebSocket
protocol version(s). A client request names the single version it
intends to use (currently @13@); a server's @426 Upgrade Required@
response lists the versions it supports.

== Grammar

@
Sec-WebSocket-Version = version
version               = DIGIT | (NZDIGIT DIGIT) | ...   ; 0-255
@

On the response side the value is a comma-separated list of versions,
so the type holds a non-empty list of numbers.

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.5>

See also: "Network.HTTP.Headers.SecWebSocketKey", "Network.HTTP.Headers.SecWebSocketAccept", "Network.HTTP.Headers.SecWebSocketProtocol", "Network.HTTP.Headers.SecWebSocketExtensions", "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.SecWebSocketVersion (
  SecWebSocketVersion (..),
  secWebSocketVersionParser,
  renderSecWebSocketVersion,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | One or more WebSocket version numbers.
newtype SecWebSocketVersion = SecWebSocketVersion {secWebSocketVersions :: NE.NonEmpty Word}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketVersion where
  type ParseFailure SecWebSocketVersion = String
  type Cardinality SecWebSocketVersion = 'ZeroOrOne
  type Direction SecWebSocketVersion = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser secWebSocketVersionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Sec-WebSocket-Version: " <> show rest)
    Fail -> Left "Failed to parse Sec-WebSocket-Version header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketVersion


  headerName _ = hSecWebSocketVersion


secWebSocketVersionParser :: ParserT st String SecWebSocketVersion
secWebSocketVersionParser = do
  ows
  v0 <- anyAsciiDecimalWord
  rest <- many (ows *> $(char ',') *> ows *> anyAsciiDecimalWord)
  ows
  pure (SecWebSocketVersion (v0 NE.:| rest))


renderSecWebSocketVersion :: SecWebSocketVersion -> M.Builder
renderSecWebSocketVersion (SecWebSocketVersion vs) =
  M.intersperse ", " (map M.wordDec (NE.toList vs))
