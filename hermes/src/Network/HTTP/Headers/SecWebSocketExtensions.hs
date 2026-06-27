{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 6455 §9.1 / §11.3.2 @Sec-WebSocket-Extensions@ — negotiates
protocol extensions (e.g. @permessage-deflate@) for the WebSocket
connection. The value is a comma-separated list of extensions, each a
token optionally followed by semicolon-separated parameters.

== Grammar

@
Sec-WebSocket-Extensions = extension-list
extension-list           = 1#extension
extension                = extension-token *( ";" extension-param )
extension-token          = registered-token
extension-param          = token [ "=" (token | quoted-string) ]
@

A parameter value may be a token or a quoted-string on the wire; the
parsed value is stored unquoted and re-rendered as a bare token (which
is the only form used by the registered WebSocket extensions). Empty
parameter lists and value-less parameters are preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.2>

See also: "Network.HTTP.Headers.SecWebSocketProtocol", "Network.HTTP.Headers.SecWebSocketKey", "Network.HTTP.Headers.SecWebSocketAccept", "Network.HTTP.Headers.SecWebSocketVersion", "Network.HTTP.Headers.Upgrade".
-}
module Network.HTTP.Headers.SecWebSocketExtensions (
  SecWebSocketExtensions (..),
  Extension (..),
  ExtensionParam (..),
  secWebSocketExtensionsParser,
  renderSecWebSocketExtensions,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketExtensions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single @token [ "=" value ]@ extension parameter.
data ExtensionParam = ExtensionParam
  { extensionParamName :: !ST.ShortText
  , extensionParamValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | An @extension-token@ together with its (possibly empty) parameter list.
data Extension = Extension
  { extensionToken :: !ST.ShortText
  , extensionParams :: ![ExtensionParam]
  }
  deriving stock (Eq, Show)


-- | A non-empty list of negotiated extensions.
newtype SecWebSocketExtensions = SecWebSocketExtensions {extensions :: NE.NonEmpty Extension}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketExtensions where
  type ParseFailure SecWebSocketExtensions = String
  type Cardinality SecWebSocketExtensions = 'ZeroOrOne
  type Direction SecWebSocketExtensions = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser secWebSocketExtensionsParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Sec-WebSocket-Extensions: " <> show rest)
    Fail -> Left "Failed to parse Sec-WebSocket-Extensions header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketExtensions


  headerName _ = hSecWebSocketExtensions


secWebSocketExtensionsParser :: ParserT st String SecWebSocketExtensions
secWebSocketExtensionsParser = do
  ows
  first <- extension
  rest <- many (ows *> $(char ',') *> ows *> extension)
  ows
  pure (SecWebSocketExtensions (first NE.:| rest))
  where
    extension = do
      tok <- rfc9110Token
      params <- many (ows *> $(char ';') *> ows *> param)
      pure (Extension tok params)
    param = do
      name <- rfc9110Token
      value <- optional ($(char '=') *> (quotedString <|> rfc9110Token))
      pure (ExtensionParam name value)


renderSecWebSocketExtensions :: SecWebSocketExtensions -> M.Builder
renderSecWebSocketExtensions (SecWebSocketExtensions exts) =
  M.intersperse ", " (map renderExtension (NE.toList exts))
  where
    renderExtension (Extension tok params) =
      shortText tok <> mconcat (map renderParam params)
    renderParam (ExtensionParam name value) =
      M.char7 ';' <> shortText name <> maybe mempty (\v -> M.char7 '=' <> shortText v) value
