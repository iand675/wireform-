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

A quoted parameter must decode to a token. Rendering emits that token
directly. Empty parameter lists and value-less parameters are preserved.
Multiple field lines are combined in received order in both directions,
as clarified by RFC 6455 erratum 3433.

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

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
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


  parseFromHeaders _ headers =
    parseListFieldLines "Sec-WebSocket-Extensions" extensionList headers >>= extensionsFromList


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketExtensions


  headerName _ = hSecWebSocketExtensions


secWebSocketExtensionsParser :: ParserT st String SecWebSocketExtensions
secWebSocketExtensionsParser = extensionList >>= either err pure . extensionsFromList


extensionsFromList :: [Extension] -> Either String SecWebSocketExtensions
extensionsFromList = fmap SecWebSocketExtensions . requireNonEmptyList "Sec-WebSocket-Extensions"


extensionList :: ParserT st String [Extension]
extensionList = httpList extension
  where
    extension = do
      tok <- rfc9110Token
      params <- many (ows *> $(char ';') *> ows *> param)
      pure (Extension tok params)
    param = do
      name <- rfc9110Token
      value <- optional (ows *> $(char '=') *> ows *> (rfc9110Token <|> quotedToken))
      pure (ExtensionParam name value)
    quotedToken = ($(char '"') *> rfc9110Token <* $(char '"')) <|> escapedToken
    escapedToken = do
      bytes <- httpQuotedBytes
      if not (B8.null bytes) && B8.all isTokenChar bytes
        then pure (STU.fromByteStringUnsafe bytes)
        else err "Invalid quoted WebSocket extension token"


renderSecWebSocketExtensions :: SecWebSocketExtensions -> M.Builder
renderSecWebSocketExtensions (SecWebSocketExtensions exts) =
  M.intersperse ", " (map renderExtension (NE.toList exts))
  where
    renderExtension (Extension tok params) =
      shortText tok <> mconcat (map renderParam params)
    renderParam (ExtensionParam name value) =
      M.char7 ';' <> shortText name <> maybe mempty (\v -> M.char7 '=' <> shortText v) value
