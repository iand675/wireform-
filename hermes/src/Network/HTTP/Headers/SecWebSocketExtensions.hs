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

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Foreign.Ptr (plusPtr)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketExtensions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Encode.Direct (dWord8, directEncode)


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


  renderToHeadersChecked _ (SecWebSocketExtensions exts)
    | all validExtension exts = Just (encodeExtensions exts)
    | otherwise = Nothing
    where
      validExtension (Extension name params) = isHttpTokenText name && all validParam params
      validParam (ExtensionParam name param) = isHttpTokenText name && maybe True isHttpTokenText param


  headerName _ = hSecWebSocketExtensions


encodeExtensions :: NE.NonEmpty Extension -> ByteString
encodeExtensions (first NE.:| rest) = directEncode size $ \ptr start -> do
  end <- writeExtension ptr start first
  foldM
    ( \offset extension -> do
        comma <- dWord8 ptr offset 44
        space <- dWord8 ptr comma 32
        writeExtension ptr space extension
    )
    end
    rest
  where
    size = foldl' (\n extension -> extensionSize (add n 2) extension) (extensionSize 0 first) rest
    add = BSI.checkedAdd "WebSocket extensions"
    tokenLength = SBS.length . ST.toShortByteString
    extensionSize n (Extension name params) = foldl' parameterSize (add n (tokenLength name)) params
    parameterSize n (ExtensionParam name value) =
      let named = add (add n 1) (tokenLength name)
      in maybe named (\token -> add (add named 1) (tokenLength token)) value
    writeExtension ptr offset (Extension name params) = do
      end <- copyToken ptr offset name
      foldM (writeParam ptr) end params
    writeParam ptr offset (ExtensionParam name value) = do
      sep <- dWord8 ptr offset 59
      end <- copyToken ptr sep name
      case value of
        Nothing -> pure end
        Just token -> dWord8 ptr end 61 >>= \equals -> copyToken ptr equals token
    copyToken ptr offset token = do
      let bytes = ST.toShortByteString token
          len = SBS.length bytes
      SBS.copyToPtr bytes 0 (ptr `plusPtr` offset) len
      pure $! offset + len


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
