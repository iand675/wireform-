{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9112 §6.1 @Transfer-Encoding@ — names the transfer codings (e.g.
@chunked@, @gzip@, @deflate@) applied to the message body to form the
payload, in the order they were applied; the recipient reverses them to
recover the content. When present, @chunked@ must come last and delimits
the body in place of @Content-Length@. It is a request and response
header, primarily an HTTP\/1.1 mechanism, and is forbidden in HTTP\/2 and
HTTP\/3.

== Grammar

@
Transfer-Encoding = #transfer-coding
transfer-coding   = token *( OWS ";" OWS transfer-parameter )
@

Spec: <https://www.rfc-editor.org/rfc/rfc9112#section-6.1>

See also: "Network.HTTP.Headers.TE", "Network.HTTP.Headers.Trailer",
"Network.HTTP.Headers.ContentLength", "Network.HTTP.Headers.ContentEncoding".
-}
module Network.HTTP.Headers.TransferEncoding (
  TransferEncoding (..),
  transferEncodingParser,
  renderTransferEncoding,
) where

import Control.Monad.Combinators (sepBy1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTransferEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Transfer-Encoding is a list of codings such as "chunked", "gzip" etc.
newtype TransferEncoding = TransferEncoding {encodings :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader TransferEncoding where
  type ParseFailure TransferEncoding = String
  type Cardinality TransferEncoding = 'ZeroOrOne
  type Direction TransferEncoding = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser transferEncodingParser $ NE.head headers of
    OK te "" -> Right te
    OK _ rest -> Left $ "Unconsumed input after parsing Transfer-Encoding header: " <> show rest
    Fail -> Left "Failed to parse Transfer-Encoding header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderTransferEncoding


  headerName _ = hTransferEncoding


transferEncodingParser :: ParserT st String TransferEncoding
transferEncodingParser = do
  firstCoding <- rfc9110Token
  rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
  pure $ TransferEncoding (firstCoding NE.:| rest)


renderTransferEncoding :: TransferEncoding -> M.Builder
renderTransferEncoding (TransferEncoding codings) =
  M.intersperse ", " $ map shortText $ NE.toList codings
