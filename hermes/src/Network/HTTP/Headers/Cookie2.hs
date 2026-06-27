{- |
Module      : Network.HTTP.Headers.Cookie2
Description : The obsolete @Cookie2@ request header (RFC 2965)

The @Cookie2@ request header was defined by RFC 2965 (HTTP State Management
Mechanism, version 1) to let a user agent advertise the highest cookie
specification version it understands:

> cookie2        = "Cookie2:" cookie-version
> cookie-version = "$Version" "=" value

For example: @Cookie2: $Version="1"@.

RFC 2965 has been moved to Historic and obsoleted by RFC 6265, which
explicitly deprecates the @Cookie2@ and @Set-Cookie2@ header fields. Because
this header is obsolete and never saw wide deployment, this module follows the
project's depth guidance for historic headers and preserves the value
faithfully as a raw 'ST.ShortText' rather than fabricating a dead structured
grammar.

Spec: <https://www.rfc-editor.org/rfc/rfc2965#section-3.3.5>

See also: "Network.HTTP.Headers.Cookie", "Network.HTTP.Headers.SetCookie2", "Network.HTTP.Headers.SetCookie".
-}
module Network.HTTP.Headers.Cookie2 (
  Cookie2 (..),
  cookie2Parser,
  renderCookie2,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCookie2)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | The obsolete @Cookie2@ request header. The wrapped value is the raw
@cookie-version@ directive (e.g. @$Version="1"@), preserved verbatim.
-}
newtype Cookie2 = Cookie2 {cookie2Value :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Cookie2 where
  type ParseFailure Cookie2 = String
  type Cardinality Cookie2 = 'ZeroOrOne
  type Direction Cookie2 = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser cookie2Parser header of
      OK cookie2 "" -> Right cookie2
      OK _ rest -> Left $ "Unconsumed input after parsing Cookie2 header: " <> show rest
      Fail -> Left "Failed to parse Cookie2 header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCookie2


  headerName _ = hCookie2


cookie2Parser :: ParserT st String Cookie2
cookie2Parser = Cookie2 <$> takeRestShortText


renderCookie2 :: Cookie2 -> M.Builder
renderCookie2 (Cookie2 value) = shortText value
