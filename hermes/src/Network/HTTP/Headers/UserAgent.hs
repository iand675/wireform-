{- |
The @User-Agent@ request header identifies the client software making the
request — a whitespace-separated sequence of @product@ identifiers (each a
@token@ with an optional @\/version@) interspersed with @comment@s — letting
servers tailor responses or gather statistics. It shares its grammar with the
@Server@ response header.

@
User-Agent = product *( RWS ( product / comment ) )
product    = token [ "/" product-version ]
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.5>

See also: "Network.HTTP.Headers.Server", "Network.HTTP.Headers.From", "Network.HTTP.Headers.AcceptCH".
-}
module Network.HTTP.Headers.UserAgent where

import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util


data UserAgent = UserAgent
  { firstProduct :: Product
  , remainingUserAgentDefinition :: [Either Comment Product]
  }
  deriving stock (Eq, Show)


data Product = Product
  { productName :: !ShortText
  , productVersion :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)


instance KnownHeader UserAgent where
  type ParseFailure UserAgent = String
  type Cardinality UserAgent = 'ZeroOrOne
  type Direction UserAgent = 'Request


  parseFromHeaders _ = parseSingleFieldLine "User-Agent" userAgentParser


  renderToHeaders _ = M.toStrictByteString . renderUserAgent


  renderToHeadersChecked _ (UserAgent first rest) =
    encodeProductSequenceChecked productName productVersion first rest


  headerName _ = hUserAgent


userAgentParser :: ParserT st String UserAgent
userAgentParser = productSequence Product UserAgent


eitherP :: ParserT st e a -> ParserT st e b -> ParserT st e (Either a b)
eitherP l r = (Right <$> r) <|> (Left <$> l)


renderUserAgent :: UserAgent -> M.Builder
renderUserAgent (UserAgent first rest) = renderProductSequence renderProduct first rest
  where
    renderProduct (Product name version) = renderProductToken name version
