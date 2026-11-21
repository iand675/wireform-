{- | The @Server@ response header (RFC 9110 §10.2.4).

@Server@ advertises the software the origin server used to handle the
request, as a whitespace-separated sequence of @product@ identifiers (each
a @token@ with an optional @\/version@) interspersed with @comment@s:

> Server  = product *( RWS ( product / comment ) )
> product = token [ "/" product-version ]

This mirrors the @User-Agent@ request header, which shares the grammar. A
'Server' is modelled as a leading 'Product' plus the remaining
whitespace-separated elements, each either a 'Comment' or a 'Product'.

Comments use the RFC 9110 §5.6.5 grammar, including nested comments and
quoted pairs. The renderer quotes literal delimiters in the opaque text.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.4>

See also: "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.Via", "Network.HTTP.Headers.XPoweredBy".
-}
module Network.HTTP.Headers.Server (
  Server (..),
  Product (..),
  serverParser,
  renderServer,
) where

import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hServer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Grammar.Rendering (encodeProductSequenceChecked, renderProductSequence, renderProductToken)


data Server = Server
  { firstProduct :: Product
  , remainingServerDefinition :: [Either Comment Product]
  }
  deriving stock (Eq, Show)


data Product = Product
  { productName :: !ShortText
  , productVersion :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)


instance KnownHeader Server where
  type ParseFailure Server = String
  type Cardinality Server = 'ZeroOrOne
  type Direction Server = 'Response


  parseFromHeaders _ = parseSingleFieldLine "Server" serverParser


  renderToHeaders _ = M.toStrictByteString . renderServer


  renderToHeadersChecked _ (Server first rest) =
    encodeProductSequenceChecked productName productVersion first rest


  headerName _ = hServer


serverParser :: ParserT st String Server
serverParser = productSequence Product Server


renderServer :: Server -> M.Builder
renderServer (Server first rest) = renderProductSequence renderProduct first rest
  where
    renderProduct (Product name version) = renderProductToken name version
