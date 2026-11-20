{- |
@Ordering-Type@ (RFC 3648 §5.1) — request header used with @MKCOL@ to
declare the semantics of the ordering of a newly created collection.
Its value is an absolute URI identifying the ordering type. The grammar
preserves its spelling, including the special values @DAV:custom@ and
@DAV:unordered@.

Spec: <https://www.rfc-editor.org/rfc/rfc3648.html#section-5.1>

See also: "Network.HTTP.Headers.Position", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.OrderingType (
  OrderingType (..),
  orderingTypeParser,
  renderOrderingType,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (absoluteURI)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOrderingType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The ordering-type URI reference, preserved verbatim.
newtype OrderingType = OrderingType {orderingTypeUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader OrderingType where
  type ParseFailure OrderingType = String
  type Cardinality OrderingType = 'ZeroOrOne
  type Direction OrderingType = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Ordering-Type" orderingTypeParser


  renderToHeaders _ = M.toStrictByteString . renderOrderingType


  headerName _ = hOrderingType


orderingTypeParser :: ParserT st String OrderingType
orderingTypeParser = OrderingType <$> absoluteURI


renderOrderingType :: OrderingType -> M.Builder
renderOrderingType (OrderingType uri) = shortText uri
