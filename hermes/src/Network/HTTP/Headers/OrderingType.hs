{- |
@Ordering-Type@ (RFC 3648 §5.1) — request header used with @MKCOL@ to
declare the semantics of the ordering of a newly created collection.
Its value is a @Simple-ref@ (an absolute URI or a path-absolute
reference) identifying the ordering type, e.g.
@http://example.com/orderings/custom.xml@.

The value is a URI reference, preserved verbatim as a faithful raw
newtype (mirroring 'Network.HTTP.Headers.Referer.Referer').

Spec: <https://www.rfc-editor.org/rfc/rfc3648.html#section-5.1>

See also: "Network.HTTP.Headers.Position", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.OrderingType (
  OrderingType (..),
  orderingTypeParser,
  renderOrderingType,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
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


  parseFromHeaders _ headers = case runParser orderingTypeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Ordering-Type header: " <> show rest
    Fail -> Left "Failed to parse Ordering-Type header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderOrderingType


  headerName _ = hOrderingType


orderingTypeParser :: ParserT st String OrderingType
orderingTypeParser = OrderingType <$> takeRestShortText


renderOrderingType :: OrderingType -> M.Builder
renderOrderingType (OrderingType uri) = shortText uri
