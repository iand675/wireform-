{- |
RFC 9110 §7.6.1 @Close@ — a reserved field name registered to avoid
conflicts with the @close@ connection option carried by the
'Network.HTTP.Headers.Connection.Connection' header. It has no value of
its own beyond the literal @close@ connection-option token, so this is a
deliberately thin type.

== Grammar

@
Close = "close"
@

The token is matched case-insensitively and rendered lower-case.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.KeepAlive".
-}
module Network.HTTP.Headers.Close (
  Close (..),
  closeParser,
  renderClose,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hClose)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The @Close@ connection-option token. The only valid value is @close@.
data Close = Close
  deriving stock (Eq, Show)


instance KnownHeader Close where
  type ParseFailure Close = String
  type Cardinality Close = 'ZeroOrOne
  type Direction Close = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser closeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Close header: " <> show rest
    Fail -> Left "Failed to parse Close header"
    Err e -> Left e
  renderToHeaders _ = M.toStrictByteString . renderClose
  headerName _ = hClose


closeParser :: ParserT st String Close
closeParser = do
  tok <- rfc9110Token
  if T.toLower (ST.toText tok) == "close"
    then pure Close
    else failed


renderClose :: Close -> M.Builder
renderClose Close = "close"
