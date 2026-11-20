{- |
The @close@ connection option from RFC 9112 §9.6. The typed header instance
reads the option from @Connection@ and writes @Connection: close@. Use the
full 'Network.HTTP.Headers.Connection.Connection' type to retain other
options when editing the field. RFC 9112 reserves the field name @Close@.

Spec: <https://www.rfc-editor.org/rfc/rfc9112#section-9.6>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.KeepAlive".
-}
module Network.HTTP.Headers.Close (
  Close (..),
  closeParser,
  renderClose,
) where

import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import qualified Network.HTTP.Headers.Connection as Connection
import Network.HTTP.Headers.HeaderFieldName (hConnection)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A typed view of the @close@ connection option.
data Close = Close
  deriving stock (Eq, Show)


instance KnownHeader Close where
  type ParseFailure Close = String
  type Cardinality Close = 'ZeroOrOne
  type Direction Close = 'RequestAndResponse


  parseFromHeaders _ ("close" :| []) = Right Close
  parseFromHeaders settings fields = do
    options <- parseFromHeaders @Connection.Connection settings fields
    if Connection.connectionHasClose options
      then Right Close
      else Left "Connection has no close option"


  renderToHeaders _ Close = "close"


  headerName _ = hConnection


closeParser :: ParserT st String Close
closeParser = do
  tok <- rfc9110TokenBS
  if tok == "close" || CI.foldCase tok == "close"
    then pure Close
    else failed


renderClose :: Close -> M.Builder
renderClose Close = "close"
