{- |
The @Referer-Root@ request header is an obsolete field from an early W3C draft
of cross-site access control (a precursor to CORS); it carried the origin root
of the referring document so a server could make a cross-site allow/deny
decision. That mechanism never shipped and was superseded by CORS, so the
value is preserved verbatim as an opaque string.

Spec (obsolete; superseded by CORS): <https://www.w3.org/TR/cors/>

See also: "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.Origin", "Network.HTTP.Headers.AccessControlAllowOrigin".
-}
module Network.HTTP.Headers.RefererRoot (
  RefererRoot (..),
  refererRootParser,
  renderRefererRoot,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRefererRoot)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Referer-Root@ value (the referring origin).
newtype RefererRoot = RefererRoot {refererRootValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader RefererRoot where
  type ParseFailure RefererRoot = String
  type Cardinality RefererRoot = 'ZeroOrOne
  type Direction RefererRoot = 'Request


  parseFromHeaders _ headers = case runParser refererRootParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Referer-Root header: " <> show rest
    Fail -> Left "Failed to parse Referer-Root header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderRefererRoot


  headerName _ = hRefererRoot


refererRootParser :: ParserT st String RefererRoot
refererRootParser = RefererRoot <$> takeRestShortText


renderRefererRoot :: RefererRoot -> M.Builder
renderRefererRoot = shortText . refererRootValue
