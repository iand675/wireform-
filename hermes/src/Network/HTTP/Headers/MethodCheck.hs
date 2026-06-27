{- |
@Method-Check@ — obsolete request header from an early draft of cross-site
access control (the precursor to CORS). It was used during preflight to probe
methods; the draft never shipped, so the value is preserved verbatim as an
opaque string rather than committing to a dead grammar.

Spec: abandoned early draft of cross-site access control, superseded by CORS — <https://www.w3.org/TR/cors/ Cross-Origin Resource Sharing>.

See also: "Network.HTTP.Headers.MethodCheckExpires", "Network.HTTP.Headers.AccessControlRequestMethod", "Network.HTTP.Headers.AccessControlAllowMethods".
-}
module Network.HTTP.Headers.MethodCheck (
  MethodCheck (..),
  methodCheckParser,
  renderMethodCheck,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMethodCheck)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Method-Check@ value.
newtype MethodCheck = MethodCheck {methodCheckValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader MethodCheck where
  type ParseFailure MethodCheck = String
  type Cardinality MethodCheck = 'ZeroOrOne
  type Direction MethodCheck = 'Request


  parseFromHeaders _ headers = case runParser methodCheckParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Method-Check header: " <> show rest
    Fail -> Left "Failed to parse Method-Check header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderMethodCheck


  headerName _ = hMethodCheck


methodCheckParser :: ParserT st String MethodCheck
methodCheckParser = MethodCheck <$> takeRestShortText


renderMethodCheck :: MethodCheck -> M.Builder
renderMethodCheck = shortText . methodCheckValue
