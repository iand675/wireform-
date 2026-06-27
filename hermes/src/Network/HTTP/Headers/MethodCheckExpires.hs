{- |
@Method-Check-Expires@ — obsolete response header from an early draft of
cross-site access control (the precursor to CORS), used to bound the lifetime
of a @Method-Check@ result. The draft never shipped, so the value is preserved
verbatim as an opaque string rather than committing to a dead grammar.

Spec: abandoned early draft of cross-site access control, superseded by CORS — <https://www.w3.org/TR/cors/ Cross-Origin Resource Sharing>.

See also: "Network.HTTP.Headers.MethodCheck", "Network.HTTP.Headers.AccessControlMaxAge".
-}
module Network.HTTP.Headers.MethodCheckExpires (
  MethodCheckExpires (..),
  methodCheckExpiresParser,
  renderMethodCheckExpires,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMethodCheckExpires)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Method-Check-Expires@ value.
newtype MethodCheckExpires = MethodCheckExpires {methodCheckExpiresValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader MethodCheckExpires where
  type ParseFailure MethodCheckExpires = String
  type Cardinality MethodCheckExpires = 'ZeroOrOne
  type Direction MethodCheckExpires = 'Response


  parseFromHeaders _ headers = case runParser methodCheckExpiresParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Method-Check-Expires header: " <> show rest
    Fail -> Left "Failed to parse Method-Check-Expires header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderMethodCheckExpires


  headerName _ = hMethodCheckExpires


methodCheckExpiresParser :: ParserT st String MethodCheckExpires
methodCheckExpiresParser = MethodCheckExpires <$> takeRestShortText


renderMethodCheckExpires :: MethodCheckExpires -> M.Builder
renderMethodCheckExpires = shortText . methodCheckExpiresValue
