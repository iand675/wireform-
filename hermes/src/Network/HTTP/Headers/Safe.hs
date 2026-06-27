{- |
@Safe@ — obsolete HTTP response header (RFC 2310) indicating whether the
corresponding request may be automatically repeated. The value is a simple,
case-insensitive @yes@/@no@ enumeration, so it is parsed structurally.

@
Safe        = "Safe" ":" safe-nature
safe-nature = "yes" | "no"
@

Spec: <https://datatracker.ietf.org/doc/html/rfc2310 RFC 2310: The Safe Response Header Field>.

See also: "Network.HTTP.Headers.PublicHeader", "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.RetryAfter".
-}
module Network.HTTP.Headers.Safe (
  Safe (..),
  safeParser,
  renderSafe,
) where

import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSafe)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Whether repeating the corresponding request is considered safe.
data Safe
  = SafeYes
  | SafeNo
  deriving stock (Eq, Show)


instance KnownHeader Safe where
  type ParseFailure Safe = String
  type Cardinality Safe = 'ZeroOrOne
  type Direction Safe = 'Response


  parseFromHeaders _ headers = case runParser safeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Safe header: " <> show rest
    Fail -> Left "Failed to parse Safe header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSafe


  headerName _ = hSafe


safeParser :: ParserT st String Safe
safeParser = do
  ows
  tok <- rfc9110Token
  ows
  case toLower <$> ST.toString tok of
    "yes" -> pure SafeYes
    "no" -> pure SafeNo
    other -> err $ "Invalid Safe value: " <> other


renderSafe :: Safe -> M.Builder
renderSafe = \case
  SafeYes -> "yes"
  SafeNo -> "no"
