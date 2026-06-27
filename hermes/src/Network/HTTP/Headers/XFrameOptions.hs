{- |
@X-Frame-Options@ — a legacy response header controlling whether a
browser may render the page inside a @\<frame>@, @\<iframe>@,
@\<embed>@ or @\<object>@, used to defend against clickjacking.

== Grammar

@
X-Frame-Options = "DENY"
                / "SAMEORIGIN"
                / ( "ALLOW-FROM" RWS serialized-origin )
@

@DENY@ and @SAMEORIGIN@ are the only forms honoured by current
browsers; the obsolete @ALLOW-FROM <origin>@ directive (RFC 7034
§2.1) is still parsed and preserved verbatim for fidelity. The
keywords are matched case-insensitively.

Spec: <https://www.rfc-editor.org/rfc/rfc7034>

See also: "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.XContentTypeOptions", "Network.HTTP.Headers.CrossOriginOpenerPolicy", "Network.HTTP.Headers.XPermittedCrossDomainPolicies".
-}
module Network.HTTP.Headers.XFrameOptions (
  XFrameOptions (..),
  xFrameOptionsParser,
  renderXFrameOptions,
) where

import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXFrameOptions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @X-Frame-Options@ value.
data XFrameOptions
  = -- | @DENY@ — never permit framing.
    Deny
  | -- | @SAMEORIGIN@ — permit framing only by same-origin documents.
    SameOrigin
  | -- | Obsolete @ALLOW-FROM <serialized-origin>@; the origin is preserved verbatim.
    AllowFrom !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader XFrameOptions where
  type ParseFailure XFrameOptions = String
  type Cardinality XFrameOptions = 'ZeroOrOne
  type Direction XFrameOptions = 'Response


  parseFromHeaders _ headers = case runParser xFrameOptionsParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise ->
          Left ("Unconsumed input after parsing X-Frame-Options: " <> show leftover)
    Fail -> Left "Failed to parse X-Frame-Options header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderXFrameOptions


  headerName _ = hXFrameOptions


xFrameOptionsParser :: ParserT st String XFrameOptions
xFrameOptionsParser = do
  ows
  tok <- rfc9110Token
  case map toLower (ST.toString tok) of
    "deny" -> Deny <$ ows
    "sameorigin" -> SameOrigin <$ ows
    "allow-from" -> do
      rws
      AllowFrom <$> takeRestShortText
    _ -> failed


renderXFrameOptions :: XFrameOptions -> M.Builder
renderXFrameOptions = \case
  Deny -> "DENY"
  SameOrigin -> "SAMEORIGIN"
  AllowFrom origin -> "ALLOW-FROM " <> shortText origin
