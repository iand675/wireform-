{- |
@X-Content-Type-Options@ — a de-facto response header whose single
meaningful value, @nosniff@, instructs the user agent to block
requests whose computed MIME type disagrees with the declared
@Content-Type@ (i.e. to disable content-type sniffing).

The only value defined by the Fetch standard is the case-insensitive
token @nosniff@; any other value is preserved verbatim so callers can
observe (and re-emit) what the origin actually sent.

Spec: <https://fetch.spec.whatwg.org/#x-content-type-options-header>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.XFrameOptions", "Network.HTTP.Headers.XDownloadOptions".
-}
module Network.HTTP.Headers.XContentTypeOptions (
  XContentTypeOptions (..),
  xContentTypeOptionsParser,
  renderXContentTypeOptions,
) where

import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXContentTypeOptions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @X-Content-Type-Options@ value.
data XContentTypeOptions
  = -- | The @nosniff@ directive — disables MIME sniffing.
    NoSniff
  | -- | Any other (non-standard) value, preserved verbatim.
    XContentTypeOptionsOther !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader XContentTypeOptions where
  type ParseFailure XContentTypeOptions = String
  type Cardinality XContentTypeOptions = 'ZeroOrOne
  type Direction XContentTypeOptions = 'Response


  parseFromHeaders _ headers = case runParser xContentTypeOptionsParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise ->
          Left ("Unconsumed input after parsing X-Content-Type-Options: " <> show leftover)
    Fail -> Left "Failed to parse X-Content-Type-Options header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderXContentTypeOptions


  headerName _ = hXContentTypeOptions


xContentTypeOptionsParser :: ParserT st String XContentTypeOptions
xContentTypeOptionsParser = do
  ows
  v <- rfc9110Token
  ows
  pure $ case map toLower (ST.toString v) of
    "nosniff" -> NoSniff
    _ -> XContentTypeOptionsOther v


renderXContentTypeOptions :: XContentTypeOptions -> M.Builder
renderXContentTypeOptions = \case
  NoSniff -> "nosniff"
  XContentTypeOptionsOther v -> shortText v
