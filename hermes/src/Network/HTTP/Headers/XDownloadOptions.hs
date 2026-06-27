{-# LANGUAGE TemplateHaskell #-}

{- |
@X-Download-Options@ — a /de-facto/ (non-IANA-registered) response header
introduced by Internet Explorer 8. Its only defined value is the literal
@noopen@, which instructs the browser not to offer an \"Open\" option for a
downloaded file (forcing the user to save it first), mitigating attacks that
rely on a file executing in the site's security context.

== Grammar (de-facto)

@
X-Download-Options = "noopen"
@

This is a /de-facto/ header with no governing RFC; it was introduced by Internet
Explorer 8 — see the original vendor documentation,
<https://learn.microsoft.com/en-us/archive/blogs/ie/ie8-security-part-v-comprehensive-protection>.

See also: "Network.HTTP.Headers.ContentDisposition", "Network.HTTP.Headers.XContentTypeOptions", "Network.HTTP.Headers.XFrameOptions".
-}
module Network.HTTP.Headers.XDownloadOptions (
  XDownloadOptions (..),
  xDownloadOptionsParser,
  renderXDownloadOptions,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXDownloadOptions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @X-Download-Options@ value. The only defined value is @noopen@.
data XDownloadOptions
  = -- | @noopen@ — disallow opening the download directly.
    NoOpen
  deriving stock (Eq, Show)


instance KnownHeader XDownloadOptions where
  type ParseFailure XDownloadOptions = String
  type Cardinality XDownloadOptions = 'ZeroOrOne
  type Direction XDownloadOptions = 'Response


  parseFromHeaders _ headers = case runParser xDownloadOptionsParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Download-Options header: " <> show rest
    Fail -> Left "Failed to parse X-Download-Options header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXDownloadOptions


  headerName _ = hXDownloadOptions


xDownloadOptionsParser :: ParserT st String XDownloadOptions
xDownloadOptionsParser = NoOpen <$ $(string "noopen")


renderXDownloadOptions :: XDownloadOptions -> M.Builder
renderXDownloadOptions NoOpen = "noopen"
