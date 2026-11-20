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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
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


  parseFromHeaders _ (wire :| [])
    | BS.length wire == 6
    , BSU.unsafeIndex wire 0 == 0x6e
    , BSU.unsafeIndex wire 1 == 0x6f
    , BSU.unsafeIndex wire 2 == 0x6f
    , BSU.unsafeIndex wire 3 == 0x70
    , BSU.unsafeIndex wire 4 == 0x65
    , BSU.unsafeIndex wire 5 == 0x6e =
        Right NoOpen
  parseFromHeaders _ fields = parseSingleFieldLine "X-Download-Options" xDownloadOptionsParser fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ NoOpen = "noopen"


  headerName _ = hXDownloadOptions


xDownloadOptionsParser :: ParserT st String XDownloadOptions
xDownloadOptionsParser = NoOpen <$ $(string "noopen")


renderXDownloadOptions :: XDownloadOptions -> M.Builder
renderXDownloadOptions NoOpen = "noopen"
