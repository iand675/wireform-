{-# LANGUAGE TemplateHaskell #-}

{- |
@Clear-Site-Data@ — a response header (W3C) by which an origin
instructs the user agent to clear browsing data (cookies, storage,
cache, …) associated with the requesting origin.

== Grammar

@
Clear-Site-Data = 1#( quoted-string )
@

Each member is a quoted directive: @"cache"@, @"cookies"@,
@"storage"@, @"executionContexts"@, or the wildcard @"*"@ (every
data type). Unrecognised directives are preserved verbatim (without
the surrounding quotes).

Spec: <https://www.w3.org/TR/clear-site-data/>

See also: "Network.HTTP.Headers.SetCookie", "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.StrictTransportSecurity".
-}
module Network.HTTP.Headers.ClearSiteData (
  ClearSiteData (..),
  SiteData (..),
  clearSiteDataParser,
  renderClearSiteData,
) where

import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hClearSiteData)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A single @Clear-Site-Data@ directive (the unquoted value).
data SiteData
  = -- | @"cache"@
    SiteDataCache
  | -- | @"cookies"@
    SiteDataCookies
  | -- | @"storage"@
    SiteDataStorage
  | -- | @"executionContexts"@
    SiteDataExecutionContexts
  | -- | @"*"@ — every data type.
    SiteDataWildcard
  | -- | Any other directive, preserved verbatim (without quotes).
    SiteDataOther !ST.ShortText
  deriving stock (Eq, Show)


-- | Non-empty list of @Clear-Site-Data@ directives, in document order.
newtype ClearSiteData = ClearSiteData {clearSiteDataDirectives :: NonEmpty SiteData}
  deriving stock (Eq, Show)


instance KnownHeader ClearSiteData where
  type ParseFailure ClearSiteData = String
  type Cardinality ClearSiteData = 'ZeroOrOne
  type Direction ClearSiteData = 'Response


  parseFromHeaders _ headers = case runParser clearSiteDataParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise ->
          Left ("Unconsumed input after parsing Clear-Site-Data: " <> show leftover)
    Fail -> Left "Failed to parse Clear-Site-Data header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderClearSiteData


  headerName _ = hClearSiteData


clearSiteDataParser :: ParserT st String ClearSiteData
clearSiteDataParser = do
  ows
  first <- siteData
  rest <- many (ows *> $(char ',') *> ows *> siteData)
  ows
  pure (ClearSiteData (first :| rest))
  where
    siteData = do
      s <- quotedString
      pure $ case ST.toString s of
        "cache" -> SiteDataCache
        "cookies" -> SiteDataCookies
        "storage" -> SiteDataStorage
        "executionContexts" -> SiteDataExecutionContexts
        "*" -> SiteDataWildcard
        _ -> SiteDataOther s


renderClearSiteData :: ClearSiteData -> M.Builder
renderClearSiteData (ClearSiteData ds) =
  R.sepByCommas1 (fmap renderSiteData ds)
  where
    renderSiteData = \case
      SiteDataCache -> "\"cache\""
      SiteDataCookies -> "\"cookies\""
      SiteDataStorage -> "\"storage\""
      SiteDataExecutionContexts -> "\"executionContexts\""
      SiteDataWildcard -> "\"*\""
      SiteDataOther t -> R.rfc8941String (RFC8941String t)
