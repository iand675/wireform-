{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@Clear-Site-Data@ — a response header (W3C) by which an origin
instructs the user agent to clear browsing data (cookies, storage,
cache, …) associated with the requesting origin.

== Grammar

@
Clear-Site-Data = 1#( quoted-string )
@

Each member is a quoted directive: @"cache"@, @"cookies"@,
@"storage"@, @"executionContexts"@, @"clientHints"@, or the wildcard
@"*"@ (every data type). Unrecognised directives retain their quoted
body, including any escapes or opaque bytes.

Spec: <https://w3c.github.io/webappsec-clear-site-data/#header>

See also: "Network.HTTP.Headers.SetCookie", "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.StrictTransportSecurity".
-}
module Network.HTTP.Headers.ClearSiteData (
  ClearSiteData (..),
  SiteData (..),
  clearSiteDataParser,
  renderClearSiteData,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Foreign.Ptr (plusPtr)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hClearSiteData)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.Encode.Direct (dBytes, dWord8)


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
  | -- | @"clientHints"@
    SiteDataClientHints
  | -- | @"*"@ — every data type.
    SiteDataWildcard
  | -- | Any other directive with a UTF-8 body and canonical quoting.
    SiteDataOther !ST.ShortText
  | -- | An extension's validated wire body, with escapes intact.
    SiteDataOtherWireBytes !SBS.ShortByteString
  deriving stock (Eq, Show)


-- | Non-empty list of @Clear-Site-Data@ directives, in document order.
newtype ClearSiteData = ClearSiteData {clearSiteDataDirectives :: NonEmpty SiteData}
  deriving stock (Eq, Show)


instance KnownHeader ClearSiteData where
  type ParseFailure ClearSiteData = String
  type Cardinality ClearSiteData = 'ZeroOrOne
  type Direction ClearSiteData = 'Response


  parseFromHeaders _ fields =
    ClearSiteData <$> (parseListFieldLines "Clear-Site-Data" siteDataList fields >>= requireNonEmptyList "Clear-Site-Data")


  renderToHeaders _ = M.toStrictByteString . renderClearSiteData


  renderToHeadersChecked _ = R.encodeCommaSeparatedChecked memberBound writeMember . NE.toList . clearSiteDataDirectives
    where
      memberBound (SiteDataOther value)
        | isHttpQuotedText value, extensionBody (ST.toShortByteString value) =
            let size = SBS.length (ST.toShortByteString value)
            in add 2 (add size size)
        | otherwise = -1
      memberBound (SiteDataOtherWireBytes body)
        | isHttpQuotedWireBody body, extensionBody body = add 2 (SBS.length body)
        | otherwise = -1
      memberBound directive = B.length (registeredSiteDataBytes directive)
      writeMember ptr offset (SiteDataOther value) = R.writeHttpQuotedBytes ptr offset (ST.toShortByteString value)
      writeMember ptr offset (SiteDataOtherWireBytes body) = do
        start <- dWord8 ptr offset 34
        let size = SBS.length body
        SBSI.copyToPtr body 0 (ptr `plusPtr` start) size
        dWord8 ptr (start + size) 34
      writeMember ptr offset directive = dBytes ptr offset (registeredSiteDataBytes directive)
      add = BSI.checkedAdd "Clear-Site-Data"


  headerName _ = hClearSiteData


clearSiteDataParser :: ParserT st String ClearSiteData
clearSiteDataParser = do
  directives <- siteDataList
  either err (pure . ClearSiteData) (requireNonEmptyList "Clear-Site-Data" directives)


siteDataList :: ParserT st String [SiteData]
siteDataList = listWithEmptySlots $(char ',') siteDataParser


siteDataParser :: ParserT st String SiteData
siteDataParser =
  $( switch
       [|
         case _ of
           "\"cache\"" -> pure SiteDataCache
           "\"cookies\"" -> pure SiteDataCookies
           "\"storage\"" -> pure SiteDataStorage
           "\"executionContexts\"" -> pure SiteDataExecutionContexts
           "\"clientHints\"" -> pure SiteDataClientHints
           "\"*\"" -> pure SiteDataWildcard
         |]
   )
    <|> extension
  where
    extension = do
      body <- httpQuotedWireBytes
      let decoded
            | not (B.elem 92 body) = ST.fromByteString body
            | canonicalPairs body = ST.fromByteString (unquotePairs body)
            | otherwise = Nothing
      pure (maybe (SiteDataOtherWireBytes (SBS.toShort body)) SiteDataOther decoded)

    -- Unescaping any other pair would change the wire spelling on render.
    canonicalPairs body = case B.uncons (B.drop 1 (B.dropWhile (/= 92) body)) of
      Nothing -> True
      Just (escaped, rest) -> (escaped == 34 || escaped == 92) && canonicalPairs rest


renderClearSiteData :: ClearSiteData -> M.Builder
renderClearSiteData (ClearSiteData ds) =
  R.sepByCommas1 (fmap renderSiteData ds)
  where
    renderSiteData = \case
      SiteDataOther t -> M.byteString (R.encodeHttpQuotedBytes (ST.toShortByteString t))
      SiteDataOtherWireBytes body -> M.char7 '"' <> M.shortByteString body <> M.char7 '"'
      directive -> M.byteString (registeredSiteDataBytes directive)


extensionBody :: SBS.ShortByteString -> Bool
extensionBody body = case SBS.length body of
  1 -> body /= "*"
  5 -> body /= "cache"
  7 -> body /= "cookies" && body /= "storage"
  11 -> body /= "clientHints"
  17 -> body /= "executionContexts"
  _ -> True


registeredSiteDataBytes :: SiteData -> B.ByteString
registeredSiteDataBytes = \case
  SiteDataCache -> "\"cache\""
  SiteDataCookies -> "\"cookies\""
  SiteDataStorage -> "\"storage\""
  SiteDataExecutionContexts -> "\"executionContexts\""
  SiteDataClientHints -> "\"clientHints\""
  SiteDataWildcard -> "\"*\""
  SiteDataOther _ -> ""
  SiteDataOtherWireBytes _ -> ""
