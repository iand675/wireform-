{-# LANGUAGE TemplateHaskell #-}

{- |
@X-DNS-Prefetch-Control@ — a /de-facto/ (non-IANA-registered) response header
that toggles the browser's speculative DNS prefetching of links, hostnames, and
other resources referenced by a document. Turning it @off@ avoids leaking DNS
lookups for links a user may never follow.

== Grammar (de-facto)

@
X-DNS-Prefetch-Control = "on" / "off"
@

This is a /de-facto/ header with no governing RFC; see MDN
<https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-DNS-Prefetch-Control>.

See also: "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.Link", "Network.HTTP.Headers.XContentTypeOptions".
-}
module Network.HTTP.Headers.XDNSPrefetchControl (
  XDNSPrefetchControl (..),
  xDNSPrefetchControlParser,
  renderXDNSPrefetchControl,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXDNSPrefetchControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @True@ when DNS prefetching is enabled (@on@), @False@ otherwise (@off@).
newtype XDNSPrefetchControl = XDNSPrefetchControl {xdnsPrefetchEnabled :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader XDNSPrefetchControl where
  type ParseFailure XDNSPrefetchControl = String
  type Cardinality XDNSPrefetchControl = 'ZeroOrOne
  type Direction XDNSPrefetchControl = 'Response


  parseFromHeaders _ headers = case runParser xDNSPrefetchControlParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-DNS-Prefetch-Control header: " <> show rest
    Fail -> Left "Failed to parse X-DNS-Prefetch-Control header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXDNSPrefetchControl


  headerName _ = hXDNSPrefetchControl


xDNSPrefetchControlParser :: ParserT st String XDNSPrefetchControl
xDNSPrefetchControlParser =
  $( switch
      [|
        case _ of
          "on" -> pure (XDNSPrefetchControl True)
          "off" -> pure (XDNSPrefetchControl False)
        |]
   )


renderXDNSPrefetchControl :: XDNSPrefetchControl -> M.Builder
renderXDNSPrefetchControl (XDNSPrefetchControl True) = "on"
renderXDNSPrefetchControl (XDNSPrefetchControl False) = "off"
