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

The codec accepts ASCII case variants of the two keywords.
<https://www.chromium.org/developers/design-documents/dns-prefetching/ Chromium DNS prefetching>

See also: "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.Link", "Network.HTTP.Headers.XContentTypeOptions".
-}
module Network.HTTP.Headers.XDNSPrefetchControl (
  XDNSPrefetchControl (..),
  xDNSPrefetchControlParser,
  renderXDNSPrefetchControl,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
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


  parseFromHeaders _ (wire :| [])
    | BS.length wire == 2
    , foldedByte wire 0 == 0x6f
    , foldedByte wire 1 == 0x6e =
        Right (XDNSPrefetchControl True)
    | BS.length wire == 3
    , foldedByte wire 0 == 0x6f
    , foldedByte wire 1 == 0x66
    , foldedByte wire 2 == 0x66 =
        Right (XDNSPrefetchControl False)
  parseFromHeaders _ fields = parseSingleFieldLine "X-DNS-Prefetch-Control" xDNSPrefetchControlParser fields


  renderToHeaders _ (XDNSPrefetchControl True) = "on"
  renderToHeaders _ (XDNSPrefetchControl False) = "off"


  headerName _ = hXDNSPrefetchControl


foldedByte :: BS.ByteString -> Int -> Word8
foldedByte wire offset = BSU.unsafeIndex wire offset .|. 0x20
{-# INLINE foldedByte #-}


xDNSPrefetchControlParser :: ParserT st String XDNSPrefetchControl
xDNSPrefetchControlParser =
  $( switchAsciiCI
       [|
         case _ of
           "on" -> pure (XDNSPrefetchControl True)
           "off" -> pure (XDNSPrefetchControl False)
         |]
   )


renderXDNSPrefetchControl :: XDNSPrefetchControl -> M.Builder
renderXDNSPrefetchControl (XDNSPrefetchControl True) = "on"
renderXDNSPrefetchControl (XDNSPrefetchControl False) = "off"
