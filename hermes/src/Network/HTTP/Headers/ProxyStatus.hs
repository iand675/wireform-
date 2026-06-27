{-# LANGUAGE TemplateHaskell #-}

{- |
@Proxy-Status@ — response header by which an intermediary (forward or
reverse proxy, CDN, load balancer) reports how it handled the response:
which next hop and protocol it used, the status it received, and any
error it encountered. It lets clients and operators attribute failures
to a point inside the proxy chain rather than to the origin server.

== Grammar (RFC 9209 §2)

The field value is an RFC 8941 List; each member is an Item whose
bare value is the proxy identifier (a Token or String) followed by
RFC 8941 Parameters:

@
Proxy-Status = sf-list
member       = ( sf-token \/ sf-string ) parameters
@

The first member represents the intermediary closest to the origin
server and the last the one closest to the user agent.  Registered
parameters include @error@, @next-hop@, @next-protocol@,
@received-status@, and @details@; all parameters are surfaced
generically (as RFC 8941 'ItemValue's) and in document order so the
field round-trips and callers can interpret the parameters they
recognise.

Spec: <https://www.rfc-editor.org/rfc/rfc9209#section-2>

See also: "Network.HTTP.Headers.Via", "Network.HTTP.Headers.Forwarded",
"Network.HTTP.Headers.XForwardedFor", "Network.HTTP.Headers.CacheStatus".
-}
module Network.HTTP.Headers.ProxyStatus (
  ProxyStatus (..),
  ProxyInfo (..),
  proxyStatusParser,
  renderProxyStatus,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyStatus)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A single @Proxy-Status@ member: an intermediary identifier (a
Token or String) and its parameters in document order.  A parameter
value of 'Nothing' is the bare-key (boolean true) form.
-}
data ProxyInfo = ProxyInfo
  { proxyId :: !ItemValue
  , proxyParams :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


-- | The @Proxy-Status@ header value: the list of intermediary members.
newtype ProxyStatus = ProxyStatus
  { proxyStatusEntries :: [ProxyInfo]
  }
  deriving stock (Eq, Show)


instance KnownHeader ProxyStatus where
  type ParseFailure ProxyStatus = String
  type Cardinality ProxyStatus = 'ZeroOrMore
  type Direction ProxyStatus = 'Response


  parseFromHeaders _ headers = do
    entryss <- traverse parseOne (NE.toList headers)
    pure (ProxyStatus (concat entryss))
    where
      parseOne hdr = case runParser proxyStatusParser hdr of
        OK (ProxyStatus es) leftover
          | B.null (dropOws leftover) -> Right es
          | otherwise ->
              Left ("Unconsumed input after parsing Proxy-Status: " <> show leftover)
        Fail -> Left "Failed to parse Proxy-Status header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ ps = [M.toStrictByteString (renderProxyStatus ps)]


  headerName _ = hProxyStatus


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse a @Proxy-Status@ value: a comma-separated list of members.
proxyStatusParser :: ParserT st String ProxyStatus
proxyStatusParser = do
  ows
  es <- proxyInfoParser `sepBy` (ows *> $(char ',') *> ows)
  ows
  pure (ProxyStatus es)


-- | Parse one member: a Token\/String identifier plus its parameters.
proxyInfoParser :: ParserT st String ProxyInfo
proxyInfoParser = do
  pid <- (Token <$> rfc8941Token) <|> (String <$> rfc8941String)
  ProxyInfo pid <$> rfc8941Parameters


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderProxyStatus :: ProxyStatus -> M.Builder
renderProxyStatus (ProxyStatus es) =
  M.intersperse ", " (map renderProxyInfo es)


renderProxyInfo :: ProxyInfo -> M.Builder
renderProxyInfo (ProxyInfo pid params) =
  R.rfc8941ItemValue pid <> mconcat (map renderParam params)
  where
    renderParam (k, mv) =
      R.rfc8941Parameter R.IncludeIfEmpty R.rfc8941ItemValue k mv
