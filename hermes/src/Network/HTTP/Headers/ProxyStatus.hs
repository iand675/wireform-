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
generically in wire order so extensions round-trip. Use
'proxyInfoParameters' for a typed view of the registered parameters.

Spec: <https://www.rfc-editor.org/rfc/rfc9209#section-2>

See also: "Network.HTTP.Headers.Via", "Network.HTTP.Headers.Forwarded",
"Network.HTTP.Headers.XForwardedFor", "Network.HTTP.Headers.CacheStatus".
-}
module Network.HTTP.Headers.ProxyStatus (
  ProxyStatus (..),
  ProxyInfo (..),
  ProxyStatusParameters (..),
  proxyInfoParameters,
  proxyStatusParser,
  renderProxyStatus,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
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


-- | The parameters defined in RFC 9209 section 2.1.
data ProxyStatusParameters = ProxyStatusParameters
  { proxyError :: !(Maybe RFC8941Token)
  , proxyNextHop :: !(Maybe (Either RFC8941Token RFC8941String))
  , proxyNextProtocol :: !(Maybe (Either RFC8941Token ByteString))
  , proxyReceivedStatus :: !(Maybe Int)
  , proxyDetails :: !(Maybe RFC8941String)
  }
  deriving stock (Eq, Show)


{- | Validate and project the registered parameters. Extension parameters
remain available through 'proxyParams'. The ALPN projection uses the
Token form whenever the protocol identifier fits its ASCII grammar.
-}
proxyInfoParameters :: ProxyInfo -> Either String ProxyStatusParameters
proxyInfoParameters info =
  ProxyStatusParameters
    <$> get "error" asToken
    <*> get "next-hop" asIdentifier
    <*> get "next-protocol" asProtocol
    <*> get "received-status" asStatus
    <*> get "details" asString
  where
    parameters = orderedFieldEntries fst (proxyParams info)
    get :: ShortText -> (ItemValue -> Maybe a) -> Either String (Maybe a)
    get key convert = case lookup key parameters of
      Nothing -> Right Nothing
      Just (Just value) | Just projected <- convert value -> Right (Just projected)
      Just _ -> Left ("Invalid Proxy-Status parameter " <> show key)

    asToken (Token value) = Just value
    asToken _ = Nothing
    asIdentifier (Token value) = Just (Left value)
    asIdentifier (String value) = Just (Right value)
    asIdentifier _ = Nothing
    asProtocol (Token value)
      | validProtocolLength (SBS.length (ST.toShortByteString (unsafeToRFC8941Token value))) = Just (Left value)
    asProtocol (Binary value)
      | validProtocolLength (B.length value) = Just (maybe (Right value) Left (alpnToken value))
    asProtocol _ = Nothing
    asStatus (Integer value) | value >= 100 && value <= 599 = Just value
    asStatus _ = Nothing
    asString (String value) = Just value
    asString _ = Nothing


validProtocolLength :: Int -> Bool
validProtocolLength n = n >= 1 && n <= 255


alpnToken :: ByteString -> Maybe RFC8941Token
alpnToken bytes = case runParser (rfc8941Token <* eof) bytes of
  OK token _ -> Just token
  _ -> Nothing


-- | The @Proxy-Status@ header value: the list of intermediary members.
newtype ProxyStatus = ProxyStatus
  { proxyStatusEntries :: [ProxyInfo]
  }
  deriving stock (Eq, Show)


instance KnownHeader ProxyStatus where
  type ParseFailure ProxyStatus = String
  type Cardinality ProxyStatus = 'ZeroOrMore
  type Direction ProxyStatus = 'Response


  parseFromHeaders _ = parseStructuredFieldLines "Proxy-Status" proxyStatusParser


  renderToHeaders _ (ProxyStatus []) = []
  renderToHeaders _ ps = [M.toStrictByteString (renderProxyStatus ps)]


  omitHeaderValue = null . proxyStatusEntries
  {-# INLINE omitHeaderValue #-}


  headerName _ = hProxyStatus


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse a @Proxy-Status@ value: a comma-separated list of members.
proxyStatusParser :: ParserT st String ProxyStatus
proxyStatusParser = ProxyStatus <$> structuredFieldList proxyInfoParser


-- | Parse one member: a Token\/String identifier plus its parameters.
proxyInfoParser :: ParserT st String ProxyInfo
proxyInfoParser = do
  pid <- either Token String <$> structuredFieldTokenOrString
  ProxyInfo pid . structuredFieldParameterPairs <$> structuredFieldParameters


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderProxyStatus :: ProxyStatus -> M.Builder
renderProxyStatus (ProxyStatus es) =
  M.intersperse ", " (map renderProxyInfo es)


renderProxyInfo :: ProxyInfo -> M.Builder
renderProxyInfo (ProxyInfo pid params) =
  R.rfc8941ItemValue pid <> foldMap renderParam (orderedFieldEntries fst params)
  where
    renderParam (k, mv) =
      R.renderStructuredFieldParameter k (canonicalValue k mv)
    canonicalValue "next-protocol" (Just (Binary bytes))
      | validProtocolLength (B.length bytes)
      , Just token <- alpnToken bytes = Token token
    canonicalValue _ value = maybe (Boolean True) id value
