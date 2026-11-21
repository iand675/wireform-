{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

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
'proxyInfoParameters' to read registered parameters and
'proxyInfoFromParameters' to author a validated member.
Use 'lookupProxyStatusWithTrailers' to read the effective chain once the
trailer section is available.

Spec: <https://www.rfc-editor.org/rfc/rfc9209#section-2>

See also: "Network.HTTP.Headers.Via", "Network.HTTP.Headers.Forwarded",
"Network.HTTP.Headers.XForwardedFor", "Network.HTTP.Headers.CacheStatus".
-}
module Network.HTTP.Headers.ProxyStatus (
  ProxyStatus (..),
  ProxyInfo (..),
  ProxyStatusParameters (..),
  proxyInfoParameters,
  proxyInfoFromParameters,
  promoteProxyStatusTrailers,
  lookupProxyStatusWithTrailers,
  proxyStatusParser,
  renderProxyStatus,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Hashtable
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
    parameters = orderedProxyParameters (proxyParams info)
    get :: ShortText -> (ItemValue -> Maybe a) -> Either String (Maybe a)
    get key convert = case lookup key parameters of
      Nothing -> Right Nothing
      Just (Just value) | Just projected <- convert value -> Right (Just projected)
      Just _ -> invalidProxyParameter key

    asToken (Token value) = Just value
    asToken _ = Nothing
    asIdentifier (Token value) = Just (Left value)
    asIdentifier (String value) = Just (Right value)
    asIdentifier _ = Nothing
    asProtocol (Token value)
      | validProtocolTokenLength value = Just (Left value)
    asProtocol (Binary value)
      | validProtocolLength (B.length value) = Just (maybe (Right value) Left (alpnToken value))
    asProtocol _ = Nothing
    asStatus (Integer value) | validReceivedStatus value = Just value
    asStatus _ = Nothing
    asString (String value) = Just value
    asString _ = Nothing


{- | Author a member with typed registered parameters and optional extensions.
Extension keys keep their first position and last value. A registered name
in the extension list is rejected, even if that typed parameter is absent.
-}
proxyInfoFromParameters
  :: Either RFC8941Token RFC8941String
  -> ProxyStatusParameters
  -> [(ShortText, Maybe ItemValue)]
  -> Either String ProxyInfo
proxyInfoFromParameters identifier (ProxyStatusParameters err hop protocol status details) extensions
  | not (validProxyIdentifier item) = Left "Invalid Proxy-Status intermediary identifier"
  | not (maybe True isStructuredFieldToken err) = invalidProxyParameter "error"
  | not (maybe True (validProxyIdentifier . either Token String) hop) = invalidProxyParameter "next-hop"
  | not (maybe True validProtocol protocol) = invalidProxyParameter "next-protocol"
  | not (maybe True validReceivedStatus status) = invalidProxyParameter "received-status"
  | not (maybe True isStructuredFieldString details) = invalidProxyParameter "details"
  | Just (name, _) <- find (not . validExtension) normalized = invalidProxyParameter name
  | otherwise = Right (ProxyInfo item params)
  where
    item = either Token String identifier
    normalized = orderedProxyParameters extensions
    params =
      parameter "error" Token err $
        parameter "next-hop" (either Token String) hop $
          parameter "next-protocol" (either Token binaryProtocolItem) protocol $
            parameter "received-status" Integer status $
              parameter "details" String details normalized

    parameter key encode = maybe id (\value rest -> (key, Just (encode value)) : rest)

    validProtocol = either (\token -> validProtocolTokenLength token && isStructuredFieldToken token) (validProtocolLength . B.length)
    validExtension entry@(key, _) = case key of
      "error" -> False
      "next-hop" -> False
      "next-protocol" -> False
      "received-status" -> False
      "details" -> False
      _ -> validProxyParameter entry


invalidProxyParameter :: ShortText -> Either String a
invalidProxyParameter key = Left ("Invalid Proxy-Status parameter " <> show key)


-- Most fully populated members have five registered keys and a few
-- extensions. Reuse their list if the keys are already distinct.
orderedProxyParameters :: [(ShortText, Maybe ItemValue)] -> [(ShortText, Maybe ItemValue)]
orderedProxyParameters entries@(_ : _ : _ : _ : _ : rest)
  | fewerThanFour rest, distinct entries = entries
  where
    fewerThanFour (_ : _ : _ : _ : _) = False
    fewerThanFour _ = True
    distinct [] = True
    distinct ((key, _) : remaining) = missing key remaining && distinct remaining
    missing _ [] = True
    missing key ((other, _) : remaining) = key /= other && missing key remaining
orderedProxyParameters entries = orderedFieldEntries fst entries


validReceivedStatus :: Int -> Bool
validReceivedStatus value = value >= 100 && value <= 599
{-# INLINE validReceivedStatus #-}


validProtocolTokenLength :: RFC8941Token -> Bool
validProtocolTokenLength (RFC8941Token value) = validProtocolLength (SBS.length (ST.toShortByteString value))
{-# INLINE validProtocolTokenLength #-}


validProtocolLength :: Int -> Bool
validProtocolLength n = n >= 1 && n <= 255


binaryProtocolItem :: ByteString -> ItemValue
binaryProtocolItem bytes = maybe (Binary bytes) Token (alpnToken bytes)
{-# INLINE binaryProtocolItem #-}


alpnToken :: ByteString -> Maybe RFC8941Token
alpnToken bytes = case runParser (rfc8941Token <* eof) bytes of
  OK token _ -> Just token
  _ -> Nothing


-- | The @Proxy-Status@ header value: the list of intermediary members.
newtype ProxyStatus = ProxyStatus
  { proxyStatusEntries :: [ProxyInfo]
  }
  deriving stock (Eq, Show)


{- | Promote trailer members as specified by RFC 9209 §2. Match case-sensitive
identifier text, including between a String and a Token. The last trailer
for an identifier replaces the entire first matching header member. A
trailer without a matching header member leaves the chain as it was.
-}
promoteProxyStatusTrailers :: ProxyStatus -> ProxyStatus -> ProxyStatus
promoteProxyStatusTrailers header@(ProxyStatus members) (ProxyStatus trailers) = case (members, trailers) of
  ([], _) -> header
  (_, []) -> header
  (_, [trailer]) -> applyOne header trailer
  (_, [first, second]) -> applyOne (applyOne header first) second
  _ -> ProxyStatus $ runST $ do
    pending <- Hashtable.newSized (length trailers)
    forM_ trailers $ \trailer -> case proxyIdentifierText trailer of
      Nothing -> pure ()
      Just identifier -> Hashtable.insert pending identifier trailer
    applyUpdates pending members
  where
    applyOne value@(ProxyStatus entries) trailer = case proxyIdentifierText trailer of
      Just identifier -> case replaceFirstProxy identifier trailer entries of
        (# | updated #) -> ProxyStatus updated
        (# (# #) | #) -> value
      Nothing -> value

    applyUpdates :: Hashtable.Dictionary s Hashtable.Boxed ShortText Hashtable.Boxed ProxyInfo -> [ProxyInfo] -> ST s [ProxyInfo]
    applyUpdates _ [] = pure []
    applyUpdates pending remaining@(member : rest) = do
      done <- Hashtable.null pending
      if done
        then pure remaining
        else do
          entry <- case proxyIdentifierText member of
            Nothing -> pure member
            Just identifier -> Hashtable.mutate pending (\current -> (Nothing, fromMaybe member current)) identifier
          (entry :) <$> applyUpdates pending rest


replaceFirstProxy :: ShortText -> ProxyInfo -> [ProxyInfo] -> (# (# #) | [ProxyInfo] #)
replaceFirstProxy identifier replacement = go
  where
    go [] = (# (# #) | #)
    go (member : rest)
      | Just name <- proxyIdentifierText member, identifier == name = (# | replacement : rest #)
      | otherwise = case go rest of
          (# | updated #) -> (# | member : updated #)
          (# (# #) | #) -> (# (# #) | #)


proxyIdentifierText :: ProxyInfo -> Maybe ShortText
proxyIdentifierText member = case proxyId member of
  Token (RFC8941Token text) -> Just text
  String (RFC8941String text) -> Just text
  _ -> Nothing
{-# INLINE proxyIdentifierText #-}


{- | Read the effective chain without modifying the two field sections.
A malformed field in either section is reported as a parse failure.
-}
lookupProxyStatusWithTrailers :: HeaderMap -> HeaderMap -> Either String (Maybe ProxyStatus)
lookupProxyStatusWithTrailers headers trailers = do
  initial <- lookupHeader @ProxyStatus headers
  updates <- lookupHeader @ProxyStatus trailers
  pure (maybe initial (\value -> fmap (`promoteProxyStatusTrailers` value) initial) updates)


instance KnownHeader ProxyStatus where
  type ParseFailure ProxyStatus = String
  type Cardinality ProxyStatus = 'ZeroOrMore
  type Direction ProxyStatus = 'Response


  parseFromHeaders _ = parseStructuredFieldLines "Proxy-Status" proxyStatusParser


  renderToHeaders _ value = case proxyStatusBuilder value of
    (# (# #) | #) -> []
    (# | builder #) -> [M.toStrictByteString builder]


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

-- | Render the field value. An invalid identifier or parameter produces no bytes.
renderProxyStatus :: ProxyStatus -> M.Builder
renderProxyStatus value = case proxyStatusBuilder value of
  (# (# #) | #) -> mempty
  (# | builder #) -> builder


type RenderResult = (# (# #) | M.Builder #)


proxyStatusBuilder :: ProxyStatus -> RenderResult
proxyStatusBuilder (ProxyStatus []) = (# (# #) | #)
proxyStatusBuilder (ProxyStatus (first : rest)) = renderMembers first rest
  where
    renderMembers entry remainder = case renderProxyInfo entry of
      (# (# #) | #) -> (# (# #) | #)
      (# | member #) -> case remainder of
        [] -> (# | member #)
        next : later -> case renderMembers next later of
          (# (# #) | #) -> (# (# #) | #)
          (# | suffix #) -> (# | member <> ", " <> suffix #)


renderProxyInfo :: ProxyInfo -> RenderResult
renderProxyInfo (ProxyInfo identifier params)
  | validProxyIdentifier identifier
  , all validProxyParameter normalized =
      (# | R.rfc8941ItemValue identifier <> foldMap renderParameter normalized #)
  | otherwise = (# (# #) | #)
  where
    normalized = orderedProxyParameters params

    renderParameter (key, value) =
      R.renderStructuredFieldParameter key (canonicalValue key value)

    canonicalValue "next-protocol" (Just (Binary bytes))
      | validProtocolLength (B.length bytes)
      , Just token <- alpnToken bytes =
          Token token
    canonicalValue _ value = maybe (Boolean True) id value


validProxyIdentifier :: ItemValue -> Bool
validProxyIdentifier (Token value) = isStructuredFieldToken value
validProxyIdentifier (String value) = isStructuredFieldString value
validProxyIdentifier _ = False
{-# INLINE validProxyIdentifier #-}


validProxyParameter :: (ShortText, Maybe ItemValue) -> Bool
validProxyParameter (key, value) = isStructuredFieldKey key && maybe True isStructuredFieldBareItem value
{-# INLINE validProxyParameter #-}
