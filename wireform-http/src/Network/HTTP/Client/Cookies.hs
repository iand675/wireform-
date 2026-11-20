{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Cookie jar and middleware.

Implements RFC 6265 scheme, host, path and expiry matching. The
request field orders cookies by descending path length, then by creation.
The jar stores @SameSite@ and @HttpOnly@ as metadata. Request matching
has no top-level navigation or non-HTTP context.

The jar is intentionally stored in an 'STM.TVar' for cheap
concurrent reads.
-}
module Network.HTTP.Client.Cookies (
  CookieJar,
  Cookie (..),
  SameSite (..),
  newCookieJar,
  newCookieJarWithPSL,
  insertCookie,
  insertCookieChecked,
  getCookies,
  clearCookies,
  pruneExpired,
  withCookies,

  -- * Validation and public-suffix hook
  CookieError (..),
  validateCookieName,
  validateCookieValue,
  PublicSuffixCheck,
  noPublicSuffix,

  -- * Limits
  CookieJarLimits (..),
  defaultCookieJarLimits,
  newCookieJarWith,
) where

import Control.Concurrent.STM
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text.Encoding qualified as TE
import Data.Text.Short qualified as ST
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word64)
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI
import Network.HTTP.Headers.Cookie qualified as Hermes
import Network.HTTP.Grammar.Parser (Result (..), cookieValueBytes, eof, runParser)
import Network.HTTP.Headers.SetCookie qualified as Hermes
import Network.HTTP.Types.Header qualified as H
import Network.IPAddress (ipv4Parser)
import Wireform.Builder qualified as WB


-- ---------------------------------------------------------------------------
-- Cookie data
-- ---------------------------------------------------------------------------

data SameSite = SameSiteStrict | SameSiteLax | SameSiteNone
  deriving stock (Eq, Show)


data Cookie = Cookie
  { cookieName :: !ByteString
  , cookieValue :: !ByteString
  , cookieDomain :: !ByteString
  {- ^ Effective domain. Defaulted from the request host when
  the response carried no @Domain=...@ attribute.
  -}
  , cookieDomainExplicit :: !Bool
  {- ^ 'True' for an accepted @Domain@ scope. Host-only cookies use
  'False', including an exact-host public suffix normalized at ingest.
  -}
  , cookiePath :: !ByteString
  , cookieExpires :: !(Maybe UTCTime)
  , cookieSecure :: !Bool
  , cookieHttpOnly :: !Bool
  , cookieSameSite :: !SameSite
  }
  deriving stock (Eq, Show)


-- Include the effective domain scope so both kinds of cookie can coexist.
data CookieKey = CookieKey !ByteString !ByteString !ByteString !Bool
  deriving stock (Eq, Ord)


{- | Predicate run on @Set-Cookie@'s @Domain=...@: returns 'True' for
a domain that's on the public suffix list (e.g. @co.uk@ or @github.io@).
An exact match to the setting host becomes host-only. Other requests to
set a cookie with that @Domain@ are rejected.
Defaults to 'noPublicSuffix' which never reports a hit; wire your
own (e.g. backed by @publicsuffix@ data) for browser-grade
behaviour.
-}
type PublicSuffixCheck = ByteString -> Bool


{- | The trivial 'PublicSuffixCheck' that never rejects.  Use this
when the application is talking to a known endpoint set and
doesn't need PSL enforcement.
-}
noPublicSuffix :: PublicSuffixCheck
noPublicSuffix = const False


{- | Per-cookie size limits applied at ingest. The defaults match
the browser-ecosystem convergence point; override for backends
that need to exceed them.
-}
data CookieJarLimits = CookieJarLimits
  { cjlMaxNameValueBytes :: !Int
  -- ^ Max combined byte size of @name=value@. Default 4096.
  , cjlMaxAttributeBytes :: !Int
  {- ^ Max byte size of a trimmed response attribute value. An oversized
  attribute is ignored. Default 1024.
  -}
  }
  deriving stock (Eq, Show)


defaultCookieJarLimits :: CookieJarLimits
defaultCookieJarLimits =
  CookieJarLimits
    { cjlMaxNameValueBytes = 4096
    , cjlMaxAttributeBytes = 1024
    }


data CookieJar = CookieJar
  { cjStore :: !(TVar CookieStore)
  , cjPSL :: !PublicSuffixCheck
  , cjLimits :: !CookieJarLimits
  }


data CookieStore = CookieStore
  { nextCreationOrder :: {-# UNPACK #-} !Word64
  , storedCookies :: !(Map CookieKey StoredCookie)
  }


data StoredCookie = StoredCookie
  { storedCookie :: !Cookie
  , creationOrder :: {-# UNPACK #-} !Word64
  , storedPairBytes :: ~ByteString
  }


emptyCookieStore :: CookieStore
emptyCookieStore = CookieStore 0 Map.empty


newCookieJar :: IO CookieJar
newCookieJar = newCookieJarWith noPublicSuffix defaultCookieJarLimits


-- | Allocate a 'CookieJar' with a custom public-suffix check.
newCookieJarWithPSL :: PublicSuffixCheck -> IO CookieJar
newCookieJarWithPSL psl = newCookieJarWith psl defaultCookieJarLimits


{- | Allocate a 'CookieJar' with both a custom PSL check and size
limits.
-}
newCookieJarWith :: PublicSuffixCheck -> CookieJarLimits -> IO CookieJar
newCookieJarWith psl limits = do
  s <- newTVarIO emptyCookieStore
  pure CookieJar {cjStore = s, cjPSL = psl, cjLimits = limits}


cookieKey :: Cookie -> CookieKey
cookieKey c = CookieKey (cookieDomain c) (cookiePath c) (cookieName c) (cookieDomainExplicit c)


insertCookie :: CookieJar -> Cookie -> IO ()
insertCookie jar c = do
  now <- getCurrentTime
  atomically $ modifyTVar' (cjStore jar) (rememberCookie now c)


-- Replacements retain their creation order. Assign order and update the
-- map together so responses received by concurrent clients cannot tie.
rememberCookie :: UTCTime -> Cookie -> CookieStore -> CookieStore
rememberCookie now cookie store =
  case Map.lookup key cookies of
    Just previous | notExpired now (storedCookie previous) ->
      withOrder (creationOrder previous) next
    _ -> withOrder next (next + 1)
  where
    key = cookieKey cookie
    cookies = storedCookies store
    next = nextCreationOrder store
    withOrder order following = CookieStore following (Map.insert key new cookies)
      where
        new = StoredCookie cookie order (renderCookiePair cookie)


{- | Insert a cookie after checking its grammar, flags, size, and
effective domain scope. A domain-scoped manual insert must supply
a domain outside the configured public suffix list.
-}
insertCookieChecked :: CookieJar -> Cookie -> IO (Either CookieError ())
insertCookieChecked jar c = case validateCookie jar c of
  Left err -> pure (Left err)
  Right () -> Right () <$ insertCookie jar c


getCookies :: CookieJar -> IO [Cookie]
getCookies jar = map storedCookie . Map.elems . storedCookies <$> readTVarIO (cjStore jar)


clearCookies :: CookieJar -> IO ()
clearCookies jar = atomically $ writeTVar (cjStore jar) emptyCookieStore


pruneExpired :: CookieJar -> IO ()
pruneExpired jar = do
  now <- getCurrentTime
  atomically $ modifyTVar' (cjStore jar) $ \store ->
    store{storedCookies = Map.filter (notExpired now . storedCookie) (storedCookies store)}


-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

data CookieError
  = CookieNameInvalid !ByteString
  | CookieValueInvalid !ByteString
  | CookieDomainIsPublicSuffix !ByteString
  | {- | @Domain=…@ is not a parent of the request host. First
    field is the request host; second is the offending
    attribute value.
    -}
    CookieDomainNotMatching !ByteString !ByteString
  | {- | @SameSite=None@ without @Secure@ is rejected by every
    modern UA; the field carries the cookie's name.
    -}
    CookieSameSiteNoneRequiresSecure !ByteString
  | -- | @__Secure-@-prefixed cookie without the @Secure@ flag.
    CookieSecurePrefixWithoutSecure !ByteString
  | {- | @__Host-@-prefixed cookie missing one of the constraints
    (must have @Secure@, no @Domain@, @Path=\/@).
    -}
    CookieHostPrefixViolation !ByteString
  | -- | Cookie size exceeds the configured limit (size, limit).
    CookieTooLarge !Int !Int
  deriving stock (Eq, Show)


-- | RFC 6265bis cookie-name grammar: a token (RFC 9110 \u00a75.6.2).
validateCookieName :: ByteString -> Either CookieError ()
validateCookieName bs
  | BS.null bs = Left (CookieNameInvalid bs)
  | BS.all isToken bs = Right ()
  | otherwise = Left (CookieNameInvalid bs)
  where
    isToken w =
      (w >= 0x30 && w <= 0x39) -- 0-9
        || (w >= 0x41 && w <= 0x5A) -- A-Z
        || (w >= 0x61 && w <= 0x7A) -- a-z
        || w == 0x21
        || (w >= 0x23 && w <= 0x27)
        || w == 0x2A
        || w == 0x2B
        || w == 0x2D
        || w == 0x2E
        || w == 0x5E
        || w == 0x5F
        || w == 0x60
        || w == 0x7C
        || w == 0x7E


-- | Validate an RFC 6265 cookie value, including an optional pair of quotes.
validateCookieValue :: ByteString -> Either CookieError ()
validateCookieValue bs = case runParser (cookieValueBytes <* eof) bs of
  OK () _ -> Right ()
  _ -> Left (CookieValueInvalid bs)


validateCookie :: CookieJar -> Cookie -> Either CookieError ()
validateCookie jar c = do
  validateCookieName (cookieName c)
  validateCookieValue (cookieValue c)
  validateSize jar c
  validatePrefixes (cookiePath c == "/") c
  validateSameSite c
  if cookieDomainExplicit c && cjPSL jar (cookieDomain c)
    then Left (CookieDomainIsPublicSuffix (cookieDomain c))
    else Right ()


{- | RFC 6265bis: the @__Secure-@ prefix requires @Secure@; the
@__Host-@ prefix additionally requires no @Domain@ and
@Path=\/@.
-}
validatePrefixes :: Bool -> Cookie -> Either CookieError ()
validatePrefixes rootPathSupplied c
  | hasPrefix "__host-" =
      if cookieSecure c
        && rootPathSupplied
        && not (cookieDomainExplicit c)
        then Right ()
        else Left (CookieHostPrefixViolation (cookieName c))
  | hasPrefix "__secure-" =
      if cookieSecure c
        then Right ()
        else Left (CookieSecurePrefixWithoutSecure (cookieName c))
  | otherwise = Right ()
  where
    name = cookieName c
    hasPrefix prefix =
      "__" `BS.isPrefixOf` name && BS8.map toLower (BS.take (BS.length prefix) name) == prefix


validateSameSite :: Cookie -> Either CookieError ()
validateSameSite c = case cookieSameSite c of
  SameSiteNone
    | not (cookieSecure c) ->
        Left (CookieSameSiteNoneRequiresSecure (cookieName c))
  _ -> Right ()


validateSize :: CookieJar -> Cookie -> Either CookieError ()
validateSize jar c =
  let nv = BS.length (cookieName c) + 1 + BS.length (cookieValue c)
      lim = cjlMaxNameValueBytes (cjLimits jar)
  in if nv > lim then Left (CookieTooLarge nv lim) else Right ()


notExpired :: UTCTime -> Cookie -> Bool
notExpired now c = case cookieExpires c of
  Nothing -> True
  Just e -> e > now


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

{- | Cookie middleware: attach matching @Cookie@ header values to
outgoing requests, parse @Set-Cookie@ on responses and store into
the jar.
-}
withCookies :: CookieJar -> Middleware IO
withCookies jar inner = Transport $ \req -> do
  cookies <- storedCookies <$> readTVarIO (cjStore jar)
  let target = renderRequestURI (requestURI req)
  req' <-
    if Map.null cookies
      then pure req
      else case target of
        Left _ -> pure req
        Right resolved -> do
          now <- getCurrentTime
          pure $ case matchCookies now resolved (Map.elems cookies) of
            [] -> req
            matching ->
              req
                { Network.HTTP.Client.Request.headers =
                    H.insertHeader
                      H.hCookie
                      (renderCookieHeader matching)
                      (Network.HTTP.Client.Request.headers req)
                }
  raw <- sendRaw inner req'
  case H.lookupHeaders H.hSetCookie (Network.HTTP.Client.Response.headers raw) of
    fields@(_ : _) | Right resolved <- target -> do
      now <- getCurrentTime
      case mapMaybe (parseSetCookie jar now resolved) fields of
        [] -> pure ()
        accepted -> atomically $ modifyTVar' (cjStore jar) $ \store ->
          List.foldl' (applySetCookie (uriScheme resolved == SchemeHttps) now) store accepted
    _ -> pure ()
  pure raw
  where
    applySetCookie secureOrigin now store c
      | not secureOrigin
      , any (overlapsSecureCookie now c) (storedCookies store) = store
      | notExpired now c = rememberCookie now c store
      | otherwise = store{storedCookies = Map.delete (cookieKey c) (storedCookies store)}


-- | Combine the already encoded cookie pairs in RFC 6265 request order.
renderCookieHeader :: [StoredCookie] -> ByteString
renderCookieHeader [cookie] = storedPairBytes cookie
renderCookieHeader cookies = BS.intercalate "; " (map storedPairBytes ordered)
  where
    ordered = List.sortBy compareForRequest cookies
    compareForRequest left right =
      compare (pathLength right) (pathLength left) <> compare (creationOrder left) (creationOrder right)
    pathLength = BS.length . cookiePath . storedCookie


renderCookiePair :: Cookie -> ByteString
renderCookiePair cookie = WB.toStrictByteString (Hermes.renderCookie (Hermes.Cookie [pair]))
  where
    pair = Hermes.CookiePair (bytesToShort (cookieName cookie)) (bytesToShort (cookieValue cookie))


-- | Convert to 'ShortText', with replacement for malformed UTF-8
-- supplied through unchecked manual insertion.
bytesToShort :: ByteString -> ST.ShortText
bytesToShort bs = case ST.fromByteString bs of
  Just t -> t
  Nothing -> ST.fromText (TE.decodeUtf8With (\_ _ -> Just '\xfffd') bs)


-- RFC 6265 § 5.4 (simplified): a cookie matches if its domain
-- matches the request host, its path is a prefix of the request
-- path, the scheme is HTTPS if 'secure' is set, and it hasn't
-- expired.
matchCookies :: UTCTime -> URI -> [StoredCookie] -> [StoredCookie]
matchCookies now uri_ = filter (ok . storedCookie)
  where
    host = BS8.map toLower (uriHost uri_)
    pathBytes = uriPath uri_
    secureReq = uriScheme uri_ == SchemeHttps
    ok c =
      notExpired now c
        && (if cookieDomainExplicit c then domainMatches host (cookieDomain c) else host == cookieDomain c)
        && pathMatches pathBytes (cookiePath c)
        && (not (cookieSecure c) || secureReq)


domainMatches :: ByteString -> ByteString -> Bool
domainMatches host dom
  | host == dom = True
  | BS.null dom = False
  | BS.length host > BS.length dom + 1
  , dom `BS.isSuffixOf` host
  , BS.index host (BS.length host - BS.length dom - 1) == 0x2E =
      not (isIPv6Host host) && case runParser (ipv4Parser <* eof) host of
        OK _ _ -> False
        _ -> True
  | otherwise = False


pathMatches :: ByteString -> ByteString -> Bool
pathMatches reqPath cookiePathBs
  | reqPath == cookiePathBs = True
  | cookiePathBs `BS.isPrefixOf` reqPath
  , BS.length reqPath > BS.length cookiePathBs
  , "/" `BS.isSuffixOf` cookiePathBs || BS.index reqPath (BS.length cookiePathBs) == 0x2F =
      True
  | cookiePathBs == "/" = True
  | otherwise = False


-- Compare the incoming path against the existing secure path, in that order.
overlapsSecureCookie :: UTCTime -> Cookie -> StoredCookie -> Bool
overlapsSecureCookie now incoming stored =
  cookieSecure existing
    && cookieName incoming == cookieName existing
    && notExpired now existing
    && pathMatches (cookiePath incoming) (cookiePath existing)
    && (domainMatches domain oldDomain || domainMatches oldDomain domain)
  where
    existing = storedCookie stored
    domain = cookieDomain incoming
    oldDomain = cookieDomain existing


-- ---------------------------------------------------------------------------
-- Set-Cookie parsing (delegates to hermes)
-- ---------------------------------------------------------------------------

-- | Use the typed response codec, then apply the jar's origin policy.
parseSetCookie :: CookieJar -> UTCTime -> URI -> ByteString -> Maybe Cookie
parseSetCookie jar now ctx raw = do
  let limits = cjLimits jar
  parsed <- either (const Nothing) Just $
    Hermes.parseSetCookieFieldWithLimits (cjlMaxNameValueBytes limits) (cjlMaxAttributeBytes limits) raw
  cookie <- fromHermesSetCookie (cjPSL jar) now ctx parsed
  guard (not (cookieSecure cookie) || uriScheme ctx == SchemeHttps)
  case validatePrefixes (Hermes.setCookiePath parsed == Just "/") cookie >> validateSameSite cookie of
    Left _ -> Nothing
    Right () -> Just cookie


fromHermesSetCookie :: PublicSuffixCheck -> UTCTime -> URI -> Hermes.SetCookie -> Maybe Cookie
fromHermesSetCookie isPublicSuffix now ctx sc = case Hermes.setCookieDomain sc of
  Nothing -> Just hostCookie
  Just raw
    | BS.null domain -> Just hostCookie
    | not (domainMatches host domain) -> Nothing
    | not (isPublicSuffix domain) -> Just (newCookie domain True)
    | domain == host -> Just hostCookie
    | otherwise -> Nothing
    where
      domain = BS8.map toLower (stripLeadingDot (ST.toByteString raw))
  where
    host = BS8.map toLower (uriHost ctx)
    hostCookie = newCookie host False
    stBytes = ST.toByteString
    pathBytes = case stBytes <$> Hermes.setCookiePath sc of
        Just path | "/" `BS.isPrefixOf` path -> path
        _ -> defaultPath (uriPath ctx)
    -- RFC 6265 §5.2.2 gives Max-Age precedence over Expires.
    expires = case Hermes.setCookieMaxAge sc of
        Just s -> Just (addUTCTime (fromIntegral s :: NominalDiffTime) now)
        Nothing -> Hermes.setCookieExpires sc
    newCookie domain hasDomain = Cookie
       { cookieName = stBytes (Hermes.setCookieName sc)
       , cookieValue = stBytes (Hermes.setCookieValue sc)
       , cookieDomain = domain
       , cookieDomainExplicit = hasDomain
       , cookiePath = pathBytes
       , cookieExpires = expires
       , cookieSecure = Hermes.setCookieSecure sc
       , cookieHttpOnly = Hermes.setCookieHttpOnly sc
       , cookieSameSite = case Hermes.setCookieSameSite sc of
           Just Hermes.SameSiteStrict -> SameSiteStrict
           Just Hermes.SameSiteLax -> SameSiteLax
           Just Hermes.SameSiteNone -> SameSiteNone
           Nothing -> SameSiteLax
       }


defaultPath :: ByteString -> ByteString
defaultPath p
  | "/" `BS.isPrefixOf` p
  , Just lastSlash <- BS.elemIndexEnd 0x2F p
  , lastSlash > 0 = BS.take lastSlash p
  | otherwise = "/"


stripLeadingDot :: ByteString -> ByteString
stripLeadingDot d = case BS.uncons d of
  Just (0x2E, rest) -> rest
  _ -> d
