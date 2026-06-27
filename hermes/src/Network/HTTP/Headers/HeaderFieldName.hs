{-# LANGUAGE BangPatterns #-}

module Network.HTTP.Headers.HeaderFieldName (
  HeaderFieldName,
  fromHeaderFieldName,
  toText,
  toCIByteString,
  cachedHeaderFieldName,
  headerFieldName,
  unsafeCachedHeaderFromBytestring,
  unsafeUnknownHeaderFromBytestring,
  headerNameToShortByteString,
  headerNameFromShortByteString,

  -- * IANA-registered HTTP Header Field Names

  -- ** Permanent Message Header Field Names
  hAIM,
  hAccept,
  hAcceptAdditions,
  hAcceptCH,
  hAcceptDatetime,
  hAcceptEncoding,
  hAcceptFeatures,
  hAcceptLanguage,
  hAcceptPatch,
  hAcceptPost,
  hAcceptRanges,
  hAcceptSignature,
  hAccessControlAllowCredentials,
  hAccessControlAllowHeaders,
  hAccessControlAllowMethods,
  hAccessControlAllowOrigin,
  hAccessControlExposeHeaders,
  hAccessControlMaxAge,
  hAccessControlRequestHeaders,
  hAccessControlRequestMethod,
  hAge,
  hAllow,
  hALPN,
  hAltSvc,
  hAltUsed,
  hAlternates,
  hApplyToRedirectRef,
  hAuthenticationControl,
  hAuthenticationInfo,
  hAuthorization,
  hCacheControl,
  hCacheStatus,
  hCalManagedID,
  hCalDAVTimezones,
  hCapsuleProtocol,
  hCDNCacheControl,
  hCDNLoop,
  hCertNotAfter,
  hCertNotBefore,
  hClearSiteData,
  hClientCert,
  hClientCertChain,
  hClose,
  hConnection,
  hContentDisposition,
  hContentEncoding,
  hContentLanguage,
  hContentLength,
  hContentLocation,
  hContentRange,
  hContentSecurityPolicy,
  hContentSecurityPolicyReportOnly,
  hContentDigest,
  hContentType,
  hCookie,
  hCrossOriginEmbedderPolicy,
  hCrossOriginEmbedderPolicyReportOnly,
  hCrossOriginOpenerPolicy,
  hCrossOriginOpenerPolicyReportOnly,
  hCrossOriginResourcePolicy,
  hDASL,
  hDate,
  hDAV,
  hDeltaBase,
  hDepth,
  hDerivedFrom,
  hDestination,
  hDPoP,
  hDPoPNonce,
  hEarlyData,
  hEDIINTFeatures,
  hETag,
  hExpect,
  hExpires,
  hForwarded,
  hFrom,
  hHobareg,
  hHost,
  hIf,
  hIfMatch,
  hIfModifiedSince,
  hIfNoneMatch,
  hIfRange,
  hIfScheduleTagMatch,
  hIfUnmodifiedSince,
  hIM,
  hIncludeReferredTokenBindingID,
  hIsolation,
  hKeepAlive,
  hLabel,
  hLastEventID,
  hLastModified,
  hLink,
  hLocation,
  hLockToken,
  hMaxForwards,
  hMementoDatetime,
  hMeter,
  hMIMEVersion,
  hNegotiate,
  hNEL,
  hODataEntityId,
  hODataIsolation,
  hODataMaxVersion,
  hODataVersion,
  hOptionalWWWAuthenticate,
  hOrderingType,
  hOrigin,
  hOriginAgentCluster,
  hOSCORE,
  hOSLCCoreVersion,
  hOverwrite,
  hPermissionsPolicy,
  hPingFrom,
  hPingTo,
  hPosition,
  hPrefer,
  hPreferenceApplied,
  hPriority,
  hProxyAuthenticate,
  hProxyAuthenticationInfo,
  hProxyAuthorization,
  hProxyStatus,
  hPublicKeyPins,
  hPublicKeyPinsReportOnly,
  hRange,
  hRedirectRef,
  hReferer,
  hRefresh,
  hReplayNonce,
  hReportingEndpoints,
  hReprDigest,
  hRetryAfter,
  hScheduleReply,
  hScheduleTag,
  hSecGPC,
  hSecPurpose,
  hSecTokenBinding,
  hSecWebSocketAccept,
  hSecWebSocketExtensions,
  hSecWebSocketKey,
  hSecWebSocketProtocol,
  hSecWebSocketVersion,
  hSecurityScheme,
  hServer,
  hServerTiming,
  hSetCookie,
  hSignature,
  hSignatureInput,
  hSlug,
  hSoapAction,
  hStatusURI,
  hStrictTransportSecurity,
  hSunset,
  hSurrogateCapability,
  hSurrogateControl,
  hTCN,
  hTE,
  hTimeout,
  hTopic,
  hTraceparent,
  hTracestate,
  hTrailer,
  hTransferEncoding,
  hTTL,
  hUpgrade,
  hUrgency,
  hUserAgent,
  hVariantVary,
  hVary,
  hVia,
  hWantContentDigest,
  hWantReprDigest,
  hWWWAuthenticate,
  hXContentTypeOptions,
  hXFrameOptions,
  hWildcardHeader,

  -- ** Provisional Message Header Field Names
  hAMPCacheTransform,
  hConfigurationContext,
  hRepeatabilityClientID,
  hRepeatabilityFirstSent,
  hRepeatabilityRequestID,
  hRepeatabilityResult,
  hTimingAllowOrigin,

  -- ** Deprecated Message Header Field Names
  hAcceptCharset,
  hContentID,
  hDifferentialID,
  hExpectCT,
  hPragma,
  hProtocolInfo,
  hProtocolQuery,

  -- ** Obsoleted Message Header Field Names
  hContentBase,
  hContentMD5,
  hContentScriptType,
  hContentStyleType,
  hContentVersion,
  hCookie2,
  hDefaultStyle,
  hDigest,
  hExt,
  hGetProfile,
  hHTTP2Settings,
  hMan,
  hMethodCheck,
  hMethodCheckExpires,
  hOpt,
  hP3P,
  hPEP,
  hPEPInfo,
  hPICSLabel,
  hProfileObject,
  hProtocol,
  hProtocolRequest,
  hProxyFeatures,
  hProxyInstruction,
  hPublicHeader,
  hRefererRoot,
  hSafe,
  hSetCookie2,
  hSetProfile,
  hURI,
  hWantDigest,
  hWarning,
  hXForwardedFor,
  hXForwardedHost,
  hXForwardedProto,
  hXForwardedPort,
  hXRealIP,
  hXHttpMethodOverride,
  hXRequestID,
  hXCorrelationID,
  hXRequestStart,
  hXTraceID,
  hXXSSProtection,
  hXDownloadOptions,
  hXPermittedCrossDomainPolicies,
  hXDNSPrefetchControl,
  hDNT,
  hXUACompatible,
  hXPoweredBy,
  hXRobotsTag,
  hSaveData,
) where

import Control.DeepSeq (NFData (..))
import Control.Exception
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive.Unsafe (unsafeMk)
import Data.Char (toLower)
import Data.Hashable (Hashable (..))
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as TE
import Data.Text.Internal (Text (..))
import Symbolize
import Text.Read


-- | The type of a header field name.
newtype HeaderFieldName = HeaderFieldName {fromHeaderFieldName :: Symbol}
  deriving newtype (Binary, NFData, Eq, Ord, Hashable)


instance Read HeaderFieldName where
  readPrec = do
    str <- readPrec @String
    return $ HeaderFieldName $ intern $ fmap toLower str


instance Show HeaderFieldName where
  showsPrec _ (HeaderFieldName symbol) =
    let !str = unintern @String symbol
    in shows str


instance IsString HeaderFieldName where
  fromString = HeaderFieldName . fromString . fmap toLower


newtype InvalidHeaderFieldName = InvalidHeaderFieldName Text
  deriving (Show)


instance Exception InvalidHeaderFieldName


toText :: HeaderFieldName -> Text
toText (HeaderFieldName name) = unintern name


toCIByteString :: HeaderFieldName -> CI ByteString
toCIByteString = unsafeMk . TE.encodeUtf8 . toText


{- | Construct a 'HeaderFieldName' from a text input.

This function will intern the text for fast comparisons,
which makes it efficient for lookups on well-known header field names
and other frequently used headers.
-}
cachedHeaderFieldName :: Text -> HeaderFieldName
cachedHeaderFieldName txt
  | T.isAscii txt = HeaderFieldName $ intern $ T.toLower txt
  | otherwise = throw $ InvalidHeaderFieldName txt
{-# INLINE cachedHeaderFieldName #-}


{- | Construct a 'HeaderFieldName' from a text input.

If the input is cached, this function will return the cached value.
If the input is not cached, will return a new 'HeaderFieldName' that is not cached.

This function is therefore suitable for arbitrary user input while reducing the
number of allocations and comparisons for well-known header field names.
-}
headerFieldName :: Text -> HeaderFieldName
headerFieldName txt
  | T.isAscii txt = HeaderFieldName $ intern txt
  | otherwise = throw $ InvalidHeaderFieldName txt
{-# INLINE headerFieldName #-}


{- | Construct a 'HeaderFieldName' from a bytestring input that is a well-formed, case-folded header.

This function will interns the text, so it is not suitable for arbitrary user input.

__Warning__: This function is unsafe because it does not check if the input is valid ASCII,
a valid header field name according to the HTTP spec, and it does not fold the input to
lowercase.
-}
unsafeCachedHeaderFromBytestring :: ByteString -> HeaderFieldName
unsafeCachedHeaderFromBytestring = HeaderFieldName . intern
{-# INLINE unsafeCachedHeaderFromBytestring #-}


{- | Construct a 'HeaderFieldName' from a bytestring input that is a well-formed, case-folded header.

This function will not intern the text, so it is suitable for arbitrary user input.

__Warning__: This function is unsafe because it does not check if the input is valid ASCII,
a valid header field name according to the HTTP spec, and it does not fold the input to
lowercase.
-}
unsafeUnknownHeaderFromBytestring :: ByteString -> HeaderFieldName
unsafeUnknownHeaderFromBytestring = HeaderFieldName . intern
{-# INLINE unsafeUnknownHeaderFromBytestring #-}


-- | Convert a 'HeaderFieldName' to a 'ShortByteString'.
headerNameToShortByteString :: HeaderFieldName -> SBS.ShortByteString
headerNameToShortByteString (HeaderFieldName sym) = unintern sym


headerNameFromShortByteString :: SBS.ShortByteString -> HeaderFieldName
headerNameFromShortByteString sbs@(SBS.SBS arr) = headerFieldName $ Text (A.ByteArray arr) 0 (SBS.length sbs)


{- | @A-IM@ HTTP Header
Permanent: [RFC 3229: Delta encoding in HTTP](https://datatracker.ietf.org/doc/html/rfc3229)

See "Network.HTTP.Headers.AIM" for the typed codec.
-}
hAIM :: HeaderFieldName
hAIM = "A-IM"


{- | @Accept@ HTTP Header
Permanent: [RFC9110, Section 12.5.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.1)

See "Network.HTTP.Headers.Accept" for the typed codec.
-}
hAccept :: HeaderFieldName
hAccept = "Accept"


{- | @Accept-Additions@ HTTP Header
Permanent: [RFC 2324: Hyper Text Coffee Pot Control Protocol (HTCPCP/1.0)](https://datatracker.ietf.org/doc/html/rfc2324)

See "Network.HTTP.Headers.AcceptAdditions" for the typed codec.
-}
hAcceptAdditions :: HeaderFieldName
hAcceptAdditions = "Accept-Additions"


{- | @Accept-CH@ HTTP Header
Permanent: [RFC 8942, Section 3.1: HTTP Client Hints](https://datatracker.ietf.org/doc/html/rfc8942#section-3.1)

See "Network.HTTP.Headers.AcceptCH" for the typed codec.
-}
hAcceptCH :: HeaderFieldName
hAcceptCH = "Accept-CH"


{- | @Accept-Charset@ HTTP Header
Deprecated: [RFC9110, Section 12.5.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.2)

See "Network.HTTP.Headers.AcceptCharset" for the typed codec.
-}
hAcceptCharset :: HeaderFieldName
hAcceptCharset = "Accept-Charset"


{- | @Accept-Datetime@ HTTP Header
Permanent: [RFC 7089: HTTP Framework for Time-Based Access to Resource States -- Memento](https://datatracker.ietf.org/doc/html/rfc7089)

See "Network.HTTP.Headers.AcceptDatetime" for the typed codec.
-}
hAcceptDatetime :: HeaderFieldName
hAcceptDatetime = "Accept-Datetime"


{- | @Accept-Encoding@ HTTP Header
Permanent: [RFC9110, Section 12.5.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.3)

See "Network.HTTP.Headers.AcceptEncoding" for the typed codec.
-}
hAcceptEncoding :: HeaderFieldName
hAcceptEncoding = "Accept-Encoding"


{- | @Accept-Features@ HTTP Header
Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)

See "Network.HTTP.Headers.AcceptFeatures" for the typed codec.
-}
hAcceptFeatures :: HeaderFieldName
hAcceptFeatures = "Accept-Features"


{- | @Accept-Language@ HTTP Header
Permanent: [RFC9110, Section 12.5.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.4)

See "Network.HTTP.Headers.AcceptLanguage" for the typed codec.
-}
hAcceptLanguage :: HeaderFieldName
hAcceptLanguage = "Accept-Language"


{- | @Accept-Patch@ HTTP Header
Permanent: [RFC 5789: PATCH Method for HTTP](https://datatracker.ietf.org/doc/html/rfc5789)

See "Network.HTTP.Headers.AcceptPatch" for the typed codec.
-}
hAcceptPatch :: HeaderFieldName
hAcceptPatch = "Accept-Patch"


{- | @Accept-Post@ HTTP Header
Permanent: [Linked Data Platform 1.0](https://www.w3.org/TR/ldp/)

See "Network.HTTP.Headers.AcceptPost" for the typed codec.
-}
hAcceptPost :: HeaderFieldName
hAcceptPost = "Accept-Post"


{- | @Accept-Ranges@ HTTP Header
Permanent: [RFC9110, Section 14.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-14.3)

See "Network.HTTP.Headers.AcceptRanges" for the typed codec.
-}
hAcceptRanges :: HeaderFieldName
hAcceptRanges = "Accept-Ranges"


{- | @Accept-Signature@ HTTP Header
Permanent: [RFC 9421, Section 5.1: HTTP Message Signatures](https://datatracker.ietf.org/doc/html/rfc9421#section-5.1)

See "Network.HTTP.Headers.AcceptSignature" for the typed codec.
-}
hAcceptSignature :: HeaderFieldName
hAcceptSignature = "Accept-Signature"


{- | @Access-Control-Allow-Credentials@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlAllowCredentials" for the typed codec.
-}
hAccessControlAllowCredentials :: HeaderFieldName
hAccessControlAllowCredentials = "Access-Control-Allow-Credentials"


{- | @Access-Control-Allow-Headers@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlAllowHeaders" for the typed codec.
-}
hAccessControlAllowHeaders :: HeaderFieldName
hAccessControlAllowHeaders = "Access-Control-Allow-Headers"


{- | @Access-Control-Allow-Methods@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlAllowMethods" for the typed codec.
-}
hAccessControlAllowMethods :: HeaderFieldName
hAccessControlAllowMethods = "Access-Control-Allow-Methods"


{- | @Access-Control-Allow-Origin@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlAllowOrigin" for the typed codec.
-}
hAccessControlAllowOrigin :: HeaderFieldName
hAccessControlAllowOrigin = "Access-Control-Allow-Origin"


{- | @Access-Control-Expose-Headers@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlExposeHeaders" for the typed codec.
-}
hAccessControlExposeHeaders :: HeaderFieldName
hAccessControlExposeHeaders = "Access-Control-Expose-Headers"


{- | @Access-Control-Max-Age@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlMaxAge" for the typed codec.
-}
hAccessControlMaxAge :: HeaderFieldName
hAccessControlMaxAge = "Access-Control-Max-Age"


{- | @Access-Control-Request-Headers@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlRequestHeaders" for the typed codec.
-}
hAccessControlRequestHeaders :: HeaderFieldName
hAccessControlRequestHeaders = "Access-Control-Request-Headers"


{- | @Access-Control-Request-Method@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.AccessControlRequestMethod" for the typed codec.
-}
hAccessControlRequestMethod :: HeaderFieldName
hAccessControlRequestMethod = "Access-Control-Request-Method"


{- | @Age@ HTTP Header
Permanent: [RFC9111, Section 5.1: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.1)

See "Network.HTTP.Headers.Age" for the typed codec.
-}
hAge :: HeaderFieldName
hAge = "Age"


{- | @Allow@ HTTP Header
Permanent: [RFC9110, Section 10.2.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.1)

See "Network.HTTP.Headers.Allow" for the typed codec.
-}
hAllow :: HeaderFieldName
hAllow = "Allow"


{- | @ALPN@ HTTP Header
Permanent: [RFC 7639, Section 2: The ALPN HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc7639#section-2)

See "Network.HTTP.Headers.ALPN" for the typed codec.
-}
hALPN :: HeaderFieldName
hALPN = "ALPN"


{- | @Alt-Svc@ HTTP Header
Permanent: [RFC 7838: HTTP Alternative Services](https://datatracker.ietf.org/doc/html/rfc7838)

See "Network.HTTP.Headers.AltSvc" for the typed codec.
-}
hAltSvc :: HeaderFieldName
hAltSvc = "Alt-Svc"


{- | @Alt-Used@ HTTP Header
Permanent: [RFC 7838: HTTP Alternative Services](https://datatracker.ietf.org/doc/html/rfc7838)

See "Network.HTTP.Headers.AltUsed" for the typed codec.
-}
hAltUsed :: HeaderFieldName
hAltUsed = "Alt-Used"


{- | @Alternates@ HTTP Header
Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)

See "Network.HTTP.Headers.Alternates" for the typed codec.
-}
hAlternates :: HeaderFieldName
hAlternates = "Alternates"


{- | @AMP-Cache-Transform@ HTTP Header
Provisional: [AMP-Cache-Transform HTTP request header](https://github.com/ampproject/amphtml/blob/main/spec/amp-cache-transform.md)

See "Network.HTTP.Headers.AMPCacheTransform" for the typed codec.
-}
hAMPCacheTransform :: HeaderFieldName
hAMPCacheTransform = "AMP-Cache-Transform"


{- | @Apply-To-Redirect-Ref@ HTTP Header
Permanent: [RFC 4437: Web Distributed Authoring and Versioning (WebDAV) Redirect Reference Resources](https://datatracker.ietf.org/doc/html/rfc4437)

See "Network.HTTP.Headers.ApplyToRedirectRef" for the typed codec.
-}
hApplyToRedirectRef :: HeaderFieldName
hApplyToRedirectRef = "Apply-To-Redirect-Ref"


{- | @Authentication-Control@ HTTP Header
Permanent: [RFC 8053, Section 4: HTTP Authentication Extensions for Interactive Clients](https://datatracker.ietf.org/doc/html/rfc8053#section-4)

See "Network.HTTP.Headers.AuthenticationControl" for the typed codec.
-}
hAuthenticationControl :: HeaderFieldName
hAuthenticationControl = "Authentication-Control"


{- | @Authentication-Info@ HTTP Header
Permanent: [RFC9110, Section 11.6.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.6.3)

See "Network.HTTP.Headers.AuthenticationInfo" for the typed codec.
-}
hAuthenticationInfo :: HeaderFieldName
hAuthenticationInfo = "Authentication-Info"


{- | @Authorization@ HTTP Header
Permanent: [RFC9110, Section 11.6.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.6.2)

See "Network.HTTP.Headers.Authorization" for the typed codec.
-}
hAuthorization :: HeaderFieldName
hAuthorization = "Authorization"


{- | @Cache-Control@ HTTP Header
Permanent: [RFC9111, Section 5.2](https://datatracker.ietf.org/doc/html/rfc9111#section-5.2)

See "Network.HTTP.Headers.CacheControl" for the typed codec.
-}
hCacheControl :: HeaderFieldName
hCacheControl = "Cache-Control"


{- | @Cache-Status@ HTTP Header
Permanent: [RFC9211: The Cache-Status HTTP Response Header Field](https://datatracker.ietf.org/doc/html/rfc9211)

See "Network.HTTP.Headers.CacheStatus" for the typed codec.
-}
hCacheStatus :: HeaderFieldName
hCacheStatus = "Cache-Status"


{- | @Cal-Managed-ID@ HTTP Header
Permanent: [RFC 8607, Section 5.1: Calendaring Extensions to WebDAV (CalDAV): Managed Attachments](https://datatracker.ietf.org/doc/html/rfc8607#section-5.1)

See "Network.HTTP.Headers.CalManagedID" for the typed codec.
-}
hCalManagedID :: HeaderFieldName
hCalManagedID = "Cal-Managed-ID"


{- | @CalDAV-Timezones@ HTTP Header
Permanent: [RFC 7809, Section 7.1: Calendaring Extensions to WebDAV (CalDAV): Time Zones by Reference](https://datatracker.ietf.org/doc/html/rfc7809#section-7.1)

See "Network.HTTP.Headers.CalDAVTimezones" for the typed codec.
-}
hCalDAVTimezones :: HeaderFieldName
hCalDAVTimezones = "CalDAV-Timezones"


{- | @Capsule-Protocol@ HTTP Header
Permanent: [RFC9297](https://datatracker.ietf.org/doc/html/rfc9297)

See "Network.HTTP.Headers.CapsuleProtocol" for the typed codec.
-}
hCapsuleProtocol :: HeaderFieldName
hCapsuleProtocol = "Capsule-Protocol"


{- | @CDN-Cache-Control@ HTTP Header
Permanent: [RFC9213: Targeted HTTP Cache Control](https://datatracker.ietf.org/doc/html/rfc9213) Cache directives targeted at content delivery networks

See "Network.HTTP.Headers.CDNCacheControl" for the typed codec.
-}
hCDNCacheControl :: HeaderFieldName
hCDNCacheControl = "CDN-Cache-Control"


{- | @CDN-Loop@ HTTP Header
Permanent: [RFC 8586: Loop Detection in Content Delivery Networks (CDNs)](https://datatracker.ietf.org/doc/html/rfc8586)

See "Network.HTTP.Headers.CDNLoop" for the typed codec.
-}
hCDNLoop :: HeaderFieldName
hCDNLoop = "CDN-Loop"


{- | @Cert-Not-After@ HTTP Header
Permanent: [RFC 8739, Section 3.3: Support for Short-Term, Automatically Renewed (STAR) Certificates in the Automated Certificate Management Environment (ACME)](https://datatracker.ietf.org/doc/html/rfc8739#section-3.3)

See "Network.HTTP.Headers.CertNotAfter" for the typed codec.
-}
hCertNotAfter :: HeaderFieldName
hCertNotAfter = "Cert-Not-After"


{- | @Cert-Not-Before@ HTTP Header
Permanent: [RFC 8739, Section 3.3: Support for Short-Term, Automatically Renewed (STAR) Certificates in the Automated Certificate Management Environment (ACME)](https://datatracker.ietf.org/doc/html/rfc8739#section-3.3)

See "Network.HTTP.Headers.CertNotBefore" for the typed codec.
-}
hCertNotBefore :: HeaderFieldName
hCertNotBefore = "Cert-Not-Before"


{- | @Clear-Site-Data@ HTTP Header
Permanent: [Clear Site Data](https://w3c.github.io/webappsec-clear-site-data/)

See "Network.HTTP.Headers.ClearSiteData" for the typed codec.
-}
hClearSiteData :: HeaderFieldName
hClearSiteData = "Clear-Site-Data"


{- | @Client-Cert@ HTTP Header
Permanent: [RFC9440, Section 2: Client-Cert HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc9440#section-2)

See "Network.HTTP.Headers.ClientCert" for the typed codec.
-}
hClientCert :: HeaderFieldName
hClientCert = "Client-Cert"


{- | @Client-Cert-Chain@ HTTP Header
Permanent: [RFC9440, Section 2: Client-Cert HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc9440#section-2)

See "Network.HTTP.Headers.ClientCertChain" for the typed codec.
-}
hClientCertChain :: HeaderFieldName
hClientCertChain = "Client-Cert-Chain"


{- | @Close@ HTTP Header
Permanent: [RFC9112, Section 9.6: HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc9112#section-9.6) (reserved)

See "Network.HTTP.Headers.Close" for the typed codec.
-}
hClose :: HeaderFieldName
hClose = "Close"


{- | @Configuration-Context@ HTTP Header
Provisional: [OSLC Configuration Management Version 1.0. Part 3: Configuration Specification](https://docs.oasis-open.org/oslc-core/oslc-cm/v1.0/oslc-cm-v1.0-part3-configuration-management-spec.html)

See "Network.HTTP.Headers.ConfigurationContext" for the typed codec.
-}
hConfigurationContext :: HeaderFieldName
hConfigurationContext = "Configuration-Context"


{- | @Connection@ HTTP Header
Permanent: [RFC9110, Section 7.6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.1)

See "Network.HTTP.Headers.Connection" for the typed codec.
-}
hConnection :: HeaderFieldName
hConnection = "Connection"


{- | @Content-Base@ HTTP Header
Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)[RFC 2616: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2616)

See "Network.HTTP.Headers.ContentBase" for the typed codec.
-}
hContentBase :: HeaderFieldName
hContentBase = "Content-Base"


{- | @Content-Digest@ HTTP Header
Permanent: [RFC 9530, Section 2: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-2)

See "Network.HTTP.Headers.ContentDigest" for the typed codec.
-}
hContentDigest :: HeaderFieldName
hContentDigest = "Content-Digest"


{- | @Content-Disposition@ HTTP Header
Permanent: [RFC 6266: Use of the Content-Disposition Header Field in the Hypertext Transfer Protocol (HTTP)](https://datatracker.ietf.org/doc/html/rfc6266)

See "Network.HTTP.Headers.ContentDisposition" for the typed codec.
-}
hContentDisposition :: HeaderFieldName
hContentDisposition = "Content-Disposition"


{- | @Content-Encoding@ HTTP Header
Permanent: [RFC9110, Section 8.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.4)

See "Network.HTTP.Headers.ContentEncoding" for the typed codec.
-}
hContentEncoding :: HeaderFieldName
hContentEncoding = "Content-Encoding"


{- | @Content-ID@ HTTP Header
Deprecated: [The HTTP Distribution and Replication Protocol](https://www.w3.org/TR/NOTE-drp-19970625.html)

See "Network.HTTP.Headers.ContentID" for the typed codec.
-}
hContentID :: HeaderFieldName
hContentID = "Content-ID"


{- | @Content-Language@ HTTP Header
Permanent: [RFC9110, Section 8.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.5)

See "Network.HTTP.Headers.ContentLanguage" for the typed codec.
-}
hContentLanguage :: HeaderFieldName
hContentLanguage = "Content-Language"


{- | @Content-Length@ HTTP Header
Permanent: [RFC9110, Section 8.6: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.6)

See "Network.HTTP.Headers.ContentLength" for the typed codec.
-}
hContentLength :: HeaderFieldName
hContentLength = "Content-Length"


{- | @Content-Location@ HTTP Header
Permanent: [RFC9110, Section 8.7: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.7)

See "Network.HTTP.Headers.ContentLocation" for the typed codec.
-}
hContentLocation :: HeaderFieldName
hContentLocation = "Content-Location"


{- | @Content-MD5@ HTTP Header
Obsoleted: [RFC 2616, Section 14.15: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2616#section-14.15)[RFC 7231, Appendix B: Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content](https://datatracker.ietf.org/doc/html/rfc7231#appendix-B)

See "Network.HTTP.Headers.ContentMD5" for the typed codec.
-}
hContentMD5 :: HeaderFieldName
hContentMD5 = "Content-MD5"


{- | @Content-Range@ HTTP Header
Permanent: [RFC9110, Section 14.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-14.4)

See "Network.HTTP.Headers.ContentRange" for the typed codec.
-}
hContentRange :: HeaderFieldName
hContentRange = "Content-Range"


{- | @Content-Script-Type@ HTTP Header
Obsoleted: [HTML 4.01 Specification](https://www.w3.org/TR/html401/)

See "Network.HTTP.Headers.ContentScriptType" for the typed codec.
-}
hContentScriptType :: HeaderFieldName
hContentScriptType = "Content-Script-Type"


{- | @Content-Security-Policy@ HTTP Header
Permanent: [Content Security Policy Level 3](https://www.w3.org/TR/CSP3/)

See "Network.HTTP.Headers.ContentSecurityPolicy" for the typed codec.
-}
hContentSecurityPolicy :: HeaderFieldName
hContentSecurityPolicy = "Content-Security-Policy"


{- | @Content-Security-Policy-Report-Only@ HTTP Header
Permanent: [Content Security Policy Level 3](https://www.w3.org/TR/CSP3/)

See "Network.HTTP.Headers.ContentSecurityPolicyReportOnly" for the typed codec.
-}
hContentSecurityPolicyReportOnly :: HeaderFieldName
hContentSecurityPolicyReportOnly = "Content-Security-Policy-Report-Only"


{- | @Content-Style-Type@ HTTP Header
Obsoleted: [HTML 4.01 Specification](https://www.w3.org/TR/html401/)

See "Network.HTTP.Headers.ContentStyleType" for the typed codec.
-}
hContentStyleType :: HeaderFieldName
hContentStyleType = "Content-Style-Type"


{- | @Content-Type@ HTTP Header
Permanent: [RFC9110, Section 8.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.3)

See "Network.HTTP.Headers.ContentType" for the typed codec.
-}
hContentType :: HeaderFieldName
hContentType = "Content-Type"


{- | @Content-Version@ HTTP Header
Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)

See "Network.HTTP.Headers.ContentVersion" for the typed codec.
-}
hContentVersion :: HeaderFieldName
hContentVersion = "Content-Version"


{- | @Cookie@ HTTP Header
Permanent: [RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)

See "Network.HTTP.Headers.Cookie" for the typed codec.
-}
hCookie :: HeaderFieldName
hCookie = "Cookie"


{- | @Cookie2@ HTTP Header
Obsoleted: [RFC 2965: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc2965)[RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)

See "Network.HTTP.Headers.Cookie2" for the typed codec.
-}
hCookie2 :: HeaderFieldName
hCookie2 = "Cookie2"


{- | @Cross-Origin-Embedder-Policy@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.CrossOriginEmbedderPolicy" for the typed codec.
-}
hCrossOriginEmbedderPolicy :: HeaderFieldName
hCrossOriginEmbedderPolicy = "Cross-Origin-Embedder-Policy"


{- | @Cross-Origin-Embedder-Policy-Report-Only@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.CrossOriginEmbedderPolicyReportOnly" for the typed codec.
-}
hCrossOriginEmbedderPolicyReportOnly :: HeaderFieldName
hCrossOriginEmbedderPolicyReportOnly = "Cross-Origin-Embedder-Policy-Report-Only"


{- | @Cross-Origin-Opener-Policy@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.CrossOriginOpenerPolicy" for the typed codec.
-}
hCrossOriginOpenerPolicy :: HeaderFieldName
hCrossOriginOpenerPolicy = "Cross-Origin-Opener-Policy"


{- | @Cross-Origin-Opener-Policy-Report-Only@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.CrossOriginOpenerPolicyReportOnly" for the typed codec.
-}
hCrossOriginOpenerPolicyReportOnly :: HeaderFieldName
hCrossOriginOpenerPolicyReportOnly = "Cross-Origin-Opener-Policy-Report-Only"


{- | @Cross-Origin-Resource-Policy@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.CrossOriginResourcePolicy" for the typed codec.
-}
hCrossOriginResourcePolicy :: HeaderFieldName
hCrossOriginResourcePolicy = "Cross-Origin-Resource-Policy"


{- | @DASL@ HTTP Header
Permanent: [RFC 5323: Web Distributed Authoring and Versioning (WebDAV) SEARCH](https://datatracker.ietf.org/doc/html/rfc5323)

See "Network.HTTP.Headers.DASL" for the typed codec.
-}
hDASL :: HeaderFieldName
hDASL = "DASL"


{- | @Date@ HTTP Header
Permanent: [RFC9110, Section 6.6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-6.6.1)

See "Network.HTTP.Headers.Date" for the typed codec.
-}
hDate :: HeaderFieldName
hDate = "Date"


{- | @DAV@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.DAV" for the typed codec.
-}
hDAV :: HeaderFieldName
hDAV = "DAV"


{- | @Default-Style@ HTTP Header
Obsoleted: [HTML 4.01 Specification](https://www.w3.org/TR/html401/)

See "Network.HTTP.Headers.DefaultStyle" for the typed codec.
-}
hDefaultStyle :: HeaderFieldName
hDefaultStyle = "Default-Style"


{- | @Delta-Base@ HTTP Header
Permanent: [RFC 3229: Delta encoding in HTTP](https://datatracker.ietf.org/doc/html/rfc3229)

See "Network.HTTP.Headers.DeltaBase" for the typed codec.
-}
hDeltaBase :: HeaderFieldName
hDeltaBase = "Delta-Base"


{- | @Depth@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.Depth" for the typed codec.
-}
hDepth :: HeaderFieldName
hDepth = "Depth"


{- | @Derived-From@ HTTP Header
Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)

See "Network.HTTP.Headers.DerivedFrom" for the typed codec.
-}
hDerivedFrom :: HeaderFieldName
hDerivedFrom = "Derived-From"


{- | @Destination@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.Destination" for the typed codec.
-}
hDestination :: HeaderFieldName
hDestination = "Destination"


{- | @Differential-ID@ HTTP Header
Deprecated: [The HTTP Distribution and Replication Protocol](https://www.w3.org/TR/NOTE-drp-19970625.html)

See "Network.HTTP.Headers.DifferentialID" for the typed codec.
-}
hDifferentialID :: HeaderFieldName
hDifferentialID = "Differential-ID"


{- | @Digest@ HTTP Header
Obsoleted: [RFC 3230: Instance Digests in HTTP](https://datatracker.ietf.org/doc/html/rfc3230)[RFC 9530, Section 1.3: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-1.3)

See "Network.HTTP.Headers.Digest" for the typed codec.
-}
hDigest :: HeaderFieldName
hDigest = "Digest"


{- | @DPoP@ HTTP Header
Permanent: [RFC9449: OAuth 2.0 Demonstrating Proof of Possession (DPoP)](https://datatracker.ietf.org/doc/html/rfc9449)

See "Network.HTTP.Headers.DPoP" for the typed codec.
-}
hDPoP :: HeaderFieldName
hDPoP = "DPoP"


{- | @DPoP-Nonce@ HTTP Header
Permanent: [RFC9449: OAuth 2.0 Demonstrating Proof of Possession (DPoP)](https://datatracker.ietf.org/doc/html/rfc9449)

See "Network.HTTP.Headers.DPoPNonce" for the typed codec.
-}
hDPoPNonce :: HeaderFieldName
hDPoPNonce = "DPoP-Nonce"


{- | @Early-Data@ HTTP Header
Permanent: [RFC 8470: Using Early Data in HTTP](https://datatracker.ietf.org/doc/html/rfc8470)

See "Network.HTTP.Headers.EarlyData" for the typed codec.
-}
hEarlyData :: HeaderFieldName
hEarlyData = "Early-Data"


{- | @EDIINT-Features@ HTTP Header
Provisional: [RFC 6017: Electronic Data Interchange - Internet Integration (EDIINT) Features Header Field](https://datatracker.ietf.org/doc/html/rfc6017)

See "Network.HTTP.Headers.EDIINTFeatures" for the typed codec.
-}
hEDIINTFeatures :: HeaderFieldName
hEDIINTFeatures = "EDIINT-Features"


{- | @ETag@ HTTP Header
Permanent: [RFC9110, Section 8.8.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.8.3)

See "Network.HTTP.Headers.ETag" for the typed codec.
-}
hETag :: HeaderFieldName
hETag = "ETag"


{- | @Expect@ HTTP Header
Permanent: [RFC9110, Section 10.1.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.1)

See "Network.HTTP.Headers.Expect" for the typed codec.
-}
hExpect :: HeaderFieldName
hExpect = "Expect"


{- | @Expect-CT@ HTTP Header
Deprecated: [RFC9163: Expect-CT Extension for HTTP](https://datatracker.ietf.org/doc/html/rfc9163)[IESG](https://www.ietf.org/)[HTTPBIS](https://datatracker.ietf.org/doc/html/rfc9110)

See "Network.HTTP.Headers.ExpectCT" for the typed codec.
-}
hExpectCT :: HeaderFieldName
hExpectCT = "Expect-CT"


{- | @Expires@ HTTP Header
Permanent: [RFC9111, Section 5.3: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.3)

See "Network.HTTP.Headers.Expires" for the typed codec.
-}
hExpires :: HeaderFieldName
hExpires = "Expires"


{- | @Ext@ HTTP Header
Obsoleted: [RFC 2774: An HTTP Extension Framework](https://datatracker.ietf.org/doc/html/rfc2774)[status-change-http-experiments-to-historic](https://www.ietf.org/)

See "Network.HTTP.Headers.Ext" for the typed codec.
-}
hExt :: HeaderFieldName
hExt = "Ext"


{- | @Forwarded@ HTTP Header
Permanent: [RFC 7239: Forwarded HTTP Extension](https://datatracker.ietf.org/doc/html/rfc7239)

See "Network.HTTP.Headers.Forwarded" for the typed codec.
-}
hForwarded :: HeaderFieldName
hForwarded = "Forwarded"


{- | @From@ HTTP Header
Permanent: [RFC9110, Section 10.1.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.2)

See "Network.HTTP.Headers.From" for the typed codec.
-}
hFrom :: HeaderFieldName
hFrom = "From"


{- | @GetProfile@ HTTP Header
Obsoleted: [Implementation of OPS Over HTTP](https://www.w3.org/TR/OPS-Implementation/)

See "Network.HTTP.Headers.GetProfile" for the typed codec.
-}
hGetProfile :: HeaderFieldName
hGetProfile = "GetProfile"


{- | @Hobareg@ HTTP Header
Permanent: [RFC 7486, Section 6.1.1: HTTP Origin-Bound Authentication (HOBA)](https://datatracker.ietf.org/doc/html/rfc7486#section-6.1.1)

See "Network.HTTP.Headers.Hobareg" for the typed codec.
-}
hHobareg :: HeaderFieldName
hHobareg = "Hobareg"


{- | @Host@ HTTP Header
Permanent: [RFC9110, Section 7.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.2)

See "Network.HTTP.Headers.Host" for the typed codec.
-}
hHost :: HeaderFieldName
hHost = "Host"


{- | @HTTP2-Settings@ HTTP Header
Obsoleted: [RFC 7540, Section 3.2.1: Hypertext Transfer Protocol Version 2 (HTTP/2)](https://datatracker.ietf.org/doc/html/rfc7540#section-3.2.1) Obsolete; see Section 11.1 of [RFC9113](https://datatracker.ietf.org/doc/html/rfc9113)

See "Network.HTTP.Headers.HTTP2Settings" for the typed codec.
-}
hHTTP2Settings :: HeaderFieldName
hHTTP2Settings = "HTTP2-Settings"


{- | @If@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.If" for the typed codec.
-}
hIf :: HeaderFieldName
hIf = "If"


{- | @If-Match@ HTTP Header
Permanent: [RFC9110, Section 13.1.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.1)

See "Network.HTTP.Headers.IfMatch" for the typed codec.
-}
hIfMatch :: HeaderFieldName
hIfMatch = "If-Match"


{- | @If-Modified-Since@ HTTP Header
Permanent: [RFC9110, Section 13.1.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.3)

See "Network.HTTP.Headers.IfModifiedSince" for the typed codec.
-}
hIfModifiedSince :: HeaderFieldName
hIfModifiedSince = "If-Modified-Since"


{- | @If-None-Match@ HTTP Header
Permanent: [RFC9110, Section 13.1.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.2)

See "Network.HTTP.Headers.IfNoneMatch" for the typed codec.
-}
hIfNoneMatch :: HeaderFieldName
hIfNoneMatch = "If-None-Match"


{- | @If-Range@ HTTP Header
Permanent: [RFC9110, Section 13.1.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.5)

See "Network.HTTP.Headers.IfRange" for the typed codec.
-}
hIfRange :: HeaderFieldName
hIfRange = "If-Range"


{- | @If-Schedule-Tag-Match@ HTTP Header
Permanent: [ RFC 6338: Scheduling Extensions to CalDAV](https://datatracker.ietf.org/doc/html/rfc6338)

See "Network.HTTP.Headers.IfScheduleTagMatch" for the typed codec.
-}
hIfScheduleTagMatch :: HeaderFieldName
hIfScheduleTagMatch = "If-Schedule-Tag-Match"


{- | @If-Unmodified-Since@ HTTP Header
Permanent: [RFC9110, Section 13.1.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.4)

See "Network.HTTP.Headers.IfUnmodifiedSince" for the typed codec.
-}
hIfUnmodifiedSince :: HeaderFieldName
hIfUnmodifiedSince = "If-Unmodified-Since"


{- | @IM@ HTTP Header
Permanent: [RFC 3229: Delta encoding in HTTP](https://datatracker.ietf.org/doc/html/rfc3229)

See "Network.HTTP.Headers.IM" for the typed codec.
-}
hIM :: HeaderFieldName
hIM = "IM"


{- | @Include-Referred-Token-Binding-ID@ HTTP Header
Permanent: [RFC 8473: Token Binding over HTTP](https://datatracker.ietf.org/doc/html/rfc8473)

See "Network.HTTP.Headers.IncludeReferredTokenBindingID" for the typed codec.
-}
hIncludeReferredTokenBindingID :: HeaderFieldName
hIncludeReferredTokenBindingID = "Include-Referred-Token-Binding-ID"


{- | @Isolation@ HTTP Header
Provisional: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)

See "Network.HTTP.Headers.Isolation" for the typed codec.
-}
hIsolation :: HeaderFieldName
hIsolation = "Isolation"


{- | @Keep-Alive@ HTTP Header
Permanent: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)

See "Network.HTTP.Headers.KeepAlive" for the typed codec.
-}
hKeepAlive :: HeaderFieldName
hKeepAlive = "Keep-Alive"


{- | @Label@ HTTP Header
Permanent: [RFC 3253: Versioning Extensions to WebDAV: (Web Distributed Authoring and Versioning)](https://datatracker.ietf.org/doc/html/rfc3253)

See "Network.HTTP.Headers.Label" for the typed codec.
-}
hLabel :: HeaderFieldName
hLabel = "Label"


{- | @Last-Event-ID@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.LastEventID" for the typed codec.
-}
hLastEventID :: HeaderFieldName
hLastEventID = "Last-Event-ID"


{- | @Last-Modified@ HTTP Header
Permanent: [RFC9110, Section 8.8.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.8.2)

See "Network.HTTP.Headers.LastModified" for the typed codec.
-}
hLastModified :: HeaderFieldName
hLastModified = "Last-Modified"


{- | @Link@ HTTP Header
Permanent: [RFC 8288: Web Linking](https://datatracker.ietf.org/doc/html/rfc8288)

See "Network.HTTP.Headers.Link" for the typed codec.
-}
hLink :: HeaderFieldName
hLink = "Link"


{- | @Location@ HTTP Header
Permanent: [RFC9110, Section 10.2.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.2)

See "Network.HTTP.Headers.Location" for the typed codec.
-}
hLocation :: HeaderFieldName
hLocation = "Location"


{- | @Lock-Token@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.LockToken" for the typed codec.
-}
hLockToken :: HeaderFieldName
hLockToken = "Lock-Token"


{- | @Man@ HTTP Header
Obsoleted: [RFC 2774: An HTTP Extension Framework](https://datatracker.ietf.org/doc/html/rfc2774)[status-change-http-experiments-to-historic](https://www.ietf.org/)

See "Network.HTTP.Headers.Man" for the typed codec.
-}
hMan :: HeaderFieldName
hMan = "Man"


{- | @Max-Forwards@ HTTP Header
Permanent: [RFC9110, Section 7.6.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.2)

See "Network.HTTP.Headers.MaxForwards" for the typed codec.
-}
hMaxForwards :: HeaderFieldName
hMaxForwards = "Max-Forwards"


{- | @Memento-Datetime@ HTTP Header
Permanent: [RFC 7089: HTTP Framework for Time-Based Access to Resource States -- Memento](https://datatracker.ietf.org/doc/html/rfc7089)

See "Network.HTTP.Headers.MementoDatetime" for the typed codec.
-}
hMementoDatetime :: HeaderFieldName
hMementoDatetime = "Memento-Datetime"


{- | @Meter@ HTTP Header
Permanent: [RFC 2227: Simple Hit-Metering and Usage-Limiting for HTTP](https://datatracker.ietf.org/doc/html/rfc2227)

See "Network.HTTP.Headers.Meter" for the typed codec.
-}
hMeter :: HeaderFieldName
hMeter = "Meter"


{- | @Method-Check@ HTTP Header
Obsoleted: [Access Control for Cross-site Requests](https://www.w3.org/TR/cors/)

See "Network.HTTP.Headers.MethodCheck" for the typed codec.
-}
hMethodCheck :: HeaderFieldName
hMethodCheck = "Method-Check"


{- | @Method-Check-Expires@ HTTP Header
Obsoleted: [Access Control for Cross-site Requests](https://www.w3.org/TR/cors/)

See "Network.HTTP.Headers.MethodCheckExpires" for the typed codec.
-}
hMethodCheckExpires :: HeaderFieldName
hMethodCheckExpires = "Method-Check-Expires"


{- | @MIME-Version@ HTTP Header
Permanent: [RFC9112, Appendix B.1: HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc9112#appendix-B.1)

See "Network.HTTP.Headers.MIMEVersion" for the typed codec.
-}
hMIMEVersion :: HeaderFieldName
hMIMEVersion = "MIME-Version"


{- | @Negotiate@ HTTP Header
Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)

See "Network.HTTP.Headers.Negotiate" for the typed codec.
-}
hNegotiate :: HeaderFieldName
hNegotiate = "Negotiate"


{- | @NEL@ HTTP Header
Permanent: [Network Error Logging](https://www.w3.org/TR/network-error-logging/)

See "Network.HTTP.Headers.NEL" for the typed codec.
-}
hNEL :: HeaderFieldName
hNEL = "NEL"


{- | @OData-EntityId@ HTTP Header
Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)

See "Network.HTTP.Headers.ODataEntityId" for the typed codec.
-}
hODataEntityId :: HeaderFieldName
hODataEntityId = "OData-EntityId"


{- | @OData-Isolation@ HTTP Header
Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)

See "Network.HTTP.Headers.ODataIsolation" for the typed codec.
-}
hODataIsolation :: HeaderFieldName
hODataIsolation = "OData-Isolation"


{- | @OData-MaxVersion@ HTTP Header
Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)

See "Network.HTTP.Headers.ODataMaxVersion" for the typed codec.
-}
hODataMaxVersion :: HeaderFieldName
hODataMaxVersion = "OData-MaxVersion"


{- | @OData-Version@ HTTP Header
Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)

See "Network.HTTP.Headers.ODataVersion" for the typed codec.
-}
hODataVersion :: HeaderFieldName
hODataVersion = "OData-Version"


{- | @Opt@ HTTP Header
Obsoleted: [RFC 2774: An HTTP Extension Framework](https://datatracker.ietf.org/doc/html/rfc2774)[status-change-http-experiments-to-historic](https://www.ietf.org/)

See "Network.HTTP.Headers.Opt" for the typed codec.
-}
hOpt :: HeaderFieldName
hOpt = "Opt"


{- | @Optional-WWW-Authenticate@ HTTP Header
Permanent: [RFC 8053, Section 3: HTTP Authentication Extensions for Interactive Clients](https://datatracker.ietf.org/doc/html/rfc8053#section-3)

See "Network.HTTP.Headers.OptionalWWWAuthenticate" for the typed codec.
-}
hOptionalWWWAuthenticate :: HeaderFieldName
hOptionalWWWAuthenticate = "Optional-WWW-Authenticate"


{- | @Ordering-Type@ HTTP Header
Permanent: [RFC 3648: Web Distributed Authoring and Versioning (WebDAV) Ordered Collections Protocol](https://datatracker.ietf.org/doc/html/rfc3648)

See "Network.HTTP.Headers.OrderingType" for the typed codec.
-}
hOrderingType :: HeaderFieldName
hOrderingType = "Ordering-Type"


{- | @Origin@ HTTP Header
Permanent: [RFC 6454: The Web Origin Concept](https://datatracker.ietf.org/doc/html/rfc6454)

See "Network.HTTP.Headers.Origin" for the typed codec.
-}
hOrigin :: HeaderFieldName
hOrigin = "Origin"


{- | @Origin-Agent-Cluster@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.OriginAgentCluster" for the typed codec.
-}
hOriginAgentCluster :: HeaderFieldName
hOriginAgentCluster = "Origin-Agent-Cluster"


{- | @OSCORE@ HTTP Header
Permanent: [RFC 8613, Section 11.1: Object Security for Constrained RESTful Environments (OSCORE)](https://datatracker.ietf.org/doc/html/rfc8613#section-11.1)

See "Network.HTTP.Headers.OSCORE" for the typed codec.
-}
hOSCORE :: HeaderFieldName
hOSCORE = "OSCORE"


{- | @OSLC-Core-Version@ HTTP Header
Permanent: [OASIS Project Specification 01](https://www.oasis-open.org/)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)

See "Network.HTTP.Headers.OSLCCoreVersion" for the typed codec.
-}
hOSLCCoreVersion :: HeaderFieldName
hOSLCCoreVersion = "OSLC-Core-Version"


{- | @Overwrite@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.Overwrite" for the typed codec.
-}
hOverwrite :: HeaderFieldName
hOverwrite = "Overwrite"


{- | @P3P@ HTTP Header
Obsoleted: [The Platform for Privacy Preferences 1.0 (P3P1.0) Specification](https://www.w3.org/TR/P3P/)

See "Network.HTTP.Headers.P3P" for the typed codec.
-}
hP3P :: HeaderFieldName
hP3P = "P3P"


{- | @PEP@ HTTP Header
Obsoleted: [PEP - an Extension Mechanism for HTTP](https://www.w3.org/TR/pep/)

See "Network.HTTP.Headers.PEP" for the typed codec.
-}
hPEP :: HeaderFieldName
hPEP = "PEP"


{- | @PEP-Info@ HTTP Header
Obsoleted: [PEP - an Extension Mechanism for HTTP](https://www.w3.org/TR/pep/)

See "Network.HTTP.Headers.PEPInfo" for the typed codec.
-}
hPEPInfo :: HeaderFieldName
hPEPInfo = "PEP-Info"


{- | @Permissions-Policy@ HTTP Header
Provisional: [Permissions Policy](https://www.w3.org/TR/permissions-policy-1/)

See "Network.HTTP.Headers.PermissionsPolicy" for the typed codec.
-}
hPermissionsPolicy :: HeaderFieldName
hPermissionsPolicy = "Permissions-Policy"


{- | @PICS-Label@ HTTP Header
Obsoleted: [PICS Label Distribution Label Syntax and Communication Protocols](https://www.w3.org/TR/PICS-labels/)

See "Network.HTTP.Headers.PICSLabel" for the typed codec.
-}
hPICSLabel :: HeaderFieldName
hPICSLabel = "PICS-Label"


{- | @Ping-From@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.PingFrom" for the typed codec.
-}
hPingFrom :: HeaderFieldName
hPingFrom = "Ping-From"


{- | @Ping-To@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.PingTo" for the typed codec.
-}
hPingTo :: HeaderFieldName
hPingTo = "Ping-To"


{- | @Position@ HTTP Header
Permanent: [RFC 3648: Web Distributed Authoring and Versioning (WebDAV) Ordered Collections Protocol](https://datatracker.ietf.org/doc/html/rfc3648)

See "Network.HTTP.Headers.Position" for the typed codec.
-}
hPosition :: HeaderFieldName
hPosition = "Position"


{- | @Pragma@ HTTP Header
Deprecated: [RFC9111, Section 5.4: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.4)

See "Network.HTTP.Headers.Pragma" for the typed codec.
-}
hPragma :: HeaderFieldName
hPragma = "Pragma"


{- | @Prefer@ HTTP Header
Permanent: [RFC 7240: Prefer Header for HTTP](https://datatracker.ietf.org/doc/html/rfc7240)

See "Network.HTTP.Headers.Prefer" for the typed codec.
-}
hPrefer :: HeaderFieldName
hPrefer = "Prefer"


{- | @Preference-Applied@ HTTP Header
Permanent: [RFC 7240: Prefer Header for HTTP](https://datatracker.ietf.org/doc/html/rfc7240)

See "Network.HTTP.Headers.PreferenceApplied" for the typed codec.
-}
hPreferenceApplied :: HeaderFieldName
hPreferenceApplied = "Preference-Applied"


{- | @Priority@ HTTP Header
Permanent: [RFC9218: Extensible Prioritization Scheme for HTTP](https://datatracker.ietf.org/doc/html/rfc9218)

See "Network.HTTP.Headers.Priority" for the typed codec.
-}
hPriority :: HeaderFieldName
hPriority = "Priority"


{- | @ProfileObject@ HTTP Header
Obsoleted: [Implementation of OPS Over HTTP](https://www.w3.org/TR/OPS-Implementation/)

See "Network.HTTP.Headers.ProfileObject" for the typed codec.
-}
hProfileObject :: HeaderFieldName
hProfileObject = "ProfileObject"


{- | @Protocol@ HTTP Header
Obsoleted: [PICS Label Distribution Label Syntax and Communication Protocols](https://www.w3.org/TR/PICS-labels/)

See "Network.HTTP.Headers.Protocol" for the typed codec.
-}
hProtocol :: HeaderFieldName
hProtocol = "Protocol"


{- | @Protocol-Info@ HTTP Header
Deprecated: [White Paper: Joint Electronic Payment Initiative](https://www.electronic-payments-initiative.org/)

See "Network.HTTP.Headers.ProtocolInfo" for the typed codec.
-}
hProtocolInfo :: HeaderFieldName
hProtocolInfo = "Protocol-Info"


{- | @Protocol-Query@ HTTP Header
Deprecated: [White Paper: Joint Electronic Payment Initiative](https://www.electronic-payments-initiative.org/)

See "Network.HTTP.Headers.ProtocolQuery" for the typed codec.
-}
hProtocolQuery :: HeaderFieldName
hProtocolQuery = "Protocol-Query"


{- | @Protocol-Request@ HTTP Header
Obsoleted: [PICS Label Distribution Label Syntax and Communication Protocols](https://www.w3.org/TR/PICS-labels/)

See "Network.HTTP.Headers.ProtocolRequest" for the typed codec.
-}
hProtocolRequest :: HeaderFieldName
hProtocolRequest = "Protocol-Request"


{- | @Proxy-Authenticate@ HTTP Header
Permanent: [RFC9110, Section 11.7.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.7.1)

See "Network.HTTP.Headers.ProxyAuthenticate" for the typed codec.
-}
hProxyAuthenticate :: HeaderFieldName
hProxyAuthenticate = "Proxy-Authenticate"


{- | @Proxy-Authentication-Info@ HTTP Header
Permanent: [RFC9110, Section 11.7.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.7.3)

See "Network.HTTP.Headers.ProxyAuthenticationInfo" for the typed codec.
-}
hProxyAuthenticationInfo :: HeaderFieldName
hProxyAuthenticationInfo = "Proxy-Authentication-Info"


{- | @Proxy-Authorization@ HTTP Header
Permanent: [RFC9110, Section 11.7.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.7.2)

See "Network.HTTP.Headers.ProxyAuthorization" for the typed codec.
-}
hProxyAuthorization :: HeaderFieldName
hProxyAuthorization = "Proxy-Authorization"


{- | @Proxy-Features@ HTTP Header
Obsoleted: [Notification for Proxy Caches](https://www.w3.org/TR/proxy-features/)

See "Network.HTTP.Headers.ProxyFeatures" for the typed codec.
-}
hProxyFeatures :: HeaderFieldName
hProxyFeatures = "Proxy-Features"


{- | @Proxy-Instruction@ HTTP Header
Obsoleted: [Notification for Proxy Caches](https://www.w3.org/TR/proxy-features/)

See "Network.HTTP.Headers.ProxyInstruction" for the typed codec.
-}
hProxyInstruction :: HeaderFieldName
hProxyInstruction = "Proxy-Instruction"


{- | @Proxy-Status@ HTTP Header
Permanent: [RFC9209: The Proxy-Status HTTP Response Header Field](https://datatracker.ietf.org/doc/html/rfc9209)

See "Network.HTTP.Headers.ProxyStatus" for the typed codec.
-}
hProxyStatus :: HeaderFieldName
hProxyStatus = "Proxy-Status"


{- | @Public@ HTTP Header
Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)

See "Network.HTTP.Headers.PublicHeader" for the typed codec.
-}
hPublicHeader :: HeaderFieldName
hPublicHeader = "Public"


{- | @Public-Key-Pins@ HTTP Header
Permanent: [RFC 7469: Public Key Pinning Extension for HTTP](https://datatracker.ietf.org/doc/html/rfc7469)

See "Network.HTTP.Headers.PublicKeyPins" for the typed codec.
-}
hPublicKeyPins :: HeaderFieldName
hPublicKeyPins = "Public-Key-Pins"


{- | @Public-Key-Pins-Report-Only@ HTTP Header
Permanent: [RFC 7469: Public Key Pinning Extension for HTTP](https://datatracker.ietf.org/doc/html/rfc7469)

See "Network.HTTP.Headers.PublicKeyPinsReportOnly" for the typed codec.
-}
hPublicKeyPinsReportOnly :: HeaderFieldName
hPublicKeyPinsReportOnly = "Public-Key-Pins-Report-Only"


{- | @Range@ HTTP Header
Permanent: [RFC9110, Section 14.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-14.2)

See "Network.HTTP.Headers.Range" for the typed codec.
-}
hRange :: HeaderFieldName
hRange = "Range"


{- | @Redirect-Ref@ HTTP Header
Permanent: [RFC 4437: Web Distributed Authoring and Versioning (WebDAV) Redirect Reference Resources](https://datatracker.ietf.org/doc/html/rfc4437)

See "Network.HTTP.Headers.RedirectRef" for the typed codec.
-}
hRedirectRef :: HeaderFieldName
hRedirectRef = "Redirect-Ref"


{- | @Referer@ HTTP Header
Permanent: [RFC9110, Section 10.1.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.3)

See "Network.HTTP.Headers.Referer" for the typed codec.
-}
hReferer :: HeaderFieldName
hReferer = "Referer"


{- | @Referer-Root@ HTTP Header
Obsoleted: [Access Control for Cross-site Requests](https://www.w3.org/TR/cors/)

See "Network.HTTP.Headers.RefererRoot" for the typed codec.
-}
hRefererRoot :: HeaderFieldName
hRefererRoot = "Referer-Root"


{- | @Refresh@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.Refresh" for the typed codec.
-}
hRefresh :: HeaderFieldName
hRefresh = "Refresh"


{- | @Repeatability-Client-ID@ HTTP Header
Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityClientID)

See "Network.HTTP.Headers.RepeatabilityClientID" for the typed codec.
-}
hRepeatabilityClientID :: HeaderFieldName
hRepeatabilityClientID = "Repeatability-Client-ID"


{- | @Repeatability-First-Sent@ HTTP Header
Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityFirstSent)

See "Network.HTTP.Headers.RepeatabilityFirstSent" for the typed codec.
-}
hRepeatabilityFirstSent :: HeaderFieldName
hRepeatabilityFirstSent = "Repeatability-First-Sent"


{- | @Repeatability-Request-ID@ HTTP Header
Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityRequestID)

See "Network.HTTP.Headers.RepeatabilityRequestID" for the typed codec.
-}
hRepeatabilityRequestID :: HeaderFieldName
hRepeatabilityRequestID = "Repeatability-Request-ID"


{- | @Repeatability-Result@ HTTP Header
Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityResult)

See "Network.HTTP.Headers.RepeatabilityResult" for the typed codec.
-}
hRepeatabilityResult :: HeaderFieldName
hRepeatabilityResult = "Repeatability-Result"


{- | @Replay-Nonce@ HTTP Header
Permanent: [RFC 8555, Section 6.5.1: Automatic Certificate Management Environment (ACME)](https://datatracker.ietf.org/doc/html/rfc8555#section-6.5.1)

See "Network.HTTP.Headers.ReplayNonce" for the typed codec.
-}
hReplayNonce :: HeaderFieldName
hReplayNonce = "Replay-Nonce"


{- | @Reporting-Endpoints@ HTTP Header
Provisional: [Reporting API](https://www.w3.org/TR/reporting/)

See "Network.HTTP.Headers.ReportingEndpoints" for the typed codec.
-}
hReportingEndpoints :: HeaderFieldName
hReportingEndpoints = "Reporting-Endpoints"


{- | @Repr-Digest@ HTTP Header
Permanent: [RFC 9530, Section 3: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-3)

See "Network.HTTP.Headers.ReprDigest" for the typed codec.
-}
hReprDigest :: HeaderFieldName
hReprDigest = "Repr-Digest"


{- | @Retry-After@ HTTP Header
Permanent: [RFC9110, Section 10.2.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.3)

See "Network.HTTP.Headers.RetryAfter" for the typed codec.
-}
hRetryAfter :: HeaderFieldName
hRetryAfter = "Retry-After"


{- | @Safe@ HTTP Header
Obsoleted: [RFC 2310: The Safe Response Header Field](https://datatracker.ietf.org/doc/html/rfc2310)[status-change-http-experiments-to-historic](https://www.ietf.org/)

See "Network.HTTP.Headers.Safe" for the typed codec.
-}
hSafe :: HeaderFieldName
hSafe = "Safe"


{- | @Schedule-Reply@ HTTP Header
Permanent: [RFC 6638: Scheduling Extensions to CalDAV](https://datatracker.ietf.org/doc/html/rfc6638)

See "Network.HTTP.Headers.ScheduleReply" for the typed codec.
-}
hScheduleReply :: HeaderFieldName
hScheduleReply = "Schedule-Reply"


{- | @Schedule-Tag@ HTTP Header
Permanent: [RFC 6338: Scheduling Extensions to CalDAV](https://datatracker.ietf.org/doc/html/rfc6338)

See "Network.HTTP.Headers.ScheduleTag" for the typed codec.
-}
hScheduleTag :: HeaderFieldName
hScheduleTag = "Schedule-Tag"


{- | @Sec-GPC@ HTTP Header
Provisional: [Global Privacy Control (GPC)](https://globalprivacycontrol.org/)

See "Network.HTTP.Headers.SecGPC" for the typed codec.
-}
hSecGPC :: HeaderFieldName
hSecGPC = "Sec-GPC"


{- | @Sec-Purpose@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/) Intended to replace the (not registered) Purpose and x-moz headers.

See "Network.HTTP.Headers.SecPurpose" for the typed codec.
-}
hSecPurpose :: HeaderFieldName
hSecPurpose = "Sec-Purpose"


{- | @Sec-Token-Binding@ HTTP Header
Permanent: [RFC 8473: Token Binding over HTTP](https://datatracker.ietf.org/doc/html/rfc8473)

See "Network.HTTP.Headers.SecTokenBinding" for the typed codec.
-}
hSecTokenBinding :: HeaderFieldName
hSecTokenBinding = "Sec-Token-Binding"


{- | @Sec-WebSocket-Accept@ HTTP Header
Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)

See "Network.HTTP.Headers.SecWebSocketAccept" for the typed codec.
-}
hSecWebSocketAccept :: HeaderFieldName
hSecWebSocketAccept = "Sec-WebSocket-Accept"


{- | @Sec-WebSocket-Extensions@ HTTP Header
Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)

See "Network.HTTP.Headers.SecWebSocketExtensions" for the typed codec.
-}
hSecWebSocketExtensions :: HeaderFieldName
hSecWebSocketExtensions = "Sec-WebSocket-Extensions"


{- | @Sec-WebSocket-Key@ HTTP Header
Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)

See "Network.HTTP.Headers.SecWebSocketKey" for the typed codec.
-}
hSecWebSocketKey :: HeaderFieldName
hSecWebSocketKey = "Sec-WebSocket-Key"


{- | @Sec-WebSocket-Protocol@ HTTP Header
Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)

See "Network.HTTP.Headers.SecWebSocketProtocol" for the typed codec.
-}
hSecWebSocketProtocol :: HeaderFieldName
hSecWebSocketProtocol = "Sec-WebSocket-Protocol"


{- | @Sec-WebSocket-Version@ HTTP Header
Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)

See "Network.HTTP.Headers.SecWebSocketVersion" for the typed codec.
-}
hSecWebSocketVersion :: HeaderFieldName
hSecWebSocketVersion = "Sec-WebSocket-Version"


{- | @Security-Scheme@ HTTP Header
Obsoleted: [RFC 2660: The Secure HyperText Transfer Protocol](https://datatracker.ietf.org/doc/html/rfc2660)[status-change-http-experiments-to-historic](https://www.ietf.org/)

See "Network.HTTP.Headers.SecurityScheme" for the typed codec.
-}
hSecurityScheme :: HeaderFieldName
hSecurityScheme = "Security-Scheme"


{- | @Server@ HTTP Header
Permanent: [RFC9110, Section 10.2.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.4)

See "Network.HTTP.Headers.Server" for the typed codec.
-}
hServer :: HeaderFieldName
hServer = "Server"


{- | @Server-Timing@ HTTP Header
Permanent: [Server Timing](https://www.w3.org/TR/server-timing/)

See "Network.HTTP.Headers.ServerTiming" for the typed codec.
-}
hServerTiming :: HeaderFieldName
hServerTiming = "Server-Timing"


{- | @Set-Cookie@ HTTP Header
Permanent: [RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)

See "Network.HTTP.Headers.SetCookie" for the typed codec.
-}
hSetCookie :: HeaderFieldName
hSetCookie = "Set-Cookie"


{- | @Set-Cookie2@ HTTP Header
Obsoleted: [RFC 2965: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc2965)[RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)

See "Network.HTTP.Headers.SetCookie2" for the typed codec.
-}
hSetCookie2 :: HeaderFieldName
hSetCookie2 = "Set-Cookie2"


{- | @SetProfile@ HTTP Header
Obsoleted: [Implementation of OPS Over HTTP](https://www.w3.org/TR/OPS-Implementation/)

See "Network.HTTP.Headers.SetProfile" for the typed codec.
-}
hSetProfile :: HeaderFieldName
hSetProfile = "SetProfile"


{- | @Signature@ HTTP Header
Permanent: [RFC 9421, Section 4.2: HTTP Message Signatures](https://datatracker.ietf.org/doc/html/rfc9421#section-4.2)

See "Network.HTTP.Headers.Signature" for the typed codec.
-}
hSignature :: HeaderFieldName
hSignature = "Signature"


{- | @Signature-Input@ HTTP Header
Permanent: [RFC 9421, Section 4.1: HTTP Message Signatures](https://datatracker.ietf.org/doc/html/rfc9421#section-4.1)

See "Network.HTTP.Headers.SignatureInput" for the typed codec.
-}
hSignatureInput :: HeaderFieldName
hSignatureInput = "Signature-Input"


{- | @SLUG@ HTTP Header
Permanent: [RFC 5023: The Atom Publishing Protocol](https://datatracker.ietf.org/doc/html/rfc5023)

See "Network.HTTP.Headers.Slug" for the typed codec.
-}
hSlug :: HeaderFieldName
hSlug = "SLUG"


{- | @SoapAction@ HTTP Header
Permanent: [Simple Object Access Protocol (SOAP) 1.1](https://www.w3.org/TR/soap/)

See "Network.HTTP.Headers.SoapAction" for the typed codec.
-}
hSoapAction :: HeaderFieldName
hSoapAction = "SoapAction"


{- | @Status-URI@ HTTP Header
Permanent: [RFC 2518: HTTP Extensions for Distributed Authoring -- WEBDAV](https://datatracker.ietf.org/doc/html/rfc2518)

See "Network.HTTP.Headers.StatusURI" for the typed codec.
-}
hStatusURI :: HeaderFieldName
hStatusURI = "Status-URI"


{- | @Strict-Transport-Security@ HTTP Header
Permanent: [RFC 6797: HTTP Strict Transport Security (HSTS)](https://datatracker.ietf.org/doc/html/rfc6797)

See "Network.HTTP.Headers.StrictTransportSecurity" for the typed codec.
-}
hStrictTransportSecurity :: HeaderFieldName
hStrictTransportSecurity = "Strict-Transport-Security"


{- | @Sunset@ HTTP Header
Permanent: [RFC 8594: The Sunset HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc8594)

See "Network.HTTP.Headers.Sunset" for the typed codec.
-}
hSunset :: HeaderFieldName
hSunset = "Sunset"


{- | @Surrogate-Capability@ HTTP Header
Provisional: [Edge Architecture Specification](https://www.edge-spec.org/)

See "Network.HTTP.Headers.SurrogateCapability" for the typed codec.
-}
hSurrogateCapability :: HeaderFieldName
hSurrogateCapability = "Surrogate-Capability"


{- | @Surrogate-Control@ HTTP Header
Provisional: [Edge Architecture Specification](https://www.edge-spec.org/)

See "Network.HTTP.Headers.SurrogateControl" for the typed codec.
-}
hSurrogateControl :: HeaderFieldName
hSurrogateControl = "Surrogate-Control"


{- | @TCN@ HTTP Header
Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)

See "Network.HTTP.Headers.TCN" for the typed codec.
-}
hTCN :: HeaderFieldName
hTCN = "TCN"


{- | @TE@ HTTP Header
Permanent: [RFC9110, Section 10.1.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.4)

See "Network.HTTP.Headers.TE" for the typed codec.
-}
hTE :: HeaderFieldName
hTE = "TE"


{- | @Timeout@ HTTP Header
Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)

See "Network.HTTP.Headers.Timeout" for the typed codec.
-}
hTimeout :: HeaderFieldName
hTimeout = "Timeout"


{- | @Timing-Allow-Origin@ HTTP Header
Provisional: [Resource Timing Level 1](https://www.w3.org/TR/resource-timing-1/)

See "Network.HTTP.Headers.TimingAllowOrigin" for the typed codec.
-}
hTimingAllowOrigin :: HeaderFieldName
hTimingAllowOrigin = "Timing-Allow-Origin"


{- | @Topic@ HTTP Header
Permanent: [RFC 8030, Section 5.4: Generic Event Delivery Using HTTP Push](https://datatracker.ietf.org/doc/html/rfc8030#section-5.4)

See "Network.HTTP.Headers.Topic" for the typed codec.
-}
hTopic :: HeaderFieldName
hTopic = "Topic"


{- | @Traceparent@ HTTP Header
Permanent: [Trace Context](https://www.w3.org/TR/trace-context/)

See "Network.HTTP.Headers.Traceparent" for the typed codec.
-}
hTraceparent :: HeaderFieldName
hTraceparent = "Traceparent"


{- | @Tracestate@ HTTP Header
Permanent: [Trace Context](https://www.w3.org/TR/trace-context/)

See "Network.HTTP.Headers.Tracestate" for the typed codec.
-}
hTracestate :: HeaderFieldName
hTracestate = "Tracestate"


{- | @Trailer@ HTTP Header
Permanent: [RFC9110, Section 6.6.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-6.6.2)

See "Network.HTTP.Headers.Trailer" for the typed codec.
-}
hTrailer :: HeaderFieldName
hTrailer = "Trailer"


{- | @Transfer-Encoding@ HTTP Header
Permanent: [RFC9112, Section 6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9112#section-6.1)

See "Network.HTTP.Headers.TransferEncoding" for the typed codec.
-}
hTransferEncoding :: HeaderFieldName
hTransferEncoding = "Transfer-Encoding"


{- | @TTL@ HTTP Header
Permanent: [RFC 8030, Section 5.2: Generic Event Delivery Using HTTP Push](https://datatracker.ietf.org/doc/html/rfc8030#section-5.2)

See "Network.HTTP.Headers.TTL" for the typed codec.
-}
hTTL :: HeaderFieldName
hTTL = "TTL"


{- | @Upgrade@ HTTP Header
Permanent: [RFC9110, Section 7.8: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.8)

See "Network.HTTP.Headers.Upgrade" for the typed codec.
-}
hUpgrade :: HeaderFieldName
hUpgrade = "Upgrade"


{- | @Urgency@ HTTP Header
Permanent: [RFC 8030, Section 5.3: Generic Event Delivery Using HTTP Push](https://datatracker.ietf.org/doc/html/rfc8030#section-5.3)

See "Network.HTTP.Headers.Urgency" for the typed codec.
-}
hUrgency :: HeaderFieldName
hUrgency = "Urgency"


{- | @URI@ HTTP Header
Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)

See "Network.HTTP.Headers.URI" for the typed codec.
-}
hURI :: HeaderFieldName
hURI = "URI"


{- | @User-Agent@ HTTP Header
Permanent: [RFC9110, Section 10.1.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.5)

See "Network.HTTP.Headers.UserAgent" for the typed codec.
-}
hUserAgent :: HeaderFieldName
hUserAgent = "User-Agent"


{- | @Variant-Vary@ HTTP Header
Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)

See "Network.HTTP.Headers.VariantVary" for the typed codec.
-}
hVariantVary :: HeaderFieldName
hVariantVary = "Variant-Vary"


{- | @Vary@ HTTP Header
Permanent: [RFC9110, Section 12.5.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.5)

See "Network.HTTP.Headers.Vary" for the typed codec.
-}
hVary :: HeaderFieldName
hVary = "Vary"


{- | @Via@ HTTP Header
Permanent: [RFC9110, Section 7.6.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.3)

See "Network.HTTP.Headers.Via" for the typed codec.
-}
hVia :: HeaderFieldName
hVia = "Via"


{- | @Want-Content-Digest@ HTTP Header
Permanent: [RFC 9530, Section 4: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-4)

See "Network.HTTP.Headers.WantContentDigest" for the typed codec.
-}
hWantContentDigest :: HeaderFieldName
hWantContentDigest = "Want-Content-Digest"


{- | @Want-Digest@ HTTP Header
Obsoleted: [RFC 3230: Instance Digests in HTTP](https://datatracker.ietf.org/doc/html/rfc3230)[RFC 9530, Section 1.3: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-1.3)

See "Network.HTTP.Headers.WantDigest" for the typed codec.
-}
hWantDigest :: HeaderFieldName
hWantDigest = "Want-Digest"


{- | @Want-Repr-Digest@ HTTP Header
Permanent: [RFC 9530, Section 4: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-4)

See "Network.HTTP.Headers.WantReprDigest" for the typed codec.
-}
hWantReprDigest :: HeaderFieldName
hWantReprDigest = "Want-Repr-Digest"


{- | @Warning@ HTTP Header
Obsoleted: [RFC9111, Section 5.5: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.5)

See "Network.HTTP.Headers.Warning" for the typed codec.
-}
hWarning :: HeaderFieldName
hWarning = "Warning"


{- | @WWW-Authenticate@ HTTP Header
Permanent: [RFC9110, Section 11.6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.6.1)

See "Network.HTTP.Headers.WWWAuthenticate" for the typed codec.
-}
hWWWAuthenticate :: HeaderFieldName
hWWWAuthenticate = "WWW-Authenticate"


{- | @X-Content-Type-Options@ HTTP Header
Permanent: [Fetch](https://fetch.spec.whatwg.org/)

See "Network.HTTP.Headers.XContentTypeOptions" for the typed codec.
-}
hXContentTypeOptions :: HeaderFieldName
hXContentTypeOptions = "X-Content-Type-Options"


{- | @X-Frame-Options@ HTTP Header
Permanent: [HTML](https://html.spec.whatwg.org/)

See "Network.HTTP.Headers.XFrameOptions" for the typed codec.
-}
hXFrameOptions :: HeaderFieldName
hXFrameOptions = "X-Frame-Options"


-- De facto (non-IANA-registered) header field names

{- | @X-Forwarded-For@ HTTP Header
De facto (non-IANA-registered, widely deployed): client IP through proxies (Squid/Mozilla); see RFC 7239 Forwarded for the standard form.

See "Network.HTTP.Headers.XForwardedFor" for the typed codec.
-}
hXForwardedFor :: HeaderFieldName
hXForwardedFor = "X-Forwarded-For"


{- | @X-Forwarded-Host@ HTTP Header
De facto (non-IANA-registered, widely deployed): original Host requested by the client (reverse proxies).

See "Network.HTTP.Headers.XForwardedHost" for the typed codec.
-}
hXForwardedHost :: HeaderFieldName
hXForwardedHost = "X-Forwarded-Host"


{- | @X-Forwarded-Proto@ HTTP Header
De facto (non-IANA-registered, widely deployed): originating protocol (http/https) of the client request (reverse proxies).

See "Network.HTTP.Headers.XForwardedProto" for the typed codec.
-}
hXForwardedProto :: HeaderFieldName
hXForwardedProto = "X-Forwarded-Proto"


{- | @X-Forwarded-Port@ HTTP Header
De facto (non-IANA-registered, widely deployed): originating port of the client request (reverse proxies).

See "Network.HTTP.Headers.XForwardedPort" for the typed codec.
-}
hXForwardedPort :: HeaderFieldName
hXForwardedPort = "X-Forwarded-Port"


{- | @X-Real-IP@ HTTP Header
De facto (non-IANA-registered, widely deployed): client IP set by nginx and other reverse proxies.

See "Network.HTTP.Headers.XRealIP" for the typed codec.
-}
hXRealIP :: HeaderFieldName
hXRealIP = "X-Real-IP"


{- | @X-Http-Method-Override@ HTTP Header
De facto (non-IANA-registered, widely deployed): tunnels a real method (PUT/DELETE/PATCH) over POST.

See "Network.HTTP.Headers.XHttpMethodOverride" for the typed codec.
-}
hXHttpMethodOverride :: HeaderFieldName
hXHttpMethodOverride = "X-Http-Method-Override"


{- | @X-Request-ID@ HTTP Header
De facto (non-IANA-registered, widely deployed): per-request correlation id (Heroku, Rails, many gateways).

See "Network.HTTP.Headers.XRequestID" for the typed codec.
-}
hXRequestID :: HeaderFieldName
hXRequestID = "X-Request-ID"


{- | @X-Correlation-ID@ HTTP Header
De facto (non-IANA-registered, widely deployed): cross-service correlation id.

See "Network.HTTP.Headers.XCorrelationID" for the typed codec.
-}
hXCorrelationID :: HeaderFieldName
hXCorrelationID = "X-Correlation-ID"


{- | @X-Request-Start@ HTTP Header
De facto (non-IANA-registered, widely deployed): request receipt timestamp injected by load balancers (Heroku/NGINX).

See "Network.HTTP.Headers.XRequestStart" for the typed codec.
-}
hXRequestStart :: HeaderFieldName
hXRequestStart = "X-Request-Start"


{- | @X-Trace-ID@ HTTP Header
De facto (non-IANA-registered, widely deployed): distributed-trace id (vendor-specific).

See "Network.HTTP.Headers.XTraceID" for the typed codec.
-}
hXTraceID :: HeaderFieldName
hXTraceID = "X-Trace-ID"


{- | @X-XSS-Protection@ HTTP Header
De facto (non-IANA-registered, widely deployed): De facto (legacy): controls the browser XSS auditor; superseded by CSP.

See "Network.HTTP.Headers.XXSSProtection" for the typed codec.
-}
hXXSSProtection :: HeaderFieldName
hXXSSProtection = "X-XSS-Protection"


{- | @X-Download-Options@ HTTP Header
De facto (non-IANA-registered, widely deployed): De facto (IE): `noopen` to prevent opening downloads in the site context.

See "Network.HTTP.Headers.XDownloadOptions" for the typed codec.
-}
hXDownloadOptions :: HeaderFieldName
hXDownloadOptions = "X-Download-Options"


{- | @X-Permitted-Cross-Domain-Policies@ HTTP Header
De facto (non-IANA-registered, widely deployed): Adobe cross-domain policy control (none/master-only/by-content-type/all).

See "Network.HTTP.Headers.XPermittedCrossDomainPolicies" for the typed codec.
-}
hXPermittedCrossDomainPolicies :: HeaderFieldName
hXPermittedCrossDomainPolicies = "X-Permitted-Cross-Domain-Policies"


{- | @X-DNS-Prefetch-Control@ HTTP Header
De facto (non-IANA-registered, widely deployed): enable/disable browser DNS prefetching (on/off).

See "Network.HTTP.Headers.XDNSPrefetchControl" for the typed codec.
-}
hXDNSPrefetchControl :: HeaderFieldName
hXDNSPrefetchControl = "X-DNS-Prefetch-Control"


{- | @DNT@ HTTP Header
De facto (non-IANA-registered, widely deployed): (W3C, retired) Do Not Track signal (0/1).

See "Network.HTTP.Headers.DNT" for the typed codec.
-}
hDNT :: HeaderFieldName
hDNT = "DNT"


{- | @X-UA-Compatible@ HTTP Header
De facto (non-IANA-registered, widely deployed): De facto (IE): rendering engine selection, e.g. IE=edge.

See "Network.HTTP.Headers.XUACompatible" for the typed codec.
-}
hXUACompatible :: HeaderFieldName
hXUACompatible = "X-UA-Compatible"


{- | @X-Powered-By@ HTTP Header
De facto (non-IANA-registered, widely deployed): backend technology advertisement (often stripped for security).

See "Network.HTTP.Headers.XPoweredBy" for the typed codec.
-}
hXPoweredBy :: HeaderFieldName
hXPoweredBy = "X-Powered-By"


{- | @X-Robots-Tag@ HTTP Header
De facto (non-IANA-registered, widely deployed): per-resource robots indexing directives.

See "Network.HTTP.Headers.XRobotsTag" for the typed codec.
-}
hXRobotsTag :: HeaderFieldName
hXRobotsTag = "X-Robots-Tag"


{- | @Save-Data@ HTTP Header
De facto (non-IANA-registered, widely deployed): De facto (Network Information API): client opted into data savings (on).

See "Network.HTTP.Headers.SaveData" for the typed codec.
-}
hSaveData :: HeaderFieldName
hSaveData = "Save-Data"


{- | @*@ HTTP Header
Permanent: [RFC9110, Section 12.5.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.5)
-}
hWildcardHeader :: HeaderFieldName
hWildcardHeader = "*"
