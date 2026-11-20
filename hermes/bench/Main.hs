{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.CaseInsensitive (CI)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Short as ST
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime, toModifiedJulianDay)
import Data.Word (Word64)
import Network.HTTP.ContentCoding (ContentCoding (..), contentCodingParser)
import qualified Network.HTTP.Headers.AcceptEncoding as AcceptEncoding
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType (..), WeightedMediaRange (..))
import qualified Network.HTTP.Grammar.ExtensionDeclaration as Extension
import qualified Network.HTTP.Grammar.LegacyProtocol as Legacy
import qualified Network.HTTP.Grammar.PEP as PepGrammar
import qualified Network.HTTP.Grammar.PICS as Pics
import Network.HTTP.Headers
import qualified Network.HTTP.Headers.AltSvc as AltSvc
import qualified Network.HTTP.Headers.XRobotsTag as Robots
import qualified Network.HTTP.Headers.CDNLoop as CDNLoop
import qualified Network.HTTP.Headers.ServerTiming as ServerTiming
import qualified Network.HTTP.Headers.XUACompatible as UA
import qualified Network.HTTP.Headers.From as From
import qualified Network.HTTP.Headers.XHttpMethodOverride as MethodOverride
import qualified Network.HTTP.Methods as Methods
import qualified Network.Mailbox as Mailbox
import qualified Network.HTTP.Headers.Link as Link
import qualified Network.HTTP.Headers.Prefer as Prefer
import qualified Network.HTTP.Headers.PreferenceApplied as Applied
import qualified Network.HTTP.Headers.Expect as Expect
import qualified Network.HTTP.Headers.AIM as AIM
import qualified Network.HTTP.Headers.ALPN as ALPN
import qualified Network.HTTP.Headers.AMPCacheTransform as AMP
import Network.HTTP.Headers.Accept (Accept (..))
import qualified Network.HTTP.Headers.AcceptCH as AcceptCH
import qualified Network.HTTP.Headers.AcceptAdditions as Additions
import qualified Network.HTTP.Headers.AcceptFeatures as AcceptFeatures
import Network.HTTP.Headers.AcceptLanguage (AcceptLanguage (..), WeightedLanguage (..))
import qualified Network.HTTP.Headers.AcceptSignature as AcceptSignature
import qualified Network.HTTP.Headers.AccessControlAllowCredentials as CorsCredentials
import qualified Network.HTTP.Headers.AccessControlAllowHeaders as CorsAllowHeaders
import qualified Network.HTTP.Headers.AccessControlMaxAge as CorsMaxAge
import qualified Network.HTTP.Headers.AccessControlRequestMethod as CorsRequestMethod
import Network.HTTP.Headers.Age (Age (..))
import Network.HTTP.Headers.Allow (Allow (..))
import qualified Network.HTTP.Headers.Alternates as Alternates
import qualified Network.HTTP.Headers.ApplyToRedirectRef as ApplyToRedirectRef
import qualified Network.HTTP.Headers.AuthenticationControl as AuthControl
import qualified Network.HTTP.Headers.Authorization as Auth
import qualified Network.HTTP.Headers.CDNCacheControl as CDN
import Network.HTTP.Headers.CacheControl (CacheControl (..), CacheControlDirective (..), parseCacheControlHeader)
import qualified Network.HTTP.Headers.CacheStatus as CacheStatus
import qualified Network.HTTP.Headers.CalDAVTimezones as CalDAVTimezones
import qualified Network.HTTP.Headers.CalManagedID as CalManagedID
import qualified Network.HTTP.Headers.CapsuleProtocol as Capsule
import qualified Network.HTTP.Headers.CertNotAfter as CertNotAfter
import qualified Network.HTTP.Headers.ClearSiteData as CSD
import qualified Network.HTTP.Headers.ClientCert as ClientCert
import qualified Network.HTTP.Headers.ClientCertChain as ClientCertChain
import qualified Network.HTTP.Headers.ConfigurationContext as ConfigurationContext
import Network.HTTP.Headers.Connection (Connection (..))
import qualified Network.HTTP.Headers.ContentBase as ContentBase
import qualified Network.HTTP.Headers.ContentDigest as Digest
import qualified Network.HTTP.Headers.Digest as LegacyDigest
import qualified Network.HTTP.Headers.WantDigest as WantDigest
import Network.HTTP.Headers.ContentDisposition (ContentDisposition (..), contentDispositionFilename)
import Network.HTTP.Headers.ContentEncoding (ContentEncoding, contentCodings)
import qualified Network.HTTP.Headers.ContentID as ContentID
import Network.HTTP.Headers.ContentLanguage (ContentLanguage (..), LanguageTag (..), contentLanguageTagList)
import Network.HTTP.Headers.ContentLength (ContentLength (..))
import qualified Network.HTTP.Headers.ContentMD5 as MD5
import qualified Network.HTTP.Headers.ContentRange as ContentRange
import qualified Network.HTTP.Headers.ContentScriptType as ContentScriptType
import qualified Network.HTTP.Headers.ContentSecurityPolicy as CSP
import qualified Network.HTTP.Headers.ContentStyleType as ContentStyleType
import Network.HTTP.Headers.ContentType (ContentType (..), contentTypeParameters)
import qualified Network.HTTP.Headers.ContentVersion as ContentVersion
import Network.HTTP.Headers.Cookie (Cookie (..), CookiePair (..))
import qualified Network.HTTP.Headers.Cookie2 as Cookie2
import qualified Network.HTTP.Headers.CrossOriginEmbedderPolicy as COEP
import qualified Network.HTTP.Headers.CrossOriginOpenerPolicy as COOP
import qualified Network.HTTP.Headers.CrossOriginResourcePolicy as CORP
import qualified Network.HTTP.Headers.DASL as DASL
import qualified Network.HTTP.Headers.DAV as DAV
import qualified Network.HTTP.Headers.DNT as DNT
import qualified Network.HTTP.Headers.DPoP as DPoP
import qualified Network.HTTP.Headers.DPoPNonce as DPoPNonce
import Network.HTTP.Headers.Date (Date (..))
import qualified Network.HTTP.Headers.DefaultStyle as DefaultStyle
import qualified Network.HTTP.Headers.Depth as Depth
import qualified Network.HTTP.Headers.DerivedFrom as DerivedFrom
import qualified Network.HTTP.Headers.EDIINTFeatures as EDIINT
import qualified Network.HTTP.Headers.Destination as Destination
import qualified Network.HTTP.Headers.DifferentialID as DifferentialID
import qualified Network.HTTP.Headers.ETag as ETag
import qualified Network.HTTP.Headers.EarlyData as EarlyData
import qualified Network.HTTP.Headers.ExpectCT as ExpectCT
import qualified Network.HTTP.Headers.Ext as Ext
import qualified Network.HTTP.Headers.Forwarded as Forwarded
import qualified Network.HTTP.Headers.GetProfile as GetProfile
import qualified Network.HTTP.Headers.HTTP2Settings as H2Settings
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, hCacheControl)
import Network.HTTP.Headers.Host (Host (..))
import qualified Network.HTTP.Headers.IM as IM
import qualified Network.HTTP.Headers.If as DAVIf
import Network.HTTP.Headers.IfNoneMatch (IfNoneMatch (..))
import qualified Network.HTTP.Headers.KeepAlive as KeepAlive
import qualified Network.HTTP.Headers.IfScheduleTagMatch as IfScheduleTagMatch
import qualified Network.HTTP.Headers.IncludeReferredTokenBindingID as ReferredBinding
import qualified Network.HTTP.Headers.Isolation as Isolation
import qualified Network.HTTP.Headers.Label as Label
import qualified Network.HTTP.Headers.LastEventID as LastEventID
import Network.HTTP.Headers.Location (Location (..))
import qualified Network.HTTP.Headers.LockToken as LockToken
import qualified Network.HTTP.Headers.MIMEVersion as MIME
import qualified Network.HTTP.Headers.Man as Man
import qualified Network.HTTP.Headers.Mason as Mason
import qualified Network.HTTP.Headers.MaxForwards as MaxForwards
import qualified Network.HTTP.Headers.Meter as Meter
import qualified Network.HTTP.Headers.MethodCheck as MethodCheck
import qualified Network.HTTP.Headers.MethodCheckExpires as MethodCheckExpires
import qualified Network.HTTP.Headers.NEL as NEL
import qualified Network.HTTP.Headers.Negotiate as Negotiate
import qualified Network.HTTP.Headers.ODataEntityId as ODataEntityId
import qualified Network.HTTP.Headers.ODataIsolation as ODataIsolation
import qualified Network.HTTP.Headers.ODataMaxVersion as ODataMaxVersion
import qualified Network.HTTP.Headers.ODataVersion as ODataVersion
import qualified Network.HTTP.Headers.OSCORE as OSCORE
import qualified Network.HTTP.Headers.OSLCCoreVersion as OSLCCoreVersion
import qualified Network.HTTP.Headers.Opt as Opt
import qualified Network.HTTP.Headers.OrderingType as OrderingType
import Network.HTTP.Headers.Origin (Origin (..), OriginValue (..))
import qualified Network.HTTP.Headers.OriginAgentCluster as OAC
import qualified Network.HTTP.Headers.Overwrite as Overwrite
import qualified Network.HTTP.Headers.P3P as P3P
import qualified Network.HTTP.Headers.PEP as PEP
import qualified Network.HTTP.Headers.PEPInfo as PEPInfo
import qualified Network.HTTP.Headers.PICSLabel as PICSLabel
import Network.HTTP.Headers.Parsing.Util (Comment (..), ItemValue (..), RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.PermissionsPolicy as PermissionsPolicy
import Network.HTTP.Headers.PingTo (PingTo (..))
import qualified Network.HTTP.Headers.Position as Position
import qualified Network.HTTP.Headers.Priority as Priority
import qualified Network.HTTP.Headers.ProfileObject as ProfileObject
import qualified Network.HTTP.Headers.Protocol as Protocol
import qualified Network.HTTP.Headers.ProtocolInfo as ProtocolInfo
import qualified Network.HTTP.Headers.ProtocolQuery as ProtocolQuery
import qualified Network.HTTP.Headers.ProtocolRequest as ProtocolRequest
import qualified Network.HTTP.Headers.ProxyFeatures as ProxyFeatures
import qualified Network.HTTP.Headers.ProxyInstruction as ProxyInstruction
import qualified Network.HTTP.Headers.ProxyStatus as ProxyStatus
import qualified Network.HTTP.Headers.Pragma as Pragma
import qualified Network.HTTP.Headers.PublicHeader as Public
import qualified Network.HTTP.Headers.Safe as Safe
import qualified Network.HTTP.Headers.PublicKeyPins as PKP
import qualified Network.HTTP.Headers.PublicKeyPinsReportOnly as PKPRO
import qualified Network.HTTP.Headers.Range as Range
import qualified Network.HTTP.Headers.RedirectRef as RedirectRef
import Network.HTTP.Headers.Referer (Referer (..))
import qualified Network.HTTP.Headers.RefererRoot as RefererRoot
import qualified Network.HTTP.Headers.Refresh as Refresh
import qualified Network.HTTP.Headers.RepeatabilityClientID as RepeatabilityClientID
import qualified Network.HTTP.Headers.RepeatabilityFirstSent as RepeatabilityFirstSent
import qualified Network.HTTP.Headers.RepeatabilityRequestID as RepeatabilityRequestID
import qualified Network.HTTP.Headers.RepeatabilityResult as RepeatabilityResult
import qualified Network.HTTP.Headers.ReplayNonce as ReplayNonce
import qualified Network.HTTP.Headers.ReportingEndpoints as Reporting
import Network.HTTP.Headers.RetryAfter (RetryAfter (..), largeDelayDigits)
import qualified Network.HTTP.Headers.SaveData as SaveData
import qualified Network.HTTP.Headers.ScheduleReply as ScheduleReply
import qualified Network.HTTP.Headers.ScheduleTag as ScheduleTag
import qualified Network.HTTP.Headers.SecGPC as GPC
import qualified Network.HTTP.Headers.SecPurpose as SecPurpose
import qualified Network.HTTP.Headers.SecTokenBinding as TokenBinding
import qualified Network.HTTP.Headers.SecWebSocketAccept as WebSocketAccept
import qualified Network.HTTP.Headers.SecWebSocketExtensions as WebSocketExtensions
import qualified Network.HTTP.Headers.SecWebSocketKey as WebSocketKey
import qualified Network.HTTP.Headers.SecWebSocketProtocol as WebSocketProtocol
import qualified Network.HTTP.Headers.SecWebSocketVersion as WebSocketVersion
import qualified Network.HTTP.Headers.SecurityScheme as SecurityScheme
import qualified Network.HTTP.Headers.Server as Server
import Network.HTTP.Headers.SetCookie (SameSitePolicy (..), SetCookie (..), SetCookies (..), setCookieExtensionText, setCookieExtensions)
import qualified Network.HTTP.Headers.SetCookie2 as SetCookie2
import qualified Network.HTTP.Headers.SetProfile as SetProfile
import qualified Network.HTTP.Headers.Signature as Signature
import qualified Network.HTTP.Headers.SignatureInput as SignatureInput
import qualified Network.HTTP.Headers.Slug as Slug
import qualified Network.HTTP.Headers.SoapAction as SoapAction
import qualified Network.HTTP.Headers.StatusURI as StatusURI
import qualified Network.HTTP.Headers.StrictTransportSecurity as HSTS
import qualified Network.HTTP.Headers.SurrogateCapability as SurrogateCapability
import qualified Network.HTTP.Headers.SurrogateControl as SurrogateControl
import qualified Network.HTTP.Headers.TCN as TCN
import qualified Network.HTTP.Headers.TE as TE
import qualified Network.HTTP.Headers.TTL as TTL
import qualified Network.HTTP.Headers.Timeout as Timeout
import qualified Network.HTTP.Headers.TimingAllowOrigin as TimingAllowOrigin
import qualified Network.HTTP.Headers.Topic as Topic
import qualified Network.HTTP.Headers.Traceparent as Traceparent
import qualified Network.HTTP.Headers.Tracestate as Tracestate
import qualified Network.HTTP.Headers.TransferEncoding as Transfer
import qualified Network.HTTP.Headers.URI as LegacyURI
import qualified Network.HTTP.Headers.Urgency as Urgency
import qualified Network.HTTP.Headers.UserAgent as UserAgent
import qualified Network.HTTP.Headers.VariantVary as VariantVary
import Network.HTTP.Headers.Vary (Vary (..), VaryDirective (..))
import qualified Network.HTTP.Headers.Warning as Warning
import qualified Network.HTTP.Headers.Via as Via
import qualified Network.HTTP.Headers.WWWAuthenticate as WWW
import qualified Network.HTTP.Headers.XContentTypeOptions as XCTO
import qualified Network.HTTP.Headers.XCorrelationID as XCorrelationID
import qualified Network.HTTP.Headers.XDNSPrefetchControl as XDNS
import qualified Network.HTTP.Headers.XDownloadOptions as XDownload
import qualified Network.HTTP.Headers.XForwardedFor as XFF
import qualified Network.HTTP.Headers.XForwardedHost as XFH
import qualified Network.HTTP.Headers.XForwardedPort as XFP
import qualified Network.HTTP.Headers.XForwardedProto as XProto
import qualified Network.HTTP.Headers.XFrameOptions as XFO
import qualified Network.HTTP.Headers.XPermittedCrossDomainPolicies as XCrossDomain
import qualified Network.HTTP.Headers.XRealIP as XRealIP
import qualified Network.HTTP.Headers.XRequestStart as XRequestStart
import qualified Network.HTTP.Headers.XXSSProtection as XXSS
import Network.IPAddress (IPAddress (..), IPv4 (..))
import qualified Network.TLS.Extensions as TLS


main :: IO ()
main =
  defaultMain
    [ bgroup "small" (headerMapBenchmarks small)
    , bgroup "repeated" (headerMapBenchmarks repeated)
    , bgroup
        "link"
        [ bench "parse" $ whnf linkScore linkFixture
        , bench "parse-repeated" $ whnf linkScore linkRepeatedFixture
        , bench "render-preload" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Link.Link ("</main.css>; rel=preload; as=style" :| []))
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Link.Link linkFixture)
        , bench "render-repeated" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Link.Link linkRepeatedFixture)
        ]
    , bgroup
        "prefer"
        [ bench "parse" $ whnf preferScore preferFixture
        , bench "parse-repeated" $ whnf preferScore preferRepeatedFixture
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Prefer.Prefer preferFixture)
        , bench "parse-applied" $ whnf appliedScore appliedFixture
        , bench "render-applied" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Applied.PreferenceApplied appliedFixture)
        ]
    , bgroup
        "expect"
        [ bench "continue" $ whnf expectScore ("100-continue" :| [])
        , bench "extension" $ whnf expectScore expectFixture
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Expect.Expect expectFixture)
        ]
    , bgroup
        "alt-svc"
        [ bench "parse" $ whnf altSvcScore altSvcFixture
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @AltSvc.AltSvc altSvcFixture)
        ]
    , bgroup
        "x-robots-tag"
        [ bench "flags" $ whnf robotsScore ("noindex, nofollow" :| [])
        , bench "scoped" $ whnf robotsScore robotsFixture
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Robots.XRobotsTag robotsFixture)
        ]
    , bgroup
        "from-mailbox"
        [ bench "bare" $ whnf mailboxScore mailboxBareFixture
        , bench "named" $ whnf mailboxScore mailboxNamedFixture
        , bench "render-bare" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @From.From mailboxBareFixture)
        , bench "render-named" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @From.From mailboxNamedFixture)
        ]
    , bgroup
        "method-override"
        [ bench "standard" $ whnf methodOverrideScore ("PATCH" :| [])
        , bench "extension" $ whnf methodOverrideScore ("Custom-Verb" :| [])
        , bench "extension-ows" $ whnf methodOverrideScore ("\tCustom-Verb " :| [])
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @MethodOverride.XHttpMethodOverride ("PATCH" :| []))
        , bench "render-extension" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @MethodOverride.XHttpMethodOverride ("Custom-Verb" :| []))
        ]
    , bgroup
        "public-and-ediint"
        [ bench "public-single" $ whnf publicScore ("OPTIONS, MGET, MHEAD, GET, HEAD" :| [])
        , bench "public-split" $ whnf publicScore ("OPTIONS, MGET" :| ["MHEAD, GET, HEAD"])
        , bench "public-render" $ nf (renderToHeaders defaultHeaderSettings) (Public.PublicHeader ("OPTIONS" :| ["MGET", "MHEAD", "GET", "HEAD"]))
        , bench "ediint-single" $ whnf ediintScore ("CEM, multiple-attachments" :| [])
        , bench "ediint-split" $ whnf ediintScore ("CEM" :| ["multiple-attachments"])
        , bench "ediint-render" $ nf (renderToHeaders defaultHeaderSettings) (EDIINT.EDIINTFeatures ("CEM" :| ["multiple-attachments"]))
        ]
    , bgroup
        "safe-and-additions"
        [ bench "safe-yes" $ whnf safeScore ("yes" :| [])
        , bench "safe-mixed-case" $ whnf safeScore ("YeS" :| [])
        , bench "safe-render" $ nf (renderToHeaders defaultHeaderSettings) Safe.SafeYes
        , bench "additions-parse" $ whnf additionsScore additionsFixture
        , bench "additions-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Additions.AcceptAdditions additionsFixture)
        ]
    , bgroup
        "ua-compatible"
        [ bench "edge" $ whnf uaCompatibleScore ("IE=edge" :| [])
        , bench "two-engines" $ whnf uaCompatibleScore ("IE=edge, chrome=1" :| [])
        , bench "split" $ whnf uaCompatibleScore ("IE=edge" :| ["chrome=1"])
        , bench "render-edge" $ nf (renderToHeaders defaultHeaderSettings) (UA.XUACompatible (UA.UACompatDirective "IE" "edge" :| []))
        , bench "render-two-engines" $ nf (renderToHeaders defaultHeaderSettings) (UA.XUACompatible (UA.UACompatDirective "IE" "edge" :| [UA.UACompatDirective "chrome" "1"]))
        , bench "document-mode-edge" $ whnf uaDocumentModeScore (UA.UACompatDirective "IE" "edge")
        , bench "document-mode-emulate" $ whnf uaDocumentModeScore (UA.UACompatDirective "iE" "EmulateIE11")
        , bench "document-mode-version" $ whnf uaDocumentModeScore (UA.UACompatDirective "IE" "11")
        ]
    , bgroup
        "accept"
        [ bench "parse" $ nf acceptScore "text/html;level=1;q=0.8,application/json,*/*;q=0.001"
        , bench "parse-content-type" $ nf contentTypeScore "multipart/form-data;boundary=\"Aa:B, 01\""
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) browserAccept
        , bench "render-content-type" $ nf (renderToHeaders defaultHeaderSettings) multipartContentType
        , bench "language-parse" $ nf languageScore "en-US, zh-Hant-TW;q=0.9, es-419;q=0.8, *;q=0.1"
        ]
    , bgroup
        "content-encoding"
        [ bench "registered-token" $ nf codingScore "gzip"
        , bench "coding-chain" $ nf codingChainLength "gzip, br, zstd"
        , bench "coding-chain-lines" $ nf codingChainLengthFromFields ("gzip" :| ["br", "zstd"])
        , bench "empty" $ nf codingChainLength ""
        ]
    , bgroup
        "content-metadata"
        [ bench "disposition-parse" $ nf dispositionScore "attachment;filename=\"report 2026.txt\""
        , bench "disposition-parse-extended" $ nf dispositionScore "attachment;filename=report.txt;filename*=UTF-8'en-GB'%E2%82%AC+report.txt"
        , bench "disposition-filename" $ nf dispositionFilenameScore exampleDisposition
        , bench "disposition-render" $ nf (renderToHeaders defaultHeaderSettings) exampleDisposition
        , bench "language-parse" $ nf contentLanguageScore "en-US, es-419, zh-Hant-TW"
        ]
    , bgroup
        "cache-control"
        [ bench "canonical" $ nf cacheDirectiveScore "max-age=60, stale-while-revalidate=30, public"
        , bench "qualified" $ nf cacheDirectiveScore "private=\"Set-Cookie, X-Cache\", max-age=60"
        , bench "qualified-render" $
            nf (renderToHeaders defaultHeaderSettings) (CacheControl (Private (Just "Set-Cookie, X-Cache") :| [MaxAge 60]))
        , bench "mixed-case" $ nf cacheDirectiveScore "\t, MAX-AGE=60, , No-Store, \t"
        ]
    , bgroup
        "cdn-cache-control"
        [ bench "canonical" $ nf cdnDirectiveScore ("max-age=60, stale-while-revalidate=30, public" :| [])
        , bench "canonical-render" $
            nf (renderToHeaders defaultHeaderSettings) (CDN.CDNCacheControl (MaxAge 60 :| [StaleWhileRevalidate 30, Public]))
        , bench "parameterized" $ nf cdnDirectiveScore ("max-age=60;scope=cdn, no-store, extension=%\"caf%c3%a9\"" :| [])
        , bench "repeated" $ nf cdnDirectiveScore ("public, max-age=10" :| ["max-age=60, no-cache"])
        ]
    , bgroup
        "legacy-caching"
        [ bench "pragma-parse" $ whnf pragmaScore pragmaFixture
        , bench "pragma-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Pragma.Pragma pragmaFixture)
        , bench "warning-parse" $ whnf warningScore warningFixture
        , bench "warning-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Warning.Warning warningFixture)
        ]
    , let fields =
            headerMapFromList
              [ ("Content-Type", "text/plain")
              , ("Access-Control-Allow-Credentials", "true")
              , ("CDN-Cache-Control", "max-age=60")
              , ("Set-Cookie", "session=old")
              , ("Set-Cookie", "state=old")
              ]
      in bgroup
          "typed-header-updates"
          [ bench "credentials-set" $
              whnf (headerByteCount "Access-Control-Allow-Credentials" . setHeader (CorsCredentials.AccessControlAllowCredentials True)) fields
          , bench "credentials-clear" $
              whnf (headerByteCount "Access-Control-Allow-Credentials" . setHeader (CorsCredentials.AccessControlAllowCredentials False)) fields
          , bench "cdn-cache-control-set" $
              whnf (headerByteCount "CDN-Cache-Control" . setHeader (CDN.CDNCacheControl (MaxAge 120 :| [NoStore]))) fields
          , bench "cdn-cache-control-clear" $
              whnf (headerByteCount "CDN-Cache-Control" . setHeader CDN.CDNCacheControlEmpty) fields
          , bench "cdn-cache-control-delete" $
              whnf (headerByteCount "CDN-Cache-Control" . deleteHeader "CDN-Cache-Control") fields
          ]
    , bgroup
        "structured-headers"
        [ bench "priority" $ nf priorityScore ("u=5, i" :| [])
        , bench "priority-repeated" $ nf priorityScore ("u=1, i" :| ["u=4;scope=cdn"])
        , bench "content-digest" $
            nf contentDigestScore ("sha-256=:RK/0qy18MlBSVnWgNXKtPNwc4yQQnGzpdvc6tNkXLDg=:" :| [])
        , bench "content-digest-render" $
            nf (renderToHeaders defaultHeaderSettings) (Digest.ContentDigest (Digest.ContentDigestEntry "sha-256" (BS.replicate 32 0) [] :| []))
        , bench "content-digest-repeated" $
            nf contentDigestScore ("sha-256=:AA==:, ext=:Ag==:" :| ["sha-256=:AQ==:;stamp=@3"])
        ]
    , bgroup
        "signature-reporting"
        [ bench "signature" $ nf signatureScore signatureFixture
        , bench "signature-input" $ nf signatureInputScore signatureInputFixture
        , bench "signature-input-render" $ nf (renderToHeaders defaultHeaderSettings) signatureInputRenderFixture
        , bench "accept-signature" $ nf acceptSignatureScore acceptSignatureFixture
        , bench "reporting-endpoints" $ nf reportingScore reportingFixture
        , bench "reporting-endpoints-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Reporting.ReportingEndpoints reportingFixture)
        , bench "nel" $ whnf (shortBytes . NEL.nelPolicy . parseBenchmarkHeader) nelFixture
        , bench "nel-author" $ nf (either error (renderToHeaders defaultHeaderSettings) . NEL.nelFromPolicy) nelAuthoringFixture
        ]
    , bgroup
        "status-and-certificates"
        [ bench "cache-status" $ nf cacheStatusScore cacheStatusFixture
        , bench "proxy-status" $ nf proxyStatusScore proxyStatusFixture
        , bench "proxy-status-typed" $ nf typedProxyStatusScore proxyStatusFixture
        , bench "client-cert" $ nf clientCertScore (certificateFixture :| [])
        , bench "client-cert-chain" $ nf clientCertChainScore (certificateFixture :| [certificateFixture])
        , bench "cert-not-after" $ whnf certValidityScore ("Thu, 10 Oct 2019 00:00:00 GMT" :| [])
        ]
    , bgroup
        "cors"
        [ bench "allow-headers" $ nf corsAllowHeadersScore ("X-Custom-Header, Content-Type" :| [])
        , bench "allow-headers-repeated" $ nf corsAllowHeadersScore ("X-Custom-Header" :| ["Content-Type"])
        , bench "request-method" $ nf corsRequestMethodScore ("PATCH" :| [])
        , bench "max-age" $ nf corsMaxAgeScore ("86400" :| [])
        , bench "timing-allow-origin" $ nf timingAllowOriginScore ("https://example.test, https://other.test:8443" :| [])
        ]
    , bgroup
        "dates-and-delays"
        [ bench "date-parse" $ nf dateScore "Sun, 06 Nov 1994 08:49:37 GMT"
        , bench "date-render" $ nf (renderToHeaders defaultHeaderSettings) exampleDate
        , bench "age-parse" $ nf ageScore "3600"
        , bench "age-first-member" $ nf ageScore "3600, 7200"
        , bench "age-render" $ nf (renderToHeaders defaultHeaderSettings) (Age 3600)
        , bench "retry-after-parse" $ nf retryAfterScore "3600"
        , bench "retry-after-large-parse" $ nf largeRetryAfterScore (BS.replicate 4096 57)
        ]
    , bgroup
        "entity-tags"
        [ bench "strong" $ nf etagScore "\"b72a2352fd753cde308e8589dbca1a79\""
        , bench "weak" $ nf etagScore "W/\"b72a2352fd753cde308e8589dbca1a79\""
        , bench "opaque" $ nf etagScore ("\"b72a2352fd753cde" <> BS.pack [0x80, 0xff] <> "08e8589dbca1a79\"")
        , bench "match-list" $
            nf matchTagCount ("\"first\", W/\"middle\"" :| ["\"last\""])
        ]
    , bgroup
        "field-lists"
        [ bench "allow-parse" $ nf allowScore ("GET, HEAD, OPTIONS" :| [])
        , bench "connection-parse" $ nf connectionScore ("keep-alive, Upgrade" :| [])
        , bench "vary-parse" $ nf varyScore ("accept-encoding, accept-language" :| [])
        , bench "vary-repeated" $ nf varyScore ("accept-encoding" :| ["accept-language", "x-locale"])
        , bench "keep-alive-parse" $ whnf keepAliveScore ("timeout=10, max=20" :| [])
        , bench "keep-alive-repeated" $ whnf keepAliveScore keepAliveFixture
        , bench "keep-alive-limits" $ whnf keepAliveLimitsScore ("TIMEOUT=10, MAX=20" :| [])
        , bench "keep-alive-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @KeepAlive.KeepAlive keepAliveFixture)
        ]
    , bgroup
        "product-comments"
        [ bench "agent-bare" $ nf agentScore "Example/2"
        , bench "server-parse" $ nf serverScore "Example/2 (Unix build) TLS/1"
        , bench "server-render" $ nf (renderToHeaders defaultHeaderSettings) exampleServer
        , bench "via-parse" $ nf viaScore "HTTP/2 cache.example.org (edge, fast)"
        ]
    , bench "content-length-parse" $ nf contentLengthScore "16384"
    , bgroup
        "ranges"
        [ bench "closed" $ whnf rangeScore ("bytes=0-499" :| [])
        , bench "multiple" $ whnf rangeScore ("bytes=0-499, 1000-, -512" :| [])
        , bench "content-range" $ whnf contentRangeScore ("bytes 42-1233/1234" :| [])
        , bench "content-range-unsatisfied" $ whnf contentRangeScore ("bytes */1234" :| [])
        , bench "content-range-render" $
            nf
              (renderToHeaders defaultHeaderSettings)
              (ContentRange.ContentRange "bytes" (ContentRange.RangeRespSatisfied 42 1233 (Just 1234)))
        ]
    , bgroup
        "websocket-headers"
        [ bench "key" $ whnf webSocketKeySize ("dGhlIHNhbXBsZSBub25jZQ==" :| [])
        , bench "accept" $ whnf webSocketAcceptSize ("s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" :| [])
        , bench "key-render" $ nf (renderToHeaders defaultHeaderSettings) (WebSocketKey.SecWebSocketKey "the sample nonce")
        , bench "version" $ whnf webSocketVersionScore ("13" :| [])
        , bench "protocols" $ whnf webSocketProtocolScore ("chat, superchat" :| [])
        , bench "extensions" $
            whnf webSocketExtensionsScore ("permessage-deflate; client_max_window_bits=\"15\"; server_no_context_takeover" :| [])
        ]
    , bgroup
        "transparent-negotiation"
        [ bench "negotiate" $
            whnf (NE.length . Negotiate.negotiateDirectives . parseBenchmarkHeader) ("trans, 1.0, 2.5, x-mode=fast" :| [])
        , bench "negotiate-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Negotiate.Negotiate ("trans, 1.0, 2.5, x-mode=fast" :| []))
        , bench "tcn" $ whnf (length . TCN.tcnDirectives . parseBenchmarkHeader) ("choice, keep, x-note=\"vendor, mode\"" :| [])
        , bench "tcn-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @TCN.TCN ("choice, keep, x-note=\"vendor, mode\"" :| []))
        , bench "variant-vary" $ whnf variantVaryScore ("Accept-Language, Accept-Encoding, X-Edition" :| [])
        , bench "variant-vary-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @VariantVary.VariantVary ("Accept-Language, Accept-Encoding, X-Edition" :| []))
        ]
    , bgroup
        "binary-headers"
        [ bench "oscore" $ whnf (shortBytes . OSCORE.oscoreValue . parseBenchmarkHeader) ("CQUl" :| [])
        , bench "oscore-empty" $ whnf (shortBytes . OSCORE.oscoreValue . parseBenchmarkHeader) ("AA" :| [])
        , bench "oscore-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @OSCORE.OSCORE ("CQUl" :| []))
        , bench "oscore-typed" $ whnf (either error BS.length . OSCORE.oscoreOptionBytes . parseBenchmarkHeader) ("CQUl" :| [])
        , bench "oscore-encode" $ nf (renderToHeaders defaultHeaderSettings . OSCORE.oscoreFromOptionBytes) (BS.pack [9, 5, 37])
        , bench "token-binding" $ whnf (shortBytes . TokenBinding.secTokenBindingMessage . parseBenchmarkHeader) (tokenBindingWire :| [])
        , bench "token-binding-typed" $
            whnf (either error NE.length . TokenBinding.secTokenBindingEntries . parseBenchmarkHeader) (tokenBindingWire :| [])
        , bench "token-binding-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @TokenBinding.SecTokenBinding (tokenBindingWire :| []))
        , bench "token-binding-author" $
            nf
              (either error (renderToHeaders defaultHeaderSettings) . TokenBinding.secTokenBindingFromEntries)
              (either error id (TokenBinding.secTokenBindingEntries (parseBenchmarkHeader (tokenBindingWire :| []))))
        , bench "referred-binding" $ whnf (ReferredBinding.includeReferredTokenBindingID . parseBenchmarkHeader) ("true" :| [])
        ]
    , bgroup
        "variant-fields"
        [ bench "accept-features" $
            whnf (shortBytes . AcceptFeatures.rawAcceptFeatures . parseBenchmarkHeader) (featureWire :| [])
        , bench "accept-features-typed" $
            whnf (either error length . AcceptFeatures.acceptFeatureEntries . parseBenchmarkHeader) (featureWire :| [])
        , bench "accept-features-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @AcceptFeatures.AcceptFeatures (featureWire :| []))
        , bench "alternates" $
            whnf (shortBytes . Alternates.rawAlternates . parseBenchmarkHeader) (alternatesWire :| [])
        , bench "alternates-typed" $
            whnf alternatesScore (alternatesWire :| [])
        , bench "alternates-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Alternates.Alternates (alternatesWire :| []))
        ]
    , bgroup
        "soap-and-meter"
        [ bench "soap-parse" $ whnf (shortBytes . SoapAction.soapActionURI . parseBenchmarkHeader) ("\"http://electrocommerce.org/abc#MyMessage\"" :| [])
        , bench "soap-relative" $ whnf (shortBytes . SoapAction.soapActionURI . parseBenchmarkHeader) ("\"../order%20now#submit\"" :| [])
        , bench "soap-render" $
            nf (renderToHeaders defaultHeaderSettings) (SoapAction.SoapAction "http://electrocommerce.org/abc#MyMessage")
        , bench "meter-parse" $ whnf meterScore ("u=3,r=6,e" :| [])
        , bench "meter-count" $ whnf meterScore ("c=18446744073709551616/42,x" :| ["wont-limit"])
        , bench "meter-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Meter.Meter ("u=3,r=6,e" :| []))
        ]
    , bgroup
        "surrogate-fields"
        [ bench "capability" $ whnf surrogateCapabilityScore surrogateCapabilityFixture
        , bench "capability-lines" $ whnf surrogateCapabilityScore ("abc=\"Surrogate/1.0\"" :| ["def=\"Surrogate/1.0 ESI/1.0\""])
        , bench "capability-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @SurrogateCapability.SurrogateCapability surrogateCapabilityFixture)
        , bench "control" $ whnf surrogateControlScore surrogateControlFixture
        , bench "control-targets" $ whnf surrogateControlScore ("max-age=60+300;abc;def, no-store;ghi" :| ["content=\"ESI/1.0 ESI-Inline/1.0\";abc"])
        , bench "control-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @SurrogateControl.SurrogateControl surrogateControlFixture)
        ]
    , bgroup
        "legacy-digest"
        [ bench "parse-sha256" $ whnf legacyDigestScore (legacyDigestFixture :| [])
        , bench "parse-digest-lines" $ whnf legacyDigestScore (legacyDigestFixture :| ["CRC32c=0a72a4df, unixsum=30637"])
        , bench "parse-want-lines" $ whnf wantDigestScore wantDigestFixture
        , bench "render-digest" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @LegacyDigest.Digest (legacyDigestFixture :| ["CRC32c=0a72a4df, unixsum=30637"]))
        , bench "render-want" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @WantDigest.WantDigest wantDigestFixture)
        ]
    , bgroup
        "mime-and-atom-fields"
        [ bench "content-md5" $ whnf (BS.length . MD5.contentMD5Digest . parseBenchmarkHeader) ("Q2hlY2sgSW50ZWdyaXR5IQ==" :| [])
        , bench "content-md5-render" $ nf (renderToHeaders defaultHeaderSettings) (MD5.ContentMD5 "Check Integrity!")
        , bench "mime-version" $ whnf (MIME.mimeVersionMajor . parseBenchmarkHeader) ("1.0" :| [])
        , bench "mime-version-long" $ whnf (MIME.mimeVersionMajor . parseBenchmarkHeader) ("123.456" :| [])
        , bench "mime-version-comments" $
            whnf (MIME.mimeVersionMajor . parseBenchmarkHeader) ("(producer (MIME)) 1. (minor)0" :| [])
        , bench "mime-version-render" $ nf (renderToHeaders defaultHeaderSettings) (MIME.MIMEVersion 1 0)
        , bench "mime-version-render-long" $ nf (renderToHeaders defaultHeaderSettings) (MIME.MIMEVersion 123 456)
        , bench "slug" $ whnf (shortBytes . Slug.slugText . parseBenchmarkHeader) ("The Beach at Vacation" :| [])
        , bench "slug-escaped" $ whnf (shortBytes . Slug.slugText . parseBenchmarkHeader) ("The Beach at S%C3%A8te" :| [])
        , bench "slug-render" $ nf (renderToHeaders defaultHeaderSettings) (Slug.Slug "The Beach at S%C3%A8te")
        , bench "slug-decode-title" $
            nf Slug.slugDecodedText (Slug.Slug "The Beach at S%C3%A8te")
        , bench "slug-encode-title" $
            whnf (shortBytes . Slug.slugText . Slug.slugFromText) "The Beach at Sète"
        ]
    , bgroup
        "connection-negotiation"
        [ bench "alpn-token" $ whnf alpnBytes ("h2" :| [])
        , bench "alpn-list" $ whnf alpnBytes ("h2, http%2F1.1" :| [])
        , bench "alpn-render" $ nf (renderToHeaders defaultHeaderSettings) (ALPN.ALPN (TLS.alpnHttp2 :| [TLS.alpnHttp11]))
        , bench "h2-settings" $ whnf (BS.length . H2Settings.http2SettingsPayload . parseBenchmarkHeader) ("AAMAAABkAAQAAP__" :| [])
        , bench "h2-settings-render" $
            nf (renderToHeaders defaultHeaderSettings) (H2Settings.HTTP2Settings (BS.pack [0, 3, 0, 0, 0, 100, 0, 4, 0, 0, 255, 255]))
        ]
    , bgroup
        "sse-headers"
        [ bench "last-event-id" $ whnf (shortBytes . LastEventID.lastEventID . parseBenchmarkHeader) ("evt-42:checkpoint" :| [])
        , bench "last-event-id-utf8" $ whnf (shortBytes . LastEventID.lastEventID . parseBenchmarkHeader) ("caf\195\169-\240\159\140\141" :| [])
        , bench "last-event-id-render" $ nf (renderToHeaders defaultHeaderSettings) (LastEventID.LastEventID "evt-42:checkpoint")
        , bench "refresh-delay" $ whnf (Refresh.refreshSeconds . parseBenchmarkHeader) ("30" :| [])
        , bench "refresh-target" $ whnf (maybe 0 shortBytes . Refresh.refreshUrl . parseBenchmarkHeader) ("0; url=https://example.com/next" :| [])
        , bench "refresh-render" $ nf (renderToHeaders defaultHeaderSettings) (Refresh.Refresh 0 (Just "https://example.com/next"))
        ]
    , bgroup
        "legacy-browser-controls"
        [ bench "dns" $ whnf (XDNS.xdnsPrefetchEnabled . parseBenchmarkHeader) ("off" :| [])
        , bench "dns-folded" $ whnf (XDNS.xdnsPrefetchEnabled . parseBenchmarkHeader) ("OfF" :| [])
        , bench "download" $ whnf (parseBenchmarkHeader @XDownload.XDownloadOptions) ("noopen" :| [])
        , bench "cross-domain" $ whnf (parseBenchmarkHeader @XCrossDomain.XPermittedCrossDomainPolicies) ("master-only" :| [])
        , bench "cross-domain-paired" $ whnf (parseBenchmarkHeader @XCrossDomain.XPermittedCrossDomainPolicies) ("master-only" :| ["none-this-response"])
        , bench "xss" $ whnf (parseBenchmarkHeader @XXSS.XXSSProtection) ("1; mode=block" :| [])
        , bench "xss-report" $ whnf xxssReportScore ("1; mode=block; report=https://example.com/report" :| [])
        ]
    , bgroup
        "oauth-header-fields"
        [ bench "dpop" $ whnf (shortBytes . DPoP.dPoPProof . parseBenchmarkHeader) dpopBenchmarkField
        , bench "dpop-nonce" $ whnf (shortBytes . DPoPNonce.dPoPNonceValue . parseBenchmarkHeader) ("eyJ7S_zG.eyJH0-Z.HX4w-7v" :| [])
        , bench "replay-nonce" $ whnf (shortBytes . ReplayNonce.replayNonce . parseBenchmarkHeader) ("oFvnlf8Y9eK4Q9wQ-CKZ4g" :| [])
        ]
    , bgroup
        "web-push"
        [ bench "ttl" $ whnf (TTL.ttlSeconds . parseBenchmarkHeader) ("86400" :| [])
        , bench "ttl-render" $ nf (renderToHeaders defaultHeaderSettings) (TTL.TTL 86400)
        , bench "topic" $ whnf (shortBytes . Topic.topicValue . parseBenchmarkHeader) ("Update_2026-11-19_Group10" :| [])
        , bench "urgency" $ whnf (parseBenchmarkHeader @Urgency.Urgency) ("very-low" :| [])
        , bench "urgency-render" $ nf (renderToHeaders defaultHeaderSettings) Urgency.VeryLow
        , bench "urgency-render-literal" $ nf id ("very-low" :: ByteString)
        ]
    , bgroup
        "delta-encoding"
        [ bench "aim" $ whnf aimScore ("vcdiff; q=0.125, gzip" :| [])
        , bench "im" $ whnf imScore ("vcdiff, gzip, range" :| [])
        , bench "aim-extension" $ whnf aimExtensionScore aimExtensionFixture
        , bench "im-extension" $ whnf imExtensionScore imExtensionFixture
        , bench "aim-render" $
            nf (renderToHeaders defaultHeaderSettings) (AIM.AIM [AIM.WeightedIM "vcdiff" 0.125, AIM.WeightedIM "gzip" 1])
        , bench "im-render" $ nf (renderToHeaders defaultHeaderSettings) (IM.IM ["vcdiff", "gzip", "range"])
        , bench "aim-extension-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @AIM.AIM aimExtensionFixture)
        , bench "im-extension-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @IM.IM imExtensionFixture)
        ]
    , bgroup
        "content-security-policy"
        [ bench "parse" $ whnf cspScore cspFixture
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @CSP.ContentSecurityPolicy cspFixture)
        , bench "parse-policy-list" $ whnf cspScore cspListFixture
        , bench "render-policy-list" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @CSP.ContentSecurityPolicy cspListFixture)
        , bench "project-policy-list" $
            whnf
              (foldl' (\n policy -> n + cspDirectiveScore (CSP.effectiveDirectives policy)) 0 . CSP.cspPolicies)
              (parseBenchmarkHeader @CSP.ContentSecurityPolicy cspListFixture)
        ]
    , bgroup
        "permissions-policy"
        [ bench "parse" $ whnf permissionsPolicyScore ("geolocation=(self \"https://example.com\"), camera=(), fullscreen=*" :| [])
        , bench "parse-repeated" $ whnf permissionsPolicyScore ("geolocation=(self \"https://example.com\")" :| ["camera=(), fullscreen=*"])
        , bench "reporting" $ whnf permissionsReportingScore ("geolocation=(self \"https://*.example.com\");report-to=policy, camera=();report-to=camera" :| [])
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (PermissionsPolicy.PermissionsPolicy [("geolocation", [PermissionsPolicy.AllowSelf, PermissionsPolicy.AllowOrigin "https://example.com"]), ("camera", []), ("fullscreen", [PermissionsPolicy.AllowAll])])
        ]
    , bgroup
        "privacy-preferences"
        [ bench "gpc" $ whnf (parseBenchmarkHeader @GPC.SecGPC) ("1" :| [])
        , bench "gpc-repeated" $ whnf (parseBenchmarkHeader @GPC.SecGPC) ("0" :| ["invalid, 1", "1"])
        , bench "dnt" $ whnf (DNT.dntDoNotTrack . parseBenchmarkHeader) ("1" :| [])
        , bench "dnt-extension" $ whnf (shortBytes . DNT.dntExtension . parseBenchmarkHeader) ("1abc:/[];!%_~" :| [])
        , bench "save-data" $ whnf (shortBytes . SaveData.saveDataToken . parseBenchmarkHeader) ("on" :| [])
        , bench "save-data-list" $ whnf (SaveData.saveDataPreference . parseBenchmarkHeader) ("on;mode=mobile" :| ["future;flag"])
        , bench "save-data-render" $ nf (renderToHeaders defaultHeaderSettings) (SaveData.SaveData "on")
        ]
    , bgroup
        "request-signals"
        [ bench "accept-ch" $ whnf (length . AcceptCH.acceptCHHints . parseBenchmarkHeader) ("Sec-CH-UA, DPR, Width" :| [])
        , bench "accept-ch-repeated" $ whnf (length . AcceptCH.acceptCHHints . parseBenchmarkHeader) ("Sec-CH-UA" :| ["DPR, Width"])
        , bench "accept-ch-parameters" $ whnf (length . AcceptCH.acceptCHHints . parseBenchmarkHeader) ("Sec-CH-UA;sample=5, DPR;flag, Width" :| [])
        , bench "sec-purpose" $ whnf (parseBenchmarkHeader @SecPurpose.SecPurpose) ("prefetch" :| [])
        , bench "sec-purpose-prerender" $ whnf prefetchPrerenderScore ("prefetch;prerender;anonymous-client-ip=?0" :| [])
        , bench "sec-purpose-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @SecPurpose.SecPurpose ("prefetch" :| []))
        , bench "capsule" $ whnf (parseBenchmarkHeader @Capsule.CapsuleProtocol) ("?1" :| [])
        , bench "capsule-parameters" $ whnf (parseBenchmarkHeader @Capsule.CapsuleProtocol) ("?1;flag;label=\"udp\"" :| [])
        , bench "early-data" $ whnf (parseBenchmarkHeader @EarlyData.EarlyData) ("1" :| [])
        ]
    , bgroup
        "browser-response-fields"
        [ bench "nosniff" $ whnf (parseBenchmarkHeader @XCTO.XContentTypeOptions) ("nosniff" :| [])
        , bench "nosniff-folded" $ whnf (parseBenchmarkHeader @XCTO.XContentTypeOptions) ("NoSnIfF" :| [])
        , bench "frame-deny" $ whnf (parseBenchmarkHeader @XFO.XFrameOptions) ("DENY" :| [])
        , bench "frame-allow-from" $ whnf frameOptionsScore ("ALLOW-FROM https://example.com:8443/" :| [])
        , bench "frame-deny-render" $ nf (renderToHeaders defaultHeaderSettings) XFO.Deny
        , bench "clear-site-data" $ whnf (NE.length . CSD.clearSiteDataDirectives . parseBenchmarkHeader) ("\"cache\", \"cookies\", \"storage\"" :| [])
        ]
    , bgroup
        "cross-origin-policies"
        [ bench "embedder" $ whnf (parseBenchmarkHeader @COEP.CrossOriginEmbedderPolicy) ("require-corp" :| [])
        , bench "embedder-endpoint" $ whnf (reportToBytes . COEP.coepReportTo . parseBenchmarkHeader) ("require-corp;report-to=\"embedder\"" :| [])
        , bench "opener" $ whnf (parseBenchmarkHeader @COOP.CrossOriginOpenerPolicy) ("same-origin-allow-popups" :| [])
        , bench "opener-endpoint" $ whnf (reportToBytes . COOP.coopReportTo . parseBenchmarkHeader) ("same-origin;report-to=\"opener\"" :| [])
        , bench "resource" $ whnf (parseBenchmarkHeader @CORP.CrossOriginResourcePolicy) ("same-origin" :| [])
        , bench "agent-cluster" $ whnf (parseBenchmarkHeader @OAC.OriginAgentCluster) ("?1" :| [])
        , bench "embedder-render" $ nf (renderToHeaders defaultHeaderSettings) (COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp Nothing)
        , bench "embedder-endpoint-render" $ nf (renderToHeaders defaultHeaderSettings) (COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp (Just (RFC8941String "embedder")))
        ]
    , bgroup
        "oslc"
        [ bench "version" $ whnf (shortBytes . OSLCCoreVersion.oslcCoreVersion . parseBenchmarkHeader) ("3.0" :| [])
        , bench "version-padded" $ whnf (shortBytes . OSLCCoreVersion.oslcCoreVersion . parseBenchmarkHeader) ("002.00401" :| [])
        , bench "version-numbers" $ whnf (maybe 0 OSLCCoreVersion.oslcCoreMinor . OSLCCoreVersion.oslcCoreVersionNumbers . parseBenchmarkHeader) ("002.00401" :| [])
        , bench "context" $ whnf (shortBytes . ConfigurationContext.configurationContextValue . parseBenchmarkHeader) ("https://example.com/configurations/myConfiguration1" :| [])
        , bench "context-render" $ nf (renderToHeaders defaultHeaderSettings) (ConfigurationContext.ConfigurationContext "https://example.com/configurations/myConfiguration1")
        ]
    , bgroup
        "html-metadata"
        [ bench "script-type" $ whnf (SBS.length . ContentScriptType.contentScriptTypeBytes . parseBenchmarkHeader) ("text/javascript" :| [])
        , bench "style-type" $ whnf (SBS.length . ContentStyleType.contentStyleTypeBytes . parseBenchmarkHeader) ("text/css; charset=utf-8" :| [])
        , bench "default-style" $ whnf (SBS.length . DefaultStyle.defaultStyleTitleBytes . parseBenchmarkHeader) ("\"Compact layout\"" :| [])
        , bench "default-style-render" $ nf (renderToHeaders defaultHeaderSettings) (DefaultStyle.DefaultStyle "Compact layout")
        ]
    , bgroup
        "legacy-version-tags"
        [ bench "content-version" $ whnf (SBS.length . ContentVersion.contentVersionTagBytes . parseBenchmarkHeader) ("\"release-2026.11.19 v2\"" :| [])
        , bench "content-version-text" $ whnf (shortBytes . ContentVersion.contentVersionTag . parseBenchmarkHeader) ("\"release-2026.11.19 v2\"" :| [])
        , bench "derived-from" $ whnf (SBS.length . DerivedFrom.derivedFromTagBytes . parseBenchmarkHeader) ("\"release-2026.11.19 v2\"" :| [])
        , bench "quoted-pairs" $ whnf (SBS.length . ContentVersion.contentVersionTagBytes . parseBenchmarkHeader) ("\"release\\\"v2\\\\stable\"" :| [])
        , bench "plain-render" $ nf (renderToHeaders defaultHeaderSettings) (ContentVersion.ContentVersion "release-2026.11.19 v2")
        , bench "escaped-render" $ nf (renderToHeaders defaultHeaderSettings) (DerivedFrom.DerivedFrom "release\"v2\\stable")
        ]
    , bgroup
        "legacy-uri"
        [ bench "content-base" $ whnf (shortBytes . ContentBase.contentBaseUri . parseBenchmarkHeader) ("https://example.org/documents/base%20folder/" :| [])
        , bench "content-base-render" $ nf (renderToHeaders defaultHeaderSettings) (ContentBase.ContentBase "https://example.org/documents/base%20folder/")
        , bench "uri-list" $ whnf legacyUriScore ("<https://example.org/a,b>, <../manual%20page#sec>" :| [])
        , bench "uri-lines" $ whnf legacyUriScore ("<https://example.org/a,b>" :| ["<../manual%20page#sec>"])
        , bench "uri-render" $ nf (renderToHeaders defaultHeaderSettings) (LegacyURI.URI ("https://example.org/a,b" :| ["../manual%20page#sec"]))
        ]
    , bgroup
        "content-id"
        [ bench "batch" $ whnf (shortBytes . ContentID.contentID . parseBenchmarkHeader) ("request-42_part.1" :| [])
        , bench "mime" $ whnf (shortBytes . ContentID.contentID . parseBenchmarkHeader) ("<part.42+alt@example.org>" :| [])
        , bench "drp" $ whnf (shortBytes . ContentID.contentID . parseBenchmarkHeader) ("urn:md5:PEFjWBDv/sd9alS9BYuX0w==" :| [])
        , bench "drp-composite" $ whnf (shortBytes . ContentID.contentID . parseBenchmarkHeader) ("http://www.acme.com/Example/,urn:x-version:5" :| [])
        , bench "differential" $ whnf (shortBytes . DifferentialID.differentialID . parseBenchmarkHeader) ("urn:md5:3FS2oCnWPZptpN05oKBemA==" :| [])
        ]
    , bgroup
        "odata"
        [ bench "version" $ whnf (ODataVersion.oDataVersionMinor . parseBenchmarkHeader) ("4.01" :| [])
        , bench "version-render" $ nf (renderToHeaders defaultHeaderSettings) (ODataVersion.ODataVersion 4 1)
        , bench "max-version" $ whnf (ODataMaxVersion.oDataMaxVersionMinor . parseBenchmarkHeader) ("4.01" :| [])
        , bench "max-version-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @ODataMaxVersion.ODataMaxVersion ("4.01" :| []))
        , bench "max-version-extended" $ whnf (ODataMaxVersion.oDataMaxVersionLeadingZeros . parseBenchmarkHeader) ("12.0001234567890" :| [])
        , bench "isolation" $ whnf (parseBenchmarkHeader @Isolation.Isolation) ("snapshot" :| [])
        , bench "legacy-isolation" $ whnf (parseBenchmarkHeader @ODataIsolation.ODataIsolation) ("snapshot" :| [])
        , bench "entity-absolute" $ whnf (shortBytes . ODataEntityId.oDataEntityId . parseBenchmarkHeader) ("https://example.com/service/Customers(17)" :| [])
        , bench "entity-relative" $ whnf (shortBytes . ODataEntityId.oDataEntityId . parseBenchmarkHeader) ("../Customers('Caf%C3%A9')?view=short" :| [])
        , bench "entity-unicode" $ whnf (shortBytes . ODataEntityId.oDataEntityId . parseBenchmarkHeader) ("https://m\xc3\xbcnich.example/Caf\xc3\xa9s(1)" :| [])
        , bench "entity-render" $ nf (renderToHeaders defaultHeaderSettings) (ODataEntityId.ODataEntityId "https://example.com/service/Customers(17)")
        , bench "entity-unicode-render" $ nf (renderToHeaders defaultHeaderSettings) (ODataEntityId.ODataEntityId "https://münich.example/Cafés(1)")
        ]
    , bgroup
        "repeatability"
        [ bench "request-id" $ whnf (shortBytes . RepeatabilityRequestID.repeatabilityRequestID . parseBenchmarkHeader) ("8b59ABCD-1234-4321-abcd-001122334455" :| [])
        , bench "client-id" $ whnf (shortBytes . RepeatabilityClientID.repeatabilityClientID . parseBenchmarkHeader) ("0e8a7e9f-a3c4-4b2e-9f1a-1234567890ab" :| [])
        , bench "first-sent" $ nf (RepeatabilityFirstSent.repeatabilityFirstSent . parseBenchmarkHeader) ("Tue, 28 Feb 2012 12:34:56 GMT" :| [])
        , bench "first-sent-render" $ nf (renderToHeaders defaultHeaderSettings) (RepeatabilityFirstSent.RepeatabilityFirstSent (UTCTime (fromGregorian 2012 2 28) (secondsToDiffTime 45296)))
        , bench "result" $ whnf (parseBenchmarkHeader @RepeatabilityResult.RepeatabilityResult) ("accepted" :| [])
        ]
    , bgroup
        "caldav"
        [ bench "schedule-tag" $ whnf (shortBytes . ScheduleTag.scheduleTag . parseBenchmarkHeader) ("\"12ab34-cd56ef\"" :| [])
        , bench "if-schedule-tag-match" $ whnf (shortBytes . ScheduleTag.scheduleTag . IfScheduleTagMatch.ifScheduleTagMatch . parseBenchmarkHeader) ("\"12ab34-cd56ef\"" :| [])
        , bench "schedule-reply" $ whnf (ScheduleReply.scheduleReply . parseBenchmarkHeader) ("F" :| [])
        , bench "timezones" $ whnf (CalDAVTimezones.calDAVTimezones . parseBenchmarkHeader) ("T" :| [])
        , bench "managed-id" $ whnf (shortBytes . CalManagedID.calManagedID . parseBenchmarkHeader) ("aUNhbGVuZGFy" :| [])
        ]
    , bgroup
        "webdav-extension"
        [ bench "label" $ whnf (shortBytes . Label.labelName . parseBenchmarkHeader) ("stable-v1.0" :| [])
        , bench "label-escaped" $ whnf (shortBytes . Label.labelName . parseBenchmarkHeader) ("release%20B.3%20caf%C3%A9" :| [])
        , bench "position" $ whnf positionScore ("after chapter%201;rev=2" :| [])
        , bench "ordering-type" $ whnf (shortBytes . OrderingType.orderingTypeUri . parseBenchmarkHeader) ("DAV:custom" :| [])
        , bench "redirect-ref" $ whnf (shortBytes . RedirectRef.redirectRefTarget . parseBenchmarkHeader) ("../docs/known%20item?view=full#details" :| [])
        , bench "apply-to-redirect-ref" $ whnf (ApplyToRedirectRef.applyToReference . parseBenchmarkHeader) ("T" :| [])
        , bench "status-uri" $ whnf statusUriScore ("404 <https://a.example/x> 423 <https://b.example/y>" :| [])
        ]
    , bgroup
        "webdav"
        [ bench "if" $ whnf (shortBytes . DAVIf.ifConditions . parseBenchmarkHeader) davIfFixture
        , bench "if-tree" $ whnf davIfTreeScore davIfFixture
        , bench "if-tree-render" $ nf (renderToHeaders defaultHeaderSettings . DAVIf.fromIfConditionLists) davIfTree
        , bench "destination" $ whnf (shortBytes . Destination.destinationUri . parseBenchmarkHeader) ("https://dav.example/docs/a%20b?rev=2" :| [])
        , bench "lock-token" $ whnf (shortBytes . LockToken.lockTokenUri . parseBenchmarkHeader) ("<urn:uuid:a515cfa4-5da4-22e1-f5b5-00a0451e6bf7>" :| [])
        , bench "depth" $ whnf depthScore ("infinity" :| [])
        , bench "overwrite" $ whnf (Overwrite.overwriteAllowed . parseBenchmarkHeader) ("T" :| [])
        , bench "timeout" $ whnf timeoutScore ("Second-120, Infinite" :| [])
        , bench "dav" $ whnf davScore ("1, 2, <https://dav.example/dav/propset>" :| [])
        , bench "dasl" $ whnf (shortBytes . DASL.daslGrammar . parseBenchmarkHeader) ("<DAV:basicsearch>" :| [])
        ]
    , bgroup
        "forwarding"
        [ bench "forwarded" $ whnf forwardedScore ("for=192.0.2.60;proto=https;by=203.0.113.43, for=\"[2001:db8::1]:4711\"" :| [])
        , bench "xff" $ whnf xffScore ("192.0.2.60, 203.0.113.43, [2001:db8::1]:4711" :| [])
        , bench "host" $ whnf (shortBytes . XFH.xForwardedHostValue . parseBenchmarkHeader) ("app.example:443" :| [])
        , bench "port" $ whnf (XFP.xForwardedPortNumber . parseBenchmarkHeader) ("443" :| [])
        , bench "proto" $ whnf xProtoScore ("https" :| [])
        , bench "real-ip" $ whnf realIPScore ("192.0.2.60" :| [])
        , bench "max-hops" $ whnf (MaxForwards.maxForwards . parseBenchmarkHeader) ("31" :| [])
        ]
    , bgroup
        "transport-policy"
        [ bench "hsts" $ whnf hstsScore ("max-age=31536000; includeSubDomains; preload" :| [])
        , bench "hsts-age-only" $ whnf hstsScore ("max-age=31536000" :| [])
        , bench "hsts-render" $ nf (renderToHeaders defaultHeaderSettings) (HSTS.StrictTransportSecurity 31536000 True True)
        , bench "expect-ct" $ whnf expectCTScore ("max-age=86400, enforce, report-uri=\"https://ct.example/report\"" :| [])
        , bench "pins" $ whnf pinPolicyScore ((pinFixture <> "; max-age=3000; includeSubDomains") :| [])
        , bench "report-only" $ whnf reportOnlyScore ((pinFixture <> "; max-age=3000; report-uri=\"https://ct.example/report\"") :| [])
        ]
    , bgroup
        "server-timing-and-cdn-loop"
        [ bench "timing-parse" $ whnf serverTimingScore (timingFixture :| [])
        , bench "timing-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @ServerTiming.ServerTiming (timingFixture :| []))
        , bench "cdn-parse" $ whnf cdnLoopScore (cdnLoopFixture :| [])
        , bench "cdn-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @CDNLoop.CDNLoop (cdnLoopFixture :| []))
        ]
    , bgroup
        "trace-context"
        [ bench "parent" $ whnf traceparentScore (traceparentFixture :| [])
        , bench "state" $ whnf tracestateScore ("rojo=00f067aa0ba902b7,congo=t61rcWkgMzE" :| [])
        , bench "parent-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Traceparent.Traceparent (traceparentFixture :| []))
        ]
    , bgroup
        "authority"
        [ bench "host" $ nf hostScore "app.example.org:8443"
        , bench "origin" $ nf originScore "https://app.example.org:8443"
        , bench "via" $ nf viaScore "1.1 app.example.org:8443 (edge)"
        ]
    , bench "authority-ipv6" $ nf hostScore "[2001:db8::1]:8443"
    , bgroup
        "uri-references"
        [ bench "location-relative" $ nf locationBytes "/widgets/%2f?view=full#details"
        , bench "referer-absolute" $ nf refererBytes "https://example.org/articles/http?sort=recent"
        , bench "ping-to" $ nf pingBytes "https://example.org/next?section=[api]"
        , bench "location-render" $ nf (renderToHeaders defaultHeaderSettings) (Location "/widgets/%2f?view=full#details")
        , bench "ping-to-render" $ nf (renderToHeaders defaultHeaderSettings) (PingTo "https://example.org/next?section=[api]")
        ]
    , bgroup
        "amp-cache-transform"
        [ bench "text" $ whnf (shortBytes . AMP.ampCacheTransformValue . parseBenchmarkHeader) (ampFixture :| [])
        , bench "render" $ nf (Mason.toStrictByteString . AMP.renderAMPCacheTransform) (parseBenchmarkHeader (ampFixture :| []))
        , bench "parse-select" $ whnf ampSelectionScore (ampFixture :| [])
        , bench "parse-select-render" $ nf ampSelectedResponse (ampFixture :| [])
        , bench "parse-repeated-select" $ whnf ampSelectionScore ("google;v=\"1..3,5\"" :| ["any"])
        , bench "render-field-lines" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @AMP.AMPCacheTransform (ampFixture :| []))
        ]
    , bgroup
        "diagnostic-fields"
        [ bench "correlation-id" $ whnf (shortBytes . XCorrelationID.xCorrelationID . parseBenchmarkHeader) (correlationFixture :| [])
        , bench "correlation-bytes" $ whnf (BS.length . XCorrelationID.xCorrelationIDBytes . parseBenchmarkHeader) (correlationFixture :| [])
        , bench "request-start" $ whnf (shortBytes . XRequestStart.xRequestStartTimestamp . parseBenchmarkHeader) (requestStartFixture :| [])
        , bench "request-start-epoch" $ whnf (either error id . XRequestStart.xRequestStartEpoch . parseBenchmarkHeader) (requestStartFixture :| [])
        , bench "render-request-start" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @XRequestStart.XRequestStart (requestStartFixture :| []))
        ]
    , bgroup
        "ops"
        [ bench "read" $ whnf (shortBytes . GetProfile.getProfileValue . parseBenchmarkHeader) (opsReadFixture :| [])
        , bench "write" $ whnf (shortBytes . SetProfile.setProfileValue . parseBenchmarkHeader) (opsWriteFixture :| [])
        , bench "profile-object" $ whnf (shortBytes . ProfileObject.profileObjectValue . parseBenchmarkHeader) (opsObjectFixture :| [])
        , bench "read-bytes" $ whnf (BS.length . GetProfile.getProfileBytes . parseBenchmarkHeader) (opsReadFixture :| [])
        , bench "write-bytes" $ whnf (BS.length . SetProfile.setProfileBytes . parseBenchmarkHeader) (opsWriteFixture :| [])
        , bench "profile-object-bytes" $ whnf (BS.length . ProfileObject.profileObjectBytes . parseBenchmarkHeader) (opsObjectFixture :| [])
        , bench "profile-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @ProfileObject.ProfileObject (opsObjectFixture :| []))
        ]
    , bgroup
        "p3p"
        [ bench "policyref" $ whnf (shortBytes . P3P.p3pValue . parseBenchmarkHeader) (p3pReferenceFixture :| [])
        , bench "compact" $ whnf (shortBytes . P3P.p3pValue . parseBenchmarkHeader) (p3pCompactFixture :| [])
        , bench "policyref-typed" $ whnf p3pDirectiveScore (p3pReferenceFixture :| [])
        , bench "compact-typed" $ whnf p3pDirectiveScore (p3pCompactFixture :| [])
        , bench "render-field-lines" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @P3P.P3P (p3pCompactFixture :| []))
        , bench "render" $
            nf (Mason.toStrictByteString . P3P.renderP3P) (parseBenchmarkHeader @P3P.P3P (p3pCompactFixture :| []))
        ]
    , bgroup
        "pics-label"
        [ bench "simple" $ whnf (shortBytes . PICSLabel.picsLabelValue . parseBenchmarkHeader) (picsSimpleLabel :| [])
        , bench "options-and-range" $ whnf (shortBytes . PICSLabel.picsLabelValue . parseBenchmarkHeader) (picsComplexLabel :| [])
        , bench "simple-typed" $ whnf picsLabelScore (picsSimpleLabel :| [])
        , bench "options-and-range-typed" $ whnf picsLabelScore (picsComplexLabel :| [])
        , bench "render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @PICSLabel.PICSLabel (picsComplexLabel :| []))
        ]
    , bgroup
        "legacy-protocol"
        [ env (pure $! BS.copy "{PICS-1.1 {headers PICS-Label}}") $ \wire ->
            bench "pics-response" $ whnf (shortBytes . Protocol.protocolValue . parseBenchmarkHeader) (wire :| [])
        , bench "pics-request" $ whnf (shortBytes . ProtocolRequest.protocolRequestValue . parseBenchmarkHeader) (picsRequestFixture :| [])
        , bench "jepi-info" $ whnf (shortBytes . ProtocolInfo.protocolInfoValue . parseBenchmarkHeader) (jepiInfoFixture :| [])
        , bench "jepi-query" $ whnf (shortBytes . ProtocolQuery.protocolQueryValue . parseBenchmarkHeader) (jepiQueryFixture :| [])
        , env (pure $! BS.copy "{PICS-1.1 {headers PICS-Label}}") $ \wire ->
            bench "pics-response-typed" $
              whnf (either error NE.head . Protocol.protocolEntries . parseBenchmarkHeader) (wire :| [])
        , bench "pics-request-typed" $
            whnf (either error (foldl' picsRequestScore 0) . ProtocolRequest.protocolRequestEntries . parseBenchmarkHeader) (picsRequestFixture :| [])
        , bench "jepi-info-typed" $
            whnf (either error (foldl' legacyDirectiveScore 0) . ProtocolInfo.protocolInfoEntries . parseBenchmarkHeader) (jepiInfoFixture :| [])
        , bench "jepi-query-typed" $
            whnf (either error (foldl' legacyDirectiveScore 0) . ProtocolQuery.protocolQueryEntries . parseBenchmarkHeader) (jepiQueryFixture :| [])
        , bench "jepi-info-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @ProtocolInfo.ProtocolInfo (jepiInfoFixture :| []))
        ]
    , bgroup
        "pep"
        [ bench "declaration" $ whnf (shortBytes . PEP.rawPEP . parseBenchmarkHeader) (pepDeclarationWire :| [])
        , bench "policy" $ whnf (shortBytes . PEPInfo.rawPEPInfo . parseBenchmarkHeader) (pepPolicyWire :| [])
        , bench "declaration-typed" $
            whnf (either error (foldl' pepDeclarationScore 0) . PEP.pepDeclarations . parseBenchmarkHeader) (pepDeclarationWire :| [])
        , bench "policy-typed" $
            whnf (either error (foldl' pepPolicyScore 0) . PEPInfo.pepInfoPolicies . parseBenchmarkHeader) (pepPolicyWire :| [])
        , bench "declaration-render" $
            nf (Mason.toStrictByteString . PEP.renderPEP) (parseBenchmarkHeader @PEP.PEP (pepDeclarationWire :| []))
        , bench "declaration-render-field-lines" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @PEP.PEP (pepDeclarationWire :| []))
        ]
    , bgroup
        "extension-framework"
        [ bench "man-uri" $ whnf (shortBytes . Man.rawMan . parseBenchmarkHeader) (extensionFixture :| [])
        , bench "opt-field" $ whnf (shortBytes . Opt.rawOpt . parseBenchmarkHeader) ("\"Range\"" :| [])
        , bench "man-uri-typed" $
            whnf (either error extensionDeclarationScore . Man.manDeclarations . parseBenchmarkHeader) (extensionFixture :| [])
        , bench "opt-field-typed" $
            whnf (either error extensionDeclarationScore . Opt.optDeclarations . parseBenchmarkHeader) ("\"Range\"" :| [])
        , bench "ext" $ whnf (shortBytes . Ext.rawExt . parseBenchmarkHeader) ("" :| [])
        , bench "man-render" $
            nf (Mason.toStrictByteString . Man.renderMan) (parseBenchmarkHeader @Man.Man (extensionFixture :| []))
        , bench "man-render-field-lines" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Man.Man (extensionFixture :| []))
        ]
    , bgroup
        "proxy-notification"
        [ bench "features-notify" $ whnf (shortBytes . ProxyFeatures.proxyFeaturesValue . parseBenchmarkHeader) ("proxy.w3.org; notify" :| [])
        , bench "features-notify-typed" $ whnf proxyFeaturesScore ("proxy.w3.org; notify" :| [])
        , bench "features-log" $ whnf (shortBytes . ProxyFeatures.proxyFeaturesValue . parseBenchmarkHeader) (proxyFeatureWire :| [])
        , bench "features-log-typed" $ whnf proxyFeaturesScore (proxyFeatureWire :| [])
        , bench "features-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @ProxyFeatures.ProxyFeatures (proxyFeatureWire :| []))
        , bench "instruction" $ whnf (shortBytes . ProxyInstruction.proxyInstructionValue . parseBenchmarkHeader) ("proxy.w3.org; log" :| [])
        , bench "instruction-typed" $
            whnf (either error ProxyInstruction.proxyInstructionCommand . ProxyInstruction.proxyInstructionDetails . parseBenchmarkHeader) ("proxy.w3.org; log" :| [])
        , bench "instruction-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @ProxyInstruction.ProxyInstruction ("proxy.w3.org; log" :| []))
        ]
    , bgroup
        "legacy-access-fields"
        [ bench "method" $ whnf (shortBytes . MethodCheck.methodCheckValue . parseBenchmarkHeader) ("PATCH" :| [])
        , bench "method-typed" $ whnf (either (error . show) id . MethodCheck.methodCheckMethod . parseBenchmarkHeader) ("PATCH" :| [])
        , bench "expires" $ whnf (shortBytes . MethodCheckExpires.methodCheckExpiresValue . parseBenchmarkHeader) (legacyExpiryWire :| [])
        , bench "expiry-timestamp" $
            whnf (either error (toModifiedJulianDay . utctDay) . MethodCheckExpires.methodCheckExpiresTime . parseBenchmarkHeader) (legacyExpiryWire :| [])
        , bench "expires-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @MethodCheckExpires.MethodCheckExpires (legacyExpiryWire :| []))
        , bench "root" $ whnf (shortBytes . RefererRoot.refererRootValue . parseBenchmarkHeader) ("https://[2001:db8::1]:443" :| [])
        , bench "root-typed" $ whnf (either error scoreRefererRoot . RefererRoot.refererRootOrigin . parseBenchmarkHeader) ("https://[2001:db8::1]:443" :| [])
        , bench "root-null" $ whnf (either error scoreRefererRoot . RefererRoot.refererRootOrigin . parseBenchmarkHeader) ("null" :| [])
        , bench "root-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @RefererRoot.RefererRoot ("https://[2001:db8::1]:443" :| []))
        , bench "security-scheme" $ whnf (shortBytes . SecurityScheme.securitySchemeValue . parseBenchmarkHeader) ("S-HTTP/1.4" :| [])
        ]
    , bgroup
        "legacy-cookies"
        [ bench "request" $ whnf (shortBytes . Cookie2.cookie2Value . parseBenchmarkHeader) ("$Version=\"1\"" :| [])
        , bench "request-typed" $ whnf (either error id . Cookie2.cookie2Version . parseBenchmarkHeader) ("$Version=\"1\"" :| [])
        , bench "request-render" $ nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @Cookie2.Cookie2 ("$Version=\"1\"" :| []))
        , bench "response" $
            whnf (sum . map (shortBytes . SetCookie2.setCookie2Value) . SetCookie2.getSetCookie2s . parseBenchmarkHeader) (legacyCookieWire :| [])
        , bench "response-typed" $ whnf legacyCookieScore (legacyCookieWire :| [])
        , bench "response-folded" $ whnf legacyCookieScore ((legacyCookieWire <> ", Locale=\"da-DK\"; Version=1") :| [])
        , bench "response-render" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @SetCookie2.SetCookie2s (legacyCookieWire :| []))
        ]
    , bgroup
        "request-cookie"
        [ bench "parse-pairs" $ nf cookieBytes ("session=0123456789abcdef; locale=en-US; flags=new_ui" :| [])
        , bench "parse-base64" $ nf cookieBytes ("session=ab+c/def==; flag=; quoted=\"v:1\"" :| [])
        , bench "parse-split" $ nf cookieBytes ("session=ab+c/def==" :| ["flag=", "quoted=\"v:1\""])
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) exampleCookie
        ]
    , bgroup
        "response-cookie"
        [ bench "parse-pair" $ nf setCookieScore "session=abc"
        , bench "parse-attributes" $ nf setCookieScore "session=abc; Domain=example.org; Secure; HttpOnly; SameSite=Lax"
        , bench "parse-scoped" $ nf setCookieScore "session=\"ab+c/def==\"; path=/account/; max-age=3600; secure; samesite=lax"
        , bench "expires-http-date" $ nf setCookieScore "session=abc; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
        , bench "expires-token-date" $ nf setCookieScore "session=abc; Expires=08:49:37 | 6 nOVeMBer 1994"
        , bench "parse-extensions" $ nf setCookieExtensionScore setCookieExtensionFixture
        , bench "render-extensions" $
            nf (renderToHeaders defaultHeaderSettings) (parseBenchmarkHeader @SetCookies (setCookieExtensionFixture :| []))
        , bench "render" $ nf (renderToHeaders defaultHeaderSettings) (SetCookies [exampleSetCookie])
        ]
    , bgroup
        "accept-encoding-selection"
        [ bench "plain" $
            whnf selectEncoding ("br, gzip, identity" :| [])
        , bench "weighted-lines" $
            whnf selectEncoding ("gzip;q=0, *;q=0.2" :| ["br;q=0.8, identity;q=0.1"])
        , bench "already-typed" $
            whnf (AcceptEncoding.preferredContentCoding (Just (parseBenchmarkHeader ("br, gzip, identity" :| []))))
              [Identity, GZip, Brotli, ZStd]
        ]
    , bgroup
        "transfer-codings"
        [ bench "applied" $ nf transferCodingCount "gzip, chunked"
        , bench "negotiated" $ nf teScore "trailers, gzip;q=0.5"
        ]
    , bench "transfer-extension" $ nf parameterizedTransferScore "example;level=2;note=\"edge, fast\", chunked"
    , bgroup
        "authentication"
        [ bench "token68" $ nf credentialScore "Example ab.+_/~=="
        , bench "challenges" $ nf challengeScore "Example realm=\"a,b\", nonce=one, Next abc=="
        , bench "control" $
            nf controlScore ("Basic realm=\"entrance\", no-auth=true, Digest realm=\"account\"" :| [])
        , bench "control-extended" $
            nf controlScore ("Basic realm=\"entrance\", username*=UTF-8''Ren%C3%A9e" :| [])
        , bench "control-repeated" $
            nf controlScore ("Basic realm=\"entrance\"" :| ["username*=UTF-8''Ren%C3%A9e", "Digest realm=\"account\""])
        ]
    ]
  where
    headerMapBenchmarks fields =
      let headers = headerMapFromList fields
      in headers `seq`
           [ bench "build" $ whnf headerMapFromList fields
           , bench "to-list" $ nf (foldl' (\n (_, value) -> n + BS.length value) 0 . headerMapToList) headers
           , bench "lookup-raw" $ nf (maybe 0 (foldl' (\n value -> n + BS.length value) 0) . lookupRawHeader hCacheControl) headers
           , bench "lookup-typed" $ nf cacheControlScore headers
           ]


controlScore :: NonEmpty ByteString -> Int
controlScore values = case parseFromHeaders defaultHeaderSettings values of
  Right (AuthControl.AuthenticationControl entries) ->
    foldl' (\n entry -> n + length (AuthControl.authControlParams entry)) 0 entries
  Left err -> error err


etagScore :: ByteString -> Int
etagScore value = case ETag.parseETag value of
  Right (ETag.ETag tag) -> SBS.length (ETag.entityTagBytes tag)
  Left err -> error err


matchTagCount :: NonEmpty ByteString -> Int
matchTagCount values = case parseFromHeaders defaultHeaderSettings values of
  Right (IfNoneMatchTags tags) -> length tags
  _ -> error "invalid If-None-Match benchmark fixture"


small :: [(CI ByteString, ByteString)]
small = [("host", "example.org"), ("accept", "application/json"), ("cache-control", "max-age=10"), ("user-agent", "hermes-bench")]


repeated :: [(CI ByteString, ByteString)]
repeated = small <> [("cache-control", "no-cache"), ("Cache-Control", "no-transform"), ("cache-control", "max-age=20")]


linkFixture :: NonEmpty BS.ByteString
linkFixture = "</assets/main.css?v=2>; rel=preload; as=style; title=\"Main, \\\"dark\\\"\"" :| []


linkRepeatedFixture :: NonEmpty BS.ByteString
linkRepeatedFixture =
  NE.head linkFixture
    :| ["<https://[2001:db8::1]/about%20us#info>; rel=alternate; hreflang=en", "<../page,2>; rel=next"]


linkScore :: NonEmpty BS.ByteString -> Int
linkScore = foldl' scoreLink 0 . Link.linkValues . parseBenchmarkHeader @Link.Link
  where
    scoreLink n (Link.LinkValue target params) = foldl' scoreParam (n + shortBytes target) params
    scoreParam n (Link.LinkParam name value) = n + shortBytes name + maybe 0 scoreValue value
    scoreValue (Link.LinkParamToken token) = shortBytes token
    scoreValue (Link.LinkParamQuoted text) = shortBytes text


preferFixture, preferRepeatedFixture, appliedFixture :: NonEmpty BS.ByteString
preferFixture = "respond-async, return=minimal;reason=\"user, \\\"requested\\\"\", wait=10" :| []
preferRepeatedFixture = "respond-async, return=minimal" :| ["handling=lenient;flag", "wait=10"]
appliedFixture = "respond-async, return=minimal" :| []


preferScore :: NonEmpty BS.ByteString -> Int
preferScore = foldl' score 0 . Prefer.preferPreferences . parseBenchmarkHeader @Prefer.Prefer
  where
    score n (Prefer.Preference name value params) =
      foldl' (\acc (key, arg) -> acc + shortBytes key + maybe 0 shortBytes arg) (n + shortBytes name + maybe 0 shortBytes value) params


appliedScore :: NonEmpty BS.ByteString -> Int
appliedScore = foldl' score 0 . Applied.appliedPreferences . parseBenchmarkHeader @Applied.PreferenceApplied
  where
    score n (Applied.AppliedPreference name value _) = n + shortBytes name + maybe 0 shortBytes value


expectFixture :: NonEmpty BS.ByteString
expectFixture = "100-continue, extension=\"upload\";hint=\"a,b\";mode=fast" :| []


expectScore :: NonEmpty BS.ByteString -> Int
expectScore fields = case parseBenchmarkHeader @Expect.Expect fields of
  Expect.Expect es -> foldl' score 0 es
  Expect.ExpectEmpty -> 0
  where
    score n (Expect.Expectation name value params) =
      foldl' (\acc (key, arg) -> acc + shortBytes key + maybe 0 shortBytes arg) (n + shortBytes name + maybe 0 shortBytes value) params


altSvcFixture :: NonEmpty BS.ByteString
altSvcFixture = "h3=\":443\"; ma=3600, h2=\"alt.example.net:443\"; persist=\"1\"" :| []


altSvcScore :: NonEmpty BS.ByteString -> Int
altSvcScore fields = case parseBenchmarkHeader @AltSvc.AltSvc fields of
  AltSvc.AltSvcClear -> 0
  AltSvc.AltSvcValues values -> foldl' score 0 values
  where
    score n (AltSvc.AltValue name (AltSvc.AltAuthority host port) params) =
      foldl' parameter (n + shortBytes name + maybe 0 shortBytes host + fromIntegral port) params
    parameter n (AltSvc.AltParameter name value) = n + shortBytes name + case value of
      AltSvc.AltParamToken token -> shortBytes token
      AltSvc.AltParamQuoted word -> shortBytes word


robotsFixture :: NonEmpty BS.ByteString
robotsFixture = "googlebot: max-snippet: 50, nofollow" :| []


robotsScore :: NonEmpty BS.ByteString -> Int
robotsScore fields =
  let value = parseBenchmarkHeader @Robots.XRobotsTag fields
  in foldl' score (maybe 0 shortBytes (Robots.xRobotsBotName value)) (Robots.xRobotsDirectives value)
  where
    score n (Robots.RobotsDirective name value) = n + shortBytes name + maybe 0 shortBytes value


mailboxBareFixture, mailboxNamedFixture :: NonEmpty BS.ByteString
mailboxBareFixture = "spider-admin@example.org" :| []
mailboxNamedFixture = "\"Crawler, Support\" <\"spider admin\"@example.org>" :| []


mailboxScore :: NonEmpty BS.ByteString -> Int
mailboxScore fields = case From.fromMailbox (parseBenchmarkHeader @From.From fields) of
  Mailbox.Mailbox name (Mailbox.AddrSpec local domain) ->
    maybe 0 shortBytes name + shortBytes local + case domain of
      Mailbox.DomainName value -> shortBytes value
      Mailbox.DomainLiteral value -> shortBytes value


methodOverrideScore :: NonEmpty BS.ByteString -> Int
methodOverrideScore = BS.length . Methods.methodToBytes . MethodOverride.xHttpMethodOverrideMethod . parseBenchmarkHeader @MethodOverride.XHttpMethodOverride


publicScore :: NonEmpty BS.ByteString -> Int
publicScore = foldl' (\total token -> total + shortBytes token) 0 . Public.publicMethods . parseBenchmarkHeader


ediintScore :: NonEmpty BS.ByteString -> Int
ediintScore = foldl' (\total token -> total + shortBytes token) 0 . EDIINT.ediintFeatures . parseBenchmarkHeader


safeScore :: NonEmpty BS.ByteString -> Bool
safeScore fields = parseBenchmarkHeader @Safe.Safe fields == Safe.SafeYes


additionsFixture :: NonEmpty BS.ByteString
additionsFixture = "Cream, Whisky;q=0.7;name=\"dry, oak\"" :| []


additionsScore :: NonEmpty BS.ByteString -> Int
additionsScore = foldl' addition 0 . Additions.acceptAdditions . parseBenchmarkHeader
  where
    addition n (Additions.Addition name params) = foldl' parameter (n + shortBytes name) params
    parameter n (Additions.AdditionParam name value) = n + shortBytes name + maybe 0 shortBytes value


uaCompatibleScore :: NonEmpty BS.ByteString -> Int
uaCompatibleScore = foldl' directive 0 . UA.uaCompatDirectives . parseBenchmarkHeader
  where
    directive n (UA.UACompatDirective engine mode) = n + shortBytes engine + shortBytes mode


uaDocumentModeScore :: UA.UACompatDirective -> Word
uaDocumentModeScore directive = case UA.ieDocumentMode directive of
  Just UA.IEEdge -> 1
  Just (UA.IEEmulate version) -> version
  Just (UA.IEVersion version) -> version
  Nothing -> 0


cacheControlScore :: HeaderMap -> Int
cacheControlScore headers = case lookupHeader @CacheControl headers of
  Right (Just (CacheControl directives)) -> foldl' count 0 directives
  _ -> error "invalid Cache-Control benchmark fixture"
  where
    count n (MaxAge age) = n + fromIntegral age
    count n _ = n + 1


cacheDirectiveScore :: ByteString -> Word
cacheDirectiveScore value = case parseCacheControlHeader value of
  Right directives -> scoreCacheDirectives directives
  Left err -> error err


pragmaFixture :: NonEmpty ByteString
pragmaFixture = "no-cache, debug=\"a,b\"" :| ["foo=bar"]


warningFixture :: NonEmpty ByteString
warningFixture = "110 edge \"Response is stale\"" :| ["112 [::1]:8080 \"Disconnected\" \"Sun, 06 Nov 1994 08:49:37 GMT\""]


pragmaScore :: NonEmpty ByteString -> Int
pragmaScore = foldl' score 0 . Pragma.pragmaDirectives . parseBenchmarkHeader @Pragma.Pragma
  where
    score total directive = total + shortBytes (Pragma.pragmaName directive) + case Pragma.pragmaValue directive of
      Nothing -> 0
      Just (Pragma.PragmaToken value) -> shortBytes value
      Just (Pragma.PragmaQuoted (RFC8941String value)) -> shortBytes value


warningScore :: NonEmpty ByteString -> Int
warningScore = foldl' score 0 . Warning.warningValues . parseBenchmarkHeader @Warning.Warning
  where
    score total (Warning.WarningValue code agent (RFC8941String message) date) =
      total + fromIntegral code + shortBytes agent + shortBytes message + maybe 0 (const 1) date


headerByteCount :: HeaderFieldName -> HeaderMap -> Int
headerByteCount name = maybe 0 (foldl' (\total bytes -> total + BS.length bytes) 0) . lookupRawHeader name
{-# INLINE headerByteCount #-}


cdnDirectiveScore :: NonEmpty ByteString -> Word
cdnDirectiveScore fields = case parseFromHeaders @CDN.CDNCacheControl defaultHeaderSettings fields of
  Right value -> scoreCacheDirectives (CDN.cdnCacheControlDirectiveList value)
  Left err -> error err


priorityScore :: NonEmpty ByteString -> Int
priorityScore fields = case parseFromHeaders @Priority.Priority defaultHeaderSettings fields of
  Right (Priority.Priority urgency incremental) -> urgency + fromEnum incremental
  Left err -> error err


contentDigestScore :: NonEmpty ByteString -> Int
contentDigestScore fields = case parseFromHeaders @Digest.ContentDigest defaultHeaderSettings fields of
  Right value -> foldl' (\n entry -> n + BS.length (Digest.contentDigestValue entry)) 0 (Digest.contentDigestEntryList value)
  Left err -> error err


legacyDigestFixture :: ByteString
legacyDigestFixture = "sha-256=X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE="


wantDigestFixture :: NonEmpty ByteString
wantDigestFixture = "sha-256, md5;q=0.3" :| ["sha-512;q=1.000, contentMD5;q=0"]


legacyDigestScore :: NonEmpty ByteString -> Int
legacyDigestScore = foldl' score 0 . LegacyDigest.digestValues . parseBenchmarkHeader @LegacyDigest.Digest
  where
    score total entry = total + shortBytes (LegacyDigest.digestAlgorithm entry) + shortBytes (LegacyDigest.digestEncoded entry)


wantDigestScore :: NonEmpty ByteString -> Int
wantDigestScore = foldl' score 0 . WantDigest.wantDigestValues . parseBenchmarkHeader @WantDigest.WantDigest
  where
    score total entry = total + shortBytes (WantDigest.wantDigestAlgorithm entry) + maybe 0 shortBytes (WantDigest.wantDigestQ entry)


signatureFixture, signatureInputFixture, acceptSignatureFixture, reportingFixture :: NonEmpty ByteString
signatureFixture = "sig1=:X5spyd6CFnAG5QnDyHfqoSNICd+BUP4LYMz2Q0JXlb//4Ijpzp+kve2w4NIyqeAuM7jTDX+sNalzA8ESSaHD3A==:" :| []
signatureInputFixture = "sig1=(\"@method\" \"@authority\" \"@path\" \"content-digest\" \"content-type\" \"content-length\");created=1618884475;keyid=\"test-key-ecc-p256\"" :| []
acceptSignatureFixture = "sig1=(\"@method\" \"@target-uri\" \"@authority\" \"content-digest\" \"cache-control\");keyid=\"test-key-rsa-pss\";created;tag=\"app-123\"" :| []
reportingFixture = "default=\"https://reports.example/default\", backup=\"https://reports.example/backup\"" :| []


nelFixture :: NonEmpty ByteString
nelFixture = "{\"report_to\":\"default\",\"max_age\":86400,\"include_subdomains\":true}" :| []


nelAuthoringFixture :: NEL.NELPolicy
nelAuthoringFixture = NEL.NELRegister (NEL.NELRegistration 86400 "default" True 0 1 [] [])


signatureInputRenderFixture :: SignatureInput.SignatureInput
signatureInputRenderFixture =
  SignatureInput.SignatureInput
    ( SignatureInput.SignatureInputEntry
        "sig1"
        (map (\name -> SignatureInput.SignatureComponent (RFC8941String name) []) names)
        [("created", Just (Integer 1618884475)), ("keyid", Just (String (RFC8941String "test-key-ecc-p256")))]
        :| []
    )
  where
    names = ["@method", "@authority", "@path", "content-digest", "content-type", "content-length"]


parseBenchmarkHeader :: (KnownHeader a, ParseFailure a ~ String) => NonEmpty ByteString -> a
parseBenchmarkHeader = either error id . parseFromHeaders defaultHeaderSettings
{-# INLINE parseBenchmarkHeader #-}


cspFixture :: NonEmpty ByteString
cspFixture = "default-src 'none'; script-src 'self' https://cdn.example.com; img-src https: data:; report-to endpoint" :| []


cspListFixture :: NonEmpty ByteString
cspListFixture =
  "Default-Src 'none'; default-src https:; script-src 'self' https://cdn.example.com"
    :| ["img-src https: data:; report-to endpoint, frame-ancestors 'none'"]


cspScore :: NonEmpty ByteString -> Int
cspScore = foldl' (\n policy -> n + cspDirectiveScore (CSP.policyDirectives policy)) 0 . CSP.cspPolicies . parseBenchmarkHeader


cspDirectiveScore :: [CSP.Directive] -> Int
cspDirectiveScore = foldl' (\n d -> n + shortBytes (CSP.directiveName d) + foldl' (\k v -> k + shortBytes v) 0 (CSP.directiveValues d)) 0


aimScore :: NonEmpty ByteString -> Double
aimScore = foldl' (\n value -> n + fromIntegral (shortBytes (AIM.imTag value)) + AIM.imWeight value) 0 . AIM.acceptInstanceManipulations . parseBenchmarkHeader


imScore :: NonEmpty ByteString -> Int
imScore = foldl' (\n value -> n + shortBytes (IM.manipulationName value)) 0 . IM.instanceManipulationEntries . parseBenchmarkHeader


aimExtensionFixture, imExtensionFixture :: NonEmpty ByteString
aimExtensionFixture = "vcdiff;flavor=\"a,b\";q=0.125" :| ["gzip, identity;q=0"]
imExtensionFixture = "ext;flavor=\"a,b\"" :| ["gzip, range;raw=DATA"]


aimExtensionScore :: NonEmpty ByteString -> Double
aimExtensionScore = foldl' score 0 . AIM.acceptInstanceManipulations . parseBenchmarkHeader
  where
    score n value = n + fromIntegral (shortBytes (AIM.imTag value) + imParameterScore (AIM.weightedIMParameter value)) + AIM.imWeight value


imExtensionScore :: NonEmpty ByteString -> Int
imExtensionScore = foldl' score 0 . IM.instanceManipulationEntries . parseBenchmarkHeader
  where
    score n value = n + shortBytes (IM.manipulationName value) + imParameterScore (IM.manipulationParameter value)


imParameterScore :: Maybe IM.IMParameter -> Int
imParameterScore Nothing = 0
imParameterScore (Just (IM.IMParameter name value)) =
  shortBytes name + case value of
    Nothing -> 0
    Just (IM.IMParameterToken token) -> shortBytes token
    Just (IM.IMParameterQuoted bytes) -> SBS.length bytes


dpopBenchmarkField :: NonEmpty ByteString
dpopBenchmarkField =
  (BS.replicate 140 0x41 <> "." <> BS.replicate 130 0x42 <> "." <> BS.replicate 86 0x43)
    :| []


legacyUriScore :: NonEmpty ByteString -> Int
legacyUriScore = foldl' (\total uri -> total + shortBytes uri) 0 . LegacyURI.uriList . parseBenchmarkHeader


signatureScore :: NonEmpty ByteString -> Int
signatureScore = foldl' (\total (_, bytes) -> total + BS.length bytes) 0 . Signature.signatureValues . parseBenchmarkHeader


signatureInputScore :: NonEmpty ByteString -> Int
signatureInputScore = foldl' count 0 . SignatureInput.signatureInputEntries . parseBenchmarkHeader
  where
    count total entry = total + length (SignatureInput.signatureComponents entry) + length (SignatureInput.signatureParameters entry)


acceptSignatureScore :: NonEmpty ByteString -> Int
acceptSignatureScore = foldl' count 0 . AcceptSignature.acceptSignatureEntries . parseBenchmarkHeader
  where
    count total entry = total + length (AcceptSignature.signatureComponents entry) + length (AcceptSignature.signatureParameters entry)


reportingScore :: NonEmpty ByteString -> Int
reportingScore = length . Reporting.reportingEndpoints . parseBenchmarkHeader


cacheStatusFixture, proxyStatusFixture :: NonEmpty ByteString
cacheStatusFixture = "ReverseProxy;hit;ttl=120" :| ["\"shared cache\";fwd=stale;fwd-status=304;stored;detail=MEMORY"]
proxyStatusFixture = "edge;next-protocol=h2;received-status=200" :| ["\"upstream\";next-hop=backend.example:443;error=connection_timeout;details=\"upstream timeout\""]


certificateFixture :: ByteString
certificateFixture = ":" <> BS.concat (replicate 250 "ABEi") <> ":"


cacheStatusScore :: NonEmpty ByteString -> Int
cacheStatusScore = either (error . show) (foldl' score 0 . CacheStatus.cacheStatus) . parseFromHeaders defaultHeaderSettings
  where
    score total entry =
      total + case CacheStatus.cacheStatusHitOrForward entry of
        Left _ -> maybe 0 id (CacheStatus.cacheStatusTtl (CacheStatus.cacheStatusCommonFields entry))
        Right forwarded -> maybe 0 id (CacheStatus.forwardStatus forwarded)


proxyStatusScore :: NonEmpty ByteString -> Int
proxyStatusScore = foldl' (\total entry -> total + length (ProxyStatus.proxyParams entry)) 0 . ProxyStatus.proxyStatusEntries . parseBenchmarkHeader


typedProxyStatusScore :: NonEmpty ByteString -> Int
typedProxyStatusScore = foldl' score 0 . ProxyStatus.proxyStatusEntries . parseBenchmarkHeader
  where
    score total = (total +) . maybe 0 id . ProxyStatus.proxyReceivedStatus . either error id . ProxyStatus.proxyInfoParameters


clientCertScore :: NonEmpty ByteString -> Int
clientCertScore = BS.length . ClientCert.clientCertBytes . parseBenchmarkHeader


clientCertChainScore :: NonEmpty ByteString -> Int
clientCertChainScore = foldl' (\total cert -> total + BS.length cert) 0 . ClientCertChain.clientCertChain . parseBenchmarkHeader


certValidityScore :: NonEmpty ByteString -> UTCTime
certValidityScore = CertNotAfter.certNotAfterTime . parseBenchmarkHeader


corsAllowHeadersScore :: NonEmpty ByteString -> Int
corsAllowHeadersScore fields = case parseBenchmarkHeader fields of
  CorsAllowHeaders.AllowHeaders names -> foldl' (\total name -> total + shortBytes name) 0 names
  CorsAllowHeaders.AllowHeadersWildcard -> 1
  CorsAllowHeaders.AllowHeadersEmpty -> 0


corsRequestMethodScore :: NonEmpty ByteString -> Int
corsRequestMethodScore = shortBytes . CorsRequestMethod.requestMethod . parseBenchmarkHeader


corsMaxAgeScore :: NonEmpty ByteString -> Int
corsMaxAgeScore = CorsMaxAge.maxAgeSeconds . parseBenchmarkHeader


timingAllowOriginScore :: NonEmpty ByteString -> Int
timingAllowOriginScore = foldl' score 0 . TimingAllowOrigin.timingAllowOriginItems . parseBenchmarkHeader
  where
    score total TimingAllowOrigin.TimingAllowOriginAny = total + 1
    score total (TimingAllowOrigin.TimingAllowOriginOrigin origin) = total + shortBytes origin


rangeScore :: NonEmpty ByteString -> Word64
rangeScore fields = case parseBenchmarkHeader fields of
  Range.ByteRanges ranges -> foldl' score 0 ranges
  Range.OtherRanges unit ranges -> fromIntegral (shortBytes unit + foldl' rawSize 0 ranges)
  where
    score n (Range.ByteRangeInt first lastByte) = n + first + maybe 0 id lastByte
    score n (Range.ByteRangeSuffix len) = n + len
    rawSize n = (n +) . BS.length . Range.rawRangeBytes


contentRangeScore :: NonEmpty ByteString -> Word64
contentRangeScore fields = case parseBenchmarkHeader fields of
  ContentRange.ContentRange unit response -> fromIntegral (shortBytes unit) + score response
  where
    score (ContentRange.RangeRespSatisfied first lastByte total) = first + lastByte + maybe 0 id total
    score (ContentRange.RangeRespUnsatisfied total) = maybe 0 id total


webSocketKeySize :: NonEmpty ByteString -> Int
webSocketKeySize = BS.length . WebSocketKey.secWebSocketKeyBytes . parseBenchmarkHeader


webSocketAcceptSize :: NonEmpty ByteString -> Int
webSocketAcceptSize = BS.length . WebSocketAccept.secWebSocketAcceptBytes . parseBenchmarkHeader


webSocketVersionScore :: NonEmpty ByteString -> Word
webSocketVersionScore = sum . WebSocketVersion.secWebSocketVersions . parseBenchmarkHeader


webSocketProtocolScore :: NonEmpty ByteString -> Int
webSocketProtocolScore = foldl' (\n protocol -> n + shortBytes protocol) 0 . WebSocketProtocol.secWebSocketProtocols . parseBenchmarkHeader


webSocketExtensionsScore :: NonEmpty ByteString -> Int
webSocketExtensionsScore = foldl' extensionSize 0 . WebSocketExtensions.extensions . parseBenchmarkHeader
  where
    extensionSize n (WebSocketExtensions.Extension token params) = foldl' paramSize (n + shortBytes token) params
    paramSize n (WebSocketExtensions.ExtensionParam name value) = n + shortBytes name + maybe 0 shortBytes value


positionScore :: NonEmpty ByteString -> Int
positionScore fields = case parseBenchmarkHeader fields of
  Position.PositionFirst -> 0
  Position.PositionLast -> 1
  Position.PositionBefore path -> shortBytes path
  Position.PositionAfter path -> shortBytes path


statusUriScore :: NonEmpty ByteString -> Int
statusUriScore = foldl' count 0 . StatusURI.statusURIEntries . parseBenchmarkHeader
  where
    count acc (StatusURI.StatusURIEntry code uri) = acc + code + shortBytes uri


depthScore :: NonEmpty ByteString -> Int
depthScore fields = case parseBenchmarkHeader fields of
  Depth.Depth0 -> 0
  Depth.Depth1 -> 1
  Depth.DepthInfinity -> 2


davIfFixture :: NonEmpty ByteString
davIfFixture = "</docs/a> (<urn:uuid:1234> Not [W/\"rev2\"]) ( [\"rev3\"] )" :| []


davIfTree :: DAVIf.IfConditionLists
davIfTree = either error id (DAVIf.ifConditionLists (parseBenchmarkHeader davIfFixture))


timeoutScore :: NonEmpty ByteString -> Word
timeoutScore = foldl' add 0 . Timeout.timeoutValues . parseBenchmarkHeader
  where
    add n (Timeout.TimeSeconds seconds) = n + seconds
    add n Timeout.TimeInfinite = n + 1


davScore :: NonEmpty ByteString -> Int
davScore = foldl' add 0 . DAV.davClasses . parseBenchmarkHeader
  where
    add n (DAV.ComplianceToken token) = n + shortBytes token
    add n (DAV.ComplianceURL uri) = n + shortBytes uri


davIfTreeScore :: NonEmpty ByteString -> Int
davIfTreeScore inputs =
  case DAVIf.ifConditionLists (parseBenchmarkHeader inputs) of
    Right (DAVIf.IfUntagged lists) -> foldl' addList 0 lists
    Right (DAVIf.IfTagged resources) -> foldl' addResource 0 resources
    Left message -> error message
  where
    addList n (DAVIf.IfList conditions) = foldl' addCondition n conditions
    addResource n (DAVIf.IfResource uri lists) = foldl' addList (n + shortBytes uri) lists
    addCondition n (DAVIf.IfCondition negated operand) =
      n + fromEnum negated + case operand of
        DAVIf.IfStateToken uri -> shortBytes uri
        DAVIf.IfEntityTag tag -> SBS.length (ETag.entityTagBytes tag)


forwardedScore :: NonEmpty ByteString -> Int
forwardedScore = foldl' element 0 . Forwarded.forwardedElements . parseBenchmarkHeader
  where
    element n = foldl' pair n . Forwarded.forwardedPairs
    pair n (name, value) =
      n + shortBytes name + case value of
        Forwarded.ForwardedToken t -> shortBytes t
        Forwarded.ForwardedQuoted t -> shortBytes t


xffScore :: NonEmpty ByteString -> Int
xffScore = foldl' (\n node -> n + shortBytes node) 0 . XFF.xForwardedForNodes . parseBenchmarkHeader


xProtoScore :: NonEmpty ByteString -> Int
xProtoScore fields = case parseBenchmarkHeader fields of
  XProto.XForwardedProtoHttp -> 4
  XProto.XForwardedProtoHttps -> 5
  XProto.XForwardedProtoOther t -> shortBytes t


realIPScore :: NonEmpty ByteString -> Int
realIPScore fields = case XRealIP.xRealIPAddress (parseBenchmarkHeader fields) of
  IPv4Address (IPv4 a b c d) -> sum (map fromIntegral [a, b, c, d])
  _ -> 0


pinFixture :: ByteString
pinFixture = "pin-sha256=\"E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=\"; pin-sha256=\"LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=\""


hstsScore :: NonEmpty ByteString -> Word
hstsScore fields =
  let HSTS.StrictTransportSecurity age subdomains preload = parseBenchmarkHeader fields
  in age + fromIntegral (fromEnum subdomains + fromEnum preload)


expectCTScore :: NonEmpty ByteString -> Word
expectCTScore fields =
  let ExpectCT.ExpectCT age enforce uri = parseBenchmarkHeader fields
  in age + fromIntegral (fromEnum enforce + maybe 0 shortBytes uri)


pinPolicyScore :: NonEmpty ByteString -> Word
pinPolicyScore fields =
  let PKP.PublicKeyPins pins age subdomains uri = parseBenchmarkHeader fields
      size n (PKP.Pin alg fingerprint) = n + shortBytes alg + shortBytes fingerprint
  in age + fromIntegral (foldl' size (fromEnum subdomains + maybe 0 shortBytes uri) pins)


reportOnlyScore :: NonEmpty ByteString -> Word
reportOnlyScore fields =
  let PKPRO.PublicKeyPinsReportOnly pins age subdomains uri = parseBenchmarkHeader fields
      size n (PKPRO.Pin alg fingerprint) = n + shortBytes alg + shortBytes fingerprint
  in age + fromIntegral (foldl' size (fromEnum subdomains + maybe 0 shortBytes uri) pins)


timingFixture :: ByteString
timingFixture = "db;dur=53.2, cache;desc=\"Cache hit\""


cdnLoopFixture :: ByteString
cdnLoopFixture = "edge.example, upstream;k=value"


serverTimingScore :: NonEmpty ByteString -> Int
serverTimingScore = foldl' addMetric 0 . ServerTiming.serverTimingMetrics . parseBenchmarkHeader
  where
    addMetric bytes (ServerTiming.ServerTimingMetric name params) = foldl' addParameter (bytes + shortBytes name) params
    addParameter bytes (ServerTiming.ServerTimingParam name value) = bytes + shortBytes name + shortBytes value


cdnLoopScore :: NonEmpty ByteString -> Int
cdnLoopScore = foldl' addInfo 0 . CDNLoop.cdnLoopEntries . parseBenchmarkHeader
  where
    addInfo bytes (CDNLoop.CDNInfo name params) = foldl' (\n (key, value) -> n + shortBytes key + shortBytes value) (bytes + shortBytes name) params


traceparentFixture :: ByteString
traceparentFixture = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"


traceparentScore :: NonEmpty ByteString -> Int
traceparentScore fields =
  let value = parseBenchmarkHeader fields
  in fromIntegral (Traceparent.traceparentVersion value)
       + shortBytes (Traceparent.traceparentTraceId value)
       + shortBytes (Traceparent.traceparentParentId value)
       + fromIntegral (Traceparent.traceparentFlags value)


tracestateScore :: NonEmpty ByteString -> Int
tracestateScore = foldl' memberSize 0 . Tracestate.tracestateMembers . parseBenchmarkHeader
  where
    memberSize n (Tracestate.TracestateMember key value) = n + shortBytes key + shortBytes value


alpnBytes :: NonEmpty ByteString -> Int
alpnBytes = foldl' (\n -> (n +) . BS.length . TLS.alpnIdentificationSequence) 0 . ALPN.alpnProtocols . parseBenchmarkHeader


surrogateCapabilityFixture :: NonEmpty ByteString
surrogateCapabilityFixture = "abc=\"Surrogate/1.0\", def=\"Surrogate/1.0 ESI/1.0\"" :| []


surrogateCapabilityScore :: NonEmpty ByteString -> Int
surrogateCapabilityScore =
  foldl' (\n entry -> n + shortBytes (SurrogateCapability.surrogateCapabilityDevice entry) + shortBytes (SurrogateCapability.surrogateCapabilitySet entry)) 0
    . SurrogateCapability.surrogateCapabilityEntries
    . parseBenchmarkHeader


surrogateControlFixture :: NonEmpty ByteString
surrogateControlFixture = "max-age=300+60, no-store, content=\"ESI/1.0\", extension=\"a,b\"" :| []


surrogateControlScore :: NonEmpty ByteString -> Int
surrogateControlScore =
  foldl' (\n directive -> n + shortBytes (SurrogateControl.surrogateControlName directive) + maybe 0 shortBytes (SurrogateControl.surrogateControlValue directive)) 0
    . SurrogateControl.surrogateControlDirectives
    . parseBenchmarkHeader


meterScore :: NonEmpty ByteString -> Int
meterScore =
  foldl' (\n directive -> n + shortBytes (Meter.meterName directive) + maybe 0 shortBytes (Meter.meterValue directive)) 0
    . Meter.meterDirectives
    . parseBenchmarkHeader


variantVaryScore :: NonEmpty ByteString -> Int
variantVaryScore fields = case parseBenchmarkHeader @VariantVary.VariantVary fields of
  VariantVary.VariantVaryAll -> 0
  VariantVary.VariantVaryFields names -> NE.length names


shortBytes :: ST.ShortText -> Int
shortBytes = SBS.length . ST.toShortByteString


reportToBytes :: Maybe RFC8941String -> Int
reportToBytes = maybe 0 (\(RFC8941String value) -> shortBytes value)


frameOptionsScore :: NonEmpty ByteString -> Int
frameOptionsScore fields = case parseBenchmarkHeader fields of
  XFO.AllowFrom origin -> shortBytes origin
  _ -> 0


xxssReportScore :: NonEmpty ByteString -> Int
xxssReportScore fields = case parseBenchmarkHeader fields of
  XXSS.XXSSDisabled -> 0
  XXSS.XXSSEnabled _ endpoint -> maybe 0 shortBytes endpoint


permissionsPolicyScore :: NonEmpty ByteString -> Int
permissionsPolicyScore = foldl' (\total (feature, allowed) -> total + shortBytes feature + length allowed) 0 . PermissionsPolicy.permissionsPolicyDirectives . parseBenchmarkHeader


permissionsReportingScore :: NonEmpty ByteString -> Int
permissionsReportingScore = foldl' add 0 . PermissionsPolicy.permissionsPolicyDeclarations . parseBenchmarkHeader
  where
    add total declaration = total + maybe 0 length (PermissionsPolicy.permissionsPolicyAllowlist declaration) + maybe 0 (const 1) (PermissionsPolicy.permissionsPolicyReportTo declaration)


prefetchPrerenderScore :: NonEmpty ByteString -> Bool
prefetchPrerenderScore = any SecPurpose.prefetchPrerender . SecPurpose.secPurposePrefetches . parseBenchmarkHeader


scoreCacheDirectives :: Foldable t => t CacheControlDirective -> Word
scoreCacheDirectives = foldl' count 0
  where
    count n (MaxAge seconds) = n + seconds
    count n (StaleWhileRevalidate seconds) = n + seconds
    count n (Private (Just names)) = n + fromIntegral (BS.length (ST.toByteString names))
    count n _ = n + 1


acceptScore :: ByteString -> Double
acceptScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Accept ranges) -> foldl' (\n entry -> n + mediaWeight entry) 0 ranges
  Left _ -> error "invalid Accept benchmark fixture"


languageScore :: ByteString -> Double
languageScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (AcceptLanguage ranges) -> foldl' (\score entry -> score + languageWeight entry) 0 ranges
  Left _ -> error "invalid Accept-Language benchmark fixture"


contentTypeScore :: ByteString -> Int
contentTypeScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right content -> foldl' (\n (_, parameter) -> n + SBS.length (ST.toShortByteString parameter)) 0 (contentTypeParameters content)
  Left _ -> error "invalid Content-Type benchmark fixture"


dispositionScore :: ByteString -> Int
dispositionScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (ContentDisposition kind parameters) ->
    foldl' (\n (key, parameter) -> n + bytes key + bytes parameter) (bytes kind) parameters
  Left _ -> error "invalid Content-Disposition benchmark fixture"
  where
    bytes = SBS.length . ST.toShortByteString


dispositionFilenameScore :: ContentDisposition -> Int
dispositionFilenameScore = maybe 0 Text.length . contentDispositionFilename


exampleDisposition :: ContentDisposition
exampleDisposition = ContentDisposition "attachment" [("filename", "report 2026.txt"), ("filename*", "UTF-8'en-GB'%E2%82%AC+report.txt")]


contentLanguageScore :: ByteString -> Int
contentLanguageScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right tags -> foldl' (\n tag -> n + SBS.length (ST.toShortByteString (unLanguageTag tag))) 0 (contentLanguageTagList tags)
  Left _ -> error "invalid Content-Language benchmark fixture"


codingScore :: ByteString -> Bool
codingScore value = case runParser contentCodingParser value of
  OK GZip "" -> True
  _ -> False


locationBytes :: ByteString -> Int
locationBytes value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Location uri) -> SBS.length (ST.toShortByteString uri)
  Left failure -> error failure


cookieBytes :: NonEmpty ByteString -> Int
cookieBytes values = case parseFromHeaders defaultHeaderSettings values of
  Right (Cookie pairs) -> foldl' countPair 0 pairs
  Left failure -> error failure
  where
    countPair n (CookiePair name value) =
      n + SBS.length (ST.toShortByteString name) + SBS.length (ST.toShortByteString value)


exampleCookie :: Cookie
exampleCookie =
  Cookie [CookiePair "session" "0123456789abcdef", CookiePair "locale" "en-US", CookiePair "flags" "new_ui"]


setCookieScore :: ByteString -> Int
setCookieScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (SetCookies [cookie]) ->
    let measure = SBS.length . ST.toShortByteString
    in measure (setCookieName cookie)
         + measure (setCookieValue cookie)
         + maybe 0 measure (setCookieDomain cookie)
         + maybe 0 measure (setCookiePath cookie)
         + maybe 0 id (setCookieMaxAge cookie)
         + maybe 0 (fromInteger . toModifiedJulianDay . utctDay) (setCookieExpires cookie)
         + fromEnum (setCookieSecure cookie)
         + fromEnum (setCookieHttpOnly cookie)
         + fromEnum (setCookieSameSite cookie == Just SameSiteLax)
  _ -> error "invalid Set-Cookie benchmark fixture"


exampleSetCookie :: SetCookie
exampleSetCookie =
  SetCookie "session" "abc" Nothing Nothing (Just "example.org") Nothing True True (Just SameSiteLax)


selectEncoding :: NonEmpty ByteString -> Maybe ContentCoding
selectEncoding fields =
  AcceptEncoding.preferredContentCoding (Just (parseBenchmarkHeader fields)) [Identity, GZip, Brotli, ZStd]


setCookieExtensionFixture :: ByteString
setCookieExtensionFixture = "session=abc; Secure; Partitioned; Priority=High; future=one two"


setCookieExtensionScore :: ByteString -> Int
setCookieExtensionScore bytes =
  case parseBenchmarkHeader @SetCookies (bytes :| []) of
    SetCookies [cookie] -> sum (map (shortBytes . setCookieExtensionText) (setCookieExtensions cookie))
    _ -> error "Expected one Set-Cookie field"


refererBytes :: ByteString -> Int
refererBytes value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Referer uri) -> SBS.length (ST.toShortByteString uri)
  Left failure -> error failure


pingBytes :: ByteString -> Int
pingBytes value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (PingTo uri) -> length uri
  Left failure -> error failure


codingChainLength :: ByteString -> Int
codingChainLength value = codingChainLengthFromFields (value :| [])


codingChainLengthFromFields :: NonEmpty ByteString -> Int
codingChainLengthFromFields fields = case parseFromHeaders @ContentEncoding defaultHeaderSettings fields of
  Right codings -> length (contentCodings codings)
  Left _ -> error "invalid Content-Encoding benchmark fixture"


dateScore :: ByteString -> UTCTime
dateScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Date stamp) -> stamp
  Left _ -> error "invalid Date benchmark fixture"


exampleDate :: Date
exampleDate = Date (UTCTime (fromGregorian 1994 11 6) (secondsToDiffTime 31777))


ageScore :: ByteString -> Word
ageScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Age seconds) -> fromIntegral seconds
  Left _ -> error "invalid Age benchmark fixture"


retryAfterScore :: ByteString -> Word
retryAfterScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (DelaySeconds seconds) -> seconds
  _ -> error "invalid Retry-After benchmark fixture"


largeRetryAfterScore :: ByteString -> Int
largeRetryAfterScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (LargeDelaySeconds delay) -> SBS.length (largeDelayDigits delay)
  _ -> error "invalid large Retry-After benchmark fixture"


allowScore :: NonEmpty ByteString -> Int
allowScore values = case parseFromHeaders defaultHeaderSettings values of
  Right (Allow methods) -> foldl' (\count method -> method `seq` count + 1) 0 methods
  Left _ -> error "invalid Allow benchmark fixture"


connectionScore :: NonEmpty ByteString -> Int
connectionScore values = case parseFromHeaders defaultHeaderSettings values of
  Right (Connection options) -> foldl' (\count option -> option `seq` count + 1) 0 options
  _ -> error "invalid Connection benchmark fixture"


keepAliveFixture :: NonEmpty ByteString
keepAliveFixture = "timeout=10" :| ["max=20, note=\"idle, \\\"warm\\\"\""]


keepAliveScore :: NonEmpty ByteString -> Int
keepAliveScore = foldl' score 0 . KeepAlive.keepAliveParams . parseBenchmarkHeader @KeepAlive.KeepAlive
  where
    score total (name, value) = total + shortBytes name + shortBytes value


keepAliveLimitsScore :: NonEmpty ByteString -> Word64
keepAliveLimitsScore fields =
  let value = parseBenchmarkHeader @KeepAlive.KeepAlive fields
      limit = either error (maybe 0 id)
  in limit (KeepAlive.keepAliveTimeout value) + limit (KeepAlive.keepAliveMaxRequests value)


varyScore :: NonEmpty ByteString -> Int
varyScore values = case parseFromHeaders defaultHeaderSettings values of
  Right (Vary rules) -> foldl' countRule 0 rules
  _ -> error "invalid Vary benchmark fixture"
  where
    countRule count VaryAll = count + 1
    countRule count (VaryHeader name) = name `seq` count + 1


agentScore :: ByteString -> Int
agentScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (UserAgent.UserAgent first elements) -> first `seq` productExtras elements
  Left _ -> error "invalid User-Agent benchmark fixture"


serverScore :: ByteString -> Int
serverScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Server.Server first elements) -> first `seq` productExtras elements
  Left _ -> error "invalid Server benchmark fixture"


productExtras :: [Either Comment product] -> Int
productExtras = foldl' (\count element -> count + either (Text.length . fromComment) (\product -> product `seq` 1) element) 0


exampleServer :: Server.Server
exampleServer =
  Server.Server
    (Server.Product "Example" (Just "2"))
    [Left (Comment "Unix build"), Right (Server.Product "TLS" (Just "1"))]


viaScore :: ByteString -> Int
viaScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Via.Via entries) -> foldl' (\count entry -> count + maybe 0 Text.length (Via.viaComment entry)) 0 entries
  _ -> error "invalid Via benchmark fixture"


contentLengthScore :: ByteString -> Word
contentLengthScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (ContentLength size) -> size
  Left _ -> error "invalid Content-Length benchmark fixture"


hostScore :: ByteString -> Word
hostScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Host host (Just port)) -> host `seq` fromIntegral port
  _ -> error "invalid Host benchmark fixture"


originScore :: ByteString -> Word
originScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (OriginHeader (Origin scheme host (Just port))) -> scheme `seq` host `seq` fromIntegral port
  _ -> error "invalid Origin benchmark fixture"


transferCodingCount :: ByteString -> Int
transferCodingCount value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Transfer.TransferEncoding names) -> foldl' (\count name -> name `seq` count + 1) 0 names
  _ -> error "invalid Transfer-Encoding benchmark fixture"


teScore :: ByteString -> Double
teScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (TE.TE codings) -> foldl' (\count coding -> count + maybe 1 id (TE.teCodingWeight coding)) 0 codings
  _ -> error "invalid TE benchmark fixture"


parameterizedTransferScore :: ByteString -> Int
parameterizedTransferScore value = case parseFromHeaders @Transfer.TransferEncoding defaultHeaderSettings (value :| []) of
  Right codingList -> sum (map (length . Transfer.transferCodingParameters) (Transfer.transferCodings codingList))
  Left _ -> error "invalid transfer extension benchmark fixture"


credentialScore :: ByteString -> Int
credentialScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (Auth.Authorization (Auth.Credentials _ (Auth.CredentialToken bytes))) -> BS.length bytes
  _ -> error "invalid credential benchmark fixture"


challengeScore :: ByteString -> Int
challengeScore value = case parseFromHeaders defaultHeaderSettings (value :| []) of
  Right (WWW.WWWAuthenticate challenges) -> foldl' count 0 challenges
  Left _ -> error "invalid challenge benchmark fixture"
  where
    count n (WWW.AuthChallenge _ (WWW.ChallengeParams params)) = n + length params
    count n (WWW.AuthChallenge _ (WWW.ChallengeToken68 bytes)) = n + BS.length bytes
    count n _ = n + 1


browserAccept :: Accept
browserAccept =
  Accept
    [ WeightedMediaRange (MediaRange (MediaType "text" "html") [("level", "1")]) 0.8
    , WeightedMediaRange (MediaRange (MediaType "application" "json") []) 1
    , WeightedMediaRange (MediaRange (MediaType "" "") []) 0.001
    ]


multipartContentType :: ContentType
multipartContentType =
  ParameterizedContentType (MediaType "multipart" "form-data") (("boundary", "Aa:B, 01") :| [])


featureWire :: ByteString
featureWire = "blex, !blebber, colordepth={5}, paper!=A2;mode=plain, *"


alternatesWire :: ByteString
alternatesWire =
  "{\"paper.1\" 0.9 {type text/html} {language en}}, "
    <> "{\"paper.2\" 0.7 {features !blink;-0.5 [blebber !wolx];+1.4-0.8}}, "
    <> "{\"fallback\"}, proxy-rvsa=\"1.0, 2.5\""


alternatesScore :: NonEmpty ByteString -> Int
alternatesScore =
  either error (foldl' add 0) . Alternates.alternateEntries . parseBenchmarkHeader
  where
    add score (Alternates.AlternateVariant variant) = score + 1 + length (Alternates.variantAttributes variant)
    add score (Alternates.AlternateProxyRVSA versions) = score + 1 + length versions
    add score _ = score + 1


tokenBindingWire :: ByteString
tokenBindingWire =
  "AIkAAgBBQFzK4_bhAqLDwRQxqJWte33d7hZ0hZWHwk-miKPg4E"
    <> "9fcgs7gBPoz-9RfuDfN9WCw6keHEw1ZPQMGs9CxpuHm-YAQM_j"
    <> "aOwwej6a-cQBGU7CJpUHOvXG4VvjNq8jDsvta9Y8_bPEPj25Gg"
    <> "mKiPjhJEtZA6mJ_9SNifLvVBTi7fR9wSAAAA"


legacyCookieWire :: ByteString
legacyCookieWire = "Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\"; Port=\"80,8000\"; Secure"


legacyCookieScore :: NonEmpty ByteString -> Int
legacyCookieScore = foldl' addLine 0 . SetCookie2.getSetCookie2s . parseBenchmarkHeader
  where
    addLine total line = either error (foldl' addCookie total) (SetCookie2.setCookie2Entries line)
    addCookie total cookie = total + BS.length (SetCookie2.cookie2Payload cookie) + length (SetCookie2.cookie2Attributes cookie)


legacyExpiryWire :: ByteString
legacyExpiryWire = "Wed, 21 Oct 2015 07:28:00 GMT"


scoreRefererRoot :: RefererRoot.RefererRootOrigin -> Int
scoreRefererRoot RefererRoot.RefererRootNull = 0
scoreRefererRoot (RefererRoot.RefererRootHost scheme host port) = BS.length scheme + BS.length host + fromIntegral port


proxyFeaturesScore :: NonEmpty ByteString -> Int
proxyFeaturesScore fields =
  case ProxyFeatures.proxyFeaturesDetails (parseBenchmarkHeader fields) of
    Left failure -> error failure
    Right (ProxyFeatures.ProxyFeaturesDetails name options) ->
      foldl' scoreFeature (BS.length name) options
  where
    scoreFeature count ProxyFeatures.ProxyNotify = count + 1
    scoreFeature count ProxyFeatures.ProxyLogging = count + 1
    scoreFeature count (ProxyFeatures.ProxyLog (ProxyFeatures.ProxyLogOffer uri options)) =
      foldl' scoreOption (count + BS.length uri) options
    scoreOption count (ProxyFeatures.ProxyLogExpiryHours hours) = count + fromIntegral hours
    scoreOption count (ProxyFeatures.ProxyLogPeakHours mask) = count + fromIntegral (ProxyFeatures.proxyPeakHoursMask mask)


picsRequestScore :: Int -> Legacy.ProtocolRequestEntry -> Int
picsRequestScore count (Legacy.ProtocolExtensionRequest directive) = legacyDirectiveScore count directive
picsRequestScore count (Legacy.ProtocolPicsRequest (Legacy.PicsLabelRequest completeness extensions services)) =
  foldl' extensionScore (foldl' (\n service -> n + BS.length service) (count + formatScore completeness) services) extensions
  where
    extensionScore n = foldl' item n
    item n (Legacy.PicsToken bytes) = n + BS.length bytes
    item n (Legacy.PicsQuotedName bytes) = n + BS.length bytes
    formatScore (Just (Legacy.PicsOtherCompleteness bytes)) = BS.length bytes
    formatScore value = maybe 0 (const 1) value


legacyDirectiveScore :: Int -> Legacy.LegacyDirective -> Int
legacyDirectiveScore count (Legacy.LegacyDirective ident attrs) = foldl' attr (count + nameSize ident) attrs
  where
    nameSize (Legacy.LegacyProtocolToken tag) = BS.length tag
    nameSize (Legacy.LegacyProtocolURI uri) = BS.length uri
    attr n (Legacy.LegacyScope scope) = case scope of
      Legacy.LegacyOrigin -> n + 1
      Legacy.LegacyConnection -> n + 2
    attr n (Legacy.LegacyStrength strength) = case strength of
      Legacy.LegacyOptional -> n + 1
      Legacy.LegacyRequired -> n + 2
      Legacy.LegacyRefused -> n + 3
    attr n (Legacy.LegacyHeaders names) = foldl' header n names
    attr n (Legacy.LegacyParameters items) = foldl' item n items
    attr n (Legacy.LegacyFor targets) = foldl' target n targets
    attr n (Legacy.LegacyVia via) = n + nameSize via
    attr n (Legacy.LegacyExtension entry) = item n entry
    header n (Legacy.LegacyHeaderName name) = n + BS.length name
    header n (Legacy.LegacyHeaderPrefix prefix) = n + BS.length prefix
    target n (Legacy.LegacyTarget uri prefix) = n + BS.length uri + fromEnum prefix
    item n (Legacy.LegacyToken bytes) = n + BS.length bytes
    item n (Legacy.LegacyQuoted bytes) = n + BS.length bytes
    item n (Legacy.LegacyBag name children) = foldl' item (n + nameSize name) children


ampFixture :: ByteString
ampFixture = "google;v=\"1..3,5\", any"


ampSelectionScore :: NonEmpty ByteString -> Word64
ampSelectionScore = maybe 0 AMP.ampSelectedVersion . selectAMPFixture


ampSelectedResponse :: NonEmpty ByteString -> ByteString
ampSelectedResponse = maybe "" render . selectAMPFixture
  where
    render selected = case renderToHeaders defaultHeaderSettings (either error id (AMP.ampCacheTransformFromSelection selected)) of
      [wire] -> wire
      _ -> error "Expected one AMP response field line"


selectAMPFixture :: NonEmpty ByteString -> Maybe AMP.AMPTransformSelection
selectAMPFixture = either error id . AMP.selectAMPTransform available . parseBenchmarkHeader
  where
    available "google" = [1 .. 8]
    available "any" = [1 .. 8]
    available _ = []


correlationFixture, requestStartFixture :: ByteString
correlationFixture = "550e8400-e29b-41d4-a716-446655440000"
requestStartFixture = "t=1700000000.123"


opsReadFixture, opsWriteFixture, opsObjectFixture :: ByteString
opsReadFixture = "\"*\" \"user.name\""
opsWriteFixture = "\"user.bdate=1970.01.01\""
opsObjectFixture = "MIIBkzCCAX2gAwIBAgIBADAN"


p3pDirectiveScore :: NonEmpty ByteString -> Int
p3pDirectiveScore = either error (foldl' directive 0) . P3P.p3pDirectives . parseBenchmarkHeader
  where
    directive n (P3P.P3PPolicyRef uri) = n + BS.length uri
    directive n (P3P.P3PCompactPolicy tokens) = foldl' compact n tokens
    directive n (P3P.P3PExtension name value) = n + BS.length name + maybe 0 extension value
    compact n (P3P.P3PKnownCompact claim requirement) = seq claim (n + maybe 1 (const 2) requirement)
    compact n (P3P.P3PUnknownCompact bytes) = n + BS.length bytes
    extension (P3P.P3PExtensionToken bytes) = BS.length bytes
    extension (P3P.P3PExtensionQuoted bytes) = BS.length bytes


p3pReferenceFixture, p3pCompactFixture :: ByteString
p3pReferenceFixture = "policyref=\"/w3c/p3p.xml#policy\""
p3pCompactFixture = "policyref=\"/w3c/p3p.xml\", CP=\"NON DSP ADM DEV PSA IVDo OUR IND STP PHY PRE NAV UNI\""


picsLabelScore :: NonEmpty ByteString -> Float
picsLabelScore = either error (foldl' service 0 . Pics.picsServices) . PICSLabel.picsLabelList . parseBenchmarkHeader
  where
    service total (Pics.PicsServiceLabels url opts labels) =
      foldl' entry (foldl' option (total + size url) opts) labels
    service total (Pics.PicsServiceFailure url _) = total + size url
    service total (Pics.PicsNoRatings notes) = foldl' (\n b -> n + size b) total notes
    entry total (Pics.PicsLabel value) = label total value
    entry total (Pics.PicsLabelTree entries) = foldl' label total entries
    entry total (Pics.PicsLabelFailure _) = total + 1
    label total (Pics.PicsSingleLabel opts ratings) = foldl' rating (foldl' option total opts) ratings
    option total (Pics.PicsBy name) = total + size name
    option total (Pics.PicsOn date) = total + dateScore date
    option total (Pics.PicsUntil date) = total + dateScore date
    option total _ = total + 1
    dateScore date =
      fromIntegral (Pics.picsYear date)
        + fromIntegral (Pics.picsMonth date)
        + fromIntegral (Pics.picsDay date)
        + fromIntegral (Pics.picsHour date)
        + fromIntegral (Pics.picsMinute date)
        + fromIntegral (Pics.picsOffsetMinutes date)
    rating total (Pics.PicsRating category (Pics.PicsScalar scalar)) =
      total + size category + Pics.picsNumberValue scalar
    rating total (Pics.PicsRating category (Pics.PicsMultiple values)) = foldl' multiple (total + size category) values
    multiple total (Pics.PicsExact scalar) = total + Pics.picsNumberValue scalar
    multiple total (Pics.PicsRange first lastValue) = total + Pics.picsNumberValue first + Pics.picsNumberValue lastValue
    size = fromIntegral . BS.length


picsSimpleLabel, picsComplexLabel :: ByteString
picsSimpleLabel = "(PICS-1.1 \"http://www.gcf.org/v2.5\" labels ratings (s 0))"
picsComplexLabel =
  "(PICS-1.1 \"http://www.gcf.org/v2.5\" by \"John Doe\" labels \
  \on \"1994.11.05T08:15-0500\" until \"1995.12.31T23:59-0000\" \
  \ratings (suds 0.5 density 0 color/hue 1 subject (0.5:1.5 2)))"


picsRequestFixture, jepiInfoFixture, jepiQueryFixture :: ByteString
picsRequestFixture = "{PICS-1.1 {params full {services \"http://www.gcf.org/v2.5\"}}}"
jepiInfoFixture = "{http://www.SET.org/PEPSpec {params {upp {instrument-brand VISA}}} {for /PaymentPages/*}}"
jepiQueryFixture = "{http://www.w3.org/UPP {for /PaymentPages/*}}"


pepDeclarationScore :: Int -> PepGrammar.PepDeclaration -> Int
pepDeclarationScore total (PepGrammar.PepDeclaration uri prefixes attrs) =
  foldl' attrScore (foldl' (\count prefix -> count + BS.length prefix) (total + BS.length uri) prefixes) attrs
  where
    attrScore count (PepGrammar.PepDeclarationStrength value) = count + pepStrengthScore value
    attrScore count (PepGrammar.PepDeclarationExtension bag) = count + pepBagScore bag


pepPolicyScore :: Int -> PepGrammar.PepPolicy -> Int
pepPolicyScore total (PepGrammar.PepPolicy uri attrs) = foldl' attrScore (total + BS.length uri) attrs
  where
    attrScore count (PepGrammar.PepFor targets) = foldl' targetScore count targets
    attrScore count (PepGrammar.PepMaxAge seconds) = count + fromIntegral seconds
    attrScore count (PepGrammar.PepParameters items) = foldl' pepItemScore count items
    attrScore count (PepGrammar.PepPolicyStrength value) = count + pepStrengthScore value
    attrScore count (PepGrammar.PepPolicyExtension bag) = count + pepBagScore bag
    targetScore count (PepGrammar.PepTarget uriTarget isPrefix) = count + BS.length uriTarget + fromEnum isPrefix


pepStrengthScore :: PepGrammar.PepStrength -> Int
pepStrengthScore PepGrammar.PepMay = 1
pepStrengthScore PepGrammar.PepMust = 2


pepBagScore :: PepGrammar.PepBag -> Int
pepBagScore (PepGrammar.PepBag name items) = foldl' pepItemScore (BS.length name) items


pepItemScore :: Int -> PepGrammar.PepBagItem -> Int
pepItemScore count (PepGrammar.PepNestedBag bag) = count + pepBagScore bag
pepItemScore count (PepGrammar.PepToken token) = count + BS.length token
pepItemScore count (PepGrammar.PepQuoted bytes) = count + BS.length bytes


pepDeclarationWire :: ByteString
pepDeclarationWire = "{{map \"http://www.w3.org/PEP/Digest\" 4-} {strength must}}"


pepPolicyWire :: ByteString
pepPolicyWire = "{{id \"http://www.w3.org/PEP/MiniPayment\"} {params {Price 0.02USD}} {strength must}}"


extensionDeclarationScore :: NonEmpty Extension.ExtensionDeclaration -> Int
extensionDeclarationScore = foldl' score 0
  where
    score count (Extension.ExtensionDeclaration ident namespace params) =
      let nameSize = case ident of
            Extension.ExtensionURI uri -> BS.length uri
            Extension.ExtensionFieldName field -> BS.length field
      in foldl' parameter (count + nameSize + maybe 0 BS.length namespace) params
    parameter count (Extension.ExtensionParameter name value) =
      count + BS.length name + maybe 0 argument value
    argument (Extension.ExtensionToken token) = BS.length token
    argument (Extension.ExtensionQuoted quoted) = BS.length quoted


extensionFixture :: ByteString
extensionFixture = "\"http://www.example.org/extension\"; ns=015; flag; version=2; note=\"a,b\""


proxyFeatureWire :: ByteString
proxyFeatureWire = "proxy.w3.org; log=http://proxy.w3.org/logs/xxxxx.org; expiry=24; peak=0f0000; notify"
