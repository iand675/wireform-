{- | TLS Application-Layer Protocol Negotiation (ALPN) protocol-ID registry.

ALPN (RFC 7301) lets a TLS client and server agree on the application protocol
to be carried over the connection. Each protocol is identified on the wire by an
opaque /identification sequence/ — a sequence of octets, conventionally the
UTF-8 (ASCII) encoding of a short protocol label (e.g. @h2@, @http\/1.1@,
@acme-tls\/1@).

This module mirrors the IANA \"TLS Application-Layer Protocol Negotiation (ALPN)
Protocol IDs\" registry, defined by [RFC 7301](https://www.rfc-editor.org/rfc/rfc7301)
and maintained at
<https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids>.
It provides a newtype over the raw octet sequence, a named constant for every
registered protocol ID, and a parser\/renderer for the on-wire form.

The GREASE \"Reserved\" entries from [RFC 8701](https://www.rfc-editor.org/rfc/rfc8701)
(@0x?A 0x?A@) are intentionally omitted: they are randomized placeholders, not
real protocol labels.
-}
module Network.TLS.Extensions (
  -- * ALPN protocol identifier
  ALPNProtocol (..),
  mkALPNProtocol,
  alpnProtocolText,

  -- * Parsing and rendering
  alpnProtocolParser,
  renderALPNProtocol,

  -- * Registered protocol IDs
  registeredALPNProtocols,

  -- ** HTTP family
  alpnHttp09,
  alpnHttp10,
  alpnHttp11,
  alpnHttp2,
  alpnHttp2Cleartext,
  alpnHttp3,

  -- ** SPDY
  alpnSpdy1,
  alpnSpdy2,
  alpnSpdy3,

  -- ** STUN \/ TURN
  alpnTurn,
  alpnNatDiscovery,
  alpnStun,

  -- ** WebRTC
  alpnWebRTC,
  alpnConfidentialWebRTC,

  -- ** Mail and messaging
  alpnFtp,
  alpnImap,
  alpnPop3,
  alpnManageSieve,
  alpnXmppClient,
  alpnXmppServer,
  alpnIrc,
  alpnNntp,
  alpnNnsp,
  alpnMqtt,

  -- ** Constrained \/ IoT
  alpnCoap,
  alpnCoapDtls,

  -- ** TLS, naming, and time
  alpnAcmeTls1,
  alpnDnsOverTls,
  alpnDoq,
  alpnNtske1,

  -- ** RPC, database, and storage
  alpnSunRpc,
  alpnSmb2,
  alpnSip,
  alpnTds8,
  alpnDicom,
  alpnPostgreSQL,

  -- ** AAA
  alpnRadius10,
  alpnRadius11,

  -- ** Measurement and experimental
  alpnNetPerfMeterControl,
  alpnNetPerfMeterData,
  alpnNPamp2,
) where

import Data.ByteString (ByteString)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.Text.Short (ShortText)
import qualified Data.Text.Short.Unsafe as STU
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | An ALPN protocol identifier: the precise sequence of octets that identifies
an application protocol on the wire (RFC 7301 §3.1, @ProtocolName@). The bytes
are preserved verbatim; ALPN comparison is octet-for-octet, so no case folding
or normalization is applied.
-}
newtype ALPNProtocol = ALPNProtocol {alpnIdentificationSequence :: ByteString}
  deriving stock (Eq, Ord, Show)


{- | Wrap a raw octet sequence as an 'ALPNProtocol'. Total: any byte string is a
syntactically valid identification sequence.
-}
mkALPNProtocol :: ByteString -> ALPNProtocol
mkALPNProtocol = ALPNProtocol
{-# INLINE mkALPNProtocol #-}


{- | Decode an identification sequence as ASCII text. Registered ALPN IDs are
ASCII (a subset of UTF-8), so this is a faithful, lossless view of the label
for display purposes.
-}
alpnProtocolText :: ALPNProtocol -> ShortText
alpnProtocolText = STU.fromByteStringUnsafe . alpnIdentificationSequence
{-# INLINE alpnProtocolText #-}


{- | The octet set used by registered identification sequences: the RFC 9110
@token@ characters plus @\'\/\'@ (which appears in labels such as @http\/1.1@,
@acme-tls\/1@ and @sip\/2@ but is not itself a @token@ character).
-}
alpnTokenCharSet :: CharSet
alpnTokenCharSet = tokenCharSet <> CharSet.singleton '/'


{- | Parse an ALPN identification sequence: one or more characters from
'alpnTokenCharSet'. RFC 7301 identification sequences are opaque octets; in
practice every registered label is drawn from this set, so the parser accepts
exactly that token shape and captures the raw bytes.
-}
alpnProtocolParser :: ParserT st e ALPNProtocol
alpnProtocolParser =
  ALPNProtocol
    <$> byteStringOf (skipSome (skipSatisfyAscii (`CharSet.member` alpnTokenCharSet)))
{-# INLINE alpnProtocolParser #-}


-- | Render an 'ALPNProtocol' as its raw octet sequence.
renderALPNProtocol :: ALPNProtocol -> M.Builder
renderALPNProtocol = M.byteString . alpnIdentificationSequence
{-# INLINE renderALPNProtocol #-}


------------------------------------------------------------------------
-- Registered protocol IDs (IANA / RFC 7301 registry)
------------------------------------------------------------------------

-- | HTTP/0.9 — @\"http\/0.9\"@. [RFC 1945](https://www.rfc-editor.org/rfc/rfc1945)
alpnHttp09 :: ALPNProtocol
alpnHttp09 = ALPNProtocol "http/0.9"


-- | HTTP/1.0 — @\"http\/1.0\"@. [RFC 1945](https://www.rfc-editor.org/rfc/rfc1945)
alpnHttp10 :: ALPNProtocol
alpnHttp10 = ALPNProtocol "http/1.0"


-- | HTTP/1.1 — @\"http\/1.1\"@. [RFC 9112](https://www.rfc-editor.org/rfc/rfc9112)
alpnHttp11 :: ALPNProtocol
alpnHttp11 = ALPNProtocol "http/1.1"


-- | HTTP/2 over TLS — @\"h2\"@. [RFC 9113](https://www.rfc-editor.org/rfc/rfc9113)
alpnHttp2 :: ALPNProtocol
alpnHttp2 = ALPNProtocol "h2"


{- | HTTP/2 over cleartext TCP — @\"h2c\"@. Reserved; not valid in a TLS ALPN
negotiation. [RFC 9113](https://www.rfc-editor.org/rfc/rfc9113)
-}
alpnHttp2Cleartext :: ALPNProtocol
alpnHttp2Cleartext = ALPNProtocol "h2c"


-- | HTTP/3 — @\"h3\"@. [RFC 9114](https://www.rfc-editor.org/rfc/rfc9114)
alpnHttp3 :: ALPNProtocol
alpnHttp3 = ALPNProtocol "h3"


-- | SPDY/1 — @\"spdy\/1\"@ (Chromium SPDY draft 1).
alpnSpdy1 :: ALPNProtocol
alpnSpdy1 = ALPNProtocol "spdy/1"


-- | SPDY/2 — @\"spdy\/2\"@ (Chromium SPDY draft 2).
alpnSpdy2 :: ALPNProtocol
alpnSpdy2 = ALPNProtocol "spdy/2"


-- | SPDY/3 — @\"spdy\/3\"@ (Chromium SPDY draft 3).
alpnSpdy3 :: ALPNProtocol
alpnSpdy3 = ALPNProtocol "spdy/3"


{- | Traversal Using Relays around NAT (TURN) — @\"stun.turn\"@.
[RFC 7443](https://www.rfc-editor.org/rfc/rfc7443)
-}
alpnTurn :: ALPNProtocol
alpnTurn = ALPNProtocol "stun.turn"


{- | NAT discovery using STUN — @\"stun.nat-discovery\"@.
[RFC 7443](https://www.rfc-editor.org/rfc/rfc7443)
-}
alpnNatDiscovery :: ALPNProtocol
alpnNatDiscovery = ALPNProtocol "stun.nat-discovery"


{- | Alias for 'alpnNatDiscovery' (STUN NAT-discovery usage), @\"stun.nat-discovery\"@.
[RFC 7443](https://www.rfc-editor.org/rfc/rfc7443)
-}
alpnStun :: ALPNProtocol
alpnStun = alpnNatDiscovery


-- | WebRTC Media and Data — @\"webrtc\"@. [RFC 8833](https://www.rfc-editor.org/rfc/rfc8833)
alpnWebRTC :: ALPNProtocol
alpnWebRTC = ALPNProtocol "webrtc"


{- | Confidential WebRTC Media and Data — @\"c-webrtc\"@.
[RFC 8833](https://www.rfc-editor.org/rfc/rfc8833)
-}
alpnConfidentialWebRTC :: ALPNProtocol
alpnConfidentialWebRTC = ALPNProtocol "c-webrtc"


{- | FTP over TLS — @\"ftp\"@. [RFC 959](https://www.rfc-editor.org/rfc/rfc959),
[RFC 4217](https://www.rfc-editor.org/rfc/rfc4217)
-}
alpnFtp :: ALPNProtocol
alpnFtp = ALPNProtocol "ftp"


-- | IMAP over TLS — @\"imap\"@. [RFC 2595](https://www.rfc-editor.org/rfc/rfc2595)
alpnImap :: ALPNProtocol
alpnImap = ALPNProtocol "imap"


-- | POP3 over TLS — @\"pop3\"@. [RFC 2595](https://www.rfc-editor.org/rfc/rfc2595)
alpnPop3 :: ALPNProtocol
alpnPop3 = ALPNProtocol "pop3"


-- | ManageSieve — @\"managesieve\"@. [RFC 5804](https://www.rfc-editor.org/rfc/rfc5804)
alpnManageSieve :: ALPNProtocol
alpnManageSieve = ALPNProtocol "managesieve"


{- | XMPP @jabber:client@ namespace — @\"xmpp-client\"@.
[XEP-0368](https://xmpp.org/extensions/xep-0368.html)
-}
alpnXmppClient :: ALPNProtocol
alpnXmppClient = ALPNProtocol "xmpp-client"


{- | XMPP @jabber:server@ namespace — @\"xmpp-server\"@.
[XEP-0368](https://xmpp.org/extensions/xep-0368.html)
-}
alpnXmppServer :: ALPNProtocol
alpnXmppServer = ALPNProtocol "xmpp-server"


-- | IRC over TLS — @\"irc\"@. [RFC 1459](https://www.rfc-editor.org/rfc/rfc1459)
alpnIrc :: ALPNProtocol
alpnIrc = ALPNProtocol "irc"


-- | NNTP (reading) — @\"nntp\"@. [RFC 3977](https://www.rfc-editor.org/rfc/rfc3977)
alpnNntp :: ALPNProtocol
alpnNntp = ALPNProtocol "nntp"


-- | NNTP (transit) — @\"nnsp\"@. [RFC 3977](https://www.rfc-editor.org/rfc/rfc3977)
alpnNnsp :: ALPNProtocol
alpnNnsp = ALPNProtocol "nnsp"


{- | OASIS MQTT — @\"mqtt\"@.
([MQTT v5.0](http://docs.oasis-open.org/mqtt/mqtt/v5.0/mqtt-v5.0.html))
-}
alpnMqtt :: ALPNProtocol
alpnMqtt = ALPNProtocol "mqtt"


-- | CoAP over TLS — @\"coap\"@. [RFC 8323](https://www.rfc-editor.org/rfc/rfc8323)
alpnCoap :: ALPNProtocol
alpnCoap = ALPNProtocol "coap"


{- | CoAP over DTLS — @\"co\"@. [RFC 7252](https://www.rfc-editor.org/rfc/rfc7252),
[RFC 9952](https://www.rfc-editor.org/rfc/rfc9952)
-}
alpnCoapDtls :: ALPNProtocol
alpnCoapDtls = ALPNProtocol "co"


{- | ACME TLS/1 (tls-alpn-01 challenge) — @\"acme-tls\/1\"@.
[RFC 8737](https://www.rfc-editor.org/rfc/rfc8737)
-}
alpnAcmeTls1 :: ALPNProtocol
alpnAcmeTls1 = ALPNProtocol "acme-tls/1"


-- | DNS-over-TLS — @\"dot\"@. [RFC 7858](https://www.rfc-editor.org/rfc/rfc7858)
alpnDnsOverTls :: ALPNProtocol
alpnDnsOverTls = ALPNProtocol "dot"


-- | DNS-over-QUIC — @\"doq\"@. [RFC 9250](https://www.rfc-editor.org/rfc/rfc9250)
alpnDoq :: ALPNProtocol
alpnDoq = ALPNProtocol "doq"


{- | Network Time Security Key Establishment, version 1 — @\"ntske\/1\"@.
[RFC 8915 §4](https://www.rfc-editor.org/rfc/rfc8915#section-4)
-}
alpnNtske1 :: ALPNProtocol
alpnNtske1 = ALPNProtocol "ntske/1"


-- | SunRPC over TLS — @\"sunrpc\"@. [RFC 9289](https://www.rfc-editor.org/rfc/rfc9289)
alpnSunRpc :: ALPNProtocol
alpnSunRpc = ALPNProtocol "sunrpc"


{- | SMB2 — @\"smb\"@.
([MS-SMB2](https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-smb2/5606ad47-5ee0-437a-817e-70c366052962))
-}
alpnSmb2 :: ALPNProtocol
alpnSmb2 = ALPNProtocol "smb"


-- | SIP version 2 — @\"sip\/2\"@. [RFC 3261](https://www.rfc-editor.org/rfc/rfc3261)
alpnSip :: ALPNProtocol
alpnSip = ALPNProtocol "sip/2"


-- | TDS 8.0 — @\"tds\/8.0\"@ (Tabular Data Stream, [MS-TDS]).
alpnTds8 :: ALPNProtocol
alpnTds8 = ALPNProtocol "tds/8.0"


-- | DICOM — @\"dicom\"@. ([DICOM standard](https://www.dicomstandard.org/current))
alpnDicom :: ALPNProtocol
alpnDicom = ALPNProtocol "dicom"


{- | PostgreSQL — @\"postgresql\"@.
([PostgreSQL protocol](https://www.postgresql.org/docs/current/protocol.html))
-}
alpnPostgreSQL :: ALPNProtocol
alpnPostgreSQL = ALPNProtocol "postgresql"


{- | RADIUS/1.0 (RADIUS over TLS) — @\"radius\/1.0\"@.
[RFC 9765](https://www.rfc-editor.org/rfc/rfc9765)
-}
alpnRadius10 :: ALPNProtocol
alpnRadius10 = ALPNProtocol "radius/1.0"


{- | RADIUS/1.1 (RADIUS over TLS) — @\"radius\/1.1\"@.
[RFC 9765](https://www.rfc-editor.org/rfc/rfc9765)
-}
alpnRadius11 :: ALPNProtocol
alpnRadius11 = ALPNProtocol "radius/1.1"


-- | NetPerfMeter Protocol Control Channel (NPMP-CONTROL) — @\"netperfmeter\/control\"@.
alpnNetPerfMeterControl :: ALPNProtocol
alpnNetPerfMeterControl = ALPNProtocol "netperfmeter/control"


-- | NetPerfMeter Protocol Data Channel (NPMP-DATA) — @\"netperfmeter\/data\"@.
alpnNetPerfMeterData :: ALPNProtocol
alpnNetPerfMeterData = ALPNProtocol "netperfmeter/data"


{- | N-PAMP (Native Post-Quantum Agent Messaging Protocol), wire major version 2
— @\"n-pamp\/2\"@.
-}
alpnNPamp2 :: ALPNProtocol
alpnNPamp2 = ALPNProtocol "n-pamp/2"


{- | Every registered ALPN protocol ID, one entry per unique identification
sequence (aliases such as 'alpnStun' are not duplicated). Useful for building
ALPN advertisement lists and for exhaustive round-trip testing.
-}
registeredALPNProtocols :: [ALPNProtocol]
registeredALPNProtocols =
  [ alpnHttp09
  , alpnHttp10
  , alpnHttp11
  , alpnSpdy1
  , alpnSpdy2
  , alpnSpdy3
  , alpnTurn
  , alpnNatDiscovery
  , alpnHttp2
  , alpnHttp2Cleartext
  , alpnWebRTC
  , alpnConfidentialWebRTC
  , alpnFtp
  , alpnImap
  , alpnPop3
  , alpnManageSieve
  , alpnCoap
  , alpnCoapDtls
  , alpnXmppClient
  , alpnXmppServer
  , alpnAcmeTls1
  , alpnMqtt
  , alpnDnsOverTls
  , alpnNtske1
  , alpnSunRpc
  , alpnHttp3
  , alpnSmb2
  , alpnIrc
  , alpnNntp
  , alpnNnsp
  , alpnDoq
  , alpnSip
  , alpnTds8
  , alpnDicom
  , alpnPostgreSQL
  , alpnRadius10
  , alpnRadius11
  , alpnNetPerfMeterControl
  , alpnNetPerfMeterData
  , alpnNPamp2
  ]
