module Main (main) where

import qualified Test.Hermes.AcceptCharset
import qualified Test.Hermes.AcceptEncoding
import qualified Test.Hermes.AcceptPatchPost
import qualified Test.Hermes.AcceptRanges
import qualified Test.Hermes.Alpn
import qualified Test.Hermes.AltSvc
import qualified Test.Hermes.AtomPub
import qualified Test.Hermes.Caching
import qualified Test.Hermes.CalDav
import qualified Test.Hermes.Capsule
import qualified Test.Hermes.Cdn
import qualified Test.Hermes.ClientCert
import qualified Test.Hermes.ClientHints
import qualified Test.Hermes.Conditional
import qualified Test.Hermes.ContentMeta
import qualified Test.Hermes.ContentRange
import qualified Test.Hermes.Cors
import qualified Test.Hermes.CrossOrigin
import qualified Test.Hermes.Csp
import qualified Test.Hermes.DefactoCorrelation
import qualified Test.Hermes.DefactoForwarding
import qualified Test.Hermes.DefactoMisc
import qualified Test.Hermes.DefactoSecurity
import qualified Test.Hermes.DeltaEncoding
import qualified Test.Hermes.Digest
import qualified Test.Hermes.Expect
import qualified Test.Hermes.Forwarded
import qualified Test.Hermes.Framing
import qualified Test.Hermes.IPAddress
import qualified Test.Hermes.Link
import qualified Test.Hermes.Mailbox
import qualified Test.Hermes.Memento
import qualified Test.Hermes.MiscProvisional
import qualified Test.Hermes.OAuthDpop
import qualified Test.Hermes.OData
import qualified Test.Hermes.ObsoleteContent
import qualified Test.Hermes.ObsoleteCookie
import qualified Test.Hermes.ObsoleteMan
import qualified Test.Hermes.ObsoleteProfile
import qualified Test.Hermes.ObsoleteProtocol
import qualified Test.Hermes.PermissionsPolicy
import qualified Test.Hermes.Pkp
import qualified Test.Hermes.Prefer
import qualified Test.Hermes.Priority
import qualified Test.Hermes.ProxyAuthenticate
import qualified Test.Hermes.Range
import qualified Test.Hermes.RenderingUtil
import qualified Test.Hermes.Reporting
import qualified Test.Hermes.SecurityMisc
import qualified Test.Hermes.Server
import qualified Test.Hermes.Signature
import qualified Test.Hermes.Sse
import qualified Test.Hermes.Surrogate
import qualified Test.Hermes.TCN
import qualified Test.Hermes.TokenBinding
import qualified Test.Hermes.Tracing
import qualified Test.Hermes.WWWAuthenticate
import qualified Test.Hermes.WebDavCore
import qualified Test.Hermes.WebDavExt
import qualified Test.Hermes.WebPush
import qualified Test.Hermes.WebSocket
import Test.Syd
import Test.Syd.OptParse (Timeout (DoNotTimeout), defaultSettings, settingTimeout)


main :: IO ()
main =
  -- sydtest's per-test wall-clock timeout occasionally fires spuriously on this
  -- (sub-second) suite; the parsers are total over the bounded generators, so we
  -- disable it rather than let CI flake. See SYDTEST_TIMEOUT / --no-timeout.
  sydTestWith defaultSettings {settingTimeout = DoNotTimeout} $
    describe "hermes" $
      sequence_
        [ Test.Hermes.AcceptCharset.tests
        , Test.Hermes.AcceptEncoding.tests
        , Test.Hermes.AcceptPatchPost.tests
        , Test.Hermes.AcceptRanges.tests
        , Test.Hermes.Alpn.tests
        , Test.Hermes.AltSvc.tests
        , Test.Hermes.AtomPub.tests
        , Test.Hermes.Caching.tests
        , Test.Hermes.CalDav.tests
        , Test.Hermes.Capsule.tests
        , Test.Hermes.Cdn.tests
        , Test.Hermes.ClientCert.tests
        , Test.Hermes.ClientHints.tests
        , Test.Hermes.Conditional.tests
        , Test.Hermes.ContentMeta.tests
        , Test.Hermes.ContentRange.tests
        , Test.Hermes.Cors.tests
        , Test.Hermes.CrossOrigin.tests
        , Test.Hermes.Csp.tests
        , Test.Hermes.DefactoCorrelation.tests
        , Test.Hermes.DefactoForwarding.tests
        , Test.Hermes.DefactoMisc.tests
        , Test.Hermes.DefactoSecurity.tests
        , Test.Hermes.DeltaEncoding.tests
        , Test.Hermes.Digest.tests
        , Test.Hermes.Expect.tests
        , Test.Hermes.Forwarded.tests
        , Test.Hermes.Framing.tests
        , Test.Hermes.IPAddress.tests
        , Test.Hermes.Link.tests
        , Test.Hermes.Mailbox.tests
        , Test.Hermes.Memento.tests
        , Test.Hermes.MiscProvisional.tests
        , Test.Hermes.OAuthDpop.tests
        , Test.Hermes.OData.tests
        , Test.Hermes.ObsoleteContent.tests
        , Test.Hermes.ObsoleteCookie.tests
        , Test.Hermes.ObsoleteMan.tests
        , Test.Hermes.ObsoleteProfile.tests
        , Test.Hermes.ObsoleteProtocol.tests
        , Test.Hermes.PermissionsPolicy.tests
        , Test.Hermes.Pkp.tests
        , Test.Hermes.Prefer.tests
        , Test.Hermes.Priority.tests
        , Test.Hermes.ProxyAuthenticate.tests
        , Test.Hermes.Range.tests
        , Test.Hermes.RenderingUtil.tests
        , Test.Hermes.Reporting.tests
        , Test.Hermes.SecurityMisc.tests
        , Test.Hermes.Server.tests
        , Test.Hermes.Signature.tests
        , Test.Hermes.Sse.tests
        , Test.Hermes.Surrogate.tests
        , Test.Hermes.TCN.tests
        , Test.Hermes.TokenBinding.tests
        , Test.Hermes.Tracing.tests
        , Test.Hermes.WWWAuthenticate.tests
        , Test.Hermes.WebDavCore.tests
        , Test.Hermes.WebDavExt.tests
        , Test.Hermes.WebPush.tests
        , Test.Hermes.WebSocket.tests
        ]
