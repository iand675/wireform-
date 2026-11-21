{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Hermes.URIReference (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.ConfigurationContext (ConfigurationContext (..))
import Network.HTTP.Headers.ContentBase (ContentBase (..))
import Network.HTTP.Headers.ContentID (ContentID (..))
import Network.HTTP.Headers.ContentLocation (ContentLocation (..))
import Network.HTTP.Headers.DASL (DASL (..))
import Network.HTTP.Headers.DAV (ComplianceClass (..), DAV (..))
import Network.HTTP.Headers.Destination (Destination (..))
import Network.HTTP.Headers.DifferentialID (DifferentialID (..))
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.If as If
import Network.HTTP.Headers.Label (Label (..))
import qualified Network.HTTP.Headers.Link as Link
import Network.HTTP.Headers.Location (Location (..))
import Network.HTTP.Headers.LockToken (LockToken (..))
import Network.HTTP.Headers.ODataEntityId (ODataEntityId (..))
import Network.HTTP.Headers.OrderingType (OrderingType (..))
import Network.HTTP.Headers.PingFrom (PingFrom (..))
import Network.HTTP.Headers.PingTo (PingTo (..))
import Network.HTTP.Headers.Position (Position (..))
import Network.HTTP.Headers.RedirectRef (RedirectRef (..))
import Network.HTTP.Headers.Referer (Referer (..))
import Network.HTTP.Headers.SoapAction (SoapAction (..))
import Network.HTTP.Headers.StatusURI (StatusURI (..), StatusURIEntry (..))
import Network.HTTP.Headers.URI (URI (..))
import Network.HTTP.Headers.XFrameOptions (XFrameOptions (..))
import Test.Syd


tests :: Spec
tests = describe "URI-valued HTTP fields" $ do
  it "preserves paths, query and fragments in Location" $ do
    let locations =
          [ ""
          , "../widgets/%2f?a=b#details"
          , "https://example.org:123456789012345678901/a?x=//y?z#/?"
          , "//user:p%40ss@[2001:db8::1]:/a"
          , "https://[v2.a:b]/"
          , "file:///etc/hosts"
          , "urn:example:widget"
          , "1/a:b"
          , "#"
          ]
    forM_ locations (checkReference hLocation Location)
    lookupHeader @Location (headerMapFromList []) `shouldBe` Right Nothing
  it "validates escapes, components and authority in Location" $
    forM_
      [ "1scheme:a"
      , "/docs/%ZZ"
      , "/docs/%2"
      , "/a b"
      , "/a?x=[y]"
      , "/a#x#y"
      , "//[2001:db8::]:bad"
      , "//[not-an-ip]/"
      , "//u@@example.org/"
      , "/a" <> BS.pack [0x7f]
      , "/a" <> BS.pack [0xff]
      ]
      (reject @Location hLocation)
  it "allows fragmentless Content-Location references" $ do
    forM_ ["https://example.org/a", "/widget", "?view=json", "//user@example.org/"] $
      checkReference hContentLocation ContentLocation
    forM_ ["/widget#details", "/widget?x=%G2"] (reject @ContentLocation hContentLocation)
  it "enforces the Referer restrictions on fragments and userinfo" $ do
    forM_ ["", "../index", "/?ref=http://example.org", "//example.org/", "https://example.org/p%40ge", "/@user"] $
      checkReference hReferer Referer
    forM_ ["/#", "https://example.org/page#fragment", "https://user@example.org/", "//@example.org/", "/x?y=%"] $
      reject @Referer hReferer
  it "preserves the browser URL serialization in both Ping fields" $ do
    let urls =
          [ "https://example.org/"
          , "https://example.org/a%20b?q=[y]&raw=%ZZ#one#two"
          , "https://u:99999@[2001:db8::1]:65535/[slot]|next"
          , "https://example.org/?q=%27yes%27"
          , "file:///tmp/example"
          , "data:text/plain,hello world"
          , "mailto:alice@example.org"
          , "custom://example/%4"
          , "custom:/a\\b"
          , "custom:opaque  ?search"
          ]
    forM_ urls $ \url -> do
      checkReference hPingFrom (PingFrom . ST.toString) url
      checkReference hPingTo (PingTo . ST.toString) url
    lookupHeader @PingFrom (headerMapFromList [("ping-from", "\t data:text/plain,hello world  \t")])
      `shouldBe` Right (Just (PingFrom "data:text/plain,hello world"))
  it "rejects relative and malformed serialized Ping URLs"
    $ forM_
      [ ""
      , "relative/file"
      , "//example.org/"
      , "https:example.org/"
      , "https:///a"
      , "https://example.org"
      , "https://example.org/a b"
      , "https://example.org/a\\b"
      , "https://example.org:65536/"
      , "https://example.org:80bad/"
      , "https://[v2.a:b]/"
      , "https://example.org/?q='yes'"
      , "file://user@host/path"
      , "custom://:80/path"
      , "custom://host:80bad"
      , "https://example.org/" <> BS.pack [0xc3, 0xa9]
      ]
    $ \url -> do
      reject @PingFrom hPingFrom url
      reject @PingTo hPingTo url
  it "rejects repeated lines for all five singleton fields" $ do
    rejectDuplicates @Location hLocation "/a"
    rejectDuplicates @ContentLocation hContentLocation "/a"
    rejectDuplicates @Referer hReferer "/a"
    rejectDuplicates @PingFrom hPingFrom "https://example.org/"
    rejectDuplicates @PingTo hPingTo "https://example.org/"
  describe "checked URI field writes" $ do
    it "rejects invalid authored URI fields in Location" $ do
      rejectWrite
        hLocation
        "/old"
        Location
        [ "/unescaped space"
        , "/bad%2"
        , "/a#two#fragments"
        , "HTTPS://user@example.org/a"
        , "http:///no-host"
        , "https:/no-authority"
        , "https://host:12malformed"
        , "https://[2001:db8::1]no-slash"
        ]
      renderHeaderLines defaultHeaderSettings (Location " \t../x%2f?q=m#f \t")
        `shouldBe` Just (" \t../x%2f?q=m#f \t" :| [])
    it "rejects invalid authored URI fields in Content-Location" $
      rejectWrite hContentLocation "/old" ContentLocation ["/a#fragment", "/bad%XZ", "http://user@example.org/", "https://:443/"]
    it "rejects invalid authored URI fields in Referer" $
      rejectWrite hReferer "/old" Referer ["/a#fragment", "//user@example.org/", "https:///no-host", "/space here"]
    it "rejects invalid authored URI fields in Ping-From" $
      rejectWrite hPingFrom "https://example.org/" PingFrom ["relative/path", "https://example.org/a b", "https://example.org/\x161"]
    it "rejects invalid authored URI fields in Ping-To" $
      rejectWrite hPingTo "https://example.org/" PingTo ["//example.org/", "https://example.org:65536/", "https://example.org/\x141"]
    it "rejects invalid authored URI fields in Content-Base" $
      rejectWrite hContentBase "https://example.org/" ContentBase ["../base", "https://example.org/base#frag", "http://user@example.org/"]
    it "rejects invalid authored URI fields in Destination" $ do
      rejectWrite hDestination "/old" Destination ["relative/path", "//example.org/a", "/a#frag", "HTTPS://user@example.org/", "https:///"]
      renderHeaderLines defaultHeaderSettings (Destination " /collection/new?view=full\t")
        `shouldBe` Just (" /collection/new?view=full\t" :| [])
    it "rejects invalid authored URI fields anywhere in a legacy URI list" $ do
      rejectWrite hURI "<old>" URI ["a>, <b" :| [], "ok" :| ["a "], "ok" :| ["broken%"], "http://user@example.org/" :| []]
      renderHeaderLines defaultHeaderSettings (URI ("/a,b" :| ["", "urn:example:leaf#about"]))
        `shouldBe` Just ("</a,b>, <>, <urn:example:leaf#about>" :| [])
  describe "checked structured URI writes" $ do
    it "rejects malformed structured URI writes in Link" $ do
      let token name value = Link.LinkParam name (Just (Link.LinkParamToken value))
          quoted name value = Link.LinkParam name (Just (Link.LinkParamQuoted value))
          entry = Link.LinkValue "/ok" [token "rel" "next"]
          badTarget uri = entry {Link.linkTarget = uri}
          params extra = entry {Link.linkParams = Link.linkParams entry <> extra}
      rejectWrite
        hLink
        "</old>; rel=prev"
        (Link.Link . (entry :) . pure)
        [ badTarget "/bad%x"
        , badTarget "/a>, </b"
        , badTarget "https://user@example.org/"
        , Link.LinkValue "/ok" []
        , Link.LinkValue "/ok" [quoted "rel" "next\tprev"]
        , Link.LinkValue "/ok" [Link.LinkParam "rel" Nothing]
        , params [token "REL" "prev"]
        , params [token "x=y" "value"]
        , params [token "x" "a,b"]
        , params [quoted "title" "bad\rtext"]
        , params [quoted "anchor" "/bad%2"]
        , params [token "hreflang" "en--GB"]
        , params [quoted "type" "text/html; charset=utf-8"]
        , params [quoted "type" "text/html", quoted "TYPE" "text/xml"]
        , params [token "title*" "UTF-8''%FF"]
        , params [quoted "title*" "UTF-8''Title"]
        ]
      renderHeaderLines defaultHeaderSettings (Link.Link [params [quoted "anchor" "#section", token "hreflang" "en", token "HREFLANG" "fr"]])
        `shouldBe` Just ("</ok>; rel=next; anchor=\"#section\"; hreflang=en; HREFLANG=fr" :| [])
    it "rejects malformed structured URI writes in Lock-Token" $
      rejectWrite hLockToken "<urn:old>" LockToken ["relative", "urn:a#f", "urn:a> <urn:b", "urn:a ", "http://user@example.org/"]
    it "rejects malformed structured URI writes in Status-URI" $ do
      let valid = StatusURIEntry 423 "https://example.org/object"
      rejectWrite hStatusURI "423 <urn:old>" (StatusURI . (valid :) . pure) [StatusURIEntry 99 "urn:a", StatusURIEntry 600 "urn:a", StatusURIEntry 400 "urn:a> 200 <urn:b", StatusURIEntry 423 "/relative"]
      renderHeaderLines defaultHeaderSettings (StatusURI [valid, StatusURIEntry 599 "urn:test"])
        `shouldBe` Just ("423 <https://example.org/object> 599 <urn:test>" :| [])
    it "rejects malformed structured URI writes in Redirect-Ref" $
      rejectWrite hRedirectRef "/old" RedirectRef ["/bad%2", "/two#fragments#extra", "https:///empty", "https://user@example.org/"]
    it "rejects malformed structured URI writes in Ordering-Type" $
      rejectWrite hOrderingType "DAV:unordered" OrderingType ["relative", "DAV:order#fragment", "https://user@example.org/"]
    it "rejects malformed structured URI writes in DASL" $
      rejectWrite hDASL "<DAV:old>" (DASLGrammars . ("<DAV:basicsearch>" :|) . pure) ["urn:no-brackets", "<urn:a>, <urn:b>", "<urn:trailing >", "<relative>"]
    it "rejects malformed structured URI writes in DAV" $
      rejectWrite hDAV "1" (DAV . (ComplianceToken "1" :|) . pure) [ComplianceToken "2, 3", ComplianceToken "", ComplianceURL "urn:a>, <urn:b", ComplianceURL "/relative"]
    it "rejects malformed structured URI writes in If" $ do
      rejectWrite hIf "(<urn:old>)" If.If ["(<relative>)", "(<urn:one>) invalid", "(<https://user@example.org/>)"]
      let condition token = If.IfList (If.IfCondition False (If.IfStateToken token) :| [])
          untagged = If.fromIfConditionLists . If.IfUntagged . (:| []) . condition
          tagged resource = If.fromIfConditionLists (If.IfTagged (If.IfResource resource (condition "urn:state" :| []) :| []))
      rejectWrite hIf "(<urn:old>)" untagged ["urn:a> <urn:b", "urn:a#fragment", "/relative"]
      rejectWrite hIf "(<urn:old>)" tagged ["relative", "/path> (<urn:injected>) <", "/path#fragment"]
  describe "checked URI metadata writes" $ do
    it "rejects malformed URI metadata writes in OData-EntityId" $ do
      rejectWrite hODataEntityId "/old" ODataEntityId ["", " \t", "/a b", "/bad%X0", "/\x80", "/\xe000", "/\xfdd0", "HTTP:///empty", "https://u@example.org/item"]
      renderHeaderLines defaultHeaderSettings (ODataEntityId " /caf\xe9?private=\xe000\t")
        `shouldBe` Just (" /caf%C3%A9?private=%EE%80%80\t" :| [])
    it "rejects malformed URI metadata writes in Configuration-Context" $ do
      rejectWrite hConfigurationContext "urn:old" ConfigurationContext ["../relative", "<urn:config>", "https://u@example.org/config", "https:///empty", "urn:config%2"]
      renderHeaderLines defaultHeaderSettings (ConfigurationContext " \tURN:config#version ")
        `shouldBe` Just (" \tURN:config#version " :| [])
    it "rejects malformed URI metadata writes in SOAPAction" $ do
      rejectWrite hSoapAction "\"urn:old\"" SoapAction ["a\" ", "/bad%2", " /space", "https://u@example.org/action"]
      renderHeaderLines defaultHeaderSettings SoapActionNoIntent `shouldBe` Just ("" :| [])
      renderHeaderLines defaultHeaderSettings (SoapAction "") `shouldBe` Just ("\"\"" :| [])
    it "rejects malformed URI metadata writes in Content-ID" $ do
      rejectWrite hContentID "<old@example.org>" ContentID ["", "batch/id", "<part@>", "<a@b><c@d>", "urn:x,urn:y,", "urn:x,https://u@example.org/a"]
      renderHeaderLines defaultHeaderSettings (ContentID " (part) <target@example.org>\t")
        `shouldBe` Just (" (part) <target@example.org>\t" :| [])
    it "rejects malformed URI metadata writes in Differential-ID" $ do
      rejectWrite hDifferentialID "urn:old" DifferentialID ["request-42", "<a@b>", "urn:x, urn:y", "urn:x,,urn:y", "urn:x,https://u@example.org/a", "urn:x#f"]
      renderHeaderLines defaultHeaderSettings (DifferentialID " urn:part:1,https://example.org/%2c\t")
        `shouldBe` Just (" urn:part:1,https://example.org/%2c\t" :| [])
    it "rejects malformed URI metadata writes in X-Frame-Options" $ do
      rejectWrite hXFrameOptions "DENY" AllowFrom ["*", "https://*.example.org/", "https://a,b.example/", "https://u@example.org/", "https://example.org:65536/", "https://example.org/%zz"]
      renderHeaderLines defaultHeaderSettings (AllowFrom "https://example.org:443/a%20b?q=x#f")
        `shouldBe` Just ("ALLOW-FROM https://example.org:443/a%20b?q=x#f" :| [])
    it "rejects malformed URI metadata writes in Label" $ do
      rejectWrite hLabel "old" Label ["a b", "raw#mark", "%FF", "%C0%AF", "%ED%A0%80", "%2", "\xe9"]
      renderHeaderLines defaultHeaderSettings (Label "release%2fCaf%c3%a9")
        `shouldBe` Just ("release%2fCaf%c3%a9" :| [])
    it "rejects malformed URI metadata writes in Position" $ do
      rejectWrite hPosition "first" PositionBefore ["", "a/b", "a?b", "a#b", "a b", "%Q0"]
      rejectWrite hPosition "last" PositionAfter ["", "a/b", "a\r\n", "%2"]
      renderHeaderLines defaultHeaderSettings (PositionAfter "a%2fb;v=1")
        `shouldBe` Just ("after a%2fb;v=1" :| [])


rejectWrite
  :: (KnownHeader a, HeaderRenderingResultToNonEmpty (Cardinality a))
  => HeaderFieldName
  -> ByteString
  -> (value -> a)
  -> [value]
  -> IO ()
rejectWrite name existing construct values = do
  let previous = headerMapFromList [(toCIByteString name, existing)]
  forM_ values $ \value -> do
    renderHeaderLines defaultHeaderSettings (construct value) `shouldBe` Nothing
    setHeader (construct value) previous `shouldBe` previous


checkReference
  :: (KnownHeader a, Eq a, Show a, Eq (ParseFailure a), Show (ParseFailure a), HeaderRenderingResultToNonEmpty (Cardinality a))
  => HeaderFieldName
  -> (ShortText -> a)
  -> ShortText
  -> IO ()
checkReference name construct spelling = do
  let field = construct spelling
      bytes = ST.toByteString spelling
  lookupHeader (headerMapFromList [(toCIByteString name, " \t" <> bytes <> "\t ")]) `shouldBe` Right (Just field)
  lookupRawHeader name (setHeader field (headerMapFromList [])) `shouldBe` Just (bytes :| [])


reject :: forall a. (KnownHeader a) => HeaderFieldName -> ByteString -> IO ()
reject name value = isLeft (lookupHeader @a (headerMapFromList [(toCIByteString name, value)])) `shouldBe` True


rejectDuplicates :: forall a. (KnownHeader a) => HeaderFieldName -> ByteString -> IO ()
rejectDuplicates name value =
  let line = (toCIByteString name, value)
  in isLeft (lookupHeader @a (headerMapFromList [line, line])) `shouldBe` True
