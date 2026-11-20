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
import Network.HTTP.Headers.ContentLocation (ContentLocation (..))
import Network.HTTP.Headers.HeaderFieldName
import Network.HTTP.Headers.Location (Location (..))
import Network.HTTP.Headers.PingFrom (PingFrom (..))
import Network.HTTP.Headers.PingTo (PingTo (..))
import Network.HTTP.Headers.Referer (Referer (..))
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
