{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteProtocol (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.MethodCheck as MethodCheck
import qualified Network.HTTP.Headers.MethodCheckExpires as MethodCheckExpires
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.Protocol as Protocol
import qualified Network.HTTP.Headers.ProtocolInfo as ProtocolInfo
import qualified Network.HTTP.Headers.ProtocolQuery as ProtocolQuery
import qualified Network.HTTP.Headers.ProtocolRequest as ProtocolRequest
import qualified Network.HTTP.Headers.ProxyFeatures as ProxyFeatures
import qualified Network.HTTP.Headers.ProxyInstruction as ProxyInstruction
import qualified Network.HTTP.Headers.PublicHeader as PublicHeader
import qualified Network.HTTP.Headers.RefererRoot as RefererRoot
import qualified Network.HTTP.Headers.Safe as Safe
import qualified Network.HTTP.Headers.SecurityScheme as SecurityScheme
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser and require the leftover to be only optional whitespace.
parseOk :: ParserT st String a -> ByteString -> Either String a
parseOk p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


renderBS :: (a -> M.Builder) -> a -> ByteString
renderBS f = M.toStrictByteString . f


-- Opaque, raw-preserving headers --------------------------------------------

unit_protocol :: Spec
unit_protocol = it "Protocol preserves opaque value" $ do
  parseOk Protocol.protocolParser "{PICS-1.1 labels}"
    `shouldBe` Right (Protocol.Protocol (ST.fromString "{PICS-1.1 labels}"))
  renderBS Protocol.renderProtocol (Protocol.Protocol (ST.fromString "{PICS-1.1 labels}"))
    `shouldBe` "{PICS-1.1 labels}"


unit_protocol_request :: Spec
unit_protocol_request = it "Protocol-Request preserves opaque value" $ do
  parseOk ProtocolRequest.protocolRequestParser "service=label; version=1.1"
    `shouldBe` Right (ProtocolRequest.ProtocolRequest (ST.fromString "service=label; version=1.1"))
  renderBS
    ProtocolRequest.renderProtocolRequest
    (ProtocolRequest.ProtocolRequest (ST.fromString "service=label; version=1.1"))
    `shouldBe` "service=label; version=1.1"


unit_protocol_info :: Spec
unit_protocol_info = it "Protocol-Info preserves opaque value" $ do
  parseOk ProtocolInfo.protocolInfoParser "jepi/1.0 negotiate"
    `shouldBe` Right (ProtocolInfo.ProtocolInfo (ST.fromString "jepi/1.0 negotiate"))
  renderBS
    ProtocolInfo.renderProtocolInfo
    (ProtocolInfo.ProtocolInfo (ST.fromString "jepi/1.0 negotiate"))
    `shouldBe` "jepi/1.0 negotiate"


unit_protocol_query :: Spec
unit_protocol_query = it "Protocol-Query preserves opaque value" $ do
  parseOk ProtocolQuery.protocolQueryParser "jepi/1.0 query"
    `shouldBe` Right (ProtocolQuery.ProtocolQuery (ST.fromString "jepi/1.0 query"))
  renderBS
    ProtocolQuery.renderProtocolQuery
    (ProtocolQuery.ProtocolQuery (ST.fromString "jepi/1.0 query"))
    `shouldBe` "jepi/1.0 query"


unit_method_check :: Spec
unit_method_check = it "Method-Check preserves opaque value" $ do
  parseOk MethodCheck.methodCheckParser "(GET PUT DELETE)"
    `shouldBe` Right (MethodCheck.MethodCheck (ST.fromString "(GET PUT DELETE)"))
  renderBS
    MethodCheck.renderMethodCheck
    (MethodCheck.MethodCheck (ST.fromString "(GET PUT DELETE)"))
    `shouldBe` "(GET PUT DELETE)"


unit_method_check_expires :: Spec
unit_method_check_expires = it "Method-Check-Expires preserves opaque value" $ do
  parseOk MethodCheckExpires.methodCheckExpiresParser "Wed, 21 Oct 2015 07:28:00 GMT"
    `shouldBe` Right (MethodCheckExpires.MethodCheckExpires (ST.fromString "Wed, 21 Oct 2015 07:28:00 GMT"))
  renderBS
    MethodCheckExpires.renderMethodCheckExpires
    (MethodCheckExpires.MethodCheckExpires (ST.fromString "Wed, 21 Oct 2015 07:28:00 GMT"))
    `shouldBe` "Wed, 21 Oct 2015 07:28:00 GMT"


unit_proxy_features :: Spec
unit_proxy_features = it "Proxy-Features preserves opaque value" $ do
  parseOk ProxyFeatures.proxyFeaturesParser "vary-content, push"
    `shouldBe` Right (ProxyFeatures.ProxyFeatures (ST.fromString "vary-content, push"))
  renderBS
    ProxyFeatures.renderProxyFeatures
    (ProxyFeatures.ProxyFeatures (ST.fromString "vary-content, push"))
    `shouldBe` "vary-content, push"


unit_proxy_instruction :: Spec
unit_proxy_instruction = it "Proxy-Instruction preserves opaque value" $ do
  parseOk ProxyInstruction.proxyInstructionParser "no-store; flush"
    `shouldBe` Right (ProxyInstruction.ProxyInstruction (ST.fromString "no-store; flush"))
  renderBS
    ProxyInstruction.renderProxyInstruction
    (ProxyInstruction.ProxyInstruction (ST.fromString "no-store; flush"))
    `shouldBe` "no-store; flush"


unit_security_scheme :: Spec
unit_security_scheme = it "Security-Scheme preserves opaque value" $ do
  parseOk SecurityScheme.securitySchemeParser "S-HTTP/1.4"
    `shouldBe` Right (SecurityScheme.SecurityScheme (ST.fromString "S-HTTP/1.4"))
  renderBS
    SecurityScheme.renderSecurityScheme
    (SecurityScheme.SecurityScheme (ST.fromString "S-HTTP/1.4"))
    `shouldBe` "S-HTTP/1.4"


unit_referer_root :: Spec
unit_referer_root = it "Referer-Root preserves opaque value" $ do
  parseOk RefererRoot.refererRootParser "https://example.com"
    `shouldBe` Right (RefererRoot.RefererRoot (ST.fromString "https://example.com"))
  renderBS
    RefererRoot.renderRefererRoot
    (RefererRoot.RefererRoot (ST.fromString "https://example.com"))
    `shouldBe` "https://example.com"


-- Structured: Public ---------------------------------------------------------

unit_public :: Spec
unit_public = it "Public parses a comma-separated method list" $
  case parseOk PublicHeader.publicHeaderParser "OPTIONS, GET, HEAD" of
    Right v ->
      NE.toList (PublicHeader.publicMethods v)
        `shouldBe` map ST.fromString ["OPTIONS", "GET", "HEAD"]
    other -> error (show other)


unit_render_public :: Spec
unit_render_public =
  it "Public renders a comma-separated method list" $
    renderBS
      PublicHeader.renderPublicHeader
      (PublicHeader.PublicHeader (ST.fromString "GET" :| [ST.fromString "HEAD"]))
      `shouldBe` "GET, HEAD"


genMethod :: Gen ST.ShortText
genMethod = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element ['A' .. 'Z'])


genPublic :: Gen PublicHeader.PublicHeader
genPublic = PublicHeader.PublicHeader <$> Gen.nonEmpty (Range.linear 1 5) genMethod


prop_public :: Property
prop_public = property $ do
  v <- forAll genPublic
  let bs = renderBS PublicHeader.renderPublicHeader v
  case parseOk PublicHeader.publicHeaderParser bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


-- Structured: Safe -----------------------------------------------------------

unit_safe :: Spec
unit_safe = it "Safe parses yes/no case-insensitively" $ do
  parseOk Safe.safeParser "yes" `shouldBe` Right Safe.SafeYes
  parseOk Safe.safeParser "NO" `shouldBe` Right Safe.SafeNo


unit_render_safe :: Spec
unit_render_safe = it "Safe renders canonical lowercase" $ do
  renderBS Safe.renderSafe Safe.SafeYes `shouldBe` "yes"
  renderBS Safe.renderSafe Safe.SafeNo `shouldBe` "no"


genSafe :: Gen Safe.Safe
genSafe = Gen.element [Safe.SafeYes, Safe.SafeNo]


prop_safe :: Property
prop_safe = property $ do
  v <- forAll genSafe
  let bs = renderBS Safe.renderSafe v
  case parseOk Safe.safeParser bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


tests :: Spec
tests =
  describe "ObsoleteProtocol" $
    sequence_
      [ unit_protocol
      , unit_protocol_request
      , unit_protocol_info
      , unit_protocol_query
      , unit_method_check
      , unit_method_check_expires
      , unit_proxy_features
      , unit_proxy_instruction
      , unit_security_scheme
      , unit_referer_root
      , unit_public
      , unit_render_public
      , it "Public round-trips" prop_public
      , unit_safe
      , unit_render_safe
      , it "Safe round-trips" prop_safe
      ]
