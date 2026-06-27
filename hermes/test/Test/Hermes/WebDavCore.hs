{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.WebDavCore (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers.DAV
import Network.HTTP.Headers.Depth
import Network.HTTP.Headers.Destination
import Network.HTTP.Headers.If
import Network.HTTP.Headers.LockToken
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Overwrite
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Network.HTTP.Headers.Timeout
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

checkParse :: Result String a -> Either String a
checkParse = \case
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: (a -> M.Builder) -> a -> ByteString
render f = M.toStrictByteString . f


-- ---------------------------------------------------------------------------
-- Depth
-- ---------------------------------------------------------------------------

unit_depth_parse :: Spec
unit_depth_parse = it "parses 0 / 1 / infinity" $ do
  checkParse (runParser depthParser "0") `shouldBe` Right Depth0
  checkParse (runParser depthParser "1") `shouldBe` Right Depth1
  checkParse (runParser depthParser "infinity") `shouldBe` Right DepthInfinity


unit_depth_render :: Spec
unit_depth_render = it "renders Depth values" $ do
  render renderDepth DepthInfinity `shouldBe` "infinity"
  render renderDepth Depth0 `shouldBe` "0"


depthGen :: Gen Depth
depthGen = Gen.element [Depth0, Depth1, DepthInfinity]


prop_depth :: Property
prop_depth = property $ do
  v <- forAll depthGen
  let bs = render renderDepth v
  case checkParse (runParser depthParser bs) of
    Right r -> r === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Overwrite
-- ---------------------------------------------------------------------------

unit_overwrite_parse :: Spec
unit_overwrite_parse = it "parses T / F" $ do
  checkParse (runParser overwriteParser "T") `shouldBe` Right (Overwrite True)
  checkParse (runParser overwriteParser "F") `shouldBe` Right (Overwrite False)


unit_overwrite_render :: Spec
unit_overwrite_render = it "renders Overwrite flag" $ do
  render renderOverwrite (Overwrite True) `shouldBe` "T"
  render renderOverwrite (Overwrite False) `shouldBe` "F"


prop_overwrite :: Property
prop_overwrite = property $ do
  v <- forAll (Overwrite <$> Gen.bool)
  let bs = render renderOverwrite v
  case checkParse (runParser overwriteParser bs) of
    Right r -> r === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Destination
-- ---------------------------------------------------------------------------

unit_destination_parse :: Spec
unit_destination_parse =
  it "parses an opaque URI" $
    checkParse (runParser destinationParser "http://www.example.com/other/C2/")
      `shouldBe` Right (Destination (ST.fromString "http://www.example.com/other/C2/"))


unit_destination_render :: Spec
unit_destination_render =
  it "renders the URI verbatim" $
    render renderDestination (Destination (ST.fromString "/path?a=b"))
      `shouldBe` "/path?a=b"


-- ---------------------------------------------------------------------------
-- If
-- ---------------------------------------------------------------------------

unit_if_parse :: Spec
unit_if_parse =
  it "preserves the raw condition list" $
    checkParse (runParser ifParser "(<urn:uuid:181d4fae> [\"etag\"])")
      `shouldBe` Right (If (ST.fromString "(<urn:uuid:181d4fae> [\"etag\"])"))


unit_if_render :: Spec
unit_if_render =
  it "renders the raw condition list verbatim" $
    render renderIf (If (ST.fromString "(Not <urn:uuid:abc>)"))
      `shouldBe` "(Not <urn:uuid:abc>)"


-- ---------------------------------------------------------------------------
-- Lock-Token
-- ---------------------------------------------------------------------------

unit_locktoken_parse :: Spec
unit_locktoken_parse =
  it "parses a Coded-URL" $
    checkParse (runParser lockTokenParser "<urn:uuid:a515cfa4-5da4-22e1-f5b5-00a0451e6bf7>")
      `shouldBe` Right (LockToken (ST.fromString "urn:uuid:a515cfa4-5da4-22e1-f5b5-00a0451e6bf7"))


unit_locktoken_render :: Spec
unit_locktoken_render =
  it "renders with angle brackets" $
    render renderLockToken (LockToken (ST.fromString "opaquelocktoken:foo"))
      `shouldBe` "<opaquelocktoken:foo>"


uriText :: Gen ST.ShortText
uriText = ST.fromString <$> Gen.string (Range.linear 1 20) (Gen.element uriChars)
  where
    uriChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ":/?#@.-_~"


prop_locktoken :: Property
prop_locktoken = property $ do
  v <- forAll (LockToken <$> uriText)
  let bs = render renderLockToken v
  case checkParse (runParser lockTokenParser bs) of
    Right r -> r === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- DAV
-- ---------------------------------------------------------------------------

unit_dav_parse :: Spec
unit_dav_parse = it "parses a mixed compliance-class list" $
  case checkParse (runParser davParser "1, 2, <http://apache.org/dav/propset/fs/1>") of
    Right (DAV (a :| [b, c])) -> do
      a `shouldBe` ComplianceToken (ST.fromString "1")
      b `shouldBe` ComplianceToken (ST.fromString "2")
      c `shouldBe` ComplianceURL (ST.fromString "http://apache.org/dav/propset/fs/1")
    other -> error (show other)


unit_dav_render :: Spec
unit_dav_render =
  it "renders compliance classes comma-separated" $
    render
      renderDAV
      (DAV (ComplianceToken (ST.fromString "1") :| [ComplianceURL (ST.fromString "http://x/y")]))
      `shouldBe` "1, <http://x/y>"


tokenText :: Gen ST.ShortText
tokenText = ST.fromString <$> Gen.string (Range.linear 1 10) (Gen.element tokenChars)
  where
    tokenChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-._~"


complianceGen :: Gen ComplianceClass
complianceGen =
  Gen.choice
    [ ComplianceToken <$> tokenText
    , ComplianceURL <$> uriText
    ]


neGen :: Gen a -> Gen (NonEmpty a)
neGen g = (:|) <$> g <*> Gen.list (Range.linear 0 4) g


prop_dav :: Property
prop_dav = property $ do
  v <- forAll (DAV <$> neGen complianceGen)
  let bs = render renderDAV v
  case checkParse (runParser davParser bs) of
    Right r -> r === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Timeout
-- ---------------------------------------------------------------------------

unit_timeout_parse :: Spec
unit_timeout_parse = it "parses Second-<n> and Infinite" $
  case checkParse (runParser timeoutParser "Second-120, Infinite") of
    Right (Timeout (a :| [b])) -> do
      a `shouldBe` TimeSeconds 120
      b `shouldBe` TimeInfinite
    other -> error (show other)


unit_timeout_render :: Spec
unit_timeout_render =
  it "renders the TimeType list" $
    render renderTimeout (Timeout (TimeSeconds 4100000000 :| [TimeInfinite]))
      `shouldBe` "Second-4100000000, Infinite"


timeTypeGen :: Gen TimeType
timeTypeGen =
  Gen.choice
    [ TimeSeconds <$> Gen.word (Range.linear 0 1000000)
    , pure TimeInfinite
    ]


prop_timeout :: Property
prop_timeout = property $ do
  v <- forAll (Timeout <$> neGen timeTypeGen)
  let bs = render renderTimeout v
  case checkParse (runParser timeoutParser bs) of
    Right r -> r === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "WebDavCore" $
    sequence_
      [ unit_depth_parse
      , unit_depth_render
      , it "Depth round-trips" prop_depth
      , unit_overwrite_parse
      , unit_overwrite_render
      , it "Overwrite round-trips" prop_overwrite
      , unit_destination_parse
      , unit_destination_render
      , unit_if_parse
      , unit_if_render
      , unit_locktoken_parse
      , unit_locktoken_render
      , it "Lock-Token round-trips" prop_locktoken
      , unit_dav_parse
      , unit_dav_render
      , it "DAV round-trips" prop_dav
      , unit_timeout_parse
      , unit_timeout_render
      , it "Timeout round-trips" prop_timeout
      ]
