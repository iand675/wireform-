{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.WebDavCore (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
import Network.HTTP.Headers.DAV
import Network.HTTP.Headers.Depth
import Network.HTTP.Headers.Destination
import Network.HTTP.Headers.ETag (EntityTag (..))
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


unit_if_typed :: Spec
unit_if_typed = it "decodes tagged alternatives, negation and opaque entity-tag bytes" $ do
  let tagBody = BS.snoc (BS.replicate 4096 0xff) 0x5d
      wire = "</dav/doc?q=%2F> (not[W/\"" <> tagBody <> "\"] <urn:uuid:one>)([\"other\"]) <https://example.org> (<urn:uuid:two>)"
      fields = headerMapFromList [("If", " \t" <> wire <> " ")]
      matchTag tag = IfCondition False (IfEntityTag (StrongETagBytes tag))
      matchToken uri = IfCondition False (IfStateToken uri)
      expected =
        IfTagged $
          IfResource
            "/dav/doc?q=%2F"
            ( IfList (IfCondition True (IfEntityTag (WeakETagBytes (SBS.toShort tagBody))) :| [matchToken "urn:uuid:one"])
                :| [IfList (matchTag "other" :| [])]
            )
            :| [IfResource "https://example.org" (IfList (matchToken "urn:uuid:two" :| []) :| [])]
      canonical = "</dav/doc?q=%2F> (Not [W/\"" <> tagBody <> "\"] <urn:uuid:one>) ([\"other\"]) <https://example.org> (<urn:uuid:two>)"
      write value = headerMapToList (setHeader @If value (headerMapFromList []))
  case lookupHeader @If fields of
    Right (Just value) -> do
      ifConditionLists value `shouldBe` Right expected
      ifConditionsBytes value `shouldBe` SBS.toShort wire
      write value `shouldBe` [("if", wire)]
      write (fromIfConditionLists expected) `shouldBe` [("if", canonical)]
      render renderIf (fromIfConditionLists expected) `shouldBe` canonical
      M.toStrictByteString ("prefix=" <> renderIf (fromIfConditionLists expected) <> ";suffix")
        `shouldBe` "prefix=" <> canonical <> ";suffix"
    other -> expectationFailure (show other)


unit_if_invalid :: Spec
unit_if_invalid = it "rejects mixed list modes, empty conditions and invalid operand syntax" $ do
  let field wire = ("If", wire)
      readIf = lookupHeader @If . headerMapFromList
  readIf [field "(<urn:one>)", field "(<urn:two>)"] `shouldSatisfy` isLeft
  mapM_
    (\wire -> readIf [field wire] `shouldSatisfy` isLeft)
    [ ""
    , "()"
    , "(/dav/x)"
    , "([ \"tag\"])"
    , "([\"tag\" ])"
    , "([*])"
    , "(([\"nested\"]))"
    , "(Not Not <urn:uuid:c>)"
    , "(<relative>)"
    , "(</dav/x>)"
    , "(<urn:uuid:x#fragment>)"
    , "(/docs <urn:one>)"
    , "(<urn:one>) </a> (<urn:two>)"
    , "</a>"
    , "</a> <https://example.org/a> (<urn:one>)"
    , "<relative> (<urn:one>)"
    , "<//example.org> (<urn:one>)"
    , "</ path> (<urn:one>)"
    ]


prop_if :: Property
prop_if = property $ do
  lists <- forAll (Gen.choice [IfUntagged <$> neGen genList, IfTagged <$> neGen genResource])
  let value = fromIfConditionLists lists
      fields = setHeader value (headerMapFromList [])
  fmap (fmap ifConditionLists) (lookupHeader @If fields) === Right (Just (Right lists))
  where
    genResource = IfResource <$> Gen.element ["/docs/a?q=1", "https://example.test", "https://[::1]:8443/"] <*> neGen genList
    genList = IfList <$> neGen (IfCondition <$> Gen.bool <*> genOperand)
    genOperand = Gen.choice [IfStateToken <$> uriText, IfEntityTag <$> genTag]
    genTag = Gen.element [StrongETagBytes "", WeakETagBytes "revision", StrongETagBytes (SBS.toShort (BS.pack [0x80, 0xff, 0x5d, 0x5c]))]


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
uriText = ST.fromString . ("urn:example:" ++) <$> Gen.string (Range.linear 1 20) (Gen.element uriChars)
  where
    uriChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ":/?@.-_~"


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


typedFields :: Spec
typedFields = describe "typed field lines" $ do
  it "validates Destination and Lock-Token URIs and rejects repeated singleton fields" $ do
    let destination wire = ("Destination", wire)
        lockToken wire = ("Lock-Token", wire)
        readDestination = lookupHeader @Destination . headerMapFromList
        readLockToken = lookupHeader @LockToken . headerMapFromList
    readDestination [destination "\t/a%20b?q=/ "]
      `shouldBe` Right (Just (Destination "/a%20b?q=/"))
    readDestination [destination "https://[2001:db8::1]:443/"]
      `shouldBe` Right (Just (Destination "https://[2001:db8::1]:443/"))
    readDestination [destination "/a", destination "/b"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> readDestination [destination wire] `shouldSatisfy` isLeft)
      ["", "relative", "//example.org", "/x#anchor", "/a%0Z", "https://[broken]/", "/space here"]
    readLockToken [lockToken " <https://lock.example>\t"]
      `shouldBe` Right (Just (LockToken "https://lock.example"))
    readLockToken [lockToken "<urn:one>", lockToken "<urn:two>"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> readLockToken [lockToken wire] `shouldSatisfy` isLeft)
      ["<>", "</absolute-path>", "< urn:one>", "<urn:one >", "<urn:one#fragment>", "<urn:a%ZZ>", "<urn:one>garbage"]

  it "combines DAV classes and Timeout choices across field lines" $ do
    let dav wire = ("DAV", wire)
        timeout wire = ("Timeout", wire)
        readDAV = lookupHeader @DAV . headerMapFromList
        readTimeout = lookupHeader @Timeout . headerMapFromList
    readDAV [dav " , 1,, ", dav "<https://dav.example>, 3"]
      `shouldBe` Right (Just (DAV (ComplianceToken "1" :| [ComplianceURL "https://dav.example", ComplianceToken "3"])))
    readDAV [dav ",,"] `shouldBe` Right (Just DAVEmpty)
    readDAV [dav "1, </relative>"] `shouldSatisfy` isLeft
    readTimeout [timeout ",sEcOnD-0004294967295, ", timeout "infinite, Second-0"]
      `shouldBe` Right (Just (Timeout (TimeSeconds 4294967295 :| [TimeInfinite, TimeSeconds 0])))
    mapM_
      (\wire -> readTimeout [timeout wire] `shouldSatisfy` isLeft)
      ["", ",,", "Second-4294967296", "Second-18446744073709551616", "Second- 1", "Second-+1", "InfiniteX"]

  it "accepts case-insensitive Depth and Overwrite literals with singleton cardinality" $ do
    let depth wire = ("Depth", wire)
        overwrite wire = ("Overwrite", wire)
        readDepth = lookupHeader @Depth . headerMapFromList
        readOverwrite = lookupHeader @Overwrite . headerMapFromList
    readDepth [depth " InFInIty\t"] `shouldBe` Right (Just DepthInfinity)
    readOverwrite [overwrite "\tf "] `shouldBe` Right (Just (Overwrite False))
    readDepth [depth "0", depth "1"] `shouldSatisfy` isLeft
    readOverwrite [overwrite "T", overwrite "F"] `shouldSatisfy` isLeft
    readDepth [depth "infinity0"] `shouldSatisfy` isLeft
    readOverwrite [overwrite "True"] `shouldSatisfy` isLeft


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
      , unit_if_typed
      , unit_if_invalid
      , it "typed If conditions round-trip" prop_if
      , unit_locktoken_parse
      , unit_locktoken_render
      , it "Lock-Token round-trips" prop_locktoken
      , unit_dav_parse
      , unit_dav_render
      , it "DAV round-trips" prop_dav
      , unit_timeout_parse
      , unit_timeout_render
      , it "Timeout round-trips" prop_timeout
      , typedFields
      ]
