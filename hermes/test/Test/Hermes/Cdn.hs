{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Cdn (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
import qualified Network.HTTP.Headers.CDNCacheControl as CCC
import qualified Network.HTTP.Headers.CDNLoop as CL
import qualified Network.HTTP.Headers.CacheControl as CC
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


isWs :: Word8 -> Bool
isWs w = w == 0x20 || w == 0x09


parseCC :: ByteString -> Either String CCC.CDNCacheControl
parseCC bs = case runParser CCC.cdnCacheControlParser bs of
  OK v rest
    | BS.null (BS.dropWhile isWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


parseCL :: ByteString -> Either String CL.CDNLoop
parseCL bs = case runParser CL.cdnLoopParser bs of
  OK v rest
    | BS.null (BS.dropWhile isWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCC :: CCC.CDNCacheControl -> ByteString
renderCC = M.toStrictByteString . CCC.renderCDNCacheControl


renderCL :: CL.CDNLoop -> ByteString
renderCL = M.toStrictByteString . CL.renderCDNLoop


-- CDN-Cache-Control -----------------------------------------------------------

unit_cc_parse :: Spec
unit_cc_parse = it "parses public, max-age" $
  case parseCC "public, max-age=600" of
    Right (CCC.CDNCacheControl (CC.Public :| [CC.MaxAge 600])) -> pure () :: IO ()
    other -> error (show other)


unit_cc_render :: Spec
unit_cc_render =
  it "renders directive list" $
    let v = CCC.CDNCacheControl (CC.NoStore :| [CC.MaxAge 0])
    in renderCC v `shouldBe` "no-store, max-age=0"


genWord :: Gen Word
genWord = Gen.word (Range.linear 0 1_000_000)


genDirective :: Gen CC.CacheControlDirective
genDirective =
  Gen.choice
    [ pure CC.Public
    , pure CC.NoStore
    , pure CC.MustRevalidate
    , pure CC.ProxyRevalidate
    , pure CC.NoTransform
    , pure CC.Immutable
    , CC.MaxAge <$> genWord
    , CC.SMaxAge <$> genWord
    , CC.StaleWhileRevalidate <$> genWord
    , CC.StaleIfError <$> genWord
    ]


prop_cc_roundtrip :: Property
prop_cc_roundtrip = property $ do
  ds <- forAll (genNonEmpty genDirective)
  let v = CCC.CDNCacheControl ds
      bs = renderCC v
  case parseCC bs of
    Right v' -> renderCC v' === bs
    Left err -> error (err <> " on " <> show bs)


cdnDictionaryTests :: Spec
cdnDictionaryTests = do
  it "applies the last dictionary value and retains the first key position across lines" $ do
    let headers =
          headerMapFromList
            [ ("CDN-Cache-Control", "max-age=?0, public, no-store=?0")
            , ("cdn-cache-control", "no-store=?1, max-age=60")
            ]
        expected = CCC.CDNCacheControl (CC.MaxAge 60 :| [CC.Public, CC.NoStore])
    lookupHeader @CCC.CDNCacheControl headers `shouldBe` Right (Just expected)
    renderCC expected `shouldBe` "max-age=60, public, no-store"
    renderCC (CCC.CDNCacheControl (CC.MaxAge 5 :| [CC.NoStore, CC.MaxAge 60]))
      `shouldBe` "max-age=60, no-store"

  it "validates parameters and extensions while projecting the understood directives" $ do
    let wire =
          "no-cache=\"Set-Cookie, X-Cache\";tag=1;hint;tag=2, "
            <> "debug=(@23;valid %\"caf%c3%a9\");lang=en, "
            <> "max-age=60;origin=?1"
        canonical =
          "no-cache=\"Set-Cookie, X-Cache\";tag=2;hint, "
            <> "debug=(@23;valid %\"caf%c3%a9\");lang=en, "
            <> "max-age=60;origin"
    case parseCC wire of
      Left err -> expectationFailure err
      Right value -> do
        CCC.cdnCacheControlDirectiveList value `shouldBe` [CC.NoCache (Just "Set-Cookie, X-Cache"), CC.MaxAge 60]
        renderCC value `shouldBe` canonical
        parseCC canonical `shouldBe` Right value

  it "represents a received empty field and omits it when rendering" $ do
    let headers = headerMapFromList [("CDN-Cache-Control", "  ")]
    lookupHeader @CCC.CDNCacheControl headers `shouldBe` Right (Just CCC.CDNCacheControlEmpty)
    let populated = headerMapFromList [("CDN-Cache-Control", "max-age=60"), ("Cache-Control", "no-store")]
    headerMapToList (setHeader CCC.CDNCacheControlEmpty populated) `shouldBe` [("cache-control", "no-store")]

  it "rejects malformed dictionary syntax and invalid known directive types" $
    mapM_
      (\wire -> parseCC wire `shouldSatisfy` isLeft)
      [ "Max-age=60"
      , "max-age=60,"
      , "max-age=60,,no-store"
      , "max-age =60"
      , "max-age"
      , "max-age=\"60\""
      , "max-age=-1"
      , "max-age=1.0"
      , "max-age=1000000000000000"
      , "no-store=?0"
      , "no-cache=Set-Cookie"
      , "private=\"abc:x\""
      , "no-store;\tdebug"
      , "no-store;debug=\"bad\\e\""
      , "no-store;debug=%\"%C3%a9\""
      , "no-store;debug=%\"%ff\""
      , "max-age=(60)"
      ]

  it "reports an unrepresentable response directive before rendering" $ do
    let invalid = CCC.CDNCacheControl (CC.MaxAge maxBound :| [])
    isLeft (CCC.renderCDNCacheControlChecked invalid) `shouldBe` True
    let fields = headerMapFromList [("CDN-Cache-Control", "max-age=60")]
    setHeader invalid fields `shouldBe` fields


-- CDN-Loop --------------------------------------------------------------------

unit_cl_parse :: Spec
unit_cl_parse = it "parses cdn-info entries with parameters" $
  case parseCL "foo123.foocdn.example, barcdn.example; trace=abc" of
    Right (CL.CDNLoop (a :| [b])) -> do
      CL.cdnInfoId a `shouldBe` ST.fromString "foo123.foocdn.example"
      CL.cdnInfoParameters a `shouldBe` []
      CL.cdnInfoId b `shouldBe` ST.fromString "barcdn.example"
      CL.cdnInfoParameters b `shouldBe` [(ST.fromString "trace", ST.fromString "abc")]
    other -> error (show other)


unit_cl_render :: Spec
unit_cl_render =
  it "renders cdn-info list" $
    let v =
          CL.CDNLoop
            ( CL.CDNInfo (ST.fromString "alpha.example") []
                :| [CL.CDNInfo (ST.fromString "beta.example") [(ST.fromString "k", ST.fromString "v")]]
            )
    in renderCL v `shouldBe` "alpha.example, beta.example;k=v"


unit_cl_field_lines :: Spec
unit_cl_field_lines = it "reads CDN authorities and re-quotes decoded parameters" $ do
  let readLoop = lookupHeader @CL.CDNLoop . headerMapFromList . map (\field -> ("CDN-Loop", field))
      ipv6 = "[2001:db8::1]:0080"
      expected = CL.CDNLoop
        (CL.CDNInfo ipv6 [("trace", "text, \xff; \"quote\"\\path")] :| [CL.CDNInfo "cdn.example:70000" [], CL.CDNInfo "edge#id" []])
      wire = "[2001:db8::1]:0080;trace=\"text, \xff; \\\"quote\\\"\\\\path\", cdn.example:70000, edge#id"
  readLoop [" , [2001:db8::1]:0080;trace=\"text", "\xff; \\\"quote\\\"\\\\path\", cdn.example:70000,,edge#id,"]
    `shouldBe` Right (Just expected)
  headerMapToList (setHeader expected (headerMapFromList [("CDN-Loop", "earlier")]))
    `shouldBe` [("cdn-loop", wire)]
  renderCL expected `shouldBe` wire
  readLoop [wire] `shouldBe` Right (Just expected)
  readLoop [" \t, ", " ,"] `shouldBe` Right (Just CL.CDNLoopEmpty)
  headerMapToList (setHeader CL.CDNLoopEmpty (headerMapFromList [("CDN-Loop", "earlier")])) `shouldBe` []
  mapM_ (\invalid -> readLoop ["edge", invalid] `shouldSatisfy` isLeft)
    ["user@cdn.example", "cdn.example:abc", "[no-ip]:42", "cdn.example;k=", "cdn.example;k=\"unterminated", "cdn.example;k=\"\n\""]


genToken :: Gen ST.ShortText
genToken = ST.fromString <$> Gen.string (Range.linear 1 12) tokenChar
  where
    tokenChar = Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-.")


genCDNInfo :: Gen CL.CDNInfo
genCDNInfo =
  CL.CDNInfo
    <$> genToken
    <*> Gen.list (Range.linear 0 3) ((,) <$> genToken <*> genToken)


prop_cl_roundtrip :: Property
prop_cl_roundtrip = property $ do
  infos <- forAll (genNonEmpty genCDNInfo)
  let v = CL.CDNLoop infos
      bs = renderCL v
  case parseCL bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Helpers ---------------------------------------------------------------------

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = (:|) <$> g <*> Gen.list (Range.linear 0 4) g


tests :: Spec
tests =
  describe "Cdn" $
    sequence_
      [ unit_cc_parse
      , unit_cc_render
      , cdnDictionaryTests
      , it "CDN-Cache-Control round-trips its canonical dictionary" prop_cc_roundtrip
      , unit_cl_parse
      , unit_cl_render
      , unit_cl_field_lines
      , it "CDN-Loop round-trip" prop_cl_roundtrip
      ]
