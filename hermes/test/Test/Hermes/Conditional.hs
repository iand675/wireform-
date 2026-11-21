{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Hermes.Conditional (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either (isLeft)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import qualified Data.Text.Short as ST
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (HeaderCardinality (ZeroOrOne), KnownHeader (Cardinality, headerName), defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, renderHeaderLines, setHeader, setRawHeader)
import qualified Network.HTTP.Headers.DeltaBase as DB
import qualified Network.HTTP.Headers.ETag as E
import qualified Network.HTTP.Headers.From as From
import qualified Network.HTTP.Headers.IfMatch as IM
import qualified Network.HTTP.Headers.IfNoneMatch as INM
import qualified Network.HTTP.Headers.IfRange as IR
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.Mailbox as MB
import Test.Syd
import Test.Syd.Hedgehog ()


{- | Parse fully, tolerating only trailing optional whitespace (mirrors the
leftover handling in the AcceptRanges/ContentRange tests).
-}
parseWith :: (forall st. ParserT st String a) -> ByteString -> Either String a
parseWith p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


-- Generators ----------------------------------------------------------------

genEntityTag :: Gen E.EntityTag
genEntityTag = do
  wrap <- Gen.element [E.StrongETagBytes, E.WeakETagBytes]
  wrap . SBS.pack <$> Gen.list (Range.linear 0 20) (Gen.element (0x21 : [0x23 .. 0x7e] <> [0x80 .. 0xff]))


genIfNoneMatch :: Gen INM.IfNoneMatch
genIfNoneMatch =
  Gen.choice
    [ pure INM.IfNoneMatchAny
    , INM.IfNoneMatchTags <$> genTagList
    ]
  where
    genTagList = do
      t <- genEntityTag
      ts <- Gen.list (Range.linear 0 4) genEntityTag
      pure (t :| ts)


-- A simple, canonical lowercase mailbox of the form @local\@a.b.c@.
genFrom :: Gen From.From
genFrom = do
  lp <- genLabel
  dom <- intercalate "." <$> Gen.list (Range.linear 1 3) genLabel
  pure $
    From.From $
      MB.Mailbox Nothing $
        MB.AddrSpec (ST.fromString lp) (MB.DomainName (ST.fromString dom))
  where
    genLabel = Gen.list (Range.linear 1 8) (Gen.element ['a' .. 'z'])


-- If-None-Match -------------------------------------------------------------

unit_inm_any :: Spec
unit_inm_any = it "parses the wildcard" $ do
  case parseWith INM.ifNoneMatchParser "*" of
    Right INM.IfNoneMatchAny -> pure () :: IO ()
    other -> error (show other)
  M.toStrictByteString (INM.renderIfNoneMatch INM.IfNoneMatchAny) `shouldBe` "*"


unit_inm_tags :: Spec
unit_inm_tags = it "parses a comma list of entity-tags" $
  case parseWith INM.ifNoneMatchParser "\"abc\", W/\"xyz\"" of
    Right (INM.IfNoneMatchTags (a :| [b])) -> do
      a `shouldBe` E.StrongETag (ST.fromString "abc")
      b `shouldBe` E.WeakETag (ST.fromString "xyz")
    other -> error (show other)


unit_inm_render :: Spec
unit_inm_render =
  it "renders an entity-tag list" $
    let v = INM.IfNoneMatchTags (E.StrongETag (ST.fromString "abc") :| [E.WeakETag (ST.fromString "xyz")])
    in M.toStrictByteString (INM.renderIfNoneMatch v) `shouldBe` "\"abc\", W/\"xyz\""


prop_inm_roundtrip :: Property
prop_inm_roundtrip = property $ do
  v <- forAll genIfNoneMatch
  let bs = M.toStrictByteString (INM.renderIfNoneMatch v)
  case parseWith INM.ifNoneMatchParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


fieldLines :: Spec
fieldLines = do
  it "preserves entity tag order across comma lists and repeated field lines" $ do
    let tags = E.StrongETag "a,b" :| [E.WeakETag "next", E.StrongETag ""]
        fields name = headerMapFromList [(name, " , \"a,b\", ,"), (name, " W/\"next\", \"\" \t")]
    lookupHeader @IM.IfMatch (fields "if-match") `shouldBe` Right (Just (IM.IfMatchEntityTags tags))
    lookupHeader @INM.IfNoneMatch (fields "if-none-match") `shouldBe` Right (Just (INM.IfNoneMatchTags tags))
    headerMapToList (setHeader (INM.IfNoneMatchTags tags) (headerMapFromList []))
      `shouldBe` [("if-none-match", "\"a,b\", W/\"next\", \"\"")]
  it "distinguishes an empty precondition list from the wildcard and absence" $ do
    lookupHeader @IM.IfMatch (headerMapFromList [("if-match", " , \t")]) `shouldBe` Right (Just IM.IfMatchNoTags)
    lookupHeader @INM.IfNoneMatch (headerMapFromList [("if-none-match", ",")]) `shouldBe` Right (Just INM.IfNoneMatchNoTags)
    lookupHeader @INM.IfNoneMatch (headerMapFromList [("if-none-match", " * \t")]) `shouldBe` Right (Just INM.IfNoneMatchAny)
    lookupHeader @INM.IfNoneMatch (headerMapFromList []) `shouldBe` Right Nothing
  it "rejects a wildcard combined with another field line and an invalid later tag" $ do
    isLeft (lookupHeader @IM.IfMatch (headerMapFromList [("if-match", "*"), ("if-match", "\"other\"")])) `shouldBe` True
    isLeft (lookupHeader @INM.IfNoneMatch (headerMapFromList [("if-none-match", "\"first\""), ("if-none-match", "bad")])) `shouldBe` True
  it "accepts field whitespace around ETag and rejects duplicate ETag lines" $ do
    lookupHeader @E.ETag (headerMapFromList [("etag", "\tW/\"x\" ")]) `shouldBe` Right (Just (E.ETag (E.WeakETag "x")))
    isLeft (lookupHeader @E.ETag (headerMapFromList [("etag", "\"x\""), ("etag", "\"y\"")])) `shouldBe` True
  it "accepts and preserves exactly the RFC 9110 entity-tag octets" $
    mapM_ checkByte [0 .. 255]
  it "shares the opaque tag codec across match lists, range validators and delta bases" $ do
    let body = BS.pack [0x5c, 0x2c, 0x80, 0xff]
        wire = "\"" <> body <> "\""
        weak = "W/" <> wire
        strongTag = E.StrongETagBytes (SBS.toShort body)
        weakTag = E.WeakETagBytes (SBS.toShort body)
        tags = strongTag :| [weakTag]
        entries name = headerMapFromList [(name, wire), (name, weak)]
        singleton name = headerMapFromList [(name, wire)]
    lookupHeader @IM.IfMatch (entries "if-match") `shouldBe` Right (Just (IM.IfMatchEntityTags tags))
    lookupHeader @INM.IfNoneMatch (entries "if-none-match") `shouldBe` Right (Just (INM.IfNoneMatchTags tags))
    lookupHeader @IR.IfRange (singleton "if-range") `shouldBe` Right (Just (IR.IfRangeETag strongTag))
    lookupHeader @DB.DeltaBase (singleton "delta-base") `shouldBe` Right (Just (DB.DeltaBase strongTag))
    isLeft (lookupHeader @DB.DeltaBase (entries "delta-base")) `shouldBe` True
    headerMapToList (setHeader (INM.IfNoneMatchTags tags) (headerMapFromList []))
      `shouldBe` [("if-none-match", wire <> ", " <> weak)]
  where
    checkByte byte = do
      let wire = BS.pack [0x22, byte, 0x22]
      case E.parseETag wire of
        Right (E.ETag tag) -> do
          (byte == 0x21 || (byte >= 0x23 && byte <= 0x7e) || byte >= 0x80) `shouldBe` True
          E.entityTagBytes tag `shouldBe` SBS.singleton byte
          M.toStrictByteString (E.renderEntityTag tag) `shouldBe` wire
        Left _ -> (byte <= 0x20 || byte == 0x22 || byte == 0x7f) `shouldBe` True


-- If-Range ------------------------------------------------------------------

unit_ifrange_tag :: Spec
unit_ifrange_tag = it "parses an entity-tag validator" $
  case parseWith IR.ifRangeParser "\"abc\"" of
    Right (IR.IfRangeETag t) -> t `shouldBe` E.StrongETag (ST.fromString "abc")
    other -> error (show other)


unit_ifrange_render :: Spec
unit_ifrange_render =
  it "renders an entity-tag validator" $
    let v = IR.IfRangeETag (E.WeakETag (ST.fromString "abc"))
    in M.toStrictByteString (IR.renderIfRange v) `shouldBe` "W/\"abc\""


unit_ifrange_date :: Spec
unit_ifrange_date = it "parses and renders an HTTP-date validator" $
  case parseWith IR.ifRangeParser "Wed, 21 Oct 2015 07:28:00 GMT" of
    Right v@(IR.IfRangeDate _) -> do
      M.toStrictByteString (IR.renderIfRange v) `shouldBe` "Wed, 21 Oct 2015 07:28:00 GMT"
      renderHeaderLines defaultHeaderSettings v `shouldBe` Just ("Wed, 21 Oct 2015 07:28:00 GMT" :| [])
    other -> error (show other)


unit_ifrange_checked_write :: Spec
unit_ifrange_checked_write = it "rejects invalid typed If-Range writes without replacing existing lines" $ do
  let fields = headerMapFromList [("If-Range", "\"previous\"")]
      invalid =
        [ IR.IfRangeETag (E.WeakETagBytes "weak")
        , IR.IfRangeETag (E.StrongETagBytes "bad\"tag")
        , IR.IfRangeETag (E.StrongETagBytes (SBS.pack [0x7f]))
        , IR.IfRangeDate (UTCTime (fromGregorian 1899 12 31) 0)
        , IR.IfRangeDate (UTCTime (fromGregorian 10000 1 1) 0)
        , IR.IfRangeDate (UTCTime (fromGregorian 2024 1 1) 86401)
        ]
  forM_ invalid $ \value -> do
    renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
    setHeader value fields `shouldBe` fields
  let valid = IR.IfRangeETag (E.StrongETagBytes (SBS.pack [0x80, 0xff]))
  lookupHeader @IR.IfRange (setHeader valid fields) `shouldBe` Right (Just valid)
  let writeTag bytes =
        renderHeaderLines defaultHeaderSettings (IR.IfRangeETag (E.StrongETagBytes (SBS.toShort bytes)))
  forM_ [0 .. 16] $ \offset -> do
    let before = BS.replicate offset 0x61
        after = BS.replicate (16 - offset) 0xff
    forM_ [0 .. 255] $ \octet -> do
      let bytes = before <> BS.singleton octet <> after
          expected
            | octet >= 0x21 && octet /= 0x22 && octet /= 0x7f = Just (("\"" <> bytes <> "\"") :| [])
            | otherwise = Nothing
      writeTag bytes `shouldBe` expected
  writeTag BS.empty `shouldBe` Just ("\"\"" :| [])


unit_ifrange_match :: Spec
unit_ifrange_match = it "requires strong tag equality or an exact strong date" $ do
  let field wire = lookupHeader @IR.IfRange (headerMapFromList [("If-Range", wire)])
      opaque = SBS.pack [0x80, 0xff]
      tag = E.StrongETagBytes opaque
      weak = E.WeakETagBytes opaque
      quoted = "\"" <> SBS.fromShort opaque <> "\""
  case (field quoted, field ("W/" <> quoted)) of
    (Right (Just strongField), Right (Just weakField)) -> do
      IR.ifRangeMatches (Just tag) Nothing strongField `shouldBe` True
      IR.ifRangeMatches (Just weak) Nothing strongField `shouldBe` False
      IR.ifRangeMatches (Just tag) Nothing weakField `shouldBe` False
      IR.ifRangeMatches Nothing Nothing strongField `shouldBe` False
    other -> expectationFailure (show other)
  case field "Wed, 21 Oct 2015 07:28:00 GMT" of
    Right (Just validator@(IR.IfRangeDate date)) -> do
      IR.ifRangeMatches Nothing (Just date) validator `shouldBe` True
      IR.ifRangeMatches Nothing (Just (addUTCTime (-1) date)) validator `shouldBe` False
      IR.ifRangeMatches Nothing (Just (addUTCTime 1 date)) validator `shouldBe` False
      IR.ifRangeMatches (Just tag) Nothing validator `shouldBe` False
    other -> expectationFailure (show other)


unit_tag_compare_lengths :: Spec
unit_tag_compare_lengths = it "compares every opaque tag octet across short and long lengths" $
  forM_ ([0 .. 33] <> [63, 64, 65, 256]) $ \size -> do
    let wire = BS.replicate size 0xff
        strong = E.StrongETagBytes (SBS.toShort wire)
        sameBytes = BS.copy wire
        longer = BS.snoc wire 0x80
        field = IR.IfRangeETag . E.StrongETagBytes . SBS.toShort
        matches = IR.ifRangeMatches (Just strong) Nothing . field
        weakMatches = E.weakMatch strong . E.WeakETagBytes . SBS.toShort
    matches sameBytes `shouldBe` True
    weakMatches sameBytes `shouldBe` True
    matches longer `shouldBe` False
    weakMatches longer `shouldBe` False
    forM_ [0 .. size - 1] $ \position -> do
      let difference = BS.take position wire <> BS.singleton 0x80 <> BS.drop (position + 1) wire
      matches difference `shouldBe` False
      weakMatches difference `shouldBe` False


prop_ifrange_roundtrip :: Property
prop_ifrange_roundtrip = property $ do
  v <- forAll (IR.IfRangeETag <$> genEntityTag)
  let bs = M.toStrictByteString (IR.renderIfRange v)
  case parseWith IR.ifRangeParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- From ----------------------------------------------------------------------

unit_from_parse :: Spec
unit_from_parse = it "parses a bare addr-spec mailbox" $
  case parseWith From.fromParser "a@b.com" of
    Right (From.From (MB.Mailbox Nothing (MB.AddrSpec lp (MB.DomainName dom)))) -> do
      lp `shouldBe` ST.fromString "a"
      dom `shouldBe` ST.fromString "b.com"
    other -> error (show other)


unit_from_render :: Spec
unit_from_render =
  it "renders a bare addr-spec mailbox" $
    let v = From.From (MB.Mailbox Nothing (MB.AddrSpec (ST.fromString "a") (MB.DomainName (ST.fromString "b.com"))))
    in M.toStrictByteString (From.renderFrom v) `shouldBe` "a@b.com"


unit_from_field_lines :: Spec
unit_from_field_lines = it "reads a single mailbox with comments and canonicalizes its spelling" $ do
  let field value = ("From", value)
      typed = lookupHeader @From.From . headerMapFromList . map field
      wire = " (sender) Build (queue (ops\\) note)) Bot <(local) \"ci ops\"(label)@(host)[ 192.0.2.4\t ]> (end) "
      address = From.From (MB.Mailbox (Just "Build Bot") (MB.AddrSpec "ci ops" (MB.DomainLiteral "192.0.2.4")))
  typed [wire] `shouldBe` Right (Just address)
  headerMapToList (setHeader address (headerMapFromList [])) `shouldBe`
    [("from", "Build Bot <\"ci ops\"@[192.0.2.4]>")]
  typed ["\t(first) ops+robot (local) @ (domain) example.org (last) "] `shouldBe`
    Right (Just (From.From (MB.Mailbox Nothing (MB.AddrSpec "ops+robot" (MB.DomainName "example.org")))))
  typed ["", "a@b.com"] `shouldSatisfy` isLeft
  typed ["a@b.com", "a@b.com"] `shouldSatisfy` isLeft
  typed ["a@b.com", "c@d.com"] `shouldSatisfy` isLeft
  mapM_ (\value -> typed [value] `shouldSatisfy` isLeft)
    ["", "a@b.com, c@d.com", "Team: a@b.com;", "a@b.com (unclosed", "a@b.com\r\n (folded)"]


prop_from_roundtrip :: Property
prop_from_roundtrip = property $ do
  v <- forAll genFrom
  let bs = M.toStrictByteString (From.renderFrom v)
  case parseWith From.fromParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


checkedTagWrites :: Spec
checkedTagWrites = describe "rejects invalid authored entity-tags" $ do
  it "ETag" $ rejectTag E.ETag
  it "Delta-Base" $ rejectTag DB.DeltaBase
  it "If-Match" $ rejectTag (\tag -> IM.IfMatchEntityTags (E.StrongETag "first" :| [tag]))
  it "If-None-Match" $ rejectTag (\tag -> INM.IfNoneMatchTags (E.StrongETag "first" :| [tag]))


rejectTag
  :: forall a. (KnownHeader a, Cardinality a ~ 'ZeroOrOne)
  => (E.EntityTag -> a)
  -> IO ()
rejectTag build = do
  let previous = setRawHeader (headerName (Proxy @a)) ("\"first\"" :| ["\"second\""]) (headerMapFromList [])
  forM_ ["has space", "bad\"quote", "del\DEL"] $ \bytes -> do
    forM_ [E.StrongETagBytes bytes, E.WeakETagBytes bytes] $ \tag -> do
      let value = build tag
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value previous `shouldBe` previous


checkedFromWrites :: Spec
checkedFromWrites = it "checks authored From mailbox components before replacing the field" $ do
  let old = headerMapFromList [("From", "old@example.net")]
      value display local domain = From.From (MB.Mailbox display (MB.AddrSpec local domain))
      invalid =
        [ value Nothing "local" (MB.DomainName "example..net")
        , value Nothing "local" (MB.DomainLiteral "bad]")
        , value Nothing "a\NULb" (MB.DomainName "example.net")
        , value (Just "name 🚲") "local" (MB.DomainName "example.net")
        ]
  mapM_ (\authored -> do
    renderHeaderLines defaultHeaderSettings authored `shouldBe` Nothing
    setHeader authored old `shouldBe` old) invalid
  let valid = value (Just "café") "ci ops" (MB.DomainLiteral "192.0.2.4")
  lookupHeader @From.From (setHeader valid old) `shouldBe` Right (Just valid)


tests :: Spec
tests =
  describe "Conditional" $
    sequence_
      [ describe "If-None-Match" $
          sequence_
            [ unit_inm_any
            , unit_inm_tags
            , unit_inm_render
            , it "round-trips" prop_inm_roundtrip
            ]
      , describe "Entity tag field lines" fieldLines
      , checkedTagWrites
      , unit_tag_compare_lengths
      , describe "If-Range" $
          sequence_
            [ unit_ifrange_tag
            , unit_ifrange_render
            , unit_ifrange_date
            , unit_ifrange_checked_write
            , unit_ifrange_match
            , it "round-trips entity-tags" prop_ifrange_roundtrip
            ]
      , describe "From" $
          sequence_
            [ unit_from_parse
            , unit_from_render
            , unit_from_field_lines
            , checkedFromWrites
            , it "round-trips" prop_from_roundtrip
            ]
      ]
