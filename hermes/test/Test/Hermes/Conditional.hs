{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Hermes.Conditional (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ETag as E
import qualified Network.HTTP.Headers.From as From
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

genTagText :: Gen ST.ShortText
genTagText = ST.fromString <$> Gen.list (Range.linear 0 10) (Gen.element tagChars)
  where
    tagChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']


genEntityTag :: Gen E.EntityTag
genEntityTag =
  Gen.choice
    [ E.StrongETag <$> genTagText
    , E.WeakETag <$> genTagText
    ]


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
    Right v@(IR.IfRangeDate _) ->
      M.toStrictByteString (IR.renderIfRange v) `shouldBe` "Wed, 21 Oct 2015 07:28:00 GMT"
    other -> error (show other)


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


prop_from_roundtrip :: Property
prop_from_roundtrip = property $ do
  v <- forAll genFrom
  let bs = M.toStrictByteString (From.renderFrom v)
  case parseWith From.fromParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


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
      , describe "If-Range" $
          sequence_
            [ unit_ifrange_tag
            , unit_ifrange_render
            , unit_ifrange_date
            , it "round-trips entity-tags" prop_ifrange_roundtrip
            ]
      , describe "From" $
          sequence_
            [ unit_from_parse
            , unit_from_render
            , it "round-trips" prop_from_roundtrip
            ]
      ]
