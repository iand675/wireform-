{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Mailbox (tests) where

import Data.ByteString (ByteString)
import Data.List (intercalate)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.Mailbox as MB
import Test.Syd
import Test.Syd.Hedgehog ()


renderAddrSpec :: MB.AddrSpec -> ByteString
renderAddrSpec = M.toStrictByteString . MB.renderAddrSpec


renderMailbox :: MB.Mailbox -> ByteString
renderMailbox = M.toStrictByteString . MB.renderMailbox


renderAddress :: MB.Address -> ByteString
renderAddress = M.toStrictByteString . MB.renderAddress


-- ---------------------------------------------------------------------------
-- Unit examples
-- ---------------------------------------------------------------------------

unit_addrSpec :: Spec
unit_addrSpec = it "parses a bare addr-spec a@b.com" $
  case MB.parseAddrSpec "a@b.com" of
    Right (MB.AddrSpec lp (MB.DomainName d)) -> do
      lp `shouldBe` "a"
      d `shouldBe` "b.com"
    other -> error (show other)


unit_quotedDisplayName :: Spec
unit_quotedDisplayName = it "parses a quoted display name" $
  case MB.parseMailbox "\"John Doe\" <jd@example.com>" of
    Right (MB.Mailbox (Just n) (MB.AddrSpec lp (MB.DomainName d))) -> do
      n `shouldBe` "John Doe"
      lp `shouldBe` "jd"
      d `shouldBe` "example.com"
    other -> error (show other)


unit_phraseDisplayName :: Spec
unit_phraseDisplayName = it "parses a multi-word phrase display name" $
  case MB.parseMailbox "Display Name <x@y.z>" of
    Right (MB.Mailbox (Just n) (MB.AddrSpec lp (MB.DomainName d))) -> do
      n `shouldBe` "Display Name"
      lp `shouldBe` "x"
      d `shouldBe` "y.z"
    other -> error (show other)


unit_group :: Spec
unit_group = it "parses a group with two members" $
  case MB.parseAddress "friends: a@x.com, b@y.com;" of
    Right (MB.AddressGroup name members) -> do
      name `shouldBe` "friends"
      map (MB.localPart . MB.mailboxAddr) members `shouldBe` ["a", "b"]
      map (MB.domain . MB.mailboxAddr) members
        `shouldBe` [MB.DomainName "x.com", MB.DomainName "y.com"]
    other -> error (show other)


unit_quotedLocalPart :: Spec
unit_quotedLocalPart = it "parses and re-renders a quoted local part" $ do
  case MB.parseAddrSpec "\"weird name\"@example.com" of
    Right (MB.AddrSpec lp (MB.DomainName d)) -> do
      lp `shouldBe` "weird name"
      d `shouldBe` "example.com"
    other -> error (show other)
  renderAddrSpec (MB.AddrSpec "weird name" (MB.DomainName "example.com"))
    `shouldBe` "\"weird name\"@example.com"


unit_domainLiteral :: Spec
unit_domainLiteral = it "parses and re-renders a domain literal" $ do
  case MB.parseAddrSpec "u@[192.0.2.1]" of
    Right (MB.AddrSpec lp (MB.DomainLiteral d)) -> do
      lp `shouldBe` "u"
      d `shouldBe` "192.0.2.1"
    other -> error (show other)
  renderAddrSpec (MB.AddrSpec "u" (MB.DomainLiteral "192.0.2.1"))
    `shouldBe` "u@[192.0.2.1]"


unit_render_namedMailbox :: Spec
unit_render_namedMailbox =
  it "renders a named mailbox with angle brackets" $
    renderMailbox (MB.Mailbox (Just "John Doe") (MB.AddrSpec "jd" (MB.DomainName "example.com")))
      `shouldBe` "John Doe <jd@example.com>"


unit_render_bareMailbox :: Spec
unit_render_bareMailbox =
  it "renders a name-less mailbox bare (no angle brackets)" $
    renderMailbox (MB.Mailbox Nothing (MB.AddrSpec "jd" (MB.DomainName "example.com")))
      `shouldBe` "jd@example.com"


unit_render_group :: Spec
unit_render_group =
  it "renders a group, members joined by ', '" $
    renderAddress
      ( MB.AddressGroup
          "friends"
          [ MB.Mailbox Nothing (MB.AddrSpec "a" (MB.DomainName "x.com"))
          , MB.Mailbox Nothing (MB.AddrSpec "b" (MB.DomainName "y.com"))
          ]
      )
      `shouldBe` "friends: a@x.com, b@y.com;"


-- ---------------------------------------------------------------------------
-- Round-trip property over generated simple values
-- ---------------------------------------------------------------------------

genLabel :: Gen String
genLabel = Gen.list (Range.linear 1 6) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))


genDotAtom :: Gen ST.ShortText
genDotAtom = do
  labels <- Gen.list (Range.linear 1 3) genLabel
  pure (ST.fromString (intercalate "." labels))


genAddrSpec :: Gen MB.AddrSpec
genAddrSpec = MB.AddrSpec <$> genDotAtom <*> (MB.DomainName <$> genDotAtom)


genDisplayName :: Gen ST.ShortText
genDisplayName = do
  ws <- Gen.list (Range.linear 1 3) genWord
  pure (ST.fromString (unwords ws))
  where
    genWord = Gen.list (Range.linear 1 6) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z']))


genMailbox :: Gen MB.Mailbox
genMailbox = MB.Mailbox <$> Gen.maybe genDisplayName <*> genAddrSpec


prop_addrSpec_roundtrip :: Property
prop_addrSpec_roundtrip = property $ do
  a <- forAll genAddrSpec
  let bs = renderAddrSpec a
  case MB.parseAddrSpec bs of
    Right a' -> a' === a
    Left err -> error (err <> " on " <> show bs)


prop_mailbox_roundtrip :: Property
prop_mailbox_roundtrip = property $ do
  m <- forAll genMailbox
  let bs = renderMailbox m
  case MB.parseMailbox bs of
    Right m' -> m' === m
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Mailbox" $
    sequence_
      [ unit_addrSpec
      , unit_quotedDisplayName
      , unit_phraseDisplayName
      , unit_group
      , unit_quotedLocalPart
      , unit_domainLiteral
      , unit_render_namedMailbox
      , unit_render_bareMailbox
      , unit_render_group
      , it "addr-spec round-trips" prop_addrSpec_roundtrip
      , it "mailbox round-trips" prop_mailbox_roundtrip
      ]
