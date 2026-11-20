{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the obsolete HTTP Extension Framework (RFC 2774) and PEP
header family. RFC 2774 tests cover typed declarations and acknowledgement
through the header map. PEP tests cover declarations and extension policies.
-}
module Test.Hermes.ObsoleteMan (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Grammar.ExtensionDeclaration
import Network.HTTP.Grammar.PEP
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.Ext as Ext
import Network.HTTP.Headers.HeaderFieldName (hExt, hMan, hOpt, hPEP, hPEPInfo)
import qualified Network.HTTP.Headers.Man as Man
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Opt as Opt
import qualified Network.HTTP.Headers.PEP as PEP
import qualified Network.HTTP.Headers.PEPInfo as PEPInfo
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Unwrap a fully-consumed parse, failing loudly otherwise.
expectOk :: (Show a) => Result String a -> a
expectOk r = case r of
  OK v "" -> v
  OK _ rest -> error ("unconsumed: " <> show rest)
  Fail -> error "parse failed"
  Err err -> error err


{- | A known-good ASCII value must parse to the constructed newtype and
render back to the exact same bytes.
-}
checkValue
  :: (Eq a, Show a)
  => (ST.ShortText -> a)
  -> (a -> M.Builder)
  -> (ByteString -> Result String a)
  -> String
  -> IO ()
checkValue mk renderA parse raw = do
  let value = mk (ST.fromString raw)
      bytes = BSC.pack raw
  expectOk (parse bytes) `shouldBe` value
  M.toStrictByteString (renderA value) `shouldBe` bytes


unit_ext :: Spec
unit_ext = describe "Ext acknowledgement" $ do
  it "distinguishes omission from an empty acknowledgement and preserves field whitespace" $ do
    lookupHeader @Ext.Ext (headerMapFromList []) `shouldBe` Right Nothing
    lookupHeader @Ext.Ext (headerMapFromList [("Ext", "")]) `shouldBe` Right (Just Ext.extAcknowledged)
    lookupRawHeader hExt (setHeader Ext.extAcknowledged (headerMapFromList [])) `shouldBe` Just ("" :| [])
    checkValue Ext.Ext Ext.renderExt (runParser Ext.extParser) "\t   "
    case lookupHeader @Ext.Ext (headerMapFromList [("Ext", "\t  ")]) of
      Right (Just header) -> lookupRawHeader hExt (setHeader header (headerMapFromList [])) `shouldBe` Just ("\t  " :| [])
      result -> expectationFailure (show result)
  it "rejects an acknowledgement carrying data or repeated lines" $ do
    forM_ ["12, 15", ",", " \r\n", "nil", "\255"] $ \wire ->
      lookupHeader @Ext.Ext (headerMapFromList [("Ext", wire)]) `shouldSatisfy` isLeft
    lookupHeader @Ext.Ext (headerMapFromList [("Ext", ""), ("Ext", "")]) `shouldSatisfy` isLeft


unit_man :: Spec
unit_man = describe "Man" $ do
  it "decodes URIs, namespace spelling and token, quoted and absent arguments" $ do
    let uri = "http://www.copyright.org/rights-management"
        wire = "\"http://www.copyright.org/rights-management\" ; NS \t= 0016; note=\"a\\\"b, c;d\"; Flag ; version=2; raw=\"\255\""
        expected =
          ExtensionDeclaration
            (ExtensionURI uri)
            (Just "0016")
            [ ExtensionParameter "note" (Just (ExtensionQuoted "a\"b, c;d"))
            , ExtensionParameter "Flag" Nothing
            , ExtensionParameter "version" (Just (ExtensionToken "2"))
            , ExtensionParameter "raw" (Just (ExtensionQuoted (BS.singleton 255)))
            ]
    case lookupHeader @Man.Man (headerMapFromList [("Man", wire)]) of
      Right (Just header) -> do
        Man.manDeclarations header `shouldBe` Right (expected :| [])
        M.toStrictByteString (Man.renderMan header) `shouldBe` wire
        lookupRawHeader hMan (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
      result -> expectationFailure (show result)
  it "validates the raw compatibility constructor when projecting declarations" $ do
    let raw = "\"http://www.copyright.org/rights-management\"; ns=16"
    checkValue Man.Man Man.renderMan (runParser Man.manParser) raw
    Man.manDeclarations (Man.Man {Man.rawMan = "bar"}) `shouldSatisfy` isLeft
  it "requires complete identifiers, declarations, and namespace bindings" $ do
    forM_
      [ ""
      , ", \t ,"
      , "Range"
      , "\"\""
      , "\"unquoted space\""
      , "\"a\\b\""
      , "\"/relative\""
      , "\"https://p.test/a#fragment\""
      , "\"https://p.test/%XY\""
      , "\"https://[bad]/\""
      , "\"Range\"; ns"
      , "\"Range\"; ns=9"
      , "\"Range\"; ns=\"15\""
      , "\"Range\"; ns=+15"
      , "\"Range\"; ns=015x"
      , "\"Range\"; ns=15; NS=24"
      , "\"Range\"; a=x; ns=15"
      , "\"Range\";"
      , "\"Range\"; x="
      , "\"Range\"; x=\"unterminated"
      , "\"Range\"; x=\"\r\""
      , "\"Range\", invalid"
      , "\"Range\" trailing"
      , "\"Range\"\r\n"
      ]
      $ \wire -> lookupHeader @Man.Man (headerMapFromList [("Man", wire)]) `shouldSatisfy` isLeft
    lookupHeader @Man.Man (headerMapFromList [("Man", "\"Range\""), ("Man", "invalid")]) `shouldSatisfy` isLeft


unit_opt :: Spec
unit_opt = describe "Opt" $ do
  it "combines declarations in field order and retains the original physical lines" $ do
    let first = " \t, \"Range\" , "
        second = "\t\"urn:example:optional\"; ns=012345678901234567890"
        third = ""
        expected =
          ExtensionDeclaration (ExtensionFieldName "Range") Nothing []
            :| [ExtensionDeclaration (ExtensionURI "urn:example:optional") (Just "012345678901234567890") []]
    case lookupHeader @Opt.Opt (headerMapFromList [("Opt", first), ("Opt", second), ("Opt", third)]) of
      Right (Just header) -> do
        Opt.optDeclarations header `shouldBe` Right expected
        lookupRawHeader hOpt (setHeader header (headerMapFromList [])) `shouldBe` Just (first :| [second, third])
        M.toStrictByteString (Opt.renderOpt header) `shouldBe` first <> "," <> second <> ","
      result -> expectationFailure (show result)
    lookupHeader @Opt.Opt (headerMapFromList [("Opt", ","), ("Opt", "")]) `shouldSatisfy` isLeft
  it "renders checked declarations and keeps quoted values distinct from tokens" $ do
    let authored =
          ExtensionDeclaration
            (ExtensionURI "https://p.test/a,b")
            (Just "012")
            [ExtensionParameter "Flag" Nothing, ExtensionParameter "value" (Just (ExtensionQuoted "x"))]
            :| [ExtensionDeclaration (ExtensionFieldName "Range") Nothing []]
    fmap (M.toStrictByteString . Opt.renderOpt) (Opt.optFromDeclarations authored)
      `shouldBe` Right "\"https://p.test/a,b\"; ns=012; Flag; value=\"x\", \"Range\""
    (Opt.optFromDeclarations authored >>= Opt.optDeclarations) `shouldBe` Right authored
    forM_
      [ ExtensionDeclaration (ExtensionURI "Range") Nothing []
      , ExtensionDeclaration (ExtensionFieldName "https://p.test") Nothing []
      , ExtensionDeclaration (ExtensionFieldName "Range") (Just "9") []
      , ExtensionDeclaration (ExtensionFieldName "Range") Nothing [ExtensionParameter "ns" (Just (ExtensionToken "25"))]
      , ExtensionDeclaration (ExtensionFieldName "Range") Nothing [ExtensionParameter "P" (Just (ExtensionToken "x; k"))]
      , ExtensionDeclaration (ExtensionFieldName "Range") Nothing [ExtensionParameter "P" (Just (ExtensionQuoted "\NUL"))]
      ]
      $ \bad -> Opt.optFromDeclarations (bad :| []) `shouldSatisfy` isLeft


prop_declarations :: Property
prop_declarations = property $ do
  quoted <- forAll (BS.pack <$> Gen.list (Range.linear 0 48) (Gen.element (9 : [32 .. 126] <> [128 .. 255])))
  namespace <- forAll (("00" <>) . BS.pack <$> Gen.list (Range.linear 0 20) (Gen.word8 (Range.linear 48 57)))
  let entries =
        ExtensionDeclaration (ExtensionFieldName "Range") (Just namespace) [ExtensionParameter "note" (Just (ExtensionQuoted quoted))]
          :| [ExtensionDeclaration (ExtensionURI "urn:example:range") Nothing []]
  (Man.manFromDeclarations entries >>= Man.manDeclarations) === Right entries
  (Opt.optFromDeclarations entries >>= Opt.optDeclarations) === Right entries


unit_pep :: Spec
unit_pep = describe "PEP" $ do
  it "decodes the draft mapping example and accepts relative URIs, prefixes and extension bags" $ do
    let wire = "{{map \"http://www.w3.org/PEP/Digest\" 4-}}"
        expected = PepDeclaration "http://www.w3.org/PEP/Digest" ["4-"] []
    checkValue PEP.PEP PEP.renderPEP (runParser PEP.pepParser) "{{map \"http://www.w3.org/PEP/Digest\" 4-}}"
    (lookupHeader @PEP.PEP (headerMapFromList [("PEP", wire)]) >>= traverse PEP.pepDeclarations)
      `shouldBe` Right (Just (expected :| []))
    let first = " \t{{MaP \"../Inventory?sku=1#id\" 004-, 12-} {StReNgTh MUST}{vendor yes {Arg \"x,\\\"y;z\"}}}, "
        second = "{{map \"/optional\"}}"
        other = PepDeclaration "/optional" [] []
        declarations =
          PepDeclaration
            "../Inventory?sku=1#id"
            ["004-", "12-"]
            [PepDeclarationStrength PepMust, PepDeclarationExtension (PepBag "vendor" [PepToken "yes", PepNestedBag (PepBag "Arg" [PepQuoted "x,\"y;z"])])]
            :| [other]
    case lookupHeader @PEP.PEP (headerMapFromList [("PEP", first), ("PEP", second)]) of
      Right (Just parsed) -> do
        PEP.pepDeclarations parsed `shouldBe` Right declarations
        lookupRawHeader hPEP (setHeader parsed (headerMapFromList [])) `shouldBe` Just (first :| [second])
      result -> expectationFailure (show result)
    fmap (M.toStrictByteString . PEP.renderPEP) (PEP.pepFromDeclarations (expected :| [other]))
      `shouldBe` Right (wire <> ", " <> second)
  it "rejects malformed maps, modifiers, prefixes, and empty declaration lists" $ do
    forM_
      [ ""
      , " , , "
      , "{map \"/x\"}"
      , "{{id \"/x\"}}"
      , "{{strength must}{map \"/x\"}}"
      , "{{map /unquoted}}"
      , "{{map \"/broken%XX\"}}"
      , "{{map \"http://[broken]/\"}}"
      , "{{map \"/x\" 4}}"
      , "{{map \"/x\" -}}"
      , "{{map \"/x\" 4- 5-}}"
      , "{{map \"/x\" 4-x}}"
      , "{{map \"/x\"} {map \"/y\"}}"
      , "{{map \"/x\"}{strength nope}}"
      , "{{map \"/x\"}{strength may must}}"
      , "{{map \"/x\"}{x (comment)}}"
      , "{{map \"/x\"}{x \"\r\"}}"
      , "{{map \"/x\"}} junk"
      , "{{map \"/x\"}}\r\n"
      , "{{map \"/x\"}{x"
      ]
      $ \wire -> lookupHeader @PEP.PEP (headerMapFromList [("PEP", wire)]) `shouldSatisfy` isLeft
    lookupHeader @PEP.PEP (headerMapFromList [("PEP", "{{map \"/x\"}}"), ("PEP", "unparsed")]) `shouldSatisfy` isLeft
    PEP.pepDeclarations (PEP.PEP "bad") `shouldSatisfy` isLeft
    PEP.pepFromDeclarations (PepDeclaration "broken uri" [] [] :| []) `shouldSatisfy` isLeft


unit_pepInfo :: Spec
unit_pepInfo = describe "PEP-Info" $ do
  it "decodes policy targets, an empty realm, TTL, strength, and opaque nested parameters" $ do
    let first =
          "{{ID \"http://some.org/payment-extension\"} {for \"/cgi-bin/buy\" *, \"secret/Overview.html\"} \
          \{max-age 18446744073709551616} {Params {Price 0.02USD} comment \"\255\"} {strength May} {x \"data\"}}"
        second = "{{id \"/generic\"} {for}}"
        expected =
          PepPolicy
            "http://some.org/payment-extension"
            [ PepFor [PepTarget "/cgi-bin/buy" True, PepTarget "secret/Overview.html" False]
            , PepMaxAge 18446744073709551616
            , PepParameters [PepNestedBag (PepBag "Price" [PepToken "0.02USD"]), PepToken "comment", PepQuoted (BS.singleton 255)]
            , PepPolicyStrength PepMay
            , PepPolicyExtension (PepBag "x" [PepQuoted "data"])
            ]
            :| [PepPolicy "/generic" [PepFor []]]
    case lookupHeader @PEPInfo.PEPInfo (headerMapFromList [("PEP-Info", first), ("PEP-Info", second)]) of
      Right (Just parsed) -> do
        PEPInfo.pepInfoPolicies parsed `shouldBe` Right expected
        lookupRawHeader hPEPInfo (setHeader parsed (headerMapFromList [])) `shouldBe` Just (first :| [second])
        M.toStrictByteString (PEPInfo.renderPEPInfo parsed) `shouldBe` first <> "," <> second
      result -> expectationFailure (show result)
    (PEPInfo.pepInfoFromPolicies expected >>= PEPInfo.pepInfoPolicies) `shouldBe` Right expected
    checkValue PEPInfo.PEPInfo PEPInfo.renderPEPInfo (runParser PEPInfo.pepInfoParser) "{{id \"/generic\"} {for}}"
  it "rejects misplaced required attributes, bare wildcards, malformed lifetimes and broken bags" $ do
    forM_
      [ ""
      , "{}"
      , "{{map \"/x\"}}"
      , "{{for}{id \"/x\"}}"
      , "{{id \"/x\"} {id \"/y\"}}"
      , "{{id \"/bad%Q0\"}}"
      , "{{id \"/x\"} {for *}}"
      , "{{id \"/x\"} {for \"/x\"*}}"
      , "{{id \"/x\"} {for \"/x\" * *}}"
      , "{{id \"/x\"} {for \"/x\" \"/y\"}}"
      , "{{id \"/x\"} {max-age -1}}"
      , "{{id \"/x\"} {max-age +0}}"
      , "{{id \"/x\"} {max-age 1x}}"
      , "{{id \"/x\"} {params {foo {}}}}"
      , "{{id \"/x\"} {params {a \"x}}}"
      , "{{id \"/x\"} {strength yes}}"
      , "{{id \"/x\"} {x {a \"\NUL\"}}}"
      ]
      $ \wire -> lookupHeader @PEPInfo.PEPInfo (headerMapFromList [("PEP-Info", wire)]) `shouldSatisfy` isLeft
    PEPInfo.pepInfoFromPolicies (PepPolicy "/x" [PepPolicyExtension (PepBag "strength" [PepToken "may"])] :| [])
      `shouldSatisfy` isLeft
  it "retains deep extension bags in order" $ do
    let initial = PepQuoted "x"
        nested = foldr (\_ child -> PepNestedBag (PepBag "nested" [child])) initial ([1 .. 128] :: [Int])
        entries = PepPolicy "/x" [PepParameters [nested]] :| []
    (PEPInfo.pepInfoFromPolicies entries >>= PEPInfo.pepInfoPolicies) `shouldBe` Right entries


prop_pep :: Property
prop_pep = property $ do
  octets <- forAll (BS.pack <$> Gen.list (Range.linear 0 48) (Gen.element (9 : [32 .. 126] <> [128 .. 255])))
  ttl <- forAll (Gen.word64 (Range.linear 0 maxBound))
  let args = [PepNestedBag (PepBag "camelCase" [PepToken "name", PepQuoted octets])]
      declarations = PepDeclaration "/example" ["04-", "100-"] [PepDeclarationExtension (PepBag "Vendor" args)] :| []
      policies = PepPolicy "/example" [PepFor [PepTarget "/a" True], PepParameters args, PepMaxAge (fromIntegral ttl)] :| []
  (PEP.pepFromDeclarations declarations >>= PEP.pepDeclarations) === Right declarations
  (PEPInfo.pepInfoFromPolicies policies >>= PEPInfo.pepInfoPolicies) === Right policies


tests :: Spec
tests =
  describe "ObsoleteMan" $
    sequence_
      [ unit_ext
      , unit_man
      , unit_opt
      , unit_pep
      , unit_pepInfo
      , it "round-trips typed declarations with opaque quoted bytes" prop_declarations
      , it "round-trips typed PEP fields with nested bags and opaque bytes" prop_pep
      ]
