{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ClientHints (tests) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.AcceptCH as ACH
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), ParserT, RFC8941String (..), RFC8941Token (..), Result (..), StructuredFieldItem (..), StructuredFieldMember (..), StructuredFieldParameter (..), runParser)
import qualified Network.HTTP.Headers.SecGPC as GPC
import qualified Network.HTTP.Headers.SecPurpose as SP
import Test.Syd
import Test.Syd.Hedgehog ()


-- Run a standalone header parser, requiring it to consume the whole input.
runP :: ParserT () String a -> ByteString -> Either String a
runP p bs = case runParser p bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


tok :: String -> RFC8941Token
tok = RFC8941Token . ST.fromString


-- A valid RFC 8941 token: ALPHA leader followed by token characters.
tokenGen :: Gen RFC8941Token
tokenGen = do
  c <- Gen.element (['a' .. 'z'] ++ ['A' .. 'Z'])
  rest <- Gen.list (Range.linear 0 8) (Gen.element tokenChars)
  pure (RFC8941Token (ST.fromString (c : rest)))
  where
    tokenChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_."


renderACH :: ACH.AcceptCH -> ByteString
renderACH = M.toStrictByteString . ACH.renderAcceptCH


renderSP :: SP.SecPurpose -> ByteString
renderSP = M.toStrictByteString . SP.renderSecPurpose


renderGPC :: GPC.SecGPC -> ByteString
renderGPC = M.toStrictByteString . GPC.renderSecGPC


-- Accept-CH ---------------------------------------------------------------

unit_acceptCH_parse :: Spec
unit_acceptCH_parse = it "parses a client-hint token list" $
  case runP ACH.acceptCHParser "Sec-CH-UA, DPR, Width" of
    Right (ACH.AcceptCH hints) ->
      map unsafeToRFC8941Token hints
        `shouldBe` map ST.fromString ["Sec-CH-UA", "DPR", "Width"]
    other -> error (show other)


unit_acceptCH_render :: Spec
unit_acceptCH_render =
  it "renders a client-hint token list" $
    renderACH (ACH.AcceptCH [tok "DPR", tok "Width"]) `shouldBe` "DPR, Width"


prop_acceptCH_roundtrip :: Property
prop_acceptCH_roundtrip = property $ do
  hints <- forAll (Gen.list (Range.linear 1 6) tokenGen)
  let v = ACH.AcceptCH hints
  case runP ACH.acceptCHParser (renderACH v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderACH v))


-- Sec-Purpose -------------------------------------------------------------

unit_secPurpose_parse :: Spec
unit_secPurpose_parse = it "parses the prefetch purpose" $
  case runP SP.secPurposeParser "prefetch" of
    Right (SP.SecPurpose t) -> unsafeToRFC8941Token t `shouldBe` ST.fromString "prefetch"
    other -> error (show other)


unit_secPurpose_render :: Spec
unit_secPurpose_render =
  it "renders the prefetch purpose" $
    renderSP (SP.SecPurpose (tok "prefetch")) `shouldBe` "prefetch"


prop_secPurpose_roundtrip :: Property
prop_secPurpose_roundtrip = property $ do
  t <- forAll tokenGen
  let v = SP.SecPurpose t
  case runP SP.secPurposeParser (renderSP v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderSP v))


unit_typed_accept_ch :: Spec
unit_typed_accept_ch = it "retains the ordered hints and an explicit empty client-hint opt-in" $ do
  let empty = headerMapFromList []
      singleton value = headerMapFromList [("Accept-CH", value)]
      repeated = headerMapFromList [("Accept-CH", "\t Sec-CH-UA;sample=1\t"), ("accept-ch", "DPR;new=%\"caf%c3%a9\", Width ")]
      hints = ACH.AcceptCH [tok "Sec-CH-UA", tok "DPR", tok "Width"]
  lookupHeader @ACH.AcceptCH repeated `shouldBe` Right (Just hints)
  lookupHeader @ACH.AcceptCH empty `shouldBe` Right Nothing
  lookupHeader @ACH.AcceptCH (singleton "\t ") `shouldBe` Right (Just (ACH.AcceptCH []))
  lookupRawHeader "Accept-CH" (setHeader (ACH.AcceptCH []) empty) `shouldBe` Just ("" :| [])
  lookupRawHeader "Accept-CH" (setHeader hints empty) `shouldBe` Just ("Sec-CH-UA, DPR, Width" :| [])
  mapM_
    (\wire -> lookupHeader @ACH.AcceptCH (singleton wire) `shouldSatisfy` isLeft)
    ["DPR,, Width", "DPR,", "DPR/Width", "DPR:Width", "1Width", "\"DPR\"", "(DPR)", "DPR;\tflag", "DPR;bad="]
  lookupHeader @ACH.AcceptCH (headerMapFromList [("Accept-CH", "DPR"), ("Accept-CH", "")]) `shouldSatisfy` isLeft


unit_typed_sec_purpose :: Spec
unit_typed_sec_purpose = it "retains purpose Items, normalized parameters, and extension members" $ do
  let empty = headerMapFromList []
      params = [StructuredFieldParameter "prerender" (Boolean False), StructuredFieldParameter "anonymous-client-ip" (Integer 2)]
      otherParams = [StructuredFieldParameter "label" (String (RFC8941String "a,b"))]
      members =
        [ StructuredItem (StructuredFieldItem (Token (tok "prefetch")) params)
        , StructuredInnerList [StructuredFieldItem (Token (tok "future")) [], StructuredFieldItem (Integer 3) []] otherParams
        , StructuredItem (StructuredFieldItem (Token (tok "prefetch")) [StructuredFieldParameter "prerender" (Boolean True)])
        ]
      wire = "prefetch;prerender=?0;anonymous-client-ip=2, (future 3);label=\"a,b\", prefetch;prerender"
      fields = headerMapFromList [("Sec-Purpose", "\t prefetch;prerender;anonymous-client-ip=2;prerender=?0"), ("Sec-Purpose", "(future 3);label=\"a,b\", prefetch;prerender=?1\t")]
      expected = SP.SecPurposeList members
  lookupHeader @SP.SecPurpose fields `shouldBe` Right (Just expected)
  SP.secPurposePrefetches expected `shouldBe` [SP.PrefetchPurpose False True, SP.PrefetchPurpose True False]
  lookupRawHeader "Sec-Purpose" (setHeader expected empty) `shouldBe` Just (wire :| [])
  mapM_
    (\bad -> lookupHeader @SP.SecPurpose (headerMapFromList [("Sec-Purpose", bad)]) `shouldSatisfy` isLeft)
    ["prefetch;", "prefetch;\tprerender", "prefetch,, future", "prefetch,", "prefetch;x=\"bad\\q\""]


-- Sec-GPC -----------------------------------------------------------------

unit_secGPC_parse :: Spec
unit_secGPC_parse = it "parses the GPC signal" $
  case runP GPC.secGPCParser "1" of
    Right GPC.SecGPC -> pure () :: IO ()
    other -> error (show other)


unit_secGPC_render :: Spec
unit_secGPC_render =
  it "renders the GPC signal" $
    renderGPC GPC.SecGPC `shouldBe` "1"


unit_secGPC_field_lines :: Spec
unit_secGPC_field_lines = it "uses any exact asserted Sec-GPC field line" $ do
  let field values = headerMapFromList (map (\wire -> ("Sec-GPC", wire)) values)
      check values = do
        let headers = field values
        GPC.hasSecGPC headers `shouldBe` True
        lookupHeader @GPC.SecGPC headers `shouldBe` Right (Just GPC.SecGPC)
  mapM_ check [["1"], ["\t 1 \t"], ["0", "1"], ["bad", "1", "1"], ["1", "0"]]
  mapM_
    ( \values -> do
        let headers = field values
        GPC.hasSecGPC headers `shouldBe` False
        lookupHeader @GPC.SecGPC headers `shouldSatisfy` isLeft
    )
    [[""], ["1, 1"], ["0, 1"], ["0", "01"], ["1\r"], ["\"1\""], ["1;flag"]]
  GPC.hasSecGPC (field []) `shouldBe` False
  lookupRawHeader "Sec-GPC" (setHeader GPC.SecGPC (field ["0", "0"])) `shouldBe` Just ("1" :| [])


prop_secGPC_roundtrip :: Property
prop_secGPC_roundtrip = property $ do
  v <- forAll (Gen.constant GPC.SecGPC)
  case runP GPC.secGPCParser (renderGPC v) of
    Right v' -> v === v'
    Left err -> error err


tests :: Spec
tests =
  describe "ClientHints" $
    sequence_
      [ unit_acceptCH_parse
      , unit_acceptCH_render
      , it "Accept-CH round-trips" prop_acceptCH_roundtrip
      , unit_typed_accept_ch
      , unit_secPurpose_parse
      , unit_secPurpose_render
      , it "Sec-Purpose round-trips" prop_secPurpose_roundtrip
      , unit_typed_sec_purpose
      , unit_secGPC_parse
      , unit_secGPC_render
      , unit_secGPC_field_lines
      , it "Sec-GPC round-trips" prop_secGPC_roundtrip
      ]
