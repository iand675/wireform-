{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Signature (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.AcceptSignature as AS
import Network.HTTP.Headers.HeaderFieldName (hAcceptSignature, hSignature, hSignatureInput)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.Signature as Sig
import qualified Network.HTTP.Headers.SignatureInput as SI
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- parse / render helpers
-- ---------------------------------------------------------------------------

dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseSig :: ByteString -> Either String Sig.Signature
parseSig bs = case runParser Sig.signatureParser bs of
  OK v rest
    | BS.null (dropOws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


parseSI :: ByteString -> Either String SI.SignatureInput
parseSI bs = case runParser SI.signatureInputParser bs of
  OK v rest
    | BS.null (dropOws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


parseAS :: ByteString -> Either String AS.AcceptSignature
parseAS bs = case runParser AS.acceptSignatureParser bs of
  OK v rest
    | BS.null (dropOws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSig :: Sig.Signature -> ByteString
renderSig = M.toStrictByteString . Sig.renderSignature


renderSI :: SI.SignatureInput -> ByteString
renderSI = M.toStrictByteString . SI.renderSignatureInput


renderAS :: AS.AcceptSignature -> ByteString
renderAS = M.toStrictByteString . AS.renderAcceptSignature


str :: String -> RFC8941String
str = RFC8941String . ST.fromString


-- ---------------------------------------------------------------------------
-- Signature unit tests
-- ---------------------------------------------------------------------------

unit_signature_parse :: Spec
unit_signature_parse = it "parses a Signature dictionary" $
  case parseSig "sig1=:dGVzdA==:, proof=:YWJj:" of
    Right (Sig.Signature ((l1, b1) :| [(l2, b2)])) -> do
      l1 `shouldBe` ST.fromString "sig1"
      b1 `shouldBe` ("test" :: ByteString)
      l2 `shouldBe` ST.fromString "proof"
      b2 `shouldBe` ("abc" :: ByteString)
    other -> error (show other)


unit_signature_render :: Spec
unit_signature_render =
  it "renders a Signature dictionary" $
    let v = Sig.Signature ((ST.fromString "sig1", "test") :| [(ST.fromString "proof", "abc")])
    in renderSig v `shouldBe` "sig1=:dGVzdA==:, proof=:YWJj:"


-- ---------------------------------------------------------------------------
-- Signature-Input unit tests
-- ---------------------------------------------------------------------------

unit_siginput_parse :: Spec
unit_siginput_parse = it "parses a Signature-Input dictionary with a component flag" $
  case parseSI "sig1=(\"@method\" \"content-digest\";sf);created=1618884475;keyid=\"test-key\"" of
    Right (SI.SignatureInput (e :| [])) -> do
      SI.signatureLabel e `shouldBe` ST.fromString "sig1"
      SI.signatureComponents e
        `shouldBe` [ SI.SignatureComponent (str "@method") []
                   , SI.SignatureComponent (str "content-digest") [(ST.fromString "sf", Nothing)]
                   ]
      SI.signatureParameters e
        `shouldBe` [ (ST.fromString "created", Just (Integer 1618884475))
                   , (ST.fromString "keyid", Just (String (str "test-key")))
                   ]
    other -> error (show other)


unit_siginput_render :: Spec
unit_siginput_render =
  it "renders a Signature-Input dictionary" $
    let v =
          SI.SignatureInput
            ( SI.SignatureInputEntry
                (ST.fromString "sig1")
                [ SI.SignatureComponent (str "@method") []
                , SI.SignatureComponent (str "@target-uri") []
                ]
                [ (ST.fromString "created", Just (Integer 1618884475))
                , (ST.fromString "keyid", Just (String (str "test-key")))
                ]
                :| []
            )
    in renderSI v
         `shouldBe` "sig1=(\"@method\" \"@target-uri\");created=1618884475;keyid=\"test-key\""


-- ---------------------------------------------------------------------------
-- Accept-Signature unit tests
-- ---------------------------------------------------------------------------

unit_acceptsig_parse :: Spec
unit_acceptsig_parse = it "parses an Accept-Signature dictionary" $
  case parseAS "acc=(\"@method\" \"@authority\");keyid=\"test-key\";alg=\"rsa-pss-sha512\"" of
    Right (AS.AcceptSignature (e :| [])) -> do
      AS.signatureLabel e `shouldBe` ST.fromString "acc"
      AS.signatureComponents e
        `shouldBe` [ AS.AcceptSignatureComponent (str "@method") []
                   , AS.AcceptSignatureComponent (str "@authority") []
                   ]
      AS.signatureParameters e
        `shouldBe` [ (ST.fromString "keyid", Just (String (str "test-key")))
                   , (ST.fromString "alg", Just (String (str "rsa-pss-sha512")))
                   ]
    other -> error (show other)


unit_acceptsig_render :: Spec
unit_acceptsig_render =
  it "renders an Accept-Signature dictionary" $
    let v =
          AS.AcceptSignature
            ( AS.AcceptSignatureEntry
                (ST.fromString "acc")
                [AS.AcceptSignatureComponent (str "@method") []]
                [(ST.fromString "keyid", Just (String (str "test-key")))]
                :| []
            )
    in renderAS v `shouldBe` "acc=(\"@method\");keyid=\"test-key\""


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genLabel :: Gen ST.ShortText
genLabel = do
  c <- Gen.element ['a' .. 'z']
  rest <- Gen.list (Range.linear 0 6) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))
  pure (ST.pack (c : rest))


genComponentName :: Gen RFC8941String
genComponentName = do
  prefix <- Gen.element ["", "@"]
  body <- Gen.list (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> "-"))
  pure (RFC8941String (ST.pack (prefix <> body)))


genItemValue :: Gen ItemValue
genItemValue =
  Gen.choice
    [ Integer <$> Gen.int (Range.linear 0 1_000_000)
    , String . RFC8941String . ST.pack
        <$> Gen.list (Range.linear 0 10) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-"))
    ]


genParam :: Gen (ST.ShortText, Maybe ItemValue)
genParam = do
  k <- genLabel
  mv <- Gen.maybe genItemValue
  pure ("x-" <> k, mv)


genParams :: Gen [(ST.ShortText, Maybe ItemValue)]
genParams = nubBy ((==) `on` fst) <$> Gen.list (Range.linear 0 3) genParam


-- Signature

genSigValue :: Gen (ST.ShortText, ByteString)
genSigValue = do
  label <- genLabel
  bytes <- BS.pack <$> Gen.list (Range.linear 0 32) (Gen.word8 Range.constantBounded)
  pure (label, bytes)


genSignature :: Gen Sig.Signature
genSignature = Sig.Signature . NE.nubBy ((==) `on` fst) <$> Gen.nonEmpty (Range.linear 1 3) genSigValue


-- Signature-Input

genSIComponent :: Gen SI.SignatureComponent
genSIComponent = SI.SignatureComponent <$> genComponentName <*> genParams


genSIEntry :: Gen SI.SignatureInputEntry
genSIEntry =
  SI.SignatureInputEntry
    <$> genLabel
    <*> (nubBy ((==) `on` SI.componentName) <$> Gen.list (Range.linear 0 3) genSIComponent)
    <*> genParams


genSignatureInput :: Gen SI.SignatureInput
genSignatureInput = SI.SignatureInput . NE.nubBy ((==) `on` SI.signatureLabel) <$> Gen.nonEmpty (Range.linear 1 3) genSIEntry


-- Accept-Signature

genASComponent :: Gen AS.AcceptSignatureComponent
genASComponent = AS.AcceptSignatureComponent <$> genComponentName <*> genParams


genASEntry :: Gen AS.AcceptSignatureEntry
genASEntry =
  AS.AcceptSignatureEntry
    <$> genLabel
    <*> (nubBy ((==) `on` AS.componentName) <$> Gen.list (Range.linear 0 3) genASComponent)
    <*> genParams


genAcceptSignature :: Gen AS.AcceptSignature
genAcceptSignature = AS.AcceptSignature . NE.nubBy ((==) `on` AS.signatureLabel) <$> Gen.nonEmpty (Range.linear 1 3) genASEntry


-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

prop_signature_roundtrip :: Property
prop_signature_roundtrip = property $ do
  v <- forAll genSignature
  let bs = renderSig v
  case parseSig bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


prop_siginput_roundtrip :: Property
prop_siginput_roundtrip = property $ do
  v <- forAll genSignatureInput
  let bs = renderSI v
  case parseSI bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


prop_acceptsig_roundtrip :: Property
prop_acceptsig_roundtrip = property $ do
  v <- forAll genAcceptSignature
  let bs = renderAS v
  case parseAS bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Signature" $
    sequence_
      [ unit_signature_parse
      , unit_signature_render
      , unit_siginput_parse
      , unit_siginput_render
      , unit_acceptsig_parse
      , unit_acceptsig_render
      , it "pairs according to input labels in header and trailer order" $ do
          let headers =
                headerMapFromList
                  [ ("Signature-Input", "first=(\"@status\");created=1")
                  , ("Signature-Input", "second=(\"date\")")
                  , ("Signature", "last=:bA==:, second=:cw==:")
                  ]
              trailers =
                headerMapFromList
                  [ ("Signature-Input", "last=(\"content-digest\";tr)")
                  , ("Signature", "first=:Zg==:")
                  ]
              pairs = fmap (fmap (\match ->
                (SI.signatureLabel (Sig.messageSignatureInput match), Sig.messageSignatureBytes match)))
          pairs <$> Sig.lookupMessageSignaturesWithTrailers headers trailers
            `shouldBe` Right (Just (("first", "f") :| [("second", "s"), ("last", "l")]))
          case Sig.lookupMessageSignaturesWithTrailers headers trailers of
            Right (Just (Sig.MessageSignature input _ :| _)) ->
              SI.signatureParameters input `shouldBe` [("created", Just (Integer 1))]
            other -> expectationFailure (show other)
          let unsigned = headerMapFromList []
              single = headerMapFromList [("Signature-Input", "one=()"), ("Signature", "one=:YQ==:")]
          Sig.lookupMessageSignatures unsigned `shouldBe` Right Nothing
          pairs <$> Sig.lookupMessageSignatures single `shouldBe` Right (Just (("one", "a") :| []))
          Sig.lookupMessageSignaturesWithTrailers unsigned single `shouldBe` Sig.lookupMessageSignatures single
      , it "reports a label missing from either signature field" $ do
          let onlyInput = headerMapFromList [("Signature-Input", "a=()")]
              onlyValue = headerMapFromList [("Signature", "b=:Yg==:")]
              excessValue = headerMapFromList [("Signature-Input", "a=()"), ("Signature", "a=:YQ==:, b=:Yg==:")]
              excessInput = headerMapFromList [("Signature-Input", "a=(), b=()"), ("Signature", "a=:YQ==:")]
          Sig.lookupMessageSignatures onlyInput `shouldBe` Left (Sig.MissingMessageSignatureLabel hSignature "a")
          Sig.lookupMessageSignatures onlyValue `shouldBe` Left (Sig.MissingMessageSignatureLabel hSignatureInput "b")
          Sig.lookupMessageSignaturesWithTrailers onlyInput onlyValue
            `shouldBe` Left (Sig.MissingMessageSignatureLabel hSignature "a")
          Sig.lookupMessageSignatures excessValue `shouldBe` Left (Sig.MissingMessageSignatureLabel hSignatureInput "b")
          Sig.lookupMessageSignatures excessInput `shouldBe` Left (Sig.MissingMessageSignatureLabel hSignature "b")
      , it "keeps signature field sections independent and rejects labels repeated across them" $ do
          let inputs = headerMapFromList [("Signature-Input", "a=()")]
              values = headerMapFromList [("Signature", "a=:YQ==:")]
              broken = headerMapFromList [("Signature", "a")]
              left = headerMapFromList [("Signature-Input", "a=(\"date\";x=\"left")]
              right = headerMapFromList [("Signature-Input", "right\")")]
          Sig.lookupMessageSignaturesWithTrailers inputs inputs
            `shouldBe` Left (Sig.DuplicateMessageSignatureLabel hSignatureInput "a")
          Sig.lookupMessageSignaturesWithTrailers values values
            `shouldBe` Left (Sig.DuplicateMessageSignatureLabel hSignature "a")
          case Sig.lookupMessageSignaturesWithTrailers inputs broken of
            Left (Sig.MalformedMessageSignatureField Sig.SignatureTrailerSection field _) ->
              field `shouldBe` hSignature
            other -> expectationFailure (show other)
          case Sig.lookupMessageSignaturesWithTrailers left right of
            Left (Sig.MalformedMessageSignatureField Sig.SignatureHeaderSection field _) ->
              field `shouldBe` hSignatureInput
            other -> expectationFailure (show other)
      , it "combines signed field lines in order and rejects duplicate labels" $ do
          let signatures = headerMapFromList [("Signature", "a=:YQ:;ext"), ("Signature", "b=:Yg==:")]
              inputs = headerMapFromList [("Signature-Input", "a=();created=1"), ("Signature-Input", "b=(\"@status\")")]
              requests = headerMapFromList [("Accept-Signature", "a=();created"), ("Accept-Signature", "b=(\"@method\")")]
          fmap (fmap renderSig) (lookupHeader @Sig.Signature signatures) `shouldBe` Right (Just "a=:YQ==:, b=:Yg==:")
          fmap (fmap renderSI) (lookupHeader @SI.SignatureInput inputs) `shouldBe` Right (Just "a=();created=1, b=(\"@status\")")
          fmap (fmap renderAS) (lookupHeader @AS.AcceptSignature requests) `shouldBe` Right (Just "a=();created, b=(\"@method\")")
          lookupHeader @Sig.Signature (headerMapFromList [("Signature", "a=:YQ==:"), ("Signature", "a=:Yg==:")])
            `shouldSatisfy` isLeft
          lookupHeader @SI.SignatureInput (headerMapFromList [("Signature-Input", "a=()"), ("Signature-Input", "a=(\"@status\")")])
            `shouldSatisfy` isLeft
          parseAS "a=(), a=(\"@method\")" `shouldSatisfy` isLeft
      , it "rejects authored signature labels that the typed reader would reject" $ do
          let populated =
                headerMapFromList
                  [ ("Signature", "original=:YQ==:")
                  , ("Signature-Input", "original=()")
                  , ("Accept-Signature", "original=()")
                  ]
              empty = headerMapFromList []
              signature = Sig.Signature . fmap (\label -> (label, "proof"))
              input = SI.SignatureInput . fmap (\label -> SI.SignatureInputEntry label [] [])
              request = AS.AcceptSignature . fmap (\label -> AS.AcceptSignatureEntry label [] [])
              invalidLabels =
                [ "same" :| ["same"]
                , "same" :| ["other", "third", "same"]
                , "valid" :| ["Uppercase"]
                , "" :| []
                , "two words" :| []
                , "7start" :| []
                , "café" :| []
                ]
              keepsFields write = do
                write populated `shouldBe` populated
                write empty `shouldBe` empty
          mapM_
            ( \labels -> do
                keepsFields (setHeader (signature labels))
                keepsFields (setHeader (input labels))
                keepsFields (setHeader (request labels))
                renderSig (signature labels) `shouldBe` ""
                renderSI (input labels) `shouldBe` ""
                renderAS (request labels) `shouldBe` ""
            )
            invalidLabels
          let valid = "second" :| ["*.first", "a_1.b-2"]
          lookupHeader @Sig.Signature (setHeader (signature valid) populated) `shouldBe` Right (Just (signature valid))
          lookupHeader @SI.SignatureInput (setHeader (input valid) populated) `shouldBe` Right (Just (input valid))
          lookupHeader @AS.AcceptSignature (setHeader (request valid) populated) `shouldBe` Right (Just (request valid))
      , it "rejects invalid authored signature components and metadata as a whole" $ do
          let existing =
                headerMapFromList
                  [ ("Signature-Input", "original=()")
                  , ("Signature-Input", "second=()")
                  , ("Accept-Signature", "original=()")
                  , ("Accept-Signature", "second=()")
                  ]
              empty = headerMapFromList []
              input components metadata =
                SI.SignatureInput (SI.SignatureInputEntry "first" [] [] :| [SI.SignatureInputEntry "second" (map (\(name, parameters) -> SI.SignatureComponent (RFC8941String name) parameters) components) metadata])
              request components metadata =
                AS.AcceptSignature (AS.AcceptSignatureEntry "first" [] [] :| [AS.AcceptSignatureEntry "second" (map (\(name, parameters) -> AS.AcceptSignatureComponent (RFC8941String name) parameters) components) metadata])
              text = String . RFC8941String
              rejectInput components metadata = do
                let value = input components metadata
                setHeader value existing `shouldBe` existing
                setHeader value empty `shouldBe` empty
                renderSI value `shouldBe` ""
              rejectRequest components metadata = do
                let value = request components metadata
                setHeader value existing `shouldBe` existing
                setHeader value empty `shouldBe` empty
                renderAS value `shouldBe` ""
              check components = do
                rejectInput components []
                rejectRequest components []
              checkMetadata metadata = do
                rejectInput [] metadata
                rejectRequest [] metadata
              duplicate = [("date", [("a", Nothing), ("b", Just (Integer 2))]), ("date", [("b", Just (Integer 2)), ("a", Just (Boolean True))])]
              distinct = map (\name -> (name, [])) ["a", "b", "c", "d", "e", "f", "g"]
          mapM_
            check
            [ [("", [])]
            , [("DATE", [])]
            , [("dáte", [])]
            , [("@", [])]
            , [("@extension_1", [])]
            , [("@signature-params", [])]
            , [("@query-param", [("name", Just (text "page")), ("name", Just (Integer 1))])]
            , [("@query-param", [])]
            , [("@method", [("tr", Nothing)])]
            , [("date", [("bs", Nothing), ("sf", Just (Boolean False))])]
            , [("date", [("key", Just (text "Invalid"))])]
            , [("date", [("ext", Just (text "bad\tvalue"))])]
            , [("date", [("Upper", Nothing)])]
            , duplicate
            , distinct <> duplicate
            ]
          mapM_
            checkMetadata
            [ [("keyid", Just (Integer 1))]
            , [("nonce", Just (text "bad\nvalue"))]
            , [("extra", Just (DateValue 1_000_000_000_000_000))]
            , [("Invalid", Nothing)]
            ]
          rejectInput [] [("created", Nothing)]
          rejectInput [] [("expires", Just (Integer 1_000_000_000_000_000))]
          rejectRequest [] [("created", Just (Integer 1))]
          rejectRequest [] [("expires", Just (Boolean False))]
          let components = [("@query-param", [("name", Just (Integer 1)), ("req", Just (Boolean True)), ("name", Just (text "page"))]), ("date", []), ("date", [("req", Nothing)])]
              params = [("tag", Just (Integer 1)), ("tag", Just (text "demo"))]
              authoredInput = input components (params <> [("created", Just (Integer 123))])
              authoredRequest = request components (params <> [("created", Nothing)])
              expected = "first=(), second=(\"@query-param\";name=\"page\";req \"date\" \"date\";req);tag=\"demo\";created"
          renderSI authoredInput `shouldBe` expected <> "=123"
          renderAS authoredRequest `shouldBe` expected
          lookupHeader @SI.SignatureInput (setHeader authoredInput existing) `shouldBe` fmap Just (parseSI (expected <> "=123"))
          lookupHeader @AS.AcceptSignature (setHeader authoredRequest existing) `shouldBe` fmap Just (parseAS expected)
      , it "uses inner-list whitespace and String rules for both fields" $ do
          let wire = "s=(  \"@method\"   \"example\";extension=\"escaped\\\"value\"  )"
              canonical = "s=(\"@method\" \"example\";extension=\"escaped\\\"value\")"
          fmap renderSI (parseSI wire) `shouldBe` Right canonical
          fmap renderAS (parseAS wire) `shouldBe` Right canonical
          mapM_
            ( \bad -> do
                parseSI bad `shouldSatisfy` isLeft
                parseAS bad `shouldSatisfy` isLeft
            )
            ["s=(\t\"@method\")", "s=(\"@method\"\t\"date\")", "s=(\"@method\"\t)", "s=(7)", "s=(\"DATE\")", "s=(\"@signature-params\")"]
      , it "keeps parameter positions and detects equivalent covered components" $ do
          fmap renderSI (parseSI "s=(\"date\"; z=?1;a=1;a=2);keyid=\"a\";created=1;keyid=\"b\"")
            `shouldBe` Right "s=(\"date\";z;a=2);keyid=\"b\";created=1"
          parseSI "s=(\"date\";a;b \"date\";b;a=?1)" `shouldSatisfy` isLeft
          fmap renderSI (parseSI "s=(\"date\" \"date\";req)") `shouldBe` Right "s=(\"date\" \"date\";req)"
          let manyComponents = "\"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"date\";a;b"
          parseSI ("s=(" <> manyComponents <> " \"date\";b;a)") `shouldSatisfy` isLeft
          fmap renderSI (parseSI ("s=(" <> manyComponents <> " \"date\";b)")) `shouldBe` Right ("s=(" <> manyComponents <> " \"date\";b)")
          parseSig "a=::, b=::, c=::, a=::" `shouldSatisfy` isLeft
      , it "validates registered component identifiers and parameter types in both fields" $ do
          let inputs bytes = headerMapFromList [("Signature-Input", bytes)]
              requests bytes = headerMapFromList [("Accept-Signature", bytes)]
              valid = "s=(\"@query-param\";name=\"page\";req \"content-digest\";key=\"sha-256\";tr \"date\";bs \"@extension-1\";x-note=@2)"
              duplicate = "s=(\"@query-param\";name=4;name=\"page\")"
              canonical = "s=(\"@query-param\";name=\"page\")"
          fmap (fmap renderSI) (lookupHeader @SI.SignatureInput (inputs valid)) `shouldBe` Right (Just valid)
          fmap (fmap renderAS) (lookupHeader @AS.AcceptSignature (requests valid)) `shouldBe` Right (Just valid)
          fmap renderSI (parseSI duplicate) `shouldBe` Right canonical
          fmap renderAS (parseAS duplicate) `shouldBe` Right canonical
          mapM_
            ( \invalid -> do
                lookupHeader @SI.SignatureInput (inputs invalid) `shouldSatisfy` isLeft
                lookupHeader @AS.AcceptSignature (requests invalid) `shouldSatisfy` isLeft
            )
            [ "s=(\"@extension_name\")"
            , "s=(\"@query-param\")"
            , "s=(\"@query-param\";name=token)"
            , "s=(\"@query-param\";name=\"page\";name=1)"
            , "s=(\"date\";sf=1)"
            , "s=(\"date\";bs=\"binary\")"
            , "s=(\"date\";tr=token)"
            , "s=(\"@method\";req=1)"
            , "s=(\"content-digest\";key)"
            , "s=(\"content-digest\";key=\"SHA-256\")"
            , "s=(\"@method\";tr)"
            , "s=(\"@path\";sf)"
            , "s=(\"date\";bs;sf)"
            , "s=(\"content-digest\";key=\"sha-256\";bs)"
            ]
      , it "distinguishes signature timestamps from negotiation flags" $ do
          parseSI "s=();created" `shouldSatisfy` isLeft
          parseAS "s=();created=1" `shouldSatisfy` isLeft
          parseSI "s=();keyid=token" `shouldSatisfy` isLeft
          parseAS "s=();expires=?0" `shouldSatisfy` isLeft
          case parseAS "s=();created=?1;ext=@1" of
            Right request ->
              lookupRawHeader hAcceptSignature (setHeader request (headerMapFromList [])) `shouldBe` Just ("s=();created;ext=@1" :| [])
            Left failure -> expectationFailure failure
      , it "Signature round-trip" prop_signature_roundtrip
      , it "Signature-Input round-trip" prop_siginput_roundtrip
      , it "Accept-Signature round-trip" prop_acceptsig_roundtrip
      ]
