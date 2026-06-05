{-# LANGUAGE OverloadedStrings #-}

module Test.EDI (ediTests) where

import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified EDI.Decode as Decode
import qualified EDI.Encode as Encode
import qualified EDI.Query as Query
import qualified EDI.Validation as Validation
import EDI.Value
import qualified EDI.X12 as X12

ediTests :: Spec
ediTests = describe "EDI" $ sequence_
  [ decodeTests
  , encodeTests
  , queryTests
  , validationTests
  , x12Tests
  , propertyRoundtrips
  ]

decodeTests :: Spec
decodeTests = describe "decode" $ sequence_
  [ it "decodes simple X12 segments with default delimiters" $
      Decode.decode "ST*850*0001~BEG*00*SA*12345~SE*3*0001~" `shouldBe`
        Right
          ( Interchange
              defaultSyntax
              ( V.fromList
                  [ Segment "ST" (V.fromList [Simple "850", Simple "0001"])
                  , Segment "BEG" (V.fromList [Simple "00", Simple "SA", Simple "12345"])
                  , Segment "SE" (V.fromList [Simple "3", Simple "0001"])
                  ]
              )
          )
  , it "splits composite elements" $
      Decode.decode "REF*PO:12345~" `shouldBe`
        Right
          ( Interchange
              defaultSyntax
              (V.singleton (Segment "REF" (V.singleton (Composite (V.fromList ["PO", "12345"])))))
          )
  , it "infers X12 ISA delimiters" $
      Decode.decode isaSample `shouldBe`
        Right isaDoc
  ]

queryTests :: Spec
queryTests = describe "query helpers" $ sequence_
  [ it "finds segments by tag and reads element text" $ do
      doc <- either expectationFailure pure (Decode.decode "ST*850*0001~BEG*00*SA*12345~")
      Query.countSegments "BEG" doc `shouldBe` 1
      beg <- either expectationFailure pure (Query.requireSegmentByTag "BEG" doc)
      Query.requireElementText 2 beg `shouldBe` Right "12345"
  , it "reads composite elements by position" $ do
      doc <- either expectationFailure pure (Decode.decode "REF*PO:12345~")
      ref <- either expectationFailure pure (Query.requireSegmentByTag "REF" doc)
      Query.compositeAt 0 ref `shouldBe` Just (V.fromList ["PO", "12345"])
  ]

validationTests :: Spec
validationTests = describe "validation" $ sequence_
  [ it "accepts a parsed interchange" $
      Validation.validateInterchange isaDoc `shouldBe` Right ()
  , it "rejects payload text containing an element separator" $ do
      let doc = Interchange defaultSyntax (V.singleton (Segment "N1" (V.singleton (Simple "bad*value"))))
      case Validation.validateInterchange doc of
        Left errs -> V.length errs `shouldBe` 1
        Right () -> expectationFailure "expected delimiter validation failure"
  ]

x12Tests :: Spec
x12Tests = describe "X12 helpers" $ sequence_
  [ it "parses and validates an X12 envelope" $ do
      doc <- either expectationFailure pure (Decode.decode x12Sample)
      env <- either (expectationFailure . show) pure (X12.parseX12Envelope doc)
      V.length (X12.x12Groups env) `shouldBe` 1
      let group = X12.x12Groups env V.! 0
      V.length (X12.groupTransactions group) `shouldBe` 1
      X12.validateX12 doc `shouldBe` Right ()
  , it "reports control number mismatches" $ do
      doc <- either expectationFailure pure (Decode.decode (T.replace "IEA*1*000000001" "IEA*1*999999999" x12Sample))
      case X12.validateX12 doc of
        Left errs -> V.any isControlMismatch errs `shouldBe` True
        Right () -> expectationFailure "expected X12 control-number validation failure"
  , it "builds an accepted 997 acknowledgement" $ do
      doc <- either expectationFailure pure (Decode.decode x12Sample)
      env <- either (expectationFailure . show) pure (X12.parseX12Envelope doc)
      let ack = X12.functionalAcknowledgment997 ackSettings env
      Query.countSegments "ST" ack `shouldBe` 1
      Query.countSegments "AK1" ack `shouldBe` 1
      Query.countSegments "AK2" ack `shouldBe` 1
      Query.countSegments "AK5" ack `shouldBe` 1
      Query.countSegments "AK9" ack `shouldBe` 1
      Encode.encode ack `shouldBe`
        "ISA*00*          *00*          *ZZ*RECEIVER       *ZZ*SENDER         *260605*1914*U*004010*000000002*0*T*:~GS*FA*RECEIVER*SENDER*20260605*1914*2*X*004010~ST*997*0001~AK1*PO*1~AK2*850*0001~AK5*A~AK9*A*1*1*1~SE*6*0001~GE*1*2~IEA*1*000000002~"
  ]

encodeTests :: Spec
encodeTests = describe "encode" $ sequence_
  [ it "renders simple and composite elements" $
      Encode.encode
        ( Interchange
            defaultSyntax
            ( V.fromList
                [ Segment "ST" (V.fromList [Simple "850", Simple "0001"])
                , Segment "REF" (V.singleton (Composite (V.fromList ["PO", "12345"])))
                ]
            )
        )
        `shouldBe` "ST*850*0001~REF*PO:12345~"
  , it "renders custom delimiters" $
      Encode.encode
        ( Interchange
            (Syntax '^' '>' Nothing '!')
            (V.singleton (Segment "A" (V.fromList [Simple "B", Composite (V.fromList ["C", "D"])])))
        )
        `shouldBe` "A^B^C>D!"
  ]

propertyRoundtrips :: Spec
propertyRoundtrips = describe "property roundtrips" $ sequence_
  [ it "round-trips generated simple interchanges" $ property $ do
      doc <- forAll genInterchange
      Decode.decodeWithSyntax (interchangeSyntax doc) (Encode.encode doc) === Right doc
  ]

genInterchange :: Gen Interchange
genInterchange = do
  segments <- Gen.list (Range.linear 0 12) genSegment
  pure (Interchange defaultSyntax (V.fromList segments))

genSegment :: Gen Segment
genSegment = do
  tag <- Gen.text (Range.singleton 3) Gen.upper
  elems <- Gen.list (Range.linear 0 8) genSimpleElement
  pure (Segment tag (V.fromList elems))

genSimpleElement :: Gen Element
genSimpleElement = Simple <$> Gen.text (Range.linear 0 12) genElementChar

genElementChar :: Gen Char
genElementChar =
  Gen.filter
    (\c -> c /= '*' && c /= ':' && c /= '~' && c /= '\r' && c /= '\n')
    Gen.alphaNum

isControlMismatch :: X12.X12Error -> Bool
isControlMismatch (X12.X12ControlNumberMismatch _ _ _) = True
isControlMismatch _ = False

x12Sample :: Text
x12Sample =
  "ISA*00*          *00*          *ZZ*SENDER         *ZZ*RECEIVER       *260605*1914*U*004010*000000001*0*T*:~GS*PO*SENDER*RECEIVER*20260605*1914*1*X*004010~ST*850*0001~BEG*00*SA*12345~SE*3*0001~GE*1*1~IEA*1*000000001~"

ackSettings :: X12.AckSettings
ackSettings = X12.defaultAckSettings
  { X12.ackSenderId = "RECEIVER"
  , X12.ackReceiverId = "SENDER"
  , X12.ackDateYYMMDD = "260605"
  , X12.ackDateCCYYMMDD = "20260605"
  , X12.ackTimeHHMM = "1914"
  , X12.ackInterchangeControlNumber = "2"
  , X12.ackGroupControlNumber = "2"
  , X12.ackTransactionControlNumber = "0001"
  , X12.ackVersion = "004010"
  }

isaSample :: Text
isaSample =
  "ISA*00*          *00*          *ZZ*SENDER         *ZZ*RECEIVER       *260605*1914*U*00401*000000001*0*T*:~GS*PO*SENDER*RECEIVER*20260605*1914*1*X*004010~"

isaDoc :: Interchange
isaDoc =
  Interchange
    (Syntax '*' ':' (Just 'U') '~')
    ( V.fromList
        [ Segment
            "ISA"
            ( V.fromList
                [ Simple "00"
                , Simple "          "
                , Simple "00"
                , Simple "          "
                , Simple "ZZ"
                , Simple "SENDER         "
                , Simple "ZZ"
                , Simple "RECEIVER       "
                , Simple "260605"
                , Simple "1914"
                , Simple "U"
                , Simple "00401"
                , Simple "000000001"
                , Simple "0"
                , Simple "T"
                , Simple ":"
                ]
            )
        , Segment
            "GS"
            ( V.fromList
                [ Simple "PO"
                , Simple "SENDER"
                , Simple "RECEIVER"
                , Simple "20260605"
                , Simple "1914"
                , Simple "1"
                , Simple "X"
                , Simple "004010"
                ]
            )
        ]
    )
