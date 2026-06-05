{-# LANGUAGE OverloadedStrings #-}

module Test.EDI (ediTests) where

import qualified Data.Vector as V
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified EDI.Decode as Decode
import qualified EDI.Encode as Encode
import EDI.Value

ediTests :: Spec
ediTests = describe "EDI" $ sequence_
  [ decodeTests
  , encodeTests
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
