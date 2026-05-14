{- | Round-trip + variant-overwrite tests for the oneof bridge
rewire. Exercises 'Proto.TH.loadProto'-generated codecs for
@Envelope@, whose @oneof envelope_choice@ produces an
@Envelope'EnvelopeChoice@ sum type via 'Proto.TH.mkOneofDataDecs'.
-}
module Test.Proto.Derive.Oneof (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Proto qualified as PD
import Proto qualified as PE
import Test.Proto.Derive.OneofInstances (
  Envelope (..),
  Envelope'EnvelopeChoice (..),
  Inner (..),
  defaultEnvelope,
  defaultInner,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: TestTree
tests =
  testGroup
    "Proto.TH oneof bridge"
    [ testCase "no choice variant set: round-trips empty payload" $ do
        let e = defaultEnvelope
        let bs = PE.encodeMessage e
        bs @?= BS.empty
        PD.decodeMessage bs @?= Right e
    , testCase "label only: round-trips" $ do
        let e = defaultEnvelope {envelopeEnvelopeLabel = T.pack "labelled"}
        PD.decodeMessage (PE.encodeMessage e) @?= Right e
    , testCase "choice_url variant round-trips" $ do
        let e =
              defaultEnvelope
                { envelopeEnvelopeLabel = T.pack "withUrl"
                , envelopeEnvelopeChoice =
                    Just
                      ( Envelope'EnvelopeChoice'ChoiceUrl
                          (T.pack "https://example.test/x")
                      )
                }
        PD.decodeMessage (PE.encodeMessage e) @?= Right e
    , testCase "choice_blob variant round-trips" $ do
        let e =
              defaultEnvelope
                { envelopeEnvelopeChoice =
                    Just
                      ( Envelope'EnvelopeChoice'ChoiceBlob
                          (BS.pack [0xCA, 0xFE, 0xBA, 0xBE])
                      )
                }
        PD.decodeMessage (PE.encodeMessage e) @?= Right e
    , testCase "choice_seed variant round-trips" $ do
        let e =
              defaultEnvelope
                { envelopeEnvelopeChoice = Just (Envelope'EnvelopeChoice'ChoiceSeed 12345)
                }
        PD.decodeMessage (PE.encodeMessage e) @?= Right e
    , testCase "choice_inner submessage variant round-trips" $ do
        let inner = defaultInner {innerInnerId = 99}
            e =
              defaultEnvelope
                { envelopeEnvelopeLabel = T.pack "nested"
                , envelopeEnvelopeChoice = Just (Envelope'EnvelopeChoice'ChoiceInner inner)
                }
        PD.decodeMessage (PE.encodeMessage e) @?= Right e
    , testCase "later variant on the wire wins (proto3 oneof semantics)" $ do
        -- Concatenate two encodings that each set a different
        -- variant; per proto3 the last one wins on decode.
        let eUrl =
              defaultEnvelope
                { envelopeEnvelopeChoice =
                    Just
                      ( Envelope'EnvelopeChoice'ChoiceUrl
                          (T.pack "old")
                      )
                }
            eSeed =
              defaultEnvelope
                { envelopeEnvelopeChoice = Just (Envelope'EnvelopeChoice'ChoiceSeed 7)
                }
            combined = PE.encodeMessage eUrl `BS.append` PE.encodeMessage eSeed
        PD.decodeMessage combined @?= Right eSeed
    ]
