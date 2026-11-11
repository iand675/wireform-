{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.PerMessageDeflate (tests) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Network.WebSocket.Connection.Role
import Network.WebSocket.PerMessageDeflate
import Test.Syd


tests :: Spec
tests =
  describe "PerMessageDeflate" $
    sequence_
      [ describe "offer parsing" $
          sequence_
            [ it "vanilla permessage-deflate" $ do
                let r = parseOffers ["permessage-deflate"]
                length r `shouldBe` 1
                offClientMaxWindowBitsHint (head r) `shouldBe` ClientWindowBitsAbsent
            , it "all four parameters" $ do
                let r =
                      parseOffers
                        [ "permessage-deflate; client_no_context_takeover; \
                          \server_no_context_takeover; server_max_window_bits=14; \
                          \client_max_window_bits=12"
                        ]
                length r `shouldBe` 1
                let o = head r
                offClientNoContextTakeover o `shouldBe` True
                offServerNoContextTakeover o `shouldBe` True
                offServerMaxWindowBits o `shouldBe` Just 14
                offClientMaxWindowBitsHint o `shouldBe` ClientWindowBitsSet 12
            , it "bare client_max_window_bits is treated as a hint" $ do
                let r = parseOffers ["permessage-deflate; client_max_window_bits"]
                length r `shouldBe` 1
                offClientMaxWindowBitsHint (head r) `shouldBe` ClientWindowBitsHinted
            , it "non-PMD extensions are ignored" $ do
                let r = parseOffers ["x-foo; bar", "permessage-deflate"]
                length r `shouldBe` 1
            , it "rejects window_bits out of range" $ do
                let r = parseOffers ["permessage-deflate; server_max_window_bits=16"]
                r `shouldBe` []
            , it "declines offers with duplicate parameters" $
                mapM_ (\offer -> parseOffers [offer] `shouldBe` []) duplicateParams
            , it "still accepts a separate valid offer after a duplicate" $
                parseOffers
                  [ "permessage-deflate; server_max_window_bits=10; server_max_window_bits=15"
                  , "permessage-deflate; server_max_window_bits=12"
                  ]
                  `shouldBe` parseOffers ["permessage-deflate; server_max_window_bits=12"]
            ]
      , describe "selectOffer" $
          sequence_
            [ it "default server policy + default offer picks 15/15" $ do
                let Just p = selectOffer defaultPmdParams [defaultPmdOffer]
                pmdServerMaxWindowBits p `shouldBe` 15
                pmdClientMaxWindowBits p `shouldBe` 15
            , it "client_no_context_takeover propagates" $ do
                let off = defaultPmdOffer {offClientNoContextTakeover = True}
                    Just p = selectOffer defaultPmdParams [off]
                pmdClientNoContextTakeover p `shouldBe` True
            , it "server policy can demand no_context_takeover" $ do
                let pol = defaultPmdParams {pmdServerNoContextTakeover = True}
                    Just p = selectOffer pol [defaultPmdOffer]
                pmdServerNoContextTakeover p `shouldBe` True
            , it "client window bits ceiling clamps to server policy" $ do
                let off = defaultPmdOffer {offClientMaxWindowBitsHint = ClientWindowBitsSet 14}
                    pol = defaultPmdParams {pmdClientMaxWindowBits = 12}
                    Just p = selectOffer pol [off]
                pmdClientMaxWindowBits p `shouldBe` 12
            ]
      , describe "response header" $
          sequence_
            [ it "round-trip default params" $ do
                let p = defaultPmdParams
                parseResponseParams (responseHeader p) `shouldBe` Just p
            , it "round-trip with smaller server window" $ do
                let p = defaultPmdParams {pmdServerMaxWindowBits = 12}
                parseResponseParams (responseHeader p) `shouldBe` Just p
            , it "round-trip with both no_context_takeover flags" $ do
                let p =
                      defaultPmdParams
                        { pmdServerNoContextTakeover = True
                        , pmdClientNoContextTakeover = True
                        }
                parseResponseParams (responseHeader p) `shouldBe` Just p
            , it "rejects responses with duplicate parameters" $
                mapM_ (\reply -> parseResponseParams reply `shouldBe` Nothing) duplicateParams
            ]
      , describe "compress / decompress round-trip" $
          sequence_
            [ it "ascii small" $
                roundTrip
                  defaultPmdParams
                  "the quick brown fox jumps over the lazy dog"
            , it "highly compressible repetition" $
                roundTrip
                  defaultPmdParams
                  (BS.replicate 16384 (fromIntegral (fromEnum 'A')))
            , it "incompressible (random-ish)" $
                roundTrip defaultPmdParams $
                  BS.pack (cycle [0, 1, 2, 3, 4, 5, 6, 7] `takeBytes` 4096)
            , it "many small messages with context takeover" $
                manyMessages defaultPmdParams 50
            , it "many small messages with no_context_takeover" $
                manyMessages
                  defaultPmdParams
                    { pmdServerNoContextTakeover = True
                    , pmdClientNoContextTakeover = True
                    }
                  50
            , it "smaller window bits still round-trips" $
                roundTrip
                  defaultPmdParams {pmdServerMaxWindowBits = 9, pmdClientMaxWindowBits = 9}
                  "smaller window bits force more frequent dictionary reset"
            ]
      ]


duplicateParams :: [BS.ByteString]
duplicateParams =
  [ "permessage-deflate; server_no_context_takeover; Server_No_Context_Takeover"
  , "permessage-deflate; client_no_context_takeover; client_no_context_takeover"
  , "permessage-deflate; server_max_window_bits=15; server_max_window_bits=10"
  , "permessage-deflate; client_max_window_bits=15; client_max_window_bits=15"
  ]


-- Test that one Server-side compress + Client-side decompress
-- (and vice-versa) recovers the input.  Uses a pair of contexts:
-- the server's deflate / client's inflate handle one direction,
-- the client's deflate / server's inflate handle the other.
roundTrip :: PmdParams -> BS.ByteString -> IO ()
roundTrip params input =
  bracket (newPmdContext Server params) freePmdContext $ \server ->
    bracket (newPmdContext Client params) freePmdContext $ \client -> do
      -- server -> client direction
      compressed <- compressMessage server input
      pmdMaybeReset server Outbound
      inflated <- decompressMessage client compressed
      pmdMaybeReset client Inbound
      inflated `shouldBe` input
      -- client -> server direction
      compressed2 <- compressMessage client input
      pmdMaybeReset client Outbound
      inflated2 <- decompressMessage server compressed2
      pmdMaybeReset server Inbound
      inflated2 `shouldBe` input


-- Drive n round-trips on the same pair of contexts to exercise the
-- dictionary-takeover path.
manyMessages :: PmdParams -> Int -> IO ()
manyMessages params n =
  bracket (newPmdContext Server params) freePmdContext $ \server ->
    bracket (newPmdContext Client params) freePmdContext $ \client ->
      mapM_ (oneRound server client) [1 .. n]
  where
    oneRound server client i = do
      let msg = BS8.pack ("hello there number " <> show i)
      compressed <- compressMessage server msg
      pmdMaybeReset server Outbound
      inflated <- decompressMessage client compressed
      pmdMaybeReset client Inbound
      inflated `shouldBe` msg


takeBytes :: [a] -> Int -> [a]
takeBytes = flip take
