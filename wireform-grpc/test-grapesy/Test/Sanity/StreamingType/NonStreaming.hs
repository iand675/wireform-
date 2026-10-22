{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.StreamingType.NonStreaming (tests) where

import Data.Word
import Test.Syd
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.FilePath ((</>))

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary (RawRpc)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server (ContentType(..))
import Network.GRPC.Server.StreamType qualified as Server
import Network.GRPC.Server.StreamType.Binary qualified as Binary

import Test.Driver.ClientServer
import Data.Foldable (toList)

tests :: Spec
tests = describe "Test.Sanity.StreamingType.NonStreaming" $ sequence_ [
      describe "increment" $ sequence_ [
          it "default" $
            test_increment def
        , it "unix socket" $ do
            -- A fresh directory per run: a fixed path collides with any
            -- other run of this suite (bind: address already in use, or a
            -- client reaching the other run's server).
            tmpDir <- getCanonicalTemporaryDirectory
            withTempDirectory tmpDir "grapesy" $ \dir ->
              test_increment def { serverPort = Left (dir </> "grapesy.sock") }
        , describe "Content-Type" $ sequence_ [
              describe "ok" $ sequence_ [
                  -- Without the +format part
                  it "application/grpc" $
                    test_increment def {
                        clientContentType = ValidOverride $
                          ContentTypeOverride "application/grpc"
                      }

                  -- Random other format
                  -- See discussion in 'parseContentType'
                , it "application/grpc+gibberish" $
                    test_increment def {
                        clientContentType = ValidOverride $
                          ContentTypeOverride "application/grpc+gibberish"
                      }
                ]
            , describe "fail" $ sequence_ [
                  it "application/invalid-subtype" $
                    test_increment def {
                        isExpectedServerException =
                          isInvalidRequestHeaders
                      , isExpectedClientException =
                          isGrpc415
                      , clientContentType = InvalidOverride . Just $
                          ContentTypeOverride "application/invalid-subtype"
                      }

                  -- gRPC spec does not allow parameters
                , it "charset" $
                    test_increment def {
                        isExpectedServerException =
                          isInvalidRequestHeaders
                      , isExpectedClientException =
                          isGrpc415
                      , clientContentType = InvalidOverride . Just $
                          ContentTypeOverride "application/grpc; charset=utf-8"
                      }
                ]
            ]
        , describe "TLS" $ sequence_ [
              describe "ok" $ sequence_ [
                  it "certAsRoot" $
                    test_increment def {
                        useTLS = Just $ TlsOk TlsOkCertAsRoot
                      }
                , it "skipValidation" $
                    test_increment def {
                        useTLS = Just $ TlsOk TlsOkSkipValidation
                      }
                  ]
            , describe "fail" $ sequence_ [
                  it "validation" $
                    test_increment def {
                        isExpectedClientException =
                          isHandshakeFailed
                      , useTLS =
                          Just $ TlsFail TlsFailValidation
                      }
                , it "unsupported" $
                    test_increment def {
                        isExpectedClientException =
                          isHandshakeFailed
                      , useTLS =
                          Just $ TlsFail TlsFailUnsupported
                      }
                ]
            ]
        , describe "compression" $ sequence_ [
              describe "supported" $
                let mkTest :: Compr.Compression -> Spec
                    mkTest compr = it comprId $
                        test_increment def {
                            clientCompr = Compr.only compr
                          , serverCompr = Compr.only compr
                          }
                      where
                        comprId :: String
                        comprId = show (Compr.compressionId compr)
                in mapM_ mkTest (toList Compr.allSupportedCompression)
            , describe "unsupported" $ sequence_ [
                  it "clientChoosesUnsupported" $
                    test_increment def {
                        isExpectedServerException =
                          isServerUnsupportedCompression
                      , isExpectedClientException =
                          isGrpc400
                      , clientInitCompr =
                          Just Compr.gzip
                      , serverCompr =
                          Compr.none
                      }
                , it "serverChoosesUnsupported" $
                    test_increment def {
                        isExpectedClientException =
                           isClientUnsupportedCompression
                      , isExpectedServerException =
                           isClientDisconnected
                      , clientCompr =
                          Compr.none
                      , serverCompr =
                          Compr.insist Compr.gzip
                      }
                ]
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Binary (without Protobuf)
-------------------------------------------------------------------------------}

type BinaryIncrement = RawRpc "binary" "increment"

type instance RequestMetadata          BinaryIncrement = NoMetadata
type instance ResponseInitialMetadata  BinaryIncrement = NoMetadata
type instance ResponseTrailingMetadata BinaryIncrement = NoMetadata

test_increment :: ClientServerConfig -> IO ()
test_increment config = testClientServer $ ClientServerTest {
      config
    , client = simpleTestClient $ \conn -> do
        Client.withRPC conn def (Proxy @BinaryIncrement) $ \call -> do
          Binary.sendFinalInput @Word8 call 1
          resp <- fst <$> Binary.recvFinalOutput @Word8 call
          resp `shouldBe` 2
    , server = [
         Server.fromMethod @BinaryIncrement $ Binary.mkNonStreaming $ \n ->
           return (succ (n :: Word8))
        ]
    }
