{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A runnable Connect implementation of the @catalog.v1.CatalogService@ from
-- @catalog.proto@ — the same schema the OpenAPI example is generated from.
--
-- One @.proto@, one 'Service' value, served over the Connect protocol and hit
-- by an in-process Connect client exercising all four RPC kinds plus the
-- cacheable GET. Run it with @cabal run example-connect-catalog@.
--
-- The message types and the @Protobuf CatalogService "meth"@ tags are spliced
-- straight from the schema by 'loadProto' / 'loadProtoServices' — there is no
-- Connect-specific codegen (see the wireform-connect README). The same
-- 'catalogService' value could be served over gRPC by @wireform-grpc@'s
-- @fromService@.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally, try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Proxy (Proxy (..))
import Network.Connect.Client
import Network.Connect.Error (ConnectError (..), ConnectException (..), throwConnect)
import Network.Connect.Server
import Network.GRPC.Protobuf.TH (loadProtoServices)
import Network.GRPC.Spec (GrpcError (..), Proto (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (http2Only)
import Network.Socket qualified as NS
import Proto.TH (loadProto)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Timeout (timeout)

-- Splice the message records (with proto3-JSON + wire codecs) and the
-- protocol-agnostic service tags from the schema.
$(loadProto "examples/openapi/catalog.proto")
$(loadProtoServices "examples/openapi/catalog.proto")

------------------------------------------------------------------------
-- Service implementation
--
-- Registration is order-insensitive and completeness-checked at compile
-- time: drop a method (or add a stray one) and this stops compiling.
------------------------------------------------------------------------

catalogService :: Service CatalogService ConnectServerM
catalogService =
  service
    ( method @GetProduct getProductH
        :& method @CreateProduct createProductH
        :& method @ListProducts listProductsH
        :& method @ImportProducts importProductsH
        :& method @Watch watchH
        :& method @SearchLegacy searchLegacyH
        :& Done
    )

-- Unary (side-effect-free → also reachable over a cacheable GET).
getProductH :: Proto GetProductRequest -> ConnectServerM (Proto Product)
getProductH (Proto req) =
  let pid = getProductRequestProductId req
   in if T.null pid
        then liftIO (throwConnect GrpcInvalidArgument "product_id is required")
        else pure (Proto (sampleProduct pid "Widget" 1999))

-- Unary write → POST only.
createProductH :: Proto CreateProductRequest -> ConnectServerM (Proto Product)
createProductH (Proto req) =
  pure . Proto $
    defaultProduct
      { productProductId = "prod-" <> createProductRequestSlug req
      , productName = createProductRequestName req
      , productPriceCents = createProductRequestPriceCents req
      , productSlug = createProductRequestSlug req
      , productCategory = createProductRequestCategory req
      , productTags = createProductRequestTags req
      , productAttributes = createProductRequestAttributes req
      }

-- Server streaming → one request, many responses.
listProductsH :: Proto ListProductsRequest -> (Proto Product -> ConnectServerM ()) -> ConnectServerM ()
listProductsH (Proto req) send =
  forM_ [1 .. 3 :: Int] $ \i ->
    send (Proto (sampleProduct (T.pack ("prod-" <> show i)) ("Item " <> T.pack (show i)) (fromIntegral (i * 500))))
  where
    _cat = listProductsRequestCategory req -- (would filter in a real impl)

-- Client streaming → many requests, one summary.
importProductsH :: ConnectServerM (Maybe (Proto CreateProductRequest)) -> ConnectServerM (Proto ImportSummary)
importProductsH recv = go 0 0
  where
    go !imported !skipped = do
      m <- recv
      case m of
        Nothing -> pure (Proto defaultImportSummary{importSummaryImported = imported, importSummarySkipped = skipped})
        Just (Proto r)
          | T.null (createProductRequestName r) -> go imported (skipped + 1)
          | otherwise -> go (imported + 1) skipped

-- Bidirectional streaming → a response event per request.
watchH :: ConnectServerM (Maybe (Proto WatchRequest)) -> (Proto ProductEvent -> ConnectServerM ()) -> ConnectServerM ()
watchH recv send = loop
  where
    loop = do
      m <- recv
      case m of
        Nothing -> pure ()
        Just (Proto r) -> do
          let pid = case watchRequestSelector r of
                Just (WatchRequest'Selector'ProductId p) -> p
                _ -> "*"
          send . Proto $
            defaultProductEvent
              { productEventKind = ProductEvent'EventKind'EventKindCreated
              , productEventProduct = Just (sampleProduct pid "Watched" 0)
              }
          loop

-- A retired unary RPC.
searchLegacyH :: Proto GetProductRequest -> ConnectServerM (Proto Product)
searchLegacyH _ = liftIO (throwConnect GrpcUnimplemented "SearchLegacy is retired; use ListProducts")

sampleProduct :: Text -> Text -> Int -> Product
sampleProduct pid nm cents =
  defaultProduct
    { productProductId = pid
    , productName = nm
    , productPriceCents = fromIntegral cents
    , productSlug = T.toLower nm
    , productCategory = Category'CategoryBooks
    , productTags = V.fromList ["sample"]
    , productAttributes = Map.fromList [("origin", "demo")]
    }

------------------------------------------------------------------------
-- Serve it, then drive it with an in-process client
------------------------------------------------------------------------

catalogHandlers :: [MethodHandler]
catalogHandlers = connectHandlers catalogService

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  withServerSocket $ \sock port -> do
    ready <- newEmptyMVar
    tid <- forkIO (putMVar ready () >> runServerOnListener (srvCfg port) sock)
    takeMVar ready
    let conncfg = defaultConnectionConfig{connectionHost = "127.0.0.1", connectionPort = show port, connectionVersionRange = http2Only}
    ( withConnectClient defaultConnectClientConfig conncfg runClient
        `finally` killThread tid
      )
  where
    srvCfg port =
      defaultServerConfig
        { serverHost = "127.0.0.1"
        , serverPort = show port
        , serverVersionRange = http2Only
        , serverHandler = connectApplication defaultConnectServerConfig catalogHandlers
        }

runClient :: ConnectClient -> IO ()
runClient cl = do
  TIO.putStrLn "== unary CreateProduct (POST) =="
  created <- nonStreaming cl (Proxy @CreateProduct) (mkCreate "Notebook" 4999 "notebook")
  printProduct created

  TIO.putStrLn "\n== unary GetProduct over a cacheable GET =="
  got <- nonStreamingGet cl (Proxy @GetProduct) (Proto defaultGetProductRequest{getProductRequestProductId = "abc"})
  printProduct got

  TIO.putStrLn "\n== server streaming ListProducts =="
  serverStreaming cl (Proxy @ListProducts) (Proto defaultListProductsRequest) $ \recv ->
    let drain = recv >>= maybe (pure ()) (\p -> printProduct p >> drain) in drain

  TIO.putStrLn "\n== client streaming ImportProducts =="
  summary <- clientStreaming cl (Proxy @ImportProducts) $ \send -> do
    send (mkCreate "A" 100 "a")
    send (mkCreate "" 0 "blank") -- skipped (empty name)
    send (mkCreate "B" 200 "b")
  let Proto s = summary
  TIO.putStrLn ("imported=" <> tshow (importSummaryImported s) <> " skipped=" <> tshow (importSummarySkipped s))

  -- NOTE: the Watch handler + its registration are real and compiled; a
  -- live full-duplex exchange works against the conformance reference client
  -- (see the 1411/1411 connectconformance run). The in-process HTTP/2 loopback
  -- used by this demo cannot drive concurrent send+recv on one thread, so the
  -- live call is guarded by a timeout (the same limitation the loopback test
  -- suite carries for `bidi Converse (http2)`).
  TIO.putStrLn "\n== bidi streaming Watch (in-process loopback; see note) =="
  mres <- timeout 2000000 $
    biDiStreaming cl (Proxy @Watch) $ \send recv ->
      forM_ ["p1", "p2"] $ \p -> do
        send (Proto defaultWatchRequest{watchRequestSelector = Just (WatchRequest'Selector'ProductId p)})
        ev <- recv
        case ev of
          Just (Proto e) -> TIO.putStrLn ("  event: " <> maybe "?" productName (productEventProduct e))
          Nothing -> pure ()
  case mres of
    Just () -> pure ()
    Nothing -> TIO.putStrLn "  (loopback bidi did not complete in time — known harness limitation)"

  TIO.putStrLn "\n== the retired SearchLegacy returns an RPC error =="
  r <- try (nonStreaming cl (Proxy @SearchLegacy) (Proto defaultGetProductRequest{getProductRequestProductId = "x"}))
  case r of
    Left (ConnectException e) -> TIO.putStrLn ("caught " <> tshow (ceCode e) <> ": " <> maybe "" id (ceMessage e))
    Right (_ :: Proto Product) -> TIO.putStrLn "unexpected success"
  where
    mkCreate nm cents slug =
      Proto
        defaultCreateProductRequest
          { createProductRequestName = nm
          , createProductRequestPriceCents = cents
          , createProductRequestSlug = slug
          , createProductRequestCategory = Category'CategoryBooks
          }

printProduct :: Proto Product -> IO ()
printProduct (Proto p) =
  TIO.putStrLn
    ( "  "
        <> productProductId p
        <> " | "
        <> productName p
        <> " | "
        <> tshow (productPriceCents p)
        <> "c | tags="
        <> tshow (V.toList (productTags p))
    )

tshow :: Show a => a -> Text
tshow = T.pack . show

------------------------------------------------------------------------
-- Ephemeral listener
------------------------------------------------------------------------

withServerSocket :: (NS.Socket -> Int -> IO a) -> IO a
withServerSocket k = do
  let hints = NS.defaultHints{NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr available"
    (addr : _) ->
      bracket (NS.openSocket addr) NS.close $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        bound <- NS.getSocketName sock
        let port = case bound of NS.SockAddrInet p _ -> fromIntegral p; _ -> 0 :: Int
        k sock port
