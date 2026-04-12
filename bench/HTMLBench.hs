{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList, sizeofSmallArray, indexSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stats
import HTML.DOM hiding (SmallArray)
import HTML.DOM (streamHTMLEventsRaw)
import HTML.Parse (parseHTML, tokenizeCountChunk, tokenizeOnlyIO, treeBuildOnlyIO)
import HTML.Rewriter
import HTML.Selector
import HTML.Value (HTMLAttribute (..), HTMLDocument (..), HTMLNode (..))
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Mem (performGC)


-- ---------------------------------------------------------------------------
-- Test input: ~29 KB HTML document
-- ---------------------------------------------------------------------------

mediumHTML :: BS.ByteString
mediumHTML =
  let header = "<html><body><div class=\"catalog\">\n"
      footer = "</div></body></html>\n"
      mkItem :: Int -> String
      mkItem i =
        concat
          [ "  <div class=\"item\" id=\"i"
          , show i
          , "\">\n"
          , "    <span class=\"name\">Product "
          , show i
          , "</span>\n"
          , "    <span class=\"price\">"
          , show (fromIntegral i * 9.99 :: Double)
          , "</span>\n"
          , "    <p class=\"description\">This is the description for product number "
          , show i
          , " in our catalog</p>\n"
          , "    <span class=\"category\">Category "
          , show (i `mod` 10)
          , "</span>\n"
          , "    <span class=\"inStock\">"
          , if even i then "true" else "false"
          , "</span>\n"
          , "  </div>\n"
          ]
      items = concatMap mkItem [1 .. 100 :: Int]
  in BS8.pack (header ++ items ++ footer)


-- ---------------------------------------------------------------------------
-- Bench harness
-- ---------------------------------------------------------------------------

data BenchResult = BenchResult
  { brLabel :: !String
  , brAllocIter :: !Int
  , brMbps :: !Double
  , brMutSec :: !Double
  }


bench :: String -> Int -> Int -> IO a -> IO BenchResult
bench label iters inputSize act = do
  performGC
  s0 <- getRTSStats
  let !alloc0 = allocated_bytes s0
      !mut0 = mutator_elapsed_ns s0
  go iters
  performGC
  s1 <- getRTSStats
  let !alloc1 = allocated_bytes s1
      !mut1 = mutator_elapsed_ns s1
      !totalAlloc = alloc1 - alloc0
      !perIter = fromIntegral totalAlloc `div` iters
      !mutNs = mut1 - mut0
      !mutSec = fromIntegral mutNs / 1e9 :: Double
      !totalBytes = fromIntegral inputSize * fromIntegral iters :: Double
      !mbps = totalBytes / (mutSec * 1e6)
  pure (BenchResult label perIter mbps mutSec)
  where
    go 0 = pure ()
    go !i = act >> go (i - 1)


benchSmall :: String -> Int -> IO a -> IO (Int, Double)
benchSmall label iters act = do
  performGC
  s0 <- getRTSStats
  let !alloc0 = allocated_bytes s0
      !mut0 = mutator_elapsed_ns s0
  go iters
  performGC
  s1 <- getRTSStats
  let !alloc1 = allocated_bytes s1
      !mut1 = mutator_elapsed_ns s1
      !totalAlloc = alloc1 - alloc0
      !perIter = fromIntegral totalAlloc `div` iters
      !mutNs = mut1 - mut0
      !nsPerIter = fromIntegral mutNs / fromIntegral iters :: Double
  pure (perIter, nsPerIter)
  where
    go 0 = pure ()
    go !i = act >> go (i - 1)


-- ---------------------------------------------------------------------------
-- Formatters
-- ---------------------------------------------------------------------------

printResult :: BenchResult -> Int -> Int -> IO Bool
printResult br targetMbps targetAlloc = do
  let allocOk = brAllocIter br <= targetAlloc
      thrptOk = targetMbps <= 0 || round (brMbps br) >= (targetMbps :: Int)
      pass = allocOk && thrptOk
      tag = if pass then "  OK  " else " MISS "
      thrptLabel = if targetMbps > 0
                   then "≥" ++ show targetMbps ++ " MB/s"
                   else "diag"
  putStrLn $ "[" ++ tag ++ "] " ++ brLabel br
  putStrLn $ "          throughput: " ++ show (round (brMbps br) :: Int) ++ " MB/s   (target: " ++ thrptLabel ++ ")"
  putStrLn $ "          alloc/iter: " ++ show (brAllocIter br) ++ " bytes   (target: ≤" ++ show targetAlloc ++ ")"
  putStrLn $ "          MUT time:   " ++ showMs (brMutSec br)
  pure pass


printSmallResult :: String -> Int -> Double -> Int -> Double -> IO Bool
printSmallResult label allocIter nsIter allocTarget nsTarget = do
  let allocOk = allocIter <= allocTarget
      timeOk = nsIter <= nsTarget
      pass = allocOk && timeOk
      tag = if pass then "  OK  " else " MISS "
  putStrLn $ "[" ++ tag ++ "] " ++ label
  putStrLn $ "          time/iter:  " ++ showNs nsIter ++ "   (target: <" ++ showNs nsTarget ++ ")"
  putStrLn $ "          alloc/iter: " ++ show allocIter ++ " bytes   (target: ≤" ++ show allocTarget ++ ")"
  pure pass


showMs :: Double -> String
showMs s = show (round (s * 1000) :: Int) ++ " ms"


showNs :: Double -> String
showNs ns
  | ns < 1000 = show (round ns :: Int) ++ " ns"
  | ns < 1e6 = show (round (ns / 1000) :: Int) ++ " µs"
  | otherwise = show (round (ns / 1e6) :: Int) ++ " ms"


-- ---------------------------------------------------------------------------
-- Benchmark implementations
-- ---------------------------------------------------------------------------

parseHTMLIO :: BS.ByteString -> IO HTMLDocument
parseHTMLIO !bs = evaluate $! parseHTML bs
{-# NOINLINE parseHTMLIO #-}


tokenizeIO :: BS.ByteString -> IO Int
tokenizeIO !bs = tokenizeOnlyIO bs
{-# NOINLINE tokenizeIO #-}


treeBuildIO :: BS.ByteString -> IO ()
treeBuildIO !bs = treeBuildOnlyIO bs
{-# NOINLINE treeBuildIO #-}


qsaCountIO :: Selector -> Node -> IO Int
qsaCountIO !sel !root = evaluate $! length (querySelectorAllSel sel root)
{-# NOINLINE qsaCountIO #-}

qsaCountDocIO :: Selector -> Document -> IO Int
qsaCountDocIO !sel !doc = evaluate $! length (querySelectorAllDoc sel doc)
{-# NOINLINE qsaCountDocIO #-}


-- Tokenize incremental: feed 4KB chunks
tokenizeIncrementalIO :: BS.ByteString -> IO Int
tokenizeIncrementalIO !bs = do
  countRef <- newIORef (0 :: Int)
  leftoverRef <- newIORef BS.empty
  let chunks = splitChunks 4096 bs
      processChunk !chunk = do
        prev <- readIORef leftoverRef
        let !combined = if BS.null prev then chunk else prev <> chunk
            !splitPt = findSafeBreak combined
            !toProcess = BS.take splitPt combined
            !remainder = BS.drop splitPt combined
        writeIORef leftoverRef remainder
        when (not (BS.null toProcess)) $ do
          n <- tokenizeCountChunk toProcess
          modifyIORef' countRef (+ n)
  mapM_ processChunk chunks
  lo <- readIORef leftoverRef
  when (not (BS.null lo)) $ do
    n <- tokenizeCountChunk lo
    modifyIORef' countRef (+ n)
  readIORef countRef
{-# NOINLINE tokenizeIncrementalIO #-}


findSafeBreak :: BS.ByteString -> Int
findSafeBreak !bs = go (BS.length bs - 1)
  where
    go !i
      | i < 0 = BS.length bs
      | otherwise = case BS.index bs i of
          0x3C -> i
          0x3E -> BS.length bs
          _ -> go (i - 1)


-- Tree build incremental
treeBuildIncrementalIO :: BS.ByteString -> IO ()
treeBuildIncrementalIO !bs = do
  p <- newParser
  let chunks = splitChunks 4096 bs
  mapM_ (feedParser p) chunks
  doc <- finishParser p
  _ <- evaluate $! documentElement doc
  pure ()
{-# NOINLINE treeBuildIncrementalIO #-}


-- Streaming tree events (one-shot): count events
streamEventsIO :: BS.ByteString -> IO Int
streamEventsIO !bs = do
  arr <- streamHTMLEvents bs
  pure $! sizeofSmallArray arr
{-# NOINLINE streamEventsIO #-}

streamEventsRawIO :: BS.ByteString -> IO Int
streamEventsRawIO !bs = do
  arr <- streamHTMLEventsRaw bs
  pure $! sizeofSmallArray arr
{-# NOINLINE streamEventsRawIO #-}


-- Streaming tree events (incremental, 4KB chunks)
streamEventsIncrementalIO :: BS.ByteString -> IO Int
streamEventsIncrementalIO !bs = do
  sp <- newStreamParser
  let chunks = splitChunks 4096 bs
  acc <- goChunks sp chunks 0
  final <- finishStreamEvents sp
  pure $! acc + sizeofSmallArray final
  where
    goChunks _ [] !acc = pure acc
    goChunks sp (c:cs) !acc = do
      arr <- feedChunkEvents sp c
      goChunks sp cs (acc + sizeofSmallArray arr)
{-# NOINLINE streamEventsIncrementalIO #-}


-- Rewriter passthrough: no handlers
rewriterPassthroughIO :: Rewriter -> BS.ByteString -> IO ()
rewriterPassthroughIO !rw !bs = do
  !result <- rewrite rw bs
  _ <- evaluate $! BS.length result
  pure ()
{-# NOINLINE rewriterPassthroughIO #-}


-- Rewriter with selector matching
rewriterSelectorIO :: Rewriter -> BS.ByteString -> IO ()
rewriterSelectorIO !rw !bs = do
  !result <- rewrite rw bs
  _ <- evaluate $! BS.length result
  pure ()
{-# NOINLINE rewriterSelectorIO #-}


-- Rewriter with mutations
rewriterMutateIO :: Rewriter -> BS.ByteString -> IO ()
rewriterMutateIO !rw !bs = do
  !result <- rewrite rw bs
  _ <- evaluate $! BS.length result
  pure ()
{-# NOINLINE rewriterMutateIO #-}


-- CSS selector parse
selectorParseIO :: Text -> IO ()
selectorParseIO !sel = do
  case parseSelector sel of
    Right !_ -> pure ()
    Left e -> error (show e)
{-# NOINLINE selectorParseIO #-}


-- Flat DOM traversal: counts nodes matching a CompoundSelector without
-- zipper overhead or list allocation.
countMatchesFlat :: CompoundSelector -> HTMLNode -> Int
countMatchesFlat sel = \node -> go node 0
  where
    go (HTMLElement tag attrs children) !acc =
      let !acc' = if matchCompound sel tag attrs then acc + 1 else acc
          !len = sizeofSmallArray children
      in goChildren children 0 len acc'
    go _ !acc = acc
    goChildren :: SmallArray HTMLNode -> Int -> Int -> Int -> Int
    goChildren !arr !i !len !acc
      | i >= len = acc
      | otherwise = goChildren arr (i + 1) len (go (indexSmallArray arr i) acc)
{-# NOINLINE countMatchesFlat #-}


-- CSS selector match
selectorMatchSingleIO :: Selector -> IO ()
selectorMatchSingleIO !(Selector (ComplexSelector (CompoundSelector hd subs) _ : _)) = do
  let !attrs = smallArrayFromList [HTMLAttribute "class" "item", HTMLAttribute "id" "i42"]
      !_ = matchCompound (CompoundSelector hd subs) "div" attrs
  pure ()
selectorMatchSingleIO _ = pure ()
{-# NOINLINE selectorMatchSingleIO #-}


splitChunks :: Int -> BS.ByteString -> [BS.ByteString]
splitChunks chunkSize bs = go 0
  where
    !len = BS.length bs
    go !off
      | off >= len = []
      | otherwise =
          let !end = min (off + chunkSize) len
          in BS.take (end - off) (BS.drop off bs) : go end


-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  rtsEnabled <- getRTSStatsEnabled
  if not rtsEnabled
    then putStrLn "Run with +RTS -T to get allocation stats"
    else runBenchmarks


runBenchmarks :: IO ()
runBenchmarks = do
  let !inputSize = BS.length mediumHTML
  putStrLn $ "Input size: " ++ show inputSize ++ " bytes"
  putStrLn $ replicate 72 '='

  -- Build rewriter configs
  let Right rwPassthrough = buildRewriter (pure ())

  let selDiv = mustParse "div.item"
      selSpan = mustParse "span.name"
      selPrice = mustParse "span.price"
      selDesc = mustParse "p.description"
      selCat = mustParse "span.category"

  let Right rwSelector = buildRewriter $ do
        onElement selDiv $ \er -> do
          _ <- getTagName er
          pure ()
        onElement selSpan $ \er -> do
          _ <- getTagName er
          pure ()
        onElement selPrice $ \er -> do
          _ <- getTagName er
          pure ()
        onText selDesc $ \tr -> do
          _ <- getTextContent tr
          pure ()
        onElement selCat $ \er -> do
          _ <- getElemAttr er "class"
          pure ()

  let Right rwMutate = buildRewriter $ do
        onElement selDiv $ \er -> do
          setElemAttr er "data-processed" "true"
        onElement selSpan $ \er -> do
          setTagName er "strong"
        onText selDesc $ \tr -> do
          content <- getTextContent tr
          replaceTextChunk tr ("[" <> content <> "]") AsText

  let doc = parseDocument mediumHTML

  let n = 5000
  passRef <- newIORef (0 :: Int)
  totalRef <- newIORef (0 :: Int)

  let checkPass ok = do
        modifyIORef' totalRef (+ 1)
        if ok then modifyIORef' passRef (+ 1) else pure ()

  -- ==================== Tokenizer ====================
  putStrLn ""
  putStrLn "--- Tokenizer ---"
  -- Throughput targets derived from lol-html (Rust) on same ~25KB input,
  -- same machine. Targets = 70% of lol-html's measured throughput.
  --   lol-html tag scanner (no handlers):  886 MiB/s  → 70% = 620 MB/s
  --   lol-html lexer (full parse):         436 MiB/s  → 70% = 305 MB/s
  --   lol-html match-all (*) noop:         228 MiB/s  → 70% = 160 MB/s
  --   lol-html multiple selectors (7):     181 MiB/s  → 70% = 127 MB/s
  --   lol-html class selector (.note):     204 MiB/s  → 70% = 143 MB/s
  --   lol-html body rename + after:        541 MiB/s  → 70% = 379 MB/s

  r1 <- bench "tokenize (one-shot)" n inputSize (tokenizeIO mediumHTML)
  printResult r1 900 69000 >>= checkPass

  r2 <- bench "tokenize (incremental, 4KB)" n inputSize (tokenizeIncrementalIO mediumHTML)
  printResult r2 800 50000 >>= checkPass

  -- ==================== Tree Builder ====================
  putStrLn ""
  putStrLn "--- Tree Builder ---"
  r3 <- bench "tree-build (one-shot)" n inputSize (treeBuildIO mediumHTML)
  printResult r3 305 355000 >>= checkPass

  r4 <- bench "tree-build (incremental)" n inputSize (treeBuildIncrementalIO mediumHTML)
  printResult r4 135 880000 >>= checkPass

  -- ==================== Streaming Events ====================
  putStrLn ""
  putStrLn "--- Streaming Tree Events ---"
  r4a <- bench "stream events (one-shot)" n inputSize (streamEventsIO mediumHTML)
  printResult r4a 300 335000 >>= checkPass

  r4b <- bench "stream events (incremental, 4KB)" n inputSize (streamEventsIncrementalIO mediumHTML)
  printResult r4b 300 305000 >>= checkPass

  r4c <- bench "stream events raw (one-shot)" n inputSize (streamEventsRawIO mediumHTML)
  printResult r4c 510 240000 >>= checkPass

  -- ==================== Rewriter ====================
  -- Throughput targets: 70% of lol-html on comparable workloads.
  putStrLn ""
  putStrLn "--- Rewriter ---"
  r5 <- bench "rewriter (passthrough)" n inputSize (rewriterPassthroughIO rwPassthrough mediumHTML)
  printResult r5 0 300 >>= checkPass

  r6 <- bench "rewriter (selector matching)" n inputSize (rewriterSelectorIO rwSelector mediumHTML)
  printResult r6 190 120000 >>= checkPass

  r7 <- bench "rewriter (with mutations)" n inputSize (rewriterMutateIO rwMutate mediumHTML)
  printResult r7 180 200000 >>= checkPass

  -- lol-html-comparable: single body rename + after insertion (1 match in ~600 tags).
  -- lol-html does 541 MiB/s via dual-parser (tag scanner handles 99.8% of content).
  -- Our single-pass arch processes every byte equally — compare against lol-html's
  -- full-lexer match-all (*) at 228 MiB/s instead. 70% of 228 = 160 MB/s.
  let selBody = mustParse "body"
  let Right rwSparseBody = buildRewriter $ do
        onElement selBody $ \er -> do
          setTagName er "body1"
          afterElement er "test" AsText
  r7s <- bench "rewriter (sparse mutation)" n inputSize (rewriterMutateIO rwSparseBody mediumHTML)
  printResult r7s 380 82000 >>= checkPass

  -- Mutation sub-benchmarks
  let Right rwMutTagOnly = buildRewriter $ do
        onElement selSpan $ \er -> setTagName er "strong"
  let Right rwMutAttrOnly = buildRewriter $ do
        onElement selDiv $ \er -> setElemAttr er "data-processed" "true"
  let Right rwMutTextOnly = buildRewriter $ do
        onText selDesc $ \tr -> do
          content <- getTextContent tr
          replaceTextChunk tr ("[" <> content <> "]") AsText
  let Right rwMutTextIdentity = buildRewriter $ do
        onText selDesc $ \tr -> do
          content <- getTextContent tr
          replaceTextChunk tr content AsText
  let Right rwMutElemOnly = buildRewriter $ do
        onElement selDiv $ \er -> setElemAttr er "data-processed" "true"
        onElement selSpan $ \er -> setTagName er "strong"

  let Right rwScanOnly = buildRewriter $ do
        onElement (mustParse "nonexistent-tag-xyz") $ \_ -> pure ()
  let Right rwOneHandler = buildRewriter $ do
        onElement selDiv $ \_ -> pure ()
  let Right rwUniversal = buildRewriter $ do
        onElement (mustParse "*") $ \_ -> pure ()
  let Right rwSpanOnly = buildRewriter $ do
        onElement selSpan $ \_ -> pure ()
  let Right rw2Elem = buildRewriter $ do
        onElement selDiv $ \_ -> pure ()
        onElement selSpan $ \_ -> pure ()
  let Right rw3Elem = buildRewriter $ do
        onElement selDiv $ \_ -> pure ()
        onElement selSpan $ \_ -> pure ()
        onElement selPrice $ \_ -> pure ()
  let Right rw4Elem = buildRewriter $ do
        onElement selDiv $ \_ -> pure ()
        onElement selSpan $ \_ -> pure ()
        onElement selPrice $ \_ -> pure ()
        onElement selCat $ \_ -> pure ()
  let Right rwTextOnly = buildRewriter $ do
        onText selDesc $ \_ -> pure ()
  let Right rwMutateNoop = buildRewriter $ do
        onElement selDiv $ \_ -> pure ()
        onElement selSpan $ \_ -> pure ()
        onText selDesc $ \_ -> pure ()
  r6s <- bench "diag: scan only (no tag match)" n inputSize (rewriterSelectorIO rwScanOnly mediumHTML)
  printResult r6s 0 48000 >>= checkPass
  r6a <- bench "diag: 1 handler (div.item)" n inputSize (rewriterSelectorIO rwOneHandler mediumHTML)
  printResult r6a 0 56000 >>= checkPass
  r6a1s <- bench "diag: 1 handler (span.name)" n inputSize (rewriterSelectorIO rwSpanOnly mediumHTML)
  printResult r6a1s 0 84000 >>= checkPass
  r6a2 <- bench "diag: 2 elem handlers" n inputSize (rewriterSelectorIO rw2Elem mediumHTML)
  printResult r6a2 0 94000 >>= checkPass
  r6a3 <- bench "diag: 3 elem handlers" n inputSize (rewriterSelectorIO rw3Elem mediumHTML)
  printResult r6a3 0 102000 >>= checkPass
  r6b <- bench "diag: universal (*) handler" n inputSize (rewriterSelectorIO rwUniversal mediumHTML)
  printResult r6b 0 88000 >>= checkPass
  r6c <- bench "diag: 4 elem handlers (no text)" n inputSize (rewriterSelectorIO rw4Elem mediumHTML)
  printResult r6c 0 105000 >>= checkPass
  r6d <- bench "diag: 1 text handler only" n inputSize (rewriterSelectorIO rwTextOnly mediumHTML)
  printResult r6d 0 60000 >>= checkPass

  -- Span no-op: same tag filter as setTagName but no mutation
  let Right rwSpanNoop = buildRewriter $ do
        onElement selSpan $ \_ -> pure ()
  r7noop <- bench "diag: span.name no-op handler" n inputSize (rewriterSelectorIO rwSpanNoop mediumHTML)
  printResult r7noop 0 84000 >>= checkPass

  r7a <- bench "diag: mut setTagName only" n inputSize (rewriterMutateIO rwMutTagOnly mediumHTML)
  printResult r7a 0 135000 >>= checkPass
  r7b <- bench "diag: mut setElemAttr only" n inputSize (rewriterMutateIO rwMutAttrOnly mediumHTML)
  printResult r7b 0 115000 >>= checkPass
  r7c <- bench "diag: mut replaceText only" n inputSize (rewriterMutateIO rwMutTextOnly mediumHTML)
  printResult r7c 0 125000 >>= checkPass
  r7ci <- bench "diag: mut text identity" n inputSize (rewriterMutateIO rwMutTextIdentity mediumHTML)
  printResult r7ci 0 105000 >>= checkPass
  r7e <- bench "diag: mut elem only (tag+attr)" n inputSize (rewriterMutateIO rwMutElemOnly mediumHTML)
  printResult r7e 0 165000 >>= checkPass

  -- Read-only handler: getElemAttr forces attrs but doesn't mutate
  let Right rwReadAttr = buildRewriter $ do
        onElement selDiv $ \er -> do
          _ <- getElemAttr er "class"
          pure ()
  r7ro <- bench "diag: getElemAttr (read only)" n inputSize (rewriterSelectorIO rwReadAttr mediumHTML)
  printResult r7ro 0 65000 >>= checkPass
  r7mn <- bench "diag: mutate selectors, noop" n inputSize (rewriterSelectorIO rwMutateNoop mediumHTML)
  printResult r7mn 0 108000 >>= checkPass

  -- ==================== CSS Selectors ====================
  putStrLn ""
  putStrLn "--- CSS Selectors ---"
  let selectorN = 200000
  (selAlloc, selNs) <- benchSmall "selector-parse" selectorN (selectorParseIO "div.item > span.name[href^=\"https\"]")
  printSmallResult "selector parse" selAlloc selNs 3200 1000.0 >>= checkPass

  let !rawRoot = htmlRoot (documentHTML doc)
      Right (Selector [ComplexSelector compound []]) = parseSelector "div.item"
  let matchN = 200000
  (matchAlloc, matchNs) <- benchSmall "selector-match" matchN $
    evaluate $! countMatchesFlat compound rawRoot
  printSmallResult "selector match (flat traversal)" matchAlloc matchNs 20 100.0 >>= checkPass

  -- ==================== DOM querySelector ====================
  -- Reference: JSDOM on same 29KB document (M-series Mac):
  --   div             : 150 µs    div.item        : 18 µs
  --   div.item span   : 34 µs     div:first-child : 26 µs
  --   :nth-child(2n+1): 33 µs     :not(.item)     : 28 µs
  --   [id]            : 170 µs    child + sibling : 44 µs
  -- Target: match or exceed estimated Blink speeds (high estimate).
  -- Blink estimates (JSDOM/5–10): div 15-30µs, div.item 1.8-3.6µs,
  -- descendant 3.4-6.8µs, structural 2.6-6.6µs, sibling 4.4-8.8µs
  putStrLn ""
  putStrLn "--- DOM querySelector (indexed) ---"
  let !domDoc = parseDocument mediumHTML

  let qsN = 50000
      qsParse :: Text -> Selector
      qsParse s = case parseSelector s of Right r -> r; Left e -> error (show e)
      !selDiv2 = qsParse "div"
      !selDivItem = qsParse "div.item"
      !selDescSpan = qsParse "div.item span.name"
      !selFirstChild = qsParse "div:first-child"
      !selNthChild = qsParse "div.item:nth-child(2n+1)"
      !selNotItem = qsParse "div:not(.item)"
      !selHasId = qsParse "[id]"
      !selChildSib = qsParse "div.catalog > div + div"

  (qs1a, qs1ns) <- benchSmall "qsa(div)" qsN $
    qsaCountDocIO selDiv2 domDoc
  printSmallResult "querySelectorAll(\"div\")" qs1a qs1ns 10500 1200.0 >>= checkPass

  (qs2a, qs2ns) <- benchSmall "qsa(.item)" qsN $
    qsaCountDocIO selDivItem domDoc
  printSmallResult "querySelectorAll(\"div.item\")" qs2a qs2ns 11000 1500.0 >>= checkPass

  (qs3a, qs3ns) <- benchSmall "qsa(div.item span.name)" qsN $
    qsaCountDocIO selDescSpan domDoc
  printSmallResult "querySelectorAll(\"div.item span.name\")" qs3a qs3ns 22000 3500.0 >>= checkPass

  (qs4a, qs4ns) <- benchSmall "qsa(div:first-child)" qsN $
    qsaCountDocIO selFirstChild domDoc
  printSmallResult "querySelectorAll(\"div:first-child\")" qs4a qs4ns 3800 1500.0 >>= checkPass

  (qs5a, qs5ns) <- benchSmall "qsa(:nth-child(2n+1))" qsN $
    qsaCountDocIO selNthChild domDoc
  printSmallResult "querySelectorAll(\":nth-child(2n+1)\")" qs5a qs5ns 9500 3000.0 >>= checkPass

  (qs6a, qs6ns) <- benchSmall "qsa(:not(.item))" qsN $
    qsaCountDocIO selNotItem domDoc
  printSmallResult "querySelectorAll(\":not(.item)\")" qs6a qs6ns 8000 5500.0 >>= checkPass

  (qs7a, qs7ns) <- benchSmall "qsa([id])" qsN $
    qsaCountDocIO selHasId domDoc
  printSmallResult "querySelectorAll(\"[id]\")" qs7a qs7ns 30000 10000.0 >>= checkPass

  (qs8a, qs8ns) <- benchSmall "qsa(div.catalog > div + div)" qsN $
    qsaCountDocIO selChildSib domDoc
  printSmallResult "querySelectorAll(\"div.catalog > div + div\")" qs8a qs8ns 22000 8000.0 >>= checkPass

  -- ==================== Micro: PrimArray vs IORef ====================
  putStrLn ""
  putStrLn "--- Micro-benchmarks ---"
  do pa <- newPrimArray 3
     writePrimArray pa (0 :: Int) (0 :: Int)
     writePrimArray pa 1 (-1 :: Int)
     writePrimArray pa 2 (-1 :: Int)
     let microN = 100000
     (paAlloc, paNs) <- benchSmall "PrimArray read+write depth" microN $ do
       d <- readPrimArray pa 0
       writePrimArray pa (0 :: Int) (d + 1 :: Int)
     printSmallResult "PrimArray read+write" paAlloc paNs 0 1000.0 >>= checkPass

  do ref <- newIORef (0 :: Int)
     let microN = 100000
     (ioAlloc, ioNs) <- benchSmall "IORef read+write depth" microN $ do
       d <- readIORef ref
       writeIORef ref $! (d + 1 :: Int)
     printSmallResult "IORef strict read+write" ioAlloc ioNs 16 1000.0 >>= checkPass

  do let sampleText = "This is the description for product number 42 in our catalog" :: T.Text
         microN = 100000
     ref <- newIORef sampleText
     (tuAlloc, tuNs) <- benchSmall "T.toUpper 60-char ASCII" microN $ do
       t <- readIORef ref
       let !u = T.toUpper t
       writeIORef ref u
     printSmallResult "T.toUpper 60-char" tuAlloc tuNs 1200 1000.0 >>= checkPass

  -- ==================== Summary ====================
  putStrLn ""
  putStrLn $ replicate 72 '='
  passed <- readIORef passRef
  total <- readIORef totalRef
  putStrLn $ show passed ++ "/" ++ show total ++ " benchmarks within target"


mustParse :: Text -> Selector
mustParse t = case parseSelector t of
  Right s -> s
  Left e -> error ("parseSelector: " ++ show e)
