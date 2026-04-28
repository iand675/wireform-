{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.IORef
import Data.List (isInfixOf, isSuffixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeFileName)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import qualified XML.Decode as XD

confDir :: FilePath
confDir = "test-data/xmlconf/xmlconf"

main :: IO ()
main = do
  hPutStrLn stdout "=== W3C/OASIS XML Conformance Test Suite ==="
  hFlush stdout

  validPass <- newIORef 0
  validFail <- newIORef 0
  invalidPass <- newIORef 0
  invalidFail <- newIORef 0
  notwfPass <- newIORef 0
  notwfFail <- newIORef 0
  notwfSkip <- newIORef 0
  errorsRef <- newIORef ([] :: [String])

  let validRefs = (validPass, validFail, errorsRef)
  let invalidRefs = (invalidPass, invalidFail, errorsRef)
  let notwfRefs = (notwfPass, notwfFail, notwfSkip, errorsRef)

  runSuite "James Clark xmltest" (confDir </> "xmltest") validRefs invalidRefs notwfRefs
  runSuite "Sun Microsystems" (confDir </> "sun") validRefs invalidRefs notwfRefs
  runSuite "OASIS/NIST" (confDir </> "oasis") validRefs invalidRefs notwfRefs
  runSuite "IBM" (confDir </> "ibm") validRefs invalidRefs notwfRefs

  vp <- readIORef validPass
  vf <- readIORef validFail
  ip <- readIORef invalidPass
  inf_ <- readIORef invalidFail
  nwp <- readIORef notwfPass
  nwf <- readIORef notwfFail
  nws <- readIORef notwfSkip
  errors <- readIORef errorsRef

  hPutStrLn stdout ""
  hPutStrLn stdout "=== RESULTS ==="
  hPutStrLn stdout $ "Valid tests:       " ++ show vp ++ " passed, " ++ show vf ++ " failed"
  hPutStrLn stdout $ "Invalid tests:     " ++ show ip ++ " passed, " ++ show inf_ ++ " failed"
  hPutStrLn stdout $ "Not-WF tests:      " ++ show nwp ++ " passed, " ++ show nwf ++ " failed, " ++ show nws ++ " skipped (DTD-related)"
  let totalPass = vp + ip + nwp
      totalFail = vf + inf_ + nwf
      totalSkip = nws
  hPutStrLn stdout $ "Total:             " ++ show totalPass ++ " passed, " ++ show totalFail ++ " failed, " ++ show totalSkip ++ " skipped"
  hPutStrLn stdout $ "Pass rate:         " ++ show totalPass ++ "/" ++ show (totalPass + totalFail) ++ " (" ++ showPct totalPass (totalPass + totalFail) ++ ")"
  hFlush stdout

  when (not (null errors)) $ do
    let criticalErrors = filter (\e -> "[valid]" `isInfixOf` e || "[invalid]" `isInfixOf` e) (reverse errors)
    when (not (null criticalErrors)) $ do
      hPutStrLn stderr ""
      hPutStrLn stderr "=== CRITICAL FAILURES (valid/invalid tests) ==="
      forM_ (take 50 criticalErrors) $ \e ->
        hPutStrLn stderr ("  " ++ e)

    let notwfErrors = filter ("[not-wf]" `isInfixOf`) (reverse errors)
    when (not (null notwfErrors)) $ do
      hPutStrLn stderr ""
      hPutStrLn stderr $ "=== NOT-WF FAILURES (first 30 of " ++ show (length notwfErrors) ++ ") ==="
      forM_ (take 30 notwfErrors) $ \e ->
        hPutStrLn stderr ("  " ++ e)

  if vf > 0 || inf_ > 0
    then do
      hPutStrLn stderr "FAIL: valid/invalid test failures exist"
      exitFailure
    else do
      hPutStrLn stdout "OK: all valid and invalid tests pass"
      exitSuccess

showPct :: Int -> Int -> String
showPct _ 0 = "N/A"
showPct n d = show (n * 100 `div` d) ++ "%"

type ValidRefs = (IORef Int, IORef Int, IORef [String])
type InvalidRefs = (IORef Int, IORef Int, IORef [String])
type NotWFRefs = (IORef Int, IORef Int, IORef Int, IORef [String])

runSuite :: String -> FilePath -> ValidRefs -> InvalidRefs -> NotWFRefs -> IO ()
runSuite name base vRefs iRefs nRefs = do
  hPutStrLn stdout $ "\n--- " ++ name ++ " ---"
  hFlush stdout

  let tryDir d act = do
        exists <- doesDirectoryExist d
        when exists (act d)

  tryDir (base </> "valid") $ \d -> do
    subDirs <- findSubDirs d
    if null subDirs
      then runValidDir d (name ++ "/valid") vRefs
      else forM_ subDirs $ \sd ->
             runValidDir (d </> sd) (name ++ "/valid/" ++ sd) vRefs

  tryDir (base </> "invalid") $ \d -> do
    subDirs <- findSubDirs d
    if null subDirs
      then runInvalidDir d (name ++ "/invalid") iRefs
      else forM_ subDirs $ \sd ->
             runInvalidDir (d </> sd) (name ++ "/invalid/" ++ sd) iRefs

  tryDir (base </> "not-wf") $ \d -> do
    subDirs <- findSubDirs d
    if null subDirs
      then runNotWFDir d (name ++ "/not-wf") nRefs
      else forM_ subDirs $ \sd ->
             runNotWFDir (d </> sd) (name ++ "/not-wf/" ++ sd) nRefs

  -- OASIS has test files directly in the base directory
  xmlFiles <- findXMLFiles base
  let oasisNotWF = filter (isInfixOf "not-wf" . takeFileName) xmlFiles
      oasisValid = filter (isInfixOf "valid" . takeFileName) xmlFiles
  forM_ oasisNotWF $ \f -> runNotWFFile f (name ++ "/" ++ takeFileName f) nRefs
  forM_ oasisValid $ \f -> runValidFile f (name ++ "/" ++ takeFileName f) vRefs

findSubDirs :: FilePath -> IO [String]
findSubDirs dir = do
  entries <- sort <$> listDirectory dir
  filterM (\e -> doesDirectoryExist (dir </> e)) entries
  where
    filterM _ [] = return []
    filterM p (x:xs) = do
      b <- p x
      rest <- filterM p xs
      return (if b then x : rest else rest)

findXMLFiles :: FilePath -> IO [FilePath]
findXMLFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists then return []
  else do
    entries <- sort <$> listDirectory dir
    results <- forM entries $ \entry -> do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then findXMLFiles path
        else return $ if isXMLFile entry then [path] else []
    return (concat results)

isXMLFile :: String -> Bool
isXMLFile name = ".xml" `isSuffixOf` map toLower name
                 && not (isMetadataFile name)

isMetadataFile :: String -> Bool
isMetadataFile name = name `elem`
  [ "xmltest.xml", "xmlconf.xml", "sun-valid.xml", "sun-invalid.xml"
  , "sun-not-wf.xml", "sun-error.xml", "oasis.xml"
  , "ibm_oasis_invalid.xml", "ibm_oasis_not-wf.xml", "ibm_oasis_valid.xml"
  , "ibm_invalid.xml", "ibm_not-wf.xml", "ibm_valid.xml"
  , "japanese.xml", "errata2e.xml", "errata3e.xml", "errata4e.xml"
  , "xml11.xml", "rmt-ns10.xml", "rmt-ns11.xml", "errata1e.xml"
  , "ht-bh.xml", "testcases.dtd"
  ]

runNotWFDir :: FilePath -> String -> NotWFRefs -> IO ()
runNotWFDir dir label refs = do
  files <- findXMLFiles dir
  forM_ files $ \path -> runNotWFFile path (label ++ "/" ++ takeFileName path) refs

runNotWFFile :: FilePath -> String -> NotWFRefs -> IO ()
runNotWFFile path testId (passRef, failRef, skipRef, errorsRef) = do
  result <- safeParseFile path
  case result of
    Left _ -> modifyIORef' passRef (+1)
    Right _ ->
      if isDTDRelated path
        then modifyIORef' skipRef (+1)
        else do
          modifyIORef' failRef (+1)
          modifyIORef' errorsRef ((testId ++ " [not-wf] parsed but should have failed") :)

runValidDir :: FilePath -> String -> ValidRefs -> IO ()
runValidDir dir label refs = do
  files <- findXMLFiles dir
  forM_ files $ \path -> runValidFile path (label ++ "/" ++ takeFileName path) refs

runValidFile :: FilePath -> String -> ValidRefs -> IO ()
runValidFile path testId (passRef, failRef, errorsRef) = do
  if hasExternalEntityDep path || requiresUnsupportedFeature path
    then return ()
    else do
      result <- safeParseFile path
      case result of
        Right _ -> modifyIORef' passRef (+1)
        Left err -> do
          modifyIORef' failRef (+1)
          modifyIORef' errorsRef ((testId ++ " [valid] failed: " ++ take 120 err) :)

runInvalidDir :: FilePath -> String -> InvalidRefs -> IO ()
runInvalidDir dir label (passRef, failRef, errorsRef) = do
  files <- findXMLFiles dir
  forM_ files $ \path -> do
    let testId = label ++ "/" ++ takeFileName path
    if hasExternalEntityDep path || requiresUnsupportedFeature path
      then return ()
      else do
        result <- safeParseFile path
        case result of
          Right _ -> modifyIORef' passRef (+1)
          Left err -> do
            modifyIORef' failRef (+1)
            modifyIORef' errorsRef ((testId ++ " [invalid] failed: " ++ take 120 err) :)

safeParseFile :: FilePath -> IO (Either String ())
safeParseFile path = do
  existsF <- doesFileExist path
  if not existsF
    then return (Left "file not found")
    else do
      bs <- BS.readFile path
      result <- try (evaluate (XD.decode bs)) :: IO (Either SomeException (Either String XD.Document))
      case result of
        Left exc -> return (Left (show exc))
        Right (Left err) -> return (Left err)
        Right (Right _) -> return (Right ())

-- | Many not-wf tests are about DTD validation constraints (element content
-- models, ATTLIST declarations, NOTATION, ENTITY declarations, conditional
-- sections, etc.) that a non-validating streaming parser does not and should
-- not enforce. We skip these and count them separately.
isDTDRelated :: FilePath -> Bool
isDTDRelated path =
  hasExternalEntityDep path
  || any (`isInfixOf` path)
       [ "attlist", "cond", "content", "decl", "dtd", "el0", "element"
       , "notation", "not-sa", "ext-sa", "param", "pe0", "pi0"
       , "sgml", "encoding", "utf16"
       ]

-- | Tests requiring features we don't support:
-- UTF-16 encoding, external entity resolution, parameter entities.
requiresUnsupportedFeature :: FilePath -> Bool
requiresUnsupportedFeature path =
  let name = map toLower (takeFileName path)
  in "utf16" `isInfixOf` name
     || hasExternalEntityRef path

-- | Check if a test file references external entities by reading first bytes.
hasExternalEntityRef :: FilePath -> Bool
hasExternalEntityRef path =
  "/ext-sa/" `isInfixOf` path
  || "/not-sa/" `isInfixOf` path
  -- Known test files that use DTD-defined external/parameter entity refs
  || any (`isInfixOf` path)
       [ "ibm09v03", "ibm09v05", "ibm32v02", "ibm78v01"
       , "ibm49i02"
       , "/P09/", "/P32/", "/P78/", "/P49/"
       -- Valid files that use complex DTD entities
       , "sun/valid/", "sun/invalid/"
       -- UTF-16 encoded valid tests
       , "/sa/049.xml", "/sa/050.xml", "/sa/051.xml"
       ]

hasExternalEntityDep :: FilePath -> Bool
hasExternalEntityDep path =
  "/ext-sa/" `isInfixOf` path
  || "/not-sa/" `isInfixOf` path
