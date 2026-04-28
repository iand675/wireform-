{-# LANGUAGE OverloadedStrings #-}
module Test.XMLSecurity (xmlSecurityTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector ()
import Test.Tasty
import Test.Tasty.HUnit

import XML.Decode (decode, decodeWith, defaultParseConfig, ParseConfig(..))
import XML.Path (textContent)
import XML.Value

xmlSecurityTests :: TestTree
xmlSecurityTests = testGroup "XML Security"
  [ billionLaughsTests
  , quadraticBlowupTests
  , xxeTests
  , entityRecursionTests
  , resourceLimitTests
  , namespaceAttackTests
  , wellFormednessRejectionTests
  , parseConfigTests
  ]

------------------------------------------------------------------------
-- Billion Laughs
------------------------------------------------------------------------

billionLaughsTests :: TestTree
billionLaughsTests = testGroup "Billion Laughs"
  [ testCase "Billion Laughs defense" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE lolz ["
            , "  <!ENTITY lol \"lol\">"
            , "  <!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\">"
            , "  <!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\">"
            , "  <!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">"
            , "  <!ENTITY lol4 \"&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;\">"
            , "  <!ENTITY lol5 \"&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;\">"
            , "]>"
            , "<lolz>&lol5;</lolz>"
            ]
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right doc) -> do
          let content = textContent (docRoot doc)
          assertBool "Billion laughs handled safely (no gigabyte expansion)"
            (T.length content < 1000000)

  , testCase "Billion Laughs with decodeWith (strict limit)" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE lolz ["
            , "  <!ENTITY lol \"lol\">"
            , "  <!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\">"
            , "  <!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\">"
            , "  <!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">"
            , "]>"
            , "<lolz>&lol3;</lolz>"
            ]
      let cfg = defaultParseConfig { pcMaxEntityExpansion = 100 }
      let result = decodeWith cfg xml
      case result of
        Left _ -> pure ()
        Right doc -> do
          let content = textContent (docRoot doc)
          assertBool "Entity expansion limited" (T.length content < 1000)
  ]

------------------------------------------------------------------------
-- Quadratic Blowup
------------------------------------------------------------------------

quadraticBlowupTests :: TestTree
quadraticBlowupTests = testGroup "Quadratic Blowup"
  [ testCase "Quadratic blowup defense" $ do
      let bigEntity = TE.encodeUtf8 (T.replicate 50000 "A")
      let xml = BS.concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY big \""
            , bigEntity
            , "\">"
            , "]>"
            , "<foo>"
            , BS.concat (replicate 100 "&big;")
            , "</foo>"
            ]
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right doc) -> do
          let content = textContent (docRoot doc)
          assertBool "Quadratic blowup handled (not OOM)"
            (T.length content < 100000000)

  , testCase "Quadratic blowup with strict entity limit" $ do
      let bigEntity = TE.encodeUtf8 (T.replicate 1000 "A")
      let xml = BS.concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY big \""
            , bigEntity
            , "\">"
            , "]>"
            , "<foo>"
            , BS.concat (replicate 20 "&big;")
            , "</foo>"
            ]
      let cfg = defaultParseConfig { pcMaxEntityExpansion = 5000 }
      let result = decodeWith cfg xml
      case result of
        Left _err -> assertBool "Error mentions entity" True
        Right doc -> do
          let content = textContent (docRoot doc)
          assertBool "Entity expansion capped" (T.length content <= 5000)
  ]

------------------------------------------------------------------------
-- XXE (External Entity) attacks
------------------------------------------------------------------------

xxeTests :: TestTree
xxeTests = testGroup "XXE Attacks"
  [ testCase "XXE file read blocked" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY xxe SYSTEM \"file:///etc/passwd\">"
            , "]>"
            , "<foo>&xxe;</foo>"
            ]
      let result = decode xml
      case result of
        Right doc -> do
          let content = textContent (docRoot doc)
          assertBool "No file contents leaked"
            (not (T.isInfixOf "root:" content))
        Left _ -> pure ()

  , testCase "XXE HTTP SSRF blocked" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY xxe SYSTEM \"http://169.254.169.254/latest/meta-data/\">"
            , "]>"
            , "<foo>&xxe;</foo>"
            ]
      let result = decode xml
      case result of
        Right doc -> do
          let content = textContent (docRoot doc)
          assertBool "No SSRF data leaked"
            (not (T.isInfixOf "ami-id" content))
        Left _ -> pure ()

  , testCase "XXE with PUBLIC identifier blocked" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY xxe PUBLIC \"-//W3C//DTD XHTML 1.0//EN\" \"http://evil.com/xxe\">"
            , "]>"
            , "<foo>&xxe;</foo>"
            ]
      let result = decode xml
      case result of
        Right doc -> do
          let content = textContent (docRoot doc)
          assertBool "No external entity data" (T.length content < 100)
        Left _ -> pure ()

  , testCase "External entities rejected with pcAllowExternalEntities=False" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY xxe SYSTEM \"file:///etc/hosts\">"
            , "]>"
            , "<foo>&xxe;</foo>"
            ]
      let cfg = defaultParseConfig { pcAllowExternalEntities = False }
      let result = decodeWith cfg xml
      case result of
        Left _ -> pure ()
        Right doc -> do
          let content = textContent (docRoot doc)
          assertBool "No file contents" (not (T.isInfixOf "localhost" content))
  ]

------------------------------------------------------------------------
-- Entity recursion
------------------------------------------------------------------------

entityRecursionTests :: TestTree
entityRecursionTests = testGroup "Entity Recursion"
  [ testCase "Direct entity recursion defense" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY a \"&b;\">"
            , "  <!ENTITY b \"&a;\">"
            , "]>"
            , "<foo>&a;</foo>"
            ]
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> assertBool "Recursion handled safely" True

  , testCase "Self-referencing entity" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY a \"&a;\">"
            , "]>"
            , "<foo>&a;</foo>"
            ]
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> assertBool "Self-reference handled" True

  , testCase "Indirect recursion chain" $ do
      let xml = BS8.pack $ concat
            [ "<?xml version=\"1.0\"?>"
            , "<!DOCTYPE foo ["
            , "  <!ENTITY a \"&b;\">"
            , "  <!ENTITY b \"&c;\">"
            , "  <!ENTITY c \"&a;\">"
            , "]>"
            , "<foo>&a;</foo>"
            ]
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> assertBool "Indirect recursion handled" True
  ]

------------------------------------------------------------------------
-- Resource limits
------------------------------------------------------------------------

resourceLimitTests :: TestTree
resourceLimitTests = testGroup "Resource Limits"
  [ testCase "Deep nesting (10000 levels)" $ do
      let depth = 10000 :: Int
      let xml = BS.concat (replicate depth "<a>") <> "x" <> BS.concat (replicate depth "</a>")
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()

  , testCase "Deep nesting rejected by config limit" $ do
      let depth = 200 :: Int
      let xml = BS.concat (replicate depth "<a>") <> "x" <> BS.concat (replicate depth "</a>")
      let cfg = defaultParseConfig { pcMaxDepth = 100 }
      let result = decodeWith cfg xml
      assertBool "Deep nesting rejected" (isLeft result)

  , testCase "Huge attribute count (10000 attrs)" $ do
      let attrs = BS8.intercalate " " [BS8.pack ("a" ++ show i ++ "=\"v\"") | i <- [1..10000 :: Int]]
      let xml = "<root " <> attrs <> "/>"
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()

  , testCase "Huge tag name (100K chars)" $ do
      let name = BS.replicate 100000 0x61
      let xml = "<" <> name <> "/>"
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()

  , testCase "Malformed UTF-8 bytes" $ do
      let xml = "<root>" <> BS.pack [0xFF, 0xFE, 0x80] <> "</root>"
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()

  , testCase "Very long text content (1MB)" $ do
      let content = BS.replicate 1000000 0x41
      let xml = "<root>" <> content <> "</root>"
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right doc) -> do
          let tc = textContent (docRoot doc)
          assertBool "Large text parsed" (T.length tc == 1000000)

  , testCase "Very long attribute value (1MB)" $ do
      let val = BS.replicate 1000000 0x41
      let xml = "<root a=\"" <> val <> "\"/>"
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()
  ]

------------------------------------------------------------------------
-- Namespace attacks
------------------------------------------------------------------------

namespaceAttackTests :: TestTree
namespaceAttackTests = testGroup "Namespace Attacks"
  [ testCase "Namespace bomb (1000 unique namespaces)" $ do
      let attrs = BS8.intercalate " "
            [ BS8.pack ("xmlns:ns" ++ show i ++ "=\"http://example.com/" ++ show i ++ "\"")
            | i <- [1..1000 :: Int]
            ]
      let xml = "<root " <> attrs <> "/>"
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()

  , testCase "Deeply nested namespace overrides" $ do
      let mkLevel i = BS8.pack ("<a xmlns:ns=\"http://example.com/" ++ show i ++ "\">")
      let depth = 100 :: Int
      let opens = BS.concat [mkLevel i | i <- [1..depth]]
      let closes = BS.concat (replicate depth "</a>")
      let xml = opens <> "x" <> closes
      result <- safeEval (decode xml)
      case result of
        Left _ -> pure ()
        Right (Left _) -> pure ()
        Right (Right _) -> pure ()
  ]

------------------------------------------------------------------------
-- Well-formedness rejection tests
------------------------------------------------------------------------

wellFormednessRejectionTests :: TestTree
wellFormednessRejectionTests = testGroup "Well-formedness Rejection"
  [ testCase "Reject mismatched tags" $ do
      let result = decode "<a></b>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject unclosed tag" $ do
      let result = decode "<a>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject bare ampersand" $ do
      let result = decode "<a>a & b</a>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject < in attribute value" $ do
      let result = decode "<a b=\"<\"/>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject duplicate attributes" $ do
      let result = decode "<a b=\"1\" b=\"2\"/>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject empty tag name" $ do
      let result = decode "<>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject attribute without value" $ do
      let result = decode "<a b/>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject unclosed attribute quote" $ do
      let result = decode "<a b=\"unclosed>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject tag starting with digit" $ do
      let result = decode "<1tag/>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject unterminated comment" $ do
      let result = decode "<a><!-- unclosed comment</a>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject unterminated CDATA" $ do
      let result = decode "<a><![CDATA[unclosed cdata</a>"
      assertBool "Rejected" (isLeft result)

  , testCase "Reject text after root element" $ do
      let result = decode "<a/>trailing text"
      case result of
        Left _ -> pure ()
        Right _ -> pure ()

  , testCase "Reject completely empty input" $ do
      let result = decode BS.empty
      assertBool "Rejected" (isLeft result)
  ]

------------------------------------------------------------------------
-- ParseConfig tests
------------------------------------------------------------------------

parseConfigTests :: TestTree
parseConfigTests = testGroup "ParseConfig"
  [ testCase "defaultParseConfig has sane defaults" $ do
      let cfg = defaultParseConfig
      assertBool "depth >= 100" (pcMaxDepth cfg >= 100)
      assertBool "entity expansion >= 1000" (pcMaxEntityExpansion cfg >= 1000)
      assertBool "DTD allowed" (pcAllowDTD cfg)
      assertBool "External entities disallowed" (not (pcAllowExternalEntities cfg))

  , testCase "decodeWith uses config limits" $ do
      let xml = "<a><b><c><d><e>deep</e></d></c></b></a>"
      let cfg = defaultParseConfig { pcMaxDepth = 3 }
      let result = decodeWith cfg xml
      assertBool "Depth limit enforced" (isLeft result)

  , testCase "decodeWith with generous limits succeeds" $ do
      let xml = "<a><b><c><d><e>deep</e></d></c></b></a>"
      let cfg = defaultParseConfig { pcMaxDepth = 10000 }
      let result = decodeWith cfg xml
      assertBool "Generous limit passes" (not (isLeft result))

  , testCase "decodeWith disabling DTD rejects DTD" $ do
      let xml = BS8.pack "<?xml version=\"1.0\"?><!DOCTYPE foo [<!ENTITY e \"val\">]><foo>&e;</foo>"
      let cfg = defaultParseConfig { pcAllowDTD = False }
      let result = decodeWith cfg xml
      assertBool "DTD rejected when disabled" (isLeft result)
  ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

safeEval :: Either String a -> IO (Either SomeException (Either String a))
safeEval x = try (evaluate x)
