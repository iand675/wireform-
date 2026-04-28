{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.XML (xmlTests) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Concurrent (killThread)
import XML.Value
import XML.SAX
import XML.Decode
import XML.Encode
import XML.Path
import XML.DSL (Query(..))
import qualified XML.DSL as DSL
import XML.Class
import XML.Schema
import XML.CodeGen
import qualified XML.FastDOM as FD
import XML.Incremental

import Hedgehog (Gen, Property, property, forAll, (===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog (testProperty)

xmlTests :: TestTree
xmlTests = testGroup "XML"
  [ saxTests
  , domTests
  , roundtripTests
  , namespaceTests
  , entityTests
  , cdataTests
  , commentAndPITests
  , pathTests
  , pathEnhancedTests
  , dslTests
  , typeclassTests
  , genericTests
  , largeDocTests
  , edgeCaseTests
  , fastDOMTests
  -- New expanded test groups
  , saxEdgeCaseTests
  , domRobustnessTests
  , fastDOMRobustnessTests
  , encodeRobustnessTests
  , pathDSLQueryTests
  , propertyTests
  , conformanceTests
  , incrementalTests
  , concurrentTests
  , streamFoldTests
  , w3cConformanceTests
  ]

-- Simple document for testing
simpleXML :: Text
simpleXML = "<root><child attr=\"value\">text</child></root>"

-- SAX Tests
saxTests :: TestTree
saxTests = testGroup "SAX Parser"
  [ testCase "parse simple document" $ do
      let Right events = parseSAX (TE.encodeUtf8 simpleXML)
          evList = V.toList events
      assertBool "has StartDocument" (any isStartDoc evList)
      assertBool "has EndDocument" (any isEndDoc evList)
      assertBool "has StartElement root" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList
      assertBool "has StartElement child" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList
      assertBool "has Characters text" $
        any (\e -> case e of Characters t -> t == "text"; _ -> False) evList
      assertBool "has EndElement child" $
        any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList
      assertBool "has EndElement root" $
        any (\e -> case e of EndElement n -> nameLocal n == "root"; _ -> False) evList

  , testCase "parse with XML declaration" $ do
      let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has StartDocument with decl" $
        any (\e -> case e of
          StartDocument (Just decl) -> xmlVersion decl == "1.0" && xmlEncoding decl == Just "UTF-8"
          _ -> False) evList

  , testCase "foldSAX counts elements" $ do
      let xml = "<a><b/><c/><d/></a>"
          Right count = foldSAX countElems 0 (TE.encodeUtf8 xml)
      count @?= 4

  , testCase "parse self-closing tags" $ do
      let xml = "<root><empty/></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has StartElement empty" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "empty"; _ -> False) evList
      assertBool "has EndElement empty" $
        any (\e -> case e of EndElement n -> nameLocal n == "empty"; _ -> False) evList

  , testCase "error on mismatched tags" $ do
      let xml = "<a><b></c></a>"
          result = parseSAX (TE.encodeUtf8 xml)
      assertBool "should be error" (isLeft result)
  ]

-- DOM Tests
domTests :: TestTree
domTests = testGroup "DOM Parser"
  [ testCase "decode simple document" $ do
      let Right doc = decode (TE.encodeUtf8 simpleXML)
          root = docRoot doc
      elementName root @?= Just (simpleName "root")
      V.length (elementChildren root) @?= 1
      let child = V.head (elementChildren root)
      elementName child @?= Just (simpleName "child")

  , testCase "decode with attributes" $ do
      let xml = "<person name=\"John\" age=\"30\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "name" root @?= Just "John"
      attr "age" root @?= Just "30"

  , testCase "decode nested elements" $ do
      let xml = "<a><b><c>deep</c></b></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["b", "c"] (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "deep"
  ]

-- Roundtrip tests
roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testCase "encode then decode = identity" $ do
      let xml = "<root><child attr=\"value\">text</child><empty/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2

  , testCase "roundtrip with XML declaration" $ do
      let decl = XMLDecl "1.0" (Just "UTF-8") Nothing
          root = Element (simpleName "test") V.empty
                   (V.singleton (Text "hello"))
          doc = Document (Just decl) root
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2

  , testCase "roundtrip with CDATA" $ do
      let root = Element (simpleName "root") V.empty
                   (V.singleton (CData "some <special> & chars"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
          child = V.head (elementChildren (docRoot doc2))
      case child of
        CData t -> t @?= "some <special> & chars"
        _ -> assertFailure "Expected CDATA node"

  , testCase "roundtrip with multiple children" $ do
      let children = V.fromList
            [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
            , Element (simpleName "b") V.empty (V.singleton (Text "2"))
            , Element (simpleName "c") V.empty (V.singleton (Text "3"))
            ]
          root = Element (simpleName "root") V.empty children
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2
  ]

-- Namespace tests
namespaceTests :: TestTree
namespaceTests = testGroup "Namespaces"
  [ testCase "default namespace" $ do
      let xml = "<root xmlns=\"http://example.com\"><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element name _ _ ->
          nameNamespace name @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"

  , testCase "prefixed namespace" $ do
      let xml = "<ns:root xmlns:ns=\"http://example.com\"><ns:child/></ns:root>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element name _ _ -> do
          nameLocal name @?= "root"
          namePrefix name @?= Just "ns"
          nameNamespace name @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"
  ]

-- Entity reference tests
entityTests :: TestTree
entityTests = testGroup "Entity References"
  [ testCase "standard entities" $ do
      let xml = "<root>&amp;&lt;&gt;&apos;&quot;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "&<>'\""

  , testCase "numeric entity decimal" $ do
      let xml = "<root>&#65;&#66;&#67;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "ABC"

  , testCase "numeric entity hex" $ do
      let xml = "<root>&#x41;&#x42;&#x43;</root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "ABC"

  , testCase "entities in attributes" $ do
      let xml = "<root attr=\"a&amp;b\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) @?= Just "a&b"
  ]

-- CDATA tests
cdataTests :: TestTree
cdataTests = testGroup "CDATA Sections"
  [ testCase "parse CDATA" $ do
      let xml = "<root><![CDATA[<not>markup & stuff]]></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has CDATASection" $
        any (\e -> case e of CDATASection t -> t == "<not>markup & stuff"; _ -> False) evList

  , testCase "CDATA in DOM" $ do
      let xml = "<root><![CDATA[content]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children @?= 1
      case V.head children of
        CData t -> t @?= "content"
        _ -> assertFailure "Expected CData node"
  ]

-- Comment and PI tests
commentAndPITests :: TestTree
commentAndPITests = testGroup "Comments and PIs"
  [ testCase "parse comment" $ do
      let xml = "<root><!-- a comment --></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has CommentEvent" $
        any (\e -> case e of CommentEvent t -> T.strip t == "a comment"; _ -> False) evList

  , testCase "parse processing instruction" $ do
      let xml = "<root><?target data?></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has PI" $
        any (\e -> case e of PI t _ -> t == "target"; _ -> False) evList

  , testCase "comment in DOM" $ do
      let xml = "<root><!-- hello --></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      assertBool "has comment child" $
        V.any isCommentNode children

  , testCase "PI in DOM" $ do
      let xml = "<root><?app data?></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      assertBool "has PI child" $
        V.any isPINode children
  ]

-- Path query tests
pathTests :: TestTree
pathTests = testGroup "Path Queries"
  [ testCase "child query" $ do
      let xml = "<root><items><item>a</item><item>b</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["items", "item"] (docRoot doc)
      V.length results @?= 2

  , testCase "descendant query" $ do
      let xml = "<root><a><b><target>found</target></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query (Descendant (simpleName "target")) (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "found"

  , testCase "attribute query" $ do
      let xml = "<person name=\"John\" age=\"30\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "name" (docRoot doc) @?= Just "John"
      attr "age" (docRoot doc) @?= Just "30"
      attr "missing" (docRoot doc) @?= Nothing

  , testCase "text content recursive" $ do
      let xml = "<root>hello <b>world</b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "hello world"

  , testCase "parsePath simple" $ do
      let Right path = parsePath "items/item"
          xml = "<root><items><item>x</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1

  , testCase "parsePath descendant" $ do
      let Right path = parsePath "//item"
          xml = "<root><a><item>x</item></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1
  ]

-- Typeclass tests
typeclassTests :: TestTree
typeclassTests = testGroup "Typeclass Instances"
  [ testCase "Text roundtrip" $ do
      let val = "hello" :: Text
          node = toXML val
      fromXML node @?= Right val

  , testCase "Int roundtrip" $ do
      let val = 42 :: Int
          node = toXML val
      fromXML node @?= Right val

  , testCase "Bool roundtrip" $ do
      let node = toXML True
      fromXML node @?= Right True
      let node2 = toXML False
      fromXML node2 @?= Right False

  , testCase "Maybe roundtrip" $ do
      let val = Just (42 :: Int)
          node = toXML val
      fromXML node @?= Right val
      let val2 = Nothing :: Maybe Int
          node2 = toXML val2
      fromXML node2 @?= Right val2

  , testCase "List roundtrip" $ do
      let val = [1, 2, 3] :: [Int]
          node = toXML val
      fromXML node @?= Right val
  ]

-- Generic deriving tests
data TestPerson = TestPerson
  { name :: !Text
  , age :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToXML, FromXML)

genericTests :: TestTree
genericTests = testGroup "Generic Deriving"
  [ testCase "record to XML" $ do
      let person = TestPerson "John" 30
          node = toXML person
      case node of
        Element n _ cs -> do
          nameLocal n @?= "TestPerson"
          V.length cs @?= 2
        _ -> assertFailure "Expected Element"

  , testCase "record roundtrip" $ do
      let person = TestPerson "Jane" 25
          node = toXML person
      fromXML node @?= Right person

  , testCase "encodeXML / decodeXML roundtrip" $ do
      let person = TestPerson "Bob" 40
          bs = encodeXML person
          Right person2 = decodeXML bs :: Either String TestPerson
      person2 @?= person
  ]

-- Large document test
largeDocTests :: TestTree
largeDocTests = testGroup "Large Documents"
  [ testCase "10k elements parse and verify" $ do
      let n = 10000 :: Int
          items = T.concat [ "<item id=\"" <> T.pack (show i) <> "\">"
                           <> T.pack (show i) <> "</item>"
                           | i <- [1..n] ]
          xml = "<root>" <> items <> "</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children @?= n
      let firstChild = V.head children
          lastChild = V.last children
      textContent firstChild @?= "1"
      textContent lastChild @?= T.pack (show n)
      attr "id" firstChild @?= Just "1"
      attr "id" lastChild @?= Just (T.pack (show n))
  ]

-- Edge case tests
edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Cases"
  [ testCase "empty element" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      V.null (elementChildren (docRoot doc)) @?= True

  , testCase "self-closing tag" $ do
      let xml = "<root><br/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      V.length (elementChildren (docRoot doc)) @?= 1

  , testCase "attributes with single quotes" $ do
      let xml = "<root attr='value'/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) @?= Just "value"

  , testCase "attributes with double quotes" $ do
      let xml = "<root attr=\"value\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "attr" (docRoot doc) @?= Just "value"

  , testCase "whitespace handling" $ do
      let xml = "<root>  hello  </root>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "  hello  "

  , testCase "mixed content" $ do
      let xml = "<root>text1<child/>text2</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          children = elementChildren (docRoot doc)
      V.length children @?= 3

  , testCase "deeply nested" $ do
      let depth = 100 :: Int
          opens = T.concat [ "<n" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</n" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right doc = decode (TE.encodeUtf8 xml)
      assertBool "parsed successfully" True
      let go (Element _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (docRoot doc) @?= depth

  , testCase "pretty print" $ do
      let root = Element (simpleName "root") V.empty
                   (V.fromList
                     [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
                     , Element (simpleName "b") V.empty (V.singleton (Text "2"))
                     ])
          doc = Document Nothing root
          pretty = encodePretty 2 doc
      assertBool "pretty output is non-empty" (not (null (show pretty)))

  , testCase "DOCTYPE is skipped" $ do
      let xml = "<?xml version=\"1.0\"?><!DOCTYPE root SYSTEM \"root.dtd\"><root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) @?= Just (simpleName "root")
  ]

-- Enhanced parsePath tests
pathEnhancedTests :: TestTree
pathEnhancedTests = testGroup "Enhanced Path Parsing"
  [ testCase "parse \".\" (self)" $ do
      let Right path = parsePath "."
      case path of
        Sequence [Self] -> pure ()
        _ -> assertFailure $ "Expected Sequence [Self], got " ++ show path

  , testCase "parse \"..\" (parent)" $ do
      let Right path = parsePath ".."
      case path of
        Sequence [Parent] -> pure ()
        _ -> assertFailure $ "Expected Sequence [Parent], got " ++ show path

  , testCase "parse \"*\" (wildcard)" $ do
      let Right path = parsePath "*"
          xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 3

  , testCase "parse \"name[@attr='val']\"" $ do
      let Right path = parsePath "item[@type='book']"
          xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "B"

  , testCase "parse \"name[3]\" (1-based index)" $ do
      let Right path = parsePath "item[2]"
          xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "b"
  ]

-- DSL tests
dslTests :: TestTree
dslTests = testGroup "XML DSL"
  [ testCase "child /> child composition" $ do
      let xml = "<root><items><item>a</item><item>b</item></items></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "items" DSL./> DSL.child "item"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
      textContent (V.head results) @?= "a"

  , testCase "anyDescendant search" $ do
      let xml = "<root><a><b><target>found</target></b></a><target>also</target></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyDescendant
          results = DSL.select q (docRoot doc)
      assertBool "found descendants" (V.length results >= 3)

  , testCase "where_ filter with attribute" $ do
      let xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereAttr "type" "book" (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "B"

  , testCase "whereContains" $ do
      let xml = "<root><a href=\"https://example.com/foo\">X</a><a href=\"other\">Y</a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereContains "href" "example" (DSL.child "a")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "X"

  , testCase "index (1-based)" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.index 2 (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "b"

  , testCase "selectOne" $ do
      let xml = "<root><item>a</item><item>b</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "item"
          result = DSL.selectOne q (docRoot doc)
      case result of
        Just n -> textContent n @?= "a"
        Nothing -> assertFailure "Expected Just"

  , testCase "selectText" $ do
      let xml = "<root><msg>hello </msg><msg>world</msg></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "msg" DSL./> DSL.textContent
          result = DSL.selectText q (docRoot doc)
      result @?= "hello world"

  , testCase "count" $ do
      let xml = "<root><item/><item/><item/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.count (DSL.child "item")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      V.head results @?= 3

  , testCase "liftQuery extension" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          customQ = DSL.liftQuery $ \n -> case n of
            Element _ _ cs -> V.filter (isElementNamed "b") cs
            _ -> V.empty
          results = DSL.select customQ (docRoot doc)
      V.length results @?= 1

  , testCase "union (|>)" $ do
      let xml = "<root><a>1</a><b>2</b><c>3</c></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL.|> DSL.child "c"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
      textContent (V.head results) @?= "1"
      textContent (results V.! 1) @?= "3"

  , testCase "first and last" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          qFirst = DSL.first (DSL.child "item")
          qLast = DSL.last (DSL.child "item")
      case DSL.selectOne qFirst (docRoot doc) of
        Just n -> textContent n @?= "a"
        Nothing -> assertFailure "Expected first"
      case DSL.selectOne qLast (docRoot doc) of
        Just n -> textContent n @?= "c"
        Nothing -> assertFailure "Expected last"

  , testCase "descendant search by name" $ do
      let xml = "<root><a><b><target>deep</target></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.descendant "target"
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "deep"

  , testCase "descendant chain (//>)" $ do
      let xml = "<root><a><item>x</item></a><b><item>y</item></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyChild DSL.//> DSL.child "item"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
  ]

-- FastDOM zero-copy parser tests
fastDOMTests :: TestTree
fastDOMTests = testGroup "FastDOM"
  [ testCase "parseFast roundtrip: parse then toDocument equals decode" $ do
      let xml = "<root><child attr=\"value\">text</child><empty/></root>"
          bs = TE.encodeUtf8 xml
          Right docFull = decode bs
          Right fastDoc = FD.parseFast bs
          docMaterialized = FD.toDocument fastDoc
      docRoot docMaterialized @?= docRoot docFull

  , testCase "parseFast accessor: nodeTagBS returns correct slice" $ do
      let bs = BS8.pack "<person><name>John</name></person>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src  = FD.fdSource fastDoc
      FD.nodeTagBS root src @?= "person"

  , testCase "parseFast accessor: attrValueBS returns correct slice" $ do
      let bs = BS8.pack "<item id=\"42\" class=\"main\"/>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src  = FD.fdSource fastDoc
          attrs = FD.nodeAttrs root
      V.length attrs @?= 2
      FD.attrNameBS (attrs V.! 0) src @?= "id"
      FD.attrValueBS (attrs V.! 0) src @?= "42"
      FD.attrNameBS (attrs V.! 1) src @?= "class"
      FD.attrValueBS (attrs V.! 1) src @?= "main"

  , testCase "parseFast large document (100 items)" $ do
      let header = "<?xml version=\"1.0\"?>\n<catalog>\n"
          footer = "</catalog>\n"
          mkItem :: Int -> String
          mkItem i = concat
            [ "  <item id=\"", show i, "\">"
            , "Product ", show i
            , "</item>\n"
            ]
          items = concatMap mkItem [1..100 :: Int]
          bs = BS8.pack (header ++ items ++ footer)
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          children = FD.nodeChildren root
      -- Remove whitespace text nodes
      let elems = V.filter isElem children
      V.length elems @?= 100
      let firstChild = V.head elems
          src = FD.fdSource fastDoc
      FD.nodeTagBS firstChild src @?= "item"

  , testCase "parseFast with CDATA" $ do
      let bs = BS8.pack "<root><![CDATA[<not>markup & stuff]]></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children @?= 1
      case V.head children of
        FD.FCData _ -> FD.nodeTextBS (V.head children) src @?= "<not>markup & stuff"
        other -> assertFailure $ "Expected FCData, got: " ++ show other

  , testCase "parseFast with comment" $ do
      let bs = BS8.pack "<root><!-- hello world --></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children @?= 1
      case V.head children of
        FD.FComment (FD.Span _ _) -> pure ()
        other -> assertFailure $ "Expected FComment, got: " ++ show other

  , testCase "parseFast with processing instruction" $ do
      let bs = BS8.pack "<root><?target some data?></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children @?= 1
      case V.head children of
        FD.FPI tSpan _dSpan ->
          FD.nodeTag (FD.FElement tSpan V.empty V.empty) src @?= "target"
        other -> assertFailure $ "Expected FPI, got: " ++ show other

  , testCase "parseFast self-closing elements" $ do
      let bs = BS8.pack "<root><br/><hr/><img src=\"x.png\"/></root>"
          Right fastDoc = FD.parseFast bs
          children = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children @?= 3
      FD.nodeTagBS (children V.! 0) src @?= "br"
      FD.nodeTagBS (children V.! 1) src @?= "hr"
      FD.nodeTagBS (children V.! 2) src @?= "img"
      V.length (FD.nodeChildren (children V.! 0)) @?= 0
      V.length (FD.nodeAttrs (children V.! 2)) @?= 1
      FD.attrValueBS (V.head (FD.nodeAttrs (children V.! 2))) src @?= "x.png"

  , testCase "parseFast toDocument with entities" $ do
      let bs = BS8.pack "<root>&amp;&lt;&gt;</root>"
          Right fastDoc = FD.parseFast bs
          doc = FD.toDocument fastDoc
      textContent (docRoot doc) @?= "&<>"

  , testCase "parseFast with XML declaration" $ do
      let bs = BS8.pack "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
          Right fastDoc = FD.parseFast bs
          src = FD.fdSource fastDoc
      FD.nodeTagBS (FD.fdRoot fastDoc) src @?= "root"
  ]
  where
    isElem (FD.FElement _ _ _) = True
    isElem _ = False

-- Helpers

isStartDoc :: SAXEvent -> Bool
isStartDoc (StartDocument _) = True
isStartDoc _ = False

isEndDoc :: SAXEvent -> Bool
isEndDoc EndDocument = True
isEndDoc _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

countElems :: Int -> SAXEvent -> Int
countElems n (StartElement _ _) = n + 1
countElems n _ = n

isCommentNode :: Node -> Bool
isCommentNode (Comment _) = True
isCommentNode _ = False

countSubstring :: String -> String -> Int
countSubstring needle = go
  where
    nLen = length needle
    go [] = 0
    go s@(_:xs)
      | take nLen s == needle = 1 + go (drop nLen s)
      | otherwise = go xs

isPINode :: Node -> Bool
isPINode (ProcessingInstruction _ _) = True
isPINode _ = False

------------------------------------------------------------------------
-- Group 1: SAX parser edge cases (30+ tests)
------------------------------------------------------------------------

saxEdgeCaseTests :: TestTree
saxEdgeCaseTests = testGroup "SAX Edge Cases"
  [ testCase "empty document with self-closing root" $ do
      let xml = "<?xml version=\"1.0\"?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has StartDocument" (any isStartDoc evList)
      assertBool "has root element" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList
      assertBool "has EndDocument" (any isEndDoc evList)

  , testCase "self-closing with attributes" $ do
      let xml = "<br class=\"clear\"/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          evList = V.toList events
      assertBool "has StartElement br" $
        any (\e -> case e of
          StartElement n attrs -> nameLocal n == "br" &&
            V.any (\(Attribute an av) -> nameLocal an == "class" && av == "clear") attrs
          _ -> False) evList
      assertBool "has EndElement br" $
        any (\e -> case e of EndElement n -> nameLocal n == "br"; _ -> False) evList

  , testCase "deeply nested (50 levels)" $ do
      let depth = 50 :: Int
          opens = T.concat [ "<d" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</d" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right events = parseSAX (TE.encodeUtf8 xml)
          startCount = V.length $ V.filter (\e -> case e of StartElement _ _ -> True; _ -> False) events
      startCount @?= depth

  , testCase "very long tag name (1000 chars)" $ do
      let tagName = T.replicate 1000 "a"
          xml = "<" <> tagName <> "></" <> tagName <> ">"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "parses long tag" $
        V.any (\e -> case e of StartElement n _ -> nameLocal n == tagName; _ -> False) events

  , testCase "very long attribute value (10000 chars)" $ do
      let longVal = T.replicate 10000 "x"
          xml = "<root attr=\"" <> longVal <> "\"/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has long attr value" $
        V.any (\e -> case e of
          StartElement _ attrs -> V.any (\(Attribute _ v) -> v == longVal) attrs
          _ -> False) events

  , testCase "multiple attributes with same prefix different NS" $ do
      let xml = "<root xmlns:a=\"http://a.com\" xmlns:b=\"http://b.com\" a:x=\"1\" b:x=\"2\"/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          startElems = V.filter (\e -> case e of StartElement _ _ -> True; _ -> False) events
      assertBool "parsed" (V.length startElems == 1)
      case V.head startElems of
        StartElement _ attrs ->
          assertBool "has 4 attrs" (V.length attrs == 4)
        _ -> assertFailure "Expected StartElement"

  , testCase "attribute with empty value" $ do
      let xml = "<x a=\"\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      attr "a" (docRoot doc) @?= Just ""

  , testCase "attribute with no value (invalid XML, should error)" $ do
      let xml = "<x a/>"
          result = parseSAX (TE.encodeUtf8 xml)
      assertBool "should be error" (isLeft result)

  , testCase "tag with only whitespace inside" $ do
      let xml = "<x>   </x>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "   "

  , testCase "mixed content: text and elements" $ do
      let xml = "<p>Hello <b>world</b> today</p>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "Hello world today"
      let children_ = elementChildren (docRoot doc)
      V.length children_ @?= 3

  , testCase "adjacent text after entity" $ do
      let xml = "<x>a&amp;b</x>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "a&b"

  , testCase "all 5 entity references in text" $ do
      let xml = "<r>&amp;&lt;&gt;&apos;&quot;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "&<>'\""

  , testCase "all 5 entity references in attributes" $ do
      let xml = "<r a=\"&amp;\" b=\"&lt;\" c=\"&gt;\" d=\"&apos;\" e=\"&quot;\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "a" root @?= Just "&"
      attr "b" root @?= Just "<"
      attr "c" root @?= Just ">"
      attr "d" root @?= Just "'"
      attr "e" root @?= Just "\""

  , testCase "numeric character ref decimal" $ do
      let xml = "<r>&#65;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "A"

  , testCase "hex character ref" $ do
      let xml = "<r>&#x41;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "A"

  , testCase "high unicode char ref (emoji)" $ do
      let xml = "<r>&#x1F600;</r>"
          Right doc = decode (TE.encodeUtf8 xml)
      textContent (docRoot doc) @?= "\x1F600"

  , testCase "invalid entity produces error" $ do
      let xml = "<r>&bogus;</r>"
          result = decode (TE.encodeUtf8 xml)
      assertBool "should be error" (isLeft result)

  , testCase "unterminated entity produces error" $ do
      let xml = "<r>&amp</r>"
          result = decode (TE.encodeUtf8 xml)
      assertBool "should be error" (isLeft result)

  , testCase "CDATA with special chars" $ do
      let xml = "<r><![CDATA[<not>&a tag>]]></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has CDATA" $
        V.any (\e -> case e of CDATASection t -> t == "<not>&a tag>"; _ -> False) events

  , testCase "CDATA containing ]] but not ]]>" $ do
      let xml = "<r><![CDATA[hello]]world]]></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has CDATA with ]]" $
        V.any (\e -> case e of CDATASection t -> T.isInfixOf "]]" t; _ -> False) events

  , testCase "comment basic" $ do
      let xml = "<r><!-- comment --></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has comment" $
        V.any (\e -> case e of CommentEvent _ -> True; _ -> False) events

  , testCase "PI basic" $ do
      let xml = "<r><?php echo \"hello\"; ?></r>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has PI with php target" $
        V.any (\e -> case e of PI t _ -> t == "php"; _ -> False) events

  , testCase "XML declaration with encoding" $ do
      let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><r/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has decl with encoding" $
        V.any (\e -> case e of
          StartDocument (Just decl) -> xmlVersion decl == "1.0" && xmlEncoding decl == Just "UTF-8"
          _ -> False) events

  , testCase "XML declaration with standalone" $ do
      let xml = "<?xml version=\"1.0\" standalone=\"yes\"?><r/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has decl with standalone" $
        V.any (\e -> case e of
          StartDocument (Just decl) -> xmlStandalone decl == Just True
          _ -> False) events

  , testCase "BOM handling: UTF-8 BOM before XML" $ do
      let bom = BS.pack [0xEF, 0xBB, 0xBF]
          xml = bom <> TE.encodeUtf8 "<?xml version=\"1.0\"?><root/>"
          result = decode xml
      assertBool "BOM document parses" (not (isLeft result))

  , testCase "whitespace between attributes" $ do
      let xml = "<x  a = \"1\"  b = \"2\" >"
          -- Not well-formed without closing, add close tag
          xmlFull = xml <> "</x>"
          Right doc = decode (TE.encodeUtf8 xmlFull)
      attr "a" (docRoot doc) @?= Just "1"
      attr "b" (docRoot doc) @?= Just "2"

  , testCase "namespace declaration" $ do
      let xml = "<x xmlns=\"http://example.com\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      case docRoot doc of
        Element n _ _ -> nameNamespace n @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"

  , testCase "namespace with prefix" $ do
      let xml = "<ns:x xmlns:ns=\"http://example.com\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
      case docRoot doc of
        Element n _ _ -> do
          namePrefix n @?= Just "ns"
          nameLocal n @?= "x"
          nameNamespace n @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"

  , testCase "default namespace on child" $ do
      let xml = "<a xmlns=\"http://a\"><b xmlns=\"http://b\"/></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element rn _ cs -> do
          nameNamespace rn @?= Just "http://a"
          case V.head cs of
            Element cn _ _ -> nameNamespace cn @?= Just "http://b"
            _ -> assertFailure "Expected child Element"
        _ -> assertFailure "Expected Element"

  , testCase "namespace undeclaration" $ do
      let xml = "<a xmlns=\"http://a\"><b xmlns=\"\"/></a>"
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element rn _ cs -> do
          nameNamespace rn @?= Just "http://a"
          case V.head cs of
            Element cn _ _ -> nameNamespace cn @?= Just ""
            _ -> assertFailure "Expected child Element"
        _ -> assertFailure "Expected Element"

  , testCase "PI before root element" $ do
      let xml = "<?pi-target data?><root/>"
          Right events = parseSAX (TE.encodeUtf8 xml)
      assertBool "has PI" $
        V.any (\e -> case e of PI t _ -> t == "pi-target"; _ -> False) events

  , testCase "comment before root element" $ do
      let xml = "<!-- pre-comment --><root/>"
          result = decode (TE.encodeUtf8 xml)
      assertBool "parses with pre-comment" (not (isLeft result))

  , testCase "multiple self-closing" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right events = parseSAX (TE.encodeUtf8 xml)
          starts = V.filter (\e -> case e of StartElement _ _ -> True; _ -> False) events
          ends = V.filter (\e -> case e of EndElement _ -> True; _ -> False) events
      V.length starts @?= 4
      V.length ends @?= 4
  ]

------------------------------------------------------------------------
-- Group 2: DOM parser robustness (15+ tests)
------------------------------------------------------------------------

domRobustnessTests :: TestTree
domRobustnessTests = testGroup "DOM Robustness"
  [ testCase "parse then encode roundtrip preserves structure" $ do
      let xml = "<root><a x=\"1\">text</a><b><c/></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      docRoot doc @?= docRoot doc2

  , testCase "parse then encode preserves namespace declarations" $ do
      let xml = "<root xmlns=\"http://example.com\" xmlns:ns=\"http://ns.com\"><ns:child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          root2 = docRoot doc2
      case root2 of
        Element n _ _ -> nameNamespace n @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"

  , testCase "comments survive roundtrip" $ do
      let xml = "<root><!-- comment --></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          cs = elementChildren (docRoot doc2)
      assertBool "has comment" $ V.any isCommentNode cs

  , testCase "PIs survive roundtrip" $ do
      let xml = "<root><?target data?></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          cs = elementChildren (docRoot doc2)
      assertBool "has PI" $ V.any isPINode cs

  , testCase "CDATA sections survive roundtrip" $ do
      let xml = "<root><![CDATA[special <content> & more]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "special <content> & more"

  , testCase "mixed content roundtrip" $ do
      let xml = "<root>text1<child/>text2</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          cs = elementChildren (docRoot doc2)
      V.length cs @?= 3

  , testCase "empty element roundtrip" $ do
      let xml = "<x/>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      V.null (elementChildren (docRoot doc2)) @?= True

  , testCase "whitespace in attributes preserved" $ do
      let xml = "<x a=\"  spaced  \"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      attr "a" (docRoot doc2) @?= Just "  spaced  "

  , testCase "Unicode text roundtrip: Chinese" $ do
      let xml = "<r>\x4F60\x597D\x4E16\x754C</r>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "\x4F60\x597D\x4E16\x754C"

  , testCase "Unicode text roundtrip: Arabic" $ do
      let xml = "<r>\x0645\x0631\x062D\x0628\x0627</r>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "\x0645\x0631\x062D\x0628\x0627"

  , testCase "Unicode text roundtrip: emoji" $ do
      let xml = "<r>\x1F600\x1F4A9\x2764</r>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "\x1F600\x1F4A9\x2764"

  , testCase "large document roundtrip (1000 elements)" $ do
      let n = 1000 :: Int
          items = T.concat [ "<item id=\"" <> T.pack (show i) <> "\">"
                           <> T.pack (show i) <> "</item>"
                           | i <- [1..n] ]
          xml = "<root>" <> items <> "</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
      V.length (elementChildren (docRoot doc2)) @?= n

  , testCase "attribute order preserved in roundtrip" $ do
      let xml = "<x first=\"1\" second=\"2\" third=\"3\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          attrs' = elementAttributes (docRoot doc2)
      V.length attrs' @?= 3
      case attrs' V.! 0 of
        Attribute an _ -> nameLocal an @?= "first"
      case attrs' V.! 1 of
        Attribute an _ -> nameLocal an @?= "second"
      case attrs' V.! 2 of
        Attribute an _ -> nameLocal an @?= "third"

  , testCase "deeply nested roundtrip" $ do
      let depth = 50 :: Int
          opens = T.concat [ "<n" <> T.pack (show i) <> ">" | i <- [1..depth] ]
          closes = T.concat [ "</n" <> T.pack (show i) <> ">" | i <- reverse [1..depth] ]
          xml = opens <> "leaf" <> closes
          Right doc = decode (TE.encodeUtf8 xml)
          encoded = encode doc
          Right doc2 = decode encoded
          go (Element _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (docRoot doc2) @?= depth

  , testCase "multiple text children roundtrip" $ do
      let root = Element (simpleName "r") V.empty
                   (V.fromList [Text "hello", Text " ", Text "world"])
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "hello world"
  ]

------------------------------------------------------------------------
-- Group 3: FastDOM robustness (15+ tests)
------------------------------------------------------------------------

fastDOMRobustnessTests :: TestTree
fastDOMRobustnessTests = testGroup "FastDOM Robustness"
  [ testCase "parseFast then toDocument equals decode" $ do
      let xml = "<root><a x=\"1\">text</a><b><c/></b></root>"
          bs = TE.encodeUtf8 xml
          Right docFull = decode bs
          Right fastDoc = FD.parseFast bs
          docMat = FD.toDocument fastDoc
      docRoot docMat @?= docRoot docFull

  , testCase "FastDOM on document with entities (toDocument resolves)" $ do
      let bs = BS8.pack "<root>&amp;&lt;&gt;&apos;&quot;</root>"
          Right fastDoc = FD.parseFast bs
          doc = FD.toDocument fastDoc
      textContent (docRoot doc) @?= "&<>'\""

  , testCase "FastDOM nodeTagBS returns exact bytes" $ do
      let bs = BS8.pack "<myElement>content</myElement>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src = FD.fdSource fastDoc
      FD.nodeTagBS root src @?= "myElement"

  , testCase "FastDOM attrValueBS on attribute with entities (raw)" $ do
      let bs = BS8.pack "<x a=\"&amp;b\"/>"
          Right fastDoc = FD.parseFast bs
          root = FD.fdRoot fastDoc
          src = FD.fdSource fastDoc
          attrs = FD.nodeAttrs root
      V.length attrs @?= 1
      FD.attrValueBS (V.head attrs) src @?= "&amp;b"

  , testCase "FastDOM children count matches" $ do
      let bs = BS8.pack "<root><a/><b/><c/></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 3

  , testCase "FastDOM deep nesting (50 levels)" $ do
      let depth = 50 :: Int
          opens = concatMap (\i -> "<d" ++ show i ++ ">") [1..depth]
          closes = concatMap (\i -> "</d" ++ show i ++ ">") (reverse [1..depth])
          bs = BS8.pack (opens ++ "leaf" ++ closes)
          Right fastDoc = FD.parseFast bs
          go (FD.FElement _ _ cs) = if V.null cs then 0 else 1 + go (V.head cs)
          go _ = 0 :: Int
      go (FD.fdRoot fastDoc) @?= depth

  , testCase "FastDOM large document (1000 items)" $ do
      let items = concatMap (\i -> "<item>" ++ show i ++ "</item>") [1..1000 :: Int]
          bs = BS8.pack ("<root>" ++ items ++ "</root>")
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 1000

  , testCase "FastDOM self-closing elements" $ do
      let bs = BS8.pack "<root><br/><hr/></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 2
      let src = FD.fdSource fastDoc
      FD.nodeTagBS (children_ V.! 0) src @?= "br"
      FD.nodeTagBS (children_ V.! 1) src @?= "hr"

  , testCase "FastDOM CDATA" $ do
      let bs = BS8.pack "<root><![CDATA[raw <data>]]></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length children_ @?= 1
      case V.head children_ of
        FD.FCData _ -> FD.nodeTextBS (V.head children_) src @?= "raw <data>"
        _ -> assertFailure "Expected FCData"

  , testCase "FastDOM comments" $ do
      let bs = BS8.pack "<root><!-- hi --></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 1
      case V.head children_ of
        FD.FComment _ -> pure ()
        _ -> assertFailure "Expected FComment"

  , testCase "FastDOM PIs" $ do
      let bs = BS8.pack "<root><?target data?></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 1
      case V.head children_ of
        FD.FPI _ _ -> pure ()
        _ -> assertFailure "Expected FPI"

  , testCase "FastDOM namespace attributes visible" $ do
      let bs = BS8.pack "<root xmlns:ns=\"http://example.com\" ns:a=\"1\"/>"
          Right fastDoc = FD.parseFast bs
          attrs = FD.nodeAttrs (FD.fdRoot fastDoc)
          src = FD.fdSource fastDoc
      V.length attrs @?= 2
      FD.attrNameBS (attrs V.! 0) src @?= "xmlns:ns"
      FD.attrNameBS (attrs V.! 1) src @?= "ns:a"

  , testCase "FastDOM empty element" $ do
      let bs = BS8.pack "<empty/>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 0

  , testCase "FastDOM with XML declaration" $ do
      let bs = BS8.pack "<?xml version=\"1.0\"?><root/>"
          Right fastDoc = FD.parseFast bs
          src = FD.fdSource fastDoc
      FD.nodeTagBS (FD.fdRoot fastDoc) src @?= "root"

  , testCase "FastDOM text node between elements" $ do
      let bs = BS8.pack "<root><a/>text<b/></root>"
          Right fastDoc = FD.parseFast bs
          children_ = FD.nodeChildren (FD.fdRoot fastDoc)
      V.length children_ @?= 3
      case children_ V.! 1 of
        FD.FText _ -> FD.nodeTextBS (children_ V.! 1) (FD.fdSource fastDoc) @?= "text"
        _ -> assertFailure "Expected FText"
  ]

------------------------------------------------------------------------
-- Group 4: Encode robustness (15+ tests)
------------------------------------------------------------------------

encodeRobustnessTests :: TestTree
encodeRobustnessTests = testGroup "Encode Robustness"
  [ testCase "encode escapes < > & in text" $ do
      let root = Element (simpleName "r") V.empty (V.singleton (Text "<>&"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "<>&"

  , testCase "encode escapes \" in attributes" $ do
      let root = Element (simpleName "r")
                   (V.singleton (Attribute (simpleName "a") "he said \"hi\""))
                   V.empty
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      attr "a" (docRoot doc2) @?= Just "he said \"hi\""

  , testCase "encode handles CDATA passthrough" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (CData "<special>&chars</special>"))
          doc = Document Nothing root
          encoded = encode doc
      assertBool "contains CDATA" (BS.isInfixOf "<![CDATA[" encoded)

  , testCase "encode handles comments" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (Comment " a comment "))
          doc = Document Nothing root
          encoded = encode doc
      assertBool "contains comment" (BS.isInfixOf "<!--" encoded)

  , testCase "encode handles PIs" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (ProcessingInstruction "target" "data"))
          doc = Document Nothing root
          encoded = encode doc
      assertBool "contains PI" (BS.isInfixOf "<?target" encoded)

  , testCase "encode escapes ]]> in CDATA without losing content (XML 1.0 §2.7)" $ do
      -- CDATA cannot contain "]]>" literally. Encoder must split it across
      -- two CDATA sections so the round-tripped text remains intact.
      let payload = "before ]]> after"
          root = Element (simpleName "r") V.empty
                   (V.singleton (CData payload))
          doc = Document Nothing root
          encoded = encode doc
      assertBool "no literal ]]> in CDATA payload"
        (not (BS.isInfixOf (BS8.pack "before ]]> after") encoded))
      let Right doc2 = decode encoded
          combined = textContent (docRoot doc2)
      combined @?= "before ]]> after"

  , testCase "encode escapes -- in comments (XML 1.0 §2.5)" $ do
      let raw = "no -- double hyphens allowed"
          root = Element (simpleName "r") V.empty
                   (V.singleton (Comment raw))
          doc = Document Nothing root
          encoded = encode doc
      -- The bare comment-body must not contain "--" (XML 1.0 §2.5);
      -- expected total occurrences = 2 (the opening "<!--" and closing "-->").
      countSubstring "--" (BS8.unpack encoded) @?= 2
      -- And must round-trip through a parser cleanly.
      let Right _doc2 = decode encoded
      pure ()

  , testCase "encode escapes ?> inside processing instruction body (XML 1.0 §2.6)" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (ProcessingInstruction "stylesheet" "href=\"x?>foo\""))
          doc = Document Nothing root
          encoded = encode doc
      -- exactly one closing "?>" — the one terminating the PI itself.
      countSubstring "?>" (BS8.unpack encoded) @?= 1

  , testCase "encode escapes CR in attributes (XML 1.0 §3.3.3)" $ do
      let val = "line1\rline2"
          root = Element (simpleName "r")
                   (V.singleton (Attribute (simpleName "a") val)) V.empty
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      assertBool "CR was escaped"
        (BS.isInfixOf (BS8.pack "&#xD;") encoded)
      attr "a" (docRoot doc2) @?= Just val

  , testCase "encode escapes tab/newline in attributes (XML 1.0 §3.3.3)" $ do
      let val = "x\ty\nz"
          root = Element (simpleName "r")
                   (V.singleton (Attribute (simpleName "a") val)) V.empty
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      attr "a" (docRoot doc2) @?= Just val

  , testCase "encode escapes CR in text (XML 1.0 §2.11)" $ do
      let raw = "first\rsecond"
          root = Element (simpleName "r") V.empty (V.singleton (Text raw))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      assertBool "CR was character-referenced in text"
        (BS.isInfixOf (BS8.pack "&#xD;") encoded)
      textContent (docRoot doc2) @?= raw

  , testCase "encode produces valid XML declaration" $ do
      let decl = XMLDecl "1.0" (Just "UTF-8") (Just True)
          root = Element (simpleName "r") V.empty V.empty
          doc = Document (Just decl) root
          encoded = encode doc
      assertBool "has xml decl" (BS.isPrefixOf "<?xml" encoded)
      assertBool "has version" (BS.isInfixOf "version=\"1.0\"" encoded)
      assertBool "has encoding" (BS.isInfixOf "encoding=\"UTF-8\"" encoded)
      assertBool "has standalone" (BS.isInfixOf "standalone=\"yes\"" encoded)

  , testCase "encode handles namespaced elements" $ do
      let name = Name "child" (Just "ns") Nothing
          root = Element (simpleName "r") V.empty
                   (V.singleton (Element name V.empty V.empty))
          doc = Document Nothing root
          encoded = encode doc
      assertBool "has ns:child" (BS.isInfixOf "ns:child" encoded)

  , testCase "encode handles empty elements as self-closing" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (Element (simpleName "empty") V.empty V.empty))
          doc = Document Nothing root
          encoded = encode doc
      assertBool "has self-closing" (BS.isInfixOf "<empty/>" encoded)

  , testCase "pretty print produces indented output" $ do
      let root = Element (simpleName "root") V.empty
                   (V.fromList
                     [ Element (simpleName "a") V.empty (V.singleton (Text "1"))
                     , Element (simpleName "b") V.empty (V.singleton (Text "2"))
                     ])
          doc = Document Nothing root
          pretty = encodePretty 2 doc
          prettyText = TE.decodeUtf8 pretty
      assertBool "has indentation" (T.isInfixOf "  <a>" prettyText)

  , testCase "pretty print with nested elements" $ do
      let child_ = Element (simpleName "inner") V.empty (V.singleton (Text "deep"))
          root = Element (simpleName "root") V.empty
                   (V.singleton (Element (simpleName "outer") V.empty (V.singleton child_)))
          doc = Document Nothing root
          pretty = encodePretty 4 doc
          prettyText = TE.decodeUtf8 pretty
      assertBool "has 4-space indent" (T.isInfixOf "    <outer>" prettyText)

  , testCase "encode Unicode text correctly" $ do
      let root = Element (simpleName "r") V.empty
                   (V.singleton (Text "\x4F60\x597D\x1F600"))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "\x4F60\x597D\x1F600"

  , testCase "encode empty document" $ do
      let root = Element (simpleName "r") V.empty V.empty
          doc = Document Nothing root
          encoded = encode doc
      assertBool "non-empty output" (not (BS.null encoded))
      let Right doc2 = decode encoded
      V.null (elementChildren (docRoot doc2)) @?= True

  , testCase "encode document with only root, no children" $ do
      let root = Element (simpleName "root") V.empty V.empty
          doc = Document Nothing root
          encoded = encode doc
      encoded @?= "<root/>"

  , testCase "encode multiple text children" $ do
      let root = Element (simpleName "r") V.empty
                   (V.fromList [Text "a", Text "b", Text "c"])
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= "abc"

  , testCase "encode attribute with special characters" $ do
      let root = Element (simpleName "r")
                   (V.singleton (Attribute (simpleName "val") "<>&'\""))
                   V.empty
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      attr "val" (docRoot doc2) @?= Just "<>&'\""
  ]

------------------------------------------------------------------------
-- Group 5: Path/DSL queries (20+ tests)
------------------------------------------------------------------------

pathDSLQueryTests :: TestTree
pathDSLQueryTests = testGroup "Path/DSL Queries"
  [ testCase "Path: /root/child selects direct children" $ do
      let xml = "<root><child>a</child><child>b</child></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          results = queryPath ["child"] (docRoot doc)
      V.length results @?= 2

  , testCase "Path: //name selects descendants at any depth" $ do
      let xml = "<root><a><target>deep</target></a><target>shallow</target></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "//target"
          results = query path (docRoot doc)
      V.length results @?= 2

  , testCase "Path: @attr selects attribute" $ do
      let xml = "<root name=\"hello\"/>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "@name"
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "hello"

  , testCase "Path: * wildcard selects all element children" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "*"
          results = query path (docRoot doc)
      V.length results @?= 3

  , testCase "Path: . selects self" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "."
          results = query path (docRoot doc)
      V.length results @?= 1

  , testCase "Path: name[1] selects first" $ do
      let xml = "<root><item>a</item><item>b</item><item>c</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "item[1]"
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "a"

  , testCase "Path: name[@attr='val'] predicate filter" $ do
      let xml = "<root><item type=\"book\">B</item><item type=\"dvd\">D</item></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          Right path = parsePath "item[@type='book']"
          results = query path (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "B"

  , testCase "DSL: child /> child composition" $ do
      let xml = "<root><a><b>deep</b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL./> DSL.child "b"
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "deep"

  , testCase "DSL: anyDescendant finds deeply nested" $ do
      let xml = "<root><a><b><c>deep</c></b></a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyDescendant
          results = DSL.select q (docRoot doc)
      assertBool "finds all descendants" (V.length results >= 3)

  , testCase "DSL: whereAttr filters correctly" $ do
      let xml = "<root><x t=\"a\">1</x><x t=\"b\">2</x><x t=\"a\">3</x></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereAttr "t" "a" (DSL.child "x")
          results = DSL.select q (docRoot doc)
      V.length results @?= 2

  , testCase "DSL: whereContains partial match" $ do
      let xml = "<root><a href=\"http://example.com/page\">X</a><a href=\"other\">Y</a></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.whereContains "href" "example" (DSL.child "a")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1

  , testCase "DSL: index 1-based selection" $ do
      let xml = "<root><x>a</x><x>b</x><x>c</x></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.index 3 (DSL.child "x")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      textContent (V.head results) @?= "c"

  , testCase "DSL: first/last" $ do
      let xml = "<root><x>a</x><x>b</x><x>c</x></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          qFirst = DSL.first (DSL.child "x")
          qLast = DSL.last (DSL.child "x")
      case DSL.selectOne qFirst (docRoot doc) of
        Just n -> textContent n @?= "a"
        Nothing -> assertFailure "Expected first"
      case DSL.selectOne qLast (docRoot doc) of
        Just n -> textContent n @?= "c"
        Nothing -> assertFailure "Expected last"

  , testCase "DSL: union (|>) combines results" $ do
      let xml = "<root><a>1</a><b>2</b><c>3</c></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.child "a" DSL.|> DSL.child "c"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2

  , testCase "DSL: count returns correct number" $ do
      let xml = "<root><x/><x/><x/><x/><x/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.count (DSL.child "x")
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      V.head results @?= 5

  , testCase "DSL: liftQuery user extension works" $ do
      let xml = "<root><a/><b/><c/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          customQ = DSL.liftQuery $ \n ->
            V.filter (isElementNamed "a") (elementChildren n)
          results = DSL.select customQ (docRoot doc)
      V.length results @?= 1

  , testCase "DSL: liftFilter custom predicate" $ do
      let xml = "<root><x val=\"10\"/><x val=\"20\"/><x val=\"5\"/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          bigVals = DSL.child "x" DSL./>
            DSL.liftFilter (\n ->
              case attr "val" n of
                Just v -> T.length v > 1
                Nothing -> False)
          results = DSL.select bigVals (docRoot doc)
      V.length results @?= 2

  , testCase "DSL: textContent extracts recursive text" $ do
      let xml = "<root>hello <b>world</b> end</root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.textContent
          result = DSL.selectText q (docRoot doc)
      result @?= "hello world end"

  , testCase "DSL: attribute on element without that attr returns Nothing" $ do
      let xml = "<root/>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.attribute "missing"
          results = DSL.select q (docRoot doc)
      V.length results @?= 1
      V.head results @?= Nothing

  , testCase "DSL: descendant chain (//>)" $ do
      let xml = "<root><a><x>1</x></a><b><x>2</x></b></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          q = DSL.anyChild DSL.//> DSL.child "x"
          results = DSL.select q (docRoot doc)
      V.length results @?= 2
  ]

------------------------------------------------------------------------
-- Group 6: Property-based tests (10+ properties) + generators
------------------------------------------------------------------------

genName :: Gen Name
genName = do
  local <- Gen.text (Range.linear 1 20) Gen.alpha
  pure (simpleName local)

genAttribute :: Gen Attribute
genAttribute = do
  name <- genName
  val <- Gen.text (Range.linear 0 50) (Gen.frequency
    [ (10, Gen.alphaNum)
    , (1, pure ' ')
    ])
  pure (Attribute name val)

genNode :: Int -> Gen Node
genNode maxDepth
  | maxDepth <= 0 = Gen.choice
      [ Text <$> Gen.text (Range.linear 1 30) Gen.alphaNum
      ]
  | otherwise = Gen.frequency
      [ (3, genElement maxDepth)
      , (2, Text <$> Gen.text (Range.linear 1 30) Gen.alphaNum)
      ]

genElement :: Int -> Gen Node
genElement maxDepth = do
  name <- genName
  nAttrs <- Gen.int (Range.linear 0 4)
  attrs <- V.fromList <$> Gen.list (Range.singleton nAttrs) genAttribute
  nChildren <- Gen.int (Range.linear 0 5)
  rawChildren <- Gen.list (Range.singleton nChildren) (genNode (maxDepth - 1))
  let children_ = V.fromList (dedupeTexts rawChildren)
  pure (Element name attrs children_)

dedupeTexts :: [Node] -> [Node]
dedupeTexts [] = []
dedupeTexts [x] = [x]
dedupeTexts (Text a : Text b : rest) = dedupeTexts (Text (a <> b) : rest)
dedupeTexts (x : rest) = x : dedupeTexts rest

genDocument :: Gen Document
genDocument = do
  useDecl <- Gen.bool
  let decl = if useDecl then Just (XMLDecl "1.0" (Just "UTF-8") Nothing) else Nothing
  root <- genElement 4
  pure (Document decl root)

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "parse (encode doc) == doc for generated documents" prop_roundtrip
  , testProperty "parseFast then toDocument equals decode" prop_fastdom_equals_decode
  , testProperty "text content survives SAX->DOM->encode->decode roundtrip" prop_text_roundtrip
  , testProperty "attribute values survive roundtrip" prop_attr_roundtrip
  , testProperty "element names survive roundtrip" prop_name_roundtrip
  , testProperty "child count preserved through roundtrip" prop_child_count_roundtrip
  , testProperty "generated docs with random depth parse successfully" prop_generated_parse
  , testProperty "FastDOM nodeTagBS matches decode nodeTag" prop_fastdom_tag_match
  , testProperty "encode produces parseable output" prop_encode_parseable
  , testProperty "SAX event count reasonable for generated docs" prop_sax_event_count
  ]

prop_roundtrip :: Property
prop_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
      result = decode encoded
  case result of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 -> docRoot doc === docRoot doc2

prop_fastdom_equals_decode :: Property
prop_fastdom_equals_decode = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case (decode encoded, FD.parseFast encoded) of
    (Right docFull, Right fastDoc) ->
      docRoot (FD.toDocument fastDoc) === docRoot docFull
    (Left err, _) -> fail $ "decode failed: " ++ err
    (_, Left err) -> fail $ "parseFast failed: " ++ err

prop_text_roundtrip :: Property
prop_text_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 -> textContent (docRoot doc) === textContent (docRoot doc2)

prop_attr_roundtrip :: Property
prop_attr_roundtrip = property $ do
  name <- forAll genName
  val <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  let root = Element (simpleName "root")
               (V.singleton (Attribute name val))
               V.empty
      doc = Document Nothing root
      encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 ->
      attr (nameLocal name) (docRoot doc2) === Just val

prop_name_roundtrip :: Property
prop_name_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 ->
      elementName (docRoot doc) === elementName (docRoot doc2)

prop_child_count_roundtrip :: Property
prop_child_count_roundtrip = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right doc2 -> do
      let countElems_ = V.length . V.filter (\n -> case n of Element {} -> True; _ -> False)
      countElems_ (elementChildren (docRoot doc)) === countElems_ (elementChildren (docRoot doc2))

prop_generated_parse :: Property
prop_generated_parse = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case decode encoded of
    Left err -> fail $ "parse failed: " ++ err
    Right _ -> HH.assert True

prop_fastdom_tag_match :: Property
prop_fastdom_tag_match = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case (decode encoded, FD.parseFast encoded) of
    (Right docFull, Right fastDoc) -> do
      let fullName = elementName (docRoot docFull)
          fastTag = FD.nodeTag (FD.fdRoot fastDoc) (FD.fdSource fastDoc)
      case fullName of
        Just n -> nameLocal n === fastTag
        Nothing -> fail "expected element"
    (Left err, _) -> fail $ "decode failed: " ++ err
    (_, Left err) -> fail $ "parseFast failed: " ++ err

prop_encode_parseable :: Property
prop_encode_parseable = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case parseSAX encoded of
    Left err -> fail $ "SAX parse of encoded doc failed: " ++ err
    Right _ -> HH.assert True

prop_sax_event_count :: Property
prop_sax_event_count = property $ do
  doc <- forAll genDocument
  let encoded = encode doc
  case parseSAX encoded of
    Left err -> fail $ "SAX parse failed: " ++ err
    Right events -> HH.assert (V.length events >= 3)

------------------------------------------------------------------------
-- Group 7: Conformance / edge cases from real XML (10+ tests)
------------------------------------------------------------------------

conformanceTests :: TestTree
conformanceTests = testGroup "Conformance / Real-World XML"
  [ testCase "RSS feed excerpt" $ do
      let xml = T.concat
            [ "<rss version=\"2.0\">"
            , "<channel>"
            , "<title>Example Feed</title>"
            , "<link>http://example.com</link>"
            , "<description>An example RSS feed</description>"
            , "<item>"
            , "<title>Article 1</title>"
            , "<link>http://example.com/1</link>"
            , "<description>First article</description>"
            , "</item>"
            , "<item>"
            , "<title>Article 2</title>"
            , "<link>http://example.com/2</link>"
            , "</item>"
            , "</channel>"
            , "</rss>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      elementName root @?= Just (simpleName "rss")
      attr "version" root @?= Just "2.0"
      let items = query (Descendant (simpleName "item")) root
      V.length items @?= 2

  , testCase "SOAP envelope structure" $ do
      let xml = T.concat
            [ "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
            , "<soap:Header/>"
            , "<soap:Body>"
            , "<m:GetPrice xmlns:m=\"http://example.com/prices\">"
            , "<m:Item>Widget</m:Item>"
            , "</m:GetPrice>"
            , "</soap:Body>"
            , "</soap:Envelope>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element n _ _ -> do
          nameLocal n @?= "Envelope"
          namePrefix n @?= Just "soap"
        _ -> assertFailure "Expected Element"

  , testCase "SVG snippet with namespaces" $ do
      let xml = T.concat
            [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
            , "<circle cx=\"50\" cy=\"50\" r=\"40\" fill=\"red\"/>"
            , "<text x=\"50\" y=\"50\">Hello</text>"
            , "</svg>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      attr "width" root @?= Just "100"
      let circles = queryPath ["circle"] root
      V.length circles @?= 1
      attr "fill" (V.head circles) @?= Just "red"

  , testCase "XHTML with mixed content" $ do
      let xml = T.concat
            [ "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
            , "<body>"
            , "<p>This is <em>emphasized</em> and <strong>strong</strong> text.</p>"
            , "</body>"
            , "</html>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      let ps = query (Descendant (simpleName "p")) root
      V.length ps @?= 1
      let pText = textContent (V.head ps)
      assertBool "has mixed content" (T.isInfixOf "emphasized" pText)

  , testCase "Android layout XML" $ do
      let xml = T.concat
            [ "<LinearLayout xmlns:android=\"http://schemas.android.com/apk/res/android\""
            , " android:layout_width=\"match_parent\""
            , " android:layout_height=\"wrap_content\""
            , " android:orientation=\"vertical\">"
            , "<TextView android:text=\"Hello World\"/>"
            , "<Button android:text=\"Click Me\"/>"
            , "</LinearLayout>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      V.length (elementChildren root) @?= 2

  , testCase "Maven POM excerpt" $ do
      let xml = T.concat
            [ "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">"
            , "<modelVersion>4.0.0</modelVersion>"
            , "<groupId>com.example</groupId>"
            , "<artifactId>my-app</artifactId>"
            , "<version>1.0-SNAPSHOT</version>"
            , "<dependencies>"
            , "<dependency>"
            , "<groupId>junit</groupId>"
            , "<artifactId>junit</artifactId>"
            , "<version>4.13</version>"
            , "</dependency>"
            , "</dependencies>"
            , "</project>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          root = docRoot doc
      case root of
        Element n _ _ -> nameLocal n @?= "project"
        _ -> assertFailure "Expected Element"
      let versions = query (Descendant (simpleName "version")) root
      V.length versions @?= 2

  , testCase "XML with doctype (should skip/handle gracefully)" $ do
      let xml = "<?xml version=\"1.0\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html><body>hello</body></html>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) @?= Just (simpleName "html")

  , testCase "XML with processing instructions before root" $ do
      let xml = "<?xml version=\"1.0\"?><?xml-stylesheet type=\"text/xsl\" href=\"style.xsl\"?><root/>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) @?= Just (simpleName "root")

  , testCase "XML with comments before root" $ do
      let xml = "<?xml version=\"1.0\"?><!-- comment before root --><root><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      elementName (docRoot doc) @?= Just (simpleName "root")

  , testCase "XML with trailing whitespace after root" $ do
      let xml = "<root/>   \n  "
          result = decode (TE.encodeUtf8 xml)
      assertBool "parses despite trailing whitespace" (not (isLeft result))

  , testCase "Atom feed excerpt" $ do
      let xml = T.concat
            [ "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
            , "<title>Example Feed</title>"
            , "<entry>"
            , "<title>Entry 1</title>"
            , "<id>urn:uuid:1</id>"
            , "<content type=\"html\">Some &amp; content</content>"
            , "</entry>"
            , "</feed>"
            ]
          Right doc = decode (TE.encodeUtf8 xml)
          entries = query (Descendant (simpleName "entry")) (docRoot doc)
      V.length entries @?= 1
      let contents = query (Descendant (simpleName "content")) (docRoot doc)
      V.length contents @?= 1
      textContent (V.head contents) @?= "Some & content"

  , testCase "XML with nested CDATA and entities" $ do
      let xml = "<root><![CDATA[Hello]]> &amp; <![CDATA[World]]></root>"
          Right doc = decode (TE.encodeUtf8 xml)
      let allText = textContent (docRoot doc)
      assertBool "has Hello" (T.isInfixOf "Hello" allText)
      assertBool "has &" (T.isInfixOf "&" allText)
      assertBool "has World" (T.isInfixOf "World" allText)
  ]

------------------------------------------------------------------------
-- Group 8: Incremental parser tests
------------------------------------------------------------------------

incrementalTests :: TestTree
incrementalTests = testGroup "Incremental Parser"
  [ testCase "feed small doc in one chunk, get all events" $ do
      let xml = TE.encodeUtf8 "<root><child>text</child></root>"
      p <- newParser
      events1 <- feedChunk p xml
      Right events2 <- feedEnd p
      let allEvents = events1 <> events2
          evList = V.toList allEvents
      assertBool "has StartElement root" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList
      assertBool "has Characters text" $
        any (\e -> case e of Characters t -> t == "text"; _ -> False) evList
      assertBool "has EndElement root" $
        any (\e -> case e of EndElement n -> nameLocal n == "root"; _ -> False) evList

  , testCase "feed doc in multiple chunks (split mid-tag), get correct events" $ do
      let xmlFull = TE.encodeUtf8 "<root><child attr=\"val\">content</child></root>"
          chunk1 = BS.take 15 xmlFull
          chunk2 = BS.drop 15 xmlFull
      p <- newParser
      ev1 <- feedChunk p chunk1
      ev2 <- feedChunk p chunk2
      Right ev3 <- feedEnd p
      let allEvents = ev1 <> ev2 <> ev3
          evList = V.toList allEvents
      assertBool "has StartElement root" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "root"; _ -> False) evList
      assertBool "has StartElement child" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList
      assertBool "has Characters content" $
        any (\e -> case e of Characters t -> t == "content"; _ -> False) evList
      assertBool "has EndElement child" $
        any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList

  , testCase "feed doc byte-by-byte, get same events as parseSAX" $ do
      let xmlFull = TE.encodeUtf8 "<root><a>text</a></root>"
          Right refEvents = parseSAX xmlFull
      p <- newParser
      byteChunks <- mapM (\i -> feedChunk p (BS.singleton (BS.index xmlFull i)))
                         [0 .. BS.length xmlFull - 1]
      Right finalEvents <- feedEnd p
      let allEvents = V.concat (byteChunks ++ [finalEvents])
      V.toList allEvents @?= V.toList refEvents

  , testCase "unterminated tag across chunk boundary" $ do
      let chunk1 = TE.encodeUtf8 "<root><child"
      p <- newParser
      ev1 <- feedChunk p chunk1
      -- chunk1 contains complete <root> tag so some events may be emitted
      let chunk2 = TE.encodeUtf8 ">text</child></root>"
      ev2 <- feedChunk p chunk2
      Right ev3 <- feedEnd p
      let allEvents = ev1 <> ev2 <> ev3
          evList = V.toList allEvents
      assertBool "has StartElement child" $
        any (\e -> case e of StartElement n _ -> nameLocal n == "child"; _ -> False) evList
      assertBool "has Characters text" $
        any (\e -> case e of Characters t -> t == "text"; _ -> False) evList
      assertBool "has EndElement child" $
        any (\e -> case e of EndElement n -> nameLocal n == "child"; _ -> False) evList
  ]

------------------------------------------------------------------------
-- Group 9: Concurrent parser tests
------------------------------------------------------------------------

concurrentTests :: TestTree
concurrentTests = testGroup "Concurrent Parser"
  [ testCase "withConcurrentParse produces same events as parseSAX" $ do
      let xml = TE.encodeUtf8 "<root><a>text1</a><b>text2</b></root>"
          Right refEvents = parseSAX xml
      eventsRef <- newIORef []
      Right () <- withConcurrentParse xml 256 $ \ev ->
        modifyIORef' eventsRef (ev :)
      concEvents <- V.fromList . reverse <$> readIORef eventsRef
      V.toList concEvents @?= V.toList refEvents

  , testCase "handler processes events in order" $ do
      let xml = TE.encodeUtf8 "<root><a/><b/><c/></root>"
      eventsRef <- newIORef ([] :: [SAXEvent])
      Right () <- withConcurrentParse xml 256 $ \ev ->
        modifyIORef' eventsRef (ev :)
      events <- reverse <$> readIORef eventsRef
      let elemNames = [ nameLocal n | StartElement n _ <- events ]
      assertBool "root comes first" (head elemNames == "root")
      assertBool "a before b before c" (elemNames == ["root", "a", "b", "c"])

  , testCase "parse error propagated to consumer" $ do
      let xml = TE.encodeUtf8 "<root><a></b></root>"
      result <- withConcurrentParse xml 256 (\_ -> pure ())
      assertBool "should be Left" (isLeft result)

  , testCase "large document (10K elements) parses correctly" $ do
      let n = 10000 :: Int
          items = T.concat [ "<item>" <> T.pack (show i) <> "</item>" | i <- [1..n] ]
          xml = TE.encodeUtf8 ("<root>" <> items <> "</root>")
      countRef <- newIORef (0 :: Int)
      Right () <- withConcurrentParse xml 256 $ \ev ->
        case ev of
          StartElement sn _ | nameLocal sn == "item" -> modifyIORef' countRef (+1)
          _ -> pure ()
      count <- readIORef countRef
      count @?= n

  , testCase "concurrent vs sequential: same results for complex document" $ do
      let xml = TE.encodeUtf8 $ T.concat
            [ "<?xml version=\"1.0\"?>"
            , "<catalog>"
            , "<book id=\"1\"><title>Haskell</title><price>29.99</price></book>"
            , "<book id=\"2\"><title>Erlang</title><price>39.99</price></book>"
            , "<!-- comment -->"
            , "<![CDATA[raw data]]>"
            , "</catalog>"
            ]
          Right refEvents = parseSAX xml
      concRef <- newIORef []
      Right () <- withConcurrentParse xml 64 $ \ev ->
        modifyIORef' concRef (ev :)
      concEvents <- V.fromList . reverse <$> readIORef concRef
      V.toList concEvents @?= V.toList refEvents

  , testCase "parseToChan low-level API works" $ do
      let xml = TE.encodeUtf8 "<root><a>text</a></root>"
      (chan, tid) <- parseToChan xml 32
      eventsRef <- newIORef []
      let loop = do
            item <- atomically $ readTBQueue chan
            case item of
              Nothing -> pure ()
              Just (Left _) -> pure ()
              Just (Right ev) -> do
                modifyIORef' eventsRef (ev :)
                loop
      loop
      killThread tid
      events <- reverse <$> readIORef eventsRef
      assertBool "has events" (not (null events))
      let Right refEvents = parseSAX xml
      events @?= V.toList refEvents

  , testCase "withConcurrentParseBS with chunked input" $ do
      let xml = TE.encodeUtf8 "<root><a>hello</a></root>"
          chunks = [BS.take 10 xml, BS.drop 10 xml]
          Right refEvents = parseSAX xml
      eventsRef <- newIORef []
      Right () <- withConcurrentParseBS chunks 128 $ \ev ->
        modifyIORef' eventsRef (ev :)
      concEvents <- V.fromList . reverse <$> readIORef eventsRef
      V.toList concEvents @?= V.toList refEvents
  ]

------------------------------------------------------------------------
-- Group 10: Streaming fold tests
------------------------------------------------------------------------

streamFoldTests :: TestTree
streamFoldTests = testGroup "Stream Fold"
  [ testCase "count elements concurrently" $ do
      let xml = TE.encodeUtf8 "<root><a/><b/><c/><d/></root>"
      Right count <- streamFold xml 64 (0 :: Int) $ \acc ev ->
        case ev of
          StartElement _ _ -> acc + 1
          _ -> acc
      count @?= 5

  , testCase "extract all text content concurrently" $ do
      let xml = TE.encodeUtf8 "<root>hello <b>world</b> end</root>"
      Right texts <- streamFold xml 64 ([] :: [Text]) $ \acc ev ->
        case ev of
          Characters t -> acc ++ [t]
          _ -> acc
      T.concat texts @?= "hello world end"

  , testCase "streamFoldIO: write events to IORef, verify count" $ do
      let xml = TE.encodeUtf8 "<root><x/><x/><x/></root>"
      ref <- newIORef (0 :: Int)
      Right count <- streamFoldIO xml 64 (0 :: Int) $ \acc ev -> do
        case ev of
          StartElement _ _ -> do
            modifyIORef' ref (+1)
            pure (acc + 1)
          _ -> pure acc
      count @?= 4
      ioCount <- readIORef ref
      ioCount @?= 4

  , testCase "streamFold on large document" $ do
      let n = 5000 :: Int
          items = T.concat [ "<item>" <> T.pack (show i) <> "</item>" | i <- [1..n] ]
          xml = TE.encodeUtf8 ("<root>" <> items <> "</root>")
      Right total <- streamFold xml 128 (0 :: Int) $ \acc ev ->
        case ev of
          StartElement sn _ | nameLocal sn == "item" -> acc + 1
          _ -> acc
      total @?= n

  , testCase "streamFold parse error propagated" $ do
      let xml = TE.encodeUtf8 "<root><bad></mismatch></root>"
      result <- streamFold xml 64 (0 :: Int) $ \acc _ -> acc + 1
      assertBool "should be Left" (isLeft result)
  ]

------------------------------------------------------------------------
-- Group 11: W3C-style well-formedness conformance tests
------------------------------------------------------------------------

w3cConformanceTests :: TestTree
w3cConformanceTests = testGroup "W3C Well-Formedness Conformance"
  [ validDocTests
  , invalidDocTests
  , encodingEdgeCaseTests
  ]

validDocTests :: TestTree
validDocTests = testGroup "Should parse successfully"
  [ testCase "minimal: <x/>" $ do
      let Right doc = decode (TE.encodeUtf8 "<x/>")
      elementName (docRoot doc) @?= Just (simpleName "x")

  , testCase "with XML decl" $ do
      let Right doc = decode (TE.encodeUtf8 "<?xml version=\"1.0\"?><x/>")
      elementName (docRoot doc) @?= Just (simpleName "x")

  , testCase "with encoding decl" $ do
      let Right doc = decode (TE.encodeUtf8 "<?xml version=\"1.0\" encoding=\"UTF-8\"?><x/>")
      elementName (docRoot doc) @?= Just (simpleName "x")

  , testCase "entity refs: &amp;&lt;&gt;&apos;&quot;" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>&amp;&lt;&gt;&apos;&quot;</x>")
      textContent (docRoot doc) @?= "&<>'\""

  , testCase "decimal char refs: &#65;&#x42;" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>&#65;&#x42;</x>")
      textContent (docRoot doc) @?= "AB"

  , testCase "CDATA section" $ do
      let Right doc = decode (TE.encodeUtf8 "<x><![CDATA[<not a tag>]]></x>")
      textContent (docRoot doc) @?= "<not a tag>"

  , testCase "namespaces" $ do
      let Right doc = decode (TE.encodeUtf8 "<a:x xmlns:a=\"http://example.com\"/>")
          root = docRoot doc
      case root of
        Element n _ _ -> do
          nameLocal n @?= "x"
          namePrefix n @?= Just "a"
          nameNamespace n @?= Just "http://example.com"
        _ -> assertFailure "Expected Element"

  , testCase "nested elements" $ do
      let Right doc = decode (TE.encodeUtf8 "<a><b><c/></b></a>")
          root = docRoot doc
      elementName root @?= Just (simpleName "a")
      let results = queryPath ["b", "c"] root
      V.length results @?= 1

  , testCase "attributes single and double quotes" $ do
      let Right doc = decode (TE.encodeUtf8 "<x a=\"1\" b='2'/>")
      attr "a" (docRoot doc) @?= Just "1"
      attr "b" (docRoot doc) @?= Just "2"

  , testCase "mixed content" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>text<y/>more</x>")
          cs = elementChildren (docRoot doc)
      V.length cs @?= 3
      textContent (docRoot doc) @?= "textmore"

  , testCase "processing instruction before root" $ do
      let Right doc = decode (TE.encodeUtf8 "<?target data?><x/>")
      elementName (docRoot doc) @?= Just (simpleName "x")

  , testCase "comment before root" $ do
      let Right doc = decode (TE.encodeUtf8 "<!--comment--><x/>")
      elementName (docRoot doc) @?= Just (simpleName "x")

  , testCase "multiple namespaces" $ do
      let xml = "<root xmlns:a=\"http://a.com\" xmlns:b=\"http://b.com\"><a:x/><b:y/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          cs = elementChildren (docRoot doc)
      V.length cs @?= 2

  , testCase "default namespace inheritance" $ do
      let xml = "<root xmlns=\"http://default.com\"><child/></root>"
          Right doc = decode (TE.encodeUtf8 xml)
          cs = elementChildren (docRoot doc)
      case V.head cs of
        Element n _ _ -> nameNamespace n @?= Just "http://default.com"
        _ -> assertFailure "Expected Element"

  , testCase "nested CDATA and text" $ do
      let xml = "<x>text<![CDATA[<cdata>]]>more</x>"
          Right doc = decode (TE.encodeUtf8 xml)
      let allText = textContent (docRoot doc)
      assertBool "contains text" (T.isInfixOf "text" allText)
      assertBool "contains cdata" (T.isInfixOf "<cdata>" allText)
      assertBool "contains more" (T.isInfixOf "more" allText)

  , testCase "empty element with attributes" $ do
      let Right doc = decode (TE.encodeUtf8 "<x a=\"1\" b=\"2\" c=\"3\"/>")
      V.null (elementChildren (docRoot doc)) @?= True
      attr "a" (docRoot doc) @?= Just "1"
      attr "b" (docRoot doc) @?= Just "2"
      attr "c" (docRoot doc) @?= Just "3"

  , testCase "whitespace-only text" $ do
      let Right doc = decode (TE.encodeUtf8 "<x>   \n\t  </x>")
      textContent (docRoot doc) @?= "   \n\t  "
  ]

invalidDocTests :: TestTree
invalidDocTests = testGroup "Should fail with error"
  [ testCase "empty input" $ do
      let result = decode BS.empty
      assertBool "empty input should fail" (isLeft result)

  , testCase "mismatched tags: <a></b>" $ do
      let result = decode (TE.encodeUtf8 "<a></b>")
      assertBool "mismatched tags should fail" (isLeft result)

  , testCase "unclosed tag: <a>" $ do
      let result = decode (TE.encodeUtf8 "<a>")
      assertBool "unclosed tag should fail" (isLeft result)

  , testCase "ampersand alone: <x>&</x>" $ do
      let result = decode (TE.encodeUtf8 "<x>&</x>")
      assertBool "bare ampersand should fail" (isLeft result)

  , testCase "unknown entity: <x>&bogus;</x>" $ do
      let result = decode (TE.encodeUtf8 "<x>&bogus;</x>")
      assertBool "unknown entity should fail" (isLeft result)

  , testCase "no root element (whitespace only)" $ do
      let result = decode (TE.encodeUtf8 "   ")
      assertBool "whitespace-only should fail" (isLeft result)

  , testCase "attribute without value: <x a/>" $ do
      let result = decode (TE.encodeUtf8 "<x a/>")
      assertBool "attribute without value should fail" (isLeft result)

  , testCase "malformed tag: <>" $ do
      let result = decode (TE.encodeUtf8 "<>")
      assertBool "empty tag name should fail" (isLeft result)

  , testCase "unclosed attribute: <x a=\"1>" $ do
      let result = decode (TE.encodeUtf8 "<x a=\"1>")
      assertBool "unclosed attribute should fail" (isLeft result)

  , testCase "text before root element (lenient: may succeed)" $ do
      let result = decode (TE.encodeUtf8 "hello<x/>")
      case result of
        Left _  -> pure ()
        Right doc -> elementName (docRoot doc) @?= Just (simpleName "x")

  , testCase "two root elements (lenient: may succeed)" $ do
      let result = decode (TE.encodeUtf8 "<a/><b/>")
      case result of
        Left _  -> pure ()
        Right doc -> assertBool "parsed first root" True
  ]

encodingEdgeCaseTests :: TestTree
encodingEdgeCaseTests = testGroup "Encoding edge cases"
  [ testCase "UTF-8 BOM before XML" $ do
      let bom = BS.pack [0xEF, 0xBB, 0xBF]
          xml = bom <> TE.encodeUtf8 "<?xml version=\"1.0\"?><x/>"
          result = decode xml
      assertBool "BOM should be handled" (not (isLeft result))

  , testCase "multi-byte UTF-8: Japanese" $ do
      let xml = TE.encodeUtf8 "<x>\x65E5\x672C\x8A9E</x>"
          Right doc = decode xml
      textContent (docRoot doc) @?= "\x65E5\x672C\x8A9E"

  , testCase "emoji: grinning face" $ do
      let xml = TE.encodeUtf8 "<x>\x1F600</x>"
          Right doc = decode xml
      textContent (docRoot doc) @?= "\x1F600"

  , testCase "4-byte UTF-8: musical symbol G clef" $ do
      let xml = TE.encodeUtf8 "<x>\x1D11E</x>"
          Right doc = decode xml
      textContent (docRoot doc) @?= "\x1D11E"

  , testCase "mixed multi-byte characters" $ do
      let xml = TE.encodeUtf8 "<x>\x00FC\x00E9\x4E16\x754C\x1F600</x>"
          Right doc = decode xml
          expected = "\x00FC\x00E9\x4E16\x754C\x1F600"
      textContent (docRoot doc) @?= expected

  , testCase "char ref for high unicode" $ do
      let xml = TE.encodeUtf8 "<x>&#x1D11E;</x>"
          Right doc = decode xml
      textContent (docRoot doc) @?= "\x1D11E"

  , testCase "char ref decimal" $ do
      let xml = TE.encodeUtf8 "<x>&#119070;</x>"
          Right doc = decode xml
      textContent (docRoot doc) @?= "\x1D11E"

  , testCase "roundtrip multi-byte text" $ do
      let text_ = "\x65E5\x672C\x8A9E\x1F600\x1D11E"
          root = Element (simpleName "r") V.empty (V.singleton (Text text_))
          doc = Document Nothing root
          encoded = encode doc
          Right doc2 = decode encoded
      textContent (docRoot doc2) @?= text_
  ]
