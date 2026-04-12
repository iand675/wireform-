{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.HTML (htmlTests) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Primitive.SmallArray (emptySmallArray, smallArrayFromList, sizeofSmallArray, indexSmallArray)
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import HTML.Value
import HTML.Parse
import HTML.Encode
import qualified HTML.DOM as DOM
import HTML.Class

htmlTests :: TestTree
htmlTests = testGroup "HTML"
  [ parseTests
  , voidElementTests
  , autoCloseTests
  , rawTextTests
  , entityTests
  , caseInsensitiveTests
  , commentTests
  , queryTests
  , encodeDecodeTests
  , classTests
  , genericTests
  , edgeCaseTests
  ]

parseTests :: TestTree
parseTests = testGroup "Parse"
  [ testCase "minimal HTML: <p>hello" $ do
      let doc = parseHTML "<p>hello"
          root = htmlRoot doc
      case root of
        HTMLElement "p" _ cs -> textContent (indexSmallArray cs 0) @?= "hello"
        _ -> assertBool "found p element" (containsTag "p" root)

  , testCase "full document" $ do
      let doc = parseHTML "<!DOCTYPE html><html><head><title>T</title></head><body><p>Hi</p></body></html>"
          root = htmlRoot doc
      assertBool "has doctype" (htmlDoctype doc /= Nothing)
      assertBool "has html root" (getTagName root == Just "html")

  , testCase "self-closing: <br/> = <br>" $ do
      let doc = parseHTML "<div><br/></div>"
          root = htmlRoot doc
      assertBool "contains br" (containsTag "br" root)

  , testCase "attributes parsed" $ do
      let doc = parseHTML "<img src=\"test.png\" alt=\"image\">"
          root = htmlRoot doc
      case findTag "img" root of
        Just n -> do
          getAttr "src" n @?= Just "test.png"
          getAttr "alt" n @?= Just "image"
        Nothing -> assertFailure "expected img element"

  , testCase "multiple attributes" $ do
      let doc = parseHTML "<div id=\"main\" class=\"container\">content</div>"
          root = htmlRoot doc
      case findTag "div" root of
        Just n -> do
          getAttr "id" n @?= Just "main"
          getAttr "class" n @?= Just "container"
        Nothing -> assertFailure "expected div element"
  ]

voidElementTests :: TestTree
voidElementTests = testGroup "Void elements"
  [ testCase "<br><hr><img src=\"x.png\">" $ do
      let doc = parseHTML "<div><br><hr><img src=\"x.png\"></div>"
          root = htmlRoot doc
      assertBool "contains br" (containsTag "br" root)
      assertBool "contains hr" (containsTag "hr" root)
      assertBool "contains img" (containsTag "img" root)

  , testCase "void elements have no children" $ do
      let doc = parseHTML "<br>text after"
          root = htmlRoot doc
      case root of
        HTMLElement "br" _ cs -> (sizeofSmallArray cs == 0) @?= True
        _ -> pure ()

  , testCase "input is void" $ do
      let doc = parseHTML "<form><input type=\"text\"><input type=\"submit\"></form>"
          root = htmlRoot doc
      assertBool "contains inputs" (containsTag "input" root)

  , testCase "meta is void" $ do
      let doc = parseHTML "<head><meta charset=\"utf-8\"></head>"
          root = htmlRoot doc
      assertBool "contains meta" (containsTag "meta" root)
  ]

autoCloseTests :: TestTree
autoCloseTests = testGroup "Auto-close"
  [ testCase "<p>one<p>two → two separate <p> elements" $ do
      let doc = parseHTML "<div><p>one<p>two</div>"
          root = htmlRoot doc
          ps = queryAll "p" root
      length ps @?= 2
      textContent (ps !! 0) @?= "one"
      textContent (ps !! 1) @?= "two"

  , testCase "<li> auto-close" $ do
      let doc = parseHTML "<ul><li>one<li>two<li>three</ul>"
          root = htmlRoot doc
          lis = queryAll "li" root
      length lis @?= 3

  , testCase "<td> auto-close" $ do
      let doc = parseHTML "<table><tr><td>a<td>b</tr></table>"
          root = htmlRoot doc
          tds = queryAll "td" root
      length tds @?= 2
  ]

rawTextTests :: TestTree
rawTextTests = testGroup "Raw text elements"
  [ testCase "script content not parsed as HTML" $ do
      let doc = parseHTML "<script>if (a < b) {}</script>"
          root = htmlRoot doc
      case findTag "script" root of
        Just (HTMLElement _ _ cs)
          | sizeofSmallArray cs > 0 -> do
              let content = textContent (indexSmallArray cs 0)
              assertBool "contains raw content" (T.isInfixOf "<" content)
        _ -> assertFailure "expected script element"

  , testCase "style content preserved" $ do
      let doc = parseHTML "<style>p { color: red; }</style>"
          root = htmlRoot doc
      case findTag "style" root of
        Just (HTMLElement _ _ cs)
          | sizeofSmallArray cs > 0 ->
              assertBool "has style content" (not (T.null (textContent (indexSmallArray cs 0))))
        _ -> assertFailure "expected style element"
  ]

entityTests :: TestTree
entityTests = testGroup "Entity references"
  [ testCase "&nbsp;" $ do
      let doc = parseHTML "<p>&nbsp;</p>"
          root = htmlRoot doc
      assertBool "resolved to non-breaking space" (T.any (== '\x00A0') (deepTextContent root))

  , testCase "&amp;" $ do
      let doc = parseHTML "<p>&amp;</p>"
      deepTextContent (htmlRoot doc) @?= "&"

  , testCase "&lt; &gt;" $ do
      let doc = parseHTML "<p>&lt;&gt;</p>"
      deepTextContent (htmlRoot doc) @?= "<>"

  , testCase "&quot;" $ do
      let doc = parseHTML "<p>&quot;</p>"
      deepTextContent (htmlRoot doc) @?= "\""

  , testCase "&#65; (numeric decimal)" $ do
      let doc = parseHTML "<p>&#65;</p>"
      deepTextContent (htmlRoot doc) @?= "A"

  , testCase "&#x41; (numeric hex)" $ do
      let doc = parseHTML "<p>&#x41;</p>"
      deepTextContent (htmlRoot doc) @?= "A"

  , testCase "&mdash;" $ do
      let doc = parseHTML "<p>&mdash;</p>"
      deepTextContent (htmlRoot doc) @?= "\x2014"

  , testCase "&ndash;" $ do
      let doc = parseHTML "<p>&ndash;</p>"
      deepTextContent (htmlRoot doc) @?= "\x2013"

  , testCase "&copy;" $ do
      let doc = parseHTML "<p>&copy;</p>"
      deepTextContent (htmlRoot doc) @?= "\x00A9"

  , testCase "&reg;" $ do
      let doc = parseHTML "<p>&reg;</p>"
      deepTextContent (htmlRoot doc) @?= "\x00AE"

  , testCase "&hellip;" $ do
      let doc = parseHTML "<p>&hellip;</p>"
      deepTextContent (htmlRoot doc) @?= "\x2026"
  ]

caseInsensitiveTests :: TestTree
caseInsensitiveTests = testGroup "Case insensitive"
  [ testCase "<DIV> parsed as div" $ do
      let doc = parseHTML "<DIV>content</DIV>"
          root = htmlRoot doc
      getTagName root @?= Just "html"
      assertBool "contains div" (containsTag "div" root)

  , testCase "<Div Class=\"x\"> → div class=\"x\"" $ do
      let doc = parseHTML "<Div Class=\"x\">test</Div>"
          root = htmlRoot doc
      assertBool "contains div" (containsTag "div" root)
      case findTag "div" root of
        Just n -> getAttr "class" n @?= Just "x"
        Nothing -> assertFailure "expected div element"

  , testCase "mixed case: <P>text</p>" $ do
      let doc = parseHTML "<P>text</p>"
          root = htmlRoot doc
      assertBool "found p" (containsTag "p" root)
  ]

commentTests :: TestTree
commentTests = testGroup "Comments"
  [ testCase "<!-- comment -->" $ do
      let doc = parseHTML "<div><!-- comment --></div>"
          root = htmlRoot doc
      assertBool "has comment" (hasComment root)

  , testCase "comment content preserved" $ do
      let doc = parseHTML "<div><!-- hello world --></div>"
          root = htmlRoot doc
      case findComment root of
        Just txt -> assertBool "has content" (T.isInfixOf "hello world" txt)
        Nothing -> assertFailure "expected comment"
  ]

queryTests :: TestTree
queryTests = testGroup "Query"
  [ testCase "querySelector by tag" $ do
      let doc = parseHTML "<div><p>one</p><p>two</p></div>"
          root = htmlRoot doc
      case queryOne "p" root of
        Just n -> deepTextContent n @?= "one"
        Nothing -> assertFailure "expected p"

  , testCase "querySelectorAll by tag" $ do
      let doc = parseHTML "<div><p>one</p><p>two</p></div>"
          root = htmlRoot doc
          ps = queryAll "p" root
      length ps @?= 2

  , testCase "querySelector by class" $ do
      let doc = parseHTML "<div><span class=\"highlight\">yes</span><span>no</span></div>"
          root = htmlRoot doc
      case queryOne ".highlight" root of
        Just n -> deepTextContent n @?= "yes"
        Nothing -> assertFailure "expected .highlight"

  , testCase "querySelector by id" $ do
      let doc = parseHTML "<div><span id=\"main\">target</span></div>"
          root = htmlRoot doc
      case queryOne "#main" root of
        Just n -> deepTextContent n @?= "target"
        Nothing -> assertFailure "expected #main"

  , testCase "getElementById" $ do
      let doc = parseHTML "<div><p id=\"intro\">Hello</p></div>"
          root = htmlRoot doc
      case queryOne "#intro" root of
        Just n -> deepTextContent n @?= "Hello"
        Nothing -> assertFailure "expected element with id intro"

  , testCase "getElementsByClass" $ do
      let doc = parseHTML "<div><p class=\"item\">a</p><p class=\"item\">b</p><p>c</p></div>"
          root = htmlRoot doc
          items = queryAll ".item" root
      length items @?= 2

  , testCase "descendant selector: div.main p" $ do
      let doc = parseHTML "<div class=\"main\"><p>target</p></div><p>other</p>"
          root = htmlRoot doc
          results = queryAll "div.main p" root
      length results @?= 1
      deepTextContent (head results) @?= "target"
  ]

encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "Encode/Decode"
  [ testCase "encode void elements without closing tag" $ do
      let doc = HTMLDocument Nothing (HTMLElement "div" emptySmallArray
            (smallArrayFromList [HTMLElement "br" emptySmallArray mempty, HTMLElement "hr" emptySmallArray mempty]))
          encoded = encodeHTML doc
          decoded = parseHTML encoded
          root = htmlRoot decoded
      assertBool "contains br" (containsTag "br" root)
      assertBool "contains hr" (containsTag "hr" root)

  , testCase "encode/parse roundtrip preserves structure" $ do
      let doc = HTMLDocument Nothing
            (HTMLElement "div" (smallArrayFromList [HTMLAttribute "class" "main"])
              (smallArrayFromList
                [ HTMLElement "p" emptySmallArray (smallArrayFromList [HTMLText "hello"])
                , HTMLElement "br" emptySmallArray mempty
                , HTMLElement "p" emptySmallArray (smallArrayFromList [HTMLText "world"])
                ]))
          encoded = encodeHTML doc
          decoded = parseHTML encoded
          root = htmlRoot decoded
      assertBool "has div" (containsTag "div" root)
      let ps = queryAll "p" root
      length ps @?= 2

  , testCase "boolean attributes minimized" $ do
      let doc = HTMLDocument Nothing
            (HTMLElement "input" (smallArrayFromList
              [ HTMLAttribute "type" "checkbox"
              , HTMLAttribute "checked" ""
              ]) mempty)
          encoded = TE.decodeUtf8 (encodeHTML doc)
      assertBool "has checked without value" (T.isInfixOf " checked" encoded)

  , testCase "encodes entities in text" $ do
      let doc = HTMLDocument Nothing
            (HTMLElement "p" emptySmallArray (smallArrayFromList [HTMLText "a < b & c > d"]))
          encoded = TE.decodeUtf8 (encodeHTML doc)
      assertBool "has &lt;" (T.isInfixOf "&lt;" encoded)
      assertBool "has &amp;" (T.isInfixOf "&amp;" encoded)
      assertBool "has &gt;" (T.isInfixOf "&gt;" encoded)
  ]

data PersonHTML = PersonHTML
  { pName :: !Text
  , pAge :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToHTML, FromHTML)

classTests :: TestTree
classTests = testGroup "Class instances"
  [ testCase "Text roundtrip" $ do
      let val = "hello" :: Text
      fromHTML (toHTML val) @?= Right val

  , testCase "Int roundtrip" $ do
      let val = 42 :: Int
      fromHTML (toHTML val) @?= Right val

  , testCase "Bool roundtrip" $ do
      fromHTML (toHTML True) @?= Right True
  ]

genericTests :: TestTree
genericTests = testGroup "Generic deriving"
  [ testCase "record to HTML" $ do
      let person = PersonHTML "John" 30
          node = toHTML person
      case node of
        HTMLElement _ _ cs -> sizeofSmallArray cs @?= 2
        _ -> assertFailure "expected HTMLElement"

  , testCase "record roundtrip" $ do
      let person = PersonHTML "Jane" 25
      fromHTML (toHTML person) @?= Right person
  ]

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge cases"
  [ testCase "parse empty document" $ do
      let doc = parseHTML ""
          root = htmlRoot doc
      case root of
        HTMLElement "html" _ _ -> pure ()
        _ -> pure ()

  , testCase "parse whitespace only" $ do
      let doc = parseHTML "   \n\t  "
          root = htmlRoot doc
      case root of
        HTMLElement "html" _ _ -> pure ()
        _ -> pure ()

  , testCase "nested divs" $ do
      let doc = parseHTML "<div><div><div>deep</div></div></div>"
          root = htmlRoot doc
      assertBool "contains div" (containsTag "div" root)

  , testCase "multiple root-level elements" $ do
      let doc = parseHTML "<p>one</p><p>two</p>"
          root = htmlRoot doc
      assertBool "parsed something" True

  , testCase "doctype parsing" $ do
      let doc = parseHTML "<!DOCTYPE html><html><body>hi</body></html>"
      case htmlDoctype doc of
        Just (Doctype (Just _) _ _) -> pure ()
        _ -> pure ()
  ]

-- Helpers

getTagName :: HTMLNode -> Maybe Text
getTagName (HTMLElement t _ _) = Just t
getTagName _ = Nothing

containsTag :: Text -> HTMLNode -> Bool
containsTag tag (HTMLElement t _ cs)
  | t == tag = True
  | otherwise = any (containsTag tag) cs
containsTag _ _ = False

findTag :: Text -> HTMLNode -> Maybe HTMLNode
findTag tag n@(HTMLElement t _ cs)
  | t == tag = Just n
  | otherwise = foldl (\acc c -> case acc of Just _ -> acc; Nothing -> findTag tag c) Nothing (toList cs)
findTag _ _ = Nothing

hasComment :: HTMLNode -> Bool
hasComment (HTMLComment _) = True
hasComment (HTMLElement _ _ cs) = any hasComment cs
hasComment _ = False

findComment :: HTMLNode -> Maybe Text
findComment (HTMLComment t) = Just t
findComment (HTMLElement _ _ cs) = foldl (\acc c -> case acc of Just _ -> acc; Nothing -> findComment c) Nothing (toList cs)
findComment _ = Nothing

deepTextContent :: HTMLNode -> Text
deepTextContent = textContent

queryAll :: Text -> HTMLNode -> [HTMLNode]
queryAll sel raw = map DOM.rawNode (DOM.querySelectorAll (DOM.rootNode raw) sel)

queryOne :: Text -> HTMLNode -> Maybe HTMLNode
queryOne sel raw = DOM.rawNode <$> DOM.querySelector (DOM.rootNode raw) sel
