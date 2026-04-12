{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import HTML.DOM
import qualified HTML.Selector as Sel
import HTML.Value (HTMLNode(..), HTMLAttribute(..), Doctype(..), TreeEvent(..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

simpleDoc :: Document
simpleDoc = parseDocument
  "<!DOCTYPE html><html><head><title>Test</title></head>\
  \<body><div id=\"main\" class=\"foo bar\"><p>Hello</p><p>World</p></div></body></html>"

bodyNode :: Node
bodyNode =
  let root = documentElement simpleDoc
  in case childNodes root of
       [_head, body] -> body
       _ -> error "expected <head> and <body>"

mainDiv :: Node
mainDiv = case childNodes bodyNode of
  [d] -> d
  _ -> error "expected single <div> in body"

builderToBS :: BB.Builder -> BS.ByteString
builderToBS = BL.toStrict . BB.toLazyByteString

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "HTML.DOM"
  [ testGroup "Parsing" parsingTests
  , testGroup "Document access" docAccessTests
  , testGroup "Navigation" navigationTests
  , testGroup "Node inspection" inspectionTests
  , testGroup "Serialization" serializationTests
  , testGroup "CSS selectors" selectorTests
  , testGroup "CSS selectors (extended)" extendedSelectorTests
  , testGroup "Incremental parser" incrementalTests
  , testGroup "Streaming tree events" streamingTests
  ]

parsingTests :: [TestTree]
parsingTests =
  [ testCase "parseDocument produces a document" $ do
      let doc = parseDocument "<p>hi</p>"
          root = documentElement doc
      tagName root @?= Just "html"

  , testCase "parseDocument handles doctype" $ do
      let doc = parseDocument "<!DOCTYPE html><html></html>"
      isJust (documentDoctype doc) @? "expected doctype"
  ]

docAccessTests :: [TestTree]
docAccessTests =
  [ testCase "documentElement returns root" $ do
      tagName (documentElement simpleDoc) @?= Just "html"

  , testCase "documentDoctype returns doctype" $ do
      let mdt = documentDoctype simpleDoc
      isJust mdt @? "expected doctype"
  ]

navigationTests :: [TestTree]
navigationTests =
  [ testCase "childNodes of element" $ do
      let root = documentElement simpleDoc
          kids = childNodes root
      length kids @?= 2
      tagName (head kids) @?= Just "head"
      tagName (kids !! 1) @?= Just "body"

  , testCase "childNodes of text node is empty" $ do
      let p1 = head (childNodes mainDiv)
          textKids = childNodes p1
      length textKids @?= 1
      childNodes (head textKids) @?= []

  , testCase "firstChild / lastChild" $ do
      let fc = firstChild mainDiv
          lc = lastChild mainDiv
      isJust fc @? "expected firstChild"
      isJust lc @? "expected lastChild"
      tagName (fromJust fc) @?= Just "p"
      tagName (fromJust lc) @?= Just "p"
      textContent (fromJust fc) @?= "Hello"
      textContent (fromJust lc) @?= "World"

  , testCase "firstChild of text node is Nothing" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      firstChild textNode @?= Nothing

  , testCase "nextSibling / prevSibling" $ do
      let p1 = fromJust (firstChild mainDiv)
          p2 = fromJust (nextSibling p1)
      textContent p2 @?= "World"
      let p1' = fromJust (prevSibling p2)
      textContent p1' @?= "Hello"

  , testCase "nextSibling of last is Nothing" $ do
      let p2 = fromJust (lastChild mainDiv)
      nextSibling p2 @?= Nothing

  , testCase "prevSibling of first is Nothing" $ do
      let p1 = fromJust (firstChild mainDiv)
      prevSibling p1 @?= Nothing

  , testCase "parentNode" $ do
      let p1 = fromJust (firstChild mainDiv)
          parent = fromJust (parentNode p1)
      tagName parent @?= Just "div"
      getAttribute parent "id" @?= Just "main"

  , testCase "parentNode of root is Nothing" $ do
      let root = documentElement simpleDoc
      parentNode root @?= Nothing

  , testCase "round-trip: parent then child" $ do
      let p1 = fromJust (firstChild mainDiv)
          backToDiv = fromJust (parentNode p1)
          backToP1 = fromJust (firstChild backToDiv)
      textContent backToP1 @?= "Hello"
  ]

inspectionTests :: [TestTree]
inspectionTests =
  [ testCase "nodeName for elements" $ do
      nodeName mainDiv @?= "div"

  , testCase "nodeName for text" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      nodeName textNode @?= "#text"

  , testCase "nodeType for elements" $ do
      nodeType mainDiv @?= ElementNode

  , testCase "nodeType for text" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      nodeType textNode @?= TextNode

  , testCase "textContent" $ do
      textContent mainDiv @?= "HelloWorld"

  , testCase "tagName for element" $ do
      tagName mainDiv @?= Just "div"

  , testCase "tagName for non-element" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      tagName textNode @?= Nothing

  , testCase "getAttribute" $ do
      getAttribute mainDiv "id" @?= Just "main"
      getAttribute mainDiv "class" @?= Just "foo bar"
      getAttribute mainDiv "nonexistent" @?= Nothing

  , testCase "getAttributes" $ do
      let attrs = getAttributes mainDiv
      length attrs @?= 2

  , testCase "hasAttribute" $ do
      hasAttribute mainDiv "id" @? "should have id"
      not (hasAttribute mainDiv "nope") @? "should not have nope"

  , testCase "classList" $ do
      classList mainDiv @?= ["foo", "bar"]

  , testCase "classList empty" $ do
      let p1 = fromJust (firstChild mainDiv)
      classList p1 @?= []

  , testCase "rawNode returns HTMLNode" $ do
      case rawNode mainDiv of
        HTMLElement tag _ _ -> tag @?= "div"
        _ -> assertFailure "expected HTMLElement"
  ]

serializationTests :: [TestTree]
serializationTests =
  [ testCase "serialize element" $ do
      let bs = builderToBS (serialize mainDiv)
      BS.isInfixOf "<div" bs @? "should contain <div"
      BS.isInfixOf "</div>" bs @? "should contain </div>"
      BS.isInfixOf "Hello" bs @? "should contain Hello"

  , testCase "serializeDocument includes doctype" $ do
      let bs = builderToBS (serializeDocument simpleDoc)
      BS.isInfixOf "<!DOCTYPE" bs @? "should contain doctype"
      BS.isInfixOf "<html>" bs @? "should contain <html>"

  , testCase "innerHTML" $ do
      let h = innerHTML mainDiv
      T.isInfixOf "<p>" h @? "should contain <p>"
      T.isInfixOf "Hello" h @? "should contain Hello"
      not (T.isInfixOf "<div" h) @? "should not contain <div (inner only)"

  , testCase "outerHTML" $ do
      let h = outerHTML mainDiv
      T.isInfixOf "<div" h @? "should contain <div"
      T.isInfixOf "Hello" h @? "should contain Hello"

  , testCase "innerHTML of text node" $ do
      let textNode = head (childNodes (head (childNodes mainDiv)))
      innerHTML textNode @?= ""

  , testCase "round-trip: serialize -> parse" $ do
      let bs = builderToBS (serializeDocument simpleDoc)
          doc2 = parseDocument bs
          root2 = documentElement doc2
      tagName root2 @?= Just "html"
      let kids2 = childNodes root2
      length kids2 @?= 2
  ]

selectorTests :: [TestTree]
selectorTests =
  [ testCase "querySelector by tag" $ do
      let root = documentElement simpleDoc
          result = querySelector root "p"
      isJust result @? "should find <p>"
      textContent (fromJust result) @?= "Hello"

  , testCase "querySelectorAll by tag" $ do
      let root = documentElement simpleDoc
          results = querySelectorAll root "p"
      length results @?= 2

  , testCase "querySelector by class" $ do
      let root = documentElement simpleDoc
          result = querySelector root ".foo"
      isJust result @? "should find .foo"
      tagName (fromJust result) @?= Just "div"

  , testCase "querySelector by id" $ do
      let root = documentElement simpleDoc
          result = querySelector root "#main"
      isJust result @? "should find #main"
      tagName (fromJust result) @?= Just "div"

  , testCase "querySelector tag.class" $ do
      let root = documentElement simpleDoc
          result = querySelector root "div.foo"
      isJust result @? "should find div.foo"

  , testCase "querySelector no match" $ do
      let root = documentElement simpleDoc
      querySelector root ".nonexistent" @?= Nothing

  , testCase "querySelectorAll wildcard" $ do
      let results = querySelectorAll mainDiv "*"
      length results >= 2 @? "should match at least div + children"

  , testCase "querySelector descendant combinator" $ do
      let root = documentElement simpleDoc
          result = querySelector root "div p"
      isJust result @? "should find div p"
      textContent (fromJust result) @?= "Hello"

  , testCase "selector result preserves navigation context" $ do
      let root = documentElement simpleDoc
          result = fromJust (querySelector root "p")
      isJust (parentNode result) @? "selector result should have parent"
      tagName (fromJust (parentNode result)) @?= Just "div"
  ]

-- A richer document for testing the full breadth of CSS selectors.
selectorDoc :: Document
selectorDoc = parseDocument
  "<!DOCTYPE html>\
  \<html lang=\"en\" dir=\"ltr\">\
  \<head><title>Selector Tests</title></head>\
  \<body>\
  \<div id=\"container\" class=\"wrapper main\">\
  \  <h1>Title</h1>\
  \  <p class=\"intro\">Intro</p>\
  \  <ul>\
  \    <li class=\"item active\">One</li>\
  \    <li class=\"item\">Two</li>\
  \    <li class=\"item\">Three</li>\
  \    <li class=\"item last\">Four</li>\
  \  </ul>\
  \  <form>\
  \    <input type=\"text\" name=\"user\" required=\"\" placeholder=\"Name\">\
  \    <input type=\"email\" name=\"email\" placeholder=\"Email\" value=\"test@example.com\">\
  \    <input type=\"checkbox\" name=\"agree\" checked=\"\">\
  \    <input type=\"radio\" name=\"choice\" value=\"a\">\
  \    <input type=\"hidden\" name=\"token\" value=\"abc\">\
  \    <textarea readonly=\"\">Notes</textarea>\
  \    <select name=\"color\">\
  \      <option>Red</option>\
  \      <option selected=\"\">Blue</option>\
  \    </select>\
  \    <button type=\"submit\">Go</button>\
  \    <input type=\"submit\" value=\"Submit\" disabled=\"\">\
  \  </form>\
  \  <fieldset disabled=\"\">\
  \    <legend><input type=\"text\" name=\"legend-input\" placeholder=\"Legend\"></legend>\
  \    <input type=\"text\" name=\"fs-inner\" placeholder=\"Inside\">\
  \  </fieldset>\
  \  <div class=\"empty\"></div>\
  \  <div class=\"blank\">  \n  </div>\
  \  <div class=\"has-comment\"><!-- comment --></div>\
  \  <span lang=\"fr\">Bonjour</span>\
  \  <span lang=\"en-US\">Hello</span>\
  \  <div dir=\"rtl\"><span>RTL content</span></div>\
  \</div>\
  \</body></html>"

selRoot :: Node
selRoot = documentElement selectorDoc

qsa :: Text -> [Node]
qsa sel = querySelectorAll selRoot sel

qsaLen :: Text -> Int
qsaLen sel = length (qsa sel)

qs :: Text -> Maybe Node
qs sel = querySelector selRoot sel

extendedSelectorTests :: [TestTree]
extendedSelectorTests =
  -- === Structural pseudo-classes ===
  [ testCase ":first-child" $ do
      qsaLen "li:first-child" @?= 1
      textContent (head (qsa "li:first-child")) @?= "One"

  , testCase ":last-child" $ do
      qsaLen "li:last-child" @?= 1
      textContent (head (qsa "li:last-child")) @?= "Four"

  , testCase ":only-child" $ do
      qsaLen "ul:only-child" @?= 0
      qsaLen "h1:only-child" @?= 0

  , testCase ":nth-child(odd)" $ do
      let items = qsa "li:nth-child(odd)"
      length items @?= 2
      textContent (head items) @?= "One"

  , testCase ":nth-child(even)" $ do
      let items = qsa "li:nth-child(even)"
      length items @?= 2
      textContent (head items) @?= "Two"

  , testCase ":nth-child(2n+1)" $ do
      qsaLen "li:nth-child(2n+1)" @?= 2

  , testCase ":nth-last-child(1)" $ do
      qsaLen "li:nth-last-child(1)" @?= 1
      textContent (head (qsa "li:nth-last-child(1)")) @?= "Four"

  , testCase ":first-of-type" $ do
      qsaLen "li:first-of-type" @?= 1

  , testCase ":last-of-type" $ do
      qsaLen "li:last-of-type" @?= 1

  , testCase ":nth-of-type(2)" $ do
      qsaLen "li:nth-of-type(2)" @?= 1
      textContent (head (qsa "li:nth-of-type(2)")) @?= "Two"

  , testCase ":only-of-type" $ do
      qsaLen "h1:only-of-type" @?= 1
      qsaLen "li:only-of-type" @?= 0

  , testCase ":root" $ do
      let Right sel = Sel.parseSelector ":root"
          roots = querySelectorAllDoc sel selectorDoc
      length roots @?= 1
      tagName (head roots) @?= Just "html"

  , testCase ":empty" $ do
      let empties = qsa "div:empty"
      length empties >= 1 @? "should find the empty div"

  , testCase ":blank" $ do
      qsaLen "div.blank:blank" @?= 1
      qsaLen "div.empty:blank" @?= 1
      qsaLen "div.has-comment:blank" @?= 1

  -- === :nth-child(An+B of S) ===
  , testCase ":nth-child(1 of .item)" $ do
      qsaLen "li:nth-child(1 of .item)" @?= 1
      textContent (head (qsa "li:nth-child(1 of .item)")) @?= "One"

  , testCase ":nth-child(2 of .item)" $ do
      qsaLen ":nth-child(2 of .item)" @?= 1
      textContent (head (qsa ":nth-child(2 of .item)")) @?= "Two"

  , testCase ":nth-last-child(1 of .item)" $ do
      qsaLen ":nth-last-child(1 of .item)" @?= 1
      textContent (head (qsa ":nth-last-child(1 of .item)")) @?= "Four"

  -- === Logical pseudo-classes ===
  , testCase ":not(.item)" $ do
      let items = qsa "li:not(.active)"
      length items @?= 3

  , testCase ":is(.intro, .item)" $ do
      qsaLen ":is(.intro, .item)" @?= 5

  , testCase ":where(.intro)" $ do
      qsaLen ":where(.intro)" @?= 1

  , testCase ":is() with invalid branch (forgiving)" $ do
      qsaLen ":is(.intro, ::fake, .item)" @?= 5

  , testCase ":has(> li)" $ do
      qsaLen "ul:has(> li)" @?= 1

  , testCase ":has(.item)" $ do
      qsaLen "ul:has(.item)" @?= 1

  , testCase ":has(+ .last)" $ do
      qsaLen "li:has(+ .last)" @?= 1
      textContent (head (qsa "li:has(+ .last)")) @?= "Three"

  , testCase ":has(~ .last)" $ do
      qsaLen "li.active:has(~ .last)" @?= 1

  -- === :scope ===
  , testCase ":scope" $ do
      let Right sel = Sel.parseSelector ":scope"
          scopes = querySelectorAllDoc sel selectorDoc
      length scopes @?= 1
      tagName (head scopes) @?= Just "html"

  -- === :defined ===
  , testCase ":defined matches all elements" $ do
      qsaLen "li:defined" @?= 4

  -- === :dir() ===
  , testCase ":dir(ltr)" $ do
      let ltrSpans = qsa "span:dir(ltr)"
      length ltrSpans >= 1 @? "should find ltr spans"

  , testCase ":dir(rtl)" $ do
      qsaLen "span:dir(rtl)" @?= 1

  -- === :lang() ===
  , testCase ":lang(en)" $ do
      let enNodes = qsa ":lang(en)"
      length enNodes >= 1 @? "should match elements inheriting lang=en"

  , testCase ":lang(fr)" $ do
      qsaLen "span:lang(fr)" @?= 1
      textContent (head (qsa "span:lang(fr)")) @?= "Bonjour"

  , testCase ":lang(en, fr) multi-argument" $ do
      qsaLen "span:lang(en, fr)" @?= 3

  -- === Form pseudo-classes ===
  , testCase ":enabled / :disabled" $ do
      qsaLen "input:enabled" @?= 6
      qsaLen "input:disabled" @?= 2

  , testCase ":checked" $ do
      qsaLen "input:checked" @?= 1
      qsaLen "option:checked" @?= 1

  , testCase ":required / :optional" $ do
      qsaLen "input:required" @?= 1
      qsaLen "input:optional" @?= 7

  , testCase ":read-only / :read-write" $ do
      qsaLen "textarea:read-only" @?= 1
      qsaLen "input[type=text]:read-write" @?= 2

  , testCase ":placeholder-shown" $
      qsaLen "input:placeholder-shown" @?= 3

  , testCase ":indeterminate" $ do
      qsaLen "input:indeterminate" @?= 1

  , testCase ":default" $ do
      let defaults = qsa ":default"
      length defaults >= 1 @? "should find default button/submit"

  -- === Fieldset disabled inheritance ===
  , testCase "fieldset disabled inherits to descendants" $ do
      qsaLen "input[name=fs-inner]:disabled" @?= 1
      qsaLen "input[name=fs-inner]:enabled" @?= 0

  , testCase "first legend child exemption" $ do
      qsaLen "input[name=legend-input]:disabled" @?= 0
      qsaLen "input[name=legend-input]:enabled" @?= 1

  , testCase "fieldset itself matches :disabled" $ do
      qsaLen "fieldset:disabled" @?= 1

  -- === HTML attribute case-insensitivity ===
  , testCase "[type=text] matches case-insensitively" $ do
      let doc' = parseDocument
            "<html><body><input type=\"TEXT\"><input type=\"text\"></body></html>"
          root' = documentElement doc'
      length (querySelectorAll root' "input[type=text]") @?= 2

  , testCase "[type=text s] forces case-sensitive" $ do
      let doc' = parseDocument
            "<html><body><input type=\"TEXT\"><input type=\"text\"></body></html>"
          root' = documentElement doc'
      length (querySelectorAll root' "input[type=text s]") @?= 1

  -- === Dynamic pseudo-classes (always false in static DOM) ===
  , testCase ":hover never matches" $
      qsaLen ":hover" @?= 0

  , testCase ":focus never matches" $
      qsaLen ":focus" @?= 0

  , testCase ":visited never matches" $
      qsaLen ":visited" @?= 0

  -- === querySelector document-order correctness ===
  , testCase "querySelector returns earliest match across comma branches" $ do
      let result = qs "li.last, li.active"
      isJust result @? "should find a match"
      textContent (fromJust result) @?= "One"

  -- === Combinators ===
  , testCase "descendant combinator" $
      qsaLen "ul li" @?= 4

  , testCase "child combinator" $
      qsaLen "ul > li" @?= 4

  , testCase "adjacent sibling" $ do
      qsaLen "h1 + p" @?= 1
      textContent (head (qsa "h1 + p")) @?= "Intro"

  , testCase "general sibling" $
      qsaLen "h1 ~ ul" @?= 1
  ]

incrementalTests :: [TestTree]
incrementalTests =
  [ testCase "single chunk matches one-shot" $ do
      let input = "<!DOCTYPE html><html><body><p>Hi</p></body></html>"
          oneShot = parseDocument input
      p <- newParser
      feedParser p input
      doc <- finishParser p
      let osRoot = documentElement oneShot
          incRoot = documentElement doc
      tagName osRoot @?= tagName incRoot
      textContent osRoot @?= textContent incRoot

  , testCase "multiple chunks produce same result" $ do
      let full = "<!DOCTYPE html><html><head><title>T</title></head><body><div>Content</div></body></html>"
          oneShot = parseDocument full
      p <- newParser
      feedParser p "<!DOCTYPE html><html><he"
      feedParser p "ad><title>T</title></head>"
      feedParser p "<body><div>Content</div>"
      feedParser p "</body></html>"
      doc <- finishParser p
      let osRoot = documentElement oneShot
          incRoot = documentElement doc
      textContent osRoot @?= textContent incRoot
      let osKids = childNodes osRoot
          incKids = childNodes incRoot
      length osKids @?= length incKids

  , testCase "empty parser produces valid document" $ do
      p <- newParser
      doc <- finishParser p
      let root = documentElement doc
      tagName root @?= Just "html"

  , testCase "byte-at-a-time feeding" $ do
      let full = "<p>Hello</p>"
          oneShot = parseDocument full
      p <- newParser
      mapM_ (\b -> feedParser p (BS.singleton b)) (BS.unpack full)
      doc <- finishParser p
      textContent (documentElement doc) @?= textContent (documentElement oneShot)
  ]


-- ---------------------------------------------------------------------------
-- Streaming tree event tests
-- ---------------------------------------------------------------------------

collectEvents :: IO (Step TreeEvent) -> IO [TreeEvent]
collectEvents act = do
  s <- act
  pure (stepToList s)
  where
    stepToList Done = []
    stepToList (Yield evt rest) = evt : stepToList rest

streamingTests :: [TestTree]
streamingTests =
  [ testCase "streamHTML emits open/close for simple doc" $ do
      evts <- collectEvents (streamHTML "<p>hi</p>")
      let opens = [t | TreeOpen t _ <- evts]
          closes = [t | TreeClose t <- evts]
          texts = [t | TreeText t <- evts]
      assertBool "has <html>" ("html" `elem` opens)
      assertBool "has <head>" ("head" `elem` opens)
      assertBool "has <body>" ("body" `elem` opens)
      assertBool "has <p>" ("p" `elem` opens)
      assertBool "closes <html>" ("html" `elem` closes)
      assertBool "closes <p>" ("p" `elem` closes)
      assertBool "has text 'hi'" ("hi" `elem` texts)

  , testCase "streamHTML emits doctype" $ do
      evts <- collectEvents (streamHTML "<!DOCTYPE html><html><body></body></html>")
      let doctypes = [n | TreeDoctype n _ _ <- evts]
      assertBool "has doctype" (not (null doctypes))

  , testCase "streamHTML events have correct nesting order" $ do
      evts <- collectEvents (streamHTML "<div><span>x</span></div>")
      let relevant = filter isStructural evts
      case dropWhile (not . isOpenTag "div") relevant of
        (TreeOpen "div" _ : rest) ->
          case dropWhile (not . isOpenTag "span") rest of
            (TreeOpen "span" _ : rest2) -> do
              assertBool "text before span close" (any isText (takeWhile (not . isCloseTag "span") rest2))
              let afterSpan = dropWhile (not . isCloseTag "span") rest2
              assertBool "span closes" (not (null afterSpan))
              let afterSpanClose = drop 1 afterSpan
              assertBool "div closes after span" (any (isCloseTag "div") afterSpanClose)
            _ -> assertFailure "no span open after div"
        _ -> assertFailure "no div open"

  , testCase "streamHTML with void element emits open+close" $ do
      evts <- collectEvents (streamHTML "<div><br></div>")
      let opens = [t | TreeOpen t _ <- evts]
          closes = [t | TreeClose t <- evts]
      assertBool "br opens" ("br" `elem` opens)
      assertBool "br closes" ("br" `elem` closes)

  , testCase "streamHTML preserves attributes" $ do
      evts <- collectEvents (streamHTML "<div class=\"foo\" id=\"bar\">x</div>")
      let divOpens = [() | TreeOpen "div" _ <- evts]
      assertBool "has div open event" (not (null divOpens))

  , testCase "incremental streaming matches one-shot" $ do
      let html = "<html><body><div><p>hello</p><p>world</p></div></body></html>"
      oneShotEvts <- collectEvents (streamHTML html)
      sp <- newStreamParser
      evts1 <- collectEvents (feedChunk sp "<html><body><div>")
      evts2 <- collectEvents (feedChunk sp "<p>hello</p><p>world</p>")
      evts3 <- collectEvents (feedChunk sp "</div></body></html>")
      evtsFinal <- collectEvents (finishStream sp)
      let incrEvts = evts1 ++ evts2 ++ evts3 ++ evtsFinal
      length (filter isOpen incrEvts) @?= length (filter isOpen oneShotEvts)
      length (filter isClose incrEvts) @?= length (filter isClose oneShotEvts)
  ]
  where
    isStructural (TreeOpen _ _) = True
    isStructural (TreeClose _) = True
    isStructural (TreeText _) = True
    isStructural _ = False
    isOpenTag n (TreeOpen t _) = t == n
    isOpenTag _ _ = False
    isCloseTag n (TreeClose t) = t == n
    isCloseTag _ _ = False
    isText (TreeText _) = True
    isText _ = False
    isOpen (TreeOpen _ _) = True
    isOpen _ = False
    isClose (TreeClose _) = True
    isClose _ = False
