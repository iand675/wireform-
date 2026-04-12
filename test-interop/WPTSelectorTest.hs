{-# LANGUAGE OverloadedStrings #-}
-- | W3C Web Platform Tests (WPT) conformance for CSS Selectors.
--
-- Ported from https://github.com/web-platform-tests/wpt/tree/master/css/selectors
-- Only tests that are meaningful for a static HTML DOM (no dynamic DOM
-- manipulation, no getComputedStyle, no shadow DOM) are included.
module Main (main) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty
import Test.Tasty.HUnit

import HTML.DOM

main :: IO ()
main = defaultMain $ testGroup "WPT CSS Selectors Conformance"
  [ testGroup "has-basic" hasBasicTests
  , testGroup "has-relative-argument" hasRelativeTests
  , testGroup "is-where-basic" isWhereBasicTests
  , testGroup "is-where-not" isWhereNotTests
  , testGroup "not-complex" notComplexTests
  , testGroup "child-indexed-pseudo-class" childIndexedTests
  , testGroup "first-child" firstChildTests
  , testGroup "last-child" lastChildTests
  , testGroup "only-child" onlyChildTests
  , testGroup "first-of-type" firstOfTypeTests
  , testGroup "last-of-type" lastOfTypeTests
  , testGroup "only-of-type" onlyOfTypeTests
  , testGroup "dir-selector-querySelector" dirSelectorTests
  , testGroup "pseudo-enabled-disabled" enabledDisabledTests
  , testGroup "has-argument-with-explicit-scope" hasScopeTests
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Parse a document fragment inside a <main id=main>, return the main node.
-- The WPT tests query from a #main element.
parseMain :: Text -> Node
parseMain html =
  let doc = parseDocument (TE.encodeUtf8 ("<!DOCTYPE html><html><body>" <> html <> "</body></html>"))
      root = documentElement doc
  in case findById "main" root of
       Just n -> n
       Nothing -> error "parseMain: no #main found"

findById :: Text -> Node -> Maybe Node
findById targetId node =
  case getAttribute node "id" of
    Just i | i == targetId -> Just node
    _ -> firstJust (\c -> findById targetId c) (childNodes node)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
  Just r  -> Just r
  Nothing -> firstJust f xs

-- | querySelectorAll from a node, return sorted IDs.
qsaIds :: Node -> Text -> [Text]
qsaIds root sel = sort' (mapMaybe (\n -> getAttribute n "id") (querySelectorAll root sel))

sort' :: [Text] -> [Text]
sort' [] = []
sort' (x:xs) = sort' [y | y <- xs, y < x] ++ [x] ++ sort' [y | y <- xs, y >= x]

-- | Assert that querySelectorAll returns elements with the given sorted IDs.
assertQSA :: Node -> Text -> [Text] -> Assertion
assertQSA root sel expected =
  let actual = qsaIds root sel
  in actual @?= sort' expected

-- | Assert querySelector returns element with given ID (or Nothing).
assertQS :: Node -> Text -> Maybe Text -> Assertion
assertQS root sel expected =
  let actual = querySelector root sel >>= \n -> getAttribute n "id"
  in actual @?= expected

-- | Assert that node.matches(selector) == expected.
assertMatches :: Node -> Text -> Bool -> Assertion
assertMatches node sel expected =
  matches node sel @?= expected

-- | Assert that node.closest(selector) returns element with given ID.
assertClosest :: Node -> Text -> Maybe Text -> Assertion
assertClosest node sel expected =
  let actual = closest node sel >>= \n -> getAttribute n "id"
  in actual @?= expected

-- | WPT test helper: (selector, expected_ids) -> TestTree
wptQSA :: Node -> (Text, [Text]) -> TestTree
wptQSA root (sel, ids) = testCase (T.unpack sel) $ assertQSA root sel ids

-- | WPT test helper: (selector, expected_id_or_nothing) -> TestTree for querySelector
wptQS :: Node -> (Text, Maybe Text) -> TestTree
wptQS root (sel, mid) = testCase (T.unpack sel) $ assertQS root sel mid

-- ---------------------------------------------------------------------------
-- has-basic.html
-- ---------------------------------------------------------------------------

hasBasicDoc :: Node
hasBasicDoc = parseMain
  "<main id=main>\
  \  <div id=a class=\"ancestor\">\
  \    <div id=b class=\"parent ancestor\">\
  \      <div id=c class=\"sibling descendant\">\
  \        <div id=d class=\"descendant\"></div>\
  \      </div>\
  \      <div id=e class=\"target descendant\"></div>\
  \    </div>\
  \    <div id=f class=\"parent ancestor\">\
  \      <div id=g class=\"target descendant\"></div>\
  \    </div>\
  \    <div id=h class=\"parent ancestor\">\
  \      <div id=i class=\"target descendant\"></div>\
  \      <div id=j class=\"sibling descendant\">\
  \        <div id=k class=\"descendant\"></div>\
  \      </div>\
  \    </div>\
  \  </div>\
  \</main>"

hasBasicTests :: [TestTree]
hasBasicTests = fmap (wptQSA hasBasicDoc)
  [ (":has(#a)", [])
  , (":has(.ancestor)", ["a"])
  , (":has(.target)", ["a","b","f","h"])
  , (":has(.descendant)", ["a","b","c","f","h","j"])
  , (".parent:has(.target)", ["b","f","h"])
  , (":has(.sibling ~ .target)", ["a","b"])
  , (".parent:has(.sibling ~ .target)", ["b"])
  , (".sibling:has(.descendant) ~ .target", ["e"])
  , (":has(> .parent)", ["a"])
  , (":has(> .target)", ["b","f","h"])
  , (":has(> .parent, > .target)", ["a","b","f","h"])
  , (":has(+ #h)", ["f"])
  , (".parent:has(~ #h)", ["b","f"])
  ]

-- ---------------------------------------------------------------------------
-- has-relative-argument.html
-- ---------------------------------------------------------------------------

hasRelDoc :: Node
hasRelDoc = parseMain
  "<main id=main>\
  \ <div id=d01>\
  \  <div id=d02 class=\"x\">\
  \    <div id=d03 class=\"a\"></div>\
  \    <div id=d04></div>\
  \    <div id=d05 class=\"b\"></div>\
  \  </div>\
  \  <div id=d06 class=\"x\">\
  \    <div id=d07 class=\"x\">\
  \      <div id=d08 class=\"a\"></div>\
  \    </div>\
  \  </div>\
  \  <div id=d09 class=\"x\">\
  \    <div id=d10 class=\"a\">\
  \      <div id=d11 class=\"b\"></div>\
  \    </div>\
  \  </div>\
  \  <div id=d12 class=\"x\">\
  \    <div id=d13 class=\"a\">\
  \      <div id=d14>\
  \        <div id=d15 class=\"b\"></div>\
  \      </div>\
  \    </div>\
  \    <div id=d16 class=\"b\"></div>\
  \  </div>\
  \ </div>\
  \ <div id=d17>\
  \  <div id=d18 class=\"x\"></div>\
  \  <div id=d19 class=\"x\"></div>\
  \  <div id=d20 class=\"a\"></div>\
  \  <div id=d21 class=\"x\"></div>\
  \  <div id=d22 class=\"a\">\
  \   <div id=d23 class=\"b\"></div>\
  \  </div>\
  \  <div id=d24 class=\"x\"></div>\
  \  <div id=d25 class=\"a\">\
  \   <div id=d26>\
  \    <div id=d27 class=\"b\"></div>\
  \   </div>\
  \  </div>\
  \  <div id=d28 class=\"x\"></div>\
  \  <div id=d29 class=\"a\"></div>\
  \  <div id=d30 class=\"b\">\
  \   <div id=d31 class=\"c\"></div>\
  \  </div>\
  \  <div id=d32 class=\"x\"></div>\
  \  <div id=d33 class=\"a\"></div>\
  \  <div id=d34 class=\"b\">\
  \   <div id=d35>\
  \    <div id=d36 class=\"c\"></div>\
  \   </div>\
  \  </div>\
  \  <div id=d37 class=\"x\"></div>\
  \  <div id=d38 class=\"a\"></div>\
  \  <div id=d39 class=\"b\"></div>\
  \  <div id=d40 class=\"x\"></div>\
  \  <div id=d41 class=\"a\"></div>\
  \  <div id=d42></div>\
  \  <div id=d43 class=\"b\">\
  \   <div id=d44 class=\"x\">\
  \    <div id=d45 class=\"c\"></div>\
  \   </div>\
  \  </div>\
  \  <div id=d46 class=\"x\"></div>\
  \  <div id=d47 class=\"a\"></div>\
  \ </div>\
  \</main>"

hasRelativeTests :: [TestTree]
hasRelativeTests = fmap (wptQSA hasRelDoc)
  [ (".x:has(.a)", ["d02","d06","d07","d09","d12"])
  , (".x:has(.a > .b)", ["d09"])
  , (".x:has(.a .b)", ["d09","d12"])
  , (".x:has(.a + .b)", ["d12"])
  , (".x:has(.a ~ .b)", ["d02","d12"])
  , (".x:has(> .a)", ["d02","d07","d09","d12"])
  , (".x:has(> .a > .b)", ["d09"])
  , (".x:has(> .a .b)", ["d09","d12"])
  , (".x:has(> .a + .b)", ["d12"])
  , (".x:has(> .a ~ .b)", ["d02","d12"])
  , (".x:has(+ .a)", ["d19","d21","d24","d28","d32","d37","d40","d46"])
  , (".x:has(+ .a > .b)", ["d21"])
  , (".x:has(+ .a .b)", ["d21","d24"])
  , (".x:has(+ .a + .b)", ["d28","d32","d37"])
  , (".x:has(+ .a ~ .b)", ["d19","d21","d24","d28","d32","d37","d40"])
  , (".x:has(~ .a)", ["d18","d19","d21","d24","d28","d32","d37","d40","d46"])
  , (".x:has(~ .a > .b)", ["d18","d19","d21"])
  , (".x:has(~ .a .b)", ["d18","d19","d21","d24"])
  , (".x:has(~ .a + .b)", ["d18","d19","d21","d24","d28","d32","d37"])
  , (".x:has(~ .a + .b > .c)", ["d18","d19","d21","d24","d28"])
  , (".x:has(~ .a + .b .c)", ["d18","d19","d21","d24","d28","d32"])
  ]

-- ---------------------------------------------------------------------------
-- is-where-basic.html
-- ---------------------------------------------------------------------------

isWhereDoc :: Node
isWhereDoc = parseMain
  "<main id=main>\
  \  <div id=a><div id=d></div></div>\
  \  <div id=b><div id=e></div></div>\
  \  <div id=c><div id=f></div></div>\
  \</main>"

isWhereBasicTests :: [TestTree]
isWhereBasicTests = fmap (wptQSA isWhereDoc)
  [ (":is()", [])
  , (":is(#a)", ["a"])
  , (":is(#a, #f)", ["a","f"])
  , (":is(#a, #c) :where(#a #d, #c #f)", ["d","f"])
  , ("#c > :is(#c > #f)", ["f"])
  , ("#c > :is(#b > #f)", [])
  , ("#a div:is(#d)", ["d"])
  , (":is(div) > div", ["d","e","f"])
  , (":is(*) > div", ["a","b","c","d","e","f"])
  , (":is(*) div", ["a","b","c","d","e","f"])
  , ("div > :where(#e, #f)", ["e","f"])
  , ("div > :where(*)", ["d","e","f"])
  , (":is(*) > :where(*)", ["a","b","c","d","e","f"])
  , (":is(#a + #b) + :is(#c)", ["c"])
  , (":is(#a, #b) + div", ["b","c"])
  ]

-- ---------------------------------------------------------------------------
-- is-where-not.html
-- ---------------------------------------------------------------------------

isWhereNotTests :: [TestTree]
isWhereNotTests = fmap (wptQSA isWhereDoc)
  [ (":not(:is(#a))", ["b","c","d","e","f"])
  , (":not(:where(#b))", ["a","c","d","e","f"])
  , (":not(:where(:root #c))", ["a","b","d","e","f"])
  , (":not(:is(#a, #b))", ["c","d","e","f"])
  , (":not(:is(#b div))", ["a","b","c","d","f"])
  , (":not(:is(#a div, div + div))", ["a","e","f"])
  , (":not(:is(span))", ["a","b","c","d","e","f"])
  , (":not(:is(div))", [])
  , (":not(:is(*|div))", [])
  , (":not(:is(*|*))", [])
  , (":not(:is(*))", [])
  , (":not(:is(:not(div)))", ["a","b","c","d","e","f"])
  , (":not(:is(span, b, i))", ["a","b","c","d","e","f"])
  , (":not(:is(span, b, i, div))", [])
  , (":not(:is(#b ~ div div, * + #c))", ["a","b","d","e"])
  , (":not(:is(div > :not(#e)))", ["a","b","c","e"])
  , (":not(:is(div > :not(:where(#e, #f))))", ["a","b","c","e","f"])
  ]

-- ---------------------------------------------------------------------------
-- not-complex.html
-- ---------------------------------------------------------------------------

notComplexTests :: [TestTree]
notComplexTests = fmap (wptQSA isWhereDoc)
  [ (":not(#a)", ["b","c","d","e","f"])
  , (":not(#a #d)", ["a","b","c","e","f"])
  , (":not(#b div)", ["a","b","c","d","f"])
  , (":not(div div)", ["a","b","c"])
  , (":not(div + div)", ["a","d","e","f"])
  , (":not(main > div)", ["d","e","f"])
  , (":not(#a, #b)", ["c","d","e","f"])
  , (":not(#f, main > div)", ["d","e"])
  , (":not(div + div + div, div + div > div)", ["a","b","d"])
  , (":not(div:nth-child(1))", ["b","c"])
  , (":not(:not(div))", ["a","b","c","d","e","f"])
  , (":not(:not(:not(div)))", [])
  , (":not(div, span)", [])
  , (":not(span, p)", ["a","b","c","d","e","f"])
  , (":not(#unknown, .unknown)", ["a","b","c","d","e","f"])
  , (":not(#unknown > div, span)", ["a","b","c","d","e","f"])
  , (":not(#unknown ~ div, span)", ["a","b","c","d","e","f"])
  , (":not(:hover div)", ["a","b","c","d","e","f"])
  , (":not(:link div)", ["a","b","c","d","e","f"])
  , (":not(:visited div)", ["a","b","c","d","e","f"])
  ]

-- ---------------------------------------------------------------------------
-- child-indexed-pseudo-class.html
-- ---------------------------------------------------------------------------

childIndexedTests :: [TestTree]
childIndexedTests =
  let doc = parseDocument "<!DOCTYPE html><html><body></body></html>"
      root = documentElement doc
  in [ testCase ":first-child on <html>" $
         assertMatches root ":first-child" True
     , testCase ":last-child on <html>" $
         assertMatches root ":last-child" True
     , testCase ":only-child on <html>" $
         assertMatches root ":only-child" True
     , testCase ":first-of-type on <html>" $
         assertMatches root ":first-of-type" True
     , testCase ":last-of-type on <html>" $
         assertMatches root ":last-of-type" True
     , testCase ":only-of-type on <html>" $
         assertMatches root ":only-of-type" True
     , testCase ":nth-child(1) on <html>" $
         assertMatches root ":nth-child(1)" True
     , testCase ":nth-child(n) on <html>" $
         assertMatches root ":nth-child(n)" True
     , testCase ":nth-last-child(1) on <html>" $
         assertMatches root ":nth-last-child(1)" True
     , testCase ":nth-last-child(n) on <html>" $
         assertMatches root ":nth-last-child(n)" True
     , testCase ":nth-of-type(1) on <html>" $
         assertMatches root ":nth-of-type(1)" True
     , testCase ":nth-of-type(n) on <html>" $
         assertMatches root ":nth-of-type(n)" True
     , testCase ":nth-last-of-type(1) on <html>" $
         assertMatches root ":nth-last-of-type(1)" True
     , testCase ":nth-last-of-type(n) on <html>" $
         assertMatches root ":nth-last-of-type(n)" True
     , testCase ":nth-child(2) on <html>" $
         assertMatches root ":nth-child(2)" False
     , testCase ":nth-last-child(2) on <html>" $
         assertMatches root ":nth-last-child(2)" False
     , testCase ":nth-of-type(2) on <html>" $
         assertMatches root ":nth-of-type(2)" False
     , testCase ":nth-last-of-type(2) on <html>" $
         assertMatches root ":nth-last-of-type(2)" False
     ]

-- ---------------------------------------------------------------------------
-- first-child.html
-- ---------------------------------------------------------------------------

firstChildDoc :: Node
firstChildDoc = documentElement $ parseDocument
  "<!DOCTYPE html><html><body>\
  \<div>\
  \  <div id=\"target1\">Whitespace nodes should be ignored.</div>\
  \</div>\
  \<div>\
  \  <div id=\"target2\">There is the second child element.</div>\
  \  <blockquote></blockquote>\
  \</div>\
  \<div>\
  \  <!-- -->\
  \  <div id=\"target3\">A comment node should be ignored.</div>\
  \</div>\
  \<div>\
  \  .\
  \  <div id=\"target4\">Non-whitespace text node should be ignored.</div>\
  \</div>\
  \<div>\
  \  <blockquote></blockquote>\
  \  <div id=\"target5\">The second child should not be matched.</div>\
  \</div>\
  \</body></html>"

firstChildTests :: [TestTree]
firstChildTests =
  [ testCase "target1 :first-child" $ assertMatches (fid firstChildDoc "target1") ":first-child" True
  , testCase "target2 :first-child" $ assertMatches (fid firstChildDoc "target2") ":first-child" True
  , testCase "target3 :first-child" $ assertMatches (fid firstChildDoc "target3") ":first-child" True
  , testCase "target4 :first-child" $ assertMatches (fid firstChildDoc "target4") ":first-child" True
  , testCase "target5 :first-child" $ assertMatches (fid firstChildDoc "target5") ":first-child" False
  ]

-- ---------------------------------------------------------------------------
-- last-child.html
-- ---------------------------------------------------------------------------

lastChildDoc :: Node
lastChildDoc = documentElement $ parseDocument
  "<!DOCTYPE html><html><body>\
  \<div>\
  \  <div id=\"target1\">Whitespace nodes should be ignored.</div>\
  \</div>\
  \<div>\
  \  <blockquote></blockquote>\
  \  <div id=\"target2\">There is the second child element.</div>\
  \</div>\
  \<div>\
  \  <div id=\"target3\">A comment node should be ignored.</div>\
  \  <!-- -->\
  \</div>\
  \<div>\
  \  <div id=\"target4\">Non-whitespace text node should be ignored.</div>\
  \  .\
  \</div>\
  \<div>\
  \  <div id=\"target5\">The first child should not be matched.</div>\
  \  <blockquote></blockquote>\
  \</div>\
  \</body></html>"

lastChildTests :: [TestTree]
lastChildTests =
  [ testCase "target1 :last-child" $ assertMatches (fid lastChildDoc "target1") ":last-child" True
  , testCase "target2 :last-child" $ assertMatches (fid lastChildDoc "target2") ":last-child" True
  , testCase "target3 :last-child" $ assertMatches (fid lastChildDoc "target3") ":last-child" True
  , testCase "target4 :last-child" $ assertMatches (fid lastChildDoc "target4") ":last-child" True
  , testCase "target5 :last-child" $ assertMatches (fid lastChildDoc "target5") ":last-child" False
  ]

-- ---------------------------------------------------------------------------
-- only-child.html (static cases only)
-- ---------------------------------------------------------------------------

onlyChildDoc :: Node
onlyChildDoc = documentElement $ parseDocument
  "<!DOCTYPE html><html><body>\
  \<div><div id=\"target1\"></div></div>\
  \<div><!-- --><div id=\"target2\"></div></div>\
  \<div>text<div id=\"target3\"></div></div>\
  \<div><div id=\"target4\"></div><span></span></div>\
  \</body></html>"

onlyChildTests :: [TestTree]
onlyChildTests =
  [ testCase "target1 :only-child" $ assertMatches (fid onlyChildDoc "target1") ":only-child" True
  , testCase "target2 :only-child" $ assertMatches (fid onlyChildDoc "target2") ":only-child" True
  , testCase "target3 :only-child" $ assertMatches (fid onlyChildDoc "target3") ":only-child" True
  , testCase "target4 :only-child" $ assertMatches (fid onlyChildDoc "target4") ":only-child" False
  ]

-- ---------------------------------------------------------------------------
-- first-of-type.html (static cases)
-- ---------------------------------------------------------------------------

firstOfTypeDoc :: Node
firstOfTypeDoc = documentElement $ parseDocument
  "<!DOCTYPE html><html><body>\
  \<div><div id=\"target1\"></div></div>\
  \<div><span></span><div id=\"target2\"></div></div>\
  \<div>text<div id=\"target3\"></div></div>\
  \<div><!-- --><div id=\"target4\"></div></div>\
  \<div><div id=\"target5\"></div><div></div></div>\
  \<div><div></div><span></span><div id=\"target6\"></div></div>\
  \<div><div></div><div id=\"target7\"></div></div>\
  \<div><blockquote></blockquote><div></div><div id=\"target8\"></div></div>\
  \</body></html>"

firstOfTypeTests :: [TestTree]
firstOfTypeTests =
  [ testCase "target1 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target1") "div:first-of-type" True
  , testCase "target2 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target2") "div:first-of-type" True
  , testCase "target3 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target3") "div:first-of-type" True
  , testCase "target4 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target4") "div:first-of-type" True
  , testCase "target5 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target5") "div:first-of-type" True
  , testCase "target6 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target6") "div:first-of-type" False
  , testCase "target7 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target7") "div:first-of-type" False
  , testCase "target8 div:first-of-type" $ assertMatches (fid firstOfTypeDoc "target8") "div:first-of-type" False
  ]

-- ---------------------------------------------------------------------------
-- last-of-type.html (static cases)
-- ---------------------------------------------------------------------------

lastOfTypeDoc :: Node
lastOfTypeDoc = documentElement $ parseDocument
  "<!DOCTYPE html><html><body>\
  \<div><div id=\"target1\"></div></div>\
  \<div><div id=\"target2\"></div><span></span></div>\
  \<div><div id=\"target3\"></div>text</div>\
  \<div><div id=\"target4\"></div><!-- --></div>\
  \<div><div></div><div id=\"target5\"></div></div>\
  \<div><div id=\"target6\"></div><span></span><div></div></div>\
  \<div><div id=\"target7\"></div><div></div></div>\
  \<div><div id=\"target8\"></div><div></div><blockquote></blockquote></div>\
  \</body></html>"

lastOfTypeTests :: [TestTree]
lastOfTypeTests =
  [ testCase "target1 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target1") "div:last-of-type" True
  , testCase "target2 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target2") "div:last-of-type" True
  , testCase "target3 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target3") "div:last-of-type" True
  , testCase "target4 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target4") "div:last-of-type" True
  , testCase "target5 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target5") "div:last-of-type" True
  , testCase "target6 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target6") "div:last-of-type" False
  , testCase "target7 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target7") "div:last-of-type" False
  , testCase "target8 div:last-of-type" $ assertMatches (fid lastOfTypeDoc "target8") "div:last-of-type" False
  ]

-- ---------------------------------------------------------------------------
-- only-of-type.html (static cases)
-- ---------------------------------------------------------------------------

onlyOfTypeDoc :: Node
onlyOfTypeDoc = documentElement $ parseDocument
  "<!DOCTYPE html><html><body>\
  \<div><div id=\"target1\"></div></div>\
  \<div><span></span><div id=\"target2\"></div></div>\
  \<div><div id=\"target3\"></div>text<!-- --></div>\
  \<div><div id=\"target4\"></div><div></div></div>\
  \</body></html>"

onlyOfTypeTests :: [TestTree]
onlyOfTypeTests =
  [ testCase "target1 :only-of-type" $ assertMatches (fid onlyOfTypeDoc "target1") ":only-of-type" True
  , testCase "target2 :only-of-type" $ assertMatches (fid onlyOfTypeDoc "target2") ":only-of-type" True
  , testCase "target3 :only-of-type" $ assertMatches (fid onlyOfTypeDoc "target3") ":only-of-type" True
  , testCase "target4 :only-of-type" $ assertMatches (fid onlyOfTypeDoc "target4") ":only-of-type" False
  ]

-- ---------------------------------------------------------------------------
-- dir-selector-querySelector.html
-- ---------------------------------------------------------------------------

dirDoc :: Document
dirDoc = parseDocument
  "<!DOCTYPE html><html><body>\
  \<div id=\"outer\">\
  \  <div id=\"div1\"></div>\
  \  <div id=\"div2\" dir=\"ltr\">\
  \    <div id=\"div2_1\"></div>\
  \    <div id=\"div2_2\" dir=\"ltr\"></div>\
  \    <div id=\"div2_3\" dir=\"rtl\"></div>\
  \  </div>\
  \  <div id=\"div3\" dir=\"rtl\">\
  \    <div id=\"div3_1\">\
  \      <div id=\"div3_1_1\"></div>\
  \    </div>\
  \    <div id=\"div3_2\" dir=\"ltr\"></div>\
  \    <div id=\"div3_3\" dir=\"rtl\"></div>\
  \  </div>\
  \  <div id=\"div4\" dir=\"lol\"></div>\
  \  <div id=\"div5\" dir=\"auto\"></div>\
  \</div>\
  \</body></html>"

dirRoot :: Node
dirRoot = documentElement dirDoc

dirSelectorTests :: [TestTree]
dirSelectorTests = fmap (wptQS dirRoot)
  [ (":dir(lol)", Nothing)
  , (":dir(rtl)", Just "div2_3")
  , ("*:dir(rtl)", Just "div2_3")
  , ("div:dir(ltr)", Just "outer")
  , ("div:dir(ltr):dir(ltr)", Just "outer")
  , (":dir(rtl)#div3_3", Just "div3_3")
  , (":nth-child(2):dir(rtl)", Nothing)
  , (":nth-child(3):dir(rtl)", Just "div2_3")
  , ("#div2 :dir(ltr)", Just "div2_1")
  , (":dir(rtl) div", Just "div3_1")
  , ("div + :dir(ltr)", Just "div2")
  , (":dir(ltr) + :dir(rtl)", Just "div2_3")
  , (":dir(rtl) :dir(rtl)", Just "div3_1")
  , (":dir(rtl) + :dir(ltr)", Just "div3_2")
  , (":dir(rtl) ~ :dir(rtl)", Just "div3_3")
  , (":dir(rtl) :dir(ltr)", Just "div3_2")
  , ("* :dir(rtl) *", Just "div3_1")
  , ("div :dir(rtl) div", Just "div3_1")
  , (":dir(ltr) :dir(rtl) + :dir(ltr)", Just "div3_2")
  , (":dir(ltr) + :dir(rtl) + * + *", Just "div5")
  , (":dir(rtl) > * > :dir(rtl)", Just "div3_1_1")
  ]

-- ---------------------------------------------------------------------------
-- pseudo-enabled-disabled.html
-- ---------------------------------------------------------------------------

enabledDisabledDoc :: Node
enabledDisabledDoc = parseMain
  "<main id=main>\
  \<div id=\"container\">\
  \<button id=\"button_enabled\"></button>\
  \<button id=\"button_disabled\" disabled=\"\"></button>\
  \<input id=\"input_enabled\">\
  \<input id=\"input_disabled\" disabled=\"\">\
  \<select id=\"select_enabled\"></select>\
  \<select id=\"select_disabled\" disabled=\"\"></select>\
  \<textarea id=\"textarea_enabled\"></textarea>\
  \<textarea id=\"textarea_disabled\" disabled=\"\"></textarea>\
  \<span id=\"incapable\"></span>\
  \</div>\
  \</main>"

enabledDisabledContainer :: Node
enabledDisabledContainer =
  case findById "container" enabledDisabledDoc of
    Just n -> n
    Nothing -> error "no #container"

enabledDisabledTests :: [TestTree]
enabledDisabledTests =
  [ testCase ":enabled" $
      assertQSA enabledDisabledContainer ":enabled"
        ["button_enabled","input_enabled","select_enabled","textarea_enabled"]
  , testCase ":disabled" $
      assertQSA enabledDisabledContainer ":disabled"
        ["button_disabled","input_disabled","select_disabled","textarea_disabled"]
  , testCase ":not(:enabled)" $
      assertQSA enabledDisabledContainer ":not(:enabled)"
        ["button_disabled","incapable","input_disabled","select_disabled","textarea_disabled"]
  , testCase ":not(:disabled)" $
      assertQSA enabledDisabledContainer ":not(:disabled)"
        ["button_enabled","incapable","input_enabled","select_enabled","textarea_enabled"]
  ]

-- ---------------------------------------------------------------------------
-- has-argument-with-explicit-scope.html (static subset)
-- ---------------------------------------------------------------------------

hasScopeDoc :: Node
hasScopeDoc = parseMain
  "<main id=main>\
  \ <div id=d01 class=\"a\">\
  \  <div id=scope1 class=\"b\">\
  \    <div id=d02 class=\"c\">\
  \      <div id=d03 class=\"c\">\
  \        <div id=d04 class=\"d\"></div>\
  \      </div>\
  \    </div>\
  \    <div id=d05 class=\"e\"></div>\
  \  </div>\
  \ </div>\
  \ <div id=d06>\
  \  <div id=scope2 class=\"b\">\
  \    <div id=d07 class=\"c\">\
  \      <div id=d08 class=\"c\">\
  \        <div id=d09></div>\
  \      </div>\
  \    </div>\
  \  </div>\
  \ </div>\
  \</main>"

hasScopeTests :: [TestTree]
hasScopeTests =
  let scope1 = case findById "scope1" hasScopeDoc of Just n -> n; Nothing -> error "no scope1"
      scope2 = case findById "scope2" hasScopeDoc of Just n -> n; Nothing -> error "no scope2"
  in [ testCase "scope1: .c:has(.d)" $
         assertQSA scope1 ".c:has(.d)" ["d02","d03"]
     , testCase "scope2: .c:has(.d)" $
         assertQSA scope2 ".c:has(.d)" []
     , testCase "scope1: :scope .c:has(.d)" $
         assertQSA scope1 ".c:has(.d)" ["d02","d03"]
     , testCase "scope2: :scope .c:has(.d)" $
         assertQSA scope2 ".c:has(.d)" []
     ]

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

fid :: Node -> Text -> Node
fid root targetId = case findById targetId root of
  Just n  -> n
  Nothing -> error ("fid: element #" ++ T.unpack targetId ++ " not found")
