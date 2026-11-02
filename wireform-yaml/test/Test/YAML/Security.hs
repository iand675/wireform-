{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the parser's defensive limits against the
well-known YAML denial-of-service classes:

* 'billion laughs' — a small input with N levels of K-fold
  alias references that expands to K^N nodes when traversed
  naively.
* deep nesting — '[[[[…]]]]' / line-after-indented-line that
  consumes recursive Haskell stack proportional to its depth.
* cyclic aliases — an anchor that references itself.
-}
module Test.YAML.Security (tests) where

import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as BI
import Test.Syd
import YAML.Decode qualified as D
import YAML.Decode qualified as Y
import YAML.Value qualified as YV


tests :: Spec
tests =
  describe "Security" $
    sequence_
      [ billionLaughsRefused
      , deepBlockRefused
      , selfCycleRejected
      , flowCycleResolution
      , normalAliasesAllowed
      , normalNestingAllowed
      , limitTests
      ]


{- | A 9-level / 9-fold billion-laughs document expands to 9^9
~ 4×10^8 logical nodes and must be rejected before the parser
hands a 'Value' to the caller.
-}
billionLaughsRefused :: Spec
billionLaughsRefused =
  it "9-level billion laughs is refused" $ do
    let src =
          T.unlines
            [ "a: &a [1,1,1,1,1,1,1,1,1]"
            , "b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]"
            , "c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]"
            , "d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]"
            , "e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]"
            , "f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]"
            , "g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]"
            , "h: &h [*g,*g,*g,*g,*g,*g,*g,*g,*g]"
            , "i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]"
            , "value: *i"
            ]
    case Y.decode src of
      Left e ->
        ( if ( "billion-laughs" `isInfixOf` e
                 || "alias DAG" `isInfixOf` e
                 || "expand" `isInfixOf` e
             )
            then pure ()
            else expectationFailure ("expected billion-laughs error, got: " <> e)
        )
      Right _ -> expectationFailure "expected the 9-level billion-laughs to be refused"


{- | A document nested past 'maxParserDepth' must error rather
than blowing the runtime stack.
-}
deepBlockRefused :: Spec
deepBlockRefused = it "deep block nesting is refused" $ do
  -- Build a 2000-deep nested mapping: 'a:\\n  a:\\n    a:\\n …'
  let depth = 2000 :: Int
      src =
        T.intercalate
          "\n"
          ((\i -> T.replicate (2 * i) " " <> "a:") <$> [0 .. depth - 1])
          <> "\n"
          <> T.replicate (2 * depth) " "
          <> "leaf"
  case Y.decode src of
    Left e ->
      (if ("nesting" `isInfixOf` e || "depth" `isInfixOf` e) then pure () else expectationFailure ("expected nesting error, got: " <> e))
    Right _ -> expectationFailure "expected deep nesting to be refused"


{- | A self-referencing alias must error. Forward references are
illegal per YAML 1.2, so this is a pre-existing guarantee;
the test pins it down.
-}
selfCycleRejected :: Spec
selfCycleRejected = it "self-cycle in alias is rejected" $ do
  let cases = ["a: &a [*a]", "&a\n- *a", "key: &a *a"]
  mapM_ check cases
  where
    check src =
      (if (isLeft (Y.decode src)) then pure () else expectationFailure ("expected error on " <> show src))


{- | A flow document whose registered anchor's body contains an
alias to the SAME anchor must error rather than producing a
value tree with unresolved alias sentinels.
-}
flowCycleResolution :: Spec
flowCycleResolution = it "flow alias cycle is rejected" $ do
  -- The flow alias '*a' inside '&a [ ... ]' is recorded after
  -- the surrounding flow node is fully parsed; the resolution
  -- pass then encounters the cycle.
  let src = T.pack "{a: &a {self: *a}}"
  case Y.decode src of
    Left _ -> pure () -- any rejection is acceptable; the
    -- combined record-and-resolve pass
    -- naturally surfaces the cycle as a
    -- 'no anchor' error because we walk
    -- bottom-up.
    Right _ -> expectationFailure "expected flow cycle to be refused"


{- | A modest, normal use of aliases (small fan-out, single
level) must continue to parse.
-}
normalAliasesAllowed :: Spec
normalAliasesAllowed = it "small aliases are allowed" $ do
  let src =
        T.unlines
          [ "defaults: &d"
          , "  retries: 3"
          , "  timeout: 30"
          , "alpha: *d"
          , "beta: *d"
          ]
  case Y.decode src of
    Right v ->
      case YV.lookupKey "alpha" v of
        Just _ -> pure ()
        Nothing -> expectationFailure "expected alpha to resolve"
    Left e -> expectationFailure ("unexpected error: " <> e)


{- | A document nested below 'maxParserDepth' must continue to
parse. Pin the limit at 'a few hundred'.
-}
normalNestingAllowed :: Spec
normalNestingAllowed = it "moderate nesting is allowed" $ do
  let depth = 200 :: Int
      src =
        T.intercalate
          "\n"
          ((\i -> T.replicate (2 * i) " " <> "a:") <$> [0 .. depth - 1])
          <> "\n"
          <> T.replicate (2 * depth) " "
          <> "leaf"
  case Y.decode src of
    Right _ -> pure ()
    Left e ->
      expectationFailure
        ( "unexpected error at depth "
            <> show depth
            <> ": "
            <> e
        )


limitTests :: Spec
limitTests = describe "limits" $ do
  it "bounds both flow and mixed block/flow nesting" $ do
    let nested = T.replicate 2000 "[" <> "0" <> T.replicate 2000 "]"
    D.decode nested `shouldSatisfy` isLeft
    D.decode ("items: " <> nested) `shouldSatisfy` isLeft
    let block = T.unlines ((\i -> T.replicate (2 * i) " " <> "a:") <$> [0 .. 899])
    D.decode (block <> T.replicate 1800 " " <> T.replicate 200 "[" <> "0" <> T.replicate 200 "]") `shouldSatisfy` isLeft
    D.decode (T.replicate 200 "[" <> "0" <> T.replicate 200 "]") `shouldSatisfy` either (const False) (const True)
  it "rejects alias amplification after legal name rebinding" $ do
    let layer i =
          let (old, new) = if odd i then ("a", "b") else ("b", "a")
          in "&" <> new <> " [" <> T.intercalate ", " (replicate 9 ("*" <> old) <> ["&" <> old <> " 0"]) <> "]"
        source = "[" <> T.intercalate ", " ("&a [0]" : (layer <$> [1 .. 10 :: Int])) <> "]"
    D.decode source `shouldSatisfy` isLeft
  it "bounds total document expansion as well as individual anchors" $ do
    let layer (i, prior) = "&a" <> numberText i <> " [" <> T.intercalate "," (replicate 9 ("*a" <> numberText prior)) <> "]"
        anchors = "&a0 [0]" : (layer <$> zip [1 .. 6 :: Int] [0 .. 5])
        source = "[" <> T.intercalate "," (anchors <> replicate 100 "*a6") <> "]"
    D.decode source `shouldSatisfy` isLeft
  it "rejects indentation that cannot fit the parser state instead of wrapping it" $
    D.decode ("outer:\n" <> T.replicate 1048575 " " <> "key: value\n") `shouldSatisfy` isLeft


numberText :: Int -> Text
numberText = TL.toStrict . TB.toLazyText . BI.decimal
