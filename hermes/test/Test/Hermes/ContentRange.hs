{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ContentRange (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.ContentRange as CR
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String CR.ContentRange
parseOk bs = case runParser CR.contentRangeParser bs of
  OK c leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right c
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: CR.ContentRange -> ByteString
render = M.toStrictByteString . CR.renderContentRange


unit_satisfied :: Spec
unit_satisfied = it "satisfied bytes/total" $
  case parseOk "bytes 0-499/1234" of
    Right (CR.ContentRange unit (CR.RangeRespSatisfied 0 499 (Just 1234))) ->
      unit `shouldBe` ST.fromString "bytes"
    other -> error (show other)


unit_satisfied_star :: Spec
unit_satisfied_star = it "satisfied with unknown total" $
  case parseOk "bytes 0-9/*" of
    Right (CR.ContentRange _ (CR.RangeRespSatisfied 0 9 Nothing)) -> pure () :: IO ()
    other -> error (show other)


unit_unsatisfied :: Spec
unit_unsatisfied = it "unsatisfied range" $
  case parseOk "bytes */4096" of
    Right (CR.ContentRange _ (CR.RangeRespUnsatisfied (Just 4096))) -> pure () :: IO ()
    other -> error (show other)


unit_render_satisfied :: Spec
unit_render_satisfied =
  it "render satisfied" $
    let v =
          CR.ContentRange
            (ST.fromString "bytes")
            (CR.RangeRespSatisfied 0 99 (Just 200))
    in render v `shouldBe` "bytes 0-99/200"


unit_render_unsatisfied :: Spec
unit_render_unsatisfied =
  it "render unsatisfied" $
    let v =
          CR.ContentRange
            (ST.fromString "bytes")
            (CR.RangeRespUnsatisfied (Just 4096))
    in render v `shouldBe` "bytes */4096"


unit_typed_lookup :: Spec
unit_typed_lookup = it "retains the received unit and renders the full Word64 range" $ do
  let readRange wire = lookupHeader @CR.ContentRange (headerMapFromList [("Content-Range", wire)])
      value = CR.ContentRange "Bytes" (CR.RangeRespSatisfied 42 (maxBound - 1) (Just maxBound))
  readRange " \tBytes 0042-18446744073709551614/18446744073709551615 \t"
    `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList []))
    `shouldBe` [("content-range", "Bytes 42-18446744073709551614/18446744073709551615")]
  readRange "bytes 18446744073709551615-18446744073709551615/*"
    `shouldBe` Right (Just (CR.ContentRange "bytes" (CR.RangeRespSatisfied maxBound maxBound Nothing)))
  readRange "rows */0"
    `shouldBe` Right (Just (CR.ContentRange "rows" (CR.RangeRespUnsatisfied (Just 0))))
  lookupHeader @CR.ContentRange (headerMapFromList [("Content-Range", "bytes 0-1/3"), ("Content-Range", "bytes 2-3/4")])
    `shouldSatisfy` isLeft


unit_invalid :: Spec
unit_invalid = it "rejects reversed bounds, inconsistent lengths, overflow and invalid delimiters" $ do
  let readRange wire = lookupHeader @CR.ContentRange (headerMapFromList [("Content-Range", wire)])
  mapM_
    (\wire -> readRange wire `shouldSatisfy` isLeft)
    [ "bytes 9-8/*"
    , "bytes 0-9/9"
    , "bytes 0-9/8"
    , "bytes 0-0/0"
    , "bytes */*"
    , "bytes  0-0/1"
    , "bytes\t0-0/1"
    , "bytes 0-0/1x"
    , "bytes 18446744073709551616-18446744073709551615/*"
    , "bytes 0-18446744073709551616/*"
    , "bytes 0-9/18446744073709551616"
    , "bytes */18446744073709551616"
    ]


unit_content_range_checked_write :: Spec
unit_content_range_checked_write = it "rejects invalid typed Content-Range writes without replacing existing lines" $ do
  let fields = headerMapFromList [("Content-Range", "bytes 0-1/10")]
      invalid =
        [ CR.ContentRange "two words" (CR.RangeRespUnsatisfied (Just 10))
        , CR.ContentRange "bytes" (CR.RangeRespUnsatisfied Nothing)
        , CR.ContentRange "bytes" (CR.RangeRespSatisfied 2 1 Nothing)
        , CR.ContentRange "bytes" (CR.RangeRespSatisfied 0 10 (Just 10))
        ]
  mapM_
    ( \value -> do
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value fields `shouldBe` fields
    )
    invalid
  let valid = CR.ContentRange "rows" (CR.RangeRespUnsatisfied (Just 0))
  lookupHeader @CR.ContentRange (setHeader valid fields) `shouldBe` Right (Just valid)


-- Property: satisfied form round-trips for arbitrary positions.
respGen :: Gen CR.RangeResp
respGen =
  Gen.choice
    [ do
        a <- Gen.word64 (Range.linear 0 maxBound)
        b <- Gen.word64 (Range.linear a maxBound)
        t <- if b == maxBound then pure Nothing else Gen.maybe (Gen.word64 (Range.linear (b + 1) maxBound))
        pure (CR.RangeRespSatisfied a b t)
    , CR.RangeRespUnsatisfied
        . Just
        <$> Gen.word64 (Range.linear 0 maxBound)
    ]


prop_roundtrip :: Property
prop_roundtrip = property $ do
  resp <- forAll respGen
  let v = CR.ContentRange (ST.fromString "bytes") resp
      bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "ContentRange" $
    sequence_
      [ unit_satisfied
      , unit_satisfied_star
      , unit_unsatisfied
      , unit_render_satisfied
      , unit_render_unsatisfied
      , unit_typed_lookup
      , unit_invalid
      , unit_content_range_checked_write
      , it "round-trip" prop_roundtrip
      ]
