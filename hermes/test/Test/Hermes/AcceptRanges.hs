{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AcceptRanges (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.AcceptRanges as AR
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd


parseOk :: ByteString -> Either String AR.AcceptRanges
parseOk bs = case runParser AR.acceptRangesParser bs of
  OK ar leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right ar
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: AR.AcceptRanges -> ByteString
render = M.toStrictByteString . AR.renderAcceptRanges


unit_none :: Spec
unit_none = it "literal none disables ranges" $ do
  case parseOk "none" of
    Right AR.AcceptRangesNone -> pure () :: IO ()
    other -> error (show other)
  render AR.AcceptRangesNone `shouldBe` "none"


unit_units :: Spec
unit_units = it "unit list" $
  case parseOk "bytes, custom-unit" of
    Right (AR.AcceptRangesUnits (a :| [b])) -> do
      a `shouldBe` ST.fromString "bytes"
      b `shouldBe` ST.fromString "custom-unit"
    other -> error (show other)


unit_render_units :: Spec
unit_render_units =
  it "render unit list" $
    let v = AR.AcceptRangesUnits (ST.fromString "bytes" :| [ST.fromString "rows"])
    in render v `shouldBe` "bytes, rows"


tests :: Spec
tests =
  describe "AcceptRanges" $
    sequence_
      [ unit_none
      , unit_units
      , unit_render_units
      ]
