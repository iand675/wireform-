{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AcceptCharset (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.AcceptCharset as AC
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd


parseOk :: ByteString -> Either String AC.AcceptCharset
parseOk bs = case runParser AC.acceptCharsetParser bs of
  OK ac leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right ac
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: AC.AcceptCharset -> ByteString
render = M.toStrictByteString . AC.renderAcceptCharset


unit_simple :: Spec
unit_simple = it "single charset" $
  case parseOk "utf-8" of
    Right (AC.AcceptCharset [AC.WeightedCharset c 1]) ->
      c `shouldBe` ST.fromString "utf-8"
    other -> error ("unexpected parse: " <> show other)


unit_weighted_list :: Spec
unit_weighted_list = it "weighted list with wildcard" $
  case parseOk "utf-8;q=1, iso-8859-1;q=0.5, *;q=0" of
    Right
      ( AC.AcceptCharset
          [ AC.WeightedCharset _ 1
            , AC.WeightedCharset _ 0.5
            , AC.WeightedCharset star 0
            ]
        ) ->
        star `shouldBe` ST.fromString "*"
    other -> error ("unexpected parse: " <> show other)


unit_round_trip :: Spec
unit_round_trip =
  it "render → parse round-trip" $
    let v =
          AC.AcceptCharset
            [ AC.WeightedCharset (ST.fromString "utf-8") 1
            , AC.WeightedCharset (ST.fromString "iso-8859-1") 0.5
            ]
        bs = render v
    in case parseOk bs of
        Right v' -> v' `shouldBe` v
        Left err -> error err


tests :: Spec
tests =
  describe "AcceptCharset" $
    sequence_
      [ unit_simple
      , unit_weighted_list
      , unit_round_trip
      ]
