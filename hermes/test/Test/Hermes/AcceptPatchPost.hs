{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AcceptPatchPost (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType (..))
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
import qualified Network.HTTP.Headers.AcceptPatch as APatch
import qualified Network.HTTP.Headers.AcceptPost as APost
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseOkPatch :: ByteString -> Either String APatch.AcceptPatch
parseOkPatch bs = case runParser APatch.acceptPatchParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseOkPost :: ByteString -> Either String APost.AcceptPost
parseOkPost bs = case runParser APost.acceptPostParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderPatch :: APatch.AcceptPatch -> ByteString
renderPatch = M.toStrictByteString . APatch.renderAcceptPatch


renderPost :: APost.AcceptPost -> ByteString
renderPost = M.toStrictByteString . APost.renderAcceptPost


mt :: ST.ShortText -> ST.ShortText -> MediaType
mt = MediaType


-- Accept-Patch ---------------------------------------------------------------

unit_patch_parse :: Spec
unit_patch_parse = it "parses a media-type list" $
  case parseOkPatch "text/example, application/json" of
    Right (APatch.AcceptPatch (a :| [b])) -> do
      a `shouldBe` mt "text" "example"
      b `shouldBe` mt "application" "json"
    other -> error (show other)


unit_patch_render :: Spec
unit_patch_render =
  it "renders a media-type list" $
    let v = APatch.AcceptPatch (mt "application" "json" :| [mt "text" "plain"])
    in renderPatch v `shouldBe` "application/json, text/plain"


unit_patch_parameters :: Spec
unit_patch_parameters = it "preserves parameters and rejects wildcard ranges" $ do
  let fields = headerMapFromList [("Accept-Patch", " , "), ("accept-patch", "text/example;charset=utf-8,,application/merge-patch+json")]
      value =
        APatch.AcceptPatchRanges
          (MediaRange (mt "text" "example") [("charset", "utf-8")] :| [MediaRange (mt "application" "merge-patch+json") []])
  lookupHeader @APatch.AcceptPatch fields `shouldBe` Right (Just value)
  APatch.acceptPatchMediaTypes value `shouldBe` (mt "text" "example" :| [mt "application" "merge-patch+json"])
  headerMapToList (setHeader value (headerMapFromList []))
    `shouldBe` [("accept-patch", "text/example;charset=utf-8, application/merge-patch+json")]
  map (isLeft . parseOkPatch) ["*/*", "application/*", "*/json", "application/*+json", " , ", "text/plain;bad="]
    `shouldBe` replicate 6 True


-- Accept-Post ----------------------------------------------------------------

unit_post_parse :: Spec
unit_post_parse = it "parses media types and the literal star" $
  case parseOkPost "image/png, application/ld+json, *" of
    Right (APost.AcceptPost (a :| [b, c])) -> do
      a `shouldBe` APost.AcceptPostMediaType (mt "image" "png")
      b `shouldBe` APost.AcceptPostMediaType (mt "application" "ld+json")
      c `shouldBe` APost.AcceptPostAny
    other -> error (show other)


unit_post_parse_star_star :: Spec
unit_post_parse_star_star = it "distinguishes */* from the literal star" $
  case parseOkPost "*/*" of
    Right (APost.AcceptPost (a :| [])) ->
      a `shouldBe` APost.AcceptPostMediaType (mt "" "")
    other -> error (show other)


unit_post_render :: Spec
unit_post_render =
  it "renders media types and the literal star" $
    let v = APost.AcceptPost (APost.AcceptPostMediaType (mt "text" "turtle") :| [APost.AcceptPostAny])
    in renderPost v `shouldBe` "text/turtle, *"


unit_post_parameters :: Spec
unit_post_parameters = it "preserves parameters on media ranges and handles empty fields" $ do
  let wire = "text/*;charset=utf-8, application/json;profile=\"one,two\", */*"
      value =
        APost.AcceptPost
          ( APost.AcceptPostMediaRange (MediaRange (mt "text" "") [("charset", "utf-8")])
              :| [ APost.AcceptPostMediaRange (MediaRange (mt "application" "json") [("profile", "one,two")])
                 , APost.AcceptPostMediaType (mt "" "")
                 ]
          )
      fields = headerMapFromList [("Accept-Post", ",,"), ("accept-post", wire)]
  lookupHeader @APost.AcceptPost fields `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("accept-post", wire)]
  lookupHeader @APost.AcceptPost (headerMapFromList [("accept-post", ", , ")]) `shouldBe` Right (Just APost.AcceptPostEmpty)
  lookupHeader @APost.AcceptPost (headerMapFromList []) `shouldBe` Right Nothing
  APost.acceptPostEntryList APost.AcceptPostEmpty `shouldBe` []
  headerMapToList (setHeader APost.AcceptPostEmpty (headerMapFromList [])) `shouldBe` [("accept-post", "")]
  map (isLeft . parseOkPost) ["*/json", "application/*+json", "text/plain;bad="] `shouldBe` replicate 3 True


-- Round-trip properties ------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element ['a' .. 'z'])


mediaTypeGen :: Gen MediaType
mediaTypeGen = MediaType <$> tokenGen <*> tokenGen


acceptPatchGen :: Gen APatch.AcceptPatch
acceptPatchGen =
  APatch.AcceptPatch <$> ((:|) <$> mediaTypeGen <*> Gen.list (Range.linear 0 4) mediaTypeGen)


acceptPostEntryGen :: Gen APost.AcceptPostEntry
acceptPostEntryGen =
  Gen.choice
    [ pure APost.AcceptPostAny
    , APost.AcceptPostMediaType <$> mediaTypeGen
    ]


acceptPostGen :: Gen APost.AcceptPost
acceptPostGen =
  APost.AcceptPost <$> ((:|) <$> acceptPostEntryGen <*> Gen.list (Range.linear 0 4) acceptPostEntryGen)


prop_patch_roundtrip :: Property
prop_patch_roundtrip = property $ do
  v <- forAll acceptPatchGen
  let bs = renderPatch v
  case parseOkPatch bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


prop_post_roundtrip :: Property
prop_post_roundtrip = property $ do
  v <- forAll acceptPostGen
  let bs = renderPost v
  case parseOkPost bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "AcceptPatchPost" $
    sequence_
      [ describe "Accept-Patch" $
          sequence_
            [ unit_patch_parse
            , unit_patch_render
            , unit_patch_parameters
            , it "round-trips" prop_patch_roundtrip
            ]
      , describe "Accept-Post" $
          sequence_
            [ unit_post_parse
            , unit_post_parse_star_star
            , unit_post_render
            , unit_post_parameters
            , it "round-trips" prop_post_roundtrip
            ]
      ]
