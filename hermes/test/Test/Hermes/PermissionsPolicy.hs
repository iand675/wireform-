{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.PermissionsPolicy (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (RFC8941Token (..), Result (..), runParser)
import qualified Network.HTTP.Headers.PermissionsPolicy as PP
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String PP.PermissionsPolicy
parseOk bs = case runParser PP.permissionsPolicyParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: PP.PermissionsPolicy -> ByteString
render = M.toStrictByteString . PP.renderPermissionsPolicy


unit_parse :: Spec
unit_parse = it "parses a mixed feature dictionary" $
  case parseOk "geolocation=(self \"https://example.com\"), camera=(), fullscreen=*" of
    Right (PP.PermissionsPolicy ds) ->
      ds
        `shouldBe` [ ("geolocation", [PP.AllowSelf, PP.AllowOrigin "https://example.com"])
                   , ("camera", [])
                   , ("fullscreen", [PP.AllowAll])
                   ]
    other -> error (show other)


unit_parse_src :: Spec
unit_parse_src = it "parses the src keyword inside an inner-list" $
  case parseOk "geolocation=(self src)" of
    Right (PP.PermissionsPolicy ds) ->
      ds `shouldBe` [("geolocation", [PP.AllowSelf, PP.AllowSrc])]
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "renders features as inner-lists separated by commas" $
    let v =
          PP.PermissionsPolicy
            [ ("geolocation", [PP.AllowSelf])
            , ("camera", [])
            ]
    in render v `shouldBe` "geolocation=(self), camera=()"


unit_render_origin :: Spec
unit_render_origin =
  it "quotes serialized origins on render" $
    let v = PP.PermissionsPolicy [("fullscreen", [PP.AllowAll, PP.AllowOrigin "https://a.test"])]
    in render v `shouldBe` "fullscreen=(* \"https://a.test\")"


unit_typed_dictionary :: Spec
unit_typed_dictionary = it "resolves duplicate features and retains report-to and unknown members" $ do
  let fields =
        headerMapFromList
          [ ("Permissions-Policy", "\t geolocation=(self);report-to=old, camera=*")
          , ("permissions-policy", "fullscreen=?0;trace=@5, geolocation=(\"https://*.example.com:*/adm%69n\" \"https:\" src 1);report-to=old;ext=%\"caf%c3%a9\";report-to=new")
          ]
      canonical = "geolocation=(\"https://*.example.com:*/adm%69n\" \"https:\" src 1);report-to=new;ext=%\"caf%c3%a9\", camera=*, fullscreen=?0;trace=@5"
  case lookupHeader @PP.PermissionsPolicy fields of
    Right (Just policy) -> do
      PP.permissionsPolicyDeclarations policy
        `shouldBe` [ PP.PermissionsPolicyDeclaration "geolocation" (Just [PP.AllowOrigin "https://*.example.com:*/adm%69n", PP.AllowOrigin "https:"]) (Just (RFC8941Token "new"))
                   , PP.PermissionsPolicyDeclaration "camera" (Just [PP.AllowAll]) Nothing
                   , PP.PermissionsPolicyDeclaration "fullscreen" Nothing Nothing
                   ]
      lookupRawHeader "Permissions-Policy" (setHeader policy (headerMapFromList [])) `shouldBe` Just (canonical :| [])
      parseOk canonical `shouldBe` Right policy
    result -> expectationFailure (show result)


unit_permissions_sources :: Spec
unit_permissions_sources = it "projects valid CSP source expressions and ignores unsupported allowlist items" $ do
  let field wire = lookupHeader @PP.PermissionsPolicy (headerMapFromList [("Permissions-Policy", wire)])
  case field "camera=(\"https://example.com.\" \"*.test:0123/a/b\" \"https://bad..test\" \"https://user@ex.test\" \"https://test/a;bad\" \"https://test/?x\" \"https://test/%ff\" \"https://test/%z\" \"https://test//a\" \"https://test/#f\" self);report-to=\"must-be-token\", geolocation=\"https:\", microphone=()" of
    Right (Just policy) ->
      PP.permissionsPolicyDeclarations policy
        `shouldBe` [ PP.PermissionsPolicyDeclaration "camera" (Just [PP.AllowOrigin "https://example.com.", PP.AllowOrigin "*.test:0123/a/b", PP.AllowOrigin "https://test/%ff", PP.AllowSelf]) Nothing
                   , PP.PermissionsPolicyDeclaration "geolocation" (Just [PP.AllowOrigin "https:"]) Nothing
                   , PP.PermissionsPolicyDeclaration "microphone" (Just []) Nothing
                   ]
    result -> expectationFailure (show result)
  mapM_
    (\bad -> field bad `shouldSatisfy` isLeft)
    ["camera=(self\t*)", "Camera=*", "camera=(self),", "camera=*, , microphone=()", "camera=(self);\tflag", "camera=(\"bad\\q\")"]
  let previous = headerMapFromList [("Permissions-Policy", "camera=(self)")]
  lookupRawHeader "Permissions-Policy" (setHeader (PP.PermissionsPolicy []) previous) `shouldBe` Nothing


-- Property: any well-formed dictionary round-trips through render/parse.
originGen :: Gen ST.ShortText
originGen =
  Gen.element
    [ "https://example.com"
    , "https://a.test"
    , "http://localhost:8080"
    , "https://cdn.example.org"
    ]


itemGen :: Gen PP.AllowListItem
itemGen =
  Gen.choice
    [ pure PP.AllowAll
    , pure PP.AllowSelf
    , pure PP.AllowSrc
    , PP.AllowOrigin <$> originGen
    ]


featureGen :: Gen ST.ShortText
featureGen = ST.fromString <$> Gen.string (Range.linear 3 10) Gen.lower


directiveGen :: Gen (ST.ShortText, [PP.AllowListItem])
directiveGen = (,) <$> featureGen <*> Gen.list (Range.linear 0 3) itemGen


ppGen :: Gen PP.PermissionsPolicy
ppGen = PP.PermissionsPolicy <$> Gen.list (Range.linear 1 4) directiveGen


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll ppGen
  let bs = render v
  case parseOk bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "PermissionsPolicy" $
    sequence_
      [ unit_parse
      , unit_parse_src
      , unit_render
      , unit_render_origin
      , unit_typed_dictionary
      , unit_permissions_sources
      , it "round-trips" prop_roundtrip
      ]
