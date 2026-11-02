{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.YAML qualified as HsYAML
import Data.Yaml qualified as Libyaml
import GHC.Generics (Generic)
import YAML.Class qualified as C
import YAML.Decode qualified as Y
import YAML.Derive (deriveYAML)
import YAML.Encode qualified as YE
import YAML.Value qualified as YV


data Person = Person {name :: T.Text, age :: Int, active :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Aeson.FromJSON, Aeson.ToJSON)


deriveYAML ''Person


personBytes :: BS.ByteString
personBytes = "name: Alice\nage: 42\nactive: true\n"


decodeRecord :: BS.ByteString -> Either String Person
decodeRecord = C.decodeYAMLBS


decodeRecordLibyaml :: BS.ByteString -> Either String Person
decodeRecordLibyaml bytes = case Libyaml.decodeEither' bytes of
  Left err -> Left (show err)
  Right person -> Right person


-- | A tiny mapping (~20 byte input).
tiny :: BS.ByteString
tiny = "key: value"


-- | A mid-size nested document (a few hundred bytes).
small :: BS.ByteString
small =
  BS8.unlines
    [ "name: example"
    , "version: 1.2.3"
    , "tags:"
    , "  - haskell"
    , "  - yaml"
    , "  - parser"
    , "owner:"
    , "  name: Alice"
    , "  email: alice@example.com"
    , "config:"
    , "  threads: 8"
    , "  timeout: 30"
    , "  retry: true"
    , "  endpoints:"
    , "    - https://example.com"
    , "    - https://example.org"
    ]


-- | A flow-heavy mid-size document.
flowMid :: BS.ByteString
flowMid =
  BS8.unlines
    [ "matrix: [[1,2,3], [4,5,6], [7,8,9]]"
    , "tags: { a: 1, b: 2, c: 3, d: 4 }"
    , "items: [{x: 1, y: 2}, {x: 3, y: 4}, {x: 5, y: 6}]"
    ]


-- | A scalar-heavy document (many quoted strings).
quoted :: BS.ByteString
quoted = BS8.unlines ("items:" : ((\n -> "  - \"item " <> numberBytes n <> " with some quoted text\"") <$> [1 .. 50]))


-- | A block scalar / literal text body.
literalBody :: BS.ByteString
literalBody = BS8.unlines ("code: |" : ((\n -> "  line " <> numberBytes n <> " of body text") <$> [1 .. 50]))


numberBytes :: Int -> BS.ByteString
numberBytes = BSL.toStrict . BB.toLazyByteString . BB.intDec


-- | Finite floats including precision and exponent boundaries.
floating :: BS.ByteString
floating = "[" <> BS.intercalate ", " (replicate 25 "0.1, 1.1930203655234305e70, 5e-324, -2.2250738585072014e-308, 1.7976931348623157e308") <> "]"


-- | Escaped controls and supplementary Unicode on the quoted slow path.
escaped :: BS.ByteString
escaped = "{text: \"" <> BS.concat (replicate 100 "a\\nb\\u00a0\\U0001f600") <> "\"}"


-- | A larger doc (~2 KB) of mixed content.
big :: BS.ByteString
big = BS.concat (section <$> ["doc1", "doc2", "doc3", "doc4"])
  where
    section name = name <> ":\n" <> BS8.unlines (("  " <>) <$> BS8.lines small)


decodeBytes :: BS.ByteString -> Either String YV.Stream
decodeBytes = Y.decodeStreamBS


decodeHsYAML :: BS.ByteString -> Either String ()
decodeHsYAML bs = case HsYAML.decodeNode (BSL.fromStrict bs) of
  Right ns -> Right (foldr (\doc rest -> forceHsNode (HsYAML.docRoot doc) `seq` rest) () ns)
  Left (_, e) -> Left e


-- Force the parsed data, not just the outer document list. HsYAML keeps
-- source locations too; those presentation details aren't part of this result.
forceHsNode :: HsYAML.Node location -> ()
forceHsNode node = case node of
  HsYAML.Anchor _ _ inner -> forceHsNode inner
  HsYAML.Sequence _ _ values -> foldr (\value rest -> forceHsNode value `seq` rest) () values
  HsYAML.Mapping _ _ values -> Map.foldrWithKey (\key value rest -> forceHsNode key `seq` forceHsNode value `seq` rest) () values
  HsYAML.Scalar _ scalar -> case scalar of
    HsYAML.SNull -> ()
    HsYAML.SBool value -> value `seq` ()
    HsYAML.SInt value -> value `seq` ()
    HsYAML.SFloat value -> value `seq` ()
    HsYAML.SStr value -> value `seq` ()
    HsYAML.SUnknown tag value -> tag `seq` value `seq` ()


decodeLibyaml :: BS.ByteString -> Either String Aeson.Value
decodeLibyaml bs = case Libyaml.decodeEither' bs of
  Right v -> Right v
  Left e -> Left (show e)


main :: IO ()
main = do
  mapM_ validateFixture [("tiny", tiny), ("small", small), ("flow", flowMid), ("quoted", quoted), ("literal", literalBody), ("big", big), ("floating", floating), ("escaped", escaped)]
  let expected = Right (Person "Alice" 42 True)
  if decodeRecord personBytes == expected && decodeRecordLibyaml personBytes == expected then pure () else fail "invalid native-record benchmark fixture"
  defaultMain
    [ bgroup
        "decode"
        [ bench "tiny" $ nf decodeBytes tiny
        , bench "small" $ nf decodeBytes small
        , bench "flowMid" $ nf decodeBytes flowMid
        , bench "quoted50" $ nf decodeBytes quoted
        , bench "literal50" $ nf decodeBytes literalBody
        , bench "big" $ nf decodeBytes big
        , bench "floating" $ nf decodeBytes floating
        , bench "escaped" $ nf decodeBytes escaped
        ]
    , bgroup
        "decode-hsyaml"
        [ bench "tiny" $ nf decodeHsYAML tiny
        , bench "small" $ nf decodeHsYAML small
        , bench "flowMid" $ nf decodeHsYAML flowMid
        , bench "quoted50" $ nf decodeHsYAML quoted
        , bench "literal50" $ nf decodeHsYAML literalBody
        , bench "big" $ nf decodeHsYAML big
        , bench "floating" $ nf decodeHsYAML floating
        , bench "escaped" $ nf decodeHsYAML escaped
        ]
    , bgroup
        "decode-libyaml"
        [ bench "tiny" $ nf decodeLibyaml tiny
        , bench "small" $ nf decodeLibyaml small
        , bench "flowMid" $ nf decodeLibyaml flowMid
        , bench "quoted50" $ nf decodeLibyaml quoted
        , bench "literal50" $ nf decodeLibyaml literalBody
        , bench "big" $ nf decodeLibyaml big
        , bench "floating" $ nf decodeLibyaml floating
        , bench "escaped" $ nf decodeLibyaml escaped
        ]
    , bgroup
        "preprocess"
        [ bench "small" $ nf (length . Y.preprocess) (decodeText small)
        , bench "big" $ nf (length . Y.preprocess) (decodeText big)
        ]
    , bgroup
        "decode-record"
        [ bench "wireform-yaml" $ nf decodeRecord personBytes
        , bench "libyaml" $ nf decodeRecordLibyaml personBytes
        ]
    , bgroup
        "encode-record"
        [ bench "wireform-yaml" $ nf C.encodeYAMLBS (Person "Alice" 42 True)
        , bench "libyaml" $ nf Libyaml.encode (Person "Alice" 42 True)
        ]
    , bgroup
        "encode"
        [ env (pure (decodeBody small)) $ \v ->
            bench "small" $
              nf YE.encode v
        , env (pure (decodeBody flowMid)) $ \v ->
            bench "flowMid" $
              nf YE.encode v
        ]
    ]
  where
    decodeBody bs = case decodeBytes bs of
      Right s -> YV.docBody (V.head (YV.unStream s))
      Left e -> error e

    decodeText = TE.decodeUtf8


-- Fail setup if a codec rejects a fixture, rather than timing its error path.
validateFixture :: (String, BS.ByteString) -> IO ()
validateFixture (name, bytes) = do
  accepted "wireform-yaml" (decodeBytes bytes)
  accepted "HsYAML" (decodeHsYAML bytes)
  accepted "libyaml" (decodeLibyaml bytes)
  where
    accepted codec result = case result of
      Left err -> fail (codec <> "/" <> name <> ": " <> err)
      Right _ -> pure ()
