{- | Lossy bridge between YAML 'YV.Value' and Aeson 'A.Value'.

YAML is a superset of JSON in the data model, but the YAML core
schema admits scalars (notably non-string mapping keys) that JSON
cannot represent directly. We follow the same conventions other
@<Format>.JSON@ modules use:

* Mapping keys are coerced to text via 'showKey'. Keys that are
  already 'YV.YString' pass through verbatim; numeric, boolean and
  null keys are stringified.
* Non-finite floats and unresolved aliases become JSON null.
* Tags / anchors are stripped (they are part of the YAML
  representation graph, not the data model).
-}
module YAML.JSON (
  yamlToJSON,
  jsonToYAML,
) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Text.Lazy.Builder.RealFloat qualified as BF
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import Data.Vector qualified as V
import YAML.Decode qualified as YD
import YAML.Value qualified as YV


yamlToJSON :: YV.Value -> A.Value
yamlToJSON v = case v of
  YV.YNull -> A.Null
  YV.YBool b -> A.Bool b
  YV.YInt n -> A.Number (fromIntegral n)
  YV.YFloat d
    | isNaN d || isInfinite d -> A.Null
    | otherwise -> A.Number (Sci.fromFloatDigits d)
  YV.YString t -> A.String t
  YV.YSeq xs -> A.Array (V.map yamlToJSON xs)
  YV.YMap kvs -> A.Object (AKM.fromList (V.toList (V.map mkPair kvs)))
  YV.YTagged tag inner -> applyTag tag inner
  YV.YAnchored _ inner -> yamlToJSON inner
  YV.YAlias _ -> A.Null
  where
    mkPair (k, val) = (AK.fromText (showKey k), yamlToJSON val)


{- | Resolve the value under an explicit YAML tag. The standard
@tag:yaml.org,2002:str@ tag with a null body produces @""@,
because @!!str@ on an empty source means "force-string-typed
empty scalar"; without this rule round-tripping
@!!str@-tagged null values gives the wrong JSON shape.
-}
applyTag :: YV.Tag -> YV.Value -> A.Value
applyTag tag@(YV.Tag t) v
  | t == "tag:yaml.org,2002:str" = case YV.unwrap v of
      YV.YNull -> A.String T.empty
      YV.YString text -> A.String text
      YV.YBool b -> A.String (if b then "true" else "false")
      YV.YInt n -> A.String (builderText (BI.decimal n))
      YV.YFloat d -> A.String (builderText (BF.realFloat d))
      _ -> yamlToJSON v
  | otherwise = maybe (yamlToJSON v) yamlToJSON (YD.resolveExplicitTag tag v)


builderText :: TB.Builder -> Text
builderText = TL.toStrict . TB.toLazyText


showKey :: YV.Value -> Text
showKey v = case yamlToJSON v of
  A.String s -> s
  A.Null -> T.pack ""
  A.Bool b -> if b then T.pack "true" else T.pack "false"
  A.Number n -> builderText (scientificBuilder n)
  _ -> T.pack "<complex>"


jsonToYAML :: A.Value -> YV.Value
jsonToYAML = \case
  A.Null -> YV.YNull
  A.Bool b -> YV.YBool b
  A.String t -> YV.YString t
  A.Number n ->
    case Sci.floatingOrInteger n of
      Left d -> YV.YFloat (d :: Double)
      Right i -> YV.YInt (i :: Integer)
  A.Array xs -> YV.YSeq (V.map jsonToYAML xs)
  A.Object o ->
    YV.YMap (V.fromList (fmap mkPair (AKM.toList o)))
  where
    mkPair (k, val) = (YV.YString (AK.toText k), jsonToYAML val)
