{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
@A-IM@ advertises the instance manipulations a client accepts, in the order
they may be applied. Each manipulation may have an extension parameter
followed by a quality weight.

<https://www.rfc-editor.org/rfc/rfc3229.html#section-10.5.3 RFC 3229 §10.5.3>

See also: "Network.HTTP.Headers.IM", "Network.HTTP.Headers.DeltaBase".
-}
module Network.HTTP.Headers.AIM (
  AIM (..),
  WeightedIM (WeightedIM, WeightedIMWithParameter, imTag, imWeight),
  weightedIMParameter,
  IMParameter (..),
  IMParameterValue (..),
  aIMParser,
  renderAIM,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (qualityValueParser, renderWeight)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAIM)
import Network.HTTP.Headers.IM (IMParameter (..), IMParameterValue (..), imParameterParser, imParameterSeparator, renderIMParameter)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderSeparated, shortText)


-- | An instance manipulation with a quality weight (default 1.0).
data WeightedIM
  = WeightedIM
      { imTag :: !ST.ShortText
      , imWeight :: {-# UNPACK #-} !Double
      }
  | WeightedIMWithParameter
      { imTag :: !ST.ShortText
      , imWeight :: {-# UNPACK #-} !Double
      , imParameter :: !IMParameter
      }
  deriving stock (Eq, Show)


weightedIMParameter :: WeightedIM -> Maybe IMParameter
weightedIMParameter WeightedIM {} = Nothing
weightedIMParameter WeightedIMWithParameter {imParameter = param} = Just param


newtype AIM = AIM
  { acceptInstanceManipulations :: [WeightedIM]
  }
  deriving stock (Eq, Show)


instance KnownHeader AIM where
  type ParseFailure AIM = String
  type Cardinality AIM = 'ZeroOrOne
  type Direction AIM = 'Request


  parseFromHeaders _ = fmap AIM . parseFieldLine "A-IM" aimList . combineFieldLines ","


  renderToHeaders _ = M.toStrictByteString . renderAIM


  headerName _ = hAIM


aIMParser :: ParserT st String AIM
aIMParser = AIM <$> aimList


aimList :: ParserT st String [WeightedIM]
aimList = httpList $ do
  tag <- rfc9110Token
  branch imParameterSeparator (branch qualityName (WeightedIM tag <$> qualityValue) (withParameter tag)) (pure (WeightedIM tag 1))
  where
    withParameter tag = do
      param <- imParameterParser
      weight <- branch imParameterSeparator (qualityName *> qualityValue) (pure 1)
      pure (WeightedIMWithParameter tag weight param)
    qualityName =
      skipSatisfyAscii (\c -> c == 'q' || c == 'Q') <* notFollowedBy (skipSatisfyAscii isTokenChar)
    qualityValue = ows *> $(char '=') *> ows *> qualityValueParser


renderAIM :: AIM -> M.Builder
renderAIM = renderSeparated ", " renderMember . acceptInstanceManipulations
  where
    renderMember value =
      shortText (imTag value) <> foldMap renderIMParameter (weightedIMParameter value) <> renderWeight (imWeight value)
