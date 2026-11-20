{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@IM@ lists the instance manipulations applied to a response in application
order. RFC 3229 allows each token to carry an optional named extension
parameter with an optional value.

<https://www.rfc-editor.org/rfc/rfc3229.html#section-10.5.2 RFC 3229 §10.5.2>

See also: "Network.HTTP.Headers.AIM", "Network.HTTP.Headers.DeltaBase".
-}
module Network.HTTP.Headers.IM (
  IM (IM, instanceManipulations, IMEntries, instanceManipulationEntries),
  InstanceManipulation (..),
  IMParameter (..),
  IMParameterValue (..),
  iMParser,
  renderIM,
  imParameterParser,
  imParameterSeparator,
  renderIMParameter,
) where

import qualified Data.ByteString.Short as SBS
import Data.Maybe (isNothing)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIM)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeHttpQuotedBytes, renderSeparated, shortText)


data InstanceManipulation = InstanceManipulation
  { manipulationName :: !ST.ShortText
  , manipulationParameter :: !(Maybe IMParameter)
  }
  deriving stock (Eq, Show)


data IMParameter = IMParameter
  { imParameterName :: !ST.ShortText
  , imParameterValue :: !(Maybe IMParameterValue)
  }
  deriving stock (Eq, Show)


data IMParameterValue
  = IMParameterToken !ST.ShortText
  | IMParameterQuoted !SBS.ShortByteString
  deriving stock (Eq, Show)


-- | The ordered list of manipulations applied to the response.
newtype IM = IMEntries {instanceManipulationEntries :: [InstanceManipulation]}
  deriving stock (Eq)


-- | Construct a list of bare manipulations or read just the names.
pattern IM :: [ST.ShortText] -> IM
pattern IM {instanceManipulations} <- (map manipulationName . instanceManipulationEntries -> instanceManipulations)
  where
    IM names = IMEntries (map (\name -> InstanceManipulation name Nothing) names)


{-# COMPLETE IM #-}


instance Show IM where
  showsPrec precedence (IMEntries entries) =
    showParen (precedence > 10) (record . showChar '}')
    where
      record
        | all (isNothing . manipulationParameter) entries =
            showString "IM {instanceManipulations = " . shows (map manipulationName entries)
        | otherwise =
            showString "IMEntries {instanceManipulationEntries = " . shows entries


instance KnownHeader IM where
  type ParseFailure IM = String
  type Cardinality IM = 'ZeroOrOne
  type Direction IM = 'Response


  parseFromHeaders _ = fmap IMEntries . parseFieldLine "IM" imList . combineFieldLines ","


  renderToHeaders _ = M.toStrictByteString . renderIM


  headerName _ = hIM


iMParser :: ParserT st String IM
iMParser = IMEntries <$> imList


imList :: ParserT st String [InstanceManipulation]
imList = httpList (branch identity (err "identity is reserved for A-IM") item)
  where
    identity =
      $(switchAsciiCI [|case _ of "identity" -> pure ()|])
        <* notFollowedBy (skipSatisfyAscii isTokenChar)
    item = InstanceManipulation <$> rfc9110Token <*> optional (imParameterSeparator *> imParameterParser)


imParameterSeparator :: ParserT st e ()
imParameterSeparator = ows *> $(char ';') *> ows
{-# INLINE imParameterSeparator #-}


-- | The extension parameter after its semicolon. The name q is reserved.
imParameterParser :: ParserT st String IMParameter
imParameterParser = do
  name <- rfc9110Token
  if name == "q" || name == "Q"
    then err "q is reserved for A-IM weights"
    else IMParameter name <$> optional (ows *> $(char '=') *> ows *> value)
  where
    value = branch (lookahead $(char '"')) (IMParameterQuoted . SBS.toShort <$> httpQuotedBytes) (IMParameterToken <$> rfc9110Token)


renderIM :: IM -> M.Builder
renderIM (IMEntries entries) = renderSeparated ", " renderEntry entries
  where
    renderEntry (InstanceManipulation name param) = shortText name <> foldMap renderIMParameter param


renderIMParameter :: IMParameter -> M.Builder
renderIMParameter (IMParameter name value) =
  M.char7 ';' <> shortText name <> foldMap (\v -> M.char7 '=' <> renderValue v) value
  where
    renderValue (IMParameterToken token) = shortText token
    renderValue (IMParameterQuoted bytes) = M.byteString (encodeHttpQuotedBytes bytes)
