{-# LANGUAGE TemplateHaskell #-}

{- |
@Surrogate-Capability@ advertises the processing capabilities of each
surrogate in an HTTP request chain. Device tokens are unique across the
field, and entries and capability tokens retain their wire order.

A capability has a case-sensitive name and an optional slash-delimited
version. Each device's nonempty set is enclosed in double quotes.

Spec: <https://www.w3.org/TR/edge-arch/>

See also: "Network.HTTP.Headers.SurrogateControl", "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.SurrogateCapability (
  SurrogateCapability (..),
  SurrogateCapabilityEntry (..),
  SurrogateFeature (..),
  surrogateCapabilityFeatures,
  surrogateCapabilityFromFeatures,
  surrogateCapabilitySetParser,
  parseSurrogateFeatureSet,
  isSurrogateFeatureSet,
  renderSurrogateFeatureSet,
  surrogateCapabilityParser,
  renderSurrogateCapability,
) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (renderProductToken, shortText)
import qualified Network.HTTP.Grammar.Rendering as R
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSurrogateCapability)
import qualified Network.HTTP.Headers.Mason as M
import qualified Wireform.Builder as WB
import Wireform.Encode.Direct (dBytes, dWord8)


newtype SurrogateCapability = SurrogateCapability
  { surrogateCapabilityEntries :: NonEmpty SurrogateCapabilityEntry
  }
  deriving stock (Eq, Show)


data SurrogateCapabilityEntry = SurrogateCapabilityEntry
  { surrogateCapabilityDevice :: !ST.ShortText
  , surrogateCapabilitySet :: !ST.ShortText
  -- ^ The validated, unquoted wire spelling. See 'surrogateCapabilityFeatures'.
  }
  deriving stock (Eq, Show)


-- | A case-sensitive capability product token.
data SurrogateFeature = SurrogateFeature
  { surrogateFeatureName :: !ST.ShortText
  , surrogateFeatureVersion :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


instance KnownHeader SurrogateCapability where
  type ParseFailure SurrogateCapability = String
  type Cardinality SurrogateCapability = 'ZeroOrOne
  type Direction SurrogateCapability = 'Request


  parseFromHeaders _ fields = parseListFieldLines "Surrogate-Capability" capabilityList fields >>= validateEntries


  renderToHeaders _ value@(SurrogateCapability (first :| rest)) =
    WB.toStrictByteStringExact (foldl' (addEntry 5) (addEntry 3 0 first) rest) (renderSurrogateCapability value)
    where
      -- Three bytes for equals and quotes, plus two for each comma separator.
      addEntry punctuation size (SurrogateCapabilityEntry device capabilities) =
        add (add (add size punctuation) (byteLength device)) (byteLength capabilities)
      byteLength = SBS.length . ST.toShortByteString
      add = BSI.checkedAdd "Surrogate-Capability"


  renderToHeadersChecked _ (SurrogateCapability entries)
    | distinctDevices values = R.encodeCommaSeparatedChecked bound write values
    | otherwise = Nothing
    where
      values = NE.toList entries
      add = BSI.checkedAdd "Surrogate-Capability"
      width = SBS.length . ST.toShortByteString
      bound (SurrogateCapabilityEntry device capabilities)
        | isHttpTokenText device && isSurrogateFeatureSet capabilities = add (add 3 (width device)) (width capabilities)
        | otherwise = -1
      write ptr offset (SurrogateCapabilityEntry device capabilities) = do
        deviceEnd <- R.copyText ptr offset device
        opened <- dBytes ptr deviceEnd "=\""
        end <- R.copyText ptr opened capabilities
        dWord8 ptr end 34


  headerName _ = hSurrogateCapability


surrogateCapabilityParser :: ParserT st String SurrogateCapability
surrogateCapabilityParser = capabilityList >>= either err pure . validateEntries


capabilityList :: ParserT st String [SurrogateCapabilityEntry]
capabilityList = httpList $ do
  device <- rfc9110Token
  ows
  $(char '=')
  ows
  value <- surrogateCapabilitySetParser
  pure (SurrogateCapabilityEntry device value)


validateEntries :: [SurrogateCapabilityEntry] -> Either String SurrogateCapability
validateEntries entries = do
  nonEmpty <- requireNonEmptyList "Surrogate-Capability" entries
  if distinctDevices entries
    then Right (SurrogateCapability nonEmpty)
    else Left "Duplicate Surrogate-Capability device token"


distinctDevices :: [SurrogateCapabilityEntry] -> Bool
distinctDevices [_] = True
distinctDevices [a, b] = surrogateCapabilityDevice a /= surrogateCapabilityDevice b
distinctDevices xs = Set.size (Set.fromList (map surrogateCapabilityDevice xs)) == length xs


-- | Validate the unquoted sequence of products, separated by single spaces.
isSurrogateFeatureSet :: ST.ShortText -> Bool
isSurrogateFeatureSet value = feature 0
  where
    bytes = ST.toShortByteString value
    size = SBS.length bytes
    at = SBSI.unsafeIndex bytes
    tokenEnd offset
      | offset < size && isTokenChar (BSI.w2c (at offset)) = tokenEnd (offset + 1)
      | otherwise = offset
    feature start =
      let end = tokenEnd start
      in end /= start && (end == size || case at end of
            47 ->
              let version = end + 1
                  next = tokenEnd version
              in next /= version && separator next
            32 -> feature (end + 1)
            _ -> False)
    separator end = end == size || (at end == 32 && feature (end + 1))
{-# INLINE isSurrogateFeatureSet #-}


-- | Read a quoted capability set and retain its spelling without the quotes.
surrogateCapabilitySetParser :: ParserT st String ST.ShortText
surrogateCapabilitySetParser =
  $(char '"') *> shortASCIIFromParser_ (featureBytes *> skipMany ($(char ' ') *> featureBytes)) <* $(char '"')
  where
    featureBytes = tokenBytes *> optional_ ($(char '/') *> tokenBytes)


-- | Read the typed features in a device's advertised order.
surrogateCapabilityFeatures :: SurrogateCapabilityEntry -> Either String (NonEmpty SurrogateFeature)
surrogateCapabilityFeatures = parseSurrogateFeatureSet . surrogateCapabilitySet


-- | Read an unquoted capability set, as used by both surrogate fields.
parseSurrogateFeatureSet :: ST.ShortText -> Either String (NonEmpty SurrogateFeature)
parseSurrogateFeatureSet = parseFieldLine "surrogate capability set" features . ST.toByteString
  where
    features = (:|) <$> feature <*> many ($(char ' ') *> feature)
    feature = SurrogateFeature <$> rfc9110Token <*> optional ($(char '/') *> rfc9110Token)


-- | Build a capability entry from product tokens in advertised order.
surrogateCapabilityFromFeatures :: ST.ShortText -> NonEmpty SurrogateFeature -> SurrogateCapabilityEntry
surrogateCapabilityFromFeatures device =
  SurrogateCapabilityEntry device . STU.fromByteStringUnsafe . M.toStrictByteString . renderSurrogateFeatureSet


renderSurrogateFeatureSet :: NonEmpty SurrogateFeature -> M.Builder
renderSurrogateFeatureSet (first :| rest) = renderFeature first <> foldMap ((" " <>) . renderFeature) rest
  where
    renderFeature (SurrogateFeature name version) = renderProductToken name version


renderSurrogateCapability :: SurrogateCapability -> M.Builder
renderSurrogateCapability (SurrogateCapability (first :| rest)) =
  renderEntry first <> foldMap ((", " <>) . renderEntry) rest
  where
    renderEntry (SurrogateCapabilityEntry device value) = shortText device <> "=\"" <> shortText value <> M.char7 '"'
{-# INLINE renderSurrogateCapability #-}
