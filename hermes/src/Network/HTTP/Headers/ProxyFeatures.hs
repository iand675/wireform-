{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | Proxy capability advertisement from the W3C Notification for Proxy Caches
working draft. See <https://www.w3.org/TR/WD-proxy>.
-}
module Network.HTTP.Headers.ProxyFeatures (
  ProxyFeatures (ProxyFeatures, proxyFeaturesValue),
  ProxyFeaturesDetails (..),
  ProxyFeature (..),
  ProxyLogOffer (..),
  ProxyLogOption (..),
  ProxyPeakHours,
  proxyPeakHoursMask,
  mkProxyPeakHours,
  proxyFeaturesDetails,
  proxyFeaturesFromDetails,
  proxyFeaturesParser,
  renderProxyFeatures,
  proxyNameParser,
) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word32, Word8)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Grammar.URIReference (absoluteURIBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyFeatures)
import qualified Network.HTTP.Headers.Mason as M
import Numeric.Natural (Natural)


data ProxyFeatures
  = RawProxyFeatures !ST.ShortText
  | ParsedProxyFeatures !ByteString !ProxyFeaturesDetails


-- | The legacy wire spelling. Use 'proxyFeaturesFromDetails' for checked authoring.
pattern ProxyFeatures :: ST.ShortText -> ProxyFeatures
pattern ProxyFeatures {proxyFeaturesValue} <- (legacyText -> proxyFeaturesValue)
  where
    ProxyFeatures raw = RawProxyFeatures raw


{-# COMPLETE ProxyFeatures #-}


instance Eq ProxyFeatures where
  left == right = wireBytes left == wireBytes right


instance Show ProxyFeatures where
  showsPrec precedence (ProxyFeatures value) =
    showParen (precedence > 10) $ showString "ProxyFeatures {proxyFeaturesValue = " . shows value . showChar '}'


legacyText :: ProxyFeatures -> ST.ShortText
legacyText (RawProxyFeatures text) = text
legacyText (ParsedProxyFeatures wire _) = STU.fromByteStringUnsafe wire


wireBytes :: ProxyFeatures -> ByteString
wireBytes (RawProxyFeatures text) = ST.toByteString text
wireBytes (ParsedProxyFeatures wire _) = wire


data ProxyFeaturesDetails = ProxyFeaturesDetails
  { proxyFeaturesName :: !ByteString
  , proxyFeatureOptions :: !(NonEmpty ProxyFeature)
  }
  deriving stock (Eq, Show)


data ProxyFeature
  = ProxyNotify
  | ProxyLogging
  | ProxyLog !ProxyLogOffer
  deriving stock (Eq, Show)


data ProxyLogOffer = ProxyLogOffer
  { proxyLogURI :: !ByteString
  , proxyLogOptions :: !(NonEmpty ProxyLogOption)
  }
  deriving stock (Eq, Show)


data ProxyLogOption
  = ProxyLogExpiryHours !Natural
  | ProxyLogPeakHours !ProxyPeakHours
  deriving stock (Eq, Show)


-- | A 24-bit mask of peak hours, starting at midnight GMT in bit zero.
newtype ProxyPeakHours = ProxyPeakHours {proxyPeakHoursMask :: Word32}
  deriving stock (Eq, Show)


mkProxyPeakHours :: Word32 -> Maybe ProxyPeakHours
mkProxyPeakHours mask
  | mask <= 0xffffff = Just (ProxyPeakHours mask)
  | otherwise = Nothing


proxyFeaturesDetails :: ProxyFeatures -> Either String ProxyFeaturesDetails
proxyFeaturesDetails (RawProxyFeatures text) =
  parseSingleFieldLine "Proxy-Features" featuresParser (ST.toByteString text :| [])
proxyFeaturesDetails (ParsedProxyFeatures _ details) = Right details


proxyFeaturesFromDetails :: ProxyFeaturesDetails -> Either String ProxyFeatures
proxyFeaturesFromDetails details = do
  parsed <- parseFieldLine "Proxy-Features" proxyFeaturesParser (M.toStrictByteString (renderDetails details))
  if proxyFeaturesDetails parsed == Right details
    then Right parsed
    else Left "Proxy-Features details do not match their wire representation"


instance KnownHeader ProxyFeatures where
  type ParseFailure ProxyFeatures = String
  type Cardinality ProxyFeatures = 'ZeroOrOne
  type Direction ProxyFeatures = 'Request


  parseFromHeaders _ fields@(wire :| []) =
    ParsedProxyFeatures wire <$> parseSingleFieldLine "Proxy-Features" featuresParser fields
  parseFromHeaders _ _ = Left "Multiple Proxy-Features field lines"


  renderToHeaders _ = wireBytes


  renderToHeadersChecked _ (ParsedProxyFeatures wire _) = Just wire
  renderToHeadersChecked _ (RawProxyFeatures text) =
    let wire = ST.toByteString text
    in either (const Nothing) (const (Just wire)) $
         parseSingleFieldLine "Proxy-Features" featuresParser (wire :| [])


  headerName _ = hProxyFeatures


proxyFeaturesParser :: ParserT st String ProxyFeatures
proxyFeaturesParser = withByteString (ows *> featuresParser <* ows) $ \details wire ->
  pure (ParsedProxyFeatures wire details)


featuresParser :: ParserT st String ProxyFeaturesDetails
featuresParser = do
  name <- proxyNameParser
  feature <- separator *> featureParser
  rest <- many (separator *> featureParser)
  let options = feature :| rest
  if elem ProxyLogging options && any isOffer options
    then err "Proxy-Features log and logging are mutually exclusive"
    else pure (ProxyFeaturesDetails name options)
  where
    isOffer (ProxyLog _) = True
    isOffer _ = False


featureParser :: ParserT st String ProxyFeature
featureParser =
  optionName >>= \case
    NotifyName -> pure ProxyNotify
    LoggingName -> pure ProxyLogging
    LogName -> do
      uri <- assignment logURI
      first <- separator *> logOption
      rest <- many (separator *> logOption)
      pure (ProxyLog (ProxyLogOffer uri (first :| rest)))
    _ -> err "Expected notify, log or logging in Proxy-Features"
{-# NOINLINE featureParser #-}


logOption :: ParserT st String ProxyLogOption
logOption =
  optionName >>= \case
    ExpiryName -> ProxyLogExpiryHours <$> assignment hours
    PeakName -> ProxyLogPeakHours <$> assignment peakHours
    _ -> failed
  where
    hours = do
      digits <- byteStringOf (skipSome (skipSatisfyAscii isDigit))
      case BSC.readNatural digits of
        Just (value, _) -> pure value
        Nothing -> failed


peakHours :: ParserT st String ProxyPeakHours
peakHours = do
  digits <- takeBs 6
  let scan !i !mask
        | i == 6 = pure (ProxyPeakHours mask)
        | octet >= 48 && octet <= 57 = next (octet - 48)
        | folded >= 97 && folded <= 102 = next (folded - 87)
        | otherwise = err "Proxy-Features peak requires six hexadecimal digits"
        where
          octet = BSU.unsafeIndex digits i
          folded = if octet >= 65 && octet <= 70 then octet + 32 else octet
          next nibble = scan (i + 1) (16 * mask + fromIntegral nibble)
  scan 0 0


-- URI sub-delimiters include semicolons. A semicolon followed by a draft
-- directive name at a field boundary belongs to the header grammar.
logURI :: ParserT st String ByteString
logURI = do
  wire <- byteStringOf (skipSome (asciiSetBytes uriRun <|> semicolonInURI))
  case parseFieldLine "Proxy-Features log URI" absoluteURIBytes wire of
    Right () -> pure wire
    Left failure -> err failure
  where
    semicolonInURI = $(char ';') *> notFollowedBy (ows *> optionName *> ows *> ($(char '=') <|> $(char ';') <|> eof))
{-# NOINLINE logURI #-}


uriRun :: AsciiSet
uriRun = asciiSet (CharSet.difference (CharSet.range '!' '~') ";")
{-# NOINLINE uriRun #-}


data OptionName = NotifyName | LogName | LoggingName | ExpiryName | PeakName


optionName :: ParserT st e OptionName
optionName = do
  name <- rfc9110TokenBS
  case if BSC.any isAsciiUpper name then BSC.map toLower name else name of
    "notify" -> pure NotifyName
    "log" -> pure LogName
    "logging" -> pure LoggingName
    "loging" -> pure LoggingName
    "expires" -> pure ExpiryName
    "expiry" -> pure ExpiryName
    "peak" -> pure PeakName
    _ -> failed
{-# NOINLINE optionName #-}


assignment :: ParserT st e a -> ParserT st e a
assignment value = ows *> $(char '=') *> ows *> value
{-# INLINE assignment #-}


separator :: ParserT st e ()
separator = ows *> $(char ';') *> ows
{-# INLINE separator #-}


-- | An ASCII DNS host name, allowing a final root dot.
proxyNameParser :: ParserT st String ByteString
proxyNameParser = do
  name <- rfc9110TokenBS
  if validName name then pure name else err "Invalid DNS proxy name"
{-# INLINE proxyNameParser #-}


validName :: ByteString -> Bool
validName name
  | size > 254 = False
  | size == 254 && BSU.unsafeLast name /= 46 = False
  | otherwise = check 0 0 False
  where
    size = BS.length name
    check :: Int -> Int -> Bool -> Bool
    check !offset !width !hyphen
      | offset == size = not hyphen && (width > 0 || offset > 0)
      | isAlnum octet = width < 63 && check (offset + 1) (width + 1) False
      | octet == 46 = width > 0 && not hyphen && check (offset + 1) 0 False
      | octet == 45 = width > 0 && width < 63 && check (offset + 1) (width + 1) True
      | otherwise = False
      where
        octet = BSU.unsafeIndex name offset
    isAlnum octet =
      let letter = octet .|. 32
      in (letter >= 97 && letter <= 122) || (octet >= 48 && octet <= 57)


renderProxyFeatures :: ProxyFeatures -> M.Builder
renderProxyFeatures (RawProxyFeatures raw) = shortText raw
renderProxyFeatures (ParsedProxyFeatures wire _) = M.byteString wire


renderDetails :: ProxyFeaturesDetails -> M.Builder
renderDetails (ProxyFeaturesDetails name options) = M.byteString name <> foldMap renderFeature options
  where
    renderFeature ProxyNotify = "; notify"
    renderFeature ProxyLogging = "; logging"
    renderFeature (ProxyLog (ProxyLogOffer uri settings)) = "; log=" <> M.byteString uri <> foldMap renderOption settings
    renderOption (ProxyLogExpiryHours value) = "; expires=" <> M.integerDec (toInteger value)
    renderOption (ProxyLogPeakHours (ProxyPeakHours mask)) =
      "; peak="
        <> digit (shiftR mask 20)
        <> digit (shiftR mask 16)
        <> digit (shiftR mask 12)
        <> digit (shiftR mask 8)
        <> digit (shiftR mask 4)
        <> digit mask
    digit value =
      let nibble = fromIntegral (value .&. 15) :: Word8
      in M.word8 (if nibble < 10 then nibble + 48 else nibble + 87)
