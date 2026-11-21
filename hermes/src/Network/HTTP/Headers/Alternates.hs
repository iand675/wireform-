{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | RFC 2295 §§5, 6.4 and 8.3 variant descriptions and proxy directives.
module Network.HTTP.Headers.Alternates (
  Alternates (Alternates, rawAlternates),
  Alternate (..),
  Variant (..),
  VariantAttribute (..),
  VariantDescription (..),
  VariantFeature (..),
  FeatureRequirement (..),
  FeaturePredicate (..),
  FeatureRange (..),
  FeatureFactors (..),
  ShortFloat,
  shortFloatThousandths,
  mkShortFloat,
  alternateEntries,
  alternatesFromEntries,
  alternatesParser,
  renderAlternates,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAsciiUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word32)
import Network.HTTP.ContentNegotiation (MediaRange (..), mediaTypeWithParametersParser, qualityValueParser, renderMediaRange, renderQualityValue)
import Network.HTTP.Grammar.LanguageTag (languageTagBytes)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (renderHttpQuotedString, shortText)
import Network.HTTP.Grammar.URIReference (uriReference)
import Network.HTTP.Headers
import Network.HTTP.Headers.AcceptFeatures (FeatureCondition, FeatureTag, decodeFeatureOctets, featureConditionParser, featureTokenOrQuoted, quoteFeatureOctets, renderFeatureCondition)
import Network.HTTP.Headers.ContentLanguage (LanguageTag (..))
import Network.HTTP.Headers.HeaderFieldName (hAlternates)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Negotiate (RvsaVersion, parseRvsaVersion, renderRvsaVersion)
import Numeric.Natural (Natural)


data Alternates
  = RawAlternates !ST.ShortText
  | ParsedAlternates !ByteString !(NonEmpty Alternate)


{- | The original text API. Typed lookup validates the grammar. Constructing
with this pattern retains the caller's text. Checked writes validate it.
-}
pattern Alternates :: ST.ShortText -> Alternates
pattern Alternates {rawAlternates} <- (alternatesText -> rawAlternates)
  where
    Alternates value = RawAlternates value


{-# COMPLETE Alternates #-}


instance Eq Alternates where
  a == b = alternatesWire a == alternatesWire b


instance Show Alternates where
  showsPrec precedence (Alternates value) =
    showParen (precedence > 10) $ showString "Alternates {rawAlternates = " . shows value . showChar '}'


alternatesText :: Alternates -> ST.ShortText
alternatesText (RawAlternates value) = value
alternatesText (ParsedAlternates wire _) =
  maybe (ST.fromText (TE.decodeLatin1 wire)) id (ST.fromByteString wire)


alternatesWire :: Alternates -> ByteString
alternatesWire (RawAlternates value) = ST.toByteString value
alternatesWire (ParsedAlternates wire _) = wire


data Alternate
  = AlternateVariant !Variant
  | AlternateFallback !ST.ShortText
  | AlternateProxyRVSA ![RvsaVersion]
  | AlternateDirective !ST.ShortText !(Maybe ByteString)
  deriving stock (Eq, Show)


-- | URI reference, source quality, and attributes in wire order.
data Variant = Variant
  { variantURI :: !ST.ShortText
  , variantSourceQuality :: {-# UNPACK #-} !Double
  , variantAttributes :: ![VariantAttribute]
  }
  deriving stock (Eq, Show)


data VariantAttribute
  = VariantType !MediaRange
  | VariantCharset !ST.ShortText
  | VariantLanguages !(NonEmpty LanguageTag)
  | VariantLength !Natural
  | VariantFeatures !(NonEmpty VariantFeature)
  | VariantDescriptionAttribute !VariantDescription
  | VariantExtension !ST.ShortText !ByteString
  deriving stock (Eq, Show)


data VariantDescription = VariantDescription
  { variantDescriptionText :: !Text
  , variantDescriptionLanguage :: !(Maybe LanguageTag)
  }
  deriving stock (Eq, Show)


data VariantFeature = VariantFeature
  { variantFeatureRequirement :: !FeatureRequirement
  , variantFeatureFactors :: !FeatureFactors
  }
  deriving stock (Eq, Show)


data FeatureRequirement
  = RequiredPredicate !FeaturePredicate
  | AnyPredicate !(NonEmpty FeaturePredicate)
  deriving stock (Eq, Show)


data FeaturePredicate = FeaturePredicate !FeatureTag !(FeatureCondition FeatureRange)
  deriving stock (Eq, Show)


-- | A missing lower or upper bound means zero or infinity, respectively.
data FeatureRange = FeatureRange
  { featureLowerBound :: !(Maybe Natural)
  , featureUpperBound :: !(Maybe Natural)
  }
  deriving stock (Eq, Show)


data FeatureFactors = FeatureFactors
  { featureTrueImprovement :: !(Maybe ShortFloat)
  , featureFalseDegradation :: !(Maybe ShortFloat)
  }
  deriving stock (Eq, Show)


-- | Exact thousandths for RFC 2295's @1*3DIGIT [ "." 0*3DIGIT ]@.
newtype ShortFloat = ShortFloat {shortFloatThousandths :: Word32}
  deriving stock (Eq, Ord, Show)


mkShortFloat :: Word32 -> Maybe ShortFloat
mkShortFloat value
  | value <= 999999 = Just (ShortFloat value)
  | otherwise = Nothing


alternateEntries :: Alternates -> Either String (NonEmpty Alternate)
alternateEntries (ParsedAlternates _ entries) = Right entries
alternateEntries (RawAlternates wire) =
  parseFieldLine "Alternates" alternateListParser (ST.toByteString wire)


{- | Construct an ordered variant list, checking attribute cardinality,
fallback uniqueness, and that each entry is representable.
-}
alternatesFromEntries :: NonEmpty Alternate -> Either String Alternates
alternatesFromEntries entries = do
  value <- parseFieldLine "Alternates" alternatesParser wire
  if alternateEntries value == Right entries
    then Right value
    else Left "Alternates entries do not match their wire representation"
  where
    (first :| rest) = entries
    wire = M.toStrictByteString (renderAlternate first <> foldMap ((", " <>) . renderAlternate) rest)


instance KnownHeader Alternates where
  type ParseFailure Alternates = String
  type Cardinality Alternates = 'ZeroOrOne
  type Direction Alternates = 'Response


  parseFromHeaders _ (wire :| []) = ParsedAlternates wire <$> parseFieldLine "Alternates" alternateListParser wire
  parseFromHeaders _ fields = do
    entries <- parseListFieldLines "Alternates" (httpList alternateParser) fields >>= checkAlternateList
    pure (ParsedAlternates (BS.intercalate ", " (NE.toList fields)) entries)


  renderToHeaders _ = alternatesWire


  renderToHeadersChecked _ (ParsedAlternates wire _) = Just wire
  renderToHeadersChecked _ (RawAlternates value) =
    case parseFieldLine "Alternates" alternateListParser wire of
      Left _ -> Nothing
      Right _ -> Just wire
    where
      wire = ST.toByteString value


  headerName _ = hAlternates


alternatesParser :: ParserT st String Alternates
alternatesParser = withByteString alternateListParser $ \entries wire ->
  pure (ParsedAlternates wire entries)


renderAlternates :: Alternates -> M.Builder
renderAlternates (RawAlternates value) = shortText value
renderAlternates (ParsedAlternates wire _) = M.byteString wire


alternateListParser :: ParserT st String (NonEmpty Alternate)
alternateListParser = httpList alternateParser >>= either err pure . checkAlternateList


checkAlternateList :: [Alternate] -> Either String (NonEmpty Alternate)
checkAlternateList entries = do
  nonempty <- requireNonEmptyList "Alternates" entries
  case dropWhile (not . isFallback) entries of
    _ : rest | any isFallback rest -> Left "Duplicate Alternates fallback"
    _ -> Right nonempty
  where
    isFallback (AlternateFallback _) = True
    isFallback _ = False


alternateParser :: ParserT st String Alternate
alternateParser = branch ($(char '{') *> ows) variantOrFallback directive
  where
    variantOrFallback = do
      uri <- quotedVariantURI <* ows
      branch
        $(char '}')
        (pure (AlternateFallback uri))
        ( do
            quality <- qualityValueParser
            attrs <- attributes Set.empty
            pure (AlternateVariant (Variant uri quality attrs))
        )
    directive = do
      name <- rfc9110TokenBS
      if foldAscii name == "proxy-rvsa"
        then do
          versions <- ows *> $(char '=') *> ows *> httpQuotedWireBytes
          AlternateProxyRVSA <$> either err pure (parseFieldLine "proxy-rvsa" (httpList rvsaVersionParser) versions)
        else AlternateDirective (STU.fromByteStringUnsafe name) <$> optional (ows *> $(char '=') *> ows *> featureTokenOrQuoted)
{-# NOINLINE alternateParser #-}


-- Keep the shared RFC 3986 scanner out of the variant list driver.
quotedVariantURI :: ParserT st String ST.ShortText
quotedVariantURI = $(char '"') *> uriReference <* $(char '"')
{-# NOINLINE quotedVariantURI #-}


rvsaVersionParser :: ParserT st String RvsaVersion
rvsaVersionParser = withByteString (skipSome (skipSatisfyAscii isDigit) *> $(char '.') *> skipSome (skipSatisfyAscii isDigit)) $ \() bytes ->
  maybe failed pure (parseRvsaVersion bytes)


attributes :: Set.Set ByteString -> ParserT st String [VariantAttribute]
attributes seen = do
  ows
  branch $(char '}') (pure []) $ do
    name <- $(char '{') *> ows *> rfc9110TokenBS
    let folded = foldAscii name
    if Set.member folded seen
      then err "Duplicate Alternates variant attribute"
      else do
        attribute <- variantAttribute name folded
        rest <- ows *> $(char '}') *> attributes (Set.insert folded seen)
        pure (attribute : rest)


foldAscii :: ByteString -> ByteString
foldAscii bytes
  | BSC.any isAsciiUpper bytes = BSC.map toLower bytes
  | otherwise = bytes


variantAttribute :: ByteString -> ByteString -> ParserT st String VariantAttribute
variantAttribute name = \case
  "type" -> do
    media <- ows *> mediaTypeWithParametersParser
    if any ((== "charset") . foldAscii . ST.toByteString . fst) (mediaParams media)
      then err "Alternates type carries charset in a separate attribute"
      else pure (VariantType media)
  "charset" -> VariantCharset <$> (ows *> rfc9110Token)
  "language" -> VariantLanguages <$> (httpList languageParser >>= either err pure . requireNonEmptyList "Alternates language")
  "length" -> VariantLength <$> (ows *> naturalParser)
  "features" -> VariantFeatures <$> (ows *> whitespaceList variantFeatureParser)
  "description" -> do
    wire <- ows *> httpQuotedBytes
    text <- either (const (err "Invalid UTF-8 Alternates description")) pure (TE.decodeUtf8' (decodeFeatureOctets wire))
    language <- optional (ows *> languageParser)
    pure (VariantDescriptionAttribute (VariantDescription text language))
  _ -> VariantExtension (STU.fromByteStringUnsafe name) <$> extensionBody


extensionBody :: ParserT st String ByteString
extensionBody =
  byteStringOf (skipMany (asciiSetBytes extensionChars <|> (() <$ httpQuotedWireBytes)))
  where
    extensionChars = asciiSet (tokenCharSet <> " \t()<>@,;:\\/[]?={")


languageParser :: ParserT st e LanguageTag
languageParser = LanguageTag <$> shortASCIIFromParser_ languageTagBytes
{-# INLINE languageParser #-}


naturalParser :: ParserT st e Natural
naturalParser = withByteString (skipSome (skipSatisfyAscii isDigit)) $ \() bytes ->
  case BSC.readNatural bytes of
    Just (value, _) -> pure value
    Nothing -> failed
{-# INLINE naturalParser #-}


whitespaceList :: ParserT st e a -> ParserT st e (NonEmpty a)
whitespaceList element = (:|) <$> element <*> many (rws *> element)
{-# INLINE whitespaceList #-}


variantFeatureParser :: ParserT st String VariantFeature
variantFeatureParser = VariantFeature <$> requirement <*> factors
  where
    requirement =
      branch
        ($(char '[') *> ows)
        (AnyPredicate <$> whitespaceList predicate <* ows <* $(char ']'))
        (RequiredPredicate <$> predicate)
    predicate = featureConditionParser range FeaturePredicate
    range =
      FeatureRange
        <$> ($(char '[') *> ows *> optional naturalParser)
        <*> (ows *> $(char '-') *> ows *> optional naturalParser <* ows <* $(char ']'))
    factors =
      withOption
        (ows *> $(char ';'))
        ( \() ->
            FeatureFactors
              <$> optional (ows *> $(char '+') *> ows *> shortFloatParser)
              <*> optional (ows *> $(char '-') *> ows *> shortFloatParser)
        )
        (pure (FeatureFactors Nothing Nothing))
{-# NOINLINE variantFeatureParser #-}


shortFloatParser :: ParserT st String ShortFloat
shortFloatParser = do
  integer <- threeDigits anyAsciiDecimalWord (\value _ -> value)
  fractional <- branch $(char '.') (threeDigits (anyAsciiDecimalWord <|> pure 0) scale) (pure 0)
  pure (ShortFloat (fromIntegral (integer * 1000 + fractional)))
  where
    threeDigits digits convert = withSpan digits $ \value (Span (Pos start) (Pos end)) ->
      let size = end - start
      in if size <= 3 then pure (convert value size) else err "Alternates feature factor has more than three digits"
    scale value 1 = value * 100
    scale value 2 = value * 10
    scale value _ = value


renderAlternate :: Alternate -> M.Builder
renderAlternate = \case
  AlternateVariant (Variant uri quality attrs) ->
    "{\"" <> shortText uri <> "\" " <> renderQualityValue quality <> foldMap ((" " <>) . renderAttribute) attrs <> "}"
  AlternateFallback uri -> "{\"" <> shortText uri <> "\"}"
  AlternateProxyRVSA versions -> "proxy-rvsa=\"" <> commaList renderRvsaVersion versions <> "\""
  AlternateDirective name value -> shortText name <> foldMap (("=" <>) . renderHttpQuotedString . TE.decodeLatin1) value


renderAttribute :: VariantAttribute -> M.Builder
renderAttribute attribute = "{" <> body attribute <> "}"
  where
    body = \case
      VariantType media -> "type " <> renderMediaRange media
      VariantCharset charset -> "charset " <> shortText charset
      VariantLanguages tags -> "language " <> commaList renderLanguage (NE.toList tags)
      VariantLength size -> "length " <> renderNatural size
      VariantFeatures features -> "features " <> spaces renderFeature features
      VariantDescriptionAttribute (VariantDescription description language) ->
        "description " <> renderDescription description <> foldMap ((" " <>) . renderLanguage) language
      VariantExtension name value -> shortText name <> M.byteString value
    renderDescription = quoteFeatureOctets . TE.encodeUtf8


renderLanguage :: LanguageTag -> M.Builder
renderLanguage (LanguageTag tag) = shortText tag


renderNatural :: Natural -> M.Builder
renderNatural = M.integerDec . toInteger


renderFeature :: VariantFeature -> M.Builder
renderFeature (VariantFeature requirement (FeatureFactors improvement degradation)) =
  case requirement of
    RequiredPredicate predicate -> renderPredicate predicate <> suffix
    AnyPredicate predicates -> "[" <> spaces renderPredicate predicates <> "]" <> suffix
  where
    suffix = case (improvement, degradation) of
      (Nothing, Nothing) -> mempty
      _ -> ";" <> foldMap (("+" <>) . renderShortFloat) improvement <> foldMap (("-" <>) . renderShortFloat) degradation
    renderPredicate (FeaturePredicate tag test) = renderFeatureCondition renderRange tag test
    renderRange (FeatureRange lower upper) = "[" <> foldMap renderNatural lower <> "-" <> foldMap renderNatural upper <> "]"


renderShortFloat :: ShortFloat -> M.Builder
renderShortFloat (ShortFloat value) =
  let (major, minor) = quotRem value 1000
  in M.word32Dec major
       <> if minor == 0
         then mempty
         else "." <> (if minor < 100 then "0" else mempty) <> (if minor < 10 then "0" else mempty) <> M.word32Dec minor


commaList :: (a -> M.Builder) -> [a] -> M.Builder
commaList _ [] = mempty
commaList render (first : rest) = render first <> foldMap ((", " <>) . render) rest


spaces :: (a -> M.Builder) -> NonEmpty a -> M.Builder
spaces render (first :| rest) = render first <> foldMap ((" " <>) . render) rest
