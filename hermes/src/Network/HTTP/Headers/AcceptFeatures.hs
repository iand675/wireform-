{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | RFC 2295 §8.2 feature-set information for remote variant selection.
module Network.HTTP.Headers.AcceptFeatures (
  AcceptFeatures (AcceptFeatures, rawAcceptFeatures),
  FeatureTag,
  featureTagBytes,
  parseFeatureTag,
  FeatureValue (..),
  FeatureCondition (..),
  FeatureExpression (..),
  FeatureExtension (..),
  FeatureEntry (..),
  acceptFeatureEntries,
  acceptFeaturesFromEntries,
  acceptFeaturesParser,
  renderAcceptFeatures,

  -- * Feature grammar shared with Alternates
  featureConditionParser,
  renderFeatureCondition,
  featureTokenOrQuoted,
  decodeFeatureOctets,
  renderFeatureOctets,
  quoteFeatureOctets,
) where

import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAsciiUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (renderHttpQuotedString, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptFeatures)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.URL.Decode (urlDecodeMaybe)


data AcceptFeatures
  = RawAcceptFeatures !ST.ShortText
  | ParsedAcceptFeatures !ByteString ![FeatureEntry]


{- | The original text API. Typed lookup validates the grammar. Constructing
with this pattern retains the caller's text for rendering.
-}
pattern AcceptFeatures :: ST.ShortText -> AcceptFeatures
pattern AcceptFeatures {rawAcceptFeatures} <- (featureText -> rawAcceptFeatures)
  where
    AcceptFeatures value = RawAcceptFeatures value


{-# COMPLETE AcceptFeatures #-}


instance Eq AcceptFeatures where
  a == b = featureWire a == featureWire b


instance Show AcceptFeatures where
  showsPrec precedence (AcceptFeatures value) =
    showParen (precedence > 10) $ showString "AcceptFeatures {rawAcceptFeatures = " . shows value . showChar '}'


featureText :: AcceptFeatures -> ST.ShortText
featureText (RawAcceptFeatures value) = value
featureText (ParsedAcceptFeatures wire _) =
  maybe (ST.fromText (TE.decodeLatin1 wire)) id (ST.fromByteString wire)


featureWire :: AcceptFeatures -> ByteString
featureWire (RawAcceptFeatures value) = ST.toByteString value
featureWire (ParsedAcceptFeatures wire _) = wire


-- | Case-insensitive ASCII feature tag, with quoted pairs removed.
newtype FeatureTag = FeatureTag {featureTagBytes :: ByteString}
  deriving stock (Eq, Ord, Show)


parseFeatureTag :: ByteString -> Either String FeatureTag
parseFeatureTag = parseFieldLine "feature tag" (featureTokenOrQuoted >>= normalizeTag)


-- | Case-sensitive octets after decoding quoted pairs and percent escapes.
newtype FeatureValue = FeatureValue {featureValueBytes :: ByteString}
  deriving stock (Eq, Ord, Show)


{- | The extra condition is an exclusive value in Accept-Features and a
numeric range in an Alternates feature predicate.
-}
data FeatureCondition a
  = FeaturePresent
  | FeatureAbsent
  | FeatureEqual !FeatureValue
  | FeatureUnequal !FeatureValue
  | FeatureSpecial !a
  deriving stock (Eq, Show)


data FeatureExpression
  = FeatureExpression !FeatureTag !(FeatureCondition FeatureValue)
  | FeatureWildcard
  deriving stock (Eq, Show)


data FeatureExtension = FeatureExtension
  { featureExtensionName :: !ST.ShortText
  , featureExtensionValue :: !(Maybe ByteString)
  }
  deriving stock (Eq, Show)


data FeatureEntry = FeatureEntry
  { featureExpression :: !FeatureExpression
  , featureExtensions :: ![FeatureExtension]
  }
  deriving stock (Eq, Show)


acceptFeatureEntries :: AcceptFeatures -> Either String [FeatureEntry]
acceptFeatureEntries (ParsedAcceptFeatures _ entries) = Right entries
acceptFeatureEntries (RawAcceptFeatures wire) =
  parseFieldLine "Accept-Features" featureEntriesParser (ST.toByteString wire)


-- | Render typed entries and check that each extension is representable.
acceptFeaturesFromEntries :: [FeatureEntry] -> Either String AcceptFeatures
acceptFeaturesFromEntries entries = do
  value <- parseFieldLine "Accept-Features" acceptFeaturesParser wire
  if acceptFeatureEntries value == Right entries
    then Right value
    else Left "Accept-Features entries do not match their wire representation"
  where
    wire = M.toStrictByteString (commaList renderEntry entries)


instance KnownHeader AcceptFeatures where
  type ParseFailure AcceptFeatures = String
  type Cardinality AcceptFeatures = 'ZeroOrOne
  type Direction AcceptFeatures = 'Request


  parseFromHeaders _ (wire :| []) = ParsedAcceptFeatures wire <$> parseFieldLine "Accept-Features" featureEntriesParser wire
  parseFromHeaders _ fields =
    ParsedAcceptFeatures (BS.intercalate ", " (NE.toList fields))
      <$> parseListFieldLines "Accept-Features" featureEntriesParser fields


  renderToHeaders _ = featureWire


  headerName _ = hAcceptFeatures


acceptFeaturesParser :: ParserT st String AcceptFeatures
acceptFeaturesParser = withByteString featureEntriesParser $ \entries wire ->
  pure (ParsedAcceptFeatures wire entries)


renderAcceptFeatures :: AcceptFeatures -> M.Builder
renderAcceptFeatures (RawAcceptFeatures value) = shortText value
renderAcceptFeatures (ParsedAcceptFeatures wire _) = M.byteString wire


featureEntriesParser :: ParserT st String [FeatureEntry]
featureEntriesParser = httpList $ FeatureEntry <$> expression <*> many extension
  where
    expression =
      (FeatureWildcard <$ ($(char '*') <* notFollowedBy (skipSatisfyAscii isTokenChar) <* notFollowedBy (ows *> operator)))
        <|> featureConditionParser ($(char '{') *> ows *> featureValueParser <* ows <* $(char '}')) FeatureExpression
    extension = do
      ows *> $(char ';') *> ows
      name <- rfc9110Token
      value <- optional (ows *> $(char '=') *> ows *> featureTokenOrQuoted)
      pure (FeatureExtension name value)
{-# NOINLINE featureEntriesParser #-}


featureConditionParser :: ParserT st String a -> (FeatureTag -> FeatureCondition a -> b) -> ParserT st String b
featureConditionParser special build =
  absent <|> quoted <|> bare
  where
    absent = do
      $(char '!') *> ows
      name <- featureTokenOrQuoted <* notFollowedBy (ows *> operator)
      tag <- normalizeTag name
      pure (build tag FeatureAbsent)
    quoted = quotedFeatureTag >>= named
    bare = do
      name <- bareFeatureTag
      if BS.length name > 1 && BS.last name == 33
        then branch $(char '=') (unequal (BS.init name)) (named (normalizeAsciiTag name))
        else named (normalizeAsciiTag name)
    named tag = build tag <$> namedCondition special
    unequal name = build (normalizeAsciiTag name) <$> (FeatureUnequal <$> (ows *> featureValueParser))
{-# INLINE featureConditionParser #-}


bareFeatureTag :: ParserT st e ByteString
bareFeatureTag = rfc9110TokenBS
{-# NOINLINE bareFeatureTag #-}


quotedFeatureTag :: ParserT st e FeatureTag
quotedFeatureTag = httpQuotedBytes >>= normalizeTag
{-# NOINLINE quotedFeatureTag #-}


namedCondition :: ParserT st String a -> ParserT st String (FeatureCondition a)
namedCondition special =
  withOption
    (ows *> operator)
    ( \negated ->
        ows
          *> if negated
            then FeatureUnequal <$> featureValueParser
            else (FeatureSpecial <$> special) <|> (FeatureEqual <$> featureValueParser)
    )
    (pure FeaturePresent)
{-# NOINLINE namedCondition #-}


operator :: ParserT st e Bool
operator = (True <$ $(string "!=")) <|> (False <$ $(char '='))
{-# INLINE operator #-}


featureTokenOrQuoted :: ParserT st e ByteString
featureTokenOrQuoted = branch (lookahead $(char '"')) httpQuotedBytes bareFeatureTag
{-# NOINLINE featureTokenOrQuoted #-}


normalizeTag :: ByteString -> ParserT st e FeatureTag
normalizeTag bytes
  | BS.any (>= 128) bytes = failed
  | otherwise = pure $! normalizeAsciiTag bytes
{-# INLINE normalizeTag #-}


normalizeAsciiTag :: ByteString -> FeatureTag
normalizeAsciiTag bytes
  | BSC.any isAsciiUpper bytes = FeatureTag (BSC.map toLower bytes)
  | otherwise = FeatureTag bytes
{-# INLINE normalizeAsciiTag #-}


featureValueParser :: ParserT st e FeatureValue
featureValueParser = FeatureValue . decodeFeatureOctets <$> (quoted <|> rfc9110TokenBS)
  where
    quoted = do
      bytes <- httpQuotedBytes
      if BS.any (>= 128) bytes then failed else pure bytes
{-# NOINLINE featureValueParser #-}


-- RFC 2295 decodes %HH before comparison. A percent outside a %HH
-- sequence remains a literal token or quoted-string character.
decodeFeatureOctets :: ByteString -> ByteString
decodeFeatureOctets bytes
  | not (BS.elem 37 bytes) = bytes
  | otherwise = maybe (M.toStrictByteString (copyEscapes bytes)) id (urlDecodeMaybe bytes)
  where
    copyEscapes rest = case BS.break (== 37) rest of
      (prefix, suffix)
        | BS.null suffix -> M.byteString prefix
        | BS.length suffix >= 3
        , Just high <- fromHex (BS.index suffix 1)
        , Just low <- fromHex (BS.index suffix 2) ->
            M.byteString prefix <> M.word8 (16 * high + low) <> copyEscapes (BS.drop 3 suffix)
        | otherwise -> M.byteString prefix <> M.word8 37 <> copyEscapes (BS.drop 1 suffix)
    fromHex byte
      | byte >= 48 && byte <= 57 = Just (byte - 48)
      | folded >= 97 && folded <= 102 = Just (folded - 87)
      | otherwise = Nothing
      where
        folded = byte Bits..|. 32


renderFeatureOctets :: ByteString -> M.Builder
renderFeatureOctets bytes
  | BS.all isTokenByte bytes && not (BS.null bytes) = M.byteString bytes
  | otherwise = quoteFeatureOctets bytes
  where
    isTokenByte byte = byte /= 37 && byte < 128 && isTokenChar (toEnum (fromIntegral byte))


quoteBytes :: ByteString -> M.Builder
quoteBytes = renderHttpQuotedString . TE.decodeLatin1


quoteFeatureOctets :: ByteString -> M.Builder
quoteFeatureOctets bytes = M.char7 '"' <> BS.foldr escape mempty bytes <> M.char7 '"'
  where
    escape byte rest
      | byte == 34 || byte == 92 = M.char7 '\\' <> M.word8 byte <> rest
      | byte == 37 || byte < 32 || byte >= 127 =
          M.char7 '%' <> M.word8 (hexDigit (byte `Bits.shiftR` 4)) <> M.word8 (hexDigit (byte Bits..&. 15)) <> rest
      | otherwise = M.word8 byte <> rest
    hexDigit :: Word8 -> Word8
    hexDigit digit
      | digit < 10 = 48 + digit
      | otherwise = 55 + digit


renderFeatureCondition :: (a -> M.Builder) -> FeatureTag -> FeatureCondition a -> M.Builder
renderFeatureCondition renderSpecial (FeatureTag tag) = \case
  FeaturePresent -> name
  FeatureAbsent -> "!" <> name
  FeatureEqual value -> name <> "=" <> renderValue value
  FeatureUnequal value -> name <> "!=" <> renderValue value
  FeatureSpecial value -> name <> "=" <> renderSpecial value
  where
    name
      | BS.null tag || tag == "*" || BS.head tag == 33 || BS.last tag == 33 = quoteBytes tag
      | BSC.all isTokenChar tag = M.byteString tag
      | otherwise = quoteBytes tag
    renderValue = renderFeatureOctets . featureValueBytes


renderEntry :: FeatureEntry -> M.Builder
renderEntry (FeatureEntry expression extensions) = renderExpression expression <> foldMap renderExtension extensions
  where
    renderExpression FeatureWildcard = "*"
    renderExpression (FeatureExpression name condition) =
      renderFeatureCondition (\value -> "{" <> renderFeatureOctets (featureValueBytes value) <> "}") name condition
    renderExtension (FeatureExtension name value) =
      ";" <> shortText name <> foldMap (("=" <>) . quoteBytes) value


commaList :: (a -> M.Builder) -> [a] -> M.Builder
commaList _ [] = mempty
commaList render (first : rest) = render first <> foldMap ((", " <>) . render) rest
