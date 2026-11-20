{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | @AMP-Cache-Transform@ negotiates the cache and transform version for
an AMP signed exchange. It uses the parameterised-list grammar from
draft-ietf-httpbis-header-structure-07. Received field lines are kept in order.

Spec: <https://github.com/ampproject/amphtml/blob/main/docs/spec/amp-cache-transform.md>
-}
module Network.HTTP.Headers.AMPCacheTransform (
  AMPCacheTransform (AMPCacheTransform, ampCacheTransformValue),
  AMPTransform (..),
  AMPParameter (..),
  AMPParameterValue (..),
  AMPVersionConstraint (..),
  AMPVersionRange (..),
  AMPTransformSelection (..),
  ampTransforms,
  ampCacheTransformFromEntries,
  ampVersionConstraint,
  ampVersionParameter,
  selectAMPTransform,
  ampResponseSelection,
  ampCacheTransformFromSelection,
  ampCacheTransformParser,
  renderAMPCacheTransform,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.CharSet as CS
import Data.Int (Int64)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text.Short as ST
import Data.Word (Word64)
import Network.HTTP.Grammar.Parser hiding (fieldName)
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAMPCacheTransform)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R
import Wireform.Base64 (decodeBase64, encodeBase64)


newtype AMPCacheTransform = TransformList (R.RetainedList AMPTransform)
  deriving stock (Eq)


-- | Legacy wire-text view. 'ampCacheTransformFromEntries' checks authored values.
pattern AMPCacheTransform :: ST.ShortText -> AMPCacheTransform
pattern AMPCacheTransform {ampCacheTransformValue} <- (fieldText -> ampCacheTransformValue)
  where
    AMPCacheTransform text = TransformList (R.RawList text)


{-# COMPLETE AMPCacheTransform #-}


instance Show AMPCacheTransform where
  showsPrec precedence (AMPCacheTransform text) =
    showParen (precedence > 10) $ showString "AMPCacheTransform {ampCacheTransformValue = " . shows text . showChar '}'


fieldText :: AMPCacheTransform -> ST.ShortText
fieldText (TransformList entries) = R.legacyText entries


data AMPTransform = AMPTransform
  { ampTransformIdentifier :: !ByteString
  , ampTransformParameters :: ![AMPParameter]
  }
  deriving stock (Eq, Show)


data AMPParameter = AMPParameter
  { ampParameterName :: !ByteString
  , ampParameterValue :: !(Maybe AMPParameterValue)
  }
  deriving stock (Eq, Show)


{- | The old Structured Headers item types. A float retains the exact
signed decimal coefficient and the number of fractional digits.
-}
data AMPParameterValue
  = AMPString !ByteString
  | AMPInteger {-# UNPACK #-} !Int64
  | AMPFloat {-# UNPACK #-} !Int64 {-# UNPACK #-} !Int
  | AMPBytes !ByteString
  deriving stock (Eq, Show)


data AMPVersionRange
  = AMPVersion {-# UNPACK #-} !Word64
  | AMPVersionRange {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving stock (Eq, Show)


data AMPVersionConstraint
  = AMPAnyVersion
  | AMPVersions ![AMPVersionRange]
  deriving stock (Eq, Show)


data AMPTransformSelection = AMPTransformSelection
  { ampSelectedCache :: !ByteString
  , ampSelectedVersion :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)


ampTransforms :: AMPCacheTransform -> Either String (NonEmpty AMPTransform)
ampTransforms (TransformList entries) = R.listEntries fieldName transformList entries


-- | Validate and serialize an ordered set of alternatives.
ampCacheTransformFromEntries :: NonEmpty AMPTransform -> Either String AMPCacheTransform
ampCacheTransformFromEntries =
  fmap TransformList . R.listFromEntries fieldName transformList renderTransform


{- | Project an alternative's negotiation constraint. A malformed version
string or an unsupported parameter makes only this alternative ineligible.
-}
ampVersionConstraint :: AMPTransform -> Either String AMPVersionConstraint
ampVersionConstraint (AMPTransform _ params) =
  case params of
    [] -> Right AMPAnyVersion
    [AMPParameter "v" (Just (AMPString versionText))] -> do
      ranges <- parseFieldLine "AMP version constraint" (httpList versionRange) versionText
      AMPVersions <$> checkVersionRanges ranges
    _ -> Left "Unsupported AMP transform parameters"


-- | Author a quoted @v@ parameter containing disjoint versions and ranges.
ampVersionParameter :: [AMPVersionRange] -> Either String AMPParameter
ampVersionParameter ranges = do
  _ <- checkVersionRanges ranges
  let wire = M.toStrictByteString (M.intersperse ", " (map renderVersionRange ranges))
  pure (AMPParameter "v" (Just (AMPString wire)))


{- | Select the highest offered version for the first satisfiable transform.
The caller supplies available versions for each identifier, including @any@.
An empty list represents an unsupported cache or transform.
-}
selectAMPTransform
  :: (ByteString -> [Word64])
  -> AMPCacheTransform
  -> Either String (Maybe AMPTransformSelection)
selectAMPTransform available = fmap (first . NE.toList) . ampTransforms
  where
    first [] = Nothing
    first (candidate : rest) =
      case ampVersionConstraint candidate of
        Right constraint
          | let name = ampTransformIdentifier candidate
          , Just version <- foldl' (pickVersion constraint) Nothing (available name) ->
              Just (AMPTransformSelection name version)
        _ -> first rest
    pickVersion constraint current version
      | version <= maximumVersion
      , maybe True (< version) current
      , acceptsVersion constraint version =
          Just version
      | otherwise = current


-- | A successful response contains a single identifier and exact version.
ampResponseSelection :: AMPCacheTransform -> Either String AMPTransformSelection
ampResponseSelection field = do
  entries <- ampTransforms field
  case entries of
    entry :| [] -> do
      constraint <- ampVersionConstraint entry
      case constraint of
        AMPVersions [AMPVersion version] -> Right (AMPTransformSelection (ampTransformIdentifier entry) version)
        _ -> Left "AMP transform response requires one exact version"
    _ -> Left "AMP transform response requires one alternative"


ampCacheTransformFromSelection :: AMPTransformSelection -> Either String AMPCacheTransform
ampCacheTransformFromSelection (AMPTransformSelection name version)
  | version > maximumVersion = Left "AMP transform version outside signed 64-bit range"
  | otherwise = do
      parseFieldLine "AMP transform identifier" identifierBytes name
      let prefix = ";v=\""
          wire = M.toStrictByteString (M.byteString name <> M.byteString prefix <> M.word64Dec version <> "\"")
          start = BS.length name + BS.length prefix
          versionText = BS.take (BS.length wire - start - 1) (BS.drop start wire)
          entry = AMPTransform name [AMPParameter "v" (Just (AMPString versionText))]
      pure (TransformList (R.DecodedList (wire :| []) (entry :| [])))


instance KnownHeader AMPCacheTransform where
  type ParseFailure AMPCacheTransform = String
  type Cardinality AMPCacheTransform = 'ZeroOrMore
  type Direction AMPCacheTransform = 'RequestAndResponse


  parseFromHeaders _ wire@(_ :| []) =
    TransformList <$> R.decodeList fieldName transformList wire
  parseFromHeaders _ wire =
    -- Preserve the draft codec's spelling for version strings split across lines.
    TransformList . R.DecodedList wire
      <$> (parseFieldLine fieldName transformList (combineFieldLines "," wire) >>= requireNonEmptyList fieldName)


  renderToHeaders _ (TransformList entries) = NE.toList (R.fieldLines entries)


  headerName _ = hAMPCacheTransform


ampCacheTransformParser :: ParserT st String AMPCacheTransform
ampCacheTransformParser = TransformList <$> R.listParser fieldName transformList


renderAMPCacheTransform :: AMPCacheTransform -> M.Builder
renderAMPCacheTransform (TransformList entries) = R.renderList entries


fieldName :: String
fieldName = "AMP-Cache-Transform"


transformList :: ParserT st String [AMPTransform]
transformList = ows *> entries
  where
    entries = do
      name <- identifier
      params <- parameters
      if distinctParameters params
        then do
          rest <- withOption ($(char ',') *> ows) (const entries) (pure [])
          pure (AMPTransform name params : rest)
        else err "Duplicate AMP transform parameter"


parameters :: ParserT st String [AMPParameter]
parameters = do
  ows
  withOption
    ($(char ';') *> ows)
    ( \() -> do
        name <- identifier
        value <- withOption $(char '=') (\() -> Just <$> (ows *> item)) (pure Nothing)
        rest <- parameters
        pure (AMPParameter name value : rest)
    )
    (pure [])
{-# NOINLINE parameters #-}


distinctParameters :: [AMPParameter] -> Bool
distinctParameters [] = True
distinctParameters [_] = True
distinctParameters params = go Set.empty params
  where
    go _ [] = True
    go seen (AMPParameter name _ : rest) =
      Set.notMember name seen && go (Set.insert name seen) rest


identifier :: ParserT st String ByteString
identifier = byteStringOf identifierBytes
{-# INLINE identifier #-}


identifierBytes :: ParserT st e ()
identifierBytes = skipSatisfyAscii lower *> optional_ (asciiSetBytes identifierChars)
  where
    lower c = c >= 'a' && c <= 'z'
{-# INLINE identifierBytes #-}


identifierChars :: AsciiSet
identifierChars = asciiSet (CS.fromList ['a' .. 'z'] <> CS.fromList ['0' .. '9'] <> "_-*/")
{-# NOINLINE identifierChars #-}


item :: ParserT st String AMPParameterValue
item =
  (AMPString <$> ($(char '"') *> withByteString scanStructuredString decodeOpaqueBody))
    <|> (AMPBytes <$> binaryItem)
    <|> number
{-# NOINLINE item #-}


binaryItem :: ParserT st String ByteString
binaryItem = do
  raw <- $(char '*') *> byteStringOf (optional_ (asciiAlnumSetBytes base64Chars)) <* $(char '*')
  maybe (err "Invalid AMP binary item") pure (decodeBase64 raw)


base64Chars :: AsciiSet
base64Chars =
  asciiSet (CS.fromList ['a' .. 'z'] <> CS.fromList ['A' .. 'Z'] <> CS.fromList ['0' .. '9'] <> "+/=")
{-# NOINLINE base64Chars #-}


number :: ParserT st String AMPParameterValue
number = do
  negative <- branch $(char '-') (pure True) (pure False)
  digits $ \whole count ->
    branch
      $(char '.')
      ( digits $ \fraction places ->
          if count + places <= 15
            then pure (AMPFloat (signed negative (whole * 10 ^ places + fraction)) (fromIntegral places))
            else err "AMP float exceeds 15 digits"
      )
      (AMPInteger <$> checkedInteger negative whole count)


digits :: (Word64 -> Word64 -> ParserT st String a) -> ParserT st String a
digits use =
  withSpan (boundedDecimalWord64 (err "AMP number exceeds 64 bits")) $ \value (Span (Pos start) (Pos end)) ->
    use value (end - start)
{-# INLINE digits #-}


checkedInteger :: Bool -> Word64 -> Word64 -> ParserT st String Int64
checkedInteger negative magnitude count
  | count <= 19
  , magnitude <= (if negative then maximumVersion + 1 else maximumVersion) =
      pure (signed negative magnitude)
  | otherwise = err "AMP integer outside signed 64-bit range"
{-# INLINE checkedInteger #-}


signed :: Bool -> Word64 -> Int64
signed negative magnitude = fromIntegral (if negative then negate magnitude else magnitude)
{-# INLINE signed #-}


maximumVersion :: Word64
maximumVersion = fromIntegral (maxBound :: Int64)


versionRange :: ParserT st String AMPVersionRange
versionRange =
  version >>= \first ->
    withOption (ows *> $(string "..") *> ows) (\() -> AMPVersionRange first <$> version) (pure (AMPVersion first))
  where
    version = do
      negative <- branch $(char '-') (pure True) (pure False)
      n <- digits (checkedInteger negative)
      if n < 0 then err "AMP version must be nonnegative" else pure (fromIntegral n)


checkVersionRanges :: [AMPVersionRange] -> Either String [AMPVersionRange]
checkVersionRanges ranges =
  case ranges of
    [] -> Right ranges
    [only]
      | valid only -> Right ranges
      | otherwise -> invalid
    [first, second]
      | valid first && valid second
      , rangeLast first < rangeFirst second || rangeLast second < rangeFirst first ->
          Right ranges
      | otherwise -> invalid
    _
      | nonOverlapping (sortOn rangeFirst ranges) -> Right ranges
      | otherwise -> invalid
  where
    invalid = Left "Invalid or overlapping AMP version range"
    valid range = rangeFirst range <= rangeLast range && rangeLast range <= maximumVersion
    nonOverlapping [] = True
    nonOverlapping (range : rest) = valid range && following (rangeLast range) rest
    following _ [] = True
    following previousLast (range : rest) =
      previousLast < rangeFirst range && valid range && following (rangeLast range) rest


rangeFirst, rangeLast :: AMPVersionRange -> Word64
rangeFirst (AMPVersion v) = v
rangeFirst (AMPVersionRange first _) = first
rangeLast (AMPVersion v) = v
rangeLast (AMPVersionRange _ lastVersion) = lastVersion


acceptsVersion :: AMPVersionConstraint -> Word64 -> Bool
acceptsVersion AMPAnyVersion _ = True
acceptsVersion (AMPVersions ranges) version =
  any (\range -> rangeFirst range <= version && version <= rangeLast range) ranges


renderVersionRange :: AMPVersionRange -> M.Builder
renderVersionRange (AMPVersion v) = M.word64Dec v
renderVersionRange (AMPVersionRange first lastVersion) = M.word64Dec first <> ".." <> M.word64Dec lastVersion


renderTransform :: AMPTransform -> M.Builder
renderTransform (AMPTransform name params) = M.byteString name <> foldMap renderParam params
  where
    renderParam (AMPParameter key value) =
      ";" <> M.byteString key <> foldMap (("=" <>) . renderItem) value


renderItem :: AMPParameterValue -> M.Builder
renderItem (AMPString bytes) = M.byteString (encodeHttpQuotedBytes (SBS.toShort bytes))
renderItem (AMPInteger n) = M.integerDec (toInteger n)
renderItem (AMPBytes bytes) = "*" <> M.byteString (encodeBase64 bytes) <> "*"
renderItem (AMPFloat coefficient places)
  | places >= 1 && places <= 14 =
      let (whole, fraction) = abs (toInteger coefficient) `quotRem` (10 ^ places)
      in (if coefficient < 0 then "-" else mempty)
           <> M.integerDec whole
           <> "."
           <> M.intDecPadded places (fromIntegral fraction)
  | otherwise = mempty
