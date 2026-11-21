{-# LANGUAGE TemplateHaskell #-}

-- | Wire rules shared by the RFC 9421 signature fields.
module Network.HTTP.Headers.Signature.Internal (
  signatureDictionary,
  signatureInputDictionary,
  renderSignatureDictionary,
  renderSignatureInnerList,
  validSignatureInputEntry,
) where

import Control.Monad (unless, when)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.CharSet as CS
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import qualified Network.HTTP.Grammar.Rendering as R
import qualified Network.HTTP.Headers.Mason as M


signatureDictionary :: (a -> ShortText) -> (ShortText -> ParserT st String a) -> ParserT st String (NonEmpty a)
signatureDictionary label member = do
  structuredFieldSpaces
  entries <- rfc8941List1 (structuredFieldKey >>= member)
  ows
  case duplicateLabel label entries of
    Nothing -> pure entries
    Just key -> err ("Duplicate signature label " <> show key)
{-# INLINE signatureDictionary #-}


duplicateLabel :: (a -> ShortText) -> NonEmpty a -> Maybe ShortText
duplicateLabel label = \case
  _ :| [] -> Nothing
  first :| [second]
    | label first == label second -> Just (label first)
    | otherwise -> Nothing
  first :| rest -> checkLabels (Set.singleton (label first)) rest
  where
    checkLabels _ [] = Nothing
    checkLabels seen (entry : rest)
      | Set.member key seen = Just key
      | otherwise = checkLabels (Set.insert key seen) rest
      where
        key = label entry
{-# INLINE duplicateLabel #-}


{- | Check the dictionary labels and entries before rendering. Preserve
the member order used during signing.
-}
renderSignatureDictionary :: (a -> ShortText) -> (a -> Bool) -> (a -> M.Builder) -> NonEmpty a -> Maybe M.Builder
renderSignatureDictionary label valid render entries@(first :| rest)
  | all (\entry -> isStructuredFieldKey (label entry) && valid entry) entries
  , Nothing <- duplicateLabel label entries =
      Just (render first <> foldMap (\entry -> ", " <> render entry) rest)
  | otherwise = Nothing
{-# INLINE renderSignatureDictionary #-}


signatureInputDictionary
  :: (ItemValue -> Bool)
  -> (entry -> ShortText)
  -> (component -> RFC8941String)
  -> (component -> [(ShortText, Maybe ItemValue)])
  -> (RFC8941String -> [(ShortText, Maybe ItemValue)] -> component)
  -> (ShortText -> [component] -> [(ShortText, Maybe ItemValue)] -> entry)
  -> ParserT st String (NonEmpty entry)
signatureInputDictionary validTimestamp label name parameters component entry =
  signatureDictionary label $ \key -> do
    $(char '=')
    components <- structuredFieldInnerList parseComponent
    case duplicateComponent name (sameParameters `on` parameters) components of
      Just (RFC8941String identifier) -> err ("Duplicate signature component " <> show identifier)
      Nothing -> pure ()
    metadata <- structuredFieldParameters
    mapM_ checkMetadata metadata
    pure (entry key components (structuredFieldParameterPairs metadata))
  where
    parseComponent = do
      $(char '"')
      text <- shortASCIIFromParser_ $
        branch $(char '@')
          (skipSome (skipSatisfyAscii derivedComponentChar))
          (skipSome (skipSatisfyAscii (`CS.member` lowerTokenChars)))
      $(char '"')
      when (text == "@signature-params") (err "Reserved signature component identifier @signature-params")
      params <- structuredFieldParameters
      case params of
        [] | text /= "@query-param" -> pure (component (RFC8941String text) [])
        _ -> do
          checkComponentParameters text params
          pure (component (RFC8941String text) (structuredFieldParameterPairs params))

    sameParameters left right =
      length left == length right && all (`elem` right) left

    checkMetadata (StructuredFieldParameter key value) =
      unless (validSignatureMetadata validTimestamp key value) (err ("Invalid signature metadata parameter " <> show key))
{-# INLINE signatureInputDictionary #-}


duplicateComponent
  :: (component -> RFC8941String)
  -> (component -> component -> Bool)
  -> [component]
  -> Maybe RFC8941String
duplicateComponent name sameParameters components
  | exceeds 8 components = checkMany Map.empty components
  | otherwise = checkSmall components
  where
    checkSmall [] = Nothing
    checkSmall (current : rest)
      | any (\other -> name current == name other && sameParameters current other) rest = Just (name current)
      | otherwise = checkSmall rest

    checkMany _ [] = Nothing
    checkMany seen (current : rest)
      | Just earlier <- Map.lookup key seen
      , any (sameParameters current) earlier =
          Just (name current)
      | otherwise = case rest of
          [] -> Nothing
          _ -> checkMany (Map.insertWith (++) key [current] seen) rest
      where
        key = unsafeToRFC8941String (name current)
{-# INLINE duplicateComponent #-}


validSignatureMetadata :: (ItemValue -> Bool) -> ShortText -> ItemValue -> Bool
validSignatureMetadata validTimestamp "created" = validTimestamp
validSignatureMetadata validTimestamp "expires" = validTimestamp
validSignatureMetadata _ "keyid" = isStringItem
validSignatureMetadata _ "alg" = isStringItem
validSignatureMetadata _ "nonce" = isStringItem
validSignatureMetadata _ "tag" = isStringItem
validSignatureMetadata _ _ = const True
{-# INLINE validSignatureMetadata #-}


isStringItem :: ItemValue -> Bool
isStringItem (String _) = True
isStringItem _ = False
{-# INLINE isStringItem #-}


-- Validate rules that can be checked from the component identifier alone.
-- Extension parameters are kept for the application that understands them.
checkComponentParameters :: ShortText -> [StructuredFieldParameter] -> ParserT st String ()
checkComponentParameters name params = do
  when (name == "@query-param" && not (any isQueryName params)) $
    err "@query-param requires a String name parameter"
  mapM_ check params
  where
    derived = SBS.index (ST.toShortByteString name) 0 == 0x40
    isQueryName (StructuredFieldParameter key value) = key == "name" && isStringItem value
    hasStructuredSelector = any (selectsStructured . structuredParameterKey) params
    check (StructuredFieldParameter key value) =
      unless (validComponentParameter derived hasStructuredSelector key value) $
        err ("Invalid signature component parameter " <> show key)
{-# NOINLINE checkComponentParameters #-}


selectsStructured :: ShortText -> Bool
selectsStructured key = key == "sf" || key == "key"
{-# INLINE selectsStructured #-}


validComponentParameter :: Bool -> Bool -> ShortText -> ItemValue -> Bool
validComponentParameter derived hasStructuredSelector = valid
  where
    valid "req" (Boolean _) = True
    valid "req" _ = False
    valid "tr" value = fieldFlag value
    valid "sf" value = fieldFlag value
    valid "bs" value = fieldFlag value && not hasStructuredSelector
    valid "key" (String (RFC8941String key)) = not derived && isStructuredFieldKey key
    valid "key" _ = False
    valid _ _ = True
    fieldFlag (Boolean _) = not derived
    fieldFlag _ = False
{-# INLINE validComponentParameter #-}


validSignatureInputEntry
  :: (ItemValue -> Bool)
  -> (component -> RFC8941String)
  -> (component -> [(ShortText, Maybe ItemValue)])
  -> [component]
  -> [(ShortText, Maybe ItemValue)]
  -> Bool
validSignatureInputEntry validTimestamp name parameters components metadata =
  all validComponent components
    && isNothing (duplicateComponent name (sameAuthoredParameters `on` parameters) components)
    && all (validAuthoredParameter (validSignatureMetadata validTimestamp)) (orderedFieldEntries fst metadata)
  where
    validComponent component =
      validComponentName text
        && case parameters component of
          [] -> text /= "@query-param"
          params -> validAuthoredComponentParameters text (orderedFieldEntries fst params)
      where
        RFC8941String text = name component
{-# INLINE validSignatureInputEntry #-}


validAuthoredComponentParameters :: ShortText -> [(ShortText, Maybe ItemValue)] -> Bool
validAuthoredComponentParameters name params =
  (name /= "@query-param" || any isQueryName params)
    && all (validAuthoredParameter (validComponentParameter derived hasStructuredSelector)) params
  where
    derived = SBS.index (ST.toShortByteString name) 0 == 0x40
    hasStructuredSelector = any (selectsStructured . fst) params
    isQueryName (key, value) = key == "name" && maybe False isStringItem value
{-# NOINLINE validAuthoredComponentParameters #-}


validAuthoredParameter :: (ShortText -> ItemValue -> Bool) -> (ShortText, Maybe ItemValue) -> Bool
validAuthoredParameter valid (key, value) =
  isStructuredFieldKey key && isStructuredFieldBareItem item && valid key item
  where
    item = maybe (Boolean True) id value
{-# INLINE validAuthoredParameter #-}


sameAuthoredParameters :: [(ShortText, Maybe ItemValue)] -> [(ShortText, Maybe ItemValue)] -> Bool
sameAuthoredParameters left right =
  length left' == length right' && all (\parameter -> any (same parameter) right') left'
  where
    left' = orderedFieldEntries fst left
    right' = orderedFieldEntries fst right
    same (leftKey, leftValue) (rightKey, rightValue) =
      leftKey == rightKey && item leftValue == item rightValue
    item = maybe (Boolean True) id


validComponentName :: ShortText -> Bool
validComponentName text =
  not (SBS.null bytes)
    && if SBS.index bytes 0 == 0x40
      then SBS.length bytes > 1 && text /= "@signature-params" && scanCompactBytes (derivedComponentChar . BSI.w2c) bytes 1
      else case lowerTokenAscii of
        AsciiSet low high -> scanCompactBytes (asciiMember low high . BSI.w2c) bytes 0
  where
    bytes = ST.toShortByteString text
{-# INLINE validComponentName #-}


derivedComponentChar :: Char -> Bool
derivedComponentChar c = ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') || c == '-'
{-# INLINE derivedComponentChar #-}


-- A bounded scan avoids allocating a map for the usual short component lists.
exceeds :: Int -> [a] -> Bool
exceeds _ [] = False
exceeds 0 (_ : _) = True
exceeds limit (_ : rest) = exceeds (limit - 1) rest


lowerTokenChars :: CS.CharSet
lowerTokenChars = CS.difference tokenCharSet (CS.range 'A' 'Z')


lowerTokenAscii :: AsciiSet
lowerTokenAscii = asciiSet lowerTokenChars
{-# NOINLINE lowerTokenAscii #-}


renderSignatureInnerList
  :: (component -> RFC8941String)
  -> (component -> [(ShortText, Maybe ItemValue)])
  -> [component]
  -> [(ShortText, Maybe ItemValue)]
  -> M.Builder
renderSignatureInnerList name parameters components metadata =
  M.char7 '(' <> R.renderSeparated " " renderComponent components <> M.char7 ')' <> renderParameters metadata
  where
    renderComponent component = R.rfc8941String (name component) <> renderParameters (parameters component)
    renderParameters = foldMap renderParameter . orderedFieldEntries fst
    renderParameter (key, value) = R.renderStructuredFieldParameter key (maybe (Boolean True) id value)
{-# INLINE renderSignatureInnerList #-}
