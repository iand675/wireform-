{-# LANGUAGE TemplateHaskell #-}

-- | Wire rules shared by the RFC 9421 signature fields.
module Network.HTTP.Headers.Signature.Internal (
  signatureDictionary,
  signatureInputDictionary,
  renderSignatureInnerList,
) where

import Control.Monad (unless, when)
import qualified Data.ByteString.Short as SBS
import qualified Data.CharSet as CS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
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
  case entries of
    _ :| [] -> pure entries
    first :| [second] -> do
      when (label first == label second) (duplicate (label first))
      pure entries
    first :| rest -> checkLabels (Set.singleton (label first)) rest *> pure entries
  where
    checkLabels _ [] = pure ()
    checkLabels seen (entry : rest)
      | Set.member key seen = duplicate key
      | otherwise = checkLabels (Set.insert key seen) rest
      where
        key = label entry
    duplicate key = err ("Duplicate signature label " <> show key)
{-# INLINE signatureDictionary #-}


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
    if exceeds 8 components then checkComponents Map.empty components else checkSmall components
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

    checkSmall [] = pure ()
    checkSmall (covered : rest)
      | any (sameComponent covered) rest = duplicateComponent (name covered)
      | otherwise = checkSmall rest
    sameComponent a b = name a == name b && sameParameters (parameters a) (parameters b)

    checkComponents _ [] = pure ()
    checkComponents seen (covered : rest)
      | Just earlier <- Map.lookup key seen
      , any (sameParameters params) earlier =
          duplicateComponent (name covered)
      | otherwise = case rest of
          [] -> pure ()
          _ -> checkComponents (Map.insertWith (++) key [params] seen) rest
      where
        key = unsafeToRFC8941String (name covered)
        params = parameters covered
    sameParameters left right =
      length left == length right && all (`elem` right) left
    duplicateComponent (RFC8941String key) = err ("Duplicate signature component " <> show key)

    checkMetadata (StructuredFieldParameter key value) =
      unless (validMetadata key value) (err ("Invalid signature metadata parameter " <> show key))

    validMetadata "created" = validTimestamp
    validMetadata "expires" = validTimestamp
    validMetadata "keyid" = isString
    validMetadata "alg" = isString
    validMetadata "nonce" = isString
    validMetadata "tag" = isString
    validMetadata _ = const True

    isString (String _) = True
    isString _ = False
{-# INLINE signatureInputDictionary #-}


-- Validate rules that can be checked from the component identifier alone.
-- Extension parameters are kept for the application that understands them.
checkComponentParameters :: ShortText -> [StructuredFieldParameter] -> ParserT st String ()
checkComponentParameters name params = do
  when (name == "@query-param" && not (any isQueryName params)) $
    err "@query-param requires a String name parameter"
  mapM_ check params
  where
    derived = SBS.index (ST.toShortByteString name) 0 == 0x40
    isQueryName (StructuredFieldParameter "name" (String _)) = True
    isQueryName _ = False
    selectsStructured (StructuredFieldParameter key _) = key == "sf" || key == "key"
    check (StructuredFieldParameter key value) =
      unless (valid key value) (err ("Invalid signature component parameter " <> show key))
    valid "req" (Boolean _) = True
    valid "req" _ = False
    valid "tr" value = fieldFlag value
    valid "sf" value = fieldFlag value
    valid "bs" value = fieldFlag value && not (any selectsStructured params)
    valid "key" (String (RFC8941String key)) =
      not derived && case runParser (scanStructuredFieldKey <* eof) (ST.toByteString key) of
        OK () _ -> True
        _ -> False
    valid "key" _ = False
    valid _ _ = True
    fieldFlag (Boolean _) = not derived
    fieldFlag _ = False
{-# NOINLINE checkComponentParameters #-}


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
