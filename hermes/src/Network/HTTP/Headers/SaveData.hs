{-# LANGUAGE PatternSynonyms #-}

{- |
@Save-Data@ is a client hint by which a client signals a preference to
reduce data usage. It is a Structured Fields List of Token Items. The
defined opt-in token is @on@. 'saveDataPreference' selects the last token
and the codec retains member parameters and extension tokens.

Spec: <https://wicg.github.io/savedata/> (WICG Save-Data API, a de-facto
specification).

See also: "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.DNT".
-}
module Network.HTTP.Headers.SaveData (
  SaveData (SaveData, SaveDataList, saveDataToken, saveDataItems),
  SaveDataItem (..),
  saveDataPreference,
  saveDataParser,
  renderSaveData,
  renderSaveDataChecked,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSaveData)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The ordered token Items of a @Save-Data@ field.
newtype SaveData = SaveDataList {saveDataItems :: [SaveDataItem]}
  deriving stock (Eq, Show)


data SaveDataItem = SaveDataItem
  { saveDataItemToken :: !RFC8941Token
  , saveDataItemParameters :: ![StructuredFieldParameter]
  }
  deriving stock (Eq, Show)


{- | The original interface for a single bare token. Use 'saveDataItems'
to inspect parameterized or multi-item values.
-}
pattern SaveData :: ST.ShortText -> SaveData
pattern SaveData {saveDataToken} = SaveDataList [SaveDataItem (RFC8941Token saveDataToken) []]


-- | Select the last preference token, if any.
saveDataPreference :: SaveData -> Maybe RFC8941Token
saveDataPreference = lastToken . saveDataItems
  where
    lastToken [] = Nothing
    lastToken [item] = Just (saveDataItemToken item)
    lastToken (_ : rest) = lastToken rest


instance KnownHeader SaveData where
  type ParseFailure SaveData = String
  type Cardinality SaveData = 'ZeroOrOne
  type Direction SaveData = 'Request


  parseFromHeaders _ ("on" :| []) = Right (SaveData "on")
  parseFromHeaders _ fields = parseStructuredFieldLines "Save-Data" saveDataParser fields


  renderToHeaders settings value = maybe "" id (renderToHeadersChecked settings value)
  {-# INLINE renderToHeaders #-}


  renderToHeadersChecked _ (SaveData "on") = Just "on"
  renderToHeadersChecked _ (SaveData token)
    | isStructuredFieldToken (RFC8941Token token) = Just (ST.toByteString token)
    | otherwise = Nothing
  renderToHeadersChecked _ value = either (const Nothing) (Just . M.toStrictByteString) (renderSaveDataChecked value)
  {-# INLINE renderToHeadersChecked #-}


  omitHeaderValue = null . saveDataItems
  {-# INLINE omitHeaderValue #-}


  headerName _ = hSaveData


saveDataParser :: ParserT st String SaveData
saveDataParser = SaveDataList <$> (ows *> rfc8941List item <* ows)
  where
    item = SaveDataItem <$> rfc8941Token <*> structuredFieldParameters


-- | Render the field value. An invalid list produces no bytes.
renderSaveData :: SaveData -> M.Builder
renderSaveData = either (const mempty) id . renderSaveDataChecked


-- | Validate tokens and the surviving parameters of every item.
renderSaveDataChecked :: SaveData -> Either String M.Builder
renderSaveDataChecked = R.renderStructuredFieldListChecked . map toMember . saveDataItems
  where
    toMember (SaveDataItem token parameters) = StructuredItem (StructuredFieldItem (Token token) parameters)
