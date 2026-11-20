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


  renderToHeaders _ (SaveData "on") = "on"
  renderToHeaders _ (SaveData token) = ST.toByteString token
  renderToHeaders _ field = M.toStrictByteString (renderSaveData field)


  omitHeaderValue = null . saveDataItems
  {-# INLINE omitHeaderValue #-}


  headerName _ = hSaveData


saveDataParser :: ParserT st String SaveData
saveDataParser = SaveDataList <$> (ows *> rfc8941List item <* ows)
  where
    item = SaveDataItem <$> rfc8941Token <*> structuredFieldParameters


renderSaveData :: SaveData -> M.Builder
renderSaveData = M.intersperse ", " . map renderItem . saveDataItems
  where
    renderItem (SaveDataItem token parameters) = R.rfc8941Token token <> foldMap renderParameter parameters
    renderParameter (StructuredFieldParameter key value) = R.renderStructuredFieldParameter key value
