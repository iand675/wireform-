{- |
@Save-Data@ is a de-facto request header (Network Information API) by which a
client signals a preference to reduce data usage, letting the origin serve
lighter responses. The only defined value is the token @on@; the header is
simply omitted when no such preference is being expressed.

This header is __not__ IANA-registered; it is modeled as the carried RFC 9110
token, preserved verbatim.

Spec: <https://wicg.github.io/savedata/> (WICG Save-Data API, a de-facto
specification).

See also: "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.DNT".
-}
module Network.HTTP.Headers.SaveData (
  SaveData (..),
  saveDataParser,
  renderSaveData,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSaveData)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A @Save-Data@ value: the carried token, typically @on@.
newtype SaveData = SaveData {saveDataToken :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SaveData where
  type ParseFailure SaveData = String
  type Cardinality SaveData = 'ZeroOrOne
  type Direction SaveData = 'Request


  parseFromHeaders _ headers = case runParser saveDataParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Save-Data header: " <> show rest
    Fail -> Left "Failed to parse Save-Data header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSaveData


  headerName _ = hSaveData


saveDataParser :: ParserT st String SaveData
saveDataParser = SaveData <$> (ows *> rfc9110Token <* ows)


renderSaveData :: SaveData -> M.Builder
renderSaveData (SaveData t) = shortText t
