{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Protocol-Info@ advertises extension availability and parameters in requests or
responses, whether unsolicited or in reply to a query.

Spec: <https://www.w3.org/TR/WD-jepi-uppflow> and <https://www.w3.org/TR/WD-http-pep-960820.html>.

See also: "Network.HTTP.Headers.ProtocolQuery", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PICSLabel".
-}
module Network.HTTP.Headers.ProtocolInfo (
  ProtocolInfo (ProtocolInfo, protocolInfoValue),
  protocolInfoEntries,
  protocolInfoFromEntries,
  protocolInfoParser,
  renderProtocolInfo,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.LegacyProtocol
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocolInfo)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype ProtocolInfo = ProtocolInfoList (R.RetainedList LegacyDirective)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'protocolInfoFromEntries' for checked authoring.
pattern ProtocolInfo :: ST.ShortText -> ProtocolInfo
pattern ProtocolInfo {protocolInfoValue} <- (fieldText -> protocolInfoValue)
  where
    ProtocolInfo text = ProtocolInfoList (R.RawList text)


{-# COMPLETE ProtocolInfo #-}


instance Show ProtocolInfo where
  showsPrec precedence (ProtocolInfo text) =
    showParen (precedence > 10) $ showString "ProtocolInfo {protocolInfoValue = " . shows text . showChar '}'


fieldText :: ProtocolInfo -> ST.ShortText
fieldText (ProtocolInfoList entries) = R.legacyText entries


protocolInfoEntries :: ProtocolInfo -> Either String (NE.NonEmpty LegacyDirective)
protocolInfoEntries (ProtocolInfoList entries) = R.listEntries "Protocol-Info" entriesParser entries


protocolInfoFromEntries :: NE.NonEmpty LegacyDirective -> Either String ProtocolInfo
protocolInfoFromEntries = fmap ProtocolInfoList . R.listFromEntries "Protocol-Info" entriesParser renderLegacyDirective


instance KnownHeader ProtocolInfo where
  type ParseFailure ProtocolInfo = String
  type Cardinality ProtocolInfo = 'ZeroOrMore
  type Direction ProtocolInfo = 'RequestAndResponse


  parseFromHeaders _ fields = ProtocolInfoList <$> R.decodeList "Protocol-Info" entriesParser fields


  renderToHeaders _ (ProtocolInfoList entries) = NE.toList (R.fieldLines entries)


  headerName _ = hProtocolInfo


protocolInfoParser :: ParserT st String ProtocolInfo
protocolInfoParser = ProtocolInfoList <$> R.listParser "Protocol-Info" entriesParser


renderProtocolInfo :: ProtocolInfo -> M.Builder
renderProtocolInfo (ProtocolInfoList entries) = R.renderList entries


entriesParser :: ParserT st String [LegacyDirective]
entriesParser = legacyProtocolList (legacyDirectiveParser LegacyInfo)
