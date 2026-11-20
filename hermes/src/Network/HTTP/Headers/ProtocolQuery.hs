{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Protocol-Query@ queries extension availability in an HTTP request or response.

Spec: <https://www.w3.org/TR/WD-jepi-uppflow> and <https://www.w3.org/TR/WD-http-pep-960820.html>.

See also: "Network.HTTP.Headers.ProtocolInfo", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PICSLabel".
-}
module Network.HTTP.Headers.ProtocolQuery (
  ProtocolQuery (ProtocolQuery, protocolQueryValue),
  protocolQueryEntries,
  protocolQueryFromEntries,
  protocolQueryParser,
  renderProtocolQuery,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.LegacyProtocol
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocolQuery)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype ProtocolQuery = ProtocolQueryList (R.RetainedList LegacyDirective)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'protocolQueryFromEntries' for checked authoring.
pattern ProtocolQuery :: ST.ShortText -> ProtocolQuery
pattern ProtocolQuery {protocolQueryValue} <- (fieldText -> protocolQueryValue)
  where
    ProtocolQuery text = ProtocolQueryList (R.RawList text)


{-# COMPLETE ProtocolQuery #-}


instance Show ProtocolQuery where
  showsPrec precedence (ProtocolQuery text) =
    showParen (precedence > 10) $ showString "ProtocolQuery {protocolQueryValue = " . shows text . showChar '}'


fieldText :: ProtocolQuery -> ST.ShortText
fieldText (ProtocolQueryList entries) = R.legacyText entries


protocolQueryEntries :: ProtocolQuery -> Either String (NE.NonEmpty LegacyDirective)
protocolQueryEntries (ProtocolQueryList entries) = R.listEntries "Protocol-Query" entriesParser entries


protocolQueryFromEntries :: NE.NonEmpty LegacyDirective -> Either String ProtocolQuery
protocolQueryFromEntries = fmap ProtocolQueryList . R.listFromEntries "Protocol-Query" entriesParser renderLegacyDirective


instance KnownHeader ProtocolQuery where
  type ParseFailure ProtocolQuery = String
  type Cardinality ProtocolQuery = 'ZeroOrMore
  type Direction ProtocolQuery = 'RequestAndResponse


  parseFromHeaders _ fields = ProtocolQueryList <$> R.decodeList "Protocol-Query" entriesParser fields


  renderToHeaders _ (ProtocolQueryList entries) = NE.toList (R.fieldLines entries)


  headerName _ = hProtocolQuery


protocolQueryParser :: ParserT st String ProtocolQuery
protocolQueryParser = ProtocolQueryList <$> R.listParser "Protocol-Query" entriesParser


renderProtocolQuery :: ProtocolQuery -> M.Builder
renderProtocolQuery (ProtocolQueryList entries) = R.renderList entries


entriesParser :: ParserT st String [LegacyDirective]
entriesParser = legacyProtocolList (legacyDirectiveParser LegacyQuery)
