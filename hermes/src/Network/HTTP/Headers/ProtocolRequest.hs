{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Protocol-Request@ asks the recipient to use an extension in a subsequent message.
PICS 1.1 clients use it to request labels from named rating services.

Spec: <https://www.w3.org/TR/REC-PICS-labels-961031> and <https://www.w3.org/TR/WD-http-pep-960820.html>.

See also: "Network.HTTP.Headers.Protocol", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PICSLabel".
-}
module Network.HTTP.Headers.ProtocolRequest (
  ProtocolRequest (ProtocolRequest, protocolRequestValue),
  protocolRequestEntries,
  protocolRequestFromEntries,
  protocolRequestParser,
  renderProtocolRequest,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.LegacyProtocol
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocolRequest)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype ProtocolRequest = ProtocolRequestList (R.RetainedList ProtocolRequestEntry)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'protocolRequestFromEntries' for checked authoring.
pattern ProtocolRequest :: ST.ShortText -> ProtocolRequest
pattern ProtocolRequest {protocolRequestValue} <- (fieldText -> protocolRequestValue)
  where
    ProtocolRequest text = ProtocolRequestList (R.RawList text)


{-# COMPLETE ProtocolRequest #-}


instance Show ProtocolRequest where
  showsPrec precedence (ProtocolRequest text) =
    showParen (precedence > 10) $ showString "ProtocolRequest {protocolRequestValue = " . shows text . showChar '}'


fieldText :: ProtocolRequest -> ST.ShortText
fieldText (ProtocolRequestList entries) = R.legacyText entries


protocolRequestEntries :: ProtocolRequest -> Either String (NE.NonEmpty ProtocolRequestEntry)
protocolRequestEntries (ProtocolRequestList entries) = R.listEntries "Protocol-Request" entriesParser entries


protocolRequestFromEntries :: NE.NonEmpty ProtocolRequestEntry -> Either String ProtocolRequest
protocolRequestFromEntries = fmap ProtocolRequestList . R.listFromEntries "Protocol-Request" entriesParser renderProtocolRequestEntry


instance KnownHeader ProtocolRequest where
  type ParseFailure ProtocolRequest = String
  type Cardinality ProtocolRequest = 'ZeroOrMore
  type Direction ProtocolRequest = 'RequestAndResponse


  parseFromHeaders _ fields = ProtocolRequestList <$> R.decodeList "Protocol-Request" entriesParser fields


  renderToHeaders _ (ProtocolRequestList entries) = NE.toList (R.fieldLines entries)


  headerName _ = hProtocolRequest


protocolRequestParser :: ParserT st String ProtocolRequest
protocolRequestParser = ProtocolRequestList <$> R.listParser "Protocol-Request" entriesParser


renderProtocolRequest :: ProtocolRequest -> M.Builder
renderProtocolRequest (ProtocolRequestList entries) = R.renderList entries


entriesParser :: ParserT st String [ProtocolRequestEntry]
entriesParser = legacyProtocolList (protocolRequestEntryParser)
