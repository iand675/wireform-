{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Protocol@ identifies the extensions applied to an HTTP message. PICS 1.1 uses it in
responses to declare the accompanying label field.

Spec: <https://www.w3.org/TR/REC-PICS-labels-961031> and <https://www.w3.org/TR/WD-http-pep-960820.html>.

See also: "Network.HTTP.Headers.ProtocolRequest", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PICSLabel".
-}
module Network.HTTP.Headers.Protocol (
  Protocol (Protocol, protocolValue),
  protocolEntries,
  protocolFromEntries,
  protocolParser,
  renderProtocol,
  protocolPicsLabels,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.LegacyProtocol
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocol)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype Protocol = ProtocolList (R.RetainedList ProtocolEntry)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'protocolFromEntries' for checked authoring.
pattern Protocol :: ST.ShortText -> Protocol
pattern Protocol {protocolValue} <- (fieldText -> protocolValue)
  where
    Protocol text = ProtocolList (R.RawList text)


{-# COMPLETE Protocol #-}


instance Show Protocol where
  showsPrec precedence (Protocol text) =
    showParen (precedence > 10) $ showString "Protocol {protocolValue = " . shows text . showChar '}'


fieldText :: Protocol -> ST.ShortText
fieldText (ProtocolList entries) = R.legacyText entries


protocolEntries :: Protocol -> Either String (NE.NonEmpty ProtocolEntry)
protocolEntries (ProtocolList entries) = R.listEntries "Protocol" entriesParser entries
{-# INLINE protocolEntries #-}


protocolFromEntries :: NE.NonEmpty ProtocolEntry -> Either String Protocol
protocolFromEntries = fmap ProtocolList . R.listFromEntries "Protocol" entriesParser renderProtocolEntry


instance KnownHeader Protocol where
  type ParseFailure Protocol = String
  type Cardinality Protocol = 'ZeroOrMore
  type Direction Protocol = 'RequestAndResponse


  parseFromHeaders _ (wire :| [])
    | wire == picsWire = Right protocolPicsLabels
  parseFromHeaders _ fields = decodeProtocolFields fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ (ProtocolList entries) = NE.toList (R.fieldLines entries)


  headerName _ = hProtocol


decodeProtocolFields :: NonEmpty ByteString -> Either String Protocol
decodeProtocolFields fields = ProtocolList <$> R.decodeList "Protocol" entriesParser fields
{-# NOINLINE decodeProtocolFields #-}


protocolParser :: ParserT st String Protocol
protocolParser = ProtocolList <$> R.listParser "Protocol" entriesParser


renderProtocol :: Protocol -> M.Builder
renderProtocol (ProtocolList entries) = R.renderList entries


entriesParser :: ParserT st String [ProtocolEntry]
entriesParser = legacyProtocolList (protocolEntryParser)


-- | The canonical PICS 1.1 response declaration.
protocolPicsLabels :: Protocol
protocolPicsLabels = ProtocolList (R.DecodedList (picsWire :| []) (ProtocolPicsLabels :| []))


picsWire :: ByteString
picsWire = "{PICS-1.1 {headers PICS-Label}}"
