{-# LANGUAGE MagicHash #-}
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

import Data.Bits (xor, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (indexWord8OffAddrAsWord64#)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), unsafeWithForeignPtr)
import GHC.Word (Word64 (W64#))
import Network.HTTP.Grammar.LegacyProtocol
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocol)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype Protocol = ProtocolList (R.RetainedList ProtocolEntry)
  deriving stock (Eq)


-- | Legacy wire spelling. Checked field writes validate the directive list.
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
protocolFromEntries (ProtocolPicsLabels :| []) = Right protocolPicsLabels
protocolFromEntries entries = ProtocolList <$> R.listFromEntries "Protocol" entriesParser renderProtocolEntry entries
{-# INLINE protocolFromEntries #-}


instance KnownHeader Protocol where
  type ParseFailure Protocol = String
  type Cardinality Protocol = 'ZeroOrMore
  type Direction Protocol = 'RequestAndResponse


  parseFromHeaders _ (wire :| [])
    | isCanonicalPicsLabels wire = Right protocolPicsLabels
  parseFromHeaders _ fields = decodeProtocolFields fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ (ProtocolList entries) = NE.toList (R.fieldLines entries)


  renderToHeadersChecked _ (ProtocolList entries) = NE.toList <$> R.checkedFieldLines "Protocol" entriesParser entries
  {-# INLINE renderToHeadersChecked #-}


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


-- The length bounds all four loads, including the overlapping final word.
-- ByteString slices may begin at any alignment.
isCanonicalPicsLabels :: ByteString -> Bool
isCanonicalPicsLabels (BSI.BS input@(ForeignPtr start _) 31) =
  BSI.accursedUnutterablePerformIO $
    unsafeWithForeignPtr input $ \_ ->
      pure $!
        ( (W64# (indexWord8OffAddrAsWord64# start 0#) `xor` first)
            .|. (W64# (indexWord8OffAddrAsWord64# start 8#) `xor` second)
            .|. (W64# (indexWord8OffAddrAsWord64# start 16#) `xor` third)
            .|. (W64# (indexWord8OffAddrAsWord64# start 23#) `xor` lastWord)
        )
          == 0
  where
    first = targetWord 0x2e312d534349507b 0x7b504943532d312e
    second = targetWord 0x65646165687b2031 0x31207b6865616465
    third = targetWord 0x2d53434950207372 0x727320504943532d
    lastWord = targetWord 0x7d7d6c6562614c2d 0x2d4c6162656c7d7d
    targetWord little big = case targetByteOrder of
      LittleEndian -> little
      BigEndian -> big
isCanonicalPicsLabels _ = False
{-# INLINE isCanonicalPicsLabels #-}
