{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @PICS-Label@ carries a list of content ratings and rating-service
results in an HTTP response. Accepted labels retain their original wire bytes.

Spec: <https://www.w3.org/TR/REC-PICS-labels-961031>

See also: "Network.HTTP.Headers.Protocol", "Network.HTTP.Headers.ProtocolRequest", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.PICSLabel (
  PICSLabel (PICSLabel, picsLabelValue),
  picsLabelList,
  picsLabelFromList,
  picsLabelParser,
  renderPICSLabel,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.PICS
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPICSLabel)
import qualified Network.HTTP.Headers.Mason as M


data PICSLabel
  = RawPICSLabel !ST.ShortText
  | ParsedPICSLabel !ByteString !PicsLabelList


-- | Legacy wire spelling. Use 'picsLabelFromList' for checked authoring.
pattern PICSLabel :: ST.ShortText -> PICSLabel
pattern PICSLabel {picsLabelValue} <- (legacyText -> picsLabelValue)
  where
    PICSLabel text = RawPICSLabel text


{-# COMPLETE PICSLabel #-}


instance Eq PICSLabel where
  left == right = wireBytes left == wireBytes right


instance Show PICSLabel where
  showsPrec precedence (PICSLabel text) =
    showParen (precedence > 10) $ showString "PICSLabel {picsLabelValue = " . shows text . showChar '}'


legacyText :: PICSLabel -> ST.ShortText
legacyText (RawPICSLabel text) = text
legacyText (ParsedPICSLabel wire _) = STU.fromByteStringUnsafe wire


wireBytes :: PICSLabel -> ByteString
wireBytes (RawPICSLabel text) = ST.toByteString text
wireBytes (ParsedPICSLabel wire _) = wire


picsLabelList :: PICSLabel -> Either String PicsLabelList
picsLabelList (RawPICSLabel text) = parseFieldLine "PICS-Label" picsLabelListParser (ST.toByteString text)
picsLabelList (ParsedPICSLabel _ entries) = Right entries


picsLabelFromList :: PicsLabelList -> Either String PICSLabel
picsLabelFromList entries = do
  let wire = M.toStrictByteString (renderPicsLabelList entries)
  parsed <- parseFieldLine "PICS-Label" picsLabelListParser wire
  if parsed == entries
    then Right (ParsedPICSLabel wire parsed)
    else Left "PICS labels do not match their wire representation"


instance KnownHeader PICSLabel where
  type ParseFailure PICSLabel = String
  type Cardinality PICSLabel = 'ZeroOrOne
  type Direction PICSLabel = 'Response


  parseFromHeaders _ (wire :| []) =
    ParsedPICSLabel wire <$> parseFieldLine "PICS-Label" picsLabelListParser wire
  parseFromHeaders _ _ = Left "Multiple PICS-Label field lines"


  renderToHeaders _ = wireBytes


  headerName _ = hPICSLabel


picsLabelParser :: ParserT st String PICSLabel
picsLabelParser = withByteString picsLabelListParser $ \entries wire ->
  pure (ParsedPICSLabel wire entries)


renderPICSLabel :: PICSLabel -> M.Builder
renderPICSLabel (RawPICSLabel value) = shortText value
renderPICSLabel (ParsedPICSLabel wire _) = M.byteString wire
