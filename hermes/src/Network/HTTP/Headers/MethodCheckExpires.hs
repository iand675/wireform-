{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Method-Check-Expires@ is an HTTP-date from the original W3C cross-site
access draft.

Spec: <https://www.w3.org/TR/2007/WD-access-control-20071126/>
-}
module Network.HTTP.Headers.MethodCheckExpires (
  MethodCheckExpires (MethodCheckExpires, methodCheckExpiresValue),
  methodCheckExpiresTime,
  methodCheckExpiresFromTime,
  methodCheckExpiresParser,
  renderMethodCheckExpires,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hMethodCheckExpires)
import qualified Network.HTTP.Headers.Mason as M


data MethodCheckExpires
  = RawMethodCheckExpires !ST.ShortText
  | ParsedMethodCheckExpires !ByteString !UTCTime


{- | The legacy date spelling. Use 'methodCheckExpiresFromTime' to author an
IMF-fixdate from a timestamp.
-}
pattern MethodCheckExpires :: ST.ShortText -> MethodCheckExpires
pattern MethodCheckExpires {methodCheckExpiresValue} <- (legacyText -> methodCheckExpiresValue)
  where
    MethodCheckExpires raw = RawMethodCheckExpires raw


{-# COMPLETE MethodCheckExpires #-}


instance Eq MethodCheckExpires where
  left == right = wireBytes left == wireBytes right


instance Show MethodCheckExpires where
  showsPrec precedence (MethodCheckExpires value) =
    showParen (precedence > 10) $ showString "MethodCheckExpires {methodCheckExpiresValue = " . shows value . showChar '}'


legacyText :: MethodCheckExpires -> ST.ShortText
legacyText (RawMethodCheckExpires text) = text
legacyText (ParsedMethodCheckExpires wire _) = STU.fromByteStringUnsafe wire


wireBytes :: MethodCheckExpires -> ByteString
wireBytes (RawMethodCheckExpires text) = ST.toByteString text
wireBytes (ParsedMethodCheckExpires wire _) = wire


{- | The timestamp validated during typed lookup. Legacy text is interpreted
using the same date settings as 'methodCheckExpiresParser'.
-}
methodCheckExpiresTime :: MethodCheckExpires -> Either String UTCTime
methodCheckExpiresTime (RawMethodCheckExpires raw) =
  parseSingleFieldLine "Method-Check-Expires" dateParser (ST.toByteString raw :| [])
methodCheckExpiresTime (ParsedMethodCheckExpires _ time) = Right time


-- | Construct an IMF-fixdate, truncating to whole seconds.
methodCheckExpiresFromTime :: UTCTime -> MethodCheckExpires
methodCheckExpiresFromTime (UTCTime day seconds) =
  ParsedMethodCheckExpires (M.toStrictByteString (renderDate time)) time
  where
    time = UTCTime day (secondsToDiffTime (floor seconds))


instance KnownHeader MethodCheckExpires where
  type ParseFailure MethodCheckExpires = String
  type Cardinality MethodCheckExpires = 'ZeroOrOne
  type Direction MethodCheckExpires = 'Response


  parseFromHeaders settings fields@(wire :| []) =
    ParsedMethodCheckExpires wire <$> parseSingleFieldLine "Method-Check-Expires" (dateParserWith settings) fields
  parseFromHeaders _ _ = Left "Multiple Method-Check-Expires field lines"


  renderToHeaders _ = wireBytes


  headerName _ = hMethodCheckExpires


methodCheckExpiresParser :: ParserT st String MethodCheckExpires
methodCheckExpiresParser = withByteString (ows *> dateParser <* ows) $ \time wire ->
  pure (ParsedMethodCheckExpires wire time)


renderMethodCheckExpires :: MethodCheckExpires -> M.Builder
renderMethodCheckExpires (RawMethodCheckExpires raw) = shortText raw
renderMethodCheckExpires (ParsedMethodCheckExpires wire _) = M.byteString wire
