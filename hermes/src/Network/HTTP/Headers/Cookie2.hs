{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | RFC 2965 §3.3.5 @Cookie2@ advertises a cookie specification version.
RFC 6265 supersedes this field.
-}
module Network.HTTP.Headers.Cookie2 (
  Cookie2 (Cookie2, cookie2Value),
  cookie2Version,
  cookie2FromVersion,
  cookie2Parser,
  renderCookie2,

  -- * Version and attribute values shared with Set-Cookie2
  cookie2ValueBytes,
  cookie2Decimal,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCookie2)
import qualified Network.HTTP.Headers.Mason as M
import Numeric.Natural (Natural)


data Cookie2
  = RawCookie2 !ST.ShortText
  | ParsedCookie2 !ByteString !Natural


-- | The legacy wire spelling. Use 'cookie2FromVersion' for typed authoring.
pattern Cookie2 :: ST.ShortText -> Cookie2
pattern Cookie2 {cookie2Value} <- (legacyText -> cookie2Value)
  where
    Cookie2 value = RawCookie2 value


{-# COMPLETE Cookie2 #-}


instance Eq Cookie2 where
  left == right = wireBytes left == wireBytes right


instance Show Cookie2 where
  showsPrec precedence (Cookie2 value) =
    showParen (precedence > 10) $ showString "Cookie2 {cookie2Value = " . shows value . showChar '}'


legacyText :: Cookie2 -> ST.ShortText
legacyText (RawCookie2 text) = text
legacyText (ParsedCookie2 wire _) = STU.fromByteStringUnsafe wire


wireBytes :: Cookie2 -> ByteString
wireBytes (RawCookie2 text) = ST.toByteString text
wireBytes (ParsedCookie2 wire _) = wire


cookie2Version :: Cookie2 -> Either String Natural
cookie2Version (RawCookie2 text) = parseFieldLine "Cookie2" (ows *> versionParser <* ows) (ST.toByteString text)
cookie2Version (ParsedCookie2 _ version) = Right version


cookie2FromVersion :: Natural -> Cookie2
cookie2FromVersion version =
  ParsedCookie2 (M.toStrictByteString ("$Version=" <> M.integerDec (toInteger version))) version


instance KnownHeader Cookie2 where
  type ParseFailure Cookie2 = String
  type Cardinality Cookie2 = 'ZeroOrOne
  type Direction Cookie2 = 'Request


  parseFromHeaders _ fields@(wire :| []) =
    ParsedCookie2 wire <$> parseSingleFieldLine "Cookie2" versionParser fields
  parseFromHeaders _ _ = Left "Multiple Cookie2 field lines"


  renderToHeaders _ = wireBytes


  headerName _ = hCookie2


cookie2Parser :: ParserT st String Cookie2
cookie2Parser = withByteString (ows *> versionParser <* ows) $ \version wire ->
  pure (ParsedCookie2 wire version)


versionParser :: ParserT st String Natural
versionParser = do
  name <- anyWord64be
  -- Fold the ASCII letters in "$Version" together before comparing.
  if name .|. 0x0020202020202020 == 0x2476657273696f6e
    then ows *> $(char '=') *> ows *> cookie2Decimal
    else failed


cookie2ValueBytes :: ParserT st e ByteString
cookie2ValueBytes = branch (lookahead $(char '"')) httpQuotedBytes rfc9110TokenBS
{-# INLINE cookie2ValueBytes #-}


cookie2Decimal :: ParserT st String Natural
cookie2Decimal = do
  bytes <- cookie2ValueBytes
  case BSC.readNatural bytes of
    Just (number, rest) | BS.null rest && not (BS.isPrefixOf "+" bytes) -> pure number
    _ -> err "Expected a non-negative decimal cookie number"


renderCookie2 :: Cookie2 -> M.Builder
renderCookie2 (RawCookie2 value) = shortText value
renderCookie2 (ParsedCookie2 wire _) = M.byteString wire
