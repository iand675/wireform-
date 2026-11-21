{-# LANGUAGE MagicHash #-}
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

import Data.Bits (xor, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int#, indexWord8OffAddrAsWord64#)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), unsafeWithForeignPtr)
import GHC.Word (Word64 (W64#), Word8)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCookie2)
import qualified Network.HTTP.Headers.Mason as M
import Numeric.Natural (Natural)


data Cookie2
  = RawCookie2 !ST.ShortText
  | ParsedCookie2 !ByteString !Natural


-- | The legacy wire spelling. Checked field writes validate the version.
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
cookie2Version (RawCookie2 text) = parseVersionLine (ST.toByteString text)
cookie2Version (ParsedCookie2 _ version) = Right version
{-# INLINE cookie2Version #-}


cookie2FromVersion :: Natural -> Cookie2
cookie2FromVersion 1 = versionOne
cookie2FromVersion version =
  ParsedCookie2 (M.toStrictByteString ("$Version=" <> M.integerDec (toInteger version))) version


versionOneWire, quotedVersionOneWire :: ByteString
versionOneWire = "$Version=1"
quotedVersionOneWire = "$Version=\"1\""


versionOne, quotedVersionOne :: Cookie2
versionOne = ParsedCookie2 versionOneWire 1
quotedVersionOne = ParsedCookie2 quotedVersionOneWire 1


instance KnownHeader Cookie2 where
  type ParseFailure Cookie2 = String
  type Cardinality Cookie2 = 'ZeroOrOne
  type Direction Cookie2 = 'Request


  parseFromHeaders _ (wire :| [])
    | matchesVersionOne wire = Right (if BS.length wire == 10 then versionOne else quotedVersionOne)
    | otherwise = ParsedCookie2 wire <$> parseVersionLine wire
  parseFromHeaders _ _ = Left "Multiple Cookie2 field lines"
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ = wireBytes


  renderToHeadersChecked _ (ParsedCookie2 wire _) = Just wire
  renderToHeadersChecked _ (RawCookie2 text) =
    case parseVersionLine wire of
      Left _ -> Nothing
      Right _ -> Just wire
    where
      wire = ST.toByteString text


  headerName _ = hCookie2


-- The exact lengths bound the two overlapping loads at any slice alignment.
matchesVersionOne :: ByteString -> Bool
matchesVersionOne (BSI.BS input 10) =
  matchVersionWords input 2# (targetWord 0x313d6e6f69737265 0x657273696f6e3d31)
matchesVersionOne (BSI.BS input 12) =
  matchVersionWords input 4# (targetWord 0x2231223d6e6f6973 0x73696f6e3d223122)
matchesVersionOne _ = False
{-# INLINE matchesVersionOne #-}


matchVersionWords :: ForeignPtr Word8 -> Int# -> Word64 -> Bool
matchVersionWords input@(ForeignPtr start _) suffixOffset suffix =
  BSI.accursedUnutterablePerformIO $
    unsafeWithForeignPtr input $ \_ ->
      pure $!
        ( (W64# (indexWord8OffAddrAsWord64# start 0#) `xor` targetWord 0x6e6f697372655624 0x2456657273696f6e)
            .|. (W64# (indexWord8OffAddrAsWord64# start suffixOffset) `xor` suffix)
        )
          == 0
{-# INLINE matchVersionWords #-}


targetWord :: Word64 -> Word64 -> Word64
targetWord little big = case targetByteOrder of
  LittleEndian -> little
  BigEndian -> big
{-# INLINE targetWord #-}


cookie2Parser :: ParserT st String Cookie2
cookie2Parser = withByteString (ows *> versionParser <* ows) $ \version wire ->
  pure (ParsedCookie2 wire version)


parseVersionLine :: ByteString -> Either String Natural
parseVersionLine = parseFieldLine "Cookie2" (ows *> versionParser <* ows)
{-# NOINLINE parseVersionLine #-}


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
