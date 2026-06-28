{- |
IP address literals as used throughout IETF protocols.

This module provides faithful parsers and canonical renderers for the
three textual address forms an @authority@/@host@ component may carry:

* __IPv4__ — dotted-quad, four decimal octets (RFC 791,
  <https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2 RFC 3986 §3.2.2>).
* __IPv6__ — the full RFC 4291 grammar including @::@ zero-compression and an
  optional IPv4-mapped tail such as @::ffff:192.0.2.1@
  (<https://www.rfc-editor.org/rfc/rfc4291#section-2.2 RFC 4291 §2.2>).
* __IPvFuture__ — the version-tagged escape hatch @vX.payload@
  (<https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2 RFC 3986 §3.2.2>),
  preserved verbatim.

'renderIPv6' emits the canonical textual representation mandated by
<https://www.rfc-editor.org/rfc/rfc5952 RFC 5952>: lower-case hex, no leading
zeros, and the longest run of all-zero hextets (leftmost on ties, length ≥ 2)
collapsed to @::@.  Consequently @renderIPv6 . ipv6Parser@ round-trips on any
RFC 5952 canonical input.
-}
module Network.IPAddress (
  IPv4 (..),
  IPv6 (..),
  IPAddress (..),
  ipv4Parser,
  ipv6Parser,
  ipAddressParser,
  renderIPv4,
  renderIPv6,
  renderIPAddress,
  parseIPv4,
  parseIPv6,
  parseIPAddress,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (foldl')
import qualified Data.Text.Short as ST
import Data.Word (Word16, Word8)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Numeric (showHex)
import Data.Bifunctor (first)
import Network.HTTP.Grammar (WireGrammar (..), grammarParseErrorToString, parseGrammar)


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | An IPv4 address as four octets, most-significant first.
data IPv4 = IPv4 {-# UNPACK #-} !Word8 !Word8 !Word8 !Word8
  deriving stock (Eq, Ord, Show)


-- | An IPv6 address as eight 16-bit hextets, most-significant first.
data IPv6 = IPv6 {-# UNPACK #-} !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
  deriving stock (Eq, Ord, Show)


{- | Any of the textual IP-literal forms permitted by RFC 3986.

The 'IPvFuture' payload retains the raw @vX.payload@ text (including the
leading @v@) so it can be re-emitted exactly as received.
-}
data IPAddress
  = IPv4Address !IPv4
  | IPv6Address !IPv6
  | IPvFuture !ST.ShortText
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- | Parse a dotted-quad IPv4 address: four decimal octets @0-255@ joined by @.@.
ipv4Parser :: ParserT st String IPv4
ipv4Parser = do
  a <- octet
  dot
  b <- octet
  dot
  c <- octet
  dot
  IPv4 a b c <$> octet
  where
    dot = satisfyAscii_ (== '.')


{- | Parse an IPv6 address per <https://www.rfc-editor.org/rfc/rfc4291#section-2.2 RFC 4291 §2.2>:
hextet groups joined by @:@, at most one @::@ zero-compression marker, and an
optional embedded IPv4 address occupying the final 32 bits.
-}
ipv6Parser :: ParserT st String IPv6
ipv6Parser = do
  bs <- byteStringOf (skipSome (satisfyAscii isIPv6Char))
  maybe failed pure (assembleIPv6 bs)


{- | Parse any IP-literal form, trying IPv6 first, then IPv4, then IPvFuture.

IPv6 is attempted first because its character set (hex digits, @:@ and @.@) is a
superset of IPv4's; the v6 attempt backtracks cleanly when the bytes are not a
valid v6 address.
-}
ipAddressParser :: ParserT st String IPAddress
ipAddressParser =
  (IPv6Address <$> ipv6Parser)
    <|> (IPv4Address <$> ipv4Parser)
    <|> (IPvFuture <$> ipvFutureParser)


-- | @IPvFuture = "v" 1*HEXDIG "." 1*( unreserved \/ sub-delims \/ ":" )@.
ipvFutureParser :: ParserT st e ST.ShortText
ipvFutureParser = shortASCIIFromParser_ $ do
  satisfyAscii_ (\c -> c == 'v' || c == 'V')
  skipSome (satisfyAscii isHexDigitChar)
  satisfyAscii_ (== '.')
  skipSome (satisfyAscii isIPvFutureChar)


-- | One decimal octet: 1–3 digits with value @0-255@.
octet :: ParserT st String Word8
octet = do
  d0 <- asciiDigit
  ds <- upTo 2 asciiDigit
  let v = foldl' (\acc x -> acc * 10 + x) d0 ds
  if v > 255 then failed else pure (fromIntegral v)


asciiDigit :: ParserT st e Int
asciiDigit = (\c -> fromEnum c - 0x30) <$> satisfyAscii isDigit


-- | Run @p@ at most @n@ times, collecting results (greedily).
upTo :: Int -> ParserT st e a -> ParserT st e [a]
upTo n p
  | n <= 0 = pure []
  | otherwise = withOption p (\x -> (x :) <$> upTo (n - 1) p) (pure [])


-- ---------------------------------------------------------------------------
-- IPv6 assembly (pure)
-- ---------------------------------------------------------------------------

{- | Turn the captured run of IPv6 characters into eight hextets, expanding a
single @::@ and any embedded trailing IPv4 literal.  Returns 'Nothing' when the
bytes do not form a valid address.
-}
assembleIPv6 :: ByteString -> Maybe IPv6
assembleIPv6 bs =
  let (before, afterRaw) = BS.breakSubstring doubleColon bs
  in if BS.null afterRaw
      then do
        hs <- parseGroups True (splitGroups bs)
        if length hs == 8 then toIPv6 hs else Nothing
      else
        let after = BS.drop 2 afterRaw
        in if doubleColon `BS.isInfixOf` after
            then Nothing -- more than one "::"
            else do
              left <- parseGroups False (splitGroups before)
              right <- parseGroups True (splitGroups after)
              let n = length left + length right
              if n <= 7 -- "::" must stand in for at least one zero hextet
                then toIPv6 (left ++ replicate (8 - n) 0 ++ right)
                else Nothing


-- | Split a colon-separated run into its groups; an empty run yields no groups.
splitGroups :: ByteString -> [ByteString]
splitGroups bs
  | BS.null bs = []
  | otherwise = BS.split 0x3A bs


{- | Parse a non-@::@ run of groups.  Every group is a hextet, except the final
group which — when @allowV4@ is set — may instead be an embedded IPv4 literal
contributing two hextets.
-}
parseGroups :: Bool -> [ByteString] -> Maybe [Word16]
parseGroups _ [] = Just []
parseGroups allowV4 groups = go groups
  where
    go [] = Just []
    go [g] = lastGroup g
    go (g : rest) = (:) <$> hextet g <*> go rest
    lastGroup g
      | allowV4
      , Just (IPv4 a b c d) <- parseV4Bytes g =
          Just [combineV4 a b, combineV4 c d]
      | otherwise = (: []) <$> hextet g


-- | Parse a complete IPv4 literal, requiring it to consume all of @g@.
parseV4Bytes :: ByteString -> Maybe IPv4
parseV4Bytes g = case runParser ipv4Parser g of
  OK ip rest | BS.null rest -> Just ip
  _ -> Nothing


-- | A single @h16@ hextet: 1–4 hex digits.
hextet :: ByteString -> Maybe Word16
hextet g
  | n >= 1 && n <= 4 && BS.all isHexByte g =
      Just (BS.foldl' (\acc w -> acc * 16 + hexVal w) 0 g)
  | otherwise = Nothing
  where
    n = BS.length g


toIPv6 :: [Word16] -> Maybe IPv6
toIPv6 [a, b, c, d, e, f, g, h] = Just (IPv6 a b c d e f g h)
toIPv6 _ = Nothing


-- | Pack two IPv4 octets into one hextet.
combineV4 :: Word8 -> Word8 -> Word16
combineV4 hi lo = fromIntegral hi * 256 + fromIntegral lo


doubleColon :: ByteString
doubleColon = BS.pack [0x3A, 0x3A]


-- ---------------------------------------------------------------------------
-- Renderers
-- ---------------------------------------------------------------------------

-- | Render an IPv4 address as dotted-quad decimal.
renderIPv4 :: IPv4 -> M.Builder
renderIPv4 (IPv4 a b c d) =
  d8 a <> dot <> d8 b <> dot <> d8 c <> dot <> d8 d
  where
    d8 w = M.wordDec (fromIntegral w)
    dot = M.char7 '.'


{- | Render an IPv6 address in the canonical form of
<https://www.rfc-editor.org/rfc/rfc5952 RFC 5952>: lower-case hex hextets with
no leading zeros, and the longest run of zero hextets (leftmost on ties, only
when its length is ≥ 2) collapsed to @::@.
-}
renderIPv6 :: IPv6 -> M.Builder
renderIPv6 (IPv6 a b c d e f g h) =
  let hs = [a, b, c, d, e, f, g, h]
      (start, len) = bestZeroRun hs
  in if len >= 2
      then
        joinColon (map hx (take start hs))
          <> M.string8 "::"
          <> joinColon (map hx (drop (start + len) hs))
      else joinColon (map hx hs)
  where
    hx w = M.string8 (showHex w "")
    joinColon = M.intersperse (M.char7 ':')


-- | Render any IP-literal form. 'IPvFuture' is emitted verbatim.
renderIPAddress :: IPAddress -> M.Builder
renderIPAddress (IPv4Address v4) = renderIPv4 v4
renderIPAddress (IPv6Address v6) = renderIPv6 v6
renderIPAddress (IPvFuture t) = M.shortByteString (ST.toShortByteString t)


-- | The all-zero hextet runs of an address, as @(startIndex, length)@ pairs.
zeroRuns :: [Word16] -> [(Int, Int)]
zeroRuns = goRuns 0
  where
    goRuns _ [] = []
    goRuns i ys@(x : xs)
      | x == 0 =
          let (zs, rest) = span (== 0) ys
              l = length zs
          in (i, l) : goRuns (i + l) rest
      | otherwise = goRuns (i + 1) xs


{- | The leftmost longest run of zero hextets, as @(startIndex, length)@.
@length@ is @0@ when there are no zero hextets at all.
-}
bestZeroRun :: [Word16] -> (Int, Int)
bestZeroRun = foldl' better (0, 0) . zeroRuns
  where
    better acc@(_, bl) cur@(_, l) = if l > bl then cur else acc


-- ---------------------------------------------------------------------------
-- WireGrammar instances
-- ---------------------------------------------------------------------------

instance WireGrammar IPv4 where
  type GrammarErr IPv4 = String
  grammarParser = ipv4Parser
  grammarRender = renderIPv4


instance WireGrammar IPv6 where
  type GrammarErr IPv6 = String
  grammarParser = ipv6Parser
  grammarRender = renderIPv6


instance WireGrammar IPAddress where
  type GrammarErr IPAddress = String
  grammarParser = ipAddressParser
  grammarRender = renderIPAddress


-- ---------------------------------------------------------------------------
-- Convenience parsers (full consumption)
-- ---------------------------------------------------------------------------

-- | Parse a complete IPv4 address from a 'ByteString'.
parseIPv4 :: ByteString -> Either String IPv4
parseIPv4 = first (grammarParseErrorToString "IPv4 address") . parseGrammar


-- | Parse a complete IPv6 address from a 'ByteString'.
parseIPv6 :: ByteString -> Either String IPv6
parseIPv6 = first (grammarParseErrorToString "IPv6 address") . parseGrammar


-- | Parse a complete IP-literal of any form from a 'ByteString'.
parseIPAddress :: ByteString -> Either String IPAddress
parseIPAddress = first (grammarParseErrorToString "IP address") . parseGrammar


-- ---------------------------------------------------------------------------
-- Character classes
-- ---------------------------------------------------------------------------

isIPv6Char :: Char -> Bool
isIPv6Char c = isHexDigitChar c || c == ':' || c == '.'


isHexDigitChar :: Char -> Bool
isHexDigitChar c =
  isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')


-- | @unreserved \/ sub-delims \/ ":"@ — the IPvFuture payload character set.
isIPvFutureChar :: Char -> Bool
isIPvFutureChar c = isUnreservedChar c || isSubDelimChar c || c == ':'


isUnreservedChar :: Char -> Bool
isUnreservedChar c =
  isAsciiUpper c
    || isAsciiLower c
    || isDigit c
    || c == '-'
    || c == '.'
    || c == '_'
    || c == '~'


isSubDelimChar :: Char -> Bool
isSubDelimChar c = c `elem` ("!$&'()*+,;=" :: String)


isHexByte :: Word8 -> Bool
isHexByte w =
  (w >= 0x30 && w <= 0x39)
    || (w >= 0x61 && w <= 0x66)
    || (w >= 0x41 && w <= 0x46)


hexVal :: Word8 -> Word16
hexVal w
  | w >= 0x30 && w <= 0x39 = fromIntegral (w - 0x30)
  | w >= 0x61 && w <= 0x66 = fromIntegral (w - 0x61) + 10
  | w >= 0x41 && w <= 0x46 = fromIntegral (w - 0x41) + 10
  | otherwise = 0
