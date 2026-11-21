{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 3230 @Digest@ — a comma-separated list of @instance-digest@
values, each pairing a digest-algorithm token with its
algorithm-specific encoded output (base64 for the @SHA-*@\/@MD5@
families, decimal for @UNIXsum@\/@UNIXcksum@, hexadecimal for @ADLER32@
and @CRC32c@). It may be sent on
requests and responses.

This field is /obsolete/: RFC 9530 deprecates it in favour of
'Network.HTTP.Headers.ContentDigest.ContentDigest' and
'Network.HTTP.Headers.ReprDigest.ReprDigest'. Registered digest encodings
are validated and retained in their original spelling. Quoted extension
values retain their delimiters and escapes, with each octet stored as its
corresponding Latin-1 code point.

== Grammar

@
Digest          = #( instance-digest )
instance-digest = digest-algorithm \"=\" <encoded digest output>
@

Spec: <https://www.rfc-editor.org/rfc/rfc3230#section-4.3.2>

See also: "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.ReprDigest", "Network.HTTP.Headers.WantDigest", "Network.HTTP.Headers.ContentMD5".
-}
module Network.HTTP.Headers.Digest (
  Digest (..),
  DigestValue (..),
  digestValueList,
  digestParser,
  renderDigest,
) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Data.Word (Word64)
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeCommaSeparatedPairs, renderHttpWordValue, renderSeparated, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDigest)
import qualified Network.HTTP.Headers.Mason as M


{- | A single @instance-digest@: a digest-algorithm token and its
opaque, algorithm-specific encoded output (preserved verbatim).
-}
data DigestValue = DigestValue
  { digestAlgorithm :: !ShortText
  , digestEncoded :: !ShortText
  }
  deriving stock (Eq, Show)


-- | An explicitly empty field or a non-empty list of instance digests.
-- Use 'digestValueList' when the field may be empty.
data Digest
  = Digest {digestValues :: NE.NonEmpty DigestValue}
  | DigestEmpty
  deriving stock (Eq, Show)


instance Semigroup Digest where
  DigestEmpty <> digests = digests
  digests <> DigestEmpty = digests
  Digest left <> Digest right = Digest (left <> right)


instance KnownHeader Digest where
  type ParseFailure Digest = String
  type Cardinality Digest = 'ZeroOrMore
  type Direction Digest = 'RequestAndResponse


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Digest" (httpList digestEntry)
  renderToHeaders _ = pure . encodeCommaSeparatedPairs "=" digestAlgorithm (Just . digestEncoded) . digestValueList
  renderToHeadersChecked _ header
    | all validDigest entries = Just [encodeCommaSeparatedPairs "=" digestAlgorithm (Just . digestEncoded) entries]
    | otherwise = Nothing
    where
      entries = digestValueList header
  headerName _ = hDigest


fromEntries :: [DigestValue] -> Digest
fromEntries = maybe DigestEmpty Digest . NE.nonEmpty


-- | Return the instance digests, including an empty list for a blank field.
digestValueList :: Digest -> [DigestValue]
digestValueList DigestEmpty = []
digestValueList (Digest entries) = NE.toList entries


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

digestParser :: ParserT st String Digest
digestParser = fromEntries <$> httpList digestEntry


digestEntry :: ParserT st String DigestValue
digestEntry = do
  algorithm <- rfc9110Token <* ows <* $(char '=') <* ows
  DigestValue algorithm <$> digestOutput (digestEncoding algorithm)


data DigestEncoding
  = Base64Output {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | DecimalOutput {-# UNPACK #-} !Word64
  | HexOutput
  | WantDigestOnly
  | ExtensionOutput


digestEncoding :: ShortText -> DigestEncoding
digestEncoding name = case SBS.length (ST.toShortByteString name) of
  3
    | matches "md5" -> Base64Output 22 2
    | matches "sha" -> Base64Output 27 1
  6 | matches "crc32c" -> HexOutput
  7
    | matches "sha-256" -> Base64Output 43 1
    | matches "sha-512" -> Base64Output 86 2
    | matches "adler32" -> HexOutput
    | matches "unixsum" -> DecimalOutput 65_535
  9 | matches "unixcksum" -> DecimalOutput 4_294_967_295
  10 | matches "contentmd5" -> WantDigestOnly
  _ -> ExtensionOutput
  where
    matches expected = sameNegotiationToken expected name
{-# INLINE digestEncoding #-}


digestOutput :: DigestEncoding -> ParserT st String ShortText
digestOutput = \case
  Base64Output digits padding ->
    base64Digest (fromIntegral digits) (if padding == 2 then $(string "==") else $(char '='))
  DecimalOutput limit -> shortASCIIFromParser_ (boundedDecimalWord (fromIntegral limit) (err "Digest checksum exceeds its width"))
  HexOutput -> hexDigest
  WantDigestOnly -> err "contentMD5 is only allowed in Want-Digest"
  ExtensionOutput -> quotedExtension <|> shortASCIIFromParser_ (asciiAlnumSetBytes extensionCharacters)
  where
    quotedExtension = shortTextFromLatin1 <$> byteStringOf ($(char '"') *> scanOpaqueBody 34 False)
    hexDigest = shortASCIIFromParser_ $
      withSpan (asciiSetBytes hexCharacters) $ \_ (Span (Pos start) (Pos end)) ->
        if end - start <= 8
          then pure ()
          else err "Hexadecimal Digest checksum exceeds eight digits"


-- The Base64 alphabet and padding positions determine the decoded width.
-- Readers accept nonzero pad bits, including the SHA example in RFC 3230.
base64Digest :: Word64 -> ParserT st String () -> ParserT st String ShortText
base64Digest digits padding = shortASCIIFromParser_ $
  withSpan (asciiAlnumSetBytes base64Characters) $ \_ (Span (Pos start) (Pos end)) ->
    if end - start == digits
      then padding
      else err "Incorrect Base64 Digest width"
{-# INLINE base64Digest #-}


base64Characters, hexCharacters, extensionCharacters :: AsciiSet
base64Characters = asciiSet (CharSet.range '0' '9' <> CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> "+/")
hexCharacters = asciiSet (CharSet.range '0' '9' <> CharSet.range 'A' 'F' <> CharSet.range 'a' 'f')
extensionCharacters = asciiSet (CharSet.range '!' '~' `CharSet.difference` "\",;")
{-# NOINLINE base64Characters #-}
{-# NOINLINE hexCharacters #-}
{-# NOINLINE extensionCharacters #-}


validDigest :: DigestValue -> Bool
validDigest (DigestValue algorithm value) = isHttpTokenText algorithm && size /= 0
  && case digestEncoding algorithm of
    Base64Output digits padding ->
      size == digits + padding && at digits == 61 && (padding == 1 || at (digits + 1) == 61)
        && validBase64Prefix bytes digits
    DecimalOutput limit -> decimal limit 0 0
    HexOutput -> size <= 8 && prefix hexCharacters size 0
    WantDigestOnly -> False
    ExtensionOutput
      | at 0 == 34 -> isHttpWordValue value
      | otherwise -> prefix extensionCharacters size 0
  where
    bytes = ST.toShortByteString value
    size = SBS.length bytes
    at = SBS.unsafeIndex bytes
    prefix (AsciiSet low high) end = scan
      where
        scan !offset = offset == end || (asciiMember low high (BSI.w2c (at offset)) && scan (offset + 1))
    -- The largest allowed checksum is 32-bit, so the multiply cannot
    -- overflow a Word64 even on the step that rejects the value.
    decimal limit !offset !total
      | offset == size = True
      | otherwise = digit <= 9 && next <= limit && decimal limit (offset + 1) next
      where
        digit = at offset - 48
        next = total * 10 + fromIntegral digit


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderDigest :: Digest -> M.Builder
renderDigest = renderSeparated ", " renderEntry . digestValueList
  where
    renderEntry (DigestValue alg val) = shortText alg <> M.char7 '=' <> renderHttpWordValue val
