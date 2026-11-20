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

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word64)
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
digestEntry = withByteString tokenBytes $ \_ algorithm -> do
  ows *> $(char '=') *> ows
  DigestValue (STU.fromByteStringUnsafe algorithm) <$> digestOutput (foldAlgorithm algorithm)
  where
    foldAlgorithm bytes
      | B.any isUpper bytes = B.map lower bytes
      | otherwise = bytes
    isUpper byte = byte >= 65 && byte <= 90
    lower byte
      | isUpper byte = byte + 32
      | otherwise = byte


digestOutput :: ByteString -> ParserT st String ShortText
digestOutput algorithm = case algorithm of
  "md5" -> base64Digest 22 $(string "==")
  "sha" -> base64Digest 27 $(char '=')
  "sha-256" -> base64Digest 43 $(char '=')
  "sha-512" -> base64Digest 86 $(string "==")
  "unixsum" -> decimalDigest 65535
  "unixcksum" -> decimalDigest 4294967295
  "adler32" -> hexDigest
  "crc32c" -> hexDigest
  "contentmd5" -> err "contentMD5 is only allowed in Want-Digest"
  _ -> quotedExtension <|> shortASCIIFromParser_ (asciiAlnumSetBytes extensionCharacters)
  where
    quotedExtension = shortTextFromLatin1 <$> byteStringOf ($(char '"') *> scanOpaqueBody 34 False)
    decimalDigest limit = shortASCIIFromParser_ (boundedDecimalWord limit (err "Digest checksum exceeds its width"))
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


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderDigest :: Digest -> M.Builder
renderDigest = renderSeparated ", " renderEntry . digestValueList
  where
    renderEntry (DigestValue alg val) = shortText alg <> M.char7 '=' <> renderHttpWordValue val
