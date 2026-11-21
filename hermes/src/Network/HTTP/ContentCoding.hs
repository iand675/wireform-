{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
The "HTTP Content Coding Registry", maintained by IANA at <https://www.iana.org/assignments/http-parameters/>, registers content-coding names.

These values are used in the @Content-Encoding@ and @Accept-Encoding@ fields
(<https://www.rfc-editor.org/rfc/rfc9110#section-8.4.1>).

Values to be added to this namespace require IETF Review (see Section 4.8 of [RFC8126]) and MUST conform to the purpose of content coding defined in Section 8.4.1.
-}
module Network.HTTP.ContentCoding (
  ContentCoding (..),
  sameContentCoding,
  contentCodingParser,
  contentCodingText,
  isValidContentCoding,
  renderContentCoding,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import qualified Data.CharSet as CharSet
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util


data ContentCoding
  = -- | AES-GCM encryption with a 128-bit content encryption key
    AES128GCM
  | -- | Brotli Compressed Data Format
    Brotli
  | -- | UNIX "compress" data format [Welch, T., "A Technique for High Performance Data Compression", IEEE Computer 17(6), June 1984.]
    Compress
  | -- | "deflate" compressed data ([RFC1951]) inside the "zlib" data format ([RFC1950])
    Deflate
  | -- | W3C Efficient XML Interchange
    EfficientXMLInterchange
  | -- | GZIP file format [RFC1952]
    GZip
  | -- | The default (identity) encoding; the use of no transformation whatsoever
    Identity
  | -- | JAR file format with Pack200 compression, then GZIP compression
    Pack200GZip
  | -- | A stream of bytes compressed using the Zstandard protocol
    ZStd
  | -- | A custom content coding. This constructor is used for content codings that are not part of the IANA registry.
    Custom {-# UNPACK #-} !ShortText
  deriving stock (Eq, Show)


-- | Compare coding names using HTTP's ASCII case-insensitive semantics.
-- The parser normalizes registered names and their legacy aliases.
sameContentCoding :: ContentCoding -> ContentCoding -> Bool
sameContentCoding (Custom a) (Custom b) = CI.foldCase (ST.toByteString a) == CI.foldCase (ST.toByteString b)
sameContentCoding a b = a == b
{-# INLINE sameContentCoding #-}


contentCodingParser :: ParserT st e ContentCoding
contentCodingParser = registeredCodingParser <|> extensionCoding
  where
    extensionCoding = do
      token <- rfc9110TokenBS
      pure $
        if BS.any (\w -> w >= 65 && w <= 90) token
          then case runParser registeredCodingParser (BSC.map toLower token) of
            OK coding "" -> coding
            _ -> Custom (STU.fromByteStringUnsafe token)
          else Custom (STU.fromByteStringUnsafe token)


-- Match common lowercase names without allocating or folding a token.
registeredCodingParser :: ParserT st e ContentCoding
registeredCodingParser =
  $( switch
       [|
         case _ of
           "aes128gcm" -> wholeToken AES128GCM
           "br" -> wholeToken Brotli
           "compress" -> wholeToken Compress
           "deflate" -> wholeToken Deflate
           "exi" -> wholeToken EfficientXMLInterchange
           "gzip" -> wholeToken GZip
           "identity" -> wholeToken Identity
           "pack200-gzip" -> wholeToken Pack200GZip
           "x-compress" -> wholeToken Compress
           "x-gzip" -> wholeToken GZip
           "zstd" -> wholeToken ZStd
         |]
   )
  where
    wholeToken coding = coding <$ notFollowedBy (skipSatisfyAscii (`CharSet.member` tokenCharSet))


contentCodingText :: ContentCoding -> ShortText
contentCodingText = \case
  AES128GCM -> "aes128gcm"
  Brotli -> "br"
  Compress -> "compress"
  Deflate -> "deflate"
  EfficientXMLInterchange -> "exi"
  GZip -> "gzip"
  Identity -> "identity"
  Pack200GZip -> "pack200-gzip"
  ZStd -> "zstd"
  Custom x -> x
{-# INLINE contentCodingText #-}


isValidContentCoding :: ContentCoding -> Bool
isValidContentCoding (Custom token) = isHttpTokenText token
isValidContentCoding _ = True
{-# INLINE isValidContentCoding #-}


renderContentCoding :: ContentCoding -> M.Builder
renderContentCoding = shortText . contentCodingText
