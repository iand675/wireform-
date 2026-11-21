{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The userinfo-free authority syntax shared by HTTP fields. Host spellings
are retained as received, including brackets on IP literals. Transport
ports use 'Word16'. Fields with a textual port grammar retain its spelling.

Spec: <https://www.rfc-editor.org/rfc/rfc3986#section-3.2>

See also: "Network.IPAddress", "Network.HTTP.Headers.Host",
"Network.HTTP.Headers.Origin", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Grammar.Authority (
  uriHost,
  uriHostBytes,
  isUriHostText,
  cdnIdentifier,
  isCdnIdentifier,
  warningAgent,
  isWarningAgent,
  uriSchemeBytes,
  isUriSchemeText,
  pctEncoded,
  portNumber,
  hostAndPort,
  hostAndPortBytes,
  hostAndPortListMemberBytes,
  renderHostAndPort,
  encodeHostAndPortChecked,
  SerializedOrigin (..),
  serializedOriginParser,
  corsSerializedOriginParser,
  serializedOriginListMember,
  isSerializedOriginListMember,
  renderSerializedOrigin,
  encodeCorsOriginsChecked,
  receivedBy,
  receivedByBytes,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.Either (isRight)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import Foreign.Ptr (minusPtr, plusPtr)
import Network.HTTP.Grammar (WireGrammar (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import qualified Network.HTTP.Headers.Mason as M
import Network.IPAddress (skipIPLiteralParser)
import Wireform.Encode.Direct (dBytes, dWord8)
import Wireform.Parser.Driver (parseByteString)


-- | A nonempty URI host, including any brackets or percent escapes.
uriHost :: ParserT st String ShortText
uriHost = shortASCIIFromParser_ uriHostBytes
{-# INLINE uriHost #-}


uriHostBytes :: ParserT st String ()
uriHostBytes = hostBytes registeredNameChars
{-# INLINE uriHostBytes #-}


-- | An RFC 8586 host with an optional decimal port, or an HTTP token
-- pseudonym. Retain the spelling and leave the field delimiters unread.
cdnIdentifier :: ParserT st String ShortText
cdnIdentifier = shortASCIIFromParser_ cdnIdentifierBytes
{-# INLINE cdnIdentifier #-}


cdnIdentifierBytes :: ParserT st String ()
cdnIdentifierBytes = hostOrPseudonymBytes cdnHostChars boundary
  where
    boundary = notFollowedBy (skipSatisfyAscii (\c -> not (isOWS c || c == ',' || c == ';')))
{-# INLINE cdnIdentifierBytes #-}


-- | Check a full CDN identifier, with no list separators left unread.
isCdnIdentifier :: ShortText -> Bool
isCdnIdentifier text =
  isHttpTokenText text || isRight (parseByteString (cdnIdentifierBytes <* eof) (ST.toByteString text))
{-# INLINE isCdnIdentifier #-}


-- | An RFC 7234 warning agent followed by the required space.
warningAgent :: ParserT st String ShortText
warningAgent = hostOrPseudonym registeredNameChars (lookahead $(char ' '))
{-# INLINE warningAgent #-}


-- | Validate just the agent, without the space required in a Warning field.
isWarningAgent :: ShortText -> Bool
isWarningAgent text =
  isHttpTokenText text || isRight (parseByteString (hostOrPseudonymBytes registeredNameChars eof) (ST.toByteString text))


hostOrPseudonym :: CharSet -> ParserT st String () -> ParserT st String ShortText
hostOrPseudonym hostChars boundary = shortASCIIFromParser_ (hostOrPseudonymBytes hostChars boundary)
{-# INLINE hostOrPseudonym #-}


hostOrPseudonymBytes :: CharSet -> ParserT st String () -> ParserT st String ()
hostOrPseudonymBytes hostChars boundary = (tokenBytes <* boundary) <|> (authority <* boundary)
  where
    authority = hostBytes hostChars *> optional_ ($(char ':') *> skipMany (skipSatisfyAscii isDigit))
{-# INLINE hostOrPseudonymBytes #-}


cdnHostChars :: CharSet
cdnHostChars = CharSet.difference registeredNameChars ",;"
{-# NOINLINE cdnHostChars #-}


uriSchemeBytes :: ParserT st String ()
uriSchemeBytes = skipSatisfyAscii isLatinLetter *> skipMany (skipSatisfyAscii uriSchemeTail)
{-# INLINE uriSchemeBytes #-}


isUriSchemeText :: ShortText -> Bool
isUriSchemeText value =
  not (SBS.null bytes)
    && isLatinLetter (BSI.w2c (SBS.unsafeIndex bytes 0))
    && scanCompactBytes (uriSchemeTail . BSI.w2c) bytes 1
  where
    bytes = ST.toShortByteString value
{-# INLINE isUriSchemeText #-}


uriSchemeTail :: Char -> Bool
uriSchemeTail c = isLatinLetter c || isDigit c || c == '+' || c == '-' || c == '.'
{-# INLINE uriSchemeTail #-}


hostBytes :: CharSet -> ParserT st String ()
hostBytes allowed =
  skipIPLiteralParser <|> registeredName
  where
    ordinary = skipSatisfyAscii (`CharSet.member` allowed)
    registeredName =
      (skipSome ordinary <|> (pctEncoded *> skipMany ordinary))
        *> skipMany (pctEncoded *> skipMany ordinary)
{-# INLINE hostBytes #-}


pctEncoded :: ParserT st e ()
pctEncoded = $(char '%') *> skipSatisfyAscii hexDigit *> skipSatisfyAscii hexDigit
  where
    hexDigit c = isDigit c || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f')
{-# INLINE pctEncoded #-}


registeredNameChars :: CharSet
registeredNameChars =
  CharSet.range 'A' 'Z'
    <> CharSet.range 'a' 'z'
    <> CharSet.range '0' '9'
    <> "-._~!$&'()*+,;="


-- | Parse a nonempty decimal port, checking the bound before conversion.
portNumber :: ParserT st String Word16
portNumber = fromIntegral <$> boundedDecimalWord 65_535 (err "Port exceeds 65535")
{-# INLINE portNumber #-}


{- | A nonempty URI host and optional port. A bare colon is an empty URI
port and is represented by 'Nothing'.
-}
hostAndPort :: (ShortText -> Maybe Word16 -> a) -> ParserT st String a
hostAndPort construct = construct <$> uriHost <*> uriPort
{-# INLINE hostAndPort #-}


-- | Parse the authority while retaining a slice of the host's wire bytes.
hostAndPortBytes :: (ByteString -> Maybe Word16 -> a) -> ParserT st String a
hostAndPortBytes construct =
  construct <$> withByteString uriHostBytes (\_ wire -> pure wire) <*> uriPort
{-# INLINE hostAndPortBytes #-}


-- | An authority followed by a field-list separator or end of input.
hostAndPortListMemberBytes :: ParserT st String ()
hostAndPortListMemberBytes = hostBytes listHostChars *> (() <$ uriPort)
{-# INLINE hostAndPortListMemberBytes #-}


uriPort :: ParserT st String (Maybe Word16)
uriPort = branch $(char ':') (optional portNumber) (pure Nothing)
{-# INLINE uriPort #-}


renderHostAndPort :: ShortText -> Maybe Word16 -> M.Builder
renderHostAndPort host port = shortText host <> maybe mempty (\p -> M.char7 ':' <> M.word16Dec p) port
{-# INLINE renderHostAndPort #-}


-- | Check the host component before appending its optional transport port.
encodeHostAndPortChecked :: ShortText -> Maybe Word16 -> Maybe ByteString
encodeHostAndPortChecked host port
  | isUriHostText host = Just $ case port of
      Nothing -> SBS.fromShort bytes
      Just number -> BSI.unsafeCreateUptoN (BSI.checkedAdd "host and port" width 6) $ \ptr -> do
        SBS.copyToPtr bytes 0 ptr width
        digits <- dWord8 ptr width 58
        end <- Prim.runB Prim.word16Dec number (ptr `plusPtr` digits)
        pure (end `minusPtr` ptr)
  | otherwise = Nothing
  where
    bytes = ST.toShortByteString host
    width = SBS.length bytes


-- | Validate a nonempty URI host, including brackets around IP literals.
isUriHostText :: ShortText -> Bool
isUriHostText = validHostText registeredNameChars
{-# INLINE isUriHostText #-}


validHostText :: CharSet -> ShortText -> Bool
validHostText allowed host =
  not (SBS.null bytes)
    && (scanCompactBytes (commonHostByte . BSI.w2c) bytes 0
        || isRight (parseByteString (hostBytes allowed <* eof) (SBS.fromShort bytes)))
  where
    bytes = ST.toShortByteString host
{-# INLINE validHostText #-}


commonHostByte :: Char -> Bool
commonHostByte c = isLatinLetter c || isDigit c || c == '.' || c == '-' || c == '_' || c == '~'
{-# INLINE commonHostByte #-}


-- | The scheme, host, and optional port of a serialized origin.
data SerializedOrigin = SerializedOrigin
  { serializedScheme :: !ShortText
  , serializedHost :: !ShortText
  , serializedPort :: !(Maybe Word16)
  }
  deriving stock (Eq, Show)


instance WireGrammar SerializedOrigin where
  type GrammarErr SerializedOrigin = String
  grammarParser = serializedOriginParser
  grammarRender = renderSerializedOrigin


serializedOriginParser :: ParserT st String SerializedOrigin
serializedOriginParser = originWithHost uriHostBytes
{-# INLINE serializedOriginParser #-}


{- | Parse a CORS origin, including host spellings emitted by the WHATWG
URL serializer. The response check compares against the request's exact
serialization.
-}
corsSerializedOriginParser :: ParserT st String SerializedOrigin
corsSerializedOriginParser = originWithHost (hostBytes corsHostChars)


originWithHost :: ParserT st String () -> ParserT st String SerializedOrigin
originWithHost hostParser = do
  scheme <- shortASCIIFromParser_ uriSchemeBytes
  $(string "://")
  host <- shortASCIIFromParser_ hostParser
  port <- branch $(char ':') (Just <$> portNumber) (pure Nothing)
  pure (SerializedOrigin scheme host port)
{-# INLINE originWithHost #-}


corsHostChars :: CharSet
-- The visible special-domain bytes absent from RFC 3986 reg-name.
corsHostChars = registeredNameChars <> "\"\x60{}"
{-# NOINLINE corsHostChars #-}


-- | A serialized origin in a comma-separated field, preserving its spelling.
serializedOriginListMember :: ParserT st String ShortText
serializedOriginListMember = shortASCIIFromParser_ serializedOriginListMemberBytes
{-# INLINE serializedOriginListMember #-}


serializedOriginListMemberBytes :: ParserT st String ()
serializedOriginListMemberBytes =
  uriSchemeBytes *> $(string "://") *> hostBytes originListHostChars *> optional_ ($(char ':') *> portNumber)
{-# INLINE serializedOriginListMemberBytes #-}


-- | Check a complete member, without normalizing wire spelling.
isSerializedOriginListMember :: ShortText -> Bool
isSerializedOriginListMember value =
  -- Common registered names can be checked in place. The full grammar
  -- handles percent escapes, IP literals and uncommon host punctuation.
  (size >= 5 && isLatinLetter (byteAt 0) && scheme 1)
    || isRight (parseByteString (serializedOriginListMemberBytes <* eof) (SBS.fromShort bytes))
  where
    bytes = ST.toShortByteString value
    size = SBS.length bytes
    byteAt = BSI.w2c . SBS.unsafeIndex bytes
    scheme !offset
      | offset > size - 4 = False
      | c == ':' =
          byteAt (offset + 1) == '/' && byteAt (offset + 2) == '/'
            && commonHostByte (byteAt (offset + 3)) && host (offset + 4)
      | otherwise = uriSchemeTail c && scheme (offset + 1)
      where
        c = byteAt offset
    host !offset
      | offset == size = True
      | c == ':' = offset + 1 < size && port 0 (offset + 1)
      | otherwise = commonHostByte c && host (offset + 1)
      where
        c = byteAt offset
    port :: Int -> Int -> Bool
    port !valueSoFar !offset
      | offset == size = True
      | otherwise = isDigit c && next <= 65_535 && port next (offset + 1)
      where
        c = byteAt offset
        next = valueSoFar * 10 + fromEnum c - fromEnum '0'


-- The comma and double quote delimit Fetch list members and quoted strings.
originListHostChars :: CharSet
originListHostChars = CharSet.difference corsHostChars ",\""
{-# NOINLINE originListHostChars #-}


-- Share the character-set construction across calls to the inlined parser.
listHostChars :: CharSet
listHostChars = CharSet.difference registeredNameChars ","
{-# NOINLINE listHostChars #-}


renderSerializedOrigin :: SerializedOrigin -> M.Builder
renderSerializedOrigin (SerializedOrigin scheme host port) =
  shortText scheme <> "://" <> renderHostAndPort host port
{-# INLINE renderSerializedOrigin #-}


-- | Validate all origins before writing the space-separated field.
encodeCorsOriginsChecked :: SerializedOrigin -> [SerializedOrigin] -> Maybe ByteString
encodeCorsOriginsChecked first rest = measure 0 first rest
  where
    add = BSI.checkedAdd "serialized origins"
    measure !size (SerializedOrigin scheme host port) pending
      | isUriSchemeText scheme && validHostText corsHostChars host =
          let separatorAndPort = case port of
                Nothing -> 3
                Just _ -> 9
              capacity = add (add size separatorAndPort) (add (width scheme) (width host))
          in case pending of
               [] -> Just $ BSI.unsafeCreateUptoN capacity $ \ptr -> writeOrigin ptr 0 first >>= writeRest ptr rest
               next : more -> measure (add capacity 1) next more
      | otherwise = Nothing
    width = SBS.length . ST.toShortByteString
    writeOrigin ptr offset (SerializedOrigin scheme host port) = do
      schemeEnd <- copyText ptr offset scheme
      hostStart <- dBytes ptr schemeEnd "://"
      hostEnd <- copyText ptr hostStart host
      case port of
        Nothing -> pure hostEnd
        Just number -> do
          start <- dWord8 ptr hostEnd 58
          end <- Prim.runB Prim.word16Dec number (ptr `plusPtr` start)
          pure (end `minusPtr` ptr)
    writeRest _ [] !offset = pure offset
    writeRest ptr (origin : more) !offset = do
      start <- dWord8 ptr offset 32
      end <- writeOrigin ptr start origin
      writeRest ptr more end
    copyText ptr offset value =
      let bytes = ST.toShortByteString value
          size = SBS.length bytes
      in SBS.copyToPtr bytes 0 (ptr `plusPtr` offset) size *> pure (offset + size)


{- | The @Via@ hop identifier. A pseudonym is a full HTTP token. A
reg-name excludes unescaped list and comment delimiters in this field.
-}
receivedBy :: ParserT st String ShortText
receivedBy = shortASCIIFromParser_ receivedByBytes
{-# INLINE receivedBy #-}


receivedByBytes :: ParserT st String ()
receivedByBytes = authority <|> pseudonym
  where
    pseudonym = tokenBytes *> (() <$ uriPort) <* boundary
    authority = hostBytes viaNameChars *> (() <$ uriPort) <* boundary
    boundary = notFollowedBy (skipSatisfyAscii (\c -> not (isOWS c || c == ',')))
{-# INLINE receivedByBytes #-}


viaNameChars :: CharSet
viaNameChars = CharSet.difference registeredNameChars ",()"
{-# NOINLINE viaNameChars #-}
