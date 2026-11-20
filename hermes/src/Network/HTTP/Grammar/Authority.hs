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
  cdnIdentifier,
  warningAgent,
  uriSchemeBytes,
  pctEncoded,
  portNumber,
  hostAndPort,
  hostAndPortBytes,
  hostAndPortListMemberBytes,
  renderHostAndPort,
  SerializedOrigin (..),
  serializedOriginParser,
  serializedOriginListMember,
  renderSerializedOrigin,
  receivedBy,
) where

import Data.ByteString (ByteString)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Network.HTTP.Grammar (WireGrammar (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import qualified Network.HTTP.Headers.Mason as M
import Network.IPAddress (skipIPLiteralParser)


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
cdnIdentifier = hostOrPseudonym cdnHostChars boundary
  where
    boundary = notFollowedBy (skipSatisfyAscii (\c -> not (isOWS c || c == ',' || c == ';')))
{-# INLINE cdnIdentifier #-}


-- | An RFC 7234 warning agent followed by the required space.
warningAgent :: ParserT st String ShortText
warningAgent = hostOrPseudonym registeredNameChars (lookahead $(char ' '))
{-# INLINE warningAgent #-}


hostOrPseudonym :: CharSet -> ParserT st String () -> ParserT st String ShortText
hostOrPseudonym hostChars boundary = shortASCIIFromParser_ ((tokenBytes <* boundary) <|> (authority <* boundary))
  where
    authority = hostBytes hostChars *> optional_ ($(char ':') *> skipMany (skipSatisfyAscii isDigit))
{-# INLINE hostOrPseudonym #-}


cdnHostChars :: CharSet
cdnHostChars = CharSet.difference registeredNameChars ",;"
{-# NOINLINE cdnHostChars #-}


uriSchemeBytes :: ParserT st String ()
uriSchemeBytes = skipSatisfyAscii isLatinLetter *> skipMany (skipSatisfyAscii schemeTail)
  where
    schemeTail c = isLatinLetter c || isDigit c || c == '+' || c == '-' || c == '.'
{-# INLINE uriSchemeBytes #-}


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
serializedOriginParser = do
  scheme <- shortASCIIFromParser_ uriSchemeBytes
  $(string "://")
  host <- uriHost
  port <- branch $(char ':') (Just <$> portNumber) (pure Nothing)
  pure (SerializedOrigin scheme host port)
{-# INLINE serializedOriginParser #-}


-- | A serialized origin in a comma-separated field, preserving its spelling.
serializedOriginListMember :: ParserT st String ShortText
serializedOriginListMember =
  shortASCIIFromParser_ $
    uriSchemeBytes *> $(string "://") *> hostBytes listHostChars *> optional_ ($(char ':') *> portNumber)
{-# INLINE serializedOriginListMember #-}


-- Share the character-set construction across calls to the inlined parser.
listHostChars :: CharSet
listHostChars = CharSet.difference registeredNameChars ","
{-# NOINLINE listHostChars #-}


renderSerializedOrigin :: SerializedOrigin -> M.Builder
renderSerializedOrigin (SerializedOrigin scheme host port) =
  shortText scheme <> "://" <> renderHostAndPort host port
{-# INLINE renderSerializedOrigin #-}


{- | The @Via@ hop identifier. A pseudonym is a full HTTP token. A
reg-name excludes unescaped list and comment delimiters in this field.
-}
receivedBy :: ParserT st String ShortText
receivedBy = shortASCIIFromParser_ (authority <|> pseudonym)
  where
    pseudonym = tokenBytes *> (() <$ uriPort) <* boundary
    authority = hostBytes viaNameChars *> (() <$ uriPort) <* boundary
    boundary = notFollowedBy (skipSatisfyAscii (\c -> not (isOWS c || c == ',')))
{-# INLINE receivedBy #-}


viaNameChars :: CharSet
viaNameChars = CharSet.difference registeredNameChars ",()"
{-# NOINLINE viaNameChars #-}
