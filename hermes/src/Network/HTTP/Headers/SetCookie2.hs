{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | RFC 2965 §3.2.2 @Set-Cookie2@ conveys an ordered list of cookies.
Physical field lines and their spelling are retained. Each line can contain
one or more cookies, including commas inside quoted values or port lists.
RFC 6265 supersedes this field.
-}
module Network.HTTP.Headers.SetCookie2 (
  SetCookie2 (SetCookie2, setCookie2Value),
  SetCookie2s (..),
  Cookie2Definition (..),
  Cookie2Attribute (..),
  setCookie2Entries,
  setCookie2FromEntries,
  setCookie2Parser,
  renderSetCookie2,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Char (isAsciiUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word16)
import Network.HTTP.Grammar.Authority (portNumber, uriHostBytes, uriSchemeBytes)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (opaqueShortText, renderHttpQuotedString, shortText)
import Network.HTTP.Grammar.URIReference (uriPathSuffixBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.Cookie2 (cookie2Decimal, cookie2ValueBytes)
import Network.HTTP.Headers.HeaderFieldName (hSetCookie2)
import qualified Network.HTTP.Headers.Mason as M
import Numeric.Natural (Natural)


data SetCookie2
  = RawSetCookie2 !ST.ShortText
  | ParsedSetCookie2 !ByteString !(NonEmpty Cookie2Definition)


{- | The legacy wire spelling of a field line. Use 'setCookie2FromEntries'
for checked authoring.
-}
pattern SetCookie2 :: ST.ShortText -> SetCookie2
pattern SetCookie2 {setCookie2Value} <- (legacyText -> setCookie2Value)
  where
    SetCookie2 value = RawSetCookie2 value


{-# COMPLETE SetCookie2 #-}


instance Eq SetCookie2 where
  left == right = wireBytes left == wireBytes right


instance Show SetCookie2 where
  showsPrec precedence (SetCookie2 value) =
    showParen (precedence > 10) $ showString "SetCookie2 {setCookie2Value = " . shows value . showChar '}'


legacyText :: SetCookie2 -> ST.ShortText
legacyText (RawSetCookie2 text) = text
legacyText (ParsedSetCookie2 wire _) = opaqueShortText (SBS.toShort wire)


wireBytes :: SetCookie2 -> ByteString
wireBytes (RawSetCookie2 text) = ST.toByteString text
wireBytes (ParsedSetCookie2 wire _) = wire


newtype SetCookie2s = SetCookie2s {getSetCookie2s :: [SetCookie2]}
  deriving stock (Eq, Show)


{- | A cookie with its required version and effective attributes. When
attribute names repeat, the first occurrence determines the value.
-}
data Cookie2Definition = Cookie2Definition
  { cookie2Name :: !ST.ShortText
  , cookie2Payload :: !ByteString
  , cookie2DefinitionVersion :: !Natural
  , cookie2Attributes :: ![Cookie2Attribute]
  }
  deriving stock (Eq, Show)


data Cookie2Attribute
  = Cookie2Comment !Text
  | Cookie2CommentURL !ST.ShortText
  | Cookie2Discard
  | Cookie2Domain !ByteString
  | Cookie2MaxAge !Natural
  | Cookie2Path !ByteString
  | Cookie2Port !(Maybe (NonEmpty Word16))
  | Cookie2Secure
  | Cookie2Extension !ST.ShortText !(Maybe ByteString)
  deriving stock (Eq, Show)


setCookie2Entries :: SetCookie2 -> Either String (NonEmpty Cookie2Definition)
setCookie2Entries (RawSetCookie2 text) = parseFieldLine "Set-Cookie2" definitionList (ST.toByteString text)
setCookie2Entries (ParsedSetCookie2 _ cookies) = Right cookies


{- | Construct one field line from cookie definitions. Invalid names,
attribute arguments and duplicate authored attributes fail validation.
-}
setCookie2FromEntries :: NonEmpty Cookie2Definition -> Either String SetCookie2
setCookie2FromEntries entries = do
  result <- parseFieldLine "Set-Cookie2" setCookie2Parser wire
  if setCookie2Entries result == Right entries
    then Right result
    else Left "Set-Cookie2 definitions do not match their wire representation"
  where
    wire = M.toStrictByteString (commaList renderDefinition entries)


instance KnownHeader SetCookie2s where
  type ParseFailure SetCookie2s = String
  type Cardinality SetCookie2s = 'ZeroOrMore
  type Direction SetCookie2s = 'Response


  parseFromHeaders _ fields = SetCookie2s <$> traverse parseLine (NE.toList fields)
    where
      parseLine wire = ParsedSetCookie2 wire <$> parseFieldLine "Set-Cookie2" definitionList wire


  renderToHeaders _ (SetCookie2s cookies) = map wireBytes cookies


  headerName _ = hSetCookie2


setCookie2Parser :: ParserT st String SetCookie2
setCookie2Parser = withByteString definitionList $ \cookies wire ->
  pure (ParsedSetCookie2 wire cookies)


renderSetCookie2 :: SetCookie2 -> M.Builder
renderSetCookie2 (RawSetCookie2 value) = shortText value
renderSetCookie2 (ParsedSetCookie2 wire _) = M.byteString wire


definitionList :: ParserT st String (NonEmpty Cookie2Definition)
definitionList = httpList definitionParser >>= either err pure . requireNonEmptyList "Set-Cookie2"


definitionParser :: ParserT st String Cookie2Definition
definitionParser = do
  name <- rfc9110TokenBS
  if BS.head name == 36
    then err "Set-Cookie2 names beginning with $ are reserved"
    else do
      payload <- assignment cookie2ValueBytes
      attributes (STU.fromByteStringUnsafe name) payload Set.empty Nothing []
{-# NOINLINE definitionParser #-}


attributes
  :: ST.ShortText
  -> ByteString
  -> Set.Set ByteString
  -> Maybe Natural
  -> [Cookie2Attribute]
  -> ParserT st String Cookie2Definition
attributes name payload seen version attrs =
  branch
    (ows *> $(char ';') *> ows)
    ( do
        wireName <- rfc9110TokenBS
        let key = attributeKey wireName
        if Set.member key seen
          then skipDuplicate *> attributes name payload seen version attrs
          else
            let updated = Set.insert key seen
            in if key == "version"
                 then assignment cookie2Decimal >>= \value -> attributes name payload updated (Just value) attrs
                 else do
                   attribute <- attributeParser key wireName
                   attributes name payload updated version (attribute : attrs)
    )
    ( case version of
        Nothing -> err "Set-Cookie2 requires Version"
        Just value -> pure (Cookie2Definition name payload value (reverse attrs))
    )
  where
    -- Ignore duplicate semantics while still consuming a complete lexical
    -- attribute, so a quoted comma cannot terminate the cookie.
    skipDuplicate = optional_ (assignment cookie2ValueBytes)
{-# NOINLINE attributes #-}


attributeParser :: ByteString -> ByteString -> ParserT st String Cookie2Attribute
attributeParser key wireName = case key of
  "comment" -> Cookie2Comment <$> assignment commentText
  "commenturl" -> Cookie2CommentURL <$> assignment commentURL
  "discard" -> pure Cookie2Discard
  "domain" -> Cookie2Domain <$> assignment cookie2ValueBytes
  "max-age" -> Cookie2MaxAge <$> assignment cookie2Decimal
  "path" -> Cookie2Path <$> assignment cookie2ValueBytes
  "port" -> Cookie2Port <$> optional (assignment quotedPorts)
  "secure" -> pure Cookie2Secure
  _ -> Cookie2Extension (STU.fromByteStringUnsafe wireName) <$> optional (assignment cookie2ValueBytes)
  where
    commentText = cookie2ValueBytes >>= either (const (err "Invalid Set-Cookie2 Comment UTF-8")) pure . TE.decodeUtf8'
    quotedPorts = do
      $(char '"')
      ports <- httpList portNumber <* $(char '"')
      either err pure (requireNonEmptyList "Set-Cookie2 Port" ports)
{-# NOINLINE attributeParser #-}


assignment :: ParserT st e a -> ParserT st e a
assignment value = ows *> $(char '=') *> ows *> value
{-# INLINE assignment #-}


commentURL :: ParserT st String ST.ShortText
commentURL = $(char '"') *> shortASCIIFromParser_ httpURL <* $(char '"')
  where
    httpURL = do
      scheme <- byteStringOf uriSchemeBytes
      if foldedName scheme == "http"
        then do
          $(string "://") *> uriHostBytes
          optional_ ($(char ':') *> skipMany (skipSatisfyAscii isDigit))
          optional_ (withByteString uriPathSuffixBytes (\() suffix -> if BS.elem 35 suffix then failed else pure ()))
        else failed
{-# NOINLINE commentURL #-}


foldedName :: ByteString -> ByteString
foldedName name
  | BSC.any isAsciiUpper name = BSC.map toLower name
  | otherwise = name


-- Standard spellings reuse static keys in the first-occurrence table.
attributeKey :: ByteString -> ByteString
attributeKey = \case
  "Version" -> "version"
  "Comment" -> "comment"
  "CommentURL" -> "commenturl"
  "Discard" -> "discard"
  "Domain" -> "domain"
  "Max-Age" -> "max-age"
  "Path" -> "path"
  "Port" -> "port"
  "Secure" -> "secure"
  name -> foldedName name


renderDefinition :: Cookie2Definition -> M.Builder
renderDefinition (Cookie2Definition name value version attrs) =
  shortText name <> "=" <> quoteBytes value <> "; Version=" <> decimal version <> foldMap renderAttribute attrs


renderAttribute :: Cookie2Attribute -> M.Builder
renderAttribute attribute =
  "; " <> case attribute of
    Cookie2Comment content -> "Comment=" <> quoteBytes (TE.encodeUtf8 content)
    Cookie2CommentURL url -> "CommentURL=\"" <> shortText url <> "\""
    Cookie2Discard -> "Discard"
    Cookie2Domain domain -> "Domain=" <> quoteBytes domain
    Cookie2MaxAge age -> "Max-Age=" <> decimal age
    Cookie2Path path -> "Path=" <> quoteBytes path
    Cookie2Port Nothing -> "Port"
    Cookie2Port (Just ports) -> "Port=\"" <> commaList M.word16Dec ports <> "\""
    Cookie2Secure -> "Secure"
    Cookie2Extension name value -> shortText name <> foldMap (("=" <>) . quoteBytes) value


quoteBytes :: ByteString -> M.Builder
quoteBytes = renderHttpQuotedString . TE.decodeLatin1


decimal :: Natural -> M.Builder
decimal = M.integerDec . toInteger


commaList :: (a -> M.Builder) -> NonEmpty a -> M.Builder
commaList render (first :| rest) = render first <> foldMap ((", " <>) . render) rest
