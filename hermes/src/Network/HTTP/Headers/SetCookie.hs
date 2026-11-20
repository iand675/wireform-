{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Network.HTTP.Headers.SetCookie
Description : The @Set-Cookie@ response header (RFC 6265)

The @Set-Cookie@ response header instructs the user agent to store a piece of
state — a @name=value@ pair plus optional attributes such as @Expires@,
@Max-Age@, @Domain@, @Path@, @Secure@, @HttpOnly@, and @SameSite@ — and to send
it back on matching future requests via @Cookie@. A single response may emit
several @Set-Cookie@ lines, so the header is modelled with cardinality
'ZeroOrMore' through the 'SetCookies' wrapper. It is a response-only header.

Spec: <https://www.rfc-editor.org/rfc/rfc6265#section-4.1>

See also: "Network.HTTP.Headers.Cookie", "Network.HTTP.Headers.SetCookie2", "Network.HTTP.Headers.Cookie2", "Network.HTTP.Headers.ClearSiteData".
-}
module Network.HTTP.Headers.SetCookie (
  SetCookie (
    SetCookie,
    setCookieName,
    setCookieValue,
    setCookieExpires,
    setCookieMaxAge,
    setCookieDomain,
    setCookiePath,
    setCookieSecure,
    setCookieHttpOnly,
    setCookieSameSite
  ),
  SetCookieExtension,
  setCookieExtensionText,
  mkSetCookieExtension,
  setCookieExtensions,
  withSetCookieExtensions,
  SameSitePolicy (..),
  SetCookies (..),
  setCookieParser,
  parseSetCookieField,
  parseSetCookieFieldWithLimits,
  renderSetCookie,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (cookieExpiryDateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hSetCookie)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Parser.Driver (parseByteString)


-- | SameSite cookie policy
data SameSitePolicy
  = SameSiteStrict
  | SameSiteLax
  | SameSiteNone
  deriving stock (Eq, Show)


{- | A cookie pair with its attributes. Use the shared record selectors
to read or update a parsed cookie, including when it has extensions.
The cookie value retains any surrounding quotes.
-}
data SetCookie
  = SetCookie
      { setCookieName :: !ST.ShortText
      , setCookieValue :: !ST.ShortText
      , setCookieExpires :: !(Maybe UTCTime)
      , setCookieMaxAge :: !(Maybe Int)
      , setCookieDomain :: !(Maybe ST.ShortText)
      , setCookiePath :: !(Maybe ST.ShortText)
      , setCookieSecure :: !Bool
      , setCookieHttpOnly :: !Bool
      , setCookieSameSite :: !(Maybe SameSitePolicy)
      }
  | SetCookieWithExtensions
      { setCookieName :: !ST.ShortText
      , setCookieValue :: !ST.ShortText
      , setCookieExpires :: !(Maybe UTCTime)
      , setCookieMaxAge :: !(Maybe Int)
      , setCookieDomain :: !(Maybe ST.ShortText)
      , setCookiePath :: !(Maybe ST.ShortText)
      , setCookieSecure :: !Bool
      , setCookieHttpOnly :: !Bool
      , setCookieSameSite :: !(Maybe SameSitePolicy)
      , setCookieExtensionEntries :: !(NonEmpty SetCookieExtension)
      }
  deriving stock (Eq, Show)


-- | An extension attribute in its original ASCII spelling, without leading
-- or trailing field whitespace.
newtype SetCookieExtension = SetCookieExtension {setCookieExtensionText :: ST.ShortText}
  deriving stock (Eq, Show)


-- | Build one extension attribute. The field's standard attributes use
-- their own record fields.
mkSetCookieExtension :: B.ByteString -> Maybe SetCookieExtension
mkSetCookieExtension input
  | not (B.null wire)
  , B.all (\b -> 32 <= b && b <= 126 && b /= 59) wire
  , CookieExtensionAttribute <- cookieAttributeKind name =
      Just (SetCookieExtension (STU.fromByteStringUnsafe wire))
  | otherwise = Nothing
  where
    wire = B.dropWhileEnd isCookieSpace (B.dropWhile isCookieSpace input)
    name = B.dropWhileEnd isCookieSpace (B.takeWhile (/= 61) wire)


-- | The extension attributes in the order received or supplied.
setCookieExtensions :: SetCookie -> [SetCookieExtension]
setCookieExtensions SetCookie {} = []
setCookieExtensions (SetCookieWithExtensions {setCookieExtensionEntries = entries}) = NE.toList entries


-- | Replace the extension attributes. An empty list clears them.
withSetCookieExtensions :: [SetCookieExtension] -> SetCookie -> SetCookie
withSetCookieExtensions [] cookie@SetCookie {} = cookie
withSetCookieExtensions [] (SetCookieWithExtensions name value expires age domain path secure httpOnly sameSite _) =
  SetCookie name value expires age domain path secure httpOnly sameSite
withSetCookieExtensions (first : rest) (SetCookie name value expires age domain path secure httpOnly sameSite) =
  SetCookieWithExtensions name value expires age domain path secure httpOnly sameSite (first :| rest)
withSetCookieExtensions (first : rest) cookie@SetCookieWithExtensions {} =
  cookie {setCookieExtensionEntries = first :| rest}


-- | Wrapper for multiple Set-Cookie headers
newtype SetCookies = SetCookies {getSetCookies :: [SetCookie]}
  deriving stock (Eq, Show)


instance KnownHeader SetCookies where
  type ParseFailure SetCookies = String
  type Cardinality SetCookies = 'ZeroOrMore
  type Direction SetCookies = 'Response


  parseFromHeaders _ (line :| []) = SetCookies . pure <$> parseSetCookieField line
  parseFromHeaders _ headers = SetCookies <$> traverse parseSetCookieField (NE.toList headers)


  renderToHeaders _ (SetCookies cookies) = map (M.toStrictByteString . renderSetCookie) cookies


  headerName _ = hSetCookie


-- | Parse one physical field line with the same rules as typed lookup.
parseSetCookieField :: B.ByteString -> Either String SetCookie
parseSetCookieField = parseSetCookieLine setCookieParser
{-# INLINE parseSetCookieField #-}


-- | Parse one physical line with limits on the @name=value@ pair and on
-- each attribute value, measured in bytes after trimming field whitespace.
-- An oversized pair fails. Oversized attributes are ignored individually.
parseSetCookieFieldWithLimits :: Int -> Int -> B.ByteString -> Either String SetCookie
parseSetCookieFieldWithLimits pairLimit attributeLimit bytes
  | B.length bytes <= pairLimit, B.length bytes <= attributeLimit = parseSetCookieField bytes
  | otherwise = parseSetCookieLine parser bytes
  where
    parser = do
      cookie <- parseCookiePair limitedName limitedValue
      cookieAttributes (checkAttributeLimit (max 0 attributeLimit)) limitedExtension cookie
    limitedName = do
      name <- rfc9110TokenBS
      if B.length name < pairLimit then pure (STU.fromByteStringUnsafe name) else failed
    limitedValue name = withByteString cookieValueBytes $ \() value ->
      let available = pairLimit - SBS.length (ST.toShortByteString name) - 1
      in if B.length value <= available then pure (STU.fromByteStringUnsafe value) else failed
    limitedExtension wire = case B.elemIndex 0x3d wire of
      Just eq | not (attributeFits (max 0 attributeLimit) (B.drop (eq + 1) wire)) -> Nothing
      _ -> mkSetCookieExtension wire


parseSetCookieLine :: ParserT st String SetCookie -> B.ByteString -> Either String SetCookie
parseSetCookieLine parser bytes =
  case parseByteString (ows *> parser <* ows <* eof) bytes of
    Right cookie -> Right cookie
    Left _ -> Left "Failed to parse Set-Cookie header"
{-# INLINE parseSetCookieLine #-}


{- | Parse a cookie pair and its attributes. Invalid standard attribute
values are ignored as in RFC 6265 section 5.2. Extension attributes
retain their spelling and relative order.
-}
setCookieParser :: ParserT st String SetCookie
setCookieParser =
  parseCookiePair rfc9110Token (const (shortASCIIFromParser_ cookieValueBytes))
    >>= cookieAttributes (pure ()) mkSetCookieExtension
{-# INLINE setCookieParser #-}


parseCookiePair
  :: ParserT st String ST.ShortText
  -> (ST.ShortText -> ParserT st String ST.ShortText)
  -> ParserT st String SetCookie
parseCookiePair parseName parseValue = do
  name <- parseName <* ows
  $(char '=')
  ows
  value <- parseValue name
  pure (SetCookie name value Nothing Nothing Nothing Nothing False False Nothing)
{-# INLINE parseCookiePair #-}


cookieAttributes
  :: ParserT st String ()
  -> (B.ByteString -> Maybe SetCookieExtension)
  -> SetCookie
  -> ParserT st String SetCookie
cookieAttributes checkValue parseExtension initial = attributes initial []
  where
    attributes !cookie !extensions =
      branch (ows *> $(char ';') *> ows) next (pure (withSetCookieExtensions (reverse extensions) cookie))
      where
        next = withOption (knownAttribute checkValue cookie)
          (\updated -> attributes updated extensions)
          (withByteString skipAttributeValue $ \() wire ->
            attributes cookie (maybe extensions (: extensions) (parseExtension wire)))
{-# INLINE cookieAttributes #-}


data CookieAttributeKind
  = CookiePath
  | CookieDomain
  | CookieMaxAge
  | CookieExpires
  | CookieSameSite
  | CookieSecure
  | CookieHttpOnly
  | CookieExtensionAttribute


cookieAttributeKind :: B.ByteString -> CookieAttributeKind
cookieAttributeKind name = case B.length name of
  4 | name `matches` "path" -> CookiePath
  6
    | name `matches` "domain" -> CookieDomain
    | name `matches` "secure" -> CookieSecure
  7
    | name `matches` "max-age" -> CookieMaxAge
    | name `matches` "expires" -> CookieExpires
  8
    | name `matches` "samesite" -> CookieSameSite
    | name `matches` "httponly" -> CookieHttpOnly
  _ -> CookieExtensionAttribute
{-# INLINE cookieAttributeKind #-}


knownAttribute :: ParserT st String () -> SetCookie -> ParserT st String SetCookie
knownAttribute checkValue cookie = do
  name <- rfc9110TokenBS <* ows
  case cookieAttributeKind name of
    CookieExtensionAttribute -> failed
    kind ->
      branch $(char '=')
        ((ows *> checkValue *> withValue kind <* attributeEnd) <|> (cookie <$ skipAttributeValue))
        (withoutValue kind <$ attributeEnd)
  where
    withValue = \case
      CookiePath ->
          (\path -> cookie {setCookiePath = Just path}) <$> pathValue
      CookieDomain ->
          (\domain -> cookie {setCookieDomain = Just domain}) <$> rfc9110Token
      CookieMaxAge ->
          (\age -> cookie {setCookieMaxAge = Just age}) <$> maxAgeValue
      CookieExpires ->
          (\date -> cookie {setCookieExpires = Just date}) <$> cookieExpiryDateParser
      CookieSameSite ->
          (\policy -> cookie {setCookieSameSite = policy}) <$> sameSiteValue
      kind -> flag kind <$ skipAttributeValue

    withoutValue = \case
      CookiePath -> cookie {setCookiePath = Just ""}
      CookieSameSite -> cookie {setCookieSameSite = Nothing}
      kind -> flag kind

    flag CookieSecure = cookie {setCookieSecure = True}
    flag CookieHttpOnly = cookie {setCookieHttpOnly = True}
    flag _ = cookie

    attributeEnd = ows *> (eof <|> lookahead $(char ';'))
{-# INLINE knownAttribute #-}


checkAttributeLimit :: Int -> ParserT st String ()
checkAttributeLimit limit = lookahead $ do
  rest <- takeRest
  let value = maybe rest (`B.take` rest) (B.elemIndex 0x3b rest)
  if attributeFits limit value then pure () else failed
{-# INLINE checkAttributeLimit #-}


attributeFits :: Int -> B.ByteString -> Bool
attributeFits limit wire =
  B.length wire <= limit || B.length (B.dropWhileEnd isCookieSpace (B.dropWhile isCookieSpace wire)) <= limit
{-# INLINE attributeFits #-}


skipAttributeValue :: ParserT st String ()
skipAttributeValue = skipMany $ withAnyWord8 $ \byte ->
  if byte == 0x09 || (byte >= 0x20 && byte /= 0x3b && byte /= 0x7f)
    then pure ()
    else failed
{-# INLINE skipAttributeValue #-}


isCookieSpace :: Word8 -> Bool
isCookieSpace byte = byte == 32 || byte == 9
{-# INLINE isCookieSpace #-}


pathValue :: ParserT st String ST.ShortText
pathValue = withByteString asciiPath $ \() bytes ->
  let trimmed = B.dropWhileEnd isCookieSpace bytes
  in if B.elem 0x09 trimmed then failed else pure (STU.fromByteStringUnsafe trimmed)
  where
    asciiPath = skipMany (skipSatisfyAscii (\c -> c == '\t' || (' ' <= c && c <= '~' && c /= ';')))


maxAgeValue :: ParserT st String Int
maxAgeValue = branch $(char '-') negative positive
  where
    positive = fromIntegral <$> saturatingDeltaSeconds (fromIntegral (maxBound :: Int))
    negative = do
      let limit = fromIntegral (maxBound :: Int) + 1
      magnitude <- saturatingDeltaSeconds limit
      pure (if magnitude == limit then minBound else negate (fromIntegral magnitude))


sameSiteValue :: ParserT st String (Maybe SameSitePolicy)
sameSiteValue = withByteString skipAttributeValue $ \() bytes ->
  let value = B.dropWhileEnd isCookieSpace bytes
  in pure $
       if value `matches` "lax"
         then Just SameSiteLax
         else
           if value `matches` "strict"
             then Just SameSiteStrict
             else
               if value `matches` "none"
                 then Just SameSiteNone
                 else Nothing


-- The expected attribute name or policy is statically lowercase ASCII.
matches :: B.ByteString -> B.ByteString -> Bool
matches input expected = B.length input == size && go 0
  where
    size = B.length expected
    go i
      | i == size = True
      | otherwise =
          let byte = BU.unsafeIndex input i
              lower = if 0x41 <= byte && byte <= 0x5a then byte .|. 0x20 else byte
          in lower == BU.unsafeIndex expected i && go (i + 1)
{-# INLINE matches #-}


renderSetCookie :: SetCookie -> M.Builder
renderSetCookie = \case
  SetCookie name value expires age domain path secure httpOnly sameSite ->
    known name value expires age domain path secure httpOnly sameSite
  SetCookieWithExtensions name value expires age domain path secure httpOnly sameSite extensions ->
    known name value expires age domain path secure httpOnly sameSite
      <> foldMap (\ext -> "; " <> shortText (setCookieExtensionText ext)) extensions
  where
    known name value expires age domain path secure httpOnly sameSite =
      shortText name <> "=" <> shortText value
        <> maybe mempty (\date -> "; Expires=" <> renderDate date) expires
        <> maybe mempty (\maxAge -> "; Max-Age=" <> M.intDec maxAge) age
        <> maybe mempty (\host -> "; Domain=" <> shortText host) domain
        <> maybe mempty (\scope -> "; Path=" <> shortText scope) path
        <> (if secure then "; Secure" else mempty)
        <> (if httpOnly then "; HttpOnly" else mempty)
        <> maybe mempty renderSameSite sameSite
    {-# INLINE known #-}

    renderSameSite SameSiteStrict = "; SameSite=Strict"
    renderSameSite SameSiteLax = "; SameSite=Lax"
    renderSameSite SameSiteNone = "; SameSite=None"
