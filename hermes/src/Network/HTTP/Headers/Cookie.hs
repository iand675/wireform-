{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Network.HTTP.Headers.Cookie
Description : The @Cookie@ request header (RFC 6265)

The @Cookie@ request header returns to the origin server the state it previously
stored on the user agent via @Set-Cookie@. The client sends it on subsequent
requests as a semicolon-separated list of @name=value@ pairs whose domain and
path match the request target, letting the server recognise a session, login,
or remembered preferences. It is a request-only header.

Spec: <https://www.rfc-editor.org/rfc/rfc6265#section-4.2>

See also: "Network.HTTP.Headers.SetCookie", "Network.HTTP.Headers.Cookie2", "Network.HTTP.Headers.SetCookie2", "Network.HTTP.Headers.ClearSiteData".
-}
module Network.HTTP.Headers.Cookie (
  Cookie (..),
  CookiePair (..),
  cookieParser,
  renderCookie,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (encodeCookiePairChecked, shortText, writeCookiePair)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCookie)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Encode.Direct (dWord8, directEncode)


-- | A single cookie pair. The value includes any surrounding double quotes.
data CookiePair = CookiePair
  { cookieName :: !ST.ShortText
  , cookieValue :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | Cookie pairs in wire order. An empty field is represented by an empty list.
newtype Cookie = Cookie {cookiePairs :: [CookiePair]}
  deriving stock (Eq, Show)


instance KnownHeader Cookie where
  type ParseFailure Cookie = String
  type Cardinality Cookie = 'ZeroOrOne
  type Direction Cookie = 'Request


  parseFromHeaders _ headers@(_ :| []) = parseSingleFieldLine "Cookie" cookieParser headers
  -- RFC 9113 §8.2.3 and RFC 9114 §4.2.1 join split fields with "; ".
  -- A semicolon cannot occur inside a pair, including a quoted value.
  parseFromHeaders _ headers =
    parseSingleFieldLine "Cookie" cookieParser (BS.intercalate "; " (NE.toList headers) :| [])


  renderToHeaders _ = M.toStrictByteString . renderCookie


  renderToHeadersChecked _ = encodeCookieChecked


  headerName _ = hCookie


cookiePairParser :: ParserT st String CookiePair
cookiePairParser = do
  name <- rfc9110Token
  $(char '=')
  value <- shortASCIIFromParser_ cookieValueBytes
  pure $ CookiePair name value
{-# INLINE cookiePairParser #-}


cookieParser :: ParserT st String Cookie
cookieParser = Cookie <$> (pairs <|> pure [])
  where
    pairs = (:) <$> cookiePairParser <*> many (ows *> $(char ';') *> ows *> cookiePairParser)
{-# INLINE cookieParser #-}


renderCookiePair :: CookiePair -> M.Builder
renderCookiePair (CookiePair name value) = shortText name <> "=" <> shortText value


renderCookie :: Cookie -> M.Builder
renderCookie (Cookie []) = mempty
renderCookie (Cookie (first : rest)) = renderCookiePair first <> foldMap renderNext rest
  where
    renderNext pair = "; " <> renderCookiePair pair


encodeCookieChecked :: Cookie -> Maybe BS.ByteString
encodeCookieChecked (Cookie []) = Just ""
encodeCookieChecked (Cookie [CookiePair name value]) = encodeCookiePairChecked name value
encodeCookieChecked (Cookie (first : rest)) = measure 0 first rest
  where
    add = BSI.checkedAdd "Cookie"
    width = SBS.length . ST.toShortByteString
    measure !size (CookiePair name value) pending
      | isHttpTokenText name, isCookieValueText value =
          let end = add (add size 1) (add (width name) (width value))
          in case pending of
               [] -> Just $ directEncode end $ \ptr start -> writePair ptr start first >>= writeRest ptr rest
               next : more -> measure (add end 2) next more
      | otherwise = Nothing
    writePair ptr offset (CookiePair name value) = writeCookiePair ptr offset name value
    writeRest _ [] !offset = pure offset
    writeRest ptr (pair : more) !offset = do
      semicolon <- dWord8 ptr offset 59
      start <- dWord8 ptr semicolon 32
      end <- writePair ptr start pair
      writeRest ptr more end
