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

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCookie)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single cookie name-value pair
data CookiePair = CookiePair
  { cookieName :: !ST.ShortText
  , cookieValue :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | Cookie header containing one or more cookie pairs
newtype Cookie = Cookie {cookiePairs :: [CookiePair]}
  deriving stock (Eq, Show)


instance KnownHeader Cookie where
  type ParseFailure Cookie = String
  type Cardinality Cookie = 'ZeroOrOne
  type Direction Cookie = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser cookieParser header of
      OK cookie "" -> Right cookie
      OK _ rest -> Left $ "Unconsumed input after parsing Cookie header: " <> show rest
      Fail -> Left "Failed to parse Cookie header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCookie


  headerName _ = hCookie


cookiePairParser :: ParserT st String CookiePair
cookiePairParser = do
  name <- rfc9110Token
  $(char '=')
  value <- rfc9110Token <|> quotedString
  pure $ CookiePair name value


cookieParser :: ParserT st String Cookie
cookieParser = Cookie <$> (cookiePairParser `sepBy` (ows *> $(char ';') *> ows))


renderCookiePair :: CookiePair -> M.Builder
renderCookiePair (CookiePair name value) = shortText name <> "=" <> shortText value


renderCookie :: Cookie -> M.Builder
renderCookie (Cookie pairs) = M.intersperse "; " (map renderCookiePair pairs)
