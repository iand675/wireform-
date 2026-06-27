{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
  SetCookie (..),
  SameSitePolicy (..),
  SetCookies (..),
  setCookieParser,
  renderSetCookie,
) where

import qualified Data.ByteString as B
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hSetCookie)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | SameSite cookie policy
data SameSitePolicy
  = SameSiteStrict
  | SameSiteLax
  | SameSiteNone
  deriving stock (Eq, Show)


-- | Set-Cookie header attributes
data SetCookie = SetCookie
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
  deriving stock (Eq, Show)


-- | Wrapper for multiple Set-Cookie headers
newtype SetCookies = SetCookies {getSetCookies :: [SetCookie]}
  deriving stock (Eq, Show)


instance KnownHeader SetCookies where
  type ParseFailure SetCookies = String
  type Cardinality SetCookies = 'ZeroOrMore
  type Direction SetCookies = 'Response


  parseFromHeaders _ headers = do
    cookies <- traverse parseSingleSetCookie (NE.toList headers)
    pure $ SetCookies cookies


  renderToHeaders _ (SetCookies cookies) = map (M.toStrictByteString . renderSetCookie) cookies


  headerName _ = hSetCookie


parseSingleSetCookie :: B.ByteString -> Either String SetCookie
parseSingleSetCookie header = case runParser setCookieParser header of
  OK setCookie "" -> Right setCookie
  OK _ rest -> Left $ "Unconsumed input after parsing Set-Cookie header: " <> show rest
  Fail -> Left "Failed to parse Set-Cookie header"
  Err err -> Left err


setCookieParser :: ParserT st String SetCookie
setCookieParser = do
  name <- rfc9110Token
  $(char '=')
  value <- rfc9110Token <|> quotedString
  attributes <- many (ows *> $(char ';') *> ows *> attributeParser)
  pure $ buildSetCookie name value attributes
  where
    attributeParser =
      expiresAttr
        <|> maxAgeAttr
        <|> domainAttr
        <|> pathAttr
        <|> secureAttr
        <|> httpOnlyAttr
        <|> sameSiteAttr

    expiresAttr = do
      $(string "Expires")
      $(char '=')
      expires <- dateParser
      pure $ \sc -> sc {setCookieExpires = Just expires}

    maxAgeAttr = do
      $(string "Max-Age")
      $(char '=')
      maxAge <- fromIntegral <$> anyAsciiDecimalWord
      pure $ \sc -> sc {setCookieMaxAge = Just maxAge}

    domainAttr = do
      $(string "Domain")
      $(char '=')
      domain <- rfc9110Token
      pure $ \sc -> sc {setCookieDomain = Just domain}

    pathAttr = do
      $(string "Path")
      $(char '=')
      path <- rfc9110Token <|> quotedString
      pure $ \sc -> sc {setCookiePath = Just path}

    secureAttr = $(string "Secure") $> (\sc -> sc {setCookieSecure = True})

    httpOnlyAttr = $(string "HttpOnly") $> (\sc -> sc {setCookieHttpOnly = True})

    sameSiteAttr = do
      $(string "SameSite")
      $(char '=')
      policy <-
        $( switch
            [|
              case _ of
                "Strict" -> pure SameSiteStrict
                "Lax" -> pure SameSiteLax
                "None" -> pure SameSiteNone
              |]
         )
      pure $ \sc -> sc {setCookieSameSite = Just policy}


buildSetCookie :: ST.ShortText -> ST.ShortText -> [SetCookie -> SetCookie] -> SetCookie
buildSetCookie name value =
  foldr ($) (SetCookie name value Nothing Nothing Nothing Nothing False False Nothing)


renderSetCookie :: SetCookie -> M.Builder
renderSetCookie sc =
  shortText (setCookieName sc)
    <> "="
    <> shortText (setCookieValue sc)
    <> maybe mempty (\expires -> "; Expires=" <> renderDate expires) (setCookieExpires sc)
    <> maybe mempty (\maxAge -> "; Max-Age=" <> M.intDec maxAge) (setCookieMaxAge sc)
    <> maybe mempty (\domain -> "; Domain=" <> shortText domain) (setCookieDomain sc)
    <> maybe mempty (\path -> "; Path=" <> shortText path) (setCookiePath sc)
    <> (if setCookieSecure sc then "; Secure" else mempty)
    <> (if setCookieHttpOnly sc then "; HttpOnly" else mempty)
    <> maybe mempty renderSameSite (setCookieSameSite sc)
  where
    renderSameSite SameSiteStrict = "; SameSite=Strict"
    renderSameSite SameSiteLax = "; SameSite=Lax"
    renderSameSite SameSiteNone = "; SameSite=None"
