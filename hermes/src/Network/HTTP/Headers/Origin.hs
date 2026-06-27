{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
@Origin@ request header naming the origin (scheme, host, and optional port — but
never a path) that caused the request. Browsers attach it to CORS requests and to
all requests with unsafe methods (@POST@, @PUT@, etc.), letting the server decide
whether to honour the cross-origin access and what to return in
@Access-Control-Allow-Origin@. The serialized value may be the literal @null@ for
opaque origins (e.g. @data:@ URLs or sandboxed iframes).

Spec: <https://www.rfc-editor.org/rfc/rfc6454#section-7>

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlAllowCredentials", "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.CrossOriginResourcePolicy", "Network.HTTP.Headers.Host".
-}
module Network.HTTP.Headers.Origin (
  Origin (..),
  OriginValue (..),
  originParser,
  renderOrigin,
) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOrigin)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Origin value can be null or a specific origin
data OriginValue
  = OriginNull
  | Origin
      { originScheme :: !ST.ShortText
      -- ^ e.g. "https"
      , originHost :: !ST.ShortText
      -- ^ e.g. "example.com"
      , originPort :: !(Maybe Word16)
      -- ^ Optional port
      }
  deriving stock (Eq, Show)


-- | Origin header containing origin information
newtype Origin = OriginHeader {originValue :: OriginValue}
  deriving stock (Eq, Show)


instance KnownHeader Origin where
  type ParseFailure Origin = String
  type Cardinality Origin = 'ZeroOrOne
  type Direction Origin = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser originParser header of
      OK o "" -> Right o
      OK _ rest -> Left $ "Unconsumed input after parsing Origin header: " <> show rest
      Fail -> Left "Failed to parse Origin header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderOrigin


  headerName _ = hOrigin


originParser :: ParserT st String Origin
originParser = OriginHeader <$> originValueParser
  where
    originValueParser = nullOrigin <|> specificOrigin
    nullOrigin = $(string "null") $> OriginNull
    specificOrigin = do
      scheme <- rfc9110Token
      $(string "://")
      host <- rfc9110Token
      port <- optional $ do
        $(char ':')
        anyAsciiDecimalWord
      pure $ Origin scheme host (fromIntegral <$> port)


renderOrigin :: Origin -> M.Builder
renderOrigin (OriginHeader OriginNull) = "null"
renderOrigin (OriginHeader (Origin scheme host mPort)) =
  shortText scheme
    <> "://"
    <> shortText host
    <> maybe mempty (\port -> M.char7 ':' <> M.word16Dec port) mPort
