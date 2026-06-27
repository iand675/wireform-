{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Allow-Origin@ (WHATWG Fetch / W3C CORS) response header
indicating whether the response may be shared with requesting code from the
given origin. It echoes a single origin, the literal @null@, or the @*@ wildcard
(the wildcard is forbidden when the request carries credentials). Servers that
vary this value per request should also emit @Vary: Origin@.

== Grammar

@
Access-Control-Allow-Origin = "*" / "null" / origin
origin                      = scheme "://" host [ ":" port ]
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-allow-origin>

See also: "Network.HTTP.Headers.Origin", "Network.HTTP.Headers.AccessControlAllowCredentials", "Network.HTTP.Headers.AccessControlExposeHeaders", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AccessControlAllowOrigin (
  AccessControlAllowOrigin (..),
  accessControlAllowOriginParser,
  renderAccessControlAllowOrigin,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowOrigin)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Value of the @Access-Control-Allow-Origin@ header.
data AccessControlAllowOrigin
  = -- | @*@ — any origin may read the response.
    AllowOriginWildcard
  | -- | @null@ — the serialized opaque origin.
    AllowOriginNull
  | -- | A single serialized origin (@scheme://host[:port]@).
    AllowOrigin
      { allowOriginScheme :: !ST.ShortText
      , allowOriginHost :: !ST.ShortText
      , allowOriginPort :: !(Maybe Word16)
      }
  deriving stock (Eq, Show)


instance KnownHeader AccessControlAllowOrigin where
  type ParseFailure AccessControlAllowOrigin = String
  type Cardinality AccessControlAllowOrigin = 'ZeroOrOne
  type Direction AccessControlAllowOrigin = 'Response


  parseFromHeaders _ headers = case runParser accessControlAllowOriginParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Allow-Origin header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Allow-Origin header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowOrigin


  headerName _ = hAccessControlAllowOrigin


accessControlAllowOriginParser :: ParserT st String AccessControlAllowOrigin
accessControlAllowOriginParser = wildcard <|> specificOrigin <|> nullOrigin
  where
    wildcard = AllowOriginWildcard <$ $(char '*')
    nullOrigin = AllowOriginNull <$ $(string "null")
    specificOrigin = do
      scheme <- rfc9110Token
      $(string "://")
      host <- rfc9110Token
      port <- optional ($(char ':') *> anyAsciiDecimalWord)
      pure $ AllowOrigin scheme host (fromIntegral <$> port)


renderAccessControlAllowOrigin :: AccessControlAllowOrigin -> M.Builder
renderAccessControlAllowOrigin = \case
  AllowOriginWildcard -> "*"
  AllowOriginNull -> "null"
  AllowOrigin scheme host mPort ->
    shortText scheme
      <> "://"
      <> shortText host
      <> maybe mempty (\p -> M.char7 ':' <> M.word16Dec p) mPort
