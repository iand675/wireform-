{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Allow-Origin@ (WHATWG Fetch / W3C CORS) response header
indicating whether the response may be shared with requesting code from the
given origin. It echoes a single origin, the literal @null@, or the @*@ wildcard
available outside credentials mode @include@. Servers that
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
  corsCheck,
  accessControlAllowOriginParser,
  renderAccessControlAllowOrigin,
) where

import qualified Data.ByteString.Char8 as BC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import Network.HTTP.Grammar.Authority (SerializedOrigin (..), corsSerializedOriginParser, encodeCorsOriginsChecked, renderSerializedOrigin)
import Network.HTTP.Headers
import Network.HTTP.Headers.AccessControlAllowCredentials (AccessControlAllowCredentials (..), CorsCredentialsMode (..))
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowOrigin)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Origin (CorsRequestOrigin, corsRequestOriginBytes)
import Network.HTTP.Headers.Parsing.Util


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


  parseFromHeaders _ = parseSingleFieldLine "Access-Control-Allow-Origin" accessControlAllowOriginParser


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowOrigin


  renderToHeadersChecked _ AllowOriginWildcard = Just "*"
  renderToHeadersChecked _ AllowOriginNull = Just "null"
  renderToHeadersChecked _ (AllowOrigin scheme host port) =
    encodeCorsOriginsChecked (SerializedOrigin scheme host port) []


  headerName _ = hAccessControlAllowOrigin


accessControlAllowOriginParser :: ParserT st String AccessControlAllowOrigin
accessControlAllowOriginParser = wildcard <|> specificOrigin <|> nullOrigin
  where
    wildcard = AllowOriginWildcard <$ $(char '*')
    nullOrigin = AllowOriginNull <$ $(string "null")
    specificOrigin = do
      SerializedOrigin scheme host port <- corsSerializedOriginParser
      pure (AllowOrigin scheme host port)


{- | Apply the Fetch CORS check to the response headers using the original
request's credentials mode and serialized origin, including redirect taint.
The comparison uses the response field bytes with HTTP OWS removed.
-}
corsCheck :: CorsCredentialsMode -> CorsRequestOrigin -> HeaderMap -> Bool
corsCheck mode requestOrigin headers =
  case lookupRawHeader hAccessControlAllowOrigin headers of
    Just (wire :| []) ->
      (matches wire || matches (BC.dropWhileEnd isOWS (BC.dropWhile isOWS wire)))
        && (mode /= CorsInclude || allowedCredentials)
    _ -> False
  where
    matches wire =
      wire == corsRequestOriginBytes requestOrigin || (mode /= CorsInclude && wire == "*")
    allowedCredentials =
      case lookupHeader @AccessControlAllowCredentials headers of
        Right (Just (AccessControlAllowCredentials True)) -> True
        _ -> False


renderAccessControlAllowOrigin :: AccessControlAllowOrigin -> M.Builder
renderAccessControlAllowOrigin = \case
  AllowOriginWildcard -> "*"
  AllowOriginNull -> "null"
  AllowOrigin scheme host port -> renderSerializedOrigin (SerializedOrigin scheme host port)
