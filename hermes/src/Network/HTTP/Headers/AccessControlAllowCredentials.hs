{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Allow-Credentials@ (WHATWG Fetch / W3C CORS) response header
telling the browser whether the response may be exposed when the request's
credentials mode is @include@ (i.e. cookies, TLS client certificates, or HTTP
authentication were sent). Per the Fetch standard the only meaningful value is
@true@; we model it as a boolean so @false@ round-trips faithfully. When this is
@true@ the matching @Access-Control-Allow-Origin@ must name a specific origin
rather than the @*@ wildcard.

Spec: <https://fetch.spec.whatwg.org/#http-access-control-allow-credentials>

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.Origin", "Network.HTTP.Headers.AccessControlExposeHeaders", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlAllowHeaders".
-}
module Network.HTTP.Headers.AccessControlAllowCredentials (
  AccessControlAllowCredentials (..),
  accessControlAllowCredentialsParser,
  renderAccessControlAllowCredentials,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowCredentials)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | Value of the @Access-Control-Allow-Credentials@ header. @True@ renders as
the case-sensitive literal @true@.
-}
newtype AccessControlAllowCredentials = AccessControlAllowCredentials
  {allowCredentials :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader AccessControlAllowCredentials where
  type ParseFailure AccessControlAllowCredentials = String
  type Cardinality AccessControlAllowCredentials = 'ZeroOrOne
  type Direction AccessControlAllowCredentials = 'Response


  parseFromHeaders _ headers = case runParser accessControlAllowCredentialsParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Allow-Credentials header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Allow-Credentials header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowCredentials


  headerName _ = hAccessControlAllowCredentials


accessControlAllowCredentialsParser :: ParserT st String AccessControlAllowCredentials
accessControlAllowCredentialsParser =
  AccessControlAllowCredentials
    <$> ((True <$ $(string "true")) <|> (False <$ $(string "false")))


renderAccessControlAllowCredentials :: AccessControlAllowCredentials -> M.Builder
renderAccessControlAllowCredentials (AccessControlAllowCredentials b) =
  if b then "true" else "false"
