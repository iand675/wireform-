{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Allow-Credentials@ (WHATWG Fetch / W3C CORS) response header
telling the browser whether the response may be exposed when the request's
credentials mode is @include@ (i.e. cookies, TLS client certificates, or HTTP
authentication were sent). Fetch defines the case-sensitive wire value
@true@. To withhold permission, omit the field.

Spec: <https://fetch.spec.whatwg.org/#http-access-control-allow-credentials>

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.Origin", "Network.HTTP.Headers.AccessControlExposeHeaders", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlAllowHeaders".
-}
module Network.HTTP.Headers.AccessControlAllowCredentials (
  AccessControlAllowCredentials (..),
  accessControlAllowCredentialsParser,
  renderAccessControlAllowCredentials,
) where

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
  type Cardinality AccessControlAllowCredentials = 'ZeroOrMore
  type Direction AccessControlAllowCredentials = 'Response


  parseFromHeaders _ = parseSingleFieldLine "Access-Control-Allow-Credentials" accessControlAllowCredentialsParser


  renderToHeaders _ (AccessControlAllowCredentials True) = ["true"]
  renderToHeaders _ (AccessControlAllowCredentials False) = []


  omitHeaderValue = not . allowCredentials
  {-# INLINE omitHeaderValue #-}


  headerName _ = hAccessControlAllowCredentials


accessControlAllowCredentialsParser :: ParserT st String AccessControlAllowCredentials
accessControlAllowCredentialsParser = AccessControlAllowCredentials True <$ $(string "true")


renderAccessControlAllowCredentials :: AccessControlAllowCredentials -> M.Builder
renderAccessControlAllowCredentials (AccessControlAllowCredentials b) =
  if b then "true" else mempty
