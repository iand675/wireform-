{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Max-Age@ (WHATWG Fetch / W3C CORS) response header indicating
how many seconds (delta-seconds) the results of a preflight request — the allowed
methods and headers — may be cached by the browser. A negative value (commonly
@-1@) signals that the result must not be cached.

== Grammar

@
Access-Control-Max-Age = [ "-" ] 1*DIGIT
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-max-age>

See also: "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlRequestMethod", "Network.HTTP.Headers.AccessControlRequestHeaders".
-}
module Network.HTTP.Headers.AccessControlMaxAge (
  AccessControlMaxAge (..),
  accessControlMaxAgeParser,
  renderAccessControlMaxAge,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlMaxAge)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Value of the @Access-Control-Max-Age@ header, in seconds.
newtype AccessControlMaxAge = AccessControlMaxAge {maxAgeSeconds :: Int}
  deriving stock (Eq, Show)


instance KnownHeader AccessControlMaxAge where
  type ParseFailure AccessControlMaxAge = String
  type Cardinality AccessControlMaxAge = 'ZeroOrOne
  type Direction AccessControlMaxAge = 'Response


  parseFromHeaders _ headers = case runParser accessControlMaxAgeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Max-Age header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Max-Age header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlMaxAge


  headerName _ = hAccessControlMaxAge


accessControlMaxAgeParser :: ParserT st String AccessControlMaxAge
accessControlMaxAgeParser = do
  negative <- optional $(char '-')
  n <- anyAsciiDecimalInt
  pure $ AccessControlMaxAge (maybe n (const (negate n)) negative)


renderAccessControlMaxAge :: AccessControlMaxAge -> M.Builder
renderAccessControlMaxAge (AccessControlMaxAge n) = M.intDec n
