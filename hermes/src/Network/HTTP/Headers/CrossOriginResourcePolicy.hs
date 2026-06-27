{-# LANGUAGE TemplateHaskell #-}

{- |
@Cross-Origin-Resource-Policy@ (CORP) response header — lets a resource
declare which origins are allowed to embed it, mitigating cross-origin
read attacks (Spectre).

The value is a single keyword token; there are no parameters.

@
Cross-Origin-Resource-Policy = \"same-origin\" / \"same-site\" / \"cross-origin\"
@

Spec: WHATWG Fetch, <https://fetch.spec.whatwg.org/#cross-origin-resource-policy-header>.

See also: "Network.HTTP.Headers.CrossOriginEmbedderPolicy",
"Network.HTTP.Headers.CrossOriginOpenerPolicy",
"Network.HTTP.Headers.Origin",
"Network.HTTP.Headers.AccessControlAllowOrigin",
"Network.HTTP.Headers.XContentTypeOptions".
-}
module Network.HTTP.Headers.CrossOriginResourcePolicy (
  CrossOriginResourcePolicy (..),
  crossOriginResourcePolicyParser,
  renderCrossOriginResourcePolicy,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCrossOriginResourcePolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @Cross-Origin-Resource-Policy@ value.
data CrossOriginResourcePolicy
  = -- | @same-origin@ — only same-origin requests may read the resource.
    CorpSameOrigin
  | -- | @same-site@ — only same-site requests may read the resource.
    CorpSameSite
  | -- | @cross-origin@ — any origin may read the resource.
    CorpCrossOrigin
  deriving stock (Eq, Show)


instance KnownHeader CrossOriginResourcePolicy where
  type ParseFailure CrossOriginResourcePolicy = String
  type Cardinality CrossOriginResourcePolicy = 'ZeroOrOne
  type Direction CrossOriginResourcePolicy = 'Response


  parseFromHeaders _ headers = case runParser crossOriginResourcePolicyParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cross-Origin-Resource-Policy header: " <> show rest
    Fail -> Left "Failed to parse Cross-Origin-Resource-Policy header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCrossOriginResourcePolicy


  headerName _ = hCrossOriginResourcePolicy


crossOriginResourcePolicyParser :: ParserT st String CrossOriginResourcePolicy
crossOriginResourcePolicyParser =
  $( switch
      [|
        case _ of
          "same-origin" -> pure CorpSameOrigin
          "same-site" -> pure CorpSameSite
          "cross-origin" -> pure CorpCrossOrigin
        |]
   )


renderCrossOriginResourcePolicy :: CrossOriginResourcePolicy -> M.Builder
renderCrossOriginResourcePolicy = \case
  CorpSameOrigin -> "same-origin"
  CorpSameSite -> "same-site"
  CorpCrossOrigin -> "cross-origin"
