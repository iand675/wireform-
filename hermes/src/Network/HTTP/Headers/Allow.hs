{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
The @Allow@ response header lists the HTTP methods a target resource
supports. Servers must send it in a @405 (Method Not Allowed)@ response and
may send it otherwise (for example, to answer an @OPTIONS@ request); an empty
field value advertises that the resource currently allows no methods at all.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.1>

See also: "Network.HTTP.Headers.AcceptPatch", "Network.HTTP.Headers.AcceptPost", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlRequestMethod".
-}
module Network.HTTP.Headers.Allow (
  Allow (..),
  HttpMethod (..),
  allowParser,
  renderAllow,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAllow)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Standard HTTP methods
data HttpMethod
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  | -- | Non-standard method
    CustomMethod !ST.ShortText
  deriving stock (Eq, Show)


-- | Allow header containing list of allowed HTTP methods
newtype Allow = Allow {allowedMethods :: [HttpMethod]}
  deriving stock (Eq, Show)


instance KnownHeader Allow where
  type ParseFailure Allow = String
  type Cardinality Allow = 'ZeroOrOne
  type Direction Allow = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser allowParser header of
      OK allow "" -> Right allow
      OK _ rest -> Left $ "Unconsumed input after parsing Allow header: " <> show rest
      Fail -> Left "Failed to parse Allow header"
      Err err -> Left err


  renderToHeaders _ = M.toStrictByteString . renderAllow


  headerName _ = hAllow


httpMethodParser :: ParserT st e HttpMethod
httpMethodParser =
  $( switch
      [|
        case _ of
          "GET" -> pure GET
          "HEAD" -> pure HEAD
          "POST" -> pure POST
          "PUT" -> pure PUT
          "DELETE" -> pure DELETE
          "CONNECT" -> pure CONNECT
          "OPTIONS" -> pure OPTIONS
          "TRACE" -> pure TRACE
          "PATCH" -> pure PATCH
          _ -> CustomMethod <$> rfc9110Token
        |]
   )


allowParser :: ParserT st String Allow
allowParser = Allow <$> (httpMethodParser `sepBy` (ows *> $(char ',') *> ows))


renderHttpMethod :: HttpMethod -> M.Builder
renderHttpMethod = \case
  GET -> "GET"
  HEAD -> "HEAD"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"
  CONNECT -> "CONNECT"
  OPTIONS -> "OPTIONS"
  TRACE -> "TRACE"
  PATCH -> "PATCH"
  CustomMethod method -> shortText method


renderAllow :: Allow -> M.Builder
renderAllow (Allow methods) = M.intersperse ", " (map renderHttpMethod methods)
