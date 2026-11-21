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

import qualified Data.CharSet as CharSet
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAllow)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedTokenPairs, shortText)


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


  parseFromHeaders _ = fmap Allow . parseListFieldLines "Allow" (httpList httpMethodParser)


  renderToHeaders _ = M.toStrictByteString . renderAllow


  renderToHeadersChecked _ (Allow methods)
    | all validMethod methods = Just (encodeCommaSeparatedTokenPairs "" methodToken (const Nothing) methods)
    | otherwise = Nothing
    where
      validMethod (CustomMethod token) = isHttpTokenText token
      validMethod _ = True


  headerName _ = hAllow


httpMethodParser :: ParserT st e HttpMethod
httpMethodParser = standardMethod <|> (CustomMethod <$> rfc9110Token)
  where
    standardMethod =
      $( switch
           [|
             case _ of
               "GET" -> whole GET
               "HEAD" -> whole HEAD
               "POST" -> whole POST
               "PUT" -> whole PUT
               "DELETE" -> whole DELETE
               "CONNECT" -> whole CONNECT
               "OPTIONS" -> whole OPTIONS
               "TRACE" -> whole TRACE
               "PATCH" -> whole PATCH
             |]
       )
    whole method = method <$ notFollowedBy (skipSatisfyAscii (`CharSet.member` tokenCharSet))


allowParser :: ParserT st String Allow
allowParser = Allow <$> httpList httpMethodParser


methodToken :: HttpMethod -> ST.ShortText
methodToken = \case
  GET -> "GET"
  HEAD -> "HEAD"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"
  CONNECT -> "CONNECT"
  OPTIONS -> "OPTIONS"
  TRACE -> "TRACE"
  PATCH -> "PATCH"
  CustomMethod method -> method


renderAllow :: Allow -> M.Builder
renderAllow (Allow methods) = M.intersperse ", " (map (shortText . methodToken) methods)
