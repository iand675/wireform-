{- |
SOAP 1.1 @SOAPAction@ request header (SOAP 1.1 §6.1.1) — a (possibly empty)
quoted URI that indicates the intent of a SOAP HTTP request. The URI has no
prescribed meaning beyond identifying the action; servers MAY use it for
routing or dispatch.

The on-the-wire value is a @quoted-string@ wrapping the URI. This module
stores the unquoted URI and re-adds the surrounding double quotes (escaping
any embedded @\"@ or @\\@) on render.

Spec: <https://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383528>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Slug".
-}
module Network.HTTP.Headers.SoapAction (
  SoapAction (..),
  soapActionParser,
  renderSoapAction,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSoapAction)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | The URI carried (in quoted form) by a @SOAPAction@ header. An empty
'ST.ShortText' corresponds to the wire value @\"\"@.
-}
newtype SoapAction = SoapAction {soapActionURI :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SoapAction where
  type ParseFailure SoapAction = String
  type Cardinality SoapAction = 'ZeroOrOne
  type Direction SoapAction = 'Request


  parseFromHeaders _ headers = case runParser soapActionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing SoapAction header: " <> show rest
    Fail -> Left "Failed to parse SoapAction header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSoapAction


  headerName _ = hSoapAction


soapActionParser :: ParserT st String SoapAction
soapActionParser = SoapAction <$> quotedString


renderSoapAction :: SoapAction -> M.Builder
renderSoapAction (SoapAction uri) =
  M.char8 '"' <> foldr esc mempty (ST.toString uri) <> M.char8 '"'
  where
    esc c acc
      | c == '"' || c == '\\' = M.char8 '\\' <> M.char8 c <> acc
      | otherwise = M.char8 c <> acc
