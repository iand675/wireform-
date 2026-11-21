{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
SOAP 1.1 @SOAPAction@ request header. A quoted URI reference names an
explicit action, a quoted empty string indicates the request URI, and an
empty field gives no indication of intent. URI references retain their
original spelling.

Spec: <https://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383528>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Slug".
-}
module Network.HTTP.Headers.SoapAction (
  SoapAction (SoapAction, SoapActionNoIntent, soapActionURI),
  SoapActionIntent (..),
  soapActionIntent,
  soapActionParser,
  renderSoapAction,
) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Grammar.URIReference (outboundURIReferenceBytes, uriReference, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSoapAction)
import qualified Network.HTTP.Headers.Mason as M
import qualified Wireform.Builder as WB


data SoapAction = QuotedSoapAction !ST.ShortText | SoapActionNoIntent
  deriving stock (Eq, Show)


{- | The legacy URI view. Reading an empty field through this pattern reports
empty text. Construction always creates the quoted form. Use
'soapActionIntent' to distinguish the two empty forms.
-}
pattern SoapAction :: ST.ShortText -> SoapAction
pattern SoapAction {soapActionURI} <- (legacyURI -> soapActionURI)
  where
    SoapAction uri = QuotedSoapAction uri


{-# COMPLETE SoapAction #-}


legacyURI :: SoapAction -> ST.ShortText
legacyURI SoapActionNoIntent = ""
legacyURI (QuotedSoapAction uri) = uri


data SoapActionIntent
  = SoapNoIntent
  | SoapRequestURI
  | SoapExplicitAction !ST.ShortText
  deriving stock (Eq, Show)


soapActionIntent :: SoapAction -> SoapActionIntent
soapActionIntent SoapActionNoIntent = SoapNoIntent
soapActionIntent (QuotedSoapAction uri)
  | ST.null uri = SoapRequestURI
  | otherwise = SoapExplicitAction uri


instance KnownHeader SoapAction where
  type ParseFailure SoapAction = String
  type Cardinality SoapAction = 'ZeroOrOne
  type Direction SoapAction = 'Request


  parseFromHeaders _ = parseSingleFieldLine "SOAPAction" soapActionParser


  renderToHeaders _ SoapActionNoIntent = ""
  renderToHeaders _ value@(QuotedSoapAction uri) =
    WB.toStrictByteStringExact (BSI.checkedAdd "SOAPAction" 2 (SBS.length (ST.toShortByteString uri))) (renderSoapAction value)


  renderToHeadersChecked _ SoapActionNoIntent = Just ""
  renderToHeadersChecked settings value@(QuotedSoapAction uri)
    | validURIText outboundURIReferenceBytes uri = Just (renderToHeaders settings value)
    | otherwise = Nothing


  headerName _ = hSoapAction


soapActionParser :: ParserT st String SoapAction
soapActionParser =
  ows *> ((QuotedSoapAction <$> ($(char '"') *> uriReference <* $(char '"'))) <|> (SoapActionNoIntent <$ eof)) <* ows


renderSoapAction :: SoapAction -> M.Builder
renderSoapAction SoapActionNoIntent = mempty
renderSoapAction (QuotedSoapAction uri) = M.char7 '"' <> shortText uri <> M.char7 '"'
{-# INLINE renderSoapAction #-}
