{- |
Module      : Network.HTTP.Headers.P3P
Description : The obsolete @P3P@ HTTP response header (Platform for Privacy Preferences).

The @P3P@ header was defined by the Platform for Privacy Preferences (P3P) 1.0.
A server emits it either to give the location of a policy reference file or to
send a compact policy:

> P3P: policyref="/w3c/p3p.xml", CP="NON DSP COR CURa ADMa DEVa OUR IND PHY ONL UNI"

The value is a comma-separated list of @policyref@, @CP@, and extension fields.
The P3P specification became a W3C Recommendation in April 2002 and was
officially obsoleted on 2018-08-30; only Internet Explorer ever acted on it.

Per the project depth guidance, this obsolete header with an opaque/host-
specific value is preserved verbatim as a faithful raw newtype rather than
reconstructing a dead grammar.

Spec: W3C Recommendation \"The Platform for Privacy Preferences 1.0 (P3P1.0)
Specification\" <https://www.w3.org/TR/P3P/> (HTTP binding:
<https://www.w3.org/2001/08/draft-w3c-p3p-header-00.html>)

See also: "Network.HTTP.Headers.GetProfile", "Network.HTTP.Headers.SetProfile", "Network.HTTP.Headers.ProfileObject", "Network.HTTP.Headers.DNT".
-}
module Network.HTTP.Headers.P3P (
  P3P (..),
  p3pParser,
  renderP3P,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hP3P)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @P3P@ header value, preserved verbatim (obsolete privacy-policy reference).
newtype P3P = P3P {p3pValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader P3P where
  type ParseFailure P3P = String
  type Cardinality P3P = 'ZeroOrOne
  type Direction P3P = 'Response


  parseFromHeaders _ headers = case runParser p3pParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing P3P header: " <> show rest
    Fail -> Left "Failed to parse P3P header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderP3P


  headerName _ = hP3P


p3pParser :: ParserT st String P3P
p3pParser = P3P <$> takeRestShortText


renderP3P :: P3P -> M.Builder
renderP3P (P3P v) = shortText v
