{- |
Module      : Network.HTTP.Headers.PICSLabel
Description : The historic @PICS-Label@ HTTP header (PICS 1.1 label distribution).

The @PICS-Label@ header carries one or more content-rating labels as defined by
the Platform for Internet Content Selection (PICS). Its value is a @labellist@:
a parenthesized structure beginning with @PICS-1.1@ followed by service
descriptions, ratings, and options — e.g.

> (PICS-1.1 "http://www.gcf.org/v2.5" labels on "1994.11.05T08:15-0500"
>   exp "1995.12.31T23:59-0000" ratings (suds 0.5 density 0))

PICS is a historic protocol with a rich, deeply nested grammar (RFC-822 style
continuation lines, quoted strings, integer/fraction ratings). Per the project
depth guidance, an opaque/historic header of this kind is preserved verbatim as
a faithful raw newtype rather than reconstructing the full dead grammar.

Spec: W3C Recommendation \"PICS 1.1 Label Distribution -- Label Syntax and
Communication Protocols\" (REC-PICS-labels-961031)
<https://www.w3.org/TR/REC-PICS-labels-961031>

See also: "Network.HTTP.Headers.Protocol", "Network.HTTP.Headers.ProtocolRequest", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.PICSLabel (
  PICSLabel (..),
  picsLabelParser,
  renderPICSLabel,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPICSLabel)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @PICS-Label@ header value (a @labellist@), preserved verbatim.
newtype PICSLabel = PICSLabel {picsLabelValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader PICSLabel where
  type ParseFailure PICSLabel = String
  type Cardinality PICSLabel = 'ZeroOrOne
  type Direction PICSLabel = 'Response


  parseFromHeaders _ headers = case runParser picsLabelParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing PICS-Label header: " <> show rest
    Fail -> Left "Failed to parse PICS-Label header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPICSLabel


  headerName _ = hPICSLabel


picsLabelParser :: ParserT st String PICSLabel
picsLabelParser = PICSLabel <$> takeRestShortText


renderPICSLabel :: PICSLabel -> M.Builder
renderPICSLabel (PICSLabel v) = shortText v
