{- |
RFC 2774 §4.3 @Ext@ — the end-to-end extension acknowledgement response
header of the (experimental) HTTP Extension Framework.

== Grammar (RFC 2774 §4.3)

@
ext = \"Ext\" \":\"
@

The @Ext@ header is used by the ultimate recipient to indicate that all
end-to-end mandatory extension declarations (carried by
'Network.HTTP.Headers.Man.Man') in the request were fulfilled.  Per the
RFC it serves \"exclusively\" as an extension acknowledgement and \"can
not carry any other information\", so its value is normally empty.

The HTTP Extension Framework is Experimental and never saw wide
deployment, so we do not fabricate a structured type: the (usually empty)
field value is preserved verbatim as a round-tripping 'ST.ShortText',
mirroring 'Network.HTTP.Headers.Referer.Referer'.

Spec: <https://www.rfc-editor.org/rfc/rfc2774#section-4.3> (RFC 2774, Experimental HTTP Extension Framework).

See also: "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Opt", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PEPInfo".
-}
module Network.HTTP.Headers.Ext (
  Ext (..),
  extParser,
  renderExt,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hExt)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of an @Ext@ acknowledgement header (normally empty).
newtype Ext = Ext {rawExt :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Ext where
  type ParseFailure Ext = String
  type Cardinality Ext = 'ZeroOrOne
  type Direction Ext = 'Response


  parseFromHeaders _ headers = case runParser extParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Ext header: " <> show rest
    Fail -> Left "Failed to parse Ext header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderExt


  headerName _ = hExt


extParser :: ParserT st String Ext
extParser = Ext <$> takeRestShortText


renderExt :: Ext -> M.Builder
renderExt (Ext v) = shortText v
