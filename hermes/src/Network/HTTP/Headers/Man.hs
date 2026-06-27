{- |
RFC 2774 §4.1 @Man@ — the end-to-end /mandatory/ extension declaration
general header of the (experimental) HTTP Extension Framework.

== Grammar (RFC 2774 §3, §4.1)

@
mandatory       = \"Man\" \":\" 1#ext-decl
ext-decl        = \<\"\> ( absoluteURI | field-name ) \<\"\> [ namespace ] [ decl-extensions ]
namespace       = \";\" \"ns\" \"=\" header-prefix
header-prefix   = 2*DIGIT
decl-extensions = *( decl-ext )
decl-ext        = \";\" token [ \"=\" ( token | quoted-string ) ]
@

A @Man@ header lists one or more /mandatory/ extension declarations: the
ultimate recipient MUST consult and obey each, or respond with @510 Not
Extended@.  Each declaration identifies an extension by a quoted absolute
URI (or a Standards-Track field-name), optionally binds a @ns=NN@
header-prefix that namespaces the instance-data header fields elsewhere in
the message, and may carry @;token[=value]@ declaration parameters.

The HTTP Extension Framework is Experimental and never saw wide
deployment; faithful structured parsing of this comma-separated,
prefix-binding grammar is not warranted, so the field value is preserved
verbatim as a round-tripping 'ST.ShortText' (mirroring
'Network.HTTP.Headers.Referer.Referer').

Spec: <https://www.rfc-editor.org/rfc/rfc2774#section-4.1> (RFC 2774, Experimental HTTP Extension Framework).

See also: "Network.HTTP.Headers.Opt", "Network.HTTP.Headers.Ext", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PEPInfo".
-}
module Network.HTTP.Headers.Man (
  Man (..),
  manParser,
  renderMan,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMan)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of a @Man@ mandatory extension declaration list.
newtype Man = Man {rawMan :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Man where
  type ParseFailure Man = String
  type Cardinality Man = 'ZeroOrOne
  type Direction Man = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser manParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Man header: " <> show rest
    Fail -> Left "Failed to parse Man header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderMan


  headerName _ = hMan


manParser :: ParserT st String Man
manParser = Man <$> takeRestShortText


renderMan :: Man -> M.Builder
renderMan (Man v) = shortText v
