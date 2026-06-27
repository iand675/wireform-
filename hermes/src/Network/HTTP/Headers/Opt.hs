{- |
RFC 2774 §4.1 @Opt@ — the end-to-end /optional/ extension declaration
general header of the (experimental) HTTP Extension Framework.

== Grammar (RFC 2774 §3, §4.1)

@
optional        = \"Opt\" \":\" 1#ext-decl
ext-decl        = \<\"\> ( absoluteURI | field-name ) \<\"\> [ namespace ] [ decl-extensions ]
namespace       = \";\" \"ns\" \"=\" header-prefix
header-prefix   = 2*DIGIT
decl-extensions = *( decl-ext )
decl-ext        = \";\" token [ \"=\" ( token | quoted-string ) ]
@

An @Opt@ header lists one or more /optional/ extension declarations: the
ultimate recipient MAY consult and obey each, or ignore it entirely.  The
declaration shape is identical to 'Network.HTTP.Headers.Man.Man' — a
quoted absolute URI (or Standards-Track field-name) with an optional
@ns=NN@ header-prefix and @;token[=value]@ declaration parameters.

The HTTP Extension Framework is Experimental and never saw wide
deployment; faithful structured parsing of this comma-separated,
prefix-binding grammar is not warranted, so the field value is preserved
verbatim as a round-tripping 'ST.ShortText' (mirroring
'Network.HTTP.Headers.Referer.Referer').

Spec: <https://www.rfc-editor.org/rfc/rfc2774#section-4.1> (RFC 2774, Experimental HTTP Extension Framework).

See also: "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Ext", "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.PEPInfo".
-}
module Network.HTTP.Headers.Opt (
  Opt (..),
  optParser,
  renderOpt,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOpt)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of an @Opt@ optional extension declaration list.
newtype Opt = Opt {rawOpt :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Opt where
  type ParseFailure Opt = String
  type Cardinality Opt = 'ZeroOrOne
  type Direction Opt = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser optParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Opt header: " <> show rest
    Fail -> Left "Failed to parse Opt header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderOpt


  headerName _ = hOpt


optParser :: ParserT st String Opt
optParser = Opt <$> takeRestShortText


renderOpt :: Opt -> M.Builder
renderOpt (Opt v) = shortText v
