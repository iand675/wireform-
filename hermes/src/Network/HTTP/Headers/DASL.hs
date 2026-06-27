{- |
@DASL@ (RFC 5323 §9.1.4) — response header by which a WebDAV SEARCH
server advertises a query grammar (a @Coded-URL@, e.g.
@\<DAV:basicsearch\>@) that it supports. One @DASL@ header is sent per
supported grammar.

The query-grammar production is server-defined and effectively
opaque on the wire, so the value is preserved verbatim as a faithful
raw newtype (per the depth guidance for opaque/host-specific values).

Spec: <https://www.rfc-editor.org/rfc/rfc5323.html#section-9.1.4>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.If".
-}
module Network.HTTP.Headers.DASL (
  DASL (..),
  daslParser,
  renderDASL,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDASL)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single advertised DASL query grammar, preserved verbatim.
newtype DASL = DASL {daslGrammar :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader DASL where
  type ParseFailure DASL = String
  type Cardinality DASL = 'ZeroOrOne
  type Direction DASL = 'Response


  parseFromHeaders _ headers = case runParser daslParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing DASL header: " <> show rest
    Fail -> Left "Failed to parse DASL header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDASL


  headerName _ = hDASL


daslParser :: ParserT st String DASL
daslParser = DASL <$> takeRestShortText


renderDASL :: DASL -> M.Builder
renderDASL (DASL grammar) = shortText grammar
