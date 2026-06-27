{- |
RFC 4918 §10.4 @If@ request header — makes a request conditional on
the state of one or more resources, expressed as state tokens
(@Coded-URL@s) and entity-tags.

== Grammar

@
If            = "If" ":" ( 1*No-tag-list | 1*Tagged-list )
No-tag-list   = List
Tagged-list   = Resource 1*List
Resource      = Coded-URL
List          = "(" 1*Condition ")"
Condition     = ["Not"] (State-token | "[" entity-tag "]")
State-token   = Coded-URL
Coded-URL     = "<" absolute-URI ">"
@

This grammar is intricate — deeply nested, optionally @Not@-negated
conditions, mixing opaque state tokens with weak\/strong entity-tags,
and freely intermixed whitespace. Rather than fabricate a brittle
structured model, the raw value is preserved verbatim; callers that
need the condition tree can re-parse 'ifConditions' against the ABNF
above.

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.4>

See also: "Network.HTTP.Headers.LockToken", "Network.HTTP.Headers.Timeout", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.ETag".
-}
module Network.HTTP.Headers.If (
  If (..),
  ifParser,
  renderIf,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIf)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The @If@ condition production, preserved exactly as received.
newtype If = If {ifConditions :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader If where
  type ParseFailure If = String
  type Cardinality If = 'ZeroOrOne
  type Direction If = 'Request


  parseFromHeaders _ headers = case runParser ifParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing If header: " <> show rest
    Fail -> Left "Failed to parse If header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIf


  headerName _ = hIf


ifParser :: ParserT st String If
ifParser = If <$> takeRestShortText


renderIf :: If -> M.Builder
renderIf (If raw) = shortText raw
