{-# LANGUAGE TemplateHaskell #-}

{- |
@tracestate@ request\/response header — W3C Trace Context.

Carries vendor-specific trace identification data as an ordered,
comma-separated list of opaque @key=value@ members:

@
tracestate  = list-member *( OWS \",\" OWS list-member )
list-member = key \"=\" value
key         = lcalpha *( lcalpha / DIGIT / \"_\" / \"-\" / \"*\" / \"/\" / \"@\" )
value       = *chr       ; printable US-ASCII excluding \",\" and \"=\"
@

Members are preserved in order: the leftmost member is the most
recently updated trace state. Keys and values are surfaced as raw
text; the spec's internal-whitespace allowance in @value@ is not
emitted so members serialize unambiguously.

Spec: <https://www.w3.org/TR/trace-context/#tracestate-header>.

See also: "Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.XCorrelationID".
-}
module Network.HTTP.Headers.Tracestate (
  Tracestate (..),
  TracestateMember (..),
  tracestateParser,
  renderTracestate,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (digit)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTracestate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single @key=value@ list member.
data TracestateMember = TracestateMember
  { tracestateKey :: !ST.ShortText
  , tracestateValue :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | The ordered, non-empty list of @tracestate@ members.
newtype Tracestate = Tracestate {tracestateMembers :: NE.NonEmpty TracestateMember}
  deriving stock (Eq, Show)


instance KnownHeader Tracestate where
  type ParseFailure Tracestate = String
  type Cardinality Tracestate = 'ZeroOrOne
  type Direction Tracestate = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser tracestateParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Tracestate header: " <> show rest
    Fail -> Left "Failed to parse Tracestate header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderTracestate


  headerName _ = hTracestate


-- | @key@ characters: @lcalpha@, @DIGIT@ and @_-*\/\@@.
keyCharSet :: CharSet
keyCharSet = CharSet.range 'a' 'z' <> digit <> CharSet.fromList "_-*/@"


{- | @value@ characters: printable US-ASCII (@0x21-0x7E@) excluding
comma (@0x2C@) and equals (@0x3D@).
-}
valueCharSet :: CharSet
valueCharSet =
  CharSet.range '\x21' '\x2B'
    <> CharSet.range '\x2D' '\x3C'
    <> CharSet.range '\x3E' '\x7E'


tracestateParser :: ParserT st String Tracestate
tracestateParser = Tracestate <$> (memberParser `sepBy1` (ows *> $(char ',') *> ows))
  where
    memberParser = do
      key <- shortASCIIFromParser_ (skipSome (skipSatisfyAscii (`CharSet.member` keyCharSet)))
      $(char '=')
      value <- shortASCIIFromParser_ (skipSome (skipSatisfyAscii (`CharSet.member` valueCharSet)))
      pure (TracestateMember key value)


renderTracestate :: Tracestate -> M.Builder
renderTracestate (Tracestate members) =
  M.intersperse "," (map renderMember (NE.toList members))
  where
    renderMember (TracestateMember key value) =
      shortText key <> M.char7 '=' <> shortText value
