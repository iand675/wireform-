{- |
The uniform contract for a wire grammar — the shape every HTTP and
adjacent-protocol grammar module in hermes follows.

A 'WireGrammar' instance ties together a 'grammarParser' (built on the
shared 'ParserT' substrate, "Network.HTTP.Grammar.Parser") and a
'grammarRender' (built on the shared 'M.Builder' renderer,
"Network.HTTP.Grammar.Rendering"), and yields the uniform
'parseGrammar' runner for free. Adding a new non-header grammar — a URI
type, a media type, … — is: define the type, write the parser and
renderer, give the instance. The per-module @parseX@ runners that
previously copy-pasted the same @case runParser … of@ boilerplate (see
the former @parseIPv4@ \/ @parseIPv6@ \/ @parseIPAddress@) now delegate
to 'parseGrammar'.

This module exports only the contract. The parser primitives, the
renderers, and the shared value types live in
"Network.HTTP.Grammar.Parser", "Network.HTTP.Grammar.Rendering", and
"Network.HTTP.Grammar.Types" respectively; import those directly. (They
are not re-exported here because the parser and renderer modules
deliberately share names — @rfc8941Integer@ parses, @rfc8941Integer@
renders — and a blanket re-export would make those ambiguous.)

The header-specific richer abstraction
('Network.HTTP.Headers.KnownHeader', with its cardinality \/ direction
\/ 'HeaderMap' machinery) sits on top of this substrate and is
unrelated to this class.
-}
module Network.HTTP.Grammar (
  -- * The contract
  WireGrammar (..),
  -- * Uniform runner
  GrammarParseError (..),
  parseGrammar,
  renderGrammar,
  grammarParseErrorToString,
) where

import Data.ByteString (ByteString)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Grammar.Parser (ParserT, Result (..), runParser)


{- | A wire grammar for @a@: a parse\/render pair over the shared
flatparse-based 'ParserT' and the 'M.Builder' renderer.

The @st@ parameter of 'ParserT' is phantom (hermes only uses pure
mode); it is kept so grammar parsers compose with the rest of the
flatparse ecosystem.
-}
class WireGrammar a where
  -- | The error type the parser reports. 'String' is the common choice.
  type GrammarErr a

  -- | Parse an @a@. The @st@ parameter is phantom; see 'ParserT'.
  grammarParser :: forall st. ParserT st (GrammarErr a) a

  -- | Render an @a@ back to its wire form.
  grammarRender :: a -> M.Builder


{- | Why a 'parseGrammar' run failed: the grammar's own error, an
unconditional parse failure, or input left over after a successful
parse.
-}
data GrammarParseError e
  = GrammarParseErr e
  | GrammarParseFail
  | GrammarParseTrailing !ByteString
  deriving stock (Eq, Show)


-- | Parse a complete value, requiring exact consumption of the input.
parseGrammar :: WireGrammar a => ByteString -> Either (GrammarParseError (GrammarErr a)) a
parseGrammar bs = case runParser grammarParser bs of
  OK a "" -> Right a
  OK _ leftover -> Left (GrammarParseTrailing leftover)
  Fail -> Left GrammarParseFail
  Err e -> Left (GrammarParseErr e)


-- | Render an @a@ to a strict 'ByteString'.
renderGrammar :: WireGrammar a => a -> ByteString
renderGrammar = M.toStrictByteString . grammarRender


{- | Flatten a 'String'-error 'GrammarParseError' to a human-readable
'String', labelling the failure with @what@ (e.g. @"IPv4 address"@).
This is the bridge back to the @'Either' 'String' a@ runners that
pre-date this class.
-}
grammarParseErrorToString :: String -> GrammarParseError String -> String
grammarParseErrorToString what = \case
  GrammarParseErr e -> e
  GrammarParseFail -> "Failed to parse " <> what
  GrammarParseTrailing rest -> "Unconsumed input after parsing " <> what <> ": " <> show rest
