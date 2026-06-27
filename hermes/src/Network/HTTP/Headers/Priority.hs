{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9218 — the HTTP @Priority@ header field, used by clients to signal,
and by servers to (re)assign, the relative priority of responses on a
connection. It carries an RFC 8941 Structured Field Dictionary.

== Grammar

@
Priority   = sf-dictionary
@

Two members are defined:

* @u@ — /urgency/, an @sf-integer@ in the range @0@–@7@. Lower means more
  urgent. Absent (or out of range) defaults to @3@.
* @i@ — /incremental/, an @sf-boolean@. Absent defaults to @?0@ (false).
  As a Dictionary member with a Boolean value it may be written bare
  (just @i@), which is equivalent to @i=?1@.

Members other than @u@ and @i@, and any member parameters, are ignored
for forward compatibility, as required by the specification.

Note: the boolean payload is parsed and rendered directly here (via the
@?1@/@?0@ literals) rather than through the shared RFC 8941 boolean
helpers, whose 'Bool' mapping is inverted relative to RFC 9218's wire
semantics (true = @?1@).

Spec: <https://www.rfc-editor.org/rfc/rfc9218.html>

See also: "Network.HTTP.Headers.Urgency", "Network.HTTP.Headers.SecPurpose".
-}
module Network.HTTP.Headers.Priority (
  Priority (..),
  priorityParser,
  renderPriority,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (digit)
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPriority)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | A parsed @Priority@ value, normalised to its two defined members.
data Priority = Priority
  { priorityUrgency :: !Int
  -- ^ Urgency @0@–@7@ (lower is more urgent). Defaults to @3@.
  , priorityIncremental :: !Bool
  -- ^ Whether the response can be processed incrementally. Defaults to 'False'.
  }
  deriving stock (Eq, Show)


-- | The default priority when a member is absent: @u=3@, @i=?0@.
defaultPriority :: Priority
defaultPriority = Priority 3 False


instance KnownHeader Priority where
  type ParseFailure Priority = String
  type Cardinality Priority = 'ZeroOrOne
  type Direction Priority = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser priorityParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise ->
          Left ("Unconsumed input after parsing Priority: " <> show leftover)
    Fail -> Left "Failed to parse Priority header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderPriority


  headerName _ = hPriority


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | One Dictionary member, reduced to whatever this header cares about.
data Member
  = MUrgency !Int
  | MIncremental !Bool
  | MOther


priorityParser :: ParserT st String Priority
priorityParser = do
  ows
  members <- member `sepBy` (ows *> $(char ',') *> ows)
  ows
  pure (foldl' applyMember defaultPriority members)


-- | Apply a member, later members overriding earlier ones (RFC 8941 §3.2).
applyMember :: Priority -> Member -> Priority
applyMember p = \case
  MUrgency n -> p {priorityUrgency = clampUrgency n}
  MIncremental b -> p {priorityIncremental = b}
  MOther -> p


-- | RFC 9218 §4.1: urgency outside @0@–@7@ is treated as the default.
clampUrgency :: Int -> Int
clampUrgency n
  | n >= 0 && n <= 7 = n
  | otherwise = 3


member :: ParserT st String Member
member = do
  key <- memberKey
  case key of
    "u" -> do
      $(char '=')
      n <- rfc8941Integer
      _ <- rfc8941Parameters
      pure (MUrgency n)
    "i" -> do
      b <- ($(char '=') *> sfBoolean) <|> pure True
      _ <- rfc8941Parameters
      pure (MIncremental b)
    _ -> do
      _ <- optional ($(char '=') *> rfc8941ItemValue)
      _ <- rfc8941Parameters
      pure MOther


-- | An RFC 8941 Dictionary @member-key@: @(lcalpha \/ "*") *(lcalpha \/ DIGIT \/ "_-.\*")@.
memberKey :: ParserT st e ST.ShortText
memberKey =
  shortASCIIFromParser_
    ( skipSatisfyAscii (`CharSet.member` firstKeyChar)
        *> skipMany (skipSatisfyAscii (`CharSet.member` keyChar))
    )
  where
    lcalpha = CharSet.range 'a' 'z'
    firstKeyChar = lcalpha <> "*"
    keyChar = lcalpha <> digit <> "_-.*"


-- | An RFC 8941 @sf-boolean@ payload, with RFC 9218's truth mapping (@?1@ = true).
sfBoolean :: ParserT st e Bool
sfBoolean = $(char '?') *> ((True <$ $(char '1')) <|> (False <$ $(char '0')))


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderPriority :: Priority -> M.Builder
renderPriority (Priority u inc) =
  "u=" <> R.rfc8941Integer u <> (if inc then ", i" else mempty)
