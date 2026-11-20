{- |
RFC 9218 — the HTTP @Priority@ header field, used by clients to signal,
and by servers to (re)assign, the relative priority of responses on a
connection. It carries a Structured Fields dictionary.

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

Receivers ignore unknown members, parameters, and values outside the defined
type or range. The last member of a repeated key determines its value.

Spec: <https://www.rfc-editor.org/rfc/rfc9218.html>

See also: "Network.HTTP.Headers.Urgency", "Network.HTTP.Headers.SecPurpose".
-}
module Network.HTTP.Headers.Priority (
  Priority (..),
  priorityParser,
  renderPriority,
) where

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


  parseFromHeaders _ = fmap fromDictionary . parseStructuredFieldDictionary "Priority"


  renderToHeaders _ = M.toStrictByteString . renderPriority


  headerName _ = hPriority


priorityParser :: ParserT st String Priority
priorityParser = fromDictionary <$> structuredFieldDictionary


fromDictionary :: [StructuredFieldDictionaryEntry] -> Priority
fromDictionary = foldl' applyMember defaultPriority
  where
    applyMember priority (StructuredFieldDictionaryEntry key (StructuredItem (StructuredFieldItem value _))) =
      case (key, value) of
        ("u", Integer n) | n >= 0 && n <= 7 -> priority {priorityUrgency = n}
        ("i", Boolean b) -> priority {priorityIncremental = b}
        _ -> priority
    applyMember priority _ = priority


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderPriority :: Priority -> M.Builder
renderPriority (Priority u inc) =
  "u=" <> R.rfc8941Integer urgency <> (if inc then ", i" else mempty)
  where
    urgency
      | u >= 0 && u <= 7 = u
      | otherwise = priorityUrgency defaultPriority
