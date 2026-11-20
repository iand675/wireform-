{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Urgency request header (RFC 8030 §5.3): tells the push service how
important a message is to the user agent, letting it conserve resources on
constrained devices by delaying or dropping low-priority messages. Defined
values are @very-low@, @low@, @normal@, and @high@. A push message without this
field has 'Normal' urgency. A monitoring request can use it as a minimum
urgency, or omit it to receive messages of every urgency.

Spec: <https://www.rfc-editor.org/rfc/rfc8030#section-5.3>

See also: "Network.HTTP.Headers.TTL", "Network.HTTP.Headers.Topic",
"Network.HTTP.Headers.Priority".
-}
module Network.HTTP.Headers.Urgency (
  Urgency (..),
  urgencyParser,
  renderUrgency,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hUrgency)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The relative importance of a push message (RFC 8030 §5.3).
data Urgency
  = VeryLow
  | Low
  | Normal
  | High
  deriving stock (Eq, Show, Enum, Bounded)


instance KnownHeader Urgency where
  type ParseFailure Urgency = String
  type Cardinality Urgency = 'ZeroOrOne
  type Direction Urgency = 'Request


  parseFromHeaders _ ("very-low" :| []) = Right VeryLow
  parseFromHeaders _ ("low" :| []) = Right Low
  parseFromHeaders _ ("normal" :| []) = Right Normal
  parseFromHeaders _ ("high" :| []) = Right High
  parseFromHeaders _ fields = parseSingleFieldLine "Urgency" urgencyParser fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ = urgencyBytes
  {-# INLINE renderToHeaders #-}


  headerName _ = hUrgency


urgencyParser :: ParserT st String Urgency
urgencyParser =
  $( switchAsciiCI
       [|
         case _ of
           "very-low" -> pure VeryLow
           "low" -> pure Low
           "normal" -> pure Normal
           "high" -> pure High
         |]
   )


renderUrgency :: Urgency -> M.Builder
renderUrgency = M.byteString . urgencyBytes


urgencyBytes :: Urgency -> ByteString
urgencyBytes = \case
  VeryLow -> "very-low"
  Low -> "low"
  Normal -> "normal"
  High -> "high"
{-# INLINE urgencyBytes #-}
