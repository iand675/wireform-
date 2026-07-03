{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Typed events.

An 'EventVal' is a named event carrying its /typed/ payload — @mkEvent
\@\"FETCH\" url@ only compiles if the chart declares a @\"FETCH\"@ event
and the payload has the declared type, and it recovers that exact type by
name ('matchEvent' \/ 'onEvent'). No serialization is involved: the value
is the real Haskell value, start to finish.

The step algorithm additionally processes lifecycle events — done, timers,
invoke results. Those carry values born at a service or output producer
(where the type is statically known) but consumed after a runtime-keyed
queue, so they travel as 'Data.Dynamic.Dynamic' and are recovered by the
consumer's expected type ('invokeOutput' \/ 'invokeError' \/ 'doneData',
each @'Data.Typeable.Typeable' a => … -> 'Maybe' a@ — the 'Maybe' mirrors
'onEvent'). Still no JSON.

The only JSON this module touches is 'decodeEvent' \/ 'EventCodec':
decoding a named event that arrived /from outside/ (a wire message, an
untyped host). That is a deliberate external-input boundary, not part of
the internal typed path.
-}
module StateMachine.Event (
  -- * Typed events
  EventVal (..),
  mkEvent,
  mkEvent_,
  eventName,
  matchEvent,

  -- * Step events (what guards and actions see)
  StepEvent (..),
  stepEventKey,
  stepEventLabel,
  onEvent,
  invokeOutput,
  invokeError,
  doneData,

  -- * External-input boundary (JSON)
  DecodeEvents (..),
  EventCodec,
  decodeEvent,
) where

import Data.Aeson (FromJSON, Value)
import Data.Aeson.Types qualified as Aeson
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)

import StateMachine.Runtime (EventKey (..), NodeName, TimerKey (..))
import StateMachine.Spec (
  ChartEvents,
  ChartSpec,
  EventPayload,
  EventSpec (..),
  HasEvent,
 )

{-------------------------------------------------------------------------------
  Typed events
-------------------------------------------------------------------------------}

-- | A declared event of chart @spec@, carrying its typed payload. The
-- 'Show' constraint is for tracing/debugging only — the payload itself is
-- never serialized on the typed path.
data EventVal (spec :: ChartSpec) where
  EventVal ::
    ( KnownSymbol e
    , HasEvent spec e
    , Show (EventPayload spec e)
    ) =>
    Proxy e ->
    EventPayload spec e ->
    EventVal spec

instance Show (EventVal spec) where
  showsPrec d (EventVal p payload) =
    showParen (d > 10) $
      showString "EventVal "
        . shows (T.pack (symbolVal p))
        . showString " "
        . showsPrec 11 payload

-- | Construct an event: @mkEvent \@\"FETCH\" url@. Compiles only if the
-- chart declares the event with that payload type.
mkEvent ::
  forall e spec.
  (KnownSymbol e, HasEvent spec e, Show (EventPayload spec e)) =>
  EventPayload spec e ->
  EventVal spec
mkEvent = EventVal (Proxy @e)

-- | Construct a payload-less event: @mkEvent_ \@\"CANCEL\"@.
mkEvent_ ::
  forall e spec.
  (KnownSymbol e, HasEvent spec e, Show (EventPayload spec e), EventPayload spec e ~ ()) =>
  EventVal spec
mkEvent_ = mkEvent @e ()

eventName :: EventVal spec -> Text
eventName (EventVal p _) = T.pack (symbolVal p)

-- | Typed projection: @matchEvent \@\"FETCH\" ev@ returns the payload when
-- the event is a @\"FETCH\"@. State names are unique, so name equality
-- proves payload-type equality.
matchEvent ::
  forall e spec.
  (KnownSymbol e) =>
  EventVal spec ->
  Maybe (EventPayload spec e)
matchEvent (EventVal (p :: Proxy e') payload) =
  case sameSymbol (Proxy @e) p of
    Just Refl -> Just payload
    Nothing -> Nothing

{-------------------------------------------------------------------------------
  Step events
-------------------------------------------------------------------------------}

{- | Every kind of event the step algorithm processes. Guards, actions,
and output producers receive the full 'StepEvent', so an @onDone@
transition's action can read the invocation's output, etc.
-}
data StepEvent (spec :: ChartSpec)
  = -- | A declared event, external or raised.
    EvExternal (EventVal spec)
  | -- | @done.state.{node}@ with the final state's done-data (typed,
    -- boxed at the output producer; recover with 'doneData').
    EvDone NodeName (Maybe Dynamic)
  | -- | The invocation resolved with this output (typed, boxed at the
    -- service; recover with 'invokeOutput').
    EvInvokeDone Text Dynamic
  | -- | The invocation failed with this error value (recover with
    -- 'invokeError').
    EvInvokeError Text Dynamic
  | -- | A delay elapsed.
    EvTimer TimerKey
  | -- | The pseudo-event driving chart initialization (what entry actions
    -- of initial states observe).
    EvInit
  deriving stock (Show)

-- | The identity the transition-selection algorithm matches on.
stepEventKey :: StepEvent spec -> EventKey
stepEventKey = \case
  EvExternal ev -> KNamed (eventName ev)
  EvDone n _ -> KDone n
  EvInvokeDone i _ -> KInvokeDone i
  EvInvokeError i _ -> KInvokeError i
  EvTimer k -> KTimer k
  EvInit -> KInit

-- | A short human-readable label for traces and debugging. Payloads are
-- shown via their 'Show' instance (events) or 'Data.Dynamic' 'TypeRep'
-- (lifecycle values) — never serialized.
stepEventLabel :: StepEvent spec -> Text
stepEventLabel = \case
  EvExternal ev -> T.pack (show ev)
  EvDone n d -> "done.state." <> n <> maybe "" (\v -> " " <> T.pack (show v)) d
  EvInvokeDone i v -> "done.invoke." <> i <> " " <> T.pack (show v)
  EvInvokeError i v -> "error.invoke." <> i <> " " <> T.pack (show v)
  EvTimer (TimerKey n ms ix) ->
    "after." <> T.pack (show ms) <> "." <> n <> "#" <> T.pack (show ix)
  EvInit -> "#init"

-- | Typed projection for guards\/actions: the payload of a named event,
-- at its declared type — @onEvent \@\"FETCH\" ev :: Maybe Url@.
onEvent ::
  forall e spec.
  (KnownSymbol e) =>
  StepEvent spec ->
  Maybe (EventPayload spec e)
onEvent = \case
  EvExternal ev -> matchEvent @e ev
  _ -> Nothing

-- | The output of a resolved invocation, at the type you expect —
-- @invokeOutput ev :: Maybe UserRecord@. Recovers the real value the
-- service produced ('Nothing' if this step was not an @onDone@, or the
-- expected type does not match what the service returned).
invokeOutput :: (Typeable a) => StepEvent spec -> Maybe a
invokeOutput = \case
  EvInvokeDone _ v -> fromDynamic v
  _ -> Nothing

-- | The error value of a failed invocation, at the type you expect.
invokeError :: (Typeable a) => StepEvent spec -> Maybe a
invokeError = \case
  EvInvokeError _ v -> fromDynamic v
  _ -> Nothing

-- | The done-data of a completed compound\/parallel state, at the type
-- its output producer emitted.
doneData :: (Typeable a) => StepEvent spec -> Maybe a
doneData = \case
  EvDone _ (Just v) -> fromDynamic v
  _ -> Nothing

{-------------------------------------------------------------------------------
  External-input boundary (JSON)
-------------------------------------------------------------------------------}

{- | Walks the chart's declared event list to build the name-indexed
decoder table used when an event arrives /from outside/ as JSON (a wire
message, an untyped host). This is the one place the library parses JSON
into a typed event; the internal path never does. Satisfied for any chart
whose event payloads are 'FromJSON' (and 'Show', for the typed value).
-}
class DecodeEvents (spec :: ChartSpec) (evs :: [EventSpec]) where
  eventDecoders :: Map Text (Value -> Either String (EventVal spec))

instance DecodeEvents spec '[] where
  eventDecoders = Map.empty

instance
  ( KnownSymbol e
  , HasEvent spec e
  , EventPayload spec e ~ p
  , FromJSON p
  , Show p
  , DecodeEvents spec rest
  ) =>
  DecodeEvents spec ('MkEvent e p ': rest)
  where
  eventDecoders =
    Map.insert
      (T.pack (symbolVal (Proxy @e)))
      (fmap (EventVal (Proxy @e)) . Aeson.parseEither Aeson.parseJSON)
      (eventDecoders @spec @rest)

-- | The constraint a chart needs to decode events arriving from outside.
type EventCodec spec = DecodeEvents spec (ChartEvents spec)

-- | Decode a named event that arrived from outside, as JSON. 'Left'
-- explains an unknown name (with the declared names) or a payload parse
-- failure. This is the deliberate external-input boundary — internal
-- routing (self\/child\/parent sends) uses typed 'EventVal's, not this.
decodeEvent ::
  forall spec.
  (EventCodec spec) =>
  Text ->
  Value ->
  Either String (EventVal spec)
decodeEvent name payload =
  case Map.lookup name table of
    Nothing ->
      Left $
        "unknown event "
          <> show name
          <> "; declared events: "
          <> show (Map.keys table)
    Just dec -> case dec payload of
      Left err -> Left ("payload of " <> show name <> " does not parse: " <> err)
      Right ev -> Right ev
 where
  table = eventDecoders @spec @(ChartEvents spec)
