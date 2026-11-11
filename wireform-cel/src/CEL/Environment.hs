{- | The evaluation environment: variable bindings, the resolution container
(protobuf package scope), extension functions, and message constructors.
-}
module CEL.Environment (
  Env (Env, envVars, envLocals, envContainer, envFuncs),
  Overload,
  MessageConstructor,
  AnyResolver,
  emptyEnv,
  bind,
  bindAll,
  bindLocal,
  withContainer,
  addFunction,
  addMessageType,
  addAnyResolver,
  lookupVar,
  lookupLocal,
  lookupFunction,
  lookupMessageType,
  lookupAnyResolver,
) where

import CEL.Error (CelError)
import CEL.Value (CelField, Value)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)


{- | A user function overload: given the evaluated arguments, either decline
(@Nothing@, so another overload / a standard function may apply) or produce
a result / error.
-}
type Overload = [Value] -> Maybe (Either CelError Value)


-- | Build the fields of a protobuf message from its supplied initializers.
-- The constructor validates names and types and provides a selection default
-- for every absent field. CEL sets the message's fully qualified type name.
type MessageConstructor = Map Text Value -> Either CelError (Map Text CelField)


{- | Decode a protobuf @Any@ payload to its CEL representation. The resolver
receives the type URL from the message and uses the application's type registry.
-}
type AnyResolver = Text -> ByteString -> Either CelError Value


data ProtoEnv = ProtoEnv
  { messageTypes :: !(Map Text MessageConstructor)
  , anyResolver :: !(Maybe AnyResolver)
  }


-- | An evaluation environment.
data Env
  = Env
      { envVars :: !(Map Text Value)
      -- ^ Bound variables keyed by their (possibly qualified) name.
      , envLocals :: !(Map Text Value)
      -- ^ Comprehension-local variables, resolved before package qualification.
      , envContainer :: !Text
      -- ^ The package used for relative resolution. Empty for the root scope.
      , envFuncs :: !(Map Text [Overload])
      -- ^ User-supplied extension functions, tried before the standard library.
      }
  | EnvWithProtobuf
      { envVars :: !(Map Text Value)
      , envLocals :: !(Map Text Value)
      , envContainer :: !Text
      , envFuncs :: !(Map Text [Overload])
      , envProtobuf :: !ProtoEnv
      }


-- | An environment with no bindings, no container, and no extension functions.
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty "" Map.empty


-- | Bind a single variable.
bind :: Text -> Value -> Env -> Env
bind k v env = env {envVars = Map.insert k v (envVars env)}


-- | Bind several variables at once.
bindAll :: [(Text, Value)] -> Env -> Env
bindAll kvs env = env {envVars = Map.union (Map.fromList kvs) (envVars env)}


{- | Bind a comprehension-local variable. Locals shadow both globals and the
package namespace when resolving simple names.
-}
bindLocal :: Text -> Value -> Env -> Env
bindLocal k v env = env {envLocals = Map.insert k v (envLocals env)}


-- | Set the resolution container (protobuf package scope).
withContainer :: Text -> Env -> Env
withContainer c env = env {envContainer = c}


-- | Register an extension function overload under a name.
addFunction :: Text -> Overload -> Env -> Env
addFunction name ov env =
  env {envFuncs = Map.insertWith (++) name [ov] (envFuncs env)}


-- | Register a message type and its constructor under a fully qualified name.
addMessageType :: Text -> MessageConstructor -> Env -> Env
addMessageType name construct (Env vars locals container functions) =
  EnvWithProtobuf vars locals container functions (ProtoEnv (Map.singleton name construct) Nothing)
addMessageType name construct env@EnvWithProtobuf {envProtobuf = proto} =
  env {envProtobuf = proto {messageTypes = Map.insert name construct (messageTypes proto)}}


-- | Look up a message constructor by exact name.
lookupMessageType :: Text -> Env -> Maybe MessageConstructor
lookupMessageType name EnvWithProtobuf {envProtobuf = proto} = Map.lookup name (messageTypes proto)
lookupMessageType _ Env {} = Nothing


-- | Set the resolver for @google.protobuf.Any@ literals.
addAnyResolver :: AnyResolver -> Env -> Env
addAnyResolver resolver (Env vars locals container functions) =
  EnvWithProtobuf vars locals container functions (ProtoEnv Map.empty (Just resolver))
addAnyResolver resolver env@EnvWithProtobuf {envProtobuf = proto} =
  env {envProtobuf = proto {anyResolver = Just resolver}}


lookupAnyResolver :: Env -> Maybe AnyResolver
lookupAnyResolver EnvWithProtobuf {envProtobuf = proto} = anyResolver proto
lookupAnyResolver Env {} = Nothing


-- | Look up a bound variable by exact name.
lookupVar :: Text -> Env -> Maybe Value
lookupVar k env = Map.lookup k (envVars env)


-- | Look up a comprehension-local variable by exact name.
lookupLocal :: Text -> Env -> Maybe Value
lookupLocal k env = Map.lookup k (envLocals env)


-- | Try the registered extension functions for a name against the arguments.
lookupFunction :: Text -> [Value] -> Env -> Maybe (Either CelError Value)
lookupFunction name args env =
  case Map.lookup name (envFuncs env) of
    Nothing -> Nothing
    Just ovs -> firstJust (map ($ args) ovs)
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : rest) = firstJust rest
