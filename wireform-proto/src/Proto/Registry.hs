{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Runtime type registry for proto messages.

A 'TypeRegistry' maps fully-qualified proto type names to typed decoders
and JSON codecs. It is passed explicitly rather than stored in global
mutable state.

== The 'IsMessage' marker

'IsMessage' is a marker class — it has no methods of its own. Its job is
to bundle together every typeclass a message needs in order to be
'register'ed into a 'TypeRegistry':

* 'MessageEncode' \/ 'MessageDecode' — wire codec.
* 'ProtoMessage' — the FQN, package, and schema metadata.
* 'Aeson.ToJSON' \/ 'Aeson.FromJSON' — JSON codec (proto3 canonical).
* 'Typeable' — runtime type identity for 'lookupDecoder' downcasting.

Generated message types ship with an empty @instance IsMessage Foo@
declaration that says "all of the above superclasses are in scope". The
generated code does not need to mention any of the superclasses
individually; the marker instance is the contract.

== Discovering instances

The 'discoverRegistry' splice walks every 'IsMessage' instance visible
in the current module and emits a 'TypeRegistry' containing them all,
plus the standard well-known type codecs (Timestamp, Duration, Any,
Struct, FieldMask, Wrappers, etc.) so you don't need to wire those up
manually.

@
{\-\# LANGUAGE TemplateHaskell \#-\}
import Proto.Registry (TypeRegistry, discoverRegistry)
-- import every module whose messages should be in the registry

myRegistry :: TypeRegistry
myRegistry = $$discoverRegistry
@

Caveat: TH only sees instances that have already been compiled when the
splice runs. Instances defined in the same module as @$$discoverRegistry@
will not be picked up. Put the splice in a leaf module that imports the
message modules.
-}
module Proto.Registry (
  -- * Marker class
  IsMessage,

  -- * Registry type
  TypeRegistry (..),
  emptyRegistry,

  -- * Registration
  registerMessage,
  registerCodec,

  -- * Process-global Any registry (for runtime-extensible Any JSON)
  globalAnyRegistry,
  registerGlobalAnyCodecs,
  readGlobalAnyRegistry,

  -- * Lookup
  lookupCodec,
  lookupDecoder,

  -- * JSON codec for Any
  AnyCodec (..),

  -- * Typed decoder (existential)
  SomeDecoder (..),

  -- * Template Haskell discovery
  discoverRegistry,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Language.Haskell.TH (Dec (..), Exp (..), Info (..), Name, Q, Type (..), mkName, reify)
import Language.Haskell.TH.Syntax (Code, unsafeCodeCoerce)
import Proto.Decode (DecodeError, MessageDecode (..), decodeMessage)
import Proto.Encode (MessageEncode (..), encodeMessage)
import Proto.Schema (ProtoMessage (..))


{- | Marker class bundling every typeclass a proto message must provide
to be eligible for inclusion in a 'TypeRegistry'.

The class has no methods of its own. Generated code emits an empty
@instance IsMessage Foo@; GHC then checks that each superclass
instance is in scope.
-}
class
  ( MessageEncode a
  , MessageDecode a
  , ProtoMessage a
  , Aeson.ToJSON a
  , Aeson.FromJSON a
  , Typeable a
  ) =>
  IsMessage a


-- | A JSON codec for a message type, used for Any field serialisation.
data AnyCodec = AnyCodec
  { acToJSON :: !(ByteString -> Either String Aeson.Value)
  -- ^ Decode wire bytes to a JSON value.
  , acFromJSON :: !(Aeson.Value -> Either String ByteString)
  -- ^ Encode a JSON value to wire bytes.
  , acIsWkt :: !Bool
  {- ^ True for well-known types that use the
  @{"\@type": …, "value": …}@ envelope.
  -}
  }


-- | An existential typed decoder, allowing type-safe recovery via 'Typeable'.
data SomeDecoder where
  SomeDecoder :: (Typeable a, IsMessage a) => Proxy a -> SomeDecoder


-- | Registry mapping proto type names to codecs and typed decoders.
data TypeRegistry = TypeRegistry
  { trCodecs :: !(Map Text AnyCodec)
  , trDecoders :: !(Map Text SomeDecoder)
  }


instance Semigroup TypeRegistry where
  a <> b =
    TypeRegistry
      { trCodecs = trCodecs a <> trCodecs b
      , trDecoders = trDecoders a <> trDecoders b
      }


instance Monoid TypeRegistry where
  mempty = emptyRegistry


-- | The empty registry.
emptyRegistry :: TypeRegistry
emptyRegistry = TypeRegistry Map.empty Map.empty


{- | Register a message type with both a JSON codec and a typed decoder.
The type must have an 'IsMessage' instance (which bundles all the
needed superclasses).
-}
registerMessage
  :: forall a
   . IsMessage a
  => Proxy a
  -> TypeRegistry
  -> TypeRegistry
registerMessage p reg =
  let name = protoMessageName p
      codec =
        AnyCodec
          { acToJSON = \bs -> case decodeMessage bs of
              Left e -> Left (show e)
              Right (v :: a) -> Right (Aeson.toJSON v)
          , acFromJSON = \val -> case Aeson.fromJSON val of
              Aeson.Error e -> Left e
              Aeson.Success (v :: a) -> Right (encodeMessage v)
          , acIsWkt = False
          }
  in reg
       { trCodecs = Map.insert name codec (trCodecs reg)
       , trDecoders = Map.insert name (SomeDecoder p) (trDecoders reg)
       }


{- | Register a raw JSON codec (for well-known types with custom JSON
representations that don't round-trip through 'Aeson.ToJSON' /
'Aeson.FromJSON').
-}
registerCodec :: Text -> AnyCodec -> TypeRegistry -> TypeRegistry
registerCodec name codec reg =
  reg {trCodecs = Map.insert name codec (trCodecs reg)}


{- | A process-global 'TypeRegistry' for runtime-extensible @Any@ JSON.

Generated code serialises @Any@-typed fields with a registry that
unions the standard well-known types with whatever has been registered
here (see 'Proto.Internal.JSON.WellKnown.anyJsonRegistry'). An
application that handles @Any@ values wrapping its own message types —
e.g. a Connect/gRPC conformance harness echoing requests packed into
@google.protobuf.Any@ — registers those types once at startup via
'registerGlobalAnyCodecs' so the canonical @{"\@type": …, …inlined…}@
JSON form is produced instead of the degenerate @{"value": <base64>}@
fallback.

This is a deliberately small piece of process-global state: it is
written once during initialisation and only read thereafter. Empty by
default, so libraries that never register anything are unaffected (the
@Any@ registry is exactly 'standardWktRegistry').
-}
{-# NOINLINE globalAnyRegistry #-}
globalAnyRegistry :: IORef TypeRegistry
globalAnyRegistry = unsafePerformIO (newIORef emptyRegistry)


{- | Merge additional codecs into the process-global 'Any' registry.
Call once at process startup, before any @Any@ value is (de)serialised.
-}
registerGlobalAnyCodecs :: TypeRegistry -> IO ()
registerGlobalAnyCodecs reg =
  atomicModifyIORef' globalAnyRegistry (\g -> (g <> reg, ()))


-- | Read the current process-global 'Any' registry.
readGlobalAnyRegistry :: IO TypeRegistry
readGlobalAnyRegistry = readIORef globalAnyRegistry


-- | Look up a JSON codec by proto type name.
lookupCodec :: Text -> TypeRegistry -> Maybe AnyCodec
lookupCodec name = Map.lookup name . trCodecs


{- | Look up a typed decoder by proto type name. Returns a decode
function when the requested Haskell type matches the registered type.
-}
lookupDecoder
  :: forall a
   . (Typeable a, IsMessage a)
  => Text
  -> TypeRegistry
  -> Maybe (ByteString -> Either DecodeError a)
lookupDecoder name reg = do
  SomeDecoder (_ :: Proxy b) <- Map.lookup name (trDecoders reg)
  cast (decodeMessage :: ByteString -> Either DecodeError b)


-- ---------------------------------------------------------------------------
-- Template Haskell discovery
-- ---------------------------------------------------------------------------

{- | Typed Template Haskell splice that builds a 'TypeRegistry' containing:

* Every 'IsMessage' instance visible at the splice site.
* The standard well-known type codecs (Timestamp, Duration, Any, Struct,
  FieldMask, Wrappers, Empty, SourceContext) with their canonical JSON
  handling.

Usage (note the @$$@ for a typed splice):

@
import Proto.Registry (TypeRegistry, discoverRegistry)

myRegistry :: TypeRegistry
myRegistry = $$discoverRegistry
@

You don't need to import the WKT modules for their instances — they're
baked in automatically. Just import the modules whose generated message
types you want in the registry.

Caveat: TH only sees instances that GHC has already compiled before
the splice runs. Same-module instances are invisible. Put the splice
in a leaf module that imports the message modules whose instances
should be registered.

Implementation note: 'reify' is untyped 'Q', so we build an untyped
expression and seal it as @'Code' Q 'TypeRegistry'@ via
'unsafeCodeCoerce'. The \"unsafe\" is bounded: the only thing it
defers to splice time is the @'Code' Q 'TypeRegistry'@ vs. actual-shape
check, which GHC enforces when the spliced expression is type-checked
in context.
-}
discoverRegistry :: Code Q TypeRegistry
discoverRegistry = unsafeCodeCoerce $ do
  info <- reify ''IsMessage
  let insts = case info of
        ClassI _ is -> is
        _ -> []
  let types = mapMaybe instanceHeadType insts
  -- Start from the standard WKT registry so well-known types
  -- (Timestamp, Duration, Any, Struct, FieldMask, Wrappers, etc.)
  -- are always included with their canonical JSON codecs.
  let wktBase = VarE (mkName "Proto.Internal.JSON.WellKnown.standardWktRegistry")
  foldr step (pure wktBase) types
  where
    step ty acc =
      [|registerMessage (Proxy :: Proxy $(pure ty)) $acc|]


-- | Extract the type from an @instance IsMessage T@ declaration.
instanceHeadType :: Dec -> Maybe Type
instanceHeadType (InstanceD _ _ (AppT _cls ty) _) = Just ty
instanceHeadType _ = Nothing
