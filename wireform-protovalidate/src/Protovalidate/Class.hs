{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{- | A compile-once, typed validation path that avoids the dynamic-message
round trip.

'compileValidator' turns a 'MessageRules' into a 'Validator' that captures
the (already CEL-compiled) constraints and a reusable environment. The same
'Validator' can then validate many messages with no per-call compilation.

'ToCel' converts a typed Haskell value (e.g. a @wireform-proto@ generated
message record) directly into the CEL value the engine consumes — no
schemaless 'Proto.Dynamic.DynamicMessage', no wire re-decode. A record type
can derive it via "GHC.Generics":

@
data User = User { id_ :: Text, age :: Word32, email :: Text }
  deriving stock (Generic)
  deriving anyclass (ToCel)

userValidator :: Validator
userValidator = compileValidator userRules

check :: User -> [Violation]
check = validateValue userValidator
@
-}
module Protovalidate.Class (
  -- * Compiled validators
  Validator,
  compileValidator,
  compileValidatorIn,
  runValidator,
  validateValue,

  -- * Typed conversion to CEL
  ToCel (..),
  genericToCel,
  GToCelRecord,
) where

import CEL.Environment (Env)
import CEL.Value (Value (..), celMapFromList)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (float2Double)
import GHC.Generics
import Protovalidate.Eval (ConstraintProgram, MessagePlan, compileValidationPlan, runValidationPlan)
import Protovalidate.Library (libraryEnv)
import Protovalidate.Rules (MessageRules)
import Protovalidate.Violation (Violation)


{- | A validator compiled from a 'MessageRules': the constraints' CEL
expressions are already compiled, and the base environment (with the
protovalidate library) is captured once.
-}
data Validator = Validator !Env !(MessagePlan ConstraintProgram)


-- | Compile a validator using the standard protovalidate CEL environment.
compileValidator :: MessageRules -> Validator
compileValidator = compileValidatorIn libraryEnv


-- | Compile a validator with a caller-supplied base environment.
compileValidatorIn :: Env -> MessageRules -> Validator
compileValidatorIn env rules = Validator env (compileValidationPlan rules)


-- | Run a compiled validator against a message already in CEL form.
runValidator :: Validator -> Value -> [Violation]
runValidator (Validator env plan) = runValidationPlan env plan


-- | Validate a typed value by converting it directly to CEL via 'ToCel'.
validateValue :: ToCel a => Validator -> a -> [Violation]
validateValue val = runValidator val . toCel


----------------------------------------------------------------------
-- ToCel
----------------------------------------------------------------------

{- | Convert a typed Haskell value into a CEL 'Value'. Records get a 'VMap'
keyed by field name; the 'Generic' default handles single-constructor
records automatically.
-}
class ToCel a where
  toCel :: a -> Value
  default toCel :: (Generic a, GToCelRecord (Rep a)) => a -> Value
  toCel = genericToCel

  -- | Field presence when converting a generic record.
  toCelField :: a -> Maybe Value
  toCelField = Just . toCel


instance ToCel Value where toCel = id


instance ToCel Bool where toCel = VBool


instance ToCel Int where toCel = VInt . fromIntegral


instance ToCel Int8 where toCel = VInt . fromIntegral


instance ToCel Int16 where toCel = VInt . fromIntegral


instance ToCel Int32 where toCel = VInt . fromIntegral


instance ToCel Int64 where toCel = VInt


instance ToCel Word where toCel = VUInt . fromIntegral


instance ToCel Word8 where toCel = VUInt . fromIntegral


instance ToCel Word16 where toCel = VUInt . fromIntegral


instance ToCel Word32 where toCel = VUInt . fromIntegral


instance ToCel Word64 where toCel = VUInt


instance ToCel Float where toCel = VDouble . float2Double


instance ToCel Double where toCel = VDouble


instance ToCel Text where toCel = VString


instance ToCel ByteString where toCel = VBytes


instance ToCel a => ToCel (Maybe a) where
  toCel Nothing = VNull
  toCel (Just a) = toCel a
  toCelField = fmap toCel


instance ToCel a => ToCel [a] where
  toCel = VList . V.fromList . map toCel


instance ToCel a => ToCel (Vector a) where
  toCel = VList . V.map toCel


instance ToCel v => ToCel (Map Text v) where
  toCel m = VMap (celMapFromList [(VString k, toCel v) | (k, v) <- Map.toList m])


-- | Build a CEL map from a record's selector names and field values.
genericToCel :: (Generic a, GToCelRecord (Rep a)) => a -> Value
genericToCel x = VMap (celMapFromList [(VString k, v) | (k, v) <- gToCelRecord (from x)])


-- | Generic helper: collect @(fieldName, celValue)@ pairs from a record.
class GToCelRecord f where
  gToCelRecord :: f p -> [(Text, Value)]


instance GToCelRecord f => GToCelRecord (D1 c f) where
  gToCelRecord (M1 x) = gToCelRecord x


instance GToCelRecord f => GToCelRecord (C1 c f) where
  gToCelRecord (M1 x) = gToCelRecord x


instance (GToCelRecord f, GToCelRecord g) => GToCelRecord (f :*: g) where
  gToCelRecord (a :*: b) = gToCelRecord a ++ gToCelRecord b


instance GToCelRecord U1 where
  gToCelRecord _ = []


instance (Selector s, ToCel a) => GToCelRecord (S1 s (K1 i a)) where
  gToCelRecord m@(M1 (K1 a)) = maybe [] (\v -> [(T.pack (selName m), v)]) (toCelField a)
