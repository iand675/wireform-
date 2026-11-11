{-# LANGUAGE DeriveLift #-}

{- | Runtime and compile-time errors produced by the CEL implementation.

The CEL language definition formally names two runtime errors,
@no_matching_overload@ and @no_such_field@; everything else (division by
zero, overflow, conversion failures, …) is simply "an error" that
terminates evaluation unless absorbed by a logical operator or macro. The
'ErrKind' classification lets callers and tests distinguish the named errors
while 'errMsg' carries a human-readable description.
-}
module CEL.Error (
  CelError (..),
  ErrKind (..),
  noOverload,
  noSuchField,
  noSuchKey,
  undeclared,
  divByZero,
  overflow,
  conversion,
  invalidArg,
  unsupported,
  parseErr,
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)


-- | A coarse classification of errors.
data ErrKind
  = ErrNoOverload
  | ErrNoSuchField
  | ErrNoSuchKey
  | ErrUndeclared
  | ErrDivByZero
  | ErrOverflow
  | ErrConversion
  | ErrInvalid
  | ErrUnsupported
  | ErrParse
  deriving stock (Eq, Show, Generic, Lift)


instance NFData ErrKind


-- | An error value with a classification and message.
data CelError = CelError
  { errKind :: !ErrKind
  , errMsg :: !Text
  }
  deriving stock (Eq, Show, Generic)


instance NFData CelError


noOverload :: Text -> CelError
noOverload n = CelError ErrNoOverload ("no matching overload for '" <> n <> "'")


noSuchField :: Text -> CelError
noSuchField f = CelError ErrNoSuchField ("no such field '" <> f <> "'")


noSuchKey :: Text -> CelError
noSuchKey k = CelError ErrNoSuchKey ("no such key: " <> k)


undeclared :: Text -> CelError
undeclared n = CelError ErrUndeclared ("undeclared reference to '" <> n <> "'")


divByZero :: Text -> CelError
divByZero = CelError ErrDivByZero


overflow :: Text -> CelError
overflow ctx = CelError ErrOverflow (ctx <> " overflow")


conversion :: Text -> CelError
conversion = CelError ErrConversion


invalidArg :: Text -> CelError
invalidArg = CelError ErrInvalid


unsupported :: Text -> CelError
unsupported = CelError ErrUnsupported


parseErr :: Text -> CelError
parseErr = CelError ErrParse
