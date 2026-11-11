{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Generate a protobuf message validator __at compile time__, with every
rule's CEL compiled to Haskell.

'compileMessageValidator' reads a @.proto@'s @buf.validate@ rules at compile
time and emits a @'Value' -> ['Violation']@ function. It uses the same rule
selection, bindings, and traversal as "Protovalidate.Eval" and compiles
each constraint's parsed CEL AST into Haskell. Evaluation requires no
runtime parsing or AST traversal.

@
{\-# LANGUAGE TemplateHaskell #-\}
import Protovalidate
import Protovalidate.TH (compileMessageValidator)
import MyProtoSource (userProto)   -- a separate module (stage restriction)

validateUser :: Value -> [Violation]
validateUser = $(compileMessageValidator userProto \"User\")
@

-}
module Protovalidate.TH (
  compileMessageValidator,
  messageValidatorE,
  runCompiledValidator,
  CompiledConstraint,
) where

import CEL.Environment (bind)
import CEL.Error (CelError (..))
import CEL.Eval (Compiled)
import CEL.TH (compileCelExprFn)
import CEL.Value (AnyPayload (..), CelField (..), CelMessage (..), Duration (..), Timestamp (..), Value (..), celMapEntries, celMapFromList, celMapLookup, celMessageUnknownFields, celMessageWithUnknownFields)
import Data.Map.Lazy qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Language.Haskell.TH (Exp, Q, listE)
import Language.Haskell.TH.Syntax (lift)
import Protovalidate.Constraint (Constraint (..))
import Protovalidate.Eval
import Protovalidate.Library (libraryEnv)
import Protovalidate.Rules
import Protovalidate.Schema (parseProtoRules)
import Protovalidate.Violation (Violation (..))


{- | A compiled constraint: its id, fallback message, and the CEL program
compiled to Haskell.
-}
type CompiledConstraint = (Text, Text, Compiled)


----------------------------------------------------------------------
-- Compile-time generation
----------------------------------------------------------------------

{- | Parse a @.proto@ at compile time and emit a compiled validator
(@'Value' -> ['Violation']@) for the named message.
-}
compileMessageValidator :: Text -> Text -> Q Exp
compileMessageValidator protoSrc name = case parseProtoRules protoSrc of
  Left err -> fail ("Protovalidate.TH: " <> T.unpack err)
  Right rs -> case lookup name rs of
    Nothing -> fail ("Protovalidate.TH: no message named " <> T.unpack name)
    Just mr -> messageValidatorE mr


-- | Emit a compiled validator for an already-known 'MessageRules'.
messageValidatorE :: MessageRules -> Q Exp
messageValidatorE rules =
  [|runValidationPlan libraryEnv $(emitMessage (prepareValidation id rules))|]


emitMessage :: MessagePlan Constraint -> Q Exp
emitMessage plan =
  [|
    MessagePlan
      $(listE (map emitNamedField (mpFields plan)))
      $(listE (map emitConstraint (mpConstraints plan)))
      $(listE (map emitOneof (mpMessageOneofs plan)))
      $(lift (mpMessageName plan))
      $(emitOptional emitSchema (mpProtoSchema plan))
    |]
  where
    emitNamedField (name, field) = [|($(lift name), $(emitField field))|]
    emitOneof (MessageOneof fields required) = [|MessageOneof $(lift fields) $(lift required)|]


emitSchema :: (Text, ProtoSchema) -> Q Exp
emitSchema (name, schema) = [|($(lift name), $(emitMap (emitMap emitShape) schema))|]
  where
    emitShape = \case
      ExplicitDefault value -> [|ExplicitDefault $(emitValue value)|]
      ImplicitDefault value -> [|ImplicitDefault $(emitValue value)|]
      MessageShape ty -> [|MessageShape $(lift ty)|]
      ListShape item -> [|ListShape $(emitShape item)|]
      MapShape item -> [|MapShape $(emitShape item)|]


emitMap :: (a -> Q Exp) -> Map.Map Text a -> Q Exp
emitMap emit values = [|Map.fromDistinctAscList $(listE (map entry (Map.toAscList values)))|]
  where
    entry (key, value) = [|($(lift key), $(emit value))|]


emitField :: FieldPlan Constraint -> Q Exp
emitField field =
  [|
    FieldPlan
      { fpRequired = $(lift (fpRequired field))
      , fpImplicitDefault = $(emitOptional emitValue (fpImplicitDefault field))
      , fpIgnoreEmpty = $(lift (fpIgnoreEmpty field))
      , fpRules = $(emitValue (fpRules field))
      , fpConstraints = $(listE (map emitConstraint (fpConstraints field)))
      , fpPredefined = $(listE (map emitPredefined (fpPredefined field)))
      , fpItems = $(emitOptional emitField (fpItems field))
      , fpMapKeys = $(emitOptional emitField (fpMapKeys field))
      , fpMapValues = $(emitOptional emitField (fpMapValues field))
      , fpMessage = $(emitOptional emitMessage (fpMessage field))
      , fpMessageReference = $(lift (fpMessageReference field))
      }
    |]
  where
    emitPredefined (constraint, value) = [|($(emitConstraint constraint), $(emitValue value))|]


emitOptional :: (a -> Q Exp) -> Maybe a -> Q Exp
emitOptional _ Nothing = [|Nothing|]
emitOptional emit (Just value) = [|Just $(emit value)|]


emitConstraint :: Constraint -> Q Exp
emitConstraint constraint =
  [|
    ConstraintProgram
      $(lift (constraintId constraint))
      $(lift (constraintMessage constraint))
      $(compileCelExprFn (constraintExpr constraint))
    |]


emitValue :: Value -> Q Exp
emitValue = \case
  VNull -> [|VNull|]
  VBool v -> [|VBool $(lift v)|]
  VInt v -> [|VInt $(lift v)|]
  VUInt v -> [|VUInt $(lift v)|]
  VDouble v -> [|VDouble (castWord64ToDouble $(lift (castDoubleToWord64 v)))|]
  VString v -> [|VString $(lift v)|]
  VBytes v -> [|VBytes $(lift v)|]
  VList values -> [|VList (V.fromList $(listE (map emitValue (V.toList values))))|]
  VMap values -> [|VMap (celMapFromList $(listE (map emitEntry (celMapEntries values))))|]
  VMessage message@(CelMessage name fields) ->
    [|VMessage (celMessageWithUnknownFields $(lift name) $(emitMap emitCelField fields) $(lift (celMessageUnknownFields message)))|]
  VAnyPayload url bytes decoded -> [|VAnyPayload $(lift url) $(lift bytes) $(emitResult emitPayload decoded)|]
  VType v -> [|VType $(lift v)|]
  VTimestamp (Timestamp s n) -> [|VTimestamp (Timestamp $(lift s) $(lift n))|]
  VDuration (Duration s n) -> [|VDuration (Duration $(lift s) $(lift n))|]
  where
    emitEntry (key, value) = [|($(emitValue key), $(emitValue value))|]
    emitCelField (CelField present value) = [|CelField $(lift present) $(emitValue value)|]
    emitCelField (CelFieldWithSelection present value selected) =
      [|CelFieldWithSelection $(lift present) $(emitValue value) $(emitResult emitValue selected)|]
    emitPayload (AnyPayload value selected) =
      [|AnyPayload $(emitValue value) $(emitResult emitValue selected)|]
    emitResult :: (a -> Q Exp) -> Either CelError a -> Q Exp
    emitResult _ (Left (CelError kind message)) = [|Left (CelError $(lift kind) $(lift message))|]
    emitResult emit (Right value) = [|Right $(emit value)|]


----------------------------------------------------------------------
-- Runtime support (referenced by the emitted code)
----------------------------------------------------------------------

{- | Run a compiled validator: per-field constraints (with @this@ bound to the
field value) and message-level constraints (with @this@ bound to the whole
message). Used by the code 'compileMessageValidator' emits.
-}
runCompiledValidator
  :: [(Text, [CompiledConstraint])]
  -> [CompiledConstraint]
  -> Value
  -> [Violation]
runCompiledValidator fields =
  runCompiledValidatorWithIgnore (map (\(name, cons) -> (name, False, cons)) fields)


runCompiledValidatorWithIgnore
  :: [(Text, Bool, [CompiledConstraint])]
  -> [CompiledConstraint]
  -> Value
  -> [Violation]
runCompiledValidatorWithIgnore fields msgCons msg =
  concatMap fieldChecks fields ++ runCons "" msg msgCons
  where
    fieldChecks (fname, ignoreEmpty, cons) = case msg of
      VMap m -> case celMapLookup (VString fname) m of
        Just v | not (ignoreEmpty && isZeroValue v) -> runCons fname v cons
        _ -> []
      _ -> []

    runCons path this = concatMap (one path this)
    one path this (cid, m, prog) =
      let env = bind "this" this libraryEnv
      in case prog env of
           Right (VBool True) -> []
           Right (VString "") -> []
           Right (VBool False) -> [Violation path cid m]
           Right (VString s) -> [Violation path cid s]
           Right _ -> [Violation path cid "constraint must evaluate to bool or string"]
           Left e -> [Violation path cid ("evaluation error: " <> errMsg e)]
