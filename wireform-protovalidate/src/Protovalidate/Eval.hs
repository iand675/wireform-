{-# LANGUAGE OverloadedStrings #-}

{- | The validation engine: evaluate a message's standard and custom CEL
constraints and collect 'Violation's.

A message is represented as a CEL 'VMap' from field name to value (use
"Protovalidate.Proto" to obtain one from a @wireform-proto@ dynamic
message). Each field's value is bound to @this@ and its rule message to
@rules@ before the applicable standard constraints (and any custom CEL) are
evaluated. A constraint that yields @false@ — or a non-empty @string@ —
produces a violation.
-}
module Protovalidate.Eval (
  validate,
  validateAt,
  validateIn,
  evalConstraint,
  isZeroValue,

  -- * Shared runtime and Template Haskell validation
  compileValidationPlan,
  MessagePlan (..),
  FieldPlan (..),
  ConstraintProgram (..),
  prepareValidation,
  runValidationPlan,
) where

import CEL (evaluate)
import CEL.Environment (Env, bind)
import CEL.Error (CelError, errMsg, invalidArg)
import CEL.Eval (Compiled, compileExpr)
import CEL.Value (
  CelField (..),
  CelMap,
  CelMessage (..),
  Timestamp,
  Value (..),
  celFieldPresent,
  celFieldStoredValue,
  celMapEntries,
  celMapFromList,
  celMapLookup,
  celMapSize,
  traverseCelMapValues,
 )
import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.Functor.Identity (Identity (..))
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.Encode (isDefaultDouble)
import Protovalidate.Constraint (Constraint (..))
import Protovalidate.Library (libraryEnv)
import Protovalidate.Rules
import Protovalidate.Violation (Violation (..))


{- | Validate a message (as a CEL value, normally a 'VMap') against a
'MessageRules' using the standard protovalidate CEL environment (base CEL
plus the protovalidate extension library).
-}
validate :: Value -> MessageRules -> [Violation]
validate = validateIn libraryEnv


{- | As 'validate', but with @now@ bound to the given timestamp, so the
time-relative timestamp rules (@lt_now@ / @gt_now@ / @within@) can be
evaluated deterministically.
-}
validateAt :: Timestamp -> Value -> MessageRules -> [Violation]
validateAt now = validateIn (bind "now" (VTimestamp now) libraryEnv)


{- | As 'validate', but with a caller-supplied base environment (e.g. with
additional custom functions or variables registered).
-}
validateIn :: Env -> Value -> MessageRules -> [Violation]
validateIn env msg rules = runValidationPlan env (compileValidationPlan rules) msg


-- | Prepare reusable validation code from parsed rules.
compileValidationPlan :: MessageRules -> MessagePlan ConstraintProgram
compileValidationPlan = prepareValidation compileProgram
  where
    compileProgram con =
      ConstraintProgram (constraintId con) (constraintMessage con) (compileExpr (constraintExpr con))


data ConstraintProgram = ConstraintProgram !Text !Text !Compiled


-- | A validation plan with the applicable constraints selected in rule order.
-- The runtime and TH paths supply their own compilation of each constraint.
data MessagePlan constraint = MessagePlan
  { mpFields :: ![(Text, FieldPlan constraint)]
  , mpConstraints :: ![constraint]
  , mpMessageOneofs :: ![MessageOneof]
  , mpMessageName :: !(Maybe Text)
  , mpProtoSchema :: !(Maybe (Text, ProtoSchema))
  }


data FieldPlan constraint = FieldPlan
  { fpRequired :: !Bool
  , fpImplicitDefault :: !(Maybe Value)
  , fpIgnoreEmpty :: !Bool
  , fpRules :: ~Value
  , fpConstraints :: ~[constraint]
  , fpPredefined :: ![(constraint, Value)]
  , fpItems :: !(Maybe (FieldPlan constraint))
  , fpMapKeys :: !(Maybe (FieldPlan constraint))
  , fpMapValues :: !(Maybe (FieldPlan constraint))
  , fpMessage :: !(Maybe (MessagePlan constraint))
  , fpMessageReference :: !(Maybe Text)
  }


prepareValidation :: (Constraint -> constraint) -> MessageRules -> MessagePlan constraint
prepareValidation compileProgram mr = (messagePlan mr) {mpProtoSchema = fmap reachable (mrProtoSchema mr)}
  where
    messagePlan rules =
      MessagePlan
        { mpFields = map (fmap fieldPlan) (mrFields rules)
        , mpConstraints = map compileProgram (mrCustom rules)
        , mpMessageOneofs = mrMessageOneofs rules
        , mpMessageName = fst <$> mrProtoSchema rules
        , mpProtoSchema = Nothing
        }

    reachable (root, schema) = (root, Map.restrictKeys schema (visit Set.empty root))
      where
        visit seen name
          | Set.member name seen = seen
          | otherwise = Map.foldl' visitField (Set.insert name seen) (Map.findWithDefault Map.empty name schema)
        visitField seen = \case
          MessageShape name -> visit seen name
          ListShape shape -> visitField seen shape
          MapShape shape -> visitField seen shape
          _ -> seen

    fieldPlan fr =
      FieldPlan
        { fpRequired = frRequired fr
        , fpImplicitDefault = frImplicitDefault fr
        , fpIgnoreEmpty = frIgnoreEmpty fr
        , fpRules = VMap (rulesAsMap (frRules fr))
        , fpConstraints = map compileProgram (applicable fr ++ frCustom fr)
        , fpPredefined = map (\(con, value) -> (compileProgram con, value)) (frPredefined fr)
        , fpItems = fieldPlan <$> frItems fr
        , fpMapKeys = fieldPlan <$> frMapKeys fr
        , fpMapValues = fieldPlan <$> frMapValues fr
        , fpMessage = messagePlan <$> frMessage fr
        , fpMessageReference = frMessageReference fr
        }

    applicable fr =
      map snd $
        filter (\(name, _) -> ruleActive name (frRules fr)) $
          maybe [] standardConstraints (frKind fr)


runValidationPlan :: Env -> MessagePlan ConstraintProgram -> Value -> [Violation]
runValidationPlan env plan value =
  case mpProtoSchema plan of
    Nothing -> validateMessage (ValidationContext env Map.empty Map.empty) "" Nothing value plan
    Just (name, schema) -> validateMessage (ValidationContext env schema Map.empty) "" (Just (MessageShape name)) value plan


data ValidationContext = ValidationContext
  { vcEnv :: !Env
  , vcSchema :: !ProtoSchema
  , vcAncestors :: !(Map.Map Text (MessagePlan ConstraintProgram))
  }


{- | Evaluate a single constraint in an environment where @this@/@rules@ are
already bound. Returns 'Nothing' when satisfied, or @'Just' message@ for a
violation.
-}
evalConstraint :: Env -> Constraint -> Either CelError (Maybe Text)
evalConstraint env con = interpretConstraint (constraintMessage con) (evaluate env (constraintExpr con))


interpretConstraint :: Text -> Either CelError Value -> Either CelError (Maybe Text)
interpretConstraint message = \case
  Left err -> Left err
  Right (VBool True) -> Right Nothing
  Right (VBool False) -> Right (Just message)
  Right (VString "") -> Right Nothing
  Right (VString s) -> Right (Just s)
  Right _ -> Left (invalidArg "constraint expression must evaluate to bool or string")


validateMessage :: ValidationContext -> Text -> Maybe ProtoFieldShape -> Value -> MessagePlan ConstraintProgram -> [Violation]
validateMessage context path shape msg mr =
  concatMap (validateNamedField nestedContext path fields msg) (mpFields mr)
    ++ runConstraints (bind "this" projected (vcEnv context)) path (mpConstraints mr)
    ++ mapMaybe (messageOneofViolation path projected) (mpMessageOneofs mr)
  where
    schema = vcSchema context
    projected = asProtoValue schema shape msg
    fields = case shape of
      Just (MessageShape name) -> Map.findWithDefault Map.empty name schema
      _ -> Map.empty
    nestedContext = case mpMessageName mr of
      Just name | Map.notMember name ancestors ->
        context {vcAncestors = Map.insert name mr ancestors}
      _ -> context
    ancestors = vcAncestors context


messageOneofViolation :: Text -> Value -> MessageOneof -> Maybe Violation
messageOneofViolation path value group = case filter (\name -> isJust (lookupField name value)) fields of
  _ : _ : _ -> problem ("only one of " <> names <> " can be set")
  [] | messageOneofRequired group -> problem ("one of " <> names <> " must be set")
  _ -> Nothing
  where
    fields = messageOneofFields group
    names = T.intercalate ", " fields
    problem = Just . Violation path "message.oneof"


validateNamedField :: ValidationContext -> Text -> Map.Map Text ProtoFieldShape -> Value -> (Text, FieldPlan ConstraintProgram) -> [Violation]
validateNamedField context prefixPath shapes msg (name, fr) =
  case lookupField name msg of
    Nothing
      | fpRequired fr -> missing
      | otherwise -> maybe [] evaluateField (fpImplicitDefault fr)
    Just v
      | fpRequired fr && maybe False (matchesDefault v) (fpImplicitDefault fr) -> missing
      | otherwise -> evaluateField v
  where
    fieldPath = if T.null prefixPath then name else prefixPath <> "." <> name
    missing = [Violation fieldPath "required" "value is required"]
    evaluateField v = validateFieldValue context fieldPath (Map.lookup name shapes) v fr


lookupField :: Text -> Value -> Maybe Value
lookupField name (VMap m) = celMapLookup (VString name) m
lookupField name (VMessage (CelMessage _ fields)) = case Map.lookup name fields of
  Just field | celFieldPresent field -> Just (celFieldStoredValue field)
  _ -> Nothing
lookupField _ _ = Nothing


-- Project the supplied map into a protobuf message only for CEL evaluation.
-- Field rules still receive the original presence information.
asProtoValue :: ProtoSchema -> Maybe ProtoFieldShape -> Value -> Value
asProtoValue schema shape value = case (shape, value) of
  (Just (MessageShape name), VMap _) -> protoMessage schema name (Just value)
  (Just (ListShape item@(MessageShape _)), VList values) -> VList (V.map (asProtoValue schema (Just item)) values)
  (Just (MapShape item@(MessageShape _)), VMap values) ->
    VMap (runIdentity (traverseCelMapValues (\_ -> Identity . asProtoValue schema (Just item)) values))
  _ -> value


protoMessage :: ProtoSchema -> Text -> Maybe Value -> Value
protoMessage schema name input = case Map.lookup name schema of
  Nothing -> fromMaybe (VMessage (CelMessage name Map.empty)) input
  Just fields -> VMessage (CelMessage name (Map.mapWithKey project fields))
  where
    project field shape =
      let supplied = input >>= lookupField field
          fallback = case shape of
            ExplicitDefault v -> v
            ImplicitDefault v -> v
            MessageShape ty -> protoMessage schema ty Nothing
            ListShape _ -> VList V.empty
            MapShape _ -> VMap (celMapFromList [])
          present = case shape of
            ExplicitDefault _ -> isJust supplied
            MessageShape _ -> isJust supplied
            _ -> maybe False (\v -> not (matchesDefault v fallback)) supplied
      in CelField present (maybe fallback (asProtoValue schema (Just shape)) supplied)


-- CEL equality treats signed zeros as equal, but only +0 is implicit in protobuf.
matchesDefault :: Value -> Value -> Bool
matchesDefault (VDouble n) (VDouble fallback)
  | isDefaultDouble fallback = isDefaultDouble n
matchesDefault value fallback = value == fallback


validateFieldValue :: ValidationContext -> Text -> Maybe ProtoFieldShape -> Value -> FieldPlan ConstraintProgram -> [Violation]
validateFieldValue context path shape v fr
  | fpIgnoreEmpty fr && maybe (isZeroValue v) (matchesDefault v) (fpImplicitDefault fr) = []
  | otherwise = violations ++ predefViols ++ itemViols ++ mapViols ++ nestedViols
  where
    env' = bind "rules" (fpRules fr) (bind "this" (asProtoValue (vcSchema context) shape v) (vcEnv context))
    violations = runConstraints env' path (fpConstraints fr)

    -- Predefined constraints additionally bind @rule@ to their value.
    predefViols =
      concatMap
        (\(con, ruleVal) -> runConstraints (bind "rule" ruleVal env') path [con])
        (fpPredefined fr)

    itemViols = case (fpItems fr, v) of
      (Just ifr, VList xs) ->
        concat (zipWith (\i x -> validateFieldValue context (indexed path i) itemShape x ifr) [0 ..] (V.toList xs))
      _ -> []

    itemShape = case shape of
      Just (ListShape item) -> Just item
      Just (MapShape item) -> Just item
      _ -> Nothing

    mapViols = case v of
      VMap m ->
        let entries = celMapEntries m
            keyViols = case fpMapKeys fr of
              Just kr -> concatMap (\(k, _) -> validateFieldValue context (keyed path k) Nothing k kr) entries
              Nothing -> []
            valViols = case fpMapValues fr of
              Just vr -> concatMap (\(k, val) -> validateFieldValue context (keyed path k) itemShape val vr) entries
              Nothing -> []
        in keyViols ++ valViols
      _ -> []

    message = fpMessage fr <|> (fpMessageReference fr >>= (\name -> Map.lookup name (vcAncestors context)))
    nestedViols = case (message, v) of
      (Just mr, VMap _) -> validateMessage context path shape v mr
      (Just mr, VMessage _) -> validateMessage context path shape v mr
      (Just mr, VList xs) ->
        concat (zipWith (\i x -> validateMessage context (indexed path i) itemShape x mr) [0 ..] (V.toList xs))
      _ -> []


indexed :: Text -> Int -> Text
indexed path i = path <> "[" <> T.pack (show i) <> "]"


keyed :: Text -> Value -> Text
keyed path k = path <> "[" <> renderKeyText k <> "]"


renderKeyText :: Value -> Text
renderKeyText = \case
  VString s -> s
  VInt n -> T.pack (show n)
  VUInt n -> T.pack (show n)
  VBool b -> if b then "true" else "false"
  other -> T.pack (show other)


rulesAsMap :: [(Text, Value)] -> CelMap
rulesAsMap rs = celMapFromList [(VString k, v) | (k, v) <- rs]


ruleActive :: Text -> [(Text, Value)] -> Bool
ruleActive ruleField rs = case lookup ruleField rs of
  Nothing -> False
  Just (VBool False) -> ruleField == "const"
  Just _ -> True


runConstraints :: Env -> Text -> [ConstraintProgram] -> [Violation]
runConstraints env path = concatMap go
  where
    go (ConstraintProgram cid message program) = case interpretConstraint message (program env) of
      Left err -> [Violation path cid ("evaluation error: " <> errMsg err)]
      Right Nothing -> []
      Right (Just m) -> [Violation path cid m]


isZeroValue :: Value -> Bool
isZeroValue = \case
  VNull -> True
  VBool b -> not b
  VInt 0 -> True
  VUInt 0 -> True
  VDouble d -> d == 0
  VString s -> T.null s
  VBytes b -> BS.null b
  VList xs -> V.null xs
  VMap m -> celMapSize m == 0
  _ -> False
