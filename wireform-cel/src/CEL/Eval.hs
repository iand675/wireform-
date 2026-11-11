{-# LANGUAGE BangPatterns #-}

{- | The CEL evaluator, structured as per-node /combinators/.

Each AST node compiles to a 'Compiled' closure @'Env' -> 'Either' 'CelError'
'Value'@ built from the 'Compiled' closures of its children
('compileExpr'). 'evalIn' / 'eval' just run that closure. This has two
benefits over a @case@-over-AST interpreter:

  * the AST is walked once ('compileExpr'); reuse the result to evaluate the
    same program against many environments with no re-dispatch; and
  * the combinators are exactly what "CEL.TH" emits at compile time, so the
    compile-time-compiled code and the runtime evaluator share one source of
    truth for semantics (error-absorbing @&&@/@||@, short-circuit @?:@, the
    comprehension macros, and longest-prefix name resolution).
-}
module CEL.Eval (
  eval,
  evalIn,
  compileExpr,
  Compiled,

  -- * Node combinators (used by "CEL.TH"; not generally needed directly)
  cLit,
  cName,
  cSelect,
  cIndex,
  cList,
  cMapLit,
  cStruct,
  cStructWithFields,
  cCond,
  cAnd,
  cOr,
  cNot,
  cNeg,
  cArith,
  cRel,
  cCall,
  cQualifiedCall,
  cHas,
  cHasInvalid,
  cAll,
  cExists,
  cExistsOne,
  cFilter,
  cMapMacro,
  cAll2,
  cExists2,
  cExistsOne2,
  cTransformList,
  cTransformMap,
) where

import CEL.Environment
import CEL.Error
import CEL.Stdlib
import CEL.Syntax
import CEL.Value
import Control.Applicative ((<|>))
import Control.Monad (foldM, (>=>))
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.List (find, inits)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Float (double2Float, float2Double)


-- | A compiled expression: a function from the binding environment to a result.
type Compiled = Env -> Either CelError Value


-- | Evaluate an expression in the empty environment.
eval :: Expr -> Either CelError Value
eval = evalIn emptyEnv


{- | Evaluate an expression in the given environment. (Equivalent to
@'compileExpr' expr env@; reuse 'compileExpr' to avoid recompiling.)
-}
evalIn :: Env -> Expr -> Either CelError Value
evalIn env expr = compileExpr expr env


{- | Compile an expression to a reusable 'Compiled' closure by structural
recursion over the combinators.
-}
compileExpr :: Expr -> Compiled
compileExpr expr = case expr of
  ELit l -> cLit l
  EIdent root name -> cName root [name]
  ESelect e f -> case identPath (ESelect e f) of
    Just (root, segs) -> cName root segs
    Nothing -> cSelect (compileExpr e) f
  EIndex e i -> cIndex (compileExpr e) (compileExpr i)
  EList es -> cList (map compileExpr es)
  EMap entries -> cMapLit (map (\(k, v) -> (compileExpr k, compileExpr v)) entries)
  EStruct root segs fields -> cStructWithFields root segs (fmap (fmap compileExpr) fields)
  ECond c t e -> cCond (compileExpr c) (compileExpr t) (compileExpr e)
  EAnd a b -> cAnd (compileExpr a) (compileExpr b)
  EOr a b -> cOr (compileExpr a) (compileExpr b)
  ENot e -> cNot (compileExpr e)
  ENeg e -> cNeg (compileExpr e)
  EArith op a b -> cArith op (compileExpr a) (compileExpr b)
  ERel op a b -> cRel op (compileExpr a) (compileExpr b)
  ECall recv name args -> compileCall recv name args


{- | Compile a call, recognizing the @has@ and comprehension macros at
compile time (so macro dispatch is not repeated per evaluation).
-}
compileCall :: Maybe Expr -> Text -> [Expr] -> Compiled
compileCall Nothing "has" [ESelect e f] = cHas (compileExpr e) f
compileCall Nothing "has" [_] = cHasInvalid
compileCall (Just recv) name args
  | Just c <- compileMacro recv name args = c
  | Just (root, segments) <- identPath recv =
      cQualifiedCall root (T.intercalate "." segments) name (compileExpr recv) (map compileExpr args)
compileCall recv name args =
  cCall (fmap compileExpr recv) name (map compileExpr args)


{- | Recognize a comprehension macro shape and compile it; 'Nothing' if the
call is an ordinary function.
-}
compileMacro :: Expr -> Text -> [Expr] -> Maybe Compiled
compileMacro recv name args = case (name, args) of
  ("all", [EIdent _ v, p]) -> Just (cAll (compileExpr recv) v (compileExpr p))
  ("exists", [EIdent _ v, p]) -> Just (cExists (compileExpr recv) v (compileExpr p))
  ("exists_one", [EIdent _ v, p]) -> Just (cExistsOne (compileExpr recv) v (compileExpr p))
  ("existsOne", [EIdent _ v, p]) -> Just (cExistsOne (compileExpr recv) v (compileExpr p))
  ("filter", [EIdent _ v, p]) -> Just (cFilter (compileExpr recv) v (compileExpr p))
  ("map", [EIdent _ v, t]) -> Just (cMapMacro (compileExpr recv) v Nothing (compileExpr t))
  ("map", [EIdent _ v, p, t]) -> Just (cMapMacro (compileExpr recv) v (Just (compileExpr p)) (compileExpr t))
  ("all", [EIdent _ a, EIdent _ b, p]) -> Just (cAll2 (compileExpr recv) a b (compileExpr p))
  ("exists", [EIdent _ a, EIdent _ b, p]) -> Just (cExists2 (compileExpr recv) a b (compileExpr p))
  ("exists_one", [EIdent _ a, EIdent _ b, p]) -> Just (cExistsOne2 (compileExpr recv) a b (compileExpr p))
  ("existsOne", [EIdent _ a, EIdent _ b, p]) -> Just (cExistsOne2 (compileExpr recv) a b (compileExpr p))
  ("transformList", [EIdent _ a, EIdent _ b, t]) -> Just (cTransformList (compileExpr recv) a b Nothing (compileExpr t))
  ("transformList", [EIdent _ a, EIdent _ b, p, t]) -> Just (cTransformList (compileExpr recv) a b (Just (compileExpr p)) (compileExpr t))
  ("transformMap", [EIdent _ a, EIdent _ b, t]) -> Just (cTransformMap (compileExpr recv) a b Nothing (compileExpr t))
  ("transformMap", [EIdent _ a, EIdent _ b, p, t]) -> Just (cTransformMap (compileExpr recv) a b (Just (compileExpr p)) (compileExpr t))
  _ -> Nothing


----------------------------------------------------------------------
-- Leaf / structural combinators
----------------------------------------------------------------------

-- | A literal (precomputed once).
cLit :: Literal -> Compiled
cLit l = let !v = literalValue l in \_ -> Right v


-- | A (possibly dotted) name resolved against the environment.
cName :: Bool -> [Text] -> Compiled
cName root segs env = resolveName env root segs >>= unpackValue


-- | Field selection on a computed value.
cSelect :: Compiled -> Text -> Compiled
cSelect e f env = e env >>= \v -> selectField v f


-- | Indexing.
cIndex :: Compiled -> Compiled -> Compiled
cIndex e i env = do
  v <- e env
  idx <- i env
  indexValue v idx


-- | List literal.
cList :: [Compiled] -> Compiled
cList cs env = VList . V.fromList <$> mapM ($ env) cs


-- | Map literal.
cMapLit :: [(Compiled, Compiled)] -> Compiled
cMapLit entries env = do
  kvs <- mapM entry entries
  case celMap kvs of
    Left m -> Left (invalidArg m)
    Right cm -> Right (VMap cm)
  where
    entry (kc, vc) = do
      k <- kc env
      validateKey k
      v <- vc env
      Right (k, v)


-- | Construct a message with no field initializers.
cStruct :: [Text] -> Compiled
cStruct segs = cStructWithFields True segs []


-- | Construct a well-known protobuf value or a registered message.
cStructWithFields :: Bool -> [Text] -> [(Text, Compiled)] -> Compiled
cStructWithFields root segs fields env =
  case firstJust (map constructor candidates) of
    Just construct -> construct fields env
    Nothing -> Left (unsupported ("message construction is not supported: " <> name))
  where
    name = T.intercalate "." segs
    candidates
      | root = [name]
      | otherwise = map (\prefix -> T.intercalate "." (prefix <> segs)) (containerScopes (envContainer env))
    constructor qualified =
      wellKnownConstructor qualified
        <|> (constructMessage qualified <$> lookupMessageType qualified env)


constructMessage :: Text -> MessageConstructor -> [(Text, Compiled)] -> Compiled
constructMessage name construct fields env = do
  values <- evalInitializers fields env
  VMessage . CelMessage name <$> construct values


evalInitializers :: [(Text, Compiled)] -> Env -> Either CelError (Map.Map Text Value)
evalInitializers fields env = foldM insertField Map.empty fields
  where
    insertField values (field, expression)
      | Map.member field values = Left (invalidArg ("duplicate message field: " <> field))
      | otherwise = do
          value <- expression env
          pure (Map.insert field value values)


wellKnownConstructor :: Text -> Maybe ([(Text, Compiled)] -> Compiled)
wellKnownConstructor = \case
  "google.protobuf.BoolValue" -> wrapper (VBool False)
  "google.protobuf.BytesValue" -> wrapper (VBytes BS.empty)
  "google.protobuf.StringValue" -> wrapper (VString "")
  "google.protobuf.Int64Value" -> wrapper (VInt 0)
  "google.protobuf.UInt64Value" -> wrapper (VUInt 0)
  "google.protobuf.DoubleValue" -> wrapper (VDouble 0)
  "google.protobuf.Int32Value" -> Just (constructWrapper (VInt 0) narrowInt32)
  "google.protobuf.UInt32Value" -> Just (constructWrapper (VUInt 0) narrowUInt32)
  "google.protobuf.FloatValue" -> Just (constructWrapper (VDouble 0) narrowFloat32)
  -- The upstream CEL fixtures exercise literals for both abstract time types.
  "google.protobuf.Timestamp" -> Just (constructTimeValue TimestampLiteral)
  "google.protobuf.Duration" -> Just (constructTimeValue DurationLiteral)
  "google.protobuf.ListValue" -> Just (constructSingleField "values" (VList V.empty) protobufJsonList)
  "google.protobuf.Struct" -> Just (constructSingleField "fields" (VMap (celMapFromList [])) protobufJsonStruct)
  "google.protobuf.Value" -> Just constructJsonValue
  "google.protobuf.Any" -> Just constructAny
  "google.protobuf.Empty" -> Just (constructMessage "google.protobuf.Empty" emptyMessage)
  "google.protobuf.FieldMask" -> Just (constructMessage "google.protobuf.FieldMask" fieldMask)
  _ -> Nothing
  where
    wrapper initial = Just (constructWrapper initial Right)
    emptyMessage fields = case Map.lookupMin fields of
      Nothing -> Right Map.empty
      Just (field, _) -> Left (noSuchField field)
    fieldMask fields
      | Just (unknown, _) <- Map.lookupMin (Map.delete "paths" fields) = Left (noSuchField unknown)
      | otherwise = case Map.lookup "paths" fields of
          Nothing -> Right (Map.singleton "paths" (CelField False (VList V.empty)))
          Just (VList paths) -> do
            values <- V.mapM (unpackValue >=> requireType "paths" TyString) paths
            Right (Map.singleton "paths" (CelField (not (V.null values)) (VList values)))
          _ -> Left (noOverload "paths")


constructWrapper :: Value -> (Value -> Either CelError Value) -> [(Text, Compiled)] -> Compiled
constructWrapper initial convert = constructSingleField "value" initial (requireType "value" (typeOf initial) >=> convert)


constructSingleField :: Text -> Value -> (Value -> Either CelError Value) -> [(Text, Compiled)] -> Compiled
constructSingleField field initial convert fields env = case fields of
  [] -> Right initial
  [(name, expression)]
    | name == field -> expression env >>= convert
    | otherwise -> Left (noSuchField name)
  _ -> Left (invalidArg ("constructor accepts at most one " <> field <> " field"))


constructJsonValue :: [(Text, Compiled)] -> Compiled
constructJsonValue fields env = case fields of
  [] -> Right VNull
  [(name, expression)] -> case name of
    "null_value" -> expression env >>= convertNullEnum
    "number_value" -> expression env >>= requireType name TyDouble
    "string_value" -> expression env >>= requireType name TyString
    "bool_value" -> expression env >>= requireType name TyBool
    "struct_value" -> expression env >>= protobufJsonStruct
    "list_value" -> expression env >>= protobufJsonList
    _ -> Left (noSuchField name)
  _ -> Left (invalidArg "google.protobuf.Value accepts at most one oneof field")
  where
    convertNullEnum (VInt n)
      | fitsInt32 n = Right VNull
      | otherwise = Left (overflow "NullValue")
    convertNullEnum _ = Left (noOverload "null_value")


constructAny :: [(Text, Compiled)] -> Compiled
constructAny fields env = do
  values <- evalInitializers fields env
  case Map.lookupMin (Map.delete "type_url" (Map.delete "value" values)) of
    Just (name, _) -> Left (noSuchField name)
    Nothing -> do
      urlValue <- unpackValue (Map.findWithDefault (VString "") "type_url" values)
      payloadValue <- unpackValue (Map.findWithDefault (VBytes BS.empty) "value" values)
      case (urlValue, payloadValue) of
        (VString url, VBytes payload)
          | T.null url -> Left (conversion "google.protobuf.Any requires a type_url")
          | Just resolve <- lookupAnyResolver env -> resolve url payload >>= unpackValue
          | otherwise -> Left (unsupported "google.protobuf.Any requires a type resolver")
        (VString _, _) -> Left (noOverload "value")
        _ -> Left (noOverload "type_url")


requireType :: Text -> CelType -> Value -> Either CelError Value
requireType field ty value
  | typeOf value == ty = Right value
  | otherwise = Left (noOverload field)


fitsInt32 :: Int64 -> Bool
fitsInt32 n = n >= fromIntegral (minBound :: Int32) && n <= fromIntegral (maxBound :: Int32)


narrowInt32, narrowUInt32, narrowFloat32 :: Value -> Either CelError Value
narrowInt32 value@(VInt n)
  | fitsInt32 n = Right value
  | otherwise = Left (overflow "Int32Value")
narrowInt32 _ = Left (noOverload "Int32Value")
narrowUInt32 value@(VUInt n)
  | n <= fromIntegral (maxBound :: Word32) = Right value
  | otherwise = Left (overflow "UInt32Value")
narrowUInt32 _ = Left (noOverload "UInt32Value")
narrowFloat32 (VDouble n) = Right (VDouble (float2Double (double2Float n)))
narrowFloat32 _ = Left (noOverload "FloatValue")


data TimeLiteral = TimestampLiteral | DurationLiteral


constructTimeValue :: TimeLiteral -> [(Text, Compiled)] -> Compiled
constructTimeValue literal fields env = do
  values <- foldM addField Map.empty fields
  let seconds = Map.findWithDefault 0 "seconds" values
      nanos = Map.findWithDefault 0 "nanos" values
      total = toInteger seconds * 1000000000 + toInteger nanos
  case literal of
    TimestampLiteral
      | nanos < 0 || nanos >= 1000000000 -> Left (overflow "timestamp nanos")
      | otherwise -> mkTimestampNanos total
    DurationLiteral
      | nanos <= -1000000000 || nanos >= 1000000000 -> Left (overflow "duration nanos")
      | (seconds < 0 && nanos > 0) || (seconds > 0 && nanos < 0) ->
          Left (invalidArg "duration seconds and nanos must have the same sign")
      | otherwise -> mkDurationNanos total
  where
    addField values (name, expression)
      | name /= "seconds" && name /= "nanos" = Left (noSuchField name)
      | Map.member name values = Left (invalidArg ("duplicate field: " <> name))
      | otherwise = do
          value <- expression env
          case value of
            VInt n -> Right (Map.insert name n values)
            _ -> Left (noOverload name)


literalValue :: Literal -> Value
literalValue = \case
  LNull -> VNull
  LBool b -> VBool b
  LInt i -> VInt i
  LUInt u -> VUInt u
  LDouble d -> VDouble d
  LString s -> VString s
  LBytes b -> VBytes b


validateKey :: Value -> Either CelError ()
validateKey v = case v of
  VInt _ -> Right ()
  VUInt _ -> Right ()
  VBool _ -> Right ()
  VString _ -> Right ()
  _ -> Left (invalidArg ("invalid map key type: " <> typeNameText (typeOf v)))


----------------------------------------------------------------------
-- Operators
----------------------------------------------------------------------

-- | Conditional @?:@ (only the taken branch is evaluated).
cCond :: Compiled -> Compiled -> Compiled -> Compiled
cCond c t e env = do
  cv <- c env
  case cv of
    VBool True -> t env
    VBool False -> e env
    _ -> Left (noOverload "_?_:_")


-- | Error-absorbing, commutative logical AND.
cAnd :: Compiled -> Compiled -> Compiled
cAnd a b env = combineAnd (asBool (a env)) (asBool (b env))


-- | Error-absorbing, commutative logical OR.
cOr :: Compiled -> Compiled -> Compiled
cOr a b env = combineOr (asBool (a env)) (asBool (b env))


cNot :: Compiled -> Compiled
cNot e env = e env >>= notValue


cNeg :: Compiled -> Compiled
cNeg e env = e env >>= negateValue


cArith :: ArithOp -> Compiled -> Compiled -> Compiled
cArith op a b env = do
  va <- a env
  vb <- b env
  arith op va vb


cRel :: RelOp -> Compiled -> Compiled -> Compiled
cRel op a b env = do
  va <- a env
  vb <- b env
  relApply op va vb


relApply :: RelOp -> Value -> Value -> Either CelError Value
relApply op va vb = case op of
  Eq -> VBool <$> equalValues va vb
  Ne -> VBool . not <$> equalValues va vb
  In -> inOp va vb
  _ -> ordCompare op va vb


-- Collection equality follows the same error-absorbing conjunction as
-- comparing each corresponding element in CEL.
equalValues :: Value -> Value -> Either CelError Bool
equalValues lhs rhs = do
  a <- unpackValue lhs
  b <- unpackValue rhs
  case (a, b) of
    (VList x, VList y)
      | V.length x == V.length y -> compareList x y 0 (Right True)
      | otherwise -> Right False
    (VMap x, VMap y)
      | celMapSize x == celMapSize y -> compareMap y (celMapEntries x) (Right True)
      | otherwise -> Right False
    _ -> Right (valueEq a b)
  where
    compareList x y i result
      | i == V.length x = result
      | otherwise = case andBool result (equalValues (V.unsafeIndex x i) (V.unsafeIndex y i)) of
          Right False -> Right False
          next -> compareList x y (i + 1) next
    compareMap _ [] result = result
    compareMap target ((key, value) : rest) result = case celMapLookup key target of
      Nothing -> Right False
      Just other -> case andBool result (equalValues value other) of
        Right False -> Right False
        next -> compareMap target rest next


asBool :: Either CelError Value -> Either CelError Bool
asBool (Right (VBool b)) = Right b
asBool (Right _) = Left (noOverload "logical operator on non-bool")
asBool (Left e) = Left e


combineAnd :: Either CelError Bool -> Either CelError Bool -> Either CelError Value
combineAnd ea eb = VBool <$> andBool ea eb


andBool :: Either CelError Bool -> Either CelError Bool -> Either CelError Bool
andBool ea eb = case (ea, eb) of
  (Right False, _) -> Right False
  (_, Right False) -> Right False
  (Right True, Right True) -> Right True
  (Left e, _) -> Left e
  (_, Left e) -> Left e


combineOr :: Either CelError Bool -> Either CelError Bool -> Either CelError Value
combineOr ea eb = case (ea, eb) of
  (Right True, _) -> Right (VBool True)
  (_, Right True) -> Right (VBool True)
  (Right False, Right False) -> Right (VBool False)
  (Left e, _) -> Left e
  (_, Left e) -> Left e


----------------------------------------------------------------------
-- Calls
----------------------------------------------------------------------

-- | Ordinary function / method call (receiver, if any, becomes the first arg).
cCall :: Maybe Compiled -> Text -> [Compiled] -> Compiled
cCall Nothing name argCs env =
  case T.stripPrefix "." name of
    Just absolute -> callGlobal absolute argCs env
    Nothing -> callGlobal (maybe name id (resolveFunctionName False name env)) argCs env
cCall (Just recv) name argCs env = do
  receiver <- recv env
  args <- (receiver :) <$> mapM ($ env) argCs
  case lookupFunction name args env of
    Just r -> r >>= unpackValue
    Nothing -> callMemberFunction name args


-- | An identifier or selection can qualify a global function name.
cQualifiedCall :: Bool -> Text -> Text -> Compiled -> [Compiled] -> Compiled
cQualifiedCall root qualifier name recv argCs =
  let qualified = qualifier <> "." <> name
  in \env -> case resolveFunctionName root qualified env of
       Just resolved -> callGlobal resolved argCs env
       Nothing -> cCall (Just recv) name argCs env


callGlobal :: Text -> [Compiled] -> Compiled
callGlobal name argCs env = do
  args <- mapM ($ env) argCs
  case lookupFunction name args env of
    Just result -> result >>= unpackValue
    Nothing -> callGlobalFunction name args


resolveFunctionName :: Bool -> Text -> Env -> Maybe Text
resolveFunctionName root name env
  | root || T.null (envContainer env) = if declared name then Just name else Nothing
  | otherwise = find declared (map qualify (containerScopes (envContainer env)))
  where
    declared = flip Map.member (envFuncs env)
    qualify scope = T.intercalate "." (scope <> [name])


-- | @has(e.f)@.
cHas :: Compiled -> Text -> Compiled
cHas ec f env = do
  v <- ec env
  case v of
    VMap m -> case celMapLookup (VString f) m of
      Nothing -> Right (VBool False)
      Just value -> VBool True <$ unpackValue value
    VMessage (CelMessage _ fields) -> case Map.lookup f fields of
      Just field -> Right (VBool (celFieldPresent field))
      Nothing -> Left (noSuchField f)
    _ -> Left (noSuchField f)


-- | @has@ applied to a non-selection argument.
cHasInvalid :: Compiled
cHasInvalid _ = Left (invalidArg "has() requires a field selection argument, e.g. has(x.y)")


----------------------------------------------------------------------
-- Comprehension macros
----------------------------------------------------------------------

rangeElems :: Value -> Either CelError [Value]
rangeElems (VList v) = Right (V.toList v)
rangeElems (VMap m) = Right (map fst (celMapEntries m))
rangeElems v = Left (noOverload ("comprehension over " <> typeNameText (typeOf v)))


rangeElems2 :: Value -> Either CelError [(Value, Value)]
rangeElems2 (VList v) = Right (zipWith (\i x -> (VInt (fromIntegral (i :: Int)), x)) [0 ..] (V.toList v))
rangeElems2 (VMap m) = Right (celMapEntries m)
rangeElems2 v = Left (noOverload ("comprehension over " <> typeNameText (typeOf v)))


cAll :: Compiled -> Text -> Compiled -> Compiled
cAll recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  go els Nothing
  where
    go [] mErr = maybe (Right (VBool True)) Left mErr
    go (el : rest) mErr = case asBool (predC (bindLocal var el env)) of
      Right False -> Right (VBool False)
      Right True -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExists :: Compiled -> Text -> Compiled -> Compiled
cExists recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  go els Nothing
  where
    go [] mErr = maybe (Right (VBool False)) Left mErr
    go (el : rest) mErr = case asBool (predC (bindLocal var el env)) of
      Right True -> Right (VBool True)
      Right False -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExistsOne :: Compiled -> Text -> Compiled -> Compiled
cExistsOne recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  go els (0 :: Int)
  where
    go [] n = Right (VBool (n == 1))
    go (el : rest) n = case asBool (predC (bindLocal var el env)) of
      Right True -> go rest (n + 1)
      Right False -> go rest n
      Left e -> Left e


cFilter :: Compiled -> Text -> Compiled -> Compiled
cFilter recvC var predC env = do
  v <- recvC env
  els <- rangeElems v
  VList . V.fromList <$> go els
  where
    go [] = Right []
    go (el : rest) = case asBool (predC (bindLocal var el env)) of
      Right True -> (el :) <$> go rest
      Right False -> go rest
      Left e -> Left e


cMapMacro :: Compiled -> Text -> Maybe Compiled -> Compiled -> Compiled
cMapMacro recvC var mFilter transformC env = do
  v <- recvC env
  els <- rangeElems v
  VList . V.fromList <$> go els
  where
    go [] = Right []
    go (el : rest) =
      let env' = bindLocal var el env
      in case mFilter of
           Nothing -> do r <- transformC env'; (r :) <$> go rest
           Just p -> case asBool (p env') of
             Right True -> do r <- transformC env'; (r :) <$> go rest
             Right False -> go rest
             Left e -> Left e


cAll2 :: Compiled -> Text -> Text -> Compiled -> Compiled
cAll2 recvC v1 v2 predC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  go pairs Nothing
  where
    go [] mErr = maybe (Right (VBool True)) Left mErr
    go ((a, b) : rest) mErr = case asBool (predC (bind2 v1 a v2 b env)) of
      Right False -> Right (VBool False)
      Right True -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExists2 :: Compiled -> Text -> Text -> Compiled -> Compiled
cExists2 recvC v1 v2 predC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  go pairs Nothing
  where
    go [] mErr = maybe (Right (VBool False)) Left mErr
    go ((a, b) : rest) mErr = case asBool (predC (bind2 v1 a v2 b env)) of
      Right True -> Right (VBool True)
      Right False -> go rest mErr
      Left e -> go rest (Just (maybe e id mErr))


cExistsOne2 :: Compiled -> Text -> Text -> Compiled -> Compiled
cExistsOne2 recvC v1 v2 predC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  go pairs (0 :: Int)
  where
    go [] n = Right (VBool (n == 1))
    go ((a, b) : rest) n = case asBool (predC (bind2 v1 a v2 b env)) of
      Right True -> go rest (n + 1)
      Right False -> go rest n
      Left e -> Left e


cTransformList :: Compiled -> Text -> Text -> Maybe Compiled -> Compiled -> Compiled
cTransformList recvC v1 v2 mFilter transformC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  VList . V.fromList <$> go pairs
  where
    go [] = Right []
    go ((a, b) : rest) =
      let env' = bind2 v1 a v2 b env
      in case mFilter of
           Nothing -> do r <- transformC env'; (r :) <$> go rest
           Just p -> case asBool (p env') of
             Right True -> do r <- transformC env'; (r :) <$> go rest
             Right False -> go rest
             Left e -> Left e


cTransformMap :: Compiled -> Text -> Text -> Maybe Compiled -> Compiled -> Compiled
cTransformMap recvC kVar vVar mFilter transformC env = do
  v <- recvC env
  pairs <- rangeElems2 v
  out <- go pairs
  VMap <$> case v of
    -- Each list index occurs once, including after filtering.
    VList _ -> Right (celMapFromList out)
    _ -> either (Left . invalidArg) Right (celMap out)
  where
    go [] = Right []
    go ((k, val) : rest) =
      let env' = bind2 kVar k vVar val env
      in case mFilter of
           Nothing -> do nv <- transformC env'; ((k, nv) :) <$> go rest
           Just p -> case asBool (p env') of
             Right True -> do nv <- transformC env'; ((k, nv) :) <$> go rest
             Right False -> go rest
             Left e -> Left e


bind2 :: Text -> Value -> Text -> Value -> Env -> Env
bind2 v1 a v2 b = bindLocal v2 b . bindLocal v1 a


----------------------------------------------------------------------
-- Name resolution
----------------------------------------------------------------------

resolveName :: Env -> Bool -> [Text] -> Either CelError Value
resolveName env root segs
  | not root
  , (s0 : rest) <- segs
  , Just v <- lookupLocal s0 env =
      foldM selectField v rest
resolveName env root segs =
  case findDeclaration (length segs) of
    Just (val, rest) -> foldM selectField val rest
    Nothing -> Left (undeclared (T.intercalate "." segs))
  where
    prefixes = if root then [[]] else containerScopes (envContainer env)
    findDeclaration 0 = Nothing
    findDeclaration k =
      let prefix = take k segs
          rest = drop k segs
          candidates = map (\cp -> T.intercalate "." (cp ++ prefix)) prefixes
      in case firstJust (map lookupDeclaration candidates) of
           Just value -> Just (value, rest)
           Nothing -> findDeclaration (k - 1)
    lookupDeclaration name =
      lookupVar name env <|> builtinName name
        <|> (VType (TyMessage name) <$ lookupMessageType name env)
    builtinName "google.protobuf.NullValue.NULL_VALUE" = Just (VInt 0)
    builtinName name = VType <$> lookupTypeName name


containerScopes :: Text -> [[Text]]
containerScopes "" = [[]]
containerScopes c = reverse (inits (T.splitOn "." c))


lookupTypeName :: Text -> Maybe CelType
lookupTypeName = \case
  "int" -> Just TyInt
  "uint" -> Just TyUInt
  "double" -> Just TyDouble
  "bool" -> Just TyBool
  "string" -> Just TyString
  "bytes" -> Just TyBytes
  "list" -> Just TyList
  "map" -> Just TyMap
  "null_type" -> Just TyNull
  "type" -> Just TyType
  "google.protobuf.Timestamp" -> Just TyTimestamp
  "google.protobuf.Duration" -> Just TyDuration
  "google.protobuf.Any" -> Just (TyMessage "google.protobuf.Any")
  "google.protobuf.Empty" -> Just (TyMessage "google.protobuf.Empty")
  "google.protobuf.FieldMask" -> Just (TyMessage "google.protobuf.FieldMask")
  _ -> Nothing


firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest
