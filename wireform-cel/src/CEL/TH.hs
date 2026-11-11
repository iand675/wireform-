{-# LANGUAGE TemplateHaskell #-}

{- | Compile CEL to Haskell at /compile time/.

Two levels are offered:

  * 'cel' / 'compileCel' parse the CEL at compile time and splice the
    resulting 'CEL.Syntax.Expr' as a baked-in constant (no runtime parse;
    evaluation still walks the AST once via 'CEL.evaluate').

  * 'celFn' / 'compileCelFn' go further and emit the fully-compiled program
    as Haskell: each CEL node becomes a direct call to the corresponding
    "CEL.Eval" combinator, producing a @'Env' -> 'Either' 'CelError'
    'Value'@ closure with no AST walk and no per-node dispatch at runtime.
    GHC compiles and optimizes the result like any other Haskell.

A CEL syntax error in any of these becomes a compile error.

@
{\-# LANGUAGE QuasiQuotes #-\}
import CEL
import CEL.TH (celFn)

check :: Env -> Either CelError Value
check = [celFn| this.size() >= 3 && this.startsWith('x') |]
@
-}
module CEL.TH (
  -- * Compile to a baked-in AST
  cel,
  compileCel,

  -- * Compile to Haskell (a 'CEL.Eval.Compiled' closure)
  celFn,
  compileCelFn,
  compileCelExprFn,
) where

import CEL.Eval
import CEL.Parser (parse)
import CEL.Syntax
import Data.Text qualified as T
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Language.Haskell.TH (Exp, Q, listE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)


----------------------------------------------------------------------
-- AST-baking splice
----------------------------------------------------------------------

-- | Parse CEL at compile time and yield the baked-in 'Expr'.
compileCel :: String -> Q Exp
compileCel src = case parseSrc src of
  Left err -> fail err
  Right expr -> lift expr


{- | Expression quasiquoter producing a compile-time-parsed 'Expr':
@[cel| 1 + 2 |]@.
-}
cel :: QuasiQuoter
cel = exprQuoter compileCel


----------------------------------------------------------------------
-- Compile-to-Haskell splice
----------------------------------------------------------------------

{- | Parse CEL at compile time and emit the fully-compiled program as a
'CEL.Eval.Compiled' (@Env -> Either CelError Value@) — each node becomes a
direct combinator call, so there is no runtime AST walk.
-}
compileCelFn :: String -> Q Exp
compileCelFn src = case parseSrc src of
  Left err -> fail err
  Right expr -> compileCelExprFn expr


-- | Emit a compiled program from an already-parsed expression.
compileCelExprFn :: Expr -> Q Exp
compileCelExprFn = emit


{- | Expression quasiquoter producing a fully-compiled closure:
@[celFn| this.size() >= 3 |]@.
-}
celFn :: QuasiQuoter
celFn = exprQuoter compileCelFn


----------------------------------------------------------------------
-- Emitter: Expr -> Haskell (combinator tree)
----------------------------------------------------------------------

emit :: Expr -> Q Exp
emit expr = case expr of
  ELit (LDouble d) -> [|cLit (LDouble (castWord64ToDouble $(lift (castDoubleToWord64 d))))|]
  ELit l -> [|cLit $(lift l)|]
  EIdent root name -> [|cName $(lift root) $(lift [name])|]
  ESelect e f -> case identPath (ESelect e f) of
    Just (root, segs) -> [|cName $(lift root) $(lift segs)|]
    Nothing -> [|cSelect $(emit e) $(lift f)|]
  EIndex e i -> [|cIndex $(emit e) $(emit i)|]
  EList es -> [|cList $(listE (map emit es))|]
  EMap entries -> [|cMapLit $(listE (map emitEntry entries))|]
  EStruct root segs fields -> [|cStructWithFields $(lift root) $(lift segs) $(listE (map emitField fields))|]
  ECond c t e -> [|cCond $(emit c) $(emit t) $(emit e)|]
  EAnd a b -> [|cAnd $(emit a) $(emit b)|]
  EOr a b -> [|cOr $(emit a) $(emit b)|]
  ENot e -> [|cNot $(emit e)|]
  ENeg e -> [|cNeg $(emit e)|]
  EArith op a b -> [|cArith $(lift op) $(emit a) $(emit b)|]
  ERel op a b -> [|cRel $(lift op) $(emit a) $(emit b)|]
  ECall recv name args -> emitCall recv name args
  where
    emitEntry (k, v) = [|($(emit k), $(emit v))|]
    emitField (name, expression) = [|($(lift name), $(emit expression))|]


emitCall :: Maybe Expr -> T.Text -> [Expr] -> Q Exp
emitCall Nothing "has" [ESelect e f] = [|cHas $(emit e) $(lift f)|]
emitCall Nothing "has" [_] = [|cHasInvalid|]
emitCall (Just recv) name args
  | Just q <- emitMacro recv name args = q
  | Just (root, segments) <- identPath recv =
      [|cQualifiedCall $(lift root) $(lift (T.intercalate "." segments)) $(lift name) $(emit recv) $(listE (map emit args))|]
emitCall recv name args =
  [|cCall $(emitMaybe recv) $(lift name) $(listE (map emit args))|]
  where
    emitMaybe Nothing = [|Nothing|]
    emitMaybe (Just r) = [|Just $(emit r)|]


emitMacro :: Expr -> T.Text -> [Expr] -> Maybe (Q Exp)
emitMacro recv name args = case (name, args) of
  ("all", [EIdent _ v, p]) -> Just [|cAll $(emit recv) $(lift v) $(emit p)|]
  ("exists", [EIdent _ v, p]) -> Just [|cExists $(emit recv) $(lift v) $(emit p)|]
  ("exists_one", [EIdent _ v, p]) -> Just [|cExistsOne $(emit recv) $(lift v) $(emit p)|]
  ("existsOne", [EIdent _ v, p]) -> Just [|cExistsOne $(emit recv) $(lift v) $(emit p)|]
  ("filter", [EIdent _ v, p]) -> Just [|cFilter $(emit recv) $(lift v) $(emit p)|]
  ("map", [EIdent _ v, t]) -> Just [|cMapMacro $(emit recv) $(lift v) Nothing $(emit t)|]
  ("map", [EIdent _ v, p, t]) -> Just [|cMapMacro $(emit recv) $(lift v) (Just $(emit p)) $(emit t)|]
  ("all", [EIdent _ a, EIdent _ b, p]) -> Just [|cAll2 $(emit recv) $(lift a) $(lift b) $(emit p)|]
  ("exists", [EIdent _ a, EIdent _ b, p]) -> Just [|cExists2 $(emit recv) $(lift a) $(lift b) $(emit p)|]
  ("exists_one", [EIdent _ a, EIdent _ b, p]) -> Just [|cExistsOne2 $(emit recv) $(lift a) $(lift b) $(emit p)|]
  ("existsOne", [EIdent _ a, EIdent _ b, p]) -> Just [|cExistsOne2 $(emit recv) $(lift a) $(lift b) $(emit p)|]
  ("transformList", [EIdent _ a, EIdent _ b, t]) -> Just [|cTransformList $(emit recv) $(lift a) $(lift b) Nothing $(emit t)|]
  ("transformList", [EIdent _ a, EIdent _ b, p, t]) -> Just [|cTransformList $(emit recv) $(lift a) $(lift b) (Just $(emit p)) $(emit t)|]
  ("transformMap", [EIdent _ a, EIdent _ b, t]) -> Just [|cTransformMap $(emit recv) $(lift a) $(lift b) Nothing $(emit t)|]
  ("transformMap", [EIdent _ a, EIdent _ b, p, t]) -> Just [|cTransformMap $(emit recv) $(lift a) $(lift b) (Just $(emit p)) $(emit t)|]
  _ -> Nothing


----------------------------------------------------------------------
-- Shared
----------------------------------------------------------------------

parseSrc :: String -> Either String Expr
parseSrc src = case parse (T.pack src) of
  Left err -> Left ("CEL.TH: parse error: " <> err)
  Right expr -> Right expr


exprQuoter :: (String -> Q Exp) -> QuasiQuoter
exprQuoter q =
  QuasiQuoter
    { quoteExp = q
    , quotePat = \_ -> fail "CEL.TH: cannot be used as a pattern"
    , quoteType = \_ -> fail "CEL.TH: cannot be used as a type"
    , quoteDec = \_ -> fail "CEL.TH: cannot be used as a declaration"
    }
