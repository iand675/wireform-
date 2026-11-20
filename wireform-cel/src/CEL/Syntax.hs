{-# LANGUAGE DeriveLift #-}

{- | Abstract syntax tree for CEL expressions.

The tree closely mirrors the grammar in the CEL language definition. Binary
operators are kept as dedicated constructors ('EArith', 'ERel', 'EAnd',
'EOr', 'ECond', 'ENot', 'ENeg') rather than being desugared into calls to
the @_+_@ style internal function names; this keeps short-circuiting and
error-absorbing semantics explicit in the evaluator. Plain and
receiver-style function calls (including macro invocations) are 'ECall'.
-}
module CEL.Syntax (
  Expr (..),
  Literal (..),
  ArithOp (..),
  RelOp (..),
  identPath,
) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Language.Haskell.TH.Syntax (Lift)


{- | Literal constants. Integer literals are always non-negative as produced by
the lexer; negation is a separate unary operator per the spec.
-}
data Literal
  = LNull
  | LBool !Bool
  | LInt !Int64
  | LUInt !Word64
  | LDouble !Double
  | LString !Text
  | LBytes !ByteString
  deriving stock (Eq, Show, Lift)


-- | Arithmetic operators (precedence 3 and 4).
data ArithOp = Add | Sub | Mul | Div | Mod
  deriving stock (Eq, Show, Lift)


-- | Relational / membership operators (precedence 5).
data RelOp = Eq | Ne | Lt | Le | Gt | Ge | In
  deriving stock (Eq, Show, Lift)


data Expr
  = -- | A literal constant.
    ELit !Literal
  | {- | An identifier. The 'Bool' is 'True' when the name was written with a
    leading @.@, forcing resolution in the root scope.
    -}
    EIdent !Bool !Text
  | -- | Field selection @e.field@.
    ESelect !Expr !Text
  | -- | Indexing @e[i]@.
    EIndex !Expr !Expr
  | {- | A function or macro call. @'Just' recv@ is written as @recv.f(args)@.
    'Nothing' is a global call, whose name retains a leading dot for a
    root-qualified call such as @.f(args)@.
    -}
    ECall !(Maybe Expr) !Text ![Expr]
  | -- | List literal @[e1, e2, ...]@.
    EList ![Expr]
  | -- | Map literal @{k1: v1, ...}@.
    EMap ![(Expr, Expr)]
  | {- | Message/struct literal @Name{f1: e1, ...}@. The 'Bool' marks a leading
    @.@ and the @['Text']@ is the dotted type name.
    -}
    EStruct !Bool ![Text] ![(Text, Expr)]
  | -- | Conditional (ternary) @c ? t : f@.
    ECond !Expr !Expr !Expr
  | -- | Logical AND (error-absorbing, commutative).
    EAnd !Expr !Expr
  | -- | Logical OR (error-absorbing, commutative).
    EOr !Expr !Expr
  | -- | Logical NOT.
    ENot !Expr
  | -- | Arithmetic negation.
    ENeg !Expr
  | -- | Binary arithmetic.
    EArith !ArithOp !Expr !Expr
  | -- | Relational / membership.
    ERel !RelOp !Expr !Expr
  deriving stock (Eq, Show, Lift)


{- | If an expression is a (possibly dotted) plain identifier chain such as
@a.b.c@, return its leading-dot flag and the segments. Used by name
resolution to find the longest bound prefix.
-}
identPath :: Expr -> Maybe (Bool, [Text])
identPath (EIdent root n) = Just (root, [n])
identPath (ESelect e f) = do
  (root, segs) <- identPath e
  Just (root, segs ++ [f])
identPath _ = Nothing
