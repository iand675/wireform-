{-# LANGUAGE LambdaCase #-}

{- | Format-agnostic predicate vocabulary for columnar storage.

The same scalar (@'PValue'@) + leaf-predicate (@'PColPredicate'@)
+ boolean-tree (@'Predicate'@) shapes power skip decisions in
both Parquet and ORC. Lifting this into the shared 'Columnar'
package lets the per-format predicate evaluators (Parquet's
row-group / page-index / bloom modules + ORC's stripe-level
statistics) reuse the same caller-facing API and the same
soundness guarantee:

  * 'PSkip' is only ever returned when the evaluator can /prove/
    that no row in the slice satisfies the predicate.
  * Ambiguity (missing statistics, types we don't compare
    cross-type) degrades to 'PMaybeKeep' so the caller decodes
    normally.

See "Parquet.Predicate" for the Parquet-specific evaluators
and "ORC.Statistics" for the ORC-specific ones.
-}
module Columnar.Predicate (
  -- * Predicate vocabulary
  Predicate (..),
  PColPredicate (..),
  PValue (..),

  -- * Evaluation results
  Decision (..),
  combineDecisions,

  -- * Range-based evaluation
  evalRange,
  pvLess,
  pvLessEq,
  pvEq,
) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)


-- | A typed scalar that can appear in a predicate.
data PValue
  = PVInt32 !Int32
  | PVInt64 !Int64
  | PVFloat !Float
  | PVDouble !Double
  | PVBool !Bool
  | PVText !Text
  | PVBinary !ByteString
  | PVUnsigned !Word64
  | PVDecimal !Rational
  deriving (Show, Eq)


-- | Per-column predicate (the leaf node of 'Predicate').
data PColPredicate
  = PEq !PValue
  | PNeq !PValue
  | PLt !PValue
  | PLtEq !PValue
  | PGt !PValue
  | PGtEq !PValue
  | {- | Membership in a (small) literal set. Treated as an OR
    of 'PEq' for stats-based skipping; consumed directly by
    bloom-filter checks.
    -}
    PIn ![PValue]
  | PIsNull
  | PIsNotNull
  deriving (Show, Eq)


{- | Tree of column predicates with boolean structure. The
evaluator pushes 'PAnd' to product and 'POr' to sum
pessimistically (so the answer is always a sound /super/-set
of the true-row predicate's answer).
-}
data Predicate
  = PCol !Text !PColPredicate
  | PAnd !Predicate !Predicate
  | POr !Predicate !Predicate
  | PNot !Predicate
  | PTrue
  | PFalse
  deriving (Show, Eq)


{- | Skipping decision for one row group / page / column chunk.

'PSkip' means "no row in this slice can satisfy the
predicate; you may skip it without decoding". 'PMaybeKeep'
means "we can't prove it's safe to skip; you must decode".
The evaluator never produces 'PSkip' with false negatives.
-}
data Decision
  = PSkip
  | PMaybeKeep
  deriving (Show, Eq)


{- | AND-combine two decisions: "if /either/ sub-predicate
proves the slice can be skipped, the conjunction can too".
-}
combineDecisions :: Decision -> Decision -> Decision
combineDecisions PSkip _ = PSkip
combineDecisions _ PSkip = PSkip
combineDecisions _ _ = PMaybeKeep


{- | Evaluate a leaf predicate against an inclusive @[mn, mx]@
value range. The format-specific layer is responsible for
decoding statistics into 'PValue's and calling this; the
range comparison itself is shared.
-}
evalRange :: PValue -> PValue -> PColPredicate -> Decision
evalRange mn mx predicate
  | pvLess mx mn = PMaybeKeep
  | otherwise = case predicate of
      PEq v -> if pvLess v mn || pvLess mx v then PSkip else PMaybeKeep
      PNeq _ -> PMaybeKeep
      PLt v -> if pvLessEq v mn then PSkip else PMaybeKeep
      PLtEq v -> if pvLess v mn then PSkip else PMaybeKeep
      PGt v -> if pvLessEq mx v then PSkip else PMaybeKeep
      PGtEq v -> if pvLess mx v then PSkip else PMaybeKeep
      PIn vs ->
        if all (\v -> pvLess v mn || pvLess mx v) vs
          then PSkip
          else PMaybeKeep
      PIsNull -> PMaybeKeep
      PIsNotNull -> PMaybeKeep


{- | Strict 'PValue' ordering. Cross-type comparisons return
'False' (the conservative "we don't know" answer that
propagates as 'PMaybeKeep' up the call chain).
-}
pvLess :: PValue -> PValue -> Bool
pvLess (PVInt32 a) (PVInt32 b) = a < b
pvLess (PVInt64 a) (PVInt64 b) = a < b
pvLess (PVFloat a) (PVFloat b) = a < b
pvLess (PVDouble a) (PVDouble b) = a < b
pvLess (PVBool a) (PVBool b) = not a && b
pvLess (PVText a) (PVText b) = TE.encodeUtf8 a < TE.encodeUtf8 b
pvLess (PVBinary a) (PVBinary b) = a < b
pvLess (PVUnsigned a) (PVUnsigned b) = a < b
pvLess (PVDecimal a) (PVDecimal b) = a < b
pvLess _ _ = False


pvLessEq :: PValue -> PValue -> Bool
pvLessEq a b = pvLess a b || pvEq a b


pvEq :: PValue -> PValue -> Bool
pvEq = (==)
