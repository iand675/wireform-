{- | Partition utilities: compute partition tuples from row values and project
row-level expressions through a 'PartitionSpec' to obtain partition-level
expressions for manifest pruning.

This module mirrors a subset of Java's
@org.apache.iceberg.expressions.Projections.inclusive(spec).project(expr)@
and the row-to-partition projection used by writers.
-}
module Iceberg.Partition (
  -- * Row -> partition tuple
  PartitionTuple (..),
  buildPartition,

  -- * Predicate projection
  inclusiveProject,
) where

import Avro.Value qualified as AV
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Iceberg.Expression qualified as E
import Iceberg.Murmur3 qualified as M
import Iceberg.Transform qualified as Tr
import Iceberg.Types


{- | A partition tuple is one transformed value per partition field, in
spec order. Values may be 'Nothing' when the source row has a null in a
nullable column.
-}
newtype PartitionTuple = PartitionTuple
  { unPartitionTuple :: Vector (Maybe AV.Value)
  }
  deriving (Show, Eq)


{- | Compute a row's partition tuple given a 'PartitionSpec' and a lookup
function from source field id to (typed) value. The lookup returns
'Nothing' for a missing field; the resulting partition slot is then
@Nothing@ and the file is taken to live in a "default" partition.
-}
buildPartition
  :: PartitionSpec
  -> Schema
  -> (Int -> Maybe AV.Value)
  -> Either Tr.TransformError PartitionTuple
buildPartition spec schema lookupSrc = do
  let mkSlot pf = case lookupSrc (pfPrimarySourceId pf) of
        Nothing -> Right Nothing
        Just v -> case sourceTypeOf schema (pfPrimarySourceId pf) of
          Just srcTy -> case Tr.applyTransform (pfTransform pf) srcTy v of
            Right out -> Right (Just out)
            Left e -> Left e
          Nothing -> Right Nothing
  slots <- V.mapM mkSlot (psFields spec)
  Right (PartitionTuple slots)


sourceTypeOf :: Schema -> Int -> Maybe IcebergType
sourceTypeOf schema fid =
  fmap sfType (V.find (\sf -> sfId sf == fid) (schemaFields schema))


-- ============================================================
-- Inclusive predicate projection
-- ============================================================

{- | Project a row-level 'E.Expression' through a 'PartitionSpec' to obtain a
partition-level expression that is /at least/ as permissive as the input
(any row matching the source predicate produces a partition tuple
matching the projected predicate). This is the safe direction used for
partition pruning.

Schema is needed to look up source-field types.
-}
inclusiveProject :: Schema -> PartitionSpec -> E.Expression -> E.Expression
inclusiveProject schema spec = go False
  where
    -- Push negation through to the row predicate first. Negating a
    -- projected predicate would negate a condition that may be weaker.
    go negated = \case
      E.ETrue -> if negated then E.EFalse else E.ETrue
      E.EFalse -> if negated then E.ETrue else E.EFalse
      E.EAnd a b -> combine (if negated then E.or_ else E.and_) a b
      E.EOr a b -> combine (if negated then E.and_ else E.or_) a b
      E.ENot e -> go (not negated) e
      E.EPredicate p ->
        let predicate = if negated then p {E.predOp = negateOp (E.predOp p)} else p
        in maybe E.ETrue id (projectPredicate schema spec predicate)
      where
        combine f a b = f (go negated a) (go negated b)


negateOp :: E.Operation -> E.Operation
negateOp = \case
  E.OpEq -> E.OpNotEq
  E.OpNotEq -> E.OpEq
  E.OpLt -> E.OpGtEq
  E.OpLtEq -> E.OpGt
  E.OpGt -> E.OpLtEq
  E.OpGtEq -> E.OpLt
  E.OpIsNull -> E.OpNotNull
  E.OpNotNull -> E.OpIsNull
  E.OpIsNan -> E.OpNotNan
  E.OpNotNan -> E.OpIsNan
  E.OpIn -> E.OpNotIn
  E.OpNotIn -> E.OpIn
  E.OpStartsWith -> E.OpNotStartsWith
  E.OpNotStartsWith -> E.OpStartsWith


projectPredicate :: Schema -> PartitionSpec -> E.Predicate -> Maybe E.Expression
projectPredicate schema spec p = do
  fid <- lookupFieldId schema (E.predField p)
  srcTy <- sourceTypeOf schema fid
  let matching = V.filter (\pf -> V.elem fid (pfSourceIds pf)) (psFields spec)
  if V.null matching
    then Nothing
    else
      Just $
        V.foldl'
          E.and_
          E.ETrue
          (V.mapMaybe (projectField srcTy p) matching)


lookupFieldId :: Schema -> Text -> Maybe Int
lookupFieldId schema name =
  fmap sfId (V.find (\sf -> sfName sf == name) (schemaFields schema))


projectField :: IcebergType -> E.Predicate -> PartitionField -> Maybe E.Expression
projectField srcTy p pf = case (E.predOp p, pfTransform pf) of
  (_, Void) -> Nothing
  (_, UnknownTransform _) -> Nothing
  (E.OpEq, Identity) -> projectIdentity p pf
  (E.OpNotEq, Identity) -> projectIdentityCmp p pf
  (E.OpEq, Truncate w) -> projectTruncEq srcTy p pf w
  (E.OpEq, Bucket n) -> projectBucketEq srcTy p pf n
  (E.OpLt, Identity) -> projectIdentityCmp p pf
  (E.OpLtEq, Identity) -> projectIdentityCmp p pf
  (E.OpGt, Identity) -> projectIdentityCmp p pf
  (E.OpGtEq, Identity) -> projectIdentityCmp p pf
  (E.OpIsNull, _) -> Just $ E.isNull (pfName pf)
  (E.OpNotNull, _) -> Just $ E.notNull (pfName pf)
  _ -> Nothing


projectIdentity :: E.Predicate -> PartitionField -> Maybe E.Expression
projectIdentity p pf = case V.toList (E.predLits p) of
  [lit] -> Just $ E.equal (pfName pf) lit
  _ -> Nothing


projectIdentityCmp :: E.Predicate -> PartitionField -> Maybe E.Expression
projectIdentityCmp p pf = case V.toList (E.predLits p) of
  [_lit] -> Just $ E.EPredicate (p {E.predField = pfName pf})
  _ -> Nothing


projectTruncEq :: IcebergType -> E.Predicate -> PartitionField -> Int -> Maybe E.Expression
projectTruncEq srcTy p pf width = case V.toList (E.predLits p) of
  [lit] -> fmap (E.equal (pfName pf)) (truncLiteral srcTy width lit)
  _ -> Nothing


projectBucketEq :: IcebergType -> E.Predicate -> PartitionField -> Int -> Maybe E.Expression
projectBucketEq srcTy p pf n = case V.toList (E.predLits p) of
  [lit] ->
    fmap
      (\k -> E.equal (pfName pf) (E.LInt (fromIntegral k)))
      (bucketLiteral srcTy n lit)
  _ -> Nothing


truncLiteral :: IcebergType -> Int -> E.Literal -> Maybe E.Literal
truncLiteral TInt w (E.LInt v) = Just (E.LInt (truncInt32 w v))
truncLiteral TLong w (E.LLong v) = Just (E.LLong (truncInt64 w v))
truncLiteral TString w (E.LString t) = Just (E.LString (T.take w t))
truncLiteral TBinary w (E.LBytes b) = Just (E.LBytes (BS.take w b))
truncLiteral _ _ _ = Nothing


bucketLiteral :: IcebergType -> Int -> E.Literal -> Maybe Int
bucketLiteral TInt n (E.LInt v) = Just (M.bucketLong n (fromIntegral v))
bucketLiteral TDate n (E.LInt v) = Just (M.bucketLong n (fromIntegral v))
bucketLiteral TTime n (E.LLong v) = Just (M.bucketLong n v)
bucketLiteral TLong n (E.LLong v) = Just (M.bucketLong n v)
bucketLiteral TTimestamp n (E.LLong v) = Just (M.bucketLong n v)
bucketLiteral TTimestampTz n (E.LLong v) = Just (M.bucketLong n v)
bucketLiteral TString n (E.LString t) = Just (M.bucketString n t)
bucketLiteral TBinary n (E.LBytes b) = Just (M.bucketBytes n b)
bucketLiteral TFixed {} n (E.LBytes b) = Just (M.bucketBytes n b)
bucketLiteral TUuid n (E.LBytes b) = Just (M.bucketBytes n b)
bucketLiteral _ _ _ = Nothing


truncInt32 :: Int -> Int32 -> Int32
truncInt32 w x = x - mod32 x (fromIntegral w)
  where
    mod32 a b = let r = a `mod` b in if r < 0 then r + b else r


truncInt64 :: Int -> Int64 -> Int64
truncInt64 w x = x - mod64 x (fromIntegral w)
  where
    mod64 a b = let r = a `mod` b in if r < 0 then r + b else r


-- Ensure ByteString is in scope so the 'truncLiteral' clause for binary
-- compiles cleanly even when the import is not otherwise referenced.
_unusedByteString :: ByteString -> ByteString
_unusedByteString = id
