{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Read-only symbol lookup hints for repeated record shapes. Every hit
checks the actual text; changed names and heterogeneous values fall back
to the ordinary symbol table. Hints never assign or assume a new SID.
-}
module Ion.Internal.SymbolPlan (
  Plan (..),
  FieldPlan (..),
  build,
  listItem,
  fieldAt,
  annotationItem,
  intern,
  lookupID,
  internField,
  fieldID,
  fieldItem,
) where

import Control.Monad.ST (runST)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Ion.SymbolTable qualified as ST
import Ion.Value qualified as I


data Plan
  = Plain
  | Items !Plan
  | Fields !(Vector FieldPlan)
  | Symbol !Text {-# UNPACK #-} !Int
  | Annotation !Text {-# UNPACK #-} !Int !Plan


data FieldPlan = NoField | Field !Text {-# UNPACK #-} !Int !Plan


data BuiltFields = BuiltFields !(Vector FieldPlan) !ST.EncoderSymbols


-- Only the first item of each list supplies hints; the full sizing pass
-- still visits every item and interns every symbol not already known.
build :: ST.EncoderSymbols -> I.Value -> (# Plan, ST.EncoderSymbols #)
build table = \case
  I.Symbol text -> case ST.internID text table of
    (# sid, next #) -> (# Symbol text sid, next #)
  I.Annotation text value -> case ST.internID text table of
    (# sid, next #) -> case build next value of
      (# child, final #) -> (# Annotation text sid child, final #)
  I.List values
    | V.null values -> (# Plain, table #)
    | otherwise -> case build table (V.unsafeHead values) of
        (# child, next #) -> (# Items child, next #)
  I.Struct fields -> case buildFields table fields of
    BuiltFields hints next -> (# Fields hints, next #)
  _ -> (# Plain, table #)


buildFields :: ST.EncoderSymbols -> Vector (Text, I.Value) -> BuiltFields
buildFields initial values = runST $ do
  output <- MV.unsafeNew (V.length values)
  let go !i !table
        | i >= V.length values = do
            hints <- V.unsafeFreeze output
            pure (BuiltFields hints table)
        | otherwise = case V.unsafeIndex values i of
            (text, value) -> case ST.internID text table of
              (# sid, next #) -> case build next value of
                (# child, final #) -> do
                  MV.unsafeWrite output i (Field text sid child)
                  go (i + 1) final
  go 0 initial


listItem :: Plan -> Plan
listItem (Items child) = child
listItem _ = Plain
{-# INLINE listItem #-}


annotationItem :: Plan -> Plan
annotationItem (Annotation _ _ child) = child
annotationItem _ = Plain
{-# INLINE annotationItem #-}


fieldAt :: Plan -> Int -> FieldPlan
fieldAt (Fields hints) i | i < V.length hints = V.unsafeIndex hints i
fieldAt _ _ = NoField
{-# INLINE fieldAt #-}


fieldItem :: FieldPlan -> Plan
fieldItem (Field _ _ child) = child
fieldItem NoField = Plain
{-# INLINE fieldItem #-}


intern :: Plan -> Text -> ST.EncoderSymbols -> (# Int, ST.EncoderSymbols #)
intern (Symbol expected sid) actual table | expected == actual = (# sid, table #)
intern (Annotation expected sid _) actual table | expected == actual = (# sid, table #)
intern _ actual table = ST.internID actual table
{-# INLINE intern #-}


lookupID :: Plan -> Text -> ST.EncoderSymbols -> Int
lookupID (Symbol expected sid) actual _ | expected == actual = sid
lookupID (Annotation expected sid _) actual _ | expected == actual = sid
lookupID _ actual table = ST.symbolID table actual
{-# INLINE lookupID #-}


internField :: FieldPlan -> Text -> ST.EncoderSymbols -> (# Int, ST.EncoderSymbols #)
internField (Field expected sid _) actual table | expected == actual = (# sid, table #)
internField _ actual table = ST.internID actual table
{-# INLINE internField #-}


fieldID :: FieldPlan -> Text -> ST.EncoderSymbols -> Int
fieldID (Field expected sid _) actual _ | expected == actual = sid
fieldID _ actual table = ST.symbolID table actual
{-# INLINE fieldID #-}
