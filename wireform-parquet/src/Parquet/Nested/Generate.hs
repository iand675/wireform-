{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Internal strict vector construction without per-element Either, list
spines or deferred indexing closures.
-}
module Parquet.Nested.Generate (generate#) where

import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import GHC.Exts (Int (I#), Int#, runRW#, (+#), (==#))
import GHC.ST (ST (..))


-- Materialize only visited rows, with no boxed index or per-row Either.
-- Force each result before storing it, avoiding deferred indexing closures
-- that retain a whole input column after conversion.
generate# :: Int# -> (Int# -> (# String | a #)) -> (# String | V.Vector a #)
generate# count produce = runRW# $ \state ->
  case VM.unsafeNew (I# count) of
    ST allocate ->
      case allocate state of
        (# state1, output #) ->
          let go index state2 =
                case index ==# count of
                  1# -> case V.unsafeFreeze output of
                    ST freeze -> case freeze state2 of
                      (# _, result #) -> (# | result #)
                  _ -> case produce index of
                    (# message | #) -> (# message | #)
                    (# | value #) -> case value of
                      !strictValue -> case VM.unsafeWrite output (I# index) strictValue of
                        ST write -> case write state2 of
                          (# state3, () #) -> go (index +# 1#) state3
          in go 0# state1
{-# INLINE generate# #-}
