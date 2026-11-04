{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | A vector builder scoped to one bounded parser. Its scheduler and
byte cursor remain unboxed; only the result values are allocated.
-}
module Ion.Internal.Vector (manyVector) where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import GHC.Exts
import GHC.IO (IO (..))
import Wireform.Parser.Internal


manyVector :: forall e a. Parser Pure e () -> Parser Pure e a -> Parser Pure e (Vector a)
manyVector (Parser prefix) (Parser item) = Parser $ \env end start state ->
  case (MV.unsafeNew 8 :: IO (MV.IOVector a)) of
    IO allocate -> case allocate state of
      (# state', buffer #) -> loop env end buffer 8 0 start state'
  where
    finish buffer count cursor state =
      case V.unsafeFreeze (MV.unsafeSlice 0 count buffer) of
        IO freeze -> case freeze state of
          (# state', vector #) -> (# state', OK# vector cursor #)
    loop env end !buffer !capacity !count cursor state =
      case prefix env end cursor state of
        (# state', OK# () next #) -> case eqAddr# next end of
          1# -> finish buffer count next state'
          _ -> case item env end next state' of
            (# state'', OK# value after #) ->
              case count < capacity of
                True -> write env end buffer capacity count value after state''
                False -> case MV.unsafeGrow buffer capacity of
                  IO grow -> case grow state'' of
                    (# state''', grown #) -> write env end grown (capacity * 2) count value after state'''
            (# state'', Fail# #) -> (# state'', Fail# #)
            (# state'', Err# err #) -> (# state'', Err# err #)
        (# state', Fail# #) -> (# state', Fail# #)
        (# state', Err# err #) -> (# state', Err# err #)
    write env end buffer capacity count value cursor state =
      case MV.unsafeWrite buffer count value of
        IO put -> case put state of
          (# state', () #) -> loop env end buffer capacity (count + 1) cursor state'
{-# INLINE manyVector #-}
