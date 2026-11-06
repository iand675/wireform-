{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Longest-prefix lookup over the generated WHATWG reference table.
module HTML.Entities (matchNamedReference) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Internal qualified as MI
import Data.Map.Strict qualified as Map
import HTML.Entities.Generated qualified as Generated


data Trie = Trie !(Maybe String) !(Map.Map Char Trie)


empty :: Trie
empty = Trie Nothing Map.empty


references :: Trie
references = foldl' (\trie (name, value) -> insert name value trie) empty Generated.entries
  where
    insert [] value (Trie _ children) = Trie (Just value) children
    insert (c : cs) value (Trie terminal children) =
      Trie terminal (Map.alter (Just . insert cs value . maybe empty id) c children)
{-# NOINLINE references #-}


-- Map.lookup would box a Just for every consumed character. This small walk
-- uses the same balanced tree with an unboxed result on the matching path.
lookupChild :: Char -> Map.Map Char Trie -> (# Trie | (# #) #)
lookupChild key = go
  where
    go MI.Tip = (# | (# #) #)
    go (MI.Bin _ candidate value left right) = case compare key candidate of
      LT -> go left
      EQ -> (# value | #)
      GT -> go right
{-# INLINE lookupChild #-}


{- | Input starts after @&@. Retain the last match as references into the
existing strings, without prefix strings, Maybe wrappers or fallback thunks.
Only legacy (semicolon-free) terminals apply the attribute restriction.
-}
matchNamedReference :: Bool -> String -> (String -> String -> a) -> a -> a
matchNamedReference inAttribute input accept reject = go references input [] [] False
  where
    finish value rest found = if found then accept value rest else reject
    go (Trie terminal children) rest best bestRest found = case terminal of
      Just value | allowed rest -> step children rest value rest True
      _ -> step children rest best bestRest found
    step _ [] best bestRest found = finish best bestRest found
    step children (c : cs) best bestRest found = case lookupChild c children of
      (# | (# #) #) -> finish best bestRest found
      (# child | #) ->
        if c == ';'
          then case child of
            Trie (Just value) _ -> accept value cs
            _ -> finish best bestRest found
          else go child cs best bestRest found
    allowed [] = True
    allowed (c : _) = not inAttribute || not (c == '=' || isAsciiLower c || isAsciiUpper c || isDigit c)
{-# INLINE matchNamedReference #-}
