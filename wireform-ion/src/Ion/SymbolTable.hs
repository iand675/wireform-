{-# LANGUAGE BangPatterns #-}
-- | Amazon Ion local symbol tables.
--
-- The Ion binary format addresses every symbol (both 'Ion.Value.Symbol' and
-- the field name in struct fields) by an integer @symbol id@ (SID). System
-- SIDs 1–9 are reserved for the @$ion_1_0@ system symbol table; user
-- symbols start at SID 10 and are declared by a @$ion_symbol_table@
-- annotation struct emitted before any values that use them.
--
-- This module factors out the symbol-table construction used by both the
-- encoder and the decoder so the wire-level details stay in one place.
module Ion.SymbolTable
  ( SymbolTable
  , empty
  , systemSidMax
  , internSymbol
  , lookupSid
  , lookupText
  , symbols
  , size
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | A growing local symbol table.  Symbol IDs (@SID@s) are returned by
-- 'internSymbol' and resolve to the original text via 'lookupText'.
data SymbolTable = SymbolTable
  { stByText :: !(Map Text Int)
  , stByIdx  :: !(Map Int Text)
  , stNext   :: !Int
  }
  deriving stock (Show, Eq)

-- | Highest SID reserved for the Ion system symbol table.
systemSidMax :: Int
systemSidMax = 9

-- | An empty user symbol table whose first allocated SID will be
-- @systemSidMax + 1 = 10@.
empty :: SymbolTable
empty = SymbolTable Map.empty Map.empty (systemSidMax + 1)

-- | Insert a text symbol if not already present, returning its SID.
internSymbol :: Text -> SymbolTable -> (Int, SymbolTable)
internSymbol !t !st =
  case Map.lookup t (stByText st) of
    Just sid -> (sid, st)
    Nothing  ->
      let !sid = stNext st
          !st' = st
            { stByText = Map.insert t sid (stByText st)
            , stByIdx  = Map.insert sid t (stByIdx st)
            , stNext   = sid + 1
            }
      in (sid, st')

-- | Look up a SID for a text. Returns 'Nothing' if the text was never
-- interned.
lookupSid :: Text -> SymbolTable -> Maybe Int
lookupSid !t !st = Map.lookup t (stByText st)

-- | Resolve a SID back to its text.  Returns 'Nothing' if the SID is
-- below 'systemSidMax' (i.e. a system SID outside this table) or
-- otherwise unknown.
lookupText :: Int -> SymbolTable -> Maybe Text
lookupText !sid !st = Map.lookup sid (stByIdx st)

-- | Total number of user symbols in the table.
size :: SymbolTable -> Int
size st = stNext st - (systemSidMax + 1)

-- | The user symbols in SID order (suitable for emission as the
-- @symbols@ field of an Ion local symbol table).
symbols :: SymbolTable -> [Text]
symbols !st = go (systemSidMax + 1)
  where
    go !sid = case Map.lookup sid (stByIdx st) of
      Just t  -> t : go (sid + 1)
      Nothing -> []
