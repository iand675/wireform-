{- |
Module      : Symbol
Description : Interned strings with O(1) equality and hashing.

A 'Symbol' is a string stored once: while it is alive, every 'intern' of
the same characters returns the same symbol, so '==' is a pointer
comparison and 'hash' is a field read. Unused symbols are reclaimed by the
garbage collector. Use it for small strings that recur and get compared or
hashed a lot: HTTP header names, field names, identifiers.

>>> import qualified Symbol
>>> let a = Symbol.intern ("content-type" :: Text)
>>> a == Symbol.intern ("content-type" :: ByteString)
True
>>> Symbol.unintern a :: Text
"content-type"

The table behind it is keyed by SipHash-1-3 under a key drawn from the OS
at start-up, so filling it with attacker-chosen strings cannot force
collisions; it is split into independently locked shards, and lookups of
existing symbols copy nothing. See "Symbol.Internal" for the design.
-}
module Symbol
  ( Symbol
  , Textual
  , intern
  , internIO
  , lookup
  , unintern
  , symbolLength
  , symbolByteArray
  , symbolTableSize
  , sweepSymbolTable
  ) where

import Symbol.Internal
import Prelude ()
