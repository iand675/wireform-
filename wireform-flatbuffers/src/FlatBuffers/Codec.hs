{-# LANGUAGE BangPatterns #-}

{- | Compile a parsed schema once, then reuse it with 'FlatBuffers.Encode.encodeWith'
and 'FlatBuffers.Decode.decodeWith'. Dynamic values follow schema declaration
order; explicit field IDs determine wire slots independently of that order.
-}
module FlatBuffers.Codec (CompiledSchema, compileSchema) where

import FlatBuffers.Internal.Schema (CompiledSchema, compileSchema)
