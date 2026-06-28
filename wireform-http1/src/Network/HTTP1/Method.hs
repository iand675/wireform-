{- | HTTP request methods for the HTTP\/1.x layer.

This is a thin re-export of the canonical 'Network.HTTP.Methods.Method'
from @hermes@; the whole wireform HTTP stack shares one method type.
-}
module Network.HTTP1.Method (
  Method (..),
  methodFromBytes,
  methodToBytes,
  methodIsSafe,
  methodIsIdempotent,
  methodBodyAllowedInRequest,
) where

import Network.HTTP.Methods (
  Method (..),
  methodBodyAllowedInRequest,
  methodFromBytes,
  methodIsIdempotent,
  methodIsSafe,
  methodToBytes,
 )
