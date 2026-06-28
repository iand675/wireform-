{- | HTTP request methods.

Re-exports the canonical 'Network.HTTP.Methods.Method' from @hermes@;
the whole wireform HTTP stack shares one method type. This module
adds the wrapper names this package's API has always used
('fromMethod', 'isSafe', 'isIdempotent', 'bodyAllowedInRequest') on
top of hermes' @method*@ functions.
-}
module Network.HTTP.Types.Method (
  module Network.HTTP.Methods,
  fromMethod,
  isSafe,
  isIdempotent,
  bodyAllowedInRequest,
) where

import Data.ByteString (ByteString)
import Network.HTTP.Methods


-- | The on-the-wire token bytes of a method (alias for 'methodToBytes').
fromMethod :: Method -> ByteString
fromMethod = methodToBytes


-- | RFC 9110 \"safe\" methods: GET \/ HEAD \/ OPTIONS \/ TRACE.
isSafe :: Method -> Bool
isSafe = methodIsSafe


-- | RFC 9110 \"idempotent\" methods.
isIdempotent :: Method -> Bool
isIdempotent = methodIsIdempotent


-- | Whether a request with this method may carry content.
bodyAllowedInRequest :: Method -> Bool
bodyAllowedInRequest = methodBodyAllowedInRequest
