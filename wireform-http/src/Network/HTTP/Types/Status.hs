{-# LANGUAGE PatternSynonyms #-}

{- | HTTP status codes.

Re-exports the canonical 'Network.HTTP.Status.StatusCode' from
@hermes@ under this package's @Status@ name; the whole wireform HTTP
stack shares one status type. A @Status@ pattern synonym preserves
the existing @Status 200@ construction / matching, and the
@statusNNN@ constants, category predicates, @statusCategory@, and
@statusReason@ all come from hermes' shared table.
-}
module Network.HTTP.Types.Status (
  module Network.HTTP.Status,
  Status,
  pattern Status,
) where

import Data.Word (Word16)
import Network.HTTP.Status


-- | The shared HTTP status type (see "Network.HTTP.Status").
type Status = StatusCode


-- | Construct / match a status by numeric code. Bidirectional, so
-- @Status 200@ builds and @case s of Status w -> ...@ matches.
pattern Status :: Word16 -> Status
pattern Status w = StatusCode w


{-# COMPLETE Status #-}
