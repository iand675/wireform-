{-# LANGUAGE PatternSynonyms #-}

{- | HTTP protocol versions.

Re-exports the canonical 'Network.HTTP.Versions.HTTPVersion' from
@hermes@ under this package's @Version@ name; the whole wireform HTTP
stack shares one version type.
-}
module Network.HTTP.Types.Version (
  Version,
  mkVersion,
  versionMajor,
  versionMinor,
  versionToBytes,
  versionFromBytes,

  -- * Common versions
  pattern HTTP0_9,
  pattern HTTP1_0,
  pattern HTTP1_1,
  pattern HTTP2,
  pattern HTTP3,
) where

import Network.HTTP.Versions (
  HTTPVersion,
  mkVersion,
  versionFromBytes,
  versionMajor,
  versionMinor,
  versionToBytes,
  pattern HTTP0_9,
  pattern HTTP1_0,
  pattern HTTP1_1,
  pattern HTTP2,
  pattern HTTP3,
 )


-- | The shared HTTP version type (see "Network.HTTP.Versions").
type Version = HTTPVersion
