{-# LANGUAGE PatternSynonyms #-}

{- | HTTP protocol version for the HTTP\/1.x layer.

A thin view over the canonical 'Network.HTTP.Versions.HTTPVersion'.
HTTP\/1.x only ever speaks @1.0@ or @1.1@ on the wire (RFC 9112);
'versionFromBytes' is therefore strict — anything but @HTTP\/1.0@ or
@HTTP\/1.1@ is rejected, so a newer-protocol token never sneaks
through the 1.x request\/status line.
-}
module Network.HTTP1.Version (
  Version,
  pattern HTTP_1_0,
  pattern HTTP_1_1,
  versionToBytes,
  versionFromBytes,
) where

import Data.ByteString (ByteString)
import Network.HTTP.Versions (HTTPVersion, pattern HTTP1_0, pattern HTTP1_1)
import qualified Network.HTTP.Versions as HV


-- | The shared HTTP version type (see "Network.HTTP.Versions").
type Version = HTTPVersion


-- | HTTP\/1.0.
pattern HTTP_1_0 :: Version
pattern HTTP_1_0 = HTTP1_0


-- | HTTP\/1.1.
pattern HTTP_1_1 :: Version
pattern HTTP_1_1 = HTTP1_1


-- | Render the canonical on-the-wire spelling. For the @1.x@ versions
-- this layer produces, this is @HTTP\/1.0@ or @HTTP\/1.1@.
{-# INLINE versionToBytes #-}
versionToBytes :: Version -> ByteString
versionToBytes = HV.versionToBytes


{- | Strict parse: requires exactly @HTTP\/1.0@ or @HTTP\/1.1@. Anything
else (including HTTP\/2 \/ 3 spellings) is 'Nothing'.
-}
{-# INLINE versionFromBytes #-}
versionFromBytes :: ByteString -> Maybe Version
versionFromBytes bs
  | bs == "HTTP/1.1" = Just HTTP_1_1
  | bs == "HTTP/1.0" = Just HTTP_1_0
  | otherwise = Nothing
