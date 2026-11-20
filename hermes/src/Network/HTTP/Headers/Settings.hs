{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP.Headers.Settings (
  HeaderSettings (HeaderSettings, maxHeaderSize),
  HeaderMessage (..),
  defaultHeaderSettings,
  headerDateCaseInsensitive,
  headerDateReference,
  headerMessage,
  withHeaderDateReference,
  withCaseInsensitiveDates,
  withHeaderMessage,
) where

import Data.Time.Clock (UTCTime)


{- | Settings for HTTP headers encoding and decoding.

Many browsers and web servers vary how they handle headers, so
this type allows you to define how you want to handle headers
in your application to accommodate these differences.
-}
data HeaderSettings = HeaderSettingsInternal Int !(Maybe UTCTime) !(Maybe HeaderMessage) !Bool


-- | The message carrying a field whose grammar differs by direction.
data HeaderMessage = HeaderRequest | HeaderResponse
  deriving stock (Eq, Show)


pattern HeaderSettings :: Int -> HeaderSettings
pattern HeaderSettings {maxHeaderSize} <- HeaderSettingsInternal maxHeaderSize _ _ _
  where
    HeaderSettings size = HeaderSettingsInternal size Nothing Nothing False


{-# COMPLETE HeaderSettings #-}


headerDateReference :: HeaderSettings -> Maybe UTCTime
headerDateReference (HeaderSettingsInternal _ ref _ _) = ref


headerMessage :: HeaderSettings -> Maybe HeaderMessage
headerMessage (HeaderSettingsInternal _ _ direction _) = direction


headerDateCaseInsensitive :: HeaderSettings -> Bool
headerDateCaseInsensitive (HeaderSettingsInternal _ _ _ folded) = folded


-- | Use the given clock reading to resolve two-digit years in HTTP dates.
withHeaderDateReference :: UTCTime -> HeaderSettings -> HeaderSettings
withHeaderDateReference now (HeaderSettingsInternal size _ direction folded) =
  HeaderSettingsInternal size (Just now) direction folded


-- | Match HTTP-date names and GMT case-insensitively, as HTTP caches can
-- do under RFC 9111 section 4.2. Rendering uses canonical capitalization.
withCaseInsensitiveDates :: HeaderSettings -> HeaderSettings
withCaseInsensitiveDates (HeaderSettingsInternal size ref direction _) =
  HeaderSettingsInternal size ref direction True


-- | Parse with the request or response grammar where a field defines both.
withHeaderMessage :: HeaderMessage -> HeaderSettings -> HeaderSettings
withHeaderMessage direction (HeaderSettingsInternal size ref _ folded) =
  HeaderSettingsInternal size ref (Just direction) folded


{- | Default settings for HTTP headers.

The default maximum header size is 8192 bytes, which is the maximum size
allowed by most web servers (excluding older NGINX builds, which support a max of 4kb per header).
If you need to support larger headers, you can increase this value, but be aware that some servers may reject
headers that are too large for security reasons.
-}
defaultHeaderSettings :: HeaderSettings
defaultHeaderSettings = HeaderSettings 8192
