module Network.HTTP.Headers.Settings where


{- | Settings for HTTP headers encoding and decoding.

Many browsers and web servers vary how they handle headers, so
this type allows you to define how you want to handle headers
in your application to accommodate these differences.
-}
newtype HeaderSettings = HeaderSettings
  { maxHeaderSize :: Int
  }


{- | Default settings for HTTP headers.

The default maximum header size is 8192 bytes, which is the maximum size
allowed by most web servers (excluding older NGINX builds, which support a max of 4kb per header).
If you need to support larger headers, you can increase this value, but be aware that some servers may reject
headers that are too large for security reasons.
-}
defaultHeaderSettings :: HeaderSettings
defaultHeaderSettings = HeaderSettings 8192
