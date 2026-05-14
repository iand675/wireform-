-- | HTTP/2 client (stub — server conformance testing is the priority).
module Network.HTTP2.New.Client where

-- Client implementation is out of scope for the initial h2spec conformance
-- pass.  The server implementation covers all the protocol state machine
-- and frame-handling logic needed by h2spec.

data ClientConfig = ClientConfig

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
