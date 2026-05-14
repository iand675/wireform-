-- | Standalone HTTP/2 server for manual testing with h2spec or curl.
module Main where

import qualified Data.ByteString.Char8 as BC
import Network.Socket
import System.IO (hPutStrLn, stderr)

import Network.HTTP2.New
import Network.HTTP2.New.Types

main :: IO ()
main = do
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just "8080")
    let addr = head addrs
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128
    hPutStrLn stderr "wireform-http2 server listening on :8080"
    run defaultServerConfig sock $ \_ -> return ResponseUnary
        { rspStatus   = 200
        , rspHeaders  = [("content-type", "text/plain")]
        , rspBody     = BC.pack "hello from wireform-http2"
        , rspTrailers = []
        }
