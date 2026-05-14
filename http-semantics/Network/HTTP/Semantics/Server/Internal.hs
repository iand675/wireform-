module Network.HTTP.Semantics.Server.Internal (
    Request (..),
    Response (..),
    Aux (..),
) where

import Network.Socket (SockAddr)
import qualified System.TimeManager as T

import Network.HTTP.Semantics.Types (InpObj (..), OutObj (..))

-- | Request from client.
newtype Request = Request InpObj deriving (Show)

-- | Response from server.
newtype Response = Response OutObj deriving (Show)

-- | Additional information.
data Aux = Aux
    { auxTimeHandle :: T.Handle
    -- ^ Time handle for the worker processing this request and response.
    , auxMySockAddr :: SockAddr
    -- ^ Local socket address copied from 'Config'.
    , auxPeerSockAddr :: SockAddr
    -- ^ Remove socket address copied from 'Config'.
    }
