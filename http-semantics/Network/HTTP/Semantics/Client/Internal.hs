module Network.HTTP.Semantics.Client.Internal (
    Request (..),
    Response (..),
    Aux (..),
) where

import Network.HTTP.Semantics.Types (InpObj (..), OutObj (..))

-- | Request from client.
newtype Request = Request OutObj deriving (Show)

-- | Response from server.
newtype Response = Response InpObj deriving (Show)

-- | Additional information.
data Aux = Aux
    { auxPossibleClientStreams :: IO Int
    -- ^ How many streams can be created without blocking.
    }
