-- | Flow control re-exports (the implementation is in 'Types').
module Network.HTTP2.New.Flow
    ( TxWindow(..)
    , newTxWindow
    , waitTxWindow
    , addTxWindow
    ) where

import Network.HTTP2.New.Types
