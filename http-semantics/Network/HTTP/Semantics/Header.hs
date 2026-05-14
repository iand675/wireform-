module Network.HTTP.Semantics.Header (
    -- * Low-level headers.
    FieldName,
    FieldValue,
    TokenHeader,
    TokenHeaderList,
    TokenHeaderTable,

    -- * Value table
    ValueTable,
    getFieldValue,

    -- * Deprecated
    HeaderTable,
    HeaderValue,
    getHeaderValue,
) where

import Data.Array (Array)
import Data.Array.Base (unsafeAt)

import Network.HTTP.Semantics.Token

import Data.ByteString (ByteString)

-- | Field name. Internal usage only.
type FieldName = ByteString

-- | Field value.
type FieldValue = ByteString

{-# DEPRECATED HeaderValue "use FieldValue instead" #-}

-- | Header value.
type HeaderValue = ByteString

-- | An array to get 'FieldValue' quickly.
--   'getHeaderValue' should be used.
--   Internally, the key is 'tokenIx'.
type ValueTable = Array Int (Maybe FieldValue)

{-# DEPRECATED HeaderTable "use TokenHeaderTable instead" #-}

-- | A pair of token list and value table.
type HeaderTable = (TokenHeaderList, ValueTable)

-- | A pair of token list and value table.
type TokenHeaderTable = (TokenHeaderList, ValueTable)

-- | TokenBased header.
type TokenHeader = (Token, FieldValue)

-- | TokenBased header list.
type TokenHeaderList = [TokenHeader]

{-# DEPRECATED getHeaderValue "use geFieldValue instead" #-}

-- | Accessing 'FieldValue' with 'Token'.
{-# INLINE getHeaderValue #-}
getHeaderValue :: Token -> ValueTable -> Maybe FieldValue
getHeaderValue t tbl = tbl `unsafeAt` tokenIx t

-- | Accessing 'FieldValue' with 'Token'.
{-# INLINE getFieldValue #-}
getFieldValue :: Token -> ValueTable -> Maybe FieldValue
getFieldValue t tbl = tbl `unsafeAt` tokenIx t
