{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | JSON Pointer (RFC 6901) implementation
--
-- JSON Pointers are a standard way to identify a specific value within
-- a JSON document. They are used extensively in JSON Schema for referencing
-- locations in schemas and instances.
--
-- Examples:
-- * @""@ - points to the root
-- * @"/foo"@ - points to the value of property "foo" at the root
-- * @"/foo/0"@ - points to the first element of array "foo"
-- * @"/foo/bar"@ - points to the value of property "bar" within object "foo"
module JsonPointer
  ( -- * JSON Pointer Type
    JsonPointer(..)
    -- * Construction
  , emptyPointer
  , (/.)
    -- * Serialization
  , renderPointer
  , parsePointer
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Network.URI (unEscapeString)

-- | JSON Pointer (RFC 6901) for identifying locations in JSON documents
--
-- Represented as a sequence of reference tokens (path segments).
-- The empty list represents the root of the document.
newtype JsonPointer = JsonPointer [Text]
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, Hashable)
  deriving stock Lift

-- | Empty JSON Pointer (root)
--
-- >>> emptyPointer
-- JsonPointer []
emptyPointer :: JsonPointer
emptyPointer = JsonPointer []

-- | Append a segment to a JSON Pointer
--
-- >>> emptyPointer /. "foo" /. "bar"
-- JsonPointer ["foo","bar"]
(/.) :: JsonPointer -> Text -> JsonPointer
(JsonPointer segments) /. seg = JsonPointer (segments <> [seg])

infixl 5 /.

-- | Render JSON Pointer to standard string format (RFC 6901)
--
-- Escapes special characters according to RFC 6901:
-- * @~@ becomes @~0@
-- * @/@ becomes @~1@
--
-- >>> renderPointer emptyPointer
-- ""
-- >>> renderPointer (emptyPointer /. "foo" /. "bar")
-- "/foo/bar"
-- >>> renderPointer (emptyPointer /. "a/b" /. "c~d")
-- "/a~1b/c~0d"
renderPointer :: JsonPointer -> Text
renderPointer (JsonPointer []) = ""
renderPointer (JsonPointer segments) = "/" <> T.intercalate "/" (map escapeSegment segments)
  where
    -- IMPORTANT: Must escape ~ before / to avoid double-escaping
    -- First: ~ → ~0
    -- Then: / → ~1
    escapeSegment seg = T.replace "/" "~1" $ T.replace "~" "~0" seg

-- | Parse JSON Pointer from string (RFC 6901)
--
-- Also handles URL-encoded pointers (e.g., %25 → %)
--
-- >>> parsePointer ""
-- Right (JsonPointer [])
-- >>> parsePointer "/foo/bar"
-- Right (JsonPointer ["foo","bar"])
-- >>> parsePointer "/a~1b/c~0d"
-- Right (JsonPointer ["a/b","c~d"])
-- >>> parsePointer "foo"
-- Left "JSON Pointer must start with /"
parsePointer :: Text -> Either Text JsonPointer
parsePointer "" = Right emptyPointer
parsePointer txt
  | T.head txt /= '/' = Left "JSON Pointer must start with /"
  | otherwise = Right $ JsonPointer $ map unescapeSegment $ T.splitOn "/" (T.tail txt)
  where
    -- IMPORTANT: Must unescape ~1 before ~0 to avoid double-unescaping
    -- First: URL decode (if pointer was in a URI fragment)
    -- Then: ~1 → /
    -- Finally: ~0 → ~
    unescapeSegment seg = 
      let urlDecoded = urlDecode seg
          step1 = T.replace "~1" "/" urlDecoded
          step2 = T.replace "~0" "~" step1
      in step2
    
    -- URL decode percent-encoded sequences
    urlDecode s = T.pack $ unEscapeString (T.unpack s)

