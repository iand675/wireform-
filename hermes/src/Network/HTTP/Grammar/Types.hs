{- |
Shared value types for the HTTP wire-grammar substrate.

These are the pure data types used by both the parser
("Network.HTTP.Grammar.Parser") and the renderer
("Network.HTTP.Grammar.Rendering"). They carry no parsing or
rendering logic and require no TemplateHaskell, so modules that only
need the shapes (e.g. renderers, or downstream code that pattern-matches
on an 'ItemValue') can depend on this module without pulling in the
flatparse \/ TH machinery.

These types are /not/ header-specific: they are the RFC 8941
(Structured Fields) and RFC 9110 value vocabulary reused by every
header module and — in time — by non-header grammars (URIs, mailboxes,
…) that live under @"Network.HTTP.Grammar.*"@.
-}
module Network.HTTP.Grammar.Types (
  -- * Structured Fields (RFC 9651)
  ItemValue (..),
  StructuredFieldParameter (..),
  StructuredFieldItem (..),
  StructuredFieldMember (..),
  StructuredFieldDictionaryEntry (..),
  orderedFieldEntries,

  -- * Opaque RFC 8941 string / token
  RFC8941String (..),
  RFC8941Token (..),

  -- * RFC 9110 comment
  Comment (..),
) where

import Data.ByteString (ByteString)
import Data.Fixed (Milli)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (ShortText)


{- | An RFC 9651 §3.3 @item@: the union of bare structured-field item
values. The 'ItemValueType' GADT (see "Network.HTTP.Grammar.Parser")
reifies a /single/ one of these legs for type-discriminated parsing.
-}
data ItemValue
  = Integer !Int
  | Decimal !Milli
  | String !RFC8941String
  | Token !RFC8941Token
  | Binary !ByteString
  | Boolean !Bool
  | DateValue {-# UNPACK #-} !Int
  | DisplayString !Text
  deriving stock (Eq, Show)


data StructuredFieldParameter = StructuredFieldParameter
  { structuredParameterKey :: !ShortText
  , structuredParameterValue :: !ItemValue
  }
  deriving stock (Eq, Show)


data StructuredFieldItem = StructuredFieldItem
  { structuredItemValue :: !ItemValue
  , structuredItemParameters :: ![StructuredFieldParameter]
  }
  deriving stock (Eq, Show)


data StructuredFieldMember
  = StructuredItem !StructuredFieldItem
  | StructuredInnerList ![StructuredFieldItem] ![StructuredFieldParameter]
  deriving stock (Eq, Show)


data StructuredFieldDictionaryEntry = StructuredFieldDictionaryEntry
  { structuredDictionaryKey :: !ShortText
  , structuredDictionaryMember :: !StructuredFieldMember
  }
  deriving stock (Eq, Show)


{- | An RFC 8941 §3.3.3 @sf-string@. The payload is the decoded text;
use 'Network.HTTP.Grammar.Parser.mkRFC8941String' to validate that a
given 'ShortText' is a legal string body before wrapping it.
-}
newtype RFC8941String = RFC8941String {unsafeToRFC8941String :: ShortText}
  deriving stock (Eq, Show)


{- | An RFC 8941 §3.3.4 @sf-token@. The payload is the raw token text;
use 'Network.HTTP.Grammar.Parser.mkRFC8941Token' to validate it first.
-}
newtype RFC8941Token = RFC8941Token {unsafeToRFC8941Token :: ShortText}
  deriving stock (Eq, Show)


{- | The opaque content of an RFC 9110 §5.6.5 @comment@. The parser
removes the outer parentheses and quoting backslashes. Nested
parentheses remain in the text, and wire bytes map to Latin-1.
-}
newtype Comment = Comment {fromComment :: Text}
  deriving stock (Eq, Show)


-- | Keep the first key position and its last value.
orderedFieldEntries :: Ord key => (a -> key) -> [a] -> [a]
orderedFieldEntries key = \case
  entries@[] -> entries
  entries@[_] -> entries
  entries@[a, b]
    | key a == key b -> [b]
    | otherwise -> entries
  entries@[a, b, c]
    | key a /= key b && key a /= key c && key b /= key c -> entries
  entries@[a, b, c, d]
    | let ka = key a
          kb = key b
          kc = key c
          kd = key d
    , ka /= kb && ka /= kc && ka /= kd && kb /= kc && kb /= kd && kc /= kd -> entries
  entries -> emit (foldl' (\values entry -> Map.insert (key entry) entry values) Map.empty entries) entries
  where
    emit _ [] = []
    emit latest (entry : rest) =
      case Map.updateLookupWithKey (\_ _ -> Nothing) (key entry) latest of
        (Just value, remaining) -> value : emit remaining rest
        (Nothing, remaining) -> emit remaining rest
{-# SPECIALIZE orderedFieldEntries :: (a -> ShortText) -> [a] -> [a] #-}
