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
  -- * RFC 8941 structured-field item
  ItemValue (..),
  -- * Opaque RFC 8941 string / token
  RFC8941String (..),
  RFC8941Token (..),
  -- * RFC 9110 comment
  Comment (..),
) where

import Data.ByteString (ByteString)
import Data.Fixed (Milli)
import Data.Text (Text)
import Data.Text.Short (ShortText)


-- | An RFC 8941 §3.3 @item@: the union of bare structured-field item
-- values. The 'ItemValueType' GADT (see "Network.HTTP.Grammar.Parser")
-- reifies a /single/ one of these legs for type-discriminated parsing.
data ItemValue
  = Integer !Int
  | Decimal !Milli
  | String !RFC8941String
  | Token !RFC8941Token
  | Binary !ByteString
  | Boolean !Bool
  deriving stock (Eq, Show)


-- | An RFC 8941 §3.3.3 @sf-string@. The payload is the decoded text;
-- use 'Network.HTTP.Grammar.Parser.mkRFC8941String' to validate that a
-- given 'ShortText' is a legal string body before wrapping it.
newtype RFC8941String = RFC8941String {unsafeToRFC8941String :: ShortText}
  deriving stock (Eq, Show)


-- | An RFC 8941 §3.3.4 @sf-token@. The payload is the raw token text;
-- use 'Network.HTTP.Grammar.Parser.mkRFC8941Token' to validate it first.
newtype RFC8941Token = RFC8941Token {unsafeToRFC8941Token :: ShortText}
  deriving stock (Eq, Show)


-- | An RFC 9110 §5.6.5 @comment@. Comments are opaque text; the parser
-- in "Network.HTTP.Grammar.Parser" returns the raw 'Text' between the
-- parens and this newtype is the typed wrapper for callers that want
-- to carry one around.
newtype Comment = Comment {fromComment :: Text}
  deriving stock (Eq, Show)
