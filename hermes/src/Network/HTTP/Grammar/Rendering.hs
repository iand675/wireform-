{- |
Renderers for the HTTP wire-grammar substrate — the inverse of
"Network.HTTP.Grammar.Parser".

This module holds the RFC 8941 (Structured Fields) and shared builder
primitives that every header renderer (and, in time, non-header
grammars) consumes. It is free of TemplateHaskell and depends only on
the pure value types in "Network.HTTP.Grammar.Types".

Header-/policy/-specific rendering (e.g. splitting a field value to fit
a maximum header size) stays in "Network.HTTP.Headers.Rendering.Util".
-}
module Network.HTTP.Grammar.Rendering (
  -- * Builder primitives
  shortText,
  sepByCommas1,

  -- * RFC 8941 item renderers
  rfc8941Integer,
  rfc8941Decimal,
  rfc8941Boolean,
  rfc8941Binary,
  rfc8941String,
  rfc8941Token,
  rfc8941ItemValue,

  -- * RFC 8941 parameters
  EmptyParameterRenderMode (..),
  rfc8941Parameter,
) where

import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.Fixed (Fixed (..), Milli)
import qualified Data.Foldable1 as F1
import Data.Text.Short (ShortText, toShortByteString)
import qualified Data.Text.Short as TS
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Grammar.Types (ItemValue (..), RFC8941String (..), RFC8941Token (..))


sepByCommas1 :: F1.Foldable1 t => t M.Builder -> M.Builder
sepByCommas1 = F1.intercalate1 ", "
{-# INLINE sepByCommas1 #-}


shortText :: ShortText -> M.Builder
shortText = M.shortByteString . toShortByteString
{-# INLINE shortText #-}


rfc8941Integer :: Int -> M.Builder
rfc8941Integer = M.intDec
{-# INLINE rfc8941Integer #-}


rfc8941Boolean :: Bool -> M.Builder
rfc8941Boolean True = "?0"
rfc8941Boolean False = "?1"
{-# INLINE rfc8941Boolean #-}


rfc8941Binary :: ByteString -> M.Builder
rfc8941Binary bs = M.char7 ':' <> M.byteString (convertToBase Base64 bs)
{-# INLINE rfc8941Binary #-}


rfc8941String :: RFC8941String -> M.Builder
rfc8941String (RFC8941String t) =
  M.char7 '"' <> escapedChars <> M.char7 '"'
  where
    -- RFC 8941 §4.1.5 and RFC 9110 §5.6.4 both require that
    -- DQUOTE and backslash inside the body be escaped with a
    -- leading backslash. The unescaped variant that lived here
    -- previously round-tripped values like @realm=\"foo\\\"bar\"@
    -- as @realm=\"foo\"bar\"@, which is a parse error on the wire.
    escapedChars :: M.Builder
    escapedChars =
      TS.foldl'
        ( \b c -> case c of
            '"' -> b <> M.char7 '\\' <> M.char7 '"'
            '\\' -> b <> M.char7 '\\' <> M.char7 '\\'
            c' -> b <> M.char7 c'
        )
        mempty
        t
{-# INLINE rfc8941String #-}


rfc8941Token :: RFC8941Token -> M.Builder
rfc8941Token = shortText . unsafeToRFC8941Token
{-# INLINE rfc8941Token #-}


rfc8941Decimal :: Milli -> M.Builder
rfc8941Decimal (MkFixed i) = case i `divMod` 1000 of
  (d, m) -> M.integerDec d <> M.char7 '.' <> M.integerDec m
{-# INLINE rfc8941Decimal #-}


rfc8941ItemValue :: ItemValue -> M.Builder
rfc8941ItemValue = \case
  Integer i -> rfc8941Integer i
  Decimal d -> rfc8941Decimal d
  Boolean b -> rfc8941Boolean b
  Binary bs -> rfc8941Binary bs
  String t -> rfc8941String t
  Token t -> rfc8941Token t
{-# INLINE rfc8941ItemValue #-}


data EmptyParameterRenderMode = IncludeIfEmpty | ExcludeIfEmpty


rfc8941Parameter :: EmptyParameterRenderMode -> (a -> M.Builder) -> ShortText -> Maybe a -> M.Builder
rfc8941Parameter IncludeIfEmpty f k = \case
  Nothing -> M.char7 ';' <> shortText k
  Just v -> M.char7 ';' <> shortText k <> M.char7 '=' <> f v
rfc8941Parameter ExcludeIfEmpty f k = \case
  Nothing -> mempty
  Just v -> M.char7 ';' <> shortText k <> M.char7 '=' <> f v
{-# INLINE rfc8941Parameter #-}
