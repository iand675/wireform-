{-# LANGUAGE FlexibleInstances #-}

{- | Mason-compatible builder surface, backed by
@Wireform.Builder@ from @wireform-core@.

Hermes was originally written against the 'Mason.Builder' API.
For the wireform stack we want a single chunked-byte builder
across all packages (@Wireform.Builder@), so this module
re-exposes the small slice of Mason that hermes uses, with
identical names, on top of the wireform builder.

This is an internal module: callers should keep importing it as
@qualified Network.HTTP.Headers.Mason as M@ where they previously
imported @qualified Mason.Builder as M@. Everything mason-shaped
(@'Buildable'@, @'BuilderFor'@) collapses to the monomorphic
'Wireform.Builder.Builder'; the type-class machinery is preserved
purely for source compatibility.
-}
module Network.HTTP.Headers.Mason (
  -- * Builder type (mason-compatible aliases)
  Builder,
  BuilderFor,
  Buildable,

  -- * Running
  toStrictByteString,

  -- * Basic primitives
  byteString,
  word8,
  char7,
  char8,
  string8,

  -- * Numeric primitives
  intDec,
  integerDec,
  wordDec,
  doubleDec,
  word16Dec,
  word32Dec,

  -- * Padding helpers
  intDecPadded,

  -- * Bytes / Text
  shortByteString,
  textUtf8,

  -- * Sequencing
  intersperse,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Short as SBS
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word8)
import qualified Wireform.Builder as WB
import qualified Wireform.Builder.Internal.Prim as WBP


-- ---------------------------------------------------------------------------
-- Type compatibility
-- ---------------------------------------------------------------------------

{- | Mason's 'Mason.Builder.Builder'. Aliased to the wireform builder
so existing hermes code reads the same.
-}
type Builder = WB.Builder


{- | Mason's 'Mason.Builder.BuilderFor', collapsed onto the
monomorphic wireform builder. The type parameter is preserved as
a phantom so call sites with @forall s. Buildable s => BuilderFor
s@ continue to type-check; nothing in the implementation cares
about it.
-}
type BuilderFor s = WB.Builder


{- | Vacuous class: every type is 'Buildable'. Preserves the
@'Buildable' s =>@ constraints that hermes signatures carry from
the mason era.
-}
class Buildable a


instance Buildable a


-- ---------------------------------------------------------------------------
-- Running
-- ---------------------------------------------------------------------------

toStrictByteString :: Builder -> BS.ByteString
toStrictByteString = WB.toStrictByteString
{-# INLINE toStrictByteString #-}


-- ---------------------------------------------------------------------------
-- Basic primitives
-- ---------------------------------------------------------------------------

byteString :: BS.ByteString -> Builder
byteString = WB.byteString
{-# INLINE byteString #-}


word8 :: Word8 -> Builder
word8 = WB.word8
{-# INLINE word8 #-}


char7 :: Char -> Builder
char7 = WB.char7
{-# INLINE char7 #-}


char8 :: Char -> Builder
char8 = WBP.char8
{-# INLINE char8 #-}


string8 :: String -> Builder
string8 = WBP.string8
{-# INLINE string8 #-}


-- ---------------------------------------------------------------------------
-- Numeric primitives
-- ---------------------------------------------------------------------------

intDec :: Int -> Builder
intDec = WBP.intDec
{-# INLINE intDec #-}


integerDec :: Integer -> Builder
integerDec = WBP.integerDec
{-# INLINE integerDec #-}


wordDec :: Word -> Builder
wordDec = WBP.wordDec
{-# INLINE wordDec #-}


doubleDec :: Double -> Builder
doubleDec = WBP.doubleDec
{-# INLINE doubleDec #-}


word16Dec :: Word16 -> Builder
word16Dec = WBP.word16Dec
{-# INLINE word16Dec #-}


word32Dec :: Word32 -> Builder
word32Dec = WBP.word32Dec
{-# INLINE word32Dec #-}


-- ---------------------------------------------------------------------------
-- Padding
-- ---------------------------------------------------------------------------

{- | @intDecPadded width n@ renders @n@ as decimal, left-padded with
ASCII @'0'@ to at least @width@ characters. Used in HTTP date
formatting where every component is zero-padded.
-}
intDecPadded :: Int -> Int -> Builder
intDecPadded width n =
  let s = show n
      pad = replicate (width - length s) '0'
  in WBP.string7 (pad <> s)


-- ---------------------------------------------------------------------------
-- Short bytes / Text
-- ---------------------------------------------------------------------------

shortByteString :: SBS.ShortByteString -> Builder
shortByteString = WB.byteString . SBS.fromShort
{-# INLINE shortByteString #-}


-- | Encode 'Text' as UTF-8 into the builder.
textUtf8 :: Text -> Builder
textUtf8 = WB.byteString . TE.encodeUtf8
{-# INLINE textUtf8 #-}


-- ---------------------------------------------------------------------------
-- Sequencing
-- ---------------------------------------------------------------------------

{- | @intersperse sep xs@ concatenates the builders in @xs@,
inserting @sep@ between each pair. Equivalent to
@mconcat . Data.List.intersperse sep . Data.Foldable.toList@.
-}
intersperse :: Foldable f => Builder -> f Builder -> Builder
intersperse sep xs = mconcat (List.intersperse sep (F.toList xs))
{-# INLINE intersperse #-}


-- Pin the imports we keep for posterity, even if a future refactor
-- drops them from the public surface.
_unusedP :: P.BoundedPrim Char
_unusedP = P.charUtf8
