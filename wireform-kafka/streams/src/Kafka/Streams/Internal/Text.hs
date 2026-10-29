-- | Rendering numbers as 'Text' without going through 'String'.
module Kafka.Streams.Internal.Text (
  decimalText,
) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB


decimalText :: Integral a => a -> Text
decimalText = TL.toStrict . TB.toLazyText . TB.decimal
{-# INLINE decimalText #-}
