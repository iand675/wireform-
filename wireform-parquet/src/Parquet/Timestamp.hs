{- | Lossless modern signed96 TIMESTAMP and legacy Julian-day INT96 values.

The Arrow bridge infers distinct wireform extension fields backed by exact
12-byte storage. Use 'projectTimestampColumn' or the bridge's schema projection
for checked conversion to an ordinary Arrow timestamp. These extensions are
wireform-specific, not canonical Apache Arrow extension types.
-}
module Parquet.Timestamp (
  Timestamp96 (..),
  TimestampEncoding (..),
  timestamp96FromInteger,
  timestamp96ToInteger,
  encodeTimestamp96,
  decodeTimestamp96,
  decodeJulianInt96,
  timestampField,
  timestampEncoding,
  projectTimestampColumn,
) where

import Parquet.Timestamp.Internal
