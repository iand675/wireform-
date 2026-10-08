{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Types + 'Table's for the TH-deriver tests, kept in a
separate module so the Template-Haskell stage restriction
('reify' can't see declarations from the same module unless
separated by a splice barrier) doesn't force us to move the
assertion block below the types.
-}
module Test.Arrow.RecordTHTypes (
  TradeTH (..),
  TradeTHSnake (..),
  tradeTH,
  tradeTHSnake,
  sampleTradesTH,
) where

import Arrow.Record (Table)
import Arrow.Record.Generic ()
-- for HasEncoder / HasDecoder instances
import Arrow.Record.TH (deriveTable, deriveTableWith)
import Data.Char (toLower)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector qualified as V


-- TradeTH uses verbatim selector names as column names
-- (tradeSymTH, tradeQtyTH, tradeNoteTH).
data TradeTH = TradeTH
  { tradeSymTH :: !Text
  , tradeQtyTH :: !Int32
  , tradeNoteTH :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


-- TradeTHSnake uses (map toLower) via deriveTableWith, producing
-- "tradesymsnake" / "tradeqtysnake" / "tradenotesnake".
data TradeTHSnake = TradeTHSnake
  { tradeSymSnake :: !Text
  , tradeQtySnake :: !Int32
  , tradeNoteSnake :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


-- Close the declaration group so the splices below can reify
-- the types (TH stage restriction, 'reify' only sees
-- declarations from PREVIOUS groups).
$(pure [])


tradeTH :: Table TradeTH
tradeTH = $(deriveTable ''TradeTH)


tradeTHSnake :: Table TradeTHSnake
tradeTHSnake = $(deriveTableWith (map toLower) ''TradeTHSnake)


sampleTradesTH :: V.Vector TradeTH
sampleTradesTH =
  V.fromList
    [ TradeTH "AAPL" 100 (Just "initial")
    , TradeTH "GOOG" 50 Nothing
    ]
