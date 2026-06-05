-- | Border definitions, a port of lipgloss's @borders.go@.
module TTY.Style.Border
  ( Border (..)
  , noBorder
  , normalBorder
  , roundedBorder
  , blockBorder
  , outerHalfBlockBorder
  , innerHalfBlockBorder
  , thickBorder
  , doubleBorder
  , hiddenBorder
  , markdownBorder
  , asciiBorder
  , getTopSize
  , getRightSize
  , getBottomSize
  , getLeftSize
  , maxRuneWidth
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import TTY.Ansi (runeWidth)


-- | The runes that make up the various parts of a border. Empty 'Text' means
-- "no rune for this part".
data Border = Border
  { btTop :: Text
  , btBottom :: Text
  , btLeft :: Text
  , btRight :: Text
  , btTopLeft :: Text
  , btTopRight :: Text
  , btBottomLeft :: Text
  , btBottomRight :: Text
  , btMiddleLeft :: Text
  , btMiddleRight :: Text
  , btMiddle :: Text
  , btMiddleTop :: Text
  , btMiddleBottom :: Text
  }
  deriving stock (Eq, Show)


emptyBorder :: Border
emptyBorder = Border "" "" "" "" "" "" "" "" "" "" "" "" ""


-- | The cell width of a border edge (the widest of its parts).
getTopSize :: Border -> Int
getTopSize b = getBorderEdgeWidth [btTopLeft b, btTop b, btTopRight b]


getRightSize :: Border -> Int
getRightSize b = getBorderEdgeWidth [btTopRight b, btRight b, btBottomRight b]


getBottomSize :: Border -> Int
getBottomSize b = getBorderEdgeWidth [btBottomLeft b, btBottom b, btBottomRight b]


getLeftSize :: Border -> Int
getLeftSize b = getBorderEdgeWidth [btTopLeft b, btLeft b, btBottomLeft b]


getBorderEdgeWidth :: [Text] -> Int
getBorderEdgeWidth = foldr (max . maxRuneWidth) 0


-- | The widest single rune in a string.
maxRuneWidth :: Text -> Int
maxRuneWidth = T.foldl' (\acc c -> max acc (runeWidth c)) 0


noBorder :: Border
noBorder = emptyBorder


normalBorder :: Border
normalBorder =
  emptyBorder
    { btTop = "\x2500"
    , btBottom = "\x2500"
    , btLeft = "\x2502"
    , btRight = "\x2502"
    , btTopLeft = "\x250c"
    , btTopRight = "\x2510"
    , btBottomLeft = "\x2514"
    , btBottomRight = "\x2518"
    , btMiddleLeft = "\x251c"
    , btMiddleRight = "\x2524"
    , btMiddle = "\x253c"
    , btMiddleTop = "\x252c"
    , btMiddleBottom = "\x2534"
    }


roundedBorder :: Border
roundedBorder =
  normalBorder
    { btTopLeft = "\x256d"
    , btTopRight = "\x256e"
    , btBottomLeft = "\x2570"
    , btBottomRight = "\x256f"
    }


blockBorder :: Border
blockBorder = Border b b b b b b b b b b b b b
  where
    b = "\x2588"


outerHalfBlockBorder :: Border
outerHalfBlockBorder =
  emptyBorder
    { btTop = "\x2580"
    , btBottom = "\x2584"
    , btLeft = "\x258c"
    , btRight = "\x2590"
    , btTopLeft = "\x259b"
    , btTopRight = "\x259c"
    , btBottomLeft = "\x2599"
    , btBottomRight = "\x259f"
    }


innerHalfBlockBorder :: Border
innerHalfBlockBorder =
  emptyBorder
    { btTop = "\x2584"
    , btBottom = "\x2580"
    , btLeft = "\x2590"
    , btRight = "\x258c"
    , btTopLeft = "\x2597"
    , btTopRight = "\x2596"
    , btBottomLeft = "\x259d"
    , btBottomRight = "\x2598"
    }


thickBorder :: Border
thickBorder =
  emptyBorder
    { btTop = "\x2501"
    , btBottom = "\x2501"
    , btLeft = "\x2503"
    , btRight = "\x2503"
    , btTopLeft = "\x250f"
    , btTopRight = "\x2513"
    , btBottomLeft = "\x2517"
    , btBottomRight = "\x251b"
    , btMiddleLeft = "\x2523"
    , btMiddleRight = "\x252b"
    , btMiddle = "\x254b"
    , btMiddleTop = "\x2533"
    , btMiddleBottom = "\x253b"
    }


doubleBorder :: Border
doubleBorder =
  emptyBorder
    { btTop = "\x2550"
    , btBottom = "\x2550"
    , btLeft = "\x2551"
    , btRight = "\x2551"
    , btTopLeft = "\x2554"
    , btTopRight = "\x2557"
    , btBottomLeft = "\x255a"
    , btBottomRight = "\x255d"
    , btMiddleLeft = "\x2560"
    , btMiddleRight = "\x2563"
    , btMiddle = "\x256c"
    , btMiddleTop = "\x2566"
    , btMiddleBottom = "\x2569"
    }


hiddenBorder :: Border
hiddenBorder = Border s s s s s s s s s s s s s
  where
    s = " "


markdownBorder :: Border
markdownBorder = Border "-" "-" "|" "|" "|" "|" "|" "|" "|" "|" "|" "|" "|"


asciiBorder :: Border
asciiBorder = Border "-" "-" "|" "|" "+" "+" "+" "+" "+" "+" "+" "+" "+"
