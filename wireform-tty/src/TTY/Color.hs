-- | The terminal color model, a port of @muesli/termenv@'s color handling.
--
-- A 'Profile' describes the terminal's color capability; a 'Color' is a
-- concrete terminal color (an ANSI 16 index, an ANSI 256 index, or a 24-bit
-- truecolor hex). 'profileColor' parses a color spec and degrades it to the
-- given profile, using the perceptual HSLuv distance ("TTY.Color.HSLuv") for
-- truecolor -> 256 -> 16 conversion, exactly like termenv.
module TTY.Color
  ( Profile (..)
  , Color (..)
  , profileColor
  , convert
  , sequenceColor
  , convertToRGB
  , hexToRGB
  , ansiHex
  , rgbaOf
  ) where

import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import TTY.Color.HSLuv (RGB (..), distanceHSLuv)


-- | A terminal's color capability.
data Profile
  = Ascii -- ^ no color
  | ANSI -- ^ 16 colors (4-bit)
  | ANSI256 -- ^ 256 colors (8-bit)
  | TrueColor -- ^ 24-bit color
  deriving stock (Eq, Ord, Show, Enum, Bounded)


-- | A concrete terminal color.
data Color
  = NoColor
  | ANSIColor Int -- ^ 0..15
  | ANSI256Color Int -- ^ 16..255 (also accepts 0..255)
  | RGBColor Text -- ^ @"#rrggbb"@
  deriving stock (Eq, Show)


-- | Parse a color spec (@"#rrggbb"@, @"#rgb"@, or a decimal ANSI index) and
-- degrade it to the target profile. Returns 'Nothing' for empty/invalid input
-- (matching termenv's @nil@), so callers can skip emitting a sequence.
profileColor :: Profile -> Text -> Maybe Color
profileColor p s
  | T.null s = Nothing
  | T.head s == '#' = Just (convert p (RGBColor s))
  | otherwise = case readDecimal s of
      Just i
        | i < 16 -> Just (convert p (ANSIColor i))
        | otherwise -> Just (convert p (ANSI256Color i))
      Nothing -> Nothing


-- | Degrade a color to the given profile.
convert :: Profile -> Color -> Color
convert Ascii _ = NoColor
convert _ NoColor = NoColor
convert _ c@(ANSIColor _) = c
convert p c@(ANSI256Color v)
  | p == ANSI = ansi256ToANSI v
  | otherwise = c
convert p c@(RGBColor hex)
  | p == TrueColor = c
  | otherwise =
      let ac = hexToANSI256 (hexToRGB hex)
       in if p == ANSI then ansi256ToANSI ac else ANSI256Color ac


-- | The SGR sub-sequence for a color (the part between @CSI@ and @m@). The
-- @bg@ flag selects background (48/+10) vs foreground (38). 'NoColor' yields
-- the empty string.
sequenceColor :: Bool -> Color -> Text
sequenceColor _ NoColor = ""
sequenceColor bg (ANSIColor c)
  | c < 8 = tshow (bgMod c + 30)
  | otherwise = tshow (bgMod (c - 8) + 90)
  where
    bgMod x = if bg then x + 10 else x
sequenceColor bg (ANSI256Color c) =
  (if bg then "48" else "38") <> ";5;" <> tshow c
sequenceColor bg (RGBColor hex) =
  let RGB r g b = hexToRGB hex
      toByte v = truncate (v * 255) :: Int
   in (if bg then "48" else "38")
        <> ";2;"
        <> tshow (toByte r)
        <> ";"
        <> tshow (toByte g)
        <> ";"
        <> tshow (toByte b)


-- | A color as an sRGB triple in @[0, 1]@ (termenv's @ConvertToRGB@).
convertToRGB :: Color -> RGB
convertToRGB = \case
  RGBColor hex -> hexToRGB hex
  ANSIColor v -> hexToRGB (ansiHex v)
  ANSI256Color v -> hexToRGB (ansiHex v)
  NoColor -> RGB 0 0 0


-- | The 16-bit-per-channel RGBA of a color (go-colorful's @RGBA@): each
-- channel is @round(c * 65535)@, alpha is @0xFFFF@.
rgbaOf :: Color -> (Int, Int, Int, Int)
rgbaOf c =
  let RGB r g b = convertToRGB c
      q v = floor (v * 65535.0 + 0.5) :: Int
   in (q r, q g, q b, 0xFFFF)


-- | Parse a hex color (@"#rrggbb"@ or @"#rgb"@) to an 'RGB' using the same
-- @1\/255@ factor as go-colorful, so the (lossy) round-trip back to bytes
-- matches termenv exactly.
hexToRGB :: Text -> RGB
hexToRGB t
  | T.length t == 7 && T.head t == '#' =
      RGB (chan 1 2) (chan 3 4) (chan 5 6)
  | T.length t == 4 && T.head t == '#' =
      RGB (chan1 1) (chan1 2) (chan1 3)
  | otherwise = RGB 0 0 0
  where
    factor = 1.0 / 255.0 :: Double
    factor1 = 1.0 / 15.0 :: Double
    h i = hexDigit (T.index t i)
    chan i j = fromIntegral (h i * 16 + h j) * factor
    chan1 i = fromIntegral (h i) * factor1


hexDigit :: Char -> Int
hexDigit ch
  | ch >= '0' && ch <= '9' = ord ch - ord '0'
  | ch >= 'a' && ch <= 'f' = ord ch - ord 'a' + 10
  | ch >= 'A' && ch <= 'F' = ord ch - ord 'A' + 10
  | otherwise = 0


readDecimal :: Text -> Maybe Int
readDecimal s
  | not (T.null s) && T.all (\c -> c >= '0' && c <= '9') s =
      Just (T.foldl' (\acc c -> acc * 10 + (ord c - ord '0')) 0 s)
  | otherwise = Nothing


-- Color degradation ---------------------------------------------------------

-- | Nearest ANSI-256 index to an sRGB color (termenv's @hexToANSI256Color@).
hexToANSI256 :: RGB -> Int
hexToANSI256 c@(RGB cr cg cb) =
  if colorDist <= grayDist then 16 + ci else 232 + grayIdx
  where
    v2ci v
      | v < 48 = 0
      | v < 115 = 1
      | otherwise = truncate ((v - 35) / 40 :: Double) :: Int
    r = v2ci (cr * 255.0)
    g = v2ci (cg * 255.0)
    b = v2ci (cb * 255.0)
    ci = 36 * r + 6 * g + b
    i2cv = [0, 0x5f, 0x87, 0xaf, 0xd7, 0xff] :: [Int]
    cr' = i2cv !! r
    cg' = i2cv !! g
    cb' = i2cv !! b
    average = (r + g + b) `div` 3
    grayIdx = if average > 238 then 23 else (average - 3) `div` 10
    gv = 8 + 10 * grayIdx
    c2 = RGB (fromIntegral cr' / 255.0) (fromIntegral cg' / 255.0) (fromIntegral cb' / 255.0)
    g2 = RGB (fromIntegral gv / 255.0) (fromIntegral gv / 255.0) (fromIntegral gv / 255.0)
    colorDist = distanceHSLuv c c2
    grayDist = distanceHSLuv c g2


-- | Nearest ANSI-16 color to an ANSI-256 index (termenv's @ansi256ToANSIColor@).
ansi256ToANSI :: Int -> Color
ansi256ToANSI idx = ANSIColor best
  where
    h = hexToRGB (ansiHex idx)
    best = snd (minimum [(distanceHSLuv h (hexToRGB (ansiHex i)), i) | i <- [0 .. 15]])


-- | The 256-color palette hex values (matches termenv's @ansiHex@ table).
ansiHex :: Int -> Text
ansiHex i
  | i < 16 = baseAnsiHex !! i
  | i < 232 =
      let j = i - 16
          (rq, rem') = j `divMod` 36
          (gq, bq) = rem' `divMod` 6
       in hexOf (cubeLevel rq) (cubeLevel gq) (cubeLevel bq)
  | i < 256 = let v = 8 + (i - 232) * 10 in hexOf v v v
  | otherwise = "#000000"
  where
    cubeLevel 0 = 0
    cubeLevel n = 55 + n * 40


baseAnsiHex :: [Text]
baseAnsiHex =
  [ "#000000"
  , "#800000"
  , "#008000"
  , "#808000"
  , "#000080"
  , "#800080"
  , "#008080"
  , "#c0c0c0"
  , "#808080"
  , "#ff0000"
  , "#00ff00"
  , "#ffff00"
  , "#0000ff"
  , "#ff00ff"
  , "#00ffff"
  , "#ffffff"
  ]


hexOf :: Int -> Int -> Int -> Text
hexOf r g b = T.pack ('#' : toHex2 r ++ toHex2 g ++ toHex2 b)
  where
    toHex2 v = [hexChar (v `div` 16), hexChar (v `mod` 16)]
    hexChar n
      | n < 10 = chr (ord '0' + n)
      | otherwise = chr (ord 'a' + n - 10)


tshow :: Int -> Text
tshow = T.pack . show
