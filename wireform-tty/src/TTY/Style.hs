-- | Declarative terminal styling, a port of @charmbracelet/lipgloss@.
--
-- A 'Style' is an immutable bag of rules (colors, attributes, padding,
-- margins, borders, alignment, width\/height, transforms). 'render' applies the
-- rules to a string, producing ANSI-styled output. The layout helpers
-- ('joinHorizontal', 'joinVertical', 'place', etc.) compose styled blocks.
--
-- Setters are @value -> Style -> Style@ so they chain with @Data.Function.(&)@:
--
-- > newStyle & bold True & foreground (color "#5A56E0") & render' "hi"
module TTY.Style
  ( -- * Style
    Style
  , newStyle
  , renderer
  , withRenderer
  , render
  , render'
  , displayStyle
  , setString
  , styleValue
  , inherit

    -- * Position
  , Position (..)
  , top
  , bottom
  , center
  , left
  , right

    -- * Attribute setters
  , bold
  , italic
  , underline
  , strikethrough
  , reverseStyle
  , blink
  , faint
  , underlineSpaces
  , strikethroughSpaces
  , colorWhitespace
  , inline
  , foreground
  , background
  , width
  , height
  , align
  , alignHorizontal
  , alignVertical
  , padding
  , paddingTop
  , paddingRight
  , paddingBottom
  , paddingLeft
  , margin
  , marginTop
  , marginRight
  , marginBottom
  , marginLeft
  , marginBackground
  , border
  , borderStyle
  , borderTop
  , borderRight
  , borderBottom
  , borderLeft
  , borderForeground
  , borderBackground
  , maxWidth
  , maxHeight
  , tabWidth
  , noTabConversion
  , transform

    -- * Getters
  , getBold
  , getItalic
  , getUnderline
  , getStrikethrough
  , getReverse
  , getBlink
  , getFaint
  , getUnderlineSpaces
  , getStrikethroughSpaces
  , getForeground
  , getBackground
  , getWidth
  , getHeight
  , getAlign
  , getAlignHorizontal
  , getAlignVertical
  , getPadding
  , getPaddingTop
  , getPaddingRight
  , getPaddingBottom
  , getPaddingLeft
  , getHorizontalPadding
  , getVerticalPadding
  , getMargin
  , getMarginTop
  , getMarginRight
  , getMarginBottom
  , getMarginLeft
  , getHorizontalMargins
  , getVerticalMargins
  , getBorder
  , getBorderStyle
  , getBorderTop
  , getBorderRight
  , getBorderBottom
  , getBorderLeft
  , getBorderTopSize
  , getBorderRightSize
  , getBorderBottomSize
  , getBorderLeftSize
  , getHorizontalBorderSize
  , getVerticalBorderSize
  , getInline
  , getMaxWidth
  , getMaxHeight
  , getTabWidth
  , getHorizontalFrameSize
  , getVerticalFrameSize
  , getFrameSize

    -- * Unsetters
  , unsetBold
  , unsetItalic
  , unsetUnderline
  , unsetUnderlineSpaces
  , unsetStrikethrough
  , unsetStrikethroughSpaces
  , unsetReverse
  , unsetBlink
  , unsetFaint
  , unsetInline
  , unsetForeground
  , unsetBackground
  , unsetMarginTop
  , unsetMarginRight
  , unsetMarginBottom
  , unsetMarginLeft
  , unsetPaddingTop
  , unsetPaddingRight
  , unsetPaddingBottom
  , unsetPaddingLeft
  , unsetBorderTop
  , unsetBorderRight
  , unsetBorderBottom
  , unsetBorderLeft
  , unsetTabWidth

    -- * Layout helpers
  , styleWidth
  , styleHeight
  , styleSize
  , joinHorizontal
  , joinVertical
  , place
  , placeHorizontal
  , placeVertical
  , styleRunes

    -- * Re-exports
  , module TTY.Style.Color
  , module TTY.Style.Border
  ) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import TTY.Ansi (stringWidth, truncateString)
import qualified TTY.Color as C
import TTY.Style.Border
import TTY.Style.Color


-- | A position on an axis: 0 is the start (left\/top), 1 the end
-- (right\/bottom), 0.5 the center.
newtype Position = Position Double
  deriving stock (Eq, Ord, Show)


top, bottom, center, left, right :: Position
top = Position 0.0
bottom = Position 1.0
center = Position 0.5
left = Position 0.0
right = Position 1.0


posValue :: Position -> Double
posValue (Position p) = min 1 (max 0 p)


-- | A set of styling rules. Unset rules are 'Nothing'.
data Style = Style
  { sRenderer :: Renderer
  , sValue :: Text
  , sBold :: Maybe Bool
  , sItalic :: Maybe Bool
  , sUnderline :: Maybe Bool
  , sStrikethrough :: Maybe Bool
  , sReverse :: Maybe Bool
  , sBlink :: Maybe Bool
  , sFaint :: Maybe Bool
  , sUnderlineSpaces :: Maybe Bool
  , sStrikethroughSpaces :: Maybe Bool
  , sColorWhitespace :: Maybe Bool
  , sInline :: Maybe Bool
  , sForeground :: Maybe TerminalColor
  , sBackground :: Maybe TerminalColor
  , sWidth :: Maybe Int
  , sHeight :: Maybe Int
  , sAlignHorizontal :: Maybe Position
  , sAlignVertical :: Maybe Position
  , sPaddingTop :: Maybe Int
  , sPaddingRight :: Maybe Int
  , sPaddingBottom :: Maybe Int
  , sPaddingLeft :: Maybe Int
  , sMarginTop :: Maybe Int
  , sMarginRight :: Maybe Int
  , sMarginBottom :: Maybe Int
  , sMarginLeft :: Maybe Int
  , sMarginBackground :: Maybe TerminalColor
  , sBorderStyle :: Maybe Border
  , sBorderTop :: Maybe Bool
  , sBorderRight :: Maybe Bool
  , sBorderBottom :: Maybe Bool
  , sBorderLeft :: Maybe Bool
  , sBorderTopFg :: Maybe TerminalColor
  , sBorderRightFg :: Maybe TerminalColor
  , sBorderBottomFg :: Maybe TerminalColor
  , sBorderLeftFg :: Maybe TerminalColor
  , sBorderTopBg :: Maybe TerminalColor
  , sBorderRightBg :: Maybe TerminalColor
  , sBorderBottomBg :: Maybe TerminalColor
  , sBorderLeftBg :: Maybe TerminalColor
  , sMaxWidth :: Maybe Int
  , sMaxHeight :: Maybe Int
  , sTabWidth :: Maybe Int
  , sTransform :: Maybe (Text -> Text)
  }


-- | A new, empty style using 'defaultRenderer'.
newStyle :: Style
newStyle =
  Style
    { sRenderer = defaultRenderer
    , sValue = ""
    , sBold = Nothing
    , sItalic = Nothing
    , sUnderline = Nothing
    , sStrikethrough = Nothing
    , sReverse = Nothing
    , sBlink = Nothing
    , sFaint = Nothing
    , sUnderlineSpaces = Nothing
    , sStrikethroughSpaces = Nothing
    , sColorWhitespace = Nothing
    , sInline = Nothing
    , sForeground = Nothing
    , sBackground = Nothing
    , sWidth = Nothing
    , sHeight = Nothing
    , sAlignHorizontal = Nothing
    , sAlignVertical = Nothing
    , sPaddingTop = Nothing
    , sPaddingRight = Nothing
    , sPaddingBottom = Nothing
    , sPaddingLeft = Nothing
    , sMarginTop = Nothing
    , sMarginRight = Nothing
    , sMarginBottom = Nothing
    , sMarginLeft = Nothing
    , sMarginBackground = Nothing
    , sBorderStyle = Nothing
    , sBorderTop = Nothing
    , sBorderRight = Nothing
    , sBorderBottom = Nothing
    , sBorderLeft = Nothing
    , sBorderTopFg = Nothing
    , sBorderRightFg = Nothing
    , sBorderBottomFg = Nothing
    , sBorderLeftFg = Nothing
    , sBorderTopBg = Nothing
    , sBorderRightBg = Nothing
    , sBorderBottomBg = Nothing
    , sBorderLeftBg = Nothing
    , sMaxWidth = Nothing
    , sMaxHeight = Nothing
    , sTabWidth = Nothing
    , sTransform = Nothing
    }


-- | The renderer this style uses.
renderer :: Style -> Renderer
renderer = sRenderer


-- | Set the renderer for a style.
withRenderer :: Renderer -> Style -> Style
withRenderer r s = s {sRenderer = r}


-- Setters -------------------------------------------------------------------

bold :: Bool -> Style -> Style
bold v s = s {sBold = Just v}


italic :: Bool -> Style -> Style
italic v s = s {sItalic = Just v}


underline :: Bool -> Style -> Style
underline v s = s {sUnderline = Just v}


strikethrough :: Bool -> Style -> Style
strikethrough v s = s {sStrikethrough = Just v}


reverseStyle :: Bool -> Style -> Style
reverseStyle v s = s {sReverse = Just v}


blink :: Bool -> Style -> Style
blink v s = s {sBlink = Just v}


faint :: Bool -> Style -> Style
faint v s = s {sFaint = Just v}


underlineSpaces :: Bool -> Style -> Style
underlineSpaces v s = s {sUnderlineSpaces = Just v}


strikethroughSpaces :: Bool -> Style -> Style
strikethroughSpaces v s = s {sStrikethroughSpaces = Just v}


colorWhitespace :: Bool -> Style -> Style
colorWhitespace v s = s {sColorWhitespace = Just v}


inline :: Bool -> Style -> Style
inline v s = s {sInline = Just v}


foreground :: TerminalColor -> Style -> Style
foreground c s = s {sForeground = Just c}


background :: TerminalColor -> Style -> Style
background c s = s {sBackground = Just c}


width :: Int -> Style -> Style
width n s = s {sWidth = Just (max 0 n)}


height :: Int -> Style -> Style
height n s = s {sHeight = Just (max 0 n)}


-- | Shorthand for horizontal (and optionally vertical) alignment.
align :: [Position] -> Style -> Style
align ps s = case ps of
  (h : v : _) -> s {sAlignHorizontal = Just h, sAlignVertical = Just v}
  [h] -> s {sAlignHorizontal = Just h}
  [] -> s


alignHorizontal :: Position -> Style -> Style
alignHorizontal p s = s {sAlignHorizontal = Just p}


alignVertical :: Position -> Style -> Style
alignVertical p s = s {sAlignVertical = Just p}


-- | CSS-shorthand padding (1, 2, 3, or 4 values; otherwise no-op).
padding :: [Int] -> Style -> Style
padding xs s = case whichSidesInt xs of
  Just (t, r, b, l) ->
    s
      { sPaddingTop = Just (max 0 t)
      , sPaddingRight = Just (max 0 r)
      , sPaddingBottom = Just (max 0 b)
      , sPaddingLeft = Just (max 0 l)
      }
  Nothing -> s


paddingTop :: Int -> Style -> Style
paddingTop n s = s {sPaddingTop = Just (max 0 n)}


paddingRight :: Int -> Style -> Style
paddingRight n s = s {sPaddingRight = Just (max 0 n)}


paddingBottom :: Int -> Style -> Style
paddingBottom n s = s {sPaddingBottom = Just (max 0 n)}


paddingLeft :: Int -> Style -> Style
paddingLeft n s = s {sPaddingLeft = Just (max 0 n)}


-- | CSS-shorthand margin (1, 2, 3, or 4 values; otherwise no-op).
margin :: [Int] -> Style -> Style
margin xs s = case whichSidesInt xs of
  Just (t, r, b, l) ->
    s
      { sMarginTop = Just (max 0 t)
      , sMarginRight = Just (max 0 r)
      , sMarginBottom = Just (max 0 b)
      , sMarginLeft = Just (max 0 l)
      }
  Nothing -> s


marginTop :: Int -> Style -> Style
marginTop n s = s {sMarginTop = Just (max 0 n)}


marginRight :: Int -> Style -> Style
marginRight n s = s {sMarginRight = Just (max 0 n)}


marginBottom :: Int -> Style -> Style
marginBottom n s = s {sMarginBottom = Just (max 0 n)}


marginLeft :: Int -> Style -> Style
marginLeft n s = s {sMarginLeft = Just (max 0 n)}


marginBackground :: TerminalColor -> Style -> Style
marginBackground c s = s {sMarginBackground = Just c}


-- | Set the border style and which sides to draw (CSS-shorthand booleans;
-- empty means all sides).
border :: Border -> [Bool] -> Style -> Style
border b sides s =
  let (t, r, bo, l) = case whichSidesBool sides of
        Just q -> q
        Nothing -> (True, True, True, True)
   in s
        { sBorderStyle = Just b
        , sBorderTop = Just t
        , sBorderRight = Just r
        , sBorderBottom = Just bo
        , sBorderLeft = Just l
        }


borderStyle :: Border -> Style -> Style
borderStyle b s = s {sBorderStyle = Just b}


borderTop :: Bool -> Style -> Style
borderTop v s = s {sBorderTop = Just v}


borderRight :: Bool -> Style -> Style
borderRight v s = s {sBorderRight = Just v}


borderBottom :: Bool -> Style -> Style
borderBottom v s = s {sBorderBottom = Just v}


borderLeft :: Bool -> Style -> Style
borderLeft v s = s {sBorderLeft = Just v}


borderForeground :: [TerminalColor] -> Style -> Style
borderForeground cs s = case whichSidesColor cs of
  Just (t, r, b, l) ->
    s {sBorderTopFg = Just t, sBorderRightFg = Just r, sBorderBottomFg = Just b, sBorderLeftFg = Just l}
  Nothing -> s


borderBackground :: [TerminalColor] -> Style -> Style
borderBackground cs s = case whichSidesColor cs of
  Just (t, r, b, l) ->
    s {sBorderTopBg = Just t, sBorderRightBg = Just r, sBorderBottomBg = Just b, sBorderLeftBg = Just l}
  Nothing -> s


maxWidth :: Int -> Style -> Style
maxWidth n s = s {sMaxWidth = Just (max 0 n)}


maxHeight :: Int -> Style -> Style
maxHeight n s = s {sMaxHeight = Just (max 0 n)}


-- | Disable tab-to-space conversion (pass to 'tabWidth').
noTabConversion :: Int
noTabConversion = -1


tabWidth :: Int -> Style -> Style
tabWidth n s = s {sTabWidth = Just (if n <= -1 then -1 else n)}


transform :: (Text -> Text) -> Style -> Style
transform f s = s {sTransform = Just f}


-- | The underlying string value for this style.
setString :: [Text] -> Style -> Style
setString strs s = s {sValue = T.intercalate " " strs}


styleValue :: Style -> Text
styleValue = sValue


whichSidesInt :: [Int] -> Maybe (Int, Int, Int, Int)
whichSidesInt = \case
  [a] -> Just (a, a, a, a)
  [a, b] -> Just (a, b, a, b)
  [a, b, c] -> Just (a, b, c, b)
  [a, b, c, d] -> Just (a, b, c, d)
  _ -> Nothing


whichSidesBool :: [Bool] -> Maybe (Bool, Bool, Bool, Bool)
whichSidesBool = \case
  [a] -> Just (a, a, a, a)
  [a, b] -> Just (a, b, a, b)
  [a, b, c] -> Just (a, b, c, b)
  [a, b, c, d] -> Just (a, b, c, d)
  _ -> Nothing


whichSidesColor :: [TerminalColor] -> Maybe (TerminalColor, TerminalColor, TerminalColor, TerminalColor)
whichSidesColor = \case
  [a] -> Just (a, a, a, a)
  [a, b] -> Just (a, b, a, b)
  [a, b, c] -> Just (a, b, c, b)
  [a, b, c, d] -> Just (a, b, c, d)
  _ -> Nothing


-- Getters -------------------------------------------------------------------

getBold :: Style -> Bool
getBold = orFalse . sBold


getItalic :: Style -> Bool
getItalic = orFalse . sItalic


getUnderline :: Style -> Bool
getUnderline = orFalse . sUnderline


getStrikethrough :: Style -> Bool
getStrikethrough = orFalse . sStrikethrough


getReverse :: Style -> Bool
getReverse = orFalse . sReverse


getBlink :: Style -> Bool
getBlink = orFalse . sBlink


getFaint :: Style -> Bool
getFaint = orFalse . sFaint


getUnderlineSpaces :: Style -> Bool
getUnderlineSpaces = orFalse . sUnderlineSpaces


getStrikethroughSpaces :: Style -> Bool
getStrikethroughSpaces = orFalse . sStrikethroughSpaces


getForeground :: Style -> TerminalColor
getForeground = orNoColor . sForeground


getBackground :: Style -> TerminalColor
getBackground = orNoColor . sBackground


getWidth :: Style -> Int
getWidth = orZero . sWidth


getHeight :: Style -> Int
getHeight = orZero . sHeight


getAlign :: Style -> Position
getAlign = getAlignHorizontal


getAlignHorizontal :: Style -> Position
getAlignHorizontal s = maybe left id (sAlignHorizontal s)


getAlignVertical :: Style -> Position
getAlignVertical s = maybe top id (sAlignVertical s)


getPadding :: Style -> (Int, Int, Int, Int)
getPadding s = (getPaddingTop s, getPaddingRight s, getPaddingBottom s, getPaddingLeft s)


getPaddingTop :: Style -> Int
getPaddingTop = orZero . sPaddingTop


getPaddingRight :: Style -> Int
getPaddingRight = orZero . sPaddingRight


getPaddingBottom :: Style -> Int
getPaddingBottom = orZero . sPaddingBottom


getPaddingLeft :: Style -> Int
getPaddingLeft = orZero . sPaddingLeft


getHorizontalPadding :: Style -> Int
getHorizontalPadding s = getPaddingLeft s + getPaddingRight s


getVerticalPadding :: Style -> Int
getVerticalPadding s = getPaddingTop s + getPaddingBottom s


getMargin :: Style -> (Int, Int, Int, Int)
getMargin s = (getMarginTop s, getMarginRight s, getMarginBottom s, getMarginLeft s)


getMarginTop :: Style -> Int
getMarginTop = orZero . sMarginTop


getMarginRight :: Style -> Int
getMarginRight = orZero . sMarginRight


getMarginBottom :: Style -> Int
getMarginBottom = orZero . sMarginBottom


getMarginLeft :: Style -> Int
getMarginLeft = orZero . sMarginLeft


getHorizontalMargins :: Style -> Int
getHorizontalMargins s = getMarginLeft s + getMarginRight s


getVerticalMargins :: Style -> Int
getVerticalMargins s = getMarginTop s + getMarginBottom s


getBorder :: Style -> (Border, Bool, Bool, Bool, Bool)
getBorder s = (getBorderStyle s, getBorderTop s, getBorderRight s, getBorderBottom s, getBorderLeft s)


getBorderStyle :: Style -> Border
getBorderStyle s = maybe noBorder id (sBorderStyle s)


getBorderTop :: Style -> Bool
getBorderTop = orFalse . sBorderTop


getBorderRight :: Style -> Bool
getBorderRight = orFalse . sBorderRight


getBorderBottom :: Style -> Bool
getBorderBottom = orFalse . sBorderBottom


getBorderLeft :: Style -> Bool
getBorderLeft = orFalse . sBorderLeft


getBorderTopSize :: Style -> Int
getBorderTopSize s
  | not (getBorderTop s) && not (implicitBorders s) = 0
  | otherwise = getTopSize (getBorderStyle s)


getBorderRightSize :: Style -> Int
getBorderRightSize s
  | not (getBorderRight s) && not (implicitBorders s) = 0
  | otherwise = getRightSize (getBorderStyle s)


getBorderBottomSize :: Style -> Int
getBorderBottomSize s
  | not (getBorderBottom s) && not (implicitBorders s) = 0
  | otherwise = getBottomSize (getBorderStyle s)


getBorderLeftSize :: Style -> Int
getBorderLeftSize s
  | not (getBorderLeft s) && not (implicitBorders s) = 0
  | otherwise = getLeftSize (getBorderStyle s)


getHorizontalBorderSize :: Style -> Int
getHorizontalBorderSize s = getBorderLeftSize s + getBorderRightSize s


getVerticalBorderSize :: Style -> Int
getVerticalBorderSize s = getBorderTopSize s + getBorderBottomSize s


getInline :: Style -> Bool
getInline = orFalse . sInline


getMaxWidth :: Style -> Int
getMaxWidth = orZero . sMaxWidth


getMaxHeight :: Style -> Int
getMaxHeight = orZero . sMaxHeight


getTabWidth :: Style -> Int
getTabWidth = orZero . sTabWidth


getHorizontalFrameSize :: Style -> Int
getHorizontalFrameSize s = getHorizontalMargins s + getHorizontalPadding s + getHorizontalBorderSize s


getVerticalFrameSize :: Style -> Int
getVerticalFrameSize s = getVerticalMargins s + getVerticalPadding s + getVerticalBorderSize s


getFrameSize :: Style -> (Int, Int)
getFrameSize s = (getHorizontalFrameSize s, getVerticalFrameSize s)


implicitBorders :: Style -> Bool
implicitBorders s =
  getBorderStyle s /= noBorder
    && not (anySet [sBorderTop s, sBorderRight s, sBorderBottom s, sBorderLeft s])
  where
    anySet = any (/= Nothing)


orFalse :: Maybe Bool -> Bool
orFalse = maybe False id


orZero :: Maybe Int -> Int
orZero = maybe 0 id


orNoColor :: Maybe TerminalColor -> TerminalColor
orNoColor = maybe noColor id


-- Unsetters -----------------------------------------------------------------

unsetBold :: Style -> Style
unsetBold s = s {sBold = Nothing}


unsetItalic :: Style -> Style
unsetItalic s = s {sItalic = Nothing}


unsetUnderline :: Style -> Style
unsetUnderline s = s {sUnderline = Nothing}


unsetUnderlineSpaces :: Style -> Style
unsetUnderlineSpaces s = s {sUnderlineSpaces = Nothing}


unsetStrikethrough :: Style -> Style
unsetStrikethrough s = s {sStrikethrough = Nothing}


unsetStrikethroughSpaces :: Style -> Style
unsetStrikethroughSpaces s = s {sStrikethroughSpaces = Nothing}


unsetReverse :: Style -> Style
unsetReverse s = s {sReverse = Nothing}


unsetBlink :: Style -> Style
unsetBlink s = s {sBlink = Nothing}


unsetFaint :: Style -> Style
unsetFaint s = s {sFaint = Nothing}


unsetInline :: Style -> Style
unsetInline s = s {sInline = Nothing}


unsetForeground :: Style -> Style
unsetForeground s = s {sForeground = Nothing}


unsetBackground :: Style -> Style
unsetBackground s = s {sBackground = Nothing}


unsetMarginTop :: Style -> Style
unsetMarginTop s = s {sMarginTop = Nothing}


unsetMarginRight :: Style -> Style
unsetMarginRight s = s {sMarginRight = Nothing}


unsetMarginBottom :: Style -> Style
unsetMarginBottom s = s {sMarginBottom = Nothing}


unsetMarginLeft :: Style -> Style
unsetMarginLeft s = s {sMarginLeft = Nothing}


unsetPaddingTop :: Style -> Style
unsetPaddingTop s = s {sPaddingTop = Nothing}


unsetPaddingRight :: Style -> Style
unsetPaddingRight s = s {sPaddingRight = Nothing}


unsetPaddingBottom :: Style -> Style
unsetPaddingBottom s = s {sPaddingBottom = Nothing}


unsetPaddingLeft :: Style -> Style
unsetPaddingLeft s = s {sPaddingLeft = Nothing}


unsetBorderTop :: Style -> Style
unsetBorderTop s = s {sBorderTop = Nothing}


unsetBorderRight :: Style -> Style
unsetBorderRight s = s {sBorderRight = Nothing}


unsetBorderBottom :: Style -> Style
unsetBorderBottom s = s {sBorderBottom = Nothing}


unsetBorderLeft :: Style -> Style
unsetBorderLeft s = s {sBorderLeft = Nothing}


unsetTabWidth :: Style -> Style
unsetTabWidth s = s {sTabWidth = Nothing}


-- Inherit -------------------------------------------------------------------

-- | Overlay set values from the argument style onto this style, but only where
-- this style has not set them. Margins, padding, and the string value are not
-- inherited; a background color also seeds the margin background.
inherit :: Style -> Style -> Style
inherit i s0 =
  let s1 =
        case (sBackground i, sMarginBackground s0, sMarginBackground i) of
          (Just bg, Nothing, Nothing) -> s0 {sMarginBackground = Just bg}
          _ -> s0
   in foldl (\s f -> f s) s1 copiers
  where
    -- Copy field @get@ from @i@ to @s@ when @i@ has it set and @s@ does not.
    -- Works for any field type (including the un-@Eq@ transform function)
    -- because "is set" is tested through 'Maybe', not equality.
    cp :: (Style -> Maybe a) -> (Maybe a -> Style -> Style) -> Style -> Style
    cp get set s = case get i of
      Just _ | isNothing (get s) -> set (get i) s
      _ -> s

    copiers =
      [ cp sBold (\v t -> t {sBold = v})
      , cp sItalic (\v t -> t {sItalic = v})
      , cp sUnderline (\v t -> t {sUnderline = v})
      , cp sStrikethrough (\v t -> t {sStrikethrough = v})
      , cp sReverse (\v t -> t {sReverse = v})
      , cp sBlink (\v t -> t {sBlink = v})
      , cp sFaint (\v t -> t {sFaint = v})
      , cp sUnderlineSpaces (\v t -> t {sUnderlineSpaces = v})
      , cp sStrikethroughSpaces (\v t -> t {sStrikethroughSpaces = v})
      , cp sColorWhitespace (\v t -> t {sColorWhitespace = v})
      , cp sInline (\v t -> t {sInline = v})
      , cp sForeground (\v t -> t {sForeground = v})
      , cp sBackground (\v t -> t {sBackground = v})
      , cp sWidth (\v t -> t {sWidth = v})
      , cp sHeight (\v t -> t {sHeight = v})
      , cp sAlignHorizontal (\v t -> t {sAlignHorizontal = v})
      , cp sAlignVertical (\v t -> t {sAlignVertical = v})
      , cp sMarginBackground (\v t -> t {sMarginBackground = v})
      , cp sBorderStyle (\v t -> t {sBorderStyle = v})
      , cp sBorderTop (\v t -> t {sBorderTop = v})
      , cp sBorderRight (\v t -> t {sBorderRight = v})
      , cp sBorderBottom (\v t -> t {sBorderBottom = v})
      , cp sBorderLeft (\v t -> t {sBorderLeft = v})
      , cp sBorderTopFg (\v t -> t {sBorderTopFg = v})
      , cp sBorderRightFg (\v t -> t {sBorderRightFg = v})
      , cp sBorderBottomFg (\v t -> t {sBorderBottomFg = v})
      , cp sBorderLeftFg (\v t -> t {sBorderLeftFg = v})
      , cp sBorderTopBg (\v t -> t {sBorderTopBg = v})
      , cp sBorderRightBg (\v t -> t {sBorderRightBg = v})
      , cp sBorderBottomBg (\v t -> t {sBorderBottomBg = v})
      , cp sBorderLeftBg (\v t -> t {sBorderLeftBg = v})
      , cp sMaxWidth (\v t -> t {sMaxWidth = v})
      , cp sMaxHeight (\v t -> t {sMaxHeight = v})
      , cp sTabWidth (\v t -> t {sTabWidth = v})
      ]


-- Rendering -----------------------------------------------------------------

csi :: Text
csi = "\x1b["


-- | A minimal termenv-style SGR wrapper: a profile plus a list of codes.
data TermStyle = TermStyle Profile' [Text]


-- | Local copy of the profile used by the SGR wrapper.
type Profile' = C.Profile


emptyTerm :: C.Profile -> TermStyle
emptyTerm p = TermStyle p []


addCode :: Text -> TermStyle -> TermStyle
addCode code (TermStyle p cs) = TermStyle p (cs ++ [code])


termStyled :: TermStyle -> Text -> Text
termStyled (TermStyle p cs) s
  | p == C.Ascii = s
  | null cs = s
  | seq' == "" = s
  | otherwise = csi <> seq' <> "m" <> s <> csi <> "0m"
  where
    seq' = T.intercalate ";" cs


termForeground :: Renderer -> TerminalColor -> TermStyle -> TermStyle
termForeground r c ts = case resolveColor r c of
  Just col -> addCode (C.sequenceColor False col) ts
  Nothing -> ts


termBackground :: Renderer -> TerminalColor -> TermStyle -> TermStyle
termBackground r c ts = case resolveColor r c of
  Just col -> addCode (C.sequenceColor True col) ts
  Nothing -> ts


getAsBoolD :: Maybe Bool -> Bool -> Bool
getAsBoolD m d = maybe d id m


-- | Render this style applied to @strs@ (joined with spaces, after the value).
render :: Style -> [Text] -> Text
render s strs0 = renderImpl s strs0


-- | Convenience: render a single string.
render' :: Text -> Style -> Text
render' t s = render s [t]


-- | Render using only the style's stored value (lipgloss @String()@).
displayStyle :: Style -> Text
displayStyle s = render s []


renderImpl :: Style -> [Text] -> Text
renderImpl s strs0 =
  let r = sRenderer s
      strs = if T.null (sValue s) then strs0 else sValue s : strs0
      str0 = T.intercalate " " strs

      boldV = getAsBoolD (sBold s) False
      italicV = getAsBoolD (sItalic s) False
      underlineV = getAsBoolD (sUnderline s) False
      strikeV = getAsBoolD (sStrikethrough s) False
      reverseV = getAsBoolD (sReverse s) False
      blinkV = getAsBoolD (sBlink s) False
      faintV = getAsBoolD (sFaint s) False

      fgC = orNoColor (sForeground s)
      bgC = orNoColor (sBackground s)

      widthV = orZero (sWidth s)
      heightV = orZero (sHeight s)
      hAlign = getAlignHorizontal s
      vAlign = getAlignVertical s

      padT = orZero (sPaddingTop s)
      padR = orZero (sPaddingRight s)
      padB = orZero (sPaddingBottom s)
      padL = orZero (sPaddingLeft s)

      colorWS = getAsBoolD (sColorWhitespace s) True
      inlineV = getAsBoolD (sInline s) False
      maxW = orZero (sMaxWidth s)
      maxH = orZero (sMaxHeight s)

      uSpaces = maybe underlineV id (sUnderlineSpaces s)
      sSpaces = maybe strikeV id (sStrikethroughSpaces s)

      styleWS = reverseV
      useSpaceStyler = (underlineV && not uSpaces) || (strikeV && not sSpaces) || uSpaces || sSpaces

      str1 = case sTransform s of
        Just f -> f str0
        Nothing -> str0
   in if propsEmpty s
        then maybeConvertTabs s str1
        else
          let -- Build the three termenv styles.
              te0 = emptyTerm (rendererColorProfile r)
              teWS0 = emptyTerm (rendererColorProfile r)
              teSpace0 = emptyTerm (rendererColorProfile r)

              te1 = applyWhen boldV (addCode "1") te0
              te2 = applyWhen italicV (addCode "3") te1
              te3 = applyWhen underlineV (addCode "4") te2
              (te4, teWS1) =
                if reverseV
                  then (addCode "7" te3, addCode "7" teWS0)
                  else (te3, teWS0)
              te5 = applyWhen blinkV (addCode "5") te4
              te6 = applyWhen faintV (addCode "2") te5

              (te7, teWS2, teSpace1) =
                if not (isNoColor fgC)
                  then
                    ( termForeground r fgC te6
                    , if styleWS then termForeground r fgC teWS1 else teWS1
                    , if useSpaceStyler then termForeground r fgC teSpace0 else teSpace0
                    )
                  else (te6, teWS1, teSpace0)

              (te8, teWS3, teSpace2) =
                if not (isNoColor bgC)
                  then
                    ( termBackground r bgC te7
                    , if colorWS then termBackground r bgC teWS2 else teWS2
                    , if useSpaceStyler then termBackground r bgC teSpace1 else teSpace1
                    )
                  else (te7, teWS2, teSpace1)

              te9 = applyWhen underlineV (addCode "4") te8
              te10 = applyWhen strikeV (addCode "9") te9
              teSpace3 = applyWhen uSpaces (addCode "4") teSpace2
              teSpace = applyWhen sSpaces (addCode "9") teSpace3
              te = te10
              teWS = teWS3

              -- Tabs, CRLF, inline newline stripping.
              strA = maybeConvertTabs s str1
              strB = T.replace "\r\n" "\n" strA
              strC = if inlineV then T.replace "\n" "" strB else strB

              -- Word wrap.
              strD =
                if not inlineV && widthV > 0
                  then softWrap (widthV - padL - padR) strC
                  else strC

              -- Core text styling.
              strE = renderCore useSpaceStyler te teSpace strD

              -- Padding.
              wsStyle = if colorWS || styleWS then Just teWS else Nothing
              strF =
                if inlineV
                  then strE
                  else
                    let p1 = if padL > 0 then padLeftT strE padL wsStyle else strE
                        p2 = if padR > 0 then padRightT p1 padR wsStyle else p1
                        p3 = if padT > 0 then T.replicate padT "\n" <> p2 else p2
                        p4 = if padB > 0 then p3 <> T.replicate padB "\n" else p3
                     in p4

              -- Height.
              strG = if heightV > 0 then alignTextVertical strF vAlign heightV else strF

              -- Horizontal alignment / padding to width.
              numLines = T.count "\n" strG
              strH =
                if numLines /= 0 || widthV /= 0
                  then alignTextHorizontal strG hAlign widthV wsStyle
                  else strG

              -- Borders + margins.
              strI =
                if inlineV
                  then strH
                  else applyMargins s (applyBorder s strH) inlineV

              -- MaxWidth.
              strJ =
                if maxW > 0
                  then T.intercalate "\n" (map (truncateString maxW) (T.splitOn "\n" strI))
                  else strI

              -- MaxHeight.
              strK =
                if maxH > 0
                  then
                    let ls = T.splitOn "\n" strJ
                        h = min maxH (length ls)
                     in T.intercalate "\n" (take h ls)
                  else strJ
           in strK


applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen False _ = id


-- | Is no styling property set? (The string value does not count.)
propsEmpty :: Style -> Bool
propsEmpty s =
  all
    (== Nothing)
    [ b sBold
    , b sItalic
    , b sUnderline
    , b sStrikethrough
    , b sReverse
    , b sBlink
    , b sFaint
    , b sUnderlineSpaces
    , b sStrikethroughSpaces
    , b sColorWhitespace
    , b sInline
    , b sBorderTop
    , b sBorderRight
    , b sBorderBottom
    , b sBorderLeft
    ]
    && all (== Nothing) [c sForeground, c sBackground, c sMarginBackground, c sBorderTopFg, c sBorderRightFg, c sBorderBottomFg, c sBorderLeftFg, c sBorderTopBg, c sBorderRightBg, c sBorderBottomBg, c sBorderLeftBg]
    && all (== Nothing) [i sWidth, i sHeight, i sPaddingTop, i sPaddingRight, i sPaddingBottom, i sPaddingLeft, i sMarginTop, i sMarginRight, i sMarginBottom, i sMarginLeft, i sMaxWidth, i sMaxHeight, i sTabWidth]
    && isNothing (sAlignHorizontal s)
    && isNothing (sAlignVertical s)
    && isNothing (sBorderStyle s)
    && isNothing (sTransform s)
  where
    b f = maybe (Nothing :: Maybe ()) (const (Just ())) (f s)
    c f = maybe (Nothing :: Maybe ()) (const (Just ())) (f s)
    i f = maybe (Nothing :: Maybe ()) (const (Just ())) (f s)


maybeConvertTabs :: Style -> Text -> Text
maybeConvertTabs s str =
  case maybe tabWidthDefault id (sTabWidth s) of
    (-1) -> str
    0 -> T.replace "\t" "" str
    tw -> T.replace "\t" (T.replicate tw " ") str
  where
    tabWidthDefault = 4


-- | Render core text, styling spaces separately when @useSpaceStyler@.
renderCore :: Bool -> TermStyle -> TermStyle -> Text -> Text
renderCore useSpaceStyler te teSpace str =
  T.intercalate "\n" (map styleLine (T.splitOn "\n" str))
  where
    styleLine l
      | useSpaceStyler =
          T.concat
            [ if isSpaceChar ch then termStyled teSpace (T.singleton ch) else termStyled te (T.singleton ch)
            | ch <- T.unpack l
            ]
      | otherwise = termStyled te l
    isSpaceChar ch = ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\v' || ch == '\f'


-- Padding helpers -----------------------------------------------------------

padLeftT :: Text -> Int -> Maybe TermStyle -> Text
padLeftT str n st = padT' str (negate n) st


padRightT :: Text -> Int -> Maybe TermStyle -> Text
padRightT str n st = padT' str n st


padT' :: Text -> Int -> Maybe TermStyle -> Text
padT' str n mst
  | n == 0 = str
  | otherwise =
      T.intercalate "\n" (map padLine (T.splitOn "\n" str))
  where
    sp0 = T.replicate (abs n) " "
    sp = case mst of
      Just st -> termStyled st sp0
      Nothing -> sp0
    padLine l
      | n > 0 = l <> sp
      | otherwise = sp <> l


-- Alignment -----------------------------------------------------------------

getLines :: Text -> ([Text], Int)
getLines str =
  let ls = T.splitOn "\n" str
   in (ls, foldr (max . stringWidth) 0 ls)


alignTextHorizontal :: Text -> Position -> Int -> Maybe TermStyle -> Text
alignTextHorizontal str pos w mst =
  T.intercalate "\n" (map alignLine ls)
  where
    (ls, widest) = getLines str
    alignLine l =
      let lineW = stringWidth l
          short0 = widest - lineW
          short = short0 + max 0 (w - (short0 + lineW))
       in if short <= 0
            then l
            else case pos of
              p
                | p == right ->
                    styledSpaces short <> l
                | p == center ->
                    let leftN = short `div` 2
                        rightN = leftN + short `mod` 2
                     in styledSpaces leftN <> l <> styledSpaces rightN
                | otherwise -> l <> styledSpaces short
    styledSpaces k =
      let raw = T.replicate k " "
       in case mst of
            Just st -> termStyled st raw
            Nothing -> raw


alignTextVertical :: Text -> Position -> Int -> Text
alignTextVertical str pos h =
  let strHeight = T.count "\n" str + 1
   in if h < strHeight
        then str
        else case pos of
          p
            | p == top -> str <> T.replicate (h - strHeight) "\n"
            | p == bottom -> T.replicate (h - strHeight) "\n" <> str
            | p == center ->
                let half = (h - strHeight) `div` 2
                    (tp, bt)
                      | strHeight + half + half > h = (half - 1, half)
                      | strHeight + half + half < h = (half, half + 1)
                      | otherwise = (half, half)
                 in T.replicate tp "\n" <> str <> T.replicate bt "\n"
            | otherwise -> str


-- Borders -------------------------------------------------------------------

applyBorder :: Style -> Text -> Text
applyBorder s str =
  let b0 = getBorderStyle s
      hasTop0 = getBorderTop s
      hasRight0 = getBorderRight s
      hasBottom0 = getBorderBottom s
      hasLeft0 = getBorderLeft s
      imp = implicitBorders s
      hasTop = hasTop0 || imp
      hasRight = hasRight0 || imp
      hasBottom = hasBottom0 || imp
      hasLeft = hasLeft0 || imp
   in if b0 == noBorder || not (hasTop || hasRight || hasBottom || hasLeft)
        then str
        else
          let (ls, w0) = getLines str
              bLeft = if hasLeft && T.null (btLeft b0) then " " else btLeft b0
              w1 = if hasLeft then w0 + maxRuneWidth bLeft else w0
              bRight = if hasRight && T.null (btRight b0) then " " else btRight b0
              bTL0 = if hasTop && hasLeft && T.null (btTopLeft b0) then " " else btTopLeft b0
              bTR0 = if hasTop && hasRight && T.null (btTopRight b0) then " " else btTopRight b0
              bBL0 = if hasBottom && hasLeft && T.null (btBottomLeft b0) then " " else btBottomLeft b0
              bBR0 = if hasBottom && hasRight && T.null (btBottomRight b0) then " " else btBottomRight b0
              -- Drop corners for missing adjacent sides.
              (bTL1, bTR1) =
                if hasTop
                  then case (hasLeft, hasRight) of
                    (False, False) -> ("", "")
                    (False, _) -> ("", bTR0)
                    (_, False) -> (bTL0, "")
                    _ -> (bTL0, bTR0)
                  else (bTL0, bTR0)
              (bBL1, bBR1) =
                if hasBottom
                  then case (hasLeft, hasRight) of
                    (False, False) -> ("", "")
                    (False, _) -> ("", bBR0)
                    (_, False) -> (bBL0, "")
                    _ -> (bBL0, bBR0)
                  else (bBL0, bBR0)
              bTL = firstRune bTL1
              bTR = firstRune bTR1
              bBL = firstRune bBL1
              bBR = firstRune bBR1

              topFG = orNoColor (sBorderTopFg s)
              rightFG = orNoColor (sBorderRightFg s)
              bottomFG = orNoColor (sBorderBottomFg s)
              leftFG = orNoColor (sBorderLeftFg s)
              topBG = orNoColor (sBorderTopBg s)
              rightBG = orNoColor (sBorderRightBg s)
              bottomBG = orNoColor (sBorderBottomBg s)
              leftBG = orNoColor (sBorderLeftBg s)

              topEdge =
                if hasTop
                  then [styleBorder s (renderHorizontalEdge bTL (btTop b0) bTR w1) topFG topBG]
                  else []
              bottomEdge =
                if hasBottom
                  then [styleBorder s (renderHorizontalEdge bBL (btBottom b0) bBR w1) bottomFG bottomBG]
                  else []
              leftRunes = T.unpack bLeft
              rightRunes = T.unpack bRight
              sideLine idx l =
                let lft =
                      if hasLeft && not (null leftRunes)
                        then styleBorder s (T.singleton (leftRunes !! (idx `mod` length leftRunes))) leftFG leftBG
                        else ""
                    rgt =
                      if hasRight && not (null rightRunes)
                        then styleBorder s (T.singleton (rightRunes !! (idx `mod` length rightRunes))) rightFG rightBG
                        else ""
                 in lft <> l <> rgt
              middle = zipWith sideLine [0 ..] ls
           in T.intercalate "\n" (topEdge ++ middle ++ bottomEdge)


renderHorizontalEdge :: Text -> Text -> Text -> Int -> Text
renderHorizontalEdge lft middle0 rgt w =
  let middle = if T.null middle0 then " " else middle0
      leftW = stringWidth lft
      rightW = stringWidth rgt
      runes = T.unpack middle
      n = length runes
      build i j acc
        | i >= w + rightW = acc
        | otherwise =
            let ch = runes !! j
                j' = let jj = j + 1 in if jj >= n then 0 else jj
             in build (i + stringWidth (T.singleton (runes !! j'))) j' (acc <> T.singleton ch)
   in lft <> (if n == 0 then "" else build (leftW + rightW) 0 "") <> rgt


styleBorder :: Style -> Text -> TerminalColor -> TerminalColor -> Text
styleBorder s b fg bg
  | isNoColor fg && isNoColor bg = b
  | otherwise =
      let r = sRenderer s
          t0 = emptyTerm (rendererColorProfile r)
          t1 = if not (isNoColor fg) then termForeground r fg t0 else t0
          t2 = if not (isNoColor bg) then termBackground r bg t1 else t1
       in termStyled t2 b


firstRune :: Text -> Text
firstRune t = if T.null t then t else T.take 1 t


-- Margins -------------------------------------------------------------------

applyMargins :: Style -> Text -> Bool -> Text
applyMargins s str inlineV =
  let tM = orZero (sMarginTop s)
      rM = orZero (sMarginRight s)
      bM = orZero (sMarginBottom s)
      lM = orZero (sMarginLeft s)
      r = sRenderer s
      bgc = orNoColor (sMarginBackground s)
      styler =
        if not (isNoColor bgc)
          then Just (termBackground r bgc (emptyTerm (rendererColorProfile r)))
          else Nothing
      str1 = padLeftT str lM styler
      str2 = padRightT str1 rM styler
   in if inlineV
        then str2
        else
          let (_, w) = getLines str2
              spaces = T.replicate w " "
              styled t = case styler of
                Just st -> termStyled st t
                Nothing -> t
              str3 = if tM > 0 then styled (T.replicate tM (spaces <> "\n")) <> str2 else str2
              str4 = if bM > 0 then str3 <> styled (T.replicate bM ("\n" <> spaces)) else str3
           in str4


-- Word wrap (approximation of cellbuf.Wrap; greedy, hard-breaks long words) --

softWrap :: Int -> Text -> Text
softWrap limit str
  | limit <= 0 = str
  | otherwise = T.intercalate "\n" (concatMap wrapLine (T.splitOn "\n" str))
  where
    -- Split overlong words into hard chunks of at most @limit@ cells.
    wordChunks :: Text -> [Text]
    wordChunks w
      | stringWidth w <= limit = [w]
      | otherwise = let (a, b) = splitAtWidth limit w in a : wordChunks b

    splitAtWidth :: Int -> Text -> (Text, Text)
    splitAtWidth k t = goS 0 0 (T.unpack t)
      where
        goS _ idx [] = (T.take idx t, T.drop idx t)
        goS acc idx (ch : rest)
          | acc + cw > k = (T.take idx t, T.drop idx t)
          | otherwise = goS (acc + cw) (idx + 1) rest
          where
            cw = runeW ch
        runeW c = stringWidth (T.singleton c)

    -- Greedy fill: keep adding chunks while they fit, breaking otherwise.
    wrapLine :: Text -> [Text]
    wrapLine l = go "" 0 (concatMap wordChunks (T.words l))
      where
        go cur _ [] = [cur]
        go cur curW (w : ws)
          | T.null cur = go w (stringWidth w) ws
          | curW + 1 + stringWidth w <= limit = go (cur <> " " <> w) (curW + 1 + stringWidth w) ws
          | otherwise = cur : go w (stringWidth w) ws


-- Size ----------------------------------------------------------------------

-- | The cell width of a (possibly multi-line) string.
styleWidth :: Text -> Int
styleWidth str = foldr (max . stringWidth) 0 (T.splitOn "\n" str)


-- | The height of a string (number of lines).
styleHeight :: Text -> Int
styleHeight str = T.count "\n" str + 1


styleSize :: Text -> (Int, Int)
styleSize str = (styleWidth str, styleHeight str)


-- Joining -------------------------------------------------------------------

joinHorizontal :: Position -> [Text] -> Text
joinHorizontal pos strs = case strs of
  [] -> ""
  [x] -> x
  _ ->
    let blocks0 = map (fst . getLines) strs
        maxWidths = map (snd . getLines) strs
        maxHeight = maximum (map length blocks0)
        pad b =
          if length b >= maxHeight
            then b
            else
              let extra = maxHeight - length b
                  blanks = replicate extra ""
               in case pos of
                    p
                      | p == top -> b ++ blanks
                      | p == bottom -> blanks ++ b
                      | otherwise ->
                          let n = extra
                              split = round (fromIntegral n * posValue pos) :: Int
                              tp = n - split
                              bt = n - tp
                           in drop tp blanks ++ b ++ take bt blanks
        blocks = map pad blocks0
        rows = length (head blocks)
        mkRow i =
          T.concat
            [ block !! i <> T.replicate (mw - stringWidth (block !! i)) " "
            | (block, mw) <- zip blocks maxWidths
            ]
     in T.intercalate "\n" [mkRow i | i <- [0 .. rows - 1]]


joinVertical :: Position -> [Text] -> Text
joinVertical pos strs = case strs of
  [] -> ""
  [x] -> x
  _ ->
    let blocks = map (fst . getLines) strs
        maxW = maximum (map (snd . getLines) strs)
        renderBlock block =
          map
            ( \line ->
                let w = maxW - stringWidth line
                 in case pos of
                      p
                        | p == left -> line <> T.replicate w " "
                        | p == right -> T.replicate w " " <> line
                        | otherwise ->
                            if w < 1
                              then line
                              else
                                let split = round (fromIntegral w * posValue pos) :: Int
                                    rgt = w - split
                                    lft = w - rgt
                                 in T.replicate lft " " <> line <> T.replicate rgt " "
            )
            block
     in T.intercalate "\n" (concatMap renderBlock blocks)


-- Placement -----------------------------------------------------------------

place :: Int -> Int -> Position -> Position -> Text -> Text
place w h hPos vPos str = placeVertical h vPos (placeHorizontal w hPos str)


placeHorizontal :: Int -> Position -> Text -> Text
placeHorizontal w pos str =
  let (ls, contentWidth) = getLines str
      gap = w - contentWidth
   in if gap <= 0
        then str
        else
          T.intercalate
            "\n"
            ( map
                ( \l ->
                    let short = max 0 (contentWidth - stringWidth l)
                     in case pos of
                          p
                            | p == left -> l <> T.replicate (gap + short) " "
                            | p == right -> T.replicate (gap + short) " " <> l
                            | otherwise ->
                                let totalGap = gap + short
                                    split = round (fromIntegral totalGap * posValue pos) :: Int
                                    lft = totalGap - split
                                    rgt = totalGap - lft
                                 in T.replicate lft " " <> l <> T.replicate rgt " "
                )
                ls
            )


placeVertical :: Int -> Position -> Text -> Text
placeVertical h pos str =
  let contentHeight = T.count "\n" str + 1
      gap = h - contentHeight
      (_, w) = getLines str
      emptyLine = T.replicate w " "
   in if gap <= 0
        then str
        else case pos of
          p
            | p == top ->
                str <> "\n" <> T.intercalate "\n" (replicate gap emptyLine)
            | p == bottom ->
                T.concat (replicate gap (emptyLine <> "\n")) <> str
            | otherwise ->
                let split = round (fromIntegral gap * posValue pos) :: Int
                    tp = gap - split
                    bt = gap - tp
                 in T.concat (replicate tp (emptyLine <> "\n"))
                      <> str
                      <> T.concat (replicate bt ("\n" <> emptyLine))


-- StyleRunes ----------------------------------------------------------------

-- | Apply @matched@ to runes at the given (rune) indices and @unmatched@ to
-- the rest. Indices out of bounds are ignored. Adjacent runes with the same
-- match status are grouped and styled together, matching lipgloss.
styleRunes :: Text -> [Int] -> Style -> Style -> Text
styleRunes str indices matched unmatched = go 0 "" (T.unpack str)
  where
    isMatch i = i `elem` indices
    go _ _ [] = ""
    go i grp (ch : rest) =
      let grp' = grp <> T.singleton ch
          matches = isMatch i
          nextMatches = isMatch (i + 1)
          atEnd = null rest
       in if matches /= nextMatches || atEnd
            then render (if matches then matched else unmatched) [grp'] <> go (i + 1) "" rest
            else go (i + 1) grp' rest
