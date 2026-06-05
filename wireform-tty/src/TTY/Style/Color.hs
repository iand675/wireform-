-- | Styling color types and the renderer, a port of lipgloss's @color.go@ and
-- @renderer.go@.
--
-- A 'TerminalColor' is a high-level color spec (a plain color, an
-- adaptive light\/dark pair, or a profile-complete value). A 'Renderer'
-- pins the color 'Profile' and background darkness, which together resolve a
-- 'TerminalColor' down to a concrete "TTY.Color" 'Color'.
module TTY.Style.Color
  ( Renderer (..)
  , defaultRenderer
  , CompleteColor (..)
  , TerminalColor (..)
  , color
  , ansiColor
  , adaptiveColor
  , completeColor
  , completeAdaptiveColor
  , noColor
  , isNoColor
  , resolveColor
  , terminalColorRGBA
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import TTY.Color (Profile (..))
import qualified TTY.Color as C


-- | A renderer pins the color profile and background darkness. Unlike the Go
-- original (which detects these from the environment via @sync.Once@), this is
-- a plain immutable value; 'defaultRenderer' assumes a truecolor, dark-bg
-- terminal.
data Renderer = Renderer
  { rendererColorProfile :: Profile
  , rendererHasDarkBackground :: Bool
  }
  deriving stock (Eq, Show)


defaultRenderer :: Renderer
defaultRenderer = Renderer TrueColor True


-- | Exact values for truecolor, ANSI256, and ANSI profiles (no degradation).
data CompleteColor = CompleteColor
  { completeTrueColor :: Text
  , completeANSI256 :: Text
  , completeANSI :: Text
  }
  deriving stock (Eq, Show)


-- | A high-level color specification.
data TerminalColor
  = TCNoColor
  | TCColor Text -- ^ hex (@"#rrggbb"@) or ANSI index string
  | TCANSIColor Int
  | TCAdaptiveColor Text Text -- ^ light, dark
  | TCCompleteColor CompleteColor
  | TCCompleteAdaptiveColor CompleteColor CompleteColor -- ^ light, dark
  deriving stock (Eq, Show)


-- | @lipgloss.Color@.
color :: Text -> TerminalColor
color = TCColor


-- | @lipgloss.ANSIColor@.
ansiColor :: Int -> TerminalColor
ansiColor = TCANSIColor


-- | @lipgloss.AdaptiveColor{Light, Dark}@.
adaptiveColor :: Text -> Text -> TerminalColor
adaptiveColor = TCAdaptiveColor


-- | @lipgloss.CompleteColor{...}@.
completeColor :: CompleteColor -> TerminalColor
completeColor = TCCompleteColor


-- | @lipgloss.CompleteAdaptiveColor{Light, Dark}@.
completeAdaptiveColor :: CompleteColor -> CompleteColor -> TerminalColor
completeAdaptiveColor = TCCompleteAdaptiveColor


-- | The absence of color.
noColor :: TerminalColor
noColor = TCNoColor


isNoColor :: TerminalColor -> Bool
isNoColor TCNoColor = True
isNoColor _ = False


-- | Resolve a high-level color to a concrete terminal 'C.Color' for the given
-- renderer, or 'Nothing' if it is invalid (mirrors termenv's @nil@).
resolveColor :: Renderer -> TerminalColor -> Maybe C.Color
resolveColor r = \case
  TCNoColor -> Just C.NoColor
  TCColor s -> C.profileColor profile s
  TCANSIColor n -> C.profileColor profile (T.pack (show n))
  TCAdaptiveColor light dark ->
    C.profileColor profile (if dark' then dark else light)
  TCCompleteColor cc -> completeFor cc
  TCCompleteAdaptiveColor light dark ->
    completeFor (if dark' then dark else light)
  where
    profile = rendererColorProfile r
    dark' = rendererHasDarkBackground r
    completeFor cc = case profile of
      TrueColor -> C.profileColor TrueColor (completeTrueColor cc)
      ANSI256 -> C.profileColor ANSI256 (completeANSI256 cc)
      ANSI -> C.profileColor ANSI (completeANSI cc)
      Ascii -> Just C.NoColor


-- | The RGBA of a color under a renderer (lipgloss's @TerminalColor.RGBA@).
terminalColorRGBA :: Renderer -> TerminalColor -> (Int, Int, Int, Int)
terminalColorRGBA r tc =
  C.rgbaOf (maybe C.NoColor id (resolveColor r tc))
