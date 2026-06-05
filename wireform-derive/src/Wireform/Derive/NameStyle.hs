-- | Rich DSL for transforming Haskell field selector names into
-- on-the-wire field names.
--
-- 'NameStyle' is intentionally a pure ADT with a 'Data' instance so that
-- it can survive splice-time annotation reflection: a 'NameStyle' value
-- attached via @ANN@ can be reified, fully evaluated at compile time,
-- and inlined into the generated encode/decode code as a literal 'Text'.
--
-- A handful of constructors (notably 'Idiomatic') refer to the active
-- backend rather than to a fixed transformation. These are resolved by
-- 'resolveIdiomatic' before 'applyStyle' is run.
module Wireform.Derive.NameStyle
  ( -- * The style DSL
    NameStyle (..)
  , andThen
    -- * Application
  , applyStyle
    -- * Idiomatic resolution
  , idiomaticFor
  , resolveIdiomatic
    -- * Building blocks (re-exported for testing)
  , toSnakeCase
  , toUpperSnake
  , toKebabCase
  , toUpperKebab
  , toCamelCase
  , toPascalCase
  ) where

import Control.DeepSeq (NFData)
import Data.Char (isLower, isUpper, toUpper)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

import Wireform.Derive.Backend

-- | A composable rename strategy. 'NameStyle' is a closed ADT so that
-- per-format derivers can interpret it at splice time and bake the
-- result into generated wire-key 'Text' literals (zero runtime cost).
data NameStyle
  = -- | @snake_case@. Boundaries inferred from camelCase / PascalCase
    -- humps: @"personName"@ becomes @"person_name"@.
    SnakeCase
  | -- | @SCREAMING_SNAKE_CASE@.
    UpperSnake
  | -- | @kebab-case@.
    KebabCase
  | -- | @SCREAMING-KEBAB-CASE@.
    UpperKebab
  | -- | @lowerCamelCase@. The first character is lowercased; subsequent
    -- humps preserved.
    CamelCase
  | -- | @UpperCamelCase@ / @PascalCase@.
    PascalCase
  | -- | All lowercase.
    LowerCase
  | -- | All uppercase.
    UpperCase
  | -- | Strip a literal prefix if present; otherwise leave the input
    -- unchanged.
    StripPrefix !Text
  | -- | Strip a literal suffix if present; otherwise unchanged.
    StripSuffix !Text
  | -- | Strip a prefix case-insensitively.
    StripPrefixCI !Text
  | -- | Strip a suffix case-insensitively.
    StripSuffixCI !Text
  | -- | Drop @n@ characters from the start of the input.
    DropChars !Int
  | -- | Take only the first @n@ characters of the input.
    TakeChars !Int
  | -- | Replace every occurrence of the first 'Text' with the second.
    Replace !Text !Text
  | -- | Replace only the first occurrence.
    ReplaceFirst !Text !Text
  | -- | Sequential composition: @Compose a b@ first applies @a@, then
    -- @b@.
    Compose !NameStyle !NameStyle
  | -- | Identity transformation.
    NoStyle
  | -- | Apply the active backend's idiomatic naming convention. This
    -- constructor is resolved by 'resolveIdiomatic' before
    -- 'applyStyle' is called.
    Idiomatic
  deriving stock (Eq, Ord, Show, Data, Generic, Lift)
  deriving anyclass (NFData, Hashable)

-- | Left-to-right composition. @a `andThen` b@ first applies @a@, then
-- @b@.
andThen :: NameStyle -> NameStyle -> NameStyle
andThen = Compose
infixl 1 `andThen`

-- | Substitute every 'Idiomatic' marker with the concrete style for
-- the given backend.
resolveIdiomatic :: Backend -> NameStyle -> NameStyle
resolveIdiomatic b = go
  where
    go Idiomatic        = idiomaticFor b
    go (Compose x y)    = Compose (go x) (go y)
    go s                = s

-- | The conventional rename style for a given backend.
--
-- Defaults follow the dominant on-the-wire convention for each format:
--
-- * 'backendJSON' / 'backendProto' → 'CamelCase' (proto3's @json_name@
--   default is @lowerCamelCase@; common JS / TS APIs use the same).
-- * 'backendEDN' / 'backendYAML' → 'KebabCase'.
-- * 'backendTOML' → 'SnakeCase'.
-- * 'backendXML' → 'PascalCase'.
-- * 'backendCBOR' / 'backendMsgPack' / 'backendThrift' / 'backendBinary' /
--   'backendCSV' / 'backendEDI' / 'backendTextFormat' → 'NoStyle' (selector base used
--   verbatim).
--
-- Downstream backends not listed here fall through to 'NoStyle'.
idiomaticFor :: Backend -> NameStyle
idiomaticFor b
  | b == backendJSON        = CamelCase
  | b == backendNDJSON      = CamelCase
  | b == backendProto       = CamelCase
  | b == backendBSON        = CamelCase
  | b == backendION         = CamelCase
  | b == backendEDN         = KebabCase
  | b == backendYAML        = KebabCase
  | b == backendHTML        = KebabCase
  | b == backendTOML        = SnakeCase
  | b == backendAvro        = SnakeCase
  | b == backendBond        = SnakeCase
  | b == backendArrow       = SnakeCase
  | b == backendParquet     = SnakeCase
  | b == backendOrc         = SnakeCase
  | b == backendIceberg     = SnakeCase
  | b == backendXML         = PascalCase
  | b == backendCBOR        = NoStyle
  | b == backendMsgPack     = NoStyle
  | b == backendThrift      = NoStyle
  | b == backendBinary      = NoStyle
  | b == backendCSV         = NoStyle
  | b == backendEDI         = NoStyle
  | b == backendTextFormat  = NoStyle
  | b == backendASN1        = NoStyle  -- ASN.1 names are positional
  | b == backendBencode     = NoStyle  -- byte-string keys, verbatim
  | b == backendFlatBuffers = SnakeCase
  | b == backendCapnProto   = CamelCase
  | b == backendFory        = SnakeCase
  | otherwise               = NoStyle

-- | Apply a style. 'Idiomatic' constructors that have not been
-- resolved by 'resolveIdiomatic' degrade to 'NoStyle' rather than
-- raise.
applyStyle :: NameStyle -> Text -> Text
applyStyle = go
  where
    go SnakeCase          = toSnakeCase
    go UpperSnake         = T.toUpper . toSnakeCase
    go KebabCase          = toKebabCase
    go UpperKebab         = T.toUpper . toKebabCase
    go CamelCase          = toCamelCase
    go PascalCase         = toPascalCase
    go LowerCase          = T.toLower
    go UpperCase          = T.toUpper
    go (StripPrefix p)    = stripPrefixCS p
    go (StripSuffix s)    = stripSuffixCS s
    go (StripPrefixCI p)  = stripPrefixCI p
    go (StripSuffixCI s)  = stripSuffixCI s
    go (DropChars n)      = T.drop n
    go (TakeChars n)      = T.take n
    go (Replace o n)      = T.replace o n
    go (ReplaceFirst o n) = replaceFirst o n
    go (Compose a b)      = applyStyle b . applyStyle a
    go NoStyle            = id
    go Idiomatic          = id

-- ---------------------------------------------------------------------------
-- Hump-aware splitting
-- ---------------------------------------------------------------------------

-- | Split an identifier into lowercase \"words\" on:
--
-- * existing @_@ / @-@ / spaces, and
-- * camelCase / PascalCase humps (so @PersonName@ → @["person", "name"]@,
--   and @HTTPRequest@ → @["http", "request"]@).
--
-- Used by every snake / kebab / camel transformation.
splitWords :: Text -> [Text]
splitWords =
  filter (not . T.null)
    . map T.toLower
    . concatMap (map T.pack . splitCamelStr . T.unpack)
    . T.split isPunct
  where
    isPunct c = c == '_' || c == '-' || c == ' '

-- | Insert word boundaries at every @lower → upper@ transition and at
-- every @upper-run → (Upper, lower)@ transition. E.g.
-- @"HTTPRequest"@ → @["HTTP", "Request"]@.
splitCamelStr :: String -> [String]
splitCamelStr [] = []
splitCamelStr (c0 : cs0) = go [c0] cs0
  where
    -- @go acc xs@ accumulates the current word in reverse in @acc@.
    go :: String -> String -> [String]
    go acc []                                          = [reverse acc]
    go acc@(prev : _) (x : xs)
      | isLower prev && isUpper x                      = reverse acc : go [x] xs
      | isUpper x, isAcronymRun acc, peekLower xs      =
          -- We are inside an upper-run and the *next* char after @x@
          -- starts a lowercase tail. Close the acronym at @acc@; @x@
          -- begins the new word.
          reverse acc : go [x] xs
      | otherwise                                      = go (x : acc) xs
    go [] _ = error "splitCamelStr: impossible empty acc"

    isAcronymRun :: String -> Bool
    isAcronymRun s = length s >= 1 && all isUpper s

    peekLower :: String -> Bool
    peekLower (y : _) = isLower y
    peekLower []      = False

-- | Lowercase, underscore-separated.
toSnakeCase :: Text -> Text
toSnakeCase = T.intercalate "_" . splitWords

-- | Uppercase, underscore-separated.
toUpperSnake :: Text -> Text
toUpperSnake = T.toUpper . toSnakeCase

-- | Lowercase, hyphen-separated.
toKebabCase :: Text -> Text
toKebabCase = T.intercalate "-" . splitWords

-- | Uppercase, hyphen-separated.
toUpperKebab :: Text -> Text
toUpperKebab = T.toUpper . toKebabCase

-- | @lowerCamelCase@.
toCamelCase :: Text -> Text
toCamelCase t = case splitWords t of
  []     -> T.empty
  (w:ws) -> T.concat (w : map capitalise ws)

-- | @UpperCamelCase@ / @PascalCase@.
toPascalCase :: Text -> Text
toPascalCase = T.concat . map capitalise . splitWords

capitalise :: Text -> Text
capitalise t = case T.uncons t of
  Nothing      -> t
  Just (c, cs) -> T.cons (toUpper c) cs

-- ---------------------------------------------------------------------------
-- Affix stripping
-- ---------------------------------------------------------------------------

stripPrefixCS :: Text -> Text -> Text
stripPrefixCS p t = case T.stripPrefix p t of
  Just rest -> rest
  Nothing   -> t

stripSuffixCS :: Text -> Text -> Text
stripSuffixCS s t = case T.stripSuffix s t of
  Just rest -> rest
  Nothing   -> t

stripPrefixCI :: Text -> Text -> Text
stripPrefixCI p t
  | T.length t >= T.length p
  , T.toLower (T.take (T.length p) t) == T.toLower p
    = T.drop (T.length p) t
  | otherwise = t

stripSuffixCI :: Text -> Text -> Text
stripSuffixCI s t
  | T.length t >= T.length s
  , T.toLower (T.takeEnd (T.length s) t) == T.toLower s
    = T.dropEnd (T.length s) t
  | otherwise = t

-- ---------------------------------------------------------------------------
-- Misc helpers
-- ---------------------------------------------------------------------------

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle replacement haystack
  | T.null needle = haystack
  | otherwise = case T.breakOn needle haystack of
      (before, after) ->
        case T.stripPrefix needle after of
          Just rest -> before <> replacement <> rest
          Nothing   -> haystack
