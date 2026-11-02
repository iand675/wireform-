{- | YAML separation is ASCII space and tab. Unicode spaces and NEL/LS/PS
remain scalar content under YAML 1.2, including at scalar boundaries.
-}
module YAML.Internal (
  isYamlSpace,
  stripYaml,
  stripYamlStart,
  stripYamlEnd,
  wordsYaml,
) where

import Data.Text (Text)
import Data.Text qualified as T


isYamlSpace :: Char -> Bool
isYamlSpace char = char == ' ' || char == '\t'
{-# INLINE isYamlSpace #-}


stripYaml :: Text -> Text
stripYaml = T.dropAround isYamlSpace
{-# INLINE stripYaml #-}


stripYamlStart :: Text -> Text
stripYamlStart = T.dropWhile isYamlSpace
{-# INLINE stripYamlStart #-}


stripYamlEnd :: Text -> Text
stripYamlEnd = T.dropWhileEnd isYamlSpace
{-# INLINE stripYamlEnd #-}


wordsYaml :: Text -> [Text]
wordsYaml = filter (not . T.null) . T.split isYamlSpace
