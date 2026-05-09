{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Text.Regex.ECMA262.Base
-- Description : regex-base instances for ECMA262 regex
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides instances of the regex-base typeclasses (RegexMaker,
-- RegexLike, RegexContext) for the ECMA262 regex library, making it compatible
-- with the standard Haskell regex interface.
--
-- This allows you to use the =~ and =~~ operators:
--
-- @
-- import Text.Regex.ECMA262.Base
--
-- "hello" =~ "h.*o" :: Bool                    -- True
-- "hello" =~ "l+" :: String                    -- "ll"
-- "123-456" =~ "(\\d+)-(\\d+)" :: [[String]]   -- [["123-456","123","456"]]
-- @
module Text.Regex.ECMA262.Base
  ( -- * Re-exports from main module
    module Text.Regex.ECMA262
    -- * Re-exports from regex-base for convenience
  , (=~)
  , (=~~)
  ) where

import Text.Regex.ECMA262 hiding (match, matchAll)
import qualified Text.Regex.ECMA262 as E262
import Text.Regex.Base.RegexLike
import Text.Regex.Base.Context
import Text.Regex.Base.Impl
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Array (Array, listArray)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (MonadFail)

-- Placeholder types for regex-base compatibility
type CompOption = ()
type ExecOption = ()

-- ============================================================================
-- ByteString instances
-- ============================================================================

-- | RegexMaker instance for compiling ByteString patterns
instance RegexMaker Regex CompOption ExecOption BS.ByteString where
  makeRegexOpts _ _ pattern =
    case unsafePerformIO $ compile pattern [] of
      Left err -> error $ "Regex compilation error: " ++ err
      Right r -> r

  makeRegexOptsM _ _ pattern = do
    result <- unsafePerformIO $ compile pattern []
    case result of
      Left err -> fail $ "Regex compilation error: " ++ err
      Right r -> return r

-- | RegexLike instance for matching against ByteString
instance RegexLike Regex BS.ByteString where
  matchOnce regex source =
    case unsafePerformIO $ E262.match regex source of
      Nothing -> Nothing
      Just m -> Just $ buildMatchArray m

  matchAll regex source =
    map buildMatchArray $ unsafePerformIO $ E262.matchAll regex source

  matchCount regex source =
    length $ unsafePerformIO $ E262.matchAll regex source

  matchTest regex source =
    case unsafePerformIO $ E262.match regex source of
      Just _ -> True
      Nothing -> False

  matchAllText regex source =
    map (buildMatchText source) $ unsafePerformIO $ E262.matchAll regex source

  matchOnceText regex source =
    case unsafePerformIO $ E262.match regex source of
      Nothing -> Nothing
      Just m -> Just (before (matchStart m) source,
                     buildMatchText source m,
                     after (matchEnd m) source)

-- | RegexContext instances for ByteString
instance RegexContext Regex BS.ByteString BS.ByteString where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString (Maybe (BS.ByteString, BS.ByteString, BS.ByteString)) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString (Maybe (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString (BS.ByteString, BS.ByteString, BS.ByteString) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString]) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString [[BS.ByteString]] where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString [BS.ByteString] where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString Bool where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex BS.ByteString Int where
  match = polymatch
  matchM = polymatchM

-- ============================================================================
-- String instances
-- ============================================================================

-- | RegexMaker instance for compiling String patterns
instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts opts exec pattern =
    makeRegexOpts opts exec (BS8.pack pattern)

  makeRegexOptsM opts exec pattern =
    makeRegexOptsM opts exec (BS8.pack pattern)

-- | RegexLike instance for matching against String
instance RegexLike Regex String where
  matchOnce regex source =
    matchOnce regex (BS8.pack source)

  matchAll regex source =
    matchAll regex (BS8.pack source)

  matchCount regex source =
    matchCount regex (BS8.pack source)

  matchTest regex source =
    matchTest regex (BS8.pack source)

  matchAllText regex source =
    map (fmap BS8.unpack) $ matchAllText regex (BS8.pack source)

  matchOnceText regex source =
    fmap (\(a,b,c) -> (BS8.unpack a, fmap BS8.unpack b, BS8.unpack c)) $
      matchOnceText regex (BS8.pack source)

-- | RegexContext instances for String
instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String (Maybe (String, String, String)) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String (Maybe (String, String, String, [String])) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String (String, String, String) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String (String, String, String, [String]) where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String [[String]] where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String [String] where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String Bool where
  match = polymatch
  matchM = polymatchM

instance RegexContext Regex String Int where
  match = polymatch
  matchM = polymatchM

-- ============================================================================
-- Helper functions
-- ============================================================================

-- | Build a MatchArray from a Match
buildMatchArray :: Match -> MatchArray
buildMatchArray m =
  let fullMatch = (matchStart m, matchEnd m - matchStart m)
      captureMatches = map (\(s, e, _) -> (s, e - s)) (captures m)
      allMatches = fullMatch : captureMatches
  in listArray (0, length allMatches - 1) allMatches

-- | Build a MatchText from a Match
buildMatchText :: source -> Match -> MatchText source
buildMatchText source m =
  let fullMatch = (matchText m, (matchStart m, matchEnd m - matchStart m))
      captureMatches = map (\(s, e, t) -> (t, (s, e - s))) (captures m)
      allMatches = fullMatch : captureMatches
  in listArray (0, length allMatches - 1) allMatches
