{- | The auth-slices demo's browser page, embedded into the binary at
compile time so @cabal run example-auth@ works from any directory.

The source of truth is the checked-in @example/auth.html@; it is read at
build time (and re-read on change via 'qAddDependentFile'). The server
still prefers a live copy on disk at run time, falling back to this
embedded snapshot — see @AuthMain.loadPage@.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AuthPage (embeddedPage) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Language.Haskell.TH.Syntax (lift, qAddDependentFile, runIO)

-- | The @auth.html@ page as UTF-8 bytes (the file is ASCII-only).
embeddedPage :: ByteString
embeddedPage =
  BS8.pack
    $( do
         (p, s) <-
           runIO $
             let go [] = error "AuthPage: auth.html not found for embedding"
                 go (path : rest) =
                   try (readFile path) >>= \case
                     Right t -> pure (path, t)
                     Left (_ :: SomeException) -> go rest
             in go ["example/auth.html", "wireform-lattice/example/auth.html"]
         qAddDependentFile p
         lift s
     )
