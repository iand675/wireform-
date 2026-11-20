{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
RFC 4918 §10.7 @Timeout@ request header — used with @LOCK@ to suggest
desired lock timeout value(s) to the server.

== Grammar

@
TimeOut       = 1#TimeType
TimeType      = ("Second-" DAVTimeOutVal | "Infinite")
DAVTimeOutVal = 1*DIGIT
@

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.7>

See also: "Network.HTTP.Headers.LockToken", "Network.HTTP.Headers.If", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.Timeout (
  Timeout (..),
  TimeType (..),
  timeoutParser,
  renderTimeout,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTimeout)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1)


-- | A single requested timeout value.
data TimeType
  = -- | @Second-<n>@, a finite timeout of @n@ seconds.
    TimeSeconds !Word
  | -- | @Infinite@, no timeout.
    TimeInfinite
  deriving stock (Eq, Show)


-- | The non-empty, comma-separated list of requested timeout values.
newtype Timeout = Timeout {timeoutValues :: NE.NonEmpty TimeType}
  deriving stock (Eq, Show)


instance KnownHeader Timeout where
  type ParseFailure Timeout = String
  type Cardinality Timeout = 'ZeroOrOne
  type Direction Timeout = 'Request


  parseFromHeaders _ fields =
    Timeout <$> (parseListFieldLines "Timeout" (httpList timeTypeParser) fields >>= requireNonEmptyList "Timeout")


  renderToHeaders _ = M.toStrictByteString . renderTimeout


  headerName _ = hTimeout


timeTypeParser :: ParserT st String TimeType
timeTypeParser =
  $( switchAsciiCI
       [|
         case _ of
           "infinite" -> pure TimeInfinite
           "second-" -> TimeSeconds <$> boundedDecimalWord 4_294_967_295 (err "Timeout exceeds 2^32-1 seconds")
         |]
   )


timeoutParser :: ParserT st String Timeout
timeoutParser =
  Timeout <$> (httpList timeTypeParser >>= maybe failed pure . NE.nonEmpty)


renderTimeout :: Timeout -> M.Builder
renderTimeout (Timeout vs) =
  sepByCommas1 (fmap renderTimeType vs)
  where
    renderTimeType (TimeSeconds n) = "Second-" <> M.wordDec n
    renderTimeType TimeInfinite = "Infinite"
