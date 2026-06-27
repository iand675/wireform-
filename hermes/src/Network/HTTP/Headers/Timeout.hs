{-# LANGUAGE TemplateHaskell #-}

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

import qualified Control.Monad.Combinators.NonEmpty as NEC
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTimeout)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


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


  parseFromHeaders _ headers = case runParser timeoutParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Timeout header: " <> show rest
    Fail -> Left "Failed to parse Timeout header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderTimeout


  headerName _ = hTimeout


timeTypeParser :: ParserT st String TimeType
timeTypeParser =
  (TimeInfinite <$ $(string "Infinite"))
    <|> (TimeSeconds <$> ($(string "Second-") *> anyAsciiDecimalWord))


timeoutParser :: ParserT st String Timeout
timeoutParser = Timeout <$> (timeTypeParser `NEC.sepBy1` (ows *> $(char ',') *> ows))


renderTimeout :: Timeout -> M.Builder
renderTimeout (Timeout vs) =
  M.intersperse ", " (map renderTimeType (NE.toList vs))
  where
    renderTimeType (TimeSeconds n) = "Second-" <> M.wordDec n
    renderTimeType TimeInfinite = "Infinite"
