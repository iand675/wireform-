{-# LANGUAGE BangPatterns #-}
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

import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Foreign.Ptr (minusPtr, plusPtr)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTimeout)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1)
import Wireform.Encode.Direct (dBytes)


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


  renderToHeadersChecked _ (Timeout choices) = encodeTimeout choices


  headerName _ = hTimeout


timeTypeParser :: ParserT st String TimeType
timeTypeParser =
  $( switchAsciiCI
       [|
         case _ of
           "infinite" -> pure TimeInfinite
           "second-" -> TimeSeconds <$> boundedDecimalWord maxTimeoutSeconds (err "Timeout exceeds 2^32-1 seconds")
         |]
   )


timeoutParser :: ParserT st String Timeout
timeoutParser =
  Timeout <$> (httpList timeTypeParser >>= maybe failed pure . NE.nonEmpty)


maxTimeoutSeconds :: Word
maxTimeoutSeconds = fromIntegral (maxBound :: Word32)


encodeTimeout :: NE.NonEmpty TimeType -> Maybe BSI.ByteString
encodeTimeout (first NE.:| rest) = sizeChoices 0 first rest
  where
    sizeChoices !size choice remaining
      | TimeSeconds seconds <- choice, seconds > maxTimeoutSeconds = Nothing
      | otherwise =
          -- Allow space for "Second-", ten digits, and a comma and space.
          let bound = BSI.checkedAdd "Timeout" size 19
          in case remaining of
               [] -> Just $ BSI.unsafeCreateUptoN bound $ \ptr -> writeChoice ptr 0 first >>= writeRest ptr rest
               next : more -> sizeChoices bound next more
    writeRest _ [] !offset = pure offset
    writeRest ptr (next : more) !offset = do
      afterComma <- dBytes ptr offset ", "
      afterChoice <- writeChoice ptr afterComma next
      writeRest ptr more afterChoice
    writeChoice ptr offset TimeInfinite = dBytes ptr offset "Infinite"
    writeChoice ptr offset (TimeSeconds seconds) = do
      afterPrefix <- dBytes ptr offset "Second-"
      end <- Prim.runB Prim.word32Dec (fromIntegral seconds) (ptr `plusPtr` afterPrefix)
      pure (end `minusPtr` ptr)


renderTimeout :: Timeout -> M.Builder
renderTimeout (Timeout vs) =
  sepByCommas1 (fmap renderTimeType vs)
  where
    renderTimeType (TimeSeconds n) = "Second-" <> M.wordDec n
    renderTimeType TimeInfinite = "Infinite"
