{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 6638 §8.3 @Schedule-Tag@ — a response header carrying an opaque
scheduling entity-tag for a calendar object resource, analogous to @ETag@
but changing only when a scheduling operation (not an ordinary edit) alters
the resource.

== Grammar

@
Schedule-Tag = opaque-tag
opaque-tag   = quoted-string
@

The opaque value is stored unquoted; rendering re-wraps it in DQUOTEs. As
with @ETag@, the value itself never contains a DQUOTE, so no escaping is
required.

<https://www.rfc-editor.org/rfc/rfc6638#section-8.3 RFC 6638 §8.3>

See also: "Network.HTTP.Headers.IfScheduleTagMatch", "Network.HTTP.Headers.ScheduleReply", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfNoneMatch".
-}
module Network.HTTP.Headers.ScheduleTag (
  ScheduleTag (..),
  scheduleTagParser,
  renderScheduleTag,
) where

import Control.Monad.Combinators (between)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hScheduleTag)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | An opaque @Schedule-Tag@ value (the content found between the DQUOTEs).
newtype ScheduleTag = ScheduleTag {scheduleTag :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ScheduleTag where
  type ParseFailure ScheduleTag = String
  type Cardinality ScheduleTag = 'ZeroOrOne
  type Direction ScheduleTag = 'Response


  parseFromHeaders _ headers = case runParser scheduleTagParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Schedule-Tag header: " <> show rest
    Fail -> Left "Failed to parse Schedule-Tag header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderScheduleTag


  headerName _ = hScheduleTag


-- | Bytes permitted inside an @opaque-tag@: VCHAR except DQUOTE, plus obs-text.
opaqueTagCharSet :: CharSet
opaqueTagCharSet = "\x21" <> CharSet.fromList ['\x23' .. '\x7E'] <> obsTextCharSet


scheduleTagParser :: ParserT st String ScheduleTag
scheduleTagParser =
  ScheduleTag
    <$> between
      $(char '"')
      $(char '"')
      (shortASCIIFromParser_ (many (satisfyAscii (`CharSet.member` opaqueTagCharSet))))


renderScheduleTag :: ScheduleTag -> M.Builder
renderScheduleTag (ScheduleTag t) =
  M.char8 '"' <> M.shortByteString (ST.toShortByteString t) <> M.char8 '"'
