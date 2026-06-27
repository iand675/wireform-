{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7809 §7.1 @CalDAV-Timezones@ — a request header by which a CalDAV client
tells the server whether to include time-zone (@VTIMEZONE@) data in the
calendar object resources it returns, when the server supports the time zones
by reference extension.

== Grammar

@
CalDAV-Timezones = \"T\" / \"F\"
@

@T@ requests inclusion of time-zone data, @F@ suppresses it. Modelled as a
'Bool' (@True@ = @T@, @False@ = @F@).

<https://www.rfc-editor.org/rfc/rfc7809#section-7.1 RFC 7809 §7.1>

See also: "Network.HTTP.Headers.ScheduleReply", "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.IfScheduleTagMatch", "Network.HTTP.Headers.CalManagedID", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.CalDAVTimezones (
  CalDAVTimezones (..),
  calDAVTimezonesParser,
  renderCalDAVTimezones,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCalDAVTimezones)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @CalDAV-Timezones@ flag: 'True' renders as @T@, 'False' as @F@.
newtype CalDAVTimezones = CalDAVTimezones {calDAVTimezones :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader CalDAVTimezones where
  type ParseFailure CalDAVTimezones = String
  type Cardinality CalDAVTimezones = 'ZeroOrOne
  type Direction CalDAVTimezones = 'Request


  parseFromHeaders _ headers = case runParser calDAVTimezonesParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing CalDAV-Timezones header: " <> show rest
    Fail -> Left "Failed to parse CalDAV-Timezones header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCalDAVTimezones


  headerName _ = hCalDAVTimezones


calDAVTimezonesParser :: ParserT st String CalDAVTimezones
calDAVTimezonesParser =
  CalDAVTimezones <$> ((True <$ $(char 'T')) <|> (False <$ $(char 'F')))


renderCalDAVTimezones :: CalDAVTimezones -> M.Builder
renderCalDAVTimezones (CalDAVTimezones b) = M.char7 (if b then 'T' else 'F')
