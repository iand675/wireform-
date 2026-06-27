{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 6638 §8.1 @Schedule-Reply@ — a CalDAV scheduling request header an
Attendee includes (typically on @DELETE@) to control whether the server
sends a scheduling reply to the Organizer on its behalf.

== Grammar

@
Schedule-Reply = \"T\" / \"F\"
@

@T@ requests that the server send the scheduling message; @F@ suppresses it.
Modelled as a 'Bool' (@True@ = @T@, @False@ = @F@).

<https://www.rfc-editor.org/rfc/rfc6638#section-8.1 RFC 6638 §8.1>

See also: "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.IfScheduleTagMatch", "Network.HTTP.Headers.CalDAVTimezones", "Network.HTTP.Headers.CalManagedID".
-}
module Network.HTTP.Headers.ScheduleReply (
  ScheduleReply (..),
  scheduleReplyParser,
  renderScheduleReply,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hScheduleReply)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Schedule-Reply@ flag: 'True' renders as @T@, 'False' as @F@.
newtype ScheduleReply = ScheduleReply {scheduleReply :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader ScheduleReply where
  type ParseFailure ScheduleReply = String
  type Cardinality ScheduleReply = 'ZeroOrOne
  type Direction ScheduleReply = 'Request


  parseFromHeaders _ headers = case runParser scheduleReplyParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Schedule-Reply header: " <> show rest
    Fail -> Left "Failed to parse Schedule-Reply header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderScheduleReply


  headerName _ = hScheduleReply


scheduleReplyParser :: ParserT st String ScheduleReply
scheduleReplyParser =
  ScheduleReply <$> ((True <$ $(char 'T')) <|> (False <$ $(char 'F')))


renderScheduleReply :: ScheduleReply -> M.Builder
renderScheduleReply (ScheduleReply b) = M.char7 (if b then 'T' else 'F')
