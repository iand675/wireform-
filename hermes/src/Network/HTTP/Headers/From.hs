{- |
The @From@ request header carries an email address for the human user who
controls the requesting user agent, chiefly so an operator whose automated
client misbehaves can be contacted. Its value is a single RFC 5322 mailbox;
the grammar is reused wholesale from "Network.Mailbox".

@
From = mailbox
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.2>

See also: "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.Host".
-}
module Network.HTTP.Headers.From (
  From (..),
  fromParser,
  renderFrom,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hFrom)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.Mailbox (Mailbox, mailboxParser, parseMailbox, renderMailbox)


-- | The parsed value of a @From@ header: a single mailbox.
newtype From = From {fromMailbox :: Mailbox}
  deriving stock (Eq, Show)


instance KnownHeader From where
  type ParseFailure From = String
  type Cardinality From = 'ZeroOrOne
  type Direction From = 'Request


  parseFromHeaders _ headers = From <$> parseMailbox (NE.head headers)


  renderToHeaders _ = M.toStrictByteString . renderFrom


  headerName _ = hFrom


fromParser :: ParserT st String From
fromParser = From <$> mailboxParser


renderFrom :: From -> M.Builder
renderFrom = renderMailbox . fromMailbox
