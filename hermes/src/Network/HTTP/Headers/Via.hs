{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9110 §7.6.3 @Via@ — appended by intermediaries (proxies, gateways)
to record the protocol and recipient of each hop a message passed
through, optionally annotated with a comment naming the software.

== Grammar

@
Via               = #( received-protocol RWS received-by [ RWS comment ] )
received-protocol = [ protocol-name "/" ] protocol-version
received-by       = pseudonym / ( uri-host [ ":" port ] )
pseudonym         = token
@

@received-by@ is captured verbatim (a pseudonym token, or a
@host[:port]@). Comments are parsed and rendered as flat text; nested
parentheses (permitted by RFC 9110 but essentially never seen on @Via@)
are not modelled.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.3>

See also: "Network.HTTP.Headers.MaxForwards", "Network.HTTP.Headers.Forwarded",
"Network.HTTP.Headers.ProxyStatus", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.Via (
  Via (..),
  ViaEntry (..),
  ReceivedProtocol (..),
  viaParser,
  renderVia,
) where

import Control.Monad.Combinators (between)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hVia)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | @received-protocol@: an optional protocol name with a required version.
data ReceivedProtocol = ReceivedProtocol
  { receivedProtocolName :: !(Maybe ST.ShortText)
  , receivedProtocolVersion :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | A single @Via@ hop.
data ViaEntry = ViaEntry
  { viaReceivedProtocol :: !ReceivedProtocol
  , viaReceivedBy :: !ST.ShortText
  , viaComment :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


-- | A @Via@ header value: a non-empty list of hops.
newtype Via = Via {viaEntries :: NonEmpty ViaEntry}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader Via where
  type ParseFailure Via = String
  type Cardinality Via = 'ZeroOrMore
  type Direction Via = 'RequestAndResponse


  parseFromHeaders _ neHeaders = sconcat <$> traverse parseLine neHeaders
    where
      parseLine bs = case runParser viaParser bs of
        OK v "" -> Right v
        OK _ rest -> Left $ "Unconsumed input after parsing Via header: " <> show rest
        Fail -> Left "Failed to parse Via header"
        Err e -> Left e
  renderToHeaders _ = pure . M.toStrictByteString . renderVia
  headerName _ = hVia


viaParser :: ParserT st String Via
viaParser = Via <$> (viaEntryParser `sepBy1` (ows *> $(char ',') *> ows))


viaEntryParser :: ParserT st String ViaEntry
viaEntryParser = do
  proto <- receivedProtocolParser
  rws
  by <- receivedByParser
  mc <- optional (rws *> viaCommentParser)
  pure (ViaEntry proto by mc)


receivedProtocolParser :: ParserT st String ReceivedProtocol
receivedProtocolParser = do
  tok <- rfc9110Token
  branch
    $(char '/')
    (ReceivedProtocol (Just tok) <$> rfc9110Token)
    (pure (ReceivedProtocol Nothing tok))


-- | @received-by@ runs up to the next whitespace, comma, or comment open.
receivedByParser :: ParserT st String ST.ShortText
receivedByParser = shortASCIIFromParser_ (some (satisfyAscii isReceivedByChar))
  where
    isReceivedByChar c = not (isOWS c || c == ',' || c == '(')


-- | A flat (non-nested) @comment@: @"(" *ctext ")"@.
viaCommentParser :: ParserT st String Text
viaCommentParser =
  between $(char '(') $(char ')') (T.pack <$> many (satisfyAscii isCText))
  where
    isCText c = c == '\t' || (c >= ' ' && c <= '~' && c /= '(' && c /= ')' && c /= '\\')


renderVia :: Via -> M.Builder
renderVia (Via entries) = sepByCommas1 (fmap renderViaEntry entries)


renderViaEntry :: ViaEntry -> M.Builder
renderViaEntry (ViaEntry proto by mc) =
  renderReceivedProtocol proto
    <> M.char7 ' '
    <> shortText by
    <> maybe mempty (\c -> M.char7 ' ' <> M.char7 '(' <> M.textUtf8 c <> M.char7 ')') mc


renderReceivedProtocol :: ReceivedProtocol -> M.Builder
renderReceivedProtocol (ReceivedProtocol mname version) =
  maybe mempty (\n -> shortText n <> M.char7 '/') mname <> shortText version
