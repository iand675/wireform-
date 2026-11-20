{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9110 §7.6.3 @Via@ — appended by intermediaries (proxies, gateways)
to record the protocol and recipient of each hop a message passed
through, optionally annotated with a comment naming the software.

== Grammar

@
Via               = #( received-protocol RWS received-by [ RWS comment ] )
received-protocol = [ protocol-name "/" ] protocol-version
received-by       = pseudonym [ ":" port ]
pseudonym         = token
@

@received-by@ is captured verbatim. Bracketed IP literals are also accepted
as specified in RFC 7230. Comments use the shared RFC 9110 comment codec.

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

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (receivedBy)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hVia)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderComment, sepByCommas1, shortText)


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


-- | The advertised hops. 'ViaEmpty' represents a field with no entries.
data Via
  = Via {viaEntries :: NonEmpty ViaEntry}
  | ViaEmpty
  deriving stock (Eq, Show)


instance Semigroup Via where
  ViaEmpty <> other = other
  other <> ViaEmpty = other
  Via xs <> Via ys = Via (xs <> ys)


instance KnownHeader Via where
  type ParseFailure Via = String
  type Cardinality Via = 'ZeroOrMore
  type Direction Via = 'RequestAndResponse


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Via" (httpList viaEntryParser)
  renderToHeaders _ = pure . M.toStrictByteString . renderVia
  headerName _ = hVia


viaParser :: ParserT st String Via
viaParser = fromEntries <$> httpList viaEntryParser


fromEntries :: [ViaEntry] -> Via
fromEntries = maybe ViaEmpty Via . NE.nonEmpty


viaEntryParser :: ParserT st String ViaEntry
viaEntryParser = do
  proto <- receivedProtocolParser
  rws
  by <- receivedBy
  mc <- optional (rws *> comment)
  pure (ViaEntry proto by mc)


receivedProtocolParser :: ParserT st String ReceivedProtocol
receivedProtocolParser = do
  tok <- rfc9110Token
  branch
    $(char '/')
    (ReceivedProtocol (Just tok) <$> rfc9110Token)
    (pure (ReceivedProtocol Nothing tok))


renderVia :: Via -> M.Builder
renderVia ViaEmpty = mempty
renderVia (Via entries) = sepByCommas1 (fmap renderViaEntry entries)


renderViaEntry :: ViaEntry -> M.Builder
renderViaEntry (ViaEntry proto by mc) =
  renderReceivedProtocol proto
    <> M.char7 ' '
    <> shortText by
    <> maybe mempty (\c -> M.char7 ' ' <> renderComment (Comment c)) mc


renderReceivedProtocol :: ReceivedProtocol -> M.Builder
renderReceivedProtocol (ReceivedProtocol mname version) =
  maybe mempty (\n -> shortText n <> M.char7 '/') mname <> shortText version
