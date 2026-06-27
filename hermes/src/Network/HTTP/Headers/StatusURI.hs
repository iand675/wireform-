{-# LANGUAGE TemplateHaskell #-}

{- |
@Status-URI@ (RFC 2518 §9.7) — response header used with the
@102 (Processing)@ status code to report, to a client, the status of
one or more resources affected by a long-running method.

@
Status-URI = \"Status-URI\" \":\" *(Status-Code Coded-URL)
Coded-URL  = \"\<\" absoluteURI \"\>\"
@

@Status-Code@ is a 3-digit HTTP status code; @Coded-URL@ is an absolute
URI wrapped in angle brackets. Each pair couples a status code with the
resource it applies to.

Spec: <https://www.rfc-editor.org/rfc/rfc2518.html#section-9.7>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.LockToken", "Network.HTTP.Headers.Timeout".
-}
module Network.HTTP.Headers.StatusURI (
  StatusURI (..),
  StatusURIEntry (..),
  statusURIParser,
  renderStatusURI,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hStatusURI)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single @Status-Code Coded-URL@ pair.
data StatusURIEntry = StatusURIEntry
  { statusURICode :: !Int
  -- ^ The HTTP status code (e.g. @423@).
  , statusURIRef :: !ST.ShortText
  -- ^ The absolute URI, without its surrounding angle brackets.
  }
  deriving stock (Eq, Show)


-- | Zero or more status/URI pairs.
newtype StatusURI = StatusURI {statusURIEntries :: [StatusURIEntry]}
  deriving stock (Eq, Show)


instance KnownHeader StatusURI where
  type ParseFailure StatusURI = String
  type Cardinality StatusURI = 'ZeroOrOne
  type Direction StatusURI = 'Response


  parseFromHeaders _ headers = case runParser statusURIParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Status-URI header: " <> show rest
    Fail -> Left "Failed to parse Status-URI header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderStatusURI


  headerName _ = hStatusURI


statusURIParser :: ParserT st String StatusURI
statusURIParser = do
  ows
  entries <- statusURIEntry `sepBy` rws
  ows
  pure (StatusURI entries)
  where
    statusURIEntry = do
      code <- anyAsciiDecimalInt
      rws
      $(char '<')
      ref <- shortASCIIFromParser_ (skipSome (skipSatisfyAscii (/= '>')))
      $(char '>')
      pure (StatusURIEntry code ref)


renderStatusURI :: StatusURI -> M.Builder
renderStatusURI (StatusURI entries) = M.intersperse " " (map renderEntry entries)
  where
    renderEntry (StatusURIEntry code ref) =
      M.intDec code <> " <" <> shortText ref <> ">"
