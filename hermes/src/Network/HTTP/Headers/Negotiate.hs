{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2295 §8.4 @Negotiate@ — request header carrying directives for the
content-negotiation process initiated by the request.

== Grammar

@
Negotiate           = \"Negotiate\" \":\" 1#negotiate-directive
negotiate-directive = \"trans\" | \"vlist\" | \"guess-small\"
                    | rvsa-version | \"*\" | negotiate-extension
rvsa-version        = major \".\" minor
negotiate-extension = token [ \"=\" token ]
@

Each directive — the @trans@\\/@vlist@\\/@guess-small@ keywords, the
@\"*\"@ wildcard, an @rvsa-version@ such as @1.0@, and any extension —
matches @token [ \"=\" token ]@ (a @.@ and @*@ are valid token
characters), so directives are surfaced as a non-empty list of
name\\/optional-token-value pairs.

Spec: <https://www.rfc-editor.org/rfc/rfc2295#section-8.4>

See also: "Network.HTTP.Headers.AcceptFeatures", "Network.HTTP.Headers.Alternates", "Network.HTTP.Headers.TCN", "Network.HTTP.Headers.VariantVary".
-}
module Network.HTTP.Headers.Negotiate (
  Negotiate (..),
  NegotiateDirective (..),
  negotiateParser,
  renderNegotiate,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hNegotiate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A single negotiate directive: a token, optionally with a token value.
data NegotiateDirective = NegotiateDirective
  { negName :: !ST.ShortText
  , negValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | The non-empty list of directives carried by a @Negotiate@ header.
newtype Negotiate = Negotiate {negotiateDirectives :: NonEmpty NegotiateDirective}
  deriving stock (Eq, Show)


instance KnownHeader Negotiate where
  type ParseFailure Negotiate = String
  type Cardinality Negotiate = 'ZeroOrOne
  type Direction Negotiate = 'Request


  parseFromHeaders _ headers = case runParser negotiateParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Negotiate: " <> show leftover)
    Fail -> Left "Failed to parse Negotiate header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderNegotiate


  headerName _ = hNegotiate


negotiateParser :: ParserT st String Negotiate
negotiateParser = Negotiate <$> (ows *> directive `sepBy1` (ows *> $(char ',') *> ows))
  where
    directive = do
      name <- rfc9110Token
      val <- optional ($(char '=') *> rfc9110Token)
      pure (NegotiateDirective name val)


renderNegotiate :: Negotiate -> M.Builder
renderNegotiate (Negotiate ds) = M.intersperse ", " (map renderDirective (NE.toList ds))
  where
    renderDirective (NegotiateDirective name Nothing) = R.shortText name
    renderDirective (NegotiateDirective name (Just v)) =
      R.shortText name <> M.char7 '=' <> R.shortText v
