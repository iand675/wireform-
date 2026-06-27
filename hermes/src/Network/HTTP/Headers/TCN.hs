{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2295 §8.5 @TCN@ — response header by which a server signals that
the resource is transparently negotiated and which kind of negotiation
response this is.

== Grammar

@
TCN = \"TCN\" \":\" #( response-type
                 | server-side-override-directive
                 | tcn-extension )
response-type                   = \"list\" | \"choice\" | \"adhoc\"
server-side-override-directive  = \"re-choose\" | \"keep\"
tcn-extension                   = token [ \"=\" ( token | quoted-string ) ]
@

Every directive (the response-type and override keywords included) has
the shape @token [ \"=\" value ]@, so the value is surfaced as a flat
list of name\\/optional-value directives. A value is rendered as a bare
token when it is token-safe and as a quoted-string otherwise.

Spec: <https://www.rfc-editor.org/rfc/rfc2295#section-8.5>

See also: "Network.HTTP.Headers.AcceptFeatures", "Network.HTTP.Headers.Alternates", "Network.HTTP.Headers.Negotiate", "Network.HTTP.Headers.VariantVary", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.TCN (
  TCN (..),
  TCNDirective (..),
  tcnParser,
  renderTCN,
) where

import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTCN)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A single TCN directive: a token, optionally with a value.
data TCNDirective = TCNDirective
  { tcnName :: !ST.ShortText
  , tcnValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | The comma-separated list of directives carried by a @TCN@ header.
newtype TCN = TCN {tcnDirectives :: [TCNDirective]}
  deriving stock (Eq, Show)


instance KnownHeader TCN where
  type ParseFailure TCN = String
  type Cardinality TCN = 'ZeroOrOne
  type Direction TCN = 'Response


  parseFromHeaders _ headers = case runParser tcnParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing TCN: " <> show leftover)
    Fail -> Left "Failed to parse TCN header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderTCN


  headerName _ = hTCN


tcnParser :: ParserT st String TCN
tcnParser = TCN <$> (ows *> directives)
  where
    directives = nonEmpty <|> pure []
    nonEmpty = do
      d <- directive
      ds <- many (ows *> $(char ',') *> ows *> directive)
      pure (d : ds)
    directive = do
      name <- rfc9110Token
      val <- optional ($(char '=') *> (rfc9110Token <|> quotedString))
      pure (TCNDirective name val)


renderTCN :: TCN -> M.Builder
renderTCN (TCN ds) = M.intersperse ", " (map renderDirective ds)
  where
    renderDirective (TCNDirective name Nothing) = R.shortText name
    renderDirective (TCNDirective name (Just v)) =
      R.shortText name <> M.char7 '=' <> renderTokenOrQuoted v


{- | Render a value as a bare token when it is token-safe, otherwise as a
quoted-string with the mandatory escaping of @\"@ and @\\@.
-}
renderTokenOrQuoted :: ST.ShortText -> M.Builder
renderTokenOrQuoted t
  | isToken s = R.shortText t
  | otherwise = M.char8 '"' <> foldr esc mempty s <> M.char8 '"'
  where
    s = ST.toString t
    isToken cs = not (null cs) && all (`CharSet.member` tokenCharSet) cs
    esc c acc
      | c == '"' || c == '\\' = M.char8 '\\' <> M.char8 c <> acc
      | otherwise = M.char8 c <> acc
