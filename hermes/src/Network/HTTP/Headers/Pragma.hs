{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9111 §5.4 @Pragma@ — a deprecated, HTTP\/1.0-era request header
carrying implementation-specific caching directives.

== Grammar

@
Pragma           = #pragma-directive
pragma-directive = "no-cache" / extension-pragma
extension-pragma = token [ "=" ( token / quoted-string ) ]
@

@no-cache@ is just the well-known extension-pragma with no value, so
every directive is modelled uniformly as a token name with an optional
token-or-quoted-string value. The original token\/quoted-string form of
a value is preserved so the value round-trips exactly.

The header is obsolete (RFC 9111 deprecates it in favour of
@Cache-Control@), but is parsed\/rendered faithfully for compatibility.

See <https://www.rfc-editor.org/rfc/rfc9111.html#section-5.4>.

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Expires", "Network.HTTP.Headers.Warning".
-}
module Network.HTTP.Headers.Pragma (
  Pragma (..),
  PragmaDirective (..),
  PragmaValue (..),
  pragmaParser,
  renderPragma,
) where

import Control.Monad.Combinators (option)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import Data.Foldable1 (fold1)
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPragma)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A non-empty comma-separated list of pragma-directives.
newtype Pragma = Pragma {pragmaDirectives :: NonEmpty PragmaDirective}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


-- | A single @pragma-directive@: a token name with an optional value.
data PragmaDirective = PragmaDirective
  { pragmaName :: !ShortText
  , pragmaValue :: !(Maybe PragmaValue)
  }
  deriving stock (Eq, Show)


{- | The value of an @extension-pragma@, preserving whether it was
written as a bare token or as a quoted-string.
-}
data PragmaValue
  = PragmaToken !ShortText
  | PragmaQuoted !RFC8941String
  deriving stock (Eq, Show)


instance KnownHeader Pragma where
  type ParseFailure Pragma = String
  type Cardinality Pragma = 'ZeroOrMore
  type Direction Pragma = 'RequestAndResponse


  parseFromHeaders _ headers = fold1 <$> traverse runPragmaParser headers
  renderToHeaders _ = pure . M.toStrictByteString . renderPragma
  headerName _ = hPragma


runPragmaParser :: ByteString -> Either String Pragma
runPragmaParser bs = case runParser pragmaParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Pragma header: " <> show rest
  Fail -> Left "Failed to parse Pragma header"
  Err e -> Left e


pragmaParser :: ParserT st String Pragma
pragmaParser =
  Pragma <$> (ows *> (pragmaDirectiveParser `sepBy1` (ows *> $(char ',') *> ows)) <* ows)


pragmaDirectiveParser :: ParserT st e PragmaDirective
pragmaDirectiveParser = do
  name <- rfc9110Token
  value <- option Nothing (Just <$> ($(char '=') *> pragmaValueParser))
  pure PragmaDirective {pragmaName = name, pragmaValue = value}


pragmaValueParser :: ParserT st e PragmaValue
pragmaValueParser =
  (PragmaQuoted <$> rfc8941String) <|> (PragmaToken <$> rfc9110Token)


renderPragma :: Pragma -> M.Builder
renderPragma = R.sepByCommas1 . fmap renderPragmaDirective . pragmaDirectives


renderPragmaDirective :: PragmaDirective -> M.Builder
renderPragmaDirective (PragmaDirective name value) =
  R.shortText name <> maybe mempty (\v -> M.char7 '=' <> renderPragmaValue v) value


renderPragmaValue :: PragmaValue -> M.Builder
renderPragmaValue = \case
  PragmaToken t -> R.shortText t
  PragmaQuoted s -> R.rfc8941String s
