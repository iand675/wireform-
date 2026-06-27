{-# LANGUAGE TemplateHaskell #-}

{- |
@Expect-CT@ — an /obsolete/ response header by which a host requested that
clients evaluate its connections against its Certificate Transparency policy,
optionally enforcing the requirement and/or reporting failures. Expect-CT was
deprecated in favour of CT being enforced by default in major browsers, but the
wire grammar is well specified, so this module captures it structurally.

== Grammar

@
Expect-CT           = #expect-ct-directive
expect-ct-directive = directive-name [ "=" directive-value ]
directive-name      = token
directive-value     = token / quoted-string
@

Note that, unlike @Strict-Transport-Security@, @Expect-CT@ directives are
separated by commas (it is an RFC 9110 @#@-list). The defined directives are
@max-age=\<delta-seconds\>@ (REQUIRED), the valueless flag @enforce@, and the
optional @report-uri="\<uri\>"@. Directive names are case-insensitive; the
first occurrence of @max-age@ / @report-uri@ wins and unrecognised directives
are ignored.

Spec: <https://www.rfc-editor.org/rfc/rfc9163#section-2.1>

See also: "Network.HTTP.Headers.PublicKeyPins", "Network.HTTP.Headers.PublicKeyPinsReportOnly", "Network.HTTP.Headers.StrictTransportSecurity".
-}
module Network.HTTP.Headers.ExpectCT (
  ExpectCT (..),
  expectCTParser,
  renderExpectCT,
) where

import qualified Control.Applicative
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hExpectCT)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Parsed @Expect-CT@ policy.
data ExpectCT = ExpectCT
  { expectCtMaxAge :: !Word
  -- ^ @max-age@ delta-seconds: how long the CT policy is to be remembered.
  , expectCtEnforce :: !Bool
  -- ^ Whether the host requested enforcement (rather than report-only).
  , expectCtReportUri :: !(Maybe ST.ShortText)
  -- ^ Optional URI to which CT-compliance failures should be reported.
  }
  deriving stock (Eq, Show)


instance KnownHeader ExpectCT where
  type ParseFailure ExpectCT = String
  type Cardinality ExpectCT = 'ZeroOrOne
  type Direction ExpectCT = 'Response


  parseFromHeaders _ headers = case runParser expectCTParser (NE.head headers) of
    OK ect leftover
      | B.null (dropOws leftover) -> Right ect
      | otherwise ->
          Left ("Unconsumed input after parsing Expect-CT: " <> show leftover)
    Fail -> Left "Failed to parse Expect-CT header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderExpectCT


  headerName _ = hExpectCT


-- | One parsed directive (internal).
data Directive
  = DMaxAge !Word
  | DEnforce
  | DReportUri !ST.ShortText
  | DOther


expectCTParser :: ParserT st String ExpectCT
expectCTParser = do
  ows
  first <- directive
  rest <- many (ows *> $(char ',') *> ows *> directive)
  ows
  maybe failed pure (applyDirectives (first : rest))
  where
    directive = do
      name <- rfc9110Token
      case map toLower (ST.toString name) of
        "max-age" -> DMaxAge <$> (ows *> $(char '=') *> ows *> maxAgeValue)
        "enforce" -> pure DEnforce
        "report-uri" -> DReportUri <$> (ows *> $(char '=') *> ows *> quotedString)
        _ -> DOther <$ optional (ows *> $(char '=') *> ows *> directiveValue)
    maxAgeValue =
      ($(char '"') *> anyAsciiDecimalWord <* $(char '"'))
        <|> anyAsciiDecimalWord
    directiveValue = void quotedString <|> void rfc9110Token


{- | Fold parsed directives into the structured record. @max-age@ is required
(first occurrence wins); the first @report-uri@ wins.
-}
applyDirectives :: [Directive] -> Maybe ExpectCT
applyDirectives = go Nothing False Nothing
  where
    go (Just ma) enf rpt [] = Just (ExpectCT ma enf rpt)
    go Nothing _ _ [] = Nothing
    go ma enf rpt (d : ds) = case d of
      DMaxAge n -> go (ma Control.Applicative.<|> Just n) enf rpt ds
      DEnforce -> go ma True rpt ds
      DReportUri u -> go ma enf (rpt Control.Applicative.<|> Just u) ds
      DOther -> go ma enf rpt ds


renderExpectCT :: ExpectCT -> M.Builder
renderExpectCT (ExpectCT ma enf rpt) =
  M.intersperse ", " parts
  where
    parts =
      ["max-age=" <> M.wordDec ma]
        ++ ["enforce" | enf]
        ++ maybe [] (\u -> ["report-uri=" <> quoted u]) rpt


{- | Render a value as an HTTP @quoted-string@ (no escaping; report URIs do not
contain @\"@ or @\\@).
-}
quoted :: ST.ShortText -> M.Builder
quoted t = M.char7 '"' <> shortText t <> M.char7 '"'
