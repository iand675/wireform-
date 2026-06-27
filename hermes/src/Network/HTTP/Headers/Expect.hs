{-# LANGUAGE TemplateHaskell #-}

{- |
The @Expect@ request header field tells the server about expectations the
client has about how the request will be handled. The only expectation
registered by the core specification is @100-continue@ (sent before a request
body so the server can reject it early with @417 (Expectation Failed)@), but
the grammar is general: a comma-separated list of expectations, each an
extension token with an optional @=value@ and optional parameters.

@
  Expect      = #expectation
  expectation = token [ "=" ( token / quoted-string ) parameters ]
  parameters  = *( OWS ";" OWS [ parameter ] )
  parameter   = parameter-name "=" parameter-value
@

This module models that list faithfully, preserving each expectation's raw
token, optional value (verbatim, including quoting), and parameters, rather
than fabricating semantics for the (essentially unused) extension space.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1>

See also: "Network.HTTP.Headers.ContentLength", "Network.HTTP.Headers.TE", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.Expect (
  Expect (..),
  Expectation (..),
  expectParser,
  renderExpect,
) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hExpect)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | An @Expect@ header value: a non-empty list of expectations.
newtype Expect = Expect {expectExpectations :: NE.NonEmpty Expectation}
  deriving stock (Eq, Show)


{- | A single expectation: an extension token (e.g. @100-continue@), an
optional @=value@ captured verbatim (token or quoted-string), and an optional
list of parameters.
-}
data Expectation = Expectation
  { expectationName :: ST.ShortText
  , expectationValue :: Maybe ST.ShortText
  , expectationParameters :: [(ST.ShortText, Maybe ST.ShortText)]
  }
  deriving stock (Eq, Show)


instance KnownHeader Expect where
  type ParseFailure Expect = String
  type Cardinality Expect = 'ZeroOrOne
  type Direction Expect = 'Request


  parseFromHeaders _ headers = case runParser expectParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Expect header: " <> show rest
    Fail -> Left "Failed to parse Expect header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderExpect


  headerName _ = hExpect


expectParser :: ParserT st String Expect
expectParser = do
  first <- expectationParser
  rest <- many (ows *> $(char ',') *> ows *> expectationParser)
  pure $ Expect (first NE.:| rest)


expectationParser :: ParserT st String Expectation
expectationParser = do
  name <- rfc9110Token
  val <- optional ($(char '=') *> shortASCIIFromParser_ (quotedString <|> rfc9110Token))
  params <- catMaybes <$> many parameterParser
  pure $ Expectation name val params


-- | One iteration of @OWS ";" OWS [ parameter ]@; the parameter itself is optional.
parameterParser :: ParserT st String (Maybe (ST.ShortText, Maybe ST.ShortText))
parameterParser = ows *> $(char ';') *> ows *> optional parameterKV
  where
    parameterKV = do
      pn <- rfc9110Token
      pv <- optional ($(char '=') *> shortASCIIFromParser_ (quotedString <|> rfc9110Token))
      pure (pn, pv)


renderExpect :: Expect -> M.Builder
renderExpect (Expect es) = M.intersperse ", " $ map renderExpectation $ NE.toList es


renderExpectation :: Expectation -> M.Builder
renderExpectation (Expectation name val params) =
  shortText name
    <> maybe mempty (\v -> M.char7 '=' <> shortText v) val
    <> foldMap renderParameter params


renderParameter :: (ST.ShortText, Maybe ST.ShortText) -> M.Builder
renderParameter (pn, pv) =
  M.char7 ';' <> shortText pn <> maybe mempty (\v -> M.char7 '=' <> shortText v) pv
