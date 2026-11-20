{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

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

The codec retains token and quoted spelling. Quoted values use a
one-to-one Latin-1 mapping for wire octets.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1>

See also: "Network.HTTP.Headers.ContentLength", "Network.HTTP.Headers.TE", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.Expect (
  Expect (..),
  Expectation (..),
  expectationList,
  hasContinueExpectation,
  expectParser,
  renderExpect,
) where

import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hExpect)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderHttpWordValue, renderSeparated, shortText)


-- | An @Expect@ header value, including the explicit empty list.
data Expect
  = Expect {expectExpectations :: NE.NonEmpty Expectation}
  | ExpectEmpty
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


  parseFromHeaders _ (wire NE.:| [])
    | wire == continueWire = Right continueExpectation
  parseFromHeaders _ fields = parseExpectFields fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ (Expect (Expectation "100-continue" Nothing [] NE.:| [])) = "100-continue"
  renderToHeaders _ value = M.toStrictByteString (renderExpect value)


  headerName _ = hExpect


expectParser :: ParserT st String Expect
expectParser = fromExpectations <$> httpList expectationParser


parseExpectFields :: NE.NonEmpty ByteString -> Either String Expect
parseExpectFields fields = parseFieldLine "Expect" expectParser (combineFieldLines ", " fields)
{-# NOINLINE parseExpectFields #-}


continueWire :: ByteString
continueWire = "100-continue"


continueExpectation :: Expect
continueExpectation = Expect (Expectation "100-continue" Nothing [] NE.:| [])


fromExpectations :: [Expectation] -> Expect
fromExpectations = maybe ExpectEmpty Expect . NE.nonEmpty


-- | All expectations in field order, including an empty field.
expectationList :: Expect -> [Expectation]
expectationList ExpectEmpty = []
expectationList (Expect entries) = NE.toList entries


-- | Test the combined field for the bare, case-insensitive
-- @100-continue@ expectation.
hasContinueExpectation :: [ByteString] -> Bool
hasContinueExpectation [wire]
  | wire == continueWire = True
hasContinueExpectation fields = case parseExpectFields <$> NE.nonEmpty fields of
  Just (Right (Expect entries)) -> any continues entries
  _ -> False
  where
    continues (Expectation name Nothing []) =
      name == "100-continue" || CI.foldCase (ST.toByteString name) == "100-continue"
    continues _ = False


expectationParser :: ParserT st String Expectation
expectationParser = do
  name <- rfc9110Token
  branch $(char '=') (Expectation name . Just <$> httpWordValue <*> parameters) (pure (Expectation name Nothing []))
  where
    parameters =
      withOption (skipSome (ows *> $(char ';') *> ows)) (const nextParameter) (pure [])
    nextParameter = withOption parameter (\first -> (first :) <$> parameters) (pure [])
    parameter = (,) <$> (rfc9110Token <* $(char '=')) <*> (Just <$> httpWordValue)


renderExpect :: Expect -> M.Builder
renderExpect ExpectEmpty = mempty
renderExpect (Expect es) = renderSeparated ", " renderExpectation (NE.toList es)


renderExpectation :: Expectation -> M.Builder
renderExpectation (Expectation name Nothing _) = shortText name
renderExpectation (Expectation name (Just value) params) =
  shortText name <> M.char7 '=' <> renderHttpWordValue value <> foldMap renderParameter params


renderParameter :: (ST.ShortText, Maybe ST.ShortText) -> M.Builder
renderParameter (name, Just value) = M.char7 ';' <> shortText name <> M.char7 '=' <> renderHttpWordValue value
renderParameter (_, Nothing) = mempty
