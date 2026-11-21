{-# LANGUAGE MagicHash #-}
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
import qualified Data.ByteString.Internal as BSI
import qualified Data.CaseInsensitive as CI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (indexWord8OffAddrAsWord32#, indexWord8OffAddrAsWord64#)
import GHC.ForeignPtr (ForeignPtr (..), unsafeWithForeignPtr)
import GHC.Word (Word32 (..), Word64 (..))
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
    | isCanonicalContinue wire = Right continueExpectation
  parseFromHeaders _ fields = parseExpectFields fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ (Expect (Expectation "100-continue" Nothing [] NE.:| [])) = "100-continue"
  renderToHeaders _ value = M.toStrictByteString (renderExpect value)


  renderToHeadersChecked _ ExpectEmpty = Just ""
  renderToHeadersChecked _ (Expect (Expectation "100-continue" Nothing [] NE.:| [])) = Just "100-continue"
  renderToHeadersChecked _ value@(Expect entries)
    | all validExpectation entries = Just (M.toStrictByteString (renderExpect value))
    | otherwise = Nothing


  headerName _ = hExpect


validExpectation :: Expectation -> Bool
validExpectation (Expectation name value params) =
  isHttpTokenText name && case value of
    Nothing -> null params
    Just word -> isHttpWordValue word && all validParameter params
  where
    validParameter (key, Just word) = isHttpTokenText key && isHttpWordValue word
    validParameter (_, Nothing) = False
{-# INLINE validExpectation #-}


expectParser :: ParserT st String Expect
expectParser = fromExpectations <$> httpList expectationParser


parseExpectFields :: NE.NonEmpty ByteString -> Either String Expect
parseExpectFields fields = parseFieldLine "Expect" expectParser (combineFieldLines ", " fields)
{-# NOINLINE parseExpectFields #-}


-- Compare "100-cont" and "inue" in place. ByteString slices can begin at
-- any alignment, and the size check bounds both loads.
isCanonicalContinue :: ByteString -> Bool
isCanonicalContinue (BSI.BS input@(ForeignPtr start _) 12) =
  BSI.accursedUnutterablePerformIO $ unsafeWithForeignPtr input $ \_ ->
    pure $!
      ( W64# (indexWord8OffAddrAsWord64# start 0#) == prefix
          && W32# (indexWord8OffAddrAsWord32# start 8#) == suffix
      )
  where
    prefix = case targetByteOrder of
      BigEndian -> 0x3130302d636f6e74
      LittleEndian -> 0x746e6f632d303031
    suffix = case targetByteOrder of
      BigEndian -> 0x696e7565
      LittleEndian -> 0x65756e69
isCanonicalContinue _ = False
{-# INLINE isCanonicalContinue #-}


continueExpectation :: Expect
continueExpectation = Expect (Expectation "100-continue" Nothing [] NE.:| [])


fromExpectations :: [Expectation] -> Expect
fromExpectations = maybe ExpectEmpty Expect . NE.nonEmpty


-- | All expectations in field order, including an empty field.
expectationList :: Expect -> [Expectation]
expectationList ExpectEmpty = []
expectationList (Expect entries) = NE.toList entries


{- | Test the combined field for the bare, case-insensitive
@100-continue@ expectation.
-}
hasContinueExpectation :: [ByteString] -> Bool
hasContinueExpectation [wire]
  | isCanonicalContinue wire = True
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
