{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9421 §4.1 @Signature-Input@ — HTTP Message Signatures input.

The @Signature-Input@ field describes how each signature in the
companion @Signature@ field was produced, and may appear on both
requests and responses. It is an RFC 8941 Structured Field
Dictionary whose keys are signature labels and whose member values are
Inner Lists of /component identifiers/ (Structured Field Strings, each with
their own parameters such as @;sf@, @;key=\"..\"@, @;req@, @;bs@) carrying
the signature parameters (@created@, @keyid@, @alg@, @nonce@, @expires@,
@tag@, ...) as parameters on the Inner List itself.

@
Signature-Input = sf-dictionary
                ; label = inner-list( component-id );param=value ...
component-id    = sf-string *parameter
@

The component identifiers and the signature parameters are kept structurally
but faithfully: identifiers as their underlying Structured Field Strings and
parameters as the generic RFC 8941 @(key, Maybe value)@ pairs, so the full
set of (and any future) parameters round-trips without fabricating an
exhaustive, dead enumeration.

Spec: <https://www.rfc-editor.org/rfc/rfc9421.html#name-the-signature-input-http-fi>

See also: "Network.HTTP.Headers.Signature", "Network.HTTP.Headers.AcceptSignature", "Network.HTTP.Headers.ContentDigest".
-}
module Network.HTTP.Headers.SignatureInput (
  SignatureInput (..),
  SignatureInputEntry (..),
  SignatureComponent (..),
  signatureInputParser,
  renderSignatureInput,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSignatureInput)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A single covered component identifier: an RFC 8941 String (e.g.
@\"\@method\"@, @\"content-digest\"@) together with any parameters that
modify it (@;sf@, @;key=\"..\"@, @;req@, @;bs@, @;tr@, ...).
-}
data SignatureComponent = SignatureComponent
  { componentName :: !RFC8941String
  , componentParams :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


-- | One dictionary member: a labelled signature and its derivation.
data SignatureInputEntry = SignatureInputEntry
  { signatureLabel :: !ShortText
  , signatureComponents :: ![SignatureComponent]
  , signatureParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | The @Signature-Input@ dictionary: an ordered, non-empty list of
labelled entries.
-}
newtype SignatureInput = SignatureInput
  { signatureInputEntries :: NonEmpty SignatureInputEntry
  }
  deriving stock (Eq, Show)


instance KnownHeader SignatureInput where
  type ParseFailure SignatureInput = String
  type Cardinality SignatureInput = 'ZeroOrMore
  type Direction SignatureInput = 'RequestAndResponse


  parseFromHeaders _ headers =
    case runParser signatureInputParser (B.intercalate ", " (NE.toList headers)) of
      OK v rest
        | B.null (B.dropWhile isOwsByte rest) -> Right v
        | otherwise -> Left ("Unconsumed input after parsing Signature-Input header: " <> show rest)
      Fail -> Left "Failed to parse Signature-Input header"
      Err e -> Left e
    where
      isOwsByte w = w == 0x20 || w == 0x09


  renderToHeaders _ = pure . M.toStrictByteString . renderSignatureInput


  headerName _ = hSignatureInput


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

signatureInputParser :: ParserT st String SignatureInput
signatureInputParser = do
  ows
  SignatureInput <$> rfc8941List1 entry
  where
    entry = do
      label <- dictionaryKey
      $(char '=')
      comps <- innerList
      SignatureInputEntry label comps <$> rfc8941Parameters

    innerList = do
      $(char '(')
      ows
      cs <- component `sepBy` rws
      ows
      $(char ')')
      pure cs

    component = do
      name <- rfc8941String
      SignatureComponent name <$> rfc8941Parameters


{- | An RFC 8941 dictionary @key@:
@( lcalpha \/ "*" ) *( lcalpha \/ DIGIT \/ "_" \/ "-" \/ "." \/ "*" )@.
-}
dictionaryKey :: ParserT st e ShortText
dictionaryKey =
  shortASCIIFromParser_ $
    skipSatisfyAscii (`CharSet.member` keyFirst)
      *> skipMany (skipSatisfyAscii (`CharSet.member` keyRest))
  where
    keyFirst = CharSet.range 'a' 'z' <> CharSet.singleton '*'
    keyRest = CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "_-.*"


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderSignatureInput :: SignatureInput -> M.Builder
renderSignatureInput (SignatureInput entries) =
  M.intersperse ", " (renderEntry <$> entries)


renderEntry :: SignatureInputEntry -> M.Builder
renderEntry (SignatureInputEntry label comps params) =
  R.shortText label
    <> M.char7 '='
    <> renderInnerList comps
    <> renderParameters params


renderInnerList :: [SignatureComponent] -> M.Builder
renderInnerList comps =
  M.char7 '(' <> M.intersperse " " (renderComponent <$> comps) <> M.char7 ')'


renderComponent :: SignatureComponent -> M.Builder
renderComponent (SignatureComponent name params) =
  R.rfc8941String name <> renderParameters params


renderParameters :: [(ShortText, Maybe ItemValue)] -> M.Builder
renderParameters =
  foldMap (uncurry (R.rfc8941Parameter R.IncludeIfEmpty R.rfc8941ItemValue))
