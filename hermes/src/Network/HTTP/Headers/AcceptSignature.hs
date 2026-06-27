{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9421 §5.1 @Accept-Signature@ — HTTP Message Signatures negotiation.

The @Accept-Signature@ field lets a sender request that the peer produce a
signature, naming a desired label and the components and signature
parameters that should cover the message. Its value mirrors the
@Signature-Input@ grammar exactly: an RFC 8941 Structured Field Dictionary
whose keys are signature labels and whose member values are Inner Lists of
component identifiers (Structured Field Strings with their own parameters)
carrying the requested signature parameters (@keyid@, @alg@, @created@,
@nonce@, @tag@, ...) as parameters on the Inner List.

@
Accept-Signature = sf-dictionary
                 ; label = inner-list( component-id );param=value ...
component-id     = sf-string *parameter
@

It is a request-and-response field: a client may advertise the signatures it
wants on a response, and a server may request signatures on subsequent
requests.

Spec: <https://www.rfc-editor.org/rfc/rfc9421.html#name-the-accept-signature-field>

See also: "Network.HTTP.Headers.Signature", "Network.HTTP.Headers.SignatureInput", "Network.HTTP.Headers.ContentDigest".
-}
module Network.HTTP.Headers.AcceptSignature (
  AcceptSignature (..),
  AcceptSignatureEntry (..),
  AcceptSignatureComponent (..),
  acceptSignatureParser,
  renderAcceptSignature,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptSignature)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A requested covered component identifier: an RFC 8941 String together
with any parameters that modify it (@;sf@, @;key=\"..\"@, @;req@, ...).
-}
data AcceptSignatureComponent = AcceptSignatureComponent
  { componentName :: !RFC8941String
  , componentParams :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


-- | One dictionary member: a requested labelled signature and its parameters.
data AcceptSignatureEntry = AcceptSignatureEntry
  { signatureLabel :: !ShortText
  , signatureComponents :: ![AcceptSignatureComponent]
  , signatureParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | The @Accept-Signature@ dictionary: an ordered, non-empty list of
requested labelled entries.
-}
newtype AcceptSignature = AcceptSignature
  { acceptSignatureEntries :: NonEmpty AcceptSignatureEntry
  }
  deriving stock (Eq, Show)


instance KnownHeader AcceptSignature where
  type ParseFailure AcceptSignature = String
  type Cardinality AcceptSignature = 'ZeroOrMore
  type Direction AcceptSignature = 'RequestAndResponse


  parseFromHeaders _ headers =
    case runParser acceptSignatureParser (B.intercalate ", " (NE.toList headers)) of
      OK v rest
        | B.null (B.dropWhile isOwsByte rest) -> Right v
        | otherwise -> Left ("Unconsumed input after parsing Accept-Signature header: " <> show rest)
      Fail -> Left "Failed to parse Accept-Signature header"
      Err e -> Left e
    where
      isOwsByte w = w == 0x20 || w == 0x09


  renderToHeaders _ = pure . M.toStrictByteString . renderAcceptSignature


  headerName _ = hAcceptSignature


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

acceptSignatureParser :: ParserT st String AcceptSignature
acceptSignatureParser = do
  ows
  AcceptSignature <$> rfc8941List1 entry
  where
    entry = do
      label <- dictionaryKey
      $(char '=')
      comps <- innerList
      AcceptSignatureEntry label comps <$> rfc8941Parameters

    innerList = do
      $(char '(')
      ows
      cs <- component `sepBy` rws
      ows
      $(char ')')
      pure cs

    component = do
      name <- rfc8941String
      AcceptSignatureComponent name <$> rfc8941Parameters


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

renderAcceptSignature :: AcceptSignature -> M.Builder
renderAcceptSignature (AcceptSignature entries) =
  M.intersperse ", " (renderEntry <$> entries)


renderEntry :: AcceptSignatureEntry -> M.Builder
renderEntry (AcceptSignatureEntry label comps params) =
  R.shortText label
    <> M.char7 '='
    <> renderInnerList comps
    <> renderParameters params


renderInnerList :: [AcceptSignatureComponent] -> M.Builder
renderInnerList comps =
  M.char7 '(' <> M.intersperse " " (renderComponent <$> comps) <> M.char7 ')'


renderComponent :: AcceptSignatureComponent -> M.Builder
renderComponent (AcceptSignatureComponent name params) =
  R.rfc8941String name <> renderParameters params


renderParameters :: [(ShortText, Maybe ItemValue)] -> M.Builder
renderParameters =
  foldMap (uncurry (R.rfc8941Parameter R.IncludeIfEmpty R.rfc8941ItemValue))
