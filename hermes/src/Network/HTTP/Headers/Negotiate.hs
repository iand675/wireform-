{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2295 §8.4 @Negotiate@ conveys the transparent content negotiation
directives and remote variant selection algorithms a client allows.

The nonempty list retains its wire spelling and order across field lines.
'negotiateDirectiveKind' recognizes the RFC keywords and versions. Other
syntactically valid directives are available as extensions.

Spec: <https://www.rfc-editor.org/rfc/rfc2295.html#section-8.4>
-}
module Network.HTTP.Headers.Negotiate (
  Negotiate (..),
  NegotiateDirective (..),
  NegotiateDirectiveKind (..),
  RvsaVersion,
  rvsaMajor,
  rvsaMinor,
  mkRvsaVersion,
  parseRvsaVersion,
  renderRvsaVersion,
  negotiateDirectiveKind,
  negotiateDirectiveFromKind,
  negotiateParser,
  renderNegotiate,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAsciiUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word16)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeCommaSeparatedTokenPairs, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hNegotiate)
import qualified Network.HTTP.Headers.Mason as M


data NegotiateDirective = NegotiateDirective
  { negName :: !ST.ShortText
  , negValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


newtype Negotiate = Negotiate {negotiateDirectives :: NonEmpty NegotiateDirective}
  deriving stock (Eq, Show)


data NegotiateDirectiveKind
  = NegotiateTrans
  | NegotiateVariantList
  | NegotiateGuessSmall
  | NegotiateAnyAlgorithm
  | NegotiateAlgorithmVersion !RvsaVersion
  | NegotiateExtension !ST.ShortText !(Maybe ST.ShortText)
  deriving stock (Eq, Show)


-- | RFC 2295 §7.1 allows one through four digits per component.
data RvsaVersion = RvsaVersion {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
  deriving stock (Eq, Ord, Show)


rvsaMajor :: RvsaVersion -> Word16
rvsaMajor (RvsaVersion major _) = major


rvsaMinor :: RvsaVersion -> Word16
rvsaMinor (RvsaVersion _ minor) = minor


mkRvsaVersion :: Word16 -> Word16 -> Maybe RvsaVersion
mkRvsaVersion major minor
  | major <= 9999 && minor <= 9999 = Just (RvsaVersion major minor)
  | otherwise = Nothing


parseRvsaVersion :: ByteString -> Maybe RvsaVersion
parseRvsaVersion bytes = do
  guard (BS.length bytes >= 3 && BS.length bytes <= 9)
  let (major, dotMinor) = BSC.break (== '.') bytes
  ('.', minor) <- BSC.uncons dotMinor
  RvsaVersion <$> decimal major <*> decimal minor
  where
    decimal component = do
      guard (BS.length component <= 4)
      (number, rest) <- BSC.readWord16 component
      guard (BS.null rest)
      pure number


renderRvsaVersion :: RvsaVersion -> M.Builder
renderRvsaVersion (RvsaVersion major minor) =
  M.word16Dec major <> M.char7 '.' <> M.word16Dec minor


-- | Recognize bare RFC directives. Extension arguments preserve their spelling.
negotiateDirectiveKind :: NegotiateDirective -> NegotiateDirectiveKind
negotiateDirectiveKind (NegotiateDirective name value) = case value of
  Just _ -> extension
  Nothing -> case folded of
    "trans" -> NegotiateTrans
    "vlist" -> NegotiateVariantList
    "guess-small" -> NegotiateGuessSmall
    "*" -> NegotiateAnyAlgorithm
    _ -> maybe extension NegotiateAlgorithmVersion (parseRvsaVersion raw)
  where
    extension = NegotiateExtension name value
    raw = ST.toByteString name
    folded
      | BSC.any isAsciiUpper raw = BSC.map toLower raw
      | otherwise = raw


negotiateDirectiveFromKind :: NegotiateDirectiveKind -> NegotiateDirective
negotiateDirectiveFromKind = \case
  NegotiateTrans -> bare "trans"
  NegotiateVariantList -> bare "vlist"
  NegotiateGuessSmall -> bare "guess-small"
  NegotiateAnyAlgorithm -> bare "*"
  NegotiateAlgorithmVersion version ->
    bare (STU.fromByteStringUnsafe (M.toStrictByteString (renderRvsaVersion version)))
  NegotiateExtension name value -> NegotiateDirective name value
  where
    bare name = NegotiateDirective name Nothing


instance KnownHeader Negotiate where
  type ParseFailure Negotiate = String
  type Cardinality Negotiate = 'ZeroOrOne
  type Direction Negotiate = 'Request


  parseFromHeaders _ fields =
    Negotiate <$> (parseListFieldLines "Negotiate" directiveList fields >>= requireNonEmptyList "Negotiate")


  renderToHeaders _ = M.toStrictByteString . renderNegotiate


  renderToHeadersChecked _ (Negotiate directives)
    | all valid directives = Just (encodeCommaSeparatedTokenPairs "=" negName negValue (NE.toList directives))
    | otherwise = Nothing
    where
      valid (NegotiateDirective name value) = isHttpTokenText name && maybe True isHttpTokenText value


  headerName _ = hNegotiate


negotiateParser :: ParserT st String Negotiate
negotiateParser = directiveList >>= either err (pure . Negotiate) . requireNonEmptyList "Negotiate"


directiveList :: ParserT st String [NegotiateDirective]
directiveList = httpList $ do
  name <- rfc9110Token
  ows
  value <- optional ($(char '=') *> ows *> rfc9110Token)
  pure (NegotiateDirective name value)


renderNegotiate :: Negotiate -> M.Builder
renderNegotiate (Negotiate (first :| rest)) =
  renderDirective first <> foldMap ((", " <>) . renderDirective) rest
  where
    renderDirective (NegotiateDirective name value) =
      shortText name <> maybe mempty (\v -> M.char7 '=' <> shortText v) value
{-# INLINE renderNegotiate #-}
