{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | @P3P@ advertises a privacy-policy reference, a compact cookie policy,
or both in an HTTP response. The codec preserves the order and wire bytes
of received directives.

Spec: <https://www.w3.org/TR/P3P/>

See also: "Network.HTTP.Headers.GetProfile", "Network.HTTP.Headers.SetProfile", "Network.HTTP.Headers.DNT".
-}
module Network.HTTP.Headers.P3P (
  P3P (P3P, p3pValue),
  P3PDirective (..),
  P3PExtensionValue (..),
  P3PCompactToken (..),
  P3PClaim (..),
  P3PAccess (..),
  P3PRemedy (..),
  P3PPurpose (..),
  P3PRecipient (..),
  P3PRetention (..),
  P3PCategory (..),
  P3PRequirement (..),
  p3pDirectives,
  p3pFromDirectives,
  p3pEffectiveCompactPolicy,
  p3pParser,
  renderP3P,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes)
import Network.HTTP.Grammar.URIReference (uriReferenceBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hP3P)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype P3P = P3PDirectives (R.RetainedList P3PDirective)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'p3pFromDirectives' for checked authoring.
pattern P3P :: ST.ShortText -> P3P
pattern P3P {p3pValue} <- (fieldText -> p3pValue)
  where
    P3P text = P3PDirectives (R.RawList text)


{-# COMPLETE P3P #-}


instance Show P3P where
  showsPrec precedence (P3P text) =
    showParen (precedence > 10) $ showString "P3P {p3pValue = " . shows text . showChar '}'


fieldText :: P3P -> ST.ShortText
fieldText (P3PDirectives entries) = R.legacyText entries


data P3PDirective
  = P3PPolicyRef !ByteString
  | P3PCompactPolicy !(NonEmpty P3PCompactToken)
  | P3PExtension !ByteString !(Maybe P3PExtensionValue)
  deriving stock (Eq, Show)


data P3PExtensionValue = P3PExtensionToken !ByteString | P3PExtensionQuoted !ByteString
  deriving stock (Eq, Show)


data P3PCompactToken
  = P3PKnownCompact !P3PClaim !(Maybe P3PRequirement)
  | P3PUnknownCompact !ByteString
  deriving stock (Eq, Ord, Show)


data P3PClaim
  = P3PAccess !P3PAccess
  | P3PDisputes
  | P3PRemedy !P3PRemedy
  | P3PNonIdentifiable
  | P3PPurpose !P3PPurpose
  | P3PRecipient !P3PRecipient
  | P3PRetention !P3PRetention
  | P3PCategory !P3PCategory
  | P3PTest
  deriving stock (Eq, Ord, Show)


data P3PAccess
  = P3PNonIdent
  | P3PAll
  | P3PContactAndOther
  | P3PIdentContact
  | P3POtherIdent
  | P3PNone
  deriving stock (Eq, Ord, Show)


data P3PRemedy = P3PCorrect | P3PMoney | P3PLaw
  deriving stock (Eq, Ord, Show)


data P3PPurpose
  = P3PCurrent
  | P3PAdmin
  | P3PDevelop
  | P3PTailoring
  | P3PPseudoAnalysis
  | P3PPseudoDecision
  | P3PIndividualAnalysis
  | P3PIndividualDecision
  | P3PContact
  | P3PHistorical
  | P3PTelemarketing
  | P3POtherPurpose
  deriving stock (Eq, Ord, Show)


data P3PRecipient
  = P3POurs
  | P3PDelivery
  | P3PSame
  | P3PUnrelated
  | P3PPublic
  | P3POtherRecipient
  deriving stock (Eq, Ord, Show)


data P3PRetention
  = P3PNoRetention
  | P3PStatedPurpose
  | P3PLegalRequirement
  | P3PBusinessPractices
  | P3PIndefinitely
  deriving stock (Eq, Ord, Show)


data P3PCategory
  = P3PPhysical
  | P3POnline
  | P3PUniqueID
  | P3PPurchase
  | P3PFinancial
  | P3PComputer
  | P3PNavigation
  | P3PInteractive
  | P3PDemographic
  | P3PContent
  | P3PState
  | P3PPolitical
  | P3PHealth
  | P3PPreference
  | P3PLocation
  | P3PGovernment
  | P3POtherCategory
  deriving stock (Eq, Ord, Show)


data P3PRequirement = P3PAlways | P3POptIn | P3POptOut
  deriving stock (Eq, Ord, Show)


p3pDirectives :: P3P -> Either String (NonEmpty P3PDirective)
p3pDirectives (P3PDirectives entries) = R.listEntries "P3P" directiveList entries


-- | Build a field line from typed directives and validate their wire forms.
p3pFromDirectives :: NonEmpty P3PDirective -> Either String P3P
p3pFromDirectives = fmap P3PDirectives . R.listFromEntries "P3P" directiveList renderDirective


{- | Take the first compact policy in field order, retaining the first
occurrence of each recognized token. An unknown-only policy is @Just []@.
-}
p3pEffectiveCompactPolicy :: P3P -> Either String (Maybe [P3PCompactToken])
p3pEffectiveCompactPolicy = fmap (first . NE.toList) . p3pDirectives
  where
    first [] = Nothing
    first (P3PCompactPolicy tokens : _) = Just (claims Set.empty (NE.toList tokens))
    first (_ : rest) = first rest
    claims _ [] = []
    claims seen (P3PUnknownCompact _ : rest) = claims seen rest
    claims seen (token : rest)
      | Set.member token seen = claims seen rest
      | otherwise = token : claims (Set.insert token seen) rest


instance KnownHeader P3P where
  type ParseFailure P3P = String
  type Cardinality P3P = 'ZeroOrMore
  type Direction P3P = 'Response


  parseFromHeaders _ fields = P3PDirectives <$> R.decodeList "P3P" directiveList fields


  renderToHeaders _ (P3PDirectives entries) = NE.toList (R.fieldLines entries)


  renderToHeadersChecked _ (P3PDirectives entries) = NE.toList <$> R.checkedFieldLines "P3P" directiveList entries


  headerName _ = hP3P


p3pParser :: ParserT st String P3P
p3pParser = P3PDirectives <$> R.listParser "P3P" directiveList


renderP3P :: P3P -> M.Builder
renderP3P (P3PDirectives entries) = R.renderList entries


directiveList :: ParserT st String [P3PDirective]
directiveList = httpList directive
  where
    directive = do
      name <- rfc9110TokenBS
      case name of
        "policyref" -> P3PPolicyRef <$> assign quotedURI
        "CP" -> P3PCompactPolicy <$> assign quotedPolicy
        _ -> P3PExtension name <$> optional (assign extensionValue)
    assign value = ows *> $(char '=') *> ows *> value
    quotedURI = $(char '"') *> byteStringOf uriReferenceBytes <* $(char '"')
    extensionValue = (P3PExtensionToken <$> rfc9110TokenBS) <|> (P3PExtensionQuoted <$> httpQuotedBytes)


quotedPolicy :: ParserT st String (NonEmpty P3PCompactToken)
quotedPolicy =
  $(char '"') *> ((:|) <$> compactToken <*> many ($(char ' ') *> compactToken)) <* $(char '"')


compactToken :: ParserT st String P3PCompactToken
compactToken = knownCompactToken <|> (P3PUnknownCompact <$> rfc9110TokenBS)


-- Known three-octet codes can be dispatched without constructing a token
-- slice. Check the delimiter before committing and keep unknown words intact.
knownCompactToken :: ParserT st String P3PCompactToken
knownCompactToken = do
  prefix <- anyWord16be
  third <- anyWord8
  case IntMap.lookup (256 * fromIntegral prefix + fromIntegral third) tokensByCode of
    Just token@(P3PKnownCompact claim _) ->
      branch boundary (pure token) (qualifiedClaim claim)
    _ -> failed
  where
    boundary = lookahead ($(char ' ') <|> $(char '"'))
    qualifiedClaim claim
      | allowsRequirement claim = do
          suffix <- anyWord8
          requirement <- case suffix of
            97 -> pure P3PAlways
            105 -> pure P3POptIn
            111 -> pure P3POptOut
            _ -> failed
          boundary *> pure (P3PKnownCompact claim (Just requirement))
      | otherwise = failed
{-# NOINLINE knownCompactToken #-}


-- | Three-octet case-sensitive vocabulary keys.
packedCode :: ByteString -> Int
packedCode bytes =
  65536 * fromIntegral (BSU.unsafeIndex bytes 0)
    + 256 * fromIntegral (BSU.unsafeIndex bytes 1)
    + fromIntegral (BSU.unsafeIndex bytes 2)
{-# INLINE packedCode #-}


allowsRequirement :: P3PClaim -> Bool
allowsRequirement (P3PPurpose P3PCurrent) = False
allowsRequirement (P3PPurpose _) = True
allowsRequirement (P3PRecipient P3POurs) = False
allowsRequirement (P3PRecipient _) = True
allowsRequirement _ = False


tokensByCode :: IntMap.IntMap P3PCompactToken
tokensByCode = IntMap.fromList (map (\(wire, claim) -> (packedCode wire, P3PKnownCompact claim Nothing)) compactVocabulary)
{-# NOINLINE tokensByCode #-}


codesByClaim :: Map.Map P3PClaim ByteString
codesByClaim = Map.fromList (map (\(wire, claim) -> (claim, wire)) compactVocabulary)
{-# NOINLINE codesByClaim #-}


compactVocabulary :: [(ByteString, P3PClaim)]
compactVocabulary =
  [ ("NOI", P3PAccess P3PNonIdent)
  , ("ALL", P3PAccess P3PAll)
  , ("CAO", P3PAccess P3PContactAndOther)
  , ("IDC", P3PAccess P3PIdentContact)
  , ("OTI", P3PAccess P3POtherIdent)
  , ("NON", P3PAccess P3PNone)
  , ("DSP", P3PDisputes)
  , ("COR", P3PRemedy P3PCorrect)
  , ("MON", P3PRemedy P3PMoney)
  , ("LAW", P3PRemedy P3PLaw)
  , ("NID", P3PNonIdentifiable)
  , ("CUR", P3PPurpose P3PCurrent)
  , ("ADM", P3PPurpose P3PAdmin)
  , ("DEV", P3PPurpose P3PDevelop)
  , ("TAI", P3PPurpose P3PTailoring)
  , ("PSA", P3PPurpose P3PPseudoAnalysis)
  , ("PSD", P3PPurpose P3PPseudoDecision)
  , ("IVA", P3PPurpose P3PIndividualAnalysis)
  , ("IVD", P3PPurpose P3PIndividualDecision)
  , ("CON", P3PPurpose P3PContact)
  , ("HIS", P3PPurpose P3PHistorical)
  , ("TEL", P3PPurpose P3PTelemarketing)
  , ("OTP", P3PPurpose P3POtherPurpose)
  , ("OUR", P3PRecipient P3POurs)
  , ("DEL", P3PRecipient P3PDelivery)
  , ("SAM", P3PRecipient P3PSame)
  , ("UNR", P3PRecipient P3PUnrelated)
  , ("PUB", P3PRecipient P3PPublic)
  , ("OTR", P3PRecipient P3POtherRecipient)
  , ("NOR", P3PRetention P3PNoRetention)
  , ("STP", P3PRetention P3PStatedPurpose)
  , ("LEG", P3PRetention P3PLegalRequirement)
  , ("BUS", P3PRetention P3PBusinessPractices)
  , ("IND", P3PRetention P3PIndefinitely)
  , ("PHY", P3PCategory P3PPhysical)
  , ("ONL", P3PCategory P3POnline)
  , ("UNI", P3PCategory P3PUniqueID)
  , ("PUR", P3PCategory P3PPurchase)
  , ("FIN", P3PCategory P3PFinancial)
  , ("COM", P3PCategory P3PComputer)
  , ("NAV", P3PCategory P3PNavigation)
  , ("INT", P3PCategory P3PInteractive)
  , ("DEM", P3PCategory P3PDemographic)
  , ("CNT", P3PCategory P3PContent)
  , ("STA", P3PCategory P3PState)
  , ("POL", P3PCategory P3PPolitical)
  , ("HEA", P3PCategory P3PHealth)
  , ("PRE", P3PCategory P3PPreference)
  , ("LOC", P3PCategory P3PLocation)
  , ("GOV", P3PCategory P3PGovernment)
  , ("OTC", P3PCategory P3POtherCategory)
  , ("TST", P3PTest)
  ]


renderDirective :: P3PDirective -> M.Builder
renderDirective (P3PPolicyRef uri) = "policyref=\"" <> M.byteString uri <> "\""
renderDirective (P3PCompactPolicy (first :| rest)) =
  "CP=\"" <> renderToken first <> foldMap ((" " <>) . renderToken) rest <> "\""
renderDirective (P3PExtension name opt) = M.byteString name <> foldMap (("=" <>) . renderExtensionValue) opt


renderExtensionValue :: P3PExtensionValue -> M.Builder
renderExtensionValue (P3PExtensionToken token) = M.byteString token
renderExtensionValue (P3PExtensionQuoted bytes) = M.byteString (encodeHttpQuotedBytes (SBS.toShort bytes))


renderToken :: P3PCompactToken -> M.Builder
renderToken (P3PUnknownCompact bytes) = M.byteString bytes
renderToken (P3PKnownCompact claim requirement) =
  M.byteString (Map.findWithDefault "" claim codesByClaim) <> foldMap renderRequirement requirement
  where
    renderRequirement P3PAlways = "a"
    renderRequirement P3POptIn = "i"
    renderRequirement P3POptOut = "o"
