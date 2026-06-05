-- | EDI standards registry and lightweight detection.
module EDI.Standard
  ( EDIStandard(..)
  , StandardProfile(..)
  , allStandards
  , standardProfile
  , standardSyntax
  , detectStandard
  , detectStandards
  , standardName
  ) where

import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import EDI.Value

data EDIStandard
  = UNEDIFACT
  | ANSIASCX12
  | GS1EDI
  | TRADACOMS
  | ODETTE
  | VDA
  | HL7
  | HIPAA
  | IATACargoIMP
  | NCPDPScript
  | NCPDPTelecom
  | EDIGAS
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data StandardProfile = StandardProfile
  { profileStandard :: !EDIStandard
  , profileName :: !Text
  , profileScope :: !Text
  , profileSyntax :: !(Maybe Syntax)
  , profileEnvelopeTags :: !(Vector Text)
  , profileNotes :: !Text
  }
  deriving stock (Show, Eq)

allStandards :: Vector StandardProfile
allStandards = V.fromList
  [ StandardProfile UNEDIFACT "UN/EDIFACT" "International" (Just edifactSyntax) (V.fromList ["UNA", "UNB", "UNH", "UNT", "UNZ"]) "UN-recommended international EDI syntax and message envelope."
  , StandardProfile ANSIASCX12 "ANSI ASC X12" "North America" (Just defaultSyntax) (V.fromList ["ISA", "GS", "ST", "SE", "GE", "IEA"]) "North American EDI interchange, group, and transaction envelopes."
  , StandardProfile GS1EDI "GS1 EDI" "Global supply chain" (Just edifactSyntax) (V.fromList ["UNB", "UNH", "UNT", "UNZ"]) "GS1 EDI includes GS1 EANCOM, a UN/EDIFACT subset, plus XML forms outside segment syntax."
  , StandardProfile TRADACOMS "TRADACOMS" "UK retail" (Just tradacomsSyntax) (V.fromList ["STX", "MHD", "MTR", "END"]) "ANA / GS1 UK retail EDI syntax."
  , StandardProfile ODETTE "ODETTE" "European automotive" (Just edifactSyntax) (V.fromList ["UNB", "UNH", "UNT", "UNZ"]) "European automotive EDI, commonly carried as EDIFACT-derived messages."
  , StandardProfile VDA "VDA" "German automotive" Nothing V.empty "German automotive VDA messages are commonly fixed-width records rather than delimiter-separated segment streams."
  , StandardProfile HL7 "HL7" "Healthcare" (Just hl7Syntax) (V.fromList ["MSH", "PID", "OBR", "OBX"]) "HL7 v2 pipe-delimited healthcare messages."
  , StandardProfile HIPAA "HIPAA X12" "US healthcare" (Just defaultSyntax) (V.fromList ["ISA", "GS", "ST", "SE", "GE", "IEA"]) "HIPAA mandates specific X12 transaction sets for covered electronic healthcare transactions."
  , StandardProfile IATACargoIMP "IATA Cargo-IMP" "Air cargo" (Just cargoImpSyntax) (V.fromList ["FWB", "FHL", "FFM", "FSU"]) "IATA Cargo Interchange Message Procedures for air cargo exchange."
  , StandardProfile NCPDPScript "NCPDP SCRIPT" "US pharmacy" Nothing V.empty "NCPDP SCRIPT is XML-oriented for electronic prescription exchange."
  , StandardProfile NCPDPTelecom "NCPDP Telecommunications" "US pharmacy" (Just ncpdpTelecomSyntax) V.empty "NCPDP Telecom uses control-character-separated pharmacy claim transactions."
  , StandardProfile EDIGAS "EDIGAS" "Gas commerce and transport" (Just edifactSyntax) (V.fromList ["UNB", "UNH", "UNT", "UNZ"]) "EDIGAS / Edig@s messages are EDIFACT-based gas-market transactions."
  ]

standardProfile :: EDIStandard -> StandardProfile
standardProfile target =
  case V.find ((== target) . profileStandard) allStandards of
    Just profile -> profile
    Nothing -> error "EDI.Standard.standardProfile: impossible missing standard"

standardSyntax :: EDIStandard -> Maybe Syntax
standardSyntax = profileSyntax . standardProfile

standardName :: EDIStandard -> Text
standardName = profileName . standardProfile

detectStandard :: Text -> Maybe EDIStandard
detectStandard input =
  case detectStandards input of
    standards | V.null standards -> Nothing
    standards -> Just (V.head standards)

detectStandards :: Text -> Vector EDIStandard
detectStandards input =
  V.fromList (go stripped upperInput)
  where
    stripped = T.dropWhile isOuterWhitespace input
    upperInput = T.toUpper stripped

go :: Text -> Text -> [EDIStandard]
go stripped upperInput
  | T.isPrefixOf "ISA" stripped =
      if containsAny ["*837*", "*835*", "*834*", "*270*", "*271*", "*276*", "*277*", "*278*", "*820*", "*999*"] upperInput
        then [HIPAA, ANSIASCX12]
        else [ANSIASCX12]
  | T.isPrefixOf "UNA" stripped || T.isPrefixOf "UNB" stripped =
      edifactFamily upperInput
  | T.isPrefixOf "MSH" stripped = [HL7]
  | T.isPrefixOf "STX" stripped || T.isPrefixOf "STX=" stripped = [TRADACOMS]
  | containsAny ["<SCRIPT", "NCPDP SCRIPT", "NCPDPSCRIPT"] upperInput = [NCPDPScript]
  | containsAny ["FWB/", "FHL/", "FFM/", "FSU/"] upperInput = [IATACargoIMP]
  | containsAny ["NCPDP", "B1", "B2", "B3"] upperInput = [NCPDPTelecom]
  | containsAny ["VDA", "4905", "4913"] upperInput = [VDA]
  | otherwise = []

edifactFamily :: Text -> [EDIStandard]
edifactFamily upperInput
  | containsAny ["EDIGAS", "NOMINT", "NOMRES", "DELORD", "APERAK"] upperInput =
      [EDIGAS, UNEDIFACT]
  | containsAny ["EANCOM", "EAN0", "EAN00", "EAN01", "GS1"] upperInput =
      [GS1EDI, UNEDIFACT]
  | containsAny ["ODETTE", "AVIEXP", "DELINS", "CALDEL"] upperInput =
      [ODETTE, UNEDIFACT]
  | containsAny ["IATA", "CARGO"] upperInput =
      [IATACargoIMP, UNEDIFACT]
  | otherwise = [UNEDIFACT]

containsAny :: [Text] -> Text -> Bool
containsAny needles haystack =
  any (`T.isInfixOf` haystack) needles

isOuterWhitespace :: Char -> Bool
isOuterWhitespace c = c == '\r' || c == '\n' || c == ' ' || c == '\t'

edifactSyntax :: Syntax
edifactSyntax = Syntax
  { elementSeparator = '+'
  , componentSeparator = ':'
  , repetitionSeparator = Nothing
  , segmentTerminator = '\''
  }

tradacomsSyntax :: Syntax
tradacomsSyntax = Syntax
  { elementSeparator = '='
  , componentSeparator = ':'
  , repetitionSeparator = Nothing
  , segmentTerminator = '\''
  }

hl7Syntax :: Syntax
hl7Syntax = Syntax
  { elementSeparator = '|'
  , componentSeparator = '^'
  , repetitionSeparator = Just '~'
  , segmentTerminator = '\r'
  }

cargoImpSyntax :: Syntax
cargoImpSyntax = Syntax
  { elementSeparator = '/'
  , componentSeparator = ','
  , repetitionSeparator = Nothing
  , segmentTerminator = '\n'
  }

ncpdpTelecomSyntax :: Syntax
ncpdpTelecomSyntax = Syntax
  { elementSeparator = '\FS'
  , componentSeparator = '\GS'
  , repetitionSeparator = Nothing
  , segmentTerminator = '\RS'
  }
