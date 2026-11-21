{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | Command for a named proxy from the W3C Notification for Proxy Caches
working draft. See <https://www.w3.org/TR/WD-proxy>.
-}
module Network.HTTP.Headers.ProxyInstruction (
  ProxyInstruction (ProxyInstruction, proxyInstructionValue),
  ProxyInstructionDetails (..),
  ProxyCommand (..),
  proxyInstructionDetails,
  proxyInstructionFromDetails,
  proxyInstructionParser,
  renderProxyInstruction,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAsciiUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyInstruction)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.ProxyFeatures (proxyNameParser)


data ProxyInstruction
  = RawProxyInstruction !ST.ShortText
  | ParsedProxyInstruction !ByteString !ProxyInstructionDetails


-- | The legacy wire spelling. Use 'proxyInstructionFromDetails' for checked authoring.
pattern ProxyInstruction :: ST.ShortText -> ProxyInstruction
pattern ProxyInstruction {proxyInstructionValue} <- (legacyText -> proxyInstructionValue)
  where
    ProxyInstruction raw = RawProxyInstruction raw


{-# COMPLETE ProxyInstruction #-}


instance Eq ProxyInstruction where
  left == right = wireBytes left == wireBytes right


instance Show ProxyInstruction where
  showsPrec precedence (ProxyInstruction value) =
    showParen (precedence > 10) $ showString "ProxyInstruction {proxyInstructionValue = " . shows value . showChar '}'


legacyText :: ProxyInstruction -> ST.ShortText
legacyText (RawProxyInstruction text) = text
legacyText (ParsedProxyInstruction wire _) = STU.fromByteStringUnsafe wire


wireBytes :: ProxyInstruction -> ByteString
wireBytes (RawProxyInstruction text) = ST.toByteString text
wireBytes (ParsedProxyInstruction wire _) = wire


data ProxyInstructionDetails = ProxyInstructionDetails
  { proxyInstructionName :: !ByteString
  , proxyInstructionCommand :: !ProxyCommand
  }
  deriving stock (Eq, Show)


data ProxyCommand = ProxyCommandLog | ProxyCommandSummary | ProxyCommandInform
  deriving stock (Eq, Show)


proxyInstructionDetails :: ProxyInstruction -> Either String ProxyInstructionDetails
proxyInstructionDetails (RawProxyInstruction text) =
  parseSingleFieldLine "Proxy-Instruction" instructionParser (ST.toByteString text :| [])
proxyInstructionDetails (ParsedProxyInstruction _ details) = Right details


proxyInstructionFromDetails :: ProxyInstructionDetails -> Either String ProxyInstruction
proxyInstructionFromDetails details@(ProxyInstructionDetails name command) = do
  parsed <-
    parseFieldLine "Proxy-Instruction" proxyInstructionParser $
      M.toStrictByteString (M.byteString name <> "; " <> commandWire command)
  if proxyInstructionDetails parsed == Right details
    then Right parsed
    else Left "Proxy-Instruction details do not match their wire representation"


instance KnownHeader ProxyInstruction where
  type ParseFailure ProxyInstruction = String
  type Cardinality ProxyInstruction = 'ZeroOrOne
  type Direction ProxyInstruction = 'Response


  parseFromHeaders _ fields@(wire :| []) =
    ParsedProxyInstruction wire <$> parseSingleFieldLine "Proxy-Instruction" instructionParser fields
  parseFromHeaders _ _ = Left "Multiple Proxy-Instruction field lines"


  renderToHeaders _ = wireBytes


  renderToHeadersChecked _ (ParsedProxyInstruction wire _) = Just wire
  renderToHeadersChecked _ (RawProxyInstruction text) =
    let wire = ST.toByteString text
    in either (const Nothing) (const (Just wire)) $
         parseSingleFieldLine "Proxy-Instruction" instructionParser (wire :| [])


  headerName _ = hProxyInstruction


proxyInstructionParser :: ParserT st String ProxyInstruction
proxyInstructionParser = withByteString (ows *> instructionParser <* ows) $ \details wire ->
  pure (ParsedProxyInstruction wire details)


instructionParser :: ParserT st String ProxyInstructionDetails
instructionParser = do
  name <- proxyNameParser
  ows *> $(char ';') *> ows
  command <- commandParser
  pure (ProxyInstructionDetails name command)


commandParser :: ParserT st String ProxyCommand
commandParser =
  branch ($(string "log") <* notFollowedBy (skipSatisfyAscii isTokenChar)) (pure ProxyCommandLog) otherCommand
  where
    otherCommand = do
      token <- rfc9110TokenBS
      case if BSC.any isAsciiUpper token then BSC.map toLower token else token of
        "log" -> pure ProxyCommandLog
        "summary" -> pure ProxyCommandSummary
        "inform" -> pure ProxyCommandInform
        _ -> err "Expected log, summary or inform in Proxy-Instruction"


renderProxyInstruction :: ProxyInstruction -> M.Builder
renderProxyInstruction (RawProxyInstruction raw) = shortText raw
renderProxyInstruction (ParsedProxyInstruction wire _) = M.byteString wire


commandWire :: ProxyCommand -> M.Builder
commandWire ProxyCommandLog = "log"
commandWire ProxyCommandSummary = "summary"
commandWire ProxyCommandInform = "inform"
