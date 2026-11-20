{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Referer-Root@ was the origin field in the original W3C cross-site
access draft. It requires an explicit port, even for a default port, and
uses the literal @null@ for a source without a host-based authority.

Spec: <https://www.w3.org/TR/2007/WD-access-control-20071126/>
-}
module Network.HTTP.Headers.RefererRoot (
  RefererRoot (RefererRoot, refererRootValue),
  RefererRootOrigin (..),
  refererRootOrigin,
  refererRootFromOrigin,
  refererRootParser,
  renderRefererRoot,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word16)
import Network.HTTP.Grammar.Authority (portNumber, uriHostBytes, uriSchemeBytes)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRefererRoot)
import qualified Network.HTTP.Headers.Mason as M


data RefererRoot
  = RawRefererRoot !ST.ShortText
  | ParsedRefererRoot !ByteString !RefererRootOrigin


{- | The legacy view of the original spelling. Use 'refererRootFromOrigin'
for checked authoring.
-}
pattern RefererRoot :: ST.ShortText -> RefererRoot
pattern RefererRoot {refererRootValue} <- (legacyText -> refererRootValue)
  where
    RefererRoot raw = RawRefererRoot raw


{-# COMPLETE RefererRoot #-}


instance Eq RefererRoot where
  left == right = wireBytes left == wireBytes right


instance Show RefererRoot where
  showsPrec precedence (RefererRoot value) =
    showParen (precedence > 10) $ showString "RefererRoot {refererRootValue = " . shows value . showChar '}'


legacyText :: RefererRoot -> ST.ShortText
legacyText (RawRefererRoot text) = text
legacyText (ParsedRefererRoot wire _) = STU.fromByteStringUnsafe wire


wireBytes :: RefererRoot -> ByteString
wireBytes (RawRefererRoot text) = ST.toByteString text
wireBytes (ParsedRefererRoot wire _) = wire


data RefererRootOrigin
  = RefererRootNull
  | RefererRootHost !ByteString !ByteString {-# UNPACK #-} !Word16
  deriving stock (Eq, Show)


refererRootOrigin :: RefererRoot -> Either String RefererRootOrigin
refererRootOrigin (RawRefererRoot raw) =
  parseSingleFieldLine "Referer-Root" rootParser (ST.toByteString raw :| [])
refererRootOrigin (ParsedRefererRoot _ origin) = Right origin


refererRootFromOrigin :: RefererRootOrigin -> Either String RefererRoot
refererRootFromOrigin RefererRootNull = Right nullRoot
refererRootFromOrigin origin@(RefererRootHost scheme host port) = do
  result <-
    parseFieldLine "Referer-Root" refererRootParser $
      M.toStrictByteString (M.byteString scheme <> "://" <> M.byteString host <> ":" <> M.word16Dec port)
  if refererRootOrigin result == Right origin
    then Right result
    else Left "Referer-Root components do not match their wire representation"


nullRoot :: RefererRoot
nullRoot = ParsedRefererRoot "null" RefererRootNull


instance KnownHeader RefererRoot where
  type ParseFailure RefererRoot = String
  type Cardinality RefererRoot = 'ZeroOrOne
  type Direction RefererRoot = 'Request


  parseFromHeaders _ ("null" :| []) = Right nullRoot
  parseFromHeaders _ fields@(wire :| []) =
    ParsedRefererRoot wire <$> parseSingleFieldLine "Referer-Root" rootParser fields
  parseFromHeaders _ _ = Left "Multiple Referer-Root field lines"


  renderToHeaders _ = wireBytes


  headerName _ = hRefererRoot


refererRootParser :: ParserT st String RefererRoot
refererRootParser = withByteString (ows *> rootParser <* ows) $ \origin wire ->
  pure (ParsedRefererRoot wire origin)


rootParser :: ParserT st String RefererRootOrigin
rootParser = hostOrigin <|> (RefererRootNull <$ $(string "null"))
  where
    hostOrigin = do
      scheme <-
        branch $(string "https://") (pure "https") $
          branch $(string "http://") (pure "http") $
            byteStringOf uriSchemeBytes <* $(string "://")
      host <- byteStringOf uriHostBytes
      if BS.last host == 46
        then err "Referer-Root host has a trailing dot"
        else RefererRootHost scheme host <$> ($(char ':') *> portNumber)


renderRefererRoot :: RefererRoot -> M.Builder
renderRefererRoot (RawRefererRoot raw) = shortText raw
renderRefererRoot (ParsedRefererRoot wire _) = M.byteString wire
