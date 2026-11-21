{-# LANGUAGE TemplateHaskell #-}

{- |
@OSLC-Core-Version@ — advertises the OSLC (Open Services for Lifecycle
Collaboration) Core protocol version in effect for a request or response,
e.g. @2.0@. Sent by both clients and servers.

The value is two nonempty decimal components separated by a dot. The major
component is at least 2. Parsing preserves the spelling, and
'oslcCoreVersionNumbers' exposes both components as natural numbers.

<https://docs.oasis-open-projects.org/oslc-op/core/v3.0/os/oslc-core.html OSLC Core 3.0 §4.2.2>

See also: "Network.HTTP.Headers.CalManagedID", "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.ODataVersion", "Network.HTTP.Headers.ODataMaxVersion".
-}
module Network.HTTP.Headers.OSLCCoreVersion (
  OSLCCoreVersion (..),
  OSLCCoreVersionNumbers (..),
  oslcCoreVersionNumbers,
  oslcCoreVersionParser,
  renderOSLCCoreVersion,
) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOSLCCoreVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Numeric.Natural (Natural)
import Wireform.Parser.Driver (parseByteString)


-- | An OSLC Core protocol version string (e.g. @2.0@), preserved verbatim.
newtype OSLCCoreVersion = OSLCCoreVersion {oslcCoreVersion :: ST.ShortText}
  deriving stock (Eq, Show)


data OSLCCoreVersionNumbers = OSLCCoreVersionNumbers
  { oslcCoreMajor :: !Natural
  , oslcCoreMinor :: !Natural
  }
  deriving stock (Eq, Show)


{- | Read the numeric components. Manually constructed invalid spellings
yield 'Nothing'. The wire parser validates the grammar before returning.
-}
oslcCoreVersionNumbers :: OSLCCoreVersion -> Maybe OSLCCoreVersionNumbers
oslcCoreVersionNumbers (OSLCCoreVersion version) = do
  (major, suffix) <- BSC.readNatural (ST.toByteString version)
  ('.', minorBytes) <- BSC.uncons suffix
  (minor, rest) <- BSC.readNatural minorBytes
  guard (major >= 2 && BSC.null rest)
  pure (OSLCCoreVersionNumbers major minor)


instance KnownHeader OSLCCoreVersion where
  type ParseFailure OSLCCoreVersion = String
  type Cardinality OSLCCoreVersion = 'ZeroOrOne
  type Direction OSLCCoreVersion = 'RequestAndResponse


  parseFromHeaders _ (wire :| [])
    | BS.length wire == 3
    , BSU.unsafeIndex wire 1 == 0x2e
    , BSU.unsafeIndex wire 2 == 0x30
    , let major = BSU.unsafeIndex wire 0
    , major == 0x32 || major == 0x33 =
        if major == 0x32 then Right (OSLCCoreVersion "2.0") else Right (OSLCCoreVersion "3.0")
  parseFromHeaders _ fields = parseSingleFieldLine "OSLC-Core-Version" oslcCoreVersionParser fields


  renderToHeaders _ = ST.toByteString . oslcCoreVersion


  renderToHeadersChecked _ (OSLCCoreVersion version)
    | version == "3.0" = Just "3.0"
    | version == "2.0" = Just "2.0"
    | otherwise = checkedVersionBytes version
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hOSLCCoreVersion


oslcCoreVersionParser :: ParserT st String OSLCCoreVersion
oslcCoreVersionParser = OSLCCoreVersion <$> shortASCIIFromParser_ versionSpelling


checkedVersionBytes :: ST.ShortText -> Maybe BS.ByteString
checkedVersionBytes version =
  let wire = ST.toByteString version
  in either (const Nothing) (const (Just wire)) $
       parseByteString (versionSpelling <* eof) wire
{-# NOINLINE checkedVersionBytes #-}


versionSpelling :: ParserT st e ()
versionSpelling = do
  skipMany $(char '0')
  branch $(char '1') (skipSome digit) (skipSatisfyAscii (\c -> '2' <= c && c <= '9') *> skipMany digit)
  $(char '.')
  skipSome digit
  where
    digit = skipSatisfyAscii isDigit
{-# INLINE versionSpelling #-}


renderOSLCCoreVersion :: OSLCCoreVersion -> M.Builder
renderOSLCCoreVersion = shortText . oslcCoreVersion
