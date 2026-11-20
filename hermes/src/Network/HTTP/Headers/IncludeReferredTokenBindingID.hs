{- | RFC 8473 §5.3 @Include-Referred-Token-Binding-ID@ is a response
signal carried by the case-insensitive literal @true@.
-}
module Network.HTTP.Headers.IncludeReferredTokenBindingID (
  IncludeReferredTokenBindingID (..),
  includeReferredTokenBindingIDParser,
  renderIncludeReferredTokenBindingID,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIncludeReferredTokenBindingID)
import qualified Network.HTTP.Headers.Mason as M


-- | A disabled signal emits no field line.
newtype IncludeReferredTokenBindingID = IncludeReferredTokenBindingID
  {includeReferredTokenBindingID :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader IncludeReferredTokenBindingID where
  type ParseFailure IncludeReferredTokenBindingID = String
  type Cardinality IncludeReferredTokenBindingID = 'ZeroOrMore
  type Direction IncludeReferredTokenBindingID = 'Response


  parseFromHeaders _ fields@(wire :| [])
    | BS.length wire == 4 =
        if BSU.unsafeIndex wire 0 .|. 32 == 116
          && BSU.unsafeIndex wire 1 .|. 32 == 114
          && BSU.unsafeIndex wire 2 .|. 32 == 117
          && BSU.unsafeIndex wire 3 .|. 32 == 101
          then Right (IncludeReferredTokenBindingID True)
          else invalidSignal
    | otherwise = parseSingleFieldLine "Include-Referred-Token-Binding-ID" includeReferredTokenBindingIDParser fields
  parseFromHeaders _ _ = Left "Multiple Include-Referred-Token-Binding-ID field lines"


  renderToHeaders _ (IncludeReferredTokenBindingID True) = ["true"]
  renderToHeaders _ (IncludeReferredTokenBindingID False) = []


  omitHeaderValue = not . includeReferredTokenBindingID
  {-# INLINE omitHeaderValue #-}


  headerName _ = hIncludeReferredTokenBindingID


invalidSignal :: Either String a
invalidSignal = Left "Invalid Include-Referred-Token-Binding-ID signal"


includeReferredTokenBindingIDParser :: ParserT st String IncludeReferredTokenBindingID
includeReferredTokenBindingIDParser = do
  wire <- anyWord32be
  if wire .|. 0x20202020 == 0x74727565
    then pure (IncludeReferredTokenBindingID True)
    else failed


renderIncludeReferredTokenBindingID :: IncludeReferredTokenBindingID -> M.Builder
renderIncludeReferredTokenBindingID (IncludeReferredTokenBindingID enabled) =
  if enabled then "true" else mempty
