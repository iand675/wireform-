{- |
@Label@ (RFC 3253 §8.3) — request header used by WebDAV versioning
methods to select the version of a version-controlled resource that
carries the given label name.

@
Label = \"Label\" \":\" label-name
@

The label name is treated here as an RFC 9110 token.

Spec: <https://www.rfc-editor.org/rfc/rfc3253.html#section-8.3>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.If", "Network.HTTP.Headers.LockToken".
-}
module Network.HTTP.Headers.Label (
  Label (..),
  labelParser,
  renderLabel,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLabel)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The label name selecting a labelled version.
newtype Label = Label {labelName :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Label where
  type ParseFailure Label = String
  type Cardinality Label = 'ZeroOrOne
  type Direction Label = 'Request


  parseFromHeaders _ headers = case runParser labelParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Label header: " <> show rest
    Fail -> Left "Failed to parse Label header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderLabel


  headerName _ = hLabel


labelParser :: ParserT st String Label
labelParser = do
  ows
  name <- rfc9110Token
  ows
  pure (Label name)


renderLabel :: Label -> M.Builder
renderLabel (Label name) = shortText name
