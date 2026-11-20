{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7486 §6.1 @Hobareg@ — a response header used by HTTP Origin-Bound
Authentication (HOBA) to report the state of an account registration.

@
Hobareg = \"regok\" / \"reginwork\"
@

The tokens are case-insensitive. A present invalid or repeated field is
interpreted as @reginwork@. Registration succeeds with @regok@ on a 2xx
response.

Spec: <https://www.rfc-editor.org/rfc/rfc7486.html#section-6.1 RFC 7486 §6.1>

See also: "Network.HTTP.Headers.Authorization", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.AuthenticationInfo".
-}
module Network.HTTP.Headers.Hobareg (
  Hobareg (..),
  hobaregParser,
  renderHobareg,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHobareg)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Hobareg@ registration-status value (RFC 7486 §6.1).
data Hobareg
  = -- | @regok@ — the registration has completed on a 2xx response.
    HobaregRegister
  | -- | @reginwork@ — the registration is still in progress.
    HobaregInProgress
  deriving stock (Eq, Show)


instance KnownHeader Hobareg where
  type ParseFailure Hobareg = String
  type Cardinality Hobareg = 'ZeroOrOne
  type Direction Hobareg = 'Response


  parseFromHeaders _ ("regok" :| []) = Right HobaregRegister
  parseFromHeaders _ ("reginwork" :| []) = Right HobaregInProgress
  parseFromHeaders _ headers = case parseSingleFieldLine "Hobareg" hobaregParser headers of
    Right value -> Right value
    Left _ -> Right HobaregInProgress


  renderToHeaders _ HobaregRegister = "regok"
  renderToHeaders _ HobaregInProgress = "reginwork"


  headerName _ = hHobareg


hobaregParser :: ParserT st String Hobareg
hobaregParser =
  $( switchAsciiCI
       [|
         case _ of
           "reginwork" -> pure HobaregInProgress
           "regok" -> pure HobaregRegister
         |]
   )


renderHobareg :: Hobareg -> M.Builder
renderHobareg = \case
  HobaregRegister -> "regok"
  HobaregInProgress -> "reginwork"
