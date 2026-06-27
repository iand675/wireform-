{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7486 §9.3 @Hobareg@ — a response header used by HTTP Origin-Bound
Authentication (HOBA) to report the state of an account registration.

@
Hobareg = \"register\" / \"reg-in-progress\"
@

@register@ signals that registration has completed; @reg-in-progress@ signals
that it is still underway. The token is matched case-sensitively.

Spec: <https://www.rfc-editor.org/rfc/rfc7486.html#section-9.3 RFC 7486 §9.3>

See also: "Network.HTTP.Headers.Authorization", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.AuthenticationInfo".
-}
module Network.HTTP.Headers.Hobareg (
  Hobareg (..),
  hobaregParser,
  renderHobareg,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHobareg)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Hobareg@ registration-status value (RFC 7486 §9.3).
data Hobareg
  = -- | @register@ — the registration has completed.
    HobaregRegister
  | -- | @reg-in-progress@ — the registration is still in progress.
    HobaregInProgress
  deriving stock (Eq, Show)


instance KnownHeader Hobareg where
  type ParseFailure Hobareg = String
  type Cardinality Hobareg = 'ZeroOrOne
  type Direction Hobareg = 'Response


  parseFromHeaders _ headers = case runParser hobaregParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Hobareg header: " <> show rest
    Fail -> Left "Failed to parse Hobareg header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderHobareg


  headerName _ = hHobareg


hobaregParser :: ParserT st String Hobareg
hobaregParser =
  $( switch
      [|
        case _ of
          "reg-in-progress" -> pure HobaregInProgress
          "register" -> pure HobaregRegister
        |]
   )


renderHobareg :: Hobareg -> M.Builder
renderHobareg = \case
  HobaregRegister -> "register"
  HobaregInProgress -> "reg-in-progress"
