{-# LANGUAGE TemplateHaskell #-}

{- |
@DNT@ (\"Do Not Track\") — a /de-facto/ (formerly W3C-tracked, never
IANA-registered) request header by which a user agent signals the user's
tracking preference. It is now obsolete (the W3C specification was retired and
most browsers have removed it) but is still emitted by some clients.

== Grammar (de-facto)

@
DNT = "0" / "1"
@

@1@ means \"do not track me\"; @0@ means \"tracking is acceptable\". The absent
header (no preference expressed) is modelled by the @'ZeroOrOne'@ cardinality —
i.e. the absence of a 'DNT' value altogether — rather than by a third
in-band token.

This is a /de-facto/ header; see the (retired) W3C Tracking Preference
Expression specification, <https://www.w3.org/TR/tracking-dnt/>.

See also: "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.ClearSiteData", "Network.HTTP.Headers.PermissionsPolicy".
-}
module Network.HTTP.Headers.DNT (
  DNT (..),
  dNTParser,
  renderDNT,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDNT)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @True@ when the user requests not to be tracked (@1@), @False@ otherwise (@0@).
newtype DNT = DNT {dntDoNotTrack :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader DNT where
  type ParseFailure DNT = String
  type Cardinality DNT = 'ZeroOrOne
  type Direction DNT = 'Request


  parseFromHeaders _ headers = case runParser dNTParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing DNT header: " <> show rest
    Fail -> Left "Failed to parse DNT header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDNT


  headerName _ = hDNT


dNTParser :: ParserT st String DNT
dNTParser =
  $( switch
      [|
        case _ of
          "1" -> pure (DNT True)
          "0" -> pure (DNT False)
        |]
   )


renderDNT :: DNT -> M.Builder
renderDNT (DNT True) = "1"
renderDNT (DNT False) = "0"
