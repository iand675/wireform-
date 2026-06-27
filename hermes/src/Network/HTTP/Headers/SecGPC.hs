{-# LANGUAGE TemplateHaskell #-}

{- |
The @Sec-GPC@ HTTP request header carries the Global Privacy Control signal.
When present with the value @1@, the user is asserting a do-not-sell /
do-not-share preference; @1@ is the only valid value, and the header is omitted
when no such preference is being expressed.

Spec: <https://privacycg.github.io/gpc-spec/> (Global Privacy Control, a W3C
Privacy Community Group draft; not IANA-registered).

See also: "Network.HTTP.Headers.DNT", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.AcceptCH".
-}
module Network.HTTP.Headers.SecGPC (
  SecGPC (..),
  secGPCParser,
  renderSecGPC,
) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecGPC)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Presence of @Sec-GPC: 1@, an asserted Global Privacy Control signal.
data SecGPC = SecGPC
  deriving stock (Eq, Show)


instance KnownHeader SecGPC where
  type ParseFailure SecGPC = String
  type Cardinality SecGPC = 'ZeroOrOne
  type Direction SecGPC = 'Request


  parseFromHeaders _ headers = case runParser secGPCParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Sec-GPC header: " <> show rest)
    Fail -> Left "Failed to parse Sec-GPC header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecGPC


  headerName _ = hSecGPC


secGPCParser :: ParserT st String SecGPC
secGPCParser = $(char '1') $> SecGPC


renderSecGPC :: SecGPC -> M.Builder
renderSecGPC SecGPC = M.char7 '1'
