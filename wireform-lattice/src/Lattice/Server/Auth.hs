{- | Visibility context: claims payload and proof (spec §8.2).

The payload rides in the URL as @vc={base64url(canonicalJson(claims))}@ and
is part of the cache key. The proof rides in the @X-Vc-Auth@ header as
@{exp}.{sig}@ — outside the cache key, so token rotation never disturbs
cached entries. Origins verify the proof on every request; caches never
verify anything.

The proof scheme is deployment-pluggable ('ProofVerifier'); the bundled
implementation is HMAC-SHA256 by a shared-secret auth service:
@sig = base64url(HMAC-SHA256(secret, payloadB64 <> "." <> exp))@ with
@exp@ a unix timestamp in seconds rendered as decimal.
-}
module Lattice.Server.Auth (
  ClaimsPayload (..),
  encodeClaims,
  decodeClaims,
  ProofVerifier (..),
  hmacVerifier,
  hmacProof,
  ProofError (..),
) where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC qualified as HMAC
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (POSIXTime)
import Lattice.Types (ClaimName (..), Claims)
import Lattice.Value (canonicalJson)


-- | A decoded @vc@ parameter: the claims and the exact payload bytes presented.
data ClaimsPayload = ClaimsPayload
  { cpClaims :: Claims
  , cpRaw :: ByteString
  -- ^ The base64url text as presented (the cache-key component and the
  -- proof's subject).
  }
  deriving stock (Eq, Show)


-- | Canonical payload: base64url of the canonical JSON object of claims.
encodeClaims :: Claims -> Text
encodeClaims claims =
  TE.decodeUtf8 . B64U.encodeUnpadded . canonicalJson . A.Object $
    KM.fromList
      [ (AK.fromText c, v)
      | (ClaimName c, v) <- Map.toList claims
      ]


decodeClaims :: Text -> Either Text ClaimsPayload
decodeClaims t = do
  bs <-
    either (Left . T.pack) Right $
      B64U.decodeUnpadded (TE.encodeUtf8 t)
  v <- maybe (Left "vc payload is not JSON") Right (A.decodeStrict bs)
  case v of
    A.Object o ->
      Right
        ClaimsPayload
          { cpClaims = Map.fromList [(ClaimName (AK.toText k), val) | (k, val) <- KM.toList o]
          , cpRaw = TE.encodeUtf8 t
          }
    _ -> Left "vc payload must be a JSON object"


data ProofError
  = ProofMissing
  | ProofMalformed
  | ProofExpired
  | ProofInvalid
  deriving stock (Eq, Show)


{- | Deployment-supplied proof verification: given the presented payload
(base64url bytes) and the @X-Vc-Auth@ header value, accept or reject.
-}
newtype ProofVerifier = ProofVerifier
  { verifyProof :: ByteString -> Maybe ByteString -> IO (Either ProofError ())
  }


-- | @{exp}.{sig}@ with @sig = base64url(HMAC-SHA256(secret, payload <> "." <> exp))@.
hmacVerifier ::
  -- | Shared secret.
  ByteString ->
  -- | Current time supplier.
  IO POSIXTime ->
  ProofVerifier
hmacVerifier secret now = ProofVerifier $ \payload mProof -> case mProof of
  Nothing -> pure (Left ProofMissing)
  Just proof -> case BS8.break (== '.') proof of
    (expBytes, rest)
      | Just (sig) <- BS8.stripPrefix "." rest
      , Just (expSecs, "") <- BS8.readInteger expBytes -> do
          t <- now
          if fromInteger expSecs < t
            then pure (Left ProofExpired)
            else
              pure $
                if BA.constEq (TE.encodeUtf8 (hmacSig secret payload expSecs)) sig
                  then Right ()
                  else Left ProofInvalid
    _ -> pure (Left ProofMalformed)


-- | Mint a proof (test and demo helper; production mints live in the auth service).
hmacProof :: ByteString -> Text -> Integer -> Text
hmacProof secret payloadB64 expSecs =
  T.pack (show expSecs) <> "." <> hmacSig secret (TE.encodeUtf8 payloadB64) expSecs


hmacSig :: ByteString -> ByteString -> Integer -> Text
hmacSig secret payload expSecs =
  TE.decodeUtf8 . B64U.encodeUnpadded . BA.convert $
    hmacGetDigest (HMAC.hmac @_ @_ @SHA256 secret (payload <> "." <> BS8.pack (show expSecs)))
  where
    hmacGetDigest = HMAC.hmacGetDigest
