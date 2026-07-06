{- | The compatibility registry's deployment log (spec §17.1): every
published schema, content-addressed by its canonical IDL hash, with its
deployment timestamp. In-memory and advisory by design — the registry is
an analysis component, never a serving dependency (§17.1: total registry
loss degrades deploy-time checking, not requests), so deployments wrap
'recordDeploy' at startup and the log rebuilds from redeploys.

The Origin-facing halves live where their imports allow:
'Lattice.Server.exportCorpus' snapshots the memo + tenure + client
attribution into @[CorpusEntry]@, and the @POST \/schema\/check@ \/
@GET \/schema\/corpus@ routes are served by "Lattice.Server" when
'Lattice.Server.ocRegistry' is configured. The pure diff\/check engine is
"Lattice.Compat".
-}
module Lattice.Registry (
  Registry,
  newRegistry,
  recordDeploy,
  registryLog,
  DeployEntry (..),
  CorpusEntry (..),
) where

import Control.Concurrent.STM
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric.Natural (Natural)
import Data.Time (UTCTime)
import Lattice.Hash (schemaHash)
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Schema (Schema)


{- | One corpus entry (§17.4): a memoized canonical query text with its
tenure hit count and the client builds ('Lattice-Client' values) that
presented it. Snapshot via 'Lattice.Server.exportCorpus'; consumed by
'Lattice.Compat.checkSchemas' for traffic-weighted break reports and
served verbatim by @GET \/schema\/corpus@.
-}
data CorpusEntry = CorpusEntry
  { ceText :: Text
  , ceHits :: Natural
  , ceClients :: [Text]
  }
  deriving stock (Eq, Show)


instance A.ToJSON CorpusEntry where
  toJSON e =
    A.object
      [ "text" .= ceText e
      , "hits" .= ceHits e
      , "clients" .= ceClients e
      ]


{- | The deployment log: canonical-IDL hash → newest deployment. Keyed by
content, so redeploying an unchanged schema refreshes its timestamp
rather than growing the log — a schema is "within the window" (§17.3)
whenever its newest deployment is, which the newest-wins merge preserves.
-}
newtype Registry = Registry (TVar (Map Text DeployEntry))


-- | One logged deployment (§17.1): the schema hash, the canonical IDL
-- text it addresses, the parsed model, and the newest deployment time.
data DeployEntry = DeployEntry
  { deTime :: UTCTime
  , deHash :: Text
  -- ^ 'Lattice.Hash.schemaHash' of 'deIdl' — the same identity the origin
  -- publishes as @Lattice-Schema@.
  , deIdl :: Text
  -- ^ The canonical IDL text ('Lattice.IDL.Print.canonicalIdl').
  , deSchema :: Schema
  }


newRegistry :: IO Registry
newRegistry = Registry <$> newTVarIO Map.empty


{- | Log a deployment: parse, canonicalize, and content-address the IDL,
then record it under its hash (newest deployment time wins). An
unparseable document throws a 'userError' naming the first diagnostic —
a schema that does not parse was never deployed, so logging it is a
caller bug, not a state.
-}
recordDeploy :: Registry -> UTCTime -> Text -> IO ()
recordDeploy (Registry v) t src = case parseSchema src of
  Left errs ->
    ioError . userError $
      "recordDeploy: unparseable IDL: "
        <> maybe "no diagnostics" (T.unpack . seMessage) (listToMaybe' errs)
  Right schema -> do
    let idl = canonicalIdl schema
        h = schemaHash idl
        entry = DeployEntry {deTime = t, deHash = h, deIdl = idl, deSchema = schema}
    atomically . modifyTVar' v $
      Map.insertWith (\new old -> if deTime new >= deTime old then new else old) h entry
  where
    listToMaybe' = \case
      [] -> Nothing
      (e : _) -> Just e


-- | Every logged schema, newest deployment first. Window filtering is the
-- checker's job ('Lattice.Compat'): the log holds facts, not policy.
registryLog :: Registry -> IO [DeployEntry]
registryLog (Registry v) = sortOn (Down . deTime) . Map.elems <$> readTVarIO v
