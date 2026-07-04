-- | Coverage / novelty features distilled from a run's 'SimEvent' trace, and
-- the global visit-count bookkeeping the guided search ("Sim.Search") uses to
-- reward rarely-explored behaviour.
--
-- Three feature families feed novelty search: coverage /edges/ (@EvCover@),
-- observed /states/ (@EvObserve@ fingerprints), and /rare log templates/
-- (@EvLog@ text with numeric runs collapsed, so "commit 41" and "commit 999"
-- share one template). 'novelty' rewards a run for exhibiting features not yet
-- seen globally, so a run that treads only well-worn ground scores zero.
module Sim.Coverage
  ( CovSet (..)
  , emptyCov
  , covFromEvents
  , mergeCov
  , logTemplateKey
  , GlobalCounts
  , noNovelty
  , bumpCounts
  , novelty
  ) where

import Data.Char (isDigit)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Sim.Entropy (mixKey)
import Sim.Types (SimEvent (..), SiteId, StateKey)

-- | The coverage features exhibited by one run.
data CovSet = CovSet
  { csEdges :: !(Set SiteId)
  , csStates :: !(Set StateKey)
  , csRare :: !(Map StateKey Int)
  -- ^ log-template fingerprint → occurrences in this run
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | No coverage.
emptyCov :: CovSet
emptyCov = CovSet Set.empty Set.empty Map.empty

-- | Fingerprint a log line's /template/ (numeric runs collapsed to @#@) so that
-- structurally identical messages with different numbers count as one feature.
logTemplateKey :: Text -> StateKey
logTemplateKey = mixKey . map (fromIntegral . fromEnum) . collapse . T.unpack
  where
    -- collapse each maximal run of digits to a single '#'
    collapse [] = []
    collapse (c : cs)
      | isDigit c = '#' : collapse (dropWhile isDigit cs)
      | otherwise = c : collapse cs

-- | Distil the coverage features from an event trace.
covFromEvents :: [SimEvent] -> CovSet
covFromEvents = foldr step emptyCov
  where
    step ev cs = case ev of
      EvCover s -> cs {csEdges = Set.insert s (csEdges cs)}
      EvObserve k -> cs {csStates = Set.insert k (csStates cs)}
      EvLog _ t -> cs {csRare = Map.insertWith (+) (logTemplateKey t) 1 (csRare cs)}
      _ -> cs

-- | Union two coverage sets (rare-template counts add).
mergeCov :: CovSet -> CovSet -> CovSet
mergeCov a b =
  CovSet
    { csEdges = Set.union (csEdges a) (csEdges b)
    , csStates = Set.union (csStates a) (csStates b)
    , csRare = Map.unionWith (+) (csRare a) (csRare b)
    }

-- A unified feature key across the three families.
data Feature
  = FEdge !SiteId
  | FState !StateKey
  | FRare !StateKey
  deriving stock (Eq, Ord)

features :: CovSet -> [Feature]
features cs =
  map FEdge (Set.toList (csEdges cs))
    ++ map FState (Set.toList (csStates cs))
    ++ map FRare (Map.keys (csRare cs))

-- | Opaque per-feature global visit counts.
newtype GlobalCounts = GlobalCounts (Map Feature Int)

-- | No features seen yet.
noNovelty :: GlobalCounts
noNovelty = GlobalCounts Map.empty

-- | Record that a run exhibited these features (increment each by one).
bumpCounts :: CovSet -> GlobalCounts -> GlobalCounts
bumpCounts cs (GlobalCounts m) =
  GlobalCounts (foldr (\f -> Map.insertWith (+) f 1) m (features cs))

-- | The novelty reward of a run given the global counts: @β@ times the number
-- of features the run exhibits that have never been seen globally. A run whose
-- every feature is already known scores @0@; a run reaching one new state scores
-- @β@. (Rarity gradation across already-seen features is handled by the search's
-- bandit / beacon bonuses, not here — this keeps @novelty@ monotone and makes
-- "fully-seen ⇒ 0" exact.)
novelty :: Double -> GlobalCounts -> CovSet -> Double
novelty beta (GlobalCounts m) cs =
  beta * fromIntegral (length [() | f <- features cs, not (Map.member f m)])
