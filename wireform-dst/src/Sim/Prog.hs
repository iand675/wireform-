{-# LANGUAGE KindSignatures #-}

-- | The effect algebra: the operational monad 'Prog' and the 'SimOp' GADT of
-- primitive simulator operations, plus thin app-facing wrapper helpers. A
-- workload is a @'Prog' ()@ per node; "Sim.Interp" interprets it against an
-- immutable 'Sim.World.World'.
--
-- 'Prog' is a free-monad-ish reification (@'Pure'@ / @'Bind'@) so the
-- interpreter can grab a fiber's continuation at any yield point (@Recv@ /
-- @Delay@ / @Yield@) and stash it in the 'Sim.World.World'. That
-- resumable-continuation property is what makes O(1) snapshot/branch possible.
--
-- 'SimVar' is a typed handle into a per-node 'Data.Dynamic' heap; the
-- 'Typeable' constraints on the var ops let the interpreter store/retrieve
-- without @unsafeCoerce@.
module Sim.Prog
  ( -- * The monad
    SimVar (..)
  , Prog (..)
  , SimOp (..)

    -- * Wrappers
  , fork
  , yield
  , delay
  , getTime
  , whoAmI
  , drawWord
  , send
  , recv
  , newVar
  , readVar
  , writeVar
  , modifyVar
  , storeWrite
  , storeRead
  , fsync
  , cover
  , observe
  , buggify
  , assertOp
  , logMsg
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Sim.Types (AssertKind, FiberId, Key, NodeId, SimTime, SiteId)

-- | A typed handle into a node's 'Data.Dynamic' heap (a mutable cell scoped to
-- the node; volatile — lost on 'Sim.Types.CrashNode').
newtype SimVar a = SimVar Int
  deriving stock (Eq, Show)

-- | The reified operational monad. @'Bind' op k@ pairs a primitive with its
-- continuation so the interpreter can suspend at a yield point and resume.
data Prog a where
  Pure :: a -> Prog a
  Bind :: SimOp b -> (b -> Prog a) -> Prog a

instance Functor Prog where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind op k) = Bind op (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative Prog where
  pure = Pure
  {-# INLINE pure #-}
  Pure f <*> x = fmap f x
  Bind op k <*> x = Bind op (\b -> k b <*> x)
  {-# INLINE (<*>) #-}

instance Monad Prog where
  Pure a >>= f = f a
  Bind op k >>= f = Bind op (\b -> k b >>= f)
  {-# INLINE (>>=) #-}

-- | The primitive simulator operations. Each maps 1:1 to a hypothetical @IO@
-- action (so a real interpreter is a future additive extension), but only the
-- pure interpreter in "Sim.Interp" ships in v1.
--
-- Block/yield ops — @'Recv'@, @'Delay'@, @'Yield'@ — suspend the fiber. Every
-- other op is /inline/: it executes against the 'Sim.World.World' and continues
-- the same fiber without yielding (@'Send'@ is non-blocking; @'Fork'@ registers
-- a runnable child and continues the parent).
data SimOp (a :: Type) where
  Fork :: Prog () -> SimOp FiberId
  Yield :: SimOp ()
  Delay :: SimTime -> SimOp ()
  GetTime :: SimOp SimTime
  WhoAmI :: SimOp NodeId
  DrawWord :: SimOp Word64
  Send :: NodeId -> ByteString -> SimOp ()
  Recv :: SimOp (NodeId, ByteString)
  NewVar :: Typeable a => a -> SimOp (SimVar a)
  ReadVar :: Typeable a => SimVar a -> SimOp a
  WriteVar :: Typeable a => SimVar a -> a -> SimOp ()
  ModifyVar :: Typeable a => SimVar a -> (a -> (a, b)) -> SimOp b
  StoreWrite :: Key -> ByteString -> SimOp ()
  StoreRead :: Key -> SimOp (Maybe ByteString)
  Fsync :: SimOp ()
  Cover :: SiteId -> SimOp ()
  Observe :: [Word64] -> SimOp ()
  Buggify :: SiteId -> Double -> SimOp Bool
  AssertOp :: AssertKind -> SiteId -> Bool -> SimOp ()
  LogMsg :: Text -> SimOp ()

-- * Wrappers ---------------------------------------------------------------

-- | Spawn a child fiber on the current node; returns its id, continues parent.
fork :: Prog () -> Prog FiberId
fork p = Bind (Fork p) Pure
{-# INLINE fork #-}

-- | Cooperatively reschedule.
yield :: Prog ()
yield = Bind Yield Pure
{-# INLINE yield #-}

-- | Block this fiber until @t@ virtual ns have elapsed.
delay :: SimTime -> Prog ()
delay t = Bind (Delay t) Pure
{-# INLINE delay #-}

-- | The current virtual clock (adjusted by this node's clock skew).
getTime :: Prog SimTime
getTime = Bind GetTime Pure
{-# INLINE getTime #-}

-- | The node this fiber runs on.
whoAmI :: Prog NodeId
whoAmI = Bind WhoAmI Pure
{-# INLINE whoAmI #-}

-- | Draw a deterministic 'Word64' from the world PRNG.
drawWord :: Prog Word64
drawWord = Bind DrawWord Pure
{-# INLINE drawWord #-}

-- | Enqueue a message to @dst@ (non-blocking; latency/partition applied by the
-- interpreter).
send :: NodeId -> ByteString -> Prog ()
send dst body = Bind (Send dst body) Pure
{-# INLINE send #-}

-- | Block until a message is delivered to this fiber; returns @(from, body)@.
recv :: Prog (NodeId, ByteString)
recv = Bind Recv Pure
{-# INLINE recv #-}

-- | Allocate a volatile per-node cell.
newVar :: Typeable a => a -> Prog (SimVar a)
newVar a = Bind (NewVar a) Pure
{-# INLINE newVar #-}

-- | Read a cell.
readVar :: Typeable a => SimVar a -> Prog a
readVar v = Bind (ReadVar v) Pure
{-# INLINE readVar #-}

-- | Overwrite a cell.
writeVar :: Typeable a => SimVar a -> a -> Prog ()
writeVar v a = Bind (WriteVar v a) Pure
{-# INLINE writeVar #-}

-- | Atomically transform a cell, returning a projected result.
modifyVar :: Typeable a => SimVar a -> (a -> (a, b)) -> Prog b
modifyVar v f = Bind (ModifyVar v f) Pure
{-# INLINE modifyVar #-}

-- | Write to durable (or, if a 'Sim.Types.TearWrite' is armed, volatile)
-- storage on this node.
storeWrite :: Key -> ByteString -> Prog ()
storeWrite k b = Bind (StoreWrite k b) Pure
{-# INLINE storeWrite #-}

-- | Read from storage (volatile shadows durable).
storeRead :: Key -> Prog (Maybe ByteString)
storeRead k = Bind (StoreRead k) Pure
{-# INLINE storeRead #-}

-- | Promote this node's pending volatile writes to durable.
fsync :: Prog ()
fsync = Bind Fsync Pure
{-# INLINE fsync #-}

-- | Record a coverage edge.
cover :: SiteId -> Prog ()
cover s = Bind (Cover s) Pure
{-# INLINE cover #-}

-- | Fingerprint an observation (via 'Sim.Entropy.mixKey' at emit).
observe :: [Word64] -> Prog ()
observe ws = Bind (Observe ws) Pure
{-# INLINE observe #-}

-- | Query whether a buggify site is enabled this run (drawn once per site at
-- probability @p@, memoized, overridable by search).
buggify :: SiteId -> Double -> Prog Bool
buggify s p = Bind (Buggify s p) Pure
{-# INLINE buggify #-}

-- | Emit an assertion event (use the typed wrappers in "Sim.Assert" instead).
assertOp :: AssertKind -> SiteId -> Bool -> Prog ()
assertOp k s b = Bind (AssertOp k s b) Pure
{-# INLINE assertOp #-}

-- | Emit a log line (also feeds the rare-template coverage feature).
logMsg :: Text -> Prog ()
logMsg t = Bind (LogMsg t) Pure
{-# INLINE logMsg #-}
