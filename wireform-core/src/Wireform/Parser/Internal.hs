{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Core parser types, mirroring flatparse's representation for
identical inner-loop performance.

The parser threads @Addr#@ pointers and @State# RealWorld@ directly
(no IO wrapper, no boxed Ptr).  The result is an unboxed sum
matching flatparse's @Res#@.

Streaming suspension via @control0#@ is confined to 'ensureNSlow',
which is @NOINLINE@ and never fires on the hot path for whole-input
parsing.
-}
module Wireform.Parser.Internal (
  -- * Parser type
  Parser (..),

  -- * Mode types and class
  Pure,
  Stream,
  ParserMode (..),

  -- * Result types
  type Res#,
  type StRes#,
  pattern OK#,
  pattern Fail#,
  pattern Err#,

  -- * Step / Resume (driver protocol)
  Step (..),
  Resume (..),

  -- * Parser environment
  ParserEnv (..),
  curToPos,
  curToPosIO,

  -- * Suspension / checkpoint primitives
  ensureN#,
  ensureNSlow,
  checkpoint,

  -- * Tag storage
  tagToAny,
  pePromptTag,

  -- * End-pointer access
  readEnd#,
  writeEnd#,
  readEnd,
  writeEnd,

  -- * Anchor access
  writeAnchor#,
  writeAnchor,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Data.Kind (Type)
import Data.Word (Word64, Word8)
import GHC.Exts
import GHC.ForeignPtr (ForeignPtrContents (..))
import GHC.IO (IO (..))
import GHC.Word (Word64 (..))


------------------------------------------------------------------------
-- Result types — unboxed, identical to flatparse
------------------------------------------------------------------------

{- | Parser result. We drop the State# token from the result type
since it's threaded implicitly by IO.  The parser function signature
still takes State# and must thread it, but Res# is just the
unboxed sum of outcomes.
-}
type Res# e a =
  (#
    (# a, Addr# #) |
    (# #) |
    (# e #)
  #)


pattern OK# :: a -> Addr# -> Res# e a
pattern OK# a s = (# (# a, s #) | | #)


pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)


pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)


{-# COMPLETE OK#, Fail#, Err# #-}


------------------------------------------------------------------------
-- Parser mode (compile-time dispatch for ensure slow path)
------------------------------------------------------------------------

{- | Whole-input mode.  When bounds check fails, just return 'Fail#'.
Identical codegen to flatparse — zero suspension overhead.
-}
data Pure


{- | Streaming mode.  When bounds check fails, suspend via @control0#@
to wait for more data from the transport.
-}
data Stream


{- | Type class dispatching the ensure slow path.
Resolved at compile time via specialization.
-}
class ParserMode (m :: Type) where
  {- | Refresh the end of a streaming window. Fixed input retains the
  current bound, including when parsing a subspan.
  -}
  refreshEnd# :: ParserEnv -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
  refreshEnd# env end st
    | wholeInput env = (# st, end #)
    | otherwise = readEnd# env st
  {-# INLINE refreshEnd# #-}


  onEnsureFail
    :: ParserEnv
    -> Addr#
    -> Addr#
    -> Int#
    -> State# RealWorld
    -> StRes# e ()


  {- | Mode-polymorphic checkpoint.  In streaming mode, advances the
  transport's tail to the parser's current position (freeing ring
  space behind it).  In whole-input mode, a no-op — there is no
  ring to refill.

  Used by 'Wireform.Parser.takeBs' / 'takeBsCopy' to drain reads
  larger than the ring without deadlocking.
  -}
  modeCheckpoint :: Parser m e ()


instance ParserMode Pure where
  refreshEnd# _ end st = (# st, end #)
  {-# INLINE refreshEnd# #-}


  onEnsureFail _env _eob _s _n st = (# st, Fail# #)
  {-# INLINE onEnsureFail #-}
  modeCheckpoint = Parser \_env _eob s st -> (# st, OK# () s #)
  {-# INLINE modeCheckpoint #-}


instance ParserMode Stream where
  onEnsureFail = ensureNSlow
  {-# INLINE onEnsureFail #-}
  modeCheckpoint = checkpoint
  {-# INLINE modeCheckpoint #-}


------------------------------------------------------------------------
-- Parser environment (carries mutable end pointer for streaming)
------------------------------------------------------------------------

{- | Shared context for a parse run.

Three mutable cells (pointer-to-Ptr fields) carry state that the
driver updates between parser resumptions:

  * 'peEndPtr'    — the producer's head pointer, refreshed after
                    a 'StepSuspend' / 'StepCheckpoint' round-trip.
  * 'peAnchorPos' — the absolute byte offset of the parser's most
                    recent /anchor/.  Initially the parse run's
                    @startPos@; updated whenever the driver wraps
                    'cur' back into the first mapping (currently
                    on every 'StepCheckpoint' / 'StepSuspend' resume).
  * 'peAnchorCur' — the cur pointer that pairs with 'peAnchorPos'.

Logical position is computed as @anchorPos + (cur - anchorCur)@
(see 'curToPos').  Re-anchoring on cur-resets keeps that identity
correct even when a single parse run drains more bytes than the
ring buffer can hold (the parser's logical 'cur' advances past
@anchorCur + ringSize@, the driver wraps it back, and the anchor
shifts forward by the wrap amount).

== Concurrency model

These cells are mutable but require neither locks nor memory
barriers, because the writer (the driver) and the reader (the
parser) never run at the same time:

  * Parser writes are confined to 'resumeContinue' closures, which
    run /while the parser is suspended/ (between 'control0#'
    reifying the continuation and 'prompt#' re-entering it).
  * The parser cannot observe a cell mid-update because it is not
    executing at all during the update.
  * The update of all three cells happens-before the @prompt#@
    that resumes the parser, by virtue of being sequenced in the
    same @IO@ action.

So the three writes look atomic to the parser even though they are
three separate stores: suspension is the synchronization
primitive.  No GHC fence is needed (single-threaded use; the
transport contract requires that producer and consumer share a
thread for a given parse run).
-}
data ParserEnv = ParserEnv
  { peEndPtr :: {-# UNPACK #-} !(Ptr (Ptr Word8))
  -- ^ Mutable end pointer (single deref to read, updated on resume).
  , peBaseAddr :: {-# UNPACK #-} !(Ptr Word8)
  -- ^ Ring base address.
  , peMask :: {-# UNPACK #-} !Int
  -- ^ @ringSize - 1@ (or 'maxBound' in whole-input mode).
  , peAnchorPos :: {-# UNPACK #-} !(Ptr Word64)
  {- ^ Mutable anchor: absolute byte position corresponding to
  'peAnchorCur'.
  -}
  , peAnchorCur :: {-# UNPACK #-} !(Ptr (Ptr Word8))
  -- ^ Mutable anchor: cur address corresponding to 'peAnchorPos'.
  , peBackingFp :: !ForeignPtrContents
  -- ^ Keeps the backing memory alive for zero-copy slices.
  , peTag :: Any
  {- ^ PromptTag# stored as Any via unsafeCoerce#.
  Only meaningful for streaming mode.
  -}
  }


-- | Recover the typed PromptTag# from a streaming environment.
{-# INLINE pePromptTag #-}
pePromptTag :: forall e r. ParserEnv -> PromptTag# (Step e r)
pePromptTag env = unsafeCoerce# (peTag env)


-- | Store a PromptTag# into an Any-typed field.
{-# INLINE tagToAny #-}
tagToAny :: PromptTag# a -> Any
tagToAny t = unsafeCoerce# t


{- | Convert a cur address to an absolute logical position.  Reads
the mutable anchor cells, so it must be threaded through 'State#'.
-}
{-# INLINE curToPos #-}
curToPos
  :: ParserEnv
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Word64 #)
curToPos env cur s0 =
  let !(Ptr posCell) = peAnchorPos env
      !(Ptr curCell#) = peAnchorCur env
  in case readWord64OffAddr# posCell 0# s0 of
       (# s1, ap# #) ->
         case readAddrOffAddr# curCell# 0# s1 of
           (# s2, ac# #) ->
             let !off = I# (minusAddr# cur ac#)
                 !pos = W64# ap# + fromIntegral off
             in (# s2, pos #)


-- | Boxed wrapper for IO contexts.
curToPosIO :: ParserEnv -> Ptr Word8 -> IO Word64
curToPosIO env (Ptr cur#) = IO \s -> curToPos env cur# s
{-# INLINE curToPosIO #-}


{- | Write a fresh @(anchorPos, anchorCur)@ pair.  Called by the driver
whenever it wraps the parser's cur back into the first mapping
(StepCheckpoint resume, StepSuspend resume).
-}
writeAnchor#
  :: ParserEnv
  -> Word64
  -> Addr#
  -> State# RealWorld
  -> State# RealWorld
writeAnchor# env (W64# pos#) cur# s0 =
  let !(Ptr posCell) = peAnchorPos env
      !(Ptr curCell#) = peAnchorCur env
      !s1 = writeWord64OffAddr# posCell 0# pos# s0
      !s2 = writeAddrOffAddr# curCell# 0# cur# s1
  in s2
{-# INLINE writeAnchor# #-}


writeAnchor :: ParserEnv -> Word64 -> Ptr Word8 -> IO ()
writeAnchor env pos (Ptr cur#) = IO \s -> (# writeAnchor# env pos cur# s, () #)
{-# INLINE writeAnchor #-}


{-# INLINE readEnd# #-}
readEnd# :: ParserEnv -> State# RealWorld -> (# State# RealWorld, Addr# #)
readEnd# env s =
  let !(Ptr p) = peEndPtr env
  in case readAddrOffAddr# p 0# s of
       (# s', a #) -> (# s', a #)


wholeInput :: ParserEnv -> Bool
wholeInput env = peMask env == maxBound
{-# INLINE wholeInput #-}


{-# INLINE writeEnd# #-}
writeEnd# :: ParserEnv -> Addr# -> State# RealWorld -> State# RealWorld
writeEnd# env val s =
  let !(Ptr p) = peEndPtr env
  in writeAddrOffAddr# p 0# val s


-- Boxed wrappers for driver code
readEnd :: ParserEnv -> IO (Ptr Word8)
readEnd env = IO \s -> case readEnd# env s of
  (# s', a #) -> (# s', Ptr a #)
{-# INLINE readEnd #-}


writeEnd :: ParserEnv -> Ptr Word8 -> IO ()
writeEnd env (Ptr a) = IO \s -> (# writeEnd# env a s, () #)
{-# INLINE writeEnd #-}


------------------------------------------------------------------------
-- Step / Resume (driver protocol)
------------------------------------------------------------------------

data Step e r
  = StepDone {-# UNPACK #-} !Word64 r
  | StepFail {-# UNPACK #-} !Word64
  | StepErr {-# UNPACK #-} !Word64 e
  | StepSuspend
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Int
      !(Resume e r)
  | StepCheckpoint {-# UNPACK #-} !Word64 !(Resume e r)


data Resume e r = Resume
  { resumeContinue :: !(Ptr Word8 -> Ptr Word8 -> IO (Step e r))
  , resumeEof :: !(IO (Step e r))
  }


------------------------------------------------------------------------
-- The Parser newtype — flatparse-shaped
------------------------------------------------------------------------

{- | A parser consuming bytes from @(cur, eob)@ pointers.

The representation mirrors flatparse exactly:
@ForeignPtrContents -> Addr# (eob) -> Addr# (cur) -> State# -> Res#@

The parser environment carries a prompt for streaming runs.
The whole-input driver calls the parser directly.
| Parser result with state token.
-}
type StRes# e a = (# State# RealWorld, Res# e a #)


{- | The parser type.  Parameterized by mode @m@ ('Pure' or 'Stream'),
error type @e@, and result type @a@.

Four args on the hot path: env, eob, cur, state.  The mode @m@ is
phantom — it controls which 'ParserMode' instance is used for
bounds-check failures, resolved at compile time.
-}
newtype Parser (m :: Type) e a = Parser
  { runParser#
      :: ParserEnv
      -> Addr# -- eob (end of buffer / current end)
      -> Addr# -- cur (current position)
      -> State# RealWorld
      -> StRes# e a
  }


instance Functor (Parser m e) where
  fmap f (Parser g) = Parser \env eob s st ->
    case g env eob s st of
      (# st', OK# a s' #) -> let !b = f a in (# st', OK# b s' #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE fmap #-}


  a' <$ Parser g = Parser \env eob s st ->
    case g env eob s st of
      (# st', OK# _ s' #) -> (# st', OK# a' s' #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<$) #-}


instance Applicative (Parser m e) where
  pure !a = Parser \_ _ s st -> (# st, OK# a s #)
  {-# INLINE pure #-}


  Parser ff <*> Parser fa = Parser \env eob s st ->
    case ff env eob s st of
      (# st', OK# f s' #) -> case fa env eob s' st' of
        (# st'', OK# a s'' #) -> let !b = f a in (# st'', OK# b s'' #)
        (# st'', x #) -> (# st'', unsafeCoerce# x #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<*>) #-}


  Parser fa *> Parser fb = Parser \env eob s st ->
    case fa env eob s st of
      (# st', OK# _ s' #) -> fb env eob s' st'
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (*>) #-}


  Parser fa <* Parser fb = Parser \env eob s st ->
    case fa env eob s st of
      (# st', OK# a s' #) -> case fb env eob s' st' of
        (# st'', OK# _ s'' #) -> (# st'', OK# a s'' #)
        (# st'', x #) -> (# st'', unsafeCoerce# x #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<*) #-}


instance Monad (Parser m e) where
  Parser fa >>= f = Parser \env eob s st ->
    case fa env eob s st of
      (# st', OK# a s' #) -> runParser# (f a) env eob s' st'
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (>>=) #-}


  (>>) = (*>)
  {-# INLINE (>>) #-}


instance MonadFail (Parser m e) where
  fail _ = Parser \_ _ _ st -> (# st, Fail# #)
  {-# INLINE fail #-}


instance ParserMode m => Alternative (Parser m e) where
  empty = Parser \_ _ _ st -> (# st, Fail# #)
  {-# INLINE empty #-}
  (Parser f) <|> (Parser g) = Parser \env eob s st ->
    case f env eob s st of
      (# st', Fail# #) -> g env eob s st'
      x -> x
  {-# INLINE (<|>) #-}


instance ParserMode m => MonadPlus (Parser m e)


------------------------------------------------------------------------
-- ensureN#: bounds check + suspension
------------------------------------------------------------------------

{- | Require @n#@ bytes available from @cur@.

Fast path: single register comparison (identical to flatparse).
Semi-fast: re-read 'peEndPtr' to catch stale @eob@ after a
prior streaming resume; no function-call overhead.
Slow path: suspends to the driver via @control0#@.
-}
ensureN# :: forall m e. ParserMode m => Int# -> Parser m e ()
ensureN# n# = Parser \env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> (# st, OK# () s #)
    _ -> case refreshEnd# @m env eob st of
      (# st', eob' #) -> case n# <=# minusAddr# eob' s of
        1# -> (# st', OK# () s #)
        _ -> onEnsureFail @m env eob' s n# st'
{-# INLINE ensureN# #-}


{- | Streaming slow path.

Before suspending via @control0#@, re-read the mutable end pointer.
After a prior resume, @>>=@ threads the stale @eob@ that was captured
in its closure, so the fast-path comparison in 'ensureN#' fails even
though new data is already available in the ring.  The re-read here
catches that case and avoids a redundant suspension round-trip.
-}
ensureNSlow
  :: ParserEnv
  -> Addr#
  -> Addr#
  -> Int#
  -> State# RealWorld
  -> StRes# e ()
ensureNSlow env _eob s n# st
  | wholeInput env = (# st, Fail# #)
  | otherwise =
      case readEnd# env st of
        (# st0, eob' #) ->
          case n# <=# minusAddr# eob' s of
            1# -> (# st0, OK# () s #)
            _ ->
              let tag :: PromptTag# (Step Any Any)
                  tag = unsafeCoerce# (peTag env)
              in case curToPos env s st0 of
                   (# st0', pos #) ->
                     let !needed = I# n#
                     in control0#
                          tag
                          ( \k st1 ->
                              let resume =
                                    Resume
                                      { resumeContinue = \(Ptr newCur) (Ptr newEnd) ->
                                          IO \st2 ->
                                            -- The driver always wraps newCur back
                                            -- into the first mapping; re-anchor
                                            -- so 'curToPos' stays correct after
                                            -- the wrap.  Both stores are safe
                                            -- without a fence: the parser is
                                            -- suspended (between this 'control0#'
                                            -- and the 'prompt#' below) while
                                            -- they execute.
                                            let st3 = writeEnd# env newEnd st2
                                                st4 = writeAnchor# env pos newCur st3
                                            in prompt# tag (\st5 -> k (\st6 -> (# st6, OK# () newCur #)) st5) st4
                                      , resumeEof =
                                          IO \st2 -> prompt# tag (\st3 -> k (\st4 -> (# st4, Fail# #)) st3) st2
                                      }
                              in (# st1, StepSuspend pos needed resume #)
                          )
                          st0'
{-# NOINLINE ensureNSlow #-}


------------------------------------------------------------------------
-- checkpoint
------------------------------------------------------------------------

-- | Checkpoint is only meaningful in streaming mode.
checkpoint :: Parser Stream e ()
checkpoint = Parser checkpoint#


checkpoint# :: ParserEnv -> Addr# -> Addr# -> State# RealWorld -> StRes# e ()
checkpoint# env _ s st0
  | wholeInput env = (# st0, Fail# #)
  | otherwise =
      let tag :: PromptTag# (Step Any Any)
          tag = unsafeCoerce# (peTag env)
      in case curToPos env s st0 of
           (# st0', pos #) ->
             control0#
               tag
               ( \k st1 ->
                   let resume =
                         Resume
                           { resumeContinue = \(Ptr newCur) (Ptr newEnd) ->
                               IO \st2 ->
                                 -- Re-anchor: the driver has wrapped newCur
                                 -- back to @base + (pos .&. mask)@ which
                                 -- decouples the cur pointer from the original
                                 -- anchor.  Without re-anchoring, 'curToPos'
                                 -- would compute the wrong absolute position
                                 -- on the next call.
                                 --
                                 -- These two stores (end pointer + anchor
                                 -- pair) are not behind any lock or fence:
                                 -- they run inside the resume body, which is
                                 -- only invoked while the parser is suspended
                                 -- between 'control0#' and the matching
                                 -- 'prompt#' below.  The parser cannot observe
                                 -- a partial update because it is not running
                                 -- at all during the update — suspension is
                                 -- the synchronization mechanism.
                                 let st3 = writeEnd# env newEnd st2
                                     st4 = writeAnchor# env pos newCur st3
                                 in prompt# tag (\st5 -> k (\st6 -> (# st6, OK# () newCur #)) st5) st4
                           , resumeEof =
                               IO \st2 -> prompt# tag (\st3 -> k (\st4 -> (# st4, Fail# #)) st3) st2
                           }
                   in (# st1, StepCheckpoint pos resume #)
               )
               st0'
