---
title: Suspendable parsers with delimited continuations
description: "How wireform-core runs one parser type at flatparse speed on whole inputs and suspends for streaming, by delegating only input suspension to GHC's delimited-continuation primops."
sidebar:
  order: 3
---

The byte parser in [`wireform-core`](../../packages/core/) (`Wireform.Parser`) has two goals that usually conflict:

1. Whole-input parsing at flatparse speed: a flat pointer loop over a contiguous buffer, unboxed-sum result, no allocation on the hot path.
2. Streaming from the same parser: feed a socket, TLS stream, io_uring queue, or chunked producer, and the parser pauses when it runs out of bytes and resumes when more arrive, with no refill plumbing written by hand.

The approach: keep the parser a straight-line pointer loop, and delegate *only* the suspension to GHC's delimited-continuation primops (`newPromptTag#`, `prompt#`, and `control0#`), added to GHC by [Alexis King](https://github.com/lexi-lambda) (GHC Proposal 313, shipped in GHC 9.6).

## The trade to avoid

A flatparse-style parser is fast because it assumes the whole input is in memory. Each primitive checks a pointer against the buffer end; either it succeeds and returns an advanced pointer, or it fails. There is no way to "wait for more input."

An attoparsec-style streaming parser solves that by making every primitive able to return a `Partial` continuation. When input runs out, the parser hands back a function: "give me the next chunk and I'll keep going." The cost is paid on every primitive: the result type is boxed, the monad threads refill logic, and the continuation is a heap object. Streaming works, but the hot path is no longer a bare pointer loop.

wireform keeps flatparse's exact representation. Suspension is a non-event on the fast path. It only fires at a genuine buffer-empty boundary, and even then it uses no manual CPS transform.

## The parser representation

The parser is a function over raw addresses threading `State#`, returning an unboxed sum identical in shape to flatparse:

```haskell
newtype Parser (m :: Type) e a = Parser
  { runParser#
      :: ParserEnv
      -> Addr#            -- eob (end of buffer)
      -> Addr#            -- cur (current position)
      -> State# RealWorld
      -> StRes# e a       -- (# State#, (# (# a, Addr# #) | (# #) | (# e #) #) #)
  }
```

`Res#` is an unboxed sum: `OK# a cur`, `Fail#`, `Err# e`. The mode parameter `m` is phantom (`Pure` for whole-input or `Stream` for suspendable) and only selects, at compile time, what happens on a bounds-check failure.

## The fast path

Every primitive that needs `n` bytes goes through `ensureN#`. The common case is a single register comparison:

```haskell
ensureN# :: forall m e. ParserMode m => Int# -> Parser m e ()
ensureN# n# = Parser \env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> (# st, OK# () s #)            -- compare and continue
    _  -> case readEnd# env st of        -- re-read the live end pointer
      (# st', eob' #) -> case n# <=# minusAddr# eob' s of
        1# -> (# st', OK# () s #)
        _  -> onEnsureFail @m env eob' s n# st'   -- mode dispatch
```

The failure case is resolved by the `ParserMode` instance, specialized at compile time:

```haskell
instance ParserMode Pure where
  onEnsureFail _ _ _ _ st = (# st, Fail# #)   -- flatparse path

instance ParserMode Stream where
  onEnsureFail = ensureNSlow                  -- NOINLINE, never on the hot path
```

In `Pure` mode, no suspension code exists in the generated output at all; `parseByteString` is bit-identical to flatparse. In `Stream` mode, the slow path is `ensureNSlow`, marked `{-# NOINLINE #-}` so it never bloats the inner loop. The delimited-continuation primops appear only there, at a genuine buffer-empty boundary.

## Suspending with `control0#` / `prompt#`

Rather than CPS-transform the parser, the streaming slow path captures the parser's continuation natively using the `control0#`/`prompt#` pair and yields control to a driver:

```haskell
-- inside ensureNSlow, once we know more bytes are genuinely needed:
control0# tag
  (\k st1 ->
     let resume = Resume
           { resumeContinue = \(Ptr newCur) (Ptr newEnd) -> IO \st2 ->
               let st3 = writeEnd#    env newEnd st2
                   st4 = writeAnchor# env pos    newCur st3
               in prompt# tag (\st5 -> k (\st6 -> (# st6, OK# () newCur #)) st5) st4
           , resumeEof = IO \st2 ->
               prompt# tag (\st3 -> k (\st4 -> (# st4, Fail# #)) st3) st2
           }
     in (# st1, StepSuspend pos needed resume #))
  st0'
```

Mechanics:

- **`newPromptTag#`** creates a typed marker. The driver installs a matching `prompt#` frame around the parse before it starts.
- When the buffer empties, **`control0#`** captures the stack slice between the current point and that `prompt#` frame as a one-shot continuation `k`, aborts up to the driver, and hands it a `StepSuspend` carrying a `Resume`.
- The driver waits for the transport to deliver more bytes, then calls `resumeContinue` with the fresh `cur`/`eob`. That re-enters the captured continuation under a new `prompt#`, and `k` drives the original bounds check to return `OK# () newCur`.

For the parser author, `ensureN#` blocked and then succeeded. No `Partial` constructor, no boxed continuation per primitive, no CPS in the grammar. The RTS captures the continuation the way a hand-written CPS transform would, but without smearing it across every combinator.

## The driver/parser protocol

The two sides communicate through two types:

```haskell
data Step e r
  = StepDone       !Word64 r
  | StepFail       !Word64
  | StepErr        !Word64 e
  | StepSuspend    !Word64 !Int !(Resume e r)   -- pos, bytes needed
  | StepCheckpoint !Word64 !(Resume e r)

data Resume e r = Resume
  { resumeContinue :: !(Ptr Word8 -> Ptr Word8 -> IO (Step e r))  -- newCur, newEnd
  , resumeEof      :: !(IO (Step e r))
  }
```

`runParserInternal` installs the `prompt#` frame once and trampolines on the returned `Step`:

- `StepSuspend`: park in `waitUntilAvailable` until the ring holds `needed` bytes, then call `resumeContinue`
- `StepCheckpoint`: advance the transport tail, then call `resumeContinue` (see below)
- end-of-input: call `resumeEof`, which re-enters the continuation with `Fail#`
- `StepDone`/`StepFail`/`StepErr`: return

The loop is a tail-recursive `go :: Step -> IO …`. All the machinery is in the suspend/resume pair.

| Module | Piece |
|--------|-------|
| `Wireform.Parser.Internal` | `ensureN#` / `ensureNSlow`: bounds check; only place `control0#` fires |
| `Wireform.Parser.Internal` | `ParserMode`: compile-time dispatch (`Pure` → `Fail#`, `Stream` → suspend) |
| `Wireform.Parser.Driver` | `runParserInternal`: installs `prompt#` frame, runs trampoline |
| `Wireform.Parser.Driver` | `parseByteString`: `Pure` path, flatparse-equivalent, no driver |
| both | `Step` / `Resume`: the suspension hand-off contract |

## Why no locks or fences

The driver and the suspended parser share three mutable cells (the live end pointer plus an `(anchorPos, anchorCur)` pair used to recover the logical byte offset when `cur` wraps around the [double-mapped ring buffer](../../packages/network/)). These cells are written with **no lock and no memory fence**, and that is sound.

Between `control0#` reifying the continuation and `prompt#` re-entering it, the parser is not executing at all. The driver does all cell updates inside that window. The parser cannot observe a half-updated cell, and the stores are sequenced before the `prompt#` that resumes it. Suspension itself is the synchronization mechanism. (The transport contract requires the producer and a given parse run to share a thread, so there is no cross-thread race.)

## Checkpoint

`checkpoint` uses the same `control0#`/`prompt#` mechanism for a different purpose: advancing the transport's tail (and freeing ring space) while the parse is still running. Without this, a read larger than the ring would deadlock: the parser can't make progress because the ring is full, and the producer can't write more because the consumer hasn't advanced its tail. `checkpoint` in the parser yields `StepCheckpoint`; the driver advances the tail, wraps `cur` back into the first mapping, and calls `resumeContinue`.

## Consequences

- **One grammar, every input source.** The same `Parser` runs against a whole `ByteString` (`Pure`), a live socket/TLS/io_uring transport (`Stream`), or an existing attoparsec/binary/cereal parser via the [parser adapters](../../packages/parser-adapters/).
- **Zero streaming cost on the fast path.** Whole-input parsing is flatparse; the primops never appear in that code.
- **No hand-written CPS.** Backtracking (`<|>`) is a plain branch reset, not a continuation. Delimited continuations handle only *input* suspension, so the two compose without interference.
- **Mid-parse checkpointing.** Same mechanism, different purpose: free ring space during a parse.

## GHC version

The delimited-continuation primops require **GHC ≥ 9.6** (wireform CI tests 9.6 and 9.8). The implementation uses `MagicHash`, `UnboxedSums`, `UnboxedTuples`, and `RankNTypes`. See `Wireform.Parser.Internal` and `Wireform.Parser.Driver` for the full source with extensive comments.
