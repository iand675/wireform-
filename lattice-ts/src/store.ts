/**
 * The normalized client entity store (spec §9.1): a version-keyed entity map
 * patched uniformly by every response and every mutation result, plus a
 * registry of cached query results whose surrogate-key sets drive
 * `invalidated`-record staleness.
 *
 * Merge semantics per entity record:
 *  - incoming `ver` differs from the stored one → the record's fields REPLACE
 *    the stored fields (fields it does not carry are unknown at the new
 *    version and are dropped);
 *  - same `ver` → fields UNION (both responses are facts about one version);
 *  - `tombstone` evicts; `unchanged` and `elided` are no-ops (§9.3, §10.4).
 */

import type {
  EndRecord,
  ErrorRecord,
  FieldValue,
  LatticeRecord,
  ManifestRecord,
  Ref,
} from "./wire.ts";

export interface EntityState {
  readonly ver: string;
  readonly fields: Readonly<Record<string, FieldValue>>;
}

/** One cached query result, keyed by (canonical text, canonical variables). */
export interface QueryResultEntry {
  /** Result order per root name, from the response manifest. */
  readonly rootOrder: Readonly<Record<string, readonly Ref[]>>;
  readonly etag?: string;
  /**
   * The keys this result depends on: the response's `Surrogate-Key` header
   * when readable, plus every entity ref the response carried. Intersected
   * against `invalidated` record keys to decide staleness.
   */
  readonly keys: ReadonlySet<string>;
  readonly stale: boolean;
}

/** Accumulated per-response outcome of applying a record stream. */
export interface ApplyOutcome {
  manifest?: ManifestRecord;
  end?: EndRecord;
  errors: ErrorRecord[];
  /** Union of `invalidated` record keys seen. */
  invalidated: string[];
  /** Every entity ref the stream carried (entity, tombstone, unchanged, elided). */
  refs: Ref[];
  /**
   * §9.4.3 commit test: the response contained at least one `entity`,
   * `tombstone`, or `invalidated` record.
   */
  committed: boolean;
}

export function newApplyOutcome(): ApplyOutcome {
  return { errors: [], invalidated: [], refs: [], committed: false };
}

function deepEqual(a: unknown, b: unknown): boolean {
  if (Object.is(a, b)) return true;
  if (typeof a !== "object" || typeof b !== "object" || a === null || b === null) return false;
  const aArr = Array.isArray(a);
  if (aArr !== Array.isArray(b)) return false;
  if (aArr) {
    const x = a as unknown[];
    const y = b as unknown[];
    if (x.length !== y.length) return false;
    for (let i = 0; i < x.length; i++) {
      if (!deepEqual(x[i], y[i])) return false;
    }
    return true;
  }
  const xo = a as Record<string, unknown>;
  const yo = b as Record<string, unknown>;
  const keys = Object.keys(xo);
  if (keys.length !== Object.keys(yo).length) return false;
  for (const k of keys) {
    if (!deepEqual(xo[k], yo[k])) return false;
  }
  return true;
}

export class LatticeStore {
  private readonly entities = new Map<Ref, EntityState>();
  /** Monotonic per-ref change counters backing `refsVersion` snapshots. */
  private readonly refCounters = new Map<Ref, number>();
  private readonly refSubs = new Map<Ref, Set<() => void>>();
  private readonly results = new Map<string, QueryResultEntry>();
  private readonly resultSubs = new Map<string, Set<() => void>>();
  private readonly pendingNotify = new Set<() => void>();
  /** Monotonic store-wide version; bumps on every observable change. */
  version = 0;

  // -- entities -------------------------------------------------------------

  get(ref: Ref): EntityState | undefined {
    return this.entities.get(ref);
  }

  /** Every ref currently in the store (diagnostics / devtools). */
  refs(): IterableIterator<Ref> {
    return this.entities.keys();
  }

  private touch(ref: Ref): void {
    this.version++;
    this.refCounters.set(ref, (this.refCounters.get(ref) ?? 0) + 1);
    const subs = this.refSubs.get(ref);
    if (subs) {
      for (const cb of subs) this.pendingNotify.add(cb);
    }
  }

  /**
   * Apply one wire record. Entity/tombstone/unchanged/elided records patch
   * the entity map; `invalidated` records mark intersecting cached query
   * results stale; manifest/end/error records only accumulate into `outcome`.
   * Notifications are queued; call `flush()` (or use `applyRecords`) when a
   * batch is done.
   */
  applyRecord(rec: LatticeRecord, outcome?: ApplyOutcome): void {
    switch (rec.kind) {
      case "manifest":
        if (outcome) outcome.manifest = rec;
        break;
      case "entity": {
        if (outcome) {
          outcome.refs.push(rec.id);
          outcome.committed = true;
        }
        const prev = this.entities.get(rec.id);
        if (prev && prev.ver === rec.ver) {
          let changed = false;
          for (const [k, v] of Object.entries(rec.fields)) {
            if (!deepEqual(prev.fields[k], v)) {
              changed = true;
              break;
            }
          }
          if (!changed) break;
          this.entities.set(rec.id, { ver: rec.ver, fields: { ...prev.fields, ...rec.fields } });
        } else {
          this.entities.set(rec.id, { ver: rec.ver, fields: { ...rec.fields } });
        }
        this.touch(rec.id);
        break;
      }
      case "tombstone":
        if (outcome) {
          outcome.refs.push(rec.id);
          outcome.committed = true;
        }
        if (this.entities.delete(rec.id)) this.touch(rec.id);
        break;
      case "unchanged":
        if (outcome) outcome.refs.push(rec.id);
        break;
      case "elided":
        // The entity exists but this context may not see it; never treat as
        // nonexistence (§9.3), so the store keeps whatever it already holds.
        if (outcome) outcome.refs.push(rec.id);
        break;
      case "error":
        if (outcome) outcome.errors.push(rec);
        break;
      case "invalidated":
        if (outcome) {
          outcome.invalidated.push(...rec.keys);
          outcome.committed = true;
        }
        this.markStaleByKeys(rec.keys);
        break;
      case "end":
        if (outcome) outcome.end = rec;
        break;
      case "plan":
      case "progress":
      case "unknown":
        break; // tolerated, nothing to store
    }
  }

  /** Apply a batch of records, flush notifications, and return the outcome. */
  applyRecords(records: Iterable<LatticeRecord>, outcome: ApplyOutcome = newApplyOutcome()): ApplyOutcome {
    for (const rec of records) this.applyRecord(rec, outcome);
    this.flush();
    return outcome;
  }

  /** Deliver queued change notifications (each subscriber at most once). */
  flush(): void {
    if (this.pendingNotify.size === 0) return;
    const cbs = [...this.pendingNotify];
    this.pendingNotify.clear();
    for (const cb of cbs) cb();
  }

  // -- subscriptions ----------------------------------------------------------

  /** Subscribe to changes of any of `refs`. Returns the unsubscribe function. */
  subscribe(refs: Iterable<Ref>, cb: () => void): () => void {
    const mine = [...refs];
    for (const ref of mine) {
      let subs = this.refSubs.get(ref);
      if (!subs) {
        subs = new Set();
        this.refSubs.set(ref, subs);
      }
      subs.add(cb);
    }
    return () => {
      for (const ref of mine) {
        const subs = this.refSubs.get(ref);
        if (subs) {
          subs.delete(cb);
          if (subs.size === 0) this.refSubs.delete(ref);
        }
      }
    };
  }

  /**
   * A monotonic version over a ref set: increases exactly when one of the
   * refs changes. Suitable as a `useSyncExternalStore` snapshot.
   */
  refsVersion(refs: Iterable<Ref>): number {
    let sum = 0;
    for (const ref of refs) sum += this.refCounters.get(ref) ?? 0;
    return sum;
  }

  // -- query-result registry --------------------------------------------------

  /** Record (or refresh) a cached query result. Clears staleness. */
  registerResult(key: string, entry: Omit<QueryResultEntry, "stale">): void {
    this.results.set(key, { ...entry, stale: false });
    this.version++;
    this.notifyResult(key);
  }

  getResult(key: string): QueryResultEntry | undefined {
    return this.results.get(key);
  }

  /** Drop a cached result entirely (e.g. when its watcher is disposed). */
  dropResult(key: string): void {
    this.results.delete(key);
  }

  /** Mark one cached result stale and notify its watchers. */
  markStale(key: string): void {
    const entry = this.results.get(key);
    if (!entry || entry.stale) return;
    this.results.set(key, { ...entry, stale: true });
    this.version++;
    this.notifyResult(key);
  }

  /**
   * Mark every cached query result whose key set intersects `keys` as stale
   * (the client half of the `invalidated` record contract, §11.3).
   */
  markStaleByKeys(keys: Iterable<string>): void {
    const incoming = [...keys];
    if (incoming.length === 0) return;
    for (const [key, entry] of this.results) {
      if (entry.stale) continue;
      if (incoming.some((k) => entry.keys.has(k))) {
        this.results.set(key, { ...entry, stale: true });
        this.version++;
        this.notifyResult(key);
      }
    }
  }

  /** Subscribe to registry changes (registration or staleness) of one result. */
  subscribeResult(key: string, cb: () => void): () => void {
    let subs = this.resultSubs.get(key);
    if (!subs) {
      subs = new Set();
      this.resultSubs.set(key, subs);
    }
    subs.add(cb);
    return () => {
      const cur = this.resultSubs.get(key);
      if (cur) {
        cur.delete(cb);
        if (cur.size === 0) this.resultSubs.delete(key);
      }
    };
  }

  private notifyResult(key: string): void {
    const subs = this.resultSubs.get(key);
    if (subs) {
      for (const cb of [...subs]) cb();
    }
  }
}
