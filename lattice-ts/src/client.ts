/**
 * The Lattice HTTP client: transport ladder (spec §6), streaming NDJSON into
 * the normalized store (§9), denormalization back into the tree shape
 * components expect, microtask-batched query merging (§4.3), and mutations
 * with `invalidated`-driven staleness (§11.3).
 *
 * Transport ladder, per canonical text:
 *   1. hash-form GET once the steady-state URL is known (§6.1),
 *   2. inline GET with the deflated text in `d=` when `CompressionStream`
 *      exists and the compressed text fits the URL budget (§6.2),
 *   3. POST `?intent=introduce` (§6.4).
 * The client NEVER computes BLAKE3: the introduction handshake teaches it its
 * hash-form URL via `Location`/`Content-Location` and `Lattice-Plan` (§6.3).
 */

import type { Argument, FieldSel, QueryDoc, Selection } from "./canonical.ts";
import {
  LatticeQueryError,
  canonicalFieldKey,
  canonicalJson,
  canonicalize,
  expandLocalFragments,
  valueToJson,
} from "./canonical.ts";
import { UnmergeableError, mergeQueries } from "./merge.ts";
import type { ApplyOutcome, QueryResultEntry } from "./store.ts";
import { LatticeStore, newApplyOutcome } from "./store.ts";
import type {
  EntityTree,
  ErrorRecord,
  FieldValue,
  JsonObject,
  ManifestRecord,
  PageTree,
  QueryData,
  Ref,
} from "./wire.ts";
import {
  HEADERS,
  QUERY_MEDIA_TYPE,
  RESERVED_PARAMS,
  isPageValue,
  isRef,
  isRefValue,
  parseRef,
  readRecords,
} from "./wire.ts";

// ---------------------------------------------------------------------------
// Options and result types

export type Vars = Readonly<Record<string, unknown>>;

export type SliceName = "pub" | "ctx" | "priv";

export type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/** Emitted for every request the client issues (debug/telemetry hook). */
export interface LatticeRequestEvent {
  readonly kind: "hash" | "inline" | "introduce" | "mutation";
  readonly method: string;
  readonly url: string;
  readonly canonicalText?: string;
  /** How many consumer documents were merged into this request (absent = 1). */
  readonly merged?: number;
}

export interface LatticeClientOptions {
  /** Origin base, e.g. `"https://api.example.com"` (no trailing slash needed). */
  readonly base: string;
  readonly fetch?: FetchLike;
  /**
   * Visibility claims (§8.2): sent as `vc=base64url(canonicalJson(claims))`
   * on ctx-slice reads. Presence flips the default slice to `ctx`.
   */
  readonly claims?: Readonly<Record<string, unknown>>;
  /** The `{exp}.{sig}` proof for the claims payload, sent as `X-Vc-Auth`. */
  readonly vcAuth?: string;
  /** Slice to request; defaults to `ctx` when claims are set, else `pub`. */
  readonly slice?: SliceName;
  /** Share one store between clients, or inject a prepared one. */
  readonly store?: LatticeStore;
  readonly onRequest?: (event: LatticeRequestEvent) => void;
  /** Max length of the `d=` parameter before falling off the inline rung (~6KB, §6.2). */
  readonly inlineUrlBudget?: number;
}

export interface QueryResult<T = QueryData> {
  readonly data: T;
  readonly manifest?: ManifestRecord;
  readonly errors: readonly ErrorRecord[];
  /** §9.4.4: did the origin finish attempting the full plan? */
  readonly complete: boolean;
  /** Every ref the denormalized `data` was assembled from (drives watches). */
  readonly refs: ReadonlySet<Ref>;
}

export interface MutationResult {
  readonly manifest?: ManifestRecord;
  readonly errors: readonly ErrorRecord[];
  readonly complete: boolean;
  /**
   * §9.4.3 commit test: at least one `entity`, `tombstone`, or `invalidated`
   * record arrived. Presence of a non-error record is proof of commit, and
   * nothing else is.
   */
  readonly committed: boolean;
  /** The purge-set mirror the response carried. */
  readonly invalidatedKeys: readonly string[];
  /** Result refs, from the manifest's root map (usually `root.result`). */
  readonly refs: readonly Ref[];
}

export interface MutateOptions {
  readonly idempotencyKey?: string;
}

export interface QueryOptions {
  /** Force revalidation through shared caches (post-mutation reads, §11.6). */
  readonly noCache?: boolean;
}

/** RFC 9457 problem details, as far as the client understands them. */
export interface ProblemDetails {
  readonly type?: string;
  readonly title?: string;
  readonly status?: number;
  readonly detail?: string;
  readonly [k: string]: unknown;
}

export class LatticeHttpError extends Error {
  constructor(
    readonly status: number,
    readonly problem: ProblemDetails,
    readonly url: string,
  ) {
    super(
      `Lattice request failed: ${status}${problem.type ? ` ${problem.type}` : ""}${
        problem.detail ? ` — ${problem.detail}` : problem.title ? ` — ${problem.title}` : ""
      }`,
    );
    this.name = "LatticeHttpError";
  }
}

// ---------------------------------------------------------------------------
// Small codecs

function b64url(bytes: Uint8Array): string {
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]!);
  return btoa(bin).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

async function deflateRaw(text: string): Promise<Uint8Array> {
  const cs = new CompressionStream("deflate-raw");
  const stream = new Blob([text]).stream().pipeThrough(cs);
  return new Uint8Array(await new Response(stream).arrayBuffer());
}

async function problemOf(resp: Response, url: string): Promise<LatticeHttpError> {
  let problem: ProblemDetails = {};
  try {
    const body = (await resp.json()) as unknown;
    if (typeof body === "object" && body !== null) problem = body as ProblemDetails;
  } catch {
    // Non-JSON error body; status is all we have.
  }
  return new LatticeHttpError(resp.status, problem, url);
}

// ---------------------------------------------------------------------------
// Denormalization

function bindVariables(doc: QueryDoc<unknown>, vars: Vars): Record<string, unknown> {
  const bound: Record<string, unknown> = {};
  for (const v of doc.variables) {
    const given = vars[v.name];
    if (given !== undefined) bound[v.name] = given;
    else if (v.default !== undefined) bound[v.name] = valueToJson(v.default, {});
  }
  return bound;
}

function fieldKeyOf(field: FieldSel, bound: Readonly<Record<string, unknown>>): string {
  if (field.args.length === 0) return field.name;
  return canonicalFieldKey(
    field.name,
    field.args.map((a: Argument) => [a.name, valueToJson(a.value, bound)] as const),
  );
}

/**
 * Look a field up by its canonical key, falling back to the unique stored key
 * of the same field name. The fallback covers schema-side default erasure
 * (§5.1): the client cannot know that `friends(first:10)` canonicalizes to
 * `friends` when 10 is the collection's default page, but if exactly one
 * stored key belongs to this field name the intent is unambiguous.
 */
function lookupField(
  fields: Readonly<Record<string, FieldValue>>,
  name: string,
  exactKey: string,
): FieldValue | undefined {
  const exact = fields[exactKey];
  if (exact !== undefined) return exact;
  let found: FieldValue | undefined;
  let hits = 0;
  const prefix = name + "(";
  for (const key of Object.keys(fields)) {
    if (key === name || key.startsWith(prefix)) {
      hits++;
      if (hits > 1) return undefined;
      found = fields[key];
    }
  }
  return hits === 1 ? found : undefined;
}

/** Selections at the target of a `@depth(n)` edge: the enclosing set, the recursive edge unrolled one level (innermost level omits it). */
function depthSelections(field: FieldSel, enclosing: readonly Selection[], depth: number): Selection[] {
  const out: Selection[] = [];
  for (const sel of enclosing) {
    if (sel === field) {
      if (depth > 1) out.push({ ...field, depth: depth - 1 });
    } else {
      out.push(sel);
    }
  }
  return out;
}

class Denormalizer {
  constructor(
    private readonly store: LatticeStore,
    private readonly bound: Readonly<Record<string, unknown>>,
    private readonly refs: Set<Ref>,
  ) {}

  entity(ref: Ref, selections: readonly Selection[]): EntityTree | undefined {
    this.refs.add(ref);
    const state = this.store.get(ref);
    if (!state) return undefined;
    const tree: Record<string, unknown> = { __ref: ref };
    this.walk(tree, ref, state.fields, selections, selections);
    return tree as EntityTree;
  }

  private walk(
    tree: Record<string, unknown>,
    ref: Ref,
    fields: Readonly<Record<string, FieldValue>>,
    selections: readonly Selection[],
    enclosing: readonly Selection[],
  ): void {
    for (const sel of selections) {
      switch (sel.kind) {
        case "field": {
          const key = fieldKeyOf(sel, this.bound);
          const raw = lookupField(fields, sel.name, key);
          let value: unknown;
          if (sel.depth !== undefined) {
            value = this.edge(raw, depthSelections(sel, enclosing, sel.depth));
          } else if (sel.selections) {
            value = this.edge(raw, sel.selections);
          } else {
            value = raw;
          }
          tree[sel.name] = value;
          if (key !== sel.name) tree[key] = value;
          break;
        }
        case "inline": {
          if (parseRef(ref)[0] === sel.on) {
            this.walk(tree, ref, fields, sel.selections, sel.selections);
          }
          break;
        }
        case "spread":
          // A schema-fragment reference is late-bound: without the schema the
          // client cannot know which fields it selects. The entity record
          // still carries them; read them off `store.get(ref)` if needed.
          break;
      }
    }
  }

  private edge(raw: FieldValue | undefined, selections: readonly Selection[]): unknown {
    if (raw === undefined || raw === null) return undefined;
    if (isRefValue(raw)) return this.entity(raw.$ref, selections);
    if (isPageValue(raw)) {
      const page = raw.$page;
      const items: EntityTree[] = [];
      for (const item of page.items) {
        const tree = this.entity(item.$ref, selections);
        if (tree) items.push(tree);
      }
      const out: PageTree = {
        items,
        next: page.next ?? null,
        prev: page.prev ?? null,
        ...(page.total !== undefined ? { total: page.total } : {}),
      };
      return out;
    }
    if (Array.isArray(raw)) {
      const items: EntityTree[] = [];
      for (const item of raw) {
        const itemRef = isRefValue(item) ? item.$ref : isRef(item) ? item : undefined;
        if (itemRef === undefined) continue;
        const tree = this.entity(itemRef, selections);
        if (tree) items.push(tree);
      }
      return items;
    }
    if (isRef(raw)) return this.entity(raw, selections);
    return undefined;
  }
}

/**
 * Walk `doc`'s selections against the normalized store, building the plain
 * tree components consume: to-one edges become objects, paginated edges
 * `{items, next, prev, total?}`, bounded collections arrays, missing or
 * elided entities `undefined`. Parameterized fields are also exposed under
 * their canonical key (`avatarUrl(size:48)`); select the same field twice
 * with different arguments and the canonical keys disambiguate.
 *
 * `doc` must be local-fragment-free (pass it through `expandLocalFragments`
 * or use a doc produced by `mergeQueries`); `refsOut`, when given, collects
 * every ref the tree was assembled from.
 */
export function denormalize(
  doc: QueryDoc<unknown>,
  vars: Vars,
  store: LatticeStore,
  manifest: ManifestRecord | undefined,
  refsOut?: Set<Ref>,
): QueryData {
  const refs = refsOut ?? new Set<Ref>();
  const walker = new Denormalizer(store, bindVariables(doc, vars), refs);
  const data: QueryData = {};
  for (const sel of doc.selections) {
    if (sel.kind !== "field") continue;
    const order = manifest?.root[sel.name];
    if (!order) {
      data[sel.name] = undefined;
      continue;
    }
    const items: EntityTree[] = [];
    for (const ref of order) {
      const tree = walker.entity(ref, sel.selections ?? []);
      if (tree) items.push(tree);
    }
    data[sel.name] = items;
  }
  return data;
}

// ---------------------------------------------------------------------------
// The client

interface LearnedUrl {
  readonly hash: string;
  readonly planId?: string;
}

interface PendingQuery {
  readonly doc: QueryDoc<unknown>;
  readonly vars: Vars;
  readonly resolve: (r: QueryResult<unknown>) => void;
  readonly reject: (e: unknown) => void;
}

interface BatchGroup {
  readonly docs: QueryDoc<unknown>[];
  readonly entries: PendingQuery[];
  vars: Record<string, unknown>;
}

export class LatticeClient {
  readonly store: LatticeStore;
  private readonly base: string;
  private readonly fetchFn: FetchLike;
  private readonly claims?: Readonly<Record<string, unknown>>;
  private readonly vcAuth?: string;
  private readonly slice: SliceName;
  private readonly onRequest?: (event: LatticeRequestEvent) => void;
  private readonly inlineUrlBudget: number;
  /** canonical text → learned hash-form URL parts (§6.3 Location handoff). */
  private readonly learned = new Map<string, LearnedUrl>();
  private pending: PendingQuery[] = [];

  constructor(options: LatticeClientOptions) {
    this.base = options.base.replace(/\/$/, "");
    this.fetchFn = options.fetch ?? ((url, init) => fetch(url, init));
    if (options.claims !== undefined) this.claims = options.claims;
    if (options.vcAuth !== undefined) this.vcAuth = options.vcAuth;
    this.slice = options.slice ?? (options.claims ? "ctx" : "pub");
    if (options.onRequest !== undefined) this.onRequest = options.onRequest;
    this.inlineUrlBudget = options.inlineUrlBudget ?? 6144;
    this.store = options.store ?? new LatticeStore();
  }

  // -- query ------------------------------------------------------------------

  /**
   * Run one query: canonicalize, climb the transport ladder, stream the
   * response into the store, and denormalize the result.
   */
  async query<T = QueryData>(doc: QueryDoc<T>, vars: Vars = {}, options: QueryOptions = {}): Promise<QueryResult<T>> {
    const expanded = expandLocalFragments(doc);
    const canonical = canonicalize(doc);
    const params = this.variableParams(expanded, vars);
    const resp = await this.sendQuery(canonical, params, options, 1);
    if (!resp.ok) throw await problemOf(resp, resp.url);
    const outcome = await this.consume(resp);
    this.register(canonical, vars, outcome, resp);
    const refs = new Set<Ref>();
    const data = denormalize(expanded, vars, this.store, outcome.manifest, refs) as T;
    return {
      data,
      ...(outcome.manifest ? { manifest: outcome.manifest } : {}),
      errors: outcome.errors,
      complete: outcome.end?.complete ?? false,
      refs,
    };
  }

  /**
   * Like `query`, but queries submitted within the same microtask tick are
   * merged (`mergeQueries`) into one multi-root request where the protocol
   * allows; unmergeable documents fall back to their own request
   * transparently. This is what the React hooks ride on.
   */
  queryBatched<T = QueryData>(doc: QueryDoc<T>, vars: Vars = {}): Promise<QueryResult<T>> {
    const { promise, resolve, reject } = Promise.withResolvers<QueryResult<T>>();
    this.pending.push({
      doc: doc as QueryDoc<unknown>,
      vars,
      resolve: resolve as (r: QueryResult<unknown>) => void,
      reject,
    });
    if (this.pending.length === 1) {
      queueMicrotask(() => {
        void this.flushBatch();
      });
    }
    return promise;
  }

  private async flushBatch(): Promise<void> {
    const batch = this.pending;
    this.pending = [];
    if (batch.length === 0) return;
    const groups: BatchGroup[] = [];
    for (const entry of batch) {
      let placed = false;
      for (const group of groups) {
        if (!varsCompatible(group.vars, entry.vars)) continue;
        try {
          mergeQueries([...group.docs, entry.doc]);
        } catch {
          continue; // unmergeable with this group (or invalid; it will fail alone)
        }
        group.docs.push(entry.doc);
        group.entries.push(entry);
        group.vars = { ...group.vars, ...entry.vars };
        placed = true;
        break;
      }
      if (!placed) groups.push({ docs: [entry.doc], entries: [entry], vars: { ...entry.vars } });
    }
    await Promise.all(groups.map((g) => this.runGroup(g)));
  }

  private async runGroup(group: BatchGroup): Promise<void> {
    if (group.entries.length === 1) {
      const entry = group.entries[0]!;
      try {
        entry.resolve(await this.query(entry.doc, entry.vars));
      } catch (e) {
        entry.reject(e);
      }
      return;
    }
    try {
      const { merged, assignments } = mergeQueries(group.docs);
      const canonical = canonicalize(merged);
      const params = this.variableParams(merged, group.vars);
      const resp = await this.sendQuery(canonical, params, {}, group.entries.length);
      if (!resp.ok) throw await problemOf(resp, resp.url);
      const outcome = await this.consume(resp);
      this.register(canonical, group.vars, outcome, resp);
      const sharedRefs = new Set<string>(outcome.refs);
      const sharedKeys = surrogateKeysOf(resp);
      const mergedRoots = new Set<string>();
      for (const a of assignments) {
        for (const root of a.roots) mergedRoots.add(root);
      }
      const etag = outcome.end?.etag ?? outcome.manifest?.etag;
      for (let i = 0; i < group.entries.length; i++) {
        const entry = group.entries[i]!;
        const assignment = assignments[i]!;
        const refs = new Set<Ref>();
        const data = denormalize(assignment.doc, entry.vars, this.store, outcome.manifest, refs);
        // Register the consumer's own result too, so its watch finds a cache
        // entry and staleness marking reaches it. Shared response keys are
        // attributed per entry: entity keys by the entry's own refs, sibling
        // roots' collection keys excluded, anything unattributable shared
        // (over-invalidation is noise; under-invalidation would be a bug).
        const keys = new Set<string>(refs);
        for (const key of sharedKeys) {
          if (sharedRefs.has(key)) {
            if (refs.has(key)) keys.add(key);
            continue;
          }
          const family = key.slice(0, key.indexOf(":"));
          if (mergedRoots.has(family) && !assignment.roots.includes(family)) continue;
          keys.add(key);
        }
        if (outcome.manifest) {
          const rootOrder: Record<string, readonly Ref[]> = {};
          for (const name of assignment.roots) {
            const order = outcome.manifest.root[name];
            if (order) rootOrder[name] = order;
          }
          this.store.registerResult(this.resultKey(entry.doc, entry.vars), {
            rootOrder,
            ...(etag !== undefined ? { etag } : {}),
            keys,
          });
        }
        entry.resolve({
          data,
          ...(outcome.manifest ? { manifest: outcome.manifest } : {}),
          errors: outcome.errors,
          complete: outcome.end?.complete ?? false,
          refs,
        });
      }
    } catch (e) {
      for (const entry of group.entries) entry.reject(e);
    }
  }

  // -- watch ------------------------------------------------------------------

  /**
   * A live view over one query: serves from the cached result when present,
   * fetches (batched) otherwise, re-denormalizes when any underlying entity
   * changes, and refetches (with `Cache-Control: no-cache`, §11.6) when an
   * `invalidated` record marks the result stale. Designed to plug straight
   * into `useSyncExternalStore`.
   */
  watchQuery<T = QueryData>(doc: QueryDoc<T>, vars: Vars = {}, onChange?: (state: WatchState<T>) => void): QueryWatch<T> {
    const watch = new QueryWatch<T>(this, doc, vars);
    if (onChange) watch.attach(onChange);
    return watch;
  }

  /** The registry key for a (document, variables) pair. */
  resultKey(doc: QueryDoc<unknown>, vars: Vars): string {
    return canonicalize(doc) + "\u0000" + canonicalJson(vars);
  }

  // -- mutate -----------------------------------------------------------------

  /**
   * Invoke a named mutation: `POST {base}/m/{name}` with a JSON body (§11.1).
   * The response entity stream applies to the store like any other (read-
   * your-writes for entity fields); `invalidated` records mark intersecting
   * cached query results stale, which triggers active watches to refetch.
   */
  async mutate(name: string, input: unknown, options: MutateOptions = {}): Promise<MutationResult> {
    const url = `${this.base}/m/${encodeURIComponent(name)}`;
    const headers: Record<string, string> = { "content-type": "application/json" };
    if (options.idempotencyKey) headers[HEADERS.idempotencyKey] = options.idempotencyKey;
    if (this.vcAuth) headers[HEADERS.vcAuth] = this.vcAuth;
    this.onRequest?.({ kind: "mutation", method: "POST", url });
    const resp = await this.fetchFn(url, { method: "POST", headers, body: JSON.stringify(input ?? {}) });
    if (!resp.ok) throw await problemOf(resp, url);
    const outcome = await this.consume(resp);
    const refs: Ref[] = [];
    for (const order of Object.values(outcome.manifest?.root ?? {})) refs.push(...order);
    return {
      ...(outcome.manifest ? { manifest: outcome.manifest } : {}),
      errors: outcome.errors,
      complete: outcome.end?.complete ?? false,
      committed: outcome.committed,
      invalidatedKeys: outcome.invalidated,
      refs,
    };
  }

  // -- transport --------------------------------------------------------------

  private async consume(resp: Response): Promise<ApplyOutcome> {
    const outcome = newApplyOutcome();
    for await (const rec of readRecords(resp)) {
      this.store.applyRecord(rec, outcome);
    }
    this.store.flush();
    return outcome;
  }

  private register(canonical: string, vars: Vars, outcome: ApplyOutcome, resp: Response): void {
    const manifest = outcome.manifest;
    if (!manifest) return;
    const keys = new Set<string>(outcome.refs);
    for (const key of surrogateKeysOf(resp)) keys.add(key);
    const etag = outcome.end?.etag ?? manifest.etag;
    this.store.registerResult(canonical + "\u0000" + canonicalJson(vars), {
      rootOrder: { ...manifest.root },
      ...(etag !== undefined ? { etag } : {}),
      keys,
    });
  }

  private variableParams(doc: QueryDoc<unknown>, vars: Vars): Array<[string, string]> {
    const declared = new Map(doc.variables.map((v) => [v.name, v]));
    const out: Array<[string, string]> = [];
    for (const name of Object.keys(vars).sort()) {
      const value = vars[name];
      if (value === undefined || value === null) continue;
      if (RESERVED_PARAMS[name]) {
        throw new LatticeQueryError(`variable name ${JSON.stringify(name)} collides with a reserved URL parameter`);
      }
      const decl = declared.get(name);
      if (!decl) {
        throw new LatticeQueryError(`variable $${name} is bound but not declared by the query`);
      }
      // Defaults are omitted from the URL (§6.1).
      if (decl.default !== undefined && canonicalJson(valueToJson(decl.default, {})) === canonicalJson(value)) {
        continue;
      }
      out.push([name, paramString(value)]);
    }
    for (const v of doc.variables) {
      if (!v.optional && v.default === undefined && (vars[v.name] === undefined || vars[v.name] === null)) {
        throw new LatticeQueryError(`variable $${v.name} has no binding and no default`);
      }
    }
    return out;
  }

  private sliceParams(): Array<[string, string]> {
    const out: Array<[string, string]> = [["slice", this.slice]];
    if (this.claims && this.slice === "ctx") {
      out.push(["vc", b64url(new TextEncoder().encode(canonicalJson(this.claims)))]);
    }
    return out;
  }

  private requestHeaders(options: QueryOptions): Record<string, string> {
    const headers: Record<string, string> = {};
    if (this.vcAuth) headers[HEADERS.vcAuth] = this.vcAuth;
    if (options.noCache) headers["cache-control"] = "no-cache";
    return headers;
  }

  /** Remember the hash-form URL granted via `Location`/`Content-Location` (§6.3). */
  private learn(canonical: string, resp: Response): void {
    const location = resp.headers.get("location") ?? resp.headers.get("content-location");
    if (!location) return;
    const match = /\/q\/([A-Za-z0-9_-]+)/.exec(location);
    if (!match) return;
    const hash = match[1]!;
    let planId = resp.headers.get(HEADERS.plan) ?? undefined;
    if (!planId) {
      const fromUrl = /[?&]p=([^&]+)/.exec(location);
      if (fromUrl) planId = decodeURIComponent(fromUrl[1]!);
    }
    this.learned.set(canonical, { hash, ...(planId !== undefined ? { planId } : {}) });
  }

  private async sendQuery(
    canonical: string,
    vars: Array<[string, string]>,
    options: QueryOptions,
    merged: number,
  ): Promise<Response> {
    const common = [...this.sliceParams(), ...vars];
    const headers = this.requestHeaders(options);
    const qs = (pairs: Array<[string, string]>): string =>
      pairs.map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(v)}`).join("&");

    // Rung 1: hash form, once the steady-state URL has been learned.
    const known = this.learned.get(canonical);
    if (known) {
      const pairs: Array<[string, string]> = known.planId ? [["p", known.planId], ...common] : [...common];
      const url = `${this.base}/q/${known.hash}?${qs(pairs)}`;
      this.emit("hash", "GET", url, canonical, merged);
      const resp = await this.fetchFn(url, { headers });
      // 404 lattice:unknown-query (evicted memo) and 409 lattice:plan-superseded
      // both mean: forget the learned URL, fall down the ladder, re-introduce.
      if (resp.status !== 404 && resp.status !== 409) {
        this.learn(canonical, resp);
        return resp;
      }
      await resp.body?.cancel().catch(() => undefined);
      this.learned.delete(canonical);
    }

    // Rung 2: inline form, when the platform can produce raw DEFLATE and the
    // result fits the URL budget. No dictionary: `dv` omitted (§6.2).
    if (typeof CompressionStream !== "undefined") {
      const d = b64url(await deflateRaw(canonical));
      if (d.length <= this.inlineUrlBudget) {
        const url = `${this.base}/q?${qs([["d", d], ...common])}`;
        this.emit("inline", "GET", url, canonical, merged);
        const resp = await this.fetchFn(url, { headers });
        if (resp.status !== 404 && resp.status !== 405 && resp.status !== 414 && resp.status !== 501) {
          this.learn(canonical, resp);
          return resp;
        }
        await resp.body?.cancel().catch(() => undefined);
      }
    }

    // Rung 3: POST introduction (§6.4) — executes, memoizes, grants Location.
    const url = `${this.base}/q?${qs([["intent", "introduce"], ...common])}`;
    this.emit("introduce", "POST", url, canonical, merged);
    const resp = await this.fetchFn(url, {
      method: "POST",
      headers: { ...headers, "content-type": QUERY_MEDIA_TYPE },
      body: canonical,
    });
    this.learn(canonical, resp);
    return resp;
  }

  private emit(kind: LatticeRequestEvent["kind"], method: string, url: string, canonicalText: string, merged: number): void {
    this.onRequest?.({
      kind,
      method,
      url,
      canonicalText,
      ...(merged > 1 ? { merged } : {}),
    });
  }
}

/** The response's `Surrogate-Key` cache tags (§10.5), when the header is readable. */
function surrogateKeysOf(resp: Response): string[] {
  const header = resp.headers.get(HEADERS.surrogateKey);
  if (!header) return [];
  const out: string[] = [];
  for (const key of header.split(/[\s,]+/)) {
    if (key) out.push(key);
  }
  return out;
}

function paramString(value: unknown): string {
  if (typeof value === "string") return value;
  if (typeof value === "number" || typeof value === "boolean" || typeof value === "bigint") return String(value);
  return canonicalJson(value);
}

function varsCompatible(a: Readonly<Record<string, unknown>>, b: Vars): boolean {
  for (const key of Object.keys(b)) {
    if (key in a && canonicalJson(a[key]) !== canonicalJson(b[key])) return false;
  }
  return true;
}

// ---------------------------------------------------------------------------
// QueryWatch

export interface WatchState<T = QueryData> {
  readonly data: T | undefined;
  readonly loading: boolean;
  readonly error: unknown;
  readonly stale: boolean;
}

export class QueryWatch<T = QueryData> {
  private readonly expanded: QueryDoc<unknown>;
  private readonly key: string;
  private state: WatchState<T> = { data: undefined, loading: true, error: undefined, stale: false };
  private readonly listeners = new Set<() => void>();
  private storeUnsub?: () => void;
  private resultUnsub?: () => void;
  private active = false;
  private fetching = false;

  constructor(
    private readonly client: LatticeClient,
    private readonly doc: QueryDoc<T>,
    private readonly vars: Vars,
  ) {
    this.expanded = expandLocalFragments(doc);
    this.key = client.resultKey(doc as QueryDoc<unknown>, vars);
  }

  /** `useSyncExternalStore`-shaped subscribe; activates lazily on first use. */
  readonly subscribe = (cb: () => void): (() => void) => {
    this.listeners.add(cb);
    this.activate();
    return () => {
      this.listeners.delete(cb);
      if (this.listeners.size === 0) this.deactivate();
    };
  };

  /** Stable snapshot: the same object identity until the state changes. */
  readonly getSnapshot = (): WatchState<T> => this.state;

  /** Force a network refetch, revalidating through shared caches (§11.6). */
  readonly refetch = async (): Promise<void> => {
    if (this.fetching) return;
    this.fetching = true;
    try {
      const result = await this.client.query(this.doc, this.vars, { noCache: true });
      this.publish(result.data, result.refs);
    } catch (error) {
      this.setState({ ...this.state, loading: false, error });
    } finally {
      this.fetching = false;
    }
  };

  /** Attach a change callback (the non-React `watchQuery(doc, vars, cb)` shape). */
  attach(onChange: (state: WatchState<T>) => void): () => void {
    return this.subscribe(() => onChange(this.state));
  }

  private activate(): void {
    if (this.active) return;
    this.active = true;
    this.resultUnsub = this.client.store.subscribeResult(this.key, () => {
      const entry = this.client.store.getResult(this.key);
      if (entry?.stale) {
        this.setState({ ...this.state, stale: true });
        if (!this.fetching) void this.refetch();
      }
    });
    const cached = this.client.store.getResult(this.key);
    if (cached) {
      this.publishFromStore(cached);
      if (cached.stale) void this.refetch();
      return;
    }
    // StrictMode-style unsubscribe/resubscribe while the first fetch is in
    // flight must not issue a second one.
    if (this.fetching) return;
    this.fetching = true;
    this.client.queryBatched(this.doc, this.vars).then(
      (result) => {
        this.fetching = false;
        this.publish(result.data, result.refs);
      },
      (error: unknown) => {
        this.fetching = false;
        this.setState({ data: undefined, loading: false, error, stale: false });
      },
    );
  }

  private deactivate(): void {
    this.active = false;
    this.storeUnsub?.();
    this.resultUnsub?.();
    this.storeUnsub = undefined;
    this.resultUnsub = undefined;
  }

  private publish(data: T, refs: ReadonlySet<Ref>): void {
    this.resubscribe(refs);
    this.setState({ data, loading: false, error: undefined, stale: this.client.store.getResult(this.key)?.stale ?? false });
  }

  private publishFromStore(entry: QueryResultEntry): void {
    const refs = new Set<Ref>();
    const manifest: ManifestRecord = { kind: "manifest", root: entry.rootOrder };
    const data = denormalize(this.expanded, this.vars, this.client.store, manifest, refs) as T;
    this.resubscribe(refs);
    this.setState({ data, loading: false, error: undefined, stale: entry.stale });
  }

  private resubscribe(refs: ReadonlySet<Ref>): void {
    this.storeUnsub?.();
    this.storeUnsub = undefined;
    if (!this.active) return; // a fetch landing after unmount must not leak a subscription
    this.storeUnsub = this.client.store.subscribe(refs, () => {
      const entry = this.client.store.getResult(this.key);
      if (entry) this.publishFromStore(entry);
    });
  }

  private setState(next: WatchState<T>): void {
    this.state = next;
    for (const cb of [...this.listeners]) cb();
  }
}
