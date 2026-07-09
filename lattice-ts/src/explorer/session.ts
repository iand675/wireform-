/**
 * The explorer's headless orchestration layer: everything the UI needs, with
 * no DOM. An `ExplorerSession` talks to a Lattice origin over an injected
 * `fetch`, so it is fully testable against a mock origin.
 *
 * It deliberately does its own request assembly rather than delegating to
 * `LatticeClient`, because the explorer's whole reason to exist is
 * inspectability: it captures the exact request (method, URL, canonical text),
 * the raw entity-stream records, and every response header — the things the
 * client hides behind a denormalized result. It reuses the client's pure
 * pieces (`canonicalize`, `LatticeStore`, `denormalize`, `recordsOfText`) so
 * the denormalized view matches what a real client would compute.
 *
 * Query runs default to the introduction rung (§6.3/§6.4): it executes,
 * memoizes, and returns a `Location` grant, which lets the explorer also show
 * the steady-state hash URL, the plan id, and the `explain` document — the
 * full inspection story. `oneshot` (§6.5) is offered for throwaway runs that
 * must not pollute the origin memo, and `inline` (§6.2) for the self-contained
 * GET where the platform can produce raw DEFLATE.
 */

import {
  canonicalize,
  canonicalJson,
  expandLocalFragments,
  parse,
  valueToJson,
  type QueryDoc,
} from "../canonical.ts";
import { denormalize, type FetchLike, type SliceName } from "../client.ts";
import { LatticeStore } from "../store.ts";
import {
  HEADERS,
  QUERY_MEDIA_TYPE,
  RESERVED_PARAMS,
  recordsOfText,
  readRecords,
  type ErrorRecord,
  type LatticeRecord,
  type ManifestRecord,
  type QueryData,
  type Ref,
} from "../wire.ts";
import { parseSchema, type SchemaModel } from "./schema.ts";

export type { SliceName } from "../client.ts";

// ---------------------------------------------------------------------------
// Discovery + schema

export interface Discovery {
  readonly schema?: { readonly current?: string };
  readonly admission?: string;
  readonly queryMediaType?: string;
  readonly budgets?: Readonly<Record<string, number>>;
  readonly endpoints?: Readonly<Record<string, string>>;
  readonly [k: string]: unknown;
}

export interface LoadedSchema {
  /** The canonical IDL text served at `/schema/{hash}`. */
  readonly idl: string;
  /** The content-address hash of the schema document. */
  readonly hash: string;
  readonly model: SchemaModel;
  readonly discovery: Discovery;
}

// ---------------------------------------------------------------------------
// Run results

export type RunMode = "introduce" | "oneshot" | "inline" | "hash";

export interface RunOptions {
  readonly mode?: RunMode;
  readonly slice?: SliceName;
  /** Send `Cache-Control: no-cache` (post-mutation reads, §11.6). */
  readonly noCache?: boolean;
  /** Fetch the `explain` document when a hash is learned (default true, non-oneshot). */
  readonly explain?: boolean;
}

export interface RunHeaders {
  readonly plan?: string;
  readonly schema?: string;
  readonly snapshot?: string;
  /** `Lattice-Snapshot-Floor` (§10.2): the validity interval's left end. */
  readonly snapshotFloor?: string;
  readonly etag?: string;
  readonly cacheControl?: string;
  readonly location?: string;
  readonly contentLocation?: string;
  readonly surrogateKeys: readonly string[];
  /** Every response header, lower-cased keys. */
  readonly all: Readonly<Record<string, string>>;
}

/** RFC 9457 problem details of a failed request. */
export interface ProblemDetails {
  readonly type?: string;
  readonly title?: string;
  readonly status?: number;
  readonly detail?: string;
  readonly diagnostics?: readonly string[];
  readonly [k: string]: unknown;
}

/**
 * Wall-clock marks for one slice's request lifecycle — all `performance.now()`
 * absolute milliseconds, so they compare directly across concurrently-fetched
 * slices to build a trace waterfall.
 */
export interface SliceTiming {
  readonly requestStart: number;
  /** TTFB — response headers arrived. */
  readonly responseStart: number;
  /** First NDJSON record parsed (streaming path only). */
  readonly firstRecord?: number;
  /** Body fully read / stream complete. */
  readonly responseEnd: number;
}

export interface RunResult {
  readonly ok: boolean;
  readonly status: number;
  readonly request: { readonly method: string; readonly url: string; readonly canonicalText: string };
  readonly headers: RunHeaders;
  /** Cache classification derived from the response headers. */
  readonly cache: CacheInfo;
  readonly records: readonly LatticeRecord[];
  readonly manifest?: ManifestRecord;
  /** The denormalized tree a real client would assemble. */
  readonly data: QueryData;
  readonly errors: readonly ErrorRecord[];
  readonly complete: boolean;
  readonly refs: readonly Ref[];
  readonly durationMs: number;
  /** Fine-grained request-lifecycle marks for the trace waterfall. */
  readonly timing: SliceTiming;
  /** Learned from `Location`/`Content-Location` on an introduction. */
  readonly hash?: string;
  readonly planId?: string;
  readonly explain?: ExplainDoc;
  readonly problem?: ProblemDetails;
  /** The raw NDJSON (or problem) body text. */
  readonly raw: string;
}

/** How a response related to a cache, derived from its headers. */
export type CacheStatus =
  | "hit" // served from a (shared or CDN) cache
  | "miss" // a cache forwarded to the origin
  | "stale" // served stale (expired but within stale-while-revalidate)
  | "revalidated" // validated against the origin (304)
  | "cacheable" // a fresh, cacheable response (no cache in front, or a fill)
  | "private" // cacheable only by the browser (private)
  | "dynamic" // not a cache read (a POST) or explicitly uncacheable
  | "unknown";

export interface CacheInfo {
  readonly status: CacheStatus;
  /** Cacheable by shared caches / CDNs (public + a positive s-maxage). */
  readonly shared: boolean;
  /** `Age` in seconds, when the response carried one. */
  readonly age?: number;
  /** The effective freshness lifetime (s-maxage for shared, else max-age). */
  readonly maxAge?: number;
  /** A short human summary for the UI chip. */
  readonly detail: string;
}

/** A per-slice run: a {@link RunResult} tagged with the slice it fetched. */
export interface SliceResult extends RunResult {
  readonly slice: SliceName;
}

/** Streaming callbacks for {@link ExplorerSession.runSlices}. */
export interface RunHooks {
  /** Fires the moment a slice's response head arrives (status + headers + cache). */
  onHead?(ev: { slice: SliceName; status: number; ok: boolean; headers: RunHeaders; cache: CacheInfo; request: RunResult["request"] }): void;
  /** Fires per NDJSON record as it streams in, with the data denormalized so far. */
  onRecord?(ev: { slice: SliceName; record: LatticeRecord; index: number; data: QueryData }): void;
}

/** The result of fetching several authorization slices of one query concurrently. */
export interface SlicesRun {
  readonly canonicalText: string;
  /** One entry per requested slice, keyed by slice name. */
  readonly slices: Readonly<Record<string, SliceResult>>;
  /** The slices in request order. */
  readonly order: readonly SliceName[];
  /** The hash learned (or reused) for this canonical query, if any. */
  readonly hash?: string;
  readonly planId?: string;
  /** The shared plan explain document (fetched once for the whole run). */
  readonly explain?: ExplainDoc;
  /** Replay was requested but no hash was known, so an introduce ran instead. */
  readonly autoIntroduced: boolean;
  /** `performance.now()` at run start / return — the trace-waterfall axis. */
  readonly startedAt: number;
  readonly finishedAt: number;
  /** Timing of the once-per-run shared explain fetch, when one was made. */
  readonly explainTiming?: { requestStart: number; responseEnd: number };
}

/** One batched loader in a plan round (spec §20.2 `rounds[].loaders[]`). */
export interface LoaderInfo {
  readonly loader?: string;
  readonly fanout?: number;
  readonly collection?: string;
  readonly grouping?: readonly string[];
}

/** The `explain` document (spec §20.2). Loosely typed — origins may extend it. */
export interface ExplainDoc {
  readonly plan?: string;
  readonly query?: string;
  readonly elements?: ReadonlyArray<{ path: string; derivation: string; slice: string; type?: string }>;
  readonly rounds?: ReadonlyArray<{ round: number; loaders: ReadonlyArray<LoaderInfo> }>;
  readonly surrogateKeys?: ReadonlyArray<{ collection: string; grouping: string[] }>;
  readonly budgets?: Readonly<Record<string, { used: number; limit: number }>>;
  /** Per-type loader projections (§3.1): `"*"` (whole rows) or a sorted field list. */
  readonly projections?: Readonly<Record<string, "*" | readonly string[]>>;
  readonly [k: string]: unknown;
}

export interface MutationResult {
  readonly ok: boolean;
  readonly status: number;
  readonly request: { readonly method: string; readonly url: string };
  readonly headers: RunHeaders;
  readonly records: readonly LatticeRecord[];
  readonly errors: readonly ErrorRecord[];
  readonly committed: boolean;
  readonly invalidatedKeys: readonly string[];
  readonly problem?: ProblemDetails;
  readonly raw: string;
}

/** The `POST /schema/check` compatibility report (§17.3). Loosely typed. */
export interface CheckReport {
  readonly status: number;
  readonly ok: boolean;
  readonly report?: Readonly<Record<string, unknown>>;
  readonly problem?: ProblemDetails;
}

// ---------------------------------------------------------------------------
// Small codecs

function b64url(bytes: Uint8Array): string {
  let s = "";
  for (const b of bytes) s += String.fromCharCode(b);
  return btoa(s).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

async function deflateRaw(text: string): Promise<Uint8Array> {
  const cs = new CompressionStream("deflate-raw");
  const writer = cs.writable.getWriter();
  void writer.write(new TextEncoder().encode(text));
  void writer.close();
  const buf = await new Response(cs.readable).arrayBuffer();
  return new Uint8Array(buf);
}

function headersOf(resp: Response): RunHeaders {
  const all: Record<string, string> = {};
  resp.headers.forEach((v, k) => {
    all[k.toLowerCase()] = v;
  });
  const sk = all[HEADERS.surrogateKey];
  return {
    ...(all[HEADERS.plan] !== undefined ? { plan: all[HEADERS.plan] } : {}),
    ...(all[HEADERS.schema] !== undefined ? { schema: all[HEADERS.schema] } : {}),
    ...(all[HEADERS.snapshot] !== undefined ? { snapshot: all[HEADERS.snapshot] } : {}),
    ...(all[HEADERS.snapshotFloor] !== undefined ? { snapshotFloor: all[HEADERS.snapshotFloor] } : {}),
    ...(all["etag"] !== undefined ? { etag: all["etag"] } : {}),
    ...(all["cache-control"] !== undefined ? { cacheControl: all["cache-control"] } : {}),
    ...(all["location"] !== undefined ? { location: all["location"] } : {}),
    ...(all["content-location"] !== undefined ? { contentLocation: all["content-location"] } : {}),
    surrogateKeys: sk ? sk.trim().split(/\s+/).filter(Boolean) : [],
    all,
  };
}

/** Extract `{hash, planId}` from a `Location`/`Content-Location` grant. */
function learnFrom(headers: RunHeaders): { hash?: string; planId?: string } {
  const loc = headers.location ?? headers.contentLocation;
  if (!loc) return {};
  const m = /\/q\/([A-Za-z0-9_-]+)/.exec(loc);
  if (!m) return {};
  let planId = headers.plan;
  if (!planId) {
    const p = /[?&]p=([^&]+)/.exec(loc);
    if (p) planId = decodeURIComponent(p[1]!);
  }
  return { hash: m[1]!, ...(planId !== undefined ? { planId } : {}) };
}

function intHeader(v: string | undefined): number | undefined {
  if (v === undefined) return undefined;
  const n = Number.parseInt(v, 10);
  return Number.isFinite(n) ? n : undefined;
}

/** Read a numeric `Cache-Control` directive (e.g. `s-maxage=15`). */
function ccDirective(cc: string, name: string): number | undefined {
  const m = new RegExp(`(?:^|[,\\s])${name}=(\\d+)`, "i").exec(cc);
  return m ? Number.parseInt(m[1]!, 10) : undefined;
}

/**
 * Classify a response's relationship to caches from its headers: the RFC 9211
 * `Cache-Status` (and the common `CF-Cache-Status` / `X-Cache` variants) win
 * when present; otherwise we infer from method + `Age` + `Cache-Control`. A
 * POST introduction is `dynamic`; a `public, s-maxage=N` GET is `cacheable`
 * (or `hit` once it carries a positive `Age`); a `private` GET is `private`.
 */
function classifyCache(method: string, headers: RunHeaders): CacheInfo {
  const all = headers.all;
  const cc = headers.cacheControl ?? "";
  const age = intHeader(all["age"]);
  const sMaxAge = ccDirective(cc, "s-maxage");
  const maxAge = ccDirective(cc, "max-age");
  const shared = /(?:^|[,\s])public(?:\b|$)/i.test(cc) && sMaxAge !== undefined && sMaxAge > 0;
  const maxAgeEff = sMaxAge ?? maxAge;
  const fresh = { ...(age !== undefined ? { age } : {}), ...(maxAgeEff !== undefined ? { maxAge: maxAgeEff } : {}) };
  const parts = (lead: string): string =>
    [
      lead,
      maxAgeEff !== undefined ? `${sMaxAge !== undefined ? "s-maxage" : "max-age"}=${maxAgeEff}` : undefined,
      age !== undefined ? `age ${age}s` : undefined,
    ]
      .filter(Boolean)
      .join(" · ");

  const cdnHeader = all["cache-status"] ?? all["cf-cache-status"] ?? all["x-cache"];
  if (cdnHeader) {
    const c = cdnHeader.toLowerCase();
    if (/\bdynamic\b/.test(c)) return { status: "dynamic", shared: false, detail: "dynamic — not cached at edge" };
    if (/revalidat/.test(c)) return { status: "revalidated", shared, ...fresh, detail: parts("revalidated at edge") };
    if (/(stale|expired)/.test(c)) return { status: "stale", shared, ...fresh, detail: parts("served stale") };
    if (/\bhit\b/.test(c)) return { status: "hit", shared, ...fresh, detail: parts("cache HIT") };
    if (/(miss|fwd)/.test(c)) return { status: "miss", shared, ...fresh, detail: parts("cache MISS — origin fill") };
  }

  if (method !== "GET") return { status: "dynamic", shared: false, detail: "dynamic — POST, not a cache read" };
  if (/no-store/i.test(cc)) return { status: "dynamic", shared: false, detail: "no-store" };
  if (/(?:^|[,\s])private/i.test(cc)) return { status: "private", shared: false, ...fresh, detail: parts("private (browser-only)") };
  if (age !== undefined && age > 0 && shared) return { status: "hit", shared, ...fresh, detail: parts("served from a shared cache") };
  if (shared) return { status: "cacheable", shared, ...fresh, detail: parts("cacheable") };
  return { status: "unknown", shared: false, detail: cc || "no cache directives" };
}

// ---------------------------------------------------------------------------
// Session

export interface ExplorerSessionOptions {
  readonly base: string;
  readonly fetch?: FetchLike;
  /** Visibility claims (§8.2). Presence flips the default slice to `ctx`. */
  readonly claims?: Readonly<Record<string, unknown>>;
  /** The `{exp}.{sig}` proof for the claims, sent as `X-Vc-Auth`. */
  readonly vcAuth?: string;
  /** Bearer token sent as `Authorization` on `priv`-slice reads. */
  readonly authToken?: string;
  readonly slice?: SliceName;
}

export class ExplorerSession {
  readonly base: string;
  private readonly fetchFn: FetchLike;
  private readonly claims?: Readonly<Record<string, unknown>>;
  private readonly vcAuth?: string;
  private readonly defaultSlice: SliceName;
  private readonly authToken?: string;
  /** Canonical query text → the hash learned for it (for `mode: "hash"` replay). */
  private readonly learned = new Map<string, { hash: string; planId?: string }>();

  constructor(options: ExplorerSessionOptions) {
    this.base = options.base.replace(/\/$/, "");
    this.fetchFn = options.fetch ?? ((url, init) => fetch(url, init));
    if (options.claims !== undefined) this.claims = options.claims;
    if (options.vcAuth !== undefined) this.vcAuth = options.vcAuth;
    if (options.authToken !== undefined) this.authToken = options.authToken;
    this.defaultSlice = options.slice ?? (options.claims ? "ctx" : "pub");
  }

  /** Fetch discovery, follow it to the schema document, and parse the model. */
  async loadSchema(): Promise<LoadedSchema> {
    const discResp = await this.fetchFn(`${this.base}/.well-known/lattice`, {});
    if (!discResp.ok) throw new Error(`discovery failed: ${discResp.status}`);
    const discovery = (await discResp.json()) as Discovery;
    const current = discovery.schema?.current;
    if (!current) throw new Error("discovery did not advertise a current schema document");
    const path = current.startsWith("/") ? current : `/${current}`;
    const schemaResp = await this.fetchFn(`${this.base}${path}`, {});
    if (!schemaResp.ok) throw new Error(`schema document fetch failed: ${schemaResp.status}`);
    const idl = await schemaResp.text();
    const hash = path.split("/").pop() ?? "";
    return { idl, hash, model: parseSchema(idl), discovery };
  }

  /** Common URL params: slice, the `vc` payload on ctx, then variable bindings. */
  private commonParams(doc: QueryDoc<unknown>, vars: Vars, slice: SliceName): Array<[string, string]> {
    const out: Array<[string, string]> = [["slice", slice]];
    if (this.claims && slice === "ctx") {
      out.push(["vc", b64url(new TextEncoder().encode(canonicalJson(this.claims)))]);
    }
    const declared = new Map(doc.variables.map((v) => [v.name, v]));
    for (const name of Object.keys(vars).sort()) {
      const value = vars[name];
      if (value === undefined || value === null) continue;
      if (RESERVED_PARAMS[name]) continue;
      const decl = declared.get(name);
      if (!decl) continue;
      if (decl.default !== undefined && canonicalJson(valueToJson(decl.default, {})) === canonicalJson(value)) continue;
      out.push([name, typeof value === "string" ? value : canonicalJson(value)]);
    }
    return out;
  }

  private authHeaders(slice?: SliceName, noCache?: boolean): Record<string, string> {
    const h: Record<string, string> = {};
    if (this.vcAuth) h[HEADERS.vcAuth] = this.vcAuth;
    if (slice === "priv" && this.authToken) {
      h["authorization"] = /^bearer\s/i.test(this.authToken) ? this.authToken : `Bearer ${this.authToken}`;
    }
    if (noCache) h["cache-control"] = "no-cache";
    return h;
  }

  /** Synthesize a failed slice result for a thrown request (network error). */
  private networkError(slice: SliceName, canonical: string, e: unknown): SliceResult {
    const detail = e instanceof Error ? e.message : String(e);
    const t = now();
    return {
      slice,
      ok: false,
      status: 0,
      request: { method: "—", url: this.base, canonicalText: canonical },
      headers: { surrogateKeys: [], all: {} },
      cache: { status: "unknown", shared: false, detail: "request failed" },
      records: [],
      data: {},
      errors: [],
      complete: false,
      refs: [],
      durationMs: 0,
      timing: { requestStart: t, responseStart: t, responseEnd: t },
      problem: { title: "lattice:network", detail },
      raw: detail,
    };
  }

  /**
   * Run one slice of a query (default {@link defaultSlice}), buffered. The
   * simple single-slice path; the UI uses {@link runSlices}.
   */
  async runQuery(text: string, vars: Vars = {}, options: RunOptions = {}): Promise<RunResult> {
    const slice = options.slice ?? this.defaultSlice;
    const run = await this.runSlices(text, vars, [slice], options);
    const r = run.slices[slice]!;
    return run.explain ? { ...r, explain: run.explain } : r;
  }

  /**
   * Fetch several authorization slices of one query concurrently, each
   * streamed. The shared plan explain is fetched once. Learns (and reuses) the
   * query's hash so a later `mode: "hash"` run can replay the cacheable GET.
   */
  async runSlices(
    text: string,
    vars: Vars = {},
    slices: readonly SliceName[],
    options: RunOptions = {},
    hooks: RunHooks = {},
  ): Promise<SlicesRun> {
    const startedAt = now();
    const doc = parse(text);
    const expanded = expandLocalFragments(doc);
    const canonical = canonicalize(doc);
    let mode = options.mode ?? "introduce";
    const known = this.learned.get(canonical);
    let autoIntroduced = false;
    if (mode === "hash" && !known) {
      mode = "introduce";
      autoIntroduced = true;
    }
    const hash = known?.hash;

    const settled = await Promise.all(
      slices.map(async (slice) => {
        try {
          return [slice, await this.streamSlice(slice, mode, doc, expanded, canonical, vars, options, hash, hooks)] as const;
        } catch (e) {
          return [slice, this.networkError(slice, canonical, e)] as const;
        }
      }),
    );
    const bySlice: Record<string, SliceResult> = {};
    for (const [slice, r] of settled) bySlice[slice] = r;

    // The hash is over the canonical query text (slice-independent), so learn
    // it from whichever slice's grant carried it.
    let learnedHash = known?.hash;
    let learnedPlan = known?.planId;
    if (mode !== "oneshot") {
      const grant = settled.map(([, r]) => r).find((r) => r.ok && r.hash);
      if (grant?.hash) {
        learnedHash = grant.hash;
        learnedPlan = grant.planId;
        this.learned.set(canonical, { hash: learnedHash, ...(learnedPlan !== undefined ? { planId: learnedPlan } : {}) });
      }
    }

    let explainDoc: ExplainDoc | undefined;
    let explainTiming: { requestStart: number; responseEnd: number } | undefined;
    const wantExplain = options.explain ?? mode !== "oneshot";
    const anyOk = settled.some(([, r]) => r.ok);
    if (wantExplain && learnedHash && anyOk) {
      const es = now();
      explainDoc = await this.explain(learnedHash, learnedPlan).catch(() => undefined);
      explainTiming = { requestStart: es, responseEnd: now() };
    }

    const finishedAt = now();
    return {
      canonicalText: canonical,
      slices: bySlice,
      order: slices,
      autoIntroduced,
      startedAt,
      finishedAt,
      ...(explainTiming ? { explainTiming } : {}),
      ...(learnedHash !== undefined ? { hash: learnedHash } : {}),
      ...(learnedPlan !== undefined ? { planId: learnedPlan } : {}),
      ...(explainDoc ? { explain: explainDoc } : {}),
    };
  }

  /** Build + fire one slice request, streaming records when a hook wants them. */
  private async streamSlice(
    slice: SliceName,
    mode: RunMode,
    doc: QueryDoc<unknown>,
    expanded: QueryDoc<unknown>,
    canonical: string,
    vars: Vars,
    options: RunOptions,
    hash: string | undefined,
    hooks: RunHooks,
  ): Promise<SliceResult> {
    const common = this.commonParams(doc, vars, slice);
    const qs = (pairs: Array<[string, string]>): string =>
      pairs.map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(v)}`).join("&");
    const authHeaders = this.authHeaders(slice, options.noCache);

    let method: string;
    let url: string;
    let init: RequestInit;
    const canInline = mode === "inline" && typeof CompressionStream !== "undefined";
    if (mode === "hash" && hash) {
      method = "GET";
      url = `${this.base}/q/${encodeURIComponent(hash)}?${qs(common)}`;
      init = { headers: authHeaders };
    } else if (canInline) {
      const d = b64url(await deflateRaw(canonical));
      method = "GET";
      url = `${this.base}/q?${qs([["d", d], ...common])}`;
      init = { headers: authHeaders };
    } else {
      const intent = mode === "oneshot" ? "oneshot" : "introduce";
      method = "POST";
      url = `${this.base}/q?${qs([["intent", intent], ...common])}`;
      init = { method: "POST", headers: { ...authHeaders, "content-type": QUERY_MEDIA_TYPE }, body: canonical };
    }
    const request = { method, url, canonicalText: canonical };

    const requestStart = now();
    const resp = await this.fetchFn(url, init);
    const responseStart = now();
    const headers = headersOf(resp);
    const cache = classifyCache(method, headers);
    hooks.onHead?.({ slice, status: resp.status, ok: resp.ok, headers, cache, request });

    if (!resp.ok) {
      const raw = await resp.text();
      const responseEnd = now();
      return {
        slice,
        ok: false,
        status: resp.status,
        request,
        headers,
        cache,
        records: [],
        data: {},
        errors: [],
        complete: false,
        refs: [],
        durationMs: responseEnd - requestStart,
        timing: { requestStart, responseStart, responseEnd },
        problem: problemOf(raw),
        raw,
      };
    }

    let firstRecord: number | undefined;
    const records: LatticeRecord[] = [];
    let raw = "";
    const body = resp.body;
    const canStream = hooks.onRecord !== undefined && body !== null && typeof body.tee === "function";
    if (canStream && body) {
      const [a, b] = body.tee();
      const rawPromise = new Response(a).text();
      const liveStore = new LatticeStore();
      let liveManifest: ManifestRecord | undefined;
      let index = 0;
      for await (const rec of readRecords(new Response(b))) {
        if (firstRecord === undefined) firstRecord = now();
        records.push(rec);
        liveStore.applyRecords([rec]);
        if (rec.kind === "manifest") liveManifest = rec;
        let data: QueryData = {};
        if (liveManifest) {
          const liveRefs = new Set<Ref>();
          data = denormalize(expanded, vars, liveStore, liveManifest, liveRefs) as QueryData;
        }
        hooks.onRecord?.({ slice, record: rec, index: index++, data });
      }
      raw = await rawPromise;
    } else {
      raw = await resp.text();
      for (const rec of recordsOfText(raw)) records.push(rec);
    }

    const store = new LatticeStore();
    const outcome = store.applyRecords(records);
    const refs = new Set<Ref>();
    const data = denormalize(expanded, vars, store, outcome.manifest, refs) as QueryData;
    const learned = mode === "oneshot" ? {} : learnFrom(headers);

    const responseEnd = now();
    return {
      slice,
      ok: true,
      status: resp.status,
      request,
      headers,
      cache,
      records,
      ...(outcome.manifest ? { manifest: outcome.manifest } : {}),
      data,
      errors: outcome.errors,
      complete: outcome.end?.complete ?? false,
      refs: [...refs],
      durationMs: responseEnd - requestStart,
      timing: { requestStart, responseStart, ...(firstRecord !== undefined ? { firstRecord } : {}), responseEnd },
      ...(learned.hash !== undefined ? { hash: learned.hash } : {}),
      ...(learned.planId !== undefined ? { planId: learned.planId } : {}),
      raw,
    };
  }

  /** Fetch the `explain` document for a memoized query hash (§20.2). */
  async explain(hash: string, planId?: string): Promise<ExplainDoc> {
    const p = planId ? `?p=${encodeURIComponent(planId)}` : "";
    const resp = await this.fetchFn(`${this.base}/q/${encodeURIComponent(hash)}/explain${p}`, {
      headers: this.authHeaders(),
    });
    if (!resp.ok) throw new Error(`explain failed: ${resp.status}`);
    return (await resp.json()) as ExplainDoc;
  }

  /** Invoke a named mutation (`POST /m/{name}`), capturing the record stream. */
  async runMutation(name: string, input: unknown, idempotencyKey?: string): Promise<MutationResult> {
    const url = `${this.base}/m/${encodeURIComponent(name)}`;
    const headers: Record<string, string> = { "content-type": "application/json", ...this.authHeaders() };
    if (idempotencyKey) headers[HEADERS.idempotencyKey] = idempotencyKey;
    const resp = await this.fetchFn(url, { method: "POST", headers, body: JSON.stringify(input ?? {}) });
    const raw = await resp.text();
    const runHeaders = headersOf(resp);
    if (!resp.ok) {
      return {
        ok: false,
        status: resp.status,
        request: { method: "POST", url },
        headers: runHeaders,
        records: [],
        errors: [],
        committed: false,
        invalidatedKeys: [],
        problem: problemOf(raw),
        raw,
      };
    }
    const records = [...recordsOfText(raw)];
    const store = new LatticeStore();
    const outcome = store.applyRecords(records);
    return {
      ok: true,
      status: resp.status,
      request: { method: "POST", url },
      headers: runHeaders,
      records,
      errors: outcome.errors,
      committed: outcome.committed,
      invalidatedKeys: outcome.invalidated,
      raw,
    };
  }

  /**
   * Check a candidate IDL against the origin's deployment log (§17.3).
   * `mode` is a compatibility direction, e.g. `client-backward`.
   */
  async checkIdl(idl: string, mode = "client-backward"): Promise<CheckReport> {
    const url = `${this.base}/schema/check?mode=${encodeURIComponent(mode)}`;
    const resp = await this.fetchFn(url, {
      method: "POST",
      headers: { "content-type": "application/x-lattice-idl" },
      body: idl,
    });
    const raw = await resp.text();
    let parsed: Record<string, unknown> | undefined;
    try {
      parsed = JSON.parse(raw) as Record<string, unknown>;
    } catch {
      parsed = undefined;
    }
    if (!resp.ok) {
      return { status: resp.status, ok: false, problem: problemOf(raw) };
    }
    return { status: resp.status, ok: true, ...(parsed ? { report: parsed } : {}) };
  }
}

type Vars = Readonly<Record<string, unknown>>;

function now(): number {
  return typeof performance !== "undefined" ? performance.now() : Date.now();
}

function problemOf(raw: string): ProblemDetails {
  try {
    return JSON.parse(raw) as ProblemDetails;
  } catch {
    return { detail: raw.slice(0, 500) };
  }
}
