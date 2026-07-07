import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { describe, expect, it } from "vitest";
import { ExplorerSession } from "./session.ts";

const here = dirname(fileURLToPath(import.meta.url));
const fixtures = join(here, "..", "..", "..", "wireform-lattice", "test", "fixtures");
const STARWARS_IDL = readFileSync(join(fixtures, "starwars.lattice"), "utf8");

const SCHEMA_HASH = "s3Zuy444";
const DISCOVERY = {
  endpoints: { query: "/q", mutation: "/m", entity: "/e", schema: "/schema" },
  schema: { current: `/schema/${SCHEMA_HASH}` },
  admission: "open",
  queryMediaType: "application/x-lattice-query",
  budgets: { maxDepth: 12, maxRoots: 8, coalesceWindowMs: 5 },
};

const HERO_NDJSON =
  `{"etag":"m:AA","kind":"manifest","plan":"pl_1","query":"qh_1","root":{"hero":["Droid:2001"]},"slice":"pub"}\n` +
  `{"fields":{"name":"R2-D2"},"id":"Droid:2001","kind":"entity","ver":"e1"}\n` +
  `{"complete":true,"etag":"m:AA","kind":"end"}\n`;

const EXPLAIN = {
  plan: "pl_1",
  query: "qh_1",
  elements: [{ path: "hero", derivation: "hero@root:Public = Public", slice: "pub" }],
  rounds: [{ round: 0, loaders: [{ loader: "hero", fanout: 1 }] }],
  surrogateKeys: [],
  budgets: { roots: { used: 1, limit: 8 }, depth: { used: 1, limit: 12 } },
};

interface RecordedRequest {
  method: string;
  url: string;
  body?: string;
  headers: Record<string, string>;
}

function mockOrigin(config: { replay?: Record<string, string> } = {}): { fetch: (url: string, init?: RequestInit) => Promise<Response>; requests: RecordedRequest[] } {
  const requests: RecordedRequest[] = [];
  const fetchFn = async (rawUrl: string, init: RequestInit = {}): Promise<Response> => {
    const method = (init.method ?? "GET").toUpperCase();
    const url = new URL(rawUrl, "http://origin");
    const headers: Record<string, string> = {};
    if (init.headers) for (const [k, v] of Object.entries(init.headers as Record<string, string>)) headers[k.toLowerCase()] = v;
    requests.push({ method, url: rawUrl, ...(typeof init.body === "string" ? { body: init.body } : {}), headers });
    const path = url.pathname;
    const p = url.searchParams;

    if (path === "/.well-known/lattice") {
      return new Response(JSON.stringify(DISCOVERY), { status: 200, headers: { "content-type": "application/json" } });
    }
    if (path === `/schema/${SCHEMA_HASH}`) {
      return new Response(STARWARS_IDL, { status: 200, headers: { "content-type": "text/plain" } });
    }
    if (path === "/q" && method === "POST") {
      const intent = p.get("intent");
      if (intent === "oneshot") {
        return new Response(HERO_NDJSON, {
          status: 200,
          headers: { "content-type": "application/x-ndjson", "cache-control": "no-store", "lattice-plan": "pl_1" },
        });
      }
      // introduce
      const loc = "/q/hh_1?p=pl_1&slice=pub";
      return new Response(HERO_NDJSON, {
        status: 200,
        headers: {
          "content-type": "application/x-ndjson",
          "cache-control": "public, s-maxage=15",
          "lattice-plan": "pl_1",
          "lattice-schema": SCHEMA_HASH,
          "lattice-snapshot": 'main="mem:2"',
          etag: 'W/"m:AA"',
          location: loc,
          "content-location": loc,
          "surrogate-key": "Droid:2001 Human:1002",
        },
      });
    }
    if (path === "/q" && method === "GET" && p.has("d")) {
      const loc = "/q/hh_1?p=pl_1&slice=pub";
      return new Response(HERO_NDJSON, {
        status: 200,
        headers: {
          "content-type": "application/x-ndjson",
          "cache-control": "public, s-maxage=15",
          "lattice-plan": "pl_1",
          location: loc,
          "content-location": loc,
          "surrogate-key": "Droid:2001",
        },
      });
    }
    if (path === "/q/hh_1" && method === "GET") {
      // Hash replay: GET /q/{hash}?slice=... — echo the slice and emit the
      // cache headers each test wants (default: a warm shared-cache HIT).
      const cacheHeaders = config.replay ?? { "cache-control": "public, s-maxage=15", age: "5" };
      return new Response(HERO_NDJSON, {
        status: 200,
        headers: {
          "content-type": "application/x-ndjson",
          "lattice-plan": "pl_1",
          "lattice-slice": p.get("slice") ?? "",
          ...cacheHeaders,
        },
      });
    }
    if (path === "/q/hh_1/explain") {
      return new Response(JSON.stringify(EXPLAIN), { status: 200, headers: { "content-type": "application/json", "lattice-plan": "pl_1" } });
    }
    if (path === "/schema/check" && method === "POST") {
      return new Response(JSON.stringify({ pass: true, changes: [], mode: p.get("mode") }), {
        status: 200,
        headers: { "content-type": "application/json" },
      });
    }
    // default: problem
    return new Response(JSON.stringify({ type: "https://lattice.dev/problems/not-found", title: "lattice:not-found", status: 404 }), {
      status: 404,
      headers: { "content-type": "application/problem+json" },
    });
  };
  return { fetch: fetchFn, requests };
}

describe("ExplorerSession.loadSchema", () => {
  it("follows discovery to the schema document and parses the model", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const loaded = await s.loadSchema();
    expect(loaded.hash).toBe(SCHEMA_HASH);
    expect([...loaded.model.roots.keys()].sort()).toEqual(["hero", "reviews", "search"]);
    expect(loaded.discovery.admission).toBe("open");
    expect(loaded.discovery.budgets?.maxDepth).toBe(12);
  });
});

describe("ExplorerSession.runQuery — introduce", () => {
  it("captures request, headers, records, denormalized data, learned URL, and explain", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const r = await s.runQuery("query Hero { hero { name } }");
    expect(r.ok).toBe(true);
    expect(r.request.method).toBe("POST");
    expect(r.request.canonicalText).toBe("query{hero{name}}");
    expect(r.headers.plan).toBe("pl_1");
    expect(r.headers.surrogateKeys).toEqual(["Droid:2001", "Human:1002"]);
    expect(r.records.map((rec) => rec.kind)).toEqual(["manifest", "entity", "end"]);
    expect(r.manifest?.root).toEqual({ hero: ["Droid:2001"] });
    expect(r.data).toEqual({ hero: [{ __ref: "Droid:2001", name: "R2-D2" }] });
    expect(r.complete).toBe(true);
    expect(r.hash).toBe("hh_1");
    expect(r.planId).toBe("pl_1");
    expect(r.explain?.budgets?.["roots"]).toEqual({ used: 1, limit: 8 });
    // the introduce request carried the query as the body and slice as a param
    const q = requests.find((req) => req.url.includes("intent=introduce"));
    expect(q?.body).toBe("query{hero{name}}");
    expect(q?.url).toContain("slice=pub");
    expect(q?.headers["content-type"]).toBe("application/x-lattice-query");
  });
});

describe("ExplorerSession.runQuery — oneshot", () => {
  it("does not learn a hash or fetch explain, and marks no-store", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const r = await s.runQuery("query { hero { name } }", {}, { mode: "oneshot" });
    expect(r.ok).toBe(true);
    expect(r.hash).toBeUndefined();
    expect(r.explain).toBeUndefined();
    expect(r.headers.cacheControl).toBe("no-store");
    expect(requests.some((req) => req.url.includes("intent=oneshot"))).toBe(true);
    expect(requests.some((req) => req.url.includes("/explain"))).toBe(false);
  });
});

describe("ExplorerSession — claims + variables", () => {
  it("sends the vc payload on ctx and binds non-default variables", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch, claims: { org: 1 } });
    await s.runQuery("query H($episode: Episode) { hero(episode: $episode) { name } }", { episode: "Empire" });
    const q = requests.find((req) => req.url.includes("intent=introduce"))!;
    expect(q.url).toContain("slice=ctx");
    expect(q.url).toContain("vc=");
    expect(q.url).toContain("episode=Empire");
  });
});

describe("ExplorerSession error + checkIdl", () => {
  it("runs the inline (self-contained GET) rung when supported", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const r = await s.runQuery("query { hero { name } }", {}, { mode: "inline" });
    expect(r.ok).toBe(true);
    expect(r.request.method).toBe("GET");
    expect(r.data).toEqual({ hero: [{ __ref: "Droid:2001", name: "R2-D2" }] });
    const g = requests.find((req) => req.method === "GET" && req.url.includes("/q?"))!;
    expect(g.url).toContain("d=");
  });

  it("surfaces an RFC 9457 problem on a failed run", async () => {
    const fetch = async (): Promise<Response> =>
      new Response(
        JSON.stringify({ type: "https://lattice.dev/problems/compile-rejected", title: "lattice:compile-rejected", status: 400, diagnostics: ["bad"] }),
        { status: 400, headers: { "content-type": "application/problem+json" } },
      );
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const r = await s.runQuery("query { hero { name } }");
    expect(r.ok).toBe(false);
    expect(r.status).toBe(400);
    expect(r.problem?.title).toBe("lattice:compile-rejected");
    expect(r.problem?.diagnostics).toEqual(["bad"]);
  });

  it("posts a candidate IDL to /schema/check and returns the report", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const report = await s.checkIdl(STARWARS_IDL, "client-backward");
    expect(report.ok).toBe(true);
    expect(report.report?.["pass"]).toBe(true);
    const req = requests.find((r) => r.url.includes("/schema/check"))!;
    expect(req.url).toContain("mode=client-backward");
    expect(req.headers["content-type"]).toBe("application/x-lattice-idl");
  });
});

const HERO_TEXT = "query { hero { name } }";
const HERO_DATA = { hero: [{ __ref: "Droid:2001", name: "R2-D2" }] };

describe("ExplorerSession.runSlices — shape + concurrency", () => {
  it("returns one keyed result per slice, denormalized, with order preserved", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"]);
    expect(run.order).toEqual(["pub"]);
    expect(Object.keys(run.slices)).toEqual(["pub"]);
    expect(run.slices.pub!.slice).toBe("pub");
    expect(run.slices.pub!.data).toEqual(HERO_DATA);
    expect(run.canonicalText).toBe("query{hero{name}}");
  });

  it("fetches multiple slices concurrently and fetches the shared explain exactly once", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub", "ctx"]);
    expect(Object.keys(run.slices).sort()).toEqual(["ctx", "pub"]);
    expect(run.slices.pub!.ok).toBe(true);
    expect(run.slices.ctx!.ok).toBe(true);
    // both slice requests reached the origin (Promise.all fanned them out)
    expect(requests.some((r) => r.url.includes("slice=pub") && r.url.includes("intent=introduce"))).toBe(true);
    expect(requests.some((r) => r.url.includes("slice=ctx") && r.url.includes("intent=introduce"))).toBe(true);
    // one shared plan ⇒ explain fetched once regardless of slice count
    expect(requests.filter((r) => r.url.includes("/explain")).length).toBe(1);
    expect(run.explain?.plan).toBe("pl_1");
  });
});

describe("ExplorerSession.runSlices — cache classification", () => {
  it("classifies an introduce POST as dynamic despite cacheable response headers", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"]);
    expect(run.slices.pub!.cache.status).toBe("dynamic");
  });

  it("classifies an inline GET (public, s-maxage=15) as cacheable with maxAge 15", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "inline" });
    expect(run.slices.pub!.cache.status).toBe("cacheable");
    expect(run.slices.pub!.cache.maxAge).toBe(15);
    expect(run.slices.pub!.cache.shared).toBe(true);
  });

  it("classifies a replay GET carrying a positive Age as a shared-cache hit", async () => {
    const { fetch } = mockOrigin(); // default replay: public s-maxage=15 + age 5
    const s = new ExplorerSession({ base: "http://origin", fetch });
    await s.runSlices(HERO_TEXT, {}, ["pub"]); // introduce → learns hh_1
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "hash" });
    expect(run.slices.pub!.cache.status).toBe("hit");
    expect(run.slices.pub!.cache.age).toBe(5);
  });

  it("classifies a private, max-age GET as private (browser-only)", async () => {
    const { fetch } = mockOrigin({ replay: { "cache-control": "private, max-age=30" } });
    const s = new ExplorerSession({ base: "http://origin", fetch });
    await s.runSlices(HERO_TEXT, {}, ["pub"]);
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "hash" });
    expect(run.slices.pub!.cache.status).toBe("private");
    expect(run.slices.pub!.cache.maxAge).toBe(30);
    expect(run.slices.pub!.cache.shared).toBe(false);
  });

  it("lets a cf-cache-status HIT header win over the cacheable inference", async () => {
    const { fetch } = mockOrigin({ replay: { "cache-control": "public, s-maxage=15", "cf-cache-status": "HIT" } });
    const s = new ExplorerSession({ base: "http://origin", fetch });
    await s.runSlices(HERO_TEXT, {}, ["pub"]);
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "hash" });
    // with no Age the inference would be "cacheable"; the CDN header drives it to hit
    expect(run.slices.pub!.cache.status).toBe("hit");
  });
});

describe("ExplorerSession.runSlices — hash replay", () => {
  it("replays a learned hash via GET /q/{hash} with no intent param", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const first = await s.runSlices(HERO_TEXT, {}, ["pub"]); // introduce learns hh_1
    expect(first.hash).toBe("hh_1");
    const replay = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "hash" });
    expect(replay.autoIntroduced).toBe(false);
    const g = requests.find((r) => r.method === "GET" && r.url.includes("/q/hh_1?"))!;
    expect(g.url).toContain("slice=pub");
    expect(g.url).not.toContain("intent=");
    expect(replay.slices.pub!.request.method).toBe("GET");
    expect(replay.slices.pub!.data).toEqual(HERO_DATA);
  });

  it("transparently introduces when a hash-mode run has no learned hash", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "hash" });
    expect(run.autoIntroduced).toBe(true);
    expect(requests.some((r) => r.method === "POST" && r.url.includes("intent=introduce"))).toBe(true);
    expect(requests.some((r) => r.url.includes("/q/hh_1?"))).toBe(false); // no replay GET issued
    expect(run.slices.pub!.ok).toBe(true);
  });
});

describe("ExplorerSession.runSlices — streaming", () => {
  it("delivers records incrementally via onRecord, denormalizing progressively", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const events: Array<{ index: number; kind: string; data: unknown }> = [];
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], {}, {
      onRecord: (ev) => events.push({ index: ev.index, kind: ev.record.kind, data: ev.data }),
    });
    expect(events.length).toBeGreaterThanOrEqual(1);
    expect(events.map((e) => e.index)).toEqual([0, 1, 2]);
    expect(events.map((e) => e.kind)).toEqual(["manifest", "entity", "end"]);
    // the manifest snapshot has no materialized entity yet; the entity record fills it in
    expect(events[0]!.data).toEqual({ hero: [] });
    expect(events[1]!.data).toEqual(HERO_DATA);
    // the buffered result equals what streamed in
    expect(run.slices.pub!.records.map((r) => r.kind)).toEqual(["manifest", "entity", "end"]);
    expect(run.slices.pub!.data).toEqual(HERO_DATA);
  });
});

describe("ExplorerSession.runSlices — per-slice fault tolerance", () => {
  it("isolates one slice's network failure while siblings succeed", async () => {
    const { fetch } = mockOrigin();
    const faulty = async (url: string, init?: RequestInit): Promise<Response> => {
      if (url.includes("slice=ctx")) throw new Error("connection reset");
      return fetch(url, init);
    };
    const s = new ExplorerSession({ base: "http://origin", fetch: faulty });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub", "ctx"]);
    expect(run.slices.pub!.ok).toBe(true);
    expect(run.slices.pub!.data).toEqual(HERO_DATA);
    expect(run.slices.ctx!.ok).toBe(false);
    expect(run.slices.ctx!.status).toBe(0);
    expect(run.slices.ctx!.problem?.title).toBe("lattice:network");
  });
});

describe("ExplorerSession.runSlices — auth params + headers", () => {
  it("adds the vc payload only on the ctx slice when claims are set", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch, claims: { org: 1 } });
    await s.runSlices(HERO_TEXT, {}, ["pub", "ctx"]);
    const ctx = requests.find((r) => r.url.includes("slice=ctx"))!;
    const pub = requests.find((r) => r.url.includes("slice=pub"))!;
    expect(ctx.url).toContain("slice=ctx");
    expect(ctx.url).toContain("vc=");
    expect(pub.url).not.toContain("vc=");
  });

  it("sends the bearer token only on the priv slice, never on pub/ctx", async () => {
    const { fetch, requests } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch, authToken: "tok_abc" });
    await s.runSlices(HERO_TEXT, {}, ["pub", "ctx", "priv"]);
    const priv = requests.find((r) => r.url.includes("slice=priv"))!;
    const pub = requests.find((r) => r.url.includes("slice=pub"))!;
    const ctx = requests.find((r) => r.url.includes("slice=ctx"))!;
    expect(priv.headers["authorization"]).toMatch(/^Bearer /);
    expect(pub.headers["authorization"]).toBeUndefined();
    expect(ctx.headers["authorization"]).toBeUndefined();
  });
});

describe("ExplorerSession timing / trace", () => {
  it("emits monotonic per-slice lifecycle marks consistent with durationMs", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"]);
    const r = run.slices.pub!;
    const t = r.timing;
    expect(t.requestStart).toBeGreaterThanOrEqual(0);
    expect(t.responseStart).toBeGreaterThanOrEqual(t.requestStart);
    expect(t.responseEnd).toBeGreaterThanOrEqual(t.responseStart);
    // durationMs is derived from the same two marks — equal to within float noise.
    expect(Math.abs(r.durationMs - (t.responseEnd - t.requestStart))).toBeLessThan(1);
  });

  it("sets firstRecord on the streaming path, bracketed by responseStart/responseEnd", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    // mirror the existing streaming test's invocation: hooks as the 5th arg.
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], {}, { onRecord() {} });
    const t = run.slices.pub!.timing;
    expect(typeof t.firstRecord).toBe("number");
    expect(t.firstRecord!).toBeGreaterThanOrEqual(t.responseStart);
    expect(t.firstRecord!).toBeLessThanOrEqual(t.responseEnd);
  });

  it("leaves firstRecord undefined on the buffered (no-hooks) path", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const r = await s.runQuery(HERO_TEXT);
    expect(r.timing.firstRecord).toBeUndefined();
  });

  it("brackets every slice's marks within the run axis (startedAt..finishedAt)", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch, claims: { org: 1 }, authToken: "tok_abc" });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub", "ctx", "priv"]);
    for (const name of run.order) expect(run.slices[name]!.ok).toBe(true);
    const starts = run.order.map((sl) => run.slices[sl]!.timing.requestStart);
    const ends = run.order.map((sl) => run.slices[sl]!.timing.responseEnd);
    expect(run.startedAt).toBeLessThanOrEqual(Math.min(...starts));
    expect(run.finishedAt).toBeGreaterThanOrEqual(Math.max(...ends));
    expect(run.startedAt).toBeLessThanOrEqual(run.finishedAt);
  });

  it("records explainTiming for an introduce run that learns a hash", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"]);
    expect(run.hash).toBe("hh_1");
    expect(run.slices.pub!.ok).toBe(true);
    expect(run.explainTiming).toBeDefined();
    const et = run.explainTiming!;
    expect(et.requestStart).toBeLessThanOrEqual(et.responseEnd);
    expect(run.startedAt).toBeLessThanOrEqual(et.requestStart);
    expect(et.responseEnd).toBeLessThanOrEqual(run.finishedAt);
  });

  it("makes no explain fetch in oneshot mode, leaving explainTiming undefined", async () => {
    const { fetch } = mockOrigin();
    const s = new ExplorerSession({ base: "http://origin", fetch });
    const run = await s.runSlices(HERO_TEXT, {}, ["pub"], { mode: "oneshot" });
    expect(run.explainTiming).toBeUndefined();
  });
});
