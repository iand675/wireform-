/**
 * LatticeClient: the transport ladder (spec §6) against a scripted fetch
 * stub, and `denormalize` (store → plain trees) semantics.
 *
 * Ladder shape under test: rung 2 (inline `?d=` GET; CompressionStream
 * exists on Node) → rung 3 (POST introduce, learns the granted `/q/{hash}`
 * URL from `Location`) → rung 1 (hash GET) on subsequent queries, with 404
 * on the hash form forgetting the memo and re-introducing.
 */

import { describe, expect, it } from "vitest";
import { LatticeQueryError, canonicalize, parse } from "./canonical.ts";
import type { FetchLike, PageTree } from "./index.ts";
import { LatticeClient, denormalize } from "./client.ts";
import { LatticeStore } from "./store.ts";
import type { ManifestRecord } from "./wire.ts";
import { QUERY_MEDIA_TYPE, recordsOfText } from "./wire.ts";

const HERO_QUERY = "query { hero { name friends(first: 2) { name } } }";

const HERO_BODY =
  '{"kind":"manifest","query":"qh_abc","slice":"pub","root":{"hero":["Droid:2001"]},"etag":"m:e1"}\n' +
  '{"kind":"entity","id":"Droid:2001","ver":"f10","fields":{"name":"R2-D2","friends(first:2)":{"$page":{"items":[{"$ref":"Human:1000"},{"$ref":"Human:1003"}],"next":"cur_n","prev":null}}}}\n' +
  '{"kind":"entity","id":"Human:1000","ver":"a01","fields":{"name":"Luke Skywalker"}}\n' +
  '{"kind":"entity","id":"Human:1003","ver":"c19","fields":{"name":"Leia Organa"}}\n' +
  '{"kind":"end","complete":true,"etag":"m:e1"}\n';

const ndjson = (headers: Record<string, string> = {}): Response =>
  new Response(HERO_BODY, { status: 200, headers: { "content-type": "application/x-ndjson", ...headers } });

/** Classify a stubbed request by ladder rung. */
function rungOf(method: string, u: URL): string {
  if (u.pathname === "/q" && u.searchParams.get("intent") === "introduce") return `${method} introduce`;
  if (u.pathname === "/q" && u.searchParams.has("d")) return `${method} inline`;
  if (u.pathname.startsWith("/q/")) return `${method} ${u.pathname}`;
  return `${method} ${u.pathname}`;
}

interface StubLog {
  readonly rungs: string[];
  readonly urls: URL[];
  readonly inits: Array<RequestInit | undefined>;
}

function stubFetch(handler: (rung: string, u: URL) => Response): [FetchLike, StubLog] {
  const log: StubLog = { rungs: [], urls: [], inits: [] };
  const fetchFn: FetchLike = (url, init) => {
    const u = new URL(url);
    const rung = rungOf(init?.method ?? "GET", u);
    log.rungs.push(rung);
    log.urls.push(u);
    log.inits.push(init);
    return Promise.resolve(handler(rung, u));
  };
  return [fetchFn, log];
}

describe("transport ladder (§6)", () => {
  it("inline 404 → POST introduce learns Location → hash GET; 404 on hash re-introduces", async () => {
    let generation = 1;
    const [fetchFn, log] = stubFetch((rung, u) => {
      if (rung === "GET inline") return new Response("no inline", { status: 404 });
      if (rung === "POST introduce") {
        return ndjson({ location: `https://api.test/q/qh_gen${generation}?p=pl_${generation}` });
      }
      if (u.pathname === `/q/qh_gen${generation}`) return ndjson();
      return new Response("stale hash", { status: 404 }); // an evicted memo
    });
    const events: string[] = [];
    const client = new LatticeClient({
      base: "https://api.test",
      fetch: fetchFn,
      onRequest: (e) => events.push(e.kind),
    });
    const doc = parse(HERO_QUERY);

    // First query climbs: inline (404) → introduce (learns qh_gen1).
    const r1 = await client.query(doc);
    expect(log.rungs).toEqual(["GET inline", "POST introduce"]);
    // The introduction POSTs the canonical text under the query media type.
    const introduceInit = log.inits[1]!;
    expect(introduceInit.body).toBe(canonicalize(doc));
    expect((introduceInit.headers as Record<string, string>)["content-type"]).toBe(QUERY_MEDIA_TYPE);

    // Second query rides the learned hash URL directly, with the plan pin.
    const r2 = await client.query(doc);
    expect(log.rungs).toEqual(["GET inline", "POST introduce", "GET /q/qh_gen1"]);
    const hashUrl = log.urls[2]!;
    expect(hashUrl.searchParams.get("p")).toBe("pl_1");
    expect(hashUrl.searchParams.get("slice")).toBe("pub");

    // Evict the memo server-side: hash GET 404s → forget, fall down the
    // ladder (inline 404) → re-introduce → learn the new grant.
    generation = 2;
    await client.query(doc);
    expect(log.rungs.slice(3)).toEqual(["GET /q/qh_gen1", "GET inline", "POST introduce"]);
    await client.query(doc);
    expect(log.rungs[6]).toBe("GET /q/qh_gen2");

    expect(events).toEqual(["inline", "introduce", "hash", "hash", "inline", "introduce", "hash"]);

    // Both responses denormalized identically through the store.
    for (const r of [r1, r2]) {
      const hero = r.data["hero"]![0]!;
      expect(hero["__ref"]).toBe("Droid:2001");
      expect(hero["name"]).toBe("R2-D2");
      const friends = hero["friends"] as PageTree;
      expect(friends.items.map((i) => i["name"])).toEqual(["Luke Skywalker", "Leia Organa"]);
      expect(friends.next).toBe("cur_n");
      expect(friends.prev).toBeNull();
      expect(r.complete).toBe(true);
      expect(r.errors).toEqual([]);
      expect(r.refs.has("Droid:2001")).toBe(true);
      expect(r.refs.has("Human:1000")).toBe(true);
    }
  });

  it("a direct inline hit still learns the hash URL from Content-Location", async () => {
    const [fetchFn, log] = stubFetch((rung) => {
      if (rung === "GET inline") return ndjson({ "content-location": "/q/qh_direct" });
      return ndjson();
    });
    const client = new LatticeClient({ base: "https://api.test", fetch: fetchFn });
    const doc = parse(HERO_QUERY);

    await client.query(doc);
    await client.query(doc);
    expect(log.rungs).toEqual(["GET inline", "GET /q/qh_direct"]);
    // No plan id was granted; the learned URL carries only slice + vars.
    expect(log.urls[1]!.searchParams.get("p")).toBeNull();
  });

  it("rejects variable names colliding with reserved URL params before any request", () => {
    // Since the Draft 27 rule landed in the parser, the rejection happens at
    // parse time — no client, no fetch, nothing on the wire. The client-side
    // variableParams guard remains as defense in depth for hand-built docs.
    expect(() => parse("query($slice: Text) { hero(kind: $slice) { name } }")).toThrow(
      /reserved URL parameter/,
    );
  });

  it("rejects bound-but-undeclared and missing-required variables before any request", async () => {
    const [fetchFn, log] = stubFetch(() => ndjson());
    const client = new LatticeClient({ base: "https://api.test", fetch: fetchFn });

    await expect(client.query(parse(HERO_QUERY), { nope: 1 })).rejects.toThrow(
      /bound but not declared/,
    );
    const needsVar = parse("query($ep: Episode) { hero(episode: $ep) { name } }");
    await expect(client.query(needsVar, {})).rejects.toThrow(/no binding and no default/);
    expect(log.rungs).toEqual([]);
  });

  it("declared defaults are omitted from the URL; other variables are bound (§6.1)", async () => {
    const [fetchFn, log] = stubFetch(() => ndjson());
    const client = new LatticeClient({ base: "https://api.test", fetch: fetchFn });
    const doc = parse("query($first: I32 = 2, $ep: Episode?) { hero(episode: $ep, page: $first) { name } }");

    await client.query(doc, { first: 2, ep: "EMPIRE" });
    const u = log.urls[0]!;
    expect(u.searchParams.get("ep")).toBe("EMPIRE");
    expect(u.searchParams.get("first")).toBeNull();
  });
});

describe("denormalize", () => {
  function populatedStore(): LatticeStore {
    const store = new LatticeStore();
    const body =
      '{"kind":"entity","id":"Droid:2001","ver":"f10","fields":{"name":"R2-D2","avatarUrl(size:48)":"https://img/48.png","episodes":["Episode:4","Episode:5"],"friends(first:2)":{"$page":{"items":[{"$ref":"Human:1000"},{"$ref":"Human:1003"}],"next":"cur_n","prev":null,"total":2}}}}\n' +
      '{"kind":"entity","id":"Human:1000","ver":"a01","fields":{"name":"Luke Skywalker"}}\n' +
      '{"kind":"entity","id":"Human:1003","ver":"c19","fields":{"name":"Leia Organa"}}\n' +
      '{"kind":"entity","id":"Episode:4","ver":"e1","fields":{"title":"A New Hope"}}\n' +
      '{"kind":"entity","id":"Episode:5","ver":"e2","fields":{"title":"The Empire Strikes Back"}}\n';
    store.applyRecords(recordsOfText(body));
    return store;
  }

  const manifest: ManifestRecord = { kind: "manifest", root: { hero: ["Droid:2001"] } };

  it("builds page trees, bounded ref arrays, and parameterized field keys", () => {
    const store = populatedStore();
    const doc = parse(
      "query($size: I32) { hero { name avatarUrl(size: $size) friends(first: 2) { name } episodes { title } } }",
    );
    const refs = new Set<string>();
    const data = denormalize(doc, { size: 48 }, store, manifest, refs);

    const hero = data["hero"]![0]!;
    expect(hero["__ref"]).toBe("Droid:2001");
    expect(hero["name"]).toBe("R2-D2");

    // Parameterized field: the canonical key resolves once $size binds, and
    // the value is exposed under both the bare name and the canonical key.
    expect(hero["avatarUrl"]).toBe("https://img/48.png");
    expect(hero["avatarUrl(size:48)"]).toBe("https://img/48.png");

    // Paginated edge → {items, next, prev, total?}.
    const friends = hero["friends"] as PageTree;
    expect(friends.items.map((i) => i["name"])).toEqual(["Luke Skywalker", "Leia Organa"]);
    expect(friends.next).toBe("cur_n");
    expect(friends.prev).toBeNull();
    expect(friends.total).toBe(2);

    // Bounded collection (plain ref-string array) → array of entity trees.
    const episodes = hero["episodes"] as Array<Record<string, unknown>>;
    expect(episodes.map((e) => e["title"])).toEqual(["A New Hope", "The Empire Strikes Back"]);
    expect(episodes.map((e) => e["__ref"])).toEqual(["Episode:4", "Episode:5"]);

    // Every entity the tree was assembled from is reported.
    expect([...refs].sort()).toEqual([
      "Droid:2001",
      "Episode:4",
      "Episode:5",
      "Human:1000",
      "Human:1003",
    ]);
  });

  it("an unbound parameterized variable with a default still resolves the key", () => {
    const store = populatedStore();
    const doc = parse("query($size: I32 = 48) { hero { avatarUrl(size: $size) } }");
    const data = denormalize(doc, {}, store, manifest);
    expect(data["hero"]![0]!["avatarUrl"]).toBe("https://img/48.png");
  });

  it("falls back to the unique stored key when the schema erased an argument", () => {
    // The client asks for bare `friends`; the store holds `friends(first:2)`
    // — unique for the field name, so the intent is unambiguous (§5.1
    // default erasure, client side).
    const store = populatedStore();
    const doc = parse("query { hero { friends { name } } }");
    const data = denormalize(doc, {}, store, manifest);
    const friends = data["hero"]![0]!["friends"] as PageTree;
    expect(friends.items.map((i) => i["name"])).toEqual(["Luke Skywalker", "Leia Organa"]);
  });

  it("roots absent from the manifest denormalize to undefined", () => {
    const store = populatedStore();
    const doc = parse("query { hero { name } reviews { stars } }");
    const data = denormalize(doc, {}, store, manifest);
    expect(data["hero"]).toHaveLength(1);
    expect(data["reviews"]).toBeUndefined();
  });

  it("missing entities drop out of the tree instead of throwing", () => {
    const store = new LatticeStore();
    store.applyRecords(
      recordsOfText('{"kind":"entity","id":"Droid:2001","ver":"f10","fields":{"name":"R2-D2"}}\n'),
    );
    const doc = parse("query { hero { name friends(first: 2) { name } } }");
    const data = denormalize(doc, {}, store, manifest);
    const hero = data["hero"]![0]!;
    expect(hero["name"]).toBe("R2-D2");
    expect(hero["friends"]).toBeUndefined();
  });
});
