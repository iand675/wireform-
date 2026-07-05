/**
 * An in-browser mock Lattice origin for the Star Wars dashboard, used when
 * the real demo origin (localhost:8917) is not running. Enable with
 * `VITE_LATTICE_MOCK=1` or by adding `?mock` to the page URL.
 *
 * It is a miniature but honest origin: it decodes the inline `d=` form with
 * `DecompressionStream`, memoizes introduced query texts and grants
 * `Location` + `Lattice-Plan` (so the client's ladder upgrades to hash-form
 * GETs), evaluates parsed selections against an in-memory graph, streams
 * NDJSON entity records deduplicated by ref, emits `Surrogate-Key` headers,
 * and answers `createReview` with an entity stream plus an `invalidated`
 * record — which is what drives the reviews panel's automatic refetch.
 */

import type { FieldSel, QueryDoc, Ref, Selection } from "../../src/index.ts";
import { RESERVED_PARAMS, canonicalFieldKey, parse, parseRef, valueToJson } from "../../src/index.ts";

interface Row {
  readonly ref: Ref;
  readonly fields: Record<string, unknown>;
  readonly friends?: readonly Ref[];
}

const characters: Row[] = [
  {
    ref: "Human:1000",
    fields: { id: "1000", name: "Luke Skywalker", homePlanet: "Tatooine", appearsIn: ["NewHope", "Empire", "Jedi"] },
    friends: ["Human:1002", "Human:1003", "Droid:2000", "Droid:2001"],
  },
  {
    ref: "Human:1002",
    fields: { id: "1002", name: "Han Solo", homePlanet: "Corellia", appearsIn: ["NewHope", "Empire", "Jedi"] },
    friends: ["Human:1000", "Human:1003", "Droid:2001"],
  },
  {
    ref: "Human:1003",
    fields: { id: "1003", name: "Leia Organa", homePlanet: "Alderaan", appearsIn: ["NewHope", "Empire", "Jedi"] },
    friends: ["Human:1000", "Human:1002", "Droid:2000", "Droid:2001"],
  },
  {
    ref: "Droid:2000",
    fields: { id: "2000", name: "C-3PO", primaryFunction: "Protocol", appearsIn: ["NewHope", "Empire", "Jedi"] },
    friends: ["Human:1000", "Human:1002", "Human:1003", "Droid:2001"],
  },
  {
    ref: "Droid:2001",
    fields: { id: "2001", name: "R2-D2", primaryFunction: "Astromech", appearsIn: ["NewHope", "Empire", "Jedi"] },
    friends: ["Human:1000", "Human:1002", "Human:1003"],
  },
  { ref: "Starship:3000", fields: { id: "3000", name: "Millennium Falcon", length: 34.37 } },
  { ref: "Starship:3001", fields: { id: "3001", name: "X-Wing", length: 12.5 } },
  { ref: "Starship:3003", fields: { id: "3003", name: "Imperial shuttle", length: 20 } },
];

const heroByEpisode: Record<string, Ref> = {
  NewHope: "Droid:2001",
  Empire: "Human:1000",
  Jedi: "Droid:2001",
};

let reviewSeq = 100;
const reviews: Row[] = [
  {
    ref: "Review:1",
    fields: { id: "1", episode: "Empire", stars: 5, commentary: "Best of the trilogy.", createdAt: "2026-07-01T10:00:00Z" },
  },
  {
    ref: "Review:2",
    fields: { id: "2", episode: "Empire", stars: 4, commentary: "Dark, in a good way.", createdAt: "2026-07-02T09:30:00Z" },
  },
  {
    ref: "Review:3",
    fields: { id: "3", episode: "Jedi", stars: 3, commentary: "Too many Ewoks.", createdAt: "2026-07-03T18:12:00Z" },
  },
];

const db = new Map<Ref, Row>();
for (const row of characters) db.set(row.ref, row);
const registerReview = (row: Row): void => {
  db.set(row.ref, row);
};
for (const row of reviews) registerReview(row);

// ---------------------------------------------------------------------------
// Selection evaluation

type Vars = Record<string, unknown>;

function boundArgs(field: FieldSel, vars: Vars): Array<readonly [string, unknown]> {
  return field.args.map((a) => [a.name, valueToJson(a.value, vars)] as const);
}

function argValue(field: FieldSel, name: string, vars: Vars): unknown {
  const arg = field.args.find((a) => a.name === name);
  return arg ? valueToJson(arg.value, vars) : undefined;
}

class Evaluation {
  readonly emitted = new Map<Ref, Record<string, unknown>>();
  readonly surrogateKeys = new Set<string>();
  readonly root: Record<string, Ref[]> = {};

  constructor(private readonly vars: Vars) {}

  evalRoot(field: FieldSel): void {
    const selections = field.selections ?? [];
    switch (field.name) {
      case "hero": {
        const episode = argValue(field, "episode", this.vars);
        const ref = (typeof episode === "string" ? heroByEpisode[episode] : undefined) ?? "Droid:2001";
        this.root["hero"] = [ref];
        this.emit(ref, selections);
        break;
      }
      case "reviews": {
        const episode = argValue(field, "episode", this.vars);
        const first = Number(argValue(field, "first", this.vars) ?? 10);
        const matches = reviews
          .filter((r) => episode === undefined || r.fields["episode"] === episode)
          .sort((a, b) => String(b.fields["createdAt"]).localeCompare(String(a.fields["createdAt"])))
          .slice(0, first);
        this.root["reviews"] = matches.map((r) => r.ref);
        for (const row of matches) this.emit(row.ref, selections);
        this.surrogateKeys.add(`reviews:${String(episode ?? "*")}`);
        break;
      }
      case "search": {
        const text = String(argValue(field, "text", this.vars) ?? "").toLowerCase();
        const first = Number(argValue(field, "first", this.vars) ?? 10);
        const matches = [...db.values()]
          .filter((r) => !r.ref.startsWith("Review:") && String(r.fields["name"]).toLowerCase().includes(text))
          .sort((a, b) => String(a.fields["name"]).localeCompare(String(b.fields["name"])))
          .slice(0, first);
        this.root["search"] = matches.map((r) => r.ref);
        for (const row of matches) this.emit(row.ref, selections);
        this.surrogateKeys.add(`search:${text}`);
        break;
      }
      default:
        throw new Error(`mock origin: unknown root ${JSON.stringify(field.name)}`);
    }
  }

  private emit(ref: Ref, selections: readonly Selection[]): void {
    const row = db.get(ref);
    if (!row) return;
    this.surrogateKeys.add(ref);
    let acc = this.emitted.get(ref);
    if (!acc) {
      acc = {};
      this.emitted.set(ref, acc);
    }
    this.project(row, acc, selections);
  }

  private project(row: Row, acc: Record<string, unknown>, selections: readonly Selection[]): void {
    for (const sel of selections) {
      switch (sel.kind) {
        case "field": {
          const key = canonicalFieldKey(sel.name, boundArgs(sel, this.vars));
          if (sel.name === "friends") {
            const first = Number(argValue(sel, "first", this.vars) ?? 10);
            const all = row.friends ?? [];
            const page = all.slice(0, first);
            acc[key] = {
              $page: {
                items: page.map((r) => ({ $ref: r })),
                next: all.length > first ? `cur_mock_${row.ref}` : null,
                prev: null,
              },
            };
            for (const friend of page) this.emit(friend, sel.selections ?? []);
          } else {
            acc[key] = row.fields[sel.name] ?? null;
          }
          break;
        }
        case "inline": {
          if (parseRef(row.ref)[0] === sel.on) this.project(row, acc, sel.selections);
          break;
        }
        case "spread":
          break; // schema fragments: none in the demo schema the mock serves
      }
    }
  }
}

// ---------------------------------------------------------------------------
// HTTP surface

const memo = new Map<string, string>();
let hashSeq = 0;

function ndjson(records: unknown[], headers: Record<string, string> = {}, status = 200): Response {
  return new Response(records.map((r) => JSON.stringify(r)).join("\n") + "\n", {
    status,
    headers: { "content-type": "application/x-ndjson", ...headers },
  });
}

function problem(status: number, type: string): Response {
  return new Response(JSON.stringify({ type, status }), {
    status,
    headers: { "content-type": "application/problem+json", "cache-control": "no-store" },
  });
}

function queryResponse(text: string, params: URLSearchParams, extraHeaders: Record<string, string>): Response {
  let doc: QueryDoc;
  try {
    doc = parse(text);
  } catch {
    return problem(400, "lattice:compile-rejected");
  }
  const vars: Record<string, unknown> = {};
  for (const [name, raw] of params) {
    if (RESERVED_PARAMS[name]) continue;
    vars[name] = /^-?[0-9]+(\.[0-9]+)?$/.test(raw) ? Number(raw) : raw;
  }
  for (const v of doc.variables) {
    if (vars[v.name] === undefined && v.default !== undefined) vars[v.name] = valueToJson(v.default, {});
  }
  const evaluation = new Evaluation(vars);
  try {
    for (const sel of doc.selections) {
      if (sel.kind === "field") evaluation.evalRoot(sel);
    }
  } catch (e) {
    return problem(400, `lattice:compile-rejected#${String(e)}`);
  }
  const etag = `m:mock${(hashSeq += 1)}`;
  const records: unknown[] = [
    { kind: "manifest", query: "mock", plan: "pl_mock", slice: params.get("slice") ?? "pub", root: evaluation.root, etag },
  ];
  for (const [id, fields] of evaluation.emitted) {
    records.push({ kind: "entity", id, ver: `v${db.get(id) ? "1" : "0"}-${id}`, fields });
  }
  records.push({ kind: "end", complete: true, etag });
  return ndjson(records, {
    "surrogate-key": [...evaluation.surrogateKeys, "plan:pl_mock"].join(" "),
    "lattice-plan": "pl_mock",
    ...extraHeaders,
  });
}

async function inflateParam(d: string): Promise<string> {
  const b64 = d.replace(/-/g, "+").replace(/_/g, "/");
  const bytes = Uint8Array.from(atob(b64), (c) => c.charCodeAt(0));
  const ds = new DecompressionStream("deflate-raw");
  return await new Response(new Blob([bytes]).stream().pipeThrough(ds)).text();
}

function introduce(text: string): string {
  for (const [hash, known] of memo) {
    if (known === text) return hash;
  }
  const hash = `mock${memo.size + 1}`;
  memo.set(hash, text);
  return hash;
}

function createReview(input: Record<string, unknown>): Response {
  reviewSeq += 1;
  const id = String(reviewSeq);
  const row: Row = {
    ref: `Review:${id}`,
    fields: {
      id,
      episode: input["episode"] ?? "Empire",
      stars: input["stars"] ?? 0,
      commentary: input["commentary"] ?? null,
      createdAt: new Date().toISOString(),
    },
  };
  reviews.push(row);
  registerReview(row);
  return ndjson(
    [
      { kind: "manifest", mutation: "createReview", root: { result: [row.ref] }, etag: "m:mut" },
      { kind: "entity", id: row.ref, ver: `v1-${row.ref}`, fields: row.fields },
      { kind: "invalidated", keys: [row.ref, `reviews:${String(row.fields["episode"])}`] },
      { kind: "end", complete: true },
    ],
    { "cache-control": "no-store" },
  );
}

/** A `fetch`-compatible function implementing the mock origin. */
export function createMockFetch(): (url: string, init?: RequestInit) => Promise<Response> {
  return async (url, init = {}) => {
    const u = new URL(url);
    const method = init.method ?? "GET";

    if (u.pathname === "/q" && method === "GET" && u.searchParams.has("d")) {
      const text = await inflateParam(u.searchParams.get("d")!);
      const hash = introduce(text);
      return queryResponse(text, u.searchParams, { location: `/q/${hash}?p=pl_mock` });
    }
    if (u.pathname === "/q" && method === "POST" && u.searchParams.get("intent") === "introduce") {
      const text = String(init.body ?? "");
      const hash = introduce(text);
      return queryResponse(text, u.searchParams, { location: `/q/${hash}?p=pl_mock` });
    }
    if (u.pathname.startsWith("/q/") && method === "GET") {
      const text = memo.get(u.pathname.slice("/q/".length));
      if (text === undefined) return problem(404, "lattice:unknown-query");
      return queryResponse(text, u.searchParams, {});
    }
    if (u.pathname === "/m/createReview" && method === "POST") {
      const input = JSON.parse(String(init.body ?? "{}")) as Record<string, unknown>;
      return createReview(input);
    }
    return problem(404, "lattice:not-found");
  };
}
