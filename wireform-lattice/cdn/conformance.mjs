#!/usr/bin/env node
// Lattice CDN conformance checker.
//
// Runs the 8-step behavioral suite from the CDN tier design against a
// Lattice origin fronted by a CDN (Varnish or the Cloudflare Worker):
//
//   node conformance.mjs --cdn http://127.0.0.1:6081 \
//                        --origin http://127.0.0.1:8917 \
//                        --coalesce strict|lenient \
//                        [--purge-mode soft|hard] [--timeout SECONDS] \
//                        [--skip 4,7 | --skip coalesce]
//
// Requirements: node >= 18 (global fetch), zero dependencies. The origin
// must be the example-lattice demo (Star Wars dataset) started with
// LATTICE_DEBUG_DELAY_MS set (the coalescing window) and its purge
// forwarder pointed at the CDN under test.
//
// Exit code 0 iff every step passes; a per-step PASS/FAIL line is printed
// as the run progresses and a summary table at the end. `--coalesce
// strict` demands exactly one origin fill for 16 concurrent cold GETs
// (Varnish waitinglist); `lenient` accepts any count < 16 with identical
// bodies (per-isolate single-flight, e.g. the Cloudflare Worker).
// `--purge-mode` only annotates step 5: both modes assert
// eventual-freshness by polling the plain URL; soft purge (Varnish
// xkey.softpurge) may serve stale-while-revalidate hits during the poll,
// hard purge (Worker cache delete) misses straight to the origin.

import { setTimeout as sleep } from "node:timers/promises";
import { createHash, randomUUID } from "node:crypto";
import process from "node:process";

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

function usage(err) {
  const out = err ? process.stderr : process.stdout;
  out.write(
    "usage: conformance.mjs --cdn URL --origin URL" +
      " [--coalesce strict|lenient] [--purge-mode soft|hard]" +
      " [--timeout SECONDS] [--skip STEPS]\n",
  );
  process.exit(err ? 2 : 0);
}

function parseArgs(argv) {
  const opts = {
    cdn: null,
    origin: null,
    coalesce: "strict",
    purgeMode: "soft",
    timeoutSec: 120,
    skip: new Set(),
  };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const val = () => {
      if (i + 1 >= argv.length) usage(true);
      return argv[++i];
    };
    switch (a) {
      case "--cdn":
        opts.cdn = val().replace(/\/+$/, "");
        break;
      case "--origin":
        opts.origin = val().replace(/\/+$/, "");
        break;
      case "--coalesce":
        opts.coalesce = val();
        break;
      case "--purge-mode":
        opts.purgeMode = val();
        break;
      case "--timeout":
        opts.timeoutSec = Number(val());
        break;
      case "--skip":
        for (const s of val().split(",")) opts.skip.add(s.trim());
        break;
      case "--help":
      case "-h":
        usage(false);
        break;
      default:
        process.stderr.write(`unknown argument: ${a}\n`);
        usage(true);
    }
  }
  if (!opts.cdn || !opts.origin) usage(true);
  if (!["strict", "lenient"].includes(opts.coalesce)) usage(true);
  if (!["soft", "hard"].includes(opts.purgeMode)) usage(true);
  if (!Number.isFinite(opts.timeoutSec) || opts.timeoutSec <= 0) usage(true);
  return opts;
}

const opts = parseArgs(process.argv.slice(2));

// Global watchdog: the whole run must finish inside --timeout.
const watchdog = setTimeout(() => {
  process.stderr.write(
    `FATAL: global --timeout of ${opts.timeoutSec}s exceeded\n`,
  );
  process.exit(2);
}, opts.timeoutSec * 1000);
watchdog.unref();

// ---------------------------------------------------------------------------
// Failure + HTTP helpers
// ---------------------------------------------------------------------------

class CheckFail extends Error {
  constructor(message, detail = {}) {
    super(message);
    this.detail = detail;
  }
}

function headersOf(res) {
  const h = {};
  for (const [k, v] of res.headers) h[k] = v;
  return h;
}

function expect(cond, message, detail = {}) {
  if (!cond) throw new CheckFail(message, detail);
}

// Every await has a timeout: each fetch aborts after 10s.
const FETCH_TIMEOUT_MS = 10_000;

async function fetchx(url, init = {}) {
  let res;
  try {
    res = await fetch(url, {
      redirect: "manual",
      ...init,
      signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
    });
  } catch (e) {
    throw new CheckFail(`fetch failed: ${init.method ?? "GET"} ${url}`, {
      cause: String(e),
    });
  }
  const text = await res.text();
  return { res, text };
}

function xcache(res) {
  return res.headers.get("x-cache");
}

function isHit(res) {
  return /^hit\b/i.test(xcache(res) ?? "");
}

// "Not served from the shared cache": MISS, BYPASS, EXPIRED, or no
// X-Cache header at all — anything but a HIT.
function notHit(res) {
  return !isHit(res);
}

function parseNdjson(text) {
  const records = [];
  for (const line of text.split("\n")) {
    if (!line.trim()) continue;
    records.push(JSON.parse(line));
  }
  return records;
}

function sha256(text) {
  return createHash("sha256").update(text).digest("hex");
}

function sMaxAge(res) {
  const cc = res.headers.get("cache-control") ?? "";
  const m = /s-maxage\s*=\s*(\d+)/i.exec(cc);
  return m ? Number(m[1]) : null;
}

// ---------------------------------------------------------------------------
// Origin debug counter
// ---------------------------------------------------------------------------

async function originCount() {
  const { res, text } = await fetchx(`${opts.origin}/debug/requests`);
  expect(res.status === 200, "GET /debug/requests failed", {
    expected: 200,
    got: res.status,
    body: text,
  });
  return JSON.parse(text).count;
}

async function originReset() {
  const { res } = await fetchx(`${opts.origin}/debug/reset`, {
    method: "POST",
  });
  expect(res.status === 200, "POST /debug/reset failed", {
    expected: 200,
    got: res.status,
  });
}

// ---------------------------------------------------------------------------
// Shared run state (steps build on one another)
// ---------------------------------------------------------------------------

const state = {
  queryUrl: null, // CDN URL of the introduced query, ep=NewHope
  queryPath: null, // path+query of the same (for direct-origin reads)
  etag: null, // ETag observed on the step-2 cold fill
  nonce: `conformance ${randomUUID()}`, // this run's review commentary
  idemKey: randomUUID(), // mutation idempotency key
  mutationBody: null, // exact body bytes, replayed verbatim in step 6
  jediUrl: null, // CDN URL variant ep=Jedi
};

// Take the step-2 URL and rebind the ep variable. String surgery instead
// of URLSearchParams: the CDN caches on the *literal* URL, so the checker
// must produce byte-identical URLs for identical requests, and
// URLSearchParams re-encodes other params (`,` -> %2C) behind our back.
function withEpisode(url, episode) {
  const out = url.replace(/([?&]ep=)[^&]*/, `$1${episode}`);
  expect(out !== url || url.includes(`ep=${episode}`), "no ep param to rebind", {
    url,
  });
  return out;
}

const QUERY_TEXT =
  "query Reviews($ep: Episode) { reviews(episode: $ep, first: 10) { stars commentary } }";

// ---------------------------------------------------------------------------
// Steps
// ---------------------------------------------------------------------------

const steps = [
  {
    n: 1,
    id: "introduce",
    title: "introduce query at the origin",
    async run() {
      const { res, text } = await fetchx(
        `${opts.origin}/q?intent=introduce&ep=NewHope`,
        {
          method: "POST",
          headers: { "content-type": "application/x-lattice-query" },
          body: QUERY_TEXT,
        },
      );
      expect(res.status === 200, "introduction did not return 200", {
        expected: 200,
        got: res.status,
        headers: headersOf(res),
        body: text.slice(0, 2000),
      });
      const location = res.headers.get("location");
      expect(location, "introduction returned no Location header", {
        headers: headersOf(res),
      });
      state.queryPath = location;
      state.queryUrl = new URL(location, `${opts.cdn}/`).toString();
      // Counter accounting starts here: the introduction itself went
      // direct to the origin, so reset AFTER it.
      await originReset();
      const count = await originCount();
      expect(count === 0, "counter not zero after reset", {
        expected: 0,
        got: count,
      });
      return `Location ${location}`;
    },
  },

  {
    n: 2,
    id: "cold-miss",
    title: "cold GET via CDN misses and fills once",
    async run() {
      const { res, text } = await fetchx(state.queryUrl);
      expect(res.status === 200, "cold GET did not return 200", {
        expected: 200,
        got: res.status,
        headers: headersOf(res),
        body: text.slice(0, 2000),
      });
      expect(notHit(res), "cold GET must not be a cache HIT", {
        expected: "MISS or absent",
        got: xcache(res),
        headers: headersOf(res),
      });
      const count = await originCount();
      expect(count === 1, "cold GET should reach the origin exactly once", {
        expected: 1,
        got: count,
      });
      state.etag = res.headers.get("etag");
      expect(state.etag, "response carries no ETag", {
        headers: headersOf(res),
      });
      return `X-Cache=${xcache(res) ?? "(absent)"} count=1 etag=${state.etag}`;
    },
  },

  {
    n: 3,
    id: "warm-hits",
    title: "3 repeat GETs all HIT, ETag stable, origin untouched",
    async run() {
      for (let i = 1; i <= 3; i++) {
        const { res } = await fetchx(state.queryUrl);
        expect(res.status === 200, `repeat GET #${i} did not return 200`, {
          expected: 200,
          got: res.status,
          headers: headersOf(res),
        });
        expect(isHit(res), `repeat GET #${i} was not a HIT`, {
          expected: "HIT",
          got: xcache(res),
          headers: headersOf(res),
        });
        const etag = res.headers.get("etag");
        expect(etag === state.etag, `ETag drifted on repeat GET #${i}`, {
          expected: state.etag,
          got: etag,
        });
      }
      const count = await originCount();
      expect(count === 1, "repeat HITs must not reach the origin", {
        expected: 1,
        got: count,
      });
      return "3x HIT, etag stable, count still 1";
    },
  },

  {
    n: 4,
    id: "coalesce",
    title: `16 concurrent cold GETs coalesce (${opts.coalesce})`,
    async run() {
      const url = withEpisode(state.queryUrl, "Empire");
      const before = await originCount();
      const results = await Promise.all(
        Array.from({ length: 16 }, () => fetchx(url)),
      );
      for (const [i, { res }] of results.entries()) {
        expect(res.status === 200, `concurrent GET #${i} did not return 200`, {
          expected: 200,
          got: res.status,
          headers: headersOf(res),
        });
      }
      const bodies = new Set(results.map(({ text }) => sha256(text)));
      expect(bodies.size === 1, "concurrent GETs returned differing bodies", {
        expected: "1 distinct body",
        got: `${bodies.size} distinct bodies`,
      });
      const delta = (await originCount()) - before;
      if (opts.coalesce === "strict") {
        expect(delta === 1, "expected exactly one origin fill", {
          expected: 1,
          got: delta,
        });
      } else {
        expect(delta >= 1 && delta < 16, "expected fewer fills than requests", {
          expected: "1 <= fills < 16",
          got: delta,
        });
      }
      return `16 GETs -> ${delta} origin fill(s), identical bodies`;
    },
  },

  {
    n: 5,
    id: "purge-on-mutation",
    title: `mutation through CDN invalidates cached query (${opts.purgeMode} purge)`,
    async run() {
      state.jediUrl = withEpisode(state.queryUrl, "Jedi");

      // Fill and prove it is served from cache before the mutation.
      const fill = await fetchx(state.jediUrl);
      expect(fill.res.status === 200, "Jedi fill did not return 200", {
        expected: 200,
        got: fill.res.status,
        headers: headersOf(fill.res),
      });
      expect(
        !fill.text.includes(state.nonce),
        "nonce already present before the mutation",
        { nonce: state.nonce },
      );
      // The freshness poll below must finish well inside the object's
      // TTL, otherwise plain expiry would masquerade as a purge.
      const ttl = sMaxAge(fill.res);
      expect(ttl !== null && ttl > 12, "fill TTL too short to prove purging", {
        expected: "s-maxage > 12",
        got: fill.res.headers.get("cache-control"),
      });
      const warm = await fetchx(state.jediUrl);
      expect(isHit(warm.res), "Jedi URL not cached before the mutation", {
        expected: "HIT",
        got: xcache(warm.res),
        headers: headersOf(warm.res),
      });

      // Mutate THROUGH the CDN.
      state.mutationBody = JSON.stringify({
        episode: "Jedi",
        stars: 5,
        commentary: state.nonce,
      });
      const mut = await fetchx(`${opts.cdn}/m/createReview`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "idempotency-key": state.idemKey,
        },
        body: state.mutationBody,
      });
      expect(mut.res.status === 200, "mutation did not return 200", {
        expected: 200,
        got: mut.res.status,
        headers: headersOf(mut.res),
        body: mut.text.slice(0, 2000),
      });
      expect(notHit(mut.res), "mutation must pass through the CDN", {
        expected: "MISS/BYPASS/absent",
        got: xcache(mut.res),
      });
      expect(
        !mut.res.headers.get("idempotency-replayed"),
        "first mutation must not be a replay",
        { headers: headersOf(mut.res) },
      );
      const invalidated = parseNdjson(mut.text).find(
        (r) => r.kind === "invalidated",
      );
      expect(invalidated, "mutation response has no invalidated record", {
        body: mut.text.slice(0, 2000),
      });
      expect(
        invalidated.keys.includes("reviews:Jedi"),
        "invalidated keys do not cover reviews:Jedi",
        { expected: "includes reviews:Jedi", got: invalidated.keys },
      );

      // Poll the PLAIN URL (no cache busting): the CDN must converge on
      // fresh content within the stale-while-revalidate window. Soft
      // purge may serve stale grace HITs first; count them.
      const t0 = Date.now();
      let staleHits = 0;
      let polls = 0;
      let fresh = false;
      while (Date.now() - t0 < 10_000) {
        const probe = await fetchx(state.jediUrl);
        polls++;
        if (probe.text.includes(state.nonce)) {
          fresh = true;
          break;
        }
        if (isHit(probe.res)) staleHits++;
        await sleep(200);
      }
      expect(fresh, "CDN never served the new review within 10s", {
        expected: `body containing ${JSON.stringify(state.nonce)}`,
        got: `${polls} polls, ${staleHits} stale grace HITs, still stale`,
        hint: "purge from the origin never reached the CDN, or tags do not match",
      });
      return `fresh after ${Date.now() - t0}ms (${polls} poll(s), ${staleHits} stale grace hit(s)), keys=${JSON.stringify(invalidated.keys)}`;
    },
  },

  {
    n: 6,
    id: "idempotent-replay",
    title: "replaying the mutation returns the stored response, no duplicate",
    async run() {
      const mut = await fetchx(`${opts.cdn}/m/createReview`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "idempotency-key": state.idemKey,
        },
        body: state.mutationBody,
      });
      expect(mut.res.status === 200, "replay did not return 200", {
        expected: 200,
        got: mut.res.status,
        headers: headersOf(mut.res),
        body: mut.text.slice(0, 2000),
      });
      const replayed = mut.res.headers.get("idempotency-replayed");
      expect(replayed === "true", "replay lacks Idempotency-Replayed: true", {
        expected: "true",
        got: replayed,
        headers: headersOf(mut.res),
      });
      // A direct origin read must contain the review exactly once.
      const direct = await fetchx(`${opts.origin}${state.queryPath.replace(/([?&]ep=)[^&]*/, "$1Jedi")}`);
      const occurrences = parseNdjson(direct.text).filter(
        (r) => r.kind === "entity" && r.fields?.commentary === state.nonce,
      ).length;
      expect(occurrences === 1, "review duplicated (or lost) after replay", {
        expected: 1,
        got: occurrences,
      });
      return "Idempotency-Replayed: true, review present exactly once";
    },
  },

  {
    n: 7,
    id: "entity-masks",
    title: "/e point fetches: mask isolation + immutable ver pin",
    async run() {
      await originReset();
      const eName = `${opts.cdn}/e/Human/1000?f=name`;
      const eWide = `${opts.cdn}/e/Human/1000?f=name,homePlanet`;

      const first = await fetchx(eName);
      expect(first.res.status === 200, "point fetch did not return 200", {
        expected: 200,
        got: first.res.status,
        headers: headersOf(first.res),
      });
      expect(notHit(first.res), "cold point fetch must not HIT", {
        expected: "MISS/absent",
        got: xcache(first.res),
      });
      const second = await fetchx(eName);
      expect(isHit(second.res), "repeat point fetch did not HIT", {
        expected: "HIT",
        got: xcache(second.res),
        headers: headersOf(second.res),
      });
      let count = await originCount();
      expect(count === 1, "same-mask point fetches should fill once", {
        expected: 1,
        got: count,
      });

      // A different mask is a different URL: separate fill, and the
      // narrow object must not leak the wide mask's field.
      const wide = await fetchx(eWide);
      expect(notHit(wide.res), "different mask must not share the cache entry", {
        expected: "MISS/absent",
        got: xcache(wide.res),
      });
      count = await originCount();
      expect(count === 2, "different mask should cause a second fill", {
        expected: 2,
        got: count,
      });
      expect(
        wide.text.includes("homePlanet") && !first.text.includes("homePlanet"),
        "mask isolation violated between cached variants",
        {
          expected: "homePlanet only in the wide-mask body",
          narrowBody: first.text.slice(0, 500),
          wideBody: wide.text.slice(0, 500),
        },
      );

      // Version-pinned fetch: read the current ver from the origin, then
      // the pinned URL must be immutable and HIT on repeat.
      const fresh = await fetchx(`${opts.origin}/e/Human/1000?f=name`);
      const entity = parseNdjson(fresh.text).find((r) => r.kind === "entity");
      expect(entity?.ver, "origin entity record has no ver", {
        body: fresh.text.slice(0, 500),
      });
      const pinned = `${opts.cdn}/e/Human/1000?f=name&ver=${encodeURIComponent(entity.ver)}`;
      const p1 = await fetchx(pinned);
      expect(p1.res.status === 200, "pinned fetch did not return 200", {
        expected: 200,
        got: p1.res.status,
        headers: headersOf(p1.res),
      });
      const cc = p1.res.headers.get("cache-control") ?? "";
      expect(/immutable/.test(cc), "pinned fetch is not immutable", {
        expected: "Cache-Control containing immutable",
        got: cc,
        headers: headersOf(p1.res),
      });
      const p2 = await fetchx(pinned);
      expect(isHit(p2.res), "repeat pinned fetch did not HIT", {
        expected: "HIT",
        got: xcache(p2.res),
        headers: headersOf(p2.res),
      });
      count = await originCount();
      // 2 mask fills + 1 direct origin read + 1 pinned fill.
      expect(count === 4, "unexpected origin traffic during ver-pin check", {
        expected: 4,
        got: count,
      });
      return `masks isolated (2 fills), ver=${entity.ver} pinned immutable + HIT`;
    },
  },

  {
    n: 8,
    id: "auth-bypass",
    title: "Authorization-carrying requests never touch the shared cache",
    async run() {
      const before = await originCount();
      for (let i = 1; i <= 2; i++) {
        const { res } = await fetchx(state.queryUrl, {
          headers: { authorization: "Bearer x" },
        });
        expect(res.status === 200, `authorized GET #${i} did not return 200`, {
          expected: 200,
          got: res.status,
          headers: headersOf(res),
        });
        expect(notHit(res), `authorized GET #${i} was served from cache`, {
          expected: "MISS/BYPASS/absent",
          got: xcache(res),
          headers: headersOf(res),
        });
      }
      const delta = (await originCount()) - before;
      expect(delta === 2, "each authorized GET must reach the origin", {
        expected: 2,
        got: delta,
      });
      return "2x pass-through, origin count +2";
    },
  },
];

// ---------------------------------------------------------------------------
// Runner
// ---------------------------------------------------------------------------

console.log(
  `lattice cdn conformance: cdn=${opts.cdn} origin=${opts.origin}` +
    ` coalesce=${opts.coalesce} purge-mode=${opts.purgeMode}` +
    ` timeout=${opts.timeoutSec}s`,
);

const results = [];
let failed = false;

for (const step of steps) {
  const label = `[${step.n}/${steps.length}] ${step.id}`;
  if (opts.skip.has(String(step.n)) || opts.skip.has(step.id)) {
    console.log(`${label}: SKIP (requested)`);
    results.push({ step, outcome: "SKIP", note: "requested" });
    continue;
  }
  if (failed) {
    console.log(`${label}: SKIP (earlier step failed)`);
    results.push({ step, outcome: "SKIP", note: "earlier step failed" });
    continue;
  }
  try {
    const note = await step.run();
    console.log(`${label}: PASS — ${note}`);
    results.push({ step, outcome: "PASS", note });
  } catch (e) {
    failed = true;
    const detail = e instanceof CheckFail ? e.detail : { error: String(e) };
    console.log(`${label}: FAIL — ${e.message}`);
    for (const [k, v] of Object.entries(detail)) {
      console.log(`    ${k}: ${typeof v === "string" ? v : JSON.stringify(v)}`);
    }
    results.push({ step, outcome: "FAIL", note: e.message });
  }
}

// Summary table.
const wId = Math.max(...steps.map((s) => s.id.length));
console.log("");
console.log("┌──────┬" + "─".repeat(wId + 2) + "┬─────────┐");
console.log(`│ step │ ${"id".padEnd(wId)} │ result  │`);
console.log("├──────┼" + "─".repeat(wId + 2) + "┼─────────┤");
for (const { step, outcome } of results) {
  console.log(
    `│ ${String(step.n).padEnd(4)} │ ${step.id.padEnd(wId)} │ ${outcome.padEnd(7)} │`,
  );
}
console.log("└──────┴" + "─".repeat(wId + 2) + "┴─────────┘");
console.log(failed ? "RESULT: FAIL" : "RESULT: PASS");

process.exit(failed ? 1 : 0);
