/**
 * Lattice CDN tier — Cloudflare Worker.
 *
 * A plug-and-play shared cache for ANY conforming Lattice origin (nothing in
 * here is Star-Wars- or demo-specific): point `ORIGIN_URL` at the origin and
 * route Lattice traffic through the worker. It replicates the Varnish tier's
 * behaviour on Cloudflare, working around surrogate-key purging being
 * Enterprise-only by maintaining its own Surrogate-Key -> URLs tag index in
 * Workers KV (binding `TAGS`).
 *
 * Behaviour:
 *  - GETs under /q, /e, /schema, /.well-known/lattice are shared-cached by
 *    full URL (query string included — that is the point: slice, vc, ver and
 *    variables are all part of the cache key) via `caches.default`, honouring
 *    the origin's Cache-Control. The Cache API respects `max-age`, not
 *    `s-maxage`, so the STORED copy gets `Cache-Control: public,
 *    max-age=<s-maxage>` synthesized; the client-facing copy keeps the
 *    origin's original headers.
 *  - Requests carrying Authorization, and anything that is not a cacheable
 *    GET (mutations, QUERY, introduce POSTs, /debug/…), proxy straight to the
 *    origin untouched (`X-Cache: BYPASS`).
 *  - POST /_lattice/purge {"keys": [...]} guarded by `X-Purge-Secret ===
 *    env.PURGE_SECRET` deletes every indexed URL for each key from
 *    `caches.default` and drops the KV entries. This is a HARD purge — a
 *    documented deviation from the Varnish tier's xkey.softpurge (see
 *    README.md).
 *  - Concurrent identical-URL misses are single-flighted per isolate through
 *    an isolate-global Map<url, Promise<Response>>; the origin fill is
 *    buffered so every coalesced waiter gets a cheap clone.
 *  - Every response carries `X-Cache: HIT | MISS | BYPASS` so external
 *    conformance checkers can observe cache behaviour.
 */

export interface Env {
	/** Surrogate-Key -> JSON array of cached URLs, entries `tag:<key>`. */
	TAGS: KVNamespace;
	/** Base URL of the Lattice origin, e.g. "http://127.0.0.1:8917". */
	ORIGIN_URL: string;
	/** Shared secret expected in X-Purge-Secret on /_lattice/purge. */
	PURGE_SECRET: string;
}

/** Path prefixes eligible for shared caching (GET only). */
const CACHEABLE_PREFIXES = ["/q", "/e", "/schema", "/.well-known/lattice"];

/** TTL for KV tag-index entries (seconds). */
const TAG_INDEX_TTL = 86_400;

/**
 * A fully-buffered origin response as PURE JS data. workerd pins I/O objects
 * (Response, ReadableStream — even with a buffered body) to the request
 * context that created them: sharing a `Promise<Response>` across coalesced
 * requests throws "Cannot perform I/O on behalf of a different request".
 * Plain objects and ArrayBuffers carry no context, so the single-flight
 * table shares these instead and every request materializes its own
 * Response.
 */
interface BufferedResponse {
	status: number;
	statusText: string;
	headers: [string, string][];
	body: ArrayBuffer;
}

/**
 * Per-isolate single-flight table. Isolate-global: coalescing only spans
 * requests that land on the same isolate (documented deviation from Varnish
 * request coalescing, which is per-instance and absolute).
 */
const inflight = new Map<string, Promise<BufferedResponse>>();

export default {
	async fetch(request, env, ctx): Promise<Response> {
		const url = new URL(request.url);

		if (request.method === "POST" && url.pathname === "/_lattice/purge") {
			return handlePurge(request, env);
		}

		if (isCacheCandidate(request, url)) {
			return handleCacheable(request, url, env, ctx);
		}

		return proxyToOrigin(request, url, env);
	},
} satisfies ExportedHandler<Env>;

/* -------------------------------------------------------------------------
 * Purge endpoint
 * ---------------------------------------------------------------------- */

function isStringArray(value: unknown): value is string[] {
	return Array.isArray(value) && value.every((k) => typeof k === "string");
}

/** Narrow a parsed purge body to `{ keys: string[] }` without asserting. */
function purgeKeysOf(body: unknown): string[] | null {
	if (body === null || typeof body !== "object" || !("keys" in body)) {
		return null;
	}
	const keys: unknown = body.keys;
	return isStringArray(keys) ? keys : null;
}

async function handlePurge(request: Request, env: Env): Promise<Response> {
	if (request.headers.get("X-Purge-Secret") !== env.PURGE_SECRET) {
		return jsonResponse(403, { error: "invalid purge secret" });
	}

	let parsed: unknown;
	try {
		parsed = await request.json();
	} catch {
		return jsonResponse(400, { error: "body must be JSON" });
	}
	const keys = purgeKeysOf(parsed);
	if (keys === null) {
		return jsonResponse(400, { error: 'body must be {"keys": [string]}' });
	}

	const cache = caches.default;
	let purged = 0;
	for (const key of keys) {
		const kvKey = `tag:${key}`;
		const urls = await readTagIndex(env, kvKey);
		if (urls === null) continue;
		for (const cachedUrl of urls) {
			await cache.delete(cachedUrl);
			purged += 1;
		}
		await env.TAGS.delete(kvKey);
	}

	return jsonResponse(200, { purged });
}

/* -------------------------------------------------------------------------
 * Cacheable GET path
 * ---------------------------------------------------------------------- */

function isCacheCandidate(request: Request, url: URL): boolean {
	if (request.method !== "GET") return false;
	// Never shared-cache credentialed traffic (priv slices ride Authorization).
	if (request.headers.get("Authorization") !== null) return false;
	return CACHEABLE_PREFIXES.some(
		(p) => url.pathname === p || url.pathname.startsWith(`${p}/`),
	);
}

async function handleCacheable(
	request: Request,
	url: URL,
	env: Env,
	ctx: ExecutionContext,
): Promise<Response> {
	// Cache key: the full incoming URL, query string and all. The Lattice URL
	// contract makes representation-affecting inputs (slice, vc, ver, masks,
	// variables) URL parameters, so full-URL keying IS the protocol's
	// cache-correctness story.
	const cacheKeyUrl = url.toString();
	const cacheKey = new Request(cacheKeyUrl, { method: "GET" });
	const cache = caches.default;

	const hit = await cache.match(cacheKey);
	if (hit !== undefined) {
		return withXCache(hit, "HIT");
	}

	// Single-flight concurrent identical-URL misses within this isolate.
	let flight = inflight.get(cacheKeyUrl);
	if (flight === undefined) {
		flight = fillFromOrigin(request, url, cacheKey, env, ctx).finally(() => {
			inflight.delete(cacheKeyUrl);
		});
		inflight.set(cacheKeyUrl, flight);
	}
	const master = await flight;
	// Materialize a Response in THIS request's context from the shared data.
	const response = new Response(master.body.slice(0), {
		status: master.status,
		statusText: master.statusText,
		headers: master.headers,
	});
	return withXCache(response, "MISS");
}

/**
 * Fetch the origin once for a missed URL, store a rewritten copy when the
 * response is shared-cacheable, and index its Surrogate-Keys in KV. Resolves
 * to pure-data payload that coalesced waiters (from other request contexts)
 * can consume safely.
 */
async function fillFromOrigin(
	request: Request,
	url: URL,
	cacheKey: Request,
	env: Env,
	ctx: ExecutionContext,
): Promise<BufferedResponse> {
	// Pass Lattice (and all other) request headers through, but strip
	// conditionals: a coalesced 304 would be wrong for every other waiter, and
	// the shared cache — not the client — owns revalidation (Varnish does the
	// same on fetch).
	const headers = new Headers(request.headers);
	headers.delete("If-None-Match");
	headers.delete("If-Modified-Since");

	const originResponse = await fetch(originUrlFor(url, env), {
		method: "GET",
		headers,
		redirect: "manual",
	});

	// Buffer the body once; waiters copy the bytes. Lattice payloads are
	// small NDJSON documents.
	const body = await originResponse.arrayBuffer();
	const headerPairs: [string, string][] = [];
	originResponse.headers.forEach((value, name) => {
		headerPairs.push([name, value]);
	});
	const buffered: BufferedResponse = {
		status: originResponse.status,
		statusText: originResponse.statusText,
		headers: headerPairs,
		body,
	};

	// Header-driven cacheability — deliberately NO status special-casing (207
	// degraded responses are cache-eligible iff their headers say so; the
	// origin self-purges on degrade).
	const storedCC = storageCacheControl(originResponse.headers.get("Cache-Control"));
	if (storedCC !== null && originResponse.status !== 206) {
		const stored = new Response(body, {
			status: buffered.status,
			statusText: buffered.statusText,
			headers: buffered.headers,
		});
		stored.headers.set("Cache-Control", storedCC);
		const surrogateKeys = originResponse.headers.get("Surrogate-Key");
		ctx.waitUntil(
			Promise.all([
				caches.default.put(cacheKey, stored),
				indexTags(env, surrogateKeys, cacheKey.url),
			]),
		);
	}

	return buffered;
}

/**
 * Decide shared-cacheability from Cache-Control alone and compute the
 * Cache-Control for the STORED copy, or null when the response must not be
 * shared-cached.
 *
 * Cacheable: `public` present, neither `private` nor `no-store`, and either
 * `s-maxage` > 0 or `immutable`. The Cache API honours `max-age` (not
 * `s-maxage`), so `s-maxage` is rewritten to `max-age` on the stored copy;
 * stale-while-revalidate is dropped (the Cache API has no grace semantics —
 * documented deviation from Varnish).
 */
function storageCacheControl(headerValue: string | null): string | null {
	if (headerValue === null) return null;
	const directives = new Map<string, string>();
	for (const part of headerValue.split(",")) {
		const eq = part.indexOf("=");
		const name = (eq === -1 ? part : part.slice(0, eq)).trim().toLowerCase();
		const value = eq === -1 ? "" : part.slice(eq + 1).trim();
		if (name !== "") directives.set(name, value);
	}
	if (directives.has("private") || directives.has("no-store")) return null;
	if (!directives.has("public")) return null;

	const sMaxAge = Number.parseInt(directives.get("s-maxage") ?? "", 10);
	if (Number.isFinite(sMaxAge) && sMaxAge > 0) {
		return `public, max-age=${sMaxAge}`;
	}
	if (directives.has("immutable")) {
		// e.g. "public, max-age=31536000, immutable" — already max-age-driven.
		return headerValue;
	}
	return null;
}

/** Parse a KV tag-index entry into a URL list without asserting its shape. */
async function readTagIndex(env: Env, kvKey: string): Promise<string[] | null> {
	const raw: unknown = await env.TAGS.get(kvKey, "json");
	return isStringArray(raw) ? raw : null;
}

/**
 * Append `url` to the KV index entry of every Surrogate-Key token
 * (deduplicated, 24h TTL). Read-modify-write per key: races between isolates
 * can drop an append — acceptable for the index (purge falls back to TTL
 * expiry; see README deviations).
 */
async function indexTags(
	env: Env,
	surrogateKeys: string | null,
	url: string,
): Promise<void> {
	if (surrogateKeys === null) return;
	const keys = surrogateKeys.split(/[ \t]+/).filter((k) => k !== "");
	for (const key of keys) {
		const kvKey = `tag:${key}`;
		const existing = (await readTagIndex(env, kvKey)) ?? [];
		if (existing.includes(url)) continue;
		existing.push(url);
		await env.TAGS.put(kvKey, JSON.stringify(existing), {
			expirationTtl: TAG_INDEX_TTL,
		});
	}
}

/* -------------------------------------------------------------------------
 * Plain proxy path
 * ---------------------------------------------------------------------- */

/** Proxy anything non-cacheable to the origin untouched. */
async function proxyToOrigin(
	request: Request,
	url: URL,
	env: Env,
): Promise<Response> {
	const target = originUrlFor(url, env);
	// new Request(target, request) carries method, headers and body through.
	const upstream = new Request(target, request);
	const response = await fetch(upstream, { redirect: "manual" });
	return withXCache(response, "BYPASS");
}

/* -------------------------------------------------------------------------
 * Helpers
 * ---------------------------------------------------------------------- */

function originUrlFor(url: URL, env: Env): string {
	return new URL(url.pathname + url.search, env.ORIGIN_URL).toString();
}

/** Rewrap a response (cached responses have immutable headers) + X-Cache. */
function withXCache(
	response: Response,
	state: "HIT" | "MISS" | "BYPASS",
): Response {
	const out = new Response(response.body, response);
	out.headers.set("X-Cache", state);
	return out;
}

function jsonResponse(status: number, payload: unknown): Response {
	return new Response(JSON.stringify(payload), {
		status,
		headers: { "Content-Type": "application/json" },
	});
}
