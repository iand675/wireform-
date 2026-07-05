/**
 * Lattice wire format (spec §9): NDJSON record types over HTTP, entity refs,
 * field-value shapes, and a streaming record decoder.
 *
 * Forward tolerance (§9.4.1): unknown record kinds and unknown error-scope
 * tags MUST be tolerated; they surface here as `UnknownRecord` / opaque scope
 * objects rather than throwing.
 */

// ---------------------------------------------------------------------------
// JSON

export type JsonValue = null | boolean | number | string | JsonValue[] | JsonObject;
export interface JsonObject {
  [key: string]: JsonValue;
}

// ---------------------------------------------------------------------------
// Refs and field values

/** A typed entity reference: `"Type:key"`. */
export type Ref = string;

/** A to-one edge value on the wire: `{"$ref":"User:9"}`. */
export interface RefValue {
  readonly $ref: Ref;
}

/** The payload of a paginated edge: `{"items":[{"$ref":...}],"next":...,"prev":...}`. */
export interface Page {
  readonly items: readonly RefValue[];
  readonly next: string | null;
  readonly prev: string | null;
  readonly total?: number;
}

/** A paginated edge value on the wire: `{"$page":{...}}`. */
export interface PageValue {
  readonly $page: Page;
}

/**
 * Any value stored under an entity's field key: a plain JSON scalar or value,
 * a to-one `{$ref}` edge, a paginated `{$page}` edge, or (for bounded
 * collections) a plain array of ref strings.
 */
export type FieldValue = JsonValue | RefValue | PageValue;

export class LatticeWireError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "LatticeWireError";
  }
}

/** Split a `"Type:key"` ref into its type and key. Keys may themselves contain `:`. */
export function parseRef(ref: Ref): [type: string, key: string] {
  const i = ref.indexOf(":");
  if (i <= 0 || i === ref.length - 1) {
    throw new LatticeWireError(`malformed entity ref: ${JSON.stringify(ref)}`);
  }
  return [ref.slice(0, i), ref.slice(i + 1)];
}

/** The `Type` part of a `"Type:key"` ref. */
export function refType(ref: Ref): string {
  return parseRef(ref)[0];
}

/** Loose structural test for a ref string. */
export function isRef(v: unknown): v is Ref {
  if (typeof v !== "string") return false;
  const i = v.indexOf(":");
  return i > 0 && i < v.length - 1;
}

function isPlainObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

/** Is this field value a to-one edge (`{$ref}`)? */
export function isRefValue(v: unknown): v is RefValue {
  return isPlainObject(v) && typeof v["$ref"] === "string";
}

/** Is this field value a paginated edge (`{$page}`)? */
export function isPageValue(v: unknown): v is PageValue {
  if (!isPlainObject(v)) return false;
  const page = v["$page"];
  return isPlainObject(page) && Array.isArray(page["items"]);
}

/** Is this field value a bounded collection (a plain array of ref strings)? */
export function isRefArray(v: unknown): v is Ref[] {
  return Array.isArray(v) && v.every((x) => isRef(x) || isRefValue(x));
}

// ---------------------------------------------------------------------------
// Records

/** First record of every successful data-slice response (§9.2). */
export interface ManifestRecord {
  readonly kind: "manifest";
  readonly query?: string;
  readonly mutation?: string;
  readonly plan?: string;
  readonly slice?: string;
  /** Result order per root name (per item key `"items"` for batch mutations). */
  readonly root: Readonly<Record<string, readonly Ref[]>>;
  readonly etag?: string;
  readonly batch?: { readonly atomicity?: string; readonly count?: number };
}

export interface EntityRecord {
  readonly kind: "entity";
  readonly id: Ref;
  readonly ver: string;
  readonly fields: Readonly<Record<string, FieldValue>>;
  /** Batch-mutation item correlation (§11.8). */
  readonly item?: string;
  /** Federation source (§18.4). */
  readonly src?: string;
}

/** The entity no longer exists; clients MUST evict it (§9.3). */
export interface TombstoneRecord {
  readonly kind: "tombstone";
  readonly id: Ref;
  readonly ver: string;
}

/** The entity exists but this context may not see it; NOT nonexistence (§9.3). */
export interface ElidedRecord {
  readonly kind: "elided";
  readonly id: Ref;
}

/** Cache-digest elision marker: the client's `(id, ver)` is still current (§10.4). */
export interface UnchangedRecord {
  readonly kind: "unchanged";
  readonly id: Ref;
  readonly ver: string;
}

/**
 * What failed, per §9.4.1. `Entity` scope has a bare-ref string shorthand;
 * other constructors serialize as tagged sums. Unknown `$tag`s must be
 * tolerated (treated as unscoped for display).
 */
export type ErrorScope =
  | Ref
  | { readonly $tag: "Field"; readonly entity: Ref; readonly field: string }
  | { readonly $tag: "Edge"; readonly entity: Ref; readonly field: string }
  | { readonly $tag: "Root"; readonly root: string }
  | { readonly $tag: "Item"; readonly item: string }
  | { readonly $tag: string; readonly [k: string]: JsonValue | undefined };

export interface ErrorRecord {
  readonly kind: "error";
  readonly scope?: ErrorScope;
  /** Protocol vocabulary, `lattice:` namespace (§9.4.2). */
  readonly code?: string;
  /** Declared domain error sum, `{$tag: "AlreadyFilled", ...}` (§9.4.2). */
  readonly error?: { readonly $tag: string; readonly [k: string]: JsonValue | undefined };
  readonly retryable: boolean;
  readonly message?: string;
  readonly item?: string;
}

/** Mirror of the purge set so client stores can mark cached queries stale (§11.3). */
export interface InvalidatedRecord {
  readonly kind: "invalidated";
  readonly keys: readonly string[];
  readonly item?: string;
}

export interface EndRecord {
  readonly kind: "end";
  /** Did the origin finish attempting the full plan (§9.4.4)? Orthogonal to errors. */
  readonly complete: boolean;
  readonly etag?: string;
}

/** The single record of a `slice=plan` response (§9.2). */
export interface PlanRecord {
  readonly kind: "plan";
  readonly query?: string;
  readonly plan?: string;
  readonly slices?: JsonValue;
}

export interface ProgressRecord {
  readonly kind: "progress";
  readonly [k: string]: JsonValue | undefined;
}

/** A record kind this client does not know. Kept, never fatal (§9.4.1). */
export interface UnknownRecord {
  readonly kind: "unknown";
  readonly rawKind: string;
  readonly record: JsonObject;
}

export type LatticeRecord =
  | ManifestRecord
  | EntityRecord
  | TombstoneRecord
  | ElidedRecord
  | UnchangedRecord
  | ErrorRecord
  | InvalidatedRecord
  | EndRecord
  | PlanRecord
  | ProgressRecord
  | UnknownRecord;

// ---------------------------------------------------------------------------
// Record classification

function asUnknown(kind: string, record: JsonObject): UnknownRecord {
  return { kind: "unknown", rawKind: kind, record };
}

/**
 * Classify one decoded NDJSON value into a typed record. Records of unknown
 * kind, and records of a known kind that are missing required members, are
 * returned as `UnknownRecord` rather than thrown (forward tolerance).
 */
export function parseRecord(value: unknown): LatticeRecord {
  if (!isPlainObject(value)) {
    throw new LatticeWireError(`wire record is not a JSON object: ${JSON.stringify(value)}`);
  }
  const rec = value as JsonObject;
  const kind = typeof rec["kind"] === "string" ? (rec["kind"] as string) : "";
  switch (kind) {
    case "manifest": {
      const root = rec["root"];
      return {
        kind: "manifest",
        ...(typeof rec["query"] === "string" ? { query: rec["query"] } : {}),
        ...(typeof rec["mutation"] === "string" ? { mutation: rec["mutation"] } : {}),
        ...(typeof rec["plan"] === "string" ? { plan: rec["plan"] } : {}),
        ...(typeof rec["slice"] === "string" ? { slice: rec["slice"] } : {}),
        root: isPlainObject(root) ? (root as Record<string, Ref[]>) : {},
        ...(typeof rec["etag"] === "string" ? { etag: rec["etag"] } : {}),
        ...(isPlainObject(rec["batch"]) ? { batch: rec["batch"] as ManifestRecord["batch"] } : {}),
      };
    }
    case "entity": {
      if (typeof rec["id"] !== "string" || typeof rec["ver"] !== "string") return asUnknown(kind, rec);
      return {
        kind: "entity",
        id: rec["id"],
        ver: rec["ver"],
        fields: isPlainObject(rec["fields"]) ? (rec["fields"] as Record<string, FieldValue>) : {},
        ...(typeof rec["item"] === "string" ? { item: rec["item"] } : {}),
        ...(typeof rec["src"] === "string" ? { src: rec["src"] } : {}),
      };
    }
    case "tombstone": {
      if (typeof rec["id"] !== "string") return asUnknown(kind, rec);
      return { kind: "tombstone", id: rec["id"], ver: typeof rec["ver"] === "string" ? rec["ver"] : "" };
    }
    case "elided": {
      if (typeof rec["id"] !== "string") return asUnknown(kind, rec);
      return { kind: "elided", id: rec["id"] };
    }
    case "unchanged": {
      if (typeof rec["id"] !== "string") return asUnknown(kind, rec);
      return { kind: "unchanged", id: rec["id"], ver: typeof rec["ver"] === "string" ? rec["ver"] : "" };
    }
    case "error": {
      return {
        kind: "error",
        ...(rec["scope"] !== undefined ? { scope: rec["scope"] as ErrorScope } : {}),
        ...(typeof rec["code"] === "string" ? { code: rec["code"] } : {}),
        ...(isPlainObject(rec["error"]) ? { error: rec["error"] as ErrorRecord["error"] } : {}),
        retryable: rec["retryable"] === true,
        ...(typeof rec["message"] === "string" ? { message: rec["message"] } : {}),
        ...(typeof rec["item"] === "string" ? { item: rec["item"] } : {}),
      };
    }
    case "invalidated": {
      const keys = rec["keys"];
      return {
        kind: "invalidated",
        keys: Array.isArray(keys) ? keys.filter((k): k is string => typeof k === "string") : [],
        ...(typeof rec["item"] === "string" ? { item: rec["item"] } : {}),
      };
    }
    case "end":
      return {
        kind: "end",
        complete: rec["complete"] === true,
        ...(typeof rec["etag"] === "string" ? { etag: rec["etag"] } : {}),
      };
    case "plan":
      return {
        kind: "plan",
        ...(typeof rec["query"] === "string" ? { query: rec["query"] } : {}),
        ...(typeof rec["plan"] === "string" ? { plan: rec["plan"] } : {}),
        ...(rec["slices"] !== undefined ? { slices: rec["slices"] } : {}),
      };
    case "progress":
      return rec as unknown as ProgressRecord;
    default:
      return asUnknown(kind, rec);
  }
}

// ---------------------------------------------------------------------------
// Stream decoding

function decodeLine(line: string, strict: boolean): LatticeRecord | undefined {
  const trimmed = line.endsWith("\r") ? line.slice(0, -1) : line;
  if (trimmed.trim() === "") return undefined;
  let json: unknown;
  try {
    json = JSON.parse(trimmed);
  } catch (e) {
    if (strict) {
      throw new LatticeWireError(`malformed NDJSON line: ${trimmed.slice(0, 120)}`);
    }
    return undefined; // trailing partial line of a truncated stream
  }
  return parseRecord(json);
}

/**
 * Stream-decode an NDJSON entity stream from a fetch `Response`.
 *
 * Complete lines must parse (a malformed complete line throws
 * `LatticeWireError`); a trailing unterminated partial line — the signature of
 * a truncated transfer — is tolerated and dropped (the missing `end` record
 * already marks the response incomplete).
 */
export async function* readRecords(response: Response): AsyncGenerator<LatticeRecord, void, unknown> {
  const body = response.body;
  if (!body) {
    // Environments that buffered the body (or an empty response).
    const text = await response.text();
    yield* recordsOfText(text);
    return;
  }
  const reader = body.getReader();
  const decoder = new TextDecoder("utf-8");
  let buf = "";
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      buf += decoder.decode(value, { stream: true });
      let nl: number;
      while ((nl = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, nl);
        buf = buf.slice(nl + 1);
        const rec = decodeLine(line, true);
        if (rec) yield rec;
      }
    }
    buf += decoder.decode();
    const tail = decodeLine(buf, false);
    if (tail) yield tail;
  } finally {
    reader.releaseLock();
  }
}

/** Decode records from an already-buffered NDJSON string. */
export function* recordsOfText(text: string): Generator<LatticeRecord, void, unknown> {
  const lines = text.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const last = i === lines.length - 1;
    const rec = decodeLine(lines[i] ?? "", !last);
    if (rec) yield rec;
  }
}

// ---------------------------------------------------------------------------
// Denormalized data shape (what `client.denormalize` produces)

/** A denormalized entity: its ref plus the selected fields as plain properties. */
export type EntityTree = { readonly __ref: Ref } & { [field: string]: unknown };

/** A denormalized paginated edge. */
export interface PageTree {
  readonly items: EntityTree[];
  readonly next: string | null;
  readonly prev: string | null;
  readonly total?: number;
}

/** Default shape of a denormalized query result: one entry per root field. */
export type QueryData = { [root: string]: EntityTree[] | undefined };

// ---------------------------------------------------------------------------
// Header / URL helpers shared by client and server-ish code

/** Well-known Lattice header names. */
export const HEADERS = {
  plan: "lattice-plan",
  schema: "lattice-schema",
  snapshot: "lattice-snapshot",
  outcome: "lattice-outcome",
  queryName: "lattice-query-name",
  surrogateKey: "surrogate-key",
  vcAuth: "x-vc-auth",
  idempotencyKey: "idempotency-key",
} as const;

/** The query media type used by the introduction rungs (§6.3/§6.4). */
export const QUERY_MEDIA_TYPE = "application/x-lattice-query";

/**
 * URL parameter names reserved by the protocol; variable names must not
 * collide with these.
 */
export const RESERVED_PARAMS: Record<string, true> = {
  p: true,
  slice: true,
  vc: true,
  project: true,
  live: true,
  d: true,
  dv: true,
  intent: true,
};
