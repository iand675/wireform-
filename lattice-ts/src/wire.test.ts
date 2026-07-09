/**
 * Wire format (spec §9): NDJSON stream decoding under awkward chunking,
 * forward tolerance (§9.4.1), ref parsing, and the corpus entry 11 body
 * (partial failure) as a cross-document pin.
 */

import { describe, expect, it } from "vitest";
import type { EntityRecord, ErrorRecord, LatticeRecord, ManifestRecord } from "./wire.ts";
import {
  LatticeWireError,
  compareSnapshotTokens,
  intervalsConsistent,
  isPageValue,
  isRef,
  isRefArray,
  isRefValue,
  parseRecord,
  parseRef,
  parseSnapshotVector,
  readRecords,
  recordsOfText,
  refType,
  validityIntervals,
} from "./wire.ts";

/** A Response whose body arrives in exactly the given byte chunks. */
function chunkedResponse(chunks: Uint8Array[]): Response {
  const stream = new ReadableStream<Uint8Array>({
    start(controller) {
      for (const chunk of chunks) controller.enqueue(chunk);
      controller.close();
    },
  });
  return new Response(stream);
}

async function collect(resp: Response): Promise<LatticeRecord[]> {
  const out: LatticeRecord[] = [];
  for await (const rec of readRecords(resp)) out.push(rec);
  return out;
}

const STREAM_TEXT =
  '{"kind":"manifest","query":"qh","slice":"pub","root":{"hero":["Droid:2001"]}}\n' +
  '{"kind":"entity","id":"Human:1000","ver":"a01","fields":{"name":"Padmé Amidala"}}\n' +
  '{"kind":"entity","id":"Human:1003","ver":"c19","fields":{"name":"Leia Organa"}}\n' +
  '{"kind":"end","complete":true}';

describe("readRecords chunking", () => {
  const encoded = new TextEncoder().encode(STREAM_TEXT);

  const expectFullStream = (records: LatticeRecord[]): void => {
    expect(records.map((r) => r.kind)).toEqual(["manifest", "entity", "entity", "end"]);
    const luke = records[1] as EntityRecord;
    expect(luke.id).toBe("Human:1000");
    expect(luke.fields["name"]).toBe("Padmé Amidala");
  };

  it("split mid-line and mid-multibyte-character", async () => {
    // "é" is two UTF-8 bytes; cut between them.
    const eAcute = STREAM_TEXT.indexOf("é");
    const cut = new TextEncoder().encode(STREAM_TEXT.slice(0, eAcute)).length + 1;
    const records = await collect(chunkedResponse([encoded.slice(0, cut), encoded.slice(cut)]));
    expectFullStream(records);
  });

  it("several records per chunk, trailing newline absent", async () => {
    // One chunk carries records 1-3 plus half of the unterminated `end` line.
    const half = encoded.length - 8;
    const records = await collect(chunkedResponse([encoded.slice(0, half), encoded.slice(half)]));
    expectFullStream(records);
    expect(records[3]).toEqual({ kind: "end", complete: true });
  });

  it("one byte at a time", async () => {
    const chunks: Uint8Array[] = [];
    for (let i = 0; i < encoded.length; i++) chunks.push(encoded.slice(i, i + 1));
    expectFullStream(await collect(chunkedResponse(chunks)));
  });

  it("a bodiless Response falls back to buffered text", async () => {
    // Environments that buffer the body expose `body: null`; emulate one.
    const buffered = { body: null, text: async () => STREAM_TEXT } as unknown as Response;
    expectFullStream(await collect(buffered));
  });

  it("a malformed COMPLETE line throws LatticeWireError", async () => {
    const bad = '{"kind":"manifest","root":{}}\n{oops}\n{"kind":"end","complete":true}\n';
    await expect(collect(new Response(bad))).rejects.toThrow(LatticeWireError);
  });

  it("a truncated trailing partial line is dropped, not fatal", () => {
    const truncated =
      '{"kind":"entity","id":"Human:1","ver":"v1","fields":{}}\n{"kind":"entity","id":"Hum';
    const records = [...recordsOfText(truncated)];
    expect(records.map((r) => r.kind)).toEqual(["entity"]);
  });
});

describe("forward tolerance (§9.4.1)", () => {
  it("unknown record kinds are kept, never fatal", () => {
    const rec = parseRecord({ kind: "hologram", payload: 42 });
    expect(rec).toEqual({ kind: "unknown", rawKind: "hologram", record: { kind: "hologram", payload: 42 } });
  });

  it("a known kind missing required members degrades to unknown", () => {
    const rec = parseRecord({ kind: "entity", id: "Human:1" }); // no ver
    expect(rec.kind).toBe("unknown");
  });

  it("a non-object NDJSON value is a hard wire error", () => {
    expect(() => parseRecord([1, 2, 3])).toThrow(LatticeWireError);
  });
});

describe("corpus entry 11: partial failure body", () => {
  // The six records of the Lattice response in corpus.md §11 (the wrapped
  // entity line re-joined; NDJSON is one record per line on the wire).
  const ENTRY_11 =
    '{"kind":"manifest","query":"...","plan":"...","slice":"pub","root":{"hero":["Droid:2001"]},"etag":"m:..."}\n' +
    '{"kind":"entity","id":"Droid:2001","ver":"f10","fields":{"name":"R2-D2","friends(first:3)":{"$page":{"items":[{"$ref":"Human:1000"},{"$ref":"Human:1002"},{"$ref":"Human:1003"}]}}}}\n' +
    '{"kind":"entity","id":"Human:1000","ver":"a01","fields":{"name":"Luke Skywalker"}}\n' +
    '{"kind":"error","scope":"Human:1002","code":"lattice:loader-timeout","retryable":true}\n' +
    '{"kind":"entity","id":"Human:1003","ver":"c19","fields":{"name":"Leia Organa"}}\n' +
    '{"kind":"end","complete":true}\n';

  it("parses to 6 records in order with the error scope/code/retryable intact", () => {
    const records = [...recordsOfText(ENTRY_11)];
    expect(records.map((r) => r.kind)).toEqual([
      "manifest",
      "entity",
      "entity",
      "error",
      "entity",
      "end",
    ]);

    const manifest = records[0] as ManifestRecord;
    expect(manifest.root["hero"]).toEqual(["Droid:2001"]);
    expect(manifest.slice).toBe("pub");
    expect(manifest.etag).toBe("m:...");

    const droid = records[1] as EntityRecord;
    expect(droid.id).toBe("Droid:2001");
    expect(droid.ver).toBe("f10");
    const friends = droid.fields["friends(first:3)"];
    expect(isPageValue(friends)).toBe(true);

    const error = records[3] as ErrorRecord;
    expect(error.scope).toBe("Human:1002");
    expect(error.code).toBe("lattice:loader-timeout");
    expect(error.retryable).toBe(true);

    expect(records[5]).toEqual({ kind: "end", complete: true });
  });

  it("item-scoped domain errors (corpus §12) parse with their tagged sums", () => {
    const rec = parseRecord(
      JSON.parse(
        '{"kind":"error","scope":{"$tag":"Item","item":"ord_b"},"error":{"$tag":"AlreadyFilled"},"retryable":false}',
      ),
    ) as ErrorRecord;
    expect(rec.kind).toBe("error");
    expect(rec.scope).toEqual({ $tag: "Item", item: "ord_b" });
    expect(rec.error).toEqual({ $tag: "AlreadyFilled" });
    expect(rec.retryable).toBe(false);
  });
});

describe("refs and field-value shapes", () => {
  it("parseRef splits on the first colon; keys may contain colons", () => {
    expect(parseRef("User:9")).toEqual(["User", "9"]);
    expect(parseRef("Path:a:b")).toEqual(["Path", "a:b"]);
    expect(refType("Droid:2001")).toBe("Droid");
  });

  it("malformed refs throw; isRef mirrors the shape test", () => {
    expect(() => parseRef("nocolon")).toThrow(LatticeWireError);
    expect(() => parseRef(":key")).toThrow(LatticeWireError);
    expect(() => parseRef("Type:")).toThrow(LatticeWireError);
    expect(isRef("User:9")).toBe(true);
    expect(isRef("nocolon")).toBe(false);
    expect(isRef(":key")).toBe(false);
    expect(isRef("Type:")).toBe(false);
  });

  it("field-value classifiers distinguish $ref / $page / bounded arrays", () => {
    expect(isRefValue({ $ref: "User:9" })).toBe(true);
    expect(isRefValue({ ref: "User:9" })).toBe(false);
    expect(isPageValue({ $page: { items: [] } })).toBe(true);
    expect(isPageValue({ $page: {} })).toBe(false);
    expect(isRefArray(["User:1", { $ref: "User:2" }])).toBe(true);
    expect(isRefArray(["User:1", "plain string"])).toBe(false);
  });
});

describe("validity intervals (spec 10.2, 13.2 g2-g3)", () => {
  it("parses snapshot vectors leniently", () => {
    expect(parseSnapshotVector('main="mem:41"')).toEqual([["main", "mem:41"]]);
    expect(parseSnapshotVector('shard3="lsn:0/00C21F40", shard7="lsn:0/1B00A2F0"')).toEqual([
      ["shard3", "lsn:0/00C21F40"],
      ["shard7", "lsn:0/1B00A2F0"],
    ]);
    // malformed members are skipped, not fatal
    expect(parseSnapshotVector('main="mem:1", bogus, ="x", other="y"')).toEqual([
      ["main", "mem:1"],
      ["other", "y"],
    ]);
  });

  it("orders tokens numerically on decimal tails, else length-then-lex", () => {
    expect(compareSnapshotTokens("mem:9", "mem:41")).toBeLessThan(0);
    expect(compareSnapshotTokens("mem:41", "mem:41")).toBe(0);
    expect(compareSnapshotTokens("mem:100", "mem:99")).toBeGreaterThan(0);
    // non-decimal tails: shorter-then-lexicographic (counter-like growth)
    expect(compareSnapshotTokens("0/9F", "0/100A")).toBeLessThan(0);
    expect(compareSnapshotTokens("0/AA", "0/AB")).toBeLessThan(0);
  });

  it("defaults absent floors to the point interval", () => {
    const ivs = validityIntervals('main="mem:5"', undefined);
    expect(ivs).toEqual({ main: { floor: "mem:5", token: "mem:5" } });
    const withFloor = validityIntervals('main="mem:5"', 'main="mem:2"');
    expect(withFloor).toEqual({ main: { floor: "mem:2", token: "mem:5" } });
  });

  it("accepts overlapping intervals despite unequal tokens (the point of floors)", () => {
    const pub = validityIntervals('main="mem:2"', 'main="mem:0"');
    const priv = validityIntervals('main="mem:9"', 'main="mem:1"');
    expect(intervalsConsistent([pub, priv])).toBe(true);
  });

  it("rejects disjoint intervals (read skew) and point-interval divergence", () => {
    const stale = validityIntervals('main="mem:2"', 'main="mem:0"');
    const fresh = validityIntervals('main="mem:9"', 'main="mem:7"');
    expect(intervalsConsistent([stale, fresh])).toBe(false);
    // no floors -> degenerates to token equality
    const a = validityIntervals('main="mem:2"', undefined);
    const b = validityIntervals('main="mem:3"', undefined);
    expect(intervalsConsistent([a, b])).toBe(false);
  });

  it("checks domains independently (federation vectors, 18.5)", () => {
    const one = validityIntervals('posts/main="mem:5", social/main="mem:9"', 'posts/main="mem:1"');
    const two = validityIntervals('posts/main="mem:4"', 'posts/main="mem:2"');
    expect(intervalsConsistent([one, two])).toBe(true);
    const skewed = validityIntervals('social/main="mem:20"', 'social/main="mem:15"');
    expect(intervalsConsistent([one, skewed])).toBe(false);
  });
});
