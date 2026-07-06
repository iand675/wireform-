/**
 * Canonicalization (spec §5.1) against the Haskell golden pins in
 * `wireform-lattice/test/fixtures/golden/*.canonical.txt`.
 *
 * Each golden file is two lines: the canonical query text and its BLAKE3
 * query hash. This client cannot compute BLAKE3, so only line 1 (the
 * canonical text) is compared; the hash is validated end-to-end by the
 * server granting `/q/{hash}` URLs.
 *
 * The Haskell canonicalizer runs schema-dependent steps this schema-free
 * client cannot: default-argument erasure (`friends(first:10)` → `friends`
 * when 10 is the collection default) and paging-argument normalization
 * (`limit:` → `first:`). The source variants below are chosen on the
 * post-normalization side of those steps, so byte-identical output IS the
 * cross-implementation contract for them; the divergent verbatim Haskell
 * spellings are asserted separately with their expected divergence.
 */

import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import {
  LatticeQueryError,
  canonicalFieldKey,
  canonicalJson,
  canonicalize,
  gql,
  parse,
} from "./canonical.ts";

function goldenCanonicalText(name: string): string {
  const file = readFileSync(
    new URL(`../../wireform-lattice/test/fixtures/golden/${name}.canonical.txt`, import.meta.url),
    "utf8",
  );
  const line = file.split("\n")[0];
  if (!line) throw new Error(`golden ${name} is empty`);
  return line;
}

const canon = (text: string): string => canonicalize(parse(text));

describe("§5.1 canonical query goldens (cross-implementation pin)", () => {
  it("starwars Hero", () => {
    // Haskell source: `query Hero { hero { name friends(first: 10) { name } } }`
    // — `(first: 10)` is erased by the schema (collection default); the
    // schema-free equivalent simply omits it.
    const text = canon(`
      query Hero {
        hero {
          name,
          friends { name }
        }
      }`);
    expect(text).toBe(goldenCanonicalText("hero"));
  });

  it("starwars Search", () => {
    // Haskell source carries `first: 10` (schema-erased). Also exercises the
    // `String` → `Text` type alias and inline-fragment sorting.
    const text = canon(`
      query Search($text: String) {
        search(text: $text) {
          ... on Starship { name, length }
          ... on Human    { homePlanet name }
          ... on Droid    { primaryFunction, name }
        }
      }`);
    expect(text).toBe(goldenCanonicalText("search"));
  });

  it("blog FeedPage (post-normalization spelling)", () => {
    // The Haskell fixture spells the page argument `limit:`; the schema
    // rewrites it to `first:` (§5.1). Spelled `first:` at the source, the
    // schema-free canonicalization must be byte-identical to the golden:
    // sorted variables, `Int` → `I32`, retained `=20` default, schema
    // fragment `...UserByline` surviving late-bound, sorted fields.
    const text = canon(`
      query FeedPage($limit: Int = 20, $after: Cursor) {
        feed(first: $limit, after: $after) {
          title
          publishedAt
          author { ...UserByline }
          comments(first: 3) {
            body, createdAt,
            author { ...UserByline }
          }
        }
      }`);
    expect(text).toBe(goldenCanonicalText("feedpage"));
  });

  it("blog FeedPage (verbatim Haskell fixture spelling) diverges only in limit:/first:", () => {
    // KNOWN DIVERGENCE: `limit:` → `first:` normalization needs the schema
    // (the blog schema declares feed's page parameter). Everything else —
    // variable sort, alias normalization, field sort, fragment handling —
    // must still match, so the divergence is pinned to exactly that token.
    const verbatim = canon(`
      query FeedPage($after: Cursor, $limit: I32 = 20) {
        feed(after: $after, limit: $limit) {
          title
          publishedAt
          author { ...UserByline }
          comments(first: 3) {
            body
            createdAt
            author { ...UserByline }
          }
        }
      }`);
    expect(verbatim).toBe(goldenCanonicalText("feedpage").replace("first:$limit", "limit:$limit"));
  });

  it("golden canonical texts are fixed points of this canonicalizer", () => {
    for (const name of ["hero", "search", "feedpage"]) {
      const text = goldenCanonicalText(name);
      expect(canon(text)).toBe(text);
    }
  });

  it("golden invariants: sorted vars/fields, name erased, no local fragments", () => {
    for (const name of ["hero", "search", "feedpage"]) {
      const text = goldenCanonicalText(name);
      const doc = parse(text);
      expect(text.startsWith("query{") || text.startsWith("query(")).toBe(true);
      expect(doc.name).toBeUndefined();
      expect(Object.keys(doc.fragments)).toEqual([]);
      const varNames = doc.variables.map((v) => v.name);
      expect(varNames).toEqual([...varNames].sort());
    }
  });
});

describe("§5.1 step 4: NFC normalization (cross-implementation pin)", () => {
  // Pinned with the Haskell side (Lattice.Canonical renderCanonical): a
  // decomposed "é" (e + U+0301 COMBINING ACUTE ACCENT) in a string literal
  // collapses to the single code point U+00E9 in the canonical text, so
  // both implementations hash identical NFC bytes.
  it("collapses e + U+0301 to U+00E9 in string literals", () => {
    const canonical = canon('query { search(text: "cafe\u0301") { name } }');
    expect(canonical).toBe('query{search(text:"caf\u00E9"){name}}');
    expect(canonical.includes("\u0301")).toBe(false);
  });

  it("NFC and pre-composed spellings canonicalize identically", () => {
    const decomposed = canon('query { search(text: "cafe\u0301") { name } }');
    const composed = canon('query { search(text: "caf\u00E9") { name } }');
    expect(decomposed).toBe(composed);
  });
});

describe("two spellings, one query", () => {
  const reference = `
    query FeedPage($after: Cursor, $limit: I32 = 20) {
      feed(after: $after, first: $limit) {
        title publishedAt
        author { ...UserByline }
        comments(first: 3) { body createdAt author { ...UserByline } }
      }
    }`;

  it("reordered fields, arguments, variables, and commas are the same query", () => {
    const shuffled = `
      query($limit: Int = 20, $after: Cursor,) {
        feed(first: $limit, after: $after) {
          comments(first: 3) { author { ...UserByline }, createdAt, body },
          author { ...UserByline },
          publishedAt,
          title,
        }
      }`;
    expect(canon(shuffled)).toBe(canon(reference));
  });

  it("the query name never reaches the canonical text", () => {
    expect(canon("query Named { hero { name } }")).toBe(canon("query { hero { name } }"));
  });

  it("duplicate field selections dedupe; overlapping sub-selections union", () => {
    expect(canon("query { hero { name name friends { id } friends { name } } }")).toBe(
      "query{hero{friends{id name} name}}",
    );
  });

  it("local fragments expand away (no ... left behind)", () => {
    const text = canon(`
      query { hero { ...Bits friends { ...Bits } } }
      fragment Bits on Character { name id }`);
    expect(text).toBe("query{hero{friends{id name} id name}}");
    expect(text).not.toContain("...");
  });

  it("parameterized local fragments substitute their arguments", () => {
    const text = canon(`
      query { hero { ...Pic(size: 96) } }
      fragment Pic($size: I32) on Character { avatarUrl(size: $size) }`);
    expect(text).toBe("query{hero{avatarUrl(size:96)}}");
  });

  it("numbers canonicalize (1e3, 0.50, -0) and big integers survive verbatim", () => {
    expect(canon("query { things(a: 1e3, b: 0.50, c: -0) { name } }")).toBe(
      "query{things(a:1000,b:0.5,c:0){name}}",
    );
    expect(canon("query { things(n: 123456789012345678901) { name } }")).toBe(
      "query{things(n:123456789012345678901){name}}",
    );
  });
});

describe("grammar-level static rules", () => {
  it("@include does not exist (@depth is the entire directive grammar)", () => {
    expect(() => parse("query($yes: Bool) { hero { name @include(if: $yes) } }")).toThrow(
      LatticeQueryError,
    );
    expect(() => parse("query($yes: Bool) { hero { name @include(if: $yes) } }")).toThrow(
      /@include does not exist/,
    );
  });

  it("aliases have no production", () => {
    expect(() => parse("query { bigHero: hero { name } }")).toThrow(LatticeQueryError);
  });

  it("@depth cannot carry a selection set (grammar rule 4)", () => {
    expect(() => parse("query { hero { replies @depth(3) { name } } }")).toThrow(
      /must not carry a selection set/,
    );
  });

  it("a declared-but-unused variable is rejected (grammar rule 7)", () => {
    expect(() => canon("query($n: I32) { hero { name } }")).toThrow(/declared but never used/);
  });

  it("a used-but-undeclared variable is rejected (grammar rule 7)", () => {
    expect(() => canon("query { hero(episode: $ep) { name } }")).toThrow(/used but never declared/);
  });

  it("reserved words cannot name variables or fragments", () => {
    expect(() => parse("query($on: Text) { hero(x: $on) { name } }")).toThrow(/reserved/);
    expect(() => parse("query { hero { ...Frag } } fragment on on Character { name }")).toThrow(
      /reserved/,
    );
  });

  it("exactly one query definition per document", () => {
    expect(() => parse("query { hero { name } } query { hero { id } }")).toThrow(
      /exactly one query definition/,
    );
  });

  it("imports cannot be resolved at runtime", () => {
    const doc = parse('import "./frags.lattice"\nquery { hero { ...UserByline } }');
    expect(() => canonicalize(doc)).toThrow(/resolved at build time/);
  });

  // Spec §4.8 rule 7 (Draft 27): variable names colliding with reserved URL
  // parameter names (p, slice, vc, project, live, d, dv, intent) are a
  // parse-time rejection, since variables bind as URL query parameters in
  // the hash form (§6.1).
  it("reserved URL param names are rejected as variable names at parse time", () => {
    expect(() => canon("query($slice: Text) { hero(kind: $slice) { name } }")).toThrow(
      /reserved URL parameter/,
    );
  });
});

describe("canonical JSON and field keys", () => {
  it("canonicalJson sorts keys, drops undefined members, pins -0 to 0", () => {
    expect(canonicalJson({ b: 1, a: [true, "x"], z: undefined, m: -0 })).toBe(
      '{"a":[true,"x"],"b":1,"m":0}',
    );
    expect(() => canonicalJson({ n: Number.POSITIVE_INFINITY })).toThrow(/non-finite/);
  });

  it("canonicalFieldKey sorts args and omits null/undefined (absence has one spelling)", () => {
    expect(canonicalFieldKey("avatarUrl", [["size", 48]])).toBe("avatarUrl(size:48)");
    expect(
      canonicalFieldKey("feed", [
        ["first", 20],
        ["after", null],
      ]),
    ).toBe("feed(first:20)");
    expect(canonicalFieldKey("hero")).toBe("hero");
    expect(canonicalFieldKey("q", [["s", "a b"]])).toBe('q(s:"a b")');
  });
});

describe("gql template tag", () => {
  it("returns a frozen, pre-validated document", () => {
    const doc = gql`query { hero { name } }`;
    expect(Object.isFrozen(doc)).toBe(true);
    expect(canonicalize(doc)).toBe("query{hero{name}}");
  });

  it("throws at tag time for malformed queries", () => {
    expect(() => gql`query { hero { name }`).toThrow(LatticeQueryError);
  });
});
