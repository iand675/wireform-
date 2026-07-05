/**
 * Query merging (spec §4.3): several documents → one multi-root request,
 * with `UnmergeableError` naming the conflict when the protocol forbids it.
 */

import { describe, expect, it } from "vitest";
import { UnmergeableError, canonicalize, parse } from "./canonical.ts";
import { mergeQueries } from "./merge.ts";

describe("mergeQueries", () => {
  it("unions two documents sharing a root with different subfields", () => {
    const a = parse("query { hero { name } }");
    const b = parse("query { hero { friends(first: 3) { name } } }");
    const { merged, assignments } = mergeQueries([a, b]);

    expect(canonicalize(merged)).toBe("query{hero{friends(first:3){name} name}}");

    expect(assignments).toHaveLength(2);
    expect(assignments[0]!.index).toBe(0);
    expect(assignments[0]!.roots).toEqual(["hero"]);
    expect(assignments[1]!.index).toBe(1);
    expect(assignments[1]!.roots).toEqual(["hero"]);
    // The assignment docs are the expanded inputs, for slicing the shared
    // result back out per consumer.
    expect(canonicalize(assignments[0]!.doc)).toBe("query{hero{name}}");
    expect(canonicalize(assignments[1]!.doc)).toBe("query{hero{friends(first:3){name}}}");
  });

  it("rejects the same root selected with different arguments, naming the root", () => {
    const a = parse("query { hero(episode: EMPIRE) { name } }");
    const b = parse("query { hero(episode: JEDI) { name } }");
    let caught: unknown;
    try {
      mergeQueries([a, b]);
    } catch (e) {
      caught = e;
    }
    expect(caught).toBeInstanceOf(UnmergeableError);
    const err = caught as UnmergeableError;
    expect(err.root).toBe("hero");
    expect(err.message).toContain("hero");
  });

  it("rejects conflicting variable declarations, naming the variable", () => {
    const a = parse("query($n: I32) { reviews(stars: $n) { body } }");
    const b = parse("query($n: Text) { search(text: $n) { name } }");
    let caught: unknown;
    try {
      mergeQueries([a, b]);
    } catch (e) {
      caught = e;
    }
    expect(caught).toBeInstanceOf(UnmergeableError);
    const err = caught as UnmergeableError;
    expect(err.variable).toBe("n");
    expect(err.message).toContain("$n");
  });

  it("rejects conflicting variable defaults too", () => {
    const a = parse("query($limit: I32 = 10) { feed(first: $limit) { title } }");
    const b = parse("query($limit: I32 = 20) { feed(first: $limit) { title } }");
    expect(() => mergeQueries([a, b])).toThrow(UnmergeableError);
  });

  it("three-way merge: a document adding a distinct root gets its own assignment", () => {
    const a = parse("query { hero { name } }");
    const b = parse("query { hero { friends { name } } }");
    const c = parse("query { reviews(episode: Empire) { stars } }");
    const { merged, assignments } = mergeQueries([a, b, c]);

    expect(canonicalize(merged)).toBe(
      "query{hero{friends{name} name} reviews(episode:Empire){stars}}",
    );
    expect(assignments.map((x) => x.roots)).toEqual([["hero"], ["hero"], ["reviews"]]);
  });

  it("identical variable declarations union across documents", () => {
    const a = parse("query($ep: Episode) { hero(episode: $ep) { name } }");
    const b = parse("query($ep: Episode) { reviews(episode: $ep) { stars } }");
    const { merged } = mergeQueries([a, b]);
    expect(canonicalize(merged)).toBe(
      "query($ep:Episode){hero(episode:$ep){name} reviews(episode:$ep){stars}}",
    );
  });

  it("the merged document round-trips through canonicalize/parse as a fixed point", () => {
    const a = parse("query { hero { name friends(first: 2) { id } } }");
    const b = parse("query { reviews(episode: Empire) { stars body } }");
    const { merged } = mergeQueries([a, b]);
    const text = canonicalize(merged);
    expect(canonicalize(parse(text))).toBe(text);
  });

  it("rejects an empty document list", () => {
    expect(() => mergeQueries([])).toThrow(UnmergeableError);
  });
});
