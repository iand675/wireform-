import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { describe, expect, it } from "vitest";
import { analyzeQueryContext, completeQuery, lintQuery } from "./complete.ts";
import { parseSchema } from "./schema.ts";

const here = dirname(fileURLToPath(import.meta.url));
const fixtures = join(here, "..", "..", "..", "wireform-lattice", "test", "fixtures");
const starwars = parseSchema(readFileSync(join(fixtures, "starwars.lattice"), "utf8"));
const blog = parseSchema(readFileSync(join(fixtures, "blog.lattice"), "utf8"));

/** Split a source string at `|` into (text, caret offset). */
function at(src: string): { text: string; offset: number } {
  const offset = src.indexOf("|");
  return { text: src.slice(0, offset) + src.slice(offset + 1), offset };
}

const labels = (src: string, schema = starwars): string[] => {
  const { text, offset } = at(src);
  return completeQuery(text, offset, schema).map((i) => i.label);
};

describe("completion — context analysis", () => {
  it("root scope suggests declared roots", () => {
    expect(labels("query { |")).toEqual(["hero", "reviews", "search"]);
  });

  it("filters by the partial word under the caret", () => {
    expect(labels("query { s|")).toEqual(["search"]);
  });

  it("union root context offers common member fields and `... on`", () => {
    const ls = labels("query { hero { | } }");
    // Human ∩ Droid = appearsIn, id, name (+ friends edge), plus `... on`.
    expect(ls).toContain("name");
    expect(ls).toContain("appearsIn");
    expect(ls).toContain("friends");
    expect(ls).toContain("... on");
    // homePlanet is Human-only — not offered at the union level.
    expect(ls).not.toContain("homePlanet");
  });

  it("`... on Type` narrows to member-specific fields", () => {
    const ls = labels("query { hero { ... on Human { | } } }");
    expect(ls).toContain("homePlanet");
    expect(ls).toContain("name");
  });

  it("suggests concrete member types after `... on`", () => {
    expect(labels("query { hero { ... on |")).toEqual(["Droid", "Human"]);
  });

  it("suggests declared arguments inside a field's arg list", () => {
    const { text, offset } = at("query { hero(|) { name } }");
    const ctx = analyzeQueryContext(text, offset, starwars);
    expect(ctx.scope).toBe("args");
    expect(ctx.ownerField).toBe("hero");
    expect(completeQuery(text, offset, starwars).map((i) => i.label)).toEqual(["episode"]);
  });

  it("suggests enum values in an argument value position", () => {
    const { text, offset } = at("query { hero(episode: |) { name } }");
    const ctx = analyzeQueryContext(text, offset, starwars);
    expect(ctx.scope).toBe("argValue");
    expect(ctx.argName).toBe("episode");
    expect(completeQuery(text, offset, starwars).map((i) => i.label)).toEqual(["Empire", "Jedi", "NewHope"]);
  });

  it("suggests pagination args on a paginated edge", () => {
    const { text, offset } = at("query { hero { friends(|) { name } } }");
    const ls = completeQuery(text, offset, starwars).map((i) => i.label);
    expect(ls).toContain("first");
    expect(ls).toContain("after");
  });

  it("interface edge context suggests interface fields + spreads", () => {
    // Character (interface) is the target of the friends edge.
    const ls = labels("query { hero { friends { | } } }");
    expect(ls).toContain("name"); // Character.name
    expect(ls).toContain("... on");
    expect(ls).toContain("...CharacterName"); // schema fragment on Character
  });
});

describe("completion — blog schema (entity edges, arg defaults)", () => {
  it("descends a has-one edge to the target entity's fields", () => {
    const ls = labels("query { feed { author { | } } }", blog);
    expect(ls).toContain("name");
    expect(ls).toContain("email");
    expect(ls).toContain("avatarUrl");
  });

  it("offers root-declared args", () => {
    const { text, offset } = at("query { me { avatarUrl(|) } }");
    const ls = completeQuery(text, offset, blog).map((i) => i.label);
    expect(ls).toEqual(["size"]);
  });
});

describe("lintQuery", () => {
  it("returns no diagnostics for a valid query", () => {
    expect(lintQuery("query { hero { name } }", starwars)).toEqual([]);
  });

  it("reports a grammar error with position", () => {
    const diags = lintQuery("query { hero { name }", starwars); // missing closing brace
    expect(diags.length).toBeGreaterThan(0);
    expect(diags[0]!.severity).toBe("error");
  });

  it("warns on an unknown root", () => {
    const diags = lintQuery("query { nope { name } }", starwars);
    expect(diags).toContainEqual(expect.objectContaining({ severity: "warning", message: expect.stringContaining("nope") }));
  });

  it("warns on an unknown field of a known entity", () => {
    const diags = lintQuery("query { reviews { bogus } }", starwars);
    expect(diags.some((d) => d.severity === "warning" && d.message.includes("bogus"))).toBe(true);
  });

  it("does not warn on valid interface fields or inline fragments", () => {
    const diags = lintQuery("query { hero { name ... on Human { homePlanet } } }", starwars);
    expect(diags).toEqual([]);
  });
});
