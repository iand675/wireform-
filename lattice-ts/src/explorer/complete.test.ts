import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  analyzeQueryContext,
  completeQuery,
  findInsertionPoints,
  hoverAt,
  isInsertValidAt,
  lintQuery,
  type InsertTarget,
} from "./complete.ts";
import { parseSchema } from "./schema.ts";

const here = dirname(fileURLToPath(import.meta.url));
const fixtures = join(here, "..", "..", "..", "wireform-lattice", "test", "fixtures");
const starwars = parseSchema(readFileSync(join(fixtures, "starwars.lattice"), "utf8"));
const blog = parseSchema(readFileSync(join(fixtures, "blog.lattice"), "utf8"));
const directives = parseSchema(readFileSync(join(fixtures, "directives.lattice"), "utf8"));

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

describe("completion — surfaces descriptions + directive chips in doc", () => {
  const item = (src: string, label: string) => {
    const { text, offset } = at(src);
    return completeQuery(text, offset, directives).find((i) => i.label === label);
  };

  it("folds a field's description AND its directive chip into `doc`", () => {
    const email = item("query { user { | } }", "email");
    expect(email?.doc).toContain("Contact email");
    expect(email?.doc).toContain("never shared-cached");
    expect(email?.doc).toContain("@pii");
  });

  it("folds a relationship's description AND directive chip into `doc`", () => {
    const posts = item("query { user { | } }", "posts");
    expect(posts?.doc).toContain("Posts authored by this user.");
    expect(posts?.doc).toContain("@internal");
  });

  it("folds a root's description AND directive chip (with args) into `doc`", () => {
    const user = item("query { | }", "user");
    expect(user?.doc).toContain("Fetch a single user by id.");
    expect(user?.doc).toContain("@rateLimit(perMinute: 60)");
  });

  it("omits `doc` entirely for an item with neither description nor directives", () => {
    // `authorId` on Post carries no doc string and no directive, so completion
    // must not fabricate a doc panel for it.
    const authorId = item("query { posts { | } }", "authorId");
    expect(authorId).toBeDefined();
    expect(authorId?.doc).toBeUndefined();
  });
});

describe("hoverAt — schema-grounded hover", () => {
  // A self-contained schema exercising each hover dispatch path: a doc'd scalar
  // field with a policy, a doc'd paginated edge, an un-doc'd field, a doc'd
  // root with an arg, and a doc'd fragment.
  const schema = parseSchema(`schema demo.example.com

newtype UserId = Text

entity User by id {
  id: UserId
  name: Text
  "Contact email."
  email: Text private
  "Posts by this user."
  has many posts: Post by authorId
}

entity Post by id {
  id: UserId
  authorId: UserId
  title: Text
}

"Fetch a user."
get user(id: UserId) of User by id

"A byline."
fragment UserByline on User { name }
`);

  const query = `query {
  user(id: "u1") {
    email
    posts { title }
    ...UserByline
  }
}`;

  const hover = (needle: string) => hoverAt(query, query.indexOf(needle) + 1, schema);

  it("describes a doc'd scalar field with its type, doc, and policy meta", () => {
    const h = hover("email");
    expect(h?.kind).toBe("field");
    expect(h?.title).toBe("email");
    expect(h?.signature).toBe(": Text");
    expect(h?.doc).toBe("Contact email.");
    expect(h?.meta).toBe("private");
  });

  it("describes a doc'd paginated edge without a directive meta chip", () => {
    const h = hover("posts");
    expect(h?.kind).toBe("edge");
    expect(h?.title).toBe("posts");
    expect(h?.signature).toBe("has many: Post (paginated)");
    expect(h?.doc).toBe("Posts by this user.");
    expect(h?.meta).toBeUndefined();
  });

  it("describes a doc'd root with its arg signature and return type", () => {
    const h = hover("user(");
    expect(h?.kind).toBe("root");
    expect(h?.title).toBe("user");
    expect(h?.signature).toBe("get(id: UserId) → User");
    expect(h?.doc).toBe("Fetch a user.");
  });

  it("describes an argument name inside a root's arg list", () => {
    const h = hover("id:");
    expect(h?.kind).toBe("arg");
    expect(h?.title).toBe("id");
    expect(h?.signature).toBe(": UserId");
  });

  it("describes a doc'd fragment spread by its target and doc", () => {
    const h = hover("UserByline");
    expect(h?.kind).toBe("fragment");
    expect(h?.title).toBe("UserByline");
    expect(h?.signature).toBe("fragment on User");
    expect(h?.doc).toBe("A byline.");
  });

  it("omits `doc` for a field the schema does not document", () => {
    const h = hover("title");
    expect(h?.kind).toBe("field");
    expect(h?.title).toBe("title");
    expect(h?.doc).toBeUndefined();
  });

  it("returns null over whitespace with no identifier under the caret", () => {
    expect(hoverAt(query, query.indexOf("\n"), schema)).toBeNull();
  });
});

const reviewStars: InsertTarget = { kind: "member", name: "stars", owner: "Review" };
const humanName: InsertTarget = { kind: "member", name: "name", owner: "Human" };
const heroRoot: InsertTarget = { kind: "root", name: "hero" };

describe("isInsertValidAt", () => {
  it("accepts a member inside the matching selection set", () => {
    const { text, offset } = at("query {\n  reviews {\n    |\n  }\n}");
    expect(isInsertValidAt(text, offset, starwars, reviewStars)).toBe(true);
  });

  it("rejects a member in the document header", () => {
    const { text, offset } = at("|query {\n  reviews {\n  }\n}");
    expect(isInsertValidAt(text, offset, starwars, reviewStars)).toBe(false);
  });

  it("rejects a member whose owner is not the type in scope", () => {
    const { text, offset } = at("query {\n  reviews {\n    |\n  }\n}");
    expect(isInsertValidAt(text, offset, starwars, humanName)).toBe(false);
  });

  it("accepts a root at the query root but not inside a selection", () => {
    const rootPos = at("query {\n  |\n}");
    expect(isInsertValidAt(rootPos.text, rootPos.offset, starwars, heroRoot)).toBe(true);
    const inner = at("query {\n  reviews {\n    |\n  }\n}");
    expect(isInsertValidAt(inner.text, inner.offset, starwars, heroRoot)).toBe(false);
  });
});

describe("findInsertionPoints", () => {
  it("finds the single matching selection set", () => {
    const text = "query {\n  reviews {\n    id\n  }\n}";
    const pts = findInsertionPoints(text, starwars, reviewStars, "stars");
    expect(pts.length).toBe(1);
    expect(pts[0]!.typeLabel).toBe("Review");
    expect(pts[0]!.exact).toBe(true);
    const p = pts[0]!;
    expect(text.slice(0, p.start) + p.text + text.slice(p.end)).toBe("query {\n  reviews {\n    stars\n    id\n  }\n}");
  });

  it("expands an empty selection set", () => {
    const text = "query {\n  reviews { }\n}";
    const pts = findInsertionPoints(text, starwars, reviewStars, "stars");
    expect(pts.length).toBe(1);
    const p = pts[0]!;
    expect(text.slice(0, p.start) + p.text + text.slice(p.end)).toBe("query {\n  reviews {\n    stars\n  }\n}");
  });

  it("offers every matching selection set", () => {
    const text = "query {\n  reviews {\n    id\n  }\n  reviews {\n    id\n  }\n}";
    const pts = findInsertionPoints(text, starwars, reviewStars, "stars");
    expect(pts.length).toBe(2);
    expect(pts.map((p) => p.line)).toEqual([1, 4]);
  });

  it("does not offer a union selection set for a concrete member", () => {
    const text = "query {\n  hero {\n  }\n}";
    expect(findInsertionPoints(text, starwars, humanName, "name").length).toBe(0);
  });

  it("offers an inline-fragment selection set of the member's type", () => {
    const text = "query {\n  hero {\n    ... on Human {\n    }\n  }\n}";
    const pts = findInsertionPoints(text, starwars, humanName, "name");
    expect(pts.length).toBe(1);
    expect(pts[0]!.typeLabel).toBe("Human");
    expect(pts[0]!.exact).toBe(true);
  });

  it("sorts exact-owner matches before other valid types", () => {
    // `friends` targets the Character interface; `name` is valid on both the
    // Human set (exact) and the interface set, exact first.
    const text = "query {\n  hero {\n    ... on Human {\n      friends {\n      }\n    }\n  }\n}";
    const pts = findInsertionPoints(text, starwars, humanName, "name");
    expect(pts[0]!.exact).toBe(true);
    expect(pts[0]!.typeLabel).toBe("Human");
  });
});
