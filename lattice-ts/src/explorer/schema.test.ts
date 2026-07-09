import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { describe, expect, it } from "vitest";
import { entityMembers, parseSchema, targetLabel, targetTypes } from "./schema.ts";

const here = dirname(fileURLToPath(import.meta.url));
const fixtures = join(here, "..", "..", "..", "wireform-lattice", "test", "fixtures");
const readFixture = (name: string): string => readFileSync(join(fixtures, name), "utf8");

describe("parseSchema — starwars fixture", () => {
  const s = parseSchema(readFixture("starwars.lattice"));

  it("reads the schema name and diagnostics-free surface", () => {
    expect(s.name).toBe("starwars.example.com");
    expect(s.diagnostics).toEqual([]);
  });

  it("reads newtypes and enums", () => {
    expect(s.newtypes.get("HumanId")?.type).toBe("Text");
    const ep = s.enums.get("Episode");
    expect(ep?.open).toBe(false);
    expect(ep?.values).toEqual(["NewHope", "Empire", "Jedi"]);
  });

  it("reads the Character interface and its derived member set", () => {
    const c = s.interfaces.get("Character");
    expect(c?.fields.map((f) => f.name).sort()).toEqual(["appearsIn", "name"]);
    expect(c?.edges.map((e) => e.name)).toEqual(["friends"]);
    // Human and Droid `implements Character`.
    expect(c?.members).toEqual(["Droid", "Human"]);
  });

  it("reads entity fields, optionality markers, and edges", () => {
    const human = s.entities.get("Human");
    expect(human?.key).toEqual(["id"]);
    expect(human?.implements).toEqual(["Character"]);
    expect(human?.defaultPolicy).toBe("visible to all by default");
    const homePlanet = human?.fields.find((f) => f.name === "homePlanet");
    expect(homePlanet?.type).toBe("Text?");
    const appearsIn = human?.fields.find((f) => f.name === "appearsIn");
    expect(appearsIn?.type).toBe("[Episode]");
    const friends = human?.edges.find((e) => e.name === "friends");
    expect(friends?.kind).toBe("many");
    expect(friends?.by).toBe("ownerId");
    expect(friends?.page).toBe(10);
    expect(friends?.max).toBe(50);
    expect(friends?.target).toEqual({ kind: "interface", name: "Character" });
    expect(human?.fetchBy).toEqual({ key: "id", policy: "public" });
  });

  it("reads roots with union targets and args", () => {
    const hero = s.roots.get("hero");
    expect(hero?.kind).toBe("get");
    expect(hero?.args).toEqual([{ name: "episode", type: "Episode?" }]);
    expect(hero?.target).toEqual({ kind: "union", members: ["Human", "Droid"] });
    expect(hero?.policy).toBe("public");
    expect(targetLabel(hero!.target)).toBe("(Human | Droid)");
    expect(targetTypes(s, hero!.target)).toEqual(["Human", "Droid"]);

    const reviews = s.roots.get("reviews");
    expect(reviews?.kind).toBe("list");
    expect(reviews?.target).toEqual({ kind: "entity", name: "Review" });
    expect(reviews?.ordered).toBe("ordered by createdAt desc");

    const search = s.roots.get("search");
    expect(search?.args).toEqual([{ name: "text", type: "Text" }]);
    expect(targetTypes(s, search!.target)).toEqual(["Human", "Droid", "Starship"]);
  });

  it("reads a schema fragment", () => {
    const frag = s.fragments.get("CharacterName");
    expect(frag?.on).toBe("Character");
    expect(frag?.selection).toBe("name");
  });

  it("reads mutations with args, return type, writes and effect", () => {
    const m = s.mutations.get("createReview");
    expect(m?.args.map((a) => `${a.name}:${a.type}`)).toEqual(["episode:Episode", "stars:W8", "commentary:Text?"]);
    expect(m?.returns).toBe("Review");
    expect(m?.allow).toBe("allow public");
    expect(m?.writes).toBe("writes Review(new), reviews(episode)");
    expect(m?.effect).toBe("effect transactional");
  });
});

describe("parseSchema — blog fixture (has one, claims, arg defaults, field policies)", () => {
  const s = parseSchema(readFixture("blog.lattice"));

  it("reads the closed claim registry", () => {
    expect(s.claims.map((c) => `${c.name}:${c.type}`)).toEqual(["org:OrgId", "role:Role"]);
  });

  it("reads a has-one edge and a field with args + default", () => {
    const post = s.entities.get("Post");
    const author = post?.edges.find((e) => e.name === "author");
    expect(author?.kind).toBe("one");
    expect(author?.by).toBe("authorId");
    expect(author?.target).toEqual({ kind: "entity", name: "User" });

    const user = s.entities.get("User");
    const avatar = user?.fields.find((f) => f.name === "avatarUrl");
    expect(avatar?.type).toBe("Url");
    expect(avatar?.args).toEqual([{ name: "size", type: "I32", default: "96" }]);
    const email = user?.fields.find((f) => f.name === "email");
    expect(email?.policy).toBe("visible when caller.org = orgId");
  });

  it("reads a private field and a bounded (max-only) collection", () => {
    const post = s.entities.get("Post");
    const draft = post?.fields.find((f) => f.name === "draftNotes");
    expect(draft?.policy).toBe("private");
    const tags = post?.edges.find((e) => e.name === "tags");
    expect(tags?.max).toBe(50);
    expect(tags?.page).toBeUndefined();
  });

  it("reads a fragment with variables", () => {
    const frag = s.fragments.get("UserAvatar");
    expect(frag?.args).toEqual([{ name: "size", type: "I32", default: "96" }]);
    expect(frag?.on).toBe("User");
  });
});

describe("parseSchema — canonical single-line form", () => {
  // The shape an origin serves at GET /schema/{hash}: sorted, single-line
  // members, `data` records, derived fields, and verb bindings with `{arg}`
  // paths whose braces must not confuse body matching.
  const canonical = `schema api.example.com

data ReviewInput { episode: Episode, stars: W8, commentary: Text? }

entity Saga by id {
  visible to all by default
  id: Text
  reviewCount: W32 derived reads sagaReviews count on read
  starTotal: I32 derived reads sagaReviews sum(stars) maintained
  title: Text
  has many sagaReviews: Review by episode max 100
  fetch by id: public
}

mutation deleteReview(review: ReviewId) returns Review {
  allow public
  writes Review(review), reviews(Review.episode)
  invalidates writes
  effect natural "deleting a named entity twice deletes it once"
  as DELETE /e/Review/{review}
  batch best-effort max 100 as DELETE /e/Review
}`;
  const s = parseSchema(canonical);

  it("parses a data record", () => {
    const r = s.records.get("ReviewInput");
    expect(r?.fields.map((f) => `${f.name}:${f.type}`)).toEqual(["episode:Episode", "stars:W8", "commentary:Text?"]);
  });

  it("captures derived-field read sets and materialization", () => {
    const saga = s.entities.get("Saga");
    const rc = saga?.fields.find((f) => f.name === "reviewCount");
    expect(rc?.derived).toBe("derived reads sagaReviews count on read");
    const st = saga?.fields.find((f) => f.name === "starTotal");
    expect(st?.derived).toBe("derived reads sagaReviews sum(stars) maintained");
    const edge = saga?.edges.find((e) => e.name === "sagaReviews");
    expect(edge?.max).toBe(100);
  });

  it("captures verb bindings whose paths contain braces", () => {
    const m = s.mutations.get("deleteReview");
    expect(m?.bindings).toEqual(["as DELETE /e/Review/{review}"]);
    expect(m?.batch).toBe("batch best-effort max 100 as DELETE /e/Review");
    expect(m?.effect).toBe('effect natural "deleting a named entity twice deletes it once"');
    // The body brace-matcher must have consumed exactly the mutation block.
    expect(s.diagnostics).toEqual([]);
    expect(entityMembers(s, "Saga").edges.map((e) => e.name)).toEqual(["sagaReviews"]);
  });
});

describe("parseSchema — directives + descriptions (directives fixture)", () => {
  const s = parseSchema(readFixture("directives.lattice"));

  it("recovers every directive declaration with its shape", () => {
    expect([...s.directiveDecls.keys()].sort()).toEqual(["audit", "internal", "label", "pii", "rateLimit"]);

    const audit = s.directiveDecls.get("audit");
    expect(audit?.repeatable).toBe(true);
    expect(audit?.locations).toEqual(["ENTITY", "FIELD", "MUTATION"]);

    // `directive @rateLimit(perMinute: I32, burst: I32 = 0)` — two params, and
    // only the second carries a default recovered from its `= …` clause.
    const rl = s.directiveDecls.get("rateLimit");
    expect(rl?.args.map((a) => a.name)).toEqual(["perMinute", "burst"]);
    expect(rl?.args[0]?.default).toBeUndefined();
    expect(rl?.args[1]?.default).toBe("0");
    expect(rl?.repeatable).toBe(false);
  });

  it("attaches a description + directive to a scalar field", () => {
    const email = s.entities.get("User")?.fields.find((f) => f.name === "email");
    expect(email?.description).toBe("Contact email — never shared-cached.");
    expect(email?.directives).toEqual([{ name: "pii", args: [] }]);
  });

  it("attaches a description + directive to a relationship (edge)", () => {
    const posts = s.entities.get("User")?.edges.find((e) => e.name === "posts");
    expect(posts?.description).toBe("Posts authored by this user.");
    expect(posts?.directives).toEqual([{ name: "internal", args: [] }]);
  });

  it("attaches a description + directive-with-arg to an entity", () => {
    const user = s.entities.get("User");
    expect(user?.description).toBe("A registered account holder.");
    expect(user?.directives).toEqual([{ name: "audit", args: [{ name: "level", value: '"sensitive"' }] }]);
  });

  it("attaches directives to a root and a mutation", () => {
    expect(s.roots.get("user")?.directives).toEqual([
      { name: "rateLimit", args: [{ name: "perMinute", value: "60" }] },
    ]);
    const publish = s.mutations.get("publish");
    expect(publish?.directives?.map((d) => d.name)).toEqual(["audit", "rateLimit"]);
    expect(publish?.directives?.[0]).toEqual({ name: "audit", args: [{ name: "level", value: '"sensitive"' }] });
  });

  it("preserves BOTH applications of a repeated directive on one field", () => {
    const createdAt = s.entities.get("Post")?.fields.find((f) => f.name === "createdAt");
    expect(createdAt?.directives).toEqual([
      { name: "audit", args: [] },
      { name: "audit", args: [{ name: "level", value: '"debug"' }] },
    ]);
  });
});

describe("parseSchema — schema-level description", () => {
  it("attaches a leading doc string before `schema` to the model description", () => {
    // The directives fixture carries no schema-level doc string, so this is
    // exercised directly: a leading JSON string before `schema` becomes the
    // model description.
    const s = parseSchema('"The public API surface." schema api.example.com\n');
    expect(s.description).toBe("The public API surface.");
  });

  it("does not misattach a following declaration's doc string to the schema", () => {
    // The first string in the fixture is @audit's doc, not the schema's.
    expect(parseSchema(readFixture("directives.lattice")).description).toBeUndefined();
  });
});

describe("parseSchema — docs on newtypes, records, and fragments", () => {
  // Documentation strings on these three declaration kinds were previously
  // dropped on the floor; assert each leading JSON doc string (and the
  // newtype's leading @internal directive) now lands on the model.
  const s = parseSchema(`schema demo.example.com

"A user's stable identifier."
@internal
newtype UserId = Text

"An address-book entry."
data Contact {
  email: Text
}

entity User by id {
  id: UserId
  name: Text
}

"A short display byline."
fragment UserByline on User { name }
`);

  it("parses the well-formed inline schema with zero diagnostics", () => {
    expect(s.diagnostics).toEqual([]);
  });

  it("captures a newtype's doc string, directive, and underlying type", () => {
    const nt = s.newtypes.get("UserId");
    expect(nt?.description).toBe("A user's stable identifier.");
    expect(nt?.directives).toEqual([{ name: "internal", args: [] }]);
    expect(nt?.type).toBe("Text");
  });

  it("captures a data record's doc string", () => {
    expect(s.records.get("Contact")?.description).toBe("An address-book entry.");
  });

  it("captures a fragment's doc string", () => {
    expect(s.fragments.get("UserByline")?.description).toBe("A short display byline.");
  });
});

describe("parseSchema — canonical form equivalence (line-terminator-insensitive)", () => {
  const fx = parseSchema(readFixture("directives.lattice"));
  const cn = parseSchema(readFixture("golden/directives.canonical.lattice"));
  const decls = (s: typeof fx) => [...s.directiveDecls].sort(([a], [b]) => (a < b ? -1 : 1));

  it("recovers identical directive declarations from multi-line and inline forms", () => {
    expect(decls(cn)).toEqual(decls(fx));
  });

  it("recovers identical element metadata regardless of layout", () => {
    const createdAt = (s: typeof fx) => {
      const f = s.entities.get("Post")?.fields.find((x) => x.name === "createdAt");
      return { description: f?.description, directives: f?.directives };
    };
    // Repeated @audit applications on a multi-line field vs. an inline one.
    expect(createdAt(cn)).toEqual(createdAt(fx));

    const email = (s: typeof fx) => {
      const f = s.entities.get("User")?.fields.find((x) => x.name === "email");
      return { description: f?.description, directives: f?.directives, policy: f?.policy };
    };
    // Description + directive + policy, inline in the canonical form.
    expect(email(cn)).toEqual(email(fx));
  });
});

describe("parseSchema — tolerance (never throws on malformed directive input)", () => {
  const malformed = [
    "directive @x on", // declaration missing its locations
    "@ent", // dangling annotation with no declaration to attach to
    '"a dangling leading doc string"', // description attached to nothing
    "directive @y(a:", // unterminated parameter list
    "directive @z(a: Text =", // dangling default value
  ];

  it("returns a well-formed model instead of throwing", () => {
    for (const src of malformed) {
      const s = parseSchema(src);
      expect(s.directiveDecls).toBeInstanceOf(Map);
      expect(Array.isArray(s.diagnostics)).toBe(true);
    }
  });

  it("partially recovers a directive declaration missing its locations", () => {
    const s = parseSchema("directive @x on");
    expect(s.directiveDecls.get("x")?.locations).toEqual([]);
    expect(s.directiveDecls.get("x")?.repeatable).toBe(false);
  });
});
