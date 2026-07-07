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
    expect(s.newtypes.get("HumanId")).toBe("Text");
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
