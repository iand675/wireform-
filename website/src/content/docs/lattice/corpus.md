---
title: Lattice for GraphQL developers
description: "A worked example corpus: the GraphQL constructs every GraphQL developer has memorized, ported to Lattice side by side, with notes on what changed and why."
sidebar:
  order: 3
  label: GraphQL comparison corpus
---

This is a companion to the Lattice protocol specification, not a replacement for it. The spec argues from first principles. This document works forward from examples that any GraphQL-familiar reader already has memorized, and shows the Lattice port next to the original. The goal is a corpus usable enough to drive early client and origin implementations, not just a comparison table.

## How to read an entry

Each entry has three parts: the GraphQL original (schema fragment and operation), the Lattice port (IDL fragment and query or mutation), and a note on what changed and why, with a pointer into the spec where the full argument lives. Lattice's query surface is GraphQL-shaped, so most entries port almost verbatim. The value of those entries is in the note: what looks identical often behaves differently underneath (a normalized entity stream instead of a JSON tree, mandatory pagination, collection-based invalidation). A few constructs have no Lattice equivalent at all (aliasing, `@include`, `@skip`); those entries explain the replacement rather than pretending the syntax maps.

Spec section numbers below refer to the current specification. The query language is GraphQL-shaped (brace-nested, with fragments and field arguments), and the schema (IDL) reads as a domain model where `has many` and `list` define their collection inline.

---

## 0. Shared schema

Most entries below share one small schema. It is adapted from the schema GraphQL's own documentation has used for years to teach the language (a galaxy far, far away; characters, films, and a review system). It is recognizable on purpose. It appears once here in both languages, and later entries assume it.

### GraphQL SDL

```graphql
enum Episode { NEWHOPE, EMPIRE, JEDI }

interface Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
}

type Human implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  homePlanet: String
}

type Droid implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  primaryFunction: String
}

type Starship {
  id: ID!
  name: String!
  length: Float
}

union SearchResult = Human | Droid | Starship

type Review {
  episode: Episode!
  stars: Int!
  commentary: String
}

input ReviewInput {
  stars: Int!
  commentary: String
}

type Query {
  hero(episode: Episode): Character
  human(id: ID!): Human
  droid(id: ID!): Droid
  search(text: String!): [SearchResult]
}

type Mutation {
  createReview(episode: Episode!, review: ReviewInput!): Review
}

type Subscription {
  reviewAdded(episode: Episode): Review
}
```

### Lattice IDL

```lattice
newtype HumanId    = Text
newtype DroidId    = Text
newtype StarshipId = Text
newtype ReviewId   = Text

enum Episode closed = NewHope | Empire | Jedi

interface Character {
  name:      Text
  appearsIn: [Episode]

  has many friends: Character by ownerId
                    ordered by name asc
                    page 10 max 50
}

entity Human by id implements Character {
  visible to all by default

  id:         HumanId
  name:       Text
  homePlanet: Text?
  appearsIn:  [Episode]

  has many friends: Character by ownerId
                    ordered by name asc
                    page 10 max 50

  fetch by id: public
}

entity Droid by id implements Character {
  visible to all by default

  id:              DroidId
  name:            Text
  primaryFunction: Text?
  appearsIn:       [Episode]

  has many friends: Character by ownerId
                    ordered by name asc
                    page 10 max 50

  fetch by id: public
}

entity Starship by id {
  visible to all by default
  id:     StarshipId
  name:   Text
  length: F64?
  fetch by id: public
}

entity Review by id {
  visible to all by default
  id:         ReviewId
  episode:    Episode
  stars:      W8
  commentary: Text?
  createdAt:  Timestamp
  fetch by id: public
}

fragment CharacterName on Character { name }

get hero(episode: Episode?) of (Human | Droid) public

list reviews of Review by episode
     ordered by createdAt desc
     page 10 max 50
     public

list search(text: Text) of (Human | Droid | Starship)
     ordered by name asc
     page 10 max 50
     public

mutation createReview(episode: Episode, stars: W8, commentary: Text?) returns Review {
  allow       public
  writes      Review(new), reviews(episode)
  invalidates writes
  effect      transactional
}
```

`Character` is declared as an interface. An `interface` names the fields every implementer shares (`name`, `appearsIn`, and the `friends` collection), and `Human` and `Droid` opt in with `implements Character`. Nothing is ever *stored* as a bare `Character`. Entities remain concretely typed, and an interface has no entity identity of its own. The declaration exists so that relationship targets (`has many friends: Character`) and fragment positions (`fragment CharacterName on Character`) resolve, and so the compiler can check that every member declares the common fields compatibly.

The `friends` collection spans both concrete types: a friend list mixes species freely, same as the original. Its `by ownerId` link column is declared by no member, which the spec permits as a storage-level join column. Friendship is many-to-many, so the loader resolves it from join storage rather than a scalar foreign key. A schema that wants the join to be a queryable entity would reify it (Section 3.6).

`SearchResult`'s union needs no declaration. The `search` root's target is written inline as the alternative list, an anonymous interface with no common fields. `search` is also the corpus's example of a **parameter-backed** list root: `text` is a query parameter, not a stored column, so the collection's cache-tag family groups by the parameter (`search:{text}`), and its keyset orders by a real column (`name`) so cursors stay derivable.

---

## 1. Basic queries with nested fields

**GraphQL**

```graphql
{
  hero {
    name
    friends {
      name
    }
  }
}
```

**Lattice**

```lattice
query Hero {
  hero {
    name
    friends(first: 10) { name }
  }
}
```

Nearly the same query. `friends` takes a page argument (`first: 10`) because the schema declares it a *paginated* collection: a character's friend list can grow without a natural bound, so it is walked, not returned whole. A collection that is small by nature (a character's `appearsIn` episodes, say) would be declared *bounded* and would need no argument (Section 3.6). GraphQL's list fields are unbounded by default and page only if you build Relay connections by hand. Lattice makes the bounded-versus-paginated call once, in the schema, and small lists stay plain arrays. What differs underneath is bigger than pagination: the response is a normalized entity stream, not the nested JSON tree GraphQL returns (Entry 11).

---

## 2. Arguments and variables

**GraphQL**

```graphql
query HeroNameAndFriends($episode: Episode) {
  hero(episode: $episode) {
    name
    friends {
      name
    }
  }
}
```

**Lattice**

```lattice
query HeroNameAndFriends($episode: Episode) {
  hero(episode: $episode) {
    name
    friends(first: 10) { name }
  }
}
```

Essentially identical to GraphQL. One restriction is not visible here: `hero(episode: $episode)` works because `episode` is a grouping key the schema declared, and argument-based filtering is permitted *only* on grouping keys, not on arbitrary fields (Section 4.6). A GraphQL server can accept any resolver argument. Lattice accepts only the ones the schema pre-committed to serving and invalidating.

---

## 3. Aliases

**GraphQL**

```graphql
query FetchTwoHeroes {
  empireHero: hero(episode: EMPIRE) { name }
  jediHero:   hero(episode: JEDI)   { name }
}
```

**Lattice: no direct port**

There is no alias mechanism (Section 4.1). GraphQL aliasing exists to disambiguate two selections of one field in a response tree. Lattice's wire is keyed by entity identity and canonical field name, so there is no tree and nothing to relabel, and the same root field cannot appear twice under two client-chosen labels. There are two workarounds, and the choice between them is a substantive design decision, not a formatting one.

**As two requests:**

```lattice
query EmpireHero { hero(episode: EMPIRE) { name } }
query JediHero   { hero(episode: JEDI)   { name } }
```

Two canonical texts, two hashes, two independently cacheable and independently tenured responses. If the Empire hero and the Jedi hero are actually two different pieces of UI with two different change rates (one show's cast is fixed, another's is still airing), this is a better fit than GraphQL's version. GraphQL fuses them into one response that can only be as fresh, or as cacheable, as its least stable half.

**As two schema-declared roots**, if the pairing itself is a stable product concept worth naming:

```lattice
get empireHero of Human  where episode = Empire  public
get jediHero   of Human  where episode = Jedi    public
```

These can be combined into one multi-root query (Section 4.3) if a single fetch is wanted:

```lattice
query Heroes {
  empireHero { name }
  jediHero   { name }
}
```

This fits the common case where an aliasing use is not really "two arbitrary calls to the same field" but "two fixed, named things that happen to share an implementation." That is how most aliasing in production GraphQL schemas is actually used.

---

## 4. Fragments

**GraphQL**

```graphql
query CompareTwoHeroes {
  leftComparison: hero(episode: EMPIRE) { ...comparisonFields }
  rightComparison: hero(episode: JEDI)  { ...comparisonFields }
}

fragment comparisonFields on Character {
  name
  appearsIn
  friends { name }
}
```

**Lattice**

```lattice
-- fragments/character.lq
fragment Comparison on Character {
  name
  appearsIn
  friends(first: 10) { name }
}
```

```lattice
-- queries/empire_hero.lq
import "fragments/character.lq"
query EmpireHero { hero(episode: EMPIRE) { ...Comparison } }
```

```lattice
-- queries/jedi_hero.lq
import "fragments/character.lq"
query JediHero { hero(episode: JEDI) { ...Comparison } }
```

This is a near-verbatim port. Lattice fragments are GraphQL fragments (Section 4.5), spread with `...Comparison`, defined in a shared file, and `import`ed. The only thing that does not carry over is fusing both heroes into one operation via aliasing, for the reasons in Entry 3. Each query expands at build time into its own self-contained canonical text, with no fragment reference remaining and no server-side knowledge that a fragment was involved. Two builds that expand identically share one cache identity automatically, which a build system reusing fragments gets for free.

---

## 5. Directives: `@include` and `@skip`

**GraphQL**

```graphql
query Hero($episode: Episode, $withFriends: Boolean!) {
  hero(episode: $episode) {
    name
    friends @include(if: $withFriends) { name }
  }
}
```

**Lattice: no port, by design**

Conditional field inclusion is absent (Section 4.7). A client-toggleable selection multiplies a query's cache identity by the size of the toggle space, which is exactly the property content addressing depends on staying small and enumerable. Two replacements match the guidance already in the spec:

**Split into two queries**, the usual answer:

```lattice
query HeroName($episode: Episode) {
  hero(episode: $episode) { name }
}

query HeroNameAndFriends($episode: Episode) {
  hero(episode: $episode) {
    name
    friends(first: 10) { name }
  }
}
```

A client fetches the first when the friends panel is collapsed and the second when it opens. Each caches at whatever rate its own audience produces, rather than one response whose freshness has to serve both cases.

**Over-fetch**, when the excluded field is cheap: drop the conditional entirely and always request `friends`. For a small, always-resident field this is often the better trade than a second query. The spec says so plainly (Section 4.7) rather than treating every over-fetch as a defeat.

---

## 6. Interfaces and unions

**GraphQL**

```graphql
{
  search(text: "an") {
    ... on Human { name, homePlanet }
    ... on Droid { name, primaryFunction }
    ... on Starship { name, length }
  }
}
```

**Lattice**

```lattice
query Search($text: Text) {
  search(text: $text, first: 10) {
    ... on Human    { name homePlanet }
    ... on Droid    { name primaryFunction }
    ... on Starship { name length }
  }
}
```

This is GraphQL, verbatim: inline fragments `... on Type` are Lattice's interface-dispatch mechanism too (Section 4.4). The difference is not syntactic. The difference is what happens to a concrete type the query does not mention. A GraphQL union member with no matching inline fragment returns `{}` (the `__typename`, if requested; otherwise effectively nothing distinguishable). A Lattice interface edge emits an unlisted concrete type as a bare typed ref, identity only, which a client can render as a placeholder or point-fetch on demand rather than silently dropping. The loaders behind this batch per concrete type (Section 4.5), so adding a third or fourth searchable type later costs the plan one more per-type loader dispatch, not a new round.

---

## 7. Scalars

**GraphQL** ships five built-in scalars: `Int` (32-bit signed), `Float` (IEEE 754 double), `String`, `Boolean`, and `ID` (serialized as a string, opaque). Everything else (dates, money, UUIDs) is either a custom scalar with server-defined and often under-specified serialization, or smuggled through `String`.

**Lattice** starts from a wider catalog because the wire format has to survive hashing into etags and round-tripping through JavaScript clients without corruption (Section 3.5.3):

| GraphQL | Lattice | Why it's different |
|---|---|---|
| `Int` | `I32` (or `I8`/`I16`/`W8`/`W16`/`W32` as the domain calls for) | GraphQL's `Int` is always 32-bit signed regardless of what is being counted. Lattice makes the width and signedness a deliberate modeling choice. |
| `Float` | `F64` (or `F32`) | Same IEEE representation, but Lattice pins the canonical rendering (shortest round-trip, no `NaN`/`Infinity` on the wire) so two encoders cannot disagree about `0.1` and silently fork a cache entry. |
| `String` | `Text` | Both are UTF-8. Lattice additionally requires NFC normalization, because text feeds into canonicalization elsewhere in the protocol. |
| `Boolean` | `Bool` | No difference. |
| `ID` | a declared `newtype`, e.g. `newtype HumanId = Uuid` | GraphQL's `ID` erases the underlying representation and the type it identifies. A Lattice newtype keeps both, nominally distinct (`HumanId` and `DroidId` cannot be confused at the type level), at zero wire cost. |
| a custom `Money` scalar, usually serialized as a float and quietly wrong | `Decimal` | Arbitrary-precision, serialized as a normalized decimal string. This is the representation a ledger or order book needs and a float categorically cannot give. |
| a custom scalar for large IDs, often serialized as a string to dodge `Int`'s 32-bit ceiling | `I64` / `W64` / `Integer` | Built in, and specified to serialize as a decimal string because a bare JSON number here is where a JavaScript client silently loses precision above 2^53. |
| nothing built in for binary data | `Bytes`, `Bytes n`, `Bit n` | GraphQL has no native byte-string scalar at all. Every GraphQL API either invents a base64-string custom scalar per field or avoids binary data entirely. |

The general pattern: GraphQL's scalar set covers what a typical CRUD API needs and leaves everything precision-sensitive (money, wide integers, binary data) to ad hoc custom scalars with no shared serialization convention across servers. Lattice's catalog is wider up front so that a trading system, a ledger, or anything else where a silently-wrong float or a truncated integer costs money does not have to invent its own scalar and hope every client agrees on how to parse it.

---

## 8. Pagination: Relay connections

**GraphQL**, following the Relay connection spec, which is where most production GraphQL APIs get their pagination pattern:

```graphql
{
  hero {
    friends(first: 2, after: "opaqueCursor1") {
      edges {
        cursor
        node { name }
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
```

```json
{
  "data": {
    "hero": {
      "friends": {
        "edges": [
          { "cursor": "opaqueCursor2", "node": { "name": "Han Solo" } },
          { "cursor": "opaqueCursor3", "node": { "name": "Leia Organa" } }
        ],
        "pageInfo": { "hasNextPage": true, "endCursor": "opaqueCursor3" }
      }
    }
  }
}
```

**Lattice**

```lattice
query HeroFriends {
  hero {
    friends(first: 2, after: "cur_8f2") { name }
  }
}
```

```ndjson
{"kind":"entity","id":"Human:1000","ver":"e41",
 "fields":{"friends(first:2,after:\"cur_8f2\")":
   {"$page":{"items":[{"$ref":"Human:1002"},{"$ref":"Human:1003"}],
             "next":"cur_a91","prev":"cur_7c3"}}}}
{"kind":"entity","id":"Human:1002","ver":"b02","fields":{"name":"Han Solo"}}
{"kind":"entity","id":"Human:1003","ver":"c19","fields":{"name":"Leia Organa"}}
```

Two things are gone. First, the requirement to paginate at all: a bounded collection is a plain array (Section 3.6). Second, when you do paginate, the `edges { cursor, node { ... } }` wrapper.

The wrapper deserves explanation, because Relay's design was reasonable for what it had to work with. Relay needed a place to hang a per-edge `cursor` inside a response that is otherwise a plain nested tree. It introduced an intermediate `edges` object whose only job is carrying that cursor next to the node it belongs to. Lattice's wire format has no tree to begin with. Entities are flat, deduplicated, and referenced by id, so there is nothing for an edge wrapper to attach to, and there does not need to be. A cursor is a canonical encoding of an item's own keyset column values (Section 3.2), so any client that already holds `Human:1002` with its `name` field can derive that item's cursor locally without the server ever sending one. `pageInfo.hasNextPage` collapses to `next != null`. `pageInfo.endCursor` is just `next`. This is not a reduced version of Relay's design. It is Relay's design with the wrapper removed once the reason for the wrapper no longer applies.

The one substantive capability difference: Relay connections are forward-and-optionally-backward via `first`/`after` and `last`/`before`, which Lattice matches exactly (Section 3.6). Lattice also adds an `around` window Relay does not have, for jumping to a page centered on a permalinked item without walking from the start.

---

## 9. The N+1 problem

**GraphQL**

```graphql
{
  posts {
    title
    author { name }
  }
}
```

This is the textbook example DataLoader exists to fix. A naive resolver-per-field GraphQL server executes one query to list the posts, then a separate resolver call to fetch each post's author: N+1 round trips to the data layer for N posts. The fix is not part of the GraphQL language. It is an application-level cache-and-batch library (DataLoader) that every non-trivial GraphQL server is expected to wire in by hand, per field, and it is easy to forget on a field nobody thought to test with more than one result.

**Lattice**

```lattice
query PostsWithAuthors {
  posts(first: 20) {
    title
    author { name }
  }
}
```

The same query, and there is no library to remember to install, because the failure mode DataLoader patches over is not expressible in the first place. A relationship in the schema is declared as a set-in, map-out loader (Section 3.1):

```haskell
type Loader parent child =
  NESet (Key parent) -> m (Map (Key parent) (Page (Key child)))
```

There is no function type in the schema model that takes one parent key and returns one child. Per-row resolution is not a thing a schema author can accidentally write. This is like a Haskell function typed `[a] -> [b]` that cannot secretly be implemented by looping and hitting the database once per element without the type giving any indication that is what is happening, except here the type itself forecloses the failure rather than merely obscuring it.

The plan for this query is one round for `posts`, then one round batching every distinct author id the first round produced into a single `User` loader call (Section 19.2). The trace is two spans regardless of whether there are three posts or three thousand. GraphQL's version of this query has an execution cost that scales with result size in a way its own syntax gives no warning about. Lattice's plan has a structure fixed at compile time, and its cost is visible in `explain` before a single row is fetched.

---

## 10. Mutations

**GraphQL**

```graphql
mutation CreateReviewForEpisode($ep: Episode!, $review: ReviewInput!) {
  createReview(episode: $ep, review: $review) {
    stars
    commentary
  }
}
```

```json
{ "data": { "createReview": { "stars": 5, "commentary": "This is a great movie!" } } }
```

**Lattice**

```
POST /m/createReview
Content-Type: application/json
Idempotency-Key: 7e2c1a90-...

{"episode":"Jedi","stars":5,"commentary":"This is a great movie!"}
```

```ndjson
{"kind":"manifest","mutation":"createReview","root":{"result":["Review:501"]},"etag":"m:c210"}
{"kind":"entity","id":"Review:501","ver":"a01",
 "fields":{"episode":"Jedi","stars":5,"commentary":"This is a great movie!"}}
{"kind":"invalidated","keys":["Review:501","reviews:Jedi"]}
{"kind":"end","complete":true}
```

The domain content is identical. Everything else around it is new, because GraphQL's mutation model stops at "here is a field that runs a side effect and returns some data," and Lattice's starts there. The IDL declaration from Section 0 additionally states, and the compiler checks, three things: exactly which rows this mutation may write (`writes Review(new), reviews(episode)`); exactly what a client retrying this call is protected against (`Idempotency-Key`, at-most-once acceptance, Section 11.2); and exactly what caches this write invalidates (`invalidated`, flowing to any CDN sitting in front of a query that lists reviews for this episode).

None of that has a GraphQL equivalent. A GraphQL mutation resolver is free to be non-idempotent, and a retried `createReview` after a dropped connection creates a second review with no protocol-level way to prevent it, short of the client and server privately agreeing on some ad hoc deduplication scheme.

---

## 11. Partial failure and errors

**GraphQL**

```json
{
  "data": {
    "hero": {
      "name": "R2-D2",
      "heroFriends": [
        { "id": "1000", "name": "Luke Skywalker" },
        null,
        { "id": "1003", "name": "Leia Organa" }
      ]
    }
  },
  "errors": [
    {
      "message": "Name for character with ID 1002 could not be fetched.",
      "path": ["hero", "heroFriends", 1, "name"]
    }
  ]
}
```

This is GraphQL's canonical partial-failure form, and it is an awkward one to consume. The response is `200 OK` regardless of what is in `errors`. `data` and `errors` can both be present with no formal statement of how they relate beyond the `path` array. A client has to walk `path` against the tree it just parsed to figure out which value the error concerns: `null` at that position for a nullable field, or a hole further up for a non-nullable one.

**Lattice**

```ndjson
{"kind":"manifest","query":"...","plan":"...","slice":"pub","root":{"hero":["Droid:2001"]},"etag":"m:..."}
{"kind":"entity","id":"Droid:2001","ver":"f10","fields":{"name":"R2-D2",
   "friends(first:3)":{"$page":{"items":[{"$ref":"Human:1000"},{"$ref":"Human:1002"},{"$ref":"Human:1003"}]}}}}
{"kind":"entity","id":"Human:1000","ver":"a01","fields":{"name":"Luke Skywalker"}}
{"kind":"error","scope":"Human:1002","code":"lattice:loader-timeout","retryable":true}
{"kind":"entity","id":"Human:1003","ver":"c19","fields":{"name":"Leia Organa"}}
{"kind":"end","complete":true}
```

The failure is scoped to `Human:1002` directly, by entity id, rather than by a path a client has to interpret relative to a tree that mixes it into `data`. At the HTTP layer the response is `207` rather than `200` (Section 9.4.6), the coarse signal for any infrastructure that only inspects headers. The in-body `error` record remains what a client actually acts on: it names what failed (`Scope`, Section 9.4.1), how (`code`, drawn from the same `lattice:` namespace as whole-request errors, Section 9.4.2), and whether retrying is worth it (`retryable`).

GraphQL's design has no equivalent of `retryable` at all. Every error is presented identically whether it is a transient timeout or a permanent validation failure, leaving the client to guess. And GraphQL's response stays cacheable-by-convention at `200` even when it is visibly degraded. A `207` here is specifically excluded from RFC 9111's default-cacheable status list, so a shared cache defaults to caution on exactly the response that most needs it (Section 9.4.6). The origin additionally self-purges its own surrogate keys immediately so the degraded copy does not linger even under an explicit override.

---

## 12. Bulk mutations

**GraphQL** has no native construct for "apply this mutation to many inputs in one call." Community practice has landed on a few workarounds, none of them satisfying.

One is a mutation field that accepts a list argument (`createReviews(reviews: [ReviewInput!]!): [Review]`), which the schema author hand-writes per mutation, with no shared idempotency or partial-failure story. Another is transport-level query batching: several unrelated GraphQL *operations* packed into one HTTP request by a client-side link. That batches network calls, not domain semantics, and gives the server no idea the operations are related. For truly large volumes, teams bolt on an entirely separate asynchronous API next to GraphQL. Shopify's bulk operations API is one example: it accepts a mutation, runs it as a background job, and has the client poll or subscribe for a JSONL result file. GraphQL's synchronous request-response mutation model was never going to scale to bulk without leaving the language.

**Lattice**

```lattice
mutation cancelOrder(order: OrderId) returns Order {
  allow when   caller.org = orgId
  writes       Order(order), open_orders(Order.userId)
  invalidates  writes
  effect       transactional
  errors       CancelError open = AlreadyFilled | AlreadyCancelled | NotFound
  batch        best-effort max 500
}
```

```
POST /m/cancelOrder
Idempotency-Key: batch_20260704_09

[{"key":"ord_a","order":"501"}, {"key":"ord_b","order":"502"}, {"key":"ord_c","order":"503"}]
```

The response is `207 Multi-Status`:

```ndjson
{"kind":"manifest","mutation":"cancelOrder","batch":{"atomicity":"best-effort","count":3},
 "root":{"items":["ord_a","ord_b","ord_c"]},"etag":"m:c9f1"}
{"kind":"entity","id":"Order:501","ver":"g10","fields":{"status":"cancelled"},"item":"ord_a"}
{"kind":"error","scope":{"$tag":"Item","item":"ord_b"},"error":{"$tag":"AlreadyFilled"},"retryable":false}
{"kind":"entity","id":"Order:503","ver":"h02","fields":{"status":"cancelled"},"item":"ord_c"}
{"kind":"end","complete":true}
```

Bulk invocation (Section 11.8) is a declared property of the mutation, not a separately hand-rolled field. It comes with the same guarantees the singular form has, extended rather than abandoned: an envelope-level `Idempotency-Key` protecting the submission; a per-item `key` protecting each item independently (so resubmitting a batch with one new order appended does not reprocess the two that already committed); a declared `errors` vocabulary an item's failure is reported against; and a runtime-enforced write-set bracket per item (Section 11.4).

The Shopify approach needed a second API and an asynchronous job-and-poll model to get bulk operations working at all. Here it is the same mutation, the same schema declaration, the same protocol, with `best-effort` chosen because canceling three orders has no reason to be all-or-nothing. Where a batch does need one-transaction-or-nothing semantics, `AllOrNothing` is available for mutations whose effect class supports it (Section 11.8). A synchronous, job-free bulk API of this kind was never in a position to offer that either way.

---

## 13. Subscriptions

**GraphQL**

```graphql
subscription OnCommentAdded($postID: ID!) {
  commentAdded(postID: $postID) {
    id
    content
    author { name }
  }
}
```

**Lattice**

```
GET /q/8f2c41a9?p=pl_9dK2&postId=501&live=sse
Accept: text/event-stream
```

using an ordinary previously-registered query,

```lattice
query CommentsOnPost($postId: PostId) {
  comments(post: $postId, first: 20) {
    content
    author { name }
  }
}
```

There is no separate subscription language or distinct `Subscription` root type; any hash-form query URL can be opened live (Section 12). GraphQL's subscription and its equivalent query for the same data are two different operations, defined against two different schema roots, that a client has to keep in sync by hand: one for the initial load and a structurally different one for updates after. Here they are the same query used two ways. It is fetched once for the initial render, or opened with `live=sse` for a push stream of the identical wire records whenever an invalidation touches something the query's surrogate keys cover. A client's parsing and store-merge code does not know or care which mode produced a given record.

---

## 14. Introspection

**GraphQL**

```graphql
{
  __schema {
    types { name kind }
  }
}
__type(name: "Human") {
  fields { name type { name } }
}
```

**Lattice: no runtime equivalent, by design**

```
GET /schema/current
```

```lattice
schema api.example.com

entity Human by id {
  visible to all by default
  id:         HumanId
  name:       String
  homePlanet: String?
  ...
}
```

GraphQL's introspection system exists because a GraphQL schema is, at the protocol level, private server state that clients need a query to ask about. A Lattice schema is instead a plain, content-addressed, publicly cacheable document (Section 7.1), fetched with an ordinary `GET`, servable forever once fetched by hash, and readable by a human or a codegen tool without executing anything against the API it describes. The capability introspection provides is not lost: everything `__schema` and `__type` exist to expose is already in the one document every client, and every piece of tooling in Section 20, was going to need to read anyway.

---

## 15. Shared ids across types: subclassing and 1:1 joins

**GraphQL**

```graphql
# The two shapes teams actually ship. Either "subclassing": one row whose
# admin view is a type sharing the User's ID space...
type User  { id: ID!, name: String! }
type AdminUser { id: ID!, permissions: [Role!]! }   # same id as its User

# ...or an identifying 1:1 join, PK = FK:
type UserProfile { id: ID!, bio: String, location: String }
type User { id: ID!, name: String!, profile: UserProfile }
```

GraphQL's spec does not address either case. Both types mint unrelated identities in every client cache, and whether an `updateAdminUser` mutation should invalidate a cached `User` is left to resolver comments and `refetchQueries` lists, with no protocol-level guidance.

**Lattice**

```lattice
entity User by id {
  visible to all by default
  id:   UserId
  name: String

  has one profile: UserProfile by id     -- identity edge: link field is the key
  has one admin:   AdminUser   by id
}

-- 1:1 identifying join: its OWN record of truth behind the shared id.
entity UserProfile joins User {
  visible to all by default
  bio:      Markdown?
  location: String?

  has one user: User by id
}

-- Subclass view: the SAME record of truth as the base.
entity AdminUser refines User {
  private by default
  permissions: [Role]   visible when caller.role = Admin
}
```

The id reuse that GraphQL leaves as convention is a declaration here, because the two cases mean opposite things on the network (Section 3.8). `joins` declares *adjacent truth*: `UserProfile:7` has its own `ver`, its own surrogate keys, and its own lifecycle. A profile edit does not purge any cached response that only touched `User.name`. `refines` declares *the same truth*: one shared `ver` sequence, and a write to any family member mints surrogate keys for the whole family (`AdminUser:7` and `User:7`), so a permissions change invalidates every cached view of that row, and deleting the `User` tombstones the family.

The decision rule is invalidation coupling, not ORM aesthetics. One write invalidating both views means `refines`. Independently aging truths mean `joins`. Ids reused across *unrelated* types need nothing at all: identity is always the type-qualified pair, so `Invoice:42` and `User:42` never meet in a cache key, a ref, or a purge. And because the family fan-out is static in the schema, an out-of-band writer (a consumer materializing rows off a Kafka topic, Section 11.5) mints exactly the same keys the mutation path would.

---

## 16. Non-null and list cardinality

**GraphQL**

```graphql
type Order {
  id: ID!
  memo: String              # nullable is the default
  lineItems: [LineItem!]!   # non-null list of non-null items... but can be []
}

input CreateOrder {
  lineItems: [LineItemInput!]!   # nothing requires nonemptiness
}
```

GraphQL's `!` works against its own default (everything is nullable unless annotated). It cannot express "at least one" (`[T!]!` still admits `[]`), and it enforces non-null by **null propagation**: a violation nulls the nearest nullable ancestor, so one bad row can blank out an entire subtree of otherwise-good data.

**Lattice**

```lattice
entity Order by id {
  visible to all by default
  id:    OrderId
  memo:  String?                  -- optional is the annotation, required the default

  has one  buyer:  User by buyerId          -- exactly one: a dangle is an error
  has one? coupon: Coupon by couponId       -- zero-or-one, declared

  has many lineItems: LineItem by orderId
           min 1 max 200                    -- "an order has line items"
}

mutation createOrder(lines: [LineInput]+) returns Order { ... }
```

The defaults flip and the gaps fill in. Fields are required unless marked `?`, so the annotation burden sits on the exception. `has one` is a contract that the edge resolves; `has one?` is the declared maybe (and a required edge over an optional link column is rejected at elaboration, because the schema cannot promise what the column cannot). Bounded collections take a `min` floor, and the nonempty list `[t]+` lives in the one type language that fields, mutation inputs, and variables share. So "you cannot create an order with no lines" is the same declaration that types the field (spec §3.4-3.6). Violations behave like every other Lattice integrity problem: an `Edge`-scoped `lattice:cardinality` / `lattice:collection-underflow` error record in a `207` response that keeps the rest of the data. There is no null propagation, because a normalized entity stream has no tree to blank out.

---

## 17. Federation

**GraphQL (Apollo Federation)**

```graphql
# posts subgraph
type Post @key(fields: "id") {
  id: ID!
  title: String
}

# social subgraph
extend type Post @key(fields: "id") {
  id: ID! @external
  reactionCount: Int!
}
```

Composition runs through a dedicated toolchain (rover, a composition spec, a supergraph SDL), and the gateway executes query plans against subgraphs over bespoke `_entities` POSTs that no HTTP cache can do anything with.

**Lattice**

```lattice
-- social upstream's IDL
extend entity Post {
  has many reactions: Reaction by postId
  reactionCount: W32 derived reads reactions count on read
}
```

The gateway fuses the upstream IDLs with the same algebra in-process modules use (Section 18.1), publishes the fused document content-addressed like any schema, and compiles fused queries into per-upstream subplans that are themselves ordinary Lattice queries: canonicalized, content-addressed, issued as hash-form GETs. Cross-upstream joins ride the `nodes` root (Section 14.4).

**What changed.** Three structural differences, all downstream of content addressing and normalized streams. First, the gateway-to-upstream hop is ordinary cacheable HTTP, so a CDN can sit between gateway and upstream and behave correctly; Apollo's `_entities` POSTs forfeit that hop. Second, there is no tree stitching: upstream records forward as they arrive, tagged `src`, and the client store keys versions per (entity, contributing upstream), so one upstream's partial failure degrades exactly its own fields (Section 18.4). Third, moving a module in or out of process changes deployment topology and nothing in any client's query, because in-process fusion and network federation share one composition model; there is no supergraph artifact to regenerate and ship. Invalidation composes through the same declared footprints: each upstream's outbox relay publishes a subscribable feed (Section 18.6) the gateway translates into its own cache tier's purges.

## Index

| # | GraphQL feature | Lattice equivalent | Spec section |
|---|---|---|---|
| 1 | Nested field selection | View blocks, mandatory pagination bounds | 4.2, 3.6 |
| 2 | Variables | Typed variables, near-identical | 4.2 |
| 3 | Aliases | Separate queries, or named roots + multi-statement | 4.1, 4.3 |
| 4 | Fragments | `fragment`s, imported and build-time composed | 4.5 |
| 5 | `@include`/`@skip` | Split queries, or over-fetch | 4.7 |
| 6 | Interfaces/unions | `... on Type` inline fragments | 4.4 |
| 7 | Scalars | Wider primitive catalog, pinned wire forms | 3.5.3 |
| 8 | Relay connections | keyset pagination via field args, derivable cursors | 3.6 |
| 9 | N+1 / DataLoader | Set-in map-out loaders, inexpressible N+1 | 3.1, 19.2 |
| 10 | Mutations | Declared writes/invalidates/idempotency | 11.1-11.4 |
| 11 | `errors` array | Scoped `error` records, `207` | 9.4, 9.4.6 |
| 12 | Bulk mutation workarounds | Declared `batch` policy | 11.8 |
| 13 | Subscriptions | Live queries over ordinary query URLs | 12 |
| 14 | Introspection | Content-addressed schema documents | 7.1 |
| 15 | Shared ids: subclassing / 1:1 joins | Co-keyed entities: `refines` / `joins` | 3.8 |
| 16 | Non-null (`!`), list cardinality | Required-by-default + `?`, `has one?`, `min N`, `[t]+` | 3.4-3.6 |
| 17 | Apollo Federation | `extend entity` + one fusion algebra, subplans as ordinary queries | 18 |
