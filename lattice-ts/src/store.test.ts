/**
 * The normalized entity store: §9.1 merge semantics, tombstones (§9.3),
 * change subscriptions, and the `invalidated`-record staleness contract
 * (§11.3).
 */

import { describe, expect, it } from "vitest";
import { LatticeStore, newApplyOutcome } from "./store.ts";
import type { LatticeRecord } from "./wire.ts";

const entity = (id: string, ver: string, fields: Record<string, unknown>): LatticeRecord =>
  ({ kind: "entity", id, ver, fields }) as LatticeRecord;

describe("§9.1 entity merge semantics", () => {
  it("same ver: fields union (both responses are facts about one version)", () => {
    const store = new LatticeStore();
    store.applyRecords([entity("Human:1", "v1", { name: "Luke" })]);
    store.applyRecords([entity("Human:1", "v1", { homePlanet: "Tatooine" })]);
    expect(store.get("Human:1")).toEqual({
      ver: "v1",
      fields: { name: "Luke", homePlanet: "Tatooine" },
    });
  });

  it("ver change: fields replace; uncarried fields are dropped", () => {
    const store = new LatticeStore();
    store.applyRecords([entity("Human:1", "v1", { name: "Luke", homePlanet: "Tatooine" })]);
    store.applyRecords([entity("Human:1", "v2", { name: "Luke Skywalker" })]);
    expect(store.get("Human:1")).toEqual({
      ver: "v2",
      fields: { name: "Luke Skywalker" },
    });
  });

  it("same ver, same fields: a byte-identical record is a no-op (no notification)", () => {
    const store = new LatticeStore();
    const rec = entity("Human:1", "v1", { name: "Luke", tags: ["jedi", "pilot"] });
    store.applyRecords([rec]);
    let fired = 0;
    store.subscribe(["Human:1"], () => fired++);
    const before = store.version;
    store.applyRecords([entity("Human:1", "v1", { name: "Luke", tags: ["jedi", "pilot"] })]);
    expect(fired).toBe(0);
    expect(store.version).toBe(before);
  });

  it("tombstone evicts; a later fresh record resurrects (no tombstone memory)", () => {
    const store = new LatticeStore();
    store.applyRecords([entity("Human:1", "v1", { name: "Luke" })]);
    const outcome = store.applyRecords([{ kind: "tombstone", id: "Human:1", ver: "v2" }]);
    expect(store.get("Human:1")).toBeUndefined();
    expect(outcome.refs).toEqual(["Human:1"]);
    expect(outcome.committed).toBe(true);

    // Implemented behavior: the store keeps no tombstone memory, so a fresh
    // entity record re-inserts (the origin is the authority on existence).
    store.applyRecords([entity("Human:1", "v3", { name: "Luke, back" })]);
    expect(store.get("Human:1")).toEqual({ ver: "v3", fields: { name: "Luke, back" } });
  });

  it("unchanged and elided are no-ops on the entity map (§9.3, §10.4)", () => {
    const store = new LatticeStore();
    store.applyRecords([entity("Human:1", "v1", { name: "Luke" })]);
    const before = store.version;
    const outcome = store.applyRecords([
      { kind: "unchanged", id: "Human:1", ver: "v1" },
      { kind: "elided", id: "Human:2" },
    ]);
    expect(store.get("Human:1")).toEqual({ ver: "v1", fields: { name: "Luke" } });
    expect(store.get("Human:2")).toBeUndefined();
    expect(store.version).toBe(before);
    // ... but both still report their refs, and neither commits (§9.4.3).
    expect(outcome.refs).toEqual(["Human:1", "Human:2"]);
    expect(outcome.committed).toBe(false);
  });
});

describe("subscriptions", () => {
  it("fires exactly the subscribers of changed refs, once per flush", () => {
    const store = new LatticeStore();
    store.applyRecords([entity("Human:1", "v1", { name: "Luke" }), entity("Human:2", "v1", { name: "Leia" })]);
    let a = 0;
    let b = 0;
    store.subscribe(["Human:1"], () => a++);
    store.subscribe(["Human:2"], () => b++);

    store.applyRecords([entity("Human:1", "v2", { name: "Luke Skywalker" })]);
    expect(a).toBe(1);
    expect(b).toBe(0);

    // Two records touching one ref in a single batch coalesce to one call.
    store.applyRecords([
      entity("Human:1", "v3", { name: "L" }),
      entity("Human:1", "v3", { rank: "commander" }),
    ]);
    expect(a).toBe(2);
    expect(b).toBe(0);
  });

  it("unsubscribe stops delivery", () => {
    const store = new LatticeStore();
    let fired = 0;
    const off = store.subscribe(["Human:1"], () => fired++);
    store.applyRecords([entity("Human:1", "v1", { name: "Luke" })]);
    expect(fired).toBe(1);
    off();
    store.applyRecords([entity("Human:1", "v2", { name: "Luke S" })]);
    expect(fired).toBe(1);
  });

  it("refsVersion is monotonic over a ref set and moves only with it", () => {
    const store = new LatticeStore();
    const v0 = store.refsVersion(["Human:1"]);
    store.applyRecords([entity("Human:2", "v1", { name: "Leia" })]);
    expect(store.refsVersion(["Human:1"])).toBe(v0);
    store.applyRecords([entity("Human:1", "v1", { name: "Luke" })]);
    expect(store.refsVersion(["Human:1"])).toBeGreaterThan(v0);
  });
});

describe("§11.3 invalidated → query-registry staleness", () => {
  it("marks exactly the results whose key sets intersect the invalidated keys", () => {
    const store = new LatticeStore();
    store.registerResult("feedQuery", {
      rootOrder: { feed: ["Post:1"] },
      keys: new Set(["feed", "Post:1"]),
    });
    store.registerResult("heroQuery", {
      rootOrder: { hero: ["Droid:2001"] },
      keys: new Set(["Droid:2001"]),
    });
    let feedNotified = 0;
    let heroNotified = 0;
    store.subscribeResult("feedQuery", () => feedNotified++);
    store.subscribeResult("heroQuery", () => heroNotified++);

    store.applyRecord({ kind: "invalidated", keys: ["feed"] });

    expect(store.getResult("feedQuery")?.stale).toBe(true);
    expect(store.getResult("heroQuery")?.stale).toBe(false);
    expect(feedNotified).toBe(1);
    expect(heroNotified).toBe(0);

    // Already-stale entries are not re-notified.
    store.applyRecord({ kind: "invalidated", keys: ["feed", "Post:1"] });
    expect(feedNotified).toBe(1);
  });

  it("registerResult clears staleness; the outcome accumulates invalidated keys", () => {
    const store = new LatticeStore();
    store.registerResult("q", { rootOrder: {}, keys: new Set(["k"]) });
    const outcome = newApplyOutcome();
    store.applyRecord({ kind: "invalidated", keys: ["k"] }, outcome);
    expect(outcome.invalidated).toEqual(["k"]);
    expect(outcome.committed).toBe(true);
    expect(store.getResult("q")?.stale).toBe(true);

    store.registerResult("q", { rootOrder: {}, keys: new Set(["k"]) });
    expect(store.getResult("q")?.stale).toBe(false);
  });
});
