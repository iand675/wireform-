/**
 * Client construction for the dashboard, plus a tiny external store logging
 * every HTTP request the client issues — the debug footer renders it to make
 * the merged-query batching visible.
 *
 * Against a real origin: `VITE_LATTICE_BASE=http://localhost:8917 npm run dev`.
 * Without one: add `?mock` to the URL (or set `VITE_LATTICE_MOCK=1`) to run
 * against the in-browser mock origin.
 */

import type { LatticeRequestEvent } from "../../src/index.ts";
import { LatticeClient } from "../../src/index.ts";
import { createMockFetch } from "./mock.ts";

export interface LoggedRequest {
  readonly n: number;
  readonly method: string;
  readonly url: string;
}

class RequestLog {
  version = 0;
  entries: LoggedRequest[] = [];
  lastQuery: LatticeRequestEvent | undefined;
  private readonly listeners = new Set<() => void>();

  noteFetch(method: string, url: string): void {
    this.entries = [...this.entries, { n: this.entries.length + 1, method, url }];
    this.bump();
  }

  noteEvent(event: LatticeRequestEvent): void {
    if (event.kind !== "mutation") this.lastQuery = event;
    this.bump();
  }

  private bump(): void {
    this.version++;
    for (const cb of [...this.listeners]) cb();
  }

  readonly subscribe = (cb: () => void): (() => void) => {
    this.listeners.add(cb);
    return () => {
      this.listeners.delete(cb);
    };
  };

  readonly getSnapshot = (): number => this.version;
}

export const requestLog = new RequestLog();

const base = (import.meta.env.VITE_LATTICE_BASE as string | undefined) ?? "http://localhost:8917";
export const usingMock =
  import.meta.env.VITE_LATTICE_MOCK === "1" || new URLSearchParams(window.location.search).has("mock");

const transport = usingMock ? createMockFetch() : (url: string, init?: RequestInit) => fetch(url, init);

export const client = new LatticeClient({
  base,
  fetch: (url, init) => {
    requestLog.noteFetch(init?.method ?? "GET", url);
    return transport(url, init);
  },
  onRequest: (event) => requestLog.noteEvent(event),
});
