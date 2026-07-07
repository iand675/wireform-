/**
 * `mountExplorer` — the explorer's vanilla-DOM UI. A GraphiQL-shaped IDE for
 * Lattice: a schema-documentation sidebar, a schema-aware query editor with a
 * results panel (denormalized data, raw entity-stream records, the `explain`
 * plan, and response headers), and an IDL authoring pane with live structural
 * validation and origin compatibility checks.
 *
 * Zero third-party dependencies: it builds real DOM, injects one scoped
 * stylesheet, and drives everything through an `ExplorerSession` (which the
 * caller can supply a custom `fetch` to). The UI layer holds no protocol
 * knowledge — it renders what the session and the schema model expose.
 */

import type { FetchLike } from "../client.ts";
import { completeQuery, lintQuery, type Diagnostic } from "./complete.ts";
import { createEditor, type Editor } from "./editor.ts";
import { highlightIdl, highlightJson, highlightQuery } from "./highlight.ts";
import {
  ExplorerSession,
  type CacheInfo,
  type CheckReport,
  type ExplainDoc,
  type LoaderInfo,
  type LoadedSchema,
  type MutationResult,
  type RunMode,
  type RunResult,
  type SliceName,
  type SlicesRun,
} from "./session.ts";
import {
  parseSchema,
  targetLabel,
  type EdgeModel,
  type FieldModel,
  type SchemaModel,
} from "./schema.ts";
import { injectStyles } from "./styles.ts";
import { button, icon } from "./components.ts";
import { type LatticeRecord } from "../wire.ts";

export interface ExplorerOptions {
  /** Origin base, e.g. `"http://localhost:8917"`. */
  readonly base: string;
  readonly fetch?: FetchLike;
  readonly claims?: Readonly<Record<string, unknown>>;
  readonly vcAuth?: string;
  readonly slice?: SliceName;
  readonly defaultQuery?: string;
  readonly defaultMode?: RunMode;
  readonly authToken?: string;
}

export interface ExplorerHandle {
  readonly el: HTMLElement;
  readonly session: ExplorerSession;
  /** Re-fetch discovery + schema and repopulate the docs and IDL pane. */
  reload(): Promise<void>;
  destroy(): void;
}

// ---------------------------------------------------------------------------
// DOM helpers

type Child = Node | string | null | undefined | false;

function h(tag: string, attrs: Record<string, unknown> = {}, ...children: Child[]): HTMLElement {
  const el = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (v === false || v === null || v === undefined) continue;
    if (k === "class") el.className = String(v);
    else if (k === "html") el.innerHTML = String(v);
    else if (k.startsWith("on") && typeof v === "function") el.addEventListener(k.slice(2).toLowerCase(), v as EventListener);
    else el.setAttribute(k, String(v));
  }
  for (const c of children) {
    if (c === null || c === undefined || c === false) continue;
    el.appendChild(typeof c === "string" ? document.createTextNode(c) : c);
  }
  return el;
}

function clear(el: HTMLElement): void {
  while (el.firstChild) el.removeChild(el.firstChild);
}

/** Renders `text` into `container`, wrapping every case-insensitive match of `filter` in a `<mark>`. */
function highlightText(container: HTMLElement, text: string, filter: string): void {
  clear(container);
  if (!filter) {
    container.appendChild(document.createTextNode(text));
    return;
  }
  const lower = text.toLowerCase();
  let cursor = 0;
  let idx = lower.indexOf(filter);
  if (idx === -1) {
    container.appendChild(document.createTextNode(text));
    return;
  }
  while (idx !== -1) {
    if (idx > cursor) container.appendChild(document.createTextNode(text.slice(cursor, idx)));
    container.appendChild(h("mark", { class: "lx-match" }, text.slice(idx, idx + filter.length)));
    cursor = idx + filter.length;
    idx = lower.indexOf(filter, cursor);
  }
  if (cursor < text.length) container.appendChild(document.createTextNode(text.slice(cursor)));
}

/** A typed `<select>` with the given options (avoids casting a generic element). */
function selectEl(options: Array<{ value: string; label: string }>): HTMLSelectElement {
  const s = document.createElement("select");
  for (const o of options) {
    const opt = document.createElement("option");
    opt.value = o.value;
    opt.textContent = o.label;
    s.appendChild(opt);
  }
  return s;
}

function errMessage(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}

/**
 * Wire a drag `handle` to resize `pane` (the first track of the CSS grid
 * `container`) by writing a pixel size into `cssVar`. `axis` picks width vs
 * height; the size is clamped to `[min, containerExtent - reserve]`.
 */
function wireResize(
  handle: HTMLElement,
  container: HTMLElement,
  pane: HTMLElement,
  axis: "x" | "y",
  cssVar: string,
  min: number,
  reserve: number,
): void {
  let startPos = 0;
  let startSize = 0;
  let dragging = false;
  handle.addEventListener("pointerdown", (e: PointerEvent) => {
    dragging = true;
    handle.classList.add("lx-dragging");
    handle.setPointerCapture(e.pointerId);
    startPos = axis === "x" ? e.clientX : e.clientY;
    const rect = pane.getBoundingClientRect();
    startSize = axis === "x" ? rect.width : rect.height;
    document.body.style.userSelect = "none";
    e.preventDefault();
  });
  handle.addEventListener("pointermove", (e: PointerEvent) => {
    if (!dragging) return;
    const cur = axis === "x" ? e.clientX : e.clientY;
    const crect = container.getBoundingClientRect();
    const extent = axis === "x" ? crect.width : crect.height;
    const size = Math.max(min, Math.min(extent - reserve, startSize + (cur - startPos)));
    container.style.setProperty(cssVar, `${size}px`);
  });
  const end = (e: PointerEvent): void => {
    dragging = false;
    handle.classList.remove("lx-dragging");
    try {
      handle.releasePointerCapture(e.pointerId);
    } catch {
      /* pointer already released */
    }
    document.body.style.userSelect = "";
  };
  handle.addEventListener("pointerup", end);
  handle.addEventListener("pointercancel", end);
}

/** A teaching empty state: a lead line plus a dimmer explainer (both may carry `<kbd>`). */
function teach(lead: string, rest: string): HTMLElement {
  return h("div", { class: "lx-teach" }, h("div", { class: "lx-teach-lead", html: lead }), h("div", { html: rest }));
}

// ---------------------------------------------------------------------------
// JSON rendering (regex highlight over the pretty-printed text — no recursion,
// no casts; refs like "Type:key" get their own color).

const JSON_TOKEN = /("(?:\\u[a-fA-F0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\btrue\b|\bfalse\b|\bnull\b|-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)/g;
const REF_LIKE = /^"[A-Z][A-Za-z0-9_]*:.+"$/;

function escapeHtml(s: string): string {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function renderJsonHtml(value: unknown): string {
  let json: string;
  try {
    json = JSON.stringify(value, null, 2) ?? String(value);
  } catch {
    json = String(value);
  }
  return escapeHtml(json).replace(JSON_TOKEN, (match) => {
    let cls = "lx-json-num";
    if (match.startsWith('"')) {
      if (/:\s*$/.test(match)) cls = "lx-json-key";
      else cls = REF_LIKE.test(match) ? "lx-json-ref" : "lx-json-str";
    } else if (match === "true" || match === "false" || match === "null") {
      cls = "lx-json-bool";
    }
    return `<span class="${cls}">${match}</span>`;
  });
}

// ---------------------------------------------------------------------------
// Docs tree

const ICON_SEARCH =
  '<svg viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg"><circle cx="7" cy="7" r="5" stroke="currentColor" stroke-width="1.5"/><path d="M11 11L14.5 14.5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>';
const ICON_CLEAR =
  '<svg viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M4 4L12 12M12 4L4 12" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/></svg>';

function fieldRow(f: FieldModel, filter: string, onInsert: (text: string) => void): HTMLElement {
  const name = h("span", { class: "lx-row-name", title: f.derived ?? f.policy ?? "" });
  highlightText(name, f.name, filter);
  name.addEventListener("click", () => onInsert(f.name));
  return h(
    "div",
    { class: "lx-row lx-mem" },
    h("span", { class: "lx-dot" }, "·"),
    name,
    h("span", { class: "lx-row-type" }, `: ${f.type}`),
    f.derived ? h("span", { class: "lx-tag" }, "derived") : undefined,
  );
}

function edgeRow(e: EdgeModel, filter: string, onInsert: (text: string) => void): HTMLElement {
  const name = h("span", { class: "lx-row-name" });
  highlightText(name, e.name, filter);
  name.addEventListener("click", () => onInsert(`${e.name} { }`));
  const tag = e.kind === "many" ? "many" : e.optional ? "one?" : "one";
  return h(
    "div",
    { class: "lx-row lx-mem" },
    h("span", { class: "lx-dot lx-dot-edge" }, "→"),
    name,
    h("span", { class: "lx-row-type" }, targetLabel(e.target)),
    h("span", { class: "lx-tag" }, tag),
  );
}

/** A collapsible sidebar section with a chevron, title, and count badge. */
function group(title: string, open: boolean, rows: HTMLElement[]): HTMLElement | undefined {
  if (rows.length === 0) return undefined;
  const summary = h(
    "summary",
    {},
    h("span", { class: "lx-chev" }),
    document.createTextNode(title),
    h("span", { class: "lx-sec-count" }, String(rows.length)),
  );
  const details = h("details", { class: "lx-sec", ...(open ? { open: "" } : {}) }, summary);
  for (const r of rows) details.appendChild(r);
  return details;
}

/** A collapsible entity/interface with a name, key/members chip, and member rows. */
function typeBox(name: string, chip: string, members: HTMLElement[], filter: string): HTMLElement {
  const nameEl = h("span", { class: "lx-ent-name" });
  highlightText(nameEl, name, filter);
  const summary = h(
    "summary",
    {},
    h("span", { class: "lx-chev" }),
    nameEl,
    chip ? h("span", { class: "lx-key" }, chip) : undefined,
  );
  const body = h("div", { class: "lx-ent-body" });
  for (const m of members) body.appendChild(m);
  return h("details", { class: "lx-ent" }, summary, body);
}

function renderDocs(host: HTMLElement, model: SchemaModel, onInsert: (text: string) => void, onMutation?: (name: string) => void): void {
  const prevFilter = host.querySelector<HTMLInputElement>(".lx-search")?.value ?? "";
  clear(host);
  const search = document.createElement("input");
  search.className = "lx-search";
  search.type = "search";
  search.placeholder = "Filter schema…";
  search.autocomplete = "off";
  search.spellcheck = false;
  search.setAttribute("aria-label", "Filter schema");
  search.value = prevFilter;
  const count = h("span", { class: "lx-search-count" });
  const hint = h("span", { class: "lx-search-kbd" }, "/");
  const clearBtn = h("button", { class: "lx-search-clear", type: "button", title: "Clear filter (Esc)", html: ICON_CLEAR });
  const wrap = h(
    "div",
    { class: "lx-search-wrap" },
    h("span", { class: "lx-search-icon", html: ICON_SEARCH }),
    search,
    count,
    hint,
    clearBtn,
  );
  const tree = h("div", { class: "lx-docs" });
  host.appendChild(h("div", { class: "lx-search-bar" }, wrap));
  host.appendChild(tree);

  const build = (filter: string): void => {
    clear(tree);
    const f = filter.trim().toLowerCase();
    const match = (name: string): boolean => f === "" || name.toLowerCase().includes(f);

    const rootRows: HTMLElement[] = [];
    for (const r of model.roots.values()) {
      if (!match(r.name)) continue;
      const name = h("span", { class: "lx-row-name" });
      highlightText(name, r.name, f);
      name.addEventListener("click", () => onInsert(`${r.name} { }`));
      const argSig = r.args.length ? `(${r.args.map((a) => `${a.name}: ${a.type}`).join(", ")})` : "";
      rootRows.push(
        h(
          "div",
          { class: "lx-row" },
          h("span", { class: `lx-kind lx-kind-${r.kind}` }, r.kind),
          name,
          argSig ? h("span", { class: "lx-row-type" }, argSig) : undefined,
          h("span", { class: "lx-row-arrow" }, `→ ${targetLabel(r.target)}`),
        ),
      );
    }

    const entityRows: HTMLElement[] = [];
    for (const e of model.entities.values()) {
      const members = [...e.fields, ...e.edges];
      if (!match(e.name) && !members.some((m) => match(m.name))) continue;
      const rows: HTMLElement[] = [];
      for (const fld of e.fields) if (match(fld.name) || match(e.name)) rows.push(fieldRow(fld, f, onInsert));
      for (const edg of e.edges) if (match(edg.name) || match(e.name)) rows.push(edgeRow(edg, f, onInsert));
      entityRows.push(typeBox(e.name, e.key.join(", "), rows, f));
    }

    const ifaceRows: HTMLElement[] = [];
    for (const i of model.interfaces.values()) {
      if (!match(i.name)) continue;
      const rows: HTMLElement[] = [];
      for (const fld of i.fields) rows.push(fieldRow(fld, f, onInsert));
      for (const edg of i.edges) rows.push(edgeRow(edg, f, onInsert));
      ifaceRows.push(typeBox(i.name, i.members.join(", "), rows, f));
    }

    const mutationRows: HTMLElement[] = [];
    for (const m of model.mutations.values()) {
      if (!match(m.name)) continue;
      const argSig = m.args.map((a) => `${a.name}: ${a.type}`).join(", ");
      const name = h("span", { class: onMutation ? "lx-row-name" : "lx-name" });
      highlightText(name, m.name, f);
      if (onMutation) {
        const mn = m.name;
        name.addEventListener("click", () => onMutation(mn));
      }
      mutationRows.push(
        h(
          "div",
          { class: "lx-row", title: [m.allow, m.writes, m.effect].filter(Boolean).join("\n") },
          h("span", { class: "lx-kind lx-kind-mut" }, "mut"),
          name,
          h("span", { class: "lx-row-type" }, `(${argSig})`),
          m.returns ? h("span", { class: "lx-row-arrow" }, `→ ${m.returns}`) : undefined,
        ),
      );
    }

    const typeRows: HTMLElement[] = [];
    for (const en of model.enums.values()) {
      if (!match(en.name)) continue;
      const name = h("span", { class: "lx-ent-name" });
      highlightText(name, en.name, f);
      typeRows.push(
        h(
          "div",
          { class: "lx-row" },
          h("span", { class: "lx-kind lx-kind-enum" }, "enum"),
          name,
          h("span", { class: "lx-row-type" }, en.values.join(" | ")),
        ),
      );
    }
    for (const [name, under] of model.newtypes) {
      if (!match(name)) continue;
      const nameEl = h("span", { class: "lx-ent-name" });
      highlightText(nameEl, name, f);
      typeRows.push(
        h(
          "div",
          { class: "lx-row" },
          h("span", { class: "lx-kind lx-kind-newtype" }, "new"),
          nameEl,
          h("span", { class: "lx-row-type" }, `= ${under}`),
        ),
      );
    }

    const groups = [
      group("Roots", true, rootRows),
      group("Entities", f !== "", entityRows),
      group("Interfaces", false, ifaceRows),
      group("Mutations", false, mutationRows),
      group("Types", false, typeRows),
    ];
    let shown = 0;
    for (const g of groups) {
      if (g) {
        g.classList.add("lx-reveal");
        g.style.setProperty("--i", String(shown++));
        tree.appendChild(g);
      }
    }
    if (shown === 0) {
      tree.appendChild(
        h(
          "div",
          { class: "lx-empty" },
          model.roots.size === 0 ? "No schema loaded yet. Point the explorer at a running origin." : "No matches for that filter.",
        ),
      );
    }

    const matches = rootRows.length + entityRows.length + ifaceRows.length + mutationRows.length + typeRows.length;
    count.textContent = String(matches);
    count.style.display = f === "" ? "none" : "";
    const hasText = filter.length > 0;
    clearBtn.style.display = hasText ? "" : "none";
    hint.style.display = hasText ? "none" : "";
  };

  clearBtn.addEventListener("click", () => {
    search.value = "";
    build("");
    search.focus();
  });
  search.addEventListener("input", () => build(search.value));
  search.addEventListener("keydown", (e) => {
    if (e.key !== "Escape") return;
    if (search.value) {
      search.value = "";
      build("");
    } else {
      search.blur();
    }
  });
  build(prevFilter);
}

// ---------------------------------------------------------------------------
// Results rendering

function renderData(host: HTMLElement, value: unknown): void {
  clear(host);
  host.appendChild(h("pre", { class: "lx-json", html: renderJsonHtml(value) }));
}

function renderRecords(host: HTMLElement, result: RunResult): void {
  clear(host);
  if (result.records.length === 0) {
    host.appendChild(h("pre", { class: "lx-json" }, result.raw || "(no records)"));
    return;
  }
  for (const rec of result.records) host.appendChild(recordRow(rec));
}

function bar(used: number, limit: number): HTMLElement {
  const pct = limit > 0 ? Math.min(100, Math.round((used / limit) * 100)) : 0;
  const track = h("span", { class: "lx-bar-track" }, h("span", { class: "lx-bar", style: `width:${pct}%` }));
  return h("span", {}, track, document.createTextNode(`${used} / ${limit}`));
}

function renderExplain(host: HTMLElement, explain: ExplainDoc | undefined): void {
  clear(host);
  if (!explain) {
    host.appendChild(h("div", { class: "lx-empty" }, "No plan. Run with mode “introduce” (not one-shot) to fetch the explain document."));
    return;
  }
  if (explain.budgets) {
    const sec = h("div", { class: "lx-explain-sec" }, h("h4", {}, "Budgets"));
    const table = h("table", { class: "lx-kv" });
    for (const [k, v] of Object.entries(explain.budgets)) table.appendChild(h("tr", {}, h("td", {}, k), h("td", {}, bar(v.used, v.limit))));
    sec.appendChild(table);
    host.appendChild(sec);
  }
  if (explain.elements && explain.elements.length) {
    const sec = h("div", { class: "lx-explain-sec" }, h("h4", {}, "Path join (slices)"));
    const table = h("table", { class: "lx-kv" });
    for (const el of explain.elements) {
      table.appendChild(
        h("tr", {}, h("td", {}, `${el.path}${el.type ? ` : ${el.type}` : ""}`), h("td", {}, h("span", { class: "lx-chip" }, el.slice), document.createTextNode(" " + el.derivation))),
      );
    }
    sec.appendChild(table);
    host.appendChild(sec);
  }
  if (explain.surrogateKeys && explain.surrogateKeys.length) {
    const sec = h("div", { class: "lx-explain-sec" }, h("h4", {}, "Surrogate keys"));
    for (const k of explain.surrogateKeys) sec.appendChild(h("div", { class: "lx-row-type", style: "padding:2px 0" }, `${k.collection} — grouping [${k.grouping.join(", ")}]`));
    host.appendChild(sec);
  }
  const rounds = explain.rounds ?? [];
  if (rounds.length) {
    const sec = h("div", { class: "lx-explain-sec" }, h("h4", {}, `Loader rounds (${rounds.length})`));
    sec.appendChild(h("pre", { class: "lx-json", html: renderJsonHtml(rounds) }));
    host.appendChild(sec);
  }
}

/** A loader's short label: prefer its collection, else the raw loader string. */
function shortLoader(l: LoaderInfo): string {
  return l.collection ?? l.loader ?? "loader";
}

/**
 * The trace waterfall: a wall-clock request timeline (concurrent slices + the
 * shared explain fetch, each bar split into TTFB / body with a first-record
 * tick) over the run's `startedAt`→`finishedAt` axis, then the plan's loader
 * rounds as a structural staircase (bar width ∝ fanout; rounds sequential).
 */
function renderTrace(host: HTMLElement, run: SlicesRun): void {
  clear(host);
  const kindVar: Record<string, string> = { pub: "--lx-accent", ctx: "--lx-warn", priv: "--lx-num" };
  interface Row {
    label: HTMLElement;
    cvar: string;
    start: number;
    ttfb: number;
    end: number;
    first?: number;
    ms: number;
    ok: boolean;
  }
  const rows: Row[] = [];
  for (const slice of run.order) {
    const r = run.slices[slice];
    if (!r) continue;
    const t = r.timing;
    rows.push({
      label: h("span", { class: `lx-slice lx-slice-${slice}` }, slice),
      cvar: kindVar[slice] ?? "--lx-accent",
      start: t.requestStart,
      ttfb: t.responseStart,
      end: t.responseEnd,
      ...(t.firstRecord !== undefined ? { first: t.firstRecord } : {}),
      ms: r.durationMs,
      ok: r.ok,
    });
  }
  if (run.explainTiming) {
    rows.push({
      label: h("span", { class: "lx-chip" }, "explain"),
      cvar: "--lx-type",
      start: run.explainTiming.requestStart,
      ttfb: run.explainTiming.requestStart,
      end: run.explainTiming.responseEnd,
      ms: run.explainTiming.responseEnd - run.explainTiming.requestStart,
      ok: true,
    });
  }
  if (rows.length === 0) {
    host.appendChild(h("div", { class: "lx-empty" }, "No timing captured for this run."));
    return;
  }

  const t0 = Math.min(...rows.map((r) => r.start));
  const tEnd = Math.max(...rows.map((r) => r.end));
  const span = Math.max(tEnd - t0, 0.001);
  const pct = (x: number): number => ((x - t0) / span) * 100;

  const sec1 = h("div", { class: "lx-wf-sec" });
  sec1.appendChild(h("h4", {}, "Request timeline ", h("span", { class: "lx-wf-dim" }, `· ${span.toFixed(1)} ms wall, ${rows.length} request${rows.length === 1 ? "" : "s"}`)));
  const wf = h("div", { class: "lx-wf" });
  const axis = h("div", { class: "lx-wf-axis" });
  for (let i = 0; i <= 4; i++) {
    const ms = (span * i) / 4;
    axis.appendChild(h("span", { style: `left:${i * 25}%` }, `${ms.toFixed(ms < 10 ? 1 : 0)}${i === 4 ? " ms" : ""}`));
  }
  wf.appendChild(axis);
  const plot = h("div", { class: "lx-wf-plot" });
  const grid = h("div", { class: "lx-wf-grid" });
  for (const g of [25, 50, 75]) grid.appendChild(h("i", { style: `left:${g}%` }));
  plot.appendChild(grid);
  for (const r of rows) {
    const left = pct(r.start);
    const width = Math.max(pct(r.end) - left, 0.4);
    const inner = Math.max(r.end - r.start, 0.001);
    const waitFrac = Math.max(0, Math.min(100, ((r.ttfb - r.start) / inner) * 100));
    const detail = `${(r.end - r.start).toFixed(1)} ms · TTFB ${(r.ttfb - r.start).toFixed(1)} ms${r.first !== undefined ? ` · first record ${(r.first - r.start).toFixed(1)} ms` : ""}`;
    const bar = h("div", { class: `lx-wf-bar${r.ok ? "" : " lx-wf-bad"}`, style: `left:${left}%;width:${width}%;--c:var(${r.cvar})`, title: detail });
    bar.appendChild(h("span", { class: "lx-wf-wait", style: `width:${waitFrac}%` }));
    bar.appendChild(h("span", { class: "lx-wf-body" }));
    if (r.first !== undefined) {
      const f = Math.max(0, Math.min(100, ((r.first - r.start) / inner) * 100));
      bar.appendChild(h("span", { class: "lx-wf-first", style: `left:${f}%` }));
    }
    const track = h("span", { class: "lx-wf-track" }, bar);
    plot.appendChild(h("div", { class: "lx-wf-row" }, r.label, track, h("span", { class: "lx-wf-ms" }, `${r.ms.toFixed(0)} ms`)));
  }
  wf.appendChild(plot);
  sec1.appendChild(wf);
  host.appendChild(sec1);

  const rounds = run.explain?.rounds ?? [];
  if (!rounds.length) {
    host.appendChild(h("div", { class: "lx-wf-note" }, "No plan rounds — run in Introduce or Replay (not one-shot) to see the loader waterfall."));
    return;
  }
  const roundSum = (rd: { loaders: readonly LoaderInfo[] }): number => rd.loaders.reduce((s, l) => s + (l.fanout ?? 1), 0);
  const totalFanout = Math.max(1, rounds.reduce((a, rd) => a + roundSum(rd), 0));
  const peak = Math.max(0, ...rounds.map(roundSum));
  const sec2 = h("div", { class: "lx-wf-sec" });
  sec2.appendChild(h("h4", {}, "Plan execution ", h("span", { class: "lx-wf-dim" }, `· ${rounds.length} round${rounds.length === 1 ? "" : "s"}, peak fanout ${peak}`)));
  sec2.appendChild(h("div", { class: "lx-wf-note" }, "Rounds run in sequence; loaders within a round run in parallel. Bar width ∝ fanout (keys loaded)."));
  const plot2 = h("div", { class: "lx-wf-plot lx-wf-plan" });
  let cum = 0;
  for (const rd of rounds) {
    const rf = roundSum(rd);
    plot2.appendChild(h("div", { class: "lx-wf-round" }, `Round ${rd.round}`, h("span", { class: "lx-wf-dim" }, ` · fanout ${rf}`)));
    let off = 0;
    for (const l of rd.loaders) {
      const fan = l.fanout ?? 1;
      const left = ((cum + off) / totalFanout) * 100;
      const width = (fan / totalFanout) * 100;
      const bar = h("div", { class: "lx-wf-bar", style: `left:${left}%;width:${width}%;--c:var(--lx-accent-2)`, title: l.loader ?? shortLoader(l) }, h("span", { class: "lx-wf-body" }));
      const track = h("span", { class: "lx-wf-track" }, bar);
      plot2.appendChild(h("div", { class: "lx-wf-row" }, h("span", { class: "lx-wf-label", title: l.loader ?? "" }, shortLoader(l)), track, h("span", { class: "lx-wf-ms" }, `×${fan}`)));
      off += fan;
    }
    cum += rf;
  }
  sec2.appendChild(plot2);
  host.appendChild(sec2);
}

function renderResponse(host: HTMLElement, result: RunResult): void {
  clear(host);
  const table = h("table", { class: "lx-kv" });
  const row = (k: string, v: string): void => {
    table.appendChild(h("tr", {}, h("td", {}, k), h("td", {}, v)));
  };
  row("status", String(result.status));
  row("duration", `${result.durationMs.toFixed(1)} ms`);
  row("cache", result.cache.detail);
  row("request", `${result.request.method} ${result.request.url}`);
  row("canonical", result.request.canonicalText);
  if (result.hash) row("hash", result.hash);
  if (result.headers.surrogateKeys.length) row("surrogate-key", result.headers.surrogateKeys.join(" "));
  for (const [k, v] of Object.entries(result.headers.all)) row(k, v);
  host.appendChild(table);
}

function renderCheck(host: HTMLElement, report: CheckReport): void {
  clear(host);
  if (report.problem) {
    host.appendChild(h("div", { class: "lx-report-fail" }, `HTTP ${report.status}`));
    host.appendChild(h("pre", { class: "lx-json", html: renderJsonHtml(report.problem) }));
    return;
  }
  const pass = report.report ? report.report["pass"] : undefined;
  const banner =
    pass === true
      ? h("div", { class: "lx-report-pass" }, "PASS — candidate is compatible")
      : pass === false
        ? h("div", { class: "lx-report-fail" }, "FAIL — incompatible change")
        : h("div", { class: "lx-report" }, `HTTP ${report.status}`);
  host.appendChild(banner);
  host.appendChild(h("pre", { class: "lx-json", html: renderJsonHtml(report.report ?? {}) }));
}

// ---------------------------------------------------------------------------
// Query-workshop widgets

interface Segmented {
  readonly el: HTMLElement;
  get(): string;
  set(value: string): void;
}

/** A single-select segmented button group. */
function segmented(
  items: ReadonlyArray<{ value: string; label: string; title?: string }>,
  initial: string,
  onChange: (value: string) => void,
): Segmented {
  const el = h("div", { class: "lx-seg" });
  const btns = new Map<string, HTMLElement>();
  let cur = initial;
  const set = (value: string): void => {
    cur = value;
    for (const [k, b] of btns) b.classList.toggle("lx-seg-on", k === value);
  };
  for (const it of items) {
    const b = h("button", { class: "lx-seg-btn", ...(it.title ? { title: it.title } : {}) }, it.label);
    b.addEventListener("click", () => {
      if (cur === it.value) return;
      set(it.value);
      onChange(it.value);
    });
    btns.set(it.value, b);
    el.appendChild(b);
  }
  set(initial);
  return { el, get: () => cur, set };
}

/** Human label for a cache status (with age / freshness when known). */
function cacheLabel(cache: CacheInfo): string {
  switch (cache.status) {
    case "hit":
      return cache.age !== undefined ? `HIT · ${cache.age}s` : "HIT";
    case "miss":
      return "MISS";
    case "stale":
      return "STALE";
    case "revalidated":
      return "304";
    case "cacheable":
      return cache.maxAge !== undefined ? `cacheable · ${cache.maxAge}s` : "cacheable";
    case "private":
      return "private";
    case "dynamic":
      return "dynamic";
    default:
      return "—";
  }
}

/** A colored chip describing a response's cache relationship. */
function cacheChip(cache: CacheInfo): HTMLElement {
  return h("span", { class: `lx-cache lx-cache-${cache.status}`, title: cache.detail }, cacheLabel(cache));
}

/** A small round status light: ok / error / streaming. */
function statusDot(kind: "ok" | "err" | "run"): HTMLElement {
  return h("span", { class: `lx-sdot lx-sdot-${kind}` });
}

/** One NDJSON record row: a kind badge + its JSON. */
function recordRow(rec: LatticeRecord): HTMLElement {
  return h("div", { class: "lx-rec" }, h("span", { class: `lx-badge lx-badge-${rec.kind}` }, rec.kind), h("code", { html: renderJsonHtml(rec) }));
}

interface SliceCard {
  readonly el: HTMLElement;
  readonly body: HTMLElement;
  setStatus(status: number, ok: boolean): void;
  setCache(cache: CacheInfo): void;
  setDuration(ms: number): void;
  stop(): void;
}

/** A per-slice result card: a header (slice, status, cache, timing) plus a body. */
function sliceCard(slice: SliceName): SliceCard {
  const dot = statusDot("run");
  const cacheSlot = h("span", { class: "lx-cache-slot" });
  const dur = h("span", { class: "lx-scard-dur" });
  const spin = h("span", { class: "lx-run-dot" });
  const head = h(
    "div",
    { class: "lx-scard-head" },
    h("span", { class: `lx-slice lx-slice-${slice}` }, slice),
    dot,
    cacheSlot,
    h("span", { class: "lx-spacer" }),
    dur,
    spin,
  );
  const body = h("div", { class: "lx-scard-body" });
  const el = h("div", { class: "lx-scard" }, head, body);
  return {
    el,
    body,
    setStatus: (status, ok) => {
      dot.className = `lx-sdot lx-sdot-${ok ? "ok" : "err"}`;
      dot.title = String(status);
    },
    setCache: (cache) => {
      clear(cacheSlot);
      cacheSlot.appendChild(cacheChip(cache));
    },
    setDuration: (ms) => {
      dur.textContent = `${ms.toFixed(0)} ms`;
    },
    stop: () => {
      spin.style.display = "none";
    },
  };
}

interface SliceBoard {
  readonly dataWrap: HTMLElement;
  readonly recWrap: HTMLElement;
  readonly respWrap: HTMLElement;
  begin(slices: readonly SliceName[]): void;
  onHead(ev: { slice: SliceName; status: number; ok: boolean; cache: CacheInfo }): void;
  onRecord(ev: { slice: SliceName; record: LatticeRecord; index: number; data: unknown }): void;
  finalize(run: SlicesRun): void;
}

/** The three result panes (data / records / response), each a stack of per-slice cards. */
function makeBoard(): SliceBoard {
  const dataWrap = h("div", { class: "lx-result-body lx-scards" });
  const recWrap = h("div", { class: "lx-result-body lx-scards lx-hidden" });
  const respWrap = h("div", { class: "lx-result-body lx-scards lx-hidden" });
  let cards: Record<string, { data: SliceCard; records: SliceCard; response: SliceCard }> = {};
  let pending: Record<string, unknown> = {};
  let queued = false;
  const flush = (): void => {
    queued = false;
    for (const [slice, data] of Object.entries(pending)) {
      const c = cards[slice];
      if (c) renderData(c.data.body, data);
    }
    pending = {};
  };
  return {
    dataWrap,
    recWrap,
    respWrap,
    begin: (slices) => {
      cards = {};
      pending = {};
      clear(dataWrap);
      clear(recWrap);
      clear(respWrap);
      for (const slice of slices) {
        const data = sliceCard(slice);
        const records = sliceCard(slice);
        const response = sliceCard(slice);
        data.body.appendChild(h("div", { class: "lx-scard-wait" }, "streaming…"));
        records.body.appendChild(h("div", { class: "lx-scard-wait" }, "streaming…"));
        cards[slice] = { data, records, response };
        dataWrap.appendChild(data.el);
        recWrap.appendChild(records.el);
        respWrap.appendChild(response.el);
      }
    },
    onHead: (ev) => {
      const c = cards[ev.slice];
      if (!c) return;
      for (const card of [c.data, c.records, c.response]) {
        card.setStatus(ev.status, ev.ok);
        card.setCache(ev.cache);
      }
    },
    onRecord: (ev) => {
      const c = cards[ev.slice];
      if (!c) return;
      if (ev.index === 0) clear(c.records.body);
      c.records.body.appendChild(recordRow(ev.record));
      pending[ev.slice] = ev.data;
      if (!queued) {
        queued = true;
        requestAnimationFrame(flush);
      }
    },
    finalize: (run) => {
      for (const slice of run.order) {
        const c = cards[slice];
        const r = run.slices[slice];
        if (!c || !r) continue;
        for (const card of [c.data, c.records, c.response]) {
          card.setStatus(r.status, r.ok);
          card.setCache(r.cache);
          card.setDuration(r.durationMs);
          card.stop();
        }
        renderData(c.data.body, r.problem ?? r.data);
        renderRecords(c.records.body, r);
        renderResponse(c.response.body, r);
      }
    },
  };
}

// ---------------------------------------------------------------------------
// mountExplorer

const DEFAULT_QUERY = "query {\n  \n}\n";

export function mountExplorer(container: HTMLElement, options: ExplorerOptions): ExplorerHandle {
  const doc = container.ownerDocument ?? document;
  injectStyles(doc);
  container.classList.add("lattice-explorer");
  clear(container);

  const restoreBase = (): string => {
    try {
      return localStorage.getItem("lattice-explorer:base") || options.base;
    } catch {
      return options.base;
    }
  };
  let currentBase = restoreBase();
  const makeSession = (base: string): ExplorerSession =>
    new ExplorerSession({
      base,
      ...(options.fetch ? { fetch: options.fetch } : {}),
      ...(options.claims ? { claims: options.claims } : {}),
      ...(options.vcAuth ? { vcAuth: options.vcAuth } : {}),
      ...(options.authToken ? { authToken: options.authToken } : {}),
      ...(options.slice ? { slice: options.slice } : {}),
    });
  let session = makeSession(currentBase);

  let model: SchemaModel = parseSchema("");

  interface SchemaEntry {
    hash: string;
    idl: string;
    firstSeen: number;
    lastSeen: number;
  }
  const SCHEMA_CAP = 24;
  const historyKey = (base: string): string => `lattice-explorer:schemas:${base}`;
  const readSchemaHistory = (base: string): SchemaEntry[] => {
    try {
      const raw = localStorage.getItem(historyKey(base));
      const parsed: unknown = raw ? JSON.parse(raw) : [];
      if (!Array.isArray(parsed)) return [];
      return parsed.filter(
        (e): e is SchemaEntry =>
          !!e && typeof e === "object" && typeof (e as { hash?: unknown }).hash === "string" && typeof (e as { idl?: unknown }).idl === "string",
      );
    } catch {
      return [];
    }
  };
  const recordSchema = (base: string, hash: string, idl: string): SchemaEntry[] => {
    const at = Date.now();
    const prior = readSchemaHistory(base);
    const seen = prior.some((e) => e.hash === hash);
    const merged = seen
      ? prior.map((e) => (e.hash === hash ? { ...e, idl, lastSeen: at } : e))
      : [{ hash, idl, firstSeen: at, lastSeen: at }, ...prior];
    merged.sort((a, b) => b.lastSeen - a.lastSeen);
    const capped = merged.slice(0, SCHEMA_CAP);
    try {
      localStorage.setItem(historyKey(base), JSON.stringify(capped));
    } catch {
      /* storage unavailable — session-only history */
    }
    return capped;
  };
  let liveSchema: LoadedSchema | undefined;
  let liveHash = "";

  // -- header ---------------------------------------------------------------
  const schemaSelect = document.createElement("select");
  schemaSelect.className = "lx-schema-select";
  schemaSelect.title = "Schema versions this explorer has seen from this origin";
  const pastBadge = h("span", { class: "lx-past-badge", title: "Viewing a past schema version — queries still run against the live origin." }, "past");
  pastBadge.style.display = "none";
  const schemaPicker = h("div", { class: "lx-schema-picker" }, h("span", { class: "lx-cap" }, "schema"), schemaSelect, pastBadge);
  const renderSchemaOptions = (selected: string): void => {
    const list = readSchemaHistory(currentBase);
    while (schemaSelect.firstChild) schemaSelect.removeChild(schemaSelect.firstChild);
    if (list.length === 0) {
      const opt = document.createElement("option");
      opt.value = liveHash;
      opt.textContent = liveHash ? liveHash.slice(0, 12) : "unreachable";
      schemaSelect.appendChild(opt);
      schemaSelect.disabled = true;
      return;
    }
    schemaSelect.disabled = false;
    for (const e of list) {
      const opt = document.createElement("option");
      opt.value = e.hash;
      opt.textContent = `${e.hash.slice(0, 12)}${e.hash === liveHash ? " · current" : ""}`;
      if (e.hash === selected) opt.selected = true;
      schemaSelect.appendChild(opt);
    }
  };
  const selectSchema = (hash: string): void => {
    const e = readSchemaHistory(currentBase).find((x) => x.hash === hash);
    if (!e) return;
    const isCurrent = hash === liveHash;
    model = isCurrent && liveSchema ? liveSchema.model : parseSchema(e.idl);
    renderDocs(querySidebar, model, (t) => queryEditor.insertAtCaret(t), selectMutationInUI);
    refreshMutations();
    idlEditor.setValue(e.idl);
    refreshIdl();
    refreshLint();
    pastBadge.style.display = isCurrent ? "none" : "";
  };
  schemaSelect.addEventListener("change", () => selectSchema(schemaSelect.value));
  renderSchemaOptions("");
  const admissionChip = h("span", { class: "lx-chip" }, "");
  const queryTab = h("div", { class: "lx-tab lx-active" }, "Query");
  const schemaTab = h("div", { class: "lx-tab" }, "Schema");
  const reloadBtn = button("Reload", { variant: "outline", icon: "rotate-cw" });
  const baseInput = document.createElement("input");
  baseInput.className = "lx-base";
  baseInput.value = currentBase;
  baseInput.spellcheck = false;
  baseInput.title = "Origin base URL — press Enter to connect";
  const connectBtn = button("connect", { className: "lx-connect", icon: "plug", title: "Connect to this origin" });
  const header = h(
    "div",
    { class: "lx-header" },
    h("div", { class: "lx-title" }, h("span", { class: "lx-logo" }, "Lattice"), baseInput, connectBtn),
    h("div", { class: "lx-tabs" }, queryTab, schemaTab),
    h("div", { class: "lx-spacer" }),
    admissionChip,
    schemaPicker,
    reloadBtn,
  );

  // -- query view -----------------------------------------------------------
  const querySidebar = h("div", { class: "lx-sidebar" });
  const runBtn = button("Run", { variant: "primary", icon: "play" });
  const methodPill = h("span", { class: "lx-method" });
  const modeText = h("span", { class: "lx-mode-text" });
  const modeExplain = h("div", { class: "lx-mode-explain" }, methodPill, modeText);
  const MODE_WIRE: Record<RunMode, string> = {
    introduce: "POST /q?intent=introduce",
    inline: "GET /q?d=…",
    oneshot: "POST /q?intent=oneshot",
    hash: "GET /q/{hash}",
  };
  const setModeHint = (v: string): void => {
    const m = asMode(v);
    methodPill.textContent = MODE_WIRE[m];
    modeText.textContent = MODE_HINTS[m];
  };
  const MODE_HINTS: Record<RunMode, string> = {
    introduce: "POST the query. The origin compiles it, streams the result, and grants a cacheable hash URL you can replay.",
    inline: "Self-contained compressed GET (?d=…). One request, no hash to learn — good for cold clients.",
    oneshot: "POST that the origin runs but never memoizes. No plan, no cache, no hash.",
    hash: "Replay the learned hash as a cacheable GET — the steady-state request a CDN serves. Runs Introduce first if the hash isn't known yet.",
  };
  const modeSeg = segmented(
    [
      { value: "introduce", label: "Introduce", title: MODE_HINTS.introduce },
      { value: "inline", label: "Inline", title: MODE_HINTS.inline },
      { value: "hash", label: "Replay", title: MODE_HINTS.hash },
    ],
    options.defaultMode ?? "introduce",
    (v) => setModeHint(v),
  );
  setModeHint(modeSeg.get());

  // Slice toggles — which authorization views to fetch, concurrently.
  const sliceCap: Record<SliceName, string> = {
    pub: "Public data — cacheable by shared caches. Always available.",
    ctx: options.claims ? "Claims-scoped view; sends the vc payload." : "Needs visibility claims (vc). Set `claims` to enable.",
    priv: options.authToken ? "Private view; sends the Authorization token." : "Needs an Authorization token. Set `authToken` to enable.",
  };
  const enabledSlices = new Set<SliceName>(["pub"]);
  if (options.claims) enabledSlices.add("ctx");
  if (options.authToken) enabledSlices.add("priv");
  const sliceChips = h("div", { class: "lx-slice-chips" });
  const sliceChipEls: Record<string, HTMLElement> = {};
  const refreshSliceChips = (): void => {
    for (const s of ["pub", "ctx", "priv"] as SliceName[]) sliceChipEls[s]?.classList.toggle("lx-on", enabledSlices.has(s));
  };
  for (const s of ["pub", "ctx", "priv"] as SliceName[]) {
    const locked = (s === "ctx" && !options.claims) || (s === "priv" && !options.authToken);
    const chip = h("button", { class: `lx-slice-chip lx-slice-${s}${locked ? " lx-locked" : ""}`, title: sliceCap[s] }, s);
    if (locked) {
      chip.setAttribute("aria-disabled", "true");
    } else {
      chip.addEventListener("click", () => {
        if (enabledSlices.has(s)) {
          if (enabledSlices.size > 1) enabledSlices.delete(s);
        } else {
          enabledSlices.add(s);
        }
        refreshSliceChips();
      });
    }
    sliceChipEls[s] = chip;
    sliceChips.appendChild(chip);
  }
  refreshSliceChips();
  const varsCount = h("span", { class: "lx-vars-count" });
  const varsToggle = h("button", { class: "lx-vars-toggle" }, icon("braces"), "Variables", varsCount);

  const queryEditor: Editor = createEditor({
    language: "query",
    highlight: highlightQuery,
    value: options.defaultQuery ?? DEFAULT_QUERY,
    onRun: () => void run(),
    onChange: () => refreshLint(),
    complete: (text, offset) => completeQuery(text, offset, model),
  });
  const varsArea = document.createElement("textarea");
  varsArea.spellcheck = false;
  varsArea.placeholder = '{ "episode": "Empire" }';
  const varsValidity = h("span", { class: "lx-vars-valid" });
  const varsBox = h("div", { class: "lx-vars" }, h("div", { class: "lx-vars-head" }, h("span", { class: "lx-cap" }, "variables · json"), varsValidity), varsArea);
  const refreshVars = (): void => {
    const raw = varsArea.value.trim();
    if (raw === "") {
      varsCount.textContent = "";
      varsCount.className = "lx-vars-count";
      varsValidity.textContent = "";
      varsValidity.className = "lx-vars-valid";
      varsToggle.classList.remove("lx-has");
      return;
    }
    try {
      const parsed: unknown = JSON.parse(raw);
      if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) throw new Error("not an object");
      const n = Object.keys(parsed as Record<string, unknown>).length;
      varsCount.textContent = String(n);
      varsCount.className = "lx-vars-count lx-ok";
      varsValidity.textContent = `valid · ${n} var${n === 1 ? "" : "s"}`;
      varsValidity.className = "lx-vars-valid lx-ok";
      varsToggle.classList.add("lx-has");
    } catch {
      varsCount.textContent = "!";
      varsCount.className = "lx-vars-count lx-bad";
      varsValidity.textContent = "invalid JSON";
      varsValidity.className = "lx-vars-valid lx-bad";
      varsToggle.classList.add("lx-has");
    }
  };
  varsArea.addEventListener("input", refreshVars);
  refreshVars();
  const queryDiags = h("div", { class: "lx-diags" });

  // -- mutation runner (POST /m/{name}) -------------------------------------
  const kindSeg = segmented(
    [
      { value: "query", label: "Query", title: "Read data through the query language." },
      { value: "mutation", label: "Mutation", title: "Invoke a named mutation (POST /m/{name}) with a JSON input." },
    ],
    "query",
    (v) => setKind(v === "mutation" ? "mutation" : "query"),
  );
  const mutSelect = document.createElement("select");
  mutSelect.className = "lx-mut-select";
  mutSelect.title = "Mutation to invoke";
  const mutKey = document.createElement("input");
  mutKey.className = "lx-base lx-mut-key";
  mutKey.placeholder = "idempotency-key (optional)";
  mutKey.spellcheck = false;
  const mutKeyWrap = h("label", { class: "lx-field" }, h("span", { class: "lx-cap" }, "key"), mutKey);
  const mutCtl = h("div", { class: "lx-ctl lx-hidden" }, h("span", { class: "lx-cap" }, "mutation"), mutSelect, mutKeyWrap);
  const transportCtl = h("div", { class: "lx-ctl" }, h("span", { class: "lx-cap" }, "transport"), modeSeg.el);
  const slicesCtl = h("div", { class: "lx-ctl" }, h("span", { class: "lx-cap" }, "slices"), sliceChips);
  const mutExplain = h("div", { class: "lx-mode-explain lx-hidden" });
  const mutInput: Editor = createEditor({
    language: "query",
    highlight: highlightJson,
    value: "",
    onRun: () => void run(),
  });
  const queryPanel = h("div", { class: "lx-editor-panel" }, queryEditor.el, varsBox, queryDiags);
  const mutInputCap = h("div", { class: "lx-mut-inputcap" }, h("span", { class: "lx-cap" }, "input · json"));
  const mutationPanel = h("div", { class: "lx-editor-panel lx-hidden" }, mutInputCap, mutInput.el);

  const updateMutHint = (): void => {
    const m = model.mutations.get(mutSelect.value);
    clear(mutExplain);
    if (!m) {
      mutExplain.appendChild(h("span", { class: "lx-mode-text" }, "This schema declares no mutations."));
      return;
    }
    const sig = `${m.name}(${m.args.map((a) => `${a.name}: ${a.type}`).join(", ")})${m.returns ? ` → ${m.returns}` : ""}`;
    const note = [m.allow, m.writes, m.effect].filter(Boolean).join(" · ");
    mutExplain.appendChild(h("span", { class: "lx-method" }, `POST /m/${m.name}`));
    mutExplain.appendChild(h("span", { class: "lx-mode-text" }, note ? `${sig} · ${note}` : sig));
  };
  const fillSkeleton = (force: boolean): void => {
    const m = model.mutations.get(mutSelect.value);
    if (!m) return;
    if (!force && mutInput.getValue().trim() !== "") return;
    const obj: Record<string, null> = {};
    for (const a of m.args) obj[a.name] = null;
    mutInput.setValue(m.args.length ? JSON.stringify(obj, null, 2) + "\n" : "{}\n");
  };
  const refreshMutations = (): void => {
    const prev = mutSelect.value;
    while (mutSelect.firstChild) mutSelect.removeChild(mutSelect.firstChild);
    const names = [...model.mutations.keys()];
    if (names.length === 0) {
      const opt = document.createElement("option");
      opt.value = "";
      opt.textContent = "(no mutations)";
      mutSelect.appendChild(opt);
      mutSelect.disabled = true;
    } else {
      mutSelect.disabled = false;
      for (const n of names) {
        const opt = document.createElement("option");
        opt.value = n;
        opt.textContent = n;
        mutSelect.appendChild(opt);
      }
      if (names.includes(prev)) mutSelect.value = prev;
    }
    updateMutHint();
  };
  const setKind = (kind: "query" | "mutation"): void => {
    const mut = kind === "mutation";
    queryPanel.classList.toggle("lx-hidden", mut);
    mutationPanel.classList.toggle("lx-hidden", !mut);
    transportCtl.classList.toggle("lx-hidden", mut);
    slicesCtl.classList.toggle("lx-hidden", mut);
    mutCtl.classList.toggle("lx-hidden", !mut);
    varsToggle.classList.toggle("lx-hidden", mut);
    modeExplain.classList.toggle("lx-hidden", mut);
    mutExplain.classList.toggle("lx-hidden", !mut);
    clear(runBtn);
    runBtn.appendChild(icon("play"));
    runBtn.appendChild(document.createTextNode(mut ? "Run mutation" : "Run"));
  };
  const selectMutationInUI = (name: string): void => {
    kindSeg.set("mutation");
    setKind("mutation");
    if ([...model.mutations.keys()].includes(name)) mutSelect.value = name;
    updateMutHint();
    fillSkeleton(true);
    mutInput.focus();
  };
  mutSelect.addEventListener("change", () => {
    updateMutHint();
    fillSkeleton(false);
  });
  const renderMutation = (result: MutationResult): void => {
    clear(rData);
    clear(rRecords);
    clear(rResponse);
    const recWrap = h("div", { class: "lx-mut-records" });
    if (result.records.length === 0) recWrap.appendChild(h("div", { class: "lx-empty" }, result.raw || "(no records)"));
    else for (const rec of result.records) recWrap.appendChild(recordRow(rec));
    rRecords.appendChild(recWrap);
    if (result.problem) {
      renderData(rData, result.problem);
    } else {
      const ents: Record<string, unknown> = {};
      for (const rec of result.records) if (rec.kind === "entity") ents[String(rec.id)] = rec.fields;
      if (Object.keys(ents).length) renderData(rData, ents);
      else rData.appendChild(h("div", { class: "lx-empty" }, result.committed ? "Committed. No entities returned." : "No entities returned."));
    }
    const table = h("table", { class: "lx-kv" });
    const row = (k: string, v: string): void => {
      table.appendChild(h("tr", {}, h("td", {}, k), h("td", {}, v)));
    };
    row("status", String(result.status));
    row("request", `${result.request.method} ${result.request.url}`);
    row("committed", String(result.committed));
    if (result.invalidatedKeys.length) row("invalidated", result.invalidatedKeys.join(" "));
    if (result.errors.length) row("errors", result.errors.map((e) => e.message ?? e.code ?? e.error?.$tag ?? "error").join("; "));
    for (const [k, v] of Object.entries(result.headers.all)) row(k, v);
    rResponse.appendChild(table);
  };
  const runMut = async (): Promise<void> => {
    const name = mutSelect.value;
    if (!name) {
      statusLine.innerHTML = '<span class="lx-bad">this schema declares no mutations</span>';
      return;
    }
    let input: unknown = {};
    const raw = mutInput.getValue().trim();
    if (raw !== "") {
      try {
        input = JSON.parse(raw);
      } catch {
        statusLine.innerHTML = '<span class="lx-bad">input is not valid JSON</span>';
        return;
      }
    }
    const key = mutKey.value.trim() || undefined;
    showResultTab("Data");
    statusLine.innerHTML = '<span class="lx-run-dot"></span>running';
    runBtn.setAttribute("disabled", "");
    try {
      const result = await session.runMutation(name, input, key);
      renderMutation(result);
      const cls = !result.ok ? "lx-bad" : result.committed ? "lx-ok" : "";
      const bits = [
        String(result.status),
        result.committed ? "committed" : "not committed",
        result.invalidatedKeys.length ? `${result.invalidatedKeys.length} invalidated` : undefined,
        result.errors.length ? `${result.errors.length} error${result.errors.length === 1 ? "" : "s"}` : undefined,
      ]
        .filter(Boolean)
        .join(" · ");
      statusLine.innerHTML = `<span class="${cls}">${escapeHtml(bits)}</span>`;
    } catch (e) {
      statusLine.innerHTML = `<span class="lx-bad">${escapeHtml(errMessage(e))}</span>`;
    } finally {
      runBtn.removeAttribute("disabled");
    }
  };
  refreshMutations();

  const board = makeBoard();
  const rData = board.dataWrap;
  const rRecords = board.recWrap;
  const rResponse = board.respWrap;
  const rExplain = h("div", { class: "lx-result-body lx-hidden" });
  const rTrace = h("div", { class: "lx-result-body lx-hidden" });
  rData.appendChild(teach("Run a query to see the entity stream.", "Press <kbd>⌘/Ctrl</kbd>+<kbd>Enter</kbd> or hit ▶ Run. Enabled slices are fetched concurrently and denormalized into the tree a real client would build."));
  rRecords.appendChild(teach("The raw NDJSON records land here.", "Records stream in live, per slice — a manifest, then entity / tombstone / error records, then an end marker."));
  rExplain.appendChild(teach("The compiled plan shows up here.", "Introduce (or Replay) fetches the shared plan: path-join slices, loader rounds, surrogate keys, and budget use."));
  rTrace.appendChild(teach("Response traces appear here.", "A wall-clock waterfall of the concurrent slice fetches (TTFB, streaming body, first-record tick), then the plan's batched loader rounds — the request made legible."));
  rResponse.appendChild(teach("Status, timing, cache, and headers appear here.", "Every response carries its plan, snapshot token, surrogate keys, and cache directives — the network made inspectable."));
  const resultBodies: Record<string, HTMLElement> = { Data: rData, Records: rRecords, Trace: rTrace, Explain: rExplain, Response: rResponse };
  const statusLine = h("span", { class: "lx-status" }, "ready");
  const rtabs = h("div", { class: "lx-result-tabs" });
  const resultTabEls: Record<string, HTMLElement> = {};
  const showResultTab = (name: string): void => {
    for (const [k, el] of Object.entries(resultBodies)) el.classList.toggle("lx-hidden", k !== name);
    for (const [k, el] of Object.entries(resultTabEls)) el.classList.toggle("lx-active", k === name);
  };
  for (const name of ["Data", "Records", "Trace", "Explain", "Response"]) {
    const tab = h("div", { class: "lx-rtab" + (name === "Data" ? " lx-active" : "") }, name);
    tab.addEventListener("click", () => showResultTab(name));
    resultTabEls[name] = tab;
    rtabs.appendChild(tab);
  }
  rtabs.appendChild(statusLine);
  const results = h("div", { class: "lx-results" }, rtabs, rData, rRecords, rTrace, rExplain, rResponse);

  varsToggle.addEventListener("click", () => varsBox.classList.toggle("lx-open"));
  const queryEditorWrap = h("div", { style: "position:relative;min-height:0;display:flex;flex-direction:column" }, queryPanel, mutationPanel);
  const queryRowGutter = h("div", { class: "lx-gutter lx-gutter-h" });
  const querySplit = h("div", { class: "lx-split" }, queryEditorWrap, queryRowGutter, results);
  const queryMain = h(
    "div",
    { class: "lx-main" },
    h(
      "div",
      { class: "lx-toolbar" },
      kindSeg.el,
      runBtn,
      h("span", { class: "lx-tb-div" }),
      transportCtl,
      slicesCtl,
      mutCtl,
      h("span", { class: "lx-spacer" }),
      varsToggle,
    ),
    modeExplain,
    mutExplain,
    querySplit,
  );
  const queryColGutter = h("div", { class: "lx-gutter lx-gutter-v" });
  const queryView = h("div", { class: "lx-view lx-active" }, querySidebar, queryColGutter, queryMain);

  // -- schema view ----------------------------------------------------------
  const schemaSidebar = h("div", { class: "lx-sidebar" });
  const checkBtn = button("Check compatibility", { variant: "primary", icon: "shield-check" });
  const checkModeSel = selectEl([
    { value: "client-backward", label: "client-backward" },
    { value: "server-backward", label: "server-backward" },
    { value: "full", label: "full" },
  ]);
  const reloadIdlBtn = button("Load current", { variant: "outline", icon: "download" });
  const idlEditor: Editor = createEditor({ language: "idl", highlight: highlightIdl, value: "", onChange: () => refreshIdl() });
  const idlDiags = h("div", { class: "lx-diags" });
  const checkBody = h("div", { class: "lx-result-body" }, h("div", { class: "lx-empty" }, "Edit the IDL, then check it against the origin’s deployed schema."));
  const idlEditorWrap = h("div", { style: "display:flex;flex-direction:column;min-height:0" }, idlEditor.el, idlDiags);
  const schemaRowGutter = h("div", { class: "lx-gutter lx-gutter-h" });
  const schemaSplit = h(
    "div",
    { class: "lx-split" },
    idlEditorWrap,
    schemaRowGutter,
    h("div", { class: "lx-results" }, h("div", { class: "lx-result-tabs" }, h("div", { class: "lx-rtab lx-active" }, "Compatibility report")), checkBody),
  );
  const schemaMain = h(
    "div",
    { class: "lx-main" },
    h("div", { class: "lx-toolbar" }, checkBtn, h("div", { class: "lx-ctl" }, h("span", { class: "lx-cap" }, "direction"), checkModeSel), reloadIdlBtn),
    schemaSplit,
  );
  const schemaColGutter = h("div", { class: "lx-gutter lx-gutter-v" });
  const schemaView = h("div", { class: "lx-view" }, schemaSidebar, schemaColGutter, schemaMain);

  container.appendChild(header);
  container.appendChild(queryView);
  container.appendChild(schemaView);

  wireResize(queryColGutter, queryView, querySidebar, "x", "--lx-sidebar-w", 160, 240);
  wireResize(queryRowGutter, querySplit, queryEditorWrap, "y", "--lx-split-top", 80, 140);
  wireResize(schemaColGutter, schemaView, schemaSidebar, "x", "--lx-sidebar-w", 160, 240);
  wireResize(schemaRowGutter, schemaSplit, idlEditorWrap, "y", "--lx-split-top", 80, 140);

  // -- behavior -------------------------------------------------------------
  const showView = (which: "query" | "schema"): void => {
    queryView.classList.toggle("lx-active", which === "query");
    schemaView.classList.toggle("lx-active", which === "schema");
    queryTab.classList.toggle("lx-active", which === "query");
    schemaTab.classList.toggle("lx-active", which === "schema");
  };
  queryTab.addEventListener("click", () => showView("query"));
  schemaTab.addEventListener("click", () => showView("schema"));

  const connect = async (next: string): Promise<void> => {
    const clean = next.trim().replace(/\/+$/, "");
    if (!clean || clean === currentBase) {
      baseInput.value = currentBase;
      return;
    }
    currentBase = clean;
    baseInput.value = clean;
    try {
      localStorage.setItem("lattice-explorer:base", clean);
    } catch {
      /* storage unavailable — session-only base */
    }
    session = makeSession(clean);
    await reload();
  };
  baseInput.addEventListener("keydown", (e) => {
    if (e.key === "Enter") {
      e.preventDefault();
      void connect(baseInput.value);
    }
  });
  connectBtn.addEventListener("click", () => void connect(baseInput.value));

  const renderDiags = (host: HTMLElement, diags: readonly Diagnostic[], onGo?: (offset: number) => void): void => {
    clear(host);
    for (const d of diags) {
      const row = h("div", { class: `lx-diag lx-diag-${d.severity}` }, `● ${d.message}${d.line ? ` (line ${d.line})` : ""}`);
      if (onGo && d.offset !== undefined) {
        const off = d.offset;
        row.addEventListener("click", () => onGo(off));
      }
      host.appendChild(row);
    }
  };

  const refreshLint = (): void => {
    renderDiags(queryDiags, lintQuery(queryEditor.getValue(), model), (o) => queryEditor.revealOffset(o));
  };
  const refreshIdl = (): void => {
    const m = parseSchema(idlEditor.getValue());
    renderDocs(
      schemaSidebar,
      m,
      (t) => {
        showView("query");
        queryEditor.insertAtCaret(t);
      },
      (name) => {
        showView("query");
        selectMutationInUI(name);
      },
    );
    const ds: Diagnostic[] = m.diagnostics.map((d) => ({ message: d.message, severity: "warning", offset: d.offset }));
    renderDiags(idlDiags, ds, (o) => idlEditor.revealOffset(o));
  };

  const run = async (): Promise<void> => {
    if (kindSeg.get() === "mutation") {
      await runMut();
      return;
    }
    const text = queryEditor.getValue();
    let vars: Record<string, unknown> = {};
    const raw = varsArea.value.trim();
    if (raw !== "") {
      try {
        const parsed: unknown = JSON.parse(raw);
        if (parsed && typeof parsed === "object" && !Array.isArray(parsed)) vars = { ...parsed };
      } catch {
        statusLine.innerHTML = '<span class="lx-bad">variables are not valid JSON</span>';
        return;
      }
    }
    const order: SliceName[] = ["pub", "ctx", "priv"];
    const slices = order.filter((s) => enabledSlices.has(s));
    if (slices.length === 0) slices.push("pub");
    const mode = asMode(modeSeg.get());
    board.begin(slices);
    showResultTab("Data");
    statusLine.innerHTML = '<span class="lx-run-dot"></span>running';
    runBtn.setAttribute("disabled", "");
    try {
      const bundle = await session.runSlices(text, vars, slices, { mode }, {
        onHead: (ev) => board.onHead(ev),
        onRecord: (ev) => board.onRecord(ev),
      });
      board.finalize(bundle);
      renderExplain(rExplain, bundle.explain);
      renderTrace(rTrace, bundle);
      const allOk = slices.every((s) => bundle.slices[s]?.ok);
      const anyNet = slices.some((s) => bundle.slices[s]?.status === 0);
      const totalMs = Math.max(0, ...slices.map((s) => bundle.slices[s]?.durationMs ?? 0));
      if (anyNet && !allOk) {
        statusLine.innerHTML = `<span class="lx-bad">couldn't reach ${escapeHtml(currentBase)}</span> — is the origin running?`;
      } else {
        const okCls = allOk ? "lx-ok" : "lx-bad";
        const per = slices.map((s) => `${s} ${bundle.slices[s]?.status ?? "—"}`).join(" · ");
        const tail = [bundle.hash ? bundle.hash.slice(0, 10) : undefined, bundle.autoIntroduced ? "auto-introduced" : undefined].filter(Boolean).join(" · ");
        statusLine.innerHTML = `<span class="${okCls}">${per}</span> · ${totalMs.toFixed(0)}ms${tail ? ` · ${tail}` : ""}`;
      }
    } catch (e) {
      statusLine.innerHTML = `<span class="lx-bad">${escapeHtml(errMessage(e))}</span>`;
    } finally {
      runBtn.removeAttribute("disabled");
    }
  };
  runBtn.addEventListener("click", () => void run());

  const runCheck = async (): Promise<void> => {
    checkBtn.setAttribute("disabled", "");
    try {
      const report = await session.checkIdl(idlEditor.getValue(), checkModeSel.value);
      renderCheck(checkBody, report);
    } catch (e) {
      clear(checkBody);
      checkBody.appendChild(h("div", { class: "lx-report-fail" }, errMessage(e)));
    } finally {
      checkBtn.removeAttribute("disabled");
    }
  };
  checkBtn.addEventListener("click", () => void runCheck());

  const reload = async (): Promise<void> => {
    schemaSelect.disabled = true;
    try {
      const loaded = await session.loadSchema();
      liveSchema = loaded;
      liveHash = loaded.hash;
      model = loaded.model;
      recordSchema(currentBase, loaded.hash, loaded.idl);
      renderSchemaOptions(liveHash);
      pastBadge.style.display = "none";
      admissionChip.textContent = `admission: ${loaded.discovery.admission ?? "?"}`;
      renderDocs(querySidebar, model, (t) => queryEditor.insertAtCaret(t), selectMutationInUI);
      refreshMutations();
      idlEditor.setValue(loaded.idl);
      refreshIdl();
      refreshLint();
      greet(loaded, currentBase);
    } catch (e) {
      liveHash = "";
      renderSchemaOptions("");
      renderDocs(querySidebar, model, (t) => queryEditor.insertAtCaret(t), selectMutationInUI);
      refreshMutations();
      querySidebar.appendChild(
        teach(
          `Couldn't reach ${currentBase}.`,
          `Is a Lattice origin running there? Try <kbd>cabal run example-lattice</kbd>, or set a different base. <br><br>${escapeHtml(errMessage(e))}`,
        ),
      );
    }
  };
  reloadBtn.addEventListener("click", () => void reload());
  reloadIdlBtn.addEventListener("click", () => void reload());

  void reload();
  refreshLint();
  const focusVisibleSearch = (): void => {
    const activeView = container.querySelector<HTMLElement>(".lx-view.lx-active");
    const input = activeView?.querySelector<HTMLInputElement>(".lx-search");
    if (!input) return;
    input.focus();
    input.select();
  };
  const onGlobalKeydown = (e: KeyboardEvent): void => {
    if (e.key !== "/" || e.metaKey || e.ctrlKey || e.altKey) return;
    const target = e.target as HTMLElement | null;
    const tag = target?.tagName;
    if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || target?.isContentEditable) return;
    e.preventDefault();
    focusVisibleSearch();
  };
  doc.addEventListener("keydown", onGlobalKeydown);

  return {
    el: container,
    get session() {
      return session;
    },
    reload,
    destroy: () => {
      doc.removeEventListener("keydown", onGlobalKeydown);
      clear(container);
      container.classList.remove("lattice-explorer");
    },
  };
}

const MODES: Record<string, RunMode> = { introduce: "introduce", oneshot: "oneshot", inline: "inline", hash: "hash" };

function asMode(value: string): RunMode {
  return MODES[value] ?? "introduce";
}

/**
 * A one-time console note for the curious developer who opens devtools — the
 * quiet easter egg. Prints the loaded schema's identity and the spec link,
 * styled, once per page.
 */
let greeted = false;
function greet(loaded: LoadedSchema, base: string): void {
  if (greeted || typeof console === "undefined") return;
  greeted = true;
  const m = loaded.model;
  console.log(
    `%cLattice Explorer%c  ${m.name || "schema"} · ${loaded.hash.slice(0, 12)} · ` +
      `${m.roots.size} roots, ${m.entities.size} entities, ${m.mutations.size} mutations` +
      `%c\nThe network, made inspectable. Spec: https://iand675.github.io/wireform-/lattice/ · origin ${base}`,
    "font-weight:700;color:#6ea8fe",
    "color:inherit",
    "color:#8b97a8",
  );
}
