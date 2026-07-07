/**
 * The explorer's self-contained stylesheet, injected once into the document
 * head. Everything is scoped under `.lattice-explorer` so it never leaks into
 * a host page, and colors are driven by CSS variables with a light/dark
 * default keyed off `prefers-color-scheme`.
 */

export const EXPLORER_CSS = `
.lattice-explorer {
  --background: 222.2 84% 4.9%;
  --foreground: 210 40% 98%;
  --card: 222.2 47% 9%;
  --card-foreground: 210 40% 98%;
  --popover: 222.2 47% 9%;
  --popover-foreground: 210 40% 98%;
  --primary: 217.2 91.2% 59.8%;
  --primary-foreground: 222.2 47.4% 11.2%;
  --secondary: 217.2 32.6% 17.5%;
  --secondary-foreground: 210 40% 98%;
  --muted: 217.2 32.6% 17.5%;
  --muted-foreground: 215 20.2% 65.1%;
  --accent: 217.2 32.6% 17.5%;
  --accent-foreground: 210 40% 98%;
  --destructive: 0 62.8% 45%;
  --destructive-foreground: 210 40% 98%;
  --border: 217.2 32.6% 22%;
  --input: 217.2 32.6% 22%;
  --ring: 217.2 91.2% 59.8%;
  --radius: 0.5rem;
  /* Chrome + semantic aliases (also read from JS for the trace colors). */
  --lx-bg: hsl(var(--background));
  --lx-panel: hsl(var(--card));
  --lx-panel-2: hsl(var(--muted));
  --lx-border: hsl(var(--border));
  --lx-fg: hsl(var(--foreground));
  --lx-fg-dim: hsl(var(--muted-foreground));
  --lx-accent: hsl(var(--primary));
  --lx-accent-2: #7ee787;
  --lx-danger: #ff7b72;
  --lx-warn: #e3b341;
  --lx-kw: #ff9d76;
  --lx-type: #7ee0d0;
  --lx-field: #cdd6e3;
  --lx-str: #a5d6ff;
  --lx-num: #d2a8ff;
  --lx-var: #ffa657;
  --lx-punct: #6f7c90;
  --lx-comment: #6a7486;
  --lx-mono: ui-monospace, "SF Mono", "JetBrains Mono", "Fira Code", Menlo, Consolas, monospace;
  --lx-sans: ui-sans-serif, system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
  color: var(--lx-fg);
  background: var(--lx-bg);
  font-family: var(--lx-sans);
  font-size: 13px;
  display: flex;
  flex-direction: column;
  height: 100%;
  min-height: 0;
  box-sizing: border-box;
}
.lattice-explorer *, .lattice-explorer *::before, .lattice-explorer *::after { box-sizing: border-box; }
@media (prefers-color-scheme: light) {
  .lattice-explorer {
    --background: 0 0% 100%;
    --foreground: 222.2 84% 4.9%;
    --card: 0 0% 100%;
    --card-foreground: 222.2 84% 4.9%;
    --popover: 0 0% 100%;
    --popover-foreground: 222.2 84% 4.9%;
    --primary: 221.2 83.2% 53.3%;
    --primary-foreground: 210 40% 98%;
    --secondary: 210 40% 96.1%;
    --secondary-foreground: 222.2 47.4% 11.2%;
    --muted: 210 40% 96.1%;
    --muted-foreground: 215.4 16.3% 46.9%;
    --accent: 210 40% 96.1%;
    --accent-foreground: 222.2 47.4% 11.2%;
    --border: 214.3 31.8% 91.4%;
    --input: 214.3 31.8% 91.4%;
    --ring: 221.2 83.2% 53.3%;
    --lx-accent-2: #1a7f37; --lx-danger: #cf222e; --lx-warn: #9a6700;
    --lx-kw: #cf222e; --lx-type: #0550ae; --lx-field: #1f2430; --lx-str: #0a3069;
    --lx-num: #8250df; --lx-var: #953800; --lx-punct: #6e7781; --lx-comment: #6e7781;
  }
}

/* Header */
.lattice-explorer .lx-header {
  display: flex; align-items: center; gap: 10px; padding: 8px 12px;
  background: var(--lx-panel); border-bottom: 1px solid var(--lx-border); flex: 0 0 auto;
}
.lattice-explorer .lx-title { font-weight: 700; letter-spacing: .2px; }
.lattice-explorer .lx-title small { color: var(--lx-fg-dim); font-weight: 500; margin-left: 6px; }
.lattice-explorer .lx-tabs {
  display: inline-flex; align-items: center; gap: 2px; height: 34px; margin-left: 8px;
  padding: 3px; background: hsl(var(--muted)); border-radius: var(--radius);
}
.lattice-explorer .lx-tab {
  display: inline-flex; align-items: center; height: 100%; padding: 0 12px; border: 0; background: transparent;
  border-radius: calc(var(--radius) - 3px); cursor: pointer; user-select: none;
  color: hsl(var(--muted-foreground)); font-size: 12px; font-weight: 500;
}
.lattice-explorer .lx-tab:hover { color: hsl(var(--foreground)); }
.lattice-explorer .lx-tab.lx-active { color: hsl(var(--foreground)); background: hsl(var(--background)); box-shadow: 0 1px 2px 0 hsl(0 0% 0% / 0.18); }
.lattice-explorer .lx-spacer { flex: 1; }
.lattice-explorer .lx-chip {
  display: inline-flex; align-items: center; font-family: var(--lx-mono); font-size: 11px; font-weight: 500;
  color: hsl(var(--secondary-foreground)); background: hsl(var(--secondary)); border: 1px solid transparent;
  border-radius: calc(var(--radius) - 2px); padding: 2px 8px;
}
.lattice-explorer .lx-chip b { color: var(--lx-accent-2); font-weight: 600; }

/* Buttons + inputs */
.lattice-explorer button {
  display: inline-flex; align-items: center; justify-content: center; gap: 6px; white-space: nowrap;
  font-family: var(--lx-sans); font-size: 12px; font-weight: 500; line-height: 1;
  height: 32px; padding: 0 12px; border-radius: calc(var(--radius) - 2px);
  color: hsl(var(--secondary-foreground)); background: hsl(var(--secondary));
  border: 1px solid transparent; cursor: pointer; user-select: none;
}
.lattice-explorer button:hover { background: hsl(var(--secondary) / 0.8); }
.lattice-explorer button:focus-visible { outline: none; box-shadow: 0 0 0 2px hsl(var(--background)), 0 0 0 4px hsl(var(--ring)); }
.lattice-explorer button:disabled { opacity: .5; cursor: default; pointer-events: none; }
.lattice-explorer button.lx-primary { background: hsl(var(--primary)); color: hsl(var(--primary-foreground)); }
.lattice-explorer button.lx-primary:hover { background: hsl(var(--primary) / 0.9); }
.lattice-explorer button.lx-outline { background: transparent; border-color: hsl(var(--border)); }
.lattice-explorer button.lx-outline:hover { background: hsl(var(--accent)); color: hsl(var(--accent-foreground)); }
.lattice-explorer button.lx-ghost { background: transparent; }
.lattice-explorer button.lx-ghost:hover { background: hsl(var(--accent)); color: hsl(var(--accent-foreground)); }
.lattice-explorer .lx-ico { display: inline-flex; flex: 0 0 auto; }
.lattice-explorer .lx-ico svg { width: 14px; height: 14px; display: block; }
.lattice-explorer select {
  display: inline-flex; align-items: center; height: 32px; padding: 0 30px 0 11px;
  font-family: var(--lx-sans); font-size: 12px; color: hsl(var(--foreground));
  background-color: hsl(var(--background)); border: 1px solid hsl(var(--input)); border-radius: calc(var(--radius) - 2px);
  cursor: pointer; -webkit-appearance: none; appearance: none;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='%2364748b' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='m6 9 6 6 6-6'/%3E%3C/svg%3E");
  background-repeat: no-repeat; background-position: right 9px center; background-size: 15px;
}
.lattice-explorer select:hover { border-color: hsl(var(--ring) / 0.5); }
.lattice-explorer select:focus-visible { outline: none; box-shadow: 0 0 0 2px hsl(var(--background)), 0 0 0 4px hsl(var(--ring)); }
.lattice-explorer label.lx-field { display: inline-flex; gap: 5px; align-items: center; color: var(--lx-fg-dim); }

/* Layout */
.lattice-explorer .lx-view { display: none; min-height: 0; flex: 1; }
.lattice-explorer .lx-view.lx-active { display: grid; grid-template-columns: var(--lx-sidebar-w, 260px) 6px minmax(0, 1fr); }
.lattice-explorer .lx-sidebar {
  border-right: 1px solid var(--lx-border); background: var(--lx-panel); overflow: auto; min-height: 0;
}
.lattice-explorer .lx-main { display: flex; flex-direction: column; min-height: 0; min-width: 0; }
.lattice-explorer .lx-toolbar {
  display: flex; align-items: center; gap: 8px; padding: 7px 10px; border-bottom: 1px solid var(--lx-border);
  flex: 0 0 auto; flex-wrap: wrap;
}
.lattice-explorer .lx-split { display: grid; grid-template-rows: var(--lx-split-top, 1fr) 6px minmax(0, 1fr); min-height: 0; flex: 1; }
.lattice-explorer .lx-gutter { background: var(--lx-border); z-index: 5; }
.lattice-explorer .lx-gutter:hover, .lattice-explorer .lx-gutter.lx-dragging { background: var(--lx-accent); }
.lattice-explorer .lx-gutter-v { cursor: col-resize; }
.lattice-explorer .lx-gutter-h { cursor: row-resize; }

/* Editor */
.lattice-explorer .lx-editor { position: relative; flex: 1 1 auto; min-height: 120px; overflow: hidden; background: var(--lx-bg); }
.lattice-explorer .lx-hl, .lattice-explorer .lx-input {
  margin: 0; border: 0; padding: 10px 12px 10px calc(var(--lx-gutter-w, 0px) + 12px); font-family: var(--lx-mono); font-size: 13px; line-height: 1.55;
  white-space: pre; overflow-wrap: normal; tab-size: 2;
  position: absolute; inset: 0; overflow: auto;
}
.lattice-explorer .lx-gutter-lines {
  position: absolute; left: 0; top: 0; bottom: 0; width: var(--lx-gutter-w, 0px); z-index: 2;
  overflow: hidden; pointer-events: none; user-select: none;
  background: hsl(var(--muted) / 0.4); border-right: 1px solid var(--lx-border);
}
.lattice-explorer .lx-gutter-inner { padding: 10px 8px 0 0; text-align: right; font-family: var(--lx-mono); font-size: 13px; line-height: 1.55; color: var(--lx-fg-dim); will-change: transform; }
.lattice-explorer .lx-gutter-inner > div { white-space: pre; }
.lattice-explorer .lx-brace-layer { position: absolute; inset: 0; z-index: 0; overflow: hidden; pointer-events: none; }
.lattice-explorer .lx-brace-hl {
  position: absolute; box-sizing: border-box; border-radius: 2px;
  background: color-mix(in oklab, var(--lx-accent) 20%, transparent); box-shadow: inset 0 0 0 1px hsl(var(--ring) / 0.7);
}
.lattice-explorer .lx-editor-panel { position: relative; flex: 1 1 auto; display: flex; flex-direction: column; min-height: 0; }
.lattice-explorer .lx-mut-inputcap { padding: 6px 12px 2px; flex: 0 0 auto; }
.lattice-explorer .lx-mut-select { height: 30px; font-size: 12px; max-width: 240px; }
.lattice-explorer .lx-mut-key { height: 30px; min-width: 190px; font-family: var(--lx-mono); font-size: 11px; }
.lattice-explorer .lx-hl { pointer-events: none; z-index: 0; color: var(--lx-fg); }
.lattice-explorer .lx-hl code { font: inherit; }
.lattice-explorer .lx-input {
  z-index: 1; color: transparent; background: transparent; caret-color: var(--lx-fg);
  resize: none; outline: none;
}
.lattice-explorer .lx-input::selection { background: rgba(110,168,254,.3); }
.lattice-explorer .lx-tok-keyword { color: var(--lx-kw); }
.lattice-explorer .lx-tok-type { color: var(--lx-type); }
.lattice-explorer .lx-tok-field { color: var(--lx-field); }
.lattice-explorer .lx-tok-string { color: var(--lx-str); }
.lattice-explorer .lx-tok-number { color: var(--lx-num); }
.lattice-explorer .lx-tok-variable { color: var(--lx-var); }
.lattice-explorer .lx-tok-directive { color: var(--lx-var); font-style: italic; }
.lattice-explorer .lx-tok-comment { color: var(--lx-comment); font-style: italic; }
.lattice-explorer .lx-tok-punct { color: var(--lx-punct); }

/* Completion popup */
.lattice-explorer .lx-completions {
  position: absolute; z-index: 20; max-height: 240px; min-width: 220px; max-width: 380px; overflow: auto;
  background: hsl(var(--popover)); color: hsl(var(--popover-foreground)); border: 1px solid hsl(var(--border)); border-radius: var(--radius);
  box-shadow: 0 10px 30px -10px hsl(0 0% 0% / 0.55), 0 4px 8px -4px hsl(0 0% 0% / 0.35); padding: 4px;
}
.lattice-explorer .lx-comp-row { display: flex; align-items: center; gap: 8px; padding: 4px 8px; cursor: pointer; font-family: var(--lx-mono); font-size: 12px; }
.lattice-explorer .lx-comp-row:hover { background: var(--lx-panel-2); }
.lattice-explorer .lx-comp-sel { background: rgba(110,168,254,.22); }
.lattice-explorer .lx-comp-kind {
  width: 16px; height: 16px; border-radius: 4px; display: inline-flex; align-items: center; justify-content: center;
  font-size: 10px; font-weight: 700; color: #08111f; flex: 0 0 auto;
}
.lattice-explorer .lx-kind-root, .lattice-explorer .lx-kind-edge { background: var(--lx-accent); }
.lattice-explorer .lx-kind-field { background: var(--lx-type); }
.lattice-explorer .lx-kind-arg, .lattice-explorer .lx-kind-variable { background: var(--lx-var); }
.lattice-explorer .lx-kind-enum, .lattice-explorer .lx-kind-type { background: var(--lx-num); }
.lattice-explorer .lx-kind-keyword, .lattice-explorer .lx-kind-fragment { background: var(--lx-fg-dim); }
.lattice-explorer .lx-comp-label { color: var(--lx-fg); }
.lattice-explorer .lx-comp-detail { color: var(--lx-fg-dim); margin-left: auto; padding-left: 12px; }

/* Variables + diagnostics */
.lattice-explorer .lx-vars { border-top: 1px solid var(--lx-border); background: var(--lx-panel); display: none; }
.lattice-explorer .lx-vars.lx-open { display: block; }
.lattice-explorer .lx-vars textarea {
  width: 100%; min-height: 64px; border: 0; background: transparent; color: var(--lx-fg);
  font-family: var(--lx-mono); font-size: 12px; padding: 8px 12px; resize: vertical; outline: none;
}
.lattice-explorer .lx-diags { flex: 0 0 auto; max-height: 120px; overflow: auto; border-top: 1px solid var(--lx-border); background: var(--lx-panel); }
.lattice-explorer .lx-diag { display: flex; gap: 8px; padding: 4px 12px; font-family: var(--lx-mono); font-size: 12px; cursor: pointer; }
.lattice-explorer .lx-diag:hover { background: var(--lx-panel-2); }
.lattice-explorer .lx-diag-error { color: var(--lx-danger); }
.lattice-explorer .lx-diag-warning { color: var(--lx-warn); }

/* Results */
.lattice-explorer .lx-results { display: flex; flex-direction: column; min-height: 0; border-top: 1px solid var(--lx-border); }
.lattice-explorer .lx-result-tabs { display: flex; gap: 4px; padding: 6px 10px; border-bottom: 1px solid var(--lx-border); background: var(--lx-panel); align-items: center; }
.lattice-explorer .lx-rtab { padding: 4px 10px; border-radius: calc(var(--radius) - 2px); cursor: pointer; color: hsl(var(--muted-foreground)); font-size: 12px; font-weight: 500; }
.lattice-explorer .lx-rtab:hover { color: hsl(var(--foreground)); }
.lattice-explorer .lx-rtab.lx-active { color: hsl(var(--foreground)); background: hsl(var(--muted)); }
.lattice-explorer .lx-status { margin-left: auto; font-family: var(--lx-mono); font-size: 11px; color: var(--lx-fg-dim); }
.lattice-explorer .lx-status .lx-ok { color: var(--lx-accent-2); }
.lattice-explorer .lx-status .lx-bad { color: var(--lx-danger); }
.lattice-explorer .lx-result-body { overflow: auto; min-height: 0; flex: 1; padding: 10px 12px; }
.lattice-explorer pre.lx-json { margin: 0; font-family: var(--lx-mono); font-size: 12px; line-height: 1.5; white-space: pre-wrap; word-break: break-word; }
.lattice-explorer .lx-json-key { color: var(--lx-type); }
.lattice-explorer .lx-json-str { color: var(--lx-str); }
.lattice-explorer .lx-json-num { color: var(--lx-num); }
.lattice-explorer .lx-json-bool { color: var(--lx-kw); }
.lattice-explorer .lx-json-ref { color: var(--lx-accent); font-weight: 600; }

/* Records + explain + headers */
.lattice-explorer .lx-rec { display: flex; gap: 8px; align-items: baseline; padding: 3px 0; border-bottom: 1px solid var(--lx-border); font-family: var(--lx-mono); font-size: 12px; }
.lattice-explorer .lx-rec code { color: var(--lx-fg); white-space: pre-wrap; word-break: break-word; }
.lattice-explorer .lx-badge { flex: 0 0 auto; padding: 1px 7px; border-radius: calc(var(--radius) - 4px); font-size: 10px; font-weight: 700; color: #08111f; text-transform: uppercase; }
.lattice-explorer .lx-badge-manifest { background: var(--lx-accent); }
.lattice-explorer .lx-badge-entity { background: var(--lx-type); }
.lattice-explorer .lx-badge-end { background: var(--lx-fg-dim); }
.lattice-explorer .lx-badge-error { background: var(--lx-danger); }
.lattice-explorer .lx-badge-tombstone, .lattice-explorer .lx-badge-invalidated { background: var(--lx-warn); }
.lattice-explorer table.lx-kv { border-collapse: collapse; width: 100%; font-family: var(--lx-mono); font-size: 12px; }
.lattice-explorer table.lx-kv td { padding: 3px 8px 3px 0; vertical-align: top; border-bottom: 1px solid var(--lx-border); }
.lattice-explorer table.lx-kv td:first-child { color: var(--lx-fg-dim); white-space: nowrap; width: 1%; }
.lattice-explorer .lx-bar { display: inline-block; height: 8px; border-radius: 4px; background: var(--lx-accent); vertical-align: middle; }
.lattice-explorer .lx-bar-track { display: inline-block; width: 120px; height: 8px; border-radius: 4px; background: var(--lx-panel-2); margin-right: 8px; vertical-align: middle; overflow: hidden; }
.lattice-explorer .lx-explain-sec { margin-bottom: 14px; }
.lattice-explorer .lx-explain-sec h4 { margin: 0 0 6px; font-size: 12px; color: var(--lx-fg-dim); text-transform: uppercase; letter-spacing: .5px; }

/* Docs tree */
.lattice-explorer .lx-docs { padding: 2px 0 18px; }
.lattice-explorer .lx-search-bar {
  position: sticky; top: 0; z-index: 3;
  padding: 10px; background: var(--lx-panel);
  border-bottom: 1px solid var(--lx-border);
}
.lattice-explorer .lx-search-wrap {
  display: flex; align-items: center;
  background: hsl(var(--background)); border: 1px solid hsl(var(--input)); border-radius: calc(var(--radius) - 2px);
  transition: border-color 0.14s, box-shadow 0.14s;
}
.lattice-explorer .lx-search-wrap:focus-within {
  border-color: hsl(var(--ring)); box-shadow: 0 0 0 2px hsl(var(--background)), 0 0 0 4px hsl(var(--ring));
}
.lattice-explorer .lx-search-icon { flex: 0 0 auto; display: flex; padding-left: 9px; color: var(--lx-fg-dim); pointer-events: none; transition: color 0.12s; }
.lattice-explorer .lx-search-icon svg { width: 13px; height: 13px; display: block; }
.lattice-explorer .lx-search-wrap:focus-within .lx-search-icon { color: var(--lx-accent); }
.lattice-explorer .lx-search {
  flex: 1; min-width: 0; background: transparent; border: none; outline: none;
  padding: 7px 7px 7px 6px; color: var(--lx-fg); font-size: 12px; font-family: var(--lx-sans);
}
.lattice-explorer .lx-search::placeholder { color: var(--lx-fg-dim); }
.lattice-explorer .lx-search::-webkit-search-cancel-button,
.lattice-explorer .lx-search::-webkit-search-decoration { display: none; -webkit-appearance: none; }
.lattice-explorer .lx-search-count {
  flex: 0 0 auto; font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg-dim);
  padding: 1px 6px; margin-right: 3px; border-radius: 999px; background: var(--lx-panel); border: 1px solid var(--lx-border);
}
.lattice-explorer .lx-search-kbd {
  flex: 0 0 auto; font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg-dim);
  padding: 1px 5px; margin-right: 6px; border-radius: 4px; background: var(--lx-panel);
  border: 1px solid var(--lx-border); border-bottom-width: 2px;
}
.lattice-explorer .lx-search-clear {
  flex: 0 0 auto; display: flex; align-items: center; justify-content: center;
  width: 20px; height: 20px; margin-right: 5px; padding: 0; border: none; background: transparent;
  color: var(--lx-fg-dim); border-radius: 5px; cursor: pointer; transition: color 0.1s, background 0.1s;
}
.lattice-explorer .lx-search-clear:hover { color: var(--lx-fg); background: var(--lx-panel); }
.lattice-explorer .lx-search-clear svg { width: 10px; height: 10px; display: block; }
.lattice-explorer .lx-match { background: color-mix(in oklab, var(--lx-accent) 22%, transparent); color: var(--lx-fg); border-radius: 3px; padding: 0 1px; }

.lattice-explorer .lx-sec { margin-top: 2px; }
.lattice-explorer .lx-sec > summary, .lattice-explorer .lx-ent > summary { list-style: none; cursor: pointer; user-select: none; }
.lattice-explorer .lx-sec > summary::-webkit-details-marker, .lattice-explorer .lx-ent > summary::-webkit-details-marker { display: none; }
.lattice-explorer .lx-sec > summary {
  display: flex; align-items: center; gap: 7px; padding: 7px 12px 5px;
  font-family: var(--lx-sans); font-size: 10.5px; font-weight: 700; letter-spacing: 0.7px; text-transform: uppercase; color: var(--lx-fg-dim);
  transition: color 0.12s;
}
.lattice-explorer .lx-sec > summary:hover { color: var(--lx-fg); }
.lattice-explorer .lx-sec-count {
  margin-left: auto; font-weight: 600; font-size: 10px; letter-spacing: 0; color: var(--lx-fg-dim);
  background: var(--lx-panel-2); border: 1px solid var(--lx-border); border-radius: 999px; padding: 0 7px;
}
.lattice-explorer .lx-chev {
  flex: 0 0 auto; width: 0; height: 0; border-left: 4px solid currentColor;
  border-top: 3.5px solid transparent; border-bottom: 3.5px solid transparent; opacity: 0.55;
  transition: transform 0.16s cubic-bezier(0.22, 1, 0.36, 1);
}
.lattice-explorer details[open] > summary > .lx-chev { transform: rotate(90deg); }

.lattice-explorer .lx-row { display: flex; align-items: baseline; flex-wrap: wrap; gap: 6px; padding: 4px 12px 4px 15px; font-size: 12px; line-height: 1.4; }
.lattice-explorer .lx-row:hover { background: color-mix(in oklab, var(--lx-accent) 9%, transparent); }
.lattice-explorer .lx-row-name { font-family: var(--lx-sans); font-weight: 500; color: var(--lx-fg); cursor: pointer; }
.lattice-explorer .lx-row-name:hover { color: var(--lx-accent); text-decoration: underline; text-underline-offset: 2px; }
.lattice-explorer .lx-row-type { font-family: var(--lx-mono); font-size: 11px; color: var(--lx-fg-dim); }
.lattice-explorer .lx-row-arrow { font-family: var(--lx-mono); font-size: 11px; color: var(--lx-type); }
.lattice-explorer .lx-name { font-family: var(--lx-sans); font-weight: 500; color: var(--lx-fg); }
.lattice-explorer .lx-kind {
  flex: 0 0 auto; font-family: var(--lx-mono); font-size: 9px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.4px;
  padding: 1px 5px; border-radius: 4px; color: #07101d; align-self: center;
}
.lattice-explorer .lx-kind-get { background: var(--lx-accent); }
.lattice-explorer .lx-kind-list { background: var(--lx-type); }
.lattice-explorer .lx-kind-mut { background: var(--lx-kw); }
.lattice-explorer .lx-kind-enum { background: var(--lx-num); }
.lattice-explorer .lx-kind-newtype { background: var(--lx-var); }

.lattice-explorer .lx-ent > summary { display: flex; align-items: center; gap: 7px; padding: 5px 12px 5px 15px; transition: background 0.1s; }
.lattice-explorer .lx-ent > summary:hover { background: color-mix(in oklab, var(--lx-accent) 9%, transparent); }
.lattice-explorer .lx-ent-name { font-family: var(--lx-sans); font-weight: 600; font-size: 12.5px; color: var(--lx-type); }
.lattice-explorer .lx-key {
  margin-left: auto; font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg-dim);
  background: var(--lx-panel-2); border: 1px solid var(--lx-border); border-radius: 4px; padding: 0 5px;
}
.lattice-explorer .lx-ent-body { margin: 1px 0 5px 21px; border-left: 1px solid var(--lx-border); }
.lattice-explorer .lx-mem { padding-left: 11px; }
.lattice-explorer .lx-dot { flex: 0 0 auto; width: 11px; text-align: center; color: var(--lx-fg-dim); font-family: var(--lx-mono); font-size: 11px; }
.lattice-explorer .lx-dot-edge { color: var(--lx-accent); }
.lattice-explorer .lx-tag { margin-left: auto; font-family: var(--lx-mono); font-size: 9px; text-transform: uppercase; letter-spacing: 0.4px; color: var(--lx-fg-dim); border: 1px solid var(--lx-border); border-radius: 4px; padding: 0 5px; align-self: center; }
.lattice-explorer .lx-empty { padding: 28px 24px; color: var(--lx-fg-dim); text-align: center; line-height: 1.7; }
.lattice-explorer .lx-report { font-family: var(--lx-mono); font-size: 12px; }
.lattice-explorer .lx-report-pass { color: var(--lx-accent-2); font-weight: 700; }
.lattice-explorer .lx-report-fail { color: var(--lx-danger); font-weight: 700; }

/* Teaching empty states */
.lattice-explorer .lx-teach { padding: 32px 26px; color: var(--lx-fg-dim); text-align: center; max-width: 460px; margin: 8px auto; line-height: 1.75; }
.lattice-explorer .lx-teach .lx-teach-lead { color: var(--lx-fg); font-size: 14px; margin-bottom: 6px; }
.lattice-explorer kbd {
  font-family: var(--lx-mono); font-size: 11px; padding: 1px 6px; border-radius: calc(var(--radius) - 3px);
  background: var(--lx-panel-2); border: 1px solid var(--lx-border); border-bottom-width: 2px; color: var(--lx-fg);
}

/* Motion & polish (high-impact, transform/opacity only, ease-out-quint) */
@keyframes lx-rise { from { opacity: 0; transform: translateY(7px); } to { opacity: 1; transform: none; } }
@keyframes lx-fade { from { opacity: 0; } to { opacity: 1; } }
@keyframes lx-pop { from { opacity: 0; transform: scale(0.97); } to { opacity: 1; transform: none; } }
@keyframes lx-pulse { 0%, 100% { opacity: 0.35; } 50% { opacity: 1; } }
.lattice-explorer .lx-reveal { animation: lx-rise 0.36s cubic-bezier(0.22, 1, 0.36, 1) both; animation-delay: calc(var(--i, 0) * 45ms); }
.lattice-explorer .lx-view.lx-active { animation: lx-fade 0.22s ease-out; }
.lattice-explorer .lx-result-body:not(.lx-hidden) { animation: lx-fade 0.18s ease-out; }
.lattice-explorer .lx-hidden { display: none !important; }
.lattice-explorer .lx-completions { animation: lx-pop 0.1s cubic-bezier(0.25, 1, 0.5, 1); transform-origin: top left; }
.lattice-explorer button { transition: transform 0.11s cubic-bezier(0.25, 1, 0.5, 1), border-color 0.13s, background 0.13s, filter 0.13s; }
.lattice-explorer button:active { transform: translateY(1px); }
.lattice-explorer button.lx-primary:hover { filter: brightness(1.06); }
.lattice-explorer .lx-tab, .lattice-explorer .lx-rtab { transition: color 0.13s, background 0.13s; }
.lattice-explorer .lx-gutter { transition: background 0.12s; }
.lattice-explorer .lx-run-dot {
  display: inline-block; width: 7px; height: 7px; border-radius: 50%; background: var(--lx-accent);
  margin-right: 6px; vertical-align: middle; animation: lx-pulse 0.9s ease-in-out infinite;
}
.lattice-explorer .lx-comp-row { transition: background 0.08s; }
@media (prefers-reduced-motion: reduce) {
  .lattice-explorer *, .lattice-explorer *::before, .lattice-explorer *::after {
    animation-duration: 0.001ms !important; animation-delay: 0ms !important; transition-duration: 0.001ms !important;
  }
}

/* Editable base URL + connect (header) */
.lattice-explorer .lx-title { display: flex; align-items: center; gap: 8px; font-weight: 700; }
.lattice-explorer .lx-logo { color: var(--lx-accent); letter-spacing: .2px; }
.lattice-explorer .lx-base {
  font-family: var(--lx-mono); font-size: 12px; color: hsl(var(--foreground)); font-weight: 500;
  background: hsl(var(--background)); border: 1px solid hsl(var(--input)); border-radius: calc(var(--radius) - 2px);
  height: 32px; padding: 0 10px; min-width: 220px; outline: none;
  transition: border-color 0.14s, box-shadow 0.14s;
}
.lattice-explorer .lx-base:focus { border-color: hsl(var(--ring)); box-shadow: 0 0 0 2px hsl(var(--background)), 0 0 0 4px hsl(var(--ring)); }
.lattice-explorer .lx-connect { padding: 0 12px; }

/* Transport segmented control + explainer */
.lattice-explorer .lx-seg { display: inline-flex; background: hsl(var(--muted)); border-radius: var(--radius); padding: 3px; gap: 2px; }
.lattice-explorer .lx-seg-btn {
  border: 0; background: transparent; color: hsl(var(--muted-foreground)); height: 26px; padding: 0 11px;
  border-radius: calc(var(--radius) - 3px); cursor: pointer; font-weight: 500; font-size: 12px;
}
.lattice-explorer .lx-seg-btn:hover { color: hsl(var(--foreground)); }
.lattice-explorer .lx-seg-btn.lx-seg-on { color: hsl(var(--foreground)); background: hsl(var(--background)); box-shadow: 0 1px 2px 0 hsl(0 0% 0% / 0.18); }
.lattice-explorer .lx-mode-explain {
  color: var(--lx-fg-dim); font-size: 11.5px; padding: 5px 12px 8px; line-height: 1.5;
  border-bottom: 1px solid var(--lx-border); background: var(--lx-panel); flex: 0 0 auto;
}

/* Slice toggle chips */
.lattice-explorer .lx-slice-chips { display: inline-flex; gap: 4px; }
.lattice-explorer .lx-slice-chip {
  text-transform: uppercase; font-family: var(--lx-mono); font-size: 10px; letter-spacing: .6px; font-weight: 700;
  padding: 3px 9px; border-radius: 999px; cursor: pointer;
  background: transparent; border: 1px solid var(--lx-border); color: var(--lx-fg-dim);
  opacity: .5; transition: opacity .12s, background .12s, color .12s, border-color .12s;
}
.lattice-explorer .lx-slice-chip:hover { opacity: .82; }
.lattice-explorer .lx-slice-chip.lx-on { opacity: 1; }
.lattice-explorer .lx-slice-chip.lx-on.lx-slice-pub { color: var(--lx-accent); border-color: var(--lx-accent); background: color-mix(in oklab, var(--lx-accent) 14%, transparent); }
.lattice-explorer .lx-slice-chip.lx-on.lx-slice-ctx { color: var(--lx-warn); border-color: var(--lx-warn); background: color-mix(in oklab, var(--lx-warn) 14%, transparent); }
.lattice-explorer .lx-slice-chip.lx-on.lx-slice-priv { color: var(--lx-num); border-color: var(--lx-num); background: color-mix(in oklab, var(--lx-num) 14%, transparent); }

/* Per-slice result cards */
.lattice-explorer .lx-scards { display: grid; grid-template-columns: repeat(auto-fit, minmax(min(320px, 100%), 1fr)); gap: 10px; align-items: start; }
.lattice-explorer .lx-scard { border: 1px solid var(--lx-border); border-radius: var(--radius); overflow: hidden; background: var(--lx-panel); box-shadow: 0 1px 2px 0 hsl(0 0% 0% / 0.12); animation: lx-rise 0.28s cubic-bezier(0.22, 1, 0.36, 1) both; }
.lattice-explorer .lx-scard-head {
  display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: var(--lx-panel-2); border-bottom: 1px solid var(--lx-border);
}
.lattice-explorer .lx-scard-dur { font-family: var(--lx-mono); font-size: 11px; color: var(--lx-fg-dim); }
.lattice-explorer .lx-scard-body { padding: 8px 10px; overflow: auto; max-height: 46vh; }
.lattice-explorer .lx-scard-wait { color: var(--lx-fg-dim); font-size: 12px; font-style: italic; padding: 2px 0; }

/* Slice badge */
.lattice-explorer .lx-slice {
  text-transform: uppercase; font-family: var(--lx-mono); font-size: 10px; letter-spacing: .6px; font-weight: 700;
  padding: 2px 7px; border-radius: 5px; color: var(--lx-bg);
}
.lattice-explorer .lx-slice-pub { background: var(--lx-accent); }
.lattice-explorer .lx-slice-ctx { background: var(--lx-warn); }
.lattice-explorer .lx-slice-priv { background: var(--lx-num); }

/* Status light (reuses lx-pulse) */
.lattice-explorer .lx-sdot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; background: var(--lx-fg-dim); }
.lattice-explorer .lx-sdot-ok { background: var(--lx-accent-2); }
.lattice-explorer .lx-sdot-err { background: var(--lx-danger); }
.lattice-explorer .lx-sdot-run { background: var(--lx-warn); animation: lx-pulse 0.9s ease-in-out infinite; }

/* Cache chip */
.lattice-explorer .lx-cache-slot { display: inline-flex; }
.lattice-explorer .lx-cache {
  font-family: var(--lx-mono); font-size: 10px; font-weight: 700; letter-spacing: .3px;
  padding: 2px 7px; border-radius: 999px; border: 1px solid transparent; white-space: nowrap;
}
.lattice-explorer .lx-cache-hit { color: var(--lx-accent-2); border-color: color-mix(in oklab, var(--lx-accent-2) 45%, transparent); background: color-mix(in oklab, var(--lx-accent-2) 15%, transparent); }
.lattice-explorer .lx-cache-cacheable { color: var(--lx-accent); border-color: color-mix(in oklab, var(--lx-accent) 40%, transparent); background: color-mix(in oklab, var(--lx-accent) 12%, transparent); }
.lattice-explorer .lx-cache-revalidated { color: var(--lx-accent); border-color: color-mix(in oklab, var(--lx-accent) 40%, transparent); }
.lattice-explorer .lx-cache-miss, .lattice-explorer .lx-cache-stale { color: var(--lx-warn); border-color: color-mix(in oklab, var(--lx-warn) 45%, transparent); background: color-mix(in oklab, var(--lx-warn) 12%, transparent); }
.lattice-explorer .lx-cache-private { color: var(--lx-num); border-color: color-mix(in oklab, var(--lx-num) 40%, transparent); }
.lattice-explorer .lx-cache-dynamic, .lattice-explorer .lx-cache-unknown { color: var(--lx-fg-dim); border-color: var(--lx-border); }

/* Trace waterfall */
.lattice-explorer .lx-wf-sec { padding: 2px 2px 14px; }
.lattice-explorer .lx-wf-sec h4 { margin: 4px 0 8px; font-size: 12px; font-weight: 600; color: var(--lx-fg); }
.lattice-explorer .lx-wf-dim { color: var(--lx-fg-dim); font-weight: 400; font-family: var(--lx-mono); font-size: 11px; }
.lattice-explorer .lx-wf-note { color: var(--lx-fg-dim); font-size: 11px; margin: 0 0 8px; line-height: 1.5; }
.lattice-explorer .lx-wf { position: relative; }
.lattice-explorer .lx-wf-axis { position: relative; height: 13px; margin: 0 58px 2px 92px; }
.lattice-explorer .lx-wf-axis span { position: absolute; transform: translateX(-50%); font-family: var(--lx-mono); font-size: 9px; color: var(--lx-fg-dim); white-space: nowrap; }
.lattice-explorer .lx-wf-axis span:first-child { transform: none; }
.lattice-explorer .lx-wf-axis span:last-child { transform: translateX(-100%); }
.lattice-explorer .lx-wf-plot { position: relative; }
.lattice-explorer .lx-wf-grid { position: absolute; top: 0; bottom: 0; left: 92px; right: 58px; pointer-events: none; }
.lattice-explorer .lx-wf-grid i { position: absolute; top: 0; bottom: 0; width: 1px; background: var(--lx-border); opacity: .5; }
.lattice-explorer .lx-wf-row { display: grid; grid-template-columns: 92px 1fr 58px; align-items: center; height: 22px; }
.lattice-explorer .lx-wf-label { font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg-dim); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; padding-right: 8px; }
.lattice-explorer .lx-wf-track { position: relative; height: 13px; }
.lattice-explorer .lx-wf-bar { position: absolute; top: 0; height: 100%; min-width: 3px; display: flex; border-radius: 3px; overflow: hidden; transform-origin: left center; box-shadow: inset 0 0 0 1px color-mix(in oklab, var(--c) 55%, transparent); animation: lx-wf-grow 0.32s cubic-bezier(0.22, 1, 0.36, 1) both; }
.lattice-explorer .lx-wf-wait { height: 100%; background: color-mix(in oklab, var(--c) 24%, transparent); }
.lattice-explorer .lx-wf-body { height: 100%; flex: 1; background: var(--c); }
.lattice-explorer .lx-wf-first { position: absolute; top: 0; bottom: 0; width: 1px; background: var(--lx-bg); opacity: .9; }
.lattice-explorer .lx-wf-bad { filter: grayscale(0.6); opacity: .65; }
.lattice-explorer .lx-wf-ms { text-align: right; font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg-dim); }
.lattice-explorer .lx-wf-round { font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg); text-transform: uppercase; letter-spacing: .4px; margin: 9px 0 3px; }
.lattice-explorer .lx-wf-plan .lx-wf-row { height: 20px; }
@keyframes lx-wf-grow { from { transform: scaleX(0.02); opacity: 0; } to { transform: scaleX(1); opacity: 1; } }
@media (prefers-reduced-motion: reduce) { .lattice-explorer .lx-wf-bar { animation: none; } }

/* Header captions, control groups + schema picker */
.lattice-explorer .lx-cap { font-size: 9px; text-transform: uppercase; letter-spacing: .7px; color: var(--lx-fg-dim); font-weight: 600; }
.lattice-explorer .lx-ctl { display: inline-flex; align-items: center; gap: 6px; }
.lattice-explorer .lx-tb-div { width: 1px; align-self: stretch; margin: 2px 2px; background: var(--lx-border); }
.lattice-explorer .lx-schema-picker { display: inline-flex; align-items: center; gap: 6px; }
.lattice-explorer .lx-schema-select { font-family: var(--lx-mono); font-size: 11px; height: 30px; max-width: 200px; }
.lattice-explorer .lx-schema-select:disabled { opacity: .55; cursor: default; }
.lattice-explorer .lx-past-badge {
  font-family: var(--lx-mono); font-size: 9px; font-weight: 700; text-transform: uppercase; letter-spacing: .5px;
  color: var(--lx-warn); border: 1px solid color-mix(in oklab, var(--lx-warn) 45%, transparent);
  background: color-mix(in oklab, var(--lx-warn) 14%, transparent); border-radius: 999px; padding: 1px 7px;
}

/* Transport method pill + slice lock + variables affordances */
.lattice-explorer .lx-mode-explain { display: flex; align-items: baseline; gap: 8px; }
.lattice-explorer .lx-method {
  font-family: var(--lx-mono); font-size: 10px; font-weight: 700; color: var(--lx-accent); white-space: nowrap;
  border: 1px solid color-mix(in oklab, var(--lx-accent) 40%, transparent); background: color-mix(in oklab, var(--lx-accent) 12%, transparent);
  border-radius: calc(var(--radius) - 3px); padding: 1px 7px; flex: 0 0 auto;
}
.lattice-explorer .lx-mode-text { min-width: 0; }
.lattice-explorer .lx-slice-chip.lx-locked { border-style: dashed; opacity: .45; cursor: not-allowed; }
.lattice-explorer .lx-slice-chip.lx-locked:hover { opacity: .55; }
.lattice-explorer .lx-vars-toggle.lx-has { color: var(--lx-fg); border-color: color-mix(in oklab, var(--lx-accent) 40%, var(--lx-border)); }
.lattice-explorer .lx-vars-count { font-family: var(--lx-mono); font-size: 10px; margin-left: 6px; padding: 0 6px; border-radius: 999px; }
.lattice-explorer .lx-vars-count.lx-ok { color: var(--lx-bg); background: var(--lx-accent-2); }
.lattice-explorer .lx-vars-count.lx-bad { color: var(--lx-bg); background: var(--lx-danger); }
.lattice-explorer .lx-vars-head { display: flex; align-items: center; justify-content: space-between; padding: 6px 12px 0; }
.lattice-explorer .lx-vars-valid { font-family: var(--lx-mono); font-size: 10px; color: var(--lx-fg-dim); }
.lattice-explorer .lx-vars-valid.lx-ok { color: var(--lx-accent-2); }
.lattice-explorer .lx-vars-valid.lx-bad { color: var(--lx-danger); }
`;

/** Inject the stylesheet once per document. */
export function injectStyles(doc: Document = document): void {
  const id = "lattice-explorer-styles";
  if (doc.getElementById(id)) return;
  const style = doc.createElement("style");
  style.id = id;
  style.textContent = EXPLORER_CSS;
  doc.head.appendChild(style);
}
