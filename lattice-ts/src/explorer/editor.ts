/**
 * A tiny, dependency-free code editor: a transparent `<textarea>` layered over
 * a syntax-highlighted `<pre>`, with an optional schema-aware completion popup.
 * This is the classic overlay technique (works because the editor font is
 * monospace, so caret pixel position is `col * charWidth`), kept deliberately
 * small — the explorer wants a good-enough editor with zero third-party deps,
 * not a full code-mirror.
 */

import type { CompletionItem } from "./complete.ts";

export interface EditorOptions {
  readonly language: "query" | "idl";
  readonly highlight: (text: string) => string;
  readonly value?: string;
  /** Cmd/Ctrl+Enter. */
  readonly onRun?: () => void;
  readonly onChange?: (value: string) => void;
  /** Query completion provider; omitted for the IDL editor. */
  readonly complete?: (text: string, offset: number) => CompletionItem[];
  /** Show a line-number gutter (default true). */
  readonly lineNumbers?: boolean;
}

export interface Editor {
  readonly el: HTMLElement;
  getValue(): string;
  setValue(value: string): void;
  focus(): void;
  /** Move the caret to a character offset and scroll it into view. */
  revealOffset(offset: number): void;
  /** Insert text at the caret (used by the docs tree), placing the caret inside a `{ }` if present. */
  insertAtCaret(text: string): void;
}

const NAME_CHAR = /[A-Za-z0-9_]/;

const BRACKET_OPEN: Record<string, string> = { "(": ")", "[": "]", "{": "}" };
const BRACKET_CLOSE: Record<string, string> = { ")": "(", "]": "[", "}": "{" };

/** Mark every index that sits inside a double-quoted string (with `\\` escapes). */
function stringMask(text: string): Uint8Array {
  const mask = new Uint8Array(text.length);
  let inStr = false;
  for (let k = 0; k < text.length; k++) {
    const c = text[k];
    if (inStr) {
      mask[k] = 1;
      if (c === "\\") {
        if (k + 1 < text.length) mask[k + 1] = 1;
        k++;
      } else if (c === '"') {
        inStr = false;
      }
    } else if (c === '"') {
      inStr = true;
      mask[k] = 1;
    }
  }
  return mask;
}

/** Index of the bracket matching the one at `i`, skipping string contents; -1 if none. */
function matchBracket(text: string, i: number, mask: Uint8Array): number {
  const ch = text[i]!;
  const openWant = BRACKET_OPEN[ch];
  const closeWant = BRACKET_CLOSE[ch];
  const dir = openWant ? 1 : closeWant ? -1 : 0;
  if (dir === 0) return -1;
  const want = (openWant ?? closeWant)!;
  let depth = 1;
  for (let k = i + dir; k >= 0 && k < text.length; k += dir) {
    if (mask[k]) continue;
    const c = text[k];
    if (c === ch) depth++;
    else if (c === want) {
      depth--;
      if (depth === 0) return k;
    }
  }
  return -1;
}

export function createEditor(opts: EditorOptions): Editor {
  const el = document.createElement("div");
  el.className = "lx-editor";
  const showGutter = opts.lineNumbers !== false;
  const gutter = document.createElement("div");
  gutter.className = "lx-gutter-lines";
  gutter.setAttribute("aria-hidden", "true");
  const gutterInner = document.createElement("div");
  gutterInner.className = "lx-gutter-inner";
  gutter.appendChild(gutterInner);
  const pre = document.createElement("pre");
  pre.className = "lx-hl";
  pre.setAttribute("aria-hidden", "true");
  const code = document.createElement("code");
  pre.appendChild(code);
  const braceLayer = document.createElement("div");
  braceLayer.className = "lx-brace-layer";
  braceLayer.setAttribute("aria-hidden", "true");
  const ta = document.createElement("textarea");
  ta.className = "lx-input";
  ta.spellcheck = false;
  ta.autocapitalize = "off";
  ta.setAttribute("autocomplete", "off");
  ta.setAttribute("autocorrect", "off");
  ta.value = opts.value ?? "";
  el.appendChild(pre);
  el.appendChild(braceLayer);
  if (showGutter) el.appendChild(gutter);
  el.appendChild(ta);

  const popup = document.createElement("div");
  popup.className = "lx-completions";
  popup.style.display = "none";
  el.appendChild(popup);

  let items: CompletionItem[] = [];
  let selected = 0;

  const paint = (): void => {
    // Trailing newline needs a filler so the last line renders in the <pre>.
    code.innerHTML = opts.highlight(ta.value + "\u200b");
    pre.scrollTop = ta.scrollTop;
    pre.scrollLeft = ta.scrollLeft;
    updateGutter();
    updateBraces();
  };

  const charMetrics = (): { w: number; h: number } => {
    const probe = document.createElement("span");
    probe.style.visibility = "hidden";
    probe.style.position = "absolute";
    probe.style.whiteSpace = "pre";
    probe.textContent = "0000000000";
    code.appendChild(probe);
    const w = probe.getBoundingClientRect().width / 10;
    probe.remove();
    const lh = parseFloat(getComputedStyle(ta).lineHeight);
    return { w: w || 8, h: Number.isFinite(lh) ? lh : 20 };
  };

  const padMetrics = (): { padL: number; padT: number } => {
    const cs = getComputedStyle(ta);
    return { padL: parseFloat(cs.paddingLeft) || 12, padT: parseFloat(cs.paddingTop) || 10 };
  };

  const lineColOf = (idx: number): { line: number; col: number } => {
    const before = ta.value.slice(0, idx);
    let line = 0;
    for (let k = 0; k < before.length; k++) if (before[k] === "\n") line++;
    return { line, col: idx - (before.lastIndexOf("\n") + 1) };
  };

  const updateGutter = (): void => {
    if (!showGutter) return;
    const n = ta.value.split("\n").length;
    if (gutterInner.childElementCount !== n) {
      const frag = document.createDocumentFragment();
      for (let i = 1; i <= n; i++) {
        const d = document.createElement("div");
        d.textContent = String(i);
        frag.appendChild(d);
      }
      gutterInner.replaceChildren(frag);
      el.style.setProperty("--lx-gutter-w", `calc(${Math.max(2, String(n).length)}ch + 20px)`);
    }
    gutterInner.style.transform = `translateY(${-ta.scrollTop}px)`;
  };

  const updateBraces = (): void => {
    braceLayer.replaceChildren();
    if (ta.selectionStart !== ta.selectionEnd) return;
    const text = ta.value;
    const s = ta.selectionStart;
    const mask = stringMask(text);
    let bi = -1;
    let mi = -1;
    for (const cand of [s - 1, s]) {
      if (cand < 0 || cand >= text.length || mask[cand]) continue;
      const c = text[cand]!;
      if (!(c in BRACKET_OPEN) && !(c in BRACKET_CLOSE)) continue;
      const m = matchBracket(text, cand, mask);
      if (m >= 0) {
        bi = cand;
        mi = m;
        break;
      }
    }
    if (bi < 0) return;
    const { w, h } = charMetrics();
    const { padL, padT } = padMetrics();
    for (const idx of [bi, mi]) {
      const { line, col } = lineColOf(idx);
      const box = document.createElement("span");
      box.className = "lx-brace-hl";
      box.style.left = `${padL + col * w - ta.scrollLeft}px`;
      box.style.top = `${padT + line * h - ta.scrollTop}px`;
      box.style.width = `${w}px`;
      box.style.height = `${h}px`;
      braceLayer.appendChild(box);
    }
  };

  const closePopup = (): void => {
    popup.style.display = "none";
    items = [];
  };

  const renderPopup = (): void => {
    if (items.length === 0) {
      closePopup();
      return;
    }
    popup.innerHTML = "";
    items.forEach((item, idx) => {
      const row = document.createElement("div");
      row.className = "lx-comp-row" + (idx === selected ? " lx-comp-sel" : "");
      const kind = document.createElement("span");
      kind.className = "lx-comp-kind lx-kind-" + item.kind;
      kind.textContent = item.kind[0]!.toUpperCase();
      const label = document.createElement("span");
      label.className = "lx-comp-label";
      label.textContent = item.label;
      row.appendChild(kind);
      row.appendChild(label);
      if (item.detail) {
        const detail = document.createElement("span");
        detail.className = "lx-comp-detail";
        detail.textContent = item.detail;
        row.appendChild(detail);
      }
      row.addEventListener("mousedown", (e) => {
        e.preventDefault();
        selected = idx;
        accept();
      });
      popup.appendChild(row);
    });
    // Position near the caret.
    const upto = ta.value.slice(0, ta.selectionStart);
    const nl = upto.lastIndexOf("\n");
    const line = upto.split("\n").length - 1;
    const col = ta.selectionStart - (nl + 1);
    const { w, h } = charMetrics();
    const { padL, padT } = padMetrics();
    popup.style.display = "block";
    popup.style.left = `${Math.max(4, padL + col * w - ta.scrollLeft)}px`;
    popup.style.top = `${padT + (line + 1) * h - ta.scrollTop + 2}px`;
  };

  const currentWord = (): { start: number; prefix: string } => {
    const caret = ta.selectionStart;
    let start = caret;
    while (start > 0 && NAME_CHAR.test(ta.value[start - 1]!)) start--;
    const lead = ta.value[start - 1];
    if (lead === "$" || lead === "@") start--;
    return { start, prefix: ta.value.slice(start, caret) };
  };

  const triggerCompletion = (force: boolean): void => {
    if (!opts.complete) return;
    const caret = ta.selectionStart;
    const { prefix } = currentWord();
    if (!force && prefix === "" ) {
      // Only auto-open once there's a word to filter, to avoid noise.
      closePopup();
      return;
    }
    items = opts.complete(ta.value, caret);
    selected = 0;
    renderPopup();
  };

  const accept = (): void => {
    const item = items[selected];
    if (!item) return;
    const { start } = currentWord();
    const caret = ta.selectionStart;
    const insert = item.insertText ?? item.label;
    const before = ta.value.slice(0, start);
    const after = ta.value.slice(caret);
    ta.value = before + insert + after;
    // Place the caret inside a `{ }` if the insertion opened one.
    const brace = insert.indexOf("{ }");
    const caretPos = brace >= 0 ? start + brace + 2 : start + insert.length;
    ta.selectionStart = ta.selectionEnd = caretPos;
    closePopup();
    paint();
    opts.onChange?.(ta.value);
    ta.focus();
  };

  ta.addEventListener("input", () => {
    paint();
    opts.onChange?.(ta.value);
    triggerCompletion(false);
  });
  ta.addEventListener("scroll", () => {
    pre.scrollTop = ta.scrollTop;
    pre.scrollLeft = ta.scrollLeft;
    updateGutter();
    updateBraces();
    if (popup.style.display === "block") renderPopup();
  });
  ta.addEventListener("keyup", updateBraces);
  ta.addEventListener("click", updateBraces);
  ta.addEventListener("blur", () => {
    // Delay so a click on a completion row (mousedown) still registers.
    setTimeout(closePopup, 120);
  });
  ta.addEventListener("keydown", (e) => {
    const open = popup.style.display === "block";
    if ((e.metaKey || e.ctrlKey) && e.key === "Enter") {
      e.preventDefault();
      closePopup();
      opts.onRun?.();
      return;
    }
    if ((e.ctrlKey || e.metaKey) && e.key === " ") {
      e.preventDefault();
      triggerCompletion(true);
      return;
    }
    if (open) {
      if (e.key === "ArrowDown") {
        e.preventDefault();
        selected = (selected + 1) % items.length;
        renderPopup();
        return;
      }
      if (e.key === "ArrowUp") {
        e.preventDefault();
        selected = (selected - 1 + items.length) % items.length;
        renderPopup();
        return;
      }
      if (e.key === "Enter" || e.key === "Tab") {
        e.preventDefault();
        accept();
        return;
      }
      if (e.key === "Escape") {
        e.preventDefault();
        closePopup();
        return;
      }
    }
    if (!open && e.key === "Enter") {
      // Auto-indent: carry the current line's leading whitespace, add one
      // level after an opening `{`, and split a `{ }` pair onto three lines.
      e.preventDefault();
      const s = ta.selectionStart;
      const eEnd = ta.selectionEnd;
      const lineStart = ta.value.lastIndexOf("\n", s - 1) + 1;
      const indent = /^[ \t]*/.exec(ta.value.slice(lineStart, s))?.[0] ?? "";
      const before = ta.value[s - 1];
      const after = ta.value[s];
      let insert: string;
      let caret: number;
      if (before === "{" && after === "}") {
        const inner = indent + "  ";
        insert = "\n" + inner + "\n" + indent;
        caret = s + 1 + inner.length;
      } else if (before === "{" || before === "(" || before === "[") {
        insert = "\n" + indent + "  ";
        caret = s + insert.length;
      } else {
        insert = "\n" + indent;
        caret = s + insert.length;
      }
      ta.value = ta.value.slice(0, s) + insert + ta.value.slice(eEnd);
      ta.selectionStart = ta.selectionEnd = caret;
      paint();
      opts.onChange?.(ta.value);
      return;
    }
    if (!open && e.key === "}") {
      // Dedent a whitespace-only line one level as the closing brace lands.
      const s = ta.selectionStart;
      const lineStart = ta.value.lastIndexOf("\n", s - 1) + 1;
      const prefix = ta.value.slice(lineStart, s);
      if (/^[ \t]+$/.test(prefix) && prefix.endsWith("  ")) {
        e.preventDefault();
        const cut = s - 2;
        ta.value = ta.value.slice(0, cut) + "}" + ta.value.slice(ta.selectionEnd);
        ta.selectionStart = ta.selectionEnd = cut + 1;
        paint();
        opts.onChange?.(ta.value);
        return;
      }
    }
    if (e.key === "Tab") {
      // Indent rather than leave the field.
      e.preventDefault();
      const s = ta.selectionStart;
      const eEnd = ta.selectionEnd;
      ta.value = ta.value.slice(0, s) + "  " + ta.value.slice(eEnd);
      ta.selectionStart = ta.selectionEnd = s + 2;
      paint();
      opts.onChange?.(ta.value);
    }
  });

  paint();

  return {
    el,
    getValue: () => ta.value,
    setValue: (value: string) => {
      ta.value = value;
      paint();
      opts.onChange?.(value);
    },
    focus: () => ta.focus(),
    revealOffset: (offset: number) => {
      ta.focus();
      ta.selectionStart = ta.selectionEnd = Math.max(0, Math.min(offset, ta.value.length));
      const before = ta.value.slice(0, offset);
      const line = before.split("\n").length - 1;
      const { h } = charMetrics();
      ta.scrollTop = Math.max(0, line * h - ta.clientHeight / 2);
      paint();
    },
    insertAtCaret: (text: string) => {
      const s = ta.selectionStart;
      const e = ta.selectionEnd;
      ta.value = ta.value.slice(0, s) + text + ta.value.slice(e);
      const brace = text.indexOf("{ }");
      const caret = brace >= 0 ? s + brace + 2 : s + text.length;
      ta.selectionStart = ta.selectionEnd = caret;
      paint();
      opts.onChange?.(ta.value);
      ta.focus();
    },
  };
}
