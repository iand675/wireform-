/**
 * Minimal, dependency-free syntax highlighting for the Lattice query language
 * and the IDL. Produces HTML with `lx-tok-*` class spans, meant to sit behind
 * a transparent `<textarea>` in the explorer editor. Tokenization mirrors the
 * lexers in `complete.ts`/`schema.ts` closely enough to color correctly; it is
 * presentational only and never rejects input.
 */

const NAME_START = /[A-Za-z_]/;
const NAME_CHAR = /[A-Za-z0-9_]/;
const IDL_NAME_CHAR = /[A-Za-z0-9_.]/;
const DIGIT = /[0-9]/;
const UPPER = /[A-Z]/;

type TokClass =
  | "keyword"
  | "type"
  | "field"
  | "string"
  | "number"
  | "variable"
  | "directive"
  | "comment"
  | "punct"
  | "text";

interface Run {
  readonly cls: TokClass;
  readonly text: string;
}

function escapeHtml(s: string): string {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function render(runs: readonly Run[]): string {
  let out = "";
  for (const r of runs) {
    out += r.cls === "text" ? escapeHtml(r.text) : `<span class="lx-tok-${r.cls}">${escapeHtml(r.text)}</span>`;
  }
  return out;
}

const QUERY_KEYWORDS: Record<string, true> = { query: true, fragment: true, on: true, true: true, false: true, null: true };

/** Highlight Lattice query text (spec §4) to HTML. */
export function highlightQuery(text: string): string {
  const runs: Run[] = [];
  let i = 0;
  const n = text.length;
  const push = (cls: TokClass, s: string): void => {
    runs.push({ cls, text: s });
  };
  while (i < n) {
    const c = text[i]!;
    if (c === "#") {
      const start = i;
      while (i < n && text[i] !== "\n") i++;
      push("comment", text.slice(start, i));
      continue;
    }
    if (c === " " || c === "\t" || c === "\r" || c === "\n" || c === ",") {
      push("text", c);
      i++;
      continue;
    }
    if (c === '"') {
      const start = i;
      i++;
      while (i < n && text[i] !== '"') {
        if (text[i] === "\\" && i + 1 < n) i += 2;
        else i++;
      }
      i++;
      push("string", text.slice(start, Math.min(i, n)));
      continue;
    }
    if (c === "." && text[i + 1] === "." && text[i + 2] === ".") {
      push("punct", "...");
      i += 3;
      continue;
    }
    if (c === "$") {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      push("variable", text.slice(start, i));
      continue;
    }
    if (c === "@") {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      push("directive", text.slice(start, i));
      continue;
    }
    if (NAME_START.test(c)) {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      const word = text.slice(start, i);
      if (QUERY_KEYWORDS[word]) push("keyword", word);
      else if (UPPER.test(word[0]!)) push("type", word);
      else push("field", word);
      continue;
    }
    if (DIGIT.test(c) || (c === "-" && DIGIT.test(text[i + 1] ?? ""))) {
      const start = i;
      i++;
      while (i < n && (DIGIT.test(text[i]!) || text[i] === "." || text[i] === "e" || text[i] === "-" || text[i] === "+")) i++;
      push("number", text.slice(start, i));
      continue;
    }
    push("punct", c);
    i++;
  }
  return render(runs);
}

const IDL_KEYWORDS: Record<string, true> = {
  schema: true,
  newtype: true,
  enum: true,
  data: true,
  claims: true,
  interface: true,
  entity: true,
  extend: true,
  fragment: true,
  get: true,
  list: true,
  mutation: true,
  has: true,
  one: true,
  many: true,
  by: true,
  ordered: true,
  page: true,
  max: true,
  of: true,
  implements: true,
  returns: true,
  allow: true,
  writes: true,
  invalidates: true,
  effect: true,
  batch: true,
  as: true,
  fetch: true,
  visible: true,
  when: true,
  to: true,
  all: true,
  private: true,
  public: true,
  default: true,
  derived: true,
  reads: true,
  on: true,
  read: true,
  maintained: true,
  asc: true,
  desc: true,
  closed: true,
  open: true,
  count: true,
  sum: true,
  min: true,
};

/** Highlight Lattice IDL text (spec §3.4) to HTML. */
export function highlightIdl(text: string): string {
  const runs: Run[] = [];
  let i = 0;
  const n = text.length;
  const push = (cls: TokClass, s: string): void => {
    runs.push({ cls, text: s });
  };
  while (i < n) {
    const c = text[i]!;
    if (c === "-" && text[i + 1] === "-") {
      const start = i;
      while (i < n && text[i] !== "\n") i++;
      push("comment", text.slice(start, i));
      continue;
    }
    if (c === " " || c === "\t" || c === "\r" || c === "\n" || c === ",") {
      push("text", c);
      i++;
      continue;
    }
    if (c === '"') {
      const start = i;
      i++;
      while (i < n && text[i] !== '"') {
        if (text[i] === "\\" && i + 1 < n) i += 2;
        else i++;
      }
      i++;
      push("string", text.slice(start, Math.min(i, n)));
      continue;
    }
    if (NAME_START.test(c)) {
      const start = i;
      i++;
      while (i < n && IDL_NAME_CHAR.test(text[i]!)) i++;
      const word = text.slice(start, i);
      if (IDL_KEYWORDS[word]) push("keyword", word);
      else if (UPPER.test(word[0]!)) push("type", word);
      else push("field", word);
      continue;
    }
    if (DIGIT.test(c)) {
      const start = i;
      i++;
      while (i < n && DIGIT.test(text[i]!)) i++;
      push("number", text.slice(start, i));
      continue;
    }
    push("punct", c);
    i++;
  }
  return render(runs);
}

/** Highlight JSON (mutation input) to HTML. Presentational, never rejects. */
export function highlightJson(text: string): string {
  const runs: Run[] = [];
  let i = 0;
  const n = text.length;
  const isDigit = (ch: string): boolean => ch >= "0" && ch <= "9";
  const isLower = (ch: string): boolean => ch >= "a" && ch <= "z";
  const isWs = (ch: string): boolean => ch === " " || ch === "\t" || ch === "\n" || ch === "\r";
  while (i < n) {
    const c = text[i]!;
    if (c === '"') {
      let j = i + 1;
      while (j < n) {
        const d = text[j]!;
        if (d === "\\") {
          j += 2;
          continue;
        }
        j++;
        if (d === '"') break;
      }
      let k = j;
      while (k < n && isWs(text[k]!)) k++;
      runs.push({ cls: text[k] === ":" ? "field" : "string", text: text.slice(i, j) });
      i = j;
      continue;
    }
    if (c === "-" || isDigit(c)) {
      let j = i + 1;
      while (j < n) {
        const d = text[j]!;
        if (!(isDigit(d) || d === "." || d === "e" || d === "E" || d === "+" || d === "-")) break;
        j++;
      }
      runs.push({ cls: "number", text: text.slice(i, j) });
      i = j;
      continue;
    }
    if (isLower(c)) {
      let j = i + 1;
      while (j < n && isLower(text[j]!)) j++;
      const word = text.slice(i, j);
      runs.push({ cls: word === "true" || word === "false" || word === "null" ? "keyword" : "text", text: word });
      i = j;
      continue;
    }
    if (c === "{" || c === "}" || c === "[" || c === "]" || c === ":" || c === ",") {
      runs.push({ cls: "punct", text: c });
      i++;
      continue;
    }
    runs.push({ cls: "text", text: c });
    i++;
  }
  return render(runs);
}
