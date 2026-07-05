/**
 * The Lattice query language (spec §4.8) and canonicalization (§5.1).
 *
 * This is a schema-free client-side canonicalizer. It performs every step of
 * §5.1 that does not require the schema: local-fragment expansion, sorting
 * (fields by (name, canonical args), args by name, variables by name, inline
 * fragments by type), name/comment erasure, and minimal-separator rendering.
 * It cannot erase arguments equal to schema-declared defaults nor expand
 * schema fragments — the origin re-canonicalizes on introduction, so the
 * client string is a stable *local* cache key, not the origin's byte-exact
 * canonical text.
 */

import { RESERVED_PARAMS } from "./wire.ts";
import type { JsonObject, JsonValue, QueryData } from "./wire.ts";

// ---------------------------------------------------------------------------
// Errors

export class LatticeQueryError extends Error {
  readonly line?: number;
  readonly column?: number;
  constructor(message: string, pos?: { line: number; column: number }) {
    super(pos ? `${message} (line ${pos.line}, column ${pos.column})` : message);
    this.name = "LatticeQueryError";
    if (pos) {
      this.line = pos.line;
      this.column = pos.column;
    }
  }
}

/**
 * Thrown by `mergeQueries` (see merge.ts) when two documents cannot share one
 * request, and by selection merging when two selections of one field are
 * structurally incompatible.
 */
export class UnmergeableError extends Error {
  /** The root field that conflicted, when the conflict is root-shaped. */
  readonly root?: string;
  /** The variable that conflicted, when the conflict is a declaration clash. */
  readonly variable?: string;
  constructor(message: string, detail?: { root?: string; variable?: string }) {
    super(message);
    this.name = "UnmergeableError";
    if (detail?.root !== undefined) this.root = detail.root;
    if (detail?.variable !== undefined) this.variable = detail.variable;
  }
}

// ---------------------------------------------------------------------------
// AST

export type Value =
  | { readonly kind: "var"; readonly name: string }
  | { readonly kind: "num"; readonly text: string }
  | { readonly kind: "str"; readonly value: string }
  | { readonly kind: "bool"; readonly value: boolean }
  | { readonly kind: "enum"; readonly name: string }
  | { readonly kind: "list"; readonly items: readonly Value[] };

export interface Argument {
  readonly name: string;
  readonly value: Value;
}

export interface VarDef {
  readonly name: string;
  readonly type: string;
  readonly optional: boolean;
  readonly default?: Value;
}

export interface FieldSel {
  readonly kind: "field";
  readonly name: string;
  readonly args: readonly Argument[];
  readonly depth?: number;
  readonly selections?: readonly Selection[];
}

export interface SpreadSel {
  readonly kind: "spread";
  readonly name: string;
  readonly args: readonly Argument[];
}

export interface InlineSel {
  readonly kind: "inline";
  readonly on: string;
  readonly selections: readonly Selection[];
}

export type Selection = FieldSel | SpreadSel | InlineSel;

export interface FragmentDefNode {
  readonly name: string;
  readonly on: string;
  readonly params: readonly VarDef[];
  readonly selections: readonly Selection[];
}

/**
 * A parsed query document. The phantom `T` types the denormalized result for
 * hooks and `client.query`; it never exists at runtime.
 */
export interface QueryDoc<T = QueryData> {
  readonly kind: "query";
  readonly name?: string;
  readonly variables: readonly VarDef[];
  readonly selections: readonly Selection[];
  readonly fragments: Readonly<Record<string, FragmentDefNode>>;
  readonly imports: readonly string[];
  /** Phantom result type; never set. */
  readonly __data?: T;
}

const RESERVED_NAMES: Record<string, true> = {
  query: true,
  fragment: true,
  import: true,
  on: true,
  true: true,
  false: true,
};

/** Type-name aliases the parser accepts and the canonical form normalizes. */
const TYPE_ALIASES: Record<string, string> = {
  Int: "I32",
  String: "Text",
  Float: "F64",
  Boolean: "Bool",
};

// ---------------------------------------------------------------------------
// Lexer

interface Tok {
  readonly t: "name" | "number" | "string" | "punct" | "eof";
  readonly v: string;
  readonly pos: number;
}

function positionOf(src: string, offset: number): { line: number; column: number } {
  let line = 1;
  let column = 1;
  for (let i = 0; i < offset && i < src.length; i++) {
    if (src[i] === "\n") {
      line++;
      column = 1;
    } else {
      column++;
    }
  }
  return { line, column };
}

const NUMBER_RE = /-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/y;

function lex(src: string): Tok[] {
  const toks: Tok[] = [];
  const n = src.length;
  let i = 0;
  const fail = (msg: string, at: number): never => {
    throw new LatticeQueryError(msg, positionOf(src, at));
  };
  while (i < n) {
    const c = src[i]!;
    if (c === " " || c === "\t" || c === "\n" || c === "\r" || c === ",") {
      i++;
      continue;
    }
    if (c === "#") {
      while (i < n && src[i] !== "\n") i++;
      continue;
    }
    if (c === ".") {
      if (src.startsWith("...", i)) {
        toks.push({ t: "punct", v: "...", pos: i });
        i += 3;
        continue;
      }
      fail("unexpected '.' (did you mean '...'?)", i);
    }
    if (c === "{" || c === "}" || c === "(" || c === ")" || c === "[" || c === "]" || c === ":" || c === "=" || c === "$" || c === "@" || c === "?") {
      toks.push({ t: "punct", v: c, pos: i });
      i++;
      continue;
    }
    if (c === '"') {
      let j = i + 1;
      while (j < n && src[j] !== '"') {
        j += src[j] === "\\" ? 2 : 1;
      }
      if (j >= n) fail("unterminated string", i);
      const lexeme = src.slice(i, j + 1);
      let value: unknown;
      try {
        value = JSON.parse(lexeme);
      } catch {
        value = fail("malformed string literal", i);
      }
      toks.push({ t: "string", v: value as string, pos: i });
      i = j + 1;
      continue;
    }
    if (c === "-" || (c >= "0" && c <= "9")) {
      NUMBER_RE.lastIndex = i;
      const m = NUMBER_RE.exec(src);
      if (!m || m[0].length === 0) fail("malformed number", i);
      const end = i + m![0].length;
      const next = src[end];
      if (next !== undefined && /[A-Za-z0-9_.]/.test(next)) fail("malformed number", i);
      toks.push({ t: "number", v: m![0], pos: i });
      i = end;
      continue;
    }
    if (/[A-Za-z_]/.test(c)) {
      let j = i + 1;
      while (j < n && /[A-Za-z0-9_]/.test(src[j]!)) j++;
      toks.push({ t: "name", v: src.slice(i, j), pos: i });
      i = j;
      continue;
    }
    fail(`unexpected character ${JSON.stringify(c)}`, i);
  }
  toks.push({ t: "eof", v: "", pos: n });
  return toks;
}

/** Canonical rendering of a numeric literal (shortest round-trip; -0 → 0). */
function canonicalNumberText(text: string): string {
  // Preserve big integer literals verbatim: a JSON round-trip would lose
  // precision past 2^53.
  if (/^-?(0|[1-9][0-9]*)$/.test(text) && text.replace("-", "").length > 15) return text;
  return JSON.stringify(JSON.parse(text));
}

// ---------------------------------------------------------------------------
// Parser

class Parser {
  private k = 0;
  constructor(
    private readonly toks: Tok[],
    private readonly src: string,
  ) {}

  private peek(): Tok {
    return this.toks[this.k]!;
  }

  private next(): Tok {
    const t = this.toks[this.k]!;
    if (t.t !== "eof") this.k++;
    return t;
  }

  private fail(msg: string, tok: Tok = this.peek()): never {
    throw new LatticeQueryError(msg, positionOf(this.src, tok.pos));
  }

  private expectPunct(v: string): void {
    const t = this.next();
    if (t.t !== "punct" || t.v !== v) this.fail(`expected ${JSON.stringify(v)}, found ${t.t === "eof" ? "end of input" : JSON.stringify(t.v)}`, t);
  }

  private expectName(what: string): string {
    const t = this.next();
    if (t.t !== "name") this.fail(`expected ${what}, found ${t.t === "eof" ? "end of input" : JSON.stringify(t.v)}`, t);
    return t.v;
  }

  private atPunct(v: string): boolean {
    const t = this.peek();
    return t.t === "punct" && t.v === v;
  }

  document(): QueryDoc {
    const queries: Array<{ name?: string; variables: VarDef[]; selections: Selection[] }> = [];
    const fragments: Record<string, FragmentDefNode> = {};
    const imports: string[] = [];
    for (;;) {
      const t = this.peek();
      if (t.t === "eof") break;
      if (t.t !== "name") this.fail("expected 'query', 'fragment', or 'import' at top level");
      if (t.v === "query") {
        this.next();
        queries.push(this.queryDef());
      } else if (t.v === "fragment") {
        this.next();
        const frag = this.fragmentDef();
        if (fragments[frag.name]) this.fail(`duplicate fragment ${JSON.stringify(frag.name)}`, t);
        fragments[frag.name] = frag;
      } else if (t.v === "import") {
        this.next();
        const path = this.next();
        if (path.t !== "string") this.fail("expected a string after 'import'", path);
        imports.push(path.v);
      } else {
        this.fail(`expected 'query', 'fragment', or 'import', found ${JSON.stringify(t.v)}`);
      }
    }
    if (queries.length !== 1) {
      throw new LatticeQueryError(`a document must contain exactly one query definition (found ${queries.length})`);
    }
    const q = queries[0]!;
    return {
      kind: "query",
      ...(q.name !== undefined ? { name: q.name } : {}),
      variables: q.variables,
      selections: q.selections,
      fragments,
      imports,
    };
  }

  private queryDef(): { name?: string; variables: VarDef[]; selections: Selection[] } {
    let name: string | undefined;
    if (this.peek().t === "name") name = this.next().v;
    const variables = this.atPunct("(") ? this.varDefs() : [];
    const selections = this.selectionSet();
    return { ...(name !== undefined ? { name } : {}), variables, selections };
  }

  private fragmentDef(): FragmentDefNode {
    const nameTok = this.peek();
    const name = this.expectName("a fragment name");
    if (RESERVED_NAMES[name]) this.fail(`${JSON.stringify(name)} is reserved and cannot name a fragment`, nameTok);
    const params = this.atPunct("(") ? this.varDefs() : [];
    const on = this.expectName("'on'");
    if (on !== "on") this.fail("expected 'on' after the fragment name");
    const type = this.expectName("a type name");
    const selections = this.selectionSet();
    return { name, on: type, params, selections };
  }

  private varDefs(): VarDef[] {
    this.expectPunct("(");
    const defs: VarDef[] = [];
    const seen: Record<string, true> = {};
    do {
      this.expectPunct("$");
      const nameTok = this.peek();
      const name = this.expectName("a variable name");
      if (RESERVED_NAMES[name]) this.fail(`${JSON.stringify(name)} is reserved and cannot name a variable`, nameTok);
      if (RESERVED_PARAMS[name])
        this.fail(
          `${JSON.stringify(name)} is a reserved URL parameter name and cannot name a variable (spec §4.8 rule 7)`,
          nameTok,
        );
      if (seen[name]) this.fail(`duplicate variable $${name}`, nameTok);
      seen[name] = true;
      this.expectPunct(":");
      const rawType = this.expectName("a type name");
      const type = TYPE_ALIASES[rawType] ?? rawType;
      let optional = false;
      if (this.atPunct("?")) {
        this.next();
        optional = true;
      }
      let def: Value | undefined;
      if (this.atPunct("=")) {
        this.next();
        def = this.value();
      }
      defs.push({ name, type, optional, ...(def !== undefined ? { default: def } : {}) });
    } while (!this.atPunct(")"));
    this.expectPunct(")");
    return defs;
  }

  private selectionSet(): Selection[] {
    this.expectPunct("{");
    const sels: Selection[] = [];
    do {
      sels.push(this.selection());
    } while (!this.atPunct("}"));
    this.expectPunct("}");
    return sels;
  }

  private selection(): Selection {
    if (this.atPunct("...")) {
      this.next();
      const t = this.peek();
      if (t.t === "name" && t.v === "on") {
        this.next();
        const type = this.expectName("a type name");
        return { kind: "inline", on: type, selections: this.selectionSet() };
      }
      const nameTok = this.peek();
      const name = this.expectName("a fragment name");
      if (RESERVED_NAMES[name]) this.fail(`${JSON.stringify(name)} is reserved and cannot name a fragment`, nameTok);
      const args = this.atPunct("(") ? this.args() : [];
      return { kind: "spread", name, args };
    }
    const name = this.expectName("a field name");
    const args = this.atPunct("(") ? this.args() : [];
    let depth: number | undefined;
    if (this.atPunct("@")) {
      const at = this.next();
      const directive = this.expectName("'depth'");
      if (directive !== "depth") this.fail(`@${directive} does not exist; @depth(n) is the entire directive grammar`, at);
      this.expectPunct("(");
      const num = this.next();
      if (num.t !== "number" || !/^[0-9]+$/.test(num.v) || Number(num.v) < 1) {
        this.fail("@depth takes a positive integer", num);
      }
      depth = Number(num.v);
      this.expectPunct(")");
      if (this.atPunct("{")) this.fail("a field carrying @depth must not carry a selection set (grammar rule 4)");
    }
    const selections = this.atPunct("{") ? this.selectionSet() : undefined;
    return {
      kind: "field",
      name,
      args,
      ...(depth !== undefined ? { depth } : {}),
      ...(selections !== undefined ? { selections } : {}),
    };
  }

  private args(): Argument[] {
    this.expectPunct("(");
    const out: Argument[] = [];
    const seen: Record<string, true> = {};
    do {
      const nameTok = this.peek();
      const name = this.expectName("an argument name");
      if (seen[name]) this.fail(`duplicate argument ${JSON.stringify(name)}`, nameTok);
      seen[name] = true;
      this.expectPunct(":");
      out.push({ name, value: this.value() });
    } while (!this.atPunct(")"));
    this.expectPunct(")");
    return out;
  }

  private value(): Value {
    const t = this.next();
    if (t.t === "punct" && t.v === "$") {
      const nameTok = this.peek();
      const name = this.expectName("a variable name");
      if (RESERVED_NAMES[name]) this.fail(`${JSON.stringify(name)} is reserved and cannot name a variable`, nameTok);
      return { kind: "var", name };
    }
    if (t.t === "punct" && t.v === "[") {
      const items: Value[] = [];
      while (!this.atPunct("]")) items.push(this.value());
      this.expectPunct("]");
      return { kind: "list", items };
    }
    if (t.t === "string") return { kind: "str", value: t.v };
    if (t.t === "number") return { kind: "num", text: canonicalNumberText(t.v) };
    if (t.t === "name") {
      if (t.v === "true") return { kind: "bool", value: true };
      if (t.v === "false") return { kind: "bool", value: false };
      if (RESERVED_NAMES[t.v]) this.fail(`${JSON.stringify(t.v)} is reserved and cannot be an enum value`, t);
      return { kind: "enum", name: t.v };
    }
    this.fail(`expected a value, found ${t.t === "eof" ? "end of input" : JSON.stringify(t.v)}`, t);
  }
}

/**
 * Parse a query document (spec §4.8). Enforces the grammar-level static
 * rules: exactly one query, reserved names, `@depth` as the sole directive,
 * no aliases (no production exists for them).
 */
export function parse(text: string): QueryDoc {
  return new Parser(lex(text.normalize("NFC")), text).document();
}

// ---------------------------------------------------------------------------
// Rendering (canonical text: minimal separators)

export function renderValue(v: Value): string {
  switch (v.kind) {
    case "var":
      return "$" + v.name;
    case "num":
      return v.text;
    case "str":
      return JSON.stringify(v.value);
    case "bool":
      return v.value ? "true" : "false";
    case "enum":
      return v.name;
    case "list":
      return "[" + v.items.map(renderValue).join(",") + "]";
  }
}

/** Canonical `(a:1,b:"x")` rendering: sorted by name, comma-separated, no spaces. Empty args render as `""`. */
export function renderArgs(args: readonly Argument[]): string {
  if (args.length === 0) return "";
  const sorted = [...args].sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
  return "(" + sorted.map((a) => `${a.name}:${renderValue(a.value)}`).join(",") + ")";
}

function renderSelection(sel: Selection): string {
  switch (sel.kind) {
    case "field":
      return (
        sel.name +
        renderArgs(sel.args) +
        (sel.depth !== undefined ? `@depth(${sel.depth})` : "") +
        (sel.selections ? renderSelectionSet(sel.selections) : "")
      );
    case "spread":
      return "..." + sel.name + renderArgs(sel.args);
    case "inline":
      return "... on " + sel.on + renderSelectionSet(sel.selections);
  }
}

function renderSelectionSet(sels: readonly Selection[]): string {
  return "{" + sels.map(renderSelection).join(" ") + "}";
}

// ---------------------------------------------------------------------------
// Local-fragment expansion

type Bindings = ReadonlyMap<string, Value>;

function substValue(v: Value, bindings: Bindings): Value {
  if (v.kind === "var") return bindings.get(v.name) ?? v;
  if (v.kind === "list") return { kind: "list", items: v.items.map((x) => substValue(x, bindings)) };
  return v;
}

function substArgs(args: readonly Argument[], bindings: Bindings): Argument[] {
  return args.map((a) => ({ name: a.name, value: substValue(a.value, bindings) }));
}

function expandInto(
  out: Selection[],
  sels: readonly Selection[],
  fragments: Readonly<Record<string, FragmentDefNode>>,
  stack: readonly string[],
  bindings: Bindings,
): void {
  for (const sel of sels) {
    switch (sel.kind) {
      case "field": {
        const nested: Selection[] | undefined = sel.selections ? [] : undefined;
        if (sel.selections && nested) expandInto(nested, sel.selections, fragments, stack, bindings);
        out.push({
          kind: "field",
          name: sel.name,
          args: substArgs(sel.args, bindings),
          ...(sel.depth !== undefined ? { depth: sel.depth } : {}),
          ...(nested !== undefined ? { selections: nested } : {}),
        });
        break;
      }
      case "inline": {
        const nested: Selection[] = [];
        expandInto(nested, sel.selections, fragments, stack, bindings);
        out.push({ kind: "inline", on: sel.on, selections: nested });
        break;
      }
      case "spread": {
        const frag = fragments[sel.name];
        if (!frag) {
          // Schema fragment: late-bound, the reference survives (§4.5).
          out.push({ kind: "spread", name: sel.name, args: substArgs(sel.args, bindings) });
          break;
        }
        if (stack.includes(sel.name)) {
          throw new LatticeQueryError(`fragment spread cycle through ${JSON.stringify(sel.name)}`);
        }
        const fragBindings = new Map<string, Value>();
        const givenArgs = substArgs(sel.args, bindings);
        for (const arg of givenArgs) {
          if (!frag.params.some((p) => p.name === arg.name)) {
            throw new LatticeQueryError(`fragment ${frag.name} has no parameter ${JSON.stringify(arg.name)}`);
          }
        }
        for (const p of frag.params) {
          const given = givenArgs.find((a) => a.name === p.name);
          const value = given?.value ?? p.default;
          if (value === undefined) {
            throw new LatticeQueryError(`fragment ${frag.name} requires argument ${JSON.stringify(p.name)}`);
          }
          fragBindings.set(p.name, value);
        }
        expandInto(out, frag.selections, fragments, [...stack, sel.name], fragBindings);
        break;
      }
    }
  }
}

function collectVarUses(sels: readonly Selection[], out: Set<string>): void {
  const collectValue = (v: Value): void => {
    if (v.kind === "var") out.add(v.name);
    else if (v.kind === "list") v.items.forEach(collectValue);
  };
  for (const sel of sels) {
    if (sel.kind === "field" || sel.kind === "spread") {
      for (const a of sel.args) collectValue(a.value);
    }
    if (sel.kind !== "spread" && sel.selections) collectVarUses(sel.selections, out);
  }
}

/**
 * Expand all document-local fragment spreads inline (with parameter
 * substitution), leaving schema-fragment references intact, and validate
 * variable usage (grammar rule 7: every declared variable used, every used
 * variable declared). Throws on `import`s: they are a build-time feature the
 * runtime cannot resolve — inline the fragments instead.
 */
export function expandLocalFragments<T>(doc: QueryDoc<T>): QueryDoc<T> {
  if (doc.imports.length > 0) {
    throw new LatticeQueryError(
      `document imports ${JSON.stringify(doc.imports[0])}: imports are resolved at build time; inline the imported fragments before handing the document to the client`,
    );
  }
  const selections: Selection[] = [];
  expandInto(selections, doc.selections, doc.fragments, [], new Map());
  const used = new Set<string>();
  collectVarUses(selections, used);
  for (const v of doc.variables) {
    if (!used.has(v.name)) {
      throw new LatticeQueryError(`variable $${v.name} is declared but never used (grammar rule 7)`);
    }
    used.delete(v.name);
  }
  const undeclared = [...used];
  if (undeclared.length > 0) {
    throw new LatticeQueryError(`variable $${undeclared[0]} is used but never declared (grammar rule 7)`);
  }
  return {
    kind: "query",
    ...(doc.name !== undefined ? { name: doc.name } : {}),
    variables: [...doc.variables],
    selections,
    fragments: {},
    imports: [],
  };
}

// ---------------------------------------------------------------------------
// Selection merging + normalization

function mergeFieldPair(a: FieldSel, b: FieldSel): FieldSel {
  const aDepth = a.depth;
  const bDepth = b.depth;
  if ((aDepth !== undefined && b.selections) || (bDepth !== undefined && a.selections)) {
    throw new UnmergeableError(`field ${JSON.stringify(a.name)} is selected both with @depth and with a selection set`);
  }
  const depth = aDepth !== undefined || bDepth !== undefined ? Math.max(aDepth ?? 0, bDepth ?? 0) : undefined;
  const selections =
    a.selections || b.selections ? [...(a.selections ?? []), ...(b.selections ?? [])] : undefined;
  return {
    kind: "field",
    name: a.name,
    args: a.args,
    ...(depth !== undefined ? { depth } : {}),
    ...(selections !== undefined ? { selections } : {}),
  };
}

/**
 * Merge a list of selections: fields with identical (name, canonical args)
 * union their sub-selections recursively; identical spreads dedupe; inline
 * fragments on one type union. The result is sorted canonically: fields by
 * (name, args), then spreads by (name, args), then inline fragments by type.
 */
export function mergeSelectionList(sels: readonly Selection[]): Selection[] {
  const fields = new Map<string, FieldSel>();
  const spreads = new Map<string, SpreadSel>();
  const inlines = new Map<string, InlineSel>();
  for (const sel of sels) {
    switch (sel.kind) {
      case "field": {
        const key = sel.name + "\u0000" + renderArgs(sel.args);
        const prev = fields.get(key);
        fields.set(key, prev ? mergeFieldPair(prev, sel) : sel);
        break;
      }
      case "spread": {
        const key = sel.name + "\u0000" + renderArgs(sel.args);
        if (!spreads.has(key)) spreads.set(key, sel);
        break;
      }
      case "inline": {
        const prev = inlines.get(sel.on);
        inlines.set(
          sel.on,
          prev ? { kind: "inline", on: sel.on, selections: [...prev.selections, ...sel.selections] } : sel,
        );
        break;
      }
    }
  }
  const byKey = (x: readonly [string, Selection], y: readonly [string, Selection]): number =>
    x[0] < y[0] ? -1 : x[0] > y[0] ? 1 : 0;
  const out: Selection[] = [];
  for (const [, f] of [...fields.entries()].sort(byKey)) {
    out.push(f.selections ? { ...f, selections: mergeSelectionList(f.selections) } : f);
  }
  for (const [, s] of [...spreads.entries()].sort(byKey)) out.push(s);
  for (const [, i] of [...inlines.entries()].sort(byKey)) {
    out.push({ kind: "inline", on: i.on, selections: mergeSelectionList(i.selections) });
  }
  return out;
}

// ---------------------------------------------------------------------------
// Canonicalization

const canonicalCache = new WeakMap<QueryDoc<never>, string>();

/**
 * The canonical text of a document (§5.1, minus the schema-dependent steps —
 * see the module doc). Deterministic and stable: two documents that
 * canonicalize identically are one query as far as this client is concerned.
 */
export function canonicalize<T>(doc: QueryDoc<T>): string {
  const cached = canonicalCache.get(doc as QueryDoc<never>);
  if (cached !== undefined) return cached;
  const expanded = expandLocalFragments(doc);
  const selections = mergeSelectionList(expanded.selections);
  const variables = [...expanded.variables].sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
  let out = "query";
  if (variables.length > 0) {
    out +=
      "(" +
      variables
        .map((v) => `$${v.name}:${v.type}${v.optional ? "?" : ""}${v.default ? "=" + renderValue(v.default) : ""}`)
        .join(",") +
      ")";
  }
  out += renderSelectionSet(selections);
  out = out.normalize("NFC");
  if (Object.isFrozen(doc)) canonicalCache.set(doc as QueryDoc<never>, out);
  return out;
}

// ---------------------------------------------------------------------------
// Canonical JSON + field keys

/**
 * Canonical JSON: keys sorted, no insignificant whitespace. Used for the `vc`
 * claims payload (§8.2) and for variable-set cache keys. `undefined` object
 * members are omitted; non-finite numbers are rejected (not representable on
 * the wire, §3.5.3).
 */
export function canonicalJson(v: unknown): string {
  if (v === null || v === undefined) return "null";
  switch (typeof v) {
    case "string":
    case "boolean":
      return JSON.stringify(v);
    case "number":
      if (!Number.isFinite(v)) throw new LatticeQueryError("non-finite numbers are not representable on the wire");
      return JSON.stringify(Object.is(v, -0) ? 0 : v);
    case "object": {
      if (Array.isArray(v)) return "[" + v.map(canonicalJson).join(",") + "]";
      const obj = v as JsonObject;
      const parts: string[] = [];
      for (const key of Object.keys(obj).sort()) {
        const member = obj[key];
        if (member === undefined) continue;
        parts.push(JSON.stringify(key) + ":" + canonicalJson(member));
      }
      return "{" + parts.join(",") + "}";
    }
    default:
      throw new LatticeQueryError(`value of type ${typeof v} is not representable on the wire`);
  }
}

function renderJsonArg(v: unknown): string {
  if (typeof v === "string") return JSON.stringify(v);
  if (typeof v === "number") {
    if (!Number.isFinite(v)) throw new LatticeQueryError("non-finite numbers are not representable on the wire");
    return JSON.stringify(Object.is(v, -0) ? 0 : v);
  }
  if (typeof v === "boolean") return v ? "true" : "false";
  if (Array.isArray(v)) {
    return "[" + v.filter((x) => x !== undefined && x !== null).map(renderJsonArg).join(",") + "]";
  }
  if (typeof v === "object" && v !== null) return canonicalJson(v as JsonValue);
  throw new LatticeQueryError(`argument value of type ${typeof v} is not representable`);
}

/**
 * The canonical wire field key for a parameterized field: `avatarUrl(size:48)`.
 * Args sorted by name, comma-separated, values rendered from their bound JSON
 * form (strings as JSON strings). Args bound to `undefined`/`null` are
 * omitted — omission is the only spelling of absence (§4.8 rule 6). Mirrors
 * `Lattice.Canonical.canonicalFieldKey`.
 */
export function canonicalFieldKey(name: string, args?: Iterable<readonly [string, unknown]>): string {
  if (!args) return name;
  const present = [...args].filter(([, v]) => v !== undefined && v !== null);
  if (present.length === 0) return name;
  present.sort((a, b) => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0));
  return name + "(" + present.map(([k, v]) => `${k}:${renderJsonArg(v)}`).join(",") + ")";
}

/** Convert a query-literal value to its bound JSON form. Variables resolve through `bound`; unbound variables map to `undefined`. */
export function valueToJson(v: Value, bound: Readonly<Record<string, unknown>>): unknown {
  switch (v.kind) {
    case "var":
      return bound[v.name];
    case "num":
      return JSON.parse(v.text) as number;
    case "str":
      return v.value;
    case "bool":
      return v.value;
    case "enum":
      return v.name;
    case "list":
      return v.items.map((x) => valueToJson(x, bound));
  }
}

// ---------------------------------------------------------------------------
// gql template tag

function deepFreeze<T>(value: T): T {
  if (typeof value === "object" && value !== null && !Object.isFrozen(value)) {
    Object.freeze(value);
    for (const key of Object.keys(value)) {
      deepFreeze((value as Record<string, unknown>)[key]);
    }
  }
  return value;
}

/**
 * Parse a query at module scope: returns a frozen document whose canonical
 * text is validated (and cached) eagerly, so malformed queries and rule
 * violations throw at import time rather than at first render.
 *
 * ```ts
 * const HERO = gql<HeroData>`
 *   query Hero($episode: Episode) {
 *     hero(episode: $episode) { name }
 *   }`;
 * ```
 *
 * Interpolations are plain text splices (numbers/strings), not document
 * composition; share selections with fragments in the same template instead.
 */
export function gql<T = QueryData>(
  strings: TemplateStringsArray,
  ...values: ReadonlyArray<string | number | boolean>
): QueryDoc<T> {
  let text = strings[0] ?? "";
  for (let i = 0; i < values.length; i++) {
    text += String(values[i]) + (strings[i + 1] ?? "");
  }
  const doc = parse(text) as QueryDoc<T>;
  deepFreeze(doc);
  canonicalize(doc);
  return doc;
}
