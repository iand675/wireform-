/**
 * Schema-aware completion, context analysis, and light validation for the
 * Lattice query language (spec §4), driving the explorer's query editor.
 *
 * Grammar validation is delegated to `@wireform/lattice`'s `parse` (the same
 * canonicalizing parser the client uses), which reports line/column on
 * malformed input. Completion is a tolerant, cursor-local analysis: it never
 * needs a full parse (the document is usually mid-edit and unparseable), only
 * the *type context* at the cursor, recovered by a brace-and-paren aware walk
 * of the tokens up to the caret. Authoritative validation beyond the grammar
 * (default erasure, slice partition, budget checks) is the origin's job and
 * surfaces when the query is run.
 */

import { parse } from "../canonical.ts";
import { LatticeQueryError } from "../canonical.ts";
import type {
  ArgModel,
  EdgeModel,
  FieldModel,
  SchemaModel,
  Target,
} from "./schema.ts";
import { entityMembers, membersOf, targetLabel, targetTypes } from "./schema.ts";

// ---------------------------------------------------------------------------
// Tokens (query grammar)

type QTokKind = "name" | "punct" | "str" | "num" | "var" | "spread" | "directive";

interface QTok {
  readonly kind: QTokKind;
  readonly value: string;
  readonly start: number;
  readonly end: number;
}

const NAME_START = /[A-Za-z_]/;
const NAME_CHAR = /[A-Za-z0-9_]/;
const DIGIT = /[0-9]/;

/** Lex query text, discarding `#`-to-EOL comments and whitespace/commas. */
function lexQuery(text: string, upTo = text.length): QTok[] {
  const toks: QTok[] = [];
  let i = 0;
  const n = Math.min(text.length, upTo);
  while (i < n) {
    const c = text[i]!;
    if (c === "#") {
      while (i < n && text[i] !== "\n") i++;
      continue;
    }
    if (c === " " || c === "\t" || c === "\r" || c === "\n" || c === ",") {
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
      toks.push({ kind: "str", value: text.slice(start, i), start, end: i });
      continue;
    }
    if (c === "." && text[i + 1] === "." && text[i + 2] === ".") {
      toks.push({ kind: "spread", value: "...", start: i, end: i + 3 });
      i += 3;
      continue;
    }
    if (c === "$") {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      toks.push({ kind: "var", value: text.slice(start, i), start, end: i });
      continue;
    }
    if (c === "@") {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      toks.push({ kind: "directive", value: text.slice(start, i), start, end: i });
      continue;
    }
    if (NAME_START.test(c)) {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      toks.push({ kind: "name", value: text.slice(start, i), start, end: i });
      continue;
    }
    if (DIGIT.test(c) || (c === "-" && DIGIT.test(text[i + 1] ?? ""))) {
      const start = i;
      i++;
      while (i < n && (DIGIT.test(text[i]!) || text[i] === "." || text[i] === "e" || text[i] === "-")) i++;
      toks.push({ kind: "num", value: text.slice(start, i), start, end: i });
      continue;
    }
    toks.push({ kind: "punct", value: c, start: i, end: i + 1 });
    i++;
  }
  return toks;
}

// ---------------------------------------------------------------------------
// Context analysis

/** The kind of thing a completion offers. */
export type CompletionKind =
  | "root"
  | "field"
  | "edge"
  | "arg"
  | "enum"
  | "type"
  | "keyword"
  | "fragment"
  | "variable";

export interface CompletionItem {
  readonly label: string;
  readonly kind: CompletionKind;
  /** Short type/signature shown to the right of the label. */
  readonly detail?: string;
  /** Text to insert (defaults to `label`). */
  readonly insertText?: string;
  /** Longer documentation shown in a detail panel. */
  readonly doc?: string;
}

/** Where the caret sits, semantically. */
export type Scope = "root" | "selection" | "args" | "argValue" | "spread" | "onType";

/** The type whose members are in scope: a schema target, or the query root. */
export type TypeContext = Target | { readonly kind: "root" };

export interface QueryContext {
  readonly scope: Scope;
  readonly type: TypeContext;
  /** For `args`/`argValue`: the field/root owning the argument list. */
  readonly ownerField?: string;
  /** For `argValue`: the argument name being given a value. */
  readonly argName?: string;
  /** The partial word under the caret. */
  readonly prefix: string;
  readonly prefixKind: "word" | "var" | "directive";
}

const ROOT: TypeContext = { kind: "root" };

/** The identifier-ish run immediately preceding `offset`. */
function wordBefore(text: string, offset: number): { prefix: string; prefixKind: "word" | "var" | "directive" } {
  let i = offset;
  while (i > 0 && NAME_CHAR.test(text[i - 1]!)) i--;
  const lead = text[i - 1];
  if (lead === "$") return { prefix: text.slice(i, offset), prefixKind: "var" };
  if (lead === "@") return { prefix: text.slice(i, offset), prefixKind: "directive" };
  return { prefix: text.slice(i, offset), prefixKind: "word" };
}

/** Resolve the child type entered by selecting `field` under `parent`. */
function childType(schema: SchemaModel, parent: TypeContext, field: string): Target | undefined {
  if (parent.kind === "root") return schema.roots.get(field)?.target;
  if (parent.kind === "union") {
    for (const m of parent.members) {
      const e = entityMembers(schema, m).edges.find((x) => x.name === field);
      if (e) return e.target;
    }
    return undefined;
  }
  const { edges } = membersOf(schema, parent);
  return edges.find((e) => e.name === field)?.target;
}

/** The declared arguments of `field` under type context `ctx`. */
function argsFor(schema: SchemaModel, ctx: TypeContext, field: string): readonly ArgModel[] {
  if (ctx.kind === "root") return schema.roots.get(field)?.args ?? [];
  const scopes =
    ctx.kind === "union" ? ctx.members.map((m) => entityMembers(schema, m)) : [membersOf(schema, ctx)];
  for (const s of scopes) {
    const f = s.fields.find((x) => x.name === field);
    if (f) return f.args;
    const e = s.edges.find((x) => x.name === field);
    if (e) return e.kind === "many" ? PAGINATION_ARGS : [];
  }
  return [];
}

/** Pagination arguments available on a paginated collection (spec §4.8 rule 6). */
const PAGINATION_ARGS: readonly ArgModel[] = [
  { name: "first", type: "Int" },
  { name: "after", type: "Cursor" },
  { name: "last", type: "Int" },
  { name: "before", type: "Cursor" },
  { name: "around", type: "Cursor" },
];

/** Declared variables of the document (scanned from the `query (...)` header). */
function declaredVars(toks: readonly QTok[]): Map<string, string> {
  const vars = new Map<string, string>();
  const firstBrace = toks.findIndex((t) => t.kind === "punct" && t.value === "{");
  const header = firstBrace === -1 ? toks : toks.slice(0, firstBrace);
  for (let i = 0; i < header.length; i++) {
    const t = header[i]!;
    if (t.kind === "var" && header[i + 1]?.value === ":") {
      const typeTok = header[i + 2];
      vars.set(t.value.slice(1), typeTok?.kind === "name" ? typeTok.value : "");
    }
  }
  return vars;
}

/**
 * Recover the semantic context at `offset`: which selection set (and thus type)
 * the caret is in, or whether it is inside an argument list, a spread, or a
 * `... on` type position.
 */
export function analyzeQueryContext(text: string, offset: number, schema: SchemaModel): QueryContext {
  const { prefix, prefixKind } = wordBefore(text, offset);
  const toks = lexQuery(text, offset);
  // Frame stack: each `{` opens a selection set with a resolved type context.
  const frames: TypeContext[] = [];
  let pendingField: string | undefined; // last field name that could own the next `{`
  let pendingOn: string | undefined; // `... on Type` — the next `{`'s type
  // Paren tracking for the innermost frame: >0 means we're inside `field(...)`.
  let parenDepth = 0;
  let parenOwner: string | undefined; // field owning the open `(`
  let lastArgName: string | undefined; // arg name most recently seen inside `(`
  let afterColon = false; // caret is in an arg value position
  let afterSpread = false; // previous token was `...`
  let afterOn = false; // previous token was `on` following a `...`

  for (let i = 0; i < toks.length; i++) {
    const t = toks[i]!;
    if (parenDepth > 0) {
      // Inside an argument list: track arg name / value position, ignore braces.
      if (t.kind === "punct" && t.value === "(") parenDepth++;
      else if (t.kind === "punct" && t.value === ")") parenDepth--;
      else if (t.kind === "punct" && t.value === ":") afterColon = true;
      else if (t.kind === "name" && !afterColon) lastArgName = t.value;
      else if (t.kind === "punct" && (t.value === "," )) afterColon = false;
      // a value ends the colon state once a full value token passes and next arg begins
      else if (afterColon && (t.kind === "num" || t.kind === "str" || t.kind === "var" || t.kind === "name")) {
        // stay in value until a separator; leave afterColon set so the caret
        // right after the value still completes values (harmless)
      }
      continue;
    }
    switch (t.kind) {
      case "punct":
        if (t.value === "{") {
          const parent = frames.length === 0 ? ROOT : frames[frames.length - 1]!;
          let ty: TypeContext | undefined;
          if (frames.length === 0) ty = ROOT;
          else if (pendingOn) ty = { kind: "entity", name: pendingOn };
          else if (pendingField) ty = childType(schema, parent, pendingField);
          frames.push(ty ?? ROOT);
          pendingField = undefined;
          pendingOn = undefined;
        } else if (t.value === "}") {
          frames.pop();
        } else if (t.value === "(") {
          parenDepth = 1;
          parenOwner = pendingField;
          lastArgName = undefined;
          afterColon = false;
        }
        afterSpread = false;
        afterOn = false;
        break;
      case "spread":
        afterSpread = true;
        afterOn = false;
        break;
      case "name":
        if (afterOn) {
          pendingOn = t.value;
          afterOn = false;
          afterSpread = false;
        } else if (afterSpread && t.value === "on") {
          afterOn = true;
        } else if (afterSpread) {
          // fragment spread name; not an owner of a following `{`
          afterSpread = false;
        } else {
          pendingField = t.value;
        }
        break;
      default:
        afterSpread = false;
        afterOn = false;
    }
  }

  const currentType = frames.length === 0 ? ROOT : frames[frames.length - 1]!;

  if (parenDepth > 0) {
    if (afterColon) {
      return {
        scope: "argValue",
        type: currentType,
        ...(parenOwner !== undefined ? { ownerField: parenOwner } : {}),
        ...(lastArgName !== undefined ? { argName: lastArgName } : {}),
        prefix,
        prefixKind,
      };
    }
    return {
      scope: "args",
      type: currentType,
      ...(parenOwner !== undefined ? { ownerField: parenOwner } : {}),
      prefix,
      prefixKind,
    };
  }
  if (afterOn) return { scope: "onType", type: currentType, prefix, prefixKind };
  if (afterSpread) return { scope: "spread", type: currentType, prefix, prefixKind };
  if (frames.length === 0) return { scope: "root", type: ROOT, prefix, prefixKind };
  return { scope: "selection", type: currentType, prefix, prefixKind };
}

// ---------------------------------------------------------------------------
// Completion

function fieldItem(f: FieldModel): CompletionItem {
  const argSig = f.args.length ? `(${f.args.map((a) => `${a.name}: ${a.type}`).join(", ")})` : "";
  return {
    label: f.name,
    kind: "field",
    detail: `${argSig}: ${f.type}`,
    ...(f.derived || f.policy ? { doc: [f.derived, f.policy].filter(Boolean).join(" · ") } : {}),
  };
}

function edgeItem(e: EdgeModel): CompletionItem {
  const opt = e.kind === "one" && e.optional ? "?" : "";
  const paged = e.kind === "many" ? " (paginated)" : "";
  return {
    label: e.name,
    kind: "edge",
    detail: `has ${e.kind}${opt}: ${targetLabel(e.target)}${paged}`,
    insertText: e.name + " { }",
  };
}

/** All completion items appropriate at `offset`. */
export function completeQuery(text: string, offset: number, schema: SchemaModel): CompletionItem[] {
  const ctx = analyzeQueryContext(text, offset, schema);
  const items = itemsForContext(ctx, schema);
  // In a value position (or when the caret follows `$`), declared variables
  // are candidates — the caret prefix already carries the `$`, so the label
  // is the bare name.
  if (ctx.scope === "argValue" || ctx.prefixKind === "var") {
    for (const [name, type] of declaredVars(lexQuery(text))) {
      items.push({ label: "$" + name, kind: "variable", detail: type || "variable", insertText: "$" + name });
    }
  }
  const p = (ctx.prefixKind === "var" ? "$" + ctx.prefix : ctx.prefix).toLowerCase();
  const filtered = p ? items.filter((i) => i.label.toLowerCase().startsWith(p)) : items;
  return filtered.sort((a, b) => (a.label < b.label ? -1 : a.label > b.label ? 1 : 0));
}

function itemsForContext(ctx: QueryContext, schema: SchemaModel): CompletionItem[] {
  switch (ctx.scope) {
    case "root": {
      const out: CompletionItem[] = [];
      for (const r of schema.roots.values()) {
        const argSig = r.args.length ? `(${r.args.map((a) => `${a.name}: ${a.type}`).join(", ")})` : "";
        out.push({
          label: r.name,
          kind: "root",
          detail: `${r.kind}${argSig} of ${targetLabel(r.target)}`,
          insertText: r.name + " { }",
        });
      }
      return out;
    }
    case "selection":
      return selectionItems(ctx.type, schema);
    case "spread": {
      // Schema fragments applicable to the current type + `on`.
      const out: CompletionItem[] = [{ label: "on", kind: "keyword", detail: "inline fragment: ... on Type" }];
      const names = contextTypeNames(ctx.type, schema);
      for (const frag of schema.fragments.values()) {
        if (names.includes(frag.on)) {
          out.push({ label: frag.name, kind: "fragment", detail: `fragment on ${frag.on}` });
        }
      }
      return out;
    }
    case "onType": {
      // Concrete member types of the current interface/union.
      const out: CompletionItem[] = [];
      if (ctx.type.kind !== "root") {
        for (const t of targetTypes(schema, ctx.type)) out.push({ label: t, kind: "type", detail: "type" });
      }
      return out;
    }
    case "args": {
      const args = ctx.ownerField ? argsFor(schema, ctx.type, ctx.ownerField) : [];
      return args.map((a) => ({
        label: a.name,
        kind: "arg",
        detail: a.type + (a.default !== undefined ? ` = ${a.default}` : ""),
        insertText: a.name + ": ",
      }));
    }
    case "argValue": {
      const args = ctx.ownerField ? argsFor(schema, ctx.type, ctx.ownerField) : [];
      const arg = args.find((a) => a.name === ctx.argName);
      const out: CompletionItem[] = [];
      if (arg) {
        const base = arg.type.replace(/[?[\]+]/g, "");
        const en = schema.enums.get(base);
        if (en) for (const v of en.values) out.push({ label: v, kind: "enum", detail: `${base}` });
        if (base === "Bool" || base === "Boolean") {
          out.push({ label: "true", kind: "keyword" }, { label: "false", kind: "keyword" });
        }
      }
      return out;
    }
  }
}

function selectionItems(type: TypeContext, schema: SchemaModel): CompletionItem[] {
  if (type.kind === "root") {
    return itemsForContext({ scope: "root", type, prefix: "", prefixKind: "word" }, schema);
  }
  const { fields, edges, isInterface, isUnion } = membersOf(schema, type);
  const out: CompletionItem[] = [];
  for (const f of fields) out.push(fieldItem(f));
  for (const e of edges) out.push(edgeItem(e));
  if (isUnion && type.kind === "union") {
    // A union has no declared common fields, but the origin accepts fields
    // shared by every member (e.g. `hero { name }` over `(Human | Droid)`),
    // so offer that intersection plus the `... on` escape for member-specific
    // fields.
    const members = type.members.map((m) => entityMembers(schema, m));
    if (members.length > 0) {
      const first = members[0]!;
      for (const f of first.fields) {
        if (members.every((m) => m.fields.some((x) => x.name === f.name))) out.push(fieldItem(f));
      }
      for (const e of first.edges) {
        if (members.every((m) => m.edges.some((x) => x.name === e.name))) out.push(edgeItem(e));
      }
    }
  }
  if (isInterface || isUnion) {
    out.push({ label: "... on", kind: "keyword", detail: "inline fragment for a concrete type", insertText: "... on " });
  }
  // Applicable schema fragments.
  const names = contextTypeNames(type, schema);
  for (const frag of schema.fragments.values()) {
    if (names.includes(frag.on)) out.push({ label: "..." + frag.name, kind: "fragment", detail: `fragment on ${frag.on}` });
  }
  return out;
}

/** The set of type/interface names a fragment `on <name>` may target at this context. */
function contextTypeNames(type: TypeContext, schema: SchemaModel): string[] {
  if (type.kind === "root") return [];
  if (type.kind === "interface") return [type.name, ...targetTypes(schema, type)];
  if (type.kind === "union") return targetTypes(schema, type);
  // concrete entity: its own name plus any interfaces it implements
  const e = schema.entities.get(type.name);
  return [type.name, ...(e?.implements ?? [])];
}

// ---------------------------------------------------------------------------
// Validation

export type Severity = "error" | "warning";

export interface Diagnostic {
  readonly message: string;
  readonly severity: Severity;
  /** Character offset of the problem, when known. */
  readonly offset?: number;
  readonly line?: number;
  readonly column?: number;
}

function offsetOfLineCol(text: string, line: number, column: number): number {
  let off = 0;
  let ln = 1;
  while (ln < line && off < text.length) {
    if (text[off] === "\n") ln++;
    off++;
  }
  return off + Math.max(0, column - 1);
}

/**
 * Grammar errors (from the authoritative `parse`) plus a conservative
 * schema pass that flags unknown roots and unknown fields on known concrete
 * or interface contexts. Schema findings are warnings: the origin is the
 * authority, and this pass deliberately stays silent on anything ambiguous
 * (union contexts, unresolved fragments) to avoid false positives.
 */
export function lintQuery(text: string, schema: SchemaModel): Diagnostic[] {
  if (text.trim() === "") return [];
  try {
    parse(text);
  } catch (e) {
    if (e instanceof LatticeQueryError) {
      const d: Diagnostic = {
        message: e.message,
        severity: "error",
        ...(e.line !== undefined ? { line: e.line } : {}),
        ...(e.column !== undefined ? { column: e.column } : {}),
        ...(e.line !== undefined && e.column !== undefined
          ? { offset: offsetOfLineCol(text, e.line, e.column) }
          : {}),
      };
      return [d];
    }
    return [{ message: String((e as Error).message ?? e), severity: "error" }];
  }
  return schemaWarnings(text, schema);
}

/** A tolerant structural walk flagging unknown roots/fields (warnings only). */
function schemaWarnings(text: string, schema: SchemaModel): Diagnostic[] {
  if (schema.roots.size === 0) return []; // no schema loaded → nothing to check
  const toks = lexQuery(text);
  const out: Diagnostic[] = [];
  const frames: TypeContext[] = [];
  let pendingField: QTok | undefined;
  let pendingOn: string | undefined;
  let afterSpread = false;
  let afterOn = false;
  let parenDepth = 0;
  for (let i = 0; i < toks.length; i++) {
    const t = toks[i]!;
    if (parenDepth > 0) {
      if (t.kind === "punct" && t.value === "(") parenDepth++;
      else if (t.kind === "punct" && t.value === ")") parenDepth--;
      continue;
    }
    if (t.kind === "punct" && t.value === "(") {
      parenDepth = 1;
      continue;
    }
    if (t.kind === "punct" && t.value === "{") {
      const parent = frames.length === 0 ? ROOT : frames[frames.length - 1]!;
      let ty: TypeContext | undefined;
      if (frames.length === 0) ty = ROOT;
      else if (pendingOn) ty = { kind: "entity", name: pendingOn };
      else if (pendingField) ty = childType(schema, parent, pendingField.value);
      frames.push(ty ?? ROOT);
      pendingField = undefined;
      pendingOn = undefined;
      continue;
    }
    if (t.kind === "punct" && t.value === "}") {
      frames.pop();
      continue;
    }
    if (t.kind === "spread") {
      afterSpread = true;
      continue;
    }
    if (t.kind === "name") {
      if (afterOn) {
        pendingOn = t.value;
        afterOn = false;
        afterSpread = false;
        continue;
      }
      if (afterSpread && t.value === "on") {
        afterOn = true;
        continue;
      }
      if (afterSpread) {
        afterSpread = false;
        continue;
      }
      pendingField = t;
      // Validate the name against the current frame's type.
      const ctx = frames.length === 0 ? undefined : frames[frames.length - 1]!;
      if (ctx === undefined) continue; // header (query name / vars)
      if (ctx.kind === "root") {
        if (!schema.roots.has(t.value)) {
          out.push({ message: `unknown root "${t.value}"`, severity: "warning", offset: t.start });
        }
      } else if (ctx.kind === "entity" || ctx.kind === "interface") {
        const { fields, edges } =
          ctx.kind === "entity" ? entityMembers(schema, ctx.name) : membersOf(schema, ctx);
        const known = fields.some((f) => f.name === t.value) || edges.some((e) => e.name === t.value);
        // Only flag when we positively know the type and the member is absent.
        const typeKnown = ctx.kind === "entity" ? schema.entities.has(ctx.name) : schema.interfaces.has(ctx.name);
        if (typeKnown && !known) {
          out.push({
            message: `unknown field "${t.value}" on ${ctx.name}`,
            severity: "warning",
            offset: t.start,
          });
        }
      }
      continue;
    }
  }
  return out;
}
