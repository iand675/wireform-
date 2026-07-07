/**
 * A tolerant, browser-side parser for the Lattice IDL (spec §3.4) into a
 * *tooling* schema model — the model the explorer walks to drive schema
 * documentation, autocompletion, and hover.
 *
 * This is deliberately NOT the authoritative parser. The origin owns the
 * semantic model, canonicalization, and hashing (`Lattice.IDL.Parser`); the
 * explorer never computes any of those, exactly as `@wireform/lattice` never
 * computes query hashes. What this parser needs to do is recover the
 * *declaration surface* — types, entities, fields, edges, roots, mutations —
 * from the canonical IDL document an origin serves at `GET /schema/{hash}`,
 * well enough to power completion and docs. It is intentionally lenient: an
 * unrecognized clause is skipped rather than fatal, so a half-typed schema in
 * the IDL authoring pane still yields a usable model. Authoritative validation
 * routes to the origin (`POST /schema/check`, and query introduction).
 *
 * It parses both the multi-line surface form (as written in `.lattice`
 * fixtures) and the single-line canonical form (as served by an origin),
 * because it never relies on line terminators as separators — clauses are
 * delimited structurally by the grammar's keyword set.
 */

// ---------------------------------------------------------------------------
// Tooling model

/** The target of a root or edge: a concrete entity, an interface, or a union. */
export type Target =
  | { readonly kind: "entity"; readonly name: string }
  | { readonly kind: "interface"; readonly name: string }
  | { readonly kind: "union"; readonly members: readonly string[] };

/** A declared argument (`name: Type = default`). */
export interface ArgModel {
  readonly name: string;
  readonly type: string;
  readonly default?: string;
}

/** A scalar/value field on an entity or interface. */
export interface FieldModel {
  readonly name: string;
  readonly type: string;
  readonly args: readonly ArgModel[];
  /** The read-set + materialization text of a `derived` field (§3.7), if any. */
  readonly derived?: string;
  /** A per-field visibility clause (`private`, `visible when …`), if any. */
  readonly policy?: string;
}

/** A `has one`/`has many` relationship. */
export interface EdgeModel {
  readonly name: string;
  readonly kind: "one" | "many";
  /** `has one?` — zero-or-one (§3.4). */
  readonly optional: boolean;
  readonly target: Target;
  /** The key-holding field (`by …`). */
  readonly by?: string;
  /** The `ordered by …` clause of a paginated collection. */
  readonly ordered?: string;
  readonly page?: number;
  readonly max?: number;
  readonly policy?: string;
}

export interface EntityModel {
  readonly name: string;
  readonly key: readonly string[];
  readonly implements: readonly string[];
  readonly defaultPolicy?: string;
  readonly fields: readonly FieldModel[];
  readonly edges: readonly EdgeModel[];
  readonly fetchBy?: { readonly key: string; readonly policy: string };
}

export interface InterfaceModel {
  readonly name: string;
  readonly fields: readonly FieldModel[];
  readonly edges: readonly EdgeModel[];
  /** Concrete entity types that `implements` this interface (derived). */
  readonly members: readonly string[];
}

export interface EnumModel {
  readonly name: string;
  readonly open: boolean;
  readonly values: readonly string[];
}

export interface RecordModel {
  readonly name: string;
  readonly fields: readonly ArgModel[];
}

export interface FragmentModel {
  readonly name: string;
  readonly on: string;
  readonly args: readonly ArgModel[];
  /** The raw selection-set text between the braces. */
  readonly selection: string;
}

export interface RootModel {
  readonly name: string;
  readonly kind: "get" | "list";
  readonly args: readonly ArgModel[];
  readonly target: Target;
  readonly by?: string;
  readonly ordered?: string;
  readonly page?: number;
  readonly max?: number;
  readonly policy?: string;
}

export interface MutationModel {
  readonly name: string;
  readonly args: readonly ArgModel[];
  readonly returns?: string;
  readonly allow?: string;
  readonly writes?: string;
  readonly invalidates?: string;
  readonly effect?: string;
  /** `as VERB /e/…` verb bindings (§11.7). */
  readonly bindings: readonly string[];
  readonly batch?: string;
}

export interface SchemaDiagnostic {
  readonly message: string;
  readonly offset: number;
}

export interface SchemaModel {
  readonly name: string;
  readonly newtypes: ReadonlyMap<string, string>;
  readonly enums: ReadonlyMap<string, EnumModel>;
  readonly records: ReadonlyMap<string, RecordModel>;
  readonly claims: readonly ArgModel[];
  readonly interfaces: ReadonlyMap<string, InterfaceModel>;
  readonly entities: ReadonlyMap<string, EntityModel>;
  readonly fragments: ReadonlyMap<string, FragmentModel>;
  readonly roots: ReadonlyMap<string, RootModel>;
  readonly mutations: ReadonlyMap<string, MutationModel>;
  readonly diagnostics: readonly SchemaDiagnostic[];
}

// ---------------------------------------------------------------------------
// Lexer

type TokKind = "name" | "punct" | "str" | "num";

interface Tok {
  readonly kind: TokKind;
  readonly value: string;
  readonly start: number;
}

const NAME_START = /[A-Za-z_]/;
const NAME_CHAR = /[A-Za-z0-9_.]/;
const DIGIT = /[0-9]/;

/** Lex IDL text into a token stream, discarding `--`-to-EOL comments and whitespace. */
function lex(text: string): Tok[] {
  const toks: Tok[] = [];
  let i = 0;
  const n = text.length;
  while (i < n) {
    const c = text[i]!;
    if (c === "-" && text[i + 1] === "-") {
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
      let v = "";
      while (i < n && text[i] !== '"') {
        if (text[i] === "\\" && i + 1 < n) {
          v += text[i + 1];
          i += 2;
          continue;
        }
        v += text[i];
        i++;
      }
      i++; // closing quote
      toks.push({ kind: "str", value: v, start });
      continue;
    }
    if (NAME_START.test(c)) {
      const start = i;
      i++;
      while (i < n && NAME_CHAR.test(text[i]!)) i++;
      toks.push({ kind: "name", value: text.slice(start, i), start });
      continue;
    }
    if (DIGIT.test(c)) {
      const start = i;
      i++;
      while (i < n && DIGIT.test(text[i]!)) i++;
      toks.push({ kind: "num", value: text.slice(start, i), start });
      continue;
    }
    // single-char punctuation
    toks.push({ kind: "punct", value: c, start: i });
    i++;
  }
  return toks;
}

// ---------------------------------------------------------------------------
// Parser

/** Keywords that begin a top-level declaration; also used to bound greedy clauses. */
const TOP_KEYWORDS: Record<string, true> = {
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
};

const POLICY_START: Record<string, true> = { public: true, private: true, visible: true };

class Parser {
  private pos = 0;
  private readonly diagnostics: SchemaDiagnostic[] = [];

  private name = "";
  private readonly newtypes = new Map<string, string>();
  private readonly enums = new Map<string, EnumModel>();
  private readonly records = new Map<string, RecordModel>();
  private claims: ArgModel[] = [];
  private readonly interfaces = new Map<string, InterfaceModel>();
  private readonly entities = new Map<string, EntityModel>();
  private readonly fragments = new Map<string, FragmentModel>();
  private readonly roots = new Map<string, RootModel>();
  private readonly mutations = new Map<string, MutationModel>();
  /** Deferred targets to resolve to interface/entity once all decls are seen. */
  private readonly pendingRefs: Array<{ target: { kind: "entity" | "interface"; name: string } }> = [];

  constructor(private readonly toks: Tok[], private readonly text: string) {}

  private peek(ahead = 0): Tok | undefined {
    return this.toks[this.pos + ahead];
  }

  private next(): Tok | undefined {
    return this.toks[this.pos++];
  }

  private isName(v: string, ahead = 0): boolean {
    const t = this.peek(ahead);
    return t?.kind === "name" && t.value === v;
  }

  private isPunct(v: string, ahead = 0): boolean {
    const t = this.peek(ahead);
    return t?.kind === "punct" && t.value === v;
  }

  private expectPunct(v: string): boolean {
    if (this.isPunct(v)) {
      this.pos++;
      return true;
    }
    return false;
  }

  private diag(message: string): void {
    this.diagnostics.push({ message, offset: this.peek()?.start ?? this.text.length });
  }

  parse(): SchemaModel {
    while (this.pos < this.toks.length) {
      const t = this.peek()!;
      if (t.kind !== "name") {
        this.pos++;
        continue;
      }
      switch (t.value) {
        case "schema":
          this.pos++;
          this.name = this.readName();
          break;
        case "newtype":
          this.parseNewtype();
          break;
        case "enum":
          this.parseEnum();
          break;
        case "data":
          this.parseRecord();
          break;
        case "claims":
          this.parseClaims();
          break;
        case "interface":
          this.parseInterface();
          break;
        case "entity":
          this.parseEntity(false);
          break;
        case "extend":
          this.parseExtend();
          break;
        case "fragment":
          this.parseFragment();
          break;
        case "get":
          this.parseRoot("get");
          break;
        case "list":
          this.parseRoot("list");
          break;
        case "mutation":
          this.parseMutation();
          break;
        default:
          this.pos++;
      }
    }
    this.resolveTargets();
    this.deriveInterfaceMembers();
    return {
      name: this.name,
      newtypes: this.newtypes,
      enums: this.enums,
      records: this.records,
      claims: this.claims,
      interfaces: this.interfaces,
      entities: this.entities,
      fragments: this.fragments,
      roots: this.roots,
      mutations: this.mutations,
      diagnostics: this.diagnostics,
    };
  }

  private readName(): string {
    const t = this.peek();
    if (t?.kind === "name") {
      this.pos++;
      return t.value;
    }
    return "";
  }

  /** Consume exactly one type expression, returning its source text. */
  private parseType(): string {
    const t = this.peek();
    if (!t) return "";
    if (t.kind === "punct" && t.value === "[") {
      this.pos++;
      const inner = this.parseType();
      this.expectPunct("]");
      let s = "[" + inner + "]";
      if (this.isPunct("?")) {
        this.pos++;
        s += "?";
      } else if (this.isPunct("+")) {
        this.pos++;
        s += "+";
      }
      return s;
    }
    if (t.kind === "punct" && t.value === "(") {
      // inline union used in type position (rare); keep raw
      this.pos++;
      const parts: string[] = [];
      while (this.peek() && !this.isPunct(")")) {
        if (this.isPunct("|")) {
          this.pos++;
          continue;
        }
        parts.push(this.readName());
      }
      this.expectPunct(")");
      return "(" + parts.join(" | ") + ")";
    }
    if (t.kind === "name") {
      this.pos++;
      let s = t.value;
      if (this.isPunct("?")) {
        this.pos++;
        s += "?";
      } else if (this.isPunct("+")) {
        this.pos++;
        s += "+";
      }
      return s;
    }
    return "";
  }

  private parseNewtype(): void {
    this.pos++; // newtype
    const name = this.readName();
    if (!this.expectPunct("=")) {
      this.diag(`newtype ${name}: expected '='`);
      return;
    }
    this.newtypes.set(name, this.parseType());
  }

  private parseEnum(): void {
    this.pos++; // enum
    const name = this.readName();
    let open = false;
    if (this.isName("open")) {
      open = true;
      this.pos++;
    } else if (this.isName("closed")) {
      this.pos++;
    }
    this.expectPunct("=");
    const values: string[] = [];
    // First value, then repeat while a `|` separator is present.
    if (this.peek()?.kind === "name") values.push(this.readName());
    while (this.isPunct("|")) {
      this.pos++;
      if (this.peek()?.kind === "name") values.push(this.readName());
    }
    this.enums.set(name, { name, open, values });
  }

  private parseRecord(): void {
    this.pos++; // data
    const name = this.readName();
    if (!this.expectPunct("{")) {
      this.diag(`data ${name}: expected '{'`);
      return;
    }
    const fields = this.parseArgLikeUntilBrace();
    this.records.set(name, { name, fields });
  }

  private parseClaims(): void {
    this.pos++; // claims
    if (!this.expectPunct("{")) return;
    this.claims = this.parseArgLikeUntilBrace();
  }

  /** `name: Type [= default]` entries until the matching `}`. */
  private parseArgLikeUntilBrace(): ArgModel[] {
    const out: ArgModel[] = [];
    while (this.peek() && !this.isPunct("}")) {
      if (this.peek()!.kind !== "name") {
        this.pos++;
        continue;
      }
      const fieldName = this.readName();
      if (!this.expectPunct(":")) {
        this.diag(`field ${fieldName}: expected ':'`);
        continue;
      }
      const type = this.parseType();
      let def: string | undefined;
      if (this.isPunct("=")) {
        this.pos++;
        def = this.readValueText();
      }
      out.push(def !== undefined ? { name: fieldName, type, default: def } : { name: fieldName, type });
    }
    this.expectPunct("}");
    return out;
  }

  /** `(name: Type [= default], …)` argument list. */
  private parseArgList(): ArgModel[] {
    const out: ArgModel[] = [];
    if (!this.isPunct("(")) return out;
    this.pos++;
    while (this.peek() && !this.isPunct(")")) {
      if (this.peek()!.kind !== "name") {
        this.pos++;
        continue;
      }
      const argName = this.readName();
      if (!this.expectPunct(":")) continue;
      const type = this.parseType();
      let def: string | undefined;
      if (this.isPunct("=")) {
        this.pos++;
        def = this.readValueText();
      }
      out.push(def !== undefined ? { name: argName, type, default: def } : { name: argName, type });
    }
    this.expectPunct(")");
    return out;
  }

  /** Read a single value token's text (number, string, enum ident, or `[…]`). */
  private readValueText(): string {
    const t = this.peek();
    if (!t) return "";
    if (t.kind === "punct" && t.value === "[") {
      // list literal: read until matching ]
      let depth = 0;
      const start = t.start;
      let end = start;
      while (this.peek()) {
        const cur = this.next()!;
        end = cur.start + cur.value.length + (cur.kind === "str" ? 2 : 0);
        if (cur.kind === "punct" && cur.value === "[") depth++;
        if (cur.kind === "punct" && cur.value === "]") {
          depth--;
          if (depth === 0) break;
        }
      }
      return this.text.slice(start, end).replace(/\s+/g, " ");
    }
    this.pos++;
    return t.kind === "str" ? `"${t.value}"` : t.value;
  }

  private parseTarget(): Target {
    if (this.isPunct("(")) {
      this.pos++;
      const members: string[] = [];
      while (this.peek() && !this.isPunct(")")) {
        if (this.isPunct("|")) {
          this.pos++;
          continue;
        }
        if (this.peek()!.kind === "name") members.push(this.readName());
        else this.pos++;
      }
      this.expectPunct(")");
      return { kind: "union", members };
    }
    const name = this.readName();
    // Resolve later; assume entity provisionally.
    const t = { kind: "entity" as const, name };
    this.pendingRefs.push({ target: t });
    return t;
  }

  private parseInterface(): void {
    this.pos++; // interface
    const name = this.readName();
    if (!this.expectPunct("{")) return;
    const { fields, edges } = this.parseBody();
    this.interfaces.set(name, { name, fields, edges, members: [] });
  }

  private parseEntity(isExtend: boolean): void {
    this.pos++; // entity
    const name = this.readName();
    const key: string[] = [];
    const impls: string[] = [];
    if (this.isName("by")) {
      this.pos++;
      // key fields: one or more names (comma-separated in surface, ws in canon)
      while (this.peek()?.kind === "name" && !this.isName("implements") && !this.isPunct("{")) {
        key.push(this.readName());
      }
    }
    if (this.isName("implements")) {
      this.pos++;
      while (this.peek()?.kind === "name" && !this.isPunct("{")) {
        impls.push(this.readName());
      }
    }
    if (!this.expectPunct("{")) return;
    const body = this.parseBody();
    const existing = isExtend ? this.entities.get(name) : undefined;
    const merged: EntityModel = existing
      ? {
          ...existing,
          fields: [...existing.fields, ...body.fields],
          edges: [...existing.edges, ...body.edges],
        }
      : {
          name,
          key,
          implements: impls,
          ...(body.defaultPolicy !== undefined ? { defaultPolicy: body.defaultPolicy } : {}),
          fields: body.fields,
          edges: body.edges,
          ...(body.fetchBy ? { fetchBy: body.fetchBy } : {}),
        };
    this.entities.set(name, merged);
  }

  private parseExtend(): void {
    this.pos++; // extend
    if (this.isName("entity")) {
      // reuse entity parsing; merge into existing declaration if present
      this.parseEntity(true);
    } else {
      // unknown extend form; skip to next top-level keyword
      this.skipToTopLevel();
    }
  }

  /** Parse an entity/interface body until the matching `}`. */
  private parseBody(): {
    fields: FieldModel[];
    edges: EdgeModel[];
    defaultPolicy?: string;
    fetchBy?: { key: string; policy: string };
  } {
    const fields: FieldModel[] = [];
    const edges: EdgeModel[] = [];
    let defaultPolicy: string | undefined;
    let fetchBy: { key: string; policy: string } | undefined;
    while (this.peek() && !this.isPunct("}")) {
      const t = this.peek()!;
      if (t.kind !== "name") {
        this.pos++;
        continue;
      }
      if (t.value === "has") {
        const e = this.parseEdge();
        if (e) edges.push(e);
        continue;
      }
      if (t.value === "fetch") {
        fetchBy = this.parseFetchBy() ?? fetchBy;
        continue;
      }
      if (t.value === "visible" || t.value === "private") {
        // default-policy line: `visible to all by default` / `private by default`
        defaultPolicy = this.parseDefaultPolicy();
        continue;
      }
      // field: `name [(args)] : type [derived …] [policy]`
      const f = this.parseField();
      if (f) fields.push(f);
      else this.pos++;
    }
    this.expectPunct("}");
    return {
      fields,
      edges,
      ...(defaultPolicy !== undefined ? { defaultPolicy } : {}),
      ...(fetchBy ? { fetchBy } : {}),
    };
  }

  private parseDefaultPolicy(): string {
    const start = this.peek()!.start;
    // consume until `default` (inclusive) or a member boundary
    while (this.peek() && !this.isPunct("}")) {
      const t = this.peek()!;
      const isMemberStart = this.atMemberStart();
      if (isMemberStart && t.start !== start) break;
      this.pos++;
      if (t.kind === "name" && t.value === "default") break;
    }
    return this.sliceText(start, this.peek()?.start ?? this.text.length).trim();
  }

  /** Is the cursor at the start of a new body member? */
  private atMemberStart(): boolean {
    const t = this.peek();
    if (!t || t.kind !== "name") return false;
    if (t.value === "has" || t.value === "fetch") return true;
    // `name :` or `name (` starts a field
    const nx = this.peek(1);
    return nx?.kind === "punct" && (nx.value === ":" || nx.value === "(");
  }

  private parseField(): FieldModel | undefined {
    const name = this.readName();
    const args = this.isPunct("(") ? this.parseArgList() : [];
    if (!this.expectPunct(":")) {
      this.diag(`field ${name}: expected ':'`);
      return undefined;
    }
    const type = this.parseType();
    let derived: string | undefined;
    if (this.isName("derived")) derived = this.parseDerived();
    let policy: string | undefined;
    if (this.peek()?.kind === "name" && POLICY_START[this.peek()!.value]) {
      policy = this.parsePolicyClause();
    }
    return {
      name,
      type,
      args,
      ...(derived !== undefined ? { derived } : {}),
      ...(policy !== undefined ? { policy } : {}),
    };
  }

  /** `derived reads <read-set> (on read | maintained)`. */
  private parseDerived(): string {
    const start = this.peek()!.start;
    this.pos++; // derived
    // Consume the read set + materialization. The read set may contain
    // aggregate calls (`sum(stars)`) that look like field starts, so we bound
    // only on the materialization terminator (`maintained` / `on read`), a
    // trailing policy keyword, or the body's closing brace.
    while (this.peek() && !this.isPunct("}")) {
      if (this.peek()!.kind === "name" && POLICY_START[this.peek()!.value]) break;
      const t = this.next()!;
      if (t.kind === "name" && t.value === "maintained") break;
      if (t.kind === "name" && t.value === "read" && this.toks[this.pos - 2]?.value === "on") break;
    }
    return this.sliceText(start, this.peek()?.start ?? this.text.length).trim();
  }

  private parsePolicyClause(): string {
    const start = this.peek()!.start;
    const kw = this.readName(); // public | private | visible
    if (kw === "public" || kw === "private") return kw;
    // visible …
    if (this.isName("to")) {
      this.pos++;
      if (this.isName("all")) this.pos++;
      return this.sliceText(start, this.peek()?.start ?? this.text.length).trim();
    }
    if (this.isName("when")) {
      this.pos++;
      // consume expression greedily until a member/top-level boundary
      while (this.peek() && !this.isPunct("}")) {
        if (this.atMemberStart()) break;
        if (this.peek()!.kind === "name" && TOP_KEYWORDS[this.peek()!.value]) break;
        this.pos++;
      }
      return this.sliceText(start, this.peek()?.start ?? this.text.length).trim();
    }
    return kw;
  }

  private parseFetchBy(): { key: string; policy: string } | undefined {
    this.pos++; // fetch
    if (!this.isName("by")) {
      this.diag("fetch: expected 'by'");
      return undefined;
    }
    this.pos++;
    const key = this.readName();
    let policy = "";
    if (this.expectPunct(":")) {
      if (this.peek()?.kind === "name" && POLICY_START[this.peek()!.value]) {
        policy = this.parsePolicyClause();
      } else {
        policy = this.readName();
      }
    }
    return { key, policy };
  }

  private parseEdge(): EdgeModel | undefined {
    this.pos++; // has
    let kind: "one" | "many" = "one";
    if (this.isName("many")) {
      kind = "many";
      this.pos++;
    } else if (this.isName("one")) {
      this.pos++;
    }
    let optional = false;
    if (this.isPunct("?")) {
      optional = true;
      this.pos++;
    }
    const name = this.readName();
    if (!this.expectPunct(":")) {
      this.diag(`has ${kind} ${name}: expected ':'`);
      return undefined;
    }
    const target = this.parseTarget();
    let by: string | undefined;
    let ordered: string | undefined;
    let page: number | undefined;
    let max: number | undefined;
    let policy: string | undefined;
    // greedily consume collection clauses
    for (;;) {
      if (this.isName("by")) {
        this.pos++;
        by = this.readName();
      } else if (this.isName("ordered")) {
        const start = this.peek()!.start;
        this.pos++;
        if (this.isName("by")) this.pos++;
        this.readName(); // field
        if (this.isName("asc") || this.isName("desc")) this.pos++;
        ordered = this.sliceText(start, this.peek()?.start ?? this.text.length).trim();
      } else if (this.isName("page")) {
        this.pos++;
        page = this.readNumber();
      } else if (this.isName("max")) {
        this.pos++;
        max = this.readNumber();
      } else if (this.peek()?.kind === "name" && POLICY_START[this.peek()!.value]) {
        policy = this.parsePolicyClause();
      } else {
        break;
      }
    }
    return {
      name,
      kind,
      optional,
      target,
      ...(by !== undefined ? { by } : {}),
      ...(ordered !== undefined ? { ordered } : {}),
      ...(page !== undefined ? { page } : {}),
      ...(max !== undefined ? { max } : {}),
      ...(policy !== undefined ? { policy } : {}),
    };
  }

  private readNumber(): number | undefined {
    const t = this.peek();
    if (t?.kind === "num") {
      this.pos++;
      return Number(t.value);
    }
    return undefined;
  }

  private parseFragment(): void {
    this.pos++; // fragment
    const name = this.readName();
    const args = this.isPunct("(") ? this.parseArgList() : [];
    if (this.isName("on")) this.pos++;
    const on = this.readName();
    if (!this.isPunct("{")) {
      this.diag(`fragment ${name}: expected '{'`);
      return;
    }
    const { text: selection } = this.captureBraces();
    this.fragments.set(name, { name, on, args, selection });
  }

  private parseRoot(kind: "get" | "list"): void {
    this.pos++; // get | list
    const name = this.readName();
    const args = this.isPunct("(") ? this.parseArgList() : [];
    if (this.isName("of")) this.pos++;
    const target = this.parseTarget();
    let by: string | undefined;
    let ordered: string | undefined;
    let page: number | undefined;
    let max: number | undefined;
    let policy: string | undefined;
    for (;;) {
      if (this.peek() === undefined) break;
      if (this.peek()!.kind === "name" && TOP_KEYWORDS[this.peek()!.value]) break;
      if (this.isName("by")) {
        this.pos++;
        by = this.readName();
      } else if (this.isName("ordered")) {
        const start = this.peek()!.start;
        this.pos++;
        if (this.isName("by")) this.pos++;
        this.readName();
        if (this.isName("asc") || this.isName("desc")) this.pos++;
        ordered = this.sliceText(start, this.peek()?.start ?? this.text.length).trim();
      } else if (this.isName("page")) {
        this.pos++;
        page = this.readNumber();
      } else if (this.isName("max")) {
        this.pos++;
        max = this.readNumber();
      } else if (this.peek()!.kind === "name" && POLICY_START[this.peek()!.value]) {
        policy = this.parsePolicyClause();
      } else {
        break;
      }
    }
    this.roots.set(name, {
      name,
      kind,
      args,
      target,
      ...(by !== undefined ? { by } : {}),
      ...(ordered !== undefined ? { ordered } : {}),
      ...(page !== undefined ? { page } : {}),
      ...(max !== undefined ? { max } : {}),
      ...(policy !== undefined ? { policy } : {}),
    });
  }

  private parseMutation(): void {
    this.pos++; // mutation
    const name = this.readName();
    const args = this.isPunct("(") ? this.parseArgList() : [];
    let returns: string | undefined;
    if (this.isName("returns")) {
      this.pos++;
      returns = this.parseType();
    }
    let allow: string | undefined;
    let writes: string | undefined;
    let invalidates: string | undefined;
    let effect: string | undefined;
    const bindings: string[] = [];
    let batch: string | undefined;
    if (this.expectPunct("{")) {
      let depth = 1;
      while (this.peek() && depth > 0) {
        const t = this.peek()!;
        if (t.kind === "punct" && t.value === "{") {
          depth++;
          this.pos++;
          continue;
        }
        if (t.kind === "punct" && t.value === "}") {
          depth--;
          this.pos++;
          continue;
        }
        if (t.kind === "name") {
          switch (t.value) {
            case "allow":
              // Each clause stops only at clauses that follow it in canonical
              // order (allow < writes < invalidates < effect < as < batch), so
              // a value token equal to an earlier keyword — notably the `writes`
              // in `invalidates writes` — is not re-dispatched as a clause.
              allow = this.captureClause(["writes", "invalidates", "effect", "as", "batch"]);
              continue;
            case "writes":
              writes = this.captureClause(["invalidates", "effect", "as", "batch"]);
              continue;
            case "invalidates":
              invalidates = this.captureClause(["effect", "as", "batch"]);
              continue;
            case "effect":
              effect = this.captureClause(["as", "batch"]);
              continue;
            case "as":
              bindings.push(this.captureClause(["as", "batch"]));
              continue;
            case "batch":
              batch = this.captureClause([]);
              continue;
          }
        }
        this.pos++;
      }
    }
    this.mutations.set(name, {
      name,
      args,
      ...(returns !== undefined ? { returns } : {}),
      ...(allow !== undefined ? { allow } : {}),
      ...(writes !== undefined ? { writes } : {}),
      ...(invalidates !== undefined ? { invalidates } : {}),
      ...(effect !== undefined ? { effect } : {}),
      bindings,
      ...(batch !== undefined ? { batch } : {}),
    });
  }

  /**
   * Capture a mutation-body clause: from the current keyword up to (but not
   * including) the next clause keyword or the body's closing brace. Tracks
   * brace depth so a `{arg}` inside a verb-binding path (`as PUT
   * /e/Review/{review}`) does not end the clause.
   */
  private captureClause(stops: string[]): string {
    const start = this.peek()!.start;
    this.pos++; // the clause keyword itself
    const stopSet = new Set(stops);
    let depth = 0;
    while (this.peek()) {
      const t = this.peek()!;
      if (t.kind === "punct" && t.value === "{") depth++;
      if (t.kind === "punct" && t.value === "}") {
        if (depth === 0) break; // body close
        depth--;
        this.pos++;
        continue;
      }
      if (depth === 0 && t.kind === "name" && stopSet.has(t.value)) break;
      this.pos++;
    }
    return this.sliceText(start, this.peek()?.start ?? this.text.length).trim().replace(/\s+/g, " ");
  }

  /** Consume a `{ … }` block (balanced) and return the inner text. */
  private captureBraces(): { text: string } {
    this.pos++; // opening {
    const start = this.peek()?.start ?? this.text.length;
    let depth = 1;
    let end = start;
    while (this.peek() && depth > 0) {
      const t = this.next()!;
      if (t.kind === "punct" && t.value === "{") depth++;
      else if (t.kind === "punct" && t.value === "}") {
        depth--;
        if (depth === 0) {
          end = t.start;
          break;
        }
      }
      end = t.start + t.value.length;
    }
    return { text: this.sliceText(start, end).trim() };
  }

  private skipToTopLevel(): void {
    while (this.peek()) {
      const t = this.peek()!;
      if (t.kind === "name" && TOP_KEYWORDS[t.value]) return;
      this.pos++;
    }
  }

  private sliceText(start: number, end: number): string {
    return this.text.slice(start, Math.max(start, end));
  }

  private resolveTargets(): void {
    for (const { target } of this.pendingRefs) {
      if (this.interfaces.has(target.name)) target.kind = "interface";
      else target.kind = "entity";
    }
  }

  private deriveInterfaceMembers(): void {
    for (const [iname, idef] of this.interfaces) {
      const members: string[] = [];
      for (const [ename, edef] of this.entities) {
        if (edef.implements.includes(iname)) members.push(ename);
      }
      this.interfaces.set(iname, { ...idef, members: members.sort() });
    }
  }
}

/** Parse an IDL document into the tooling schema model. Never throws. */
export function parseSchema(text: string): SchemaModel {
  const normalized = text.normalize("NFC");
  return new Parser(lex(normalized), normalized).parse();
}

// ---------------------------------------------------------------------------
// Helpers used by completion + docs

/** Concrete entity type names a target resolves to. */
export function targetTypes(schema: SchemaModel, target: Target): string[] {
  switch (target.kind) {
    case "entity":
      return [target.name];
    case "interface":
      return schema.interfaces.get(target.name)?.members.slice() ?? [];
    case "union":
      return target.members.slice();
  }
}

/** The display name of a target (`Human`, `Character`, `(Human | Droid)`). */
export function targetLabel(target: Target): string {
  switch (target.kind) {
    case "entity":
    case "interface":
      return target.name;
    case "union":
      return "(" + target.members.join(" | ") + ")";
  }
}

/**
 * The fields + edges selectable at a type context. For a concrete entity, its
 * own members. For an interface, the interface's declared members (§4.4: an
 * interface edge's selection set contains only interface-declared fields plus
 * inline fragments). A union has no common members.
 */
export function membersOf(
  schema: SchemaModel,
  target: Target,
): { fields: readonly FieldModel[]; edges: readonly EdgeModel[]; isInterface: boolean; isUnion: boolean } {
  switch (target.kind) {
    case "entity": {
      const e = schema.entities.get(target.name);
      return { fields: e?.fields ?? [], edges: e?.edges ?? [], isInterface: false, isUnion: false };
    }
    case "interface": {
      const i = schema.interfaces.get(target.name);
      return { fields: i?.fields ?? [], edges: i?.edges ?? [], isInterface: true, isUnion: false };
    }
    case "union":
      return { fields: [], edges: [], isInterface: false, isUnion: true };
  }
}

/** Members of a concrete entity type by name (for `... on Type` contexts). */
export function entityMembers(
  schema: SchemaModel,
  type: string,
): { fields: readonly FieldModel[]; edges: readonly EdgeModel[] } {
  const e = schema.entities.get(type);
  return { fields: e?.fields ?? [], edges: e?.edges ?? [] };
}
