/**
 * `@wireform/lattice/explorer` — an embeddable, dependency-free web IDE for the
 * Lattice protocol, in the spirit of GraphiQL: workshop queries against a live
 * origin (schema-aware editor, denormalized results, raw entity-stream records,
 * the `explain` plan, response headers) and author IDLs (live structural
 * validation, origin compatibility checks).
 *
 * The flagship entry is `mountExplorer(container, { base })`, which renders the
 * full UI into any element. Everything it is built from is exported too, so you
 * can compose your own tooling: `parseSchema` (the browser-side IDL model),
 * `completeQuery`/`lintQuery` (schema-aware editor services), and
 * `ExplorerSession` (headless run/explain/checkIdl orchestration over an
 * injectable `fetch`).
 */

export { mountExplorer } from "./mount.ts";
export type { ExplorerHandle, ExplorerOptions } from "./mount.ts";

export {
  ExplorerSession,
} from "./session.ts";
export type {
  CheckReport,
  Discovery,
  ExplainDoc,
  ExplorerSessionOptions,
  LoadedSchema,
  MutationResult,
  ProblemDetails,
  RunHeaders,
  RunMode,
  RunOptions,
  RunResult,
  SliceName,
} from "./session.ts";

export {
  entityMembers,
  membersOf,
  parseSchema,
  targetLabel,
  targetTypes,
} from "./schema.ts";
export type {
  ArgModel,
  EdgeModel,
  EntityModel,
  EnumModel,
  FieldModel,
  FragmentModel,
  InterfaceModel,
  MutationModel,
  RecordModel,
  RootModel,
  SchemaDiagnostic,
  SchemaModel,
  Target,
} from "./schema.ts";

export {
  analyzeQueryContext,
  completeQuery,
  lintQuery,
} from "./complete.ts";
export type {
  CompletionItem,
  CompletionKind,
  Diagnostic,
  QueryContext,
  Scope,
  Severity,
  TypeContext,
} from "./complete.ts";

export { highlightIdl, highlightQuery } from "./highlight.ts";
export { createEditor } from "./editor.ts";
export type { Editor, EditorOptions } from "./editor.ts";
export { EXPLORER_CSS, injectStyles } from "./styles.ts";
