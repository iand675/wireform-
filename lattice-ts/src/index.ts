/**
 * @wireform/lattice — a zero-dependency TypeScript client for the Lattice
 * cache-native graph query protocol. React bindings live in
 * `@wireform/lattice/react`.
 */

export type {
  EndRecord,
  EntityRecord,
  EntityTree,
  ElidedRecord,
  ErrorRecord,
  ErrorScope,
  FieldValue,
  InvalidatedRecord,
  JsonObject,
  JsonValue,
  LatticeRecord,
  ManifestRecord,
  Page,
  PageTree,
  PageValue,
  PlanRecord,
  ProgressRecord,
  QueryData,
  Ref,
  RefValue,
  TombstoneRecord,
  UnchangedRecord,
  UnknownRecord,
} from "./wire.ts";
export {
  HEADERS,
  LatticeWireError,
  QUERY_MEDIA_TYPE,
  RESERVED_PARAMS,
  isPageValue,
  isRef,
  isRefArray,
  isRefValue,
  parseRecord,
  parseRef,
  readRecords,
  recordsOfText,
  refType,
} from "./wire.ts";

export type {
  Argument,
  FieldSel,
  FragmentDefNode,
  InlineSel,
  QueryDoc,
  Selection,
  SpreadSel,
  Value,
  VarDef,
} from "./canonical.ts";
export {
  LatticeQueryError,
  UnmergeableError,
  canonicalFieldKey,
  canonicalJson,
  canonicalize,
  expandLocalFragments,
  gql,
  mergeSelectionList,
  parse,
  renderArgs,
  renderValue,
  valueToJson,
} from "./canonical.ts";

export type { ApplyOutcome, EntityState, QueryResultEntry } from "./store.ts";
export { LatticeStore, newApplyOutcome } from "./store.ts";

export type { MergedQueries, RootAssignment } from "./merge.ts";
export { mergeQueries } from "./merge.ts";

export type {
  FetchLike,
  LatticeClientOptions,
  LatticeRequestEvent,
  MutateOptions,
  MutationResult,
  ProblemDetails,
  QueryOptions,
  QueryResult,
  SliceName,
  Vars,
  WatchState,
} from "./client.ts";
export { LatticeClient, LatticeHttpError, QueryWatch, denormalize } from "./client.ts";
