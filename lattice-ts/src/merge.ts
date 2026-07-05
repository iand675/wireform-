/**
 * Query merging (spec §4.3): combine several parsed query documents into one
 * multi-root document so that components mounting together cost one request.
 *
 * Lattice has no aliases, so the merge is only defined when shared root
 * fields agree on their arguments: the manifest's `root` map keys by bare
 * root name (§9.2), and two selections of one root with different arguments
 * would collide there. That case throws `UnmergeableError` and callers fall
 * back to separate requests — which is also what the protocol prefers, since
 * the two variants have different cache identities anyway (corpus entry 3).
 */

import type { FieldSel, QueryDoc, VarDef } from "./canonical.ts";
import {
  UnmergeableError,
  expandLocalFragments,
  mergeSelectionList,
  renderArgs,
  renderValue,
} from "./canonical.ts";

export { UnmergeableError };

/** Which roots of the merged document one input document needs. */
export interface RootAssignment {
  /** Index of the input document in the `mergeQueries` argument list. */
  readonly index: number;
  /** The root field names this document selects. */
  readonly roots: readonly string[];
  /** The input document with local fragments expanded — walk THIS to slice the shared result back out. */
  readonly doc: QueryDoc<unknown>;
}

export interface MergedQueries {
  readonly merged: QueryDoc<unknown>;
  readonly assignments: readonly RootAssignment[];
}

function sameVarDef(a: VarDef, b: VarDef): boolean {
  const aDef = a.default ? renderValue(a.default) : "";
  const bDef = b.default ? renderValue(b.default) : "";
  return a.type === b.type && a.optional === b.optional && aDef === bDef;
}

/**
 * Merge query documents into one multi-root document.
 *
 * - Root fields union; the same root name selected by several documents with
 *   identical canonical arguments unions its selection sets recursively
 *   (duplicate fields dedupe, nested selections merge).
 * - The same root with different arguments throws `UnmergeableError` naming
 *   the root.
 * - Variable declarations union; one name declared with different types or
 *   defaults throws `UnmergeableError` naming the variable.
 *
 * `assignments[i]` records which roots document `i` needs, so its result can
 * be denormalized from the shared response independently of its siblings.
 */
export function mergeQueries(docs: ReadonlyArray<QueryDoc<unknown>>): MergedQueries {
  if (docs.length === 0) {
    throw new UnmergeableError("mergeQueries requires at least one document");
  }
  const expanded = docs.map((d) => expandLocalFragments(d));

  const variables = new Map<string, VarDef>();
  for (const doc of expanded) {
    for (const v of doc.variables) {
      const prev = variables.get(v.name);
      if (!prev) {
        variables.set(v.name, v);
      } else if (!sameVarDef(prev, v)) {
        throw new UnmergeableError(
          `variable $${v.name} is declared with incompatible types or defaults across documents`,
          { variable: v.name },
        );
      }
    }
  }

  const roots = new Map<string, FieldSel>();
  const assignments: RootAssignment[] = [];
  expanded.forEach((doc, index) => {
    const names: string[] = [];
    for (const sel of doc.selections) {
      if (sel.kind !== "field") {
        throw new UnmergeableError("only root fields may appear at the top level of a mergeable query");
      }
      if (!names.includes(sel.name)) names.push(sel.name);
      const prev = roots.get(sel.name);
      if (!prev) {
        roots.set(sel.name, sel);
        continue;
      }
      if (renderArgs(prev.args) !== renderArgs(sel.args)) {
        throw new UnmergeableError(
          `root field ${JSON.stringify(sel.name)} is selected with different arguments; ` +
            "Lattice has no aliases — issue these as separate requests",
          { root: sel.name },
        );
      }
      const [merged] = mergeSelectionList([prev, sel]);
      roots.set(sel.name, merged as FieldSel);
    }
    assignments.push({ index, roots: names, doc });
  });

  const merged: QueryDoc<unknown> = {
    kind: "query",
    variables: [...variables.values()],
    selections: mergeSelectionList([...roots.values()]),
    fragments: {},
    imports: [],
  };
  return { merged, assignments };
}
