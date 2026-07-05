/**
 * Apollo-style React bindings for the Lattice client (`@wireform/lattice/react`).
 *
 * React is an optional peer dependency: this entry point is the only module
 * that imports it. `useLatticeQuery` rides `client.queryBatched`, so every
 * hook mounting within one microtask tick (one synchronous render pass)
 * merges into a single multi-root HTTP request (spec §4.3) where the
 * protocol allows, and `useSyncExternalStore` re-renders exactly the hooks
 * whose underlying entity refs changed.
 */

import {
  createContext,
  createElement,
  useCallback,
  useContext,
  useMemo,
  useState,
  useSyncExternalStore,
  type ReactElement,
  type ReactNode,
} from "react";
import type { QueryDoc } from "./canonical.ts";
import { canonicalJson, canonicalize } from "./canonical.ts";
import type { LatticeClient, MutateOptions, MutationResult, Vars, WatchState } from "./client.ts";
import type { QueryData } from "./wire.ts";

const LatticeContext = createContext<LatticeClient | null>(null);

export interface LatticeProviderProps {
  readonly client: LatticeClient;
  readonly children?: ReactNode;
}

export function LatticeProvider(props: LatticeProviderProps): ReactElement {
  return createElement(LatticeContext.Provider, { value: props.client }, props.children);
}

export function useLatticeClient(): LatticeClient {
  const client = useContext(LatticeContext);
  if (!client) {
    throw new Error("useLatticeClient: no LatticeClient in context; wrap the tree in <LatticeProvider client={...}>");
  }
  return client;
}

export interface UseLatticeQueryOptions {
  /** Do not run the query (Apollo's `skip`); returns idle state until flipped. */
  readonly skip?: boolean;
}

export interface UseLatticeQueryResult<T> {
  readonly data: T | undefined;
  readonly loading: boolean;
  readonly error: unknown;
  /** An `invalidated` record has hit this result; a refetch is in flight. */
  readonly stale: boolean;
  readonly refetch: () => Promise<void>;
}

const SKIPPED_STATE: WatchState<never> = { data: undefined, loading: false, error: undefined, stale: false };
const noopSubscribe = (): (() => void) => () => undefined;
const skippedSnapshot = (): WatchState<never> => SKIPPED_STATE;

/**
 * Subscribe a component to one query. Results denormalize out of the shared
 * normalized store; entity updates (from any query or mutation response)
 * re-render exactly the hooks whose refs changed; `invalidated`-driven
 * staleness refetches automatically.
 */
export function useLatticeQuery<T = QueryData>(
  doc: QueryDoc<T>,
  vars?: Vars,
  options?: UseLatticeQueryOptions,
): UseLatticeQueryResult<T> {
  const client = useLatticeClient();
  const skip = options?.skip === true;
  // Identity of the (document, variables) pair, independent of object identity.
  const key = canonicalize(doc) + "\u0000" + canonicalJson(vars ?? {});
  const watch = useMemo(
    () => (skip ? null : client.watchQuery(doc, vars ?? {})),
    // eslint-disable-next-line react-hooks/exhaustive-deps -- key covers doc+vars
    [client, key, skip],
  );
  const state = useSyncExternalStore(
    watch ? watch.subscribe : noopSubscribe,
    watch ? watch.getSnapshot : skippedSnapshot,
    watch ? watch.getSnapshot : skippedSnapshot,
  );
  const refetch = useCallback(() => (watch ? watch.refetch() : Promise.resolve()), [watch]);
  return { data: state.data, loading: state.loading, error: state.error, stale: state.stale, refetch };
}

export interface UseLatticeMutationState {
  readonly loading: boolean;
  readonly error: unknown;
  readonly data: MutationResult | undefined;
}

export type UseLatticeMutationResult = readonly [
  (input: unknown, options?: MutateOptions) => Promise<MutationResult>,
  UseLatticeMutationState,
];

/**
 * `const [createReview, { loading }] = useLatticeMutation("createReview")`.
 *
 * The mutation's entity stream applies to the shared store (read-your-writes
 * for entity fields with zero follow-up requests); its `invalidated` record
 * marks intersecting cached queries stale, and their active hooks refetch.
 */
export function useLatticeMutation(name: string): UseLatticeMutationResult {
  const client = useLatticeClient();
  const [state, setState] = useState<UseLatticeMutationState>({ loading: false, error: undefined, data: undefined });
  const mutate = useCallback(
    async (input: unknown, options?: MutateOptions): Promise<MutationResult> => {
      setState({ loading: true, error: undefined, data: undefined });
      try {
        const result = await client.mutate(name, input, options);
        setState({ loading: false, error: undefined, data: result });
        return result;
      } catch (error) {
        setState({ loading: false, error, data: undefined });
        throw error;
      }
    },
    [client, name],
  );
  return [mutate, state] as const;
}
