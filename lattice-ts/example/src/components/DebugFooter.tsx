import { useSyncExternalStore } from "react";
import { requestLog } from "../api.ts";

/**
 * Makes the merged-query batching visible: the three panels above declare
 * three independent `gql` queries, and on first paint the batcher folds them
 * into ONE multi-root request — watch the counter and the merged canonical
 * text below.
 */
export function DebugFooter() {
  useSyncExternalStore(requestLog.subscribe, requestLog.getSnapshot, requestLog.getSnapshot);
  const { entries, lastQuery } = requestLog;
  return (
    <footer className="debug">
      <h3>
        wire debug — <strong>{entries.length}</strong> HTTP request{entries.length === 1 ? "" : "s"}
      </h3>
      {lastQuery ? (
        <p className="canonical">
          last query request: <code>{lastQuery.kind}</code>
          {lastQuery.merged !== undefined ? ` (merged from ${lastQuery.merged} component queries)` : ""}
          <br />
          <code className="canonical-text">{lastQuery.canonicalText}</code>
        </p>
      ) : null}
      <ol className="request-list">
        {entries.map((r) => (
          <li key={r.n}>
            <code>
              {r.method} {r.url.replace(/^https?:\/\/[^/]+/, "")}
            </code>
          </li>
        ))}
      </ol>
    </footer>
  );
}
