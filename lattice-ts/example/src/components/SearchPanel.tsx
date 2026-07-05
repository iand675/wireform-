import { useState } from "react";
import { gql } from "../../../src/index.ts";
import { useLatticeQuery } from "../../../src/react.ts";

interface SearchTree {
  readonly __ref: string;
  readonly name?: string;
  readonly homePlanet?: string;
  readonly primaryFunction?: string;
  readonly length?: number;
}

type SearchData = { search?: SearchTree[] };

// `search` targets the inline union (Human | Droid | Starship): every field
// lives under a per-type inline fragment; unlisted types would come back as
// bare refs (spec §4.4).
const SEARCH_QUERY = gql<SearchData>`
  query Search($text: Text) {
    search(text: $text, first: 10) {
      ... on Human { name homePlanet }
      ... on Droid { name primaryFunction }
      ... on Starship { name length }
    }
  }
`;

function kindOf(ref: string): "Human" | "Droid" | "Starship" | "?" {
  const type = ref.slice(0, ref.indexOf(":"));
  return type === "Human" || type === "Droid" || type === "Starship" ? type : "?";
}

export function SearchPanel() {
  const [text, setText] = useState("r2");
  const { data, loading, error } = useLatticeQuery(SEARCH_QUERY, { text }, { skip: text.trim() === "" });
  return (
    <section className="card">
      <h2>Search</h2>
      <input placeholder="Search the galaxy…" value={text} onChange={(e) => setText(e.target.value)} />
      {loading ? <p className="muted">Searching…</p> : null}
      {error ? <p className="error">{String(error)}</p> : null}
      <ul className="results">
        {(data?.search ?? []).map((hit) => {
          const kind = kindOf(hit.__ref);
          return (
            <li key={hit.__ref} className={`hit hit-${kind.toLowerCase()}`}>
              <span className="hit-kind">{kind}</span>
              <span className="hit-name">{hit.name}</span>
              <span className="muted">
                {kind === "Human" && hit.homePlanet ? hit.homePlanet : null}
                {kind === "Droid" && hit.primaryFunction ? hit.primaryFunction : null}
                {kind === "Starship" && hit.length !== undefined ? `${hit.length} m` : null}
              </span>
            </li>
          );
        })}
      </ul>
    </section>
  );
}
