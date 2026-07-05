import { gql } from "../../../src/index.ts";
import { useLatticeQuery } from "../../../src/react.ts";
import type { Episode } from "../App.tsx";

interface FriendTree {
  readonly __ref: string;
  readonly name?: string;
}

interface HeroTree {
  readonly __ref: string;
  readonly name?: string;
  readonly homePlanet?: string;
  readonly primaryFunction?: string;
  readonly friends?: { readonly items: FriendTree[]; readonly next: string | null };
}

type HeroData = { hero?: HeroTree[] };

// `hero` targets the (Human | Droid) union, so concrete fields — including
// the `friends` collection — live under `... on` inline fragments (spec §4.4).
const HERO_QUERY = gql<HeroData>`
  query Hero($episode: Episode) {
    hero(episode: $episode) {
      name
      ... on Human { homePlanet friends(first: 10) { name } }
      ... on Droid { primaryFunction friends(first: 10) { name } }
    }
  }
`;

export function HeroCard({ episode }: { episode: Episode }) {
  const { data, loading, error, stale } = useLatticeQuery(HERO_QUERY, { episode });
  const hero = data?.hero?.[0];
  return (
    <section className="card">
      <h2>
        Hero of {episode}
        {stale ? <span className="badge">refreshing…</span> : null}
      </h2>
      {loading ? <p className="muted">Loading…</p> : null}
      {error ? <p className="error">{String(error)}</p> : null}
      {hero ? (
        <div>
          <p className="hero-name">{hero.name}</p>
          <p className="muted">
            {hero.homePlanet ? `Home planet: ${hero.homePlanet}` : null}
            {hero.primaryFunction ? `Primary function: ${hero.primaryFunction}` : null}
          </p>
          <h3>Friends</h3>
          <ul>
            {(hero.friends?.items ?? []).map((f) => (
              <li key={f.__ref}>{f.name}</li>
            ))}
          </ul>
          {hero.friends?.next ? <p className="muted">more… (cursor {hero.friends.next})</p> : null}
        </div>
      ) : null}
    </section>
  );
}
