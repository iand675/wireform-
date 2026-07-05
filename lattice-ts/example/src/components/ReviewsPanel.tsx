import { useState, type FormEvent } from "react";
import { gql } from "../../../src/index.ts";
import { useLatticeMutation, useLatticeQuery } from "../../../src/react.ts";
import type { Episode } from "../App.tsx";

interface ReviewTree {
  readonly __ref: string;
  readonly stars?: number;
  readonly commentary?: string | null;
  readonly createdAt?: string;
}

type ReviewsData = { reviews?: ReviewTree[] };

const REVIEWS_QUERY = gql<ReviewsData>`
  query Reviews($episode: Episode) {
    reviews(episode: $episode, first: 10) {
      stars
      commentary
      createdAt
    }
  }
`;

/**
 * The `createReview` response stream carries an `invalidated` record whose
 * keys include `reviews:{episode}`; that intersects this query's surrogate
 * keys, marks the cached result stale, and the hook refetches — the new
 * review appears with no manual cache plumbing.
 */
export function ReviewsPanel({ episode }: { episode: Episode }) {
  const { data, loading, error, stale } = useLatticeQuery(REVIEWS_QUERY, { episode });
  const [createReview, mutation] = useLatticeMutation("createReview");
  const [stars, setStars] = useState(5);
  const [commentary, setCommentary] = useState("");

  const submit = async (e: FormEvent) => {
    e.preventDefault();
    await createReview(
      { episode, stars, commentary: commentary.trim() === "" ? undefined : commentary.trim() },
      { idempotencyKey: crypto.randomUUID() },
    );
    setCommentary("");
  };

  return (
    <section className="card">
      <h2>
        Reviews — {episode}
        {stale ? <span className="badge">refreshing…</span> : null}
      </h2>
      {loading ? <p className="muted">Loading…</p> : null}
      {error ? <p className="error">{String(error)}</p> : null}
      <ul className="reviews">
        {(data?.reviews ?? []).map((r) => (
          <li key={r.__ref}>
            <span className="stars">{"★".repeat(r.stars ?? 0).padEnd(5, "☆")}</span>
            <span>{r.commentary ?? <em className="muted">no commentary</em>}</span>
          </li>
        ))}
      </ul>
      <form onSubmit={submit} className="review-form">
        <label>
          Stars
          <select value={stars} onChange={(e) => setStars(Number(e.target.value))}>
            {[1, 2, 3, 4, 5].map((n) => (
              <option key={n} value={n}>
                {n}
              </option>
            ))}
          </select>
        </label>
        <input
          placeholder="Say something…"
          value={commentary}
          onChange={(e) => setCommentary(e.target.value)}
        />
        <button type="submit" disabled={mutation.loading}>
          {mutation.loading ? "Posting…" : "Post review"}
        </button>
      </form>
      {mutation.error ? <p className="error">{String(mutation.error)}</p> : null}
    </section>
  );
}
