import { useState } from "react";
import { DebugFooter } from "./components/DebugFooter.tsx";
import { HeroCard } from "./components/HeroCard.tsx";
import { ReviewsPanel } from "./components/ReviewsPanel.tsx";
import { SearchPanel } from "./components/SearchPanel.tsx";
import { usingMock } from "./api.ts";

const EPISODES = ["NewHope", "Empire", "Jedi"] as const;
export type Episode = (typeof EPISODES)[number];

export function App() {
  const [episode, setEpisode] = useState<Episode>("Empire");
  return (
    <div className="app">
      <header className="masthead">
        <h1>Star Wars dashboard</h1>
        <p className="tagline">
          three components, three <code>gql</code> queries, one Lattice request
          {usingMock ? " — in-browser mock origin" : ""}
        </p>
        <nav className="episodes">
          {EPISODES.map((e) => (
            <button key={e} className={e === episode ? "active" : ""} onClick={() => setEpisode(e)}>
              {e}
            </button>
          ))}
        </nav>
      </header>
      <main className="grid">
        <HeroCard episode={episode} />
        <ReviewsPanel episode={episode} />
        <SearchPanel />
      </main>
      <DebugFooter />
    </div>
  );
}
