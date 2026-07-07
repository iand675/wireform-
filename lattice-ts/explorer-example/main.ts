/**
 * Standalone Lattice Explorer, mounted against a running origin. Point it at
 * the `example-lattice` Star Wars origin (`cabal run example-lattice`, port
 * 8917) or set VITE_LATTICE_BASE to another origin.
 *
 *   cd lattice-ts && npx vite explorer-example
 */
import { mountExplorer } from "../src/explorer/index.ts";

const base = import.meta.env?.VITE_LATTICE_BASE ?? "http://127.0.0.1:8917";

const app = document.getElementById("app");
if (!app) throw new Error("missing #app");

mountExplorer(app, {
  base,
  // Demo credentials so the ctx + priv slices are reachable against the dev
  // origin (which trusts the vc payload without a proof, and admits priv on any
  // Authorization header). Toggle the slice chips to fetch them concurrently.
  claims: { org: 1, role: "Admin" },
  authToken: "demo-token",
  defaultQuery: `query Hero {
  hero(episode: Empire) {
    name
    ... on Human { homePlanet }
    ... on Droid { primaryFunction }
    friends(first: 3) { name }
  }
}
`,
});
