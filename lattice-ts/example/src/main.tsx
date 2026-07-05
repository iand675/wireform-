import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { LatticeProvider } from "../../src/react.ts";
import { App } from "./App.tsx";
import { client } from "./api.ts";
import "./styles.css";

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <LatticeProvider client={client}>
      <App />
    </LatticeProvider>
  </StrictMode>,
);
