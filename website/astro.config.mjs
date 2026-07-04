// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import starlightLinksValidator from "starlight-links-validator";
import mermaid from "astro-mermaid";
import tailwindcss from "@tailwindcss/vite";
import { readFileSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Auto-generated module sidebar lives here. The Haddock ingester writes it
// each time it runs; we fall back to an empty list so the site still builds
// even when the API has not been ingested yet (e.g. in CI without GHC).
const sidebarPath = resolve(__dirname, "src/content/generated/sidebar.json");
let apiSidebar = [];
if (existsSync(sidebarPath)) {
	try {
		apiSidebar = JSON.parse(readFileSync(sidebarPath, "utf8"));
	} catch {
		apiSidebar = [];
	}
}

// Deployment-target detection.
//
// The canonical deploy is GitHub Pages at https://iand675.github.io/wireform-/,
// which forces a non-empty `base`. When the same build is shipped to Vercel,
// the site is served from the domain root instead, so the GitHub Pages `base`
// would cause every generated `<link href="/wireform-/_astro/...css">` to 404.
//
// Vercel sets `VERCEL=1` in its build environment; we also accept explicit
// `SITE_URL` / `SITE_BASE` overrides so other hosts (Netlify, Cloudflare Pages,
// custom previews) can opt into the rootless layout without code changes.
const explicitSite = process.env.SITE_URL;
const explicitBase = process.env.SITE_BASE;
const onVercel = process.env.VERCEL === "1";

const vercelHost =
	process.env.VERCEL_PROJECT_PRODUCTION_URL || process.env.VERCEL_URL;

const site =
	explicitSite ??
	(onVercel
		? vercelHost
			? `https://${vercelHost}`
			: undefined
		: "https://iand675.github.io");

const base = explicitBase ?? (onVercel ? "/" : "/wireform-");

export default defineConfig({
	site,
	base,
	trailingSlash: "ignore",
	vite: {
		plugins: [tailwindcss()],
	},
	integrations: [
		mermaid({
			theme: "neutral",
			autoTheme: true,
			mermaidConfig: {
				flowchart: { curve: "basis" },
			},
		}),
		starlight({
			title: "wireform",
			description:
				"One Haskell ecosystem for serialization, codegen, streaming, messaging, and analytics.",
			plugins: [
				// Dead-link checking. Validates every internal link (and in-page
				// anchor) against the set of pages Starlight actually builds, so a
				// typo'd slug or a renamed page fails the build instead of shipping
				// a 404. The auto-generated Haddock API pages under `api/` are only
				// present after the ingester runs, so we exclude them from the link
				// graph to avoid false positives when building without GHC.
				starlightLinksValidator({
					errorOnRelativeLinks: false,
					errorOnLocalLinks: false,
					exclude: ["/wireform-/api/**", "/api/**", "**/api/**"],
				}),
			],
			social: [
				{
					icon: "github",
					label: "GitHub",
					href: "https://github.com/iand675/wireform-",
				},
			],
			customCss: ["./src/styles/fumadocs.css"],
			components: {
				Hero: "./src/components/Hero.astro",
			},
			sidebar: [
				{
					label: "Overview",
					items: [
						{ label: "Getting started", slug: "guides/getting-started" },
						{ label: "Format catalogue", slug: "guides/formats" },
					],
				},
				{
					label: "Concepts",
					autogenerate: { directory: "concepts" },
				},
				{
					label: "Packages",
					items: [
						{
							label: "Infrastructure",
							items: [
								{ label: "wireform-core", slug: "packages/core" },
								{ label: "wireform-network", slug: "packages/network" },
								{ label: "wireform-derive", slug: "packages/derive" },
								{ label: "Parser adapters", slug: "packages/parser-adapters" },
								{ label: "wireform-columnar", slug: "packages/columnar" },
								{
									label: "wireform-columnar-core",
									slug: "packages/columnar-core",
								},
								{ label: "wireform-stats", slug: "packages/stats" },
							],
						},
						{
							label: "Schema & IDL formats",
							items: [
								{ label: "Protocol Buffers", slug: "packages/proto" },
								{ label: "Avro", slug: "packages/avro" },
								{ label: "Thrift", slug: "packages/thrift" },
								{ label: "Bond", slug: "packages/bond" },
								{ label: "Cap'n Proto", slug: "packages/capnproto" },
								{ label: "FlatBuffers", slug: "packages/flatbuffers" },
								{ label: "ASN.1", slug: "packages/asn1" },
							],
						},
						{
							label: "Binary value formats",
							items: [
								{ label: "CBOR", slug: "packages/cbor" },
								{ label: "MessagePack", slug: "packages/msgpack" },
								{ label: "BSON", slug: "packages/bson" },
								{ label: "Ion", slug: "packages/ion" },
								{ label: "Bencode", slug: "packages/bencode" },
								{ label: "Fory", slug: "packages/fory" },
							],
						},
						{
							label: "Text & markup formats",
							items: [
								{ label: "XML", slug: "packages/xml" },
								{ label: "HTML", slug: "packages/html" },
								{ label: "YAML", slug: "packages/yaml" },
								{ label: "TOML", slug: "packages/toml" },
								{ label: "CSV", slug: "packages/csv" },
								{ label: "EDN", slug: "packages/edn" },
							],
						},
						{
							label: "Analytics & lake formats",
							items: [
								{ label: "Parquet", slug: "packages/parquet" },
								{ label: "Arrow", slug: "packages/arrow" },
								{ label: "ORC", slug: "packages/orc" },
								{ label: "Iceberg", slug: "packages/iceberg" },
								{ label: "Delta Lake", slug: "packages/delta" },
								{ label: "Hudi", slug: "packages/hudi" },
								{ label: "Lance", slug: "packages/lance" },
							],
						},
						{
							label: "Validation & expressions",
							items: [
								{ label: "CEL", slug: "packages/cel" },
								{ label: "protovalidate", slug: "packages/protovalidate" },
							],
						},
						{
							label: "State machines",
							items: [
								{
									label: "wireform-state-machine",
									slug: "packages/state-machine",
								},
							],
						},
						{
							label: "Messaging & networking",
							items: [
								{
									label: "HTTP",
									items: [
										{ label: "wireform-http", slug: "packages/http" },
										{ label: "wireform-http1", slug: "packages/http1" },
										{ label: "wireform-http2", slug: "packages/http2" },
										{ label: "WAI adapter", slug: "packages/http-wai" },
										{ label: "WebSocket", slug: "packages/websocket" },
									],
								},
								{
									label: "gRPC",
									items: [
										{ label: "wireform-grpc", slug: "packages/grpc" },
										{ label: "grpc-spec", slug: "packages/grpc-spec" },
										{ label: "wireform-connect", slug: "packages/connect" },
									],
								},
								{
									label: "Kafka",
									items: [
										{ label: "Kafka client", slug: "packages/kafka" },
										{
											label: "Kafka protocol",
											slug: "packages/kafka-protocol",
										},
										{
											label: "hw-kafka-client facade",
											slug: "packages/hw-kafka-client",
										},
									],
								},
								{
									label: "Networking foundations",
									items: [
										{ label: "hermes (HTTP headers)", slug: "packages/hermes" },
										{
											label: "http-semantics",
											slug: "packages/http-semantics",
										},
									],
								},
							],
						},
					],
				},
				{
					label: "State machines",
					items: [
						{ label: "Overview", slug: "state-machine" },
						{ label: "Concepts", slug: "state-machine/concepts" },
						{ label: "The chart type", slug: "state-machine/spec" },
						{ label: "Implementing a chart", slug: "state-machine/implementing" },
						{ label: "Running a machine", slug: "state-machine/running" },
						{ label: "Testing", slug: "state-machine/testing" },
						{ label: "Persistence & recovery", slug: "state-machine/persistence" },
						{ label: "Visualization", slug: "state-machine/visualization" },
					],
				},
				{
					label: "Connect RPC",
					items: [
						{ label: "Overview", slug: "connect" },
						{ label: "Getting started", slug: "connect/getting-started" },
						{ label: "Serving", slug: "connect/server" },
						{ label: "Calling", slug: "connect/client" },
						{ label: "Wire protocol", slug: "connect/protocol" },
					],
				},
				{
					label: "Kafka Streams",
					items: [
						{ label: "Overview", slug: "kafka-streams" },
						{
							label: "Get started",
							items: [
								{
									label: "Quickstart",
									slug: "kafka-streams/get-started/quickstart",
								},
								{
									label: "1. What is Kafka Streams?",
									slug: "kafka-streams/get-started/what-is-kafka-streams",
								},
								{
									label: "2. Your first topology",
									slug: "kafka-streams/get-started/your-first-topology",
								},
								{
									label: "3. Stateful processing",
									slug: "kafka-streams/get-started/stateful-processing",
								},
								{
									label: "4. Joins and tables",
									slug: "kafka-streams/get-started/joins-and-tables",
								},
								{
									label: "5. Going to production",
									slug: "kafka-streams/get-started/going-to-production",
								},
							],
						},
						{
							label: "Riffle: Flink-class extensions",
							slug: "kafka-streams/riffle",
						},
						{
							label: "Operations",
							items: [
								{
									label: "Topology evolution",
									slug: "kafka-streams/operating/topology-evolution",
								},
								{
									label: "Scaling and rebalancing",
									slug: "kafka-streams/operating/scaling",
								},
								{
									label: "Running in containers",
									slug: "kafka-streams/operating/containers",
								},
								{
									label: "Exactly-once across systems",
									slug: "kafka-streams/operating/exactly-once",
								},
								{
									label: "Observability",
									slug: "kafka-streams/operating/observability",
								},
								{
									label: "Visibility versus ACID",
									slug: "kafka-streams/operating/visibility",
								},
								{ label: "Runbooks", slug: "kafka-streams/operating/runbooks" },
							],
						},
						{
							label: "Concepts",
							items: [
								{
									label: "Topology optimization",
									slug: "kafka-streams/concepts/topology-optimization",
								},
								{
									label: "Dynamic topology changes",
									slug: "kafka-streams/concepts/dynamic-topology",
								},
								{
									label: "Railway-oriented programming",
									slug: "kafka-streams/concepts/railway-oriented-programming",
								},
							],
						},
						{
							label: "Guides",
							items: [
								{
									label: "Enrichment via external systems",
									slug: "kafka-streams/guides/enrichment",
								},
							],
						},
						{ label: "Glossary", slug: "kafka-streams/glossary" },
					],
				},
				// The Haddock-ingested API reference appears here only once
				// 'src/content/generated/sidebar.json' has been populated by the
				// ingester. Until then the section is omitted entirely so the
				// build doesn't fail on a placeholder slug.
				...(apiSidebar.length > 0
					? [
							{
								label: "API reference",
								collapsed: false,
								items: apiSidebar,
							},
						]
					: []),
			],
			expressiveCode: {
				themes: ["github-dark-default", "github-light"],
				shiki: {
					langs: [
						JSON.parse(
							readFileSync(
								resolve(__dirname, "src/grammars/cabal.tmLanguage.json"),
								"utf8",
							),
						),
					],
				},
				styleOverrides: {
					borderRadius: "4px",
					codeFontFamily:
						"'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, monospace",
				},
			},
		}),
	],
});
