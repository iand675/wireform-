#!/usr/bin/env node
/**
 * ingest-haddock.mjs
 *
 * Ingest Haddock HTML output and re-emit it as Starlight MDX pages styled
 * after fumadocs. The goal is *not* to faithfully reproduce Haddock's layout
 * — it is to extract the underlying information (modules, declarations, type
 * signatures, doc strings, "Source" links) and present them in a clean,
 * navigable, searchable form that the rest of the docs site shares.
 *
 * Inputs:
 *   --input  <dir>   Directory tree of Haddock HTML output (the directory
 *                    that contains haddock-bundle.min.js, doc-index.html and
 *                    one index.html / Module.Name.html per module).
 *                    May be passed multiple times for multi-package layouts;
 *                    each input becomes a top-level group in the sidebar.
 *
 *                    Default: ../dist-newstyle/build/.../doc/html/<pkg>/...
 *                    auto-discovered from cabal v2-haddock output.
 *
 *   --out    <dir>   Where to write MDX pages.  Default: src/content/docs/api
 *
 *   --sidebar <path> Where to write the sidebar JSON consumed by
 *                    astro.config.mjs.  Default:
 *                    src/content/generated/sidebar.json
 *
 *   --if-present     Skip silently if no input is found.  Used by
 *                    `npm run prebuild` so the site still builds without
 *                    Haddock output.
 *
 *   --base   <url>   Optional URL prefix for "Source" links.  Defaults to
 *                    relative links into the original Haddock tree, copied
 *                    under public/haddock/<package>/.
 *
 * Output layout (per package):
 *
 *   src/content/docs/api/index.mdx          - "Browse the API" landing page
 *   src/content/docs/api/<pkg>/index.mdx    - per-package module index
 *   src/content/docs/api/<pkg>/<Mod-Sub>.mdx
 *
 *   src/content/generated/sidebar.json      - sidebar entries for astro
 */

import { readFileSync, writeFileSync, mkdirSync, readdirSync, statSync, existsSync, copyFileSync, rmSync } from 'node:fs';
import { dirname, join, resolve, basename, relative, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { load as loadHtml } from 'cheerio';

const __dirname = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(__dirname, '..', '..');
const websiteRoot = resolve(__dirname, '..');

// -- argv parsing -----------------------------------------------------

const argv = process.argv.slice(2);
const flags = {
  inputs: [],
  out: resolve(websiteRoot, 'src/content/docs/api'),
  sidebar: resolve(websiteRoot, 'src/content/generated/sidebar.json'),
  publicDir: resolve(websiteRoot, 'public/haddock'),
  ifPresent: false,
  fallbackFixture: false,
  base: null,
};

for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  switch (a) {
    case '--input':
      flags.inputs.push(resolve(argv[++i]));
      break;
    case '--out':
      flags.out = resolve(argv[++i]);
      break;
    case '--sidebar':
      flags.sidebar = resolve(argv[++i]);
      break;
    case '--public':
      flags.publicDir = resolve(argv[++i]);
      break;
    case '--base':
      flags.base = argv[++i];
      break;
    case '--if-present':
      flags.ifPresent = true;
      break;
    case '--fallback-fixture':
      flags.fallbackFixture = true;
      break;
    case '-h':
    case '--help':
      console.log('Usage: ingest-haddock.mjs [--input DIR]... [--out DIR] [--sidebar PATH] [--if-present]');
      process.exit(0);
    default:
      console.error(`Unknown flag: ${a}`);
      process.exit(2);
  }
}

// -- input discovery --------------------------------------------------

if (flags.inputs.length === 0) {
  flags.inputs = autoDiscoverInputs();
}

if (flags.inputs.length === 0 && flags.fallbackFixture) {
  const fixtureRoot = resolve(repoRoot, 'haddock-fixture');
  if (existsSync(fixtureRoot)) {
    for (const entry of readdirSync(fixtureRoot)) {
      const p = join(fixtureRoot, entry);
      if (statSync(p).isDirectory()) flags.inputs.push(p);
    }
    if (flags.inputs.length > 0) {
      console.log('[haddock] no real Haddock output; falling back to fixture under haddock-fixture/.');
    }
  }
}

if (flags.inputs.length === 0) {
  if (flags.ifPresent) {
    console.log('[haddock] no Haddock output found; skipping (use `cabal v2-haddock all` to generate it).');
    writeStubLanding();
    writeSidebar([]);
    process.exit(0);
  }
  console.error('[haddock] no Haddock HTML found.  Pass --input <dir> or run');
  console.error('[haddock]   cabal v2-haddock all --haddock-html');
  console.error('[haddock] from the repo root first.');
  process.exit(1);
}

// -- main -------------------------------------------------------------

console.log('[haddock] inputs:');
for (const i of flags.inputs) console.log(`  - ${i}`);

ensureDir(flags.out);
ensureDir(dirname(flags.sidebar));
// Wipe previous output so renamed modules don't linger.
if (existsSync(flags.out)) {
  for (const entry of readdirSync(flags.out)) {
    const p = join(flags.out, entry);
    if (statSync(p).isDirectory()) rmSync(p, { recursive: true, force: true });
  }
}

const sidebarGroups = [];
const packageIndices = [];

for (const inputDir of flags.inputs) {
  const pkg = derivePackageName(inputDir);
  console.log(`[haddock] ingesting package: ${pkg} (${inputDir})`);

  const moduleFiles = listModuleFiles(inputDir);
  if (moduleFiles.length === 0) {
    console.warn(`[haddock]   no Haddock module pages found in ${inputDir}`);
    continue;
  }

  // Copy raw Haddock assets so "Source" links keep working from MDX pages.
  const pkgPublic = join(flags.publicDir, pkg);
  ensureDir(pkgPublic);
  copyRawAssets(inputDir, pkgPublic);

  const modules = [];
  for (const file of moduleFiles) {
    const mod = parseModulePage(file, inputDir, pkg);
    if (mod) modules.push(mod);
  }
  modules.sort((a, b) => a.name.localeCompare(b.name));

  // Per-module pages
  for (const mod of modules) {
    writeModuleMdx(mod, pkg);
  }

  // Per-package index
  writePackageIndex(pkg, modules);
  packageIndices.push({ pkg, count: modules.length });

  sidebarGroups.push(buildSidebar(pkg, modules));
}

writeApiLanding(packageIndices);
writeSidebar(sidebarGroups);

console.log(`[haddock] wrote ${packageIndices.reduce((n, p) => n + p.count, 0)} module pages across ${packageIndices.length} package(s).`);

// =====================================================================
// helpers
// =====================================================================

function autoDiscoverInputs() {
  // Look for Haddock output produced by `cabal v2-haddock all`.  We just walk
  // dist-newstyle/build/<arch>/<ghc>/<pkg-version>/doc/html/<pkg>/ and pick
  // every directory that contains an `index.html` plus `*.html` siblings
  // produced by Haddock (heuristic: presence of a `haddock-bundle*.js` or
  // `mini_*.html` is a strong signal).
  const dist = resolve(repoRoot, 'dist-newstyle/build');
  if (!existsSync(dist)) return [];
  const found = [];
  walk(dist, (p, st) => {
    if (!st.isDirectory()) return;
    const idx = join(p, 'index.html');
    if (!existsSync(idx)) return;
    const files = readdirSync(p);
    if (files.some((f) => /^mini_.+\.html$/.test(f) || f === 'haddock-bundle.min.js' || f === 'haddock-util.js')) {
      found.push(p);
    }
  }, 8);
  return found;
}

function walk(root, visit, maxDepth = Infinity) {
  function go(p, depth) {
    let st;
    try { st = statSync(p); } catch { return; }
    visit(p, st);
    if (st.isDirectory() && depth < maxDepth) {
      let entries;
      try { entries = readdirSync(p); } catch { return; }
      for (const e of entries) go(join(p, e), depth + 1);
    }
  }
  go(root, 0);
}

function derivePackageName(inputDir) {
  // The Haddock layout puts the package name as the last path segment, e.g.
  // .../doc/html/wireform-core/.  Fall back to the basename otherwise.
  return basename(inputDir);
}

function listModuleFiles(inputDir) {
  // Module pages are named with hyphens substituted for dots, e.g.
  // Wireform-Core-Schema.html.  We exclude well-known non-module pages.
  const skip = new Set([
    'index.html',
    'doc-index.html',
    'doc-index-All.html',
    'doc-index-Frames.html',
    'haddock-bundle.min.js',
    'haddock-util.js',
    'frames.html',
  ]);
  const out = [];
  for (const f of readdirSync(inputDir)) {
    if (skip.has(f)) continue;
    if (!f.endsWith('.html')) continue;
    if (f.startsWith('mini_')) continue; // Haddock "mini" frame pages
    if (f.startsWith('doc-index-')) continue;
    out.push(join(inputDir, f));
  }
  return out;
}

function copyRawAssets(inputDir, outDir) {
  // Copy CSS, JS, and src/* so "Source" links resolve when navigated.
  for (const entry of readdirSync(inputDir)) {
    const src = join(inputDir, entry);
    const dst = join(outDir, entry);
    const st = statSync(src);
    if (st.isDirectory() && entry === 'src') {
      copyDir(src, dst);
    } else if (st.isFile()) {
      // Preserve everything; cheap and keeps original Haddock browsable
      // as a fallback.
      try { copyFileSync(src, dst); } catch {}
    }
  }
}

function copyDir(src, dst) {
  ensureDir(dst);
  for (const e of readdirSync(src)) {
    const s = join(src, e);
    const d = join(dst, e);
    const st = statSync(s);
    if (st.isDirectory()) copyDir(s, d);
    else copyFileSync(s, d);
  }
}

// -- module parsing ---------------------------------------------------

function parseModulePage(file, inputDir, pkg) {
  let html;
  try { html = readFileSync(file, 'utf8'); } catch { return null; }
  const $ = loadHtml(html);

  // Module name: prefer the `<p class="caption">` next to the module banner.
  const moduleName = $('#module-header .caption').first().text().trim()
    || $('#module-header').text().trim()
    || basename(file, '.html').replace(/-/g, '.');

  if (!moduleName || moduleName === 'Index') return null;

  const description = ($('#description .doc').first().text() || '').trim();

  // Synopsis links — Haddock writes these into <ul class="details-toggle">.
  const synopsis = [];
  $('#synopsis ul li').each((_, li) => {
    const txt = $(li).text().trim().replace(/\s+/g, ' ');
    if (txt) synopsis.push(txt);
  });

  // Sections: <h1>/<h2> followed by <div class="top"> declarations.
  const sections = [];
  let currentSection = { title: null, decls: [] };
  sections.push(currentSection);
  $('#interface').children().each((_, el) => {
    const tag = el.tagName?.toLowerCase();
    if (tag === 'h1' || tag === 'h2') {
      currentSection = { title: $(el).text().trim(), decls: [] };
      sections.push(currentSection);
      return;
    }
    if (tag === 'div' && $(el).hasClass('top')) {
      const decl = parseDeclaration($, el, pkg);
      if (decl) currentSection.decls.push(decl);
    }
  });
  // Remove empty leading section if present.
  while (sections.length && !sections[0].title && sections[0].decls.length === 0) {
    sections.shift();
  }

  return {
    name: moduleName,
    file,
    relFile: relative(inputDir, file),
    description,
    synopsis,
    sections,
    pkg,
  };
}

function parseDeclaration($, el, pkg) {
  const $el = $(el);
  // Header is in <p class="src"> (single-line) or <table class="src">
  // (multi-line). We render whichever exists, lifted to plain text.
  const $sig = $el.find('> .src, > p.src, > table.src').first();
  if (!$sig.length) return null;

  // "Source" link, if present, points at the line in the colourised source.
  // We extract it before flattening to plain text so it doesn't pollute the
  // signature.
  let sourceHref = null;
  const $srcLinks = $sig.find('a').filter((_, a) => /^Source$/i.test($(a).text().trim()));
  if ($srcLinks.length) {
    sourceHref = $srcLinks.first().attr('href') || null;
    $srcLinks.remove();
  }

  const signature = textNodeWalk($, $sig.get(0)).replace(/\s+/g, ' ').trim();
  if (!signature) return null;

  // Description block following the header.
  const docHtml = $el.find('> .doc').first().html() || '';

  // Constructors / fields / methods sublists.  Haddock uses
  // `<div class="subs constructors">` or `subs fields` / `subs methods`.
  const constructors = parseSubs($, $el, 'constructors');
  const fields = parseSubs($, $el, 'fields');
  const methods = parseSubs($, $el, 'methods');
  const instances = parseInstances($, $el);

  // Anchor id (used as a stable id within the page).
  const anchor = $el.attr('id') || $sig.attr('id') || null;

  // Heuristic kind: keyword at the start of the signature.
  const kw = signature.match(/^(data|newtype|type|class|instance|family|pattern|module|module instance)\b/);
  const kind = kw ? kw[1] : 'value';

  // Name: the prominent identifier inside the signature.
  let name = '';
  const nm = $sig.find('a[id]:not(.link), a.def, .name, .def').first().text().trim();
  if (nm) name = nm;
  if (!name) {
    // Fall back: first identifier-looking token after the keyword.
    const stripped = signature.replace(/^(data|newtype|type|class|instance|family|pattern)\s+/, '');
    const m = stripped.match(/^[A-Za-z_][A-Za-z0-9_'.]*/);
    if (m) name = m[0];
  }

  return {
    anchor,
    name,
    kind,
    signature,
    sourceHref,
    docHtml,
    constructors,
    fields,
    methods,
    instances,
  };
}

function parseSubs($, $decl, kind) {
  const $subs = $decl.find(`> .subs.${kind}`).first();
  if (!$subs.length) return [];
  const out = [];
  $subs.find('> dt, > .src').each((_, n) => {
    const $n = $(n);
    // Strip embedded "Source" anchors before flattening.
    $n.find('a').filter((_, a) => /^Source$/i.test($(a).text().trim())).remove();
    const sig = textWithSpaces($, $n).replace(/\s+/g, ' ').trim();
    let docHtml = '';
    const $next = $n.next();
    if ($next.is('dd, .doc')) docHtml = $next.html() || '';
    if (sig) out.push({ signature: sig, docHtml });
  });
  if (out.length === 0) {
    // Some Haddock versions emit a flat list of <li> entries instead.
    $subs.find('> ul > li').each((_, li) => {
      const $li = $(li);
      $li.find('a').filter((_, a) => /^Source$/i.test($(a).text().trim())).remove();
      const sig = textWithSpaces($, $li).replace(/\s+/g, ' ').trim();
      if (sig) out.push({ signature: sig, docHtml: '' });
    });
  }
  return out;
}

function parseInstances($, $decl) {
  // Instances are tucked inside a `<details><summary>Instances</summary>...</details>`
  // block.  We don't reproduce them inline (they're noisy); instead we surface
  // the count and let the user click through to the Haddock source if needed.
  const $det = $decl.find('details.instances').first();
  if (!$det.length) return null;
  const count = $det.find('.src.clearfix').length || $det.find('.instance').length;
  return count;
}

function textNodeWalk($, node) {
  // Walk a DOM subtree depth-first, emitting text nodes in document order.
  // Cheerio's `.text()` already does the right thing; we use this as a
  // re-implementation that adds a separating space at element boundaries
  // we know to be block-level so 'class Foo where' doesn't collapse to
  // 'classFoowhere'.
  if (!node) return '';
  if (node.type === 'text') return node.data || '';
  if (node.type !== 'tag') return '';
  let out = '';
  const tag = (node.tagName || '').toLowerCase();
  const blockish = /^(br|p|div|li|table|tr|td|th|ul|ol|dt|dd|h\d)$/i.test(tag);
  if (blockish) out += ' ';
  for (const child of node.children || []) {
    out += textNodeWalk($, child);
  }
  if (blockish) out += ' ';
  // Heuristic: join keyword-and-name boundary (a span.keyword and the next
  // identifier) with a space.  We do that crudely by always inserting a
  // separator after a tag close.
  return out;
}

function textWithSpaces($, $node) {
  return textNodeWalk($, $node.get(0));
}

// -- writers ----------------------------------------------------------

function writeModuleMdx(mod, pkg) {
  const slug = moduleSlug(mod.name);
  const file = join(flags.out, pkg, `${slug}.mdx`);
  ensureDir(dirname(file));

  const front = {
    title: mod.name,
    description: oneLine(mod.description) || `Haddock for ${mod.name}.`,
    sidebar: { label: mod.name },
  };

  const synopsis = mod.synopsis.length
    ? `<div class="synopsis">\n  <h2>Synopsis</h2>\n  <ul>\n${mod.synopsis.map((s) => `    <li><code>${escapeHtml(s)}</code></li>`).join('\n')}\n  </ul>\n</div>\n`
    : '';

  const sourceBase = `/wireform-/haddock/${pkg}`;
  const sectionsHtml = mod.sections.map((s) => renderSection(s, sourceBase)).join('\n\n');

  const moduleMeta = `<div class="haddock-module">
  <p class="module-meta">
    <span><strong>Package</strong>${escapeHtml(pkg)}</span>
    <span><strong>Module</strong>${escapeHtml(mod.name)}</span>
  </p>
</div>\n`;

  const description = mod.description
    ? `\n${markdownDescription(mod.description)}\n`
    : '';

  const content = `---
${yamlFrontmatter(front)}
---

${moduleMeta}
${description}
${synopsis}
${sectionsHtml}
`;

  writeFileSync(file, content);
}

function renderSection(section, sourceBase) {
  const heading = section.title ? `## ${escapeHtml(section.title)}\n` : '';
  const decls = section.decls.map((d) => renderDecl(d, sourceBase)).join('\n');
  return `${heading}\n${decls}`;
}

function renderDecl(decl, sourceBase) {
  const id = decl.anchor ? ` id="${escapeAttr(decl.anchor)}"` : '';
  const sourceLink = decl.sourceHref
    ? `<a class="source" href="${escapeAttr(joinUrl(sourceBase, decl.sourceHref))}" target="_blank" rel="noreferrer">Source</a>`
    : '';

  const sig = highlightSignature(decl.signature);
  const header = `<p class="decl-header">${sig}${sourceLink}</p>`;

  const docHtml = decl.docHtml ? `<div class="doc">${rewriteAnchorHtml(decl.docHtml)}</div>` : '';

  let constructors = '';
  if (decl.constructors.length) {
    constructors = `<div class="constructors">
  <h4>Constructors</h4>
  ${decl.constructors.map((c) => `<div class="constructor"><code>${escapeHtml(c.signature)}</code>${c.docHtml ? `<div class="doc">${rewriteAnchorHtml(c.docHtml)}</div>` : ''}</div>`).join('\n  ')}
</div>`;
  }
  let fields = '';
  if (decl.fields.length) {
    fields = `<div class="fields">
  <h4>Fields</h4>
  ${decl.fields.map((c) => `<div class="field"><code>${escapeHtml(c.signature)}</code>${c.docHtml ? `<div class="doc">${rewriteAnchorHtml(c.docHtml)}</div>` : ''}</div>`).join('\n  ')}
</div>`;
  }
  let methods = '';
  if (decl.methods.length) {
    methods = `<div class="methods">
  <h4>Methods</h4>
  ${decl.methods.map((c) => `<div class="method"><code>${escapeHtml(c.signature)}</code>${c.docHtml ? `<div class="doc">${rewriteAnchorHtml(c.docHtml)}</div>` : ''}</div>`).join('\n  ')}
</div>`;
  }
  let instances = '';
  if (decl.instances) {
    instances = `<p class="doc"><em>${decl.instances} instance${decl.instances === 1 ? '' : 's'} elided.</em></p>`;
  }

  const body = [docHtml, constructors, fields, methods, instances].filter(Boolean).join('\n');
  const bodyBlock = body ? `<div class="decl-body">\n${body}\n</div>` : '';

  return `<section class="decl"${id}>\n${header}\n${bodyBlock}\n</section>\n`;
}

function highlightSignature(sig) {
  // Cheap-and-cheerful Haskell highlighting for the declaration header.  We
  // identify (a) a leading keyword if any, and (b) the declared name (the
  // identifier that this declaration is *binding*).  Everything else stays
  // plain.
  const kwRe = /^(data|newtype|type|class|instance|family|pattern|module|module instance)\b/;
  const head = sig.match(kwRe);
  if (head) {
    const rest = sig.slice(head[0].length);
    const m = rest.match(/^(\s+)([A-Za-z_][A-Za-z0-9_'.]*)/);
    if (m) {
      const after = rest.slice(m[0].length);
      return `<span class="keyword">${escapeHtml(head[0])}</span>${escapeHtml(m[1])}<span class="name">${escapeHtml(m[2])}</span>${escapeHtml(after)}`;
    }
    return `<span class="keyword">${escapeHtml(head[0])}</span>${escapeHtml(rest)}`;
  }
  // Value-level declaration: 'name :: <type>'.  Bold the name, leave the
  // type plain so '::' and arrow art stays readable.
  const m = sig.match(/^([A-Za-z_'][A-Za-z0-9_'.#]*)(\s*::\s*)(.*)$/);
  if (m) {
    return `<span class="name">${escapeHtml(m[1])}</span>${escapeHtml(m[2])}${escapeHtml(m[3])}`;
  }
  return escapeHtml(sig);
}

function rewriteAnchorHtml(html) {
  // Rewrite `Foo-Bar.html#v:thing` style links to point at the new MDX
  // module pages so navigation stays inside the new docs site.  We don't
  // know what package the link came from, so we leave that disambiguation
  // for a future pass.
  const rewritten = html.replace(/href="([A-Za-z0-9_-]+)\.html(#[^"]*)?"/g, (_m, mod, anchor) => {
    const target = mod.toLowerCase();
    return `href="../${target}/${anchor || ''}"`;
  });
  // Collapse remaining physical newlines to spaces (HTML-neutral outside
  // <pre>, whose newlines are already entities via flattenPre): a line
  // beginning with "+ ", "- ", "1. ", "#", … inside embedded HTML would
  // otherwise start a markdown list/heading, end the paragraph, and orphan
  // the enclosing tag.
  return escapeMdxBraces(flattenPre(rewritten)).replace(/\n[ \t]*/g, ' ');
}

// MDX terminates an HTML flow block at a blank line, so a Haddock code
// block (`<pre>` with blank lines inside — common in wire-format grammars)
// orphans its `</pre>` and fails the build with "Expected a closing tag
// for `<pre>`".  Collapse each <pre>…</pre> onto one physical line by
// encoding its newlines; `white-space: pre` renders `&#10;` identically.
function flattenPre(html) {
  return String(html).replace(/<pre\b[^>]*>[\s\S]*?<\/pre>/g, (block) =>
    block.replace(/\n/g, '&#10;'),
  );
}

function writePackageIndex(pkg, modules) {
  const file = join(flags.out, pkg, 'index.mdx');
  ensureDir(dirname(file));
  const items = modules
    .map((m) => `  <li><a href="/wireform-/api/${pkg}/${moduleSlug(m.name)}/"><code>${escapeHtml(m.name)}</code></a>${m.description ? `<div class="desc">${escapeHtml(oneLine(m.description).slice(0, 240))}</div>` : ''}</li>`)
    .join('\n');
  const front = {
    title: pkg,
    description: `Haddock-derived API reference for ${pkg}.`,
    sidebar: { label: pkg, order: 0 },
  };
  const body = `---
${yamlFrontmatter(front)}
---

The ${escapeHtml(pkg)} package exposes ${modules.length} module${modules.length === 1 ? '' : 's'}.

<ul class="module-index">
${items}
</ul>
`;
  writeFileSync(file, body);
}

function writeApiLanding(packages) {
  ensureDir(flags.out);
  const file = join(flags.out, 'index.mdx');
  const front = {
    title: 'API reference',
    description: 'Haddock-derived API reference, re-rendered with type-signature highlighting and a per-module synopsis.',
  };
  const lines = [];
  lines.push(`---`);
  lines.push(yamlFrontmatter(front));
  lines.push(`---`);
  lines.push('');
  if (packages.length === 0) {
    lines.push(`The API reference has not been ingested yet.`);
    lines.push('');
    lines.push('Run `cabal v2-haddock all --haddock-html` from the repo root, then `npm run ingest` from `website/`.');
    lines.push('');
    lines.push('See [`scripts/ingest-haddock.mjs`](https://github.com/iand675/wireform-/blob/main/website/scripts/ingest-haddock.mjs) for details.');
  } else {
    lines.push('Browse Haddock-derived API documentation per package.');
    lines.push('');
    lines.push('<ul class="module-index">');
    for (const p of packages) {
      lines.push(`  <li><a href="/wireform-/api/${p.pkg}/"><code>${escapeHtml(p.pkg)}</code></a><div class="desc">${p.count} module${p.count === 1 ? '' : 's'}</div></li>`);
    }
    lines.push('</ul>');
  }
  writeFileSync(file, lines.join('\n'));
}

function writeStubLanding() {
  ensureDir(flags.out);
  writeApiLanding([]);
}

function buildSidebar(pkg, modules) {
  // Tree by '.' separators: Foo.Bar.Baz becomes Foo > Bar > Baz.
  const root = { children: new Map() };
  for (const m of modules) {
    const parts = m.name.split('.');
    let node = root;
    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      const isLeaf = i === parts.length - 1;
      if (!node.children.has(part)) {
        node.children.set(part, { children: new Map(), module: null });
      }
      const child = node.children.get(part);
      if (isLeaf) child.module = m;
      node = child;
    }
  }
  function toItems(node) {
    const items = [];
    for (const [name, child] of [...node.children.entries()].sort(([a], [b]) => a.localeCompare(b))) {
      const hasChildren = child.children.size > 0;
      if (hasChildren) {
        const group = {
          label: name,
          collapsed: true,
          items: toItems(child),
        };
        if (child.module) {
          // Insert the module page at the top so clicking the group label
          // also reaches a useful page.
          group.items.unshift({ label: name, slug: `api/${pkg}/${moduleSlug(child.module.name)}` });
        }
        items.push(group);
      } else if (child.module) {
        items.push({ label: name, slug: `api/${pkg}/${moduleSlug(child.module.name)}` });
      }
    }
    return items;
  }
  return {
    label: pkg,
    collapsed: true,
    items: [
      { label: 'Overview', slug: `api/${pkg}` },
      ...toItems(root),
    ],
  };
}

function writeSidebar(groups) {
  ensureDir(dirname(flags.sidebar));
  const flat = [];
  flat.push({ label: 'Overview', slug: 'api' });
  for (const g of groups) flat.push(g);
  writeFileSync(flags.sidebar, JSON.stringify(flat, null, 2));
}

// -- utilities --------------------------------------------------------

function moduleSlug(name) {
  return name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '');
}

function escapeHtml(s = '') {
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    // MDX parses '{' as the start of a JS expression even inside HTML
    // elements, so escape both braces.
    .replace(/\{/g, '&#123;')
    .replace(/\}/g, '&#125;')
    // MDX still runs the *markdown* inline parser inside raw HTML, so a
    // type signature like `f_a * f_b` becomes emphasis and orphans the
    // enclosing tag. Entity-encode the emphasis triggers.
    .replace(/_/g, '&#95;')
    .replace(/\*/g, '&#42;')
    .replace(/~/g, '&#126;')
    .replace(/`/g, '&#96;');
}

function escapeAttr(s = '') {
  return escapeHtml(s);
}

// For strings that are already HTML (Haddock output): neutralize only the
// MDX-active characters, leaving tags intact. Applied AFTER 'flattenPre';
// '_' and '*' become entities so markdown emphasis can't fire inside doc
// prose or inline code.
function escapeMdxBraces(s = '') {
  return String(s)
    .replace(/\{/g, '&#123;')
    .replace(/\}/g, '&#125;')
    .replace(/_/g, '&#95;')
    .replace(/\*/g, '&#42;')
    .replace(/~/g, '&#126;')
    .replace(/`/g, '&#96;')
    // A backslash escapes the next character in MDX ("\<" hides the '<'
    // of the very tag that follows), so it must be an entity.
    .replace(/\\/g, '&#92;')
    // Literal markdown-link syntax in prose ("[RFC 1945](https://…)")
    // otherwise interleaves with Haddock's <em>/<code> spans and breaks
    // MDX's inline parser; real links are already <a> tags.
    .replace(/\[/g, '&#91;')
    // A '<' that cannot start a tag (e.g. the literal <"> in an RFC
    // grammar) is a hard MDX error; real tags start with a letter, '/',
    // or '!'.
    .replace(/<(?![A-Za-z/!])/g, '&lt;');
}

function oneLine(s = '') {
  return String(s).replace(/\s+/g, ' ').trim();
}

function yamlFrontmatter(o) {
  // tiny safe YAML emitter — only handles strings + nested objects + arrays
  function emit(value, indent) {
    const pad = '  '.repeat(indent);
    if (Array.isArray(value)) {
      return value.map((v) => `${pad}- ${emitInline(v)}`).join('\n');
    }
    if (typeof value === 'object' && value !== null) {
      return Object.entries(value)
        .map(([k, v]) => {
          if (typeof v === 'object' && v !== null) {
            return `${pad}${k}:\n${emit(v, indent + 1)}`;
          }
          return `${pad}${k}: ${emitInline(v)}`;
        })
        .join('\n');
    }
    return `${pad}${emitInline(value)}`;
  }
  function emitInline(v) {
    if (v === null || v === undefined) return '~';
    if (typeof v === 'number' || typeof v === 'boolean') return String(v);
    const s = String(v);
    // always quote to be safe with colons/quotes/specials
    return JSON.stringify(s);
  }
  return emit(o, 0);
}

function ensureDir(d) {
  mkdirSync(d, { recursive: true });
}

function joinUrl(base, href) {
  if (!href) return base;
  if (/^[a-z]+:/i.test(href)) return href;
  if (href.startsWith('/')) return href;
  return `${base.replace(/\/$/, '')}/${href}`;
}

function markdownDescription(s) {
  // The module description was extracted with cheerio's .text(): it is
  // PLAIN TEXT with entities decoded and tags stripped, so it must be
  // fully HTML-escaped before re-embedding (an RFC grammar like <addr-spec>
  // would otherwise read as an unclosed JSX tag and fail the MDX build).
  // Paragraph breaks are preserved as <br/><br/>; every other newline
  // collapses to a space so no line can start a markdown list/heading or
  // read as an MDX blank line (whitespace-only lines included).
  return `<div class="doc">${escapeHtml(s)
    .replace(/\n(?:[ \t]*\n)+/g, '<br/><br/>')
    .replace(/\n[ \t]*/g, ' ')}</div>`;
}
