import fs from "node:fs";
import path from "node:path";

const ROOT = process.cwd();
const includeDocs = process.argv.includes("--include-docs");
const failures = [];

function read(relativePath) {
  const fullPath = path.join(ROOT, relativePath);
  try {
    return fs.readFileSync(fullPath, "utf8");
  } catch (error) {
    failures.push(`${relativePath}: no se pudo leer (${error.message})`);
    return "";
  }
}

function requireText(relativePath, checks) {
  const content = read(relativePath);
  for (const [label, pattern] of checks) {
    const ok = pattern instanceof RegExp ? pattern.test(content) : content.includes(pattern);
    if (!ok) failures.push(`${relativePath}: falta ${label}`);
  }
}

function requireMatch(relativePath, checks) {
  const content = read(relativePath);
  for (const [label, pattern] of checks) {
    if (!pattern.test(content)) failures.push(`${relativePath}: falta ${label}`);
  }
}

function forbidText(relativePath, checks) {
  const content = read(relativePath);
  for (const [label, pattern] of checks) {
    const found = pattern instanceof RegExp ? pattern.test(content) : content.includes(pattern);
    if (found) failures.push(`${relativePath}: contiene ${label}`);
  }
}

function walk(relativePath, extensions = new Set([".js", ".html"])) {
  const fullPath = path.join(ROOT, relativePath);
  if (!fs.existsSync(fullPath)) return [];
  const entries = fs.readdirSync(fullPath, { withFileTypes: true });
  return entries.flatMap((entry) => {
    const child = path.join(relativePath, entry.name);
    if (entry.isDirectory()) return walk(child, extensions);
    return extensions.has(path.extname(entry.name)) ? [child] : [];
  });
}

function checkNoConsoleLogs() {
  const files = [
    "reading-progress.html",
    "share-buttons-auto.html",
    ...walk("assets/js"),
    ...walk("atlas/js"),
    "atlas/app.js"
  ];
  for (const file of files) {
    const content = read(file);
    if (/\bconsole\.(log|debug|warn|error)\s*\(/.test(content)) {
      failures.push(`${file}: contiene console.*`);
    }
  }
}

requireText("_quarto.yml", [
  ["include-before-body: skip-link.html", "include-before-body: skip-link.html"],
  ["tokens como recurso", "assets/css/tokens.css"],
  ["pulso de portada como recurso", "assets/js/home-pulse.js"]
]);

requireText("index.qmd", [
  ["pulso de portada", "data-home-pulse"],
  ["canonical de portada", "include-in-header: home-head.html"],
  ["include del pulso de portada", "include-after-body: home-pulse.html"],
  ["titulo SEO sin nombre duplicado", 'pagetitle: "Economía aplicada RD"']
]);

requireText("home-head.html", [
  ["canonical raíz", '<link rel="canonical" href="https://leo8221.github.io/leonardo-mena-blog/">']
]);

requireText("home-pulse.html", [
  ["script del pulso de portada", "assets/js/home-pulse.js"]
]);

requireText(".github/workflows/actualizar_observatorio.yml", [
  ["ingesta de fuentes Atlas", "Rscript atlas/scripts/fetch-atlas-sources.R"],
  ["transformacion BCRD viva", "Rscript atlas/scripts/build-bcrd-live-data.R"],
  ["mapa provincial Atlas", "Rscript atlas/scripts/build-map-assets.R"],
  ["visuales de articulos Atlas", "Rscript atlas/scripts/build-article-visuals.R"],
  ["datos publicos Atlas", "node atlas/scripts/build-atlas-data.mjs"],
  ["sitemap post-render", "node atlas/scripts/ensure-sitemap.mjs"]
]);

requireText("atlas/data/source-manifest.json", [
  ["manifiesto de fuentes", '"sources"'],
  ["fuentes BCRD CustomView", '"mode": "bcrd_custom_view"'],
  ["CustomView sector real", "2533-sector-real"],
  ["CustomView precios", "2534-precios"],
  ["CustomView sector externo", "2532-sector-externo"],
  ["CustomView ENCFT", "2541-encuesta-continua-encft"],
  ["CustomView sector fiscal", "2535-sector-fiscal"],
  ["CustomView monetario", "2536-sector-monetario-y-financiero"],
  ["CustomView mercado cambiario", "2538-mercado-cambiario"],
  ["fuente local cacheable", '"cache": true']
]);

requireText("atlas/scripts/fetch-atlas-sources.R", [
  ["registro source-run", "source-run.json"],
  ["cache raw", 'file.path(atlas_dir, "data", "raw")'],
  ["soporte URL", 'mode == "url"'],
  ["soporte BCRD CustomView", 'mode == "bcrd_custom_view"'],
  ["extraccion de Excel BCRD", "extract_excel_links"]
]);

requireText("atlas/scripts/build-bcrd-live-data.R", [
  ["salida BCRD viva", "bcrd-live-data.json"],
  ["lectura IPC", "ipc_base_2019-2020.xls"],
  ["lectura dolar", "TASA_DOLAR_REFERENCIA_MC.xlsx"],
  ["lectura TPM", "Serie_TPM.xlsx"]
]);

requireText("atlas/scripts/build-atlas-data.mjs", [
  ["hidratacion BCRD", "applyLiveBcrdData"],
  ["metadatos BCRD vivos", "publicLiveData"]
]);

requireText("skip-link.html", [
  ["enlace de salto al contenido", 'href="#quarto-content"'],
  ["clase skip-link", "skip-link"]
]);

requireText("styles.css", [
  ["estilos de skip link", ".skip-link"],
  ["focus-visible global", ":focus-visible"],
  ["prefers-reduced-motion", "prefers-reduced-motion"]
]);

requireText("atlas/index.html", [
  ["skip link propio", 'href="#atlas-main"'],
  ["main destino", 'id="atlas-main"'],
  ["nav Blog hacia raíz canonical", '<a href="../">Blog</a>'],
  ["nav Archivo hacia archivo general", '<a href="../archivo.html">Archivo</a>']
]);

requireMatch("atlas/index.html", [
  ["cache busting de estilos", /styles\.css\?v=\d{8}/],
  ["cache busting de app", /app\.js\?v=\d{8}/],
  ["renderer de mapas modular", /js\/map-renderers\.js\?v=\d{8}/],
  ["bootstrap modular", /js\/bootstrap\.js\?v=\d{8}/]
]);

requireText("atlas/styles.css", [
  ["imports del CSS modular", "@import url(\"css/base.css"],
  ["imports responsive", "@import url(\"css/responsive.css"]
]);

requireText("atlas/css/base.css", [
  ["tokens compartidos desde la raiz", "../../assets/css/tokens.css"],
  ["estilos de skip link", ".skip-link"],
  ["focus-visible global", ":focus-visible"],
  ["prefers-reduced-motion", "prefers-reduced-motion"]
]);

checkNoConsoleLogs();

if (includeDocs) {
  for (const file of ["docs/index.html", "docs/about.html", "docs/suscribete.html"]) {
    requireText(file, [
      ["skip link renderizado", "skip-link"],
      ["destino Quarto", "quarto-content"]
    ]);
  }
  requireText("docs/atlas/index.html", [
    ["skip link del Atlas publicado", 'href="#atlas-main"'],
    ["nav Blog publicado hacia raíz canonical", '<a href="../">Blog</a>'],
    ["nav Archivo publicado", '<a href="../archivo.html">Archivo</a>']
  ]);
  requireMatch("docs/index.html", [
    ["canonical raíz", /<link rel="canonical" href="https:\/\/leo8221\.github\.io\/leonardo-mena-blog\/">/],
    ["titulo SEO sin duplicar nombre", /<title>Economía aplicada RD\s+[–-]\s+Leonardo Mena<\/title>/]
  ]);
  requireText("docs/sitemap.xml", [
    ["portada canonical en sitemap", "<loc>https://leo8221.github.io/leonardo-mena-blog/</loc>"],
    ["Atlas en sitemap", "<loc>https://leo8221.github.io/leonardo-mena-blog/atlas/</loc>"]
  ]);
  forbidText("docs/sitemap.xml", [
    ["portada no canonical en sitemap", "<loc>https://leo8221.github.io/leonardo-mena-blog/index.html</loc>"],
    ["observatorio heredado en sitemap", "<loc>https://leo8221.github.io/leonardo-mena-blog/observatorio.html</loc>"]
  ]);
  requireMatch("docs/atlas/index.html", [
    ["cache busting de estilos Atlas publicado", /styles\.css\?v=\d{8}/],
    ["cache busting de app Atlas publicado", /app\.js\?v=\d{8}/],
    ["renderer de mapas modular Atlas publicado", /js\/map-renderers\.js\?v=\d{8}/],
    ["bootstrap modular Atlas publicado", /js\/bootstrap\.js\?v=\d{8}/]
  ]);
}

if (failures.length) {
  console.error(`Site contract failed:\n- ${failures.join("\n- ")}`);
  process.exit(1);
}

console.log(`Site contract ok${includeDocs ? " (source + docs)" : " (source)"}`);
