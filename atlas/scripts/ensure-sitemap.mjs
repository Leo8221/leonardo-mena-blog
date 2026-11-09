import fs from "node:fs";
import path from "node:path";

const root = process.cwd();
// Segundo paso post-render de _quarto.yml. Lee solo docs/: normaliza enlaces,
// completa el sitemap y retira recursos generados que su HTML ya no referencia.
// No se ejecuta como limpiador de fuentes ni de archivos de investigación.
// La vista previa no debe modificar ni depurar la salida publicable.
const outputDir = path.resolve(root, process.env.QUARTO_PROJECT_OUTPUT_DIR || "docs");
if (outputDir !== path.resolve(root, "docs")) process.exit(0);
const sitemapPath = path.join(root, "docs", "sitemap.xml");
const siteUrl = "https://leo8221.github.io/leonardo-mena-blog/";
const homeUrl = siteUrl;
const atlasUrl = new URL("atlas/", siteUrl).href;
const legacyObservatorioUrl = new URL("observatorio.html", siteUrl).href;

if (!fs.existsSync(sitemapPath)) {
  throw new Error("docs/sitemap.xml no existe. Ejecuta quarto render antes de actualizar el sitemap.");
}

injectCanonicalLinks();

const sitemap = fs.readFileSync(sitemapPath, "utf8");

const closingTag = "</urlset>";
if (!sitemap.includes(closingTag)) {
  throw new Error("docs/sitemap.xml no contiene </urlset>.");
}

// Inicio y Atlas se añaden una sola vez al final, tras normalizar las rutas.
const seen = new Set([homeUrl, atlasUrl, legacyObservatorioUrl]);
const normalized = sitemap.replace(/\s*<url>\s*<loc>([^<]+)<\/loc>[\s\S]*?<\/url>\s*/g, (entry, url) => {
  const canonical = canonicalizeUrl(url);
  if (seen.has(canonical)) return "\n";
  seen.add(canonical);
  return entry.replace(`<loc>${url}</loc>`, `<loc>${canonical}</loc>`);
});
const canonicalEntries = [homeUrl, atlasUrl]
  .map(url => `  <url>\n    <loc>${url}</loc>\n  </url>\n`).join("");
const updated = normalized.replace(closingTag, canonicalEntries + closingTag);

if (updated !== sitemap) {
  fs.writeFileSync(sitemapPath, updated, "utf8");
}

pruneUnreferencedFigureFiles();
pruneUnreferencedSiteLibs();
normalizeGeneratedWhitespace();

function canonicalizeUrl(url) {
  return url.replace(/\/index\.html$/, "/");
}

function injectCanonicalLinks() {
  const docsDir = path.join(root, "docs");
  if (!fs.existsSync(docsDir)) return;

  for (const file of findHtmlFiles(docsDir)) {
    const relativePath = path.relative(docsDir, file).replace(/\\/g, "/");
    const canonical = canonicalUrlForHtml(relativePath);
    let html = fs.readFileSync(file, "utf8");
    const linkTag = `<link rel="canonical" href="${canonical}">`;
    const ogUrlTag = `<meta property="og:url" content="${canonical}">`;

    html = normalizeInternalLinks(html, relativePath);

    if (/<link\s+rel="canonical"[^>]*>/i.test(html)) {
      html = html.replace(/<link\s+rel="canonical"[^>]*>/i, linkTag);
    } else {
      html = html.replace("</head>", `${linkTag}\n</head>`);
    }

    if (/<meta\s+property="og:url"[^>]*>/i.test(html)) {
      html = html.replace(/<meta\s+property="og:url"[^>]*>/i, ogUrlTag);
    } else {
      html = html.replace("</head>", `${ogUrlTag}\n</head>`);
    }

    fs.writeFileSync(file, html, "utf8");
  }
}

function canonicalUrlForHtml(relativePath) {
  if (relativePath === "index.html") return homeUrl;
  if (relativePath.endsWith("/index.html")) {
    return `${siteUrl}${relativePath.slice(0, -("index.html".length))}`;
  }
  return `${siteUrl}${relativePath}`;
}

function normalizeInternalLinks(html, relativePath) {
  // La raíz pública incluye /leonardo-mena-blog/. Un /posts/... absoluto saldría
  // de ella; el prefijo relativo conserva los enlaces desde cualquier artículo.
  const prefix = relativeRootPrefix(relativePath);
  return html
    .replace(/href="\/posts\/([^"]+)\/index\.html"/g, `href="${prefix}posts/$1/"`)
    .replace(/href="((?:\.\.\/)+posts\/[^"]+)\/index\.html"/g, `href="$1/"`);
}

function relativeRootPrefix(relativePath) {
  const directory = path.posix.dirname(relativePath.replace(/\\/g, "/"));
  if (directory === ".") return "";
  return "../".repeat(directory.split("/").filter(Boolean).length);
}

function pruneUnreferencedFigureFiles() {
  // Solo considera index_files/figure-html del artículo renderizado. El contrato
  // actual exige que cada figura generada esté referenciada en su index.html.
  const postsDir = path.join(root, "docs", "posts");
  if (!fs.existsSync(postsDir)) return;

  for (const htmlPath of findHtmlFiles(postsDir).filter(file => path.basename(file) === "index.html")) {
    const postDir = path.dirname(htmlPath);
    const figureDir = path.join(postDir, "index_files", "figure-html");
    if (!fs.existsSync(figureDir)) continue;

    const html = fs.readFileSync(htmlPath, "utf8");
    for (const entry of fs.readdirSync(figureDir, { withFileTypes: true })) {
      if (!entry.isFile()) continue;

      const relativePath = `index_files/figure-html/${entry.name}`;
      const encodedPath = encodeURI(relativePath);
      if (!html.includes(relativePath) && !html.includes(encodedPath)) {
        fs.unlinkSync(path.join(figureDir, entry.name));
      }
    }
  }
}

function pruneUnreferencedSiteLibs() {
  // Se conservan carpetas de biblioteca citadas por algún HTML publicado.
  // Si se añade carga dinámica de bibliotecas, hay que revisar este criterio.
  const docsDir = path.join(root, "docs");
  const siteLibsDir = path.join(docsDir, "site_libs");
  if (!fs.existsSync(siteLibsDir)) return;

  const html = findHtmlFiles(docsDir).map((file) => fs.readFileSync(file, "utf8")).join("\n");
  for (const entry of fs.readdirSync(siteLibsDir, { withFileTypes: true })) {
    if (!entry.isDirectory()) continue;

    const reference = `site_libs/${entry.name}/`;
    if (!html.includes(reference)) {
      fs.rmSync(path.join(siteLibsDir, entry.name), { recursive: true, force: true });
    }
  }
}

function findHtmlFiles(directory) {
  const entries = fs.readdirSync(directory, { withFileTypes: true });
  return entries.flatMap((entry) => {
    const child = path.join(directory, entry.name);
    if (entry.isDirectory()) return findHtmlFiles(child);
    return entry.isFile() && entry.name.endsWith(".html") ? [child] : [];
  });
}

function normalizeGeneratedWhitespace() {
  const files = [
    path.join(root, "docs", "site_libs", "quarto-html", "axe", "axe-check.js")
  ];

  for (const file of files) {
    if (!fs.existsSync(file)) continue;
    const content = fs.readFileSync(file, "utf8");
    const normalized = content.replace(/[ \t]+$/gm, "");
    if (normalized !== content) fs.writeFileSync(file, normalized, "utf8");
  }
}
