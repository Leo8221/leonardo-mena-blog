import fs from "node:fs";
import path from "node:path";

const root = process.cwd();
const sitemapPath = path.join(root, "docs", "sitemap.xml");
const homeUrl = "https://leo8221.github.io/leonardo-mena-blog/";
const homeIndexUrl = "https://leo8221.github.io/leonardo-mena-blog/index.html";
const atlasUrl = "https://leo8221.github.io/leonardo-mena-blog/atlas/";
const legacyObservatorioUrl = "https://leo8221.github.io/leonardo-mena-blog/observatorio.html";

if (!fs.existsSync(sitemapPath)) {
  throw new Error("docs/sitemap.xml no existe. Ejecuta quarto render antes de actualizar el sitemap.");
}

const sitemap = fs.readFileSync(sitemapPath, "utf8");

const withoutLegacy = removeUrlEntry(sitemap, legacyObservatorioUrl);
const withoutIndexHome = removeUrlEntry(withoutLegacy, homeIndexUrl);
const withoutDuplicateHome = removeUrlEntry(withoutIndexHome, homeUrl);
const withoutDuplicateAtlas = removeUrlEntry(withoutDuplicateHome, atlasUrl);

const closingTag = "</urlset>";
if (!withoutDuplicateAtlas.includes(closingTag)) {
  throw new Error("docs/sitemap.xml no contiene </urlset>.");
}

const canonicalEntries = [
  `  <url>\n    <loc>${homeUrl}</loc>\n  </url>\n`,
  `  <url>\n    <loc>${atlasUrl}</loc>\n  </url>\n`
].join("");
const updated = withoutDuplicateAtlas.replace(closingTag, `${canonicalEntries}</urlset>`);

if (updated !== sitemap) {
  fs.writeFileSync(sitemapPath, updated, "utf8");
}

pruneUnreferencedFigureFiles();
pruneUnreferencedSiteLibs();
normalizeGeneratedWhitespace();

function removeUrlEntry(xml, url) {
  const escapedUrl = url.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const entryPattern = new RegExp(`\\s*<url>\\s*<loc>${escapedUrl}<\\/loc>[\\s\\S]*?<\\/url>\\s*`, "g");
  return xml.replace(entryPattern, "\n");
}

function pruneUnreferencedFigureFiles() {
  const postsDir = path.join(root, "docs", "posts");
  if (!fs.existsSync(postsDir)) return;

  for (const htmlPath of findPostIndexes(postsDir)) {
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

function findPostIndexes(directory) {
  const entries = fs.readdirSync(directory, { withFileTypes: true });
  return entries.flatMap((entry) => {
    const child = path.join(directory, entry.name);
    if (entry.isDirectory()) return findPostIndexes(child);
    return entry.isFile() && entry.name === "index.html" ? [child] : [];
  });
}

function pruneUnreferencedSiteLibs() {
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
