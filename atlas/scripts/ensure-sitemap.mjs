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

function removeUrlEntry(xml, url) {
  const escapedUrl = url.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const entryPattern = new RegExp(`\\s*<url>\\s*<loc>${escapedUrl}<\\/loc>[\\s\\S]*?<\\/url>\\s*`, "g");
  return xml.replace(entryPattern, "\n");
}
