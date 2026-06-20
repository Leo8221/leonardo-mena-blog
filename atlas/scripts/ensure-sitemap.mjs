import fs from "node:fs";
import path from "node:path";

const root = process.cwd();
const sitemapPath = path.join(root, "docs", "sitemap.xml");
const atlasUrl = "https://leo8221.github.io/leonardo-mena-blog/atlas/";

if (!fs.existsSync(sitemapPath)) {
  throw new Error("docs/sitemap.xml no existe. Ejecuta quarto render antes de actualizar el sitemap.");
}

const sitemap = fs.readFileSync(sitemapPath, "utf8");

if (sitemap.includes(`<loc>${atlasUrl}</loc>`)) {
  process.exit(0);
}

const closingTag = "</urlset>";
if (!sitemap.includes(closingTag)) {
  throw new Error("docs/sitemap.xml no contiene </urlset>.");
}

const atlasEntry = `  <url>\n    <loc>${atlasUrl}</loc>\n  </url>\n`;
const updated = sitemap.replace(closingTag, `${atlasEntry}</urlset>`);

fs.writeFileSync(sitemapPath, updated, "utf8");
